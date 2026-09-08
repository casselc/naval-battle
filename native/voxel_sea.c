// SPDX-License-Identifier: MIT
// Sea kernels for the ocean sim, called from voxel.seac.
//
// vsea_field: the sparse vortex-field sum. The pure
// voxel.ocean/sparse-velocities is the reference; this is the same sum
// (same order over actives, same truncation) at native speed. Dense churn
// still goes to the FMM - this kernel covers the sparse regime.
//
// vsea_mesh_*: the whole wave sheet as ONE raylib mesh, built, lit and
// drawn in C. Issuing rlVertex3f per corner from jolt costs microseconds
// per call; a 1849-particle sea is ~40ms of frame time that way. Here the
// same sheet costs one buffer fill + one DrawMesh.
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

#include <raylib.h>

// ---------------------------------------------------------------- field ----

// particles: xs/zs/om are n doubles each, row-major by particle index.
// act: indices (ascending) of particles whose |omega| exceeds eps.
// out: vx/vz receive the per-particle velocity [vx, vz].
// Contributions beyond radius-squared r2 are dropped (the 1/r tail is under
// the noise floor there - that water rests); coincident points skip.
void vsea_field(const double *xs, const double *zs, const double *om,
                const int64_t *act, int64_t na,
                double r2, double eps,
                double *vx, double *vz, int64_t n)
{
	for (int64_t i = 0; i < n; i++) {
		double xi = xs[i], zi = zs[i], fx = 0.0, fz = 0.0;
		for (int64_t a = 0; a < na; a++) {
			int64_t j = act[a];
			if (j == i)
				continue;
			double dx = xi - xs[j];
			double dz = zi - zs[j];
			double d2 = dx * dx + dz * dz;
			if (d2 > r2 || d2 < 1e-24)
				continue;
			double k = om[j] / (6.283185307179586 * d2);
			fx -= k * dz;
			fz += k * dx;
		}
		vx[i] = fx;
		vz[i] = fz;
	}
}

// ---------------------------------------------------------------- fmm -----
//
// Multi-level quadtree FMM (Greengard 2D) for fully-churned seas - ambient
// turbulence makes every particle active, and the O(N*na) sparse kernel
// loses at that scale. The pure voxel.ocean/fmm-velocities is the
// reference; same conventions everywhere: conjugated positions
// zeta = (x, -z), charges q = i*omega/2pi, expansion order 10, leaf cap
// 12, max depth 6, separation 2 cell widths, adaptive levels compared at
// the coarser cell.

#define FMM_MAXN 65536
#define FMM_P 10
#define FMM_LEAF 12
#define FMM_DEPTH 6
// a quadtree capped at FMM_DEPTH holds at most (4^(D+1)-1)/3 nodes however
// many particles arrive, so the node arrays size off the depth, not off N
#define FMM_MAXNODES 5472
#define FMM_GRID_CELLS 5461         // sum of 4^d for d in 0..FMM_DEPTH
#define FMM_MAXNBR (1 << 21)

typedef struct {
	double cx, cz, h;
	int depth, parent, leaf;
	int ix, iz;                 // cell index within its level's 2^d grid
	int kid[4];
	int begin, end;             // slice of forder[]
} FmmNode;

static FmmNode fnodes[FMM_MAXNODES];
static int fnodes_n;
static int forder[FMM_MAXN];
static double fa_re[FMM_MAXNODES][FMM_P], fa_im[FMM_MAXNODES][FMM_P];
static double fb_re[FMM_MAXNODES][FMM_P], fb_im[FMM_MAXNODES][FMM_P];
static int flevels[FMM_DEPTH + 1][FMM_MAXNODES];
static int flevel_n[FMM_DEPTH + 1];
// per-level occupancy grid: fgrid[fgrid_off[d] + ix * 2^d + iz] is the node
// id at that cell, or -1. Turns colleague and interaction-list lookup into
// index arithmetic; scanning every same-level pair instead is O(cells^2) and
// was the whole cost of the solve.
static int fgrid[FMM_GRID_CELLS];
static const int fgrid_off[FMM_DEPTH + 2] = {0, 1, 5, 21, 85, 341, 1365, 5461};
static int fnbr[FMM_MAXNBR];
static int fnbr_off[FMM_MAXNODES + 1];
static int fnbr_all[FMM_MAXNODES];  // leaf flag: near field = everyone
static double fbinom[2 * FMM_P][2 * FMM_P];
static int fmm_ready = 0;

static const double *fmm_xs, *fmm_zs, *fmm_om;
static double fmm_bounds, fmm_cx, fmm_cz;

static void fmm_tables(void)
{
	if (fmm_ready)
		return;
	for (int n = 0; n < 2 * FMM_P; n++)
		for (int r = 0; r < 2 * FMM_P; r++)
			fbinom[n][r] = (r == 0 || r == n) ? 1.0
				: (r > n ? 0.0 : fbinom[n - 1][r - 1] + fbinom[n - 1][r]);
	fmm_ready = 1;
}

static double fmm_clamp(double v, double c)
{
	double lo = c - fmm_bounds, hi = c + fmm_bounds;
	return v < lo ? lo : (v > hi ? hi : v);
}

static int fmm_split(int parent, double cx, double cz, double h,
                     int depth, int ix, int iz, int begin, int end)
{
	int id = fnodes_n++;
	FmmNode *nd = &fnodes[id];
	nd->cx = cx; nd->cz = cz; nd->h = h;
	nd->depth = depth; nd->parent = parent;
	nd->ix = ix; nd->iz = iz;
	fgrid[fgrid_off[depth] + (ix << depth) + iz] = id;
	nd->begin = begin; nd->end = end;
	for (int q = 0; q < 4; q++)
		nd->kid[q] = -1;
	if ((end - begin) <= FMM_LEAF || depth >= FMM_DEPTH) {
		nd->leaf = 1;
		return id;
	}
	nd->leaf = 0;
	static int tmp[FMM_MAXN];
	int cnt[4] = {0, 0, 0, 0}, pos[4], start[4];
	for (int i = begin; i < end; i++) {
		int j = forder[i];
		double x = fmm_clamp(fmm_xs[j], fmm_cx);
		double z = fmm_clamp(fmm_zs[j], fmm_cz);
		int q = (z > cz) ? ((x > cx) ? 3 : 2) : ((x > cx) ? 1 : 0);
		cnt[q]++;
		tmp[i] = q;
	}
	int s = begin;
	for (int q = 0; q < 4; q++) {
		start[q] = s;
		pos[q] = s;
		s += cnt[q];
	}
	// stable scatter: two passes so order within a quadrant is preserved
	static int scratch[FMM_MAXN];
	for (int i = begin; i < end; i++)
		scratch[pos[tmp[i]]++] = forder[i];
	for (int i = begin; i < end; i++)
		forder[i] = scratch[i];
	for (int q = 0; q < 4; q++) {
		double h2 = 0.5 * h;
		double sx = (q & 1) ? h2 : -h2;
		double sz = (q >= 2) ? h2 : -h2;
		nd->kid[q] = fmm_split(id, cx + sx, cz + sz, h2, depth + 1,
		                       2 * ix + (q & 1), 2 * iz + (q >= 2),
		                       start[q], start[q] + cnt[q]);
	}
	return id;
}

// P2M: a_k = sum_j q_j (zeta_j - c)^k over the leaf's particles.
static void fmm_p2m(int id)
{
	FmmNode *nd = &fnodes[id];
	double ccx = nd->cx, cci = -nd->cz;
	double *are = fa_re[id], *aim = fa_im[id];
	for (int k = 0; k < FMM_P; k++) {
		are[k] = 0.0; aim[k] = 0.0;
	}
	for (int i = nd->begin; i < nd->end; i++) {
		int j = forder[i];
		double qi = fmm_om[j] * (0.5 / M_PI);
		double dr = fmm_xs[j] - ccx, di = -fmm_zs[j] - cci;
		are[0] += 0.0; aim[0] += qi;
		double zr = dr, zi = di;
		for (int k = 1; k < FMM_P; k++) {
			are[k] += 0.0 * zr - qi * zi;
			aim[k] += 0.0 * zi + qi * zr;
			double nr = zr * dr - zi * di, ni = zr * di + zi * dr;
			zr = nr; zi = ni;
		}
	}
}

// M2M: acc_k += child_k + sum_{l=1..k} (-1)^l C(k,l) delta^l child_{k-l},
// delta = parent center - child center (conjugated space).
static void fmm_m2m(int pid, int cid)
{
	FmmNode *p = &fnodes[pid], *c = &fnodes[cid];
	double dr = p->cx - c->cx, di = (-p->cz) - (-c->cz);
	double *are = fa_re[pid], *aim = fa_im[pid];
	const double *cre = fa_re[cid], *cim = fa_im[cid];
	for (int k = 0; k < FMM_P; k++) {
		double tr = cre[k], ti = cim[k];
		double lr = dr, li = di;  // delta^l, l starts at 1
		for (int l = 1; l <= k; l++) {
			double coef = fbinom[k][l] * ((l & 1) ? -1.0 : 1.0);
			tr += coef * (lr * cre[k - l] - li * cim[k - l]);
			ti += coef * (lr * cim[k - l] + li * cre[k - l]);
			double nr = lr * dr - li * di, ni = lr * di + li * dr;
			lr = nr; li = ni;
		}
		are[k] += tr; aim[k] += ti;
	}
}

// M2L: b_k += sum_m (-1)^(m+1) C(m+k,k) a_m d^(-m-1-k), d = src - target.
static void fmm_m2l(int tid, int sid)
{
	FmmNode *t = &fnodes[tid], *s = &fnodes[sid];
	double dr = s->cx - t->cx, di = (-s->cz) - (-t->cz);
	double m2 = dr * dr + di * di;
	if (m2 < 1e-24)
		return;
	double ir = dr / m2, ii = -di / m2;
	static double rr[FMM_P], ri[FMM_P], kr[FMM_P], ki[FMM_P];
	rr[0] = ir; ri[0] = ii;
	for (int m = 1; m < FMM_P; m++) {
		double nr = rr[m - 1] * ir - ri[m - 1] * ii;
		double ni = rr[m - 1] * ii + ri[m - 1] * ir;
		rr[m] = nr; ri[m] = ni;
	}
	kr[0] = 1.0; ki[0] = 0.0;
	for (int k = 1; k < FMM_P; k++) {
		double nr = kr[k - 1] * ir - ki[k - 1] * ii;
		double ni = kr[k - 1] * ii + ki[k - 1] * ir;
		kr[k] = nr; ki[k] = ni;
	}
	double *bre = fb_re[tid], *bim = fb_im[tid];
	const double *are = fa_re[sid], *aim = fa_im[sid];
	for (int k = 0; k < FMM_P; k++) {
		double sr = 0.0, si = 0.0;
		for (int m = 0; m < FMM_P; m++) {
			double coef = fbinom[m + k][k] * ((m & 1) ? 1.0 : -1.0);
			sr += coef * (rr[m] * are[m] - ri[m] * aim[m]);
			si += coef * (rr[m] * aim[m] + ri[m] * are[m]);
		}
		bre[k] += kr[k] * sr - ki[k] * si;
		bim[k] += kr[k] * si + ki[k] * sr;
	}
}

// L2L: b'_k = sum_{m>=k} C(m,k) mu^(m-k) b_m, mu = child - parent.
static void fmm_l2l(int tid)
{
	FmmNode *nd = &fnodes[tid];
	int pid = nd->parent;
	FmmNode *p = &fnodes[pid];
	double mr = nd->cx - p->cx, mi = (-nd->cz) - (-p->cz);
	double *bre = fb_re[tid], *bim = fb_im[tid];
	const double *pre = fb_re[pid], *pim = fb_im[pid];
	for (int k = 0; k < FMM_P; k++) {
		double tr = 0.0, ti = 0.0, wr = 1.0, wi = 0.0;
		for (int m = k; m < FMM_P; m++) {
			double coef = fbinom[m][k];
			tr += coef * (wr * pre[m] - wi * pim[m]);
			ti += coef * (wr * pim[m] + wi * pre[m]);
			double nr = wr * mr - wi * mi, ni = wr * mi + wi * mr;
			wr = nr; wi = ni;
		}
		bre[k] = tr; bim[k] = ti;
	}
}

// The node at (ix, iz) on level d, or -1 when the tree never created it
// (out of range, or an ancestor stopped splitting).
static int fmm_at(int d, int ix, int iz)
{
	int w = 1 << d;
	if (ix < 0 || iz < 0 || ix >= w || iz >= w)
		return -1;
	return fgrid[fgrid_off[d] + (ix << d) + iz];
}

static const FmmNode *fmm_ancestor(const FmmNode *nd, int lvl)
{
	while (nd->depth > lvl)
		nd = &fnodes[nd->parent];
	return nd;
}

// Same-level cells sit 2h apart, so the pure code's "centre separation
// exceeds two cell half-widths" is exactly "grid index differs by more
// than one" - colleagues are the 3x3 block, everything else is far field.
static int fmm_far(const FmmNode *a, const FmmNode *b)
{
	int dx = a->ix - b->ix, dz = a->iz - b->iz;
	if (dx < 0) dx = -dx;
	if (dz < 0) dz = -dz;
	return dx > 1 || dz > 1;
}

// The tree spans [cx-bounds, cx+bounds] x [cz-bounds, cz+bounds]. The
// ocean's window of particles scrolls with the camera, so the domain has to
// travel with it rather than sitting on the world origin.
void vsea_fmm(const double *xs, const double *zs, const double *om,
              int64_t n, int64_t p64, double bounds, double cx, double cz,
              double *vx, double *vz)
{
	int nn = (int)n;
	(void)p64;  // expansion order fixed at FMM_P, matching the pure code
	if (nn <= 0)
		return;
	fmm_tables();
	fmm_xs = xs; fmm_zs = zs; fmm_om = om;
	fmm_bounds = bounds; fmm_cx = cx; fmm_cz = cz;
	// plain direct sum if the caller exceeds the scratch capacity
	if (nn > FMM_MAXN) {
		for (int i = 0; i < nn; i++) {
			double fx = 0.0, fz = 0.0;
			for (int j = 0; j < nn; j++) {
				if (j == i)
					continue;
				double dx = xs[i] - xs[j], dz = zs[i] - zs[j];
				double d2 = dx * dx + dz * dz;
				if (d2 < 1e-24)
					continue;
				double k = om[j] / (6.283185307179586 * d2);
				fx -= k * dz;
				fz += k * dx;
			}
			vx[i] = fx; vz[i] = fz;
		}
		return;
	}
	// --- tree
	fnodes_n = 0;
	for (int i = 0; i < FMM_GRID_CELLS; i++)
		fgrid[i] = -1;
	for (int i = 0; i < nn; i++)
		forder[i] = i;
	fmm_split(-1, cx, cz, bounds, 0, 0, 0, 0, nn);
	for (int d = 0; d <= FMM_DEPTH; d++)
		flevel_n[d] = 0;
	for (int id = 0; id < fnodes_n; id++) {
		int d = fnodes[id].depth;
		flevels[d][flevel_n[d]++] = id;
	}
	// --- upward pass: deepest first
	for (int d = FMM_DEPTH; d >= 0; d--) {
		for (int li = 0; li < flevel_n[d]; li++) {
			int id = flevels[d][li];
			if (fnodes[id].leaf) {
				fmm_p2m(id);
			} else {
				for (int k = 0; k < FMM_P; k++) {
					fa_re[id][k] = 0.0;
					fa_im[id][k] = 0.0;
				}
				for (int q = 0; q < 4; q++)
					fmm_m2m(id, fnodes[id].kid[q]);
			}
		}
	}
	// --- downward pass: local expansions level by level
	for (int d = 0; d <= FMM_DEPTH; d++) {
		for (int li = 0; li < flevel_n[d]; li++) {
			int id = flevels[d][li];
			FmmNode *nd = &fnodes[id];
			if (d == 0) {
				for (int k = 0; k < FMM_P; k++) {
					fb_re[id][k] = 0.0;
					fb_im[id][k] = 0.0;
				}
			} else {
				fmm_l2l(id);
			}
			// Interaction list: the children of the parent's colleagues
			// that are NOT this cell's own colleagues. Identical to the
			// pure rule (same level, separated by more than two cell
			// widths, parents touching) but 36 candidates instead of a
			// scan over every cell on the level.
			if (d >= 1) {
				const FmmNode *par = &fnodes[nd->parent];
				for (int dx = -1; dx <= 1; dx++)
					for (int dz = -1; dz <= 1; dz++) {
						int pc = fmm_at(d - 1, par->ix + dx, par->iz + dz);
						if (pc < 0)
							continue;
						for (int q = 0; q < 4; q++) {
							int sid = fnodes[pc].kid[q];
							if (sid < 0 || sid == id)
								continue;
							if (fmm_far(&fnodes[sid], nd))
								fmm_m2l(id, sid);
						}
					}
			}
		}
	}
	// --- near-field neighbour lists per leaf.
	//
	// The pure rule: two leaves are near when their ancestors, compared at
	// the coarser of the two levels, are colleagues. Split by which leaf is
	// deeper and it becomes two cheap walks:
	//   (a) leaves at or below this leaf's level - every particle under one
	//       of its own colleagues, and a node's particles are already a
	//       contiguous forder slice, so no descent is needed;
	//   (b) leaves above it - colleagues of each ancestor that are leaves.
	// Disjoint by construction, so nothing is counted twice.
	int nbr_n = 0;
	for (int id = 0; id < fnodes_n; id++) {
		fnbr_off[id] = nbr_n;
		fnbr_all[id] = 0;
		if (!fnodes[id].leaf)
			continue;
		const FmmNode *a = &fnodes[id];
		int overflow = 0;
		for (int lvl = a->depth; lvl >= 0 && !overflow; lvl--) {
			const FmmNode *anc = fmm_ancestor(a, lvl);
			for (int dx = -1; dx <= 1 && !overflow; dx++)
				for (int dz = -1; dz <= 1 && !overflow; dz++) {
					int cid = fmm_at(lvl, anc->ix + dx, anc->iz + dz);
					if (cid < 0)
						continue;
					// above this leaf's level only leaves count; at its own
					// level the whole subtree is near field
					if (lvl < a->depth && !fnodes[cid].leaf)
						continue;
					const FmmNode *c = &fnodes[cid];
					if (nbr_n + (c->end - c->begin) > FMM_MAXNBR) {
						fnbr_all[id] = 1;
						overflow = 1;
						break;
					}
					for (int i = c->begin; i < c->end; i++)
						fnbr[nbr_n++] = forder[i];
				}
		}
	}
	fnbr_off[fnodes_n] = nbr_n;
	// --- evaluation: leaf local expansion + near-field direct
	for (int id = 0; id < fnodes_n; id++) {
		FmmNode *nd = &fnodes[id];
		if (!nd->leaf)
			continue;
		double ccx = nd->cx, cci = -nd->cz;
		for (int ii = nd->begin; ii < nd->end; ii++) {
			int i = forder[ii];
			double zr = xs[i], zi = -zs[i];
			double qim_i = 0.0;  // self term is skipped; charge unused
			(void)qim_i;
			double dr = zr - ccx, di = zi - cci;
			double wr = 0.0, wi = 0.0, tr = 1.0, ti = 0.0;
			for (int k = 0; k < FMM_P; k++) {
				wr += fb_re[id][k] * tr - fb_im[id][k] * ti;
				wi += fb_re[id][k] * ti + fb_im[id][k] * tr;
				double nr = tr * dr - ti * di, ni = tr * di + ti * dr;
				tr = nr; ti = ni;
			}
			if (fnbr_all[id]) {
				for (int j = 0; j < nn; j++) {
					if (j == i)
						continue;
					double ar = zr - xs[j], ai = zi + zs[j];
					double m2v = ar * ar + ai * ai;
					if (m2v < 1e-24)
						continue;
					double qj = om[j] * (0.5 / M_PI);
					double vr = ar / m2v, vi = -ai / m2v;
					// w += (0 + qj i)(vr + vi i)
					wr += -qj * vi;
					wi += qj * vr;
				}
			} else {
				for (int t = fnbr_off[id]; t < fnbr_off[id + 1]; t++) {
					int j = fnbr[t];
					if (j == i)
						continue;
					double ar = zr - xs[j], ai = zi + zs[j];
					double m2v = ar * ar + ai * ai;
					if (m2v < 1e-24)
						continue;
					double qj = om[j] * (0.5 / M_PI);
					double vr = ar / m2v, vi = -ai / m2v;
					wr += -qj * vi;
					wi += qj * vr;
				}
			}
			vx[i] = wr;
			vz[i] = wi;
		}
	}
}

// ---------------------------------------------------------------- sim -----
//
// The ocean's particle state, owned in C as flat arrays.
//
// It used to live as a Clojure vector of maps and cross the FFI boundary
// field-by-field twice a frame - roughly 30k ffi/write calls, which cost
// more than the solve itself. Here jolt never touches a particle in the hot
// path: it asks for a step, and the renderer's mesh is filled from the same
// arrays. voxel.ocean/step-ocean is still the readable reference and the
// tests hold this to it step for step.
//
// Every constant below mirrors voxel.ocean; voxel.ocean-test asserts they
// have not drifted apart.

// the ambient sea state, mirroring voxel.ocean/SWELL-TRAINS:
// {amplitude, wavenumber, frequency, dir x, dir z} per wave train
#define SIM_SWELL_TRAINS 3
static const double sim_swell[SIM_SWELL_TRAINS][5] = {
	{0.45, 0.55, 1.30,  0.86,  0.51},
	{0.22, 0.31, 0.83, -0.42,  0.91},
	{0.11, 1.15, 2.10,  0.62, -0.78},
};
#define SIM_CHOP_RATE   0.35
#define SIM_CHOP_AMP    0.0009
#define SIM_DRAG        0.90
#define SIM_SURFACE_BREAK 0.7
#define SIM_SURFACE_K   2.5
#define SIM_SURFACE_C   3.0
#define SIM_SPRAY_G     20.0
#define SIM_ACTIVE_EPS  1e-9
#define SIM_FIELD_RADIUS 14.0

typedef struct {
	int n, cols, ready, ambient, sparse_max;
	// The window of water we bother to simulate. It is a fixed lattice of
	// cols x cols particles that SCROLLS: (ox, oz) is where its centre sits
	// in the world, and (ioff, joff) rotate the index mapping so sliding it
	// by a tile costs one reseeded column instead of moving every particle.
	// Water that leaves the far edge comes back as still water at the near
	// one, which is what makes the ocean look endless.
	double ox, oz;
	int ioff, joff;
	double spacing, extent, bounds, viscosity, t;
	double *x, *z, *y, *vx, *vy, *vz, *om;
	double *ux, *uz;            // this step's field velocity
	int64_t *act;               // scratch: indices of the live vortices
} SeaSim;

static SeaSim sim = {0};

// Deterministic pseudo-random in [-1,1) from position and time bucket, bit
// for bit the same as voxel.ocean/chop-rand: the ambient turbulence field
// has to be reproducible in both implementations or they cannot be compared.
static double sim_chop_rand(double x, double z, int64_t k)
{
	int64_t h = ((int64_t)(x * 8.0) * 374761393)
	          ^ ((int64_t)(z * 8.0) * 668265263)
	          ^ (k * 1274126177);
	if (h < 0)
		h = -h;
	return 2.0 * ((double)(h % 1000003) / 1000003.0) - 1.0;
}

void vsea_sim_free(void)
{
	if (!sim.ready)
		return;
	free(sim.x); free(sim.z); free(sim.y);
	free(sim.vx); free(sim.vy); free(sim.vz);
	free(sim.om); free(sim.ux); free(sim.uz); free(sim.act);
	memset(&sim, 0, sizeof(sim));
}

// vsea_sim_init(cols, extent, bounds, ambient, viscosity, sparse_max): a
// still sheet of cols x cols particles evenly covering [-extent, extent]^2.
// bounds is the reflecting domain half-width; sparse_max is the live-vortex
// count above which the field solve switches from exact sums to the FMM
// (voxel.ocean owns the value, so there is one definition of it).
void vsea_sim_init(int cols, double extent, double bounds,
                   int ambient, double viscosity, int sparse_max)
{
	vsea_sim_free();
	if (cols < 2)
		cols = 2;
	int n = cols * cols;
	if (n > FMM_MAXN)
		return;
	sim.n = n;
	sim.cols = cols;
	sim.extent = extent;
	sim.bounds = bounds;
	sim.ambient = ambient;
	sim.viscosity = viscosity;
	sim.sparse_max = sparse_max;
	sim.spacing = 2.0 * extent / (double)(cols - 1);
	sim.ox = 0.0; sim.oz = 0.0;
	sim.ioff = 0; sim.joff = 0;
	sim.t = 0.0;
	size_t b = (size_t)n * sizeof(double);
	sim.x = malloc(b); sim.z = malloc(b); sim.y = malloc(b);
	sim.vx = malloc(b); sim.vy = malloc(b); sim.vz = malloc(b);
	sim.om = malloc(b); sim.ux = malloc(b); sim.uz = malloc(b);
	sim.act = malloc((size_t)n * sizeof(int64_t));
	for (int i = 0; i < cols; i++)
		for (int j = 0; j < cols; j++) {
			int p = i * cols + j;
			sim.x[p] = -extent + i * sim.spacing;
			sim.z[p] = -extent + j * sim.spacing;
			sim.y[p] = 0.0;
			sim.vx[p] = 0.0; sim.vy[p] = 0.0; sim.vz[p] = 0.0;
			sim.om[p] = 0.0;
			sim.ux[p] = 0.0; sim.uz[p] = 0.0;
		}
	sim.ready = 1;
}

// lattice cell (i, j) -> particle index, through the scroll offsets
static int sim_wrap(int v, int n)
{
	v %= n;
	return v < 0 ? v + n : v;
}

static int sim_idx(int i, int j)
{
	return sim_wrap(i + sim.ioff, sim.cols) * sim.cols
	     + sim_wrap(j + sim.joff, sim.cols);
}

// still water at the lattice cell's rest position - what a particle becomes
// when it is recycled from the trailing edge of the window to the leading one
static void sim_seed_cell(int i, int j)
{
	int p = sim_idx(i, j);
	sim.x[p] = sim.ox - sim.extent + i * sim.spacing;
	sim.z[p] = sim.oz - sim.extent + j * sim.spacing;
	sim.y[p] = 0.0;
	sim.vx[p] = 0.0; sim.vy[p] = 0.0; sim.vz[p] = 0.0;
	sim.om[p] = 0.0;
	sim.ux[p] = 0.0; sim.uz[p] = 0.0;
}

// vsea_sim_recenter(cx, cz): slide the window so it is centred on (cx, cz),
// snapped to whole tiles. Water already in the window keeps its state and
// its place in the world; only the strip that has just come into view is
// seeded fresh.
void vsea_sim_recenter(double cx, double cz)
{
	if (!sim.ready)
		return;
	int cols = sim.cols;
	int di = (int)llround((cx - sim.ox) / sim.spacing);
	int dj = (int)llround((cz - sim.oz) / sim.spacing);
	if (di == 0 && dj == 0)
		return;
	sim.ox += di * sim.spacing;
	sim.oz += dj * sim.spacing;
	if (di <= -cols || di >= cols || dj <= -cols || dj >= cols) {
		// jumped clear of the old window: none of it is worth keeping
		sim.ioff = 0;
		sim.joff = 0;
		for (int i = 0; i < cols; i++)
			for (int j = 0; j < cols; j++)
				sim_seed_cell(i, j);
		return;
	}
	sim.ioff = sim_wrap(sim.ioff + di, cols);
	sim.joff = sim_wrap(sim.joff + dj, cols);
	if (di > 0)
		for (int i = cols - di; i < cols; i++)
			for (int j = 0; j < cols; j++)
				sim_seed_cell(i, j);
	else if (di < 0)
		for (int i = 0; i < -di; i++)
			for (int j = 0; j < cols; j++)
				sim_seed_cell(i, j);
	if (dj > 0)
		for (int j = cols - dj; j < cols; j++)
			for (int i = 0; i < cols; i++)
				sim_seed_cell(i, j);
	else if (dj < 0)
		for (int j = 0; j < -dj; j++)
			for (int i = 0; i < cols; i++)
				sim_seed_cell(i, j);
}

void vsea_sim_origin(double *out)
{
	out[0] = sim.ready ? sim.ox : 0.0;
	out[1] = sim.ready ? sim.oz : 0.0;
}

int64_t vsea_sim_count(void) { return sim.ready ? sim.n : 0; }
int64_t vsea_sim_cols(void) { return sim.ready ? sim.cols : 0; }
double vsea_sim_spacing(void) { return sim.ready ? sim.spacing : 0.0; }
double vsea_sim_extent(void) { return sim.ready ? sim.extent : 0.0; }
double vsea_sim_time(void) { return sim.ready ? sim.t : 0.0; }

// vsea_sim_load(...): overwrite the whole particle state. Only the tests use
// this, to put the C sim and the pure reference on identical footing.
void vsea_sim_load(const double *x, const double *z, const double *y,
                   const double *vx, const double *vy, const double *vz,
                   const double *om, int64_t n, double t)
{
	if (!sim.ready || n != sim.n)
		return;
	for (int i = 0; i < sim.n; i++) {
		sim.x[i] = x[i]; sim.z[i] = z[i]; sim.y[i] = y[i];
		sim.vx[i] = vx[i]; sim.vy[i] = vy[i]; sim.vz[i] = vz[i];
		sim.om[i] = om[i];
	}
	sim.t = t;
}

void vsea_sim_read(double *x, double *z, double *y,
                   double *vx, double *vy, double *vz, double *om)
{
	if (!sim.ready)
		return;
	for (int i = 0; i < sim.n; i++) {
		x[i] = sim.x[i]; z[i] = sim.z[i]; y[i] = sim.y[i];
		vx[i] = sim.vx[i]; vy[i] = sim.vy[i]; vz[i] = sim.vz[i];
		om[i] = sim.om[i];
	}
}

// vsea_sim_height(x, z): the water surface height, bilinear over the
// particle lattice. Indexed by the rest lattice - particles advect at most
// half a tile, so it is still the right cell - and clamped at the edges, so
// a hull that sails past the sheet reads the nearest water rather than
// falling through a hole.
double vsea_sim_height(double x, double z)
{
	if (!sim.ready)
		return 0.0;
	int cols = sim.cols;
	double top = (double)(cols - 1) - 1e-9;
	double fi = (x - (sim.ox - sim.extent)) / sim.spacing;
	double fj = (z - (sim.oz - sim.extent)) / sim.spacing;
	if (fi < 0.0) fi = 0.0; else if (fi > top) fi = top;
	if (fj < 0.0) fj = 0.0; else if (fj > top) fj = top;
	int i = (int)fi, j = (int)fj;
	double u = fi - i, v = fj - j;
	const double *y = sim.y;
	return (1.0 - u) * (1.0 - v) * y[sim_idx(i, j)]
	     + u * (1.0 - v) * y[sim_idx(i + 1, j)]
	     + (1.0 - u) * v * y[sim_idx(i, j + 1)]
	     + u * v * y[sim_idx(i + 1, j + 1)];
}

double vsea_sim_circulation(void)
{
	double s = 0.0;
	for (int i = 0; i < sim.n; i++)
		s += sim.om[i];
	return s;
}

// The velocity field at whatever this sea state makes cheapest, mirroring
// voxel.ocean/velocities: a dead calm costs nothing, a scattering of live
// vortices is summed exactly out to SIM_FIELD_RADIUS, and a fully churned
// sea goes to the FMM so the cost stays linear in the particle count.
static void sim_velocities(void)
{
	int na = 0;
	for (int i = 0; i < sim.n; i++)
		if (fabs(sim.om[i]) > SIM_ACTIVE_EPS)
			sim.act[na++] = i;
	if (na == 0) {
		memset(sim.ux, 0, (size_t)sim.n * sizeof(double));
		memset(sim.uz, 0, (size_t)sim.n * sizeof(double));
	} else if (na <= sim.sparse_max) {
		vsea_field(sim.x, sim.z, sim.om, sim.act, na,
		           SIM_FIELD_RADIUS * SIM_FIELD_RADIUS, SIM_ACTIVE_EPS,
		           sim.ux, sim.uz, sim.n);
	} else {
		vsea_fmm(sim.x, sim.z, sim.om, sim.n, FMM_P, sim.bounds,
		         sim.ox, sim.oz, sim.ux, sim.uz);
	}
}

// vsea_sim_step(dt, blasts, nb, hulls, nh): one ocean step.
// blasts pack (x, z, r, power); hulls pack
// (x, z, r, hull_r, push, lift, swirl, displace, hx, hz).
// See voxel.ocean/apply-hulls for what each hull term means - this is the
// same arithmetic in the same order.
// The order matches voxel.ocean/step-ocean exactly: couplings, ambient
// forcing, the field solve, then advection and the vertical oscillator.
void vsea_sim_step(double dt, const double *blasts, int64_t nb,
                   const double *hulls, int64_t nh)
{
	if (!sim.ready)
		return;
	int n = sim.n;

	for (int64_t b = 0; b < nb; b++) {
		double bx = blasts[b * 4], bz = blasts[b * 4 + 1];
		double br = blasts[b * 4 + 2], bp = blasts[b * 4 + 3];
		for (int i = 0; i < n; i++) {
			double dx = sim.x[i] - bx, dz = sim.z[i] - bz;
			double d = sqrt(dx * dx + dz * dz);
			if (d >= br)
				continue;
			double w = 1.0 - d / br;
			sim.vy[i] += bp * w;
			sim.om[i] += bp * 0.8 * w;
		}
	}
	for (int64_t hh = 0; hh < nh; hh++) {
		const double *h = hulls + hh * 10;
		double cx = h[0], cz = h[1], hr = h[2];
		double r0 = h[3] > 0.0 ? h[3] : hr / 3.0;
		double push = h[4], lift = h[5], swirl = h[6], displace = h[7];
		double bx = h[8], bz = h[9];   // heading: signs wake and bow wave
		double r02 = r0 * r0;
		for (int i = 0; i < n; i++) {
			double dx = sim.x[i] - cx, dz = sim.z[i] - cz;
			double d2 = dx * dx + dz * dz;
			double d = sqrt(d2);
			if (d >= hr)
				continue;
			double taper = 1.0 - d / hr;
			double near = r02 / (d2 + r02);
			double dd = d < 1e-6 ? 1e-6 : d;
			double nx = dx / dd, nz = dz / dd;
			double hn = bx * nx + bz * nz;
			double s2 = d2 / r02;
			double prof = (s2 - 1.0) * exp(-s2);
			double flow = push * near * taper;
			sim.vx[i] += flow * (2.0 * hn * nx - bx) * dt;
			sim.vz[i] += flow * (2.0 * hn * nz - bz) * dt;
			sim.vy[i] += (displace * prof + lift * near * taper * hn) * dt;
			sim.om[i] += swirl * near * taper * (bx * nz - bz * nx) * dt;
		}
	}
	if (sim.ambient) {
		int64_t kb = (int64_t)(sim.t / SIM_CHOP_RATE);
		for (int i = 0; i < n; i++) {
			double lift = 0.0;
			for (int w = 0; w < SIM_SWELL_TRAINS; w++) {
				const double *tr = sim_swell[w];
				lift += tr[0] * dt
				      * sin(tr[1] * (tr[3] * sim.x[i] + tr[4] * sim.z[i])
				            - tr[2] * sim.t);
			}
			sim.vy[i] += lift;
			sim.om[i] += SIM_CHOP_AMP * dt
			           * sim_chop_rand(sim.x[i], sim.z[i], kb);
		}
	}

	sim_velocities();

	double drag = pow(SIM_DRAG, dt);
	double decay = 1.0 - sim.viscosity * dt;
	// the domain wall travels with the window, well outside the frame
	double bx0 = sim.ox - sim.bounds, bx1 = sim.ox + sim.bounds;
	double bz0 = sim.oz - sim.bounds, bz1 = sim.oz + sim.bounds;
	for (int i = 0; i < n; i++) {
		double fx = sim.ux[i], fz = sim.uz[i];
		// water with nothing happening to it is left exactly alone, so a
		// calm sea costs a comparison per particle and no arithmetic
		if (fx == 0.0 && fz == 0.0 && sim.vx[i] == 0.0 && sim.vz[i] == 0.0
		    && sim.vy[i] == 0.0 && sim.y[i] == 0.0
		    && fabs(sim.om[i]) <= SIM_ACTIVE_EPS)
			continue;
		double vx = sim.vx[i] + fx, vz = sim.vz[i] + fz;
		double x = sim.x[i] + vx * dt, z = sim.z[i] + vz * dt;
		if (x > bx1) { x = bx1; vx = -vx; }
		else if (x < bx0) { x = bx0; vx = -vx; }
		if (z > bz1) { z = bz1; vz = -vz; }
		else if (z < bz0) { z = bz0; vz = -vz; }
		sim.x[i] = x;
		sim.z[i] = z;
		sim.vx[i] = (vx - fx) * drag;
		sim.vz[i] = (vz - fz) * drag;
		sim.om[i] *= decay;
		// vertical: airborne spray falls ballistically, surface water is a
		// damped oscillator about y = 0 that the swell rides into waves
		if (sim.y[i] > SIM_SURFACE_BREAK) {
			double y = sim.y[i] + sim.vy[i] * dt;
			double vy = sim.vy[i] - SIM_SPRAY_G * dt;
			if (y < SIM_SURFACE_BREAK) {
				sim.y[i] = SIM_SURFACE_BREAK;
				sim.vy[i] = 0.1 * vy;
			} else {
				sim.y[i] = y;
				sim.vy[i] = vy;
			}
		} else {
			double y = sim.y[i] + sim.vy[i] * dt;
			if (y < -0.6)
				y = -0.6;
			sim.vy[i] -= dt * (SIM_SURFACE_K * y
			                   + SIM_SURFACE_C * sim.vy[i]);
			sim.y[i] = y;
		}
	}
	sim.t += dt;
}

// ---------------------------------------------------------------- mesh -----
//
// The sheet IS the particle set: one vertex per particle, two triangles per
// lattice tile, heights straight off sim.y. Nothing analytic is mixed in.
//
// It used to add a hardcoded three-term sine to every corner and ring the
// field with flat swell-only tiles out to a horizon, which meant most of the
// wave motion a player saw was a scrolling function rather than the
// simulation, and the water ended in a visible diamond. The sea is now big
// enough to run past the frame (voxel.camera sizes it), so there is nothing
// to ring, and the only height in the mesh is the height the sim computed.
//
// Per-vertex normals come from central differences over lattice neighbours;
// colour is a swell/foam mix times sun diffuse plus specular.

// Foam: water has to be thrown clear of the swell, or genuinely churned,
// before it goes white. Ambient chop sits orders below the swirl threshold;
// a wake or a shell burst saturates it.
#define SEA_FOAM_FREEBOARD 0.30     // height above which water counts as spray
#define SEA_FOAM_SPRAY 1.60         // foam per unit of that
#define SEA_FOAM_SWIRL 0.18         // foam per unit of vorticity
// troughs shade toward the deep colour and crests toward the swell colour
// over this half-range, so the shape of the sea reads as colour and not only
// as the faint diffuse gradient of a low-slope surface
#define SEA_TROUGH 0.35

typedef struct {
	int cols, n;
	double spacing, extent;
	Mesh mesh;
	Material mat;
	int ready;
	int gl;                    // GPU buffers exist (a window is up)
	float *verts, *norms;      // vertexCount x 3
	unsigned char *cols_;      // vertexCount x 4
	uint16_t *idx;             // indexCount
	int vcount, icount;
} SeaMesh;

static SeaMesh sea = {0};

static void sea_release(void)
{
	if (!sea.ready)
		return;
	if (sea.gl) {
		UnloadMesh(sea.mesh);  // frees the CPU arrays too
	} else {
		free(sea.verts); free(sea.norms);
		free(sea.cols_); free(sea.idx);
	}
	memset(&sea, 0, sizeof(sea));
}

// vsea_mesh_init(): build the sheet for the live sim's lattice. Safe to
// call again after the sim is resized.
void vsea_mesh_init(void)
{
	sea_release();
	if (!sim.ready)
		return;
	int cols = sim.cols;
	if (cols * cols > 65535)   // raylib indices are unsigned short
		return;
	sea.cols = cols;
	sea.n = sim.n;
	sea.spacing = sim.spacing;
	sea.extent = sim.extent;
	sea.vcount = cols * cols;
	sea.icount = (cols - 1) * (cols - 1) * 6;
	sea.verts = calloc((size_t)sea.vcount * 3, sizeof(float));
	sea.norms = calloc((size_t)sea.vcount * 3, sizeof(float));
	sea.cols_ = calloc((size_t)sea.vcount, 4);
	sea.idx = malloc((size_t)sea.icount * sizeof(uint16_t));
	int v = 0;
	for (int i = 0; i < cols - 1; i++)
		for (int j = 0; j < cols - 1; j++) {
			int c00 = i * cols + j;
			int c10 = c00 + cols;
			int c01 = c00 + 1;
			int c11 = c10 + 1;
			sea.idx[v++] = (uint16_t)c01;
			sea.idx[v++] = (uint16_t)c11;
			sea.idx[v++] = (uint16_t)c10;
			sea.idx[v++] = (uint16_t)c01;
			sea.idx[v++] = (uint16_t)c10;
			sea.idx[v++] = (uint16_t)c00;
		}
	sea.mesh.vertexCount = sea.vcount;
	sea.mesh.triangleCount = sea.icount / 3;
	sea.mesh.vertices = sea.verts;
	sea.mesh.normals = sea.norms;
	sea.mesh.colors = sea.cols_;
	sea.mesh.indices = sea.idx;
	// headless (tests, tools): fill the CPU arrays, skip every GL call
	sea.gl = IsWindowReady();
	if (sea.gl) {
		UploadMesh(&sea.mesh, false);
		sea.mat = LoadMaterialDefault();
	}
	sea.ready = 1;
}

// vsea_mesh_update(sun[3], half[3], deep[4], swell[4], foam[4]): refill the
// sheet from the live particle state and upload it.
void vsea_mesh_update(const double *sun, const double *half,
                      const unsigned char *deepc,
                      const unsigned char *swellc, const unsigned char *foamc)
{
	if (!sea.ready || !sim.ready || sea.n != sim.n)
		return;
	int cols = sea.cols;
	double step = sea.spacing;
	double two_s = 2.0 * step;
	// particles advect, so a vertex is drawn where its particle actually is
	// - that is the wake and the swirls showing in the surface. Clamped to
	// under half a tile so the sheet can drift but never fold over itself.
	double slack = 0.45 * step;
	for (int i = 0; i < cols; i++)
		for (int j = 0; j < cols; j++) {
			int p = sim_idx(i, j);
			double rx = sim.ox - sea.extent + i * step;
			double rz = sim.oz - sea.extent + j * step;
			double dx = sim.x[p] - rx, dz = sim.z[p] - rz;
			if (dx > slack) dx = slack; else if (dx < -slack) dx = -slack;
			if (dz > slack) dz = slack; else if (dz < -slack) dz = -slack;

			double hm = sim.y[i > 0 ? sim_idx(i - 1, j) : p];
			double hp = sim.y[i < cols - 1 ? sim_idx(i + 1, j) : p];
			double gm = sim.y[j > 0 ? sim_idx(i, j - 1) : p];
			double gp = sim.y[j < cols - 1 ? sim_idx(i, j + 1) : p];
			double gx = (hp - hm) / two_s;
			double gz = (gp - gm) / two_s;
			double l = sqrt(1.0 + gx * gx + gz * gz);
			double nx = -gx / l, ny = 1.0 / l, nz = -gz / l;

			int vi = i * cols + j;
			float *vp = sea.verts + vi * 3;
			vp[0] = (float)(rx + dx);
			vp[1] = (float)(sim.y[p] + 0.05);
			vp[2] = (float)(rz + dz);
			float *np = sea.norms + vi * 3;
			np[0] = (float)nx; np[1] = (float)ny; np[2] = (float)nz;

			double diff = nx * sun[0] + ny * sun[1] + nz * sun[2];
			if (diff < 0.0)
				diff = 0.0;
			double spec = nx * half[0] + ny * half[1] + nz * half[2];
			if (spec < 0.0)
				spec = 0.0;
			spec = pow(spec, 24.0);
			// Scaling foam off raw height instead whitens every ordinary
			// swell crest, which leaves nothing for a real wake to stand
			// out against.
			double lifted = sim.y[p] - SEA_FOAM_FREEBOARD;
			double f = (lifted > 0.0 ? SEA_FOAM_SPRAY * lifted : 0.0)
			         + SEA_FOAM_SWIRL * fabs(sim.om[p]);
			if (f > 1.0)
				f = 1.0;
			// where this water stands, deep trough to breaking crest
			double lvl = (sim.y[p] + SEA_TROUGH) / (2.0 * SEA_TROUGH);
			if (lvl < 0.0)
				lvl = 0.0;
			else if (lvl > 1.0)
				lvl = 1.0;
			double lit = 1.9 * diff + 0.7 * spec;
			unsigned char *c = sea.cols_ + vi * 4;
			// rgb only: scaling alpha by the light makes shaded water
			// translucent and the sky shows through the troughs
			for (int k = 0; k < 3; k++) {
				double water = deepc[k] / 255.0
				    + (swellc[k] / 255.0 - deepc[k] / 255.0) * lvl;
				double base = water + (foamc[k] / 255.0 - water) * f;
				int v8 = (int)(base * lit * 255.0);
				if (v8 < 0)
					v8 = 0;
				if (v8 > 255)
					v8 = 255;
				c[k] = (unsigned char)v8;
			}
			c[3] = (unsigned char)(swellc[3]
			    + (int)((foamc[3] - swellc[3]) * f));
		}

	if (sea.gl) {
		UpdateMeshBuffer(sea.mesh, 0, sea.verts,
		                sea.vcount * 3 * sizeof(float), 0);
		UpdateMeshBuffer(sea.mesh, 2, sea.norms,
		                sea.vcount * 3 * sizeof(float), 0);
		UpdateMeshBuffer(sea.mesh, 3, sea.cols_, sea.vcount * 4, 0);
	}
}

// vsea_mesh_vertex_count(): CPU vertex count, for headless inspection.
int64_t vsea_mesh_vertex_count(void)
{
	return sea.ready ? sea.vcount : 0;
}

// vsea_mesh_read(verts, norms, cols): copy the CPU-side sheet out as
// doubles (positions, normals) and bytes (rgba). Tests read the sheet the
// GPU would have got without needing a window.
void vsea_mesh_read(double *verts, double *norms, unsigned char *cols)
{
	if (!sea.ready)
		return;
	for (int i = 0; i < sea.vcount * 3; i++) {
		verts[i] = sea.verts[i];
		norms[i] = sea.norms[i];
	}
	memcpy(cols, sea.cols_, (size_t)sea.vcount * 4);
}

// vsea_mesh_draw(): one draw call for the whole sheet, identity transform.
void vsea_mesh_draw(void)
{
	if (!sea.ready || !sea.gl)
		return;
	Matrix m = {
		.m0 = 1.0f, .m1 = 0.0f, .m2 = 0.0f, .m3 = 0.0f,
		.m4 = 0.0f, .m5 = 1.0f, .m6 = 0.0f, .m7 = 0.0f,
		.m8 = 0.0f, .m9 = 0.0f, .m10 = 1.0f, .m11 = 0.0f,
		.m12 = 0.0f, .m13 = 0.0f, .m14 = 0.0f, .m15 = 1.0f,
	};
	DrawMesh(sea.mesh, sea.mat, m);
}

void vsea_mesh_free(void)
{
	sea_release();
}


// ---------------------------------------------------------------- ships ----
//
// Warships as static meshes rebuilt only when a hull is damaged. Jolt owns
// the colour per face (material + per-cell tint + sunk shade, computed once
// at build); C owns the per-frame work: rotate each face normal by the
// hull quaternion, cull faces pointing away from the camera, sun-shade,
// and submit the whole hull with one DrawMesh at the physics transform.

#define SEA_MAX_SHIPS 8
#define SEA_MAX_FACES 16384

typedef struct {
	Mesh mesh;
	Material mat;
	int faces;             // face count the mesh was built for
	double voxel;          // world units per cell edge
	int ready;
	int gl;                // GPU buffers exist (a window is up)
	// per-face static data in build order
	signed char (*dir)[3]; // local outward normal
	signed char *dirq;     // dir_tris/dir_norm index, resolved at build
	short (*v0)[3];        // local quad corner, in cells
	unsigned char (*base)[4];
	float *verts, *norms;
	unsigned char *cols_;
	uint16_t *idx;
} ShipMesh;

static ShipMesh ships[SEA_MAX_SHIPS];

// the six unit-quad corner sets, render.clj DIR-TRIS (two triangles)
static const int dir_tris[6][6][3] = {
	{{1,0,0},{1,1,0},{1,1,1},{1,0,0},{1,1,1},{1,0,1}},   // +x
	{{0,0,0},{0,0,1},{0,1,1},{0,0,0},{0,1,1},{0,1,0}},   // -x
	{{0,1,0},{0,1,1},{1,1,1},{0,1,0},{1,1,1},{1,1,0}},   // +y
	{{0,0,0},{1,0,0},{1,0,1},{0,0,0},{1,0,1},{0,0,1}},   // -y
	{{0,0,1},{1,0,1},{1,1,1},{0,0,1},{1,1,1},{0,1,1}},   // +z
	{{0,0,0},{0,1,0},{1,1,0},{0,0,0},{1,1,0},{1,0,0}},   // -z
};
static const int dir_norm[6][3] = {
	{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1},{0,0,-1},
};

// vsea_ship_init(cells, colors, n, voxel): build a hull mesh from its
// exposed faces. cells packs (i, j, k, dir-index) per face, colors the
// packed base colour per face, voxel is how many world units a cell edge
// is. Returns the ship id, or -1 when full.
int64_t vsea_ship_init(const int64_t *cells, const unsigned char *colors,
                       int64_t n, double voxel)
{
	int id = -1;
	for (int s = 0; s < SEA_MAX_SHIPS; s++)
		if (!ships[s].ready) { id = s; break; }
	if (id < 0 || n > SEA_MAX_FACES)
		return -1;
	ShipMesh *sh = &ships[id];
	if (sh->verts) {
		if (sh->gl) {
			UnloadMesh(sh->mesh);  // frees verts/norms/colors/indices
		} else {
			free(sh->verts); free(sh->norms);
			free(sh->cols_); free(sh->idx);
		}
		free(sh->dir); free(sh->dirq); free(sh->v0); free(sh->base);
	}
	memset(sh, 0, sizeof(*sh));
	int vc = (int)n * 6;
	sh->verts = calloc(vc * 3, sizeof(float));
	sh->norms = calloc(vc * 3, sizeof(float));
	sh->cols_ = calloc(vc, 4);
	sh->idx = malloc(vc * sizeof(uint16_t));
	sh->dir = malloc(n * 3);
	sh->dirq = malloc(n);
	sh->v0 = malloc(n * 3 * sizeof(short));
	sh->base = malloc(n * 4);
	sh->faces = (int)n;
	sh->voxel = voxel;
	// static indices: 6 vertices per face in order
	for (int64_t v = 0; v < vc; v++)
		sh->idx[v] = (uint16_t)v;
	sh->mesh.vertexCount = vc;
	sh->mesh.triangleCount = (int)n * 2;
	sh->mesh.vertices = sh->verts;
	sh->mesh.normals = sh->norms;
	sh->mesh.colors = sh->cols_;
	sh->mesh.indices = sh->idx;
	// headless (tests, tools): fill the CPU arrays, skip every GL call
	sh->gl = IsWindowReady();
	if (sh->gl) {
		UploadMesh(&sh->mesh, false);
		sh->mat = LoadMaterialDefault();
	}
	sh->ready = 1;
	// face records + placeholder vertex data (draw fills it per frame)
	for (int64_t f = 0; f < n; f++) {
		int64_t i = cells[f * 4 + 0];
		int64_t j = cells[f * 4 + 1];
		int64_t k = cells[f * 4 + 2];
		int d = (int)cells[f * 4 + 3];
		sh->dir[f][0] = (signed char)dir_norm[d][0];
		sh->dir[f][1] = (signed char)dir_norm[d][1];
		sh->dir[f][2] = (signed char)dir_norm[d][2];
		sh->dirq[f] = (signed char)d;    // no per-frame reverse lookup
		sh->v0[f][0] = (short)i;
		sh->v0[f][1] = (short)j;
		sh->v0[f][2] = (short)k;
		for (int q = 0; q < 4; q++)
			sh->base[f][q] = colors[f * 4 + q];
		(void)dir_tris;  // offsets applied in draw below
	}
	return id;
}

// vsea_ship_draw(id, pos, quat, anchor, sun, cam, ambient, sunk):
// rebuild the per-frame vertex data at the hull's transform and draw.
// quat is [x y z w]; anchor is the spawn anchor the mesh is built around.
void vsea_ship_draw(int64_t id, const double *pos, const double *quat,
                    const double *anchor, const double *sun,
                    const double *cam)
{
	if (id < 0 || id >= SEA_MAX_SHIPS || !ships[id].ready)
		return;
	ShipMesh *sh = &ships[id];
	double qx = quat[0], qy = quat[1], qz = quat[2], qw = quat[3];
	double ax = anchor[0], ay = anchor[1], az = anchor[2];
	double px = pos[0], py = pos[1], pz = pos[2];

	for (int f = 0; f < sh->faces; f++) {
		double nx = sh->dir[f][0], ny = sh->dir[f][1], nz = sh->dir[f][2];
		// rotate the normal by the quaternion
		double tx = 2.0 * (qy * nz - qz * ny);
		double ty = 2.0 * (qz * nx - qx * nz);
		double tz = 2.0 * (qx * ny - qy * nx);
		double wx = tx * qw + (qy * tz - qz * ty);
		double wy = ty * qw + (qz * tx - qx * tz);
		double wz = tz * qw + (qx * ty - qy * tx);
		double rnx = nx + wx, rny = ny + wy, rnz = nz + wz;
		// cull faces pointing away from the camera
		double tc = rnx * (cam[0] - px) + rny * (cam[1] - py)
		            + rnz * (cam[2] - pz);
		int d = sh->dirq[f];
		int base_v = f * 6;
		// A culled face still has six slots in the static index buffer, so
		// it MUST be written every frame: skipping it leaves whatever the
		// buffer held - malloc garbage on the first frame, the pose from
		// whenever the face was last visible after that - and the GPU draws
		// it. Collapse it to a point instead: six identical vertices are
		// two zero-area triangles the rasteriser drops.
		if (tc < -1.0 || d < 0) {
			for (int q = 0; q < 6; q++) {
				float *vp = sh->verts + (base_v + q) * 3;
				vp[0] = (float)px; vp[1] = (float)py; vp[2] = (float)pz;
				float *np = sh->norms + (base_v + q) * 3;
				np[0] = 0.0f; np[1] = 1.0f; np[2] = 0.0f;
				unsigned char *c = sh->cols_ + (base_v + q) * 4;
				c[0] = c[1] = c[2] = c[3] = 0;
			}
			continue;
		}
		// sun shading, voxel.light/sun-shade
		double dot = rnx * sun[0] + rny * sun[1] + rnz * sun[2];
		if (dot < 0.0)
			dot = 0.0;
		double shade = 0.35 + 0.65 * dot;
		if (shade > 1.0)
			shade = 1.0;
		int ci = sh->v0[f][0], cj = sh->v0[f][1], ck = sh->v0[f][2];
		double vox = sh->voxel;
		for (int q = 0; q < 6; q++) {
			// cell indices are grid coordinates; the voxel size turns them
			// into the ship's actual size, whatever the grid resolution
			double lx = (ci + dir_tris[d][q][0] - ax) * vox;
			double ly = (cj + dir_tris[d][q][1] - ay) * vox;
			double lz = (ck + dir_tris[d][q][2] - az) * vox;
			// rotate the local corner, then translate
			double vtx = 2.0 * (qy * lz - qz * ly);
			double vty = 2.0 * (qz * lx - qx * lz);
			double vtz = 2.0 * (qx * ly - qy * lx);
			double rx = vtx * qw + (qy * vtz - qz * vty);
			double ry = vty * qw + (qz * vtx - qx * vtz);
			double rz = vtz * qw + (qx * vty - qy * vtx);
			float *vp = sh->verts + (base_v + q) * 3;
			vp[0] = (float)(px + lx + rx);
			vp[1] = (float)(py + ly + ry);
			vp[2] = (float)(pz + lz + rz);
			float *np = sh->norms + (base_v + q) * 3;
			np[0] = (float)rnx;
			np[1] = (float)rny;
			np[2] = (float)rnz;
			unsigned char *c = sh->cols_ + (base_v + q) * 4;
			for (int w = 0; w < 3; w++) {
				int v8 = (int)(sh->base[f][w] * shade);
				c[w] = (unsigned char)(v8 > 255 ? 255 : v8);
			}
			c[3] = sh->base[f][3];   // shading is rgb, never alpha
		}
	}

	if (!sh->gl)
		return;
	UpdateMeshBuffer(sh->mesh, 0, sh->verts,
	                 sh->faces * 6 * 3 * sizeof(float), 0);
	UpdateMeshBuffer(sh->mesh, 2, sh->norms,
	                 sh->faces * 6 * 3 * sizeof(float), 0);
	UpdateMeshBuffer(sh->mesh, 3, sh->cols_, sh->faces * 6 * 4, 0);
	Matrix m = {
		.m0 = 1.0f, .m1 = 0.0f, .m2 = 0.0f, .m3 = 0.0f,
		.m4 = 0.0f, .m5 = 1.0f, .m6 = 0.0f, .m7 = 0.0f,
		.m8 = 0.0f, .m9 = 0.0f, .m10 = 1.0f, .m11 = 0.0f,
		.m12 = 0.0f, .m13 = 0.0f, .m14 = 0.0f, .m15 = 1.0f,
	};
	DrawMesh(sh->mesh, sh->mat, m);
}

// vsea_ship_free(id): release one hull's mesh.
void vsea_ship_free(int64_t id)
{
	if (id < 0 || id >= SEA_MAX_SHIPS || !ships[id].ready)
		return;
	ShipMesh *sh = &ships[id];
	if (sh->gl) {
		UnloadMesh(sh->mesh);  // frees verts/norms/colors/indices
	} else {
		free(sh->verts); free(sh->norms);
		free(sh->cols_); free(sh->idx);
	}
	free(sh->dir); free(sh->dirq); free(sh->v0); free(sh->base);
	memset(sh, 0, sizeof(*sh));
}

// vsea_ship_vertex_count(id): CPU vertex count (6 per face).
int64_t vsea_ship_vertex_count(int64_t id)
{
	if (id < 0 || id >= SEA_MAX_SHIPS || !ships[id].ready)
		return 0;
	return ships[id].faces * 6;
}

// vsea_ship_read(id, verts, norms, cols): copy the CPU-side hull out as
// doubles (positions, normals) and bytes (rgba), so tests can inspect what
// the GPU would have been handed without opening a window.
void vsea_ship_read(int64_t id, double *verts, double *norms,
                    unsigned char *cols)
{
	if (id < 0 || id >= SEA_MAX_SHIPS || !ships[id].ready)
		return;
	ShipMesh *sh = &ships[id];
	for (int i = 0; i < sh->faces * 6 * 3; i++) {
		verts[i] = sh->verts[i];
		norms[i] = sh->norms[i];
	}
	memcpy(cols, sh->cols_, sh->faces * 6 * 4);
}
