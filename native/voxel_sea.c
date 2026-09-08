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

#define FMM_MAXN 8192
#define FMM_P 10
#define FMM_LEAF 12
#define FMM_DEPTH 6
#define FMM_MAXNODES (2 * FMM_MAXN + 64)
#define FMM_MAXNBR (FMM_MAXN * 32)

typedef struct {
	double cx, cz, h;
	int depth, parent, leaf;
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
static int fnbr[FMM_MAXNBR];
static int fnbr_off[FMM_MAXNODES + 1];
static int fnbr_all[FMM_MAXNODES];  // leaf flag: near field = everyone
static double fbinom[2 * FMM_P][2 * FMM_P];
static int fmm_ready = 0;

static const double *fmm_xs, *fmm_zs, *fmm_om;
static double fmm_bounds;

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

static double fmm_clamp(double v)
{
	double b = fmm_bounds;
	return v < -b ? -b : (v > b ? b : v);
}

static int fmm_split(int parent, double cx, double cz, double h,
                     int depth, int begin, int end)
{
	int id = fnodes_n++;
	FmmNode *nd = &fnodes[id];
	nd->cx = cx; nd->cz = cz; nd->h = h;
	nd->depth = depth; nd->parent = parent;
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
		double x = fmm_clamp(fmm_xs[j]);
		double z = fmm_clamp(fmm_zs[j]);
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
		nd->kid[q] = fmm_split(id, cx + sx, cz + sz, h2,
		                       depth + 1, start[q], start[q] + cnt[q]);
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

static double fmm_cheb(const FmmNode *a, const FmmNode *b)
{
	double dx = fabs(a->cx - b->cx), dz = fabs(a->cz - b->cz);
	return dx > dz ? dx : dz;
}

static const FmmNode *fmm_ancestor(const FmmNode *nd, int lvl)
{
	while (nd->depth > lvl)
		nd = &fnodes[nd->parent];
	return nd;
}

void vsea_fmm(const double *xs, const double *zs, const double *om,
              int64_t n, int64_t p64, double bounds,
              double *vx, double *vz)
{
	int nn = (int)n;
	(void)p64;  // expansion order fixed at FMM_P, matching the pure code
	if (nn <= 0)
		return;
	fmm_tables();
	fmm_xs = xs; fmm_zs = zs; fmm_om = om; fmm_bounds = bounds;
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
	for (int i = 0; i < nn; i++)
		forder[i] = i;
	fmm_split(-1, 0.0, 0.0, bounds, 0, 0, nn);
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
			// interaction list: same level, separated by more than 2
			// cell widths, whose PARENTS touch (or are both at the root)
			for (int sj = 0; sj < flevel_n[d]; sj++) {
				int sid = flevels[d][sj];
				if (sid == id)
					continue;
				FmmNode *src = &fnodes[sid];
				if (fmm_cheb(src, nd) / nd->h <= 2.0)
					continue;
				if (nd->depth > 1) {
					const FmmNode *np = &fnodes[nd->parent];
					const FmmNode *sp = &fnodes[src->parent];
					if (fmm_cheb(np, sp) / np->h > 2.0)
						continue;
				}
				fmm_m2l(id, sid);
			}
		}
	}
	// --- near-field neighbour lists per leaf (cells compared at the
	// coarser of the two levels, exactly like the pure adjacency)
	int nbr_n = 0;
	for (int id = 0; id < fnodes_n; id++) {
		fnbr_off[id] = nbr_n;
		fnbr_all[id] = 0;
		if (!fnodes[id].leaf)
			continue;
		const FmmNode *a = &fnodes[id];
		for (int oid = 0; oid < fnodes_n; oid++) {
			if (!fnodes[oid].leaf || oid == id)
				continue;  // NOTE: pure includes the leaf itself via
				           // adjacency-to-self; handled below
			const FmmNode *b = &fnodes[oid];
			int lvl = a->depth < b->depth ? a->depth : b->depth;
			const FmmNode *ca = fmm_ancestor(a, lvl);
			const FmmNode *cb = fmm_ancestor(b, lvl);
			double sep = fmm_cheb(ca, cb);
			if (sep > 2.0 * ca->h + 1e-12)
				continue;
			for (int i = b->begin; i < b->end; i++) {
				if (nbr_n >= FMM_MAXNBR) {
					fnbr_all[id] = 1;
					break;
				}
				fnbr[nbr_n++] = forder[i];
			}
			if (fnbr_all[id])
				break;
		}
		// the leaf's own particles are its nearest neighbours
		for (int i = a->begin; i < a->end; i++) {
			if (nbr_n >= FMM_MAXNBR) {
				fnbr_all[id] = 1;
				break;
			}
			fnbr[nbr_n++] = forder[i];
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

// ---------------------------------------------------------------- mesh -----
//
// The sheet is drawn as two blocks of shared-corner quads:
//   - the field: the particle lattice (particles sit on a regular grid,
//     one tile per particle), heights = spray + ambient swell;
//   - the ring: the same swell continuing past the field to the horizon,
//     coarser tiles, no foam.
// Per-corner normals come from central differences over corner heights;
// color = swell/foam mix x sun diffuse + specular toward the camera.
// Everything mirrors the pure voxel.light math so the look matches.

#define SEA_MAX_FIELD 8192          // particle-lattice capacity
#define SEA_RING_STEP 6.0           // ring tile size, world units
#define SEA_HORIZON 66.0            // ring extent
#define SEA_RING_INNER 48.0         // ring starts here (under the field)

typedef struct {
	int cols, rows;             // particle lattice dimensions
	double step;               // particle spacing, world units
	double origin;             // lattice origin (square: [-origin, origin])
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

static double swell_h(double x, double z, double t)
{
	return 0.10 * sin(0.45 * x + 0.9 * t)
	     + 0.08 * sin(0.4 * z - 0.7 * t)
	     + 0.06 * sin(0.28 * (x + z) + 0.5 * t);
}

// height of lattice corner (ci, cj): mean of the adjacent particle sprays
// plus swell at the corner position. Missing neighbours fall back to the
// corner's own swell so the edge of the field stays water.
static double corner_h(const double *spray, int cols, int rows,
                        double step, double origin, int ci, int cj, double t)
{
	int i0 = ci - 1, j0 = cj - 1, cnt = 0;
	double sum = 0.0;
	for (int i = i0; i <= i0 + 1 && i < cols; i++) {
		if (i < 0)
			continue;
		for (int j = j0; j <= j0 + 1 && j < rows; j++) {
			if (j < 0)
				continue;
			sum += spray[i * rows + j];
			cnt++;
		}
	}
	double x = -origin + ci * step;
	double z = -origin + cj * step;
	double sw = swell_h(x, z, t);
	return (cnt ? sum / cnt : 0.0) + sw;
}

static double corner_foam(const double *spray, const double *om,
                          int cols, int rows, int ci, int cj)
{
	int i0 = ci - 1, j0 = cj - 1, cnt = 0;
	double sum = 0.0;
	for (int i = i0; i <= i0 + 1 && i < cols; i++) {
		if (i < 0)
			continue;
		for (int j = j0; j <= j0 + 1 && j < rows; j++) {
			if (j < 0)
				continue;
			int p = i * rows + j;
			double f = 0.8 * spray[p] + 0.3 * fabs(om[p]);
			sum += f > 1.0 ? 1.0 : f;
			cnt++;
		}
	}
	return cnt ? sum / cnt : 0.0;
}

// vsea_mesh_init(cols, rows, step, origin): (re)build the mesh for a
// cols x rows particle lattice. Safe to call again on resize.
void vsea_mesh_init(int cols, int rows, double step, double origin)
{
	if (sea.ready) {
		if (sea.gl) {
			UnloadMesh(sea.mesh);  // frees the CPU arrays too
		} else {
			free(sea.verts); free(sea.norms);
			free(sea.cols_); free(sea.idx);
		}
	}
	memset(&sea, 0, sizeof(sea));
	sea.cols = cols;
	sea.rows = rows;
	sea.step = step;
	sea.origin = origin;

	// corners: (cols+1) x (rows+1) for the field
	int fc = (cols + 1) * (rows + 1);
	// ring tiles on a SEA_RING_STEP lattice within the horizon, outside the
	// inner square; corners only where a tile touches
	int ring_tiles = 0;
	int rmin = (int)floor(-SEA_HORIZON / SEA_RING_STEP);
	int rmax = (int)ceil(SEA_HORIZON / SEA_RING_STEP);
	for (int i = rmin; i < rmax; i++)
		for (int j = rmin; j < rmax; j++) {
			double x0 = i * SEA_RING_STEP, z0 = j * SEA_RING_STEP;
			if (fabs(x0) >= SEA_RING_INNER || fabs(z0) >= SEA_RING_INNER)
				if (fabs(x0 + SEA_RING_STEP) > SEA_RING_INNER
				    || fabs(z0 + SEA_RING_STEP) > SEA_RING_INNER)
					ring_tiles++;
		}

	// ring tiles append 6 verts each in update (2 per tri, shared quad not
	// reused) — allocation must match that write or update runs off the end
	sea.vcount = fc + ring_tiles * 6;
	sea.icount = cols * rows * 6 + ring_tiles * 6;
	sea.verts = malloc(sea.vcount * 3 * sizeof(float));
	sea.norms = malloc(sea.vcount * 3 * sizeof(float));
	sea.cols_ = malloc(sea.vcount * 4);
	sea.idx = malloc(sea.icount * sizeof(uint16_t));

	// static index buffer for the field: two triangles per particle tile
	int v = 0;
	for (int i = 0; i < cols; i++)
		for (int j = 0; j < rows; j++) {
			int c00 = i * (rows + 1) + j;
			int c10 = c00 + (rows + 1);
			int c01 = c00 + 1;
			int c11 = c10 + 1;
			sea.idx[v++] = (uint16_t)c01; sea.idx[v++] = (uint16_t)c11; sea.idx[v++] = (uint16_t)c10;
			sea.idx[v++] = (uint16_t)c01; sea.idx[v++] = (uint16_t)c10; sea.idx[v++] = (uint16_t)c00;
		}
	// ring indices appended by update (vertex order depends on tiles)
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

// vsea_mesh_update(spray, xs, zs, om, n, t, sun[3], half[3],
//                  deep[4], swell[4], foamc[4]):
// refill the mesh for frame time t from the per-particle spray heights and
// vorticities (foam), then upload. Colors mirror voxel.light's shading.
// Field corners ride the mean (x,z) of their adjacent particles (clamped
// near the lattice) so wake advection and swirls visibly drag the sheet.
void vsea_mesh_update(const double *spray, const double *xs,
                      const double *zs, const double *om, int64_t n,
                      double t,
                      const double *sun, const double *half,
                      const unsigned char *deep,
                      const unsigned char *swellc,
                      const unsigned char *foamc)
{
	(void)n;
	(void)deep;
	int cols = sea.cols, rows = sea.rows;
	double step = sea.step, origin = sea.origin;

	// per-corner heights/foam for the field
	int W = rows + 1;
	double *hs = malloc((cols + 1) * W * sizeof(double));
	double *fs = malloc((cols + 1) * W * sizeof(double));
	for (int ci = 0; ci <= cols; ci++)
		for (int cj = 0; cj <= rows; cj++) {
			hs[ci * W + cj] = corner_h(spray, cols, rows, step, origin,
			                           ci, cj, t);
			fs[ci * W + cj] = corner_foam(spray, om, cols, rows, ci, cj);
		}

	// fill field vertices: position, normal (central differences), color
	double two_s = 2.0 * step;
	for (int ci = 0; ci <= cols; ci++)
		for (int cj = 0; cj <= rows; cj++) {
			int vi = ci * W + cj;
			double x = -origin + ci * step;
			double z = -origin + cj * step;
			{
				double mx = 0.0, mz = 0.0;
				int pcnt = 0, i0 = ci - 1, j0 = cj - 1;
				for (int i = i0; i <= i0 + 1 && i < cols; i++) {
					if (i < 0)
						continue;
					for (int j = j0; j <= j0 + 1 && j < rows; j++) {
						if (j < 0)
							continue;
						int p2 = i * rows + j;
						mx += xs[p2];
						mz += zs[p2];
						pcnt++;
					}
				}
				if (pcnt) {
					mx /= pcnt;
					mz /= pcnt;
					double ddx = mx - x, ddz = mz - z, cl = 0.9;
					if (ddx > cl)
						ddx = cl;
					else if (ddx < -cl)
						ddx = -cl;
					if (ddz > cl)
						ddz = cl;
					else if (ddz < -cl)
						ddz = -cl;
					x += ddx;
					z += ddz;
				}
			}
			double hm = ci > 0 ? hs[(ci - 1) * W + cj] : hs[vi];
			double hp = ci < cols ? hs[(ci + 1) * W + cj] : hs[vi];
			double gm = cj > 0 ? hs[ci * W + cj - 1] : hs[vi];
			double gp = cj < rows ? hs[ci * W + cj + 1] : hs[vi];
			double gx = (hp - hm) / two_s;
			double gz = (gp - gm) / two_s;
			double l = sqrt(1.0 + gx * gx + gz * gz);
			double nx = -gx / l, ny = 1.0 / l, nz = -gz / l;
			float *vp = sea.verts + vi * 3;
			vp[0] = (float)x;
			vp[1] = (float)(hs[vi] + 0.05);
			vp[2] = (float)z;
			float *np = sea.norms + vi * 3;
			np[0] = (float)nx; np[1] = (float)ny; np[2] = (float)nz;

			double diff = nx * sun[0] + ny * sun[1] + nz * sun[2];
			if (diff < 0.0)
				diff = 0.0;
			double spec = nx * half[0] + ny * half[1] + nz * half[2];
			if (spec < 0.0)
				spec = 0.0;
			spec = pow(spec, 24.0);
			double f = fs[vi];
			if (f > 1.0)
				f = 1.0;
			double lit = 1.9 * diff + 0.7 * spec;
			unsigned char *c = sea.cols_ + vi * 4;
			// rgb only: scaling alpha by the light makes shaded water
			// translucent and the sky shows through the troughs
			for (int k = 0; k < 3; k++) {
				double base = swellc[k] / 255.0
				    + (foamc[k] / 255.0 - swellc[k] / 255.0) * f;
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

	// ring: swell-only tiles beyond the field, slightly lower to hide the
	// seam under the field edge
	int vc = (cols + 1) * W;
	int ic = cols * rows * 6;
	int rmin = (int)floor(-SEA_HORIZON / SEA_RING_STEP);
	int rmax = (int)ceil(SEA_HORIZON / SEA_RING_STEP);
	for (int i = rmin; i < rmax; i++)
		for (int j = rmin; j < rmax; j++) {
			double x0 = i * SEA_RING_STEP, z0 = j * SEA_RING_STEP;
			if (!(fabs(x0) >= SEA_RING_INNER || fabs(z0) >= SEA_RING_INNER))
				continue;
			if (!(fabs(x0 + SEA_RING_STEP) > SEA_RING_INNER
			      || fabs(z0 + SEA_RING_STEP) > SEA_RING_INNER))
				continue;
			double xs[4] = { x0, x0 + SEA_RING_STEP, x0, x0 + SEA_RING_STEP };
			double zs[4] = { z0, z0, z0 + SEA_RING_STEP, z0 + SEA_RING_STEP };
			// v0 v1 v2 v3 -> tris (v2 v3 v1) (v2 v1 v0), flat swell quad
			int order[6] = { 2, 3, 1, 2, 1, 0 };
			for (int k = 0; k < 6; k++) {
				int p = order[k];
				int vi = vc++;
				double x = xs[p], z = zs[p];
				double h = swell_h(x, z, t) - 0.02;
				// ring normal: swell gradient by central differences
				double gx = (swell_h(x + SEA_RING_STEP, z, t)
				             - swell_h(x - SEA_RING_STEP, z, t))
				            / (2.0 * SEA_RING_STEP);
				double gz = (swell_h(x, z + SEA_RING_STEP, t)
				             - swell_h(x, z - SEA_RING_STEP, t))
				            / (2.0 * SEA_RING_STEP);
				double l = sqrt(1.0 + gx * gx + gz * gz);
				double nx = -gx / l, ny = 1.0 / l, nz = -gz / l;
				sea.verts[vi * 3] = (float)x;
				sea.verts[vi * 3 + 1] = (float)(h + 0.05);
				sea.verts[vi * 3 + 2] = (float)z;
				sea.norms[vi * 3] = (float)nx;
				sea.norms[vi * 3 + 1] = (float)ny;
				sea.norms[vi * 3 + 2] = (float)nz;
				double diff = nx * sun[0] + ny * sun[1] + nz * sun[2];
				if (diff < 0.0)
					diff = 0.0;
				double lit = 1.9 * diff;
				unsigned char *c = sea.cols_ + vi * 4;
				for (int q = 0; q < 3; q++) {
					int v8 = (int)(swellc[q] * lit);
					if (v8 < 0)
						v8 = 0;
					if (v8 > 255)
						v8 = 255;
					c[q] = (unsigned char)v8;
				}
				c[3] = swellc[3];
				sea.idx[ic++] = (uint16_t)vi;
			}
		}

	if (sea.gl) {
		UpdateMeshBuffer(sea.mesh, 0, sea.verts,
		                sea.vcount * 3 * sizeof(float), 0);
		UpdateMeshBuffer(sea.mesh, 2, sea.norms,
		                sea.vcount * 3 * sizeof(float), 0);
		UpdateMeshBuffer(sea.mesh, 3, sea.cols_,
		                sea.vcount * 4, 0);
		UpdateMeshBuffer(sea.mesh, 6, sea.idx,
		                sea.icount * sizeof(uint16_t), 0);
	}
	free(hs);
	free(fs);
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
	memcpy(cols, sea.cols_, sea.vcount * 4);
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
	int ready;
	int gl;                // GPU buffers exist (a window is up)
	// per-face static data in build order
	signed char (*dir)[3]; // local outward normal
	signed char *dirq;     // dir_tris/dir_norm index, resolved at build
	signed char (*v0)[3];  // local quad corner, anchor-relative
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

// vsea_ship_init(cells, colors, n): build a hull mesh from its exposed
// faces. cells packs (i, j, k, dir-index) per face, colors the packed
// base colour per face. Returns the ship id, or -1 when full.
int64_t vsea_ship_init(const int64_t *cells, const unsigned char *colors,
                       int64_t n)
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
	sh->v0 = malloc(n * 3);
	sh->base = malloc(n * 4);
	sh->faces = (int)n;
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
		sh->v0[f][0] = (signed char)i;   // anchor subtracted by caller
		sh->v0[f][1] = (signed char)j;
		sh->v0[f][2] = (signed char)k;
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
		int64_t ci = sh->v0[f][0], cj = sh->v0[f][1], ck = sh->v0[f][2];
		for (int q = 0; q < 6; q++) {
			double lx = ci + dir_tris[d][q][0] - ax;
			double ly = cj + dir_tris[d][q][1] - ay;
			double lz = ck + dir_tris[d][q][2] - az;
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
