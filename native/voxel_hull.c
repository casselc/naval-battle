// SPDX-License-Identifier: MIT
// Hull kernels: voxel surface extraction and the divergence-theorem
// floatation solve, called from voxel.hullc.
//
// voxel.buoyancy is the reference for all of this and voxel.buoyancy-test
// holds the two together. The algorithm there is O(triangles) and about
// eleven flops each, but written in Clojure it allocates a handful of
// vectors per triangle, and that - not the arithmetic - is what caps how
// many voxels a ship can be made of. A 5000-triangle hull cost 5.3ms a step
// per ship against a flop count worth about twenty microseconds.
//
// So the hull's mesh lives here. Clojure hands over the cell list whenever
// damage changes it and asks for volume and centre of buoyancy each step;
// nothing per-triangle crosses the boundary.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define HULL_MAX 64
#define HULL_MAX_CELLS 262144

typedef struct {
	int ready;
	int ntris;
	double *tris;          // ntris * 9, anchor-relative, in world units
	int nfaces;
	int32_t *faces;        // nfaces * 4: i, j, k, dir index
	double span[3];        // half-extents about the anchor, world units
	double com[3];         // centre of mass about the anchor, world units
	double voxel;
} Hull;

static Hull hulls[HULL_MAX];

// the six face directions, in the order voxel.buoyancy/DIRS names them
static const int hdir[6][3] = {
	{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1},
};

// --------------------------------------------------------------- cell set --
//
// A hash set over packed cell keys, so "is the neighbour solid?" is O(1)
// without assuming the hull fits any particular bounding box.

static int64_t hkey(int32_t i, int32_t j, int32_t k)
{
	return ((int64_t)(i + 0x40000000) << 42)
	     ^ ((int64_t)(j + 0x40000000) << 21)
	     ^ (int64_t)(k + 0x40000000);
}

static int64_t *hset;
static int32_t hset_cap;

static void hset_build(const int32_t *cells, int64_t n)
{
	int32_t cap = 16;
	while (cap < 2 * n)
		cap <<= 1;
	if (cap > hset_cap) {
		free(hset);
		hset = malloc((size_t)cap * sizeof(int64_t));
		hset_cap = cap;
	}
	for (int32_t i = 0; i < cap; i++)
		hset[i] = INT64_MIN;
	for (int64_t c = 0; c < n; c++) {
		int64_t key = hkey(cells[c * 3], cells[c * 3 + 1], cells[c * 3 + 2]);
		uint64_t h = (uint64_t)key * 0x9E3779B97F4A7C15ull;
		int32_t p = (int32_t)(h >> 40) & (cap - 1);
		while (hset[p] != INT64_MIN && hset[p] != key)
			p = (p + 1) & (cap - 1);
		hset[p] = key;
	}
	hset_cap = cap;
}

static int hset_has(int32_t i, int32_t j, int32_t k)
{
	int64_t key = hkey(i, j, k);
	uint64_t h = (uint64_t)key * 0x9E3779B97F4A7C15ull;
	int32_t p = (int32_t)(h >> 40) & (hset_cap - 1);
	while (hset[p] != INT64_MIN) {
		if (hset[p] == key)
			return 1;
		p = (p + 1) & (hset_cap - 1);
	}
	return 0;
}

// ------------------------------------------------------------- extraction --

// the four corners of each face, matching voxel.mesh/cell-faces winding
static const int hquad[6][4][3] = {
	{{1,0,0},{1,1,0},{1,1,1},{1,0,1}},   // +x
	{{0,0,0},{0,0,1},{0,1,1},{0,1,0}},   // -x
	{{0,1,0},{0,1,1},{1,1,1},{1,1,0}},   // +y
	{{0,0,0},{1,0,0},{1,0,1},{0,0,1}},   // -y
	{{0,0,1},{1,0,1},{1,1,1},{0,1,1}},   // +z
	{{0,0,0},{0,1,0},{1,1,0},{1,0,0}},   // -z
};

// vsea_hull_set(id, cells, n, anchor, voxel, out):
//   cells packs n triples (i, j, k). Rebuilds the hull's exposed faces and
//   its closed surface mesh, anchor-relative and scaled into world units,
//   and reports [ntris, nfaces, span xyz, com xyz] in out (8 doubles).
// Returns the face count, or -1 if the id or the cell count is out of range.
int64_t vsea_hull_set(int64_t id, const int32_t *cells, int64_t n,
                      const double *anchor, double voxel, double *out)
{
	if (id < 0 || id >= HULL_MAX || n < 0 || n > HULL_MAX_CELLS)
		return -1;
	Hull *h = &hulls[id];
	free(h->tris);
	free(h->faces);
	memset(h, 0, sizeof(*h));
	h->voxel = voxel;
	if (n == 0) {
		h->ready = 1;
		for (int q = 0; q < 8; q++)
			out[q] = 0.0;
		return 0;
	}
	hset_build(cells, n);

	// pass one: count exposed faces
	int64_t nf = 0;
	for (int64_t c = 0; c < n; c++) {
		int32_t i = cells[c * 3], j = cells[c * 3 + 1], k = cells[c * 3 + 2];
		for (int d = 0; d < 6; d++)
			if (!hset_has(i + hdir[d][0], j + hdir[d][1], k + hdir[d][2]))
				nf++;
	}
	h->faces = malloc((size_t)nf * 4 * sizeof(int32_t));
	h->tris = malloc((size_t)nf * 2 * 9 * sizeof(double));
	h->nfaces = (int)nf;
	h->ntris = (int)(nf * 2);

	double ax = anchor[0], ay = anchor[1], az = anchor[2];
	double sx = 0.0, sy = 0.0, sz = 0.0;      // span, in voxels
	double cx = 0.0, cy = 0.0, cz = 0.0;      // centre of mass, in voxels
	int64_t fi = 0, ti = 0;
	for (int64_t c = 0; c < n; c++) {
		int32_t i = cells[c * 3], j = cells[c * 3 + 1], k = cells[c * 3 + 2];
		cx += i + 0.5 - ax;
		cy += j + 0.5 - ay;
		cz += k + 0.5 - az;
		double rx = fabs(i + 0.5 - ax), ry = fabs(j + 0.5 - ay);
		double rz = fabs(k + 0.5 - az);
		if (rx > sx) sx = rx;
		if (ry > sy) sy = ry;
		if (rz > sz) sz = rz;
		for (int d = 0; d < 6; d++) {
			if (hset_has(i + hdir[d][0], j + hdir[d][1], k + hdir[d][2]))
				continue;
			h->faces[fi * 4] = i;
			h->faces[fi * 4 + 1] = j;
			h->faces[fi * 4 + 2] = k;
			h->faces[fi * 4 + 3] = d;
			fi++;
			// the quad as two triangles, [a b c] and [a c d]
			double q[4][3];
			for (int v = 0; v < 4; v++) {
				q[v][0] = (i + hquad[d][v][0] - ax) * voxel;
				q[v][1] = (j + hquad[d][v][1] - ay) * voxel;
				q[v][2] = (k + hquad[d][v][2] - az) * voxel;
			}
			const int order[6] = {0, 1, 2, 0, 2, 3};
			for (int v = 0; v < 6; v++) {
				h->tris[ti++] = q[order[v]][0];
				h->tris[ti++] = q[order[v]][1];
				h->tris[ti++] = q[order[v]][2];
			}
		}
	}
	h->span[0] = (sx + 0.5) * voxel;
	h->span[1] = (sy + 0.5) * voxel;
	h->span[2] = (sz + 0.5) * voxel;
	h->com[0] = cx / (double)n * voxel;
	h->com[1] = cy / (double)n * voxel;
	h->com[2] = cz / (double)n * voxel;
	h->ready = 1;

	out[0] = (double)h->ntris;
	out[1] = (double)h->nfaces;
	out[2] = h->span[0]; out[3] = h->span[1]; out[4] = h->span[2];
	out[5] = h->com[0];  out[6] = h->com[1];  out[7] = h->com[2];
	return nf;
}

// vsea_hull_faces(id, out): the exposed faces as nfaces quadruples
// (i, j, k, dir), in the order the mesh was built. -1 for an unknown hull.
int64_t vsea_hull_faces(int64_t id, int32_t *out)
{
	if (id < 0 || id >= HULL_MAX || !hulls[id].ready)
		return -1;
	Hull *h = &hulls[id];
	memcpy(out, h->faces, (size_t)h->nfaces * 4 * sizeof(int32_t));
	return h->nfaces;
}

void vsea_hull_free(int64_t id)
{
	if (id < 0 || id >= HULL_MAX)
		return;
	free(hulls[id].tris);
	free(hulls[id].faces);
	memset(&hulls[id], 0, sizeof(Hull));
}

// ------------------------------------------------------------- floatation --

// Rows of a rotation taking the water plane normal onto +y, so the clipped
// cap lies in that frame's y = 0 plane - identity when the water is level,
// which keeps flat-sea results bit for bit what they were.
static void plane_frame(const double *w, double q[3][3])
{
	if (w[1] > 0.999999) {
		q[0][0] = 1.0; q[0][1] = 0.0; q[0][2] = 0.0;
		q[1][0] = 0.0; q[1][1] = 1.0; q[1][2] = 0.0;
		q[2][0] = 0.0; q[2][1] = 0.0; q[2][2] = 1.0;
		return;
	}
	double a[3];
	if (fabs(w[1]) < 0.9) {
		a[0] = 0.0; a[1] = 1.0; a[2] = 0.0;
	} else {
		a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
	}
	double e[3] = {
		a[1] * w[2] - a[2] * w[1],
		a[2] * w[0] - a[0] * w[2],
		a[0] * w[1] - a[1] * w[0],
	};
	double l = sqrt(e[0] * e[0] + e[1] * e[1] + e[2] * e[2]);
	q[0][0] = e[0] / l; q[0][1] = e[1] / l; q[0][2] = e[2] / l;
	q[1][0] = w[0]; q[1][1] = w[1]; q[1][2] = w[2];
	// rows [e1, n, e1 x n]: orthonormal AND right-handed, so the divergence
	// sums keep their sign
	q[2][0] = q[0][1] * w[2] - q[0][2] * w[1];
	q[2][1] = q[0][2] * w[0] - q[0][0] * w[2];
	q[2][2] = q[0][0] * w[1] - q[0][1] * w[0];
}

static void quat_matrix(const double *q, double m[3][3])
{
	double qx = q[0], qy = q[1], qz = q[2], qw = q[3];
	double xx = qx * qx, yy = qy * qy, zz = qz * qz;
	double xy = qx * qy, xz = qx * qz, yz = qy * qz;
	double wz = qw * qz, wy = qw * qy, wx = qw * qx;
	m[0][0] = 1.0 - 2.0 * (yy + zz); m[0][1] = 2.0 * (xy - wz); m[0][2] = 2.0 * (xz + wy);
	m[1][0] = 2.0 * (xy + wz); m[1][1] = 1.0 - 2.0 * (xx + zz); m[1][2] = 2.0 * (yz - wx);
	m[2][0] = 2.0 * (xz - wy); m[2][1] = 2.0 * (yz + wx); m[2][2] = 1.0 - 2.0 * (xx + yy);
}

// One kept triangle's contribution, in the plane frame where the water is
// y = 0. V = (1/6) SUM (d1 x d2)_x (x0 + x1 + x2), and the same sum on each
// axis with F = <axis^2/2> gives the moments - voxel.buoyancy spells out the
// derivation and voxel.mesh writes it out plainly.
static void hull_accum(const double t[3][3], double *vol6, double *mx,
                       double *my, double *mz)
{
	double d1x = t[1][0] - t[0][0], d1y = t[1][1] - t[0][1], d1z = t[1][2] - t[0][2];
	double d2x = t[2][0] - t[0][0], d2y = t[2][1] - t[0][1], d2z = t[2][2] - t[0][2];
	double wx = d1y * d2z - d1z * d2y;
	double wy = d1z * d2x - d1x * d2z;
	double wz = d1x * d2y - d1y * d2x;
	*vol6 += wx * (t[0][0] + t[1][0] + t[2][0]);
#define HQ(a, b, c) ((a) * (a) * 0.5 + ((b) * (b) + (c) * (c)) / 12.0 \
                     + ((a) * (b) + (a) * (c)) / 3.0 + (b) * (c) / 12.0)
	*mx += wx * 0.5 * HQ(t[0][0], d1x, d2x);
	*my += wy * 0.5 * HQ(t[0][1], d1y, d2y);
	*mz += wz * 0.5 * HQ(t[0][2], d1z, d2z);
#undef HQ
}

// vsea_hull_metrics(id, pos, quat, plane, out):
//   displaced volume and centre of buoyancy of the part of the hull under
//   the water plane [nx ny nz d] (submerged where n.p <= d).
//   out = [volume, cx, cy, cz]; volume 0 means nothing is under.
void vsea_hull_metrics(int64_t id, const double *pos, const double *quat,
                       const double *plane, double *out)
{
	out[0] = 0.0; out[1] = 0.0; out[2] = 0.0; out[3] = 0.0;
	if (id < 0 || id >= HULL_MAX || !hulls[id].ready)
		return;
	Hull *h = &hulls[id];
	if (h->ntris == 0)
		return;

	double r[3][3], qf[3][3], m[3][3];
	quat_matrix(quat, r);
	plane_frame(plane, qf);
	// m = plane_frame . rotation: body -> world -> the frame whose up axis
	// is the water plane normal
	for (int a = 0; a < 3; a++)
		for (int b = 0; b < 3; b++)
			m[a][b] = qf[a][0] * r[0][b] + qf[a][1] * r[1][b] + qf[a][2] * r[2][b];

	// the plane in anchor-relative body coordinates: n.u <= d
	double nx = r[0][0] * plane[0] + r[1][0] * plane[1] + r[2][0] * plane[2];
	double ny = r[0][1] * plane[0] + r[1][1] * plane[1] + r[2][1] * plane[2];
	double nz = r[0][2] * plane[0] + r[1][2] * plane[1] + r[2][2] * plane[2];
	double dy = plane[3] - (plane[0] * pos[0] + plane[1] * pos[1]
	                        + plane[2] * pos[2]);

	double vol6 = 0.0, mx = 0.0, my = 0.0, mz = 0.0;
	const double *tp = h->tris;
	for (int t = 0; t < h->ntris; t++, tp += 9) {
		double s[3];
		for (int v = 0; v < 3; v++)
			s[v] = nx * tp[v * 3] + ny * tp[v * 3 + 1] + nz * tp[v * 3 + 2];
		int under = (s[0] <= dy) + (s[1] <= dy) + (s[2] <= dy);
		if (under == 0)
			continue;
		// Sutherland-Hodgman against the one plane, winding preserved
		double poly[4][3];
		int np = 0;
		if (under == 3) {
			for (int v = 0; v < 3; v++)
				for (int c = 0; c < 3; c++)
					poly[v][c] = tp[v * 3 + c];
			np = 3;
		} else {
			// walk the edges, keeping submerged vertices and crossings
			for (int v = 0; v < 3; v++) {
				int w = (v + 1) % 3;
				int vin = s[v] <= dy, win = s[w] <= dy;
				if (vin) {
					for (int c = 0; c < 3; c++)
						poly[np][c] = tp[v * 3 + c];
					np++;
				}
				if (vin != win) {
					double tt = (dy - s[v]) / (s[w] - s[v]);
					for (int c = 0; c < 3; c++)
						poly[np][c] = tp[v * 3 + c]
						    + tt * (tp[w * 3 + c] - tp[v * 3 + c]);
					np++;
				}
			}
		}
		// into the plane frame, with the water at y = 0
		double f[4][3];
		for (int v = 0; v < np; v++) {
			f[v][0] = m[0][0] * poly[v][0] + m[0][1] * poly[v][1] + m[0][2] * poly[v][2];
			f[v][1] = m[1][0] * poly[v][0] + m[1][1] * poly[v][1] + m[1][2] * poly[v][2] - dy;
			f[v][2] = m[2][0] * poly[v][0] + m[2][1] * poly[v][1] + m[2][2] * poly[v][2];
		}
		for (int v = 2; v < np; v++) {
			double tri[3][3];
			memcpy(tri[0], f[0], sizeof(tri[0]));
			memcpy(tri[1], f[v - 1], sizeof(tri[0]));
			memcpy(tri[2], f[v], sizeof(tri[0]));
			hull_accum(tri, &vol6, &mx, &my, &mz);
		}
	}
	double v = vol6 / 6.0;
	if (fabs(v) < 1e-9)
		return;
	double c[3] = {mx / v, my / v + dy, mz / v};
	out[0] = v;
	// back out of the plane frame (its transpose), then off the origin
	for (int a = 0; a < 3; a++)
		out[1 + a] = pos[a] + qf[0][a] * c[0] + qf[1][a] * c[1] + qf[2][a] * c[2];
}
