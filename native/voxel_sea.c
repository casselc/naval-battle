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

#define SEA_MAX_FIELD 4096          // particle-lattice capacity
#define SEA_RING_STEP 6.0           // ring tile size, world units
#define SEA_HORIZON 66.0            // ring extent
#define SEA_RING_INNER 30.0         // ring starts here (under the field)

typedef struct {
	int cols, rows;             // particle lattice dimensions
	double step;               // particle spacing, world units
	double origin;             // lattice origin (square: [-origin, origin])
	Mesh mesh;
	Material mat;
	int ready;
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
		UnloadMesh(sea.mesh);  // frees the CPU arrays it was uploaded with
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

	sea.vcount = fc + ring_tiles * 4;
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
	UploadMesh(&sea.mesh, false);
	sea.mat = LoadMaterialDefault();
	sea.ready = 1;
}

// vsea_mesh_update(spray, om, n, t, sun[3], half[3],
//                  deep[4], swell[4], foamc[4]):
// refill the mesh for frame time t from the per-particle spray heights and
// vorticities (foam), then upload. Colors mirror voxel.light's shading.
void vsea_mesh_update(const double *spray, const double *om, int64_t n,
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
			for (int k = 0; k < 4; k++) {
				double base = swellc[k] / 255.0
				    + (foamc[k] / 255.0 - swellc[k] / 255.0) * f;
				int v8 = (int)(base * lit * 255.0);
				if (v8 < 0)
					v8 = 0;
				if (v8 > 255)
					v8 = 255;
				c[k] = (unsigned char)v8;
			}
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
				for (int q = 0; q < 4; q++) {
					int v8 = (int)(swellc[q] * lit);
					if (v8 < 0)
						v8 = 0;
					if (v8 > 255)
						v8 = 255;
					c[q] = (unsigned char)v8;
				}
				sea.idx[ic++] = (uint16_t)vi;
			}
		}

	UpdateMeshBuffer(sea.mesh, 0, sea.verts,
	                sea.vcount * 3 * sizeof(float), 0);
	UpdateMeshBuffer(sea.mesh, 2, sea.norms,
	                sea.vcount * 3 * sizeof(float), 0);
	UpdateMeshBuffer(sea.mesh, 3, sea.cols_,
	                sea.vcount * 4, 0);
	UpdateMeshBuffer(sea.mesh, 6, sea.idx,
	                sea.icount * sizeof(uint16_t), 0);
	free(hs);
	free(fs);
}

// vsea_mesh_draw(): one draw call for the whole sheet, identity transform.
void vsea_mesh_draw(void)
{
	if (!sea.ready)
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
	UnloadMesh(sea.mesh);  // frees the CPU arrays it was uploaded with
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
#define SEA_MAX_FACES 4096

typedef struct {
	Mesh mesh;
	Material mat;
	int faces;             // face count the mesh was built for
	int ready;
	// per-face static data in build order
	signed char (*dir)[3]; // local outward normal
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
		UnloadMesh(sh->mesh);  // frees verts/norms/colors/indices
		free(sh->dir); free(sh->v0); free(sh->base);
	}
	memset(sh, 0, sizeof(*sh));
	int vc = (int)n * 6;
	sh->verts = malloc(vc * 3 * sizeof(float));
	sh->norms = malloc(vc * 3 * sizeof(float));
	sh->cols_ = malloc(vc * 4);
	sh->idx = malloc(vc * sizeof(uint16_t));
	sh->dir = malloc(n * 3);
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
	UploadMesh(&sh->mesh, false);
	sh->mat = LoadMaterialDefault();
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
		if (tc < -1.0)
			continue;
		// sun shading, voxel.light/sun-shade
		double dot = rnx * sun[0] + rny * sun[1] + rnz * sun[2];
		if (dot < 0.0)
			dot = 0.0;
		double shade = 0.35 + 0.65 * dot;
		if (shade > 1.0)
			shade = 1.0;
		int d = -1;
		for (int q = 0; q < 6; q++)
			if (dir_norm[q][0] == sh->dir[f][0]
			    && dir_norm[q][1] == sh->dir[f][1]
			    && dir_norm[q][2] == sh->dir[f][2])
				d = q;
		if (d < 0)
			continue;
		int64_t ci = sh->v0[f][0], cj = sh->v0[f][1], ck = sh->v0[f][2];
		int base_v = f * 6;
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
			for (int w = 0; w < 4; w++) {
				int v8 = (int)(sh->base[f][w] * shade);
				c[w] = (unsigned char)(v8 > 255 ? 255 : v8);
			}
		}
	}

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
	UnloadMesh(sh->mesh);  // frees verts/norms/colors/indices
	free(sh->dir); free(sh->v0); free(sh->base);
	memset(sh, 0, sizeof(*sh));
}
