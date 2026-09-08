# naval-battle

Two voxel warships duel on a simulated particle ocean until one sinks. A
[Jolt](https://github.com/jolt-lang/jolt) (Clojure on Chez Scheme, no JVM)
prototype rendered with raylib and simulated with Box3D.

![smoke run](naval_smoke.png)

## What is simulated

- **Ocean** — a 2D vortex-particle field covering the whole visible arena,
  one particle per tile. Ships shed vorticity off both sides of their track,
  blasts inject swirl, spray flies ballistically and splashes back. Velocity
  is evaluated per particle with a Greengard adaptive quadtree FMM; a
  scattering of live vortices is summed exactly instead, and dead-calm water
  costs nothing. The drawn surface is the particle set — one mesh vertex per
  particle, heights straight off the simulation, nothing analytic mixed in —
  and the sheet is sized from the camera so the water always runs past the
  frame.
- **Floatation** — buoyancy via the divergence theorem over each hull's
  surface mesh, clipped at the plane fitted to the particles under that hull.
  Ships heave on the swell and the wave slope under them rolls and pitches
  them; listing, flooding and sinking emerge from the geometry rather than
  from scripts.
- **Combat** — ballistic shells with an analytic firing solution preview arc,
  voxel-level hull carving on impact, flooding of carved cells, and debris.
  Shells have their own gravity, separate from the world's, so gun range and
  time of flight are independent: a round takes about two and a half seconds
  to cross the fighting range, which is long enough for a ship to be
  somewhere else when it lands.
- **Manoeuvring** — hulls resist moving sideways far harder than ahead, and
  propulsion acts through the centre of mass, so putting the helm over
  changes where a ship ends up rather than just which way she points. That is
  what makes evasion mean anything.

## Requirements

- [jolt](https://github.com/jolt-lang/jolt) ≥ 0.8.1 (Homebrew: `brew install jolt`)
- A C toolchain (for the Box3D and sea shims)
- raylib headers (the C shims link raylib; on macOS `brew install raylib`)

## Build and run

```
jolt native     # compile native/voxel_b3.c (the Box3D shim)
jolt sea        # compile native/voxel_sea.c (ocean sim + render kernels)
jolt -M:run     # play
jolt -M:test    # run the test suite
```

Both native tasks are mtime-checked, so they are cheap to put in front of a
run.

## Controls

- Arrow keys steer your ship
- Move the mouse to aim; the dotted arc previews the shot
- Click to fire
- `R` restarts after a battle ends

The enemy works to hold a standoff near the edge of her reach rather than
closing: time of flight grows with range, and it is time of flight that lets
her be somewhere else when a shell arrives. She watches your guns rather than
your ship - reversing her helm across your reload, so the lead you fired on
is not the course she is carrying when the round gets there - and breaks off
if the range falls to where the hulls could touch.

## Code layout

- `src/voxel/world.clj` — game state machine: ships, shells, firing solution,
  impact handling, enemy AI, and the couplings fed to the ocean
- `src/voxel/ocean.clj` — the vortex-particle model and the FMM velocity
  solve, in pure Clojure. This is the definition of what the ocean *is*; the
  native kernel below is held to it step for step by the tests
- `src/voxel/buoyancy.clj` — divergence-theorem buoyancy on meshes clipped at
  an arbitrary water plane
- `src/voxel/camera.clj` — the vantage, and the sea footprint the ocean sizes
  itself from. Pure, so both the renderer and the world can read it
- `src/voxel/ship.clj` — the dreadnought voxel layout
- `src/voxel/mesh.clj` — voxel surface extraction, and the divergence-theorem
  volume/centroid sums written out plainly
- `src/voxel/physics.clj` — Box3D bodies, floatation forces, damage
- `src/voxel/seac.clj` — FFI bindings to the sea kernels
- `src/voxel/box3d.clj` — FFI bindings to the Box3D shim
- `src/voxel/render.clj` — scene drawing
- `src/voxel/raylib.clj` — raylib FFI, immediate-mode draw helpers
- `src/voxel/input.clj` — mouse/keyboard snapshot, sea-plane picking
- `src/voxel/main.clj` — window, game loop
- `native/voxel_sea.c` — the particle state and its step, the FMM, and the
  sea and hull meshes. Particle data lives here and never crosses the FFI
  boundary per particle: jolt asks for a step, and the renderer's mesh is
  filled from the same arrays

Pure logic (world/ocean/buoyancy/mesh/ship/camera) is headless and
unit-tested. The C kernels are unit-tested too — they skip every GL call when
no window is up, so their vertex buffers can be read back and checked without
a window or a screenshot diff.

## Smoke testing headlessly

```
env RAYLIB_APP_AUTO_QUIT_MS=45000 VOXEL_APP_AUTOFIRE=90 VOXEL_APP_FPS=0 \
    RAYLIB_APP_SHOT=shot.png jolt -M:run
```

runs a fixed 45 s match with autofiring guns, saves `shot.png`, and prints a
frame/battle summary. `VOXEL_APP_FPS=0` uncaps the frame rate so the summary
reports real frame work rather than the 60fps cap.

Status: prototype — playable end to end, art direction still rough.
