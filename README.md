# naval-battle

Two voxel warships duel on a simulated particle ocean until one sinks. A
[Jolt](https://github.com/jolt-lang/jolt) (Clojure on Chez Scheme, no JVM)
prototype rendered with raylib and simulated with Box3D.

![smoke run](naval_smoke.png)

## What is simulated

- **Ocean** — a 2D vortex-particle field, one particle per tile. Ships shed
  vorticity off both sides of their track, blasts inject swirl, spray flies
  ballistically and splashes back. Velocity is evaluated per particle with a
  Greengard adaptive quadtree FMM; a scattering of live vortices is summed
  exactly instead, and dead-calm water costs nothing. The drawn surface is
  the particle set — one mesh vertex per particle, heights straight off the
  simulation, nothing analytic mixed in.

  The simulated water is a fixed window that scrolls with the camera: water
  leaving the trailing edge comes back as still water at the leading one, so
  the sea has no edge to sail off and no bound on where a battle can go,
  while costing a fixed ~9k particles wherever it happens.
- **Floatation** — buoyancy via the divergence theorem over each hull's
  surface mesh, clipped at the plane fitted to the particles under that hull.
  Ships heave on the swell and the wave slope under them rolls and pitches
  them; listing, flooding and sinking emerge from the geometry rather than
  from scripts. Water resists a hull moving up through it as well as
  sideways, so a ship rides a swell and settles from a blast rather than
  bobbing like a cork.

  The solve is linear in surface triangles and runs in C, which is what lets
  a ship be made of as many voxels as it looks like it should be: ~9000 per
  dreadnought at half-unit cells, solved in 45 microseconds a step. Voxel
  size is a property of the layout, so raising the resolution gives a finer
  ship of the same size rather than a bigger one. The collision body is a
  greedy box decomposition of the same voxels - nine solids, exactly tiling
  them, so it has their mass and shape without one shape per cell.
- **Combat** — ballistic shells with an analytic firing solution preview arc,
  voxel-level hull carving on impact, flooding of carved cells, and debris.
  Shells have their own gravity, separate from the world's, so gun range and
  time of flight are independent: a round takes about two and a half seconds
  to cross the fighting range, which is long enough for a ship to be
  somewhere else when it lands.

  Water gets in through breaches at a rate set by their area and the head
  above them, so a hit at the waterline seeps and one under the bilge floods.
  A duel is a couple of dozen salvoes, chipping voxels off a hull that
  settles as it takes water, rather than one lucky shell.
- **Manoeuvring** — hulls resist moving sideways far harder than ahead, and
  propulsion acts through the centre of mass, so putting the helm over
  changes where a ship ends up rather than just which way she points. That is
  what makes evasion mean anything.
- **Camera** — an orthographic isometric vantage that slides over the point
  between the fleets and zooms to hold them both, from the opening approach
  to a knife fight. It only ever slides and zooms, never turns, which is what
  lets the ocean be a window that scrolls beneath it.

## Requirements

- [jolt](https://github.com/jolt-lang/jolt) ≥ 0.8.1 (Homebrew: `brew install jolt`)
- A C toolchain (for the Box3D and sea shims)
- raylib headers (the C shims link raylib; on macOS `brew install raylib`)

## Build and run

```
jolt native     # compile native/voxel_b3.c (the Box3D shim)
jolt sea        # compile native/voxel_sea.c (ocean sim + render kernels)
jolt hull       # compile native/voxel_hull.c (the floatation kernel)
jolt -M:run     # play
jolt -M:test    # run the test suite
```

All three native tasks are mtime-checked, so they are cheap to put in front
of a run.

## Controls

- Arrow keys steer your ship
- Move the mouse to aim; the dotted arc always shows where the shot will
  land, dimmed while the gun is reloading
- Hold the left button to charge and release to fire — the charge sets muzzle
  speed, so it sets how far she throws. A tap still puts a shell in the air,
  just a short one, and the arc shows you that before you spend it
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
- `src/voxel/camera.clj` — the vantage, how it frames the fleets, and the sea
  footprint the ocean sizes itself from. Pure, so both the renderer and the
  world can read it
- `src/voxel/ship.clj` — the dreadnought voxel layout
- `src/voxel/mesh.clj` — voxel surface extraction, the divergence-theorem
  volume/centroid sums written out plainly, and the collision box
  decomposition
- `src/voxel/hullc.clj` — FFI bindings to the floatation kernel
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
- `native/voxel_hull.c` — voxel surface extraction and the floatation solve.
  A hull's mesh lives here for the same reason the ocean's particles do: the
  algorithm is cheap and the marshalling is not

Pure logic (world/ocean/buoyancy/mesh/ship/camera) is headless and
unit-tested, and stays the reference for what the native kernels do: the
tests hold the C ocean step, FMM and floatation solve to the Clojure versions
of the same thing. The render kernels are unit-tested too — they skip every
GL call when no window is up, so their vertex buffers can be read back and
checked without a window or a screenshot diff.

## Smoke testing headlessly

```
env RAYLIB_APP_AUTO_QUIT_MS=45000 VOXEL_APP_AUTOFIRE=90 VOXEL_APP_FPS=0 \
    RAYLIB_APP_SHOT=shot.png jolt -M:run
```

runs a fixed 45 s match with autofiring guns, saves `shot.png`, and prints a
frame/battle summary. `VOXEL_APP_FPS=0` uncaps the frame rate so the summary
reports real frame work rather than the 60fps cap.

Status: prototype — playable end to end, art direction still rough.
