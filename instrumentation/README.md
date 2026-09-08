# Fork-local OpenTelemetry advice

This directory instruments the naval-battle fork without adding telemetry calls
to the game's existing namespaces. The aspect pack is pinned to source revision
`6839a803fca573d3c65ff9df2bb9afbf10fa270b`; its build report must resolve all
nine intended call and entry seams exactly once. Run:

```sh
instrumentation/scripts/verify.sh
instrumentation/scripts/verify_oscope.sh
```

Both scripts select Chez Scheme 10.4.1 through the workspace wrapper. The first
builds plain and woven headless workloads, compares their application output
byte-for-byte, checks the compiled aspect report, and validates traces, metrics,
and logs with a small external OTLP capture process. The second sends the same
woven telemetry to oscope in a separate process, checks its viewer, stops and
reopens its chDB database, and queries the persisted signals.

Set `JOLT_ASPECT_JOLT` to the aspect-capable Jolt executable. Until that
compiler is published through the ordinary project toolchain, the default is
the reviewed local workspace build. Set `OSCOPE_ROOT` or `JOLT_CHDB_LIB` to
override the local oscope checkout or qualified chDB Durable library.
`JOLT_CHEZ_WRAPPER` is reserved for CI, where the checked-out compiler has
already provisioned and verified Chez 10.4.1; local runs should retain the
workspace wrapper default.

The direct `casselc/jolt-net` dependency is intentional. The OTel dependency
currently reaches an older pre-Jolt-0.8 network implementation transitively;
the published override fixes its native write argument order and makes numeric
loopback OTLP connections work.

Custom metric and attribute names use the fork-owned
`io.github.casselc.game.*` and `io.github.casselc.game_engine.*`
namespaces. Dimensions are closed and low-cardinality. Advice does not retain
cursor or aim coordinates, world positions, voxel cells, body identifiers,
player identity, file paths, or exception messages.

The current smoke workload executes real pure game transitions and compiles all
nine seams, but deliberately does not invoke native physics or rendering. The
phase-two advice at the frame-present seam now resolves the fork-local HUD only
when the embedded launcher retains it, draws its cached bounded model before
`EndDrawing`, and fails open before presenting the frame. The separate-process
oscope route remains phase one and does not resolve oscope or the HUD. A
playable Xvfb/raylib run remains the final native visual acceptance gate.

To prove the application sources remain untouched:

```sh
sha256sum --check instrumentation/gameplay.sha256
```
