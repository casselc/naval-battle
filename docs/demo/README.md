# Naval battle telemetry storyboard

This is one reproducible end-to-end story over real game activity. The woven
native game uses its existing smoke controls to fire repeated salvos; the fork-local
advice records those calls directly into the in-process Durable chDB store.
The browser frames then use the adjacent loopback-only oscope viewer. No demo
record is posted through OTLP and no screenshot uses fixture data.

1. `01-in-game-hud.png` — the moving battle after scripted autofire, with the
   cached non-blocking telemetry HUD showing live spans and metrics.
2. `02-fire-trace.png` — filter the trace workbench to an accepted player or AI
   shot and inspect its semantic game attributes.
3. `03-frame-metric.png` — find the cumulative presented-frame metric in the
   bounded logs-and-metrics query.
4. `04-metric-chart.png` — group the actual game and runtime metrics by at most
   six short, readable units, explicitly including the game-specific
   `{action}` and `{frame}`, instead of plotting crowded metric names.
5. `05-plotje-query-edit.png` — carry that bounded query into Plotje and edit
   the title and palette without replacing it with literal sample data.
6. `naval-telemetry-tour.webm` and `naval-telemetry-tour.gif` — the same browser
   sequence as a short video and review-friendly animation.

## Reproduce locally

First build the native shims and woven executable as described in the project
README. Install the pinned browser once with `npm ci` and
`npx playwright install chromium`. Then run:

```sh
DISPLAY=:0 \
JOLT_ASPECT_JOLT=/path/to/aspect-capable/jolt \
JOLT_CHDB_ROOT=/path/to/jolt-chdb \
RAYLIB_LIB=/path/to/libraylib.so \
JOLT_CHDB_LIB=/path/to/qualified/libchdb.so \
LD_LIBRARY_PATH=/path/to/raylib:/path/to/box3d \
  npm run demo:capture
```

If `DISPLAY` is unset, the script uses `xvfb-run`. It requires `curl`, `ffmpeg`,
and Chromium. Rendering is intentionally a local job because it needs native
OpenGL, a display server, a qualified chDB library, and video codecs. The
capture script always rebuilds the woven executable from the current checkout,
verifies its exact aspect report, and records the executable/report hashes with
the retained run evidence. CI runs `npm run demo:syntax` and
`npm run demo:check`; the latter fully decodes every artifact and validates
video codecs, dimensions, durations, and frame counts without starting a
display or trusting file extensions.

The game run lasts at most 45 seconds and uses a fresh store below
`target/telemetry`. The script verifies both gameplay/native checksum manifests
before and after capture.

The present oscope series renderer cannot use a fixed one-minute counter or
histogram bucket from this short run: its single populated point reaches the
numeric scale as `NaN` and the request fails closed. An unbucketed counter
collapses the run into one categorical bar, while line and area marks reject a
categorical x column. A follow-up oscope issue should make singleton numeric
series render as a point (and define a zero-width scale) so this scene can
become a frame-increase time series with a line-to-area edit. Until then the
bounded metric-unit chart is the legible, query-backed representation; Plotje
still demonstrates live title and palette edits over the real rows.
