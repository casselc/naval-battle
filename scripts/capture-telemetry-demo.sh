#!/usr/bin/env bash
set -euo pipefail

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
binary=${NAVAL_TELEMETRY_BINARY:-$root/target/telemetry/naval-battle}
report=${NAVAL_TELEMETRY_ASPECT_REPORT:-$root/target/telemetry/aspects.edn}
lib=${JOLT_CHDB_LIB:-/home/chuck/.cache/jolt-chdb/26.7.2-rc.2/linux-amd64/libchdb.so}
port=${NAVAL_DEMO_PORT:-18420}
display=${DISPLAY:-}

command -v curl >/dev/null
command -v ffmpeg >/dev/null
command -v npx >/dev/null
test -f "$lib"
if [[ -z "$display" ]]; then
  command -v xvfb-run >/dev/null
  display_runner=(xvfb-run -a -s "-screen 0 1280x720x24")
else
  display_runner=()
fi

mkdir -p "$root/docs/demo" "$root/target/telemetry"
run_dir=$(mktemp -d "$root/target/telemetry/demo-run.XXXXXX")
game_log="$run_dir/game.log"
shot="$run_dir/in-game-hud.png"
shot_relative=${shot#"$root/"}
game_pid=

stop_game() {
  if [[ -n "$game_pid" ]] && kill -0 "$game_pid" 2>/dev/null; then
    kill -TERM "$game_pid" 2>/dev/null || true
    wait "$game_pid" 2>/dev/null || true
  fi
}
trap stop_game EXIT INT TERM

cd "$root"
sha256sum --check instrumentation/gameplay.sha256
sha256sum --check resources/telemetry/gameplay-source.sha256

# Never capture an opaque prebuilt executable. The builder verifies the
# gameplay manifest, embeds the qualified chDB ABI, validates the exact aspect
# report, and overwrites this output from the current checkout.
NAVAL_TELEMETRY_BINARY="$binary" \
NAVAL_TELEMETRY_ASPECT_REPORT="$report" \
  "$root/instrumentation/scripts/build_embedded.sh"
test -x "$binary"
test -s "$report"
{
  printf 'source-head %s\n' "$(git rev-parse HEAD)"
  sha256sum "$binary" "$report"
} >"$run_dir/build-provenance.txt"

env JOLT_CHDB_LIB="$lib" \
  VOXEL_OTEL_STORAGE_ROOT="$run_dir/store" \
  VOXEL_OTEL_OBJECT_ID="telemetry-demo" \
  VOXEL_OTEL_VIEWER_PORT="$port" \
  OTEL_SERVICE_NAME="naval-battle-demo" \
  VOXEL_APP_AUTOFIRE=1 \
  VOXEL_APP_REFIRE=120 \
  VOXEL_APP_FPS=60 \
  RAYLIB_APP_SHOT="$shot_relative" \
  VOXEL_APP_SHOT_FRAME=150 \
  RAYLIB_APP_AUTO_QUIT_MS=45000 \
  "${display_runner[@]}" "$binary" >"$game_log" 2>&1 &
game_pid=$!

ready=0
for _ in $(seq 1 120); do
  if curl -fsS "http://127.0.0.1:$port/healthz" >/dev/null 2>&1; then
    ready=1
    break
  fi
  kill -0 "$game_pid" 2>/dev/null || break
  sleep 0.25
done
if [[ "$ready" -ne 1 ]]; then
  echo "embedded naval-battle viewer did not become healthy; diagnostics retained at $game_log" >&2
  exit 1
fi

NAVAL_DEMO_PORT="$port" npx playwright test --project=demo

# llvmpipe/Xvfb can render well below the requested 60 FPS. Wait for the
# in-frame raylib capture rather than assuming four wall-clock seconds means
# frame 150 has presented.
for _ in $(seq 1 100); do
  [[ -s "$shot" ]] && break
  kill -0 "$game_pid" 2>/dev/null || break
  sleep 0.2
done
test -s "$shot"
cp "$shot" "$root/docs/demo/01-in-game-hud.png"
stop_game
game_pid=

grep -q 'Initializing raylib 6.0' "$game_log"
grep -q 'embedded telemetry viewer: http://127.0.0.1:' "$game_log"

ffmpeg -hide_banner -loglevel error -y \
  -ss 0.3 -i "$root/docs/demo/naval-telemetry-tour.webm" \
  -vf "fps=8,scale=960:-1:flags=lanczos,split[s0][s1];[s0]palettegen=max_colors=128[p];[s1][p]paletteuse=dither=bayer:bayer_scale=3" \
  "$root/docs/demo/naval-telemetry-tour.gif"

python3 "$root/scripts/check-demo-media.py"
sha256sum --check instrumentation/gameplay.sha256
sha256sum --check resources/telemetry/gameplay-source.sha256
echo "naval-battle telemetry demo captured in docs/demo"
echo "run evidence retained at $run_dir"
