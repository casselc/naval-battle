#!/usr/bin/env bash
set -euo pipefail

root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
wrapper=${JOLT_CHEZ_WRAPPER:-/home/chuck/ai-src/tools/jolt-with-chez-10.4.1}
binary=${NAVAL_TELEMETRY_BINARY:-$root/target/telemetry/naval-battle}
lib=${JOLT_CHDB_LIB:-/home/chuck/.cache/jolt-chdb/26.7.2-rc.2/linux-amd64/libchdb.so}
out="$root/target/telemetry/native-acceptance"
mkdir -p "$out"
data=$(mktemp -d "$out/data.XXXXXX")
object_id="native-acceptance"
service="naval-battle-native-acceptance"
log="$data/game.log"

test -x "$binary"
test -f "$lib"

if [[ -n "${DISPLAY:-}" ]]; then
  display_runner=()
else
  command -v xvfb-run >/dev/null
  display_runner=(xvfb-run -a -s "-screen 0 1280x720x24")
fi

set +e
env JOLT_CHDB_LIB="$lib" \
  VOXEL_OTEL_STORAGE_ROOT="$data/store" \
  VOXEL_OTEL_OBJECT_ID="$object_id" \
  VOXEL_OTEL_VIEWER_PORT=0 \
  OTEL_SERVICE_NAME="$service" \
  "${display_runner[@]}" timeout --signal=TERM --kill-after=20s 12s \
  "$binary" >"$log" 2>&1
status=$?
set -e
test "$status" -eq 124 -o "$status" -eq 143
grep -q 'embedded telemetry viewer: http://127.0.0.1:' "$log"
grep -q 'Initializing raylib 6.0' "$log"

deps="{:paths [\"$root/instrumentation/test\"]
       :deps {io.github.chucklehead-dev/oscope
              {:git/url \"https://github.com/chucklehead-dev/oscope.git\"
               :git/sha \"4770e817ad3821c9d060d4fe4dc6ef1f32bf0420\"}}}"

cd /tmp
env JOLT_CHDB_LIB="$lib" "$wrapper" jolt -Srepro -Sdeps "$deps" -M \
  -m naval-battle.embedded-check "$data/store" "$object_id" "$service"
cd "$root"
sha256sum --check instrumentation/gameplay.sha256
echo "woven native telemetry acceptance: game signals persisted and reopened"
