#!/usr/bin/env bash
set -euo pipefail

root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
compiler=${JOLT_ASPECT_JOLT:-/home/chuck/ai-src/worktrees/jolt-aspects-v083-sync/target/release/jolt}
wrapper=/home/chuck/ai-src/tools/jolt-with-chez-10.4.1
out="$root/target/instrumentation"

mkdir -p "$out"
cd "$root"
sha256sum --check instrumentation/gameplay.sha256

cd "$root/instrumentation/plain"
"$wrapper" "$compiler" build -m naval-battle.instrumentation-smoke \
  -o "$out/plain-smoke"

cd "$root/instrumentation/woven"
"$wrapper" "$compiler" build -m naval-battle.instrumentation-smoke \
  -o "$out/woven-smoke"
"$wrapper" jolt -M:test

OTEL_SDK_DISABLED=true "$out/plain-smoke" >"$out/plain.out"

: >"$out/capture.jsonl"
: >"$out/receiver.port"
python3 "$root/instrumentation/scripts/otlp_capture.py" \
  "$out/capture.jsonl" "$out/receiver.port" &
receiver=$!
trap 'kill "$receiver" 2>/dev/null || true' EXIT

for _ in $(seq 1 100); do
  test -s "$out/receiver.port" && break
  sleep 0.05
done
test -s "$out/receiver.port"
port=$(tr -d '\r\n' <"$out/receiver.port")

OTEL_EXPORTER_OTLP_ENDPOINT="http://127.0.0.1:$port" \
  "$out/woven-smoke" >"$out/woven.out"

cmp "$out/plain.out" "$out/woven.out"
python3 "$root/instrumentation/scripts/check_capture.py" "$out/capture.jsonl"
cd "$root"
sha256sum --check "$root/instrumentation/gameplay.sha256"
echo "plain/woven application output: identical"
