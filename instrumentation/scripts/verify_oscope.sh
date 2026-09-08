#!/usr/bin/env bash
set -euo pipefail

root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
oscope=${OSCOPE_ROOT:-/home/chuck/ai-src/oscope}
wrapper=/home/chuck/ai-src/tools/jolt-with-chez-10.4.1
lib=${JOLT_CHDB_LIB:-/home/chuck/.cache/jolt-chdb/26.7.2-rc.2/linux-amd64/libchdb.so}
out="$root/target/instrumentation"
data=$(mktemp -d "$out/oscope-data.XXXXXX")
log="$data/server.log"

test -x "$out/woven-smoke"
test -f "$lib"

cd "$oscope"
env JOLT_CHDB_LIB="$lib" OSCOPE_PORT=0 OSCOPE_CHDB_SPEC="chdb:$data/db" \
  "$wrapper" jolt -M:server >"$log" 2>&1 &
server=$!
trap 'kill -TERM "$server" 2>/dev/null || true' EXIT

for _ in $(seq 1 300); do
  grep -q 'serving its viewer at http://127.0.0.1:' "$log" && break
  kill -0 "$server" 2>/dev/null || { tail -80 "$log"; exit 1; }
  sleep 0.1
done
grep -q 'serving its viewer at http://127.0.0.1:' "$log"
port=$(sed -n 's#.*http://127\.0\.0\.1:\([0-9][0-9]*\)/oscope.*#\1#p' "$log" | tail -1)
test -n "$port"

curl -fsS "http://127.0.0.1:$port/healthz" >/dev/null
OTEL_EXPORTER_OTLP_ENDPOINT="http://127.0.0.1:$port" "$out/woven-smoke" \
  >"$data/application.out"
curl -fsS "http://127.0.0.1:$port/oscope" >"$data/oscope.html"

kill -TERM "$server"
set +e
wait "$server"
server_status=$?
set -e
test "$server_status" -eq 0 -o "$server_status" -eq 143
trap - EXIT

deps="{:paths [\"$root/instrumentation/test\"]}"
env JOLT_CHDB_LIB="$lib" "$wrapper" jolt -Sdeps "$deps" -M \
  -m naval-battle.oscope-check "chdb:$data/db"

cd "$root"
sha256sum --check instrumentation/gameplay.sha256
echo "separate-process oscope acceptance: traces, metrics, and logs persisted"
