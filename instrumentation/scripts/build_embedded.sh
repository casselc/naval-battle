#!/usr/bin/env bash
set -euo pipefail

root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
compiler=${JOLT_ASPECT_JOLT:-/home/chuck/ai-src/worktrees/jolt-aspects-v083-sync/target/release/jolt}
wrapper=${JOLT_CHEZ_WRAPPER:-/home/chuck/ai-src/tools/jolt-with-chez-10.4.1}
chdb_root=${JOLT_CHDB_ROOT:-/home/chuck/ai-src/jolt-chdb}
raylib=${RAYLIB_LIB:-}
out=${NAVAL_TELEMETRY_BINARY:-$root/target/telemetry/naval-battle}
report=${NAVAL_TELEMETRY_ASPECT_REPORT:-$root/target/telemetry/aspects.edn}

case "$(uname -s)" in
  Linux) platform=linux; suffix=so
    raylib_candidates=(/usr/local/lib/libraylib.so /usr/lib/x86_64-linux-gnu/libraylib.so.6) ;;
  Darwin) platform=darwin; suffix=dylib
    raylib_candidates=(/opt/homebrew/lib/libraylib.dylib /usr/local/lib/libraylib.dylib) ;;
  *) echo "woven embedded build supports Linux and macOS" >&2; exit 2 ;;
esac

if [[ -z "$raylib" ]]; then
  for candidate in "${raylib_candidates[@]}"; do
    if [[ -f "$candidate" ]]; then raylib=$candidate; break; fi
  done
fi

test -x "$compiler"
test -x "$wrapper"
test -f "$chdb_root/resources/jdbc/chdb/abi.edn"
test -f "$raylib"
test -f "$root/native/libvoxel_b3.$suffix"
test -f "$root/native/libvoxel_sea.$suffix"
test -f "$root/native/libvoxel_hull.$suffix"

for path in "$chdb_root" "$raylib" "$root" "$out" "$report"; do
  case "$path" in
    *'"'*|*'\'*|*$'\n'*) echo "unsupported path for EDN build configuration" >&2; exit 2 ;;
  esac
done

mkdir -p "$(dirname -- "$out")" "$(dirname -- "$report")"
raylib=$(realpath "$raylib")
b3=$(realpath "$root/native/libvoxel_b3.$suffix")
sea=$(realpath "$root/native/libvoxel_sea.$suffix")
hull=$(realpath "$root/native/libvoxel_hull.$suffix")
abi_resources=$(realpath "$chdb_root/resources")

deps="{:paths [\"src\" \"instrumentation/src\" \"instrumentation/resources\"]
       :jolt/native [{:name \"raylib\" :$platform [\"$raylib\"]}
                     {:name \"voxel_b3\" :$platform [\"$b3\"]}
                     {:name \"voxel_sea\" :$platform [\"$sea\"]}
                     {:name \"voxel_hull\" :$platform [\"$hull\"]}]
       :jolt/build {:embed [\"$abi_resources\"]
                    :aspects [{:resource \"META-INF/jolt/aspects/packs/naval-battle-6839a80.edn\"
                               :provider naval-battle.instrumentation/aspect-provider}]
                    :aspect-report \"$report\"}}"

cd "$root"
sha256sum --check instrumentation/gameplay.sha256
"$wrapper" "$compiler" -Sdeps "$deps" -A:telemetry build \
  -m voxel.telemetry.main -o "$out"
report_deps="{:paths [\"$root/instrumentation/test\"]}"
NAVAL_ASPECT_REPORT="$report" "$wrapper" jolt -Srepro -Sdeps "$report_deps" \
  -M -e "(require 'naval-battle.instrumentation-report-test 'clojure.test)
         (let [r (clojure.test/run-tests 'naval-battle.instrumentation-report-test)]
           (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))"
sha256sum --check instrumentation/gameplay.sha256
echo "woven embedded telemetry binary: $out"
echo "aspect report: $report"
