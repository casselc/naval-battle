#!/usr/bin/env python3
"""Assert semantic evidence in the deterministic external OTLP capture."""

import json
import pathlib
import sys

entries = [json.loads(line) for line in pathlib.Path(sys.argv[1]).read_text().splitlines()]
paths = {entry["path"] for entry in entries}
wire = json.dumps(entries, separators=(",", ":"))

assert "/v1/traces" in paths, paths
assert "/v1/metrics" in paths, paths
assert "/v1/logs" in paths, paths
for expected in [
    "naval-battle-case-study",
    "game.action.fire",
    "io.github.casselc.game.action.outcome",
    "io.github.casselc.game_engine.operation.duration",
    "io.github.casselc.game_engine.operation.name",
    "simulation",
    "game.match.start",
]:
    assert expected in wire, expected

for forbidden in [
    "secret-target",
    "captain@example.test",
    "game.target",
    "game.position",
    "game.player.id",
]:
    assert forbidden not in wire, forbidden

print("external OTLP capture: traces, metrics, and logs conform")
