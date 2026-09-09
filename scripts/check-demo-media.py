#!/usr/bin/env python3
"""CI-safe structural checks for the bounded, checked-in demo media."""

from pathlib import Path
import json
import shutil
import struct
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
MEDIA = ROOT / "docs" / "demo"
PNG_NAMES = [
    "01-in-game-hud.png",
    "02-fire-trace.png",
    "03-frame-metric.png",
    "04-metric-chart.png",
    "05-plotje-query-edit.png",
]


def png_size(path):
    raw = path.read_bytes()[:24]
    if len(raw) != 24 or raw[:8] != b"\x89PNG\r\n\x1a\n":
        raise ValueError(f"{path}: not a PNG")
    return struct.unpack(">II", raw[16:24])


def check(path, minimum, maximum):
    size = path.stat().st_size
    if not minimum <= size <= maximum:
        raise ValueError(f"{path}: {size} bytes outside {minimum}..{maximum}")


def decode(path):
    result = subprocess.run(
        ["ffmpeg", "-v", "error", "-i", str(path), "-f", "null", "-"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        text=True,
        timeout=30,
    )
    if result.returncode:
        diagnostic = result.stderr.strip().splitlines()[-1:] or ["decode failed"]
        raise ValueError(f"{path}: {diagnostic[0]}")


def video_facts(path, codecs, duration_bounds, frame_bounds):
    result = subprocess.run(
        ["ffprobe", "-v", "error", "-count_frames", "-select_streams", "v:0",
         "-show_entries", "stream=codec_name,width,height,nb_read_frames:format=duration",
         "-of", "json", str(path)],
        capture_output=True,
        text=True,
        timeout=30,
        check=True,
    )
    facts = json.loads(result.stdout)
    streams = facts.get("streams", [])
    if len(streams) != 1:
        raise ValueError(f"{path}: expected exactly one video stream")
    stream = streams[0]
    codec = stream.get("codec_name")
    duration = float(facts.get("format", {}).get("duration", 0))
    frames = int(stream.get("nb_read_frames", 0))
    if codec not in codecs:
        raise ValueError(f"{path}: unexpected codec {codec}")
    if not duration_bounds[0] <= duration <= duration_bounds[1]:
        raise ValueError(f"{path}: duration {duration}s outside {duration_bounds}")
    if not frame_bounds[0] <= frames <= frame_bounds[1]:
        raise ValueError(f"{path}: {frames} frames outside {frame_bounds}")
    if int(stream.get("width", 0)) < 800 or int(stream.get("height", 0)) < 450:
        raise ValueError(f"{path}: unexpectedly small video frame")


try:
    for command in ("ffmpeg", "ffprobe"):
        if shutil.which(command) is None:
            raise ValueError(f"required media validator not found: {command}")
    for name in PNG_NAMES:
        path = MEDIA / name
        check(path, 10_000, 2_000_000)
        width, height = png_size(path)
        if width < 800 or height < 450:
            raise ValueError(f"{path}: unexpectedly small {width}x{height}")
        decode(path)

    gif = MEDIA / "naval-telemetry-tour.gif"
    webm = MEDIA / "naval-telemetry-tour.webm"
    check(gif, 50_000, 12_000_000)
    check(webm, 50_000, 12_000_000)
    if gif.read_bytes()[:6] not in (b"GIF87a", b"GIF89a"):
        raise ValueError(f"{gif}: not a GIF")
    if webm.read_bytes()[:4] != b"\x1aE\xdf\xa3":
        raise ValueError(f"{webm}: not a WebM container")
    decode(gif)
    decode(webm)
    video_facts(gif, {"gif"}, (3.5, 20.0), (24, 200))
    video_facts(webm, {"vp8", "vp9", "av1"}, (4.0, 20.0), (60, 600))
except (FileNotFoundError, ValueError, subprocess.SubprocessError,
        json.JSONDecodeError) as error:
    print(f"demo media check failed: {error}", file=sys.stderr)
    raise SystemExit(1)

print("demo media check passed: 5 PNGs, bounded GIF, bounded WebM")
