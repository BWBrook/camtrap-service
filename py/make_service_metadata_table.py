#!/usr/bin/env python3
"""camera_summary.py – Janus edition (v2, hang‑safe)
"""
from __future__ import annotations

import argparse
import csv
import json
import logging as log
import os
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Tuple

try:
    from tqdm import tqdm  # type: ignore
except ImportError:  # graceful fallback
    tqdm = lambda x, **_: x  # noqa: E731

COMMENT_KEYS = {"RD": "rd", "LI": "li", "BL": "bl", "ID": "cam_id"}
TIMEOUT = 120  # seconds for each exiftool chunk


def parse_comment(comment: str) -> Dict[str, str]:
    out: Dict[str, str] = {}
    for part in comment.split(","):
        if "=" in part:
            k, v = part.split("=", 1)
            if (key := k.strip()) in COMMENT_KEYS:
                out[COMMENT_KEYS[key]] = v.strip()
    return out


def exiftool_bulk(
    paths: List[str],
    exiftool: str,
    chunk: int = 200,
) -> Tuple[Dict[str, Dict], List[str]]:
    meta, errors = {}, []
    total = len(paths)
    for i in range(0, total, chunk):
        chunk_paths = paths[i : i + chunk]
        if log.getLogger().isEnabledFor(log.INFO):
            log.info("ExifTool chunk %d/%d (≈%d files)", i // chunk + 1, (total + chunk - 1) // chunk, len(chunk_paths))
        cmd = [
            exiftool,
            "-m",
            "-j",
            "-n",
            "-UserComment",
            "-DateTimeOriginal",
            "-Flash",
        ] + chunk_paths
        try:
            proc = subprocess.run(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=TIMEOUT,
            )
        except subprocess.TimeoutExpired as e:
            log.warning("ExifTool timed out after %s s on chunk starting %s; skipping.", TIMEOUT, chunk_paths[0])
            errors.extend(chunk_paths)
            continue
        # errors
        for line in proc.stderr.decode("utf-8", errors="replace").splitlines():
            if "File format not recognized" in line:
                errors.append(line.split(":", 1)[0].strip())
        # data
        try:
            records = json.loads(proc.stdout or "[]")
        except json.JSONDecodeError as e:
            log.warning("JSON decode failed for chunk starting %s: %s", chunk_paths[0], e)
            continue
        for rec in records:
            meta[os.path.normpath(rec["SourceFile"])] = rec
    return meta, errors


def to_datetime(raw: str | None) -> Tuple[str, datetime]:
    if not raw:
        return "", datetime.min
    try:
        dt = datetime.strptime(raw, "%Y:%m:%d %H:%M:%S")
        return dt.strftime("%Y-%m-%d %H:%M:%S"), dt
    except ValueError:
        return raw, datetime.min


def build_summary(root: Path, exiftool: str) -> Tuple[List[Dict], List[str]]:
    log.info("Scanning for JPEGs …")
    jpeg_paths: List[str] = []
    site_dirs: List[Path] = []
    for dirpath, _, files in os.walk(root):
        jpgs = [f for f in files if f.lower().endswith((".jpg", ".jpeg"))]
        if jpgs:
            site_dirs.append(Path(dirpath))
            jpeg_paths.extend(str(Path(dirpath) / f) for f in jpgs)
    if not jpeg_paths:
        log.warning("No JPEGs found under %s", root)
        return [], []
    log.info("Found %d images across %d site folders", len(jpeg_paths), len(site_dirs))

    meta_map, errors = exiftool_bulk(jpeg_paths, exiftool)

    rows: List[Dict] = []
    for site_dir in tqdm(site_dirs, desc="sites"):
        site = site_dir.name
        for fp in site_dir.iterdir():
            if fp.suffix.lower() not in {".jpg", ".jpeg"}:
                continue
            rec = meta_map.get(os.path.normpath(str(fp)))
            if not rec:
                continue
            dt_str, dt_obj = to_datetime(rec.get("DateTimeOriginal"))
            flash_raw = rec.get("Flash", 0)
            flash_fired = 1 if (isinstance(flash_raw, int) and flash_raw & 1) else 0
            comment_data = parse_comment(rec.get("UserComment", ""))
            rows.append({
                "site": site,
                "cam_id": comment_data.get("cam_id", ""),
                "filename": fp.name,
                "datetime": dt_str,
                "_dt": dt_obj,
                "flash_fired": flash_fired,
                "rd": comment_data.get("rd", ""),
                "li": comment_data.get("li", ""),
                "bl": comment_data.get("bl", ""),
            })
    rows.sort(key=lambda r: (r["site"], r.pop("_dt")))
    return rows, errors


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def cli() -> None:
    p = argparse.ArgumentParser(description="Summarise Cuddeback JPEG metadata.")
    p.add_argument("root", type=Path, help="root directory containing site folders")
    p.add_argument("--exiftool", default=os.getenv("EXIFTOOL_PATH", "exiftool"), help="path to exiftool executable")
    p.add_argument("--output", help="CSV file (default <root>_camera_summary.csv)")
    p.add_argument("-v", "--verbose", action="store_true", help="verbose logging")
    args = p.parse_args()

    log.basicConfig(level=log.INFO if args.verbose else log.WARNING, format="%(levelname)s: %(message)s")
    rows, errors = build_summary(args.root, args.exiftool)

    if not rows:
        sys.exit(0)

    out_csv = Path(args.output) if args.output else args.root.parent / f"{args.root.name}_camera_summary.csv"
    fieldnames = ["site", "cam_id", "filename", "datetime", "flash_fired", "rd", "li", "bl"]
    with out_csv.open("w", newline="") as fh:
        csv.DictWriter(fh, fieldnames=fieldnames).writerows([dict(zip(fieldnames, fieldnames))] + rows)
    print(f"✓ Summary written to {out_csv}")

    if errors:
        err_file = out_csv.with_name("error_files.txt")
        err_file.write_text("\n".join(errors))
        print(f"⚠ Logged {len(errors)} problem files to {err_file}")


if __name__ == "__main__":
    cli()
