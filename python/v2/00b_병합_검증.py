# -*- coding: utf-8 -*-
"""
================================================================================
 00b_병합_검증.py — Lens에서 분할 다운로드한 CSV들을 merged_3files.csv로 병합+검증
================================================================================
 사용법:
   1) lens.org에서 Export한 CSV들을 6_데이터/lens_raw_v2/ 폴더에 전부 넣는다.
   2) 이 스크립트를 실행한다:  python 00b_병합_검증.py
   3) 게이트 통과 시 6_데이터/merged_3files.csv 생성 → 바로 01 실행 가능.

 검증(§1 게이트):
   - 필수 컬럼 전부 존재 (00_config.py COL_* 스키마)
   - Jurisdiction에 EP + 유럽 회원국(DE/FR/GB 등) 실재
   - Lens ID 기준 중복 제거(분할 다운로드 경계 중복 방지)
   - 원시 총건수 출력 → 체크리스트 §1에 기록
================================================================================
"""
import os, sys, glob
import pandas as pd
from importlib import util as _u

_here = os.path.dirname(os.path.abspath(__file__))
_s = _u.spec_from_file_location("cfg", os.path.join(_here, "00_config.py"))
cfg = _u.module_from_spec(_s); _s.loader.exec_module(cfg)

RAW_DIR = os.path.join(cfg.BASE_PATH, "lens_raw_v2")
OUT     = cfg.P(cfg.RAW_INPUT)   # merged_3files.csv

REQUIRED_COLS = [
    "Title", "Abstract", "Jurisdiction", "Priority Numbers",
    "Earliest Priority Date", "Application Date", "Publication Year",
    "Cited by Patent Count", "Extended Family Members", "Extended Family Size",
    "Applicants", "Lens ID",
]

def main():
    files = sorted(glob.glob(os.path.join(RAW_DIR, "*.csv")))
    if not files:
        sys.exit(f"[중단] {RAW_DIR} 에 CSV가 없습니다. Lens Export 파일을 넣어주세요.")
    print(f"[병합] 입력 CSV {len(files)}개:")
    frames = []
    for f in files:
        df = pd.read_csv(f, dtype=str, low_memory=False)
        print(f"  - {os.path.basename(f)}: {len(df):,}행, {len(df.columns)}컬럼")
        frames.append(df)

    merged = pd.concat(frames, ignore_index=True)
    n_raw = len(merged)

    # 게이트 1: 필수 컬럼
    missing = [c for c in REQUIRED_COLS if c not in merged.columns]
    if missing:
        sys.exit(f"[중단] 필수 컬럼 누락: {missing}\n  → Lens Export 시 컬럼 선택을 확인하세요.")
    print("[게이트1 통과] 필수 컬럼 12개 전부 존재")

    # 게이트 2: Lens ID 중복 제거 (분할 다운로드 경계 중복)
    merged = merged.drop_duplicates(subset=["Lens ID"], keep="first")
    n_dedup = len(merged)
    print(f"[게이트2] Lens ID 중복 제거: {n_raw:,} → {n_dedup:,} ({n_raw - n_dedup:,}건 제거)")

    # 게이트 3: 관할 분포 — EP + 유럽 국가청 실재 확인
    jur = merged["Jurisdiction"].str.strip().str.upper().value_counts()
    print("[게이트3] Jurisdiction 분포:")
    for k, v in jur.items():
        print(f"  {k}: {v:,} ({v / n_dedup * 100:.1f}%)")
    euro_seen = [c for c in ("EP", "DE", "FR", "GB") if c in jur.index]
    if "EP" not in euro_seen:
        sys.exit("[중단] Jurisdiction에 EP가 없습니다. Export 필터를 확인하세요.")
    if len(euro_seen) < 2:
        print("[경고] 유럽 국가청(DE/FR/GB)이 안 보입니다 — 관할 확장 Export가 맞는지 확인.")

    merged.to_csv(OUT, index=False)
    print(f"\n[완료] {OUT} 저장 — 원시 총건수 {n_dedup:,}건")
    print("  → 체크리스트 §1 '원시 총건수'에 기록 후 01_정제_시드후보.py 실행.")

if __name__ == "__main__":
    main()
