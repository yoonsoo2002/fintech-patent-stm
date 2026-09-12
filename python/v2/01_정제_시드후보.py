"""
================================================================================
 01_정제_시드후보.py  —  [단계 1·2] 패밀리 정제 + 피인용 시드후보 추출
================================================================================
 입력 : merged_3files.csv (Lens IP5 원시 통합본)
 출력 : fintech_patents_family_cleaned.csv  (1패밀리 1대표)
        seed_patents_top600.csv             (기간별 피인용 상위 → 시드후보)
 다음 : 이 seed 후보를 외부 LLM(Gemini)으로 진성/노이즈 라벨 → final_seed.csv (워크플로우 §3)

 v1(embedding.py 블록1~4)을 충실히 옮기되:
   · 모든 파라미터를 00_config.py 에서 가져옴
   · ★Origin_Country를 cfg.resolve_country()로 잡아 '유럽(EP) 블록'을 살림(EUROPE_MODE)
   · 단계마다 건수/국가분포를 찍어 사용자가 검증 가능
================================================================================
"""
import os, sys
import numpy as np
import pandas as pd

# ── 설정 로드 ──────────────────────────────────────────────────────────────
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".")
import importlib.util as _u
_spec = _u.spec_from_file_location("cfg", os.path.join(os.path.dirname(__file__) if "__file__" in globals() else ".", "00_config.py"))
cfg = _u.module_from_spec(_spec); _spec.loader.exec_module(cfg)
cfg.print_config()

# Colab이면 드라이브 마운트
if cfg.USE_COLAB:
    from google.colab import drive
    drive.mount("/content/drive")

# ──────────────────────────────────────────────────────────────────────────
# 1) 로드 + 기본 전처리
# ──────────────────────────────────────────────────────────────────────────
def basic_preprocessing(df):
    df[cfg.COL_TITLE]    = df[cfg.COL_TITLE].fillna("")
    df[cfg.COL_ABSTRACT] = df[cfg.COL_ABSTRACT].fillna("")
    # Title [SEP] Abstract (임베딩은 Title 1회 — R STM에서만 Title 2회 가중)
    df["_text"]     = df[cfg.COL_TITLE].str.strip() + cfg.TEXT_SEP + df[cfg.COL_ABSTRACT].str.strip()
    df["_text_len"] = df["_text"].str.len()

    for col in [cfg.COL_APPDATE, "Earliest Priority Date", "Publication Date"]:
        if col in df.columns:
            df[col] = pd.to_datetime(df[col], errors="coerce")

    df[cfg.COL_CITES] = pd.to_numeric(df[cfg.COL_CITES], errors="coerce").fillna(0).astype(int)

    before = len(df)
    df = df[df["_text_len"] > 10].copy()
    print(f"  텍스트 부족 제거: {before - len(df):,}건")

    # IP5(또는 유럽 포함) 관할 필터
    if cfg.COL_JUR in df.columns:
        keep = df[cfg.COL_JUR].apply(lambda j: cfg.resolve_country(j) is not None)
        print(f"  비대상 관할 제거: {(~keep).sum():,}건")
        df = df[keep].copy()

    # 기간 라벨 (출원일 기준)
    df["_app_year"] = df[cfg.COL_APPDATE].dt.year
    def assign_period(y):
        if pd.isna(y): return "Unknown"
        y = int(y)
        for label, (s, e) in cfg.PERIOD_BREAKS.items():
            if s <= y <= e: return label
        return "Unknown"
    df["Period"] = df["_app_year"].apply(assign_period)
    print(f"  ✅ 전처리 후: {len(df):,}건")
    return df

# ──────────────────────────────────────────────────────────────────────────
# 2) 패밀리 ID + 대표 선정 (1패밀리 1대표) + ★Origin_Country (유럽 블록 반영)
# ──────────────────────────────────────────────────────────────────────────
def assign_family_id(df):
    if cfg.COL_FAMILY in df.columns:
        def norm(s):
            if pd.isna(s) or str(s).strip() == "": return None
            members = sorted(set(m.strip() for m in str(s).split(";;") if m.strip()))
            return "||".join(members) if members else None
        df["_family_key"] = df[cfg.COL_FAMILY].apply(norm)
        nofam = df["_family_key"].isna()
        df.loc[nofam, "_family_key"] = df.loc[nofam, "Lens ID"] if "Lens ID" in df.columns else df.index[nofam].astype(str)
        print(f"  고유 패밀리 수: {df['_family_key'].nunique():,}")
    else:
        df["_family_key"] = df["Lens ID"] if "Lens ID" in df.columns else df.index.astype(str)
    return df

def extract_origin(priority_str, fallback):
    """우선권 번호들에서 분석용 국가블록(US/EP/CN/JP/KR)을 얻는다. ★유럽 aggregate 반영."""
    if pd.notna(priority_str) and str(priority_str).strip():
        for p in str(priority_str).split(";;"):
            p = p.strip()
            if len(p) >= 2:
                resolved = cfg.resolve_country(p[:2])
                if resolved is not None:
                    return resolved
    return cfg.resolve_country(fallback)

def process_family_group(group):
    rep = group.loc[group["_text_len"].idxmax()].copy()          # 텍스트 가장 긴 멤버 = 대표
    earliest = group.loc[group["Earliest Priority Date"].idxmin()] if "Earliest Priority Date" in group else group.iloc[0]
    rep["Origin_Country"] = extract_origin(earliest.get(cfg.COL_PRIORITY, ""),
                                           earliest.get(cfg.COL_JUR, ""))
    rep["Final_Family_Size"] = group["Extended Family Size"].max() if "Extended Family Size" in group else len(group)
    rep["Family_Max_Cited"]  = group[cfg.COL_CITES].max()
    if "Earliest Priority Date" in group and pd.notna(earliest.get("Earliest Priority Date")):
        rep["Priority_Year"] = int(earliest["Earliest Priority Date"].year)
    return rep

def deduplicate_families(df):
    print(f"\n  패밀리 정제 시작 | 입력 {len(df):,} → 패밀리 {df['_family_key'].nunique():,}")
    try:
        from tqdm import tqdm; tqdm.pandas(desc="  패밀리 정제")
        res = df.groupby("_family_key").progress_apply(process_family_group)
    except ImportError:
        res = df.groupby("_family_key").apply(process_family_group)
    res = res.reset_index(drop=True)
    # ★유럽 제외 모드면 Origin이 None인 행 제거
    res = res[res["Origin_Country"].notna()].copy()
    print(f"  ✅ 정제 완료: {len(res):,}건")
    print("\n  ── Origin_Country 분포 (★유럽 비중 꼭 확인) ──")
    print(res["Origin_Country"].value_counts())
    print(f"  EP(유럽) 비중: {(res['Origin_Country']=='EP').mean()*100:.2f}%  "
          f"← v1은 1.4%였음. aggregate면 크게 오를 것")
    return res

# ──────────────────────────────────────────────────────────────────────────
# 3) 피인용 시드후보 (기간별 TOP_N)
# ──────────────────────────────────────────────────────────────────────────
def build_seed_candidates(df):
    all_seeds = []
    for period in cfg.PERIOD_BREAKS.keys():
        g = df[df["Period"] == period].copy()
        if len(g) == 0: continue
        g[cfg.COL_APPDATE] = pd.to_datetime(g[cfg.COL_APPDATE], errors="coerce")
        g["_years"] = (cfg.REFERENCE_DATE - g[cfg.COL_APPDATE]).dt.days / 365.25
        miss = g["_years"].isna()
        if miss.any() and cfg.COL_PUBYEAR in g:
            g.loc[miss, "_years"] = cfg.REFERENCE_DATE.year - g.loc[miss, cfg.COL_PUBYEAR]
        g["_years"] = g["_years"].clip(lower=cfg.MIN_YEARS)
        g["Citation_Index"] = (pd.to_numeric(g[cfg.COL_CITES], errors="coerce").fillna(0) / g["_years"]).round(4)
        top = g.sort_values("Citation_Index", ascending=False).head(cfg.TOP_N).copy()
        top["Seed_Rank"] = range(1, len(top) + 1)
        print(f"  {period}: {len(g):,}건 → 상위 {len(top)} 추출 "
              f"(국가분포 {dict(top['Origin_Country'].value_counts())})")
        all_seeds.append(top)
    seeds = pd.concat(all_seeds, ignore_index=True)
    return seeds

# ── 실행 ───────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    print("\n[로드]", cfg.P(cfg.RAW_INPUT))
    df = pd.read_csv(cfg.P(cfg.RAW_INPUT), low_memory=False)
    print(f"  원시 {len(df):,}건")

    df = basic_preprocessing(df)
    df = assign_family_id(df)
    fam = deduplicate_families(df)
    fam.to_csv(cfg.P(cfg.FAMILY_CLEANED), index=False, encoding="utf-8-sig")
    print(f"\n💾 저장: {cfg.FAMILY_CLEANED} ({len(fam):,}건)")

    seeds = build_seed_candidates(fam)
    seeds.to_csv(cfg.P(cfg.SEED_CANDIDATES), index=False, encoding="utf-8-sig")
    print(f"💾 저장: {cfg.SEED_CANDIDATES} ({len(seeds):,}건)")
    print("\n다음 단계 → 이 seed 후보를 LLM으로 진성/노이즈 분류해 final_seed.csv 생성 (워크플로우 §3)")
