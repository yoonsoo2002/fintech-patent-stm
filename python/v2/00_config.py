"""
================================================================================
 00_config.py  —  유럽데이터(v2) 파이프라인 중앙 설정 (Python 단계 01·02·03·08 공용)
================================================================================
 이 파일 하나만 고치면 Python 쪽 전 스크립트(정제·시드·임베딩·필터·검증·EDA)가
 같은 값을 쓴다. v1에서는 이 값들이 embedding.py / 임베딩성능평가.py / basic.ipynb 에
 제각각 하드코딩돼 "한 곳만 고치면 다른 데서 어긋나는" 문제가 있었다. → 여기로 통일.

 [사용법]  각 스크립트 맨 위에서:
     from importlib import util as _u
     _s = _u.spec_from_file_location("cfg", "00_config.py"); cfg = _u.module_from_spec(_s); _s.loader.exec_module(cfg)
   또는 같은 폴더라면:  import importlib, sys; sys.path.insert(0, "."); import config as cfg
   (Colab이면 이 파일을 작업폴더에 올리고  exec(open('00_config.py').read())  도 가능)

 ★ 내일 시작할 때 가장 먼저 확인할 4가지 = "결정 게이트". 마스터 워크플로우 §0 참조.
================================================================================
"""

import os
from datetime import datetime

# ──────────────────────────────────────────────────────────────────────────
# 0) 실행 환경 / 경로
# ──────────────────────────────────────────────────────────────────────────
# Colab이면 True (구글드라이브 마운트), 로컬 PC면 False
USE_COLAB = False

if USE_COLAB:
    # 구글드라이브에 '핀테크_유럽' 폴더를 만들고 그 안에서 작업한다고 가정
    BASE_PATH = "/content/drive/MyDrive/핀테크_유럽"
else:
    # 로컬 PC에서 돌릴 때 (데이터·코드 같은 폴더). 백슬래시 대신 '/' 사용 권장.
    BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "data")  # was: absolute local path

def P(filename: str) -> str:
    """BASE_PATH 기준 절대경로를 만들어 준다."""
    return os.path.join(BASE_PATH, filename)

# ──────────────────────────────────────────────────────────────────────────
# 1) 입출력 파일명 (v1과 동일한 스키마를 유지 — R 단계가 이 이름에 의존)
# ──────────────────────────────────────────────────────────────────────────
RAW_INPUT          = "merged_3files.csv"                    # Lens 원시 통합본 (사용자가 준비)
FAMILY_CLEANED     = "fintech_patents_family_cleaned.csv"   # 01 산출: 패밀리 정제본
SEED_CANDIDATES    = "seed_patents_top600.csv"             # 01 산출: 피인용 상위 시드후보
FINAL_SEED         = "final_seed.csv"                       # (외부 LLM 분류 후) 시드 라벨본
FILTERED_OUTPUT    = "final_fintech_patents_filtered.csv"   # 02 산출: ★STM 입력 (진성만)
SCORES_OUTPUT      = "full_patents_with_scores.csv"        # 02 산출: 전체+점수
POS_VECS_NPY       = "pos_seeds_vectors.npy"
NEG_VECS_NPY       = "neg_seeds_vectors.npy"
FULL_VECS_NPY      = "full_patents_vectors.npy"
VALIDATION_OUTPUT  = "Semantic_Validation_Metrics.csv"      # 03 산출: 검증표(부록 A-2)

# ──────────────────────────────────────────────────────────────────────────
# 2) ★결정 게이트 ①  EP(유럽) 처리방안  ← "유럽 데이터를 신경써서"의 핵심
# ──────────────────────────────────────────────────────────────────────────
# v1 문제: 원산지(Origin_Country)를 '최초 우선권 번호 앞 2글자'로 잡는데, 유럽 출원은
#          DE/FR/GB 등 회원국 코드이거나 EP로 흩어져 EP 비중이 1.4%로 과소→일반화 불가.
#
# EUROPE_MODE 옵션:
#   'aggregate' : (권장) EPO 회원국 코드 + EP를 전부 'EP' 한 블록으로 합쳐 유럽을 살린다.
#   'strict'    : v1 그대로. 우선권이 literal 'EP'인 것만 EP. (유럽 과소대표 유지)
#   'exclude'   : EP/유럽 전부 제외, 4개국(US·CN·JP·KR)만 분석.
EUROPE_MODE = "aggregate"

# 분석에 쓸 '국가(블록)' 집합. aggregate면 회원국이 EP로 접혀 들어오므로 이 5개로 유지.
IP5_CODES = {"US", "EP", "CN", "JP", "KR"}

# EPO 회원국/확장국 2글자 코드 (aggregate 모드에서 이들을 모두 'EP'로 본다)
EUROPE_CODES = {
    "EP",  # EPO 자체
    "DE", "FR", "GB", "NL", "CH", "SE", "IT", "ES", "FI", "BE",
    "AT", "DK", "NO", "IE", "PL", "PT", "CZ", "HU", "GR", "RO",
    "TR", "LU", "SK", "SI", "BG", "HR", "LT", "LV", "EE", "CY",
    "MT", "IS", "LI", "MC", "SM", "MK", "AL", "RS",
}

def resolve_country(code: str) -> str:
    """우선권 2글자 코드를 분석용 국가 블록으로 정규화한다."""
    if code is None:
        return None
    c = str(code).strip().upper()
    if EUROPE_MODE == "aggregate" and c in EUROPE_CODES:
        return "EP"
    if EUROPE_MODE == "exclude" and (c in EUROPE_CODES):
        return None  # 유럽 제외
    return c if c in IP5_CODES else None

# ──────────────────────────────────────────────────────────────────────────
# 3) ★결정 게이트 ②  2025·2026년 데이터 포함 여부
# ──────────────────────────────────────────────────────────────────────────
# 특허는 출원~공개까지 ~18개월 블라인드 → 최근연도 과소추정. 시계열/TLC/Gompertz 자유도와 직결.
#   - 보수적으로 가려면 END_YEAR=2024
#   - 포함하되 각주(publication lag) 달려면 END_YEAR=2025 (또는 2026)
START_YEAR = 2019
END_YEAR   = 2025      # ← 결정해서 바꾸기. Gompertz는 (END-START+1) 시점 자유도를 가짐.

# 기간 3분할 (시드 후보를 시기별로 균형 추출하기 위함). END_YEAR에 맞춰 마지막 구간 조정.
PERIOD_BREAKS = {
    "Phase1_AI_Blockchain_2019_2021":   (2019, 2021),
    "Phase2_LLM_Inflection_2022_2023":  (2022, 2023),
    "Phase3_GenAI_OpenBanking_2024_on": (2024, END_YEAR),
}

# ──────────────────────────────────────────────────────────────────────────
# 4) ★결정 게이트 ③  시드 선정 파라미터
# ──────────────────────────────────────────────────────────────────────────
TOP_N          = 200                       # 기간별 피인용 상위 N개 → 시드 후보 (3구간 × 200 = 600)
REFERENCE_DATE = datetime(END_YEAR + 1, 3, 1)   # 경과연수 계산 기준일 (말단연도+1년 3월)
MIN_YEARS      = 0.5                        # 최소 경과연수 (피인용지수 폭발 방지)

# ──────────────────────────────────────────────────────────────────────────
# 5) ★결정 게이트 ④  임베딩/필터링 (K=3, PaECTER 고정 — 바꾸지 말 것 권장)
# ──────────────────────────────────────────────────────────────────────────
PAECTER_MODEL = "mpi-inno-comp/paecter"    # HuggingFace 특허 임베딩 모델
MAX_LENGTH    = 512                         # 토큰 절단 길이
BATCH_SEED    = 32                          # 시드 임베딩 배치
BATCH_FULL    = 64                          # 전체 임베딩 배치
KNN_K         = 3                           # Top-K 평균 비교의 K (임베딩·검증 동일해야 함)
TEXT_SEP      = " [SEP] "                   # Title [SEP] Abstract 결합자 (임베딩은 Title 1회)

# ──────────────────────────────────────────────────────────────────────────
# 6) 컬럼 스키마 (Lens 원본 컬럼명 — 유럽 데이터도 같은 이름이어야 함. 다르면 여기서 매핑)
# ──────────────────────────────────────────────────────────────────────────
COL_TITLE     = "Title"
COL_ABSTRACT  = "Abstract"
COL_PRIORITY  = "Priority Numbers"
COL_FAMILY    = "Extended Family Members"
COL_JUR       = "Jurisdiction"
COL_APPDATE   = "Application Date"
COL_PUBYEAR   = "Publication Year"
COL_CITES     = "Cited by Patent Count"
COL_APPLICANT = "Applicants"

# ──────────────────────────────────────────────────────────────────────────
# 7) 그림 품질 (고화질 통일)
# ──────────────────────────────────────────────────────────────────────────
FIG_DPI = 450          # 모든 그림 저장 dpi (고화질). 600까지 올려도 됨.
FIG_BG  = "white"

# ──────────────────────────────────────────────────────────────────────────
def print_config():
    print("="*70)
    print(" [설정 확인] 유럽데이터 v2 파이프라인")
    print("="*70)
    print(f"  실행환경      : {'Colab' if USE_COLAB else '로컬'}  / BASE_PATH={BASE_PATH}")
    print(f"  EUROPE_MODE   : {EUROPE_MODE}   (국가블록={sorted(IP5_CODES)})")
    print(f"  분석기간      : {START_YEAR} ~ {END_YEAR}")
    print(f"  시드          : 구간별 TOP_N={TOP_N} (기준일 {REFERENCE_DATE:%Y-%m-%d})")
    print(f"  임베딩        : {PAECTER_MODEL}  K={KNN_K}  maxlen={MAX_LENGTH}")
    print(f"  그림          : dpi={FIG_DPI}")
    print("="*70)

if __name__ == "__main__":
    print_config()
