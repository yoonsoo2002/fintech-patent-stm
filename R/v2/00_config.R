# ==============================================================================
#  00_config.R  —  유럽데이터(v2) R 단계 중앙 설정 (스크립트 04~07 공용)
# ==============================================================================
#  R 단계(STM·국가갭·TLC·Gompertz)가 공유하는 경로·파라미터를 한 곳에 모은다.
#  v1에서는 setwd 경로가 스크립트마다 제각각(<project-root>/data vs 논문작성/...)이라
#  read.csv가 가장 먼저 깨졌다. → 여기 WORK_DIR 하나만 맞추면 전부 따라온다.
#
#  [사용법]  각 R 스크립트 맨 위에서:
#      source("00_config.R");  source("00_palette.R")
# ==============================================================================

# ── 작업 폴더 (데이터 CSV·RData·PNG가 모두 여기 모이게) ────────────────────
# ★ 내일 시작 시 이 한 줄만 본인 PC 경로로 맞추면 된다.
WORK_DIR <- here::here("data")  # was: absolute local path
setwd(WORK_DIR)
cat("작업폴더:", getwd(), "\n")

# ── 입출력 파일명 (Python 단계 산출물과 일치해야 함) ───────────────────────
FILE_FILTERED   <- "final_fintech_patents_filtered.csv"   # ★STM 입력 (02 산출)
FILE_SCORES     <- "full_patents_with_scores.csv"        # 노이즈 토픽·필터검증용
FILE_WORKSPACE  <- "STM_full_workspace_Phase0.RData"      # 04 산출 → 05·06·07이 load
FILE_COMBINED   <- "STM_TLC_TMR_Combined_Table.csv"      # 06 생성(TMR=NA) → 07이 채움

# ── ★결정 게이트 ④  STM 파라미터 (K=20 유지 강력 권장) ─────────────────────
# K를 바꾸면 모든 1:20 루프와 길이20 라벨 벡터를 전부 고쳐야 한다 → 작업량 큼. 유지 권장.
K_TOPICS    <- 20
SEED_NUM    <- 2026          # set.seed 재현성 시드 (estimateEffect/bootstrap 공통)
MAX_EM_ITS  <- 150
INIT_TYPE   <- "Spectral"
# prevalence 공변량 공식 (Origin_Country + 연도 스플라인 + 시간보정 피인용)
PREVALENCE_FORMULA <- ~ Origin_Country + s(Publication.Year) + C_adj_i

# lower.thresh = 어휘 최소 등장 문서수 컷(≈전체의 1%). v2는 문서수가 달라지므로 비율로 재계산.
# 04 스크립트가 nrow 기준 1%로 자동 계산하지만, 수동 고정하려면 아래 값을 쓴다.
LOWER_THRESH_RATIO <- 0.01   # 전체 문서의 1%
LOWER_THRESH_FIXED <- NA     # 숫자를 넣으면 ratio 대신 이 값 사용 (예: 214)

# ── ★결정 게이트 ①②  국가·기준국·연도윈도우 ──────────────────────────────
COUNTRIES   <- c("US", "EP", "CN", "JP", "KR")   # 분석 국가 블록 (config.py와 동일)
REF_COUNTRY <- "US"                              # estimateEffect 기준국 (relevel ref)
COMPARE_COUNTRIES <- c("KR", "CN", "JP", "EP")   # US 대비 비교 대상 (히트맵 열)
YEAR_START  <- 2019
YEAR_END    <- 2025                              # config.py END_YEAR 와 반드시 일치!
YEAR_SEQ    <- YEAR_START:YEAR_END

# TLC 모멘텀 평가시점 (경계 오염 제외한 'Inner' 구간이 본문 채택값)
# 전체연도가 2019:2025면 Inner = 2020:2024 (양끝 1년씩 제외). END_YEAR 바뀌면 같이 조정.
MOMENTUM_INNER <- (YEAR_START + 1):(YEAR_END - 1)

# ── 그림 품질 (고화질 통일) ────────────────────────────────────────────────
FIG_DPI <- 450
FIG_BG  <- "white"

# ── 다중비교(FDR) ──────────────────────────────────────────────────────────
# 비교 수 = K_TOPICS × length(COMPARE_COUNTRIES). 히트맵 부제에 자동 반영(하드코딩 금지).
N_COMPARISONS <- K_TOPICS * length(COMPARE_COUNTRIES)

cat(sprintf("설정 로드 완료 | K=%d, 연도 %d-%d, 국가 %s, 비교수 %d, dpi=%d\n",
            K_TOPICS, YEAR_START, YEAR_END, paste(COUNTRIES, collapse="/"),
            N_COMPARISONS, FIG_DPI))
