# ==============================================================================
#  00_config.R  —  R 단계 중앙 설정 (스크립트 04~07 공용)
# ==============================================================================
#  R 단계(STM·국가갭·TLC·Gompertz)가 공유하는 경로·파라미터를 한 곳에 모은다.
#  v1에서는 setwd 경로가 스크립트마다 제각각(<project-root>/data vs 논문작성/...)이라
#  read.csv가 가장 먼저 깨졌다. → 여기 WORK_DIR 하나만 맞추면 전부 따라온다.
#
#  [사용법]  각 R 스크립트 맨 위의 두 줄이 이 파일과 00_palette.R을 스크립트 폴더 기준으로 읽는다.
#  저장소 루트에서 `Rscript code/R/04_STM_학습.R`처럼 실행해도, RStudio에서 source()해도 된다.
# ==============================================================================

# ── 작업 폴더 (데이터 CSV·RData·PNG가 모두 여기 모이게) ────────────────────
# 기본값은 저장소의 data/. 데이터가 다른 곳에 있으면 환경변수 FINTECH_DATA_DIR로 지정한다.
#   예) Sys.setenv(FINTECH_DATA_DIR = "D:/fintech/6_데이터")
CODE_DIR <- getOption("fintech.code_dir", getwd())
WORK_DIR <- Sys.getenv("FINTECH_DATA_DIR",
                       unset = normalizePath(file.path(CODE_DIR, "..", "..", "data"), mustWork = FALSE))
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

# lower.thresh = 어휘 최소 등장 문서수 컷(≈전체의 1%). 문서수가 바뀌면 비율로 재계산.
# 04 스크립트가 nrow 기준 1%로 자동 계산하지만, 수동 고정하려면 아래 값을 쓴다.
LOWER_THRESH_RATIO <- 0.01   # 전체 문서의 1%
LOWER_THRESH_FIXED <- NA     # 숫자를 넣으면 ratio 대신 이 값 사용 (예: 214)

# ── ★결정 게이트 ①②  국가·기준국·연도윈도우 ──────────────────────────────
COUNTRIES   <- c("US", "EP", "CN", "JP", "KR")   # 분석 국가 블록 (config.py와 동일)
REF_COUNTRY <- "US"                              # estimateEffect 기준국 (relevel ref)
COMPARE_COUNTRIES <- c("KR", "CN", "JP", "EP")   # US 대비 비교 대상 (히트맵 열)
YEAR_START  <- 2019
YEAR_END    <- 2025                              # config.py END_YEAR 와 반드시 일치!
YEAR_SEQ    <- YEAR_START:YEAR_END               # Gompertz·연도별 표 윈도우 (2026 공개분 = 부분연도라 제외)

# STM 입력에 실제로 들어 있는 마지막 공개연도. s(Publication.Year) B-Spline의 경계 knot이
# 데이터 범위(YEAR_START ~ DATA_YEAR_END)로 잡히므로, 모멘텀 Inner 구간은 이 값 기준이다.
# 데이터에 2026 공개분이 있어 경계 knot = 2019/2026.
DATA_YEAR_END <- 2026

# TLC 모멘텀 평가시점 (경계 오염 제외한 'Inner' 구간이 본문 채택값, B-2 결정)
# 경계 knot 2019/2026 → Inner = 2020:2025 (정수 6개 연도). 06이 경계 knot과 대조해 경고한다.
MOMENTUM_INNER <- (YEAR_START + 1):(DATA_YEAR_END - 1)

# ── 토픽 상관 네트워크 (10 스크립트) ──────────────────────────────────────
# NET_EDGE_RULE:
#   "posadj" : (기본값 = v1) topicCorr(cutoff=NET_CORR_CUTOFF)$posadj, 즉 r > 0.01 인 양의 상관 쌍을 모두 연결.
#              v1 '네트워크 시각화 R 코드'는 posadj(0/1)에 0.15 컷을 걸어 사실상 컷이 없었다
#              → v1 매개중심성 T15 0.290 / T7 0.276 / T4 0.236 (엣지 43개)은 이 규칙에서 나온다.
#   "cor"    : 대안. topicCorr()$cor >= NET_THRESHOLD 인 쌍만 연결(논문에는 쓰지 않음).
NET_EDGE_RULE   <- "posadj"
NET_CORR_CUTOFF <- 0.01   # stm::topicCorr 기본값
NET_THRESHOLD   <- 0.15   # "cor" 규칙에서만 사용

# ── 그림 품질 (고화질 통일) ────────────────────────────────────────────────
FIG_DPI <- 450
FIG_BG  <- "white"

# ── 다중비교(FDR) ──────────────────────────────────────────────────────────
# 비교 수 = K_TOPICS × length(COMPARE_COUNTRIES). 히트맵 부제에 자동 반영(하드코딩 금지).
N_COMPARISONS <- K_TOPICS * length(COMPARE_COUNTRIES)

cat(sprintf("설정 로드 완료 | K=%d, 연도 %d-%d, 국가 %s, 비교수 %d, dpi=%d\n",
            K_TOPICS, YEAR_START, YEAR_END, paste(COUNTRIES, collapse="/"),
            N_COMPARISONS, FIG_DPI))
