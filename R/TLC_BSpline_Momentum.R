# ================================================================
# TLC_BSpline_Momentum.R
# 목적: STM B-Spline 1차 도함수 기반 TLC 성장 모멘텀 산출
# 작성 시작일: 2026-05-12
# 
# 사전 조건:
#   - Fintech_Project.R 또는 STM 워크스페이스에서 다음 객체 로드 필요
#     * final_stm_20_adj  (STM 모델)
#     * out3              (STM 입력 메타데이터)
#     * effect_country_US (estimateEffect 결과)
# ================================================================

# ----------------------------------------------------------------
# [0] 작업 디렉토리 및 라이브러리
# ----------------------------------------------------------------
setwd(here::here("data"))  # was: absolute local path
getwd()

library(stm)
library(dplyr)
library(tidyverse)
library(splines)


# ----------------------------------------------------------------
# [1] 워크스페이스 로드 — RData 파일이 있을 경우
# ----------------------------------------------------------------
# 폴더 안 RData 파일 확인
list.files(pattern = "\\.RData$")

# 아래 파일명은 실제 존재하는 RData로 수정해서 사용
load("STM_full_workspace_Phase0.RData")


# ----------------------------------------------------------------
# [2] 객체 존재 확인
# ----------------------------------------------------------------
cat("=== 객체 존재 여부 ===\n")
cat("final_stm_20_adj  :", exists("final_stm_20_adj"), "\n")
cat("out3              :", exists("out3"), "\n")
cat("effect_country_US :", exists("effect_country_US"), "\n")

# ----------------------------------------------------------------
# [추가] effect_country_US 재산출
# ----------------------------------------------------------------
library(stm)

# Origin_Country 재설정 (혹시 모르니 안전장치)
out3$meta$Origin_Country <- as.factor(out3$meta$Origin_Country)
out3$meta$Origin_Country <- relevel(out3$meta$Origin_Country, ref = "US")

# estimateEffect 실행 — 몇 분 소요됨
cat("=== estimateEffect 시작 ===\n")
cat("시작 시각:", format(Sys.time(), "%H:%M:%S"), "\n")

set.seed(2026)
effect_country_US <- estimateEffect(
  1:20 ~ Origin_Country + s(Publication.Year) + C_adj_i,
  stmobj = final_stm_20_adj,
  metadata = out3$meta,
  uncertainty = "Global"
)

cat("종료 시각:", format(Sys.time(), "%H:%M:%S"), "\n")
cat("✅ effect_country_US 산출 완료\n")

# 산출 확인
exists("effect_country_US")
class(effect_country_US)
length(effect_country_US$parameters)  # 20이 나와야 함

# 워크스페이스 백업 (다음번에 재산출 안 하도록)
save.image("STM_full_workspace_Phase0.RData")
cat("✅ 워크스페이스 백업 완료\n")

# ================================================================
# [3] B-Spline 구조 진단
# ================================================================

# ----------------------------------------------------------------
# 3-1. parameters 최상위 구조
# ----------------------------------------------------------------
cat("\n========== [3-1] parameters 최상위 구조 ==========\n")
cat("길이 (= 토픽 수):", length(effect_country_US$parameters), "\n")
cat("첫 토픽 내부 길이 (= 시뮬레이션 수):", 
    length(effect_country_US$parameters[[1]]), "\n")


# ----------------------------------------------------------------
# 3-2. 첫 토픽, 첫 시뮬레이션 결과의 구조
# ----------------------------------------------------------------
cat("\n========== [3-2] T1 첫 시뮬레이션 결과 구조 ==========\n")
str(effect_country_US$parameters[[1]][[1]], max.level = 2)

cat("\n--- T1 첫 시뮬레이션의 이름들 ---\n")
print(names(effect_country_US$parameters[[1]][[1]]))


# ----------------------------------------------------------------
# 3-3. est (계수 벡터) 직접 확인
# ----------------------------------------------------------------
cat("\n========== [3-3] T1 첫 시뮬레이션의 est (계수 벡터) ==========\n")
print(effect_country_US$parameters[[1]][[1]]$est)
cat("\n계수 벡터 길이:", length(effect_country_US$parameters[[1]][[1]]$est), "\n")


# ----------------------------------------------------------------
# 3-4. 회귀 테이블 (계수 이름 식별)
# ----------------------------------------------------------------
cat("\n========== [3-4] summary 결과의 회귀 테이블 (T1) ==========\n")
sum_eff <- summary(effect_country_US, topics = 1)
print(sum_eff$tables[[1]])


# ----------------------------------------------------------------
# 3-5. Publication.Year 분포 (분석 기간 중간점 결정용)
# ----------------------------------------------------------------
cat("\n========== [3-5] Publication.Year 분포 ==========\n")
print(table(out3$meta$Publication.Year, useNA = "always"))
cat("\n범위:", range(as.numeric(as.character(out3$meta$Publication.Year)), na.rm = TRUE), "\n")
cat("중앙값:", median(as.numeric(as.character(out3$meta$Publication.Year)), na.rm = TRUE), "\n")
cat("평균:", round(mean(as.numeric(as.character(out3$meta$Publication.Year)), na.rm = TRUE), 2), "\n")


# ----------------------------------------------------------------
# 3-6. modelframe 또는 design matrix 확인 (있을 경우)
# ----------------------------------------------------------------
cat("\n========== [3-6] estimateEffect 최상위 구조 ==========\n")
cat("--- 최상위 names ---\n")
print(names(effect_country_US))

cat("\n--- modelframe이 있는지 ---\n")
if ("modelframe" %in% names(effect_country_US)) {
  cat("modelframe 존재! 컬럼명:\n")
  print(colnames(effect_country_US$modelframe))
  cat("\nhead:\n")
  print(head(effect_country_US$modelframe, 3))
} else {
  cat("modelframe 없음. 다른 곳에서 design matrix 찾아야 함.\n")
  
  # ================================================================
  # [4] B-Spline 1차 도함수 산출 — 핵심 함수 정의
  # ================================================================
  
  library(splines)
  
  # ----------------------------------------------------------------
  # 4-1. STM 내부와 동일한 B-Spline basis 재구성
  # ----------------------------------------------------------------
  # STM의 s() 함수는 splines::bs(x, df=10)을 호출하지만, 
  # 결과 컬럼이 7개로 나온 걸 보면 df=7로 추정됨.
  # 정확히 일치시키려면 STM 내부 호출을 따라가야 함.
  
  # Publication.Year 가져오기 (NA 제거)
  pub_year <- as.numeric(as.character(out3$meta$Publication.Year))
  pub_year_clean <- pub_year[!is.na(pub_year)]
  
  cat("=== Publication.Year 정보 ===\n")
  cat("범위:", range(pub_year_clean), "\n")
  cat("길이:", length(pub_year_clean), "\n\n")
  
  
  # ----------------------------------------------------------------
  # 4-2. STM이 사용한 정확한 basis 재현 시도
  # ----------------------------------------------------------------
  # stm::s() 함수의 내부 로직을 확인
  cat("=== stm::s 함수 정의 확인 ===\n")
  print(stm:::s)
}

# ================================================================
# [4] modelframe의 spline basis 속성 추출
# ================================================================

# ----------------------------------------------------------------
# 4-1. modelframe 구조 재확인 + spline basis attributes 확인
# ----------------------------------------------------------------
mf <- effect_country_US$modelframe

cat("=== modelframe 차원 ===\n")
cat("nrow:", nrow(mf), " ncol:", ncol(mf), "\n\n")

cat("=== modelframe의 attr들 ===\n")
print(names(attributes(mf)))

# spline 컬럼은 보통 'matrix' class를 가지며 knots, degree 등이 attr로 붙어있음
cat("\n=== s(Publication.Year) 컬럼의 attr ===\n")
spline_col <- mf$`s(Publication.Year)`
cat("class:", class(spline_col), "\n")
cat("dim:", dim(spline_col), "\n")
cat("\n--- attributes ---\n")
print(attributes(spline_col))


# ----------------------------------------------------------------
# 4-2. predict()로 정확한 basis 재구성 (knots 자동 일치)
# ----------------------------------------------------------------
# splines::bs()는 attributes를 이용해 동일한 basis를 재생성 가능
# predict.bs() 또는 predict(bs_object, newx) 활용

cat("\n=== 임의의 year에서 basis 재구성 테스트 ===\n")

# 새 연도 시퀀스 (2019~2026)
year_seq <- seq(2019, 2026, by = 0.5)
cat("year_seq:", year_seq, "\n\n")

# spline_col의 attr를 이용해 새 basis 생성
# predict 메서드가 자동으로 동일 knots 사용
new_basis <- predict(spline_col, newx = year_seq)
cat("new_basis 차원:", dim(new_basis), "\n")
cat("new_basis 첫 3행:\n")
print(head(new_basis, 3))


# ----------------------------------------------------------------
# 4-3. 도함수 산출 — derivs=1 옵션
# ----------------------------------------------------------------
cat("\n=== 1차 도함수 basis 산출 ===\n")

# splines::bs는 deriv 옵션을 직접 지원하지 않지만, 
# bs() 호출 시 derivs 인자가 있는지 확인 또는 수치 미분으로 처리

# 시도 1: predict에 deriv 인자 (있을 수도)
tryCatch({
  deriv_basis <- predict(spline_col, newx = year_seq, deriv = 1)
  cat("✅ predict(..., deriv=1) 작동!\n")
  cat("deriv_basis 차원:", dim(deriv_basis), "\n")
  cat("첫 3행:\n")
  print(head(deriv_basis, 3))
}, error = function(e) {
  cat("❌ deriv=1 옵션 실패:", conditionMessage(e), "\n")
  cat("→ 수치 미분으로 진행 필요\n")
})


# ----------------------------------------------------------------
# 4-4. 수치 미분 (안전장치, deriv 옵션이 안 될 경우)
# ----------------------------------------------------------------
cat("\n=== 수치 미분 테스트 (ε=0.01) ===\n")

eps <- 0.01
year_mid <- 2023

basis_plus  <- predict(spline_col, newx = year_mid + eps)
basis_minus <- predict(spline_col, newx = year_mid - eps)
basis_deriv_numeric <- (basis_plus - basis_minus) / (2 * eps)

cat("수치 미분 결과 (year=2023):\n")
print(basis_deriv_numeric)

# ================================================================
# [4-재시작] splines 로드된 상태에서 spline_col 정보 추출
# ================================================================

library(splines)

mf <- effect_country_US$modelframe
spline_col <- mf$`s(Publication.Year)`

# ----------------------------------------------------------------
# 4-1. spline_col의 class와 attr 확인
# ----------------------------------------------------------------
cat("=== spline_col class ===\n")
print(class(spline_col))

cat("\n=== spline_col attr 이름들 ===\n")
print(names(attributes(spline_col)))

cat("\n=== degree ===\n")
print(attr(spline_col, "degree"))

cat("\n=== knots ===\n")
print(attr(spline_col, "knots"))

cat("\n=== Boundary.knots ===\n")
print(attr(spline_col, "Boundary.knots"))

cat("\n=== intercept ===\n")
print(attr(spline_col, "intercept"))


# ----------------------------------------------------------------
# 4-2. predict로 도함수 산출 가능 여부 테스트
# ----------------------------------------------------------------
cat("\n=== predict(deriv=1) 작동 테스트 ===\n")

tryCatch({
  deriv_test <- predict(spline_col, newx = 2023, deriv = 1)
  cat("✅ predict(..., deriv=1) 작동!\n")
  cat("year=2023에서의 basis 도함수 (7개):\n")
  print(deriv_test)
}, error = function(e) {
  cat("❌ predict(deriv=1) 실패:", conditionMessage(e), "\n")
  cat("→ 수치 미분 방식으로 대체합니다.\n")
})


# ----------------------------------------------------------------
# 4-3. 수치 미분 (안전망)
# ----------------------------------------------------------------
cat("\n=== 수치 미분 결과 (year=2023, ε=0.001) ===\n")

eps <- 0.001
year_mid <- 2023

basis_plus  <- predict(spline_col, newx = year_mid + eps)
basis_minus <- predict(spline_col, newx = year_mid - eps)
basis_deriv_numeric <- (basis_plus - basis_minus) / (2 * eps)

cat("year=2023에서의 basis 도함수 (수치):\n")
print(basis_deriv_numeric)

# ================================================================
# [4-우회] bs() 직접 호출로 도함수 산출
# ================================================================

library(splines)

mf <- effect_country_US$modelframe
spline_col <- mf$`s(Publication.Year)`

# ----------------------------------------------------------------
# 4-1. spline_col의 attr 추출
# ----------------------------------------------------------------
knots_internal <- attr(spline_col, "knots")
knots_boundary <- attr(spline_col, "Boundary.knots")
spline_degree  <- attr(spline_col, "degree")
spline_intercept <- attr(spline_col, "intercept")

cat("=== STM이 사용한 B-Spline 파라미터 ===\n")
cat("degree         :", spline_degree, "\n")
cat("internal knots :", knots_internal, "\n")
cat("boundary knots :", knots_boundary, "\n")
cat("intercept      :", spline_intercept, "\n")


# ----------------------------------------------------------------
# 4-2. bs() 직접 호출로 basis 재구성 — predict 우회
# ----------------------------------------------------------------
cat("\n=== year=2023에서 basis 재구성 (직접 bs 호출) ===\n")

basis_at_2023 <- bs(2023, 
                    knots = knots_internal,
                    Boundary.knots = knots_boundary,
                    degree = spline_degree,
                    intercept = spline_intercept)

cat("basis(2023):\n")
print(as.numeric(basis_at_2023))


# ----------------------------------------------------------------
# 4-3. 수치 미분으로 도함수 산출 — predict 우회
# ----------------------------------------------------------------
cat("\n=== year=2023에서 수치 미분 도함수 ===\n")

eps <- 0.001
year_mid <- 2023

basis_plus <- bs(year_mid + eps,
                 knots = knots_internal,
                 Boundary.knots = knots_boundary,
                 degree = spline_degree,
                 intercept = spline_intercept)

basis_minus <- bs(year_mid - eps,
                  knots = knots_internal,
                  Boundary.knots = knots_boundary,
                  degree = spline_degree,
                  intercept = spline_intercept)

basis_deriv <- (as.numeric(basis_plus) - as.numeric(basis_minus)) / (2 * eps)

cat("basis 도함수(year=2023) — 7개 값:\n")
print(basis_deriv)

cat("\n부호 분포 — 음수/양수 섞여있어야 정상:\n")
cat("양수 개수:", sum(basis_deriv > 0), "\n")
cat("음수 개수:", sum(basis_deriv < 0), "\n")
cat("0 개수   :", sum(basis_deriv == 0), "\n")


# ----------------------------------------------------------------
# 4-4. 검증 — bs 직접 호출 결과와 modelframe의 실제 값 일치 확인
# ----------------------------------------------------------------
cat("\n=== 검증: bs 직접 호출 vs modelframe 실제값 ===\n")
cat("modelframe 1행의 Publication.Year:", 
    as.numeric(as.character(out3$meta$Publication.Year))[1], "\n")
cat("modelframe 1행의 basis 값:\n")
print(as.numeric(spline_col[1, ]))

# 그 연도에서 bs로 재구성
year_1 <- as.numeric(as.character(out3$meta$Publication.Year))[1]
basis_reconstructed <- bs(year_1,
                          knots = knots_internal,
                          Boundary.knots = knots_boundary,
                          degree = spline_degree,
                          intercept = spline_intercept)
cat("bs로 재구성한 basis 값:\n")
print(as.numeric(basis_reconstructed))

cat("\n두 값이 일치하면 ✅ — 도함수 산출 신뢰성 확보\n")

# ================================================================
# [5] 토픽 20개 × B-Spline 모멘텀 산출 (year=2023)
# ================================================================

library(splines)
library(dplyr)
library(tidyverse)

# ----------------------------------------------------------------
# 5-1. 평가 시점 설정
# ----------------------------------------------------------------
year_eval <- 2023   # 분석 기간 중앙값
eps       <- 0.001  # 수치 미분 ε

cat("===========================================\n")
cat("  B-Spline 모멘텀 산출\n")
cat("  평가 시점: year =", year_eval, "\n")
cat("===========================================\n\n")


# ----------------------------------------------------------------
# 5-2. year_eval에서 basis 도함수 산출 (모든 토픽 공통)
# ----------------------------------------------------------------
basis_plus <- bs(year_eval + eps,
                 knots = knots_internal,
                 Boundary.knots = knots_boundary,
                 degree = spline_degree,
                 intercept = spline_intercept)

basis_minus <- bs(year_eval - eps,
                  knots = knots_internal,
                  Boundary.knots = knots_boundary,
                  degree = spline_degree,
                  intercept = spline_intercept)

basis_deriv <- as.numeric((basis_plus - basis_minus) / (2 * eps))

cat("year=", year_eval, "에서의 basis 도함수 (7개):\n", sep = "")
print(round(basis_deriv, 4))


# ----------------------------------------------------------------
# 5-3. 계수 벡터에서 s(Publication.Year) basis 7개 인덱스 식별
# ----------------------------------------------------------------
# 계수 이름: (Intercept), Origin_CountryCN/EP/JP/KR, 
#            s(Publication.Year)1~7, C_adj_i
# 따라서 spline coefficient는 index 6:12

coef_names <- names(effect_country_US$parameters[[1]][[1]]$est)
spline_idx <- grep("^s\\(Publication\\.Year\\)", coef_names)
cat("\n=== s(Publication.Year) coefficient 인덱스 ===\n")
cat("인덱스:", spline_idx, "\n")
cat("이름들:\n")
print(coef_names[spline_idx])


# ----------------------------------------------------------------
# 5-4. 토픽 20개 × 시뮬레이션 25회 → B-Spline 모멘텀
# ----------------------------------------------------------------
n_topics <- length(effect_country_US$parameters)
n_sims   <- length(effect_country_US$parameters[[1]])

cat("\n=== 산출 대상 ===\n")
cat("토픽 수      :", n_topics, "\n")
cat("시뮬레이션 수:", n_sims, "\n\n")

# 각 토픽 × 시뮬레이션의 모멘텀 매트릭스
momentum_sims <- matrix(NA, nrow = n_topics, ncol = n_sims)

for (k in 1:n_topics) {
  for (s in 1:n_sims) {
    est_k_s <- effect_country_US$parameters[[k]][[s]]$est
    spline_coef <- est_k_s[spline_idx]      # 길이 7
    momentum_sims[k, s] <- sum(spline_coef * basis_deriv)
  }
}

# 시뮬레이션 평균 (점추정) + 신뢰구간
momentum_mean  <- apply(momentum_sims, 1, mean)
momentum_lower <- apply(momentum_sims, 1, quantile, probs = 0.025)
momentum_upper <- apply(momentum_sims, 1, quantile, probs = 0.975)

# 가독성을 위해 × 100 (기존 OLS와 동일 스케일)
momentum_bspline <- momentum_mean * 100
momentum_lo_100  <- momentum_lower * 100
momentum_up_100  <- momentum_upper * 100


# ----------------------------------------------------------------
# 5-5. 기존 OLS 모멘텀 재계산 (비교용)
# ----------------------------------------------------------------
cat("=== 기존 OLS 모멘텀 재계산 (비교용) ===\n")

theta_mat <- final_stm_20_adj$theta
colnames(theta_mat) <- paste0('T', 1:20)
pub_year <- as.numeric(as.character(out3$meta$Publication.Year))

valid_idx <- !is.na(pub_year)
theta_mat_v <- theta_mat[valid_idx, ]
pub_year_v  <- pub_year[valid_idx]

momentum_ols <- sapply(1:n_topics, function(i) {
  coef(lm(theta_mat_v[, i] ~ pub_year_v))[2]
}) * 100

cat("OLS 모멘텀 산출 완료\n")


# ----------------------------------------------------------------
# 5-6. 종합 비교 테이블 작성
# ----------------------------------------------------------------
topic_label_short <- c(
  "Crypto Exchange", "Mobile POS Pay", "Insurance Claim", "Invoice Audit",
  "Bank Remittance", "Digital Wallet", "Auth & Biometrics", "NFT Rights",
  "Investment Reco", "Settlement Proc", "Enterprise Finance", "Asset Ledger",
  "Credit Risk", "Smart Contract", "Realtime Settle", "E-commerce Fee",
  "AI Detection", "Device Linking", "Charging Txn", "Face/Voice Login"
)

df_compare <- data.frame(
  Topic        = paste0("T", 1:n_topics),
  Label        = topic_label_short,
  OLS_Momentum = round(momentum_ols, 4),
  BSpline_Momentum = round(momentum_bspline, 4),
  BSpline_CI_Lower = round(momentum_lo_100, 4),
  BSpline_CI_Upper = round(momentum_up_100, 4),
  Diff         = round(momentum_bspline - momentum_ols, 4),
  stringsAsFactors = FALSE
)

# 순위 변동 산출
df_compare$Rank_OLS     <- rank(-df_compare$OLS_Momentum)
df_compare$Rank_BSpline <- rank(-df_compare$BSpline_Momentum)
df_compare$Rank_Change  <- df_compare$Rank_OLS - df_compare$Rank_BSpline

cat("\n=== 토픽별 OLS vs B-Spline 모멘텀 비교 ===\n")
print(df_compare)


# ----------------------------------------------------------------
# 5-7. 핵심 인용값 변동 점검 (T9, T14)
# ----------------------------------------------------------------
cat("\n=== 본문 핵심 인용값 변동 점검 ===\n")
cat("--- T9 (Investment Reco / 투자 분석) ---\n")
cat("  OLS     :", df_compare$OLS_Momentum[9], "\n")
cat("  B-Spline:", df_compare$BSpline_Momentum[9], 
    "  CI: [", df_compare$BSpline_CI_Lower[9], ",", df_compare$BSpline_CI_Upper[9], "]\n")
cat("  순위 변화: OLS 순위", df_compare$Rank_OLS[9], 
    "→ B-Spline 순위", df_compare$Rank_BSpline[9], "\n\n")

cat("--- T14 (Smart Contract / 스마트 계약) ---\n")
cat("  OLS     :", df_compare$OLS_Momentum[14], "\n")
cat("  B-Spline:", df_compare$BSpline_Momentum[14],
    "  CI: [", df_compare$BSpline_CI_Lower[14], ",", df_compare$BSpline_CI_Upper[14], "]\n")
cat("  순위 변화: OLS 순위", df_compare$Rank_OLS[14],
    "→ B-Spline 순위", df_compare$Rank_BSpline[14], "\n\n")


# ----------------------------------------------------------------
# 5-8. CSV 저장
# ----------------------------------------------------------------
write.csv(df_compare, 
          "TLC_Momentum_OLS_vs_BSpline_Comparison.csv", 
          row.names = FALSE)
cat("✅ 비교표 저장: TLC_Momentum_OLS_vs_BSpline_Comparison.csv\n")

# ================================================================
# [6] B-Spline 평균 도함수 모멘텀 산출 — 올바른 방식
# ================================================================

library(splines)
library(dplyr)

# ----------------------------------------------------------------
# 6-1. 평가 시점 시퀀스 설정
# ----------------------------------------------------------------
# 분석 기간 전체 (2019~2026)의 정수 연도 8개
year_eval_seq <- 2019:2026

cat("===========================================\n")
cat("  B-Spline 평균 도함수 모멘텀 산출\n")
cat("  평가 시점:", paste(year_eval_seq, collapse=", "), "\n")
cat("  시점 수  :", length(year_eval_seq), "\n")
cat("===========================================\n\n")


# ----------------------------------------------------------------
# 6-2. 각 평가 시점에서 basis 도함수 산출
# ----------------------------------------------------------------
eps <- 0.001
n_basis <- length(knots_internal) + spline_degree   # = 4 + 3 = 7

# 행: 평가 시점, 열: basis 차원
basis_deriv_mat <- matrix(NA, nrow = length(year_eval_seq), ncol = n_basis)

for (i in seq_along(year_eval_seq)) {
  y <- year_eval_seq[i]
  b_plus  <- bs(y + eps, knots = knots_internal,
                Boundary.knots = knots_boundary,
                degree = spline_degree, intercept = spline_intercept)
  b_minus <- bs(y - eps, knots = knots_internal,
                Boundary.knots = knots_boundary,
                degree = spline_degree, intercept = spline_intercept)
  basis_deriv_mat[i, ] <- as.numeric((b_plus - b_minus) / (2 * eps))
}

cat("=== 각 시점별 basis 도함수 ===\n")
rownames(basis_deriv_mat) <- as.character(year_eval_seq)
colnames(basis_deriv_mat) <- paste0("B", 1:n_basis)
print(round(basis_deriv_mat, 4))


# ----------------------------------------------------------------
# 6-3. 토픽 20개 × 시뮬레이션 25회 → 시점별 모멘텀 → 평균
# ----------------------------------------------------------------
n_topics <- length(effect_country_US$parameters)
n_sims   <- length(effect_country_US$parameters[[1]])

# 각 토픽 × 시뮬레이션 → 시점 평균 모멘텀
momentum_avg_sims <- matrix(NA, nrow = n_topics, ncol = n_sims)

for (k in 1:n_topics) {
  for (s in 1:n_sims) {
    est_k_s <- effect_country_US$parameters[[k]][[s]]$est
    spline_coef <- est_k_s[spline_idx]  # 길이 7
    
    # 각 평가 시점에서의 도함수 값
    deriv_at_each_year <- basis_deriv_mat %*% spline_coef
    # ↑ (시점 수 × 7) %*% (7 × 1) = (시점 수 × 1)
    
    # 시점 평균
    momentum_avg_sims[k, s] <- mean(deriv_at_each_year)
  }
}

# 시뮬레이션 평균 + 신뢰구간
momentum_avg_mean  <- apply(momentum_avg_sims, 1, mean) * 100
momentum_avg_lower <- apply(momentum_avg_sims, 1, quantile, probs = 0.025) * 100
momentum_avg_upper <- apply(momentum_avg_sims, 1, quantile, probs = 0.975) * 100


# ----------------------------------------------------------------
# 6-4. 종합 비교 테이블 — OLS vs B-Spline(단일점) vs B-Spline(평균)
# ----------------------------------------------------------------
df_compare2 <- data.frame(
  Topic = paste0("T", 1:n_topics),
  Label = topic_label_short,
  OLS_Momentum        = round(momentum_ols, 4),
  BSpline_2023        = round(momentum_bspline, 4),
  BSpline_Avg         = round(momentum_avg_mean, 4),
  BSpline_Avg_Lower   = round(momentum_avg_lower, 4),
  BSpline_Avg_Upper   = round(momentum_avg_upper, 4),
  Diff_OLS_vs_Avg     = round(momentum_avg_mean - momentum_ols, 4),
  stringsAsFactors = FALSE
)

df_compare2$Rank_OLS    <- rank(-df_compare2$OLS_Momentum)
df_compare2$Rank_BSAvg  <- rank(-df_compare2$BSpline_Avg)
df_compare2$Rank_Change <- df_compare2$Rank_OLS - df_compare2$Rank_BSAvg

cat("\n=== 3개 산출 방식 비교 ===\n")
print(df_compare2)


# ----------------------------------------------------------------
# 6-5. 핵심 인용값 변동 점검 (T8, T9, T14)
# ----------------------------------------------------------------
cat("\n=== 본문 핵심 인용값 — 3가지 방식 비교 ===\n")

show_topic <- function(i) {
  cat(sprintf("\n--- %s (%s) ---\n", df_compare2$Topic[i], df_compare2$Label[i]))
  cat(sprintf("  OLS         : %7.4f  (순위 %2d)\n", 
              df_compare2$OLS_Momentum[i], df_compare2$Rank_OLS[i]))
  cat(sprintf("  B-Spline@2023: %7.4f\n", 
              df_compare2$BSpline_2023[i]))
  cat(sprintf("  B-Spline 평균: %7.4f  (순위 %2d)  CI [%7.4f, %7.4f]\n", 
              df_compare2$BSpline_Avg[i], df_compare2$Rank_BSAvg[i],
              df_compare2$BSpline_Avg_Lower[i], df_compare2$BSpline_Avg_Upper[i]))
}

show_topic(8)   # NFT
show_topic(9)   # Investment Reco
show_topic(14)  # Smart Contract


# ----------------------------------------------------------------
# 6-6. CSV 저장
# ----------------------------------------------------------------
write.csv(df_compare2,
          "TLC_Momentum_3methods_Comparison.csv",
          row.names = FALSE)
cat("\n✅ 저장: TLC_Momentum_3methods_Comparison.csv\n")

# ================================================================
# [7] Boundary 오염 진단 및 수정
# ================================================================

library(splines)

# ----------------------------------------------------------------
# 7-1. 시점별 도함수 스케일 진단
# ----------------------------------------------------------------
cat("=== 시점별 도함수 절대값 합 (스케일 진단) ===\n")
scale_per_year <- rowSums(abs(basis_deriv_mat))
print(round(scale_per_year, 3))
cat("\n→ 2019, 2026이 다른 시점 대비 비정상적으로 크면 boundary 오염 신호\n\n")


# ----------------------------------------------------------------
# 7-2. 평가 시점별 모멘텀 비교 — 어느 시점이 평균을 망치는가
# ----------------------------------------------------------------
# T9(Investment Reco)에 대해 각 시점별 도함수 출력
cat("=== T9 Investment Reco: 시점별 도함수 ===\n")
T9_coef_avg <- rowMeans(sapply(1:n_sims, function(s) {
  effect_country_US$parameters[[9]][[s]]$est[spline_idx]
}))
T9_deriv_by_year <- basis_deriv_mat %*% T9_coef_avg * 100
rownames(T9_deriv_by_year) <- as.character(year_eval_seq)
print(round(T9_deriv_by_year, 4))
cat("→ 2019, 2026 값이 비정상적으로 크면 boundary 오염 확정\n\n")


# ----------------------------------------------------------------
# 7-3. Boundary 제외 평균 — 2020~2025 (6점)
# ----------------------------------------------------------------
inner_idx <- which(year_eval_seq %in% 2020:2025)
cat("=== Boundary 제외 평균: 2020~2025 (", length(inner_idx), "시점) ===\n", sep="")

momentum_inner_sims <- matrix(NA, nrow = n_topics, ncol = n_sims)
for (k in 1:n_topics) {
  for (s in 1:n_sims) {
    est_k_s <- effect_country_US$parameters[[k]][[s]]$est
    spline_coef <- est_k_s[spline_idx]
    deriv_at_each_year <- basis_deriv_mat[inner_idx, ] %*% spline_coef
    momentum_inner_sims[k, s] <- mean(deriv_at_each_year)
  }
}

momentum_inner_mean  <- apply(momentum_inner_sims, 1, mean) * 100
momentum_inner_lower <- apply(momentum_inner_sims, 1, quantile, probs = 0.025) * 100
momentum_inner_upper <- apply(momentum_inner_sims, 1, quantile, probs = 0.975) * 100


# ----------------------------------------------------------------
# 7-4. Boundary 인접 제외 (더 보수적) — 2021~2024 (4점)
# ----------------------------------------------------------------
core_idx <- which(year_eval_seq %in% 2021:2024)
cat("=== Boundary 인접 제외 평균: 2021~2024 (", length(core_idx), "시점) ===\n", sep="")

momentum_core_sims <- matrix(NA, nrow = n_topics, ncol = n_sims)
for (k in 1:n_topics) {
  for (s in 1:n_sims) {
    est_k_s <- effect_country_US$parameters[[k]][[s]]$est
    spline_coef <- est_k_s[spline_idx]
    deriv_at_each_year <- basis_deriv_mat[core_idx, ] %*% spline_coef
    momentum_core_sims[k, s] <- mean(deriv_at_each_year)
  }
}
momentum_core_mean <- apply(momentum_core_sims, 1, mean) * 100


# ----------------------------------------------------------------
# 7-5. 4가지 방식 종합 비교표
# ----------------------------------------------------------------
df_compare3 <- data.frame(
  Topic = paste0("T", 1:n_topics),
  Label = topic_label_short,
  OLS               = round(momentum_ols, 4),
  BSpline_All8      = round(momentum_avg_mean, 4),       # 2019~2026 (오염)
  BSpline_Inner6    = round(momentum_inner_mean, 4),     # 2020~2025
  BSpline_Core4     = round(momentum_core_mean, 4),      # 2021~2024
  Inner6_CI_Lower   = round(momentum_inner_lower, 4),
  Inner6_CI_Upper   = round(momentum_inner_upper, 4),
  stringsAsFactors = FALSE
)

cat("\n=== 4가지 방식 비교 ===\n")
print(df_compare3)


# ----------------------------------------------------------------
# 7-6. 핵심 토픽 변동 점검
# ----------------------------------------------------------------
cat("\n=== 핵심 토픽 — 4가지 방식 비교 ===\n")
for (i in c(8, 9, 14, 17, 1, 6)) {
  cat(sprintf("\n--- %s (%s) ---\n", df_compare3$Topic[i], df_compare3$Label[i]))
  cat(sprintf("  OLS              : %7.4f\n", df_compare3$OLS[i]))
  cat(sprintf("  All 8 (오염)     : %7.4f\n", df_compare3$BSpline_All8[i]))
  cat(sprintf("  Inner 6 (권장)   : %7.4f  CI [%7.4f, %7.4f]\n",
              df_compare3$BSpline_Inner6[i], 
              df_compare3$Inner6_CI_Lower[i], df_compare3$Inner6_CI_Upper[i]))
  cat(sprintf("  Core 4 (보수적)  : %7.4f\n", df_compare3$BSpline_Core4[i]))
}


# ----------------------------------------------------------------
# 7-7. CSV 저장
# ----------------------------------------------------------------
write.csv(df_compare3,
          "TLC_Momentum_4methods_Comparison.csv",
          row.names = FALSE)
cat("\n✅ 저장: TLC_Momentum_4methods_Comparison.csv\n")

# ================================================================
# [8] 8대 메가 클러스터 단위 모멘텀 집계 (정확한 매핑 적용)
# ================================================================

# ----------------------------------------------------------------
# 8-1. 토픽 → 클러스터 매핑 (클러스터_리스트.txt 기준)
# ----------------------------------------------------------------
topic_cluster_map <- data.frame(
  Topic = paste0("T", 1:20),
  Label = topic_label_short,
  Cluster_Code = c(
    "E",  # T1  Crypto Exchange    → E (Blockchain and Digital Assets)
    "H",  # T2  Mobile POS Pay     → H (O2O Commerce & Simplified Payment)
    "B",  # T3  Insurance Claim    → B (Customized Insurance & Target ID)
    "A",  # T4  Invoice Audit      → A (Corporate Finance & Accounting)
    "G",  # T5  Bank Remittance    → G (Banking & Payment Settlement)
    "E",  # T6  Digital Wallet     → E (Blockchain and Digital Assets)
    "F",  # T7  Auth & Biometrics  → F (Security Authentication)
    "E",  # T8  NFT Rights         → E (Blockchain and Digital Assets)
    "C",  # T9  Investment Reco    → C (AI-based Credit & Investment)
    "G",  # T10 Settlement Proc    → G (Banking & Payment Settlement)
    "A",  # T11 Enterprise Finance → A (Corporate Finance & Accounting)
    "E",  # T12 Asset Ledger       → E (Blockchain and Digital Assets)
    "C",  # T13 Credit Risk        → C (AI-based Credit & Investment)
    "D",  # T14 Smart Contract     → D (Blockchain-based Energy Finance)
    "F",  # T15 Realtime Settle    → F (Security Authentication)
    "H",  # T16 E-commerce Fee     → H (O2O Commerce & Simplified Payment)
    "C",  # T17 AI Detection       → C (AI-based Credit & Investment)
    "B",  # T18 Device Linking     → B (Customized Insurance & Target ID)
    "D",  # T19 Charging Txn       → D (Blockchain-based Energy Finance)
    "H"   # T20 Face/Voice Login   → H (O2O Commerce & Simplified Payment)
  ),
  stringsAsFactors = FALSE
)

cluster_names <- c(
  A = "Corporate Finance & Accounting Management",
  B = "Customized Insurance & Target Identification",
  C = "AI-based Credit & Investment Analysis",
  D = "Blockchain-based Energy Finance",
  E = "Blockchain and Digital Assets",
  F = "Security Authentication & Regulatory Compliance",
  G = "Banking & Payment Settlement",
  H = "O2O Commerce & Simplified Payment"
)

cluster_names_kr <- c(
  A = "기업 재무 및 회계 관리",
  B = "맞춤형 보험 및 타겟 식별",
  C = "AI 기반 신용/투자 분석",
  D = "블록체인 기반 에너지 금융",
  E = "블록체인 및 디지털 자산",
  F = "보안 인증 및 규제 컴플라이언스",
  G = "뱅킹 및 대금 정산",
  H = "O2O 상거래 및 간편 결제"
)

cat("=== 토픽 → 클러스터 매핑 확인 ===\n")
print(topic_cluster_map)


# ----------------------------------------------------------------
# 8-2. 토픽 비중 추출 (가중치용)
# ----------------------------------------------------------------
topic_proportion <- colMeans(final_stm_20_adj$theta)
names(topic_proportion) <- paste0("T", 1:20)


# ----------------------------------------------------------------
# 8-3. 클러스터 단위 모멘텀 — 비중 가중 평균
# ----------------------------------------------------------------
cluster_momentum <- data.frame(
  Cluster_Code = LETTERS[1:8],
  Cluster_Name_EN = cluster_names[LETTERS[1:8]],
  Cluster_Name_KR = cluster_names_kr[LETTERS[1:8]],
  Topic_Count = NA,
  Topics_in_Cluster = NA,
  Total_Proportion = NA,
  Momentum_Weighted = NA,
  Momentum_Simple_Avg = NA,
  CI_Lower = NA,
  CI_Upper = NA,
  stringsAsFactors = FALSE
)

# 시뮬레이션 단위 클러스터 모멘텀 집계
cluster_momentum_sims <- matrix(NA, nrow = 8, ncol = n_sims)
rownames(cluster_momentum_sims) <- LETTERS[1:8]

for (c_idx in 1:8) {
  c_code <- LETTERS[c_idx]
  topics_in_c <- which(topic_cluster_map$Cluster_Code == c_code)
  
  if (length(topics_in_c) == 0) next
  
  cluster_momentum$Topic_Count[c_idx] <- length(topics_in_c)
  cluster_momentum$Topics_in_Cluster[c_idx] <- 
    paste(paste0("T", topics_in_c), collapse = ",")
  cluster_momentum$Total_Proportion[c_idx] <- 
    sum(topic_proportion[topics_in_c])
  
  # 비중 가중치
  weights <- topic_proportion[topics_in_c]
  weights <- weights / sum(weights)
  
  for (s in 1:n_sims) {
    topic_momentums_s <- momentum_inner_sims[topics_in_c, s] * 100
    cluster_momentum_sims[c_idx, s] <- sum(topic_momentums_s * weights)
  }
  
  cluster_momentum$Momentum_Weighted[c_idx] <- 
    mean(cluster_momentum_sims[c_idx, ])
  cluster_momentum$Momentum_Simple_Avg[c_idx] <- 
    mean(momentum_inner_mean[topics_in_c])
  cluster_momentum$CI_Lower[c_idx] <- 
    quantile(cluster_momentum_sims[c_idx, ], 0.025)
  cluster_momentum$CI_Upper[c_idx] <- 
    quantile(cluster_momentum_sims[c_idx, ], 0.975)
}

# 반올림
cluster_momentum$Total_Proportion    <- round(cluster_momentum$Total_Proportion, 4)
cluster_momentum$Momentum_Weighted   <- round(cluster_momentum$Momentum_Weighted, 4)
cluster_momentum$Momentum_Simple_Avg <- round(cluster_momentum$Momentum_Simple_Avg, 4)
cluster_momentum$CI_Lower            <- round(cluster_momentum$CI_Lower, 4)
cluster_momentum$CI_Upper            <- round(cluster_momentum$CI_Upper, 4)

cat("\n=== 8대 메가 클러스터 모멘텀 (Inner6 기준) ===\n")
print(cluster_momentum)


# ----------------------------------------------------------------
# 8-4. 사분면 분류 (중앙값 기준)
# ----------------------------------------------------------------
mid_x_cluster <- median(cluster_momentum$Total_Proportion)
mid_y_cluster <- median(cluster_momentum$Momentum_Weighted)

cat(sprintf("\n=== 사분면 분류 기준선 ===\n"))
cat(sprintf("  X축 중앙값 (총 비중) : %.4f\n", mid_x_cluster))
cat(sprintf("  Y축 중앙값 (모멘텀)  : %.4f\n", mid_y_cluster))

cluster_momentum$Quadrant <- case_when(
  cluster_momentum$Total_Proportion >= mid_x_cluster & 
    cluster_momentum$Momentum_Weighted >= mid_y_cluster ~ "Q1: Mainstream",
  cluster_momentum$Total_Proportion <  mid_x_cluster & 
    cluster_momentum$Momentum_Weighted >= mid_y_cluster ~ "Q2: Emerging",
  cluster_momentum$Total_Proportion <  mid_x_cluster & 
    cluster_momentum$Momentum_Weighted <  mid_y_cluster ~ "Q3: Niche/Declining",
  cluster_momentum$Total_Proportion >= mid_x_cluster & 
    cluster_momentum$Momentum_Weighted <  mid_y_cluster ~ "Q4: Saturated/Mature"
)

cat("\n=== 8대 클러스터 사분면 분류 ===\n")
print(cluster_momentum[, c("Cluster_Code", "Cluster_Name_KR", 
                           "Total_Proportion", "Momentum_Weighted",
                           "CI_Lower", "CI_Upper", "Quadrant")])


# ----------------------------------------------------------------
# 8-5. E클러스터 내부 분화 점검 (핵심 본문 영향)
# ----------------------------------------------------------------
cat("\n=== E클러스터 내부 분화 점검 ===\n")
e_topics <- which(topic_cluster_map$Cluster_Code == "E")
e_inspection <- data.frame(
  Topic = paste0("T", e_topics),
  Label = topic_label_short[e_topics],
  Proportion = round(topic_proportion[e_topics], 4),
  OLS_Momentum = round(momentum_ols[e_topics], 4),
  Inner6_Momentum = round(momentum_inner_mean[e_topics], 4),
  CI_Lower = round(momentum_inner_lower[e_topics], 4),
  CI_Upper = round(momentum_inner_upper[e_topics], 4),
  Significant = ifelse(momentum_inner_lower[e_topics] > 0, "↑ 유의",
                       ifelse(momentum_inner_upper[e_topics] < 0, "↓ 유의", "ns")),
  stringsAsFactors = FALSE
)
print(e_inspection)


# ----------------------------------------------------------------
# 8-6. CSV 저장
# ----------------------------------------------------------------
write.csv(topic_cluster_map,
          "Topic_Cluster_Mapping.csv",
          row.names = FALSE)

write.csv(cluster_momentum,
          "Cluster_Momentum_Inner6.csv",
          row.names = FALSE)

cat("\n✅ 저장 완료:\n")
cat("  - Topic_Cluster_Mapping.csv\n")
cat("  - Cluster_Momentum_Inner6.csv\n")

# ================================================================
# [9] TLC 포트폴리오 매트릭스 시각화
# ================================================================

library(ggplot2)
library(ggrepel)
library(scales)

# ----------------------------------------------------------------
# 9-1. 클러스터 사분면 매트릭스 (8대 메가 클러스터)
# ----------------------------------------------------------------

# 시각화용 데이터프레임 — 클러스터 라벨에 코드 포함
df_cluster_plot <- cluster_momentum %>%
  mutate(
    Plot_Label = paste0(Cluster_Code, ". ", Cluster_Name_KR),
    # 비중을 퍼센트로 (가독성)
    Proportion_pct = Total_Proportion * 100
  )

# 클러스터별 색상 (8개)
cluster_colors_8 <- c(
  "A" = "#E74C3C",  # 빨강
  "B" = "#3498DB",  # 파랑
  "C" = "#2ECC71",  # 초록 (Mainstream Q1 핵심)
  "D" = "#9B59B6",  # 보라
  "E" = "#F39C12",  # 주황
  "F" = "#1ABC9C",  # 청록
  "G" = "#34495E",  # 진남
  "H" = "#E67E22"   # 갈색주황
)

# 사분면 기준선
mid_x_pct <- mid_x_cluster * 100
mid_y     <- mid_y_cluster

# 플롯 범위 계산 (annotation 위치용)
x_range <- range(df_cluster_plot$Proportion_pct)
y_range <- range(c(df_cluster_plot$CI_Lower, df_cluster_plot$CI_Upper))
x_pad <- diff(x_range) * 0.05
y_pad <- diff(y_range) * 0.10


p_cluster_tlc <- ggplot(df_cluster_plot, 
                        aes(x = Proportion_pct, y = Momentum_Weighted)) +
  # 사분면 구분선
  geom_vline(xintercept = mid_x_pct, linetype = "dashed", 
             color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = mid_y, linetype = "dashed", 
             color = "gray50", linewidth = 0.5) +
  # 0 기준선 (성장 ↔ 하락)
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray30", linewidth = 0.3, alpha = 0.5) +
  
  # 신뢰구간 (수직 오차막대)
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Cluster_Code),
                width = 0.15, linewidth = 0.7, alpha = 0.6) +
  
  # 버블 (크기 = 비중, 색 = 클러스터)
  geom_point(aes(size = Proportion_pct, color = Cluster_Code), 
             alpha = 0.85) +
  
  # 라벨
  geom_text_repel(aes(label = Plot_Label),
                  size = 4.2, fontface = "bold",
                  box.padding = 0.7, point.padding = 0.5,
                  segment.color = "gray50", segment.size = 0.4,
                  max.overlaps = Inf,
                  family = "sans") +
  
  # 사분면 텍스트
  annotate("text", x = max(df_cluster_plot$Proportion_pct) + x_pad * 0.5, 
           y = max(y_range) + y_pad * 0.5,
           label = "Q1: Mainstream\n(High Share, High Growth)",
           color = "#27AE60", fontface = "bold", size = 3.8, hjust = 1) +
  annotate("text", x = min(df_cluster_plot$Proportion_pct) - x_pad * 0.5, 
           y = max(y_range) + y_pad * 0.5,
           label = "Q2: Emerging\n(Low Share, High Growth)",
           color = "#C0392B", fontface = "bold", size = 3.8, hjust = 0) +
  annotate("text", x = min(df_cluster_plot$Proportion_pct) - x_pad * 0.5, 
           y = min(y_range) - y_pad * 0.5,
           label = "Q3: Niche/Declining\n(Low Share, Low Growth)",
           color = "gray30", fontface = "bold", size = 3.8, hjust = 0) +
  annotate("text", x = max(df_cluster_plot$Proportion_pct) + x_pad * 0.5, 
           y = min(y_range) - y_pad * 0.5,
           label = "Q4: Saturated/Mature\n(High Share, Low Growth)",
           color = "#2980B9", fontface = "bold", size = 3.8, hjust = 1) +
  
  scale_color_manual(values = cluster_colors_8) +
  scale_size_continuous(range = c(8, 22), guide = "none") +
  
  labs(
    title = "Technology Life Cycle (TLC) Portfolio Matrix — 8 Mega Clusters",
    subtitle = "B-Spline 평균 도함수 모멘텀 (2020-2025), 95% CI 표시",
    x = "토픽 비중 합계 (%)",
    y = "성장 모멘텀 (B-Spline 평균 도함수, ×100)",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  xlim(x_range[1] - x_pad, x_range[2] + x_pad) +
  ylim(y_range[1] - y_pad, y_range[2] + y_pad)


print(p_cluster_tlc)

ggsave("STM_TLC_Portfolio_Matrix_8Clusters_BSpline.png",
       plot = p_cluster_tlc,
       width = 14, height = 10, dpi = 450)
cat("✅ 저장: STM_TLC_Portfolio_Matrix_8Clusters_BSpline.png (dpi=450)\n")


# ----------------------------------------------------------------
# 9-2. 토픽 사분면 매트릭스 (20개 토픽) — 부록용
# ----------------------------------------------------------------

# 토픽 단위 데이터프레임
df_topic_plot <- data.frame(
  Topic = paste0("T", 1:20),
  Label = topic_label_short,
  Cluster_Code = topic_cluster_map$Cluster_Code,
  Proportion_pct = topic_proportion * 100,
  Momentum = momentum_inner_mean,
  CI_Lower = momentum_inner_lower,
  CI_Upper = momentum_inner_upper,
  stringsAsFactors = FALSE
)

# 토픽 사분면 기준선
mid_x_topic <- median(df_topic_plot$Proportion_pct)
mid_y_topic <- median(df_topic_plot$Momentum)

# 범위
x_range_t <- range(df_topic_plot$Proportion_pct)
y_range_t <- range(c(df_topic_plot$CI_Lower, df_topic_plot$CI_Upper))
x_pad_t <- diff(x_range_t) * 0.05
y_pad_t <- diff(y_range_t) * 0.10


p_topic_tlc <- ggplot(df_topic_plot, 
                      aes(x = Proportion_pct, y = Momentum)) +
  geom_vline(xintercept = mid_x_topic, linetype = "dashed", 
             color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = mid_y_topic, linetype = "dashed", 
             color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray30", linewidth = 0.3, alpha = 0.5) +
  
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Cluster_Code),
                width = 0.05, linewidth = 0.5, alpha = 0.5) +
  
  geom_point(aes(size = Proportion_pct, color = Cluster_Code), 
             alpha = 0.85) +
  
  geom_text_repel(aes(label = paste0(Topic, ": ", Label)),
                  size = 3.2, fontface = "bold",
                  box.padding = 0.4, point.padding = 0.3,
                  segment.color = "gray60", segment.size = 0.3,
                  max.overlaps = Inf,
                  family = "sans") +
  
  scale_color_manual(values = cluster_colors_8, name = "Cluster") +
  scale_size_continuous(range = c(3, 12), guide = "none") +
  
  labs(
    title = "Technology Life Cycle (TLC) Portfolio Matrix — 20 Topics",
    subtitle = "B-Spline 평균 도함수 모멘텀 (2020-2025), 95% CI 표시",
    x = "토픽 비중 (%)",
    y = "성장 모멘텀 (B-Spline 평균 도함수, ×100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  xlim(x_range_t[1] - x_pad_t, x_range_t[2] + x_pad_t) +
  ylim(y_range_t[1] - y_pad_t, y_range_t[2] + y_pad_t)


print(p_topic_tlc)

ggsave("STM_TLC_Portfolio_Matrix_20Topics_BSpline.png",
       plot = p_topic_tlc,
       width = 16, height = 11, dpi = 450)
cat("✅ 저장: STM_TLC_Portfolio_Matrix_20Topics_BSpline.png (dpi=450)\n")


# ----------------------------------------------------------------
# 9-3. 산출물 확인
# ----------------------------------------------------------------
cat("\n=== 시각화 산출 완료 ===\n")
cat("1. STM_TLC_Portfolio_Matrix_8Clusters_BSpline.png (1400×1000, dpi=450)\n")
cat("2. STM_TLC_Portfolio_Matrix_20Topics_BSpline.png  (1600×1100, dpi=450)\n")
cat("\n저장 경로: ", getwd(), "\n")

# ================================================================
# [9-v2] TLC 포트폴리오 매트릭스 시각화 — 가독성 개선판
# ================================================================

library(ggplot2)
library(ggrepel)
library(scales)

# ----------------------------------------------------------------
# 9-1. 클러스터 사분면 매트릭스 (개선판)
# ----------------------------------------------------------------

df_cluster_plot <- cluster_momentum %>%
  mutate(
    Plot_Label = paste0(Cluster_Code, ". ", Cluster_Name_KR),
    Proportion_pct = Total_Proportion * 100
  )

cluster_colors_8 <- c(
  "A" = "#E74C3C", "B" = "#3498DB", "C" = "#27AE60",
  "D" = "#9B59B6", "E" = "#F39C12", "F" = "#16A085",
  "G" = "#34495E", "H" = "#E67E22"
)

mid_x_pct <- mid_x_cluster * 100
mid_y     <- mid_y_cluster

# 플롯 영역을 사분면 라벨이 들어갈 만큼 충분히 확장
x_range <- range(df_cluster_plot$Proportion_pct)
y_range <- range(c(df_cluster_plot$CI_Lower, df_cluster_plot$CI_Upper))
x_pad <- diff(x_range) * 0.18    # 패딩 증가
y_pad <- diff(y_range) * 0.20

x_min <- x_range[1] - x_pad
x_max <- x_range[2] + x_pad
y_min <- y_range[1] - y_pad
y_max <- y_range[2] + y_pad


p_cluster_tlc <- ggplot(df_cluster_plot, 
                        aes(x = Proportion_pct, y = Momentum_Weighted)) +
  
  # 사분면 배경 (옅은 색)
  annotate("rect", xmin = mid_x_pct, xmax = x_max, 
           ymin = mid_y, ymax = y_max,
           fill = "#27AE60", alpha = 0.06) +
  annotate("rect", xmin = x_min, xmax = mid_x_pct,
           ymin = mid_y, ymax = y_max,
           fill = "#E74C3C", alpha = 0.06) +
  annotate("rect", xmin = x_min, xmax = mid_x_pct,
           ymin = y_min, ymax = mid_y,
           fill = "gray60", alpha = 0.06) +
  annotate("rect", xmin = mid_x_pct, xmax = x_max,
           ymin = y_min, ymax = mid_y,
           fill = "#2980B9", alpha = 0.06) +
  
  # 사분면 구분선
  geom_vline(xintercept = mid_x_pct, linetype = "dashed", 
             color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = mid_y, linetype = "dashed", 
             color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray20", linewidth = 0.4, alpha = 0.7) +
  
  # 신뢰구간 (더 두껍게)
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Cluster_Code),
                width = 0.25, linewidth = 1.2, alpha = 0.75) +
  
  # 버블 (크기 확대)
  geom_point(aes(size = Proportion_pct, color = Cluster_Code), 
             alpha = 0.9) +
  geom_point(aes(size = Proportion_pct), 
             color = "white", alpha = 0, stroke = 1) +
  
  # 라벨 (더 크고 진하게)
  geom_text_repel(aes(label = Plot_Label, color = Cluster_Code),
                  size = 5.0, fontface = "bold",
                  box.padding = 1.0, point.padding = 0.7,
                  segment.color = "gray40", segment.size = 0.5,
                  segment.alpha = 0.6,
                  max.overlaps = Inf,
                  family = "sans",
                  show.legend = FALSE) +
  
  # 사분면 라벨 (차트 안쪽 모서리)
  annotate("text", x = x_max - x_pad*0.3, y = y_max - y_pad*0.3,
           label = "Q1: Mainstream",
           color = "#27AE60", fontface = "bold", size = 5.5, hjust = 1) +
  annotate("text", x = x_max - x_pad*0.3, y = y_max - y_pad*0.6,
           label = "(High Share, High Growth)",
           color = "#27AE60", size = 3.8, hjust = 1) +
  
  annotate("text", x = x_min + x_pad*0.3, y = y_max - y_pad*0.3,
           label = "Q2: Emerging",
           color = "#E74C3C", fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = x_min + x_pad*0.3, y = y_max - y_pad*0.6,
           label = "(Low Share, High Growth)",
           color = "#E74C3C", size = 3.8, hjust = 0) +
  
  annotate("text", x = x_min + x_pad*0.3, y = y_min + y_pad*0.5,
           label = "Q3: Niche/Declining",
           color = "gray30", fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = x_min + x_pad*0.3, y = y_min + y_pad*0.25,
           label = "(Low Share, Low Growth)",
           color = "gray30", size = 3.8, hjust = 0) +
  
  annotate("text", x = x_max - x_pad*0.3, y = y_min + y_pad*0.5,
           label = "Q4: Saturated/Mature",
           color = "#2980B9", fontface = "bold", size = 5.5, hjust = 1) +
  annotate("text", x = x_max - x_pad*0.3, y = y_min + y_pad*0.25,
           label = "(High Share, Low Growth)",
           color = "#2980B9", size = 3.8, hjust = 1) +
  
  scale_color_manual(values = cluster_colors_8) +
  scale_size_continuous(range = c(14, 32), guide = "none") +
  
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  
  labs(
    title = "Technology Life Cycle (TLC) Portfolio Matrix — 8 Mega Clusters",
    subtitle = "B-Spline 평균 도함수 모멘텀 (2020-2025), 95% 신뢰구간 표시",
    x = "토픽 비중 합계 (%)",
    y = "성장 모멘텀 (B-Spline 평균 도함수, ×100)",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.margin = margin(15, 20, 15, 15)
  )

print(p_cluster_tlc)

ggsave("STM_TLC_Portfolio_Matrix_8Clusters_BSpline.png",
       plot = p_cluster_tlc,
       width = 14, height = 10, dpi = 450)
cat("✅ 저장: STM_TLC_Portfolio_Matrix_8Clusters_BSpline.png (dpi=450)\n")


# ----------------------------------------------------------------
# 9-2. 토픽 사분면 매트릭스 (개선판) — 토픽 번호만, 영문 라벨 제거
# ----------------------------------------------------------------

df_topic_plot <- data.frame(
  Topic = paste0("T", 1:20),
  Topic_Num = as.character(1:20),  # 점 안에 표시할 번호
  Cluster_Code = topic_cluster_map$Cluster_Code,
  Proportion_pct = topic_proportion * 100,
  Momentum = momentum_inner_mean,
  CI_Lower = momentum_inner_lower,
  CI_Upper = momentum_inner_upper,
  stringsAsFactors = FALSE
)

mid_x_topic <- median(df_topic_plot$Proportion_pct)
mid_y_topic <- median(df_topic_plot$Momentum)

x_range_t <- range(df_topic_plot$Proportion_pct)
y_range_t <- range(c(df_topic_plot$CI_Lower, df_topic_plot$CI_Upper))
x_pad_t <- diff(x_range_t) * 0.12
y_pad_t <- diff(y_range_t) * 0.15

x_min_t <- x_range_t[1] - x_pad_t
x_max_t <- x_range_t[2] + x_pad_t
y_min_t <- y_range_t[1] - y_pad_t
y_max_t <- y_range_t[2] + y_pad_t


p_topic_tlc <- ggplot(df_topic_plot, 
                      aes(x = Proportion_pct, y = Momentum)) +
  
  # 사분면 배경
  annotate("rect", xmin = mid_x_topic, xmax = x_max_t, 
           ymin = mid_y_topic, ymax = y_max_t,
           fill = "#27AE60", alpha = 0.06) +
  annotate("rect", xmin = x_min_t, xmax = mid_x_topic,
           ymin = mid_y_topic, ymax = y_max_t,
           fill = "#E74C3C", alpha = 0.06) +
  annotate("rect", xmin = x_min_t, xmax = mid_x_topic,
           ymin = y_min_t, ymax = mid_y_topic,
           fill = "gray60", alpha = 0.06) +
  annotate("rect", xmin = mid_x_topic, xmax = x_max_t,
           ymin = y_min_t, ymax = mid_y_topic,
           fill = "#2980B9", alpha = 0.06) +
  
  geom_vline(xintercept = mid_x_topic, linetype = "dashed", 
             color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = mid_y_topic, linetype = "dashed", 
             color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray20", linewidth = 0.4, alpha = 0.7) +
  
  # 신뢰구간 (얇게)
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Cluster_Code),
                width = 0.08, linewidth = 0.7, alpha = 0.5) +
  
  # 큰 버블
  geom_point(aes(size = Proportion_pct, color = Cluster_Code), 
             alpha = 0.85) +
  
  # 토픽 번호를 점 안에 표시 (T1, T2, ... 형태)
  geom_text(aes(label = Topic),
            size = 3.8, fontface = "bold",
            color = "white") +
  
  # 사분면 라벨
  annotate("text", x = x_max_t - x_pad_t*0.2, y = y_max_t - y_pad_t*0.2,
           label = "Q1: Mainstream",
           color = "#27AE60", fontface = "bold", size = 5, hjust = 1) +
  annotate("text", x = x_min_t + x_pad_t*0.2, y = y_max_t - y_pad_t*0.2,
           label = "Q2: Emerging",
           color = "#E74C3C", fontface = "bold", size = 5, hjust = 0) +
  annotate("text", x = x_min_t + x_pad_t*0.2, y = y_min_t + y_pad_t*0.3,
           label = "Q3: Niche/Declining",
           color = "gray30", fontface = "bold", size = 5, hjust = 0) +
  annotate("text", x = x_max_t - x_pad_t*0.2, y = y_min_t + y_pad_t*0.3,
           label = "Q4: Saturated/Mature",
           color = "#2980B9", fontface = "bold", size = 5, hjust = 1) +
  
  scale_color_manual(values = cluster_colors_8, name = "Cluster") +
  scale_size_continuous(range = c(8, 22), guide = "none") +
  
  coord_cartesian(xlim = c(x_min_t, x_max_t), ylim = c(y_min_t, y_max_t)) +
  
  labs(
    title = "Technology Life Cycle (TLC) Portfolio Matrix — 20 Topics",
    subtitle = "B-Spline 평균 도함수 모멘텀 (2020-2025), 95% 신뢰구간 표시 / 색상=메가 클러스터",
    x = "토픽 비중 (%)",
    y = "성장 모멘텀 (B-Spline 평균 도함수, ×100)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.margin = margin(15, 20, 15, 15)
  )

print(p_topic_tlc)

ggsave("STM_TLC_Portfolio_Matrix_20Topics_BSpline.png",
       plot = p_topic_tlc,
       width = 14, height = 10, dpi = 450)
cat("✅ 저장: STM_TLC_Portfolio_Matrix_20Topics_BSpline.png (dpi=450)\n")


# ----------------------------------------------------------------
# 9-3. 토픽 번호 ↔ 라벨 매핑 표 (논문 부록용)
# ----------------------------------------------------------------
topic_legend_table <- data.frame(
  Topic = paste0("T", 1:20),
  Label_EN = topic_label_short,
  Cluster = topic_cluster_map$Cluster_Code,
  stringsAsFactors = FALSE
)

cat("\n=== 토픽 번호 ↔ 라벨 매핑 (논문 부록 / 그림 설명용) ===\n")
print(topic_legend_table)

write.csv(topic_legend_table, 
          "Topic_Legend_Table.csv", 
          row.names = FALSE)
cat("\n✅ 저장: Topic_Legend_Table.csv\n")

# ================================================================
# [9-v3] TLC 포트폴리오 매트릭스 — 라벨 단순화 + 겹침 해결
# ================================================================

library(ggplot2)
library(ggrepel)
library(scales)

cluster_colors_8 <- c(
  "A" = "#E74C3C", "B" = "#3498DB", "C" = "#27AE60",
  "D" = "#9B59B6", "E" = "#F39C12", "F" = "#16A085",
  "G" = "#34495E", "H" = "#E67E22"
)


# ----------------------------------------------------------------
# 9-1. 클러스터 사분면 — 점 안에 "A"~"H"만, 한글명은 범례로
# ----------------------------------------------------------------

df_cluster_plot <- cluster_momentum %>%
  mutate(
    Proportion_pct = Total_Proportion * 100,
    # 범례용: "A — 기업 재무 및 회계 관리"
    Legend_Label = paste0(Cluster_Code, " — ", Cluster_Name_KR)
  )

mid_x_pct <- mid_x_cluster * 100
mid_y     <- mid_y_cluster

x_range <- range(df_cluster_plot$Proportion_pct)
y_range <- range(c(df_cluster_plot$CI_Lower, df_cluster_plot$CI_Upper))
x_pad <- diff(x_range) * 0.18
y_pad <- diff(y_range) * 0.20

x_min <- x_range[1] - x_pad
x_max <- x_range[2] + x_pad
y_min <- y_range[1] - y_pad
y_max <- y_range[2] + y_pad


p_cluster_tlc <- ggplot(df_cluster_plot, 
                        aes(x = Proportion_pct, y = Momentum_Weighted)) +
  
  # 사분면 배경
  annotate("rect", xmin = mid_x_pct, xmax = x_max, 
           ymin = mid_y, ymax = y_max,
           fill = "#27AE60", alpha = 0.06) +
  annotate("rect", xmin = x_min, xmax = mid_x_pct,
           ymin = mid_y, ymax = y_max,
           fill = "#E74C3C", alpha = 0.06) +
  annotate("rect", xmin = x_min, xmax = mid_x_pct,
           ymin = y_min, ymax = mid_y,
           fill = "gray60", alpha = 0.06) +
  annotate("rect", xmin = mid_x_pct, xmax = x_max,
           ymin = y_min, ymax = mid_y,
           fill = "#2980B9", alpha = 0.06) +
  
  # 사분면 구분선
  geom_vline(xintercept = mid_x_pct, linetype = "dashed", 
             color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = mid_y, linetype = "dashed", 
             color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray20", linewidth = 0.4, alpha = 0.7) +
  
  # 신뢰구간
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Cluster_Code),
                width = 0.25, linewidth = 1.2, alpha = 0.75) +
  
  # 버블 (크게)
  geom_point(aes(size = Proportion_pct, color = Cluster_Code, fill = Cluster_Code), 
             alpha = 0.85, shape = 21, stroke = 1.5) +
  
  # 점 안에 A~H 알파벳만
  geom_text(aes(label = Cluster_Code),
            size = 6.5, fontface = "bold",
            color = "white") +
  
  # 사분면 라벨
  annotate("text", x = x_max - x_pad*0.3, y = y_max - y_pad*0.3,
           label = "Q1: Mainstream",
           color = "#27AE60", fontface = "bold", size = 5.5, hjust = 1) +
  annotate("text", x = x_max - x_pad*0.3, y = y_max - y_pad*0.6,
           label = "(High Share, High Growth)",
           color = "#27AE60", size = 3.8, hjust = 1) +
  
  annotate("text", x = x_min + x_pad*0.3, y = y_max - y_pad*0.3,
           label = "Q2: Emerging",
           color = "#E74C3C", fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = x_min + x_pad*0.3, y = y_max - y_pad*0.6,
           label = "(Low Share, High Growth)",
           color = "#E74C3C", size = 3.8, hjust = 0) +
  
  annotate("text", x = x_min + x_pad*0.3, y = y_min + y_pad*0.5,
           label = "Q3: Niche/Declining",
           color = "gray30", fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = x_min + x_pad*0.3, y = y_min + y_pad*0.25,
           label = "(Low Share, Low Growth)",
           color = "gray30", size = 3.8, hjust = 0) +
  
  annotate("text", x = x_max - x_pad*0.3, y = y_min + y_pad*0.5,
           label = "Q4: Saturated/Mature",
           color = "#2980B9", fontface = "bold", size = 5.5, hjust = 1) +
  annotate("text", x = x_max - x_pad*0.3, y = y_min + y_pad*0.25,
           label = "(High Share, Low Growth)",
           color = "#2980B9", size = 3.8, hjust = 1) +
  
  # 범례에 한글명 표시 — labels로 변경
  scale_color_manual(values = cluster_colors_8,
                     labels = setNames(df_cluster_plot$Legend_Label, 
                                       df_cluster_plot$Cluster_Code),
                     guide = "none") +
  scale_fill_manual(values = cluster_colors_8,
                    labels = setNames(df_cluster_plot$Legend_Label, 
                                      df_cluster_plot$Cluster_Code),
                    name = "메가 클러스터") +
  scale_size_continuous(range = c(18, 38), guide = "none") +
  
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  
  labs(
    title = "Technology Life Cycle (TLC) Portfolio Matrix — 8 Mega Clusters",
    subtitle = "B-Spline 평균 도함수 모멘텀 (2020-2025), 95% 신뢰구간 표시",
    x = "토픽 비중 합계 (%)",
    y = "성장 모멘텀 (B-Spline 평균 도함수, ×100)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.margin = margin(15, 20, 15, 15)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 8, shape = 21, stroke = 1.5)))

print(p_cluster_tlc)

ggsave("STM_TLC_Portfolio_Matrix_8Clusters_BSpline.png",
       plot = p_cluster_tlc,
       width = 14, height = 10, dpi = 450)
cat("✅ 저장: STM_TLC_Portfolio_Matrix_8Clusters_BSpline.png (dpi=450)\n")


# ----------------------------------------------------------------
# 9-2. 토픽 사분면 — 점은 색만, 라벨은 화살표로 분리
# ----------------------------------------------------------------

df_topic_plot <- data.frame(
  Topic = paste0("T", 1:20),
  Cluster_Code = topic_cluster_map$Cluster_Code,
  Proportion_pct = topic_proportion * 100,
  Momentum = momentum_inner_mean,
  CI_Lower = momentum_inner_lower,
  CI_Upper = momentum_inner_upper,
  stringsAsFactors = FALSE
)

mid_x_topic <- median(df_topic_plot$Proportion_pct)
mid_y_topic <- median(df_topic_plot$Momentum)

x_range_t <- range(df_topic_plot$Proportion_pct)
y_range_t <- range(c(df_topic_plot$CI_Lower, df_topic_plot$CI_Upper))
x_pad_t <- diff(x_range_t) * 0.15
y_pad_t <- diff(y_range_t) * 0.18

x_min_t <- x_range_t[1] - x_pad_t
x_max_t <- x_range_t[2] + x_pad_t
y_min_t <- y_range_t[1] - y_pad_t
y_max_t <- y_range_t[2] + y_pad_t


p_topic_tlc <- ggplot(df_topic_plot, 
                      aes(x = Proportion_pct, y = Momentum)) +
  
  # 사분면 배경
  annotate("rect", xmin = mid_x_topic, xmax = x_max_t, 
           ymin = mid_y_topic, ymax = y_max_t,
           fill = "#27AE60", alpha = 0.06) +
  annotate("rect", xmin = x_min_t, xmax = mid_x_topic,
           ymin = mid_y_topic, ymax = y_max_t,
           fill = "#E74C3C", alpha = 0.06) +
  annotate("rect", xmin = x_min_t, xmax = mid_x_topic,
           ymin = y_min_t, ymax = mid_y_topic,
           fill = "gray60", alpha = 0.06) +
  annotate("rect", xmin = mid_x_topic, xmax = x_max_t,
           ymin = y_min_t, ymax = mid_y_topic,
           fill = "#2980B9", alpha = 0.06) +
  
  geom_vline(xintercept = mid_x_topic, linetype = "dashed", 
             color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = mid_y_topic, linetype = "dashed", 
             color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray20", linewidth = 0.4, alpha = 0.7) +
  
  # 신뢰구간 (얇게, 점보다 뒤에 그리도록 먼저)
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Cluster_Code),
                width = 0.08, linewidth = 0.6, alpha = 0.4) +
  
  # 점 (크기 = 비중, 색 = 클러스터) — 점 안에 글자 없음
  geom_point(aes(size = Proportion_pct, fill = Cluster_Code), 
             alpha = 0.85, shape = 21, color = "white", stroke = 0.8) +
  
  # 라벨을 화살표로 분리 — 겹침 자동 해결
  geom_text_repel(aes(label = Topic, color = Cluster_Code),
                  size = 4.2, fontface = "bold",
                  box.padding = 0.6, point.padding = 0.5,
                  segment.color = "gray50", segment.size = 0.4,
                  segment.alpha = 0.7,
                  min.segment.length = 0.1,
                  force = 2.5,
                  max.overlaps = Inf,
                  family = "sans",
                  show.legend = FALSE) +
  
  # 사분면 라벨
  annotate("text", x = x_max_t - x_pad_t*0.2, y = y_max_t - y_pad_t*0.2,
           label = "Q1: Mainstream",
           color = "#27AE60", fontface = "bold", size = 5, hjust = 1) +
  annotate("text", x = x_min_t + x_pad_t*0.2, y = y_max_t - y_pad_t*0.2,
           label = "Q2: Emerging",
           color = "#E74C3C", fontface = "bold", size = 5, hjust = 0) +
  annotate("text", x = x_min_t + x_pad_t*0.2, y = y_min_t + y_pad_t*0.3,
           label = "Q3: Niche/Declining",
           color = "gray30", fontface = "bold", size = 5, hjust = 0) +
  annotate("text", x = x_max_t - x_pad_t*0.2, y = y_min_t + y_pad_t*0.3,
           label = "Q4: Saturated/Mature",
           color = "#2980B9", fontface = "bold", size = 5, hjust = 1) +
  
  scale_color_manual(values = cluster_colors_8, guide = "none") +
  scale_fill_manual(values = cluster_colors_8, name = "Cluster") +
  scale_size_continuous(range = c(6, 18), guide = "none") +
  
  coord_cartesian(xlim = c(x_min_t, x_max_t), ylim = c(y_min_t, y_max_t)) +
  
  labs(
    title = "Technology Life Cycle (TLC) Portfolio Matrix — 20 Topics",
    subtitle = "B-Spline 평균 도함수 모멘텀 (2020-2025), 95% 신뢰구간 표시 / 색상=메가 클러스터",
    x = "토픽 비중 (%)",
    y = "성장 모멘텀 (B-Spline 평균 도함수, ×100)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.margin = margin(15, 20, 15, 15)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 7, shape = 21)))

print(p_topic_tlc)

ggsave("STM_TLC_Portfolio_Matrix_20Topics_BSpline.png",
       plot = p_topic_tlc,
       width = 14, height = 10, dpi = 450)
cat("✅ 저장: STM_TLC_Portfolio_Matrix_20Topics_BSpline.png (dpi=450)\n")


cat("\n=== 시각화 v3 저장 완료 ===\n")
cat("개선 사항:\n")
cat("  - 클러스터 그림: 점 안에 A~H 알파벳, 한글명은 범례\n")
cat("  - 토픽 그림: 점은 색상만, T번호는 화살표로 분리 표시\n")

# ================================================================
# [10] STM_TLC_TMR_Combined_Table.csv 갱신
# ================================================================

# ----------------------------------------------------------------
# 10-1. 기존 df_tlc 객체 확인 (TMR 등 보존)
# ----------------------------------------------------------------
cat("=== 기존 df_tlc 객체 확인 ===\n")
if (exists("df_tlc")) {
  cat("✅ df_tlc 존재. 컬럼:\n")
  print(names(df_tlc))
  cat("\nhead:\n")
  print(head(df_tlc, 3))
} else {
  cat("❌ df_tlc 없음. Fintech_Project.R [4.2.3 보완] 블록을 먼저 실행해야 함.\n")
  cat("   또는 TMR 정보가 담긴 객체명을 확인해야 함.\n")
  cat("\n메모리의 'tlc' 또는 'df_'로 시작하는 객체:\n")
  print(ls(pattern = "^(df_|tlc)"))
}


# ----------------------------------------------------------------
# 10-2. 새 종합 테이블 구성 — Inner6 기반
# ----------------------------------------------------------------
# 클러스터 한글명 매핑
cluster_kr_map <- data.frame(
  Cluster_Code = LETTERS[1:8],
  Cluster_Name_KR = cluster_names_kr[LETTERS[1:8]],
  stringsAsFactors = FALSE
)

# 새 종합 테이블 — 토픽 단위
df_export_new <- data.frame(
  Topic = paste0("T", 1:n_topics),
  Topic_Label = topic_label_short,
  Mega_Cluster = topic_cluster_map$Cluster_Code,
  Cluster_Name_KR = cluster_names_kr[topic_cluster_map$Cluster_Code],
  Proportion_pct = round(topic_proportion * 100, 2),
  Growth_Momentum_OLS = round(momentum_ols, 4),
  Growth_Momentum_BSpline = round(momentum_inner_mean, 4),
  BSpline_CI_Lower = round(momentum_inner_lower, 4),
  BSpline_CI_Upper = round(momentum_inner_upper, 4),
  stringsAsFactors = FALSE
)


# ----------------------------------------------------------------
# 10-3. Impact, TMR — 기존 df_tlc에서 가져오기
# ----------------------------------------------------------------
if (exists("df_tlc")) {
  # df_tlc는 토픽 순서대로 정렬되어 있다고 가정 (T1~T20)
  if ("Impact" %in% names(df_tlc)) {
    df_export_new$Impact <- round(df_tlc$Impact, 3)
    cat("✅ Impact 컬럼 추가\n")
  } else {
    cat("⚠️ df_tlc에 Impact 컬럼 없음 — Impact 재계산\n")
    # 직접 재계산 (Fintech_Project.R 라인 1495와 동일 로직)
    c_adj_safe <- out3$meta$C_adj_i
    valid_idx2 <- !is.na(c_adj_safe) & !is.na(pub_year)
    theta_mat_v2 <- final_stm_20_adj$theta[valid_idx2, ]
    c_adj_v2 <- c_adj_safe[valid_idx2]
    df_export_new$Impact <- round(
      colSums(theta_mat_v2 * c_adj_v2) / colSums(theta_mat_v2), 3)
  }
  
  if ("TMR" %in% names(df_tlc)) {
    df_export_new$TMR_pct <- round(df_tlc$TMR * 100, 1)
    cat("✅ TMR 컬럼 추가\n")
  } else {
    cat("⚠️ df_tlc에 TMR 컬럼 없음 — TMR은 별도 산출 필요\n")
    df_export_new$TMR_pct <- NA
  }
} else {
  cat("⚠️ df_tlc 없음 — Impact, TMR 컬럼은 NA 처리\n")
  df_export_new$Impact <- NA
  df_export_new$TMR_pct <- NA
}


# ----------------------------------------------------------------
# 10-4. 새 사분면 분류 (Inner6 기준)
# ----------------------------------------------------------------
mid_x_topic_new <- median(df_export_new$Proportion_pct)
mid_y_topic_new <- median(df_export_new$Growth_Momentum_BSpline)

df_export_new$Quadrant <- case_when(
  df_export_new$Proportion_pct >= mid_x_topic_new & 
    df_export_new$Growth_Momentum_BSpline >= mid_y_topic_new ~ "Q1: Mainstream",
  df_export_new$Proportion_pct <  mid_x_topic_new & 
    df_export_new$Growth_Momentum_BSpline >= mid_y_topic_new ~ "Q2: Emerging",
  df_export_new$Proportion_pct <  mid_x_topic_new & 
    df_export_new$Growth_Momentum_BSpline <  mid_y_topic_new ~ "Q3: Niche/Declining",
  df_export_new$Proportion_pct >= mid_x_topic_new & 
    df_export_new$Growth_Momentum_BSpline <  mid_y_topic_new ~ "Q4: Saturated/Mature"
)


# ----------------------------------------------------------------
# 10-5. 통계적 유의성 컬럼 추가 (보너스)
# ----------------------------------------------------------------
df_export_new$Significance <- case_when(
  df_export_new$BSpline_CI_Lower > 0 ~ "↑ 유의 (p<0.05)",
  df_export_new$BSpline_CI_Upper < 0 ~ "↓ 유의 (p<0.05)",
  TRUE ~ "ns"
)


# ----------------------------------------------------------------
# 10-6. 최종 정렬 및 저장
# ----------------------------------------------------------------
df_export_final <- df_export_new %>%
  select(Topic, Topic_Label, Mega_Cluster, Cluster_Name_KR,
         Proportion_pct, 
         Growth_Momentum_OLS, Growth_Momentum_BSpline, 
         BSpline_CI_Lower, BSpline_CI_Upper, Significance,
         Impact, TMR_pct, Quadrant) %>%
  arrange(Quadrant, desc(Growth_Momentum_BSpline))

cat("\n=== 갱신된 STM_TLC_TMR_Combined_Table (Inner6 기반) ===\n")
print(df_export_final)


# ----------------------------------------------------------------
# 10-7. 파일 저장 — 두 버전
# ----------------------------------------------------------------
# v2 (Inner6 기반, 완전판)
write.csv(df_export_final, 
          "STM_TLC_TMR_Combined_Table_BSpline.csv", 
          row.names = FALSE)
cat("\n✅ 저장: STM_TLC_TMR_Combined_Table_BSpline.csv (Inner6, OLS 병기, CI 포함)\n")

# 본문 표 4.2.4용 — 간결판 (기존 컬럼 구성 유지)
df_export_simple <- df_export_final %>%
  rename(Growth_Momentum = Growth_Momentum_BSpline,
         Cluster_Name = Cluster_Name_KR) %>%
  select(Topic, Mega_Cluster, Cluster_Name,
         Proportion_pct, Growth_Momentum, Impact, TMR_pct, Quadrant)

write.csv(df_export_simple,
          "STM_TLC_TMR_Combined_Table.csv",
          row.names = FALSE)
cat("✅ 저장: STM_TLC_TMR_Combined_Table.csv (본문 표 4.2.4용 간결판)\n")


# ----------------------------------------------------------------
# 10-8. 클러스터 단위 종합 테이블도 별도 저장
# ----------------------------------------------------------------
cluster_export <- cluster_momentum %>%
  mutate(
    Proportion_pct = round(Total_Proportion * 100, 2),
    Growth_Momentum_BSpline = round(Momentum_Weighted, 4),
    CI_Lower = round(CI_Lower, 4),
    CI_Upper = round(CI_Upper, 4),
    Significance = case_when(
      CI_Lower > 0 ~ "↑ 유의 (p<0.05)",
      CI_Upper < 0 ~ "↓ 유의 (p<0.05)",
      TRUE ~ "ns"
    )
  ) %>%
  select(Cluster_Code, Cluster_Name_KR, Cluster_Name_EN, 
         Topic_Count, Topics_in_Cluster,
         Proportion_pct, Growth_Momentum_BSpline, 
         CI_Lower, CI_Upper, Significance, Quadrant)

cat("\n=== 8대 메가 클러스터 종합 테이블 ===\n")
print(cluster_export)

write.csv(cluster_export,
          "STM_TLC_8Clusters_Summary_BSpline.csv",
          row.names = FALSE)
cat("\n✅ 저장: STM_TLC_8Clusters_Summary_BSpline.csv\n")


# ----------------------------------------------------------------
# 10-9. 저장 산출물 일람
# ----------------------------------------------------------------
cat("\n=========================================\n")
cat(" [10] 산출 완료 파일 목록\n")
cat("=========================================\n")
cat("  1. STM_TLC_TMR_Combined_Table_BSpline.csv  (완전판: 토픽 20개, OLS+BSpline+CI)\n")
cat("  2. STM_TLC_TMR_Combined_Table.csv           (간결판: 본문 표 4.2.4용)\n")
cat("  3. STM_TLC_8Clusters_Summary_BSpline.csv    (8대 메가 클러스터 종합)\n")
cat("\n저장 경로:", getwd(), "\n")

# ================================================================
# [10-v2] Impact 복구 + 본문용 CSV 재생성
# ================================================================

# ----------------------------------------------------------------
# 10v2-1. tlc 객체에서 Impact + 한글 라벨 가져오기
# ----------------------------------------------------------------
cat("=== tlc 객체 활용 ===\n")
cat("class:", class(tlc), " dim:", dim(tlc), "\n")
cat("컬럼:", paste(names(tlc), collapse=", "), "\n\n")

# tlc는 T1, T2, ... 순서가 아닐 수 있으니 Topic 컬럼 기준으로 정렬
tlc_sorted <- tlc[match(paste0("T", 1:20), tlc$Topic), ]

cat("정렬 후 head:\n")
print(head(tlc_sorted, 3))


# ----------------------------------------------------------------
# 10v2-2. Impact, 한글 라벨, Cluster_Label, Color 통합
# ----------------------------------------------------------------
df_export_new$Impact <- round(tlc_sorted$Impact, 3)
df_export_new$Topic_Label_KR <- tlc_sorted$Label
df_export_new$Cluster_Label_Full <- tlc_sorted$Cluster_Label
df_export_new$Color <- tlc_sorted$Color

cat("\n✅ Impact 복구 완료\n")
cat("✅ 한글 토픽 라벨 추가 완료\n\n")


# ----------------------------------------------------------------
# 10v2-3. 검증 — tlc의 기존 Growth vs 우리의 OLS 일치 확인
# ----------------------------------------------------------------
cat("=== 검증: tlc$Growth vs 우리의 OLS 모멘텀 ===\n")
verify_df <- data.frame(
  Topic = tlc_sorted$Topic,
  tlc_Growth = tlc_sorted$Growth,
  Our_OLS = round(momentum_ols, 2),
  Diff = round(tlc_sorted$Growth - momentum_ols, 4)
)
print(verify_df)
cat("\n→ 차이가 작으면 (소수점 자리수 차이) tlc는 OLS 기반 산출이 맞음\n")
cat("  차이가 크면 tlc는 다른 산출 방식 — 재검토 필요\n\n")


# ----------------------------------------------------------------
# 10v2-4. 최종 종합 테이블 — 한글 라벨 + Impact 포함
# ----------------------------------------------------------------
df_export_final2 <- df_export_new %>%
  select(Topic, Topic_Label_KR, Topic_Label, 
         Mega_Cluster, Cluster_Name_KR,
         Proportion_pct, 
         Growth_Momentum_OLS, Growth_Momentum_BSpline, 
         BSpline_CI_Lower, BSpline_CI_Upper, Significance,
         Impact, TMR_pct, Quadrant) %>%
  arrange(Quadrant, desc(Growth_Momentum_BSpline))

cat("=== 갱신된 종합 테이블 (Impact 포함, TMR은 B-3에서 채워질 예정) ===\n")
print(df_export_final2)


# ----------------------------------------------------------------
# 10v2-5 (수정). 본문 표 4.2.4용 간결판 — 한글 라벨 사용
# ----------------------------------------------------------------
df_export_simple2 <- df_export_final2 %>%
  # 영문 Topic_Label 먼저 제거
  select(-Topic_Label) %>%
  # 한글 라벨을 Topic_Label로 rename
  rename(
    Growth_Momentum = Growth_Momentum_BSpline,
    Topic_Label = Topic_Label_KR,
    Cluster_Name = Cluster_Name_KR
  ) %>%
  select(Topic, Topic_Label, Mega_Cluster, Cluster_Name,
         Proportion_pct, Growth_Momentum, Impact, TMR_pct, Quadrant)

cat("\n=== 본문 표 4.2.4용 간결판 ===\n")
print(df_export_simple2)


# ----------------------------------------------------------------
# 10v2-6. CSV 재저장
# ----------------------------------------------------------------
# 완전판 (모든 컬럼)
write.csv(df_export_final2, 
          "STM_TLC_TMR_Combined_Table_BSpline.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")
cat("\n✅ 갱신: STM_TLC_TMR_Combined_Table_BSpline.csv (한글 라벨 + Impact)\n")

# 본문용 간결판
write.csv(df_export_simple2,
          "STM_TLC_TMR_Combined_Table.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
cat("✅ 갱신: STM_TLC_TMR_Combined_Table.csv (본문 표 4.2.4용)\n")


# ----------------------------------------------------------------
# 10v2-7. TMR 처리 안내
# ----------------------------------------------------------------
cat("\n=========================================\n")
cat(" 산출 완료 — TMR 처리 안내\n")
cat("=========================================\n")
cat("✅ Impact: tlc 객체에서 성공적으로 복구\n")
cat("⚠️ TMR_pct: 현재 NA (B-3 Gompertz TMR 작업에서 채워질 예정)\n")

# ----------------------------------------------------------------
# 10v2-6. CSV 재저장 — 기존 파일 덮어쓰기
# ----------------------------------------------------------------
# 완전판 (모든 컬럼)
write.csv(df_export_final2, 
          "STM_TLC_TMR_Combined_Table_BSpline.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")
cat("\n✅ 갱신: STM_TLC_TMR_Combined_Table_BSpline.csv (한글 라벨 + Impact)\n")

# 본문용 간결판
write.csv(df_export_simple2,
          "STM_TLC_TMR_Combined_Table.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
cat("✅ 갱신: STM_TLC_TMR_Combined_Table.csv (본문 표 4.2.4용)\n")


# ----------------------------------------------------------------
# 10v2-7. TMR 처리 안내
# ----------------------------------------------------------------
cat("\n=========================================\n")
cat(" 산출 완료 — TMR 처리 안내\n")
cat("=========================================\n")
cat("✅ Impact: tlc 객체에서 성공적으로 복구\n")
cat("⚠️ TMR_pct: 현재 NA (B-3 Gompertz TMR 작업에서 채워질 예정)\n")
cat("\n본문 표 4.2.4 작성 시:\n")
cat("  - 현재: Topic, Topic_Label, Mega_Cluster, Cluster_Name,\n")
cat("          Proportion_pct, Growth_Momentum, Impact, Quadrant 사용 가능\n")
cat("  - TMR_pct는 B-3 완료 후 추가 병합\n")