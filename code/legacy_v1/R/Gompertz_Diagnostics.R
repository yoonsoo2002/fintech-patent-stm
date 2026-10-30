# ============================================================
# B-3: Gompertz TMR 수렴 진단 및 95% CI 부록 산출
# ------------------------------------------------------------
# 작성일: 2026-05-12
# 작성자: 조윤수
# 목적:   본문 4.2.3절 TMR%는 그대로 유지하고,
#         부록용 진단 정보(수렴 여부, 반복 횟수)와
#         파라미터 95% CI를 별도 산출
# 입력:   STM_full_workspace_Phase0.RData
# 출력:   STM_Gompertz_Diagnostics.csv
# 원칙:   STM 모델 재학습 금지, 본문 TMR% 불변
# ============================================================

# --- 작업 디렉토리 설정 ---
setwd(here::here("data"))  # was: absolute local path

# 현재 위치 확인
getwd()

# 핵심 파일 존재 점검
file.exists("STM_full_workspace_Phase0.RData")
file.exists("Topic_Yearly_Mean_Values_Filtered.csv")

# ============================================================
# 블록 ②: 패키지 로드
# ============================================================

library(stats)   # nls, confint, confint.default (기본)
library(dplyr)   # 데이터 처리

# boot 패키지 — 처음 실행 시 install 필요
# install.packages("boot")   # 안 깔려 있으면 # 제거 후 1회 실행
library(boot)

# 재현성 시드 (논문 전체 2026 통일)
set.seed(2026)

cat("✅ 패키지 로드 완료\n")
cat("   R 버전:", R.version.string, "\n\n")

# ============================================================
# 블록 ③: STM Workspace 로드 및 객체 확인
# ============================================================

cat("[STM workspace 로드 중...]\n")
load("STM_full_workspace_Phase0.RData")
cat("✅ 로드 완료\n\n")

# 로드된 객체 전체 목록
cat("===== 로드된 객체 목록 (전체) =====\n")
print(ls())

# B-3 작업에 필요한 핵심 객체 존재 확인
cat("\n===== 핵심 객체 존재 점검 =====\n")
cat("final_stm_20_adj 존재 :", exists("final_stm_20_adj"), "\n")
cat("out3 존재            :", exists("out3"), "\n")

# B-2가 추가했다고 한 객체들 (있으면 좋음, 없어도 진행 가능)
cat("\n===== B-2 추가 객체 (참고) =====\n")
cat("effect_country_US 존재:", exists("effect_country_US"), "\n")
cat("tlc 존재              :", exists("tlc"), "\n")
cat("topic_cluster_map 존재:", exists("topic_cluster_map"), "\n")

# ============================================================
# 블록 ④: Gompertz 입력 데이터 재구성
# (Fintech_Project.R 메인 블록과 100% 동일한 처리)
# ============================================================

# (1) θ 행렬 추출
theta_mat <- final_stm_20_adj$theta
colnames(theta_mat) <- paste0('T', 1:20)

# (2) 공변량 추출
c_adj    <- out3$meta$C_adj_i
pub_year <- as.numeric(as.character(out3$meta$Publication.Year))

# (3) NA 제거
valid_idx <- !is.na(c_adj) & !is.na(pub_year)
theta_mat <- theta_mat[valid_idx, ]
c_adj     <- c_adj[valid_idx]
pub_year  <- pub_year[valid_idx]

# (4) 2026년 제외 (데이터 윈도우 2019~2025 고정, 본문 수치 재현용)
year_filter <- pub_year >= 2019 & pub_year <= 2025
theta_mat <- theta_mat[year_filter, ]
c_adj     <- c_adj[year_filter]
pub_year  <- pub_year[year_filter]

# (5) 점검
cat("\n===== 입력 데이터 점검 =====\n")
cat("theta_mat 차원      :", dim(theta_mat)[1], "행 ×", dim(theta_mat)[2], "열\n")
cat("pub_year 길이       :", length(pub_year), "\n")
cat("pub_year 범위       :", min(pub_year), "~", max(pub_year), "\n")
cat("연도별 특허 수:\n")
print(table(pub_year))

# ============================================================
# 블록 ⑤: 본문 TMR% 재현 검증
# Fintech_Project.R 메인 블록의 적합을 그대로 재현하여
# 본문 4.2.3절 수치(T9 72.2%, T17 72.5% 등)와 일치 확인
# ============================================================

cat("\n===== 메인 코드 Gompertz 적합 재현 =====\n\n")

# 결과 컨테이너
tmr_replicate <- numeric(20)
fit_success   <- logical(20)

# 토픽별 적합
for (i in 1:20) {
  # 메인 코드와 동일한 입력 구성
  yearly_counts    <- tapply(theta_mat[, i], pub_year, sum)
  years_seq        <- 1:length(yearly_counts)
  cumulative_counts <- cumsum(yearly_counts)
  
  # Gompertz 적합 (메인 코드와 동일)
  fit <- tryCatch({
    nls(cumulative_counts ~ SSgompertz(years_seq, Asym, b2, b3))
  }, error = function(e) NULL)
  
  if (!is.null(fit)) {
    m_asymptote   <- coef(fit)["Asym"]
    current_value <- max(cumulative_counts)
    tmr_replicate[i] <- current_value / m_asymptote
    fit_success[i]   <- TRUE
  } else {
    tmr_replicate[i] <- NA
    fit_success[i]   <- FALSE
  }
}

# 결과 정리
tmr_df <- data.frame(
  Topic       = paste0("T", 1:20),
  TMR_pct     = round(tmr_replicate * 100, 1),
  Fit_Success = fit_success
)

cat("===== Topic별 TMR% 재현 결과 =====\n")
print(tmr_df)

# 본문 인용값과 비교 (핸드오프 §5 기준)
cat("\n===== 본문 4.2.3절 인용값 대조 =====\n")
expected <- data.frame(
  Topic        = c("T9", "T17", "T11", "T3", "T15", "T5", "T16", "T2", "T14", "T6"),
  TMR_본문     = c(72.2, 72.5, 74.3, 74.4, 74.9, 84.7, 83.7, 83.1, 83.1, 82.4),
  분류         = c("유망", "유망", "유망", "유망", "유망", "포화", "포화", "포화", "포화", "포화")
)
expected$TMR_재현 <- tmr_df$TMR_pct[match(expected$Topic, tmr_df$Topic)]
expected$차이     <- round(expected$TMR_재현 - expected$TMR_본문, 1)

print(expected)

# 일치 여부 자동 판정
max_diff <- max(abs(expected$차이), na.rm = TRUE)
cat("\n===== 일치 여부 판정 =====\n")
cat("최대 차이:", max_diff, "%p\n")

if (is.na(max_diff)) {
  cat("⚠️  일부 토픽 적합 실패 — 보고 필요\n")
} else if (max_diff < 0.5) {
  cat("✅ 본문 수치와 일치 (오차 0.5%p 미만, 부동소수점 수준)\n")
} else if (max_diff < 2.0) {
  cat("⚪ 본문 수치와 근사 일치 (오차 0.5~2.0%p) — 진행 가능, 차이 원인 확인 권장\n")
} else {
  cat("❌ 본문 수치와 큰 차이 — 작업 중단하고 입력 데이터 점검 필요\n")
}
# ============================================================
# 결정적 진단: 다양한 입력·공식으로 TMR 재산출
# T9를 기준으로 5가지 방식 비교, 본문 72.2%에 가장 가까운 방식 식별
# ============================================================

cat("\n=========================================================\n")
cat("결정적 진단: T9를 기준으로 5가지 산출 방식 비교\n")
cat("본문 인용값 T9 = 72.2%, T17 = 72.5%\n")
cat("=========================================================\n\n")

# T9, T17 두 토픽을 동시 검증
target_topics <- c("T9", "T17")
expected_target <- c(72.2, 72.5)
names(expected_target) <- target_topics

# 5가지 방식 정의 ------------------------------------------------------
methods <- list()

# 방식 1: 누적 합계 (메인 코드 그대로)
methods[["1_cumsum_sum"]] <- function(topic_idx) {
  yearly <- tapply(theta_mat[, topic_idx], pub_year, sum)
  cum <- cumsum(yearly)
  fit <- tryCatch(nls(cum ~ SSgompertz(1:length(cum), Asym, b2, b3)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NA)
  max(cum) / coef(fit)["Asym"]
}

# 방식 2: 누적 평균
methods[["2_cumsum_mean"]] <- function(topic_idx) {
  yearly <- tapply(theta_mat[, topic_idx], pub_year, mean)
  cum <- cumsum(yearly)
  fit <- tryCatch(nls(cum ~ SSgompertz(1:length(cum), Asym, b2, b3)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NA)
  max(cum) / coef(fit)["Asym"]
}

# 방식 3: 연도별 합계 자체에 적합 (누적 없음)
methods[["3_yearly_sum"]] <- function(topic_idx) {
  yearly <- tapply(theta_mat[, topic_idx], pub_year, sum)
  fit <- tryCatch(nls(yearly ~ SSgompertz(1:length(yearly), Asym, b2, b3)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NA)
  max(yearly) / coef(fit)["Asym"]
}

# 방식 4: 연도별 평균 자체에 적합
methods[["4_yearly_mean"]] <- function(topic_idx) {
  yearly <- tapply(theta_mat[, topic_idx], pub_year, mean)
  fit <- tryCatch(nls(yearly ~ SSgompertz(1:length(yearly), Asym, b2, b3)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NA)
  max(yearly) / coef(fit)["Asym"]
}

# 방식 5: 누적 합계 + 변형 TMR 공식 (current / (current + (Asym - current)))
#   = current / Asym 이지만 명시적 형태
#   다른 변형: 1 - exp(-...) 형태
methods[["5_cumsum_logitlike"]] <- function(topic_idx) {
  yearly <- tapply(theta_mat[, topic_idx], pub_year, sum)
  cum <- cumsum(yearly)
  fit <- tryCatch(nls(cum ~ SSgompertz(1:length(cum), Asym, b2, b3)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NA)
  # Asym에 도달하지 않은 비율을 다른 방식으로
  # (Asym - max(cum)) / Asym 의 역
  1 - (coef(fit)["Asym"] - max(cum)) / coef(fit)["Asym"]
  # ← 이건 방식 1과 수학적으로 동일, 검증용
}

# 실행 ----------------------------------------------------------------
results <- data.frame(
  Method = character(),
  T9     = numeric(),
  T17    = numeric(),
  stringsAsFactors = FALSE
)

for (method_name in names(methods)) {
  vals <- sapply(target_topics, function(t) methods[[method_name]](t))
  results <- rbind(results, data.frame(
    Method = method_name,
    T9     = round(vals["T9"] * 100, 1),
    T17    = round(vals["T17"] * 100, 1)
  ))
}

# 본문 인용값 추가
results <- rbind(results, data.frame(
  Method = "BODY_TEXT (본문)",
  T9     = 72.2,
  T17    = 72.5
))

cat("===== 5가지 방식 결과 대조 =====\n")
print(results)

# 본문에 가장 가까운 방식 식별
cat("\n===== 본문과의 차이 =====\n")
diff_df <- data.frame(
  Method  = results$Method[1:5],
  T9_차이 = round(results$T9[1:5] - 72.2, 1),
  T17_차이 = round(results$T17[1:5] - 72.5, 1)
)
diff_df$합산_절대차이 <- abs(diff_df$T9_차이) + abs(diff_df$T17_차이)
print(diff_df[order(diff_df$합산_절대차이), ])

cat("\n===== 판정 =====\n")
best <- diff_df$Method[which.min(diff_df$합산_절대차이)]
cat("본문에 가장 가까운 방식:", best, "\n")
cat("합산 절대 차이:", min(diff_df$합산_절대차이), "%p\n")

# T9의 연도별 합계와 평균 확인
yearly_sum_T9  <- tapply(theta_mat[, "T9"], pub_year, sum)
yearly_mean_T9 <- tapply(theta_mat[, "T9"], pub_year, mean)

cat("===== T9 연도별 합계 =====\n")
print(round(yearly_sum_T9, 2))

cat("\n===== T9 연도별 평균 =====\n")
print(round(yearly_mean_T9, 4))

cat("\n===== T9 누적 합계 =====\n")
print(round(cumsum(yearly_sum_T9), 2))

cat("\n===== T9 누적 평균 =====\n")
print(round(cumsum(yearly_mean_T9), 4))

# 방식 1: 누적 합계 (메인 코드)
cat("\n===== 방식 1: 누적 합계 =====\n")
cum_T9 <- cumsum(yearly_sum_T9)
fit1 <- tryCatch(
  nls(cum_T9 ~ SSgompertz(1:length(cum_T9), Asym, b2, b3)),
  error = function(e) {cat("적합 실패:", e$message, "\n"); NULL}
)
if (!is.null(fit1)) {
  asym1 <- coef(fit1)["Asym"]
  tmr1  <- max(cum_T9) / asym1
  cat("Asym =", round(asym1, 2), "\n")
  cat("최대 누적값 =", round(max(cum_T9), 2), "\n")
  cat("TMR = ", round(tmr1 * 100, 1), "%\n")
}
# 방식 2: 누적 평균
cat("\n===== 방식 2: 누적 평균 =====\n")
cum_mean_T9 <- cumsum(yearly_mean_T9)
fit2 <- tryCatch(
  nls(cum_mean_T9 ~ SSgompertz(1:length(cum_mean_T9), Asym, b2, b3)),
  error = function(e) {cat("적합 실패:", e$message, "\n"); NULL}
)
if (!is.null(fit2)) {
  asym2 <- coef(fit2)["Asym"]
  tmr2  <- max(cum_mean_T9) / asym2
  cat("Asym =", round(asym2, 4), "\n")
  cat("최대 누적값 =", round(max(cum_mean_T9), 4), "\n")
  cat("TMR = ", round(tmr2 * 100, 1), "%\n")
}
# 방식 3: 누적 안 함, 연도별 합계 그대로
cat("\n===== 방식 3: 연도별 합계 (누적 X) =====\n")
fit3 <- tryCatch(
  nls(yearly_sum_T9 ~ SSgompertz(1:length(yearly_sum_T9), Asym, b2, b3)),
  error = function(e) {cat("적합 실패:", e$message, "\n"); NULL}
)
if (!is.null(fit3)) {
  asym3 <- coef(fit3)["Asym"]
  tmr3  <- max(yearly_sum_T9) / asym3
  cat("Asym =", round(asym3, 2), "\n")
  cat("최대값 =", round(max(yearly_sum_T9), 2), "\n")
  cat("TMR = ", round(tmr3 * 100, 1), "%\n")
}
# 방식 4: 연도별 평균 그대로
cat("\n===== 방식 4: 연도별 평균 (누적 X) =====\n")
fit4 <- tryCatch(
  nls(yearly_mean_T9 ~ SSgompertz(1:length(yearly_mean_T9), Asym, b2, b3)),
  error = function(e) {cat("적합 실패:", e$message, "\n"); NULL}
)
if (!is.null(fit4)) {
  asym4 <- coef(fit4)["Asym"]
  tmr4  <- max(yearly_mean_T9) / asym4
  cat("Asym =", round(asym4, 4), "\n")
  cat("최대값 =", round(max(yearly_mean_T9), 4), "\n")
  cat("TMR = ", round(tmr4 * 100, 1), "%\n")
}
cat("\n=========================================================\n")
cat("===== T9 4가지 방식 결과 요약 =====\n")
cat("본문 인용값: 72.2%\n")
cat("=========================================================\n")
cat("방식 1 (누적 합계):", if(exists("tmr1")) round(tmr1*100,1) else "실패", "%\n")
cat("방식 2 (누적 평균):", if(exists("tmr2")) round(tmr2*100,1) else "실패", "%\n")
cat("방식 3 (연도별 합계):", if(exists("tmr3")) round(tmr3*100,1) else "실패", "%\n")
cat("방식 4 (연도별 평균):", if(exists("tmr4")) round(tmr4*100,1) else "실패", "%\n")

# ============================================================
# 방식 3 (연도별 합계 그대로 Gompertz 적합) — 20개 토픽 전체 검증
# 본문 수치와 가장 가까운 방식인지 최종 확인
# ============================================================

cat("\n===== 방식 3: 연도별 합계 그대로 Gompertz 적합 =====\n")
cat("입력: yearly_counts (누적 안 함)\n\n")

tmr_method3 <- numeric(20)
fit3_success <- logical(20)

for (i in 1:20) {
  yearly <- tapply(theta_mat[, i], pub_year, sum)
  fit <- tryCatch(
    nls(yearly ~ SSgompertz(1:length(yearly), Asym, b2, b3)),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    tmr_method3[i]   <- max(yearly) / coef(fit)["Asym"]
    fit3_success[i]  <- TRUE
  } else {
    tmr_method3[i]   <- NA
    fit3_success[i]  <- FALSE
  }
}

# 전체 결과
tmr_m3_df <- data.frame(
  Topic       = paste0("T", 1:20),
  TMR_방식1   = round(tmr_replicate * 100, 1),
  TMR_방식3   = round(tmr_method3 * 100, 1),
  Fit_Success = fit3_success
)
cat("===== 20개 토픽 전체: 방식 1 vs 방식 3 =====\n")
print(tmr_m3_df)

# 본문 인용값과 방식 3 대조
cat("\n===== 본문 인용값 vs 방식 3 =====\n")
body_quotes <- data.frame(
  Topic    = c("T9", "T17", "T11", "T3", "T15", "T5", "T16", "T2", "T14", "T6"),
  TMR_본문 = c(72.2, 72.5, 74.3, 74.4, 74.9, 84.7, 83.7, 83.1, 83.1, 82.4)
)
body_quotes$TMR_방식1   <- tmr_m3_df$TMR_방식1[match(body_quotes$Topic, tmr_m3_df$Topic)]
body_quotes$TMR_방식3   <- tmr_m3_df$TMR_방식3[match(body_quotes$Topic, tmr_m3_df$Topic)]
body_quotes$차이_방식1  <- round(body_quotes$TMR_방식1 - body_quotes$TMR_본문, 1)
body_quotes$차이_방식3  <- round(body_quotes$TMR_방식3 - body_quotes$TMR_본문, 1)
print(body_quotes)

# 평균 절대 차이로 판정
cat("\n===== 본문과의 평균 절대 차이 =====\n")
cat("방식 1 (누적합):  ", round(mean(abs(body_quotes$차이_방식1)), 2), "%p\n")
cat("방식 3 (연도별):  ", round(mean(abs(body_quotes$차이_방식3)), 2), "%p\n")

# 적합 실패 토픽 확인
cat("\n===== 방식 3 적합 실패 토픽 =====\n")
failed <- which(!fit3_success)
if (length(failed) == 0) {
  cat("없음 (전체 토픽 적합 성공)\n")
} else {
  cat("실패한 토픽:", paste0("T", failed, collapse = ", "), "\n")
}
# ============================================================
# 진단: 시간 축을 실제 연도로 바꿔 Gompertz 적합
# ============================================================

cat("\n===== 시간 축을 실제 연도(2019~2025)로 변경 =====\n")

tmr_yearaxis <- numeric(20)
for (i in 1:20) {
  yearly <- tapply(theta_mat[, i], pub_year, sum)
  cum <- cumsum(yearly)
  years_actual <- as.numeric(names(yearly))   # 2019, 2020, ..., 2025
  
  fit <- tryCatch(
    nls(cum ~ SSgompertz(years_actual, Asym, b2, b3)),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    tmr_yearaxis[i] <- max(cum) / coef(fit)["Asym"]
  } else {
    tmr_yearaxis[i] <- NA
  }
}

# 결과 + 본문 대조
result_df <- data.frame(
  Topic       = paste0("T", 1:20),
  TMR_방식1   = round(tmr_replicate * 100, 1),
  TMR_연도축  = round(tmr_yearaxis * 100, 1)
)
print(result_df)

# 본문 인용값 대조
body <- data.frame(
  Topic    = c("T9", "T17", "T11", "T3", "T15", "T5", "T16", "T2", "T14", "T6"),
  TMR_본문 = c(72.2, 72.5, 74.3, 74.4, 74.9, 84.7, 83.7, 83.1, 83.1, 82.4)
)
body$TMR_연도축 <- result_df$TMR_연도축[match(body$Topic, result_df$Topic)]
body$차이       <- round(body$TMR_연도축 - body$TMR_본문, 1)
cat("\n===== 본문 vs 연도축 방식 =====\n")
print(body)
cat("\n평균 절대 차이:", round(mean(abs(body$차이), na.rm = TRUE), 2), "%p\n")

# ============================================================
# 추가 진단: Logistic 모형으로 적합 (Gompertz의 대안)
# ============================================================

cat("\n===== Logistic 모형 적합 =====\n")

tmr_logis <- numeric(20)
for (i in 1:20) {
  yearly <- tapply(theta_mat[, i], pub_year, sum)
  cum <- cumsum(yearly)
  
  fit <- tryCatch(
    nls(cum ~ SSlogis(1:length(cum), Asym, xmid, scal)),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    tmr_logis[i] <- max(cum) / coef(fit)["Asym"]
  } else {
    tmr_logis[i] <- NA
  }
}

# 결과 비교
compare_models <- data.frame(
  Topic        = paste0("T", 1:20),
  TMR_Gompertz = round(tmr_replicate * 100, 1),
  TMR_Logistic = round(tmr_logis * 100, 1)
)
print(compare_models)

# 본문 인용값과 Logistic 대조
body_logis <- data.frame(
  Topic    = c("T9", "T17", "T11", "T3", "T15", "T5", "T16", "T2", "T14", "T6"),
  TMR_본문 = c(72.2, 72.5, 74.3, 74.4, 74.9, 84.7, 83.7, 83.1, 83.1, 82.4)
)
body_logis$TMR_Logistic <- compare_models$TMR_Logistic[match(body_logis$Topic, compare_models$Topic)]
body_logis$차이 <- round(body_logis$TMR_Logistic - body_logis$TMR_본문, 1)
cat("\n===== 본문 vs Logistic =====\n")
print(body_logis)
cat("\n평균 절대 차이:", round(mean(abs(body_logis$차이), na.rm = TRUE), 2), "%p\n")]

# ============================================================
# 두 모형 적합도 비교 — 데이터가 어느 모형을 선택하는가
# ============================================================

cat("\n===== Gompertz vs Logistic 적합도 비교 =====\n\n")

compare_df <- data.frame(
  Topic = character(),
  RSS_Gompertz = numeric(),
  RSS_Logistic = numeric(),
  AIC_Gompertz = numeric(),
  AIC_Logistic = numeric(),
  Better_Model = character(),
  stringsAsFactors = FALSE
)

for (i in 1:20) {
  yearly <- tapply(theta_mat[, i], pub_year, sum)
  cum <- cumsum(yearly)
  
  # Gompertz 적합
  fit_g <- tryCatch(
    nls(cum ~ SSgompertz(1:length(cum), Asym, b2, b3)),
    error = function(e) NULL
  )
  # Logistic 적합
  fit_l <- tryCatch(
    nls(cum ~ SSlogis(1:length(cum), Asym, xmid, scal)),
    error = function(e) NULL
  )
  
  if (!is.null(fit_g) && !is.null(fit_l)) {
    rss_g <- sum(resid(fit_g)^2)
    rss_l <- sum(resid(fit_l)^2)
    aic_g <- AIC(fit_g)
    aic_l <- AIC(fit_l)
    better <- ifelse(aic_g < aic_l, "Gompertz", "Logistic")
    
    compare_df <- rbind(compare_df, data.frame(
      Topic = paste0("T", i),
      RSS_Gompertz = round(rss_g, 1),
      RSS_Logistic = round(rss_l, 1),
      AIC_Gompertz = round(aic_g, 2),
      AIC_Logistic = round(aic_l, 2),
      Better_Model = better,
      stringsAsFactors = FALSE
    ))
  }
}

cat("===== 토픽별 적합도 비교 =====\n")
print(compare_df)

# 종합 판정
cat("\n===== 종합 판정 =====\n")
g_wins <- sum(compare_df$Better_Model == "Gompertz")
l_wins <- sum(compare_df$Better_Model == "Logistic")
cat("Gompertz가 더 좋은 토픽 수:", g_wins, "/ 20\n")
cat("Logistic이 더 좋은 토픽 수:", l_wins, "/ 20\n")

mean_aic_g <- mean(compare_df$AIC_Gompertz)
mean_aic_l <- mean(compare_df$AIC_Logistic)
cat("\nGompertz 평균 AIC:", round(mean_aic_g, 2), "\n")
cat("Logistic 평균 AIC:", round(mean_aic_l, 2), "\n")
cat("AIC 차이 (작은 쪽이 우수):", round(mean_aic_l - mean_aic_g, 2), "\n")

mean_rss_g <- mean(compare_df$RSS_Gompertz)
mean_rss_l <- mean(compare_df$RSS_Logistic)
cat("\nGompertz 평균 RSS:", round(mean_rss_g, 2), "\n")
cat("Logistic 평균 RSS:", round(mean_rss_l, 2), "\n")

# ============================================================
# STEP 1: 20개 토픽 Gompertz 적합 + 진단 정보 통합 저장
# ============================================================

cat("\n=========================================================\n")
cat("STEP 1: Gompertz 적합 + 진단 정보 통합 산출\n")
cat("=========================================================\n\n")

# nls 객체 보존용 리스트 (STEP 3에서 CI 산출에 재사용)
fit_list <- vector("list", 20)
names(fit_list) <- paste0("T", 1:20)

# 결과 데이터프레임 컨테이너
gompertz_results <- data.frame(
  Topic        = paste0("T", 1:20),
  Asym         = numeric(20),
  b2           = numeric(20),
  b3           = numeric(20),
  Current_Cum  = numeric(20),
  TMR_pct      = numeric(20),
  RSS          = numeric(20),
  AIC          = numeric(20),
  convergence  = logical(20),
  iterations   = integer(20),
  tolerance    = numeric(20),
  stringsAsFactors = FALSE
)

# 토픽별 적합 + 진단 추출
for (i in 1:20) {
  yearly <- tapply(theta_mat[, i], pub_year, sum)
  cum    <- cumsum(yearly)
  years_seq <- 1:length(cum)
  
  fit <- tryCatch(
    nls(cum ~ SSgompertz(years_seq, Asym, b2, b3)),
    error = function(e) NULL
  )
  
  if (!is.null(fit)) {
    fit_list[[i]] <- fit
    
    # 파라미터 추출
    coefs <- coef(fit)
    gompertz_results$Asym[i]        <- coefs["Asym"]
    gompertz_results$b2[i]          <- coefs["b2"]
    gompertz_results$b3[i]          <- coefs["b3"]
    
    # TMR
    gompertz_results$Current_Cum[i] <- max(cum)
    gompertz_results$TMR_pct[i]     <- max(cum) / coefs["Asym"] * 100
    
    # 적합도
    gompertz_results$RSS[i]         <- sum(resid(fit)^2)
    gompertz_results$AIC[i]         <- AIC(fit)
    
    # 진단 정보 (convInfo에서 추출)
    conv_info <- fit$convInfo
    gompertz_results$convergence[i] <- conv_info$isConv
    gompertz_results$iterations[i]  <- conv_info$finIter
    gompertz_results$tolerance[i]   <- conv_info$finTol
  } else {
    # 적합 실패 시 모두 NA
    gompertz_results$Asym[i]        <- NA
    gompertz_results$b2[i]          <- NA
    gompertz_results$b3[i]          <- NA
    gompertz_results$Current_Cum[i] <- max(cum)
    gompertz_results$TMR_pct[i]     <- NA
    gompertz_results$RSS[i]         <- NA
    gompertz_results$AIC[i]         <- NA
    gompertz_results$convergence[i] <- FALSE
    gompertz_results$iterations[i]  <- NA
    gompertz_results$tolerance[i]   <- NA
  }
}

# 보기 좋게 반올림 후 출력
display_df <- gompertz_results
display_df$Asym        <- round(display_df$Asym, 2)
display_df$b2          <- round(display_df$b2, 4)
display_df$b3          <- round(display_df$b3, 4)
display_df$Current_Cum <- round(display_df$Current_Cum, 2)
display_df$TMR_pct     <- round(display_df$TMR_pct, 1)
display_df$RSS         <- round(display_df$RSS, 2)
display_df$AIC         <- round(display_df$AIC, 2)
display_df$tolerance   <- signif(display_df$tolerance, 3)

cat("===== STEP 1 결과: Gompertz 적합 + 진단 =====\n")
print(display_df)

# 종합 점검
cat("\n===== 종합 점검 =====\n")
cat("총 토픽:                   ", nrow(gompertz_results), "\n")
cat("적합 성공 (convergence=T): ", sum(gompertz_results$convergence, na.rm = TRUE), "\n")
cat("적합 실패:                 ", sum(!gompertz_results$convergence, na.rm = TRUE), "\n")
cat("\n반복 횟수 분포:\n")
print(summary(gompertz_results$iterations))
cat("\nTolerance 분포 (작을수록 정밀):\n")
print(summary(gompertz_results$tolerance))
cat("\nTMR% 분포:\n")
print(summary(gompertz_results$TMR_pct))

# ============================================================
# STEP 2: Gompertz 적합 곡선 시각 점검
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)

cat("\n=========================================================\n")
cat("STEP 2: 적합 곡선 시각 점검\n")
cat("=========================================================\n\n")

# --- 2-1. 모든 토픽의 관측값/적합값 데이터프레임 구성 ---
plot_data <- data.frame()

for (i in 1:20) {
  yearly <- tapply(theta_mat[, i], pub_year, sum)
  cum_obs <- as.numeric(cumsum(yearly))
  years_actual <- as.numeric(names(yearly))
  years_seq <- 1:length(yearly)
  
  # 적합값 계산 (해당 토픽의 적합 객체에서)
  fit <- fit_list[[i]]
  if (!is.null(fit)) {
    # 부드러운 곡선을 위해 0.1 간격 시퀀스로
    smooth_seq <- seq(1, length(yearly), by = 0.1)
    cum_fit <- predict(fit, newdata = data.frame(years_seq = smooth_seq))
    smooth_years <- min(years_actual) + (smooth_seq - 1)
    
    # 관측값
    plot_data <- rbind(plot_data, data.frame(
      Topic    = paste0("T", i),
      Year     = years_actual,
      Cum      = cum_obs,
      Type     = "Observed"
    ))
    # 적합값
    plot_data <- rbind(plot_data, data.frame(
      Topic    = paste0("T", i),
      Year     = smooth_years,
      Cum      = cum_fit,
      Type     = "Fitted"
    ))
  }
}

# Topic 순서 고정 (T1 ~ T20)
plot_data$Topic <- factor(plot_data$Topic, levels = paste0("T", 1:20))

# --- 2-2. 핵심 4개 토픽 확대 그림 (T9, T17 유망 / T5, T14 포화) ---
key_topics <- c("T9", "T17", "T5", "T14")
key_data <- plot_data %>% filter(Topic %in% key_topics)
key_data$Topic <- factor(key_data$Topic, levels = key_topics)

# 점추정 TMR 라벨 (annotation용)
key_tmr <- gompertz_results %>%
  filter(Topic %in% key_topics) %>%
  mutate(label = paste0(Topic, " (TMR = ", round(TMR_pct, 1), "%)"))

p_key <- ggplot(key_data, aes(x = Year, y = Cum)) +
  geom_point(data = key_data %>% filter(Type == "Observed"),
             aes(color = Type), size = 3, alpha = 0.85) +
  geom_line(data = key_data %>% filter(Type == "Fitted"),
            aes(color = Type), linewidth = 1) +
  facet_wrap(~ Topic, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Observed" = "#E24B4A", "Fitted" = "#2980B9")) +
  scale_x_continuous(breaks = 2019:2025) +
  labs(
    title = "Gompertz S-Curve Fitting: Key Topics",
    subtitle = "Observed (red dots) vs Fitted Gompertz Curve (blue line)",
    x = "Publication Year",
    y = "Cumulative Expected Patents",
    color = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    strip.background = element_rect(fill = "gray92"),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave("STM_Gompertz_KeyTopics_Fit.png", plot = p_key,
       width = 10, height = 7, dpi = 450)
cat("✅ 저장: STM_Gompertz_KeyTopics_Fit.png (핵심 4개 토픽)\n")

# --- 2-3. 20개 토픽 전체 패널 그림 ---
p_all <- ggplot(plot_data, aes(x = Year, y = Cum)) +
  geom_point(data = plot_data %>% filter(Type == "Observed"),
             aes(color = Type), size = 1.5, alpha = 0.85) +
  geom_line(data = plot_data %>% filter(Type == "Fitted"),
            aes(color = Type), linewidth = 0.7) +
  facet_wrap(~ Topic, scales = "free_y", ncol = 5) +
  scale_color_manual(values = c("Observed" = "#E24B4A", "Fitted" = "#2980B9")) +
  scale_x_continuous(breaks = c(2019, 2022, 2025)) +
  labs(
    title = "Gompertz S-Curve Fitting: All 20 Topics",
    subtitle = "Observed (red) vs Fitted (blue) — 7 time points per topic",
    x = "Publication Year",
    y = "Cumulative Expected Patents",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    strip.background = element_rect(fill = "gray92"),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "top",
    axis.text = element_text(size = 8)
  )

ggsave("STM_Gompertz_All20Topics_Fit.png", plot = p_all,
       width = 15, height = 10, dpi = 450)
cat("✅ 저장: STM_Gompertz_All20Topics_Fit.png (20개 토픽 패널)\n")

# --- 2-4. 적합 품질 요약 ---
cat("\n===== 적합 품질 요약 (RSS / Current_Cum 비율 = 상대 잔차) =====\n")
quality_df <- gompertz_results %>%
  mutate(Relative_Residual = sqrt(RSS) / Current_Cum * 100) %>%
  select(Topic, Current_Cum, RSS, AIC, Relative_Residual) %>%
  mutate(across(c(Current_Cum, RSS, AIC, Relative_Residual), ~ round(., 2))) %>%
  arrange(desc(Relative_Residual))
print(quality_df)

cat("\n상대 잔차 분포 (낮을수록 적합 우수):\n")
print(summary(quality_df$Relative_Residual))

# ============================================================
# STEP 3: 95% 신뢰구간 산출 (3단계: profile → Wald → bootstrap)
# ============================================================

library(boot)

cat("\n=========================================================\n")
cat("STEP 3: 95% 신뢰구간 산출\n")
cat("=========================================================\n\n")

set.seed(2026)  # 재현성

# CI 결과 컨테이너
ci_results <- data.frame(
  Topic         = paste0("T", 1:20),
  Asym_lower    = numeric(20),
  Asym_upper    = numeric(20),
  b2_lower      = numeric(20),
  b2_upper      = numeric(20),
  b3_lower      = numeric(20),
  b3_upper      = numeric(20),
  CI_method     = character(20),
  stringsAsFactors = FALSE
)

# 부트스트랩용 함수 (케이스 부트스트랩)
bootstrap_gompertz <- function(yearly_data, B = 1000) {
  cum <- cumsum(yearly_data)
  years_seq <- 1:length(cum)
  n <- length(cum)
  
  # 케이스 부트스트랩: 시점-누적값 페어를 재샘플링
  boot_data <- data.frame(t = years_seq, y = cum)
  
  boot_stat <- function(data, indices) {
    d <- data[indices, ]
    d <- d[order(d$t), ]  # 시간 순서 정렬 필요
    fit_b <- tryCatch(
      nls(y ~ SSgompertz(t, Asym, b2, b3), data = d),
      error = function(e) NULL
    )
    if (is.null(fit_b)) return(c(NA, NA, NA))
    coef(fit_b)
  }
  
  results <- boot(boot_data, boot_stat, R = B)
  # 2.5%, 97.5% 분위수로 CI
  apply(results$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
}

# 토픽별 CI 산출
for (i in 1:20) {
  fit <- fit_list[[i]]
  if (is.null(fit)) {
    ci_results[i, 2:7] <- NA
    ci_results$CI_method[i] <- "fit_failed"
    next
  }
  
  cat(sprintf("[T%d] ", i))
  
  # === 1차: profile likelihood ===
  ci_prof <- tryCatch(
    suppressWarnings(suppressMessages(confint(fit, level = 0.95))),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (!is.null(ci_prof) && !any(is.na(ci_prof))) {
    ci_results$Asym_lower[i] <- ci_prof["Asym", 1]
    ci_results$Asym_upper[i] <- ci_prof["Asym", 2]
    ci_results$b2_lower[i]   <- ci_prof["b2", 1]
    ci_results$b2_upper[i]   <- ci_prof["b2", 2]
    ci_results$b3_lower[i]   <- ci_prof["b3", 1]
    ci_results$b3_upper[i]   <- ci_prof["b3", 2]
    ci_results$CI_method[i]  <- "profile"
    cat("profile ✓\n")
    next
  }
  
  # === 2차: Wald 근사 ===
  ci_wald <- tryCatch(
    suppressWarnings(confint.default(fit, level = 0.95)),
    error = function(e) NULL
  )
  
  if (!is.null(ci_wald) && !any(is.na(ci_wald))) {
    ci_results$Asym_lower[i] <- ci_wald["Asym", 1]
    ci_results$Asym_upper[i] <- ci_wald["Asym", 2]
    ci_results$b2_lower[i]   <- ci_wald["b2", 1]
    ci_results$b2_upper[i]   <- ci_wald["b2", 2]
    ci_results$b3_lower[i]   <- ci_wald["b3", 1]
    ci_results$b3_upper[i]   <- ci_wald["b3", 2]
    ci_results$CI_method[i]  <- "Wald"
    cat("Wald ✓\n")
    next
  }
  
  # === 3차: 케이스 부트스트랩 ===
  cat("bootstrap ... ")
  yearly_i <- tapply(theta_mat[, i], pub_year, sum)
  ci_boot <- tryCatch(
    bootstrap_gompertz(yearly_i, B = 1000),
    error = function(e) NULL
  )
  
  if (!is.null(ci_boot) && !any(is.na(ci_boot))) {
    ci_results$Asym_lower[i] <- ci_boot[1, 1]
    ci_results$Asym_upper[i] <- ci_boot[2, 1]
    ci_results$b2_lower[i]   <- ci_boot[1, 2]
    ci_results$b2_upper[i]   <- ci_boot[2, 2]
    ci_results$b3_lower[i]   <- ci_boot[1, 3]
    ci_results$b3_upper[i]   <- ci_boot[2, 3]
    ci_results$CI_method[i]  <- "bootstrap"
    cat("✓\n")
  } else {
    ci_results[i, 2:7] <- NA
    ci_results$CI_method[i] <- "all_failed"
    cat("✗ (모든 방법 실패)\n")
  }
}

# --- 결과 출력 ---
cat("\n===== CI 산출 방법 분포 =====\n")
print(table(ci_results$CI_method))

cat("\n===== STEP 3 결과: 토픽별 95% CI =====\n")
display_ci <- ci_results
display_ci$Asym_lower <- round(display_ci$Asym_lower, 1)
display_ci$Asym_upper <- round(display_ci$Asym_upper, 1)
display_ci$b2_lower   <- round(display_ci$b2_lower, 3)
display_ci$b2_upper   <- round(display_ci$b2_upper, 3)
display_ci$b3_lower   <- round(display_ci$b3_lower, 3)
display_ci$b3_upper   <- round(display_ci$b3_upper, 3)
print(display_ci)

# CI 폭 분석 (Asym 기준)
cat("\n===== Asym CI 폭 분석 =====\n")
asym_width <- (ci_results$Asym_upper - ci_results$Asym_lower)
asym_relative <- asym_width / gompertz_results$Asym * 100
width_df <- data.frame(
  Topic       = ci_results$Topic,
  Asym        = round(gompertz_results$Asym, 1),
  Asym_width  = round(asym_width, 1),
  Width_pct   = round(asym_relative, 1),  # Asym 대비 CI 폭 비율
  CI_method   = ci_results$CI_method
) %>% arrange(desc(Width_pct))
print(width_df)

cat("\nAsym CI 폭 (Asym 대비 %) 분포:\n")
print(summary(width_df$Width_pct))

# ============================================================
# STEP 4: 평균 TMR 4종 + 사분위수 임계값 + TMR CI 산출
# ============================================================

cat("\n=========================================================\n")
cat("STEP 4: 평균 TMR 및 사분위수 임계값\n")
cat("=========================================================\n\n")

# --- 4-1. TMR의 95% CI 계산 (Asym CI로부터 환산) ---
# TMR = current / Asym 이므로
# TMR_CI_lower = current / Asym_upper
# TMR_CI_upper = current / Asym_lower
tmr_with_ci <- data.frame(
  Topic        = gompertz_results$Topic,
  Current_Cum  = gompertz_results$Current_Cum,
  Asym         = gompertz_results$Asym,
  Asym_lower   = ci_results$Asym_lower,
  Asym_upper   = ci_results$Asym_upper,
  TMR_pct      = gompertz_results$TMR_pct,
  TMR_lower    = gompertz_results$Current_Cum / ci_results$Asym_upper * 100,
  TMR_upper    = gompertz_results$Current_Cum / ci_results$Asym_lower * 100,
  CI_method    = ci_results$CI_method,
  stringsAsFactors = FALSE
)
tmr_with_ci$TMR_CI_width <- tmr_with_ci$TMR_upper - tmr_with_ci$TMR_lower

cat("===== 토픽별 TMR% + 95% CI =====\n")
# 숫자 컬럼만 명시적으로 골라서 반올림 (CI_method 제외)
print(tmr_with_ci %>% 
        mutate(across(c(TMR_pct, TMR_lower, TMR_upper, TMR_CI_width), 
                      ~ round(., 1))) %>%
        select(Topic, TMR_pct, TMR_lower, TMR_upper, TMR_CI_width, CI_method))

# --- 4-2. 사분위수 임계값 ---
cat("\n===== 사분위수 임계값 (유망/포화 분류 기준) =====\n")
q1_threshold <- quantile(tmr_with_ci$TMR_pct, 0.25, na.rm = TRUE)
q3_threshold <- quantile(tmr_with_ci$TMR_pct, 0.75, na.rm = TRUE)
cat(sprintf("Q1 (유망 임계값, 하위 25%%): %.2f%%\n", q1_threshold))
cat(sprintf("Q3 (포화 임계값, 상위 25%%): %.2f%%\n", q3_threshold))

# 유망/포화 분류 확인
emerging <- tmr_with_ci %>% filter(TMR_pct <= q1_threshold) %>% arrange(TMR_pct)
saturated <- tmr_with_ci %>% filter(TMR_pct >= q3_threshold) %>% arrange(desc(TMR_pct))

cat("\n----- 유망 토픽 (TMR ≤ Q1) -----\n")
print(emerging %>% 
        mutate(across(c(TMR_pct, TMR_lower, TMR_upper), ~ round(., 1))) %>%
        select(Topic, TMR_pct, TMR_lower, TMR_upper))

cat("\n----- 포화 토픽 (TMR ≥ Q3) -----\n")
print(saturated %>%
        mutate(across(c(TMR_pct, TMR_lower, TMR_upper), ~ round(., 1))) %>%
        select(Topic, TMR_pct, TMR_lower, TMR_upper))

# --- 4-3. Inner6 사분면 기준 평균 TMR ---
cat("\n===== Inner6 사분면 평균 TMR =====\n")

growth_topics <- c("T17", "T9", "T11", "T8", "T7", "T13", "T3", "T15", "T4", "T19")
decline_topics <- c("T1", "T6", "T20", "T16", "T12", "T14", "T5", "T2", "T10", "T18")

mean_tmr_growth <- mean(tmr_with_ci$TMR_pct[tmr_with_ci$Topic %in% growth_topics], 
                        na.rm = TRUE)
mean_tmr_decline <- mean(tmr_with_ci$TMR_pct[tmr_with_ci$Topic %in% decline_topics], 
                         na.rm = TRUE)

cat(sprintf("성장 토픽군 (Q1+Q2, 10개) 평균 TMR: %.2f%%\n", mean_tmr_growth))
cat(sprintf("  포함: %s\n", paste(growth_topics, collapse = ", ")))
cat(sprintf("하락/포화 토픽군 (Q3+Q4, 10개) 평균 TMR: %.2f%%\n", mean_tmr_decline))
cat(sprintf("  포함: %s\n", paste(decline_topics, collapse = ", ")))
cat(sprintf("\n차이: %.2f%%p (하락/포화가 더 성숙)\n", 
            mean_tmr_decline - mean_tmr_growth))

# --- 4-4. C ↔ H 클러스터 평균 TMR ---
cat("\n===== 클러스터별 평균 TMR (본문 후반부 비교 서사) =====\n")

cluster_C <- c("T9", "T13", "T17")
cluster_H <- c("T2", "T16", "T20")

mean_tmr_C <- mean(tmr_with_ci$TMR_pct[tmr_with_ci$Topic %in% cluster_C], 
                   na.rm = TRUE)
mean_tmr_H <- mean(tmr_with_ci$TMR_pct[tmr_with_ci$Topic %in% cluster_H], 
                   na.rm = TRUE)

cat(sprintf("C클러스터 (AI 기반 신용/투자 분석, T9·T13·T17) 평균 TMR: %.2f%%\n", 
            mean_tmr_C))
cat(sprintf("H클러스터 (O2O 상거래 및 간편 결제, T2·T16·T20) 평균 TMR: %.2f%%\n", 
            mean_tmr_H))
cat(sprintf("\n차이: %.2f%%p (H가 더 성숙)\n", mean_tmr_H - mean_tmr_C))

# --- 4-5. 본문 갱신표 요약 ---
cat("\n===== 본문 갱신표 요약 =====\n")
cat("\n위치                              기존        갱신\n")
cat("------------------------------------------------------\n")
cat(sprintf("Q1 임계값 (유망 기준)             74.9%%   →   %.1f%%\n", q1_threshold))
cat(sprintf("Q3 임계값 (포화 기준)             81.1%%   →   %.1f%%\n", q3_threshold))
cat(sprintf("Q1+Q2 (성장 10개) 평균 TMR        75.9%%   →   %.1f%%\n", mean_tmr_growth))
cat(sprintf("Q3+Q4 (하락/포화 10개) 평균 TMR   81.7%%   →   %.1f%%\n", mean_tmr_decline))
cat(sprintf("C클러스터 평균 TMR                74.3%%   →   %.1f%%\n", mean_tmr_C))
cat(sprintf("H클러스터 평균 TMR                82.5%%   →   %.1f%%\n", mean_tmr_H))

colnames(theta_mat) <- paste0("T", 1:20)

save.image()

# 컬럼명 일괄 확인
  cat("gompertz_results 컬럼:\n"); print(colnames(gompertz_results))
  cat("\nci_results 컬럼:\n");       print(colnames(ci_results))
  cat("\ntmr_with_ci 컬럼:\n");      print(colnames(tmr_with_ci))
  
  # =============================================================================
  # STEP 5: 부록 CSV 생성 — STM_Gompertz_Diagnostics.csv
  # 전제: STEP 1~4 완료 (gompertz_results, ci_results, tmr_with_ci, fit_list)
  # 출력: 20행 × 21열, UTF-8
  # =============================================================================
  
  cat("===== STEP 5: 부록 CSV 생성 =====\n")
  
  # -----------------------------------------------------------------------------
  # 1. 토픽별 상대 잔차 평균(%) 재계산
  #    식: mean(|observed - fitted| / observed) * 100
  # -----------------------------------------------------------------------------
  cat("[1/3] 상대 잔차 평균 재계산 중...\n")
  
  years_filter <- 2019:2025
  n_topics <- 20
  
  rel_resid_pct <- numeric(n_topics)
  for (k in 1:n_topics) {
    yearly_sum <- tapply(theta_mat[, k], pub_year, sum)
    yearly_sum <- yearly_sum[as.character(years_filter)]
    cum_observed <- cumsum(yearly_sum)
    
    fit_k <- fit_list[[k]]
    cum_fitted <- fitted(fit_k)
    
    rel_resid <- abs(cum_observed - cum_fitted) / cum_observed * 100
    rel_resid_pct[k] <- mean(rel_resid)
  }
  
  cat(sprintf("    상대 잔차: min=%.2f%%, mean=%.2f%%, max=%.2f%%\n",
              min(rel_resid_pct), mean(rel_resid_pct), max(rel_resid_pct)))
  cat("    예상: min≈0.2%, mean≈2.18%, max≈5.07% (T8)\n")
  
  # -----------------------------------------------------------------------------
  # 2. 21개 컬럼 통합 데이터프레임 구성
  # -----------------------------------------------------------------------------
  cat("\n[2/3] 21개 컬럼 통합 중...\n")
  
  df_diag <- data.frame(
    Topic       = gompertz_results$Topic,
    # --- α (Asymptote) ---
    Asym        = round(gompertz_results$Asym, 2),
    Asym_lower  = round(ci_results$Asym_lower, 2),
    Asym_upper  = round(ci_results$Asym_upper, 2),
    # --- β (b2, 위치 모수) ---
    b2          = round(gompertz_results$b2, 4),
    b2_lower    = round(ci_results$b2_lower, 4),
    b2_upper    = round(ci_results$b2_upper, 4),
    # --- γ (b3, 성장 속도 모수) ---
    b3          = round(gompertz_results$b3, 4),
    b3_lower    = round(ci_results$b3_lower, 4),
    b3_upper    = round(ci_results$b3_upper, 4),
    # --- TMR 계열 ---
    Current_Cum = round(gompertz_results$Current_Cum, 2),
    TMR_pct     = round(tmr_with_ci$TMR_pct, 2),
    TMR_lower   = round(tmr_with_ci$TMR_lower, 2),
    TMR_upper   = round(tmr_with_ci$TMR_upper, 2),
    # --- 적합 품질 ---
    RSS                   = round(gompertz_results$RSS, 2),
    AIC                   = round(gompertz_results$AIC, 2),
    Relative_Residual_pct = round(rel_resid_pct, 2),
    # --- 수렴 진단 ---
    convergence = gompertz_results$convergence,
    iterations  = gompertz_results$iterations,
    tolerance   = formatC(gompertz_results$tolerance, format = "e", digits = 2),
    CI_method   = ci_results$CI_method,
    stringsAsFactors = FALSE
  )
  
  # Topic 순서 보장 (T1, T2, ..., T20)
  topic_order <- paste0("T", 1:20)
  df_diag <- df_diag[match(topic_order, df_diag$Topic), ]
  rownames(df_diag) <- NULL  # 행 이름 초기화
  
  cat(sprintf("    완료: %d행 × %d열\n", nrow(df_diag), ncol(df_diag)))
  
  # -----------------------------------------------------------------------------
  # 3. UTF-8 저장
  # -----------------------------------------------------------------------------
  cat("\n[3/3] CSV 저장 중...\n")
  output_path <- "STM_Gompertz_Diagnostics.csv"
  write.csv(df_diag, output_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("    저장 완료:", output_path, "\n")
  
  # -----------------------------------------------------------------------------
  # 검증 출력
  # -----------------------------------------------------------------------------
  cat("\n===== 검증 — 핵심 4개 토픽 =====\n")
  check_topics <- c("T9", "T17", "T5", "T14")
  print(df_diag[df_diag$Topic %in% check_topics,
                c("Topic", "Asym", "Asym_lower", "Asym_upper",
                  "TMR_pct", "TMR_lower", "TMR_upper",
                  "AIC", "Relative_Residual_pct", "CI_method")])
  
  cat("\n===== 전체 20행 미리보기 =====\n")
  print(df_diag[, c("Topic","Asym","TMR_pct","TMR_lower","TMR_upper",
                    "AIC","Relative_Residual_pct","CI_method")])
  
  # -----------------------------------------------------------------------------
  # 워크스페이스 저장 (휘발 방지)
  # -----------------------------------------------------------------------------
  cat("\n===== 워크스페이스 저장 =====\n")
  save.image("STM_full_workspace_Phase0.RData")
  cat("저장 완료. STEP 5 산출물(df_diag) 영구 보존.\n\n")
  
  cat("===== STEP 5 완료. 다음은 STEP 6. =====\n")
  
  # =============================================================================
  # STEP 5 보강: Relative_Residual_pct를 RSS_RMSE_pct + MAPE_pct로 분리
  # 전제: df_diag 객체가 이미 존재 (방금 STEP 5 실행 결과)
  # 변경: Relative_Residual_pct 컬럼 제거 → RSS_RMSE_pct, MAPE_pct 두 컬럼 추가
  # 결과: 20행 × 22열
  # =============================================================================
  
  cat("===== STEP 5 보강: 두 적합 품질 지표 분리 =====\n")
  
  # -----------------------------------------------------------------------------
  # 1. RSS_RMSE_pct 재계산
  #    공식: sqrt(RSS) / Current_Cum * 100
  #    의미: 전체 잔차의 RMS가 최종 누적값(곡선 최대 스케일)의 몇 %인가
  # -----------------------------------------------------------------------------
  rss_rmse_pct <- sqrt(gompertz_results$RSS) / gompertz_results$Current_Cum * 100
  
  cat(sprintf("RSS_RMSE_pct: min=%.2f%%, mean=%.2f%%, max=%.2f%%\n",
              min(rss_rmse_pct), mean(rss_rmse_pct), max(rss_rmse_pct)))
  cat("  → 예상: 평균 ≈ 2.18%, 최대 ≈ 5.07% (T8)  [인계 §3-3]\n")
  
  # -----------------------------------------------------------------------------
  # 2. MAPE_pct는 기존 rel_resid_pct를 그대로 사용
  # -----------------------------------------------------------------------------
  mape_pct <- rel_resid_pct  # 이미 STEP 5에서 산출된 객체
  
  cat(sprintf("MAPE_pct    : min=%.2f%%, mean=%.2f%%, max=%.2f%%\n",
              min(mape_pct), mean(mape_pct), max(mape_pct)))
  cat("  → 두 지표 비교를 위한 표준 예측 평가 지표\n")
  
  # -----------------------------------------------------------------------------
  # 3. 최대값 토픽 확인 (두 지표가 다른 토픽을 지목하는지)
  # -----------------------------------------------------------------------------
  cat("\n[지표별 최대값 토픽]\n")
  cat(sprintf("  RSS_RMSE_pct 최대: %s (%.2f%%)\n",
              gompertz_results$Topic[which.max(rss_rmse_pct)],
              max(rss_rmse_pct)))
  cat(sprintf("  MAPE_pct     최대: %s (%.2f%%)\n",
              gompertz_results$Topic[which.max(mape_pct)],
              max(mape_pct)))
  
  # -----------------------------------------------------------------------------
  # 4. df_diag 재구성 — Relative_Residual_pct 제거, 두 지표 삽입
  # -----------------------------------------------------------------------------
  df_diag <- data.frame(
    Topic       = gompertz_results$Topic,
    # --- α (Asymptote) ---
    Asym        = round(gompertz_results$Asym, 2),
    Asym_lower  = round(ci_results$Asym_lower, 2),
    Asym_upper  = round(ci_results$Asym_upper, 2),
    # --- β (b2, 위치 모수) ---
    b2          = round(gompertz_results$b2, 4),
    b2_lower    = round(ci_results$b2_lower, 4),
    b2_upper    = round(ci_results$b2_upper, 4),
    # --- γ (b3, 성장 속도 모수) ---
    b3          = round(gompertz_results$b3, 4),
    b3_lower    = round(ci_results$b3_lower, 4),
    b3_upper    = round(ci_results$b3_upper, 4),
    # --- TMR 계열 ---
    Current_Cum = round(gompertz_results$Current_Cum, 2),
    TMR_pct     = round(tmr_with_ci$TMR_pct, 2),
    TMR_lower   = round(tmr_with_ci$TMR_lower, 2),
    TMR_upper   = round(tmr_with_ci$TMR_upper, 2),
    # --- 적합 품질 ---
    RSS           = round(gompertz_results$RSS, 2),
    AIC           = round(gompertz_results$AIC, 2),
    RSS_RMSE_pct  = round(rss_rmse_pct, 2),   # ← 신규: 인계 §3-3 호환
    MAPE_pct      = round(mape_pct, 2),       # ← 신규: 표준 예측 평가 지표
    # --- 수렴 진단 ---
    convergence = gompertz_results$convergence,
    iterations  = gompertz_results$iterations,
    tolerance   = formatC(gompertz_results$tolerance, format = "e", digits = 2),
    CI_method   = ci_results$CI_method,
    stringsAsFactors = FALSE
  )
  
  # Topic 순서 보장
  topic_order <- paste0("T", 1:20)
  df_diag <- df_diag[match(topic_order, df_diag$Topic), ]
  rownames(df_diag) <- NULL
  
  cat(sprintf("\ndf_diag 재구성 완료: %d행 × %d열\n", nrow(df_diag), ncol(df_diag)))
  
  # -----------------------------------------------------------------------------
  # 5. CSV 재저장 (UTF-8, 기존 파일 덮어쓰기)
  # -----------------------------------------------------------------------------
  write.csv(df_diag, "STM_Gompertz_Diagnostics.csv",
            row.names = FALSE, fileEncoding = "UTF-8")
  cat("저장 완료: STM_Gompertz_Diagnostics.csv (22열)\n")
  
  # -----------------------------------------------------------------------------
  # 6. 검증 출력 — 핵심 4개 토픽
  # -----------------------------------------------------------------------------
  cat("\n===== 검증 — 핵심 4개 토픽 (두 지표 포함) =====\n")
  check_topics <- c("T9", "T17", "T5", "T14")
  print(df_diag[df_diag$Topic %in% check_topics,
                c("Topic", "TMR_pct", "TMR_lower", "TMR_upper",
                  "AIC", "RSS_RMSE_pct", "MAPE_pct", "CI_method")])
  
  cat("\n===== 전체 20행 적합 품질 비교 =====\n")
  print(df_diag[, c("Topic", "TMR_pct", "AIC", "RSS_RMSE_pct", "MAPE_pct")])
  
  # -----------------------------------------------------------------------------
  # 7. 워크스페이스 영구 저장
  # -----------------------------------------------------------------------------
  save.image("STM_full_workspace_Phase0.RData")
  cat("\n워크스페이스 저장 완료.\n")
  
  cat("\n===== STEP 5 보강 완료. 다음은 STEP 6. =====\n")
  
  # B-2가 만들어둔 본문 표 4.2.4 파일 점검
  df_tlc_table <- read.csv("STM_TLC_TMR_Combined_Table.csv",
                           fileEncoding = "UTF-8",
                           stringsAsFactors = FALSE)
  
  cat("===== 파일 점검 =====\n")
  cat("차원:", nrow(df_tlc_table), "행 ×", ncol(df_tlc_table), "열\n")
  cat("\n컬럼 목록:\n"); print(colnames(df_tlc_table))
  cat("\nTMR_pct 컬럼 상태:\n")
  if ("TMR_pct" %in% colnames(df_tlc_table)) {
    cat("  존재 ✓\n")
    cat("  NA 개수:", sum(is.na(df_tlc_table$TMR_pct)), "/", nrow(df_tlc_table), "\n")
    cat("  유효값 개수:", sum(!is.na(df_tlc_table$TMR_pct)), "\n")
    if (sum(!is.na(df_tlc_table$TMR_pct)) > 0) {
      cat("  유효값 미리보기:\n")
      print(head(df_tlc_table[!is.na(df_tlc_table$TMR_pct),
                              c("Topic", "TMR_pct")]))
    }
  } else {
    cat("  ⚠️ 컬럼 없음 — STEP 6에서 새로 추가 필요\n")
  }
  
  cat("\nTopic 컬럼 값 (정렬 확인):\n")
  print(df_tlc_table$Topic)
  
  cat("\n전체 미리보기 (첫 5행):\n")
  print(head(df_tlc_table, 5))
  
  # =============================================================================
  # STEP 6: STM_TLC_TMR_Combined_Table.csv 의 TMR_pct 컬럼 갱신
  # 전제: STEP 5 완료, tmr_with_ci 객체 존재
  # 입력: STM_TLC_TMR_Combined_Table.csv (20행 × 9열, TMR_pct 전부 NA)
  # 출력: 동일 파일에 TMR_pct 갱신 (Topic 정렬 순서 유지)
  # =============================================================================
  
  library(dplyr)
  
  cat("===== STEP 6: TMR_pct 컬럼 갱신 =====\n")
  
  # -----------------------------------------------------------------------------
  # 1. 기존 파일 로드 + 백업
  # -----------------------------------------------------------------------------
  df_tlc_table <- read.csv("STM_TLC_TMR_Combined_Table.csv",
                           fileEncoding = "UTF-8",
                           stringsAsFactors = FALSE)
  
  # 안전 백업 (타임스탬프)
  backup_path <- paste0("STM_TLC_TMR_Combined_Table_backup_",
                        format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  file.copy("STM_TLC_TMR_Combined_Table.csv", backup_path)
  cat("백업 생성:", backup_path, "\n")
  
  cat("로드 완료:", nrow(df_tlc_table), "행 ×", ncol(df_tlc_table), "열\n")
  cat("갱신 전 TMR_pct NA 개수:", sum(is.na(df_tlc_table$TMR_pct)), "/ 20\n\n")
  
  # -----------------------------------------------------------------------------
  # 2. tmr_with_ci에서 Topic + TMR_pct 추출
  # -----------------------------------------------------------------------------
  df_tmr_new <- tmr_with_ci %>%
    select(Topic, TMR_pct) %>%
    rename(TMR_pct_new = TMR_pct)  # 임시 이름 (충돌 방지)
  
  cat("STEP 5 산출 TMR_pct (T1~T20 순):\n")
  print(df_tmr_new[match(paste0("T", 1:20), df_tmr_new$Topic), ])
  
  # -----------------------------------------------------------------------------
  # 3. Topic 키로 join — 기존 정렬(Q1→Q4 + 모멘텀 순) 그대로 유지
  # -----------------------------------------------------------------------------
  df_final <- df_tlc_table %>%
    select(-TMR_pct) %>%                           # 기존 NA 컬럼 제거
    left_join(df_tmr_new, by = "Topic") %>%        # 새 TMR_pct join
    rename(TMR_pct = TMR_pct_new) %>%              # 원래 컬럼명으로 복원
    # 9개 컬럼 원래 순서 복원
    select(Topic, Topic_Label, Mega_Cluster, Cluster_Name,
           Proportion_pct, Growth_Momentum, Impact, TMR_pct, Quadrant)
  
  # 정렬 순서 유지 확인 — df_tlc_table 원본 Topic 순서와 동일해야 함
  df_final <- df_final[match(df_tlc_table$Topic, df_final$Topic), ]
  rownames(df_final) <- NULL
  
  # 반올림 (소수점 1자리)
  df_final$TMR_pct <- round(df_final$TMR_pct, 1)
  
  cat("\nJoin 완료. TMR_pct NA 개수:", sum(is.na(df_final$TMR_pct)),
      "(0이어야 함)\n")
  
  # -----------------------------------------------------------------------------
  # 4. 정합성 자동 검증 — 핵심 수치 6개 (B-3 인계 §4 갱신표 기준)
  # -----------------------------------------------------------------------------
  cat("\n===== 정합성 자동 검증 =====\n")
  
  # 4-1. Q1 임계값 (유망 기준) — 예상 48.6%
  q1_thr <- quantile(df_final$TMR_pct, 0.25)
  cat(sprintf("Q1 임계값 (유망 기준): %.1f%%   [예상 48.6%%]\n", q1_thr))
  
  # 4-2. Q3 임계값 (포화 기준) — 예상 65.0%
  q3_thr <- quantile(df_final$TMR_pct, 0.75)
  cat(sprintf("Q3 임계값 (포화 기준): %.1f%%   [예상 65.0%%]\n", q3_thr))
  
  # 4-3. 성장 10개 평균 TMR — 예상 48.9%
  growth_topics <- c("T17","T9","T11","T8","T7","T13","T3","T15","T4","T19")
  avg_growth <- mean(df_final$TMR_pct[df_final$Topic %in% growth_topics])
  cat(sprintf("Q1+Q2 성장 10개 평균 TMR: %.1f%%   [예상 48.9%%]\n", avg_growth))
  
  # 4-4. 하락/포화 10개 평균 TMR — 예상 64.9%
  declining_topics <- c("T1","T6","T20","T16","T12","T14","T5","T2","T10","T18")
  avg_declining <- mean(df_final$TMR_pct[df_final$Topic %in% declining_topics])
  cat(sprintf("Q3+Q4 하락/포화 10개 평균 TMR: %.1f%%   [예상 64.9%%]\n",
              avg_declining))
  
  # 4-5. C클러스터 평균 TMR — 예상 39.0%
  cluster_C <- c("T9", "T13", "T17")
  avg_C <- mean(df_final$TMR_pct[df_final$Topic %in% cluster_C])
  cat(sprintf("C클러스터 (T9,T13,T17) 평균 TMR: %.1f%%   [예상 39.0%%]\n",
              avg_C))
  
  # 4-6. H클러스터 평균 TMR — 예상 67.3%
  cluster_H <- c("T2", "T16", "T20")
  avg_H <- mean(df_final$TMR_pct[df_final$Topic %in% cluster_H])
  cat(sprintf("H클러스터 (T2,T16,T20) 평균 TMR: %.1f%%   [예상 67.3%%]\n",
              avg_H))
  
  # -----------------------------------------------------------------------------
  # 5. CSV 재저장 (UTF-8)
  # -----------------------------------------------------------------------------
  cat("\n===== 저장 =====\n")
  write.csv(df_final, "STM_TLC_TMR_Combined_Table.csv",
            row.names = FALSE, fileEncoding = "UTF-8")
  cat("저장 완료: STM_TLC_TMR_Combined_Table.csv\n")
  
  # -----------------------------------------------------------------------------
  # 6. 최종 결과 출력 — 본문 표 4.2.4 미리보기
  # -----------------------------------------------------------------------------
  cat("\n===== 최종 본문 표 4.2.4 미리보기 (전체 20행) =====\n")
  print(df_final)
  
  # -----------------------------------------------------------------------------
  # 7. 워크스페이스 저장
  # -----------------------------------------------------------------------------
  save.image("STM_full_workspace_Phase0.RData")
  cat("\n워크스페이스 저장 완료.\n")
  
  cat("\n===== STEP 6 완료. 다음은 STEP 7 (TMR 바 차트 재생성). =====\n")
  
  # 기존 TMR 관련 PNG/CSV 파일 탐색
  cat("===== 기존 TMR 관련 파일 탐색 =====\n\n")
  
  # TMR 관련 PNG
  cat("[TMR PNG 파일]\n")
  tmr_pngs <- list.files(pattern = "TMR.*\\.png$", ignore.case = TRUE)
  if (length(tmr_pngs) > 0) {
    for (f in tmr_pngs) {
      info <- file.info(f)
      cat(sprintf("  %s (%.1f KB, %s)\n",
                  f, info$size / 1024,
                  format(info$mtime, "%Y-%m-%d %H:%M")))
    }
  } else {
    cat("  없음\n")
  }
  
  # TMR 관련 CSV (기존 데이터 소스 확인)
  cat("\n[TMR 관련 CSV]\n")
  tmr_csvs <- list.files(pattern = "TMR.*\\.csv$", ignore.case = TRUE)
  for (f in tmr_csvs) {
    info <- file.info(f)
    cat(sprintf("  %s (%.1f KB, %s)\n",
                f, info$size / 1024,
                format(info$mtime, "%Y-%m-%d %H:%M")))
  }
  
  # Gompertz 관련 PNG도 함께 확인 (STEP 2에서 생성됐던 것)
  cat("\n[Gompertz 관련 PNG]\n")
  gomp_pngs <- list.files(pattern = "Gompertz.*\\.png$", ignore.case = TRUE)
  for (f in gomp_pngs) {
    info <- file.info(f)
    cat(sprintf("  %s (%.1f KB, %s)\n",
                f, info$size / 1024,
                format(info$mtime, "%Y-%m-%d %H:%M")))
  }
  
  # =============================================================================
  # STEP 7: 본문 TMR 바 차트 재생성 (이미지 1 스타일, 새 Gompertz 수치)
  # 입력: STM_TLC_TMR_Combined_Table.csv (STEP 6 갱신 완료)
  # 출력: STM_TMR_Gompertz_Threshold_BarChart.png (dpi=450)
  #       STM_TMR_Gompertz_Topic_TMR_Results.csv (신규 데이터 파일)
  # 원칙: 옛 파일(STM_Topic_TMR_Results.csv) 그대로 보존, 새 파일 별도 생성
  # =============================================================================
  
  library(dplyr)
  library(ggplot2)
  
  cat("===== STEP 7: TMR 바 차트 재생성 =====\n")
  
  # -----------------------------------------------------------------------------
  # 1. 데이터 로드 + 토픽 라벨 결합
  # -----------------------------------------------------------------------------
  df_chart <- read.csv("STM_TLC_TMR_Combined_Table.csv",
                       fileEncoding = "UTF-8",
                       stringsAsFactors = FALSE)
  
  cat("데이터 로드:", nrow(df_chart), "행 ×", ncol(df_chart), "열\n")
  cat("TMR_pct 범위:", round(min(df_chart$TMR_pct), 1), "~",
      round(max(df_chart$TMR_pct), 1), "%\n")
  
  # -----------------------------------------------------------------------------
  # 2. 8대 클러스터 표준 색상 (Fintech_Project.R과 일치)
  # -----------------------------------------------------------------------------
  cluster_colors <- c(
    'A. 기업 재무 및 회계 관리'        = '#AEC6CF',
    'B. 맞춤형 보험 및 타겟 식별'      = '#FFD1BA',
    'C. AI 기반 신용/투자 분석'        = '#D8BFD8',
    'D. 블록체인 기반 에너지 금융'     = '#C1E1C1',
    'E. 블록체인 및 디지털 자산'       = '#FDFD96',
    'F. 보안 인증 및 규제 컴플라이언스' = '#B0C4DE',
    'G. 뱅킹 및 대금 정산'             = '#AFEEEE',
    'H. O2O 상거래 및 간편 결제'       = '#FFB6C1'
  )
  
  # Cluster_Name 컬럼 확인 (STEP 6 결과)
  cat("\nCluster_Name 고유값:\n"); print(unique(df_chart$Cluster_Name))
  
  # -----------------------------------------------------------------------------
  # 3. 정렬 — TMR 오름차순 (유망 → 포화 자연스러운 흐름, 이미지 1과 동일)
  # -----------------------------------------------------------------------------
  df_chart <- df_chart %>%
    arrange(TMR_pct) %>%
    mutate(
      Topic = factor(Topic, levels = Topic),  # X축 정렬 고정
      Cluster_Name = factor(Cluster_Name, levels = names(cluster_colors)),
      TMR_label = sprintf("%.1f%%", TMR_pct)
    )
  
  cat("\n정렬 후 Topic 순서 (TMR 오름차순):\n"); print(levels(df_chart$Topic))
  
  # -----------------------------------------------------------------------------
  # 4. 임계값 산출 (검증 차원, STEP 6 결과와 일치해야 함)
  # -----------------------------------------------------------------------------
  q1_thr <- quantile(df_chart$TMR_pct, 0.25)
  q3_thr <- quantile(df_chart$TMR_pct, 0.75)
  cat(sprintf("\nQ1 임계값 (유망 기준): %.2f%%\n", q1_thr))
  cat(sprintf("Q3 임계값 (포화 기준): %.2f%%\n", q3_thr))
  
  # -----------------------------------------------------------------------------
  # 5. Y축 범위 — 줌인 (이미지 1 스타일)
  # -----------------------------------------------------------------------------
  y_min <- floor(min(df_chart$TMR_pct) / 5) * 5 - 5   # 25
  y_max <- ceiling(max(df_chart$TMR_pct) / 5) * 5 + 5 # 80
  cat(sprintf("\nY축 범위: %d ~ %d%%\n", y_min, y_max))
  
  # -----------------------------------------------------------------------------
  # 6. ggplot — 이미지 1 스타일 재현
  # -----------------------------------------------------------------------------
  p_tmr <- ggplot(df_chart, aes(x = Topic, y = TMR_pct, fill = Cluster_Name)) +
    
    # 막대
    geom_col(color = "gray30", linewidth = 0.3, width = 0.78) +
    
    # 데이터 라벨 (막대 위)
    geom_text(aes(label = TMR_label),
              vjust = -0.6, size = 3.6, fontface = "bold", color = "gray15") +
    
    # 임계값 라인 — Q3 (포화)
    geom_hline(yintercept = q3_thr, linetype = "dashed",
               color = "#C0392B", linewidth = 0.9) +
    annotate("text",
             x = 1, y = q3_thr + 1.8,
             label = sprintf("Relative Saturated (Q3: %.1f%%)", q3_thr),
             hjust = 0, vjust = 0, color = "#C0392B",
             fontface = "bold", size = 4) +
    
    # 임계값 라인 — Q1 (유망)
    geom_hline(yintercept = q1_thr, linetype = "dashed",
               color = "#2980B9", linewidth = 0.9) +
    annotate("text",
             x = 1, y = q1_thr - 2.5,
             label = sprintf("Relative Emerging (Q1: %.1f%%)", q1_thr),
             hjust = 0, vjust = 0, color = "#2980B9",
             fontface = "bold", size = 4) +
    
    # 색상 매핑
    scale_fill_manual(values = cluster_colors, name = "Mega Clusters (A-H)") +
    
    # Y축 — 줌인 + % 포맷
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = seq(y_min, y_max, by = 5),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    
    # 라벨
    labs(
      title    = "Final TMR Portfolio with 8 Mega Clusters (Gompertz-based)",
      subtitle = "Thresholds based on 1st and 3rd Quartiles of Gompertz TMR Distribution (Choi & Woo, 2022)",
      x        = "FinTech Topics (Ordered by Maturity)",
      y        = "Technology Maturity Rate (TMR, %)"
    ) +
    
    # 테마 — 이미지 1 스타일
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 16, hjust = 0.5,
                                      margin = margin(b = 5)),
      plot.subtitle    = element_text(size = 11, color = "gray45", hjust = 0.5,
                                      margin = margin(b = 18)),
      axis.title.x     = element_text(face = "bold", size = 12,
                                      margin = margin(t = 10)),
      axis.title.y     = element_text(face = "bold", size = 12,
                                      margin = margin(r = 10)),
      axis.text.x      = element_text(face = "bold", size = 10,
                                      angle = 45, hjust = 1, color = "gray20"),
      axis.text.y      = element_text(size = 10, color = "gray30"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 11),
      legend.text      = element_text(size = 10),
      legend.box.margin = margin(t = 10)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  # -----------------------------------------------------------------------------
  # 7. 저장 — dpi=450
  # -----------------------------------------------------------------------------
  output_png <- "STM_TMR_Gompertz_Threshold_BarChart.png"
  ggsave(output_png, plot = p_tmr,
         width = 14, height = 8, dpi = 450, bg = "white")
  cat("\n저장 완료 (dpi=450):", output_png, "\n")
  
  # 화면 출력
  print(p_tmr)
  
  # -----------------------------------------------------------------------------
  # 8. 신규 데이터 파일 별도 생성 (옛 STM_Topic_TMR_Results.csv 그대로 보존)
  # -----------------------------------------------------------------------------
  df_results <- df_chart %>%
    arrange(match(Topic, paste0("T", 1:20))) %>%  # T1~T20 순서로 복원
    select(Topic, Topic_Label, Mega_Cluster, Cluster_Name, TMR_pct) %>%
    mutate(Topic = as.character(Topic),
           Cluster_Name = as.character(Cluster_Name))
  
  new_csv <- "STM_TMR_Gompertz_Topic_TMR_Results.csv"
  write.csv(df_results, new_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("저장 완료 (신규):", new_csv, "\n")
  cat("(기존 STM_Topic_TMR_Results.csv는 그대로 보존됨)\n")
  
  # -----------------------------------------------------------------------------
  # 9. 워크스페이스 저장
  # -----------------------------------------------------------------------------
  save.image("STM_full_workspace_Phase0.RData")
  cat("\n워크스페이스 저장 완료.\n")
  
  cat("\n===== STEP 7 완료. 다음은 STEP 8 (본문 정정안 문구 작성). =====\n")
  
  # =============================================================================
  # STEP 7 수정: Cluster_Name 매핑 버그 수정 후 차트 재생성
  # 버그: CSV의 Cluster_Name에 알파벳 접두사 없음 → factor 매칭 실패 → 막대 NA
  # 수정: Mega_Cluster + Cluster_Name 조합해 "A. 기업 재무..." 형태 재구성
  # =============================================================================
  
  library(dplyr)
  library(ggplot2)
  
  cat("===== STEP 7 수정: 막대 NA 버그 수정 =====\n")
  
  # -----------------------------------------------------------------------------
  # 1. 데이터 로드
  # -----------------------------------------------------------------------------
  df_chart <- read.csv("STM_TLC_TMR_Combined_Table.csv",
                       fileEncoding = "UTF-8",
                       stringsAsFactors = FALSE)
  
  # -----------------------------------------------------------------------------
  # 2. ★ 수정 핵심: Cluster_Full 컬럼 신규 생성 (접두사 + 이름)
  # -----------------------------------------------------------------------------
  df_chart <- df_chart %>%
    mutate(Cluster_Full = paste0(Mega_Cluster, ". ", Cluster_Name))
  
  cat("수정된 Cluster_Full 고유값 (8개여야 함):\n")
  print(unique(df_chart$Cluster_Full))
  cat("→ cluster_colors 키와 일치 확인 필수\n")
  
  # -----------------------------------------------------------------------------
  # 3. 8대 클러스터 표준 색상
  # -----------------------------------------------------------------------------
  cluster_colors <- c(
    'A. 기업 재무 및 회계 관리'         = '#AEC6CF',
    'B. 맞춤형 보험 및 타겟 식별'       = '#FFD1BA',
    'C. AI 기반 신용/투자 분석'         = '#D8BFD8',
    'D. 블록체인 기반 에너지 금융'      = '#C1E1C1',
    'E. 블록체인 및 디지털 자산'        = '#FDFD96',
    'F. 보안 인증 및 규제 컴플라이언스' = '#B0C4DE',
    'G. 뱅킹 및 대금 정산'              = '#AFEEEE',
    'H. O2O 상거래 및 간편 결제'        = '#FFB6C1'
  )
  
  # 매핑 검증 — 모든 Cluster_Full이 cluster_colors 키에 있는지
  cat("\n매핑 검증 (모두 TRUE여야 함):\n")
  mapping_check <- unique(df_chart$Cluster_Full) %in% names(cluster_colors)
  print(setNames(mapping_check, unique(df_chart$Cluster_Full)))
  if (!all(mapping_check)) {
    stop("매핑 실패 — Cluster_Full 값이 cluster_colors 키와 불일치")
  }
  cat("→ 매핑 성공\n")
  
  # -----------------------------------------------------------------------------
  # 4. 정렬 + 팩터화
  # -----------------------------------------------------------------------------
  df_chart <- df_chart %>%
    arrange(TMR_pct) %>%
    mutate(
      Topic = factor(Topic, levels = Topic),
      Cluster_Full = factor(Cluster_Full, levels = names(cluster_colors)),
      TMR_label = sprintf("%.1f%%", TMR_pct)
    )
  
  # -----------------------------------------------------------------------------
  # 5. 임계값 + Y축 범위
  # -----------------------------------------------------------------------------
  q1_thr <- quantile(df_chart$TMR_pct, 0.25)
  q3_thr <- quantile(df_chart$TMR_pct, 0.75)
  y_min <- 25
  y_max <- 80
  
  # -----------------------------------------------------------------------------
  # 6. ggplot — Cluster_Full을 fill aes에 사용
  # -----------------------------------------------------------------------------
  p_tmr <- ggplot(df_chart, aes(x = Topic, y = TMR_pct, fill = Cluster_Full)) +
    
    geom_col(color = "gray30", linewidth = 0.3, width = 0.78) +
    
    geom_text(aes(label = TMR_label),
              vjust = -0.6, size = 3.6, fontface = "bold", color = "gray15") +
    
    # Q3 라인
    geom_hline(yintercept = q3_thr, linetype = "dashed",
               color = "#C0392B", linewidth = 0.9) +
    annotate("text",
             x = 1, y = q3_thr + 1.8,
             label = sprintf("Relative Saturated (Q3: %.1f%%)", q3_thr),
             hjust = 0, vjust = 0, color = "#C0392B",
             fontface = "bold", size = 4) +
    
    # Q1 라인
    geom_hline(yintercept = q1_thr, linetype = "dashed",
               color = "#2980B9", linewidth = 0.9) +
    annotate("text",
             x = 1, y = q1_thr - 2.5,
             label = sprintf("Relative Emerging (Q1: %.1f%%)", q1_thr),
             hjust = 0, vjust = 0, color = "#2980B9",
             fontface = "bold", size = 4) +
    
    scale_fill_manual(values = cluster_colors, name = "Mega Clusters (A-H)",
                      drop = FALSE) +
    
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = seq(y_min, y_max, by = 5),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    
    labs(
      title    = "Final TMR Portfolio with 8 Mega Clusters (Gompertz-based)",
      subtitle = "Thresholds based on 1st and 3rd Quartiles of Gompertz TMR Distribution (Choi & Woo, 2022)",
      x        = "FinTech Topics (Ordered by Maturity)",
      y        = "Technology Maturity Rate (TMR, %)"
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 16, hjust = 0.5,
                                      margin = margin(b = 5)),
      plot.subtitle    = element_text(size = 11, color = "gray45", hjust = 0.5,
                                      margin = margin(b = 18)),
      axis.title.x     = element_text(face = "bold", size = 12,
                                      margin = margin(t = 10)),
      axis.title.y     = element_text(face = "bold", size = 12,
                                      margin = margin(r = 10)),
      axis.text.x      = element_text(face = "bold", size = 10,
                                      angle = 45, hjust = 1, color = "gray20"),
      axis.text.y      = element_text(size = 10, color = "gray30"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 11),
      legend.text      = element_text(size = 10),
      legend.box.margin = margin(t = 10)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  # -----------------------------------------------------------------------------
  # 7. 저장 (기존 파일 덮어쓰기)
  # -----------------------------------------------------------------------------
  output_png <- "STM_TMR_Gompertz_Threshold_BarChart.png"
  ggsave(output_png, plot = p_tmr,
         width = 14, height = 8, dpi = 450, bg = "white")
  cat("\n저장 완료:", output_png, "\n")
  cat("(경고 메시지가 없어야 정상 — 'Removed 20 rows' 메시지가 사라졌는지 확인)\n")
  
  print(p_tmr)
  
  save.image("STM_full_workspace_Phase0.RData")
  cat("\n===== STEP 7 수정 완료 =====\n")
  
  # =============================================================================
  # STEP 7 재수정: Y축 줌인 방식 변경 (limits → coord_cartesian)
  # 버그: scale_y_continuous(limits=c(25,80))은 데이터를 clipping → 막대 NA
  # 수정: coord_cartesian(ylim=c(25,80))으로 시각만 줌인
  # =============================================================================
  
  library(dplyr)
  library(ggplot2)
  
  cat("===== STEP 7 재수정: Y축 줌인 방식 변경 =====\n")
  
  # -----------------------------------------------------------------------------
  # 데이터 준비 (이전과 동일)
  # -----------------------------------------------------------------------------
  df_chart <- read.csv("STM_TLC_TMR_Combined_Table.csv",
                       fileEncoding = "UTF-8",
                       stringsAsFactors = FALSE) %>%
    mutate(Cluster_Full = paste0(Mega_Cluster, ". ", Cluster_Name))
  
  cluster_colors <- c(
    'A. 기업 재무 및 회계 관리'         = '#AEC6CF',
    'B. 맞춤형 보험 및 타겟 식별'       = '#FFD1BA',
    'C. AI 기반 신용/투자 분석'         = '#D8BFD8',
    'D. 블록체인 기반 에너지 금융'      = '#C1E1C1',
    'E. 블록체인 및 디지털 자산'        = '#FDFD96',
    'F. 보안 인증 및 규제 컴플라이언스' = '#B0C4DE',
    'G. 뱅킹 및 대금 정산'              = '#AFEEEE',
    'H. O2O 상거래 및 간편 결제'        = '#FFB6C1'
  )
  
  df_chart <- df_chart %>%
    arrange(TMR_pct) %>%
    mutate(
      Topic = factor(Topic, levels = Topic),
      Cluster_Full = factor(Cluster_Full, levels = names(cluster_colors)),
      TMR_label = sprintf("%.1f%%", TMR_pct)
    )
  
  q1_thr <- quantile(df_chart$TMR_pct, 0.25)
  q3_thr <- quantile(df_chart$TMR_pct, 0.75)
  
  cat("데이터 준비 완료. TMR 범위:",
      round(min(df_chart$TMR_pct), 1), "~",
      round(max(df_chart$TMR_pct), 1), "%\n")
  
  # -----------------------------------------------------------------------------
  # ggplot — coord_cartesian 사용
  # -----------------------------------------------------------------------------
  p_tmr <- ggplot(df_chart, aes(x = Topic, y = TMR_pct, fill = Cluster_Full)) +
    
    geom_col(color = "gray30", linewidth = 0.3, width = 0.78) +
    
    geom_text(aes(label = TMR_label),
              vjust = -0.6, size = 3.6, fontface = "bold", color = "gray15") +
    
    # Q3 라인
    geom_hline(yintercept = q3_thr, linetype = "dashed",
               color = "#C0392B", linewidth = 0.9) +
    annotate("text",
             x = 1, y = q3_thr + 1.5,
             label = sprintf("Relative Saturated (Q3: %.1f%%)", q3_thr),
             hjust = 0, vjust = 0, color = "#C0392B",
             fontface = "bold", size = 4) +
    
    # Q1 라인
    geom_hline(yintercept = q1_thr, linetype = "dashed",
               color = "#2980B9", linewidth = 0.9) +
    annotate("text",
             x = 1, y = q1_thr - 2.5,
             label = sprintf("Relative Emerging (Q1: %.1f%%)", q1_thr),
             hjust = 0, vjust = 0, color = "#2980B9",
             fontface = "bold", size = 4) +
    
    scale_fill_manual(values = cluster_colors, name = "Mega Clusters (A-H)",
                      drop = FALSE) +
    
    # ★ 수정: scale_y_continuous는 breaks/labels만, limits는 coord_cartesian에서
    scale_y_continuous(
      breaks = seq(25, 80, by = 5),
      labels = function(x) paste0(x, "%")
    ) +
    
    # ★ 핵심 수정: coord_cartesian으로 시각만 줌인 (데이터 clipping 없음)
    coord_cartesian(ylim = c(25, 80)) +
    
    labs(
      title    = "Final TMR Portfolio with 8 Mega Clusters (Gompertz-based)",
      subtitle = "Thresholds based on 1st and 3rd Quartiles of Gompertz TMR Distribution (Choi & Woo, 2022)",
      x        = "FinTech Topics (Ordered by Maturity)",
      y        = "Technology Maturity Rate (TMR, %)"
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      plot.title         = element_text(face = "bold", size = 16, hjust = 0.5,
                                        margin = margin(b = 5)),
      plot.subtitle      = element_text(size = 11, color = "gray45", hjust = 0.5,
                                        margin = margin(b = 18)),
      axis.title.x       = element_text(face = "bold", size = 12,
                                        margin = margin(t = 10)),
      axis.title.y       = element_text(face = "bold", size = 12,
                                        margin = margin(r = 10)),
      axis.text.x        = element_text(face = "bold", size = 10,
                                        angle = 45, hjust = 1, color = "gray20"),
      axis.text.y        = element_text(size = 10, color = "gray30"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.4),
      legend.position    = "bottom",
      legend.title       = element_text(face = "bold", size = 11),
      legend.text        = element_text(size = 10),
      legend.box.margin  = margin(t = 10)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  # -----------------------------------------------------------------------------
  # 저장 (덮어쓰기)
  # -----------------------------------------------------------------------------
  output_png <- "STM_TMR_Gompertz_Threshold_BarChart.png"
  ggsave(output_png, plot = p_tmr,
         width = 14, height = 8, dpi = 450, bg = "white")
  cat("\n저장 완료:", output_png, "\n")
  cat("(★ 'Removed 20 rows' 경고가 사라져야 정상)\n")
  
  print(p_tmr)
  
  save.image("STM_full_workspace_Phase0.RData")
  cat("\n===== STEP 7 재수정 완료 =====\n")
  
  # =============================================================================
  # 워크스페이스 영구 저장 + 백업 (휘발 방지)
  # =============================================================================
  
  # 1. 작업 디렉토리 확인
  cat("===== 작업 디렉토리 =====\n")
  cat(getwd(), "\n\n")
  
  # 2. 저장 전 핵심 객체 점검 (다음 채팅에서 필요한 것들)
  cat("===== 저장 전 핵심 객체 점검 =====\n")
  required_objs <- c(
    # STEP 1~4 핵심
    "gompertz_results", "ci_results", "tmr_with_ci", "fit_list",
    # STEP 5~7 핵심
    "df_diag", "df_chart",
    # 기초 입력
    "theta_mat", "pub_year",
    # B-2 산출물 (이미 워크스페이스에 있던 것)
    "final_stm_20_adj", "out3", "tlc", "effect_country_US"
  )
  
  all_ok <- TRUE
  for (obj in required_objs) {
    exists_flag <- exists(obj)
    cat(sprintf("  %-25s : %s\n", obj, exists_flag))
    if (!exists_flag) all_ok <- FALSE
  }
  
  if (!all_ok) {
    warning("일부 객체 누락. 저장은 진행하되 다음 채팅에서 복원 작업 필요할 수 있음.")
  }
  
  # 3. 타임스탬프 백업 생성 (안전망)
  cat("\n===== 타임스탬프 백업 생성 =====\n")
  backup_name <- paste0("STM_full_workspace_Phase0_backup_",
                        format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
  if (file.exists("STM_full_workspace_Phase0.RData")) {
    file.copy("STM_full_workspace_Phase0.RData", backup_name)
    cat("백업 완료:", backup_name, "\n")
  } else {
    cat("기존 워크스페이스 없음 — 백업 생략\n")
  }
  
  # 4. 메인 워크스페이스 갱신 저장
  cat("\n===== 메인 워크스페이스 저장 =====\n")
  save.image("STM_full_workspace_Phase0.RData")
  info <- file.info("STM_full_workspace_Phase0.RData")
  cat("저장 완료: STM_full_workspace_Phase0.RData\n")
  cat("  크기:", round(info$size / 1024 / 1024, 2), "MB\n")
  cat("  시각:", as.character(info$mtime), "\n")
  
  # 5. 핸드오프 마크다운 파일도 함께 저장
  cat("\n===== 핸드오프 문서 저장 =====\n")
  handoff_path <- "B-3_STEP5to7_핸드오프_20260513.md"
  
  handoff_content <- '# B-3 STEP 5~7 완료 인계 (2026-05-13)

**작성일**: 2026-05-13
**From**: B-3 STEP 5~7 작업 채팅
**To**: 다음 채팅 (STEP 8 본문 정정안 문구 작성)
**작업 폴더**: <project-root>

---