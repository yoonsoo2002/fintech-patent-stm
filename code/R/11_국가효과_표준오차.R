# ==============================================================================
#  11_국가효과_표준오차.R  —  US 기준 국가효과의 표준오차 (EP 소표본 정밀도 점검)
# ==============================================================================
#  입력 : STM_full_workspace_Phase0.RData (04)
#  출력 : STM_Country_Effect_SE.csv          (Topic, Comparison, Estimate, Std_Error, t, p)
#         STM_Country_Effect_SE_Summary.csv  (비교국별 SE 평균·중앙값, EP/타3국 비율)
#
#  05와 동일 사양: Origin_Country relevel(ref=US), 같은 공식, set.seed(SEED_NUM), uncertainty="Global".
#  05의 Topic_Country_Effect_US_Baseline.csv 가 있으면 Estimate 일치 여부를 확인한다.
# ==============================================================================

options(fintech.code_dir = local({  # 스크립트 폴더 (Rscript·source 모두 지원)
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f)) else if (!is.null(sys.frame(1)$ofile)) dirname(normalizePath(sys.frame(1)$ofile)) else getwd()
}))
source(file.path(getOption("fintech.code_dir"), "00_config.R"))
library(stm)

load(FILE_WORKSPACE)   # final_stm_20_adj, out3
source(file.path(getOption("fintech.code_dir"), "00_config.R"))   # 워크스페이스의 옛 설정 위에 현재 설정 재적용

# ── 1) estimateEffect (05와 동일) ──────────────────────────────────────────
out3$meta$Origin_Country <- relevel(as.factor(out3$meta$Origin_Country), ref = REF_COUNTRY)
set.seed(SEED_NUM)
eff <- estimateEffect(1:K_TOPICS ~ Origin_Country + s(Publication.Year) + C_adj_i,
                      stmobj = final_stm_20_adj, metadata = out3$meta, uncertainty = "Global")
se <- summary(eff)

res <- do.call(rbind, lapply(1:K_TOPICS, function(i){
  tb <- as.data.frame(se$tables[[i]])            # 열 순서: Estimate, Std. Error, t value, Pr(>|t|)
  keep <- grepl("Origin_Country", rownames(tb))
  data.frame(Topic      = i,
             Comparison = paste0(gsub("Origin_Country", "", rownames(tb)[keep]), " vs ", REF_COUNTRY),
             Estimate   = tb[keep, 1], Std_Error = tb[keep, 2], t = tb[keep, 3], p = tb[keep, 4],
             stringsAsFactors = FALSE)
}))
write.csv(res, "STM_Country_Effect_SE.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("💾 STM_Country_Effect_SE.csv |", nrow(res), "행\n")

# ── 2) 05 산출물과 Estimate 대조 ───────────────────────────────────────────
if (file.exists("Topic_Country_Effect_US_Baseline.csv")) {
  b <- read.csv("Topic_Country_Effect_US_Baseline.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  k <- match(paste(b$Topic, b$Comparison), paste(res$Topic, res$Comparison))
  cat(sprintf("05 대조: 행 매칭 %d/%d, Estimate 최대 절대차 %.3g, p 최대 절대차 %.3g\n",
              sum(!is.na(k)), nrow(b), max(abs(res$Estimate[k] - b$Estimate)), max(abs(res$p[k] - b$p_value))))
}

# ── 3) 비교국별 SE 요약 + EP 비율 ──────────────────────────────────────────
cmp <- paste0(COMPARE_COUNTRIES, " vs ", REF_COUNTRY)
summ <- data.frame(Comparison = cmp,
                   Mean_SE    = sapply(cmp, function(c) mean(res$Std_Error[res$Comparison == c])),
                   Median_SE  = sapply(cmp, function(c) median(res$Std_Error[res$Comparison == c])),
                   N_docs     = as.integer(table(out3$meta$Origin_Country)[COMPARE_COUNTRIES]),
                   row.names = NULL)
ep    <- summ$Comparison == paste0("EP vs ", REF_COUNTRY)
summ$Ratio_vs_Others_Mean <- NA
summ$Ratio_vs_Others_Mean[ep] <- summ$Mean_SE[ep] / mean(summ$Mean_SE[!ep])
write.csv(summ, "STM_Country_Effect_SE_Summary.csv", row.names = FALSE, fileEncoding = "UTF-8")
print(summ, digits = 4)
cat(sprintf("EP 평균 SE / 타 3국 평균 SE의 평균 = %.3f (기준국 %s, N_%s = %d)\n",
            summ$Ratio_vs_Others_Mean[ep], REF_COUNTRY, REF_COUNTRY,
            sum(out3$meta$Origin_Country == REF_COUNTRY)))
cat("✅ 11 완료\n")
