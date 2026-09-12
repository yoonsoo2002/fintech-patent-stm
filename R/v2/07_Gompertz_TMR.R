# ==============================================================================
#  07_Gompertz_TMR.R  —  [단계 9] Gompertz S곡선 기술성숙도(TMR) + 진단 + 막대차트
# ==============================================================================
#  입력 : STM_full_workspace_Phase0.RData, STM_TLC_TMR_Combined_Table.csv(06), topic_mapping.csv
#  출력 : STM_Gompertz_Diagnostics.csv (부록 A-1)
#         STM_TMR_Gompertz_BarChart.png (파스텔·고화질)
#         STM_TLC_TMR_Combined_Table.csv 의 TMR_pct 채움
#
#  v1(Gompertz_Diagnostics.R) 충실히 옮김. TMR=현재누적/Asym. CI 3단계 폴백.
#  ★Y축 줌은 coord_cartesian(ylim=)만 (scale_y limits 쓰면 'Removed 20 rows'로 막대 사라짐).
# ==============================================================================

source("00_config.R"); source("00_palette.R")
library(stm); library(tidyverse); library(boot)

load(FILE_WORKSPACE)
source("00_config.R"); source("00_palette.R")   # 워크스페이스의 옛 설정 위에 현재 설정 재적용
tm <- read.csv("topic_mapping.csv", fileEncoding="UTF-8", stringsAsFactors=FALSE)

# ── 1) 입력 재구성 + 연도윈도우 ────────────────────────────────────────────
theta <- final_stm_20_adj$theta; colnames(theta) <- paste0("T", 1:K_TOPICS)
py <- as.numeric(as.character(out3$meta$Publication.Year))
ok <- !is.na(py); theta <- theta[ok,]; py <- py[ok]
keep <- py >= YEAR_START & py <= YEAR_END
theta <- theta[keep,]; py <- py[keep]
cat("Gompertz 연도윈도우:", YEAR_START, "-", YEAR_END, "(", length(unique(py)), "시점)\n")

# ── 2) Gompertz 적합 + TMR ─────────────────────────────────────────────────
gr <- data.frame(Topic=paste0("T",1:K_TOPICS), Asym=NA, b2=NA, b3=NA,
                 Current_Cum=NA, TMR_pct=NA, RSS=NA, AIC=NA,
                 convergence=NA, iterations=NA, tolerance=NA)
fit_list <- vector("list", K_TOPICS)
for (i in 1:K_TOPICS) {
  yearly <- tapply(theta[,i], py, sum); cum <- cumsum(yearly); ts <- 1:length(cum)
  fit <- tryCatch(nls(cum ~ SSgompertz(ts, Asym, b2, b3)), error=function(e) NULL)
  gr$Current_Cum[i] <- max(cum)
  if (!is.null(fit)) {
    fit_list[[i]] <- fit; cf <- coef(fit)
    gr$Asym[i]<-cf["Asym"]; gr$b2[i]<-cf["b2"]; gr$b3[i]<-cf["b3"]
    gr$TMR_pct[i] <- max(cum)/cf["Asym"]*100
    gr$RSS[i]<-sum(resid(fit)^2); gr$AIC[i]<-AIC(fit)
    ci<-fit$convInfo; gr$convergence[i]<-ci$isConv; gr$iterations[i]<-ci$finIter; gr$tolerance[i]<-ci$finTol
  }
}
cat("적합 성공:", sum(!sapply(fit_list,is.null)), "/", K_TOPICS, "\n")

# ── 3) CI 3단계 폴백 (profile → Wald → bootstrap) ──────────────────────────
ci_res <- data.frame(Topic=paste0("T",1:K_TOPICS), Asym_lower=NA, Asym_upper=NA, CI_method=NA)
boot_g <- function(yearly, B=1000){
  d <- data.frame(t=1:length(yearly), y=cumsum(yearly))
  bs_fn <- function(dat,idx){ dd<-dat[idx,]; dd<-dd[order(dd$t),]
    f<-tryCatch(nls(y~SSgompertz(t,Asym,b2,b3),data=dd),error=function(e)NULL)
    if(is.null(f)) c(NA,NA,NA) else coef(f) }
  r <- boot(d, bs_fn, R=B); apply(r$t,2,function(x) quantile(x,c(.025,.975),na.rm=TRUE))
}
for (i in 1:K_TOPICS) {
  f <- fit_list[[i]]; if (is.null(f)) { ci_res$CI_method[i]<-"fit_failed"; next }
  pf <- tryCatch(suppressWarnings(suppressMessages(confint(f, level=.95))), error=function(e)NULL, warning=function(w)NULL)
  if (!is.null(pf) && !any(is.na(pf))) { ci_res$Asym_lower[i]<-pf["Asym",1]; ci_res$Asym_upper[i]<-pf["Asym",2]; ci_res$CI_method[i]<-"profile"; next }
  wd <- tryCatch(suppressWarnings(confint.default(f, level=.95)), error=function(e)NULL)
  if (!is.null(wd) && !any(is.na(wd))) { ci_res$Asym_lower[i]<-wd["Asym",1]; ci_res$Asym_upper[i]<-wd["Asym",2]; ci_res$CI_method[i]<-"Wald"; next }
  yb <- tapply(theta[,i], py, sum); bt <- tryCatch(boot_g(yb), error=function(e)NULL)
  if (!is.null(bt)) { ci_res$Asym_lower[i]<-bt[1,1]; ci_res$Asym_upper[i]<-bt[2,1]; ci_res$CI_method[i]<-"bootstrap" } else ci_res$CI_method[i]<-"all_failed"
}
gr$TMR_lower <- gr$Current_Cum / ci_res$Asym_upper * 100
gr$TMR_upper <- gr$Current_Cum / ci_res$Asym_lower * 100

# ── 4) 진단표 저장 (부록 A-1) ──────────────────────────────────────────────
diag <- gr %>% mutate(Asym_lower=ci_res$Asym_lower, Asym_upper=ci_res$Asym_upper,
                      CI_method=ci_res$CI_method,
                      RSS_RMSE_pct = sqrt(RSS)/Current_Cum*100,
                      across(where(is.numeric), ~round(.,3)))
write.csv(diag, "STM_Gompertz_Diagnostics.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("💾 STM_Gompertz_Diagnostics.csv\n")

# ── 5) 종합표 TMR 채우기 ───────────────────────────────────────────────────
comb <- read.csv(FILE_COMBINED, fileEncoding="UTF-8", stringsAsFactors=FALSE)
comb$TMR_pct <- round(gr$TMR_pct[match(comb$Topic, gr$Topic)], 2)
write.csv(comb, FILE_COMBINED, row.names=FALSE, fileEncoding="UTF-8")
cat("💾", FILE_COMBINED, "(TMR_pct 채움)\n")

# ── 6) TMR 막대차트 (파스텔, coord_cartesian) ──────────────────────────────
dc <- comb %>% mutate(Cluster_Full = paste0(Mega_Cluster, ". ", Cluster_Name)) %>%
  arrange(TMR_pct) %>%
  mutate(Topic=factor(Topic, levels=Topic),
         Cluster_Full=factor(Cluster_Full, levels=names(cluster_colors_full)),
         lab=sprintf("%.1f%%", TMR_pct))
q1 <- quantile(dc$TMR_pct, .25, na.rm=TRUE); q3 <- quantile(dc$TMR_pct, .75, na.rm=TRUE)
ylo <- floor(min(dc$TMR_pct,na.rm=TRUE)/5)*5 - 5; yhi <- ceiling(max(dc$TMR_pct,na.rm=TRUE)/5)*5 + 5
p_tmr <- ggplot(dc, aes(Topic, TMR_pct, fill=Cluster_Full)) +
  geom_col(color="gray30", linewidth=.3, width=.78) +
  geom_text(aes(label=lab), vjust=-0.6, size=3.4, fontface="bold", color="gray15") +
  geom_hline(yintercept=q3, linetype="dashed", color="#C0392B", linewidth=.9) +
  annotate("text", x=1, y=q3+1.5, label=sprintf("Relative Saturated (Q3: %.1f%%)", q3),
           hjust=0, color="#C0392B", fontface="bold", size=4) +
  geom_hline(yintercept=q1, linetype="dashed", color="#2980B9", linewidth=.9) +
  annotate("text", x=1, y=q1-2.5, label=sprintf("Relative Emerging (Q1: %.1f%%)", q1),
           hjust=0, color="#2980B9", fontface="bold", size=4) +
  scale_fill_manual(values=cluster_colors_full, name="메가 클러스터 (A-H)", drop=FALSE) +
  scale_y_continuous(breaks=seq(ylo,yhi,by=5), labels=function(x) paste0(x,"%")) +
  coord_cartesian(ylim=c(ylo,yhi)) +     # ★줌은 여기서만 (scale_y limits 금지)
  labs(title="Final TMR Portfolio with 8 Mega Clusters (Gompertz-based)",
       subtitle="Thresholds = Q1/Q3 of Gompertz TMR (Choi & Woo, 2022)",
       x="FinTech Topics (Ordered by Maturity)", y="Technology Maturity Rate (TMR, %)") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold",size=16,hjust=.5),
        axis.text.x=element_text(face="bold",angle=45,hjust=1),
        legend.position="bottom") +
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("STM_TMR_Gompertz_BarChart.png", p_tmr, width=14, height=8, dpi=FIG_DPI, bg=FIG_BG)
cat("💾 STM_TMR_Gompertz_BarChart.png ('Removed rows' 경고 없어야 정상)\n✅ 07 완료\n")
