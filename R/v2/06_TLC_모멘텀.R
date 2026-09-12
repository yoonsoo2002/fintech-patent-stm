# ==============================================================================
#  06_TLC_모멘텀.R  —  [단계 9] TLC 성장 모멘텀(B-Spline 1차 도함수) + 4사분면 매트릭스
# ==============================================================================
#  입력 : STM_full_workspace_Phase0.RData, topic_mapping.csv
#  출력 : STM_TLC_Portfolio_Matrix_20Topics.png / _8Clusters.png (파스텔·고화질)
#         STM_TLC_TMR_Combined_Table.csv  (TMR_pct=NA → 07에서 채움)
#
#  v1(TLC_BSpline_Momentum.R)을 충실히 옮김. 본문 채택 모멘텀 = Inner 구간 평균 도함수.
#  ★Y축 줌은 coord_cartesian만 사용(scale_y limits 금지 — 막대/점 사라짐).
# ==============================================================================

source("00_config.R"); source("00_palette.R")
library(stm); library(tidyverse); library(splines); library(ggrepel)

load(FILE_WORKSPACE)
source("00_config.R"); source("00_palette.R")   # 워크스페이스의 옛 설정 위에 현재 설정 재적용
tm <- read.csv("topic_mapping.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
tm$Topic <- paste0("T", seq_len(nrow(tm)))

# ── 1) estimateEffect (모멘텀 입력) ────────────────────────────────────────
out3$meta$Origin_Country <- relevel(as.factor(out3$meta$Origin_Country), ref = REF_COUNTRY)
set.seed(SEED_NUM)
eff <- estimateEffect(1:K_TOPICS ~ Origin_Country + s(Publication.Year) + C_adj_i,
                      stmobj = final_stm_20_adj, metadata = out3$meta, uncertainty = "Global")

# ── 2) B-Spline basis 도함수 (수치 미분, 중심차분 ε) ───────────────────────
mf <- eff$modelframe; sc <- mf$`s(Publication.Year)`
ki <- attr(sc, "knots"); kb <- attr(sc, "Boundary.knots")
deg <- attr(sc, "degree"); itc <- attr(sc, "intercept")
eps <- 0.001
deriv_at <- function(y){
  bp <- bs(y+eps, knots=ki, Boundary.knots=kb, degree=deg, intercept=itc)
  bm <- bs(y-eps, knots=ki, Boundary.knots=kb, degree=deg, intercept=itc)
  as.numeric((bp - bm) / (2*eps))
}
# 평가시점 = Inner 구간(양끝 경계연도 제외) — 본문 채택값
basis_deriv_mat <- t(sapply(MOMENTUM_INNER, deriv_at))   # (시점 × basis)

coef_names <- names(eff$parameters[[1]][[1]]$est)
spline_idx <- grep("^s\\(Publication\\.Year\\)", coef_names)
n_sims <- length(eff$parameters[[1]])

mom_sims <- matrix(NA, K_TOPICS, n_sims)
for (k in 1:K_TOPICS) for (s in 1:n_sims) {
  cf <- eff$parameters[[k]][[s]]$est[spline_idx]
  mom_sims[k, s] <- mean(basis_deriv_mat %*% cf)
}
mom_mean  <- apply(mom_sims, 1, mean)               * 100
mom_lower <- apply(mom_sims, 1, quantile, .025)     * 100
mom_upper <- apply(mom_sims, 1, quantile, .975)     * 100

# ── 3) 토픽 데이터프레임 ───────────────────────────────────────────────────
prop <- colMeans(final_stm_20_adj$theta)
df_topic <- data.frame(
  Topic = paste0("T", 1:K_TOPICS), Cluster = tm$Cluster,
  Proportion_pct = prop*100, Momentum = mom_mean, CI_Lower = mom_lower, CI_Upper = mom_upper
)
mid_x <- median(df_topic$Proportion_pct); mid_y <- median(df_topic$Momentum)
df_topic$Quadrant <- with(df_topic, ifelse(Proportion_pct>=mid_x & Momentum>=mid_y, "Q1_Mainstream",
                     ifelse(Proportion_pct< mid_x & Momentum>=mid_y, "Q2_Emerging",
                     ifelse(Proportion_pct< mid_x & Momentum< mid_y, "Q3_Niche", "Q4_Saturated"))))

# 사분면 매트릭스 공통 함수 (파스텔 클러스터색)
plot_tlc <- function(d, label_col, title, file, size_range){
  xr <- range(d$Proportion_pct); yr <- range(c(d$CI_Lower,d$CI_Upper))
  xp <- diff(xr)*.18; yp <- diff(yr)*.20
  xmin<-xr[1]-xp; xmax<-xr[2]+xp; ymin<-yr[1]-yp; ymax<-yr[2]+yp
  mx <- median(d$Proportion_pct); my <- median(d$Momentum)
  ggplot(d, aes(Proportion_pct, Momentum)) +
    geom_vline(xintercept=mx, linetype="dashed", color="gray40") +
    geom_hline(yintercept=my, linetype="dashed", color="gray40") +
    geom_hline(yintercept=0, color="gray20", linewidth=.4, alpha=.7) +
    geom_errorbar(aes(ymin=CI_Lower, ymax=CI_Upper, color=Cluster), width=.1, alpha=.5) +
    geom_point(aes(size=Proportion_pct, fill=Cluster), shape=21, color="white", stroke=.8, alpha=.9) +
    geom_text_repel(aes(label=.data[[label_col]], color=Cluster), size=4, fontface="bold",
                    show.legend=FALSE, max.overlaps=Inf, box.padding=.5) +
    annotate("text", x=xmax, y=ymax, label="Q1 Mainstream", color="#27AE60", fontface="bold", hjust=1) +
    annotate("text", x=xmin, y=ymax, label="Q2 Emerging",   color="#E67E22", fontface="bold", hjust=0) +
    annotate("text", x=xmin, y=ymin, label="Q3 Niche",      color="gray30",  fontface="bold", hjust=0) +
    annotate("text", x=xmax, y=ymin, label="Q4 Saturated",  color="#5DADE2", fontface="bold", hjust=1) +
    scale_color_manual(values=cluster_colors_alpha, guide="none") +
    scale_fill_manual(values=cluster_colors_alpha, name="메가 클러스터",
                      labels=paste0(names(cluster_names_kr), ". ", cluster_names_kr)) +
    scale_size_continuous(range=size_range, guide="none") +
    coord_cartesian(xlim=c(xmin,xmax), ylim=c(ymin,ymax)) +   # ★줌은 여기서만
    labs(title=title,
         subtitle=sprintf("B-Spline 평균 도함수 모멘텀 (%d-%d), 95%% CI / 색=메가클러스터",
                          min(MOMENTUM_INNER), max(MOMENTUM_INNER)),
         x="토픽 비중 (%)", y="성장 모멘텀 (×100)") +
    theme_minimal(base_size=14) +
    theme(plot.title=element_text(face="bold",size=17), legend.position="right") -> p
  ggsave(file, p, width=14, height=10, dpi=FIG_DPI, bg=FIG_BG); cat("💾", file, "\n")
}

plot_tlc(df_topic, "Topic", "Technology Life Cycle (TLC) Matrix — 20 Topics",
         "STM_TLC_Portfolio_Matrix_20Topics.png", c(6,18))

# ── 4) 8클러스터판 (비중 가중 모멘텀) ──────────────────────────────────────
clu <- df_topic %>% group_by(Cluster) %>%
  summarise(Momentum=weighted.mean(Momentum, Proportion_pct),
            CI_Lower=weighted.mean(CI_Lower, Proportion_pct),
            CI_Upper=weighted.mean(CI_Upper, Proportion_pct),
            Proportion_pct=sum(Proportion_pct), .groups="drop")
# clu 는 이미 Cluster(A~H) 컬럼 보유 → plot_tlc 가 fill=Cluster, 라벨=Cluster 로 사용
plot_tlc(clu, "Cluster", "TLC Portfolio Matrix — 8 Mega Clusters",
         "STM_TLC_Portfolio_Matrix_8Clusters.png", c(18,38))

# ── 5) 종합표 (TMR=NA → 07에서 채움) ───────────────────────────────────────
combined <- df_topic %>%
  mutate(Topic_Label = tm$Label_KR, Mega_Cluster = Cluster,
         Cluster_Name = cluster_names_kr[Cluster],
         Growth_Momentum = round(Momentum,3), Proportion_pct = round(Proportion_pct,3),
         Impact = NA, TMR_pct = NA) %>%
  select(Topic, Topic_Label, Mega_Cluster, Cluster_Name, Proportion_pct,
         Growth_Momentum, Impact, TMR_pct, Quadrant)
write.csv(combined, FILE_COMBINED, row.names=FALSE, fileEncoding="UTF-8")
cat("💾", FILE_COMBINED, "(TMR_pct는 07 Gompertz에서 채움)\n✅ 06 완료\n")
