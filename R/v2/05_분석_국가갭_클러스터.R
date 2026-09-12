# ==============================================================================
#  05_분석_국가갭_클러스터.R  —  [단계 8·9 일부] 클러스터·거품매트릭스·국가갭(BH FDR)
# ==============================================================================
#  입력 : STM_full_workspace_Phase0.RData (04), topic_mapping.csv (사람이 채운 것)
#  출력 : Topic_Country_Effect_US_Baseline.csv, STM_US_Baseline_FDR_Corrected.csv
#         STM_Qualitative_Bubble_Matrix.png, STM_Country_Cluster_Bar.png,
#         STM_Strategic_Gap_Heatmap_FDR.png  (전부 파스텔·고화질)
#
#  v1(Fintech_Project.R)을 충실히 옮기되: 라벨/클러스터는 topic_mapping.csv 에서,
#  색상은 00_palette.R 에서, FDR 비교수는 자동계산(N_COMPARISONS)으로.
# ==============================================================================

source("00_config.R"); source("00_palette.R")
library(stm); library(tidyverse); library(scales); library(ggrepel)

load(FILE_WORKSPACE)   # final_stm_20_adj, out3
source("00_config.R"); source("00_palette.R")   # 워크스페이스의 옛 설정 위에 현재 설정 재적용

# ── 토픽 매핑 로드 (04 후 사람이 채운 파일) ────────────────────────────────
if (!file.exists("topic_mapping.csv"))
  stop("topic_mapping.csv 가 없습니다. 04 실행 후 topic_mapping_TEMPLATE.csv를 채워 저장하세요.")
tm <- read.csv("topic_mapping.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
tm$Topic <- paste0("T", seq_len(nrow(tm)))               # 안전: 순서 보장
tm$Cluster_Full <- paste0(tm$Cluster, ". ", cluster_names_kr[tm$Cluster])
stopifnot(all(tm$Cluster %in% LETTERS[1:8]))             # A~H 채워졌는지 검증
cat("토픽 매핑 로드 OK | 클러스터 분포:", paste(table(tm$Cluster), collapse="/"), "\n")

# ── 1) US 기준 estimateEffect ──────────────────────────────────────────────
out3$meta$Origin_Country <- relevel(as.factor(out3$meta$Origin_Country), ref = REF_COUNTRY)
set.seed(SEED_NUM)
eff <- estimateEffect(1:K_TOPICS ~ Origin_Country + s(Publication.Year) + C_adj_i,
                      stmobj = final_stm_20_adj, metadata = out3$meta, uncertainty = "Global")
se <- summary(eff)
res <- do.call(rbind, lapply(1:K_TOPICS, function(i){
  tb <- as.data.frame(se$tables[[i]])           # 열 순서: Estimate, Std.Error, t, p (고정)
  terms <- rownames(tb)
  keep  <- grepl("Origin_Country", terms)
  data.frame(
    Topic       = i,
    Topic_Label = tm$Label_EN[i],
    Comparison  = paste0(gsub("Origin_Country", "", terms[keep]), " vs ", REF_COUNTRY),
    Estimate    = tb[keep, 1],                   # 1열 = Estimate
    p_value     = tb[keep, ncol(tb)],            # 마지막열 = Pr(>|t|) — 컬럼명 mangling 무관
    stringsAsFactors = FALSE
  )
}))
star <- function(p) ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*",ifelse(p<.1,".",""))))
res$Significance <- star(res$p_value)
write.csv(res, "Topic_Country_Effect_US_Baseline.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("💾 Topic_Country_Effect_US_Baseline.csv\n")

# ── 2) BH FDR 보정 (비교수 자동) ───────────────────────────────────────────
res$p_FDR        <- p.adjust(res$p_value, method = "BH")
res$p_Bonferroni <- p.adjust(res$p_value, method = "bonferroni")
res$Sig_FDR      <- star(res$p_FDR)
write.csv(res, "STM_US_Baseline_FDR_Corrected.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("💾 STM_US_Baseline_FDR_Corrected.csv | 총 비교 %d (=%d토픽×%d국가)\n",
            nrow(res), K_TOPICS, length(COMPARE_COUNTRIES)))

# ── 3) 전략갭 히트맵 (FDR, 파스텔 7단계) ───────────────────────────────────
res$Country <- gsub(paste0(" vs ", REF_COUNTRY), "", res$Comparison)
res <- res %>% left_join(tm[,c("Topic","Cluster","Cluster_Full")] %>% mutate(Topic=row_number()), by="Topic")
res$T_Num <- paste0("T", res$Topic)
res <- res %>% mutate(Fill = case_when(
  p_FDR<.001 & Estimate>0 ~ "Target (+) : p < 0.001 (***)",
  p_FDR<.01  & Estimate>0 ~ "Target (+) : p < 0.01 (**)",
  p_FDR<.05  & Estimate>0 ~ "Target (+) : p < 0.05 (*)",
  p_FDR<.001 & Estimate<0 ~ "US (-) : p < 0.001 (***)",
  p_FDR<.01  & Estimate<0 ~ "US (-) : p < 0.01 (**)",
  p_FDR<.05  & Estimate<0 ~ "US (-) : p < 0.05 (*)",
  TRUE ~ "Not Significant"))
res$Fill <- factor(res$Fill, levels = names(sig_heatmap_colors))
res$Cluster_Full <- factor(res$Cluster_Full, levels = names(cluster_colors_full))
res$Country <- factor(res$Country, levels = COMPARE_COUNTRIES)
res$T_Num <- factor(res$T_Num, levels = paste0("T", K_TOPICS:1))

p_heat <- ggplot(res, aes(Country, T_Num)) +
  geom_tile(aes(fill = Fill), color = "white", linewidth = 1) +
  geom_text(aes(label = Sig_FDR), color = "gray30", size = 5) +
  facet_grid(Cluster_Full ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = sig_heatmap_colors, drop = FALSE, guide = guide_legend(nrow = 2)) +
  labs(title = sprintf("Strategic Gap Heatmap (FDR-Corrected) vs. %s Baseline", REF_COUNTRY),
       subtitle = sprintf("Benjamini-Hochberg on %d comparisons (%d topics × %d countries)",
                          N_COMPARISONS, K_TOPICS, length(COMPARE_COUNTRIES)),
       x = paste("vs.", REF_COUNTRY), y = "Topics (by Cluster)", fill = "Dominance & Significance") +
  theme_minimal(base_size = 13) +
  theme(plot.title=element_text(face="bold",hjust=.5), strip.text.y=element_blank(),
        legend.position="bottom", panel.grid=element_blank(),
        axis.text.x=element_text(face="bold",size=13))
ggsave("STM_Strategic_Gap_Heatmap_FDR.png", p_heat, width=13, height=10, dpi=FIG_DPI, bg=FIG_BG)
cat("💾 STM_Strategic_Gap_Heatmap_FDR.png\n")

# ── 4) 질적 거품 매트릭스 (Core vs Bubble) ─────────────────────────────────
theta <- as.data.frame(final_stm_20_adj$theta); colnames(theta) <- paste0("T", 1:K_TOPICS)
theta$Origin_Country <- as.character(out3$meta$Origin_Country)
theta$C_adj_i <- out3$meta$C_adj_i
theta <- theta %>% filter(Origin_Country %in% COUNTRIES)
thr90 <- quantile(theta$C_adj_i, 0.90, na.rm = TRUE)
theta$Hi <- theta$C_adj_i >= thr90
long <- theta %>% pivot_longer(starts_with("T"), names_to="Topic", values_to="Theta")
quant <- long %>% group_by(Topic,Origin_Country) %>% summarise(t=sum(Theta),.groups="drop") %>%
  group_by(Topic) %>% mutate(Quant=t/sum(t)) %>% ungroup()
qual <- long %>% filter(Hi) %>% group_by(Topic,Origin_Country) %>% summarise(h=sum(Theta),.groups="drop") %>%
  group_by(Topic) %>% mutate(Qual=h/sum(h)) %>% ungroup()
bub <- quant %>% left_join(qual %>% select(Topic,Origin_Country,Qual), by=c("Topic","Origin_Country")) %>%
  mutate(Qual=replace_na(Qual,0),
         Status=ifelse(Qual>=Quant,"Core Competence (Quality≥Quantity)","Qualitative Bubble (Quantity>Quality)"))
mx <- max(bub$Quant, bub$Qual)*1.05
p_bub <- ggplot(bub, aes(Quant, Qual, color=Status)) +
  geom_abline(slope=1, linetype="dashed", color="gray40") +
  geom_point(size=3.5, alpha=.8) +
  geom_text_repel(aes(label=Topic), size=3.3, fontface="bold", show.legend=FALSE, max.overlaps=20) +
  scale_x_continuous(labels=percent_format(1), limits=c(0,mx)) +
  scale_y_continuous(labels=percent_format(1), limits=c(0,mx)) +
  scale_color_manual(values=c("Core Competence (Quality≥Quantity)"="#2E86C1",
                              "Qualitative Bubble (Quantity>Quality)"="#E74C3C")) +
  facet_wrap(~Origin_Country, ncol=3) + theme_bw(base_size=12) +
  labs(title="Qualitative Bubble Matrix by Country",
       subtitle="Quantitative Share (X) vs. Top 10% High-Impact Quality Share (Y)",
       x="Quantitative Share", y="Qualitative Share (Top 10%)", color="Status") +
  theme(legend.position="bottom", plot.title=element_text(face="bold"))
ggsave("STM_Qualitative_Bubble_Matrix.png", p_bub, width=16, height=10, dpi=FIG_DPI, bg=FIG_BG)
cat("💾 STM_Qualitative_Bubble_Matrix.png\n")

# ── 5) 국가×클러스터 양적 점유율 막대 (파스텔 클러스터색) ───────────────────
theta_c <- long %>% left_join(tm %>% mutate(Topic=paste0("T",row_number())) %>% select(Topic,Cluster), by="Topic") %>%
  group_by(Origin_Country, Cluster) %>% summarise(Theta=sum(Theta), .groups="drop") %>%
  group_by(Origin_Country) %>% mutate(Share=Theta/sum(Theta)) %>% ungroup()
theta_c$Origin_Country <- factor(theta_c$Origin_Country, levels=COUNTRIES)
p_bar <- ggplot(theta_c, aes(Origin_Country, Share, fill=Cluster)) +
  geom_col(color="gray30", linewidth=.2) +
  scale_fill_manual(values=cluster_colors_alpha,
                    labels=paste0(names(cluster_names_kr), ". ", cluster_names_kr)) +
  scale_y_continuous(labels=percent_format(1)) +
  labs(title="국가별 클러스터 양적 점유율", x="국가(블록)", y="점유율", fill="메가 클러스터") +
  theme_minimal(base_size=13) + theme(plot.title=element_text(face="bold",hjust=.5), legend.position="right")
ggsave("STM_Country_Cluster_Bar.png", p_bar, width=12, height=8, dpi=FIG_DPI, bg=FIG_BG)
cat("💾 STM_Country_Cluster_Bar.png\n")

cat("\n✅ 05 완료. 거품매트릭스에서 ★EP(유럽) 패널이 의미있게 채워졌는지 확인하세요.\n")
