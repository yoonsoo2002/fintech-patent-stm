# ==============================================================================
#  10_본문그림_재현.R  —  [단계 10] 본문 그림·표 중 스크립트가 없던 산출물 재현
# ==============================================================================
#  입력 : STM_full_workspace_Phase0.RData (04), topic_mapping.csv, STM_TLC_TMR_Combined_Table.csv (06·07)
#  출력 : paper_topic_stats.csv, paper_yearly_topic.csv, paper_yearly_cluster.csv, paper_country_cluster.csv,
#         paper_network_edges.csv, paper_frex15.csv, paper_prob15.csv,
#         부록_A1_토픽종합표.csv, 부록_A3_PROB_FREX.csv,
#         STM_TLC_TMR_Combined_Table.csv 의 Impact 채움
#         STM_Topic_Dendrogram.png, STM_Topic_Scatter_Size_Impact.png, STM_Cluster_Scatter_Size_Impact.png,
#         STM_Topic_Correlation_Network.png, STM_All_Topics_Yearly_Heatmap.png,
#         STM_Cluster_Yearly_Lines.png, STM_Country_Radar.png,
#         STM_Topic_Yearly_Lines_20.png (부록: 20토픽 연도별 평균 θ)
#
#  2026-08-11 에 즉석으로 만든 산출물의 정의를 기존 CSV 값에서 역산해 고정한 것:
#   Impact      = θ 가중 평균 C_adj_i  (Σθ·C_adj / Σθ)
#   예상 건수   = Σθ (토픽 θ 합계)
#   연도 비중   = Publication.Year 별 θ 평균 (YEAR_SEQ, 2026 공개분 제외)
#   네트워크    = 00_config.R NET_EDGE_RULE ("posadj" = 논문 기본, "cor" = cor ≥ NET_THRESHOLD 대안),
#                 betweenness(normalized, 비가중)
#   덴드로그램  = exp(logbeta) 행렬 유클리드 거리 + Ward.D2
# ==============================================================================

options(fintech.code_dir = local({  # 스크립트 폴더 (Rscript·source 모두 지원)
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f)) else if (!is.null(sys.frame(1)$ofile)) dirname(normalizePath(sys.frame(1)$ofile)) else getwd()
}))
source(file.path(getOption("fintech.code_dir"), "00_config.R")); source(file.path(getOption("fintech.code_dir"), "00_palette.R"))
library(stm); library(tidyverse); library(igraph); library(ggrepel)

load(FILE_WORKSPACE)   # final_stm_20_adj, out3
source(file.path(getOption("fintech.code_dir"), "00_config.R")); source(file.path(getOption("fintech.code_dir"), "00_palette.R"))   # 워크스페이스의 옛 설정 위에 현재 설정 재적용

tm <- read.csv("topic_mapping.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
tm$Topic <- paste0("T", seq_len(nrow(tm)))
stopifnot(nrow(tm) == K_TOPICS, all(tm$Cluster %in% names(cluster_colors_alpha)))

stopifnot(NET_EDGE_RULE %in% c("posadj", "cor"))   # 네트워크 규칙·임계치는 00_config.R
net_rule_lab <- if (NET_EDGE_RULE == "posadj") sprintf("topicCorr 양의 상관, r > %.2f", NET_CORR_CUTOFF) else
  sprintf("r ≥ %.2f", NET_THRESHOLD)

theta <- final_stm_20_adj$theta; colnames(theta) <- tm$Topic
meta  <- out3$meta
N_DOCS <- nrow(theta)

# ── 1) 토픽 통계: 비중·예상건수·Impact ─────────────────────────────────────
count  <- colSums(theta)
impact <- colSums(theta * meta$C_adj_i) / count
prev   <- colMeans(theta)

# ── 2) topicCorr 네트워크 + 매개중심성 ─────────────────────────────────────
tc <- topicCorr(final_stm_20_adj, cutoff = NET_CORR_CUTOFF)
cm <- tc$cor; dimnames(cm) <- list(tm$Topic, tm$Topic)
adj <- if (NET_EDGE_RULE == "posadj") tc$posadj == 1 else cm >= NET_THRESHOLD
ij <- which(adj & upper.tri(cm), arr.ind = TRUE)
ij <- ij[order(ij[, 1], ij[, 2]), , drop = FALSE]
edges <- data.frame(from = tm$Topic[ij[, 1]], to = tm$Topic[ij[, 2]], weight = cm[ij])
g <- graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = tm$Topic))
btw <- betweenness(g, directed = FALSE, normalized = TRUE, weights = NA)[tm$Topic]
n_iso <- sum(degree(g) == 0)
cat(sprintf("네트워크: 엣지 %d개, 고립 노드 %d개\n", nrow(edges), n_iso))

topic_stats <- data.frame(Topic = tm$Topic, prevalence = round(prev, 4), impact = round(impact, 3),
                          betweenness = round(btw, 4), cluster = tm$Cluster)
write.csv(topic_stats, "paper_topic_stats.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(edges %>% mutate(weight = round(weight, 3)), "paper_network_edges.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("💾 paper_topic_stats.csv, paper_network_edges.csv\n")

# ── 3) 연도별 평균 θ (토픽·클러스터) ──────────────────────────────────────
py <- as.numeric(as.character(meta$Publication.Year))
yr_topic <- sapply(YEAR_SEQ, function(y) colMeans(theta[py == y, , drop = FALSE]))   # 토픽 × 연도
colnames(yr_topic) <- YEAR_SEQ
yr_clu <- rowsum(yr_topic, tm$Cluster)                                              # 클러스터 = 소속 토픽 합
write.csv(data.frame(Topic = tm$Topic, round(yr_topic, 4), check.names = FALSE),
          "paper_yearly_topic.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(Cluster = rownames(yr_clu), round(yr_clu * 100, 2), check.names = FALSE),
          "paper_yearly_cluster.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("💾 paper_yearly_topic.csv, paper_yearly_cluster.csv\n")

# ── 4) 국가 × 클러스터 θ 점유율 (%) ────────────────────────────────────────
cc <- rowsum(theta, as.character(meta$Origin_Country))              # 국가 × 토픽 θ 합
cc <- t(rowsum(t(cc), tm$Cluster)); cc <- cc / rowSums(cc) * 100    # 국가 × 클러스터 %
RADAR_COUNTRIES <- c("US", "CN", "KR", "JP", "EP")                  # 원본 표·레이더 순서
country_cluster <- data.frame(Country = RADAR_COUNTRIES, round(cc[RADAR_COUNTRIES, names(cluster_colors_alpha)], 1))
write.csv(country_cluster, "paper_country_cluster.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("💾 paper_country_cluster.csv\n")

# ── 5) FREX / PROB 단어표 ──────────────────────────────────────────────────
lab15 <- labelTopics(final_stm_20_adj, n = 15)
write.csv(data.frame(Topic = 1:K_TOPICS, lab15$frex), "paper_frex15.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(Topic = 1:K_TOPICS, lab15$prob), "paper_prob15.csv", row.names = FALSE, fileEncoding = "UTF-8")
lab12 <- labelTopics(final_stm_20_adj, n = 12)
frex12 <- apply(lab12$frex, 1, paste, collapse = ", ")
prob12 <- apply(lab12$prob, 1, paste, collapse = ", ")
cat("💾 paper_frex15.csv, paper_prob15.csv\n")

# ── 6) 종합표 Impact 채우기 + 부록 A1/A3 ───────────────────────────────────
comb <- read.csv(FILE_COMBINED, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
comb$Impact <- round(impact[comb$Topic], 3)
write.csv(comb, FILE_COMBINED, row.names = FALSE, fileEncoding = "UTF-8")
cat("💾", FILE_COMBINED, "(Impact 채움)\n")
write.csv(comb %>% mutate(FREX = frex12[match(Topic, tm$Topic)]),
          "부록_A1_토픽종합표.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(Topic = tm$Topic, FREX = frex12, PROB = prob12),
          "부록_A3_PROB_FREX.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("💾 부록_A1_토픽종합표.csv, 부록_A3_PROB_FREX.csv\n")

# ==============================================================================
#  그림
# ==============================================================================
topic_tag <- paste0(tm$Topic, " ", tm$Label_KR, " [", tm$Cluster, "]")
clu_fill  <- scale_fill_manual(values = cluster_colors_alpha, name = "클러스터")
base_th   <- theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = .5), panel.grid.minor = element_blank())

# ── G1) 덴드로그램 (β 유클리드 + Ward.D2, 가로형) ──────────────────────────
B <- exp(final_stm_20_adj$beta$logbeta[[1]]); rownames(B) <- topic_tag
hc <- hclust(dist(B), method = "ward.D2")
png_dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else grDevices::png
png_dev("STM_Topic_Dendrogram.png", width = 4200, height = 3000, res = FIG_DPI, bg = FIG_BG)
par(mar = c(5, 2, 3, 16), cex = 1)
plot(as.dendrogram(hc), horiz = TRUE, xlab = "거리(Distance)",
     main = "토픽 간 단어 분포 계층적 군집 (Ward.D2)")
invisible(dev.off()); cat("💾 STM_Topic_Dendrogram.png\n")

# ── G2) 토픽 산점도: 예상 건수 vs Impact ───────────────────────────────────
dt <- data.frame(Topic = tm$Topic, Cluster = tm$Cluster, Count = count, Impact = impact)
p1 <- ggplot(dt, aes(Count, Impact)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  annotate("text", x = Inf, y = 1, label = "글로벌 평균 (Y=1.0)", hjust = 1.05, vjust = -.6,
           size = 3.2, color = "gray40") +
  geom_point(aes(fill = Cluster), shape = 21, size = 10, color = "white", stroke = .8, alpha = .95) +
  geom_text(aes(label = Topic), size = 3, fontface = "bold") +
  clu_fill + guides(fill = guide_legend(ncol = 2, override.aes = list(size = 4))) +
  labs(title = sprintf("토픽별 양적 규모 vs 질적 파급력 (N=%s)", format(N_DOCS, big.mark = ",")),
       x = "예상 특허 건수 (Expected Patent Count)", y = "가중 평균 피인용 지수 (Impact)") +
  base_th + theme(legend.position = c(.99, .01), legend.justification = c(1, 0),
                  legend.background = element_rect(color = "gray70"))
ggsave("STM_Topic_Scatter_Size_Impact.png", p1, width = 10, height = 7, dpi = FIG_DPI, bg = FIG_BG)
cat("💾 STM_Topic_Scatter_Size_Impact.png\n")

# ── G3) 클러스터 산점도 ────────────────────────────────────────────────────
dclu <- dt %>% group_by(Cluster) %>%
  summarise(Impact = sum(Impact * Count) / sum(Count), Count = sum(Count), .groups = "drop")
p2 <- ggplot(dclu, aes(Count, Impact)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_point(aes(fill = Cluster), shape = 21, size = 17, color = "white", stroke = 1, alpha = .95,
             show.legend = FALSE) +
  geom_text(aes(label = Cluster), size = 5.5, fontface = "bold") +
  clu_fill +
  scale_x_continuous(expand = expansion(mult = .05)) + scale_y_continuous(expand = expansion(mult = .06)) +
  labs(title = "8대 메가 클러스터 — 규모 vs 파급력", x = "예상 특허 건수 합계", y = "가중 평균 파급력") +
  base_th
ggsave("STM_Cluster_Scatter_Size_Impact.png", p2, width = 9, height = 6.5, dpi = FIG_DPI, bg = FIG_BG)
cat("💾 STM_Cluster_Scatter_Size_Impact.png\n")

# ── G4) 토픽 상관 네트워크 (노드 크기 = 비중, 색 = 클러스터) ───────────────
set.seed(SEED_NUM)
lay <- layout_with_fr(g)
nodes <- data.frame(Topic = V(g)$name, x = lay[, 1], y = lay[, 2]) %>%
  left_join(data.frame(Topic = tm$Topic, Cluster = tm$Cluster, Prev = prev), by = "Topic")
eseg <- edges %>%
  left_join(nodes %>% select(from = Topic, x, y), by = "from") %>%
  left_join(nodes %>% select(to = Topic, xend = x, yend = y), by = "to")
p3 <- ggplot() +
  geom_segment(data = eseg, aes(x, y, xend = xend, yend = yend, linewidth = weight), color = "gray60", alpha = .7) +
  geom_point(data = nodes, aes(x, y, size = Prev, fill = Cluster), shape = 21, color = "white", stroke = 1) +
  geom_text(data = nodes, aes(x, y, label = Topic), size = 3.2, fontface = "bold") +
  scale_linewidth(range = c(1, 2.6), guide = "none") +
  scale_size(range = c(9, 18), guide = "none") + clu_fill +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  coord_equal(clip = "off") +
  labs(title = sprintf("토픽 상관 네트워크 (%s) · 엣지 %d개, 고립 노드 %d개",
                       net_rule_lab, nrow(edges), n_iso)) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(hjust = .5, margin = margin(b = 10)), plot.margin = margin(15, 15, 15, 15))
ggsave("STM_Topic_Correlation_Network.png", p3, width = 10, height = 8, dpi = FIG_DPI, bg = FIG_BG)
cat("💾 STM_Topic_Correlation_Network.png\n")

# ── G5) 토픽 × 연도 평균 비중 히트맵 ───────────────────────────────────────
# 셀 숫자는 paper_yearly_topic.csv(소수 4자리) 값 기준 → 표와 그림 숫자 일치
dh <- as.data.frame(round(yr_topic, 4) * 100) %>% mutate(Topic = tm$Topic) %>%
  pivot_longer(-Topic, names_to = "Year", values_to = "Pct") %>%
  mutate(Topic = factor(Topic, levels = rev(tm$Topic)))
p4 <- ggplot(dh, aes(Year, Topic, fill = Pct)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f", Pct), color = Pct >= 9), size = 2.6, show.legend = FALSE) +
  scale_color_manual(values = c(`FALSE` = "gray15", `TRUE` = "white")) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "비중 (%)") +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = "토픽별 연도별 평균 비중 히트맵 (%)", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = .5), panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black"),
        legend.key.height = unit(2.5, "cm"))
ggsave("STM_All_Topics_Yearly_Heatmap.png", p4, width = 8.5, height = 9, dpi = FIG_DPI, bg = FIG_BG)
cat("💾 STM_All_Topics_Yearly_Heatmap.png\n")

# ── G6) 클러스터 연도별 비중 꺾은선 ────────────────────────────────────────
dl <- as.data.frame(yr_clu * 100) %>% mutate(Cluster = rownames(yr_clu)) %>%
  pivot_longer(-Cluster, names_to = "Year", values_to = "Pct") %>% mutate(Year = as.integer(Year))
p5 <- ggplot(dl, aes(Year, Pct, color = Cluster)) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.6) +
  geom_text(data = filter(dl, Year == max(YEAR_SEQ)), aes(label = Cluster), hjust = -.6,
            fontface = "bold", color = "gray25", size = 4, show.legend = FALSE) +
  scale_color_manual(values = cluster_colors_alpha, name = NULL) +
  scale_x_continuous(breaks = YEAR_SEQ, expand = expansion(add = c(.3, .4))) +
  guides(color = guide_legend(nrow = 2, byrow = FALSE)) +
  labs(title = "8대 클러스터 연도별 비중 추이", x = "연도", y = "비중 (%)") +
  base_th + theme(legend.position = c(.99, .99), legend.justification = c(1, 1),
                  legend.background = element_rect(color = "gray70"))
ggsave("STM_Cluster_Yearly_Lines.png", p5, width = 10, height = 6.5, dpi = FIG_DPI, bg = FIG_BG)
cat("💾 STM_Cluster_Yearly_Lines.png\n")

# ── G7) 국가별 클러스터 레이더 (직선 다각형 + 원형 격자) ───────────────────
axes  <- names(cluster_colors_alpha)
ang   <- setNames(2 * pi * (seq_along(axes) - 1) / length(axes), axes)   # A=0°, 반시계
rmax  <- ceiling(max(cc) / 5) * 5
rings <- seq(5, rmax, by = 5)
circ  <- expand.grid(r = rings, a = seq(0, 2 * pi, length.out = 181)) %>% mutate(x = r * cos(a), y = r * sin(a))
dr <- as.data.frame(cc[RADAR_COUNTRIES, axes]) %>% mutate(Country = RADAR_COUNTRIES) %>%
  pivot_longer(-Country, names_to = "Cluster", values_to = "Pct") %>%
  mutate(x = Pct * cos(ang[Cluster]), y = Pct * sin(ang[Cluster]),
         Country = factor(Country, levels = RADAR_COUNTRIES), o = match(Cluster, axes)) %>% arrange(Country, o)
dr_closed <- dr %>% group_by(Country) %>% group_modify(~ bind_rows(.x, .x[1, ])) %>% ungroup()
p6 <- ggplot() +
  geom_path(data = circ, aes(x, y, group = r), color = "gray75", linewidth = .4) +
  geom_segment(data = data.frame(a = ang), aes(x = 0, y = 0, xend = rmax * cos(a), yend = rmax * sin(a)),
               color = "gray75", linewidth = .4) +
  annotate("text", x = rings * cos(pi / 8), y = rings * sin(pi / 8), label = rings, size = 3, color = "gray30") +
  annotate("text", x = (rmax + 2.5) * cos(ang), y = (rmax + 2.5) * sin(ang), label = axes, size = 4.5) +
  geom_polygon(data = dr, aes(x, y, fill = Country, group = Country), alpha = .1, show.legend = FALSE) +
  geom_path(data = dr_closed, aes(x, y, color = Country, group = Country), linewidth = 1.2) +
  scale_color_manual(values = country_colors, name = NULL) +
  scale_fill_manual(values = country_colors) +
  coord_equal(clip = "off") +
  labs(title = "IP5 국가별 클러스터 포트폴리오 레이더 (θ 비중 %)") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(hjust = .5, margin = margin(b = 12)), legend.position = "right",
        plot.margin = margin(15, 15, 15, 15))
ggsave("STM_Country_Radar.png", p6, width = 8.5, height = 8.5, dpi = FIG_DPI, bg = FIG_BG)
cat("💾 STM_Country_Radar.png\n")

# ── G8) [부록] 20토픽 연도별 평균 θ 꺾은선 (패싯) ──────────────────────────
dfa <- as.data.frame(yr_topic * 100) %>% mutate(Topic = tm$Topic, Cluster = tm$Cluster, Tag = topic_tag) %>%
  pivot_longer(as.character(YEAR_SEQ), names_to = "Year", values_to = "Pct") %>%
  mutate(Year = as.integer(Year), Tag = factor(Tag, levels = topic_tag))
p7 <- ggplot(dfa, aes(Year, Pct)) +
  geom_line(aes(color = Cluster), linewidth = 1.1) +
  geom_point(aes(fill = Cluster), shape = 21, color = "gray35", size = 1.9, stroke = .3) +
  facet_wrap(~Tag, ncol = 4, scales = "free_y") +
  scale_color_manual(values = cluster_colors_alpha, guide = "none") +
  scale_fill_manual(values = cluster_colors_alpha, name = "메가 클러스터",
                    labels = paste0(names(cluster_names_kr), ". ", cluster_names_kr)) +
  scale_x_continuous(breaks = YEAR_SEQ, labels = function(x) sprintf("'%02d", x %% 100)) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 4))) +
  labs(title = sprintf("토픽별 연도별 평균 비중 (θ 평균, %d–%d)", min(YEAR_SEQ), max(YEAR_SEQ)),
       subtitle = sprintf("N=%s, 연도 = Publication Year, 패널별 Y축 독립", format(sum(py %in% YEAR_SEQ), big.mark = ",")),
       x = "연도", y = "평균 비중 (%)") +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = .5), plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 9), strip.background = element_rect(fill = "gray95"),
        panel.grid.minor = element_blank(), legend.position = "bottom")
ggsave("STM_Topic_Yearly_Lines_20.png", p7, width = 13, height = 13, dpi = FIG_DPI, bg = FIG_BG)
cat("💾 STM_Topic_Yearly_Lines_20.png\n")

cat("\n✅ 10 완료\n")
