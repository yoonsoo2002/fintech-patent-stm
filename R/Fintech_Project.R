# =====================================================================
# [Phase 0] K-핀테크 전략 도출을 위한 STM 기반 모델 구축 통합 스크립트
# (새로운 컴퓨터 경로 반영 버전)
# =====================================================================

# 1. 필수 패키지 설치 (최초 1회만 주석 해제하여 실행)
install.packages(c("stm", "stringr", "stopwords", "dplyr", "tidyr"))

# 2. 패키지 로드
library(stm)
library(stringr)
library(stopwords)
library(dplyr)
library(tidyr)

# 3. 작업 폴더(베이스캠프) 설정 및 데이터 불러오기
# C드라이브에 만든 Fintech_Project 폴더를 전체 작업 공간으로 지정합니다.
setwd(here::here("data"))  # was: setwd("<project-root>/data")

# 파일이 같은 폴더에 있으므로 이름만 적어주면 됩니다.
file_path <- "final_fintech_patents_filtered.csv"
mydata5 <- read.csv(file_path, fileEncoding="UTF-8", stringsAsFactors=FALSE)

# 4. 텍스트 결합 (Title 가중치 2배 적용)
mydata5$text_tt <- paste(mydata5$Title, mydata5$Title, mydata5$Abstract, sep=" ")

# 5. 궁극의 특허 불용어 사전 구축 (SMART + Custom + 숫자)
stwds <- stopwords(language = "en", source = "smart")
custom_ultimate <- c(
  'comprise', 'comprises', 'comprised', 'comprising', 'compris',
  'include', 'includes', 'included', 'including', 'inclusion', 'inclusions', 'includ',
  'provide', 'provides', 'provided', 'providing', 'provision', 'provisions', 'provid',
  'configure', 'configures', 'configured', 'configuring', 'configuration', 'configurations', 'configur',
  'contain', 'contains', 'contained', 'containing', 'container', 'containers',
  'perform', 'performs', 'performed', 'performing', 'performance', 'performances',
  'generate', 'generates', 'generated', 'generating', 'generation', 'generations', 'generator', 'generators', 'generat',
  'determine', 'determines', 'determined', 'determining', 'determination', 'determinations', 'determin',
  'obtain', 'obtains', 'obtained', 'obtaining', 'obtainable',
  'receive', 'receives', 'received', 'receiving', 'receiver', 'receivers', 'receipt', 'receipts', 'receiv',
  'send', 'sends', 'sent', 'sending', 'sender', 'senders',
  'transmit', 'transmits', 'transmitted', 'transmitting', 'transmission', 'transmissions', 'transmitter', 'transmitters',
  'store', 'stores', 'stored', 'storing', 'storage', 'storages',
  'process', 'processes', 'processed', 'processing', 'processor', 'processors',
  'execute', 'executes', 'executed', 'executing', 'execution', 'executions', 'executable', 'execut',
  'calculate', 'calculates', 'calculated', 'calculating', 'calculation', 'calculations', 'calculator', 'calculators', 'calcul',
  'apply', 'applies', 'applied', 'applying', 'application', 'applications', 'applicable',
  'relate', 'relates', 'related', 'relating', 'relation', 'relations', 'relative', 'relatively', 'relat',
  'base', 'bases', 'based', 'basing', 'basis',
  'present', 'presents', 'presented', 'presenting', 'presentation', 'presentations', 'presently',
  'disclose', 'discloses', 'disclosed', 'disclosing', 'disclosure', 'disclosures', 'disclos',
  'step', 'steps', 'stepped', 'stepping',
  'mean', 'means', 'meaning', 'meanings',
  'draw', 'draws', 'drew', 'drawn', 'drawing', 'drawings',
  'figure', 'figures', 'figured', 'figuring', 'figur',
  'part', 'parts', 'parted', 'parting', 'partly',
  'piece', 'pieces', 'pieced', 'piecing', 'piec',
  'field', 'fields',
  'technique', 'techniques', 'technical', 'technically', 'technic',
  'background', 'backgrounds',
  'result', 'results', 'resulted', 'resulting',
  'require', 'requires', 'required', 'requiring', 'requirement', 'requirements', 'requir',
  'prefer', 'prefers', 'preferred', 'preferring', 'preference', 'preferences', 'preferably',
  'medium', 'mediums', 'media',
  'invention', 'thereof', 'therefore', 'therefrom', 'system', 'systems', 'method', 'methods', 'apparatus', 'plurality', 'embodiment', 'embodiments',
  "10", "10s", "100", "100s", "200", "200s"
)
csw_final_new <- c(stwds, custom_ultimate)

# 6. 텍스트 전처리 (textProcessor) - 시간이 다소 소요됩니다.
proc3 <- textProcessor(documents = mydata5$text_tt, 
                       metadata = mydata5, 
                       lowercase = TRUE, 
                       removepunctuation = TRUE, 
                       customstopwords = csw_final_new, 
                       removestopwords = TRUE, 
                       removenumbers = FALSE, 
                       stem = TRUE, 
                       wordLengths = c(2,Inf))

# 7. 1% 희귀 단어 제거 (prepDocuments)
out3 <- prepDocuments(proc3$documents, proc3$vocab, proc3$meta, lower.thresh = 214)
print(paste("🔥 최종 살아남은 단어 개수:", length(out3$vocab)))

# 8. 핵심 작업: 질적 파급력 분석을 위한 '조정된 피인용수(C_adj_i)' 계산
# (출원 연도별 평균 피인용수로 나누어 시간 편향 제거)
out3$meta$C_avg_j <- ave(out3$meta$Cited.by.Patent.Count, 
                         out3$meta$Publication.Year, 
                         FUN = function(x) mean(x, na.rm = TRUE))

out3$meta$C_adj_i <- ifelse(out3$meta$C_avg_j == 0, 
                            0, 
                            out3$meta$Cited.by.Patent.Count / out3$meta$C_avg_j)

# 9. 대망의 최종 모델 K=20 정식 학습 (시간 소요)
set.seed(2026) 
final_stm_20_adj <- stm(documents = out3$documents, 
                        vocab = out3$vocab, 
                        K = 20, 
                        prevalence = ~ Origin_Country + s(Publication.Year) + C_adj_i, 
                        data = out3$meta,
                        max.em.its = 150, 
                        init.type = "Spectral")

# 10. 이후 분석을 위해 작업 공간 영구 보존
save.image("STM_full_workspace_Phase0.RData")
print("💾 [성공] 전처리 및 K=20 최종 모델 백업 완료! 다음 분석 준비 끝!")

# ======================================================================
# ======================================================================

# =====================================================================
# [Phase 1-1] 국가별 동향 분석: 미국(US) 기준 통계 검정 결과 CSV 추출
# =====================================================================

# 1. 패키지 로드 (이미 로드되어 있다면 생략 가능)
library(stm)
library(dplyr)

# 2. Origin_Country 변수를 팩터(Factor)로 변환하고 기준점(Reference)을 "US"로 설정
out3$meta$Origin_Country <- as.factor(out3$meta$Origin_Country)
out3$meta$Origin_Country <- relevel(out3$meta$Origin_Country, ref = "US")

# 3. 미국 기준으로 estimateEffect 실행 (시간이 약간 소요될 수 있습니다)
# (공식은 처음에 모델을 학습할 때 사용했던 것과 완벽히 동일하게 맞춥니다)
set.seed(2026)
effect_country_US <- estimateEffect(
  1:20 ~ Origin_Country + s(Publication.Year) + C_adj_i,
  stmobj = final_stm_20_adj,
  metadata = out3$meta,
  uncertainty = "Global"
)

# 4. 20개 토픽의 짧은 라벨 준비 (결과 표의 가독성을 위해)
topic_label_short <- c(
  "Crypto Exchange", "Mobile POS Pay", "Insurance Claim", "Invoice Audit",
  "Bank Remittance", "Digital Wallet", "Auth & Biometrics", "NFT Rights",
  "Investment Reco", "Settlement Proc", "Enterprise Finance", "Asset Ledger",
  "Credit Risk", "Smart Contract", "Realtime Settle", "E-commerce Fee",
  "AI Detection", "Device Linking", "Charging Txn", "Face/Voice Login"
)

# 5. 각 토픽별 통계 요약 결과를 추출하여 하나의 데이터프레임으로 묶기
sum_effect <- summary(effect_country_US)
result_list <- list()

for (i in 1:20) {
  # i번째 토픽의 회귀계수 표 추출
  coef_table <- as.data.frame(sum_effect$tables[[i]])
  coef_table$Term <- rownames(coef_table)
  coef_table$Topic <- i
  coef_table$Topic_Label <- topic_label_short[i]
  
  # '국가' 비교 항목만 필터링 (Origin_Country 글자가 포함된 행만 추출)
  country_coefs <- coef_table[grep("Origin_Country", coef_table$Term), ]
  
  # Term 이름 깔끔하게 정리 (예: "Origin_CountryKR" -> "KR vs US")
  country_coefs$Term <- gsub("Origin_Country", "", country_coefs$Term)
  country_coefs$Term <- paste0(country_coefs$Term, " vs US")
  
  # 리스트에 저장
  result_list[[i]] <- country_coefs
}

# 6. 리스트를 하나의 큰 표(Data Frame)로 병합
final_result_df <- do.call(rbind, result_list)

# 7. 컬럼 순서 및 이름 예쁘게 정리
final_result_df <- final_result_df[, c("Topic", "Topic_Label", "Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
colnames(final_result_df) <- c("Topic", "Topic_Label", "Comparison", "Estimate", "Std_Error", "t_value", "p_value")

# 8. 논문용 유의성 별표(Star) 추가 (직관적인 확인용)
final_result_df$Significance <- ifelse(final_result_df$p_value < 0.001, "***",
                                       ifelse(final_result_df$p_value < 0.01, "**",
                                              ifelse(final_result_df$p_value < 0.05, "*",
                                                     ifelse(final_result_df$p_value < 0.1, ".", ""))))

# 9. CSV 파일로 저장
write.csv(final_result_df, "Topic_Country_Effect_US_Baseline.csv", row.names = FALSE)
print("✅ [성공] 미국(US) 기준 국가별 통계 검정 결과가 'Topic_Country_Effect_US_Baseline.csv'로 저장되었습니다!")

# ===================================================================================
# ===================================================================================

# 1. 레이더 차트 전용 패키지 설치 및 로드
install.packages("fmsb")
library(fmsb)
library(dplyr)

# 2. 데이터 준비 (평균 비중 추출)
theta_df <- as.data.frame(final_stm_20_adj$theta)
colnames(theta_df) <- paste0('T', 1:20)
theta_df$Origin_Country <- as.character(out3$meta$Origin_Country)

df_wide <- theta_df %>%
  filter(Origin_Country %in% c('US', 'KR', 'CN', 'JP', 'EP')) %>%
  group_by(Origin_Country) %>%
  summarise(across(starts_with('T'), ~ mean(.x, na.rm = TRUE)))

# 3. 클러스터 순서대로 열(Column) 재정렬
cluster_ordering <- c('T4', 'T11', 'T3', 'T18', 'T13', 'T9', 'T17',
                      'T14', 'T19', 'T1', 'T8', 'T6', 'T12',
                      'T7', 'T15', 'T5', 'T10', 'T16', 'T2', 'T20')
df_wide <- df_wide %>% select(Origin_Country, all_of(cluster_ordering))

# 4. fmsb 패키지용 데이터 변환 (1행:최대값, 2행:최소값, 3행부터 데이터)
rownames(df_wide) <- df_wide$Origin_Country
df_wide$Origin_Country <- NULL

# 전체 데이터 중 가장 큰 값(일본의 최고점 등)을 찾아 최대값으로 자동 설정 (여유있게 1.1배)
max_val <- max(df_wide) * 1.1 
df_radar_fmsb <- rbind(rep(max_val, 20), rep(0, 20), df_wide)

# 5. 차트 그리기 및 저장 (화면을 2줄 3칸으로 분할)
png("STM_Radar_fmsb_Perfect.png", width = 1800, height = 1200, res = 150)
par(mfrow = c(2, 3), mar = c(2, 2, 3, 2)) # 2x3 배열

colors <- c("red", "blue", "green", "purple", "orange")

for(i in 1:5) {
  # 개별 국가별로 레이더 차트 생성
  radarchart(df_radar_fmsb[c(1, 2, i + 2), ], 
             axistype = 1,
             pcol = colors[i], pfcol = scales::alpha(colors[i], 0.4), plwd = 2, 
             cglcol = "grey", cglty = 1, axislabcol = "grey", 
             caxislabels = seq(0, round(max_val*100), length.out=5), # 0~최대 % 자동 눈금
             vlcex = 1.2, # 라벨 크기
             title = rownames(df_radar_fmsb)[i + 2])
}
dev.off()
print("✅ fmsb 레이더 차트 저장 완료 (STM_Radar_fmsb_Perfect.png)")

#=====================================================
#====================================================
library(tidyverse)
library(stm)
library(scales)

# 1. 데이터 추출
theta_df <- as.data.frame(final_stm_20_adj$theta)
colnames(theta_df) <- paste0('T', 1:20)
theta_df$Origin_Country <- as.character(out3$meta$Origin_Country)

df_bar <- theta_df %>%
  filter(Origin_Country %in% c('US', 'KR', 'CN', 'JP', 'EP')) %>%
  group_by(Origin_Country) %>%
  summarise(across(starts_with('T'), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = starts_with('T'), names_to = 'Topic', values_to = 'Proportion')

# 2. 토픽별 클러스터 매핑
cluster_mapping <- data.frame(
  Topic = c('T4', 'T11', 'T3', 'T18', 'T13', 'T9', 'T17', 'T14', 'T19', 'T1', 'T8', 'T6', 'T12', 'T7', 'T15', 'T5', 'T10', 'T16', 'T2', 'T20'),
  Cluster = c(rep('1. Ops & Audit', 2), rep('2. Decision & Risk', 5), rep('3. Digital Infra', 6), rep('4. Compliance', 2), rep('5. Payment', 5))
)

df_bar <- df_bar %>% left_join(cluster_mapping, by = "Topic")

# 토픽 순서 고정
df_bar$Topic <- factor(df_bar$Topic, levels = cluster_mapping$Topic)

# 3. 막대 그래프 그리기 (ggplot2)
p_bar <- ggplot(df_bar, aes(x = Topic, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2) +
  facet_wrap(~ Origin_Country, ncol = 3, scales = "free_y") + # 국가별로 높이 자동 조절! (일본도 완벽표현)
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 12) +
  labs(title = 'K-FinTech Patent Portfolio by Country (IP5)',
       subtitle = 'Topic Proportions Grouped by Functional Clusters',
       x = "Topics", y = "Expected Proportion") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size=9),
    strip.background = element_rect(fill = "gray90", color = "white"),
    strip.text = element_text(face = 'bold', size = 14),
    legend.position = "bottom"
  )

print(p_bar)
ggsave('STM_Country_BarChart_Clusters.png', plot = p_bar, width = 16, height = 10, dpi = 300)
print("✅ 클러스터 막대 그래프 저장 완료 (STM_Country_BarChart_Clusters.png)")

#===========================================================================
#-========================================================================

# =====================================================================
# [Phase 2-1] 질적 파급력 분석: 질적 거품(Qualitative Bubble) 매트릭스
# =====================================================================

# 1. 필수 패키지 로드 (ggrepel이 없다면 설치 주석 해제)
install.packages('ggrepel')
library(tidyverse)
library(stm)
library(scales)
library(ggrepel) # 텍스트 안 겹치게 해주는 마법의 패키지

# 2. 데이터 준비: 토픽 비중(Theta)과 피인용수(C_adj_i) 결합
theta_df <- as.data.frame(final_stm_20_adj$theta)
colnames(theta_df) <- paste0('T', 1:20)
theta_df$Origin_Country <- as.character(out3$meta$Origin_Country)
theta_df$C_adj_i <- out3$meta$C_adj_i

# IP5 국가만 필터링
df_filtered <- theta_df %>% filter(Origin_Country %in% c('US', 'KR', 'CN', 'JP', 'EP'))

# 3. High-Impact (상위 10% 슈퍼 특허) 임계값 설정
# C_adj_i의 상위 10% 커트라인(90백분위수)을 구합니다.
threshold_90 <- quantile(df_filtered$C_adj_i, 0.90, na.rm = TRUE)

# 슈퍼 특허 여부 마킹
df_filtered <- df_filtered %>%
  mutate(Is_High_Impact = ifelse(C_adj_i >= threshold_90, TRUE, FALSE))

# 4. 분석을 위해 데이터를 세로형(Long format)으로 변환
df_long <- df_filtered %>%
  pivot_longer(cols = starts_with('T'), names_to = 'Topic', values_to = 'Theta')

# 5. [양적 점유율(X축)] 계산: 전 세계 해당 토픽 중 이 국가의 비중은?
quant_share <- df_long %>%
  group_by(Topic, Origin_Country) %>%
  summarise(Total_Theta = sum(Theta, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Topic) %>%
  mutate(Quant_Share = Total_Theta / sum(Total_Theta)) %>%
  ungroup()

# 6. [질적 점유율(Y축)] 계산: 상위 10% 특허 중 이 국가의 비중은?
qual_share <- df_long %>%
  filter(Is_High_Impact == TRUE) %>%
  group_by(Topic, Origin_Country) %>%
  summarise(HighImpact_Theta = sum(Theta, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Topic) %>%
  mutate(Qual_Share = HighImpact_Theta / sum(HighImpact_Theta)) %>%
  ungroup()

# 7. 양적/질적 데이터 병합 및 거품 지수(Bubble Index) 계산
bubble_df <- quant_share %>%
  left_join(qual_share %>% select(Topic, Origin_Country, Qual_Share), by = c('Topic', 'Origin_Country')) %>%
  mutate(Qual_Share = replace_na(Qual_Share, 0)) %>% # High Impact가 아예 없으면 0% 처리
  mutate(
    # 질적 점유율이 양적 점유율보다 낮으면 거품(Bubble), 높으면 핵심역량(Core)
    Status = case_when(
      Qual_Share >= Quant_Share ~ 'Core Competence (Quality >= Quantity)',
      Qual_Share < Quant_Share ~ 'Qualitative Bubble (Quantity > Quality)'
    )
  )

# 그래프 그리기 순서 꼬임 방지를 위한 팩터화
bubble_df$Status <- factor(bubble_df$Status, levels = c('Core Competence (Quality >= Quantity)', 'Qualitative Bubble (Quantity > Quality)'))

# 차트 최대/최소값 계산 (정사각형 그래프를 위해 동일하게 맞춤)
max_axis <- max(max(bubble_df$Quant_Share), max(bubble_df$Qual_Share)) * 1.05

# 8. 대망의 2x2 매트릭스 산점도 그리기
p_bubble <- ggplot(bubble_df, aes(x = Quant_Share, y = Qual_Share, color = Status)) +
  # 45도 기준선 (y=x)
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'gray40', size = 0.8) +
  
  # 산점도 점 찍기
  geom_point(size = 3.5, alpha = 0.8) +
  
  # 텍스트 라벨 (T1, T2.. 가 서로 겹치지 않게 밀어냄)
  geom_text_repel(aes(label = Topic), size = 3.5, fontface = 'bold', show.legend = FALSE, max.overlaps = 20) +
  
  # 축 스케일 동일하게 설정 (정사각형 유지)
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, max_axis)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max_axis)) +
  
  # 파란색(핵심역량), 빨간색(질적거품) 색상 지정
  scale_color_manual(values = c('Core Competence (Quality >= Quantity)' = '#2E86C1', 
                                'Qualitative Bubble (Quantity > Quality)' = '#E74C3C')) +
  
  # 국가별로 바둑판 배열
  facet_wrap(~ Origin_Country, ncol = 3) +
  
  # 디자인 및 테마
  theme_bw(base_size = 12) +
  labs(title = 'Qualitative Bubble Matrix by Country (IP5)',
       subtitle = 'Comparing Global Quantity Share (X) vs. High-Impact Top 10% Quality Share (Y)',
       x = 'Quantitative Share (Total Patents %)',
       y = 'Qualitative Share (Top 10% High-Impact Patents %)',
       color = 'Technology Status') +
  theme(
    plot.title = element_text(face = 'bold', size = 16),
    plot.subtitle = element_text(size = 12, color = 'gray30'),
    strip.background = element_rect(fill = 'gray90'),
    strip.text = element_text(face = 'bold', size = 13),
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold'),
    panel.grid.minor = element_blank()
  )

# 9. 화면 출력 및 고해상도 이미지 저장
print(p_bubble)
ggsave('STM_Qualitative_Bubble_Matrix.png', plot = p_bubble, width = 16, height = 10, dpi = 300)
print('✅ 질적 거품 걷어내기 매트릭스 완료! (STM_Qualitative_Bubble_Matrix.png)')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# =====================================================================
# [Phase 2-1] 질적 파급력 분석: 글로벌 vs 한국 질적 갭(Gap) 덤벨 차트
# =====================================================================

# 1. 필수 패키지 로드
library(tidyverse)
library(stm)

# 2. 데이터 준비 (Theta 비중 행렬, 조정된 피인용수, 국가 정보 추출)
theta_mat <- final_stm_20_adj$theta
c_adj <- out3$meta$C_adj_i
country <- as.character(out3$meta$Origin_Country)

# 피인용수가 결측치(NA)인 혹시 모를 데이터를 안전하게 제외
valid_idx <- !is.na(c_adj)
theta_mat <- theta_mat[valid_idx, ]
c_adj <- c_adj[valid_idx]
country <- country[valid_idx]

# 3. 그룹 인덱스 설정
# 글로벌(Global): 한국을 포함한 IP5 전체 데이터
global_idx <- 1:length(country)
# 한국(KR): 한국 출원 데이터만 추출
kr_idx <- which(country == 'KR')

# 4. ⭐ 토픽별 가중 평균 파급력(Weighted Impact) 계산
# 공식: sum(특허의 토픽 비중 * 특허의 피인용수) / sum(특허의 토픽 비중)
global_impact <- colSums(theta_mat[global_idx, ] * c_adj[global_idx]) / colSums(theta_mat[global_idx, ])
kr_impact <- colSums(theta_mat[kr_idx, ] * c_adj[kr_idx]) / colSums(theta_mat[kr_idx, ])

# 5. 시각화용 데이터프레임 구축
topic_label_short <- c(
  'Crypto Exchange', 'Mobile POS Pay', 'Insurance Claim', 'Invoice Audit',
  'Bank Remittance', 'Digital Wallet', 'Auth & Biometrics', 'NFT Rights',
  'Investment Reco', 'Settlement Proc', 'Enterprise Finance', 'Asset Ledger',
  'Credit Risk', 'Smart Contract', 'Realtime Settle', 'E-commerce Fee',
  'AI Detection', 'Device Linking', 'Charging Txn', 'Face/Voice Login'
)

df_dumbbell <- data.frame(
  Topic_Num = 1:20,
  Label = topic_label_short,
  Global = global_impact,
  KR = kr_impact
)

# 6. 축 라벨 생성 및 '글로벌 파급력(Global)' 기준으로 내림차순 정렬
df_dumbbell <- df_dumbbell %>%
  mutate(
    Axis_Label = paste0('T', Topic_Num, ': ', Label),
    Gap = Global - KR # 격차 계산 (참고용)
  ) %>%
  arrange(Global) %>% # 그래프에서 1등이 맨 위로 가도록 정렬
  mutate(Axis_Label = factor(Axis_Label, levels = Axis_Label))

# 7. 대망의 덤벨 차트 그리기 (ggplot2)
p_dumb <- ggplot(df_dumbbell) +
  # 아령의 손잡이(두 점을 잇는 선)
  geom_segment(aes(x = Axis_Label, xend = Axis_Label, y = Global, yend = KR), 
               color = 'gray75', linewidth = 1.5) +
  
  # 글로벌 평균 점 (회색)
  geom_point(aes(x = Axis_Label, y = Global, color = 'Global Average'), size = 4.5) +
  
  # 한국 평균 점 (보라색)
  geom_point(aes(x = Axis_Label, y = KR, color = 'South Korea (KR)'), size = 4.5) +
  
  # 가로형 차트로 뒤집기 (토픽이 Y축으로 가도록)
  coord_flip() +
  
  # 색상 수동 지정 (회색 vs 보라색의 강렬한 대비)
  scale_color_manual(values = c('Global Average' = '#95A5A6', 'South Korea (KR)' = '#8E44AD')) +
  
  # 디자인 테마 설정
  theme_minimal(base_size = 12) +
  labs(
    title = 'Qualitative Impact Gap: Global vs. South Korea',
    subtitle = 'Weighted Average of Time-Normalized Citation Index by Topic',
    x = 'Topics (Ordered by Global Impact)',
    y = 'Qualitative Impact Score (Weighted C_adj_i)',
    color = 'Group'
  ) +
  theme(
    plot.title = element_text(face = 'bold', size = 16),
    plot.subtitle = element_text(size = 12, color = 'gray40', margin = margin(b = 15)),
    axis.text.y = element_text(face = 'bold', size = 11, color = 'black'),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(), # 가로줄 제거하여 깔끔하게
    panel.grid.minor.x = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold'),
    plot.margin = margin(20, 20, 20, 20)
  )

# 8. 출력 및 고해상도 이미지 저장
print(p_dumb)
ggsave('STM_Qualitative_Impact_Dumbbell.png', plot = p_dumb, width = 12, height = 9, dpi = 300)
print('✅ 글로벌 vs 한국 덤벨 차트 완료! (STM_Qualitative_Impact_Dumbbell.png)')

# =============================================================================
# =============================================================================

# =====================================================================
# [Phase 6-1] 융합 생태계 네트워크: Global 허브 vs. KR 허브 비교
# =====================================================================

# 1. 필수 패키지 설치 (최초 1회만 주석 해제하여 실행하세요)
install.packages('igraph')
install.packages('tidygraph')
install.packages('ggraph')
install.packages('patchwork') # 두 그래프를 예쁘게 붙여주는 패키지

# 패키지 로드
library(tidyverse)
library(stm)
library(igraph)
library(tidygraph)
library(ggraph)
library(patchwork)

# 2. 데이터 준비: Theta 매트릭스와 국가 변수
theta_mat <- final_stm_20_adj$theta
colnames(theta_mat) <- paste0('T', 1:20)
country <- as.character(out3$meta$Origin_Country)

# 3. 글로벌(IP5) 및 한국(KR) 부분집합 분리
ip5_countries <- c('US', 'KR', 'CN', 'JP', 'EP')
theta_global <- theta_mat[country %in% ip5_countries, ]
theta_kr <- theta_mat[country == 'KR', ]

# 4. 노드(Node) 메타데이터 생성: 클러스터 정보 및 토픽 비중(크기)
cluster_dict <- c(
  'T4'='1. Ops & Audit', 'T11'='1. Ops & Audit',
  'T3'='2. Decision & Risk', 'T18'='2. Decision & Risk', 'T13'='2. Decision & Risk', 'T9'='2. Decision & Risk', 'T17'='2. Decision & Risk',
  'T14'='3. Digital Infra', 'T19'='3. Digital Infra', 'T1'='3. Digital Infra', 'T8'='3. Digital Infra', 'T6'='3. Digital Infra', 'T12'='3. Digital Infra',
  'T7'='4. Compliance', 'T15'='4. Compliance',
  'T5'='5. Payment', 'T10'='5. Payment', 'T16'='5. Payment', 'T2'='5. Payment', 'T20'='5. Payment'
)

node_info_global <- data.frame(
  name = paste0('T', 1:20),
  Cluster = cluster_dict[paste0('T', 1:20)],
  Size = colMeans(theta_global, na.rm = TRUE) # 노드 크기 = 글로벌 평균 비중
)

node_info_kr <- data.frame(
  name = paste0('T', 1:20),
  Cluster = cluster_dict[paste0('T', 1:20)],
  Size = colMeans(theta_kr, na.rm = TRUE) # 노드 크기 = 한국 평균 비중
)

# 5. 상관관계 기반 엣지(Edge) 추출 함수
get_edges <- function(theta_data, threshold = 0.05) {
  # 피어슨 상관계수 행렬 계산
  cor_mat <- cor(theta_data, use = 'pairwise.complete.obs')
  
  # 대각선(자기자신) 및 중복되는 상삼각행렬 0으로 처리
  cor_mat[upper.tri(cor_mat, diag = TRUE)] <- 0
  
  # 데이터프레임으로 변환 후 임계값(Threshold) 이상의 양(+)의 상관관계만 추출
  edges <- as.data.frame(as.table(cor_mat)) %>%
    filter(Freq > threshold) %>%
    rename(from = Var1, to = Var2, weight = Freq)
  return(edges)
}

# ⭐ 네트워크의 복잡도를 조절하는 변수 (중요!)
# 만약 선이 너무 많아 새까맣다면 0.1 등으로 올리시고, 선이 너무 끊어져 있으면 0.03 정도로 낮추세요.
edge_threshold <- 0.06 

edges_global <- get_edges(theta_global, threshold = edge_threshold)
edges_kr <- get_edges(theta_kr, threshold = edge_threshold)

# 6. tidygraph 객체 생성
g_global <- tbl_graph(nodes = node_info_global, edges = edges_global, directed = FALSE)
g_kr <- tbl_graph(nodes = node_info_kr, edges = edges_kr, directed = FALSE)

# 7. 네트워크 시각화 템플릿 함수
plot_network <- function(g_obj, title_text) {
  # 동일한 레이아웃 형태를 유지하기 위해 시드 고정
  set.seed(2026) 
  
  ggraph(g_obj, layout = 'fr') + # Fruchterman-Reingold (허브를 중앙으로 모으는 알고리즘)
    # 연결선 (상관관계가 높을수록 굵고 진하게)
    geom_edge_link(aes(edge_alpha = weight, edge_width = weight), color = 'gray60', show.legend = FALSE) +
    
    # 노드 (토픽 비중이 클수록 크게)
    geom_node_point(aes(size = Size, color = Cluster), alpha = 0.85) +
    
    # 라벨 (T1, T2 등이 노드 위에 안 겹치게)
    geom_node_text(aes(label = name), fontface = 'bold', size = 4.5, repel = TRUE) +
    
    # 스케일 조절
    scale_size_continuous(range = c(4, 16), guide = 'none') +
    scale_edge_width_continuous(range = c(0.5, 2.5)) +
    scale_color_brewer(palette = 'Set1') +
    
    # 테마 설정
    theme_void(base_size = 14) +
    labs(title = title_text, color = 'Functional Clusters') +
    theme(
      plot.title = element_text(face = 'bold', hjust = 0.5, size = 16, margin = margin(b=15)),
      legend.position = 'bottom',
      legend.title = element_text(face = 'bold')
    )
}

# 8. 개별 그래프 생성
p_global <- plot_network(g_global, 'Global Convergence Network (IP5)')
p_kr <- plot_network(g_kr, 'South Korea (KR) Convergence Network')

# 9. 두 그래프를 나란히 이어붙이기 (patchwork의 마법)
final_plot <- p_global + p_kr + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', plot.margin = margin(20,20,20,20))

# 10. 고해상도 이미지 저장
print(final_plot)
ggsave('STM_Convergence_Network_Global_vs_KR.png', final_plot, width = 18, height = 9, dpi = 300)
print('✅ [Phase 6-1] 글로벌 vs 한국 융합 생태계 네트워크 지도 완료!')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++=====================================================

# =====================================================================
# [Phase 6-2] 차세대 킬러 기술 도출: Breakthrough 매트릭스 (Global vs KR)
# =====================================================================

# 1. 필수 패키지 로드
library(tidyverse)
library(stm)
library(ggrepel)

# 2. 데이터 준비: Theta 비중, 피인용수, 국가, 그리고 '출원 연도'
theta_mat <- final_stm_20_adj$theta
colnames(theta_mat) <- paste0('T', 1:20)
c_adj <- out3$meta$C_adj_i
country <- as.character(out3$meta$Origin_Country)
# 연도를 숫자형으로 변환 (회귀 기울기 계산용)
pub_year <- as.numeric(as.character(out3$meta$Publication.Year)) 

# 결측치(NA) 안전 제거
valid_idx <- !is.na(c_adj) & !is.na(pub_year)
theta_mat <- theta_mat[valid_idx, ]
c_adj <- c_adj[valid_idx]
country <- country[valid_idx]
pub_year <- pub_year[valid_idx]

# 3. 글로벌(IP5) 및 한국(KR) 인덱스 분리
ip5_countries <- c('US', 'KR', 'CN', 'JP', 'EP')
global_idx <- which(country %in% ip5_countries)
kr_idx <- which(country == 'KR')

# 4. Y축 [Impact]: 토픽별 가중 파급력 계산 (Phase 2-1과 동일 로직)
global_impact <- colSums(theta_mat[global_idx, ] * c_adj[global_idx]) / colSums(theta_mat[global_idx, ])
kr_impact <- colSums(theta_mat[kr_idx, ] * c_adj[kr_idx]) / colSums(theta_mat[kr_idx, ])

# 5. X축 [Momentum]: 토픽별 연도에 따른 성장 기울기(Slope) 계산
# lm(theta ~ year)의 기울기를 구하여 성장성을 측정합니다. (보기 쉽게 100을 곱함)
global_growth <- sapply(1:20, function(i) coef(lm(theta_mat[global_idx, i] ~ pub_year[global_idx]))[2]) * 100
kr_growth <- sapply(1:20, function(i) coef(lm(theta_mat[kr_idx, i] ~ pub_year[kr_idx]))[2]) * 100

# 6. 매트릭스 시각화용 통합 데이터프레임 구축
topic_label_short <- c(
  'Crypto Exchange', 'Mobile POS Pay', 'Insurance Claim', 'Invoice Audit',
  'Bank Remittance', 'Digital Wallet', 'Auth & Biometrics', 'NFT Rights',
  'Investment Reco', 'Settlement Proc', 'Enterprise Finance', 'Asset Ledger',
  'Credit Risk', 'Smart Contract', 'Realtime Settle', 'E-commerce Fee',
  'AI Detection', 'Device Linking', 'Charging Txn', 'Face/Voice Login'
)

df_matrix <- data.frame(
  Topic = paste0('T', 1:20),
  Label = topic_label_short,
  Global_X = global_growth,
  Global_Y = global_impact,
  KR_X = kr_growth,
  KR_Y = kr_impact
) %>%
  mutate(Full_Label = paste0(Topic, ": ", Label))

# 7. 사분면 기준선: 글로벌의 중앙값(Median)
mid_x <- median(df_matrix$Global_X)
mid_y <- median(df_matrix$Global_Y)

# 8. 축 범위 동적 계산 (그래프 여백 확보)
min_x <- min(c(df_matrix$Global_X, df_matrix$KR_X)) * 1.1
max_x <- max(c(df_matrix$Global_X, df_matrix$KR_X)) * 1.1
min_y <- min(c(df_matrix$Global_Y, df_matrix$KR_Y)) * 0.9
max_y <- max(c(df_matrix$Global_Y, df_matrix$KR_Y)) * 1.1

# 9. 대망의 Breakthrough 매트릭스 그리기
p_matrix <- ggplot(df_matrix) +
  # 사분면 분할 십자선 (Global Median 기준)
  geom_vline(xintercept = mid_x, linetype = 'dashed', color = 'gray50', linewidth = 0.8) +
  geom_hline(yintercept = mid_y, linetype = 'dashed', color = 'gray50', linewidth = 0.8) +
  
  # 사분면 배경 텍스트 (Q1 ~ Q4)
  annotate("text", x = max_x*0.9, y = max_y*0.95, label = "Q1: Breakthrough\n(High Growth, High Impact)", fontface = "bold", color = "#C0392B", alpha = 0.2, size = 5) +
  annotate("text", x = min_x*0.9, y = max_y*0.95, label = "Q2: Foundational\n(Low Growth, High Impact)", fontface = "bold", color = "#2980B9", alpha = 0.2, size = 5) +
  annotate("text", x = max_x*0.9, y = min_y*1.05, label = "Q3: Emerging/Niche\n(High Growth, Low Impact)", fontface = "bold", color = "#F39C12", alpha = 0.2, size = 5) +
  annotate("text", x = min_x*0.9, y = min_y*1.05, label = "Q4: Declining\n(Low Growth, Low Impact)", fontface = "bold", color = "#7F8C8D", alpha = 0.2, size = 5) +
  
  # ⭐ 글로벌과 한국을 잇는 방향 화살표 (Gap 궤적)
  geom_segment(aes(x = Global_X, y = Global_Y, xend = KR_X, yend = KR_Y), 
               color = 'gray75', arrow = arrow(length = unit(0.2, "cm"), type = "closed"), linewidth = 0.7) +
  
  # 글로벌 평균 점 (회색 동그라미)
  geom_point(aes(x = Global_X, y = Global_Y, color = 'Global Standard (IP5)'), size = 4, alpha = 0.7) +
  
  # 한국 위치 (보라색 다이아몬드)
  geom_point(aes(x = KR_X, y = KR_Y, color = 'South Korea (KR)'), shape = 18, size = 6) +
  
  # 토픽 라벨 (한국 다이아몬드 옆에 표시)
  geom_text_repel(aes(x = KR_X, y = KR_Y, label = Topic), size = 4, fontface = 'bold', color = '#2C3E50', max.overlaps = 30) +
  
  # 색상 범례 설정
  scale_color_manual(values = c('Global Standard (IP5)' = '#95A5A6', 'South Korea (KR)' = '#8E44AD')) +
  
  # 디자인 테마 설정
  theme_bw(base_size = 13) +
  labs(
    title = 'Strategic Breakthrough Matrix: Global vs. South Korea',
    subtitle = 'Arrows indicate the strategic gap from the Global Standard to South Korea\'s position',
    x = 'Growth Momentum (Time-Series Theta Slope)',
    y = 'Qualitative Impact (Weighted Citation Index)',
    color = 'Positioning'
  ) +
  theme(
    plot.title = element_text(face = 'bold', size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = 'gray40', hjust = 0.5, margin = margin(b = 15)),
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold'),
    panel.grid.minor = element_blank()
  )

# 10. 고해상도 이미지 출력 및 저장
print(p_matrix)
ggsave('STM_Breakthrough_Matrix_Global_vs_KR.png', plot = p_matrix, width = 15, height = 11, dpi = 300)
print('✅ [Phase 6-2] 차세대 킬러 기술 도출 Breakthrough 매트릭스 완료!')

# =====================================================================
# [Phase 6-2 추가] Global vs KR 개별 매트릭스 시각화
# =====================================================================

# 1. 시각화용 롱포맷 데이터 생성 (Global과 KR을 행으로 분리)
df_matrix_long <- bind_rows(
  df_matrix %>% 
    select(Topic, Label, Full_Label, X = Global_X, Y = Global_Y) %>% 
    mutate(Group = "Global Standard (IP5)"),
  df_matrix %>% 
    select(Topic, Label, Full_Label, X = KR_X, Y = KR_Y) %>% 
    mutate(Group = "South Korea (KR)")
)

# 2. 개별 그래프 그리기 (Facet 활용)
p_matrix_sep <- ggplot(df_matrix_long, aes(x = X, y = Y, color = Group)) +
  # 사분면 분할 십자선 (Global Median 기준 고정 - 비교의 일관성)
  geom_vline(xintercept = mid_x, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = mid_y, linetype = "dashed", color = "gray60") +
  
  # 사분면 라벨 추가
  annotate("text", x = max_x*0.8, y = max_y*0.9, label = "Q1: Breakthrough", fontface = "italic", alpha = 0.3) +
  annotate("text", x = min_x*0.8, y = max_y*0.9, label = "Q2: Foundational", fontface = "italic", alpha = 0.3) +
  
  # 데이터 점 찍기
  geom_point(aes(shape = Group), size = 5, alpha = 0.8) +
  
  # 토픽 번호 라벨링
  geom_text_repel(aes(label = Topic), size = 4, fontface = "bold", max.overlaps = 20) +
  
  # 그룹별 색상 및 모양 지정
  scale_color_manual(values = c("Global Standard (IP5)" = "#7F8C8D", "South Korea (KR)" = "#8E44AD")) +
  scale_shape_manual(values = c("Global Standard (IP5)" = 16, "South Korea (KR)" = 18)) +
  
  # 글로벌과 한국을 별도의 창으로 분리
  facet_wrap(~ Group, ncol = 2) +
  
  # 테마 및 디자인
  theme_bw(base_size = 13) +
  labs(
    title = "Breakthrough Matrix Comparison: Global vs. South Korea",
    subtitle = "Comparing Market Momentum (X) and Qualitative Impact (Y) on the same Global Scale",
    x = "Growth Momentum (Theta Slope)",
    y = "Qualitative Impact (Weighted C_adj_i)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(face = "bold", size = 15),
    legend.position = "none", # 제목과 Facet 라벨이 있으므로 범례 생략
    panel.grid.minor = element_blank()
  )

# 3. 출력 및 저장
print(p_matrix_sep)
ggsave("STM_Breakthrough_Matrix_Comparison.png", plot = p_matrix_sep, width = 16, height = 8, dpi = 300)

# ================================================
#================================================

# =====================================================================
# 국가별 토픽 비중(%) 수치 테이블 추출 (CSV 저장)
# =====================================================================

library(tidyverse)

# 1. 데이터 준비 (Theta 비중 행렬)
theta_df <- as.data.frame(final_stm_20_adj$theta)
colnames(theta_df) <- paste0('T', 1:20)
theta_df$Origin_Country <- as.character(out3$meta$Origin_Country)

# 2. 국가별 토픽 평균 비중 계산
df_table <- theta_df %>%
  filter(Origin_Country %in% c('US', 'KR', 'CN', 'JP', 'EP')) %>%
  group_by(Origin_Country) %>%
  summarise(across(starts_with('T'), ~ mean(.x, na.rm = TRUE)))

# 3. 데이터프레임 구조 변경 (토픽을 행으로, 국가를 열로)
df_export <- df_table %>%
  pivot_longer(cols = starts_with('T'), names_to = "Topic", values_to = "Proportion") %>%
  pivot_wider(names_from = Origin_Country, values_from = Proportion)

# 4. 클러스터 및 라벨 정보 추가 (가독성 향상)
cluster_mapping <- data.frame(
  Topic = paste0("T", 1:20),
  Label = c('Crypto Exchange', 'Mobile POS Pay', 'Insurance Claim', 'Invoice Audit',
            'Bank Remittance', 'Digital Wallet', 'Auth & Biometrics', 'NFT Rights',
            'Investment Reco', 'Settlement Proc', 'Enterprise Finance', 'Asset Ledger',
            'Credit Risk', 'Smart Contract', 'Realtime Settle', 'E-commerce Fee',
            'AI Detection', 'Device Linking', 'Charging Txn', 'Face/Voice Login'),
  Cluster = c('3. Digital Infra', '5. Payment', '2. Decision & Risk', '1. Ops & Audit',
              '5. Payment', '3. Digital Infra', '4. Compliance', '3. Digital Infra',
              '2. Decision & Risk', '5. Payment', '1. Ops & Audit', '3. Digital Infra',
              '2. Decision & Risk', '3. Digital Infra', '4. Compliance', '5. Payment',
              '2. Decision & Risk', '2. Decision & Risk', '3. Digital Infra', '5. Payment')
)

# 5. 최종 테이블 병합 및 % 단위 변환 (소수점 2자리)
final_table <- cluster_mapping %>%
  left_join(df_export, by = "Topic") %>%
  arrange(Cluster, Topic) %>% # 클러스터별로 깔끔하게 정렬
  mutate(across(c('US', 'KR', 'CN', 'JP', 'EP'), ~ paste0(round(.x * 100, 2), "%")))

# 6. 화면 출력 및 CSV 파일로 저장
print(final_table)
write.csv(final_table, "STM_Country_Topic_Proportions.csv", row.names = FALSE, fileEncoding = "UTF-8")
print("✅ 수치 데이터 테이블이 'STM_Country_Topic_Proportions.csv'로 저장되었습니다!")

# =====================================================================
# [Phase 5] 미국(US) 대비 국가별 상대적 강점/약점 다이버징 바 차트
# =====================================================================

# 1. 필수 패키지 로드
library(tidyverse)

# 2. 데이터 불러오기
df <- read.csv("Topic_Country_Effect_US_Baseline.csv", stringsAsFactors = FALSE)

# 3. 20개 토픽 마스터 라벨 및 클러스터 순서(Y축 고정용) 정의
# (위에서부터 1번 클러스터가 오도록 역순으로 배치)
cluster_ordering <- rev(c(
  'T4', 'T11',                 # 클러스터 1: 운영·감사
  'T3', 'T18', 'T13', 'T9', 'T17', # 클러스터 2: 의사결정·리스크
  'T14', 'T19', 'T1', 'T8', 'T6', 'T12', # 클러스터 3: 블록체인·자산 인프라
  'T7', 'T15',                 # 클러스터 4: 인증·컴플라이언스
  'T5', 'T10', 'T16', 'T2', 'T20' # 클러스터 5: 계좌·결제 실행
))

# 고유 토픽 정보 추출 및 병합용 마스터 테이블 생성
topic_master <- df %>% 
  distinct(Topic, Topic_Label) %>%
  mutate(
    T_Num = paste0("T", Topic),
    Topic_Full = paste0("T", Topic, ": ", Topic_Label)
  )

# 클러스터 순서에 맞춰 Factor 레벨 지정 (그래프에서 순서 꼬임 방지)
ordered_levels <- topic_master$Topic_Full[match(cluster_ordering, topic_master$T_Num)]

# 4. 시각화를 위한 데이터 필터링 및 전처리
df_plot <- df %>%
  # 유의수준이 *** 또는 ** 인 확실한 데이터만 남김
  filter(Significance %in% c("***", "**")) %>%
  mutate(
    # 'KR vs US' 형태에서 'KR' 등 국가명만 추출
    Country = str_replace(Comparison, " vs US", ""),
    T_Num = paste0("T", Topic),
    Topic_Full = paste0("T", Topic, ": ", Topic_Label),
    Topic_Full = factor(Topic_Full, levels = ordered_levels),
    # 양수/음수에 따른 방향성(색상) 범주 생성
    Direction = ifelse(Estimate > 0, "Country > US (Advantage)", "US > Country (Disadvantage)")
  )

# 5. X축(Estimate) 스케일 자동 조절을 위한 최대/최소값 계산
max_val <- max(abs(df_plot$Estimate), na.rm = TRUE) * 1.3 # 별표(***)가 잘리지 않게 30% 여백 확보

# 6. 대망의 다이버징 바 차트 시각화
p_diverging <- ggplot(df_plot, aes(x = Estimate, y = Topic_Full, fill = Direction)) +
  # 기준선 (0 = 미국과 동일함) 먼저 그리기 (막대 뒤에 깔리게)
  geom_vline(xintercept = 0, linetype = "solid", color = "gray20", linewidth = 0.8) +
  
  # 양방향 막대그래프
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.85) +
  
  # 막대 끝에 유의수준 별표(**, ***) 표시
  geom_text(aes(label = Significance,
                hjust = ifelse(Estimate > 0, -0.2, 1.2)),
            size = 4, color = "black", vjust = 0.75) +
  
  # 4개 국가(CN, EP, JP, KR) 패널 분리
  facet_wrap(~ Country, ncol = 2) +
  
  # 색상 지정 (파란색: 미국 이김, 빨간색: 미국한테 짐)
  scale_fill_manual(values = c("Country > US (Advantage)" = "#2980B9", 
                               "US > Country (Disadvantage)" = "#C0392B")) +
  
  # X축 범위 설정 (좌우 대칭 및 여백)
  scale_x_continuous(limits = c(-max_val, max_val)) +
  
  # 테마 및 디자인 설정
  theme_bw(base_size = 12) +
  labs(
    title = "Comparative Advantage against the US Baseline",
    subtitle = "Highlighting strictly significant topics (p < 0.01). Y-axis is ordered by Functional Clusters.",
    x = "Estimated Effect Size (Difference from the US)",
    y = NULL,
    fill = "Strategic Position"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5, margin = margin(b=15)),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 10, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank() # Y축 가로줄을 지워 깔끔하게 유지
  )

# 7. 화면 출력 및 고해상도 저장
print(p_diverging)
ggsave("STM_US_Baseline_Diverging_Bar.png", plot = p_diverging, width = 15, height = 12, dpi = 300)
print("✅ [성공] 다이버징 바 차트가 저장되었습니다!")

# =====================================================================
# [Phase 2-1 백업] 질적 거품 매트릭스 수치 테이블 추출 (CSV)
# =====================================================================

library(tidyverse)

# 1. 데이터 준비 (앞선 매트릭스 코드와 동일한 로직)
theta_df <- as.data.frame(final_stm_20_adj$theta)
colnames(theta_df) <- paste0('T', 1:20)
theta_df$Origin_Country <- as.character(out3$meta$Origin_Country)
theta_df$C_adj_i <- out3$meta$C_adj_i

# IP5 필터링 및 상위 10% 임계값 설정
df_filtered <- theta_df %>% filter(Origin_Country %in% c('US', 'KR', 'CN', 'JP', 'EP'))
threshold_90 <- quantile(df_filtered$C_adj_i, 0.90, na.rm = TRUE)
df_filtered <- df_filtered %>% mutate(Is_High_Impact = ifelse(C_adj_i >= threshold_90, TRUE, FALSE))

df_long <- df_filtered %>% pivot_longer(cols = starts_with('T'), names_to = 'Topic', values_to = 'Theta')

# 2. 양적 점유율(X) 및 질적 점유율(Y) 계산
quant_share <- df_long %>%
  group_by(Topic, Origin_Country) %>%
  summarise(Total_Theta = sum(Theta, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Topic) %>%
  mutate(Quant_Share = Total_Theta / sum(Total_Theta)) %>% ungroup()

qual_share <- df_long %>%
  filter(Is_High_Impact == TRUE) %>%
  group_by(Topic, Origin_Country) %>%
  summarise(HighImpact_Theta = sum(Theta, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Topic) %>%
  mutate(Qual_Share = HighImpact_Theta / sum(HighImpact_Theta)) %>% ungroup()

# 3. 데이터 병합 및 정리 (논문 표 양식)
topic_info <- data.frame(
  Topic = paste0('T', 1:20),
  Label = c('Crypto Exchange', 'Mobile POS Pay', 'Insurance Claim', 'Invoice Audit',
            'Bank Remittance', 'Digital Wallet', 'Auth & Biometrics', 'NFT Rights',
            'Investment Reco', 'Settlement Proc', 'Enterprise Finance', 'Asset Ledger',
            'Credit Risk', 'Smart Contract', 'Realtime Settle', 'E-commerce Fee',
            'AI Detection', 'Device Linking', 'Charging Txn', 'Face/Voice Login')
)

bubble_table <- quant_share %>%
  left_join(qual_share %>% select(Topic, Origin_Country, Qual_Share), by = c('Topic', 'Origin_Country')) %>%
  mutate(Qual_Share = replace_na(Qual_Share, 0)) %>%
  left_join(topic_info, by = "Topic") %>%
  mutate(
    # % 단위로 변환 및 소수점 1자리 반올림
    Quant_Share_Pct = round(Quant_Share * 100, 1),
    Qual_Share_Pct = round(Qual_Share * 100, 1),
    # 상태 분류
    Status = ifelse(Qual_Share >= Quant_Share, "Core", "Bubble")
  ) %>%
  select(Origin_Country, Topic, Label, Quant_Share_Pct, Qual_Share_Pct, Status) %>%
  arrange(Origin_Country, desc(Quant_Share_Pct)) # 국가별로 양적 점유율이 높은 순으로 정렬

# 4. CSV 저장
write.csv(bubble_table, "STM_Bubble_Matrix_Data.csv", row.names = FALSE, fileEncoding = "UTF-8")
print("✅ 수치 데이터가 'STM_Bubble_Matrix_Data.csv'로 저장되었습니다!")


# =============== 새로운 시작 ======================
# =====================================================================
# [4.3.1] 국가별 토픽 집중도 비교: 레이더 차트 및 클러스터 막대그래프
# =====================================================================

# 1. 필수 패키지 로드
library(tidyverse)
library(fmsb)
library(scales)

# 2. 기초 데이터 준비 (토픽 비중 추출)
theta_df <- as.data.frame(final_stm_20_adj$theta)
colnames(theta_df) <- paste0('T', 1:20)
theta_df$Origin_Country <- as.character(out3$meta$Origin_Country)

# 3. IP5 국가 필터링 및 국가별 토픽 평균 비중 계산
df_country_mean <- theta_df %>%
  filter(Origin_Country %in% c('US', 'KR', 'CN', 'JP', 'EP')) %>%
  group_by(Origin_Country) %>%
  summarise(across(starts_with('T'), ~ mean(.x, na.rm = TRUE)))

# 4. 8대 메가 클러스터 맵핑 및 순서 정의 (지정된 파스텔 색상 포함)
cluster_mapping <- data.frame(
  Topic = c('T4', 'T11', 
            'T3', 'T18', 
            'T9', 'T13', 'T17', 
            'T14', 'T19', 
            'T1', 'T6', 'T8', 'T12', 
            'T7', 'T15', 
            'T5', 'T10', 
            'T2', 'T16', 'T20'),
  Cluster = c(rep('A. 기업 재무 및 회계 관리', 2),
              rep('B. 맞춤형 보험 및 타겟 식별', 2),
              rep('C. AI 기반 신용/투자 분석', 3),
              rep('D. 블록체인 기반 에너지 금융', 2),
              rep('E. 블록체인 및 디지털 자산', 4),
              rep('F. 보안 인증 및 규제 컴플라이언스', 2),
              rep('G. 뱅킹 및 대금 정산', 2),
              rep('H. O2O 상거래 및 간편 결제', 3))
)

# 클러스터별 파스텔 톤 색상표
cluster_colors <- c(
  'A. 기업 재무 및 회계 관리' = '#AEC6CF',
  'B. 맞춤형 보험 및 타겟 식별' = '#FFD1BA',
  'C. AI 기반 신용/투자 분석' = '#D8BFD8',
  'D. 블록체인 기반 에너지 금융' = '#C1E1C1',
  'E. 블록체인 및 디지털 자산' = '#FDFD96',
  'F. 보안 인증 및 규제 컴플라이언스' = '#B0C4DE',
  'G. 뱅킹 및 대금 정산' = '#AFEEEE',
  'H. O2O 상거래 및 간편 결제' = '#FFB6C1'
)

# =====================================================================
# [파트 1] 레이더 차트 (fmsb 활용)
# =====================================================================

# 데이터 구조 재정렬 (클러스터가 뭉쳐서 그려지도록 순서 고정)
df_radar <- df_country_mean %>% select(Origin_Country, all_of(cluster_mapping$Topic))

# fmsb 패키지 양식에 맞게 변환 (1행: max, 2행: min, 3행~: 데이터)
rownames(df_radar) <- df_radar$Origin_Country
df_radar$Origin_Country <- NULL

max_val <- max(df_radar) * 1.1 # 넉넉한 최대치 설정
df_radar_fmsb <- rbind(rep(max_val, 20), rep(0, 20), df_radar)

# 차트 그리기 및 저장 (2x3 배열)
png("STM_Country_RadarChart.png", width = 1800, height = 1200, res = 150)
par(mfrow = c(2, 3), mar = c(2, 2, 3, 2)) 

# IP5 국가별 레이더 차트 색상 (구분을 위한 단색 테두리)
radar_colors <- c("US"="#2980B9", "KR"="#8E44AD", "CN"="#C0392B", "JP"="#27AE60", "EP"="#F39C12")

for(country_name in c('US', 'KR', 'CN', 'JP', 'EP')) {
  if(country_name %in% rownames(df_radar_fmsb)) {
    radarchart(df_radar_fmsb[c(1, 2, which(rownames(df_radar_fmsb) == country_name)), ], 
               axistype = 1,
               pcol = radar_colors[country_name], 
               pfcol = scales::alpha(radar_colors[country_name], 0.3), 
               plwd = 2.5, 
               cglcol = "grey", cglty = 1, axislabcol = "grey", 
               caxislabels = seq(0, round(max_val*100), length.out=5), 
               vlcex = 1.1, # T1, T2 라벨 크기
               title = paste0("Technology Focus: ", country_name))
  }
}
dev.off()
print("✅ [성공] 8대 클러스터 기준 레이더 차트 저장 (STM_Country_RadarChart.png)")

# =====================================================================
# [파트 2] 국가별 클러스터 막대그래프 (ggplot2 활용)
# =====================================================================

# 롱포맷으로 변환 후 클러스터 정보 결합
df_bar <- df_country_mean %>%
  pivot_longer(cols = starts_with('T'), names_to = 'Topic', values_to = 'Proportion') %>%
  left_join(cluster_mapping, by = "Topic")

# 토픽 순서 팩터화 (A~H 순서대로 배치)
df_bar$Topic <- factor(df_bar$Topic, levels = cluster_mapping$Topic)

# 막대그래프 시각화
p_bar <- ggplot(df_bar, aes(x = Topic, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", color = "gray30", linewidth = 0.3) +
  facet_wrap(~ Origin_Country, ncol = 3, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = cluster_colors) + # 📌 우리가 지정한 파스텔 색상 적용!
  theme_minimal(base_size = 13) +
  labs(title = 'FinTech Patent Portfolio by Country (IP5)',
       subtitle = 'Topic Proportions Grouped by 8 Mega Clusters (A~H)',
       x = "Topics", y = "Expected Proportion") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10, color = "black"),
    strip.background = element_rect(fill = "gray90", color = "white"),
    strip.text = element_text(face = 'bold', size = 15),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  )

print(p_bar)
ggsave('STM_Country_BarChart_8Clusters.png', plot = p_bar, width = 16, height = 10, dpi = 300)
print("✅ [성공] 지정 색상 반영 8대 클러스터 막대그래프 저장 (STM_Country_BarChart_8Clusters.png)")

# =====================================================================
# [수정본] 국가별 토픽 집중도 레이더 차트 (fmsb 패키지 오류 해결)
# =====================================================================

# 1. 필수 패키지 로드
library(tidyverse)
library(fmsb)
library(scales)

# 2. 기초 데이터 준비 (theta 비중 추출)
theta_df <- as.data.frame(final_stm_20_adj$theta)
colnames(theta_df) <- paste0('T', 1:20)
theta_df$Origin_Country <- as.character(out3$meta$Origin_Country)

# IP5 국가 필터링 및 평균 비중 계산
df_radar_base <- theta_df %>%
  filter(Origin_Country %in% c('US', 'KR', 'CN', 'JP', 'EP')) %>%
  group_by(Origin_Country) %>%
  summarise(across(starts_with('T'), ~ mean(.x, na.rm = TRUE)))

# 3. 8대 클러스터 순서에 맞게 열 재배치 (A~H 순서대로 묶기 위함)
cluster_order <- c('T4', 'T11', 'T3', 'T18', 'T9', 'T13', 'T17', 
                   'T14', 'T19', 'T1', 'T6', 'T8', 'T12', 
                   'T7', 'T15', 'T5', 'T10', 'T2', 'T16', 'T20')

# 데이터프레임 구조 변경 및 클러스터 순서 정렬
df_radar <- as.data.frame(df_radar_base)
rownames(df_radar) <- df_radar$Origin_Country
df_radar <- df_radar[, cluster_order] 

# 4. fmsb 양식에 맞게 Max/Min 행 추가 (매우 중요)
# 레이더 차트의 척도를 맞추기 위해 전체 데이터의 최대값 확인 (여유있게 1.1배)
max_val <- max(df_radar, na.rm = TRUE) * 1.1

# Max 행과 Min 행 생성
df_max <- rep(max_val, 20)
df_min <- rep(0, 20)

# 데이터 결합 (1행: Max, 2행: Min, 3행~: 실제 국가 데이터)
df_radar_fmsb <- rbind(df_max, df_min, df_radar)
rownames(df_radar_fmsb) <- c("Max", "Min", rownames(df_radar))

# 5. 레이더 차트 시각화 및 저장
png("STM_Country_RadarChart_Fixed.png", width = 1800, height = 1200, res = 150)
par(mfrow = c(2, 3), mar = c(2, 2, 3, 2)) # 2줄 3칸 배열, 여백 조절

# 국가별 색상 지정
radar_colors <- c("US"="#2980B9", "KR"="#8E44AD", "CN"="#C0392B", "JP"="#27AE60", "EP"="#F39C12")
countries <- c("US", "KR", "CN", "JP", "EP")

for(country in countries) {
  # 특정 국가를 그리기 위해 Max(1행), Min(2행), 해당국가 데이터 행 추출
  plot_data <- df_radar_fmsb[c("Max", "Min", country), ]
  
  # 색상 투명도(Alpha) 적용
  col_border <- radar_colors[country]
  col_fill <- scales::alpha(radar_colors[country], 0.3)
  
  radarchart(plot_data, 
             axistype = 1,
             pcol = col_border,       # 선 색상
             pfcol = col_fill,        # 채우기 색상
             plwd = 2.5,              # 선 굵기
             cglcol = "grey70",       # 거미줄 색상
             cglty = 1,               # 거미줄 선 종류 (실선)
             axislabcol = "grey50",   # 중앙 눈금 라벨 색상
             caxislabels = seq(0, round(max_val*100), length.out=5), # 눈금 범위 지정
             vlcex = 1.1,             # T1, T2 등 외부 토픽 라벨 크기
             title = paste0("Technology Focus: ", country)) # 숫자가 아닌 국가명 출력
}
dev.off()
print("✅ [수정 완료] 제목과 데이터가 정상적으로 매칭된 레이더 차트가 저장되었습니다 (STM_Country_RadarChart_Fixed.png)")

# =====================================================================
# [4.3.2] 미국(US) 기준 상대적 우위 검증 및 CSV 추출 (문법 교정판)
# =====================================================================
library(stm)
library(dplyr)

# 1. 국가 변수 설정 및 미국(US) 기준점(Reference) 지정
# 이전 코드에서 발생한 괄호 및 기호 에러를 수정한 부분입니다.
out3$meta$Origin_Country <- as.factor(out3$meta$Origin_Country)
out3$meta$Origin_Country <- relevel(out3$meta$Origin_Country, ref = "US")

# 2. STM 효과 추정 실행 (미국 기준 사후 분석)
# 공변량 구조는 학습 시 모델과 완벽하게 일치해야 합니다.
set.seed(2026)
effect_country_US <- estimateEffect(
  1:20 ~ Origin_Country + s(Publication.Year) + C_adj_i,
  stmobj = final_stm_20_adj,
  metadata = out3$meta,
  uncertainty = "Global"
)

# 3. 8대 메가 클러스터 및 토픽 라벨 정의 (시각화 연동용)
cluster_mapping <- data.frame(
  Topic = 1:20,
  T_Num = paste0("T", 1:20),
  Label = c('가상자산 거래', '모바일 결제', '보험 서비스', '기업 회계/사무',
            '은행 금융 서비스', '디지털 화폐', '보안 및 인증', '디지털 자산/NFT',
            '투자 분석', '결제 및 정산', '기업 자금 분석', '분산 원장 자산',
            '대출 및 신용', '스마트 계약', '데이터 기록 관리', '온라인 상거래',
            'AI 분석/탐지', '계정 식별 및 분류', '모빌리티 핀테크', '사용자 인식'),
  Cluster = c('E. 블록체인 및 디지털 자산', 'H. O2O 상거래 및 간편 결제', 'B. 맞춤형 보험 및 타겟 식별', 'A. 기업 재무 및 회계 관리',
              'G. 뱅킹 및 대금 정산', 'E. 블록체인 및 디지털 자산', 'F. 보안 인증 및 규제 컴플라이언스', 'E. 블록체인 및 디지털 자산',
              'C. AI 기반 신용/투자 분석', 'G. 뱅킹 및 대금 정산', 'A. 기업 재무 및 회계 관리', 'E. 블록체인 및 디지털 자산',
              'C. AI 기반 신용/투자 분석', 'D. 블록체인 기반 에너지 금융', 'F. 보안 인증 및 규제 컴플라이언스', 'H. O2O 상거래 및 간편 결제',
              'C. AI 기반 신용/투자 분석', 'B. 맞춤형 보험 및 타겟 식별', 'D. 블록체인 기반 에너지 금융', 'H. O2O 상거래 및 간편 결제')
)

# 4. 통계 결과 추출 및 정제
sum_effect <- summary(effect_country_US)
result_list <- list()

for (i in 1:20) {
  coef_table <- as.data.frame(sum_effect$tables[[i]])
  coef_table$Term <- rownames(coef_table)
  coef_table$Topic <- i
  
  # 미국(US) 대비 타 국가 비교 계수만 추출
  country_coefs <- coef_table[grep("Origin_Country", coef_table$Term), ]
  country_coefs$Country <- gsub("Origin_Country", "", country_coefs$Term)
  
  result_list[[i]] <- country_coefs
}

final_result_df <- do.call(rbind, result_list)

# 5. 최종 데이터 프레임 구축 및 유의성 별표 추가
final_result_df <- final_result_df %>%
  left_join(cluster_mapping, by = "Topic") %>%
  mutate(
    Significance = case_when(
      `Pr(>|t|)` < 0.001 ~ "***",
      `Pr(>|t|)` < 0.01  ~ "**",
      `Pr(>|t|)` < 0.05  ~ "*",
      `Pr(>|t|)` < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  select(Country, Cluster, T_Num, Label, Estimate, `Std. Error`, `t value`, p_value = `Pr(>|t|)`, Significance)

# 6. CSV 파일로 저장 (인코딩 에러 해결 버전)
write.csv(final_result_df, "STM_US_Baseline_Advantage.csv", row.names = FALSE, fileEncoding = "CP949")

print("✅ [성공] 미국 대비 상대적 우위 데이터가 저장되었습니다!")

# =====================================================================
# [4.3.2] 유의도 농도(Intensity) 반영 전략적 갭 히트맵 (최종 완성판)
# =====================================================================
library(tidyverse)

# 1. 추출해둔 데이터 불러오기
df <- read.csv("STM_US_Baseline_Advantage.csv", fileEncoding = "CP949", stringsAsFactors = FALSE)

# 2. 데이터 전처리 (방향 및 유의수준에 따른 7단계 세밀한 범주화)
df_heat <- df %>%
  mutate(
    Fill_Category = case_when(
      p_value < 0.001 & Estimate > 0 ~ "Target (+) : p < 0.001 (***)",
      p_value < 0.01  & Estimate > 0 ~ "Target (+) : p < 0.01 (**)",
      p_value < 0.05  & Estimate > 0 ~ "Target (+) : p < 0.05 (*)",
      p_value < 0.001 & Estimate < 0 ~ "US (-) : p < 0.001 (***)",
      p_value < 0.01  & Estimate < 0 ~ "US (-) : p < 0.01 (**)",
      p_value < 0.05  & Estimate < 0 ~ "US (-) : p < 0.05 (*)",
      TRUE ~ "Not Significant"
    )
  )

# 범례(Legend)에 표시될 순서를 강제로 지정 (타국 압승 -> 무승부 -> 미국 압승 순서)
df_heat$Fill_Category <- factor(df_heat$Fill_Category, levels = c(
  "Target (+) : p < 0.001 (***)",
  "Target (+) : p < 0.01 (**)",
  "Target (+) : p < 0.05 (*)",
  "Not Significant",
  "US (-) : p < 0.05 (*)",
  "US (-) : p < 0.01 (**)",
  "US (-) : p < 0.001 (***)"
))

# 3. Y축(토픽 및 클러스터) 정렬을 위한 팩터화
cluster_order <- c('A. 기업 재무 및 회계 관리', 'B. 맞춤형 보험 및 타겟 식별',
                   'C. AI 기반 신용/투자 분석', 'D. 블록체인 기반 에너지 금융',
                   'E. 블록체인 및 디지털 자산', 'F. 보안 인증 및 규제 컴플라이언스',
                   'G. 뱅킹 및 대금 정산', 'H. O2O 상거래 및 간편 결제')
df_heat$Cluster <- factor(df_heat$Cluster, levels = cluster_order)

# T_Num에서 숫자만 추출하여 역순 정렬 (에러 해결된 완벽한 로직)
topic_levels <- df_heat %>%
  mutate(Topic_Int = as.numeric(gsub("T", "", T_Num))) %>% 
  arrange(desc(Cluster), desc(Topic_Int)) %>%              
  pull(T_Num) %>%
  unique()
df_heat$T_Num <- factor(df_heat$T_Num, levels = topic_levels)

# X축 국가 순서 고정
df_heat$Country <- factor(df_heat$Country, levels = c("KR", "CN", "JP", "EP"))

# 4. 🔥 7단계 파스텔 톤 농도 팔레트 정의
pastel_shades <- c(
  "Target (+) : p < 0.001 (***)" = "#5DADE2", # 짙은 파스텔 블루 (확실한 타국 우위)
  "Target (+) : p < 0.01 (**)"   = "#85C1E9", # 중간 파스텔 블루
  "Target (+) : p < 0.05 (*)"    = "#D4E6F1", # 연한 파스텔 블루 (미약한 타국 우위)
  "Not Significant"              = "#F8F9F9", # 옅은 회색 (무승부)
  "US (-) : p < 0.05 (*)"        = "#FADBD8", # 연한 파스텔 레드 (미약한 미국 우위)
  "US (-) : p < 0.01 (**)"       = "#F5B7B1", # 중간 파스텔 레드
  "US (-) : p < 0.001 (***)"     = "#EC7063"  # 짙은 파스텔 레드 (확실한 미국 독식)
)

# 5. 농도 기반의 히트맵 시각화
p_heatmap_final <- ggplot(df_heat, aes(x = Country, y = T_Num)) +
  geom_tile(aes(fill = Fill_Category), color = "white", linewidth = 1) +
  geom_text(aes(label = Significance), color = "gray30", size = 5.5, vjust = 0.75) +
  facet_grid(Cluster ~ ., scales = "free_y", space = "free_y") +
  
  # 수동 팔레트 적용 및 범례를 2줄로 깔끔하게 정렬
  scale_fill_manual(values = pastel_shades, drop = FALSE, guide = guide_legend(nrow = 2)) +
  
  # 디자인 및 테마 설정
  theme_minimal(base_size = 13) +
  labs(
    title = "Strategic Gap Heatmap: Global Competitors vs. US Baseline",
    subtitle = "Color intensity reflects statistical significance level (* to ***)",
    x = "Comparison Countries (vs. US)",
    y = "Topics (Grouped by Clusters)",
    fill = "Dominance & Significance"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray50", hjust = 0.5, margin = margin(b=15)),
    
    # 클러스터 이름 라벨 삭제 및 블록 밀착
    strip.text.y = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"), 
    
    axis.text.x = element_text(face = "bold", size = 14, color = "black"),
    axis.text.y = element_text(face = "bold", size = 11, color = "gray30"),
    
    panel.grid = element_blank(), 
    legend.position = "bottom",
    legend.title = element_text(face = "bold", margin = margin(r=15)),
    legend.text = element_text(size = 10)
  )

# 6. 화면 출력 및 저장
print(p_heatmap_final)
ggsave("STM_Strategic_Gap_Heatmap_Final_Intensity.png", plot = p_heatmap_final, width = 13, height = 10, dpi = 300)
print("✅ [성공] 농도(Intensity)가 반영된 궁극의 히트맵이 'STM_Strategic_Gap_Heatmap_Final_Intensity.png'로 저장되었습니다!")

# =====================================================================
# [4.1.3] 클러스터 지형도: 예상 특허 수(Volume) vs 가중 평균 피인용 지수(Impact)
# =====================================================================
library(tidyverse)
library(ggrepel)
library(scales)

# 1. 기초 데이터 준비 (결측치 안전 제거)
theta_mat <- final_stm_20_adj$theta
colnames(theta_mat) <- paste0('T', 1:20)
c_adj <- out3$meta$C_adj_i

valid_idx <- !is.na(c_adj)
theta_mat <- theta_mat[valid_idx, ]
c_adj <- c_adj[valid_idx]

# 2. 8대 메가 클러스터 및 파스텔 색상 맵핑
cluster_mapping <- data.frame(
  Topic = paste0('T', 1:20),
  Mega_Cluster = c('E', 'H', 'B', 'A', 'G', 'E', 'F', 'E', 'C', 'G',
                   'A', 'E', 'C', 'D', 'F', 'H', 'C', 'B', 'D', 'H')
)

cluster_colors <- c(
  'A' = '#AEC6CF', 'B' = '#FFD1BA', 'C' = '#D8BFD8', 'D' = '#C1E1C1',
  'E' = '#FDFD96', 'F' = '#B0C4DE', 'G' = '#AFEEEE', 'H' = '#FFB6C1'
)

# 3. 문서별 데이터를 롱포맷으로 변환 후 클러스터 맵핑
df_docs <- as.data.frame(theta_mat) %>%
  mutate(Impact = c_adj) %>%
  pivot_longer(cols = starts_with('T'), names_to = "Topic", values_to = "Theta") %>%
  left_join(cluster_mapping, by = "Topic")

# 4. ⭐ 핵심 로직: 규모(X축)와 1건당 밀도 높은 파급력(Y축) 계산
cluster_summary <- df_docs %>%
  group_by(Mega_Cluster) %>%
  summarise(
    # X축: 해당 클러스터의 전체 특허 예상 합계 (단위: 건)
    Expected_Patent_Count = sum(Theta, na.rm = TRUE),
    
    # Y축: 가중 평균 피인용 지수 (총 파급력을 특허 수로 나누어 '규모 편향' 제거)
    Weighted_Impact_Index = sum(Theta * Impact, na.rm = TRUE) / sum(Theta, na.rm = TRUE)
  )

# 5. 새로운 산점도 시각화
p_landscape_index <- ggplot(cluster_summary, aes(x = Expected_Patent_Count, y = Weighted_Impact_Index)) +
  
  # 학술적 기준선: 글로벌 연평균 파급력 기준 (Y = 1.0)
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.6) +
  annotate("text", x = min(cluster_summary$Expected_Patent_Count), y = 1.02, 
           label = "Global Average Baseline (Index = 1.0)", color = "red", hjust = 0, size = 4, fontface = "italic") +
  
  # 클러스터 거품(Bubble) 그리기
  geom_point(aes(color = Mega_Cluster, fill = Mega_Cluster), size = 14, shape = 21, alpha = 0.9) +
  
  # 클러스터 알파벳 텍스트 삽입
  geom_text(aes(label = Mega_Cluster), fontface = "bold", size = 6, color = "black") +
  
  # 우리가 정의한 파스텔 색상 적용
  scale_fill_manual(values = cluster_colors) +
  scale_color_manual(values = cluster_colors) +
  
  # X축 단위 표시 (1,000 등 콤마 표시)
  scale_x_continuous(labels = comma_format()) +
  
  # 테마 및 디자인
  theme_bw(base_size = 14) +
  labs(
    title = "Cluster Landscape: Expected Patent Count vs. Qualitative Impact",
    subtitle = "Y-Axis reflects Time-Normalized Citation Index (Values > 1.0 indicate above-average impact)",
    x = "Expected Patent Count (Quantitative Volume)",
    y = "Weighted Average Citation Index (Qualitative Impact)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# 6. 화면 출력 및 저장
print(p_landscape_index)
ggsave("STM_Cluster_Landscape_ImpactIndex.png", plot = p_landscape_index, width = 11, height = 8, dpi = 300)
print("✅ [성공] 가중 평균 피인용 지수로 재설계된 산점도 저장 완료! (STM_Cluster_Landscape_ImpactIndex.png)")

# =====================================================================
# [4.2.3] 기술 수명주기(TLC) 기반 포트폴리오 매트릭스 및 TMR 산출
# =====================================================================
library(tidyverse)
library(stm)
library(ggrepel)
library(scales)

# 1. 기초 데이터 준비
theta_mat <- final_stm_20_adj$theta
colnames(theta_mat) <- paste0('T', 1:20)
c_adj <- out3$meta$C_adj_i
pub_year <- as.numeric(as.character(out3$meta$Publication.Year))

valid_idx <- !is.na(c_adj) & !is.na(pub_year)
theta_mat <- theta_mat[valid_idx, ]
c_adj <- c_adj[valid_idx]
pub_year <- pub_year[valid_idx]

# 2. X축(비중), Y축(성장 기울기), Size(파급력) 계산
df_tlc <- data.frame(
  Topic = paste0('T', 1:20),
  # X축: 전체 기간 평균 토픽 비중
  Proportion = colMeans(theta_mat),
  # Y축: 연도에 따른 비중 변화 기울기 (성장 가속도)
  Growth_Momentum = sapply(1:20, function(i) coef(lm(theta_mat[, i] ~ pub_year))[2]) * 100,
  # Size: 가중 평균 피인용 지수 (질적 파급력)
  Impact = colSums(theta_mat * c_adj) / colSums(theta_mat)
)

# 3. 8대 메가 클러스터 맵핑
cluster_mapping <- data.frame(
  Topic = paste0('T', 1:20),
  Cluster = c('3. Digital Infra', '5. Payment', '2. Decision & Risk', '1. Ops & Audit',
              '5. Payment', '3. Digital Infra', '4. Compliance', '3. Digital Infra',
              '2. Decision & Risk', '5. Payment', '1. Ops & Audit', '3. Digital Infra',
              '2. Decision & Risk', '3. Digital Infra', '4. Compliance', '5. Payment',
              '2. Decision & Risk', '2. Decision & Risk', '3. Digital Infra', '5. Payment')
)

cluster_colors <- c(
  '1. Ops & Audit' = '#AEC6CF', '2. Decision & Risk' = '#D8BFD8', 
  '3. Digital Infra' = '#FDFD96', '4. Compliance' = '#B0C4DE', '5. Payment' = '#FFB6C1'
)

df_tlc <- df_tlc %>% left_join(cluster_mapping, by = "Topic")

# 4. ⭐ 최현홍 교수님(2022) Gompertz 모형 기반 TMR(기술 성숙도) 산출
tmr_results <- numeric(20)

for(i in 1:20) {
  # 연도별 해당 토픽의 예상 특허 수 합계
  yearly_counts <- tapply(theta_mat[, i], pub_year, sum)
  years_seq <- 1:length(yearly_counts)
  # 누적 합계 계산 (S-Curve 피팅용)
  cumulative_counts <- cumsum(yearly_counts)
  
  # Gompertz 모델 피팅 (에러 방지를 위해 tryCatch 사용)
  fit <- tryCatch({
    nls(cumulative_counts ~ SSgompertz(years_seq, Asym, b2, b3))
  }, error = function(e) NULL)
  
  if(!is.null(fit)) {
    # Asym = 이론적 한계치 (m)
    m_asymptote <- coef(fit)["Asym"]
    current_value <- max(cumulative_counts)
    # TMR 산출: 현재 누적량 / 이론적 한계치
    tmr_results[i] <- current_value / m_asymptote
  } else {
    tmr_results[i] <- NA # 피팅 실패 시 NA 처리
  }
}

df_tlc$TMR <- tmr_results

# TMR 결과를 보기 좋게 CSV로 추출 (본문 표 작성용)
df_tmr_export <- df_tlc %>% select(Topic, Cluster, TMR) %>% arrange(TMR)
write.csv(df_tmr_export, "STM_Topic_TMR_Results.csv", row.names = FALSE)
print("✅ Gompertz 기반 TMR 산출 완료! (STM_Topic_TMR_Results.csv)")

# 5. 사분면 기준선 (중앙값)
mid_x <- median(df_tlc$Proportion)
mid_y <- median(df_tlc$Growth_Momentum)

# 6. TLC 매트릭스 시각화 (다이내믹 버블 차트)
p_tlc <- ggplot(df_tlc, aes(x = Proportion, y = Growth_Momentum)) +
  # 사분면 십자선
  geom_vline(xintercept = mid_x, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_hline(yintercept = mid_y, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  
  # 사분면 라벨
  annotate("text", x = max(df_tlc$Proportion)*0.85, y = max(df_tlc$Growth_Momentum)*0.95, label = "Q1: Mainstream\n(High Share, High Growth)", color = "gray30", fontface = "bold", size = 4) +
  annotate("text", x = min(df_tlc$Proportion)*1.2, y = max(df_tlc$Growth_Momentum)*0.95, label = "Q2: Emerging / Breakthrough\n(Low Share, High Growth)", color = "#C0392B", fontface = "bold", size = 4) +
  annotate("text", x = min(df_tlc$Proportion)*1.2, y = min(df_tlc$Growth_Momentum)*1.1, label = "Q3: Niche / Declining\n(Low Share, Low Growth)", color = "gray30", fontface = "bold", size = 4) +
  annotate("text", x = max(df_tlc$Proportion)*0.85, y = min(df_tlc$Growth_Momentum)*1.1, label = "Q4: Saturated / Mature\n(High Share, Low Growth)", color = "#2980B9", fontface = "bold", size = 4) +
  
  # 버블 그리기 (크기는 파급력, 색상은 클러스터)
  geom_point(aes(size = Impact, fill = Cluster), shape = 21, color = "gray20", alpha = 0.85) +
  
  # 토픽 라벨 달기
  geom_text_repel(aes(label = Topic), fontface = "bold", size = 5, max.overlaps = 20) +
  
  # 스케일 설정
  scale_fill_manual(values = cluster_colors) +
  scale_size_continuous(range = c(5, 20), guide = "none") + # 버블 크기 최소-최대 설정
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  
  # 테마 및 디자인
  theme_bw(base_size = 14) +
  labs(
    title = "Technology Life Cycle (TLC) Portfolio Matrix",
    subtitle = "Bubble size represents Qualitative Impact (Weighted Citation Index)",
    x = "Market Dominance (Topic Proportion, %)",
    y = "Growth Acceleration (Time-Series Theta Slope)",
    fill = "Mega Clusters"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "gray40", hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 7. 출력 및 고해상도 저장
print(p_tlc)
ggsave("STM_TLC_Portfolio_Matrix.png", plot = p_tlc, width = 14, height = 10, dpi = 300)
print("✅ [성공] TLC 포트폴리오 매트릭스 저장 완료! (STM_TLC_Portfolio_Matrix.png)")


# =====================================================================
# [4.1.1 대안] 버려진 특허(Discarded) 5만 건 대상 노이즈 토픽 모델링 (K=20)
# =====================================================================

library(stm)
library(stringr)
library(stopwords)
library(dplyr)
library(tidyr)

# 1. 데이터 불러오기 및 노이즈(FALSE) 데이터만 추출
setwd(here::here("data"))  # was: setwd("<project-root>/data") 
df_all <- read.csv("full_patents_with_scores.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# is_fintech가 FALSE(버려진 특허)인 데이터만 필터링
df_noise <- df_all %>%
  filter(toupper(as.character(is_fintech)) == "FALSE") %>%
  mutate(
    Title = replace_na(Title, ""),
    Abstract = replace_na(Abstract, ""),
    text_tt = paste(Title, Title, Abstract, sep=" ") # Title에 가중치 2배 적용
  )

print(paste("🔥 분석 대상 버려진 특허 수:", nrow(df_noise)))

# 2. 불용어 사전 설정 (완벽한 비교를 위해 기존과 동일하게 적용)
stwds <- stopwords(language = "en", source = "smart")
custom_ultimate <- c(
  'comprise', 'comprises', 'include', 'includes', 'provide', 'provides', 
  'configure', 'configured', 'contain', 'contains', 'perform', 'performs', 
  'generate', 'generates', 'determine', 'determines', 'obtain', 'obtains', 
  'receive', 'receives', 'send', 'sends', 'transmit', 'transmits', 
  'store', 'stores', 'process', 'processes', 'execute', 'executes', 
  'calculate', 'calculates', 'apply', 'applies', 'relate', 'relates', 
  'base', 'based', 'present', 'presents', 'disclose', 'discloses', 
  'step', 'steps', 'mean', 'means', 'draw', 'drawing', 'figure', 'figures', 
  'part', 'piece', 'field', 'technique', 'background', 'result', 'require', 
  'prefer', 'medium', 'invention', 'system', 'method', 'apparatus', 
  'plurality', 'embodiment', 'device', 'data', 'information', 'server', 
  'network', 'terminal', 'user', 'object', 'unit', 'module', 'program',
  'thereof', 'wherein', 'comprising'
)
csw_final_new <- unique(c(stwds, custom_ultimate))

# 3. 텍스트 전처리
print("텍스트 전처리 진행 중...")
proc_noise <- textProcessor(documents = df_noise$text_tt, 
                            metadata = df_noise, 
                            lowercase = TRUE, 
                            removepunctuation = TRUE, 
                            customstopwords = csw_final_new, 
                            removestopwords = TRUE, 
                            removenumbers = FALSE, 
                            stem = TRUE, 
                            wordLengths = c(2,Inf))

# 4. 희귀 단어 제거 (N이 5만 건으로 매우 크므로 150번 이상 등장한 단어만 남김)
out_noise <- prepDocuments(proc_noise$documents, proc_noise$vocab, proc_noise$meta, lower.thresh = 150)

# 5. 노이즈 규명용 STM 학습 (K=20)
print("노이즈 STM 모델 학습 중 (K=20)... 데이터가 많아 5~10분 정도 소요될 수 있습니다.")
set.seed(2026) 
stm_noise_20 <- stm(documents = out_noise$documents, 
                    vocab = out_noise$vocab, 
                    K = 20, # ⭐ 진성 핀테크와 동일하게 20개로 맞춤
                    max.em.its = 100, 
                    init.type = "Spectral")

# 6. 토픽별 핵심 단어 추출
label_noise_20 <- labelTopics(stm_noise_20, n = 15)

# 7. 결과를 보기 좋게 CSV로 정리
topic_words_df_20 <- data.frame(Topic = 1:20)
topic_words_df_20$Highest_Prob <- apply(label_noise_20$prob, 1, paste, collapse = ", ")
topic_words_df_20$FREX <- apply(label_noise_20$frex, 1, paste, collapse = ", ")

write.csv(topic_words_df_20, "STM_Discarded_Noise_Topics_K20.csv", row.names = FALSE)
print("✅ 버려진 특허 대상 K=20 STM 분석 완료! (STM_Discarded_Noise_Topics_K20.csv 확인)")

# =====================================================================
# [4.1.1] 시멘틱 필터링 효과 대조 분석 (K=20 vs K=20)
# (버려진 노이즈 토픽 vs 최종 진성 핀테크 토픽 핵심 단어 비교)
# =====================================================================

# 1. 필수 패키지 로드
library(tidyverse)
library(tidytext)

# 작업 폴더 경로 설정 (실제 CSV 파일이 있는 경로로 맞춰주세요)
setwd(here::here("data"))  # was: setwd("<project-root>/data") 

# 2. 파일 불러오기
df_noise <- read.csv("STM_Discarded_Noise_Topics_K20.csv", stringsAsFactors = FALSE)
df_final <- read.csv("FINAL_K20_PROB_Words_Adj.csv", stringsAsFactors = FALSE)

# 3. 버려진 특허(Discarded) 데이터 가공
noise_long <- df_noise %>%
  select(Topic, Highest_Prob) %>%
  # 쉼표를 기준으로 단어 분리
  mutate(Highest_Prob = str_split(Highest_Prob, ",\\s*")) %>%
  unnest(Highest_Prob) %>%
  group_by(Topic) %>%
  # 1위부터 15위까지 순위에 따른 가중치 부여 (1위=15점, 15위=1점)
  mutate(Rank = row_number(),
         Score = 16 - Rank,
         Group = "Discarded (Noise / General IT)",
         Word = trimws(Highest_Prob)) %>%
  ungroup() %>%
  select(Group, Topic, Rank, Word, Score)

# 4. 정제된 특허(Final) 데이터 가공
# 파이썬/R 변환 과정에서 생성된 첫 번째 인덱스 열(Unnamed: 0)의 이름을 Topic으로 변경
colnames(df_final)[1] <- "Topic" 

final_long <- df_final %>%
  pivot_longer(cols = starts_with("V"), names_to = "Rank_Col", values_to = "Word") %>%
  mutate(Rank = as.numeric(str_remove(Rank_Col, "V")),
         Score = 16 - Rank,
         Group = "Retained (FinTech Core)",
         Word = trimws(Word)) %>%
  select(Group, Topic, Rank, Word, Score)

# 5. 두 데이터 병합 및 의미 없는 범용 단어(Stopwords) 필터링
combined_long <- bind_rows(noise_long, final_long)

# 두 집단 모두에서 너무 흔하게 쓰여서 대비 효과를 흐리는 껍데기 단어 강력 제거
patent_stopwords <- c("system", "method", "provid", "receiv", "includ", "perform", 
                      "generat", "process", "comput", "devic", "user", "data", 
                      "inform", "termin", "network", "server", "apparatus", 
                      "pluraliti", "embodiment", "thereof", "wherein", "compris", 
                      "identifi", "determin", "stor", "execut", "oper", "unit", 
                      "control", "manag", "commun", "communic", "request", "messag")

# 단어별 가중치 합산 (Total Score)
word_scores <- combined_long %>%
  filter(!Word %in% patent_stopwords) %>%
  group_by(Group, Word) %>%
  summarise(Total_Score = sum(Score), .groups = "drop")

# 각 그룹별 점수 상위 20개 단어 추출
top_words <- word_scores %>%
  group_by(Group) %>%
  slice_max(Total_Score, n = 20, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(Word = reorder_within(Word, Total_Score, Group)) # 그래프 내림차순 정렬 장치

# 6. 대망의 비교 시각화 (막대 그래프)
p_compare <- ggplot(top_words, aes(x = Total_Score, y = Word, fill = Group)) +
  geom_col(show.legend = FALSE, color = "black", linewidth = 0.3, alpha = 0.9) +
  facet_wrap(~ Group, scales = "free_y") +
  scale_y_reordered() +
  
  # 파란색(진성 핀테크) vs 빨간색(노이즈) 대비
  scale_fill_manual(values = c("Retained (FinTech Core)" = "#2980B9", 
                               "Discarded (Noise / General IT)" = "#E74C3C")) +
  theme_bw(base_size = 14) +
  labs(
    title = "Semantic Filtering Impact: Retained vs. Discarded Patents",
    subtitle = "Top 20 Distinctive Keywords across K=20 Topics (Weighted by Rank)",
    x = "Aggregate Keyword Importance Score (Rank 1 = 15pt ... Rank 15 = 1pt)",
    y = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "gray50", hjust = 0.5, margin = margin(b=15)),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 15),
    axis.text.y = element_text(face = "bold", size = 12, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# 7. 화면 출력 및 고해상도 저장
print(p_compare)
ggsave("STM_Semantic_Comparison_K20.png", plot = p_compare, width = 15, height = 8, dpi = 300)
write.csv(top_words, "Semantic_Comparison_Top_Words_K20.csv", row.names = FALSE)

print("✅ [성공] 4.1.1 시멘틱 효과 대조 시각화 및 수치 데이터 저장 완료!")

# =====================================================================
# [4.1.3 보완] 토픽 수준 산점도 (v4 - 진단 강화)
# =====================================================================

library(tidyverse)
library(ggrepel)
library(scales)
library(stm)

# =========================================================
# STEP 1: 데이터 추출 및 진단
# =========================================================

# 1-1. theta 행렬 추출
cat("\n========== [STEP 1] 데이터 진단 ==========\n")
theta_raw <- final_stm_20_adj$theta
cat("① theta 행렬 차원:", dim(theta_raw), "\n")
cat("   theta 합계:", sum(theta_raw), "\n")
cat("   theta[1,1:5] 샘플:", head(theta_raw[1,], 5), "\n")

# 1-2. C_adj_i 확인
cat("\n② out3$meta 컬럼 목록:\n")
cat("  ", paste(names(out3$meta), collapse = ", "), "\n")

# C_adj_i가 있는지 확인
if ("C_adj_i" %in% names(out3$meta)) {
  c_adj_raw <- out3$meta$C_adj_i
  cat("③ C_adj_i 발견! 길이:", length(c_adj_raw), "\n")
  cat("   NA 개수:", sum(is.na(c_adj_raw)), "\n")
  cat("   샘플:", head(c_adj_raw[!is.na(c_adj_raw)], 5), "\n")
} else {
  # C_adj_i가 없으면 직접 계산
  cat("⚠️ C_adj_i가 out3$meta에 없습니다. 직접 계산합니다.\n")
  
  # 원본 데이터에서 피인용수 컬럼명 찾기
  cite_col_candidates <- c("Cited.by.Patent.Count", "Cited by Patent Count", 
                           "Cited_by_Patent_Count", "Family_Max_Cited")
  cite_col <- intersect(cite_col_candidates, names(out3$meta))
  
  if (length(cite_col) == 0) {
    cat("   가능한 피인용수 컬럼이 없습니다. 컬럼 목록을 확인하세요.\n")
    stop("피인용수 컬럼을 찾을 수 없습니다.")
  }
  
  cite_col <- cite_col[1]
  cat("   사용할 피인용수 컬럼:", cite_col, "\n")
  
  # 연도 컬럼 확인
  year_col_candidates <- c("Publication.Year", "Publication Year", "Publication_Year")
  year_col <- intersect(year_col_candidates, names(out3$meta))
  year_col <- year_col[1]
  cat("   사용할 연도 컬럼:", year_col, "\n")
  
  cited_counts <- as.numeric(out3$meta[[cite_col]])
  pub_years <- as.numeric(as.character(out3$meta[[year_col]]))
  
  # 연도별 평균 피인용수
  C_avg_j <- ave(cited_counts, pub_years, FUN = function(x) mean(x, na.rm = TRUE))
  
  # 조정 피인용수 계산
  c_adj_raw <- ifelse(C_avg_j == 0 | is.na(C_avg_j), 0, cited_counts / C_avg_j)
  
  cat("   C_adj_i 직접 계산 완료! 길이:", length(c_adj_raw), "\n")
  cat("   샘플:", head(c_adj_raw, 5), "\n")
}

# =========================================================
# STEP 2: NA 제거 및 동기화
# =========================================================
cat("\n========== [STEP 2] NA 제거 ==========\n")

valid_idx <- !is.na(c_adj_raw)
cat("④ valid_idx TRUE 개수:", sum(valid_idx), "/ 전체:", length(valid_idx), "\n")

if (sum(valid_idx) == 0) {
  stop("❌ c_adj가 전부 NA입니다! out3$meta의 피인용수/연도 데이터를 확인하세요.")
}

theta_mat <- theta_raw[valid_idx, ]
c_adj <- c_adj_raw[valid_idx]
colnames(theta_mat) <- paste0('T', 1:20)

cat("⑤ 필터 후 theta_mat 차원:", dim(theta_mat), "\n")
cat("   필터 후 c_adj 길이:", length(c_adj), "\n")
cat("   theta colSums 샘플:", head(colSums(theta_mat), 5), "\n")

# =========================================================
# STEP 3: 토픽별 X축, Y축 계산
# =========================================================
cat("\n========== [STEP 3] 지표 계산 ==========\n")

topic_summary <- data.frame(
  Topic = paste0('T', 1:20),
  Expected_Patent_Count = colSums(theta_mat),
  Weighted_Impact_Index = colSums(theta_mat * c_adj) / colSums(theta_mat),
  stringsAsFactors = FALSE
)

cat("⑥ Expected_Patent_Count 범위:", 
    round(min(topic_summary$Expected_Patent_Count), 1), "~", 
    round(max(topic_summary$Expected_Patent_Count), 1), "\n")
cat("   Weighted_Impact_Index 범위:", 
    round(min(topic_summary$Weighted_Impact_Index, na.rm=T), 3), "~", 
    round(max(topic_summary$Weighted_Impact_Index, na.rm=T), 3), "\n")

# =========================================================
# STEP 4: 클러스터 맵핑 및 시각화
# =========================================================

# 클러스터 직접 벡터 할당 (join 에러 방지)
topic_summary$Mega_Cluster <- c('E', 'H', 'B', 'A', 'G', 'E', 'F', 'E', 'C', 'G',
                                'A', 'E', 'C', 'D', 'F', 'H', 'C', 'B', 'D', 'H')

cluster_colors <- c(
  'A' = '#AEC6CF', 'B' = '#FFD1BA', 'C' = '#D8BFD8', 'D' = '#C1E1C1',
  'E' = '#FDFD96', 'F' = '#B0C4DE', 'G' = '#AFEEEE', 'H' = '#FFB6C1'
)

cluster_labels <- c(
  'A' = 'A. 기업 재무 및 회계 관리',
  'B' = 'B. 맞춤형 보험 및 타겟 식별',
  'C' = 'C. AI 기반 신용/투자 분석',
  'D' = 'D. 블록체인 기반 에너지 금융',
  'E' = 'E. 블록체인 및 디지털 자산',
  'F' = 'F. 보안 인증 및 규제 컴플라이언스',
  'G' = 'G. 뱅킹 및 대금 정산',
  'H' = 'H. O2O 상거래 및 간편 결제'
)

# CSV 저장
cat("\n===== 토픽별 산점도 수치 (파급력 내림차순) =====\n")
print(topic_summary %>% arrange(desc(Weighted_Impact_Index)))
write.csv(topic_summary, "STM_Topic_Landscape_Data.csv", row.names = FALSE)

# 시각화
p_topic_landscape <- ggplot(topic_summary, 
                            aes(x = Expected_Patent_Count, 
                                y = Weighted_Impact_Index)) +
  
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", 
             linewidth = 0.8, alpha = 0.6) +
  annotate("text", 
           x = min(topic_summary$Expected_Patent_Count) * 0.95, 
           y = 1.0 + (max(topic_summary$Weighted_Impact_Index) - 1.0) * 0.03 + 0.01,
           label = "Global Average Baseline (Index = 1.0)", 
           color = "red", hjust = 0, size = 3.8, fontface = "italic") +
  
  geom_point(aes(fill = Mega_Cluster), 
             size = 10, shape = 21, color = "gray30", 
             alpha = 0.85, stroke = 0.8) +
  
  geom_text_repel(
    aes(label = Topic), 
    fontface = "bold", size = 4.5, 
    max.overlaps = 25, 
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "gray60",
    segment.size = 0.3,
    min.segment.length = 0.2,
    seed = 2026
  ) +
  
  scale_fill_manual(values = cluster_colors, labels = cluster_labels, name = "Mega Clusters") +
  scale_x_continuous(labels = comma_format()) +
  
  theme_bw(base_size = 14) +
  labs(
    title = "Topic Landscape: Expected Patent Count vs. Qualitative Impact",
    subtitle = "Y-Axis reflects Time-Normalized Citation Index (Values > 1.0 indicate above-average impact)",
    x = "Expected Patent Count (Quantitative Volume)",
    y = "Weighted Average Citation Index (Qualitative Impact)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 5)))

print(p_topic_landscape)

ggsave("STM_Topic_Landscape_ImpactIndex.png", plot = p_topic_landscape, width = 13, height = 9, dpi = 300)
ggsave("STM_Topic_Landscape_ImpactIndex.pdf", plot = p_topic_landscape, width = 13, height = 9)

cat("\n✅ [성공] 토픽 수준 산점도 저장 완료!\n")

# =====================================================================
# [4.2.3 보완] TLC + TMR 종합 테이블 추출 (v2 - C_adj_i 안전장치 포함)
# =====================================================================

library(tidyverse)
library(stm)

# =========================================================
# STEP 1: theta 및 C_adj_i 추출 (진단 포함)
# =========================================================
cat("\n========== [STEP 1] 데이터 진단 ==========\n")

theta_raw <- final_stm_20_adj$theta
colnames(theta_raw) <- paste0('T', 1:20)
cat("① theta 행렬:", nrow(theta_raw), "행 ×", ncol(theta_raw), "열\n")

pub_year_raw <- as.numeric(as.character(out3$meta$Publication.Year))
cat("② Publication.Year 길이:", length(pub_year_raw), "/ NA:", sum(is.na(pub_year_raw)), "\n")

# C_adj_i 확인 또는 직접 계산
if ("C_adj_i" %in% names(out3$meta) && sum(!is.na(out3$meta$C_adj_i)) > 0) {
  c_adj_raw <- out3$meta$C_adj_i
  cat("③ C_adj_i 발견! 유효 값:", sum(!is.na(c_adj_raw)), "개\n")
} else {
  cat("⚠️ C_adj_i 없거나 전부 NA. 직접 계산합니다.\n")
  
  cite_col_candidates <- c("Cited.by.Patent.Count", "Cited by Patent Count",
                           "Cited_by_Patent_Count", "Family_Max_Cited")
  cite_col <- intersect(cite_col_candidates, names(out3$meta))
  
  if (length(cite_col) == 0) {
    stop("❌ 피인용수 컬럼을 찾을 수 없습니다. out3$meta 컬럼: ", 
         paste(names(out3$meta), collapse = ", "))
  }
  cite_col <- cite_col[1]
  cat("   피인용수 컬럼:", cite_col, "\n")
  
  cited_counts <- as.numeric(out3$meta[[cite_col]])
  C_avg_j <- ave(cited_counts, pub_year_raw, FUN = function(x) mean(x, na.rm = TRUE))
  c_adj_raw <- ifelse(C_avg_j == 0 | is.na(C_avg_j), 0, cited_counts / C_avg_j)
  cat("   C_adj_i 직접 계산 완료! 유효 값:", sum(!is.na(c_adj_raw)), "개\n")
}

# =========================================================
# STEP 2: NA 제거 및 동기화
# =========================================================
cat("\n========== [STEP 2] NA 제거 ==========\n")

valid_idx <- !is.na(c_adj_raw) & !is.na(pub_year_raw)
cat("④ valid_idx TRUE:", sum(valid_idx), "/ 전체:", length(valid_idx), "\n")

if (sum(valid_idx) == 0) {
  stop("❌ 유효한 데이터가 0건입니다!")
}

theta_mat <- theta_raw[valid_idx, ]
c_adj <- c_adj_raw[valid_idx]
pub_year <- pub_year_raw[valid_idx]

cat("⑤ 필터 후 — theta:", nrow(theta_mat), "행 / c_adj:", length(c_adj), 
    "/ pub_year:", length(pub_year), "\n")

# =========================================================
# STEP 3: TLC 3축 계산
# =========================================================
cat("\n========== [STEP 3] TLC 계산 ==========\n")

df_tlc <- data.frame(
  Topic = paste0('T', 1:20),
  Proportion = colMeans(theta_mat),
  Growth_Momentum = sapply(1:20, function(i) coef(lm(theta_mat[, i] ~ pub_year))[2]) * 100,
  Impact = colSums(theta_mat * c_adj) / colSums(theta_mat),
  stringsAsFactors = FALSE
)

cat("⑥ Proportion 범위:", round(min(df_tlc$Proportion)*100, 2), "% ~", 
    round(max(df_tlc$Proportion)*100, 2), "%\n")
cat("   Growth_Momentum 범위:", round(min(df_tlc$Growth_Momentum), 4), "~", 
    round(max(df_tlc$Growth_Momentum), 4), "\n")

# =========================================================
# STEP 4: 사분면 판정 (중앙값 기준)
# =========================================================
mid_x <- median(df_tlc$Proportion)
mid_y <- median(df_tlc$Growth_Momentum)

df_tlc$Quadrant <- case_when(
  df_tlc$Proportion >= mid_x & df_tlc$Growth_Momentum >= mid_y ~ "Q1: Mainstream",
  df_tlc$Proportion <  mid_x & df_tlc$Growth_Momentum >= mid_y ~ "Q2: Emerging",
  df_tlc$Proportion <  mid_x & df_tlc$Growth_Momentum <  mid_y ~ "Q3: Niche/Declining",
  df_tlc$Proportion >= mid_x & df_tlc$Growth_Momentum <  mid_y ~ "Q4: Saturated/Mature"
)

cat("⑦ 사분면 기준선 — X:", round(mid_x * 100, 2), "% / Y:", round(mid_y, 4), "\n")

# =========================================================
# STEP 5: 8대 클러스터 맵핑 + TMR 병합
# =========================================================
df_tlc$Mega_Cluster <- c('E', 'H', 'B', 'A', 'G', 'E', 'F', 'E', 'C', 'G',
                         'A', 'E', 'C', 'D', 'F', 'H', 'C', 'B', 'D', 'H')

cluster_labels <- c(
  'A' = '기업 재무 및 회계 관리', 'B' = '맞춤형 보험 및 타겟 식별',
  'C' = 'AI 기반 신용/투자 분석', 'D' = '블록체인 기반 에너지 금융',
  'E' = '블록체인 및 디지털 자산', 'F' = '보안 인증 및 규제 컴플라이언스',
  'G' = '뱅킹 및 대금 정산', 'H' = 'O2O 상거래 및 간편 결제'
)
df_tlc$Cluster_Name <- cluster_labels[df_tlc$Mega_Cluster]

# TMR 병합
if (file.exists("STM_Topic_TMR_Results.csv")) {
  tmr_df <- read.csv("STM_Topic_TMR_Results.csv", stringsAsFactors = FALSE)
  df_tlc <- df_tlc %>% left_join(tmr_df %>% select(Topic, TMR), by = "Topic")
  cat("⑧ TMR CSV 병합 완료\n")
} else {
  cat("⚠️ TMR CSV 없음. Gompertz 직접 계산.\n")
  tmr_results <- numeric(20)
  for(i in 1:20) {
    yearly_counts <- tapply(theta_mat[, i], pub_year, sum)
    years_seq <- 1:length(yearly_counts)
    cumulative_counts <- cumsum(yearly_counts)
    fit <- tryCatch({
      nls(cumulative_counts ~ SSgompertz(years_seq, Asym, b2, b3))
    }, error = function(e) NULL)
    if(!is.null(fit)) {
      tmr_results[i] <- max(cumulative_counts) / coef(fit)["Asym"]
    } else {
      tmr_results[i] <- NA
    }
  }
  df_tlc$TMR <- tmr_results
  cat("⑧ TMR 직접 계산 완료\n")
}

# =========================================================
# STEP 6: 최종 테이블 정리 및 저장
# =========================================================
df_export <- df_tlc %>%
  mutate(
    Proportion_pct = round(Proportion * 100, 2),
    Growth_Momentum = round(Growth_Momentum, 4),
    Impact = round(Impact, 3),
    TMR_pct = round(TMR * 100, 1)
  ) %>%
  select(Topic, Mega_Cluster, Cluster_Name,
         Proportion_pct, Growth_Momentum, Impact, TMR_pct, Quadrant) %>%
  arrange(Quadrant, desc(Growth_Momentum))

cat("\n===== TLC + TMR 종합 테이블 =====\n")
print(df_export)

write.csv(df_export, "STM_TLC_TMR_Combined_Table.csv", row.names = FALSE)
cat("\n✅ 종합 테이블 저장 완료 (STM_TLC_TMR_Combined_Table.csv)\n")

# =====================================================================
# [4.3.2 보완] Benjamini-Hochberg (FDR) 다중비교 보정
# 기존 80개 국가별 사후분석 p-value에 FDR 보정 적용
# + 보정 전후 비교 CSV + 보정 후 히트맵 시각화
# =====================================================================

library(tidyverse)

# =========================================================
# 1. 기존 사후분석 결과 불러오기
# =========================================================
# ⚠️ 파일 인코딩: CP949 또는 UTF-8 중 맞는 것으로 설정
df <- tryCatch(
  read.csv("Topic_Country_Effect_US_Baseline.csv", fileEncoding = "CP949", stringsAsFactors = FALSE),
  error = function(e) read.csv("Topic_Country_Effect_US_Baseline.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
)

cat("✅ 데이터 로드 완료:", nrow(df), "행\n")
cat("   컬럼:", paste(names(df), collapse = ", "), "\n")

# p_value 컬럼명 확인 (Pr...t.. 또는 p_value 등 다양한 형태 대응)
p_col <- intersect(c("p_value", "Pr...t..", "Pr(>|t|)"), names(df))
if (length(p_col) == 0) {
  # 컬럼명에 p가 들어간 것 찾기
  p_col <- grep("^p|Pr", names(df), value = TRUE)
}
p_col <- p_col[1]
cat("   p-value 컬럼:", p_col, "\n")

# 통일된 컬럼명으로 변환
df$p_value_raw <- as.numeric(df[[p_col]])

# =========================================================
# 2. Benjamini-Hochberg (FDR) 보정 적용
# =========================================================
df$p_value_FDR <- p.adjust(df$p_value_raw, method = "BH")

# 참고용: Bonferroni도 함께 산출
df$p_value_Bonferroni <- p.adjust(df$p_value_raw, method = "bonferroni")

cat("\n✅ FDR 보정 완료!\n")
cat("   총 비교 수:", nrow(df), "\n")

# =========================================================
# 3. 보정 전후 유의성 별표 부여
# =========================================================
assign_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ ".",
    TRUE      ~ ""
  )
}

df$Sig_Raw <- assign_stars(df$p_value_raw)
df$Sig_FDR <- assign_stars(df$p_value_FDR)
df$Sig_Bonferroni <- assign_stars(df$p_value_Bonferroni)

# 보정 전후 변화 표시
df$FDR_Change <- ifelse(df$Sig_Raw != df$Sig_FDR, 
                        paste0(df$Sig_Raw, " → ", df$Sig_FDR), 
                        "변화 없음")

# =========================================================
# 4. 보정 전후 비교 요약 출력
# =========================================================
cat("\n===== 보정 전후 유의성 변화 요약 =====\n")
cat("변화 없음:", sum(df$FDR_Change == "변화 없음"), "건\n")
cat("유의성 변경:", sum(df$FDR_Change != "변화 없음"), "건\n\n")

# 변경된 항목만 출력
changed <- df %>% filter(FDR_Change != "변화 없음")
if (nrow(changed) > 0) {
  cat("--- 변경된 항목 ---\n")
  print(changed %>% select(Topic, Topic_Label, Comparison, 
                           p_value_raw, p_value_FDR, FDR_Change))
} else {
  cat("모든 항목의 유의성이 보정 후에도 유지되었습니다.\n")
}

# =========================================================
# 5. CSV 저장 (전체 결과 + 보정값 포함)
# =========================================================

# Comparison 또는 Term 컬럼 통일
comp_col <- intersect(c("Comparison", "Term"), names(df))[1]

export_df <- df %>%
  select(Topic, Topic_Label, 
         Comparison = all_of(comp_col),
         Estimate, 
         p_value_raw, Sig_Raw,
         p_value_FDR, Sig_FDR,
         p_value_Bonferroni, Sig_Bonferroni,
         FDR_Change)

write.csv(export_df, "STM_US_Baseline_FDR_Corrected.csv", row.names = FALSE)
cat("\n✅ CSV 저장 완료: STM_US_Baseline_FDR_Corrected.csv\n")

# =========================================================
# 6. FDR 보정 후 히트맵 시각화
# =========================================================

# 히트맵용 데이터 준비 (기존 히트맵 코드와 동일 구조)
# Country 추출 (Comparison에서 "KR vs US" → "KR")
df$Country <- gsub(" vs US", "", df[[comp_col]])

# 8대 클러스터 맵핑
cluster_map <- data.frame(
  Topic = 1:20,
  Cluster = c('E. 블록체인 및 디지털 자산', 'H. O2O 상거래 및 간편 결제',
              'B. 맞춤형 보험 및 타겟 식별', 'A. 기업 재무 및 회계 관리',
              'G. 뱅킹 및 대금 정산', 'E. 블록체인 및 디지털 자산',
              'F. 보안 인증 및 규제 컴플라이언스', 'E. 블록체인 및 디지털 자산',
              'C. AI 기반 신용/투자 분석', 'G. 뱅킹 및 대금 정산',
              'A. 기업 재무 및 회계 관리', 'E. 블록체인 및 디지털 자산',
              'C. AI 기반 신용/투자 분석', 'D. 블록체인 기반 에너지 금융',
              'F. 보안 인증 및 규제 컴플라이언스', 'H. O2O 상거래 및 간편 결제',
              'C. AI 기반 신용/투자 분석', 'B. 맞춤형 보험 및 타겟 식별',
              'D. 블록체인 기반 에너지 금융', 'H. O2O 상거래 및 간편 결제'),
  stringsAsFactors = FALSE
)

df$T_Num <- paste0("T", df$Topic)
df <- df %>% left_join(cluster_map, by = "Topic")

# FDR 보정 후 7단계 범주 (기존 히트맵과 동일 구조)
df_heat <- df %>%
  mutate(
    Fill_Category = case_when(
      p_value_FDR < 0.001 & Estimate > 0 ~ "Target (+) : p < 0.001 (***)",
      p_value_FDR < 0.01  & Estimate > 0 ~ "Target (+) : p < 0.01 (**)",
      p_value_FDR < 0.05  & Estimate > 0 ~ "Target (+) : p < 0.05 (*)",
      p_value_FDR < 0.001 & Estimate < 0 ~ "US (-) : p < 0.001 (***)",
      p_value_FDR < 0.01  & Estimate < 0 ~ "US (-) : p < 0.01 (**)",
      p_value_FDR < 0.05  & Estimate < 0 ~ "US (-) : p < 0.05 (*)",
      TRUE ~ "Not Significant"
    )
  )

df_heat$Fill_Category <- factor(df_heat$Fill_Category, levels = c(
  "Target (+) : p < 0.001 (***)",
  "Target (+) : p < 0.01 (**)",
  "Target (+) : p < 0.05 (*)",
  "Not Significant",
  "US (-) : p < 0.05 (*)",
  "US (-) : p < 0.01 (**)",
  "US (-) : p < 0.001 (***)"
))

# Y축 정렬
cluster_order <- c('A. 기업 재무 및 회계 관리', 'B. 맞춤형 보험 및 타겟 식별',
                   'C. AI 기반 신용/투자 분석', 'D. 블록체인 기반 에너지 금융',
                   'E. 블록체인 및 디지털 자산', 'F. 보안 인증 및 규제 컴플라이언스',
                   'G. 뱅킹 및 대금 정산', 'H. O2O 상거래 및 간편 결제')
df_heat$Cluster <- factor(df_heat$Cluster, levels = cluster_order)

topic_levels <- df_heat %>%
  mutate(Topic_Int = as.numeric(gsub("T", "", T_Num))) %>%
  arrange(desc(Cluster), desc(Topic_Int)) %>%
  pull(T_Num) %>%
  unique()
df_heat$T_Num <- factor(df_heat$T_Num, levels = topic_levels)

df_heat$Country <- factor(df_heat$Country, levels = c("KR", "CN", "JP", "EP"))

# 7단계 파스텔 팔레트 (기존과 동일)
pastel_shades <- c(
  "Target (+) : p < 0.001 (***)" = "#5DADE2",
  "Target (+) : p < 0.01 (**)"   = "#85C1E9",
  "Target (+) : p < 0.05 (*)"    = "#D4E6F1",
  "Not Significant"              = "#F8F9F9",
  "US (-) : p < 0.05 (*)"        = "#FADBD8",
  "US (-) : p < 0.01 (**)"       = "#F5B7B1",
  "US (-) : p < 0.001 (***)"     = "#EC7063"
)

# 히트맵 그리기
p_heatmap_fdr <- ggplot(df_heat, aes(x = Country, y = T_Num)) +
  geom_tile(aes(fill = Fill_Category), color = "white", linewidth = 1) +
  geom_text(aes(label = Sig_FDR), color = "gray30", size = 5.5, vjust = 0.75) +
  facet_grid(Cluster ~ ., scales = "free_y", space = "free_y") +
  
  scale_fill_manual(values = pastel_shades, drop = FALSE, guide = guide_legend(nrow = 2)) +
  
  theme_minimal(base_size = 13) +
  labs(
    title = "Strategic Gap Heatmap (FDR-Corrected): Global Competitors vs. US Baseline",
    subtitle = "Benjamini-Hochberg correction applied to 80 comparisons (20 topics × 4 countries)",
    x = "Comparison Countries (vs. US)",
    y = "Topics (Grouped by Clusters)",
    fill = "Dominance & Significance"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray50", hjust = 0.5, margin = margin(b = 15)),
    strip.text.y = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_text(face = "bold", size = 14, color = "black"),
    axis.text.y = element_text(face = "bold", size = 11, color = "gray30"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", margin = margin(r = 15)),
    legend.text = element_text(size = 10)
  )

print(p_heatmap_fdr)

ggsave("STM_Strategic_Gap_Heatmap_FDR.png", plot = p_heatmap_fdr, width = 13, height = 10, dpi = 300)
ggsave("STM_Strategic_Gap_Heatmap_FDR.pdf", plot = p_heatmap_fdr, width = 13, height = 10)

cat("\n✅ [성공] FDR 보정 히트맵 저장 완료!\n")
cat("   - STM_Strategic_Gap_Heatmap_FDR.png\n")
cat("   - STM_Strategic_Gap_Heatmap_FDR.pdf\n")
cat("   - STM_US_Baseline_FDR_Corrected.csv\n")

# =========================================================
# 4.1.1 시멘틱 필터링 정량적 검증: 유사도 분포 분리 분석
# =========================================================
library(tidyverse)

# 1. 데이터 로드
df <- read.csv("full_patents_with_scores.csv", stringsAsFactors = FALSE)

# 2. Net Score 계산 (시드 유사도 - 안티시드 유사도)
df <- df %>%
  mutate(net_score = top_k_pos_avg - top_k_neg_avg)

# 3. 기초 통계량
stats <- df %>%
  group_by(is_fintech) %>%
  summarise(
    n = n(),
    mean_pos = mean(top_k_pos_avg, na.rm = TRUE),
    mean_neg = mean(top_k_neg_avg, na.rm = TRUE),
    mean_net = mean(net_score, na.rm = TRUE),
    median_net = median(net_score, na.rm = TRUE),
    sd_net = sd(net_score, na.rm = TRUE)
  )
print(stats)
write.csv(stats, "Semantic_Filter_Score_Stats.csv", row.names = FALSE)

# 4. 통계적 검증
# Welch's t-test
t_result <- t.test(net_score ~ is_fintech, data = df)
print(t_result)

# Mann-Whitney U test (비모수)
w_result <- wilcox.test(net_score ~ is_fintech, data = df)
print(w_result)

# Cohen's d (효과 크기)
fintech_net <- df$net_score[df$is_fintech == TRUE]
noise_net   <- df$net_score[df$is_fintech == FALSE]
pooled_sd   <- sqrt((sd(fintech_net)^2 + sd(noise_net)^2) / 2)
cohens_d    <- (mean(fintech_net) - mean(noise_net)) / pooled_sd
cat(sprintf("\n✅ Cohen's d = %.3f\n", cohens_d))

# 5. 시각화: Net Score 히스토그램 (두 그룹 겹침)
p <- ggplot(df, aes(x = net_score, fill = is_fintech)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 80, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_fill_manual(
    values = c("TRUE" = "#378ADD", "FALSE" = "#E24B4A"),
    labels = c("TRUE" = "Genuine FinTech (21,381)", 
               "FALSE" = "Noise (50,282)")
  ) +
  labs(
    title = "Cosine Similarity Net Score Distribution",
    subtitle = "Net Score = Avg. Similarity to Seeds − Avg. Similarity to Anti-seeds",
    x = "Net Score (Seed Proximity − Anti-seed Proximity)",
    y = "Density",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray50", hjust = 0.5),
    legend.position = "top"
  )

ggsave("Semantic_Filter_NetScore_Distribution.png", plot = p, 
       width = 10, height = 6, dpi = 300)
print("✅ 시각화 저장 완료")

# ============================================================
# TLC Portfolio Matrix — 8대 메가 클러스터(A~H) 버전
# ============================================================
# 필요 패키지
library(ggplot2)
library(ggrepel)

# ---- 1) 데이터 직접 입력 (STM_TLC_TMR_Combined_Table 기준) ----
tlc <- data.frame(
  Topic = c("T17","T9","T11","T7","T8","T15","T13","T3","T12","T4",
            "T6","T19","T16","T20","T1","T18","T10","T5","T2","T14"),
  Label = c("AI 분석/탐지","투자 분석","기업 자금 분석","보안 및 인증","디지털 자산/NFT",
            "데이터 기록 관리","대출 및 신용","보험 서비스","분산 원장 자산","기업 회계/사무",
            "디지털 화폐","모빌리티 핀테크","온라인 상거래","사용자 인식","가상자산 거래",
            "계정 식별/분류","결제 및 정산","은행 금융 서비스","모바일 결제","스마트 계약"),
  Cluster = c("C","C","A","F","E","F","C","B","E","A",
              "E","D","H","H","E","B","G","G","H","D"),
  Proportion = c(7.37, 4.81, 6.51, 9.57, 4.56, 4.53, 3.87, 2.42, 2.52, 3.60,
                 2.65, 3.58, 3.76, 3.62, 4.34, 8.12, 4.97, 6.38, 7.03, 5.79),
  Growth = c(0.96, 0.72, 0.43, 0.19, 0.16, 0.22, 0.20, 0.14, 0.10, 0.05,
             -0.07, -0.09, -0.21, -0.25, -0.29, -0.06, -0.36, -0.51, -0.58, -0.76),
  Impact = c(1.318, 0.960, 1.013, 1.339, 1.228, 1.429, 1.063, 0.788, 1.603, 0.725,
             1.221, 1.026, 0.608, 0.715, 1.235, 0.661, 0.492, 0.663, 0.729, 1.100),
  stringsAsFactors = FALSE
)

# ---- 2) 클러스터 라벨 & 색상 (A~H, 기존 HEX) ----
cluster_info <- data.frame(
  Cluster = c("A","B","C","D","E","F","G","H"),
  Cluster_Label = c("A. 기업 재무 및 회계 관리",
                    "B. 맞춤형 보험 및 타겟 식별",
                    "C. AI 기반 신용/투자 분석",
                    "D. 블록체인 기반 에너지 금융",
                    "E. 블록체인 및 디지털 자산",
                    "F. 보안 인증 및 규제 컴플라이언스",
                    "G. 뱅킹 및 대금 정산",
                    "H. O2O 상거래 및 간편 결제"),
  Color = c("#AEC6CF","#FFD1BA","#D8BFD8","#C1E1C1","#FDFD96","#B0C4DE","#AFEEEE","#FFB6C1"),
  stringsAsFactors = FALSE
)

tlc <- merge(tlc, cluster_info, by = "Cluster")

# ---- 3) 사분면 기준선 (중앙값) ----
med_x <- median(tlc$Proportion)
med_y <- median(tlc$Growth)

# ---- 4) 시각화 ----
color_map <- setNames(cluster_info$Color, cluster_info$Cluster_Label)

p <- ggplot(tlc, aes(x = Proportion, y = Growth)) +
  # 사분면 기준선
  geom_hline(yintercept = med_y, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = med_x, linetype = "dashed", color = "grey40") +
  # 버블 (크기 = 파급력)
  geom_point(aes(size = Impact, fill = Cluster_Label),
             shape = 21, alpha = 0.85, stroke = 0.5) +
  # 토픽 라벨
  geom_text_repel(aes(label = Topic), size = 3.5, fontface = "bold",
                  max.overlaps = 20, seed = 42) +
  # 스케일
  scale_size_continuous(range = c(4, 18), name = "Qualitative Impact\n(Weighted Citation Index)") +
  scale_fill_manual(values = color_map, name = "Mega Clusters") +
  # 사분면 라벨
  annotate("text", x = max(tlc$Proportion) * 0.95, y = max(tlc$Growth) * 0.95,
           label = "Q1: Mainstream\n(High Share, High Growth)",
           hjust = 1, vjust = 1, size = 3, color = "red", fontface = "bold") +
  annotate("text", x = min(tlc$Proportion) * 1.02, y = max(tlc$Growth) * 0.95,
           label = "Q2: Emerging / Breakthrough\n(Low Share, High Growth)",
           hjust = 0, vjust = 1, size = 3, color = "red", fontface = "bold") +
  annotate("text", x = min(tlc$Proportion) * 1.02, y = min(tlc$Growth) * 0.95,
           label = "Q3: Niche / Declining\n(Low Share, Low Growth)",
           hjust = 0, vjust = 0, size = 3, color = "red", fontface = "bold") +
  annotate("text", x = max(tlc$Proportion) * 0.95, y = min(tlc$Growth) * 0.95,
           label = "Q4: Saturated / Mature\n(High Share, Low Growth)",
           hjust = 1, vjust = 0, size = 3, color = "red", fontface = "bold") +
  # 축 & 제목
  labs(
    title = "Technology Life Cycle (TLC) Portfolio Matrix",
    subtitle = "Bubble size represents Qualitative Impact (Weighted Citation Index)",
    x = "Market Dominance (Topic Proportion, %)",
    y = "Growth Acceleration (Time-Series Theta Slope)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey30"),
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 5)))

# ---- 5) 저장 ----
ggsave("STM_TLC_Portfolio_Matrix_8Clusters.png", p, width = 14, height = 10, dpi = 300)
cat("저장 완료: STM_TLC_Portfolio_Matrix_8Clusters.png\n")

