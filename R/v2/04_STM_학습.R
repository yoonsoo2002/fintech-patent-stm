# ==============================================================================
#  04_STM_학습.R  —  [단계 6·7] STM(K=20) 학습 + ★토픽 정체성 재매핑 템플릿 생성
# ==============================================================================
#  입력 : final_fintech_patents_filtered.csv (02 산출)
#  출력 : STM_full_workspace_Phase0.RData  (★05·06·07이 load)
#         FINAL_K20_FREX_Words.csv / FINAL_K20_PROB_Words.csv
#         topic_mapping_TEMPLATE.csv  ← ★사람이 채워 topic_mapping.csv 로 저장
#
#  v1(Fintech_Project.R Phase0)을 충실히 옮기되, 가장 큰 위험을 구조적으로 차단:
#   ▶ v2로 재학습하면 토픽번호↔의미가 완전히 바뀐다. v1은 라벨이 코드 곳곳에
#     하드코딩돼 "에러 없이 그림 라벨만 틀리는" 사고가 1순위 위험이었다.
#   ▶ 해결: 라벨/클러스터를 코드에서 빼고 topic_mapping.csv 한 파일로 분리.
#     04가 FREX 단어로 템플릿을 자동 생성 → 사람이 라벨·클러스터 채움 → 05/06/07이 읽음.
# ==============================================================================

source("00_config.R")   # WORK_DIR, K_TOPICS, SEED_NUM, PREVALENCE_FORMULA 등

# install.packages(c("stm","stringr","stopwords","dplyr","tidyr"))  # 최초 1회
library(stm); library(stringr); library(stopwords); library(dplyr); library(tidyr)

# ── 1) 로드 + 텍스트 결합 (★STM은 Title 2회 가중) ──────────────────────────
mydata <- read.csv(FILE_FILTERED, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
cat("입력:", nrow(mydata), "건\n")
stopifnot(all(c("Title","Abstract","Origin_Country","Publication.Year","Cited.by.Patent.Count") %in% names(mydata)))
mydata$text_tt <- paste(mydata$Title, mydata$Title, mydata$Abstract, sep = " ")  # Title 2배

# ── 2) 특허 불용어 사전 (v1 동일 — 재현성 위해 그대로) ─────────────────────
stwds <- stopwords(language = "en", source = "smart")
custom_ultimate <- c(
  'comprise','comprises','comprised','comprising','compris',
  'include','includes','included','including','inclusion','inclusions','includ',
  'provide','provides','provided','providing','provision','provisions','provid',
  'configure','configures','configured','configuring','configuration','configurations','configur',
  'contain','contains','contained','containing','container','containers',
  'perform','performs','performed','performing','performance','performances',
  'generate','generates','generated','generating','generation','generations','generator','generators','generat',
  'determine','determines','determined','determining','determination','determinations','determin',
  'obtain','obtains','obtained','obtaining','obtainable',
  'receive','receives','received','receiving','receiver','receivers','receipt','receipts','receiv',
  'send','sends','sent','sending','sender','senders',
  'transmit','transmits','transmitted','transmitting','transmission','transmissions','transmitter','transmitters',
  'store','stores','stored','storing','storage','storages',
  'process','processes','processed','processing','processor','processors',
  'execute','executes','executed','executing','execution','executions','executable','execut',
  'calculate','calculates','calculated','calculating','calculation','calculations','calculator','calculators','calcul',
  'apply','applies','applied','applying','application','applications','applicable',
  'relate','relates','related','relating','relation','relations','relative','relatively','relat',
  'base','bases','based','basing','basis',
  'present','presents','presented','presenting','presentation','presentations','presently',
  'disclose','discloses','disclosed','disclosing','disclosure','disclosures','disclos',
  'step','steps','stepped','stepping','mean','means','meaning','meanings',
  'draw','draws','drew','drawn','drawing','drawings','figure','figures','figured','figuring','figur',
  'part','parts','parted','parting','partly','piece','pieces','pieced','piecing','piec',
  'field','fields','technique','techniques','technical','technically','technic',
  'background','backgrounds','result','results','resulted','resulting',
  'require','requires','required','requiring','requirement','requirements','requir',
  'prefer','prefers','preferred','preferring','preference','preferences','preferably',
  'medium','mediums','media',
  'invention','thereof','therefore','therefrom','system','systems','method','methods',
  'apparatus','plurality','embodiment','embodiments',
  "10","10s","100","100s","200","200s"
)
csw <- c(stwds, custom_ultimate)

# ── 3) 전처리 + 희귀어 컷 ──────────────────────────────────────────────────
proc <- textProcessor(documents = mydata$text_tt, metadata = mydata,
                      lowercase = TRUE, removepunctuation = TRUE,
                      customstopwords = csw, removestopwords = TRUE,
                      removenumbers = FALSE, stem = TRUE, wordLengths = c(2, Inf))

# lower.thresh: 고정값 있으면 그것, 없으면 전체문서의 1%로 자동 (v2 문서수에 맞춰 재조정)
lt <- if (!is.na(LOWER_THRESH_FIXED)) LOWER_THRESH_FIXED else round(length(proc$documents) * LOWER_THRESH_RATIO)
cat(sprintf("lower.thresh = %d  (전체 %d건의 %.1f%%)\n", lt, length(proc$documents), lt/length(proc$documents)*100))
out3 <- prepDocuments(proc$documents, proc$vocab, proc$meta, lower.thresh = lt)
cat("살아남은 어휘:", length(out3$vocab), "\n")

# ── 4) 시간보정 피인용지수 C_adj_i (연도평균 대비) ─────────────────────────
out3$meta$C_avg_j <- ave(out3$meta$Cited.by.Patent.Count, out3$meta$Publication.Year,
                         FUN = function(x) mean(x, na.rm = TRUE))
out3$meta$C_adj_i <- ifelse(out3$meta$C_avg_j == 0, 0,
                            out3$meta$Cited.by.Patent.Count / out3$meta$C_avg_j)

# ── 5) STM 학습 (K=20) ─────────────────────────────────────────────────────
set.seed(SEED_NUM)
final_stm_20_adj <- stm(documents = out3$documents, vocab = out3$vocab,
                        K = K_TOPICS, prevalence = PREVALENCE_FORMULA,
                        data = out3$meta, max.em.its = MAX_EM_ITS, init.type = INIT_TYPE)
save.image(FILE_WORKSPACE)
cat("💾 STM 학습 완료 →", FILE_WORKSPACE, "\n")

# ── 6) ★토픽 정체성: labelTopics + 템플릿 자동생성 ─────────────────────────
lab <- labelTopics(final_stm_20_adj, n = 12)
frex <- apply(lab$frex, 1, paste, collapse = ", ")
prob <- apply(lab$prob, 1, paste, collapse = ", ")
write.csv(data.frame(Topic = paste0("T", 1:K_TOPICS), FREX = frex),
          "FINAL_K20_FREX_Words.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(Topic = paste0("T", 1:K_TOPICS), PROB = prob),
          "FINAL_K20_PROB_Words.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\n================  각 토픽 핵심어 (이걸 보고 라벨을 정하세요)  ================\n")
for (i in 1:K_TOPICS) cat(sprintf("T%-2d | FREX: %s\n", i, frex[i]))

# 토픽 비중(라벨 결정 참고용)
prop <- colMeans(final_stm_20_adj$theta)

# 사람이 채울 매핑 템플릿 (Label_EN/Label_KR/Cluster 빈칸)
template <- data.frame(
  Topic    = paste0("T", 1:K_TOPICS),
  FREX     = frex,
  Proportion = round(prop, 4),
  Label_EN = "",      # ← FREX 보고 채우기 (예: "AI Detection")
  Label_KR = "",      # ← (예: "AI 분석/탐지")
  Cluster  = "",      # ← A~H 중 하나 (00_palette.R 클러스터 정의 참고)
  stringsAsFactors = FALSE
)
write.csv(template, "topic_mapping_TEMPLATE.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\n★ 다음 할 일 (반드시):\n")
cat("  1) topic_mapping_TEMPLATE.csv 를 열어 Label_EN / Label_KR / Cluster(A~H) 20행 채우기\n")
cat("     - FREX 단어 + (참고로 v1 토픽 리스트.txt) 보고 의미 판정\n")
cat("     - 동시에 v1↔v2 토픽 대조표를 위키 _v2갱신_체크리스트 STEP0 에 기록\n")
cat("  2) 채운 파일을 topic_mapping.csv 로 저장 (같은 폴더)\n")
cat("  3) 그래야 05·06·07 스크립트가 올바른 라벨·색상으로 그림을 그림\n")
cat("  ⚠️ 이 단계를 건너뛰면 그림 라벨이 '에러 없이' 전부 틀립니다 (v2 1순위 위험).\n")
