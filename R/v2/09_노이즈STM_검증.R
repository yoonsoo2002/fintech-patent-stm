# ==============================================================================
#  09_노이즈STM_검증.R — ¶281·¶365 정성 검증용 (v2)
#  필터링 제거분(노이즈 61,781건)에 STM(K=20) 적용 → 토픽이 비금융 주제인지 확인
#  출력: NOISE_K20_FREX_Words.csv
# ==============================================================================
source("00_config.R")
library(stm); library(stringr); library(stopwords); library(dplyr)

nd <- read.csv("full_patents_with_scores.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
cat("스코어 파일:", nrow(nd), "건\n")
flag_col <- intersect(c("is_fintech","Is_Fintech"), names(nd))[1]
noise <- nd[nd[[flag_col]] %in% c(FALSE,"False","FALSE"), ]
cat("노이즈:", nrow(noise), "건\n")
noise$text_tt <- paste(noise$Title, noise$Title, noise$Abstract, sep = " ")

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
  "10","10s","100","100s","200","200s")
csw <- c(stwds, custom_ultimate)

proc <- textProcessor(documents = noise$text_tt, metadata = noise,
                      lowercase = TRUE, removepunctuation = TRUE,
                      customstopwords = csw, removestopwords = TRUE,
                      removenumbers = FALSE, stem = TRUE, wordLengths = c(2, Inf))
lt <- round(length(proc$documents) * 0.01)
out <- prepDocuments(proc$documents, proc$vocab, proc$meta, lower.thresh = lt)
cat("살아남은 어휘:", length(out$vocab), "\n")

set.seed(SEED_NUM)
m <- stm(documents = out$documents, vocab = out$vocab, K = 20,
         data = out$meta, max.em.its = 75, init.type = "Spectral")
lab <- labelTopics(m, n = 12)
frex <- apply(lab$frex, 1, paste, collapse = ", ")
prop <- round(colMeans(m$theta), 4)
write.csv(data.frame(Topic = paste0("N", 1:20), Proportion = prop, FREX = frex),
          "NOISE_K20_FREX_Words.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n================  노이즈 토픽 핵심어  ================\n")
for (i in 1:20) cat(sprintf("N%-2d (%.3f) | %s\n", i, prop[i], frex[i]))
cat("✅ 09 완료 → NOISE_K20_FREX_Words.csv\n")
