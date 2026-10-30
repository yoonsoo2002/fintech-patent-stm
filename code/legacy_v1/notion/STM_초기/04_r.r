# [1] 기존 불용어에 발견된 노이즈 단어 대거 추가
stwds <- stopwords(language = "en", source = "smart")

# 우리가 눈으로 확인했다고 가정한 노이즈 단어들 (필요시 더 추가/삭제 가능)
found_noise <- c('system', 'method', 'device', 'apparatus', 'plurality', 'embodiment', 
                 'first', 'second', 'include', 'comprise', 'provide', 'receive', 
                 'step', 'information', 'user', 'data', 'invention', 'thereof')

csw_final <- c(stwds, found_noise)

# [2] 2차 전처리 실행 (불용어 사전만 csw_final로 교체하여 다시 돌림)
# 코딩이 복잡해지는 것을 막기 위해 옵션은 1차와 완벽히 동일하게 유지합니다.
proc2 <- textProcessor(documents = mydata5$text_tt, 
                       metadata = mydata5, 
                       lowercase = TRUE, 
                       removepunctuation = TRUE, 
                       customstopwords = csw_final, 
                       removestopwords = TRUE, 
                       removenumbers = FALSE, # Web 3.0 등 숫자 유지
                       stem = TRUE, 
                       wordLengths = c(2,Inf))

# [3] 희귀 단어 제거 (1% 기준: 214건)
# 이 과정을 거치면 노이즈가 싹 빠진 "진짜 초순도" 텍스트 뭉치가 완성됩니다.
out2 <- prepDocuments(proc2$documents, 
                      proc2$vocab, 
                      proc2$meta, 
                      lower.thresh = 214)

# ------------------------------------------------------------------------
# [4] 좁은 범위(Narrow Search)에서 최종 K 탐색
# 1차 탐색에서 40 부근이 좋았으므로, 30~50 구간을 5 단위로 정밀 타격합니다.
set.seed(2026) 
kresult_stage2 <- searchK(documents = out2$documents, 
                          vocab = out2$vocab, 
                          K = seq(30, 50, by = 5), # 30, 35, 40, 45, 50 테스트
                          prevalence = ~ Origin + s(Publication.Year) + log1p(Citations),
                          data = out2$meta,
                          max.em.its = 75,
                          init.type = "Spectral")

# [5] 지표 그래프 최종 확인
plot(kresult_stage2)
