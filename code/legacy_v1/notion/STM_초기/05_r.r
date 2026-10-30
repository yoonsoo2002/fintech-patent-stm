# [6] 대망의 최종 STM 모델 학습 (예: K=35로 결정 시)
# max.em.its를 150 이상으로 넉넉히 주어 모델이 완전히 수렴(converge)하도록 합니다.
set.seed(2026)
final_stm <- stm(documents = out2$documents, 
                 vocab = out2$vocab, 
                 K = 35, # 여기서 최종 결정한 K 값을 넣어줍니다.
                 prevalence = ~ Origin + s(Publication.Year) + log1p(Citations),
                 data = out2$meta,
                 max.em.its = 150, # 최종 모델이므로 반복 횟수 증가
                 init.type = "Spectral")

# 짝짝짝! 드디어 최종 토픽 35개가 뽑혔습니다.
# 최종 결과 저장 및 확인
final_topic_words <- labelTopics(final_stm, n = 15)
write.csv(as.data.frame(final_topic_words$prob), "Final_STM_Topics_K35.csv", row.names = TRUE)
