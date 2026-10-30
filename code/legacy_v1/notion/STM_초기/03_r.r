# [1] K=40으로 임시 STM 모델 학습
# 앞서 전처리한 out1 데이터를 그대로 사용합니다.
set.seed(2026) 
temp_stm_40 <- stm(documents = out1$documents, 
                   vocab = out1$vocab, 
                   K = 40, 
                   prevalence = ~ Origin + s(Publication.Year) + log1p(Citations),
                   data = out1$meta,
                   max.em.its = 75, # 임시 확인용이므로 반복 횟수는 적당히 유지
                   init.type = "Spectral")

# [2] 토픽별 상위 15개 단어 확인 (R 콘솔에서 바로 보기)
# 이 코드를 실행하면 각 토픽(1~40)별로 출현 확률이 높은 단어들이 출력됩니다.
labelTopics(temp_stm_40, n = 15)

# [3] 결과를 엑셀(csv) 파일로 저장하여 편하게 검토하기 (추천)
# R 콘솔 창은 좁아서 보기 힘드니, 엑셀 파일로 빼서 노이즈 단어를 형광펜 칠하며 찾는 것이 좋습니다.
topic_words <- labelTopics(temp_stm_40, n = 15)

# 확률(Probability) 기준으로 정렬된 상위 단어들을 데이터프레임으로 변환 후 저장
top_words_df <- as.data.frame(topic_words$prob)
write.csv(top_words_df, "temp_k40_top_words_review.csv", row.names = TRUE)
