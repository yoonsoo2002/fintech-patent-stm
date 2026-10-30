# [1] K값 탐색을 위한 시드 고정 및 searchK 실행
set.seed(2026) 

# K를 10부터 80까지 10 단위로 탐색 (10, 20, 30, 40, 50, 60, 70, 80)
# out1은 이전 전처리 단계에서 생성된 객체입니다.
kresult_stage1 <- searchK(documents = out1$documents, 
                          vocab = out1$vocab, 
                          K = seq(10, 80, by = 10), 
                          prevalence = ~ Origin + s(Publication.Year) + log1p(Citations),
                          data = out1$meta,
                          max.em.its = 75,
                          init.type = "Spectral")

# [2] 지표 그래프 시각화
# 이 코드를 실행하면 RStudio의 Plots 창에 4가지 지표 그래프가 나타납니다.
plot(kresult_stage1)
