install.packages("stm")
install.packages("stringr")
install.pacakages("stopwords")

# 1. 패키지 로드 (미설치 시 install.packages() 먼저 실행)
library(stm)
library(stringr)
library(stopwords)

# 2. 자료 불러오기 (UTF-8 인코딩 적용)
# 데이터 파일이 R 작업 공간(Working Directory)에 있는지 확인하세요.
mydata5 <- read.csv("final_fintech_patents_filtered.csv", fileEncoding="UTF-8", stringsAsFactors=FALSE)

# 3. 텍스트 결합 (제목 가중치 2배 적용)
# 띄어쓰기 구분을 위해 sep=" " 추가
mydata5$text_tt <- paste(mydata5$Title, mydata5$Title, mydata5$Abstract, sep=" ")

# 4. 불용어 설정 (SMART 사전 + 커스텀)
stwds <- stopwords(language = "en", source = "smart")
# 특허 문서에서 토픽을 방해하는 상투적인 표현 추가 권장
custom <- c('invention', 'thereof', 'therefore', 'therefrom', 'system', 'method', 'apparatus', 'plurality', 'embodiment')
csw <- c(stwds, custom)

# 5. 전처리 (textProcessor)
# 숫자는 보존(removenumbers = FALSE)하여 Web 3.0, GPT-4 등 키워드 유지
proc1 <- textProcessor(documents = mydata5$text_tt, 
                       metadata = mydata5, 
                       lowercase = TRUE, 
                       removepunctuation = TRUE, 
                       customstopwords = csw, 
                       removestopwords = TRUE, 
                       removenumbers = FALSE, 
                       stem = TRUE, 
                       wordLengths = c(2,Inf))

# 6. 데이터 정제 및 1% 희귀 단어 제거 (prepDocuments) 
# 21,381건의 1%인 214로 lower.thresh 설정
out1 <- prepDocuments(proc1$documents, 
                      proc1$vocab, 
                      proc1$meta, 
                      lower.thresh = 214)
