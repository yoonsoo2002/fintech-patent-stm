# legacy_v1/ — v1(국내학회본) 원본 코드, 참고용

이 폴더의 파일은 **실행용이 아니다.** v1 분석 당시 Colab 노트북과 채팅에서 쓰던 코드를 이어 붙인 것으로,
`!pip` 셀이나 설명 문장이 섞여 있어 그대로는 파싱되지 않는다. 원본 위치는 연구 폴더의 `1_R코드/`, `2_Python코드/`이다.
현재 파이프라인은 `code/python/`과 `code/R/`의 스크립트다(설정은 `00_config.*`에서만 바꾼다).

| 파일 | v1에서의 역할 | 현재 대응 |
|---|---|---|
| `python/basic.ipynb` | 데이터 정제·시드 후보 (노트북) | `code/python/01_정제_시드후보.py` |
| `python/embedding.ipynb`, `python/embedding.py` | PaECTER 임베딩 + K-NN 필터링 | `code/python/02_임베딩_필터링.py` |
| `python/임베딩성능평가.py` | 필터 검증 (Cohen's d, F1) | `code/python/03_필터링검증.py` |
| `R/Fintech_Project.R` | STM 학습과 사후분석 전체 (단일 스크립트) | `code/R/04`~`07` |
| `R/TLC_BSpline_Momentum.R` | B-2: B-Spline 성장 모멘텀 | `code/R/06_TLC_모멘텀.R` |
| `R/Gompertz_Diagnostics.R` | B-3: Gompertz TMR 진단과 CI | `code/R/07_Gompertz_TMR.R` |
| `R/STM R 코드.txt` | 불용어 처리, searchK로 K 탐색 | `code/R/04_STM_학습.R` (K 탐색은 미이식) |
| `R/네트워크 시각화 R 코드.txt` | topicCorr 네트워크 그림 | `code/R/10_본문그림_재현.R` (그림 4-6) |
| `R/시계열 그래프.txt` | 토픽별 연도 추세 꺾은선·히트맵 | `code/R/10_본문그림_재현.R` (그림 4-7·4-8) |
| `R/미국 기준 시각화 코드.txt` | 미국 기준 전략적 갭 히트맵 | `code/R/05_분석_국가갭_클러스터.R` |
| `R/정윤코드.txt` | 공동저자(신정윤) 작업 코드 전체 (4,258줄) | 여러 단계에 흩어져 있음 |

노션에만 있던 초기 코드·LLM 프롬프트·검색식은 `notion/`에 모았다(대응표는 `notion/README.md`). 실제 수집에 쓴 Lens 검색식은 `code/queries/`에 있다.

주의: `.txt` 코드에는 작성자 PC 경로(`C:/Users/gram/...`)가 남아 있다. `.ipynb`는 `.gitignore`에 따라 Git에 올라가지 않는다. 기록 보존을 위해 원문 그대로 올렸다(저자 확인 2026-10-30).
