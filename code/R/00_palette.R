# ==============================================================================
#  00_palette.R  —  파스텔 색상 단일 진실원천 (R 그림용)
# ==============================================================================
#  노션 '클러스터 색상 리스트' 확정 8색 + 국가별 파스텔.
#  v1에서는 같은 8색 hex가 메인 R코드에만 3곳, TLC/Gompertz에 각각 또 정의돼 있었고
#  TLC 매트릭스만 채도 높은 다른 팔레트(#E74C3C…)를 썼다. → 여기로 통일(전부 파스텔).
#
#  [핵심 키 규칙]
#   - 알파벳 키        : cluster_colors_alpha  (예: "A","B"…)  ← 그림에서 점/막대 fill
#   - 풀네임(접두사) 키: cluster_colors_full   ("A. 기업 재무 및 회계 관리"…) ← CSV join용
#   둘 다 같은 값. 그림에서 어떤 라벨을 쓰든 매칭되게 둘 다 제공.
# ==============================================================================

# 8대 메가 클러스터 — 알파벳 키
cluster_colors_alpha <- c(
  "A" = "#AEC6CF",  # 기업 재무 및 회계 관리
  "B" = "#FFD1BA",  # 맞춤형 보험 및 타겟 식별
  "C" = "#D8BFD8",  # AI 기반 신용/투자 분석
  "D" = "#C1E1C1",  # 블록체인 기반 에너지 금융
  "E" = "#FDFD96",  # 블록체인 및 디지털 자산
  "F" = "#B0C4DE",  # 보안 인증 및 규제 컴플라이언스
  "G" = "#AFEEEE",  # 뱅킹 및 대금 정산
  "H" = "#FFB6C1"   # O2O 상거래 및 간편 결제
)

# 클러스터 한글 라벨
cluster_names_kr <- c(
  "A" = "기업 재무 및 회계 관리",
  "B" = "맞춤형 보험 및 타겟 식별",
  "C" = "AI 기반 신용/투자 분석",
  "D" = "블록체인 기반 에너지 금융",
  "E" = "블록체인 및 디지털 자산",
  "F" = "보안 인증 및 규제 컴플라이언스",
  "G" = "뱅킹 및 대금 정산",
  "H" = "O2O 상거래 및 간편 결제"
)

# 클러스터 영문 라벨
cluster_names_en <- c(
  "A" = "Corporate Finance & Accounting Management",
  "B" = "Customized Insurance & Target Identification",
  "C" = "AI-based Credit & Investment Analysis",
  "D" = "Blockchain-based Energy Finance",
  "E" = "Blockchain and Digital Assets",
  "F" = "Security Authentication & Regulatory Compliance",
  "G" = "Banking & Payment Settlement",
  "H" = "O2O Commerce & Simplified Payment"
)

# 풀네임(접두사) 키 — "A. 기업 재무 및 회계 관리" 형태. CSV의 Cluster_Full 컬럼과 매칭.
cluster_colors_full <- setNames(
  cluster_colors_alpha,
  paste0(names(cluster_colors_alpha), ". ", cluster_names_kr[names(cluster_colors_alpha)])
)

# 국가(블록)별 파스텔 색 — config.R COUNTRIES 와 동일 키
country_colors <- c(
  "US" = "#A0C4FF",  # 파스텔 블루
  "CN" = "#FFADAD",  # 파스텔 레드
  "JP" = "#CAFFBF",  # 파스텔 그린
  "KR" = "#BDB2FF",  # 파스텔 퍼플
  "EP" = "#FFD6A5"   # 파스텔 오렌지
)

# 유의성 히트맵 7단계 (양(+, 파랑계열) ↔ 음(-, 빨강계열)) — 파스텔 유지
sig_heatmap_colors <- c(
  "Target (+) : p < 0.001 (***)" = "#5DADE2",
  "Target (+) : p < 0.01 (**)"   = "#85C1E9",
  "Target (+) : p < 0.05 (*)"    = "#D4E6F1",
  "Not Significant"              = "#F8F9F9",
  "US (-) : p < 0.05 (*)"        = "#FADBD8",
  "US (-) : p < 0.01 (**)"       = "#F5B7B1",
  "US (-) : p < 0.001 (***)"     = "#EC7063"
)

cat("팔레트 로드 완료 | 클러스터 8색 + 국가 5색 (전부 파스텔)\n")
