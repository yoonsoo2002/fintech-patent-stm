import pandas as pd
import os

# 1. 이미지 및 파일 목록 정의
file_names = [
    '1 lending-capital-formation.csv',
    '2 investment-asset-management.csv',
    '3 payments-clearing-settlement.csv',
    '4 digital-banking-platforms.csv',
    '5 insurance-risk-transfer.csv',
    '6 risk-management-compliance-regtech.csv',
    '7 digital-assets-cryptocurrency.csv'
]

folder_path = './'  # 파일이 위치한 경로

def preprocess_patent_data(file_path):
    # CSV 불러오기 (인코딩 대응)
    try:
        df = pd.read_csv(file_path, encoding='utf-8-sig')
    except:
        try:
            df = pd.read_csv(file_path, encoding='cp949')
        except:
            df = pd.read_csv(file_path, encoding='latin1')

    # 컬럼명 앞뒤 공백 제거
    df.columns = df.columns.str.strip()

    # 필수 컬럼 존재 확인 (제공해주신 리스트 기준)
    required_columns = ['Document Type', 'Extended Family Members', 'Publication Year']
    for col in required_columns:
        if col not in df.columns:
            print(f"⚠️ 오류: '{file_path}' 파일에 '{col}' 컬럼이 없습니다.")
            return None, None

    # 데이터 정제 및 우선순위 부여
    df['Document Type_Cleaned'] = df['Document Type'].astype(str).str.strip().str.title()
    
    # 우선순위: Granted Patent(1), Patent Application(2), 그 외(3)
    priority_map = {'Granted Patent': 1, 'Patent Application': 2}
    df['temp_priority'] = df['Document Type_Cleaned'].map(priority_map).fillna(3)

    # 정렬: 가족번호(오름차순) -> 우선순위(1등 우선) -> 발행연도(최신순)
    df_sorted = df.sort_values(
        by=['Extended Family Members', 'temp_priority', 'Publication Year'],
        ascending=[True, True, False]
    )

    # 중복 제거: 같은 가족 번호 중 가장 상단만 유지
    df_cleaned = df_sorted.drop_duplicates(subset=['Extended Family Members'], keep='first')
    
    # 임시 컬럼 삭제
    original_count = len(df)
    df_cleaned = df_cleaned.drop(columns=['temp_priority', 'Document Type_Cleaned'])
    
    return df_cleaned, original_count

# --- 실행부 ---
for file in file_names:
    full_path = os.path.join(folder_path, file)
    
    if os.path.exists(full_path):
        print(f"처리 중: {file}...")
        # 함수에서 처리 결과와 원본 행 수를 함께 받아옴
        result_df, original_len = preprocess_patent_data(full_path)
        
        if result_df is not None:
            output_name = f"cleaned_{file}"
            result_df.to_csv(output_name, index=False, encoding='utf-8-sig')
            # 오류가 났던 부분 수정: original_len 사용
            print(f"✅ 저장 완료: {output_name} (원본: {original_len}행 -> 결과: {len(result_df)}행)")
    else:
        print(f"❌ 파일을 찾을 수 없습니다: {file}")
