import pandas as pd
import os

file_names = [
    '1 lending-capital-formation.csv',
    '2 investment-asset-management.csv',
    '3 payments-clearing-settlement.csv',
    '4 digital-banking-platforms.csv',
    '5 insurance-risk-transfer.csv',
    '6 risk-management-compliance-regtech.csv',
    '7 digital-assets-cryptocurrency.csv'
]

folder_path = './'

def extract_duplicated_families(file_path):
    try:
        df = pd.read_csv(file_path, encoding='utf-8-sig')
    except:
        df = pd.read_csv(file_path, encoding='cp949')

    # 컬럼 공백 제거
    df.columns = df.columns.str.strip()

    # 'Extended Family Members'가 비어있는 행은 제외 (분석 불가)
    df = df.dropna(subset=['Extended Family Members'])

    # 핵심 로직: 해당 컬럼의 값이 2개 이상 나타나는 모든 행(keep=False)을 필터링
    # keep=False로 설정해야 중복된 것들 중 하나만 남기는 게 아니라 '전부' 가져옵니다.
    duplicated_df = df[df.duplicated(subset=['Extended Family Members'], keep=False)]
    
    # 가족 번호와 발행 연도 순으로 보기 좋게 정렬
    duplicated_df = duplicated_df.sort_values(by=['Extended Family Members', 'Publication Year'], ascending=[True, False])
    
    return duplicated_df

# 실행부
for file in file_names:
    full_path = os.path.join(folder_path, file)
    
    if os.path.exists(full_path):
        result_df = extract_duplicated_families(full_path)
        
        if not result_df.empty:
            output_name = f"duplicated_{file}"
            result_df.to_csv(output_name, index=False, encoding='utf-8-sig')
            print(f"✅ 추출 완료: {output_name} (가족 데이터: {len(result_df)}행)")
        else:
            print(f"ℹ️ {file}: 중복된 가족 데이터가 없습니다.")
    else:
        print(f"❌ 파일을 찾을 수 없습니다: {file}")
