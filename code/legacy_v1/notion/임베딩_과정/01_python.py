import pandas as pd
import numpy as np

# 1. 데이터 로드
file_path = 'clean_publication_after_basic_cleaning.csv' # 또는 .tsv 파일 형식에 따라 수정
print("데이터를 로드 중입니다...")
df = pd.read_csv(file_path, sep=',') # 구분자가 탭일 경우 sep='\t'로 변경

# 2. 텍스트 길이 계산 (대표 선정 기준)
# 이미 _text_len 컬럼이 있지만, 더 정확한 정보량을 위해 새로 계산하거나 기존 것을 활용합니다.
df['_text_len'] = df['_text'].fillna('').str.len()

# 3. 우선권 정보 정제 (Earliest Priority Date 기준 국가 추출)
# Earliest Priority Date가 가장 빠른 행을 찾기 위해 날짜 형식으로 변환
df['Earliest Priority Date'] = pd.to_datetime(df['Earliest Priority Date'], errors='coerce')

print("패밀리 그룹화 및 대표 선정 작업을 시작합니다...")

# 4. [핵심] 패밀리별 대표 선정 및 메타데이터 통합
# _family_id를 기준으로 그룹화하여 로직을 적용합니다.

def process_family(group):
    # (A) 대표 특허 선정: 텍스트 정보량이 가장 많은 행 (긴 초록/청구항)
    rep_patent = group.loc[group['_text_len'].idxmax()].copy()
    
    # (B) 기술 기원(Origin Country) 추출: 우선권 날짜가 가장 빠른 행의 국가(Jurisdiction)
    # 만약 날짜가 같다면 첫 번째 행의 국가를 선택
    origin_row = group.loc[group['Earliest Priority Date'].idxmin()]
    origin_country = origin_row['Jurisdiction']
    
    # (C) 패밀리 규모 정보: Extended Family Size 활용 (데이터에 이미 존재하므로 해당 값 유지)
    # 만약 단순 카운팅을 원한다면 group.shape[0]를 써도 되지만, Lens의 'Extended Family Size'가 더 정확합니다.
    
    # 대표 행에 파생 변수 추가
    rep_patent['Origin_Country'] = origin_country
    rep_patent['Final_Family_Size'] = group['Extended Family Size'].max() # 패밀리 내 최대값 기준
    
    return rep_patent

# 패밀리 아이디가 없는 행은 개별 특허로 처리하기 위해 임시 아이디 부여
df['_family_id'] = df['_family_id'].fillna(df['Lens ID'])

# 그룹화 적용 (속도 향상을 위해 필요한 컬럼만 추출하여 처리할 수도 있습니다)
df_representative = df.groupby('_family_id').apply(process_family).reset_index(drop=True)

# 5. 결과 저장 및 확인
print(f"원본 데이터 개수: {len(df)}개")
print(f"패밀리 정제 후 데이터 개수: {len(df_representative)}개")

# 필요한 컬럼만 순서 조정 (선택 사항)
cols_to_keep = [
    'Lens ID', 'Jurisdiction', 'Origin_Country', 'Final_Family_Size', 
    'Publication Year', 'Earliest Priority Date', 'Title', '_text', '_text_len'
]
# 기존 컬럼들을 유지하면서 새로운 변수들을 앞쪽으로 배치
final_cols = ['Origin_Country', 'Final_Family_Size'] + [c for c in df.columns if c not in ['Origin_Country', 'Final_Family_Size']]
df_representative = df_representative[final_cols]

# 저장
df_representative.to_csv('fintech_patents_family_cleaned.csv', index=False)
print("최종 정제 데이터가 'fintech_patents_family_cleaned.csv'로 저장되었습니다.")

# 상위 5개 결과 출력
df_representative[['Display Key', 'Origin_Country', 'Final_Family_Size', '_text_len']].head()
