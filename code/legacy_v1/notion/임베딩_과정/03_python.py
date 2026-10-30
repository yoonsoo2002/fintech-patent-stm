# 4. 데이터 로드
df_full = pd.read_csv('fintech_family_with_markets.csv')
df_seed = pd.read_csv('extracted_by_citation_rank_with_title_abstract.csv')

# 텍스트 결합 (Title + Abstract)
df_full['input_text'] = df_full['Title'].fillna('') + " [SEP] " + df_full['Abstract'].fillna('')
df_seed['input_text'] = df_seed['Title'].fillna('') + " [SEP] " + df_seed['Abstract'].fillna('')

# 5. 임베딩 함수 정의
def get_embeddings(text_list, batch_size=32):
    all_embeddings = []
    for i in tqdm(range(0, len(text_list), batch_size), desc="Embedding"):
        batch_texts = text_list[i:i+batch_size]
        inputs = tokenizer(batch_texts, padding=True, truncation=True, 
                          max_length=512, return_tensors="pt").to(device)
        with torch.no_grad():
            outputs = model(**inputs)
            embeddings = outputs.last_hidden_state[:, 0, :].cpu().numpy()
            all_embeddings.append(embeddings)
    return np.vstack(all_embeddings)

# 6. 임베딩 실행
print("\n시드 특허 임베딩 중...")
seed_vecs = get_embeddings(df_seed['input_text'].tolist())

print("\n전체 특허 임베딩 중 (약 10~20분 소요)...")
full_vecs = get_embeddings(df_full['input_text'].tolist())

# 

# 7. 유사도 계산 및 필터링
print("\n유사도 분석 및 핀테크 특허 추출 중...")
cos_sim_matrix = cosine_similarity(full_vecs, seed_vecs)
df_full['fintech_score'] = cos_sim_matrix.max(axis=1) # 각 특허별 최대 유사도 점수

# 임계값 설정 (우선 0.75로 설정 후 분포 확인)
threshold = 0.75
df_real_fintech = df_full[df_full['fintech_score'] >= threshold].copy()

print(f"\n최종 추출된 핀테크 특허 수: {len(df_real_fintech)}개")

# 8. 결과 파일 저장
df_real_fintech.to_csv('final_real_fintech_patents.csv', index=False)
np.save('full_embeddings.npy', full_vecs) # 벡터 데이터도 혹시 모르니 저장

print("\n작업 완료! 결과 파일을 다운로드합니다...")
files.download('final_real_fintech_patents.csv')
