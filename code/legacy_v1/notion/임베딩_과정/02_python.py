# 1. 라이브러리 설치
!pip install transformers torch scikit-learn tqdm

import pandas as pd
import numpy as np
import torch
from transformers import AutoTokenizer, AutoModel
from tqdm import tqdm
from sklearn.metrics.pairwise import cosine_similarity
from google.colab import files # 파일 다운로드용

# 2. GPU 체크
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(f"현재 사용 중인 장치: {device} (T4 GPU가 잡혀야 합니다)")

# 3. PaECTER 모델 및 토크나이저 로드
model_name = "mpi-inno-comp/paecter"
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModel.from_pretrained(model_name).to(device)
model.eval()
