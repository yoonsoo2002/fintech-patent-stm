"""
================================================================================
 08_EDA_그림.py  —  [단계 10] 논문 EDA 그림 (국가점유·Top15 출원인·시계열·피인용 파레토)
================================================================================
 입력 : final_fintech_patents_filtered.csv (02 산출)
 출력 : EDA_01_국가점유.png / EDA_02_Top15출원인.png / EDA_03_시계열.png / EDA_04_피인용파레토.png

 v1(basic.ipynb)을 충실히 옮기되 두 가지 개선(사용자 요구):
   ① ★savefig 추가 (v1은 plt.show()만이라 파일이 안 남았음) + dpi 고화질
   ② ★국가색을 파스텔로 통일 (v1 viridis/Blues_r → 00_palette.py COUNTRY_COLORS)
================================================================================
"""
import os, sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

import importlib.util as _u
def _load(name, fname):
    s = _u.spec_from_file_location(name, os.path.join(os.path.dirname(__file__) if "__file__" in globals() else ".", fname))
    m = _u.module_from_spec(s); s.loader.exec_module(m); return m
cfg = _load("cfg", "00_config.py")
pal = _load("pal", "00_palette.py")

if cfg.USE_COLAB:
    from google.colab import drive; drive.mount("/content/drive")

plt.rcdefaults(); sns.set_theme(style="whitegrid")
DPI = cfg.FIG_DPI
df = pd.read_csv(cfg.P(cfg.FILTERED_OUTPUT), low_memory=False)
df_ip5 = df[df["Origin_Country"].isin(pal.COUNTRY_ORDER)].copy()

def order_counts(s):
    """국가 순서(US,CN,JP,KR,EP)대로 정렬된 value_counts."""
    vc = s.value_counts()
    idx = [c for c in pal.COUNTRY_ORDER if c in vc.index]
    return vc.reindex(idx)

# ── 1) 국가별 점유율 (막대 + 파이) — 파스텔 국가색 ──────────────────────────
cc = order_counts(df_ip5["Origin_Country"])
colors = pal.country_palette(cc.index.tolist())
fig, (a1, a2) = plt.subplots(1, 2, figsize=(15, 6))
sns.barplot(x=cc.index, y=cc.values, ax=a1, palette=colors)
a1.set_title(f"FinTech Patent Volume by Origin ({cfg.START_YEAR}-{cfg.END_YEAR})", fontsize=14, fontweight="bold")
a1.set_xlabel("Country / Bloc"); a1.set_ylabel("Patents")
for p in a1.patches:
    a1.annotate(f"{int(p.get_height()):,}", (p.get_x()+p.get_width()/2, p.get_height()),
                ha="center", va="bottom", xytext=(0,5), textcoords="offset points")
a2.pie(cc.values, labels=cc.index, autopct="%1.1f%%", startangle=140, colors=colors,
       textprops={"fontsize": 12})
a2.set_title("Origin Share in FinTech Patents", fontsize=14, fontweight="bold")
plt.tight_layout(); plt.savefig(cfg.P("EDA_01_국가점유.png"), dpi=DPI, bbox_inches="tight"); plt.close()
print("💾 EDA_01_국가점유.png  (★EP 비중 확인)")

# ── 2) Top15 출원인 ────────────────────────────────────────────────────────
df[cfg.COL_APPLICANT] = df[cfg.COL_APPLICANT].fillna("Unknown")
top = df[cfg.COL_APPLICANT].value_counts().head(15)
plt.figure(figsize=(12, 8))
ax = sns.barplot(x=top.values, y=top.index, palette=sns.light_palette(pal.COUNTRY_COLORS["US"], n_colors=15, reverse=True))
plt.title(f"Top 15 Assignees in FinTech Patents ({cfg.START_YEAR}-{cfg.END_YEAR})", fontsize=16, fontweight="bold")
plt.xlabel("Patents"); plt.ylabel("Assignee")
for p in ax.patches:
    w = p.get_width(); plt.text(w*1.01, p.get_y()+p.get_height()/2, f"{int(w):,}", va="center", fontsize=10)
plt.tight_layout(); plt.savefig(cfg.P("EDA_02_Top15출원인.png"), dpi=DPI, bbox_inches="tight"); plt.close()
print("💾 EDA_02_Top15출원인.png")

# ── 3) 출원/공개 시계열 ────────────────────────────────────────────────────
df[cfg.COL_APPDATE] = pd.to_datetime(df[cfg.COL_APPDATE], errors="coerce")
df["AppY"] = df[cfg.COL_APPDATE].dt.year
trend = pd.DataFrame({
    "Application": df.groupby("AppY").size(),
    "Publication": df.groupby(cfg.COL_PUBYEAR).size(),
}).fillna(0).astype(int).loc[cfg.START_YEAR:cfg.END_YEAR]
plt.figure(figsize=(12, 6))
plt.plot(trend.index, trend["Application"], marker="o", linewidth=2.5, color=pal.COUNTRY_COLORS["US"], label="Application")
plt.plot(trend.index, trend["Publication"], marker="s", linewidth=2.5, color=pal.COUNTRY_COLORS["EP"], label="Publication")
plt.title(f"FinTech Patents: Application vs. Publication ({cfg.START_YEAR}-{cfg.END_YEAR})", fontsize=16, fontweight="bold")
plt.xlabel("Year"); plt.ylabel("Patents"); plt.legend(fontsize=12)
plt.grid(axis="y", linestyle="--", alpha=.7)
plt.tight_layout(); plt.savefig(cfg.P("EDA_03_시계열.png"), dpi=DPI, bbox_inches="tight"); plt.close()
print("💾 EDA_03_시계열.png  (★최근연도 공개지연 dip 확인 — 2025 포함 결정 근거)")

# ── 4) 피인용 파레토 (Lorenz) ──────────────────────────────────────────────
cites = df[cfg.COL_CITES].fillna(0).sort_values(ascending=False).values
cum = np.cumsum(cites) / cites.sum() * 100
pct = np.arange(1, len(cites)+1) / len(cites) * 100
t5 = cum[max(int(len(cites)*0.05)-1, 0)]; t10 = cum[max(int(len(cites)*0.10)-1, 0)]
plt.figure(figsize=(9, 6))
plt.plot(pct, cum, color=pal.COUNTRY_COLORS["CN"], linewidth=3)
for xv, yv, lab in [(5, t5, f"Top 5% → {t5:.1f}%"), (10, t10, f"Top 10% → {t10:.1f}%")]:
    plt.axvline(xv, color="gray", linestyle="--", alpha=.6); plt.axhline(yv, color="gray", linestyle="--", alpha=.6)
    plt.plot(xv, yv, "o", color="black"); plt.text(xv+1, yv-5, lab, fontsize=11)
plt.title("Forward Citation Concentration (Pareto)", fontsize=14, fontweight="bold")
plt.xlabel("Top % of Patents"); plt.ylabel("Cumulative % of Citations")
plt.xlim(0, 30); plt.ylim(0, 105)
plt.tight_layout(); plt.savefig(cfg.P("EDA_04_피인용파레토.png"), dpi=DPI, bbox_inches="tight"); plt.close()
print("💾 EDA_04_피인용파레토.png")
print("\n✅ 08 완료. 4개 EDA 그림을 3_PNG시각화 폴더로 옮기고 논문 3장에 사용.")
