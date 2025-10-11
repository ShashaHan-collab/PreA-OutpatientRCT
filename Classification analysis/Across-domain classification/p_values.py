import numpy as np
from scipy import stats
import json

BERTCLS = json.loads(open('BERT_CLS_boostrap.json').read())
BERTCSE = json.loads(open('BERT_CSE_boostrap.json').read())
MedBERTCLS = json.loads(open('MedBERT_CLS_boostrap.json').read())
MedBERTCSE = json.loads(open('MedBERT_CSE_boostrap.json').read())

def non_equivalence_test(sample, mu, delta):
    sample = np.array(sample)
    n = len(sample)
    mean = np.mean(sample)
    std = np.std(sample, ddof=1)
    se = std / np.sqrt(n)
    
    # 检验两个方向：均值是否显著低于 mu - delta 或高于 mu + delta
    t_low = (mean - (mu - delta)) / se
    p_low = stats.t.cdf(t_low, df=n-1)     # 左侧
    
    t_high = (mean - (mu + delta)) / se
    p_high = stats.t.sf(t_high, df=n-1)    # 右侧
    
    # 最终p值为两个方向的最小p值（联合检验）
    final_p = min(p_low, p_high)
    return final_p

def equivalence_test(sample, mu, delta):
    """TOST"""
    sample = np.array(sample)
    n = len(sample)
    mean = np.mean(sample)
    std = np.std(sample, ddof=1)
    se = std / np.sqrt(n)
    
    # 计算两个单边检验的t统计量
    t1 = (mean - (mu - delta)) / se
    t2 = ((mu + delta) - mean) / se
    
    # 计算p值
    p1 = stats.t.sf(t1, df=n-1)  # 右侧
    p2 = stats.t.sf(t2, df=n-1)  # 右侧检验（等效性方向）
    return max(p1, p2)

def range_test(sample, lower, upper):
    sample = np.array(sample)
    n = len(sample)
    mean = np.mean(sample)
    std = np.std(sample, ddof=1)
    se = std / np.sqrt(n)
    
    # 双单边检验
    t_lower = (mean - lower) / se
    t_upper = (upper - mean) / se
    
    p_lower = stats.t.sf(t_lower, df=n-1)  # 检验mean > lower
    p_upper = stats.t.sf(t_upper, df=n-1)  # 检验upper > mean
    return max(p_lower, p_upper)

for scores in [BERTCLS, MedBERTCLS, BERTCSE, MedBERTCSE]:
    print('mu = 0.67')
    # 1. ΔF1 < 0.02
    p1 = non_equivalence_test(scores, mu=0.67, delta=0.02)
    print(f"(ΔF1 < 0.02) p-value: {p1:.4f}")

    p5 = non_equivalence_test(scores, mu=0.67, delta=0.05)
    print(f"(ΔF1 < 0.05) p-value: {p5:.4f}")
    
    # 2. ΔF1 < 0.1
    p2 = non_equivalence_test(scores, mu=0.67, delta=0.1)
    print(f"(ΔF1 < 0.1) p-value: {p2:.4f}")

    print('mu = 0.57')
    p1 = non_equivalence_test(scores, mu=0.57, delta=0.02)
    print(f"(ΔF1 < 0.02) p-value: {p1:.4f}")

    p1 = non_equivalence_test(scores, mu=0.57, delta=0.05)
    print(f"(ΔF1 < 0.05) p-value: {p1:.4f}")
    
    # 2. ΔF1 < 0.1
    p2 = non_equivalence_test(scores, mu=0.57, delta=0.1)
    print(f"(ΔF1 < 0.1) p-value: {p2:.4f}")
    
    # 3. F1 in [0.57, 0.67]
    p3 = range_test(scores, lower=0.57, upper=0.67)
    print(f"(F1 ∈ [0.57,0.67]) p-value: {p3:.4f}\n")
