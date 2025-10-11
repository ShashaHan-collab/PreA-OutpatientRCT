from transformers import AutoTokenizer, AutoModel
from tqdm import tqdm
import torch
from umap import UMAP
from scipy import stats
import pandas as pd
import numpy as np
from itertools import combinations

# load data
df_combined = pd.read_csv('clinical_unique_data.csv')

# load model
tokenizer = AutoTokenizer.from_pretrained("trueto/medbert-base-chinese")
model = AutoModel.from_pretrained("trueto/medbert-base-chinese").to(torch.device("cuda" if torch.cuda.is_available() else "cpu"))
model.eval()
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")


# embedding via medbert
def get_text_embedding(text):
    inputs = tokenizer(text, return_tensors='pt', truncation=True, max_length=512)
    inputs = {key: val.to(device) for key, val in inputs.items()}
    with torch.no_grad():
        outputs = model(**inputs)
    cls_vec = outputs.last_hidden_state[:, 0, :]
    return cls_vec.squeeze().detach().cpu().numpy()


embeddings = []
for _, row in tqdm(df_combined.iterrows(), total=len(df_combined), desc="process", leave=False):
    text_input =''
    # Physical examination
    text_input += f"体格检查：{str(row['体格检查']) if pd.notna(row['体格检查']) else ''}。"
    '''
    for History taking dimension, the following code can be used to extract the text:
    
        text_input += f"主诉：{str(row['主诉']) if pd.notna(row['主诉']) else ''}。"
        text_input += f"现病史：{str(row['现病史']) if pd.notna(row['现病史']) else ''}。"
        text_input += f"既往史：{str(row['既往史']) if pd.notna(row['既往史']) else ''}。"
    '''

    emb = get_text_embedding(text_input)
    embeddings.append(emb)
embeddings = np.array(embeddings)

# UMAP
umap_model = UMAP(n_components=2, random_state=42, n_neighbors=15, min_dist=0.1)
embeddings_umap = umap_model.fit_transform(embeddings)
viz_df = pd.DataFrame({'mode': df_combined['mode']})
viz_df['UMAP1'] = embeddings_umap[:, 0]
viz_df['UMAP2'] = embeddings_umap[:, 1]

# comparison
mode_groups = viz_df['mode'].unique()
mw_pairwise_results = []
dimensions = [('UMAP', 'UMAP1'), ('UMAP', 'UMAP2')]

for method, dim in dimensions:
    # Mann-Whitney U test
    for mode1, mode2 in combinations(mode_groups, 2):
        group1_data = viz_df[viz_df['mode'] == mode1][dim]
        group2_data = viz_df[viz_df['mode'] == mode2][dim]
        u_stat, p_value = stats.mannwhitneyu(group1_data, group2_data, alternative='two-sided')

        mw_pairwise_results.append({'方法': method, '维度': dim, '组别1': mode1, '组别2': mode2, 'p值': p_value,})

mw_results_df = pd.DataFrame(mw_pairwise_results)
mw_results_df = mw_results_df.sort_values(by=['方法', '维度', '组别1', '组别2'])
mw_p_value_table = pd.pivot_table(mw_results_df, values=['p值'], index=['方法', '维度'], columns=['组别1', '组别2'], aggfunc='first')
mw_formatted_p_table = mw_p_value_table.applymap(lambda x: f"{x:.4e}" if x < 0.001 else f"{x:.4f}")
print("\n----- Mann-Whitney U test table(p value) -----")
print(mw_formatted_p_table.to_string())

# save
mw_formatted_p_table.to_csv('domain_specific_pvalue_table.csv')


