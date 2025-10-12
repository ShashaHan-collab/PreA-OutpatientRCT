import pandas as pd
from sklearn.metrics import f1_score, confusion_matrix
import json

input_file = 'BERT_CSE.csv'
output_file = 'BERT_CSE_boostrap.json'

ratio = 0.7

df = pd.read_csv(input_file)

rs = []

for i in range(1000):
	_df = df.sample(frac=ratio)
	f1 = f1_score(_df['true'], _df['pred'])
	rs.append(f1)

open(output_file, 'w').write(json.dumps(rs))
