import pandas as pd
from scipy.stats import chi2_contingency
import itertools
import json

with open("config.json", "r") as f:
    config = json.load(f)

def get_primary(prob_file):
    df = pd.read_csv(prob_file)
    topic_cols = [c for c in df.columns if c != 'original_index']
    df['primary'] = df[topic_cols].idxmax(axis=1)
    return df[['original_index', 'primary']]

def get_topic_names(model_dir):
    info = pd.read_csv(f"{model_dir}/topic_info.csv")
    mapping = {}
    for _, row in info.iterrows():
        t = row['Topic']
        if t == -1:
            mapping[str(t)] = "Outlier"
        else:
            clean_name = "_".join(row['Name'].split('_')[1:]).replace('_', ' ').title()
            mapping[str(t)] = clean_name
    return mapping

gd_names = get_topic_names(config["good_model"])
bd_names = get_topic_names(config["bad_model"])

gd = get_primary(f"{config['good_model']}/document_topic_probabilities.csv").rename(columns={'primary': 'Good_Def'})
bd = get_primary(f"{config['bad_model']}/document_topic_probabilities.csv").rename(columns={'primary': 'Bad_Def'})

df = gd.merge(bd, on='original_index')

# Apply mapped names and drop outliers
df = df[(df['Good_Def'] != '-1') & (df['Bad_Def'] != '-1')].copy()
df['Good_Def'] = df['Good_Def'].apply(lambda x: gd_names.get(str(x), str(x)))
df['Bad_Def'] = df['Bad_Def'].apply(lambda x: bd_names.get(str(x), str(x)))

domains = ['Good_Def', 'Bad_Def']
for d1, d2 in itertools.combinations(domains, 2):
    print(f"\n======================================")
    print(f"Crosstab: {d1} vs {d2}")
    ct = pd.crosstab(df[d1], df[d2])
    print(ct)
    
    if ct.shape[0] > 1 and ct.shape[1] > 1:
        chi2, p, dof, ex = chi2_contingency(ct)
        print(f"\nChi-square: {chi2:.2f}, p-value: {p:.4f}, df: {dof}")
    else:
        print("Not enough variation for Chi-square.")