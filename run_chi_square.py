import pandas as pd
from scipy.stats import chi2_contingency
import itertools

def get_primary(prob_file):
    df = pd.read_csv(prob_file)
    topic_cols = [c for c in df.columns if c != 'original_index']
    df['primary'] = df[topic_cols].idxmax(axis=1)
    return df[['original_index', 'primary']]

gd = get_primary("good_taste_def_model_min10/document_topic_probabilities.csv").rename(columns={'primary': 'Good_Def'})
bd = get_primary("bad_taste_def_model_min5/document_topic_probabilities.csv").rename(columns={'primary': 'Bad_Def'})

df = gd.merge(bd, on='original_index')

# We will drop outliers for the chi-square analysis
for col in ['Good_Def', 'Bad_Def']:
    df = df[df[col] != '-1']
    df[col] = df[col].apply(lambda x: 'Topic ' + str(x))

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
