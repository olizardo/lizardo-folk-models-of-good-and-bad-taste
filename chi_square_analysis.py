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
ge = get_primary("good_taste_example_model_min5/document_topic_probabilities.csv").rename(columns={'primary': 'Good_Ex'})
be = get_primary("bad_taste_example_model_min10/document_topic_probabilities.csv").rename(columns={'primary': 'Bad_Ex'})

df = gd.merge(bd, on='original_index').merge(ge, on='original_index').merge(be, on='original_index')

# Optional: map numeric topics back to readable names
topic_names = {
    'Good_Def': {'-1': 'Outlier', '0': 'Similarity/Subjectivity', '1': 'Aesthetics', '2': 'Style/Quality'},
    'Bad_Def': {'-1': 'Outlier', '0': 'Dislike/Subjective', '1': 'Poor Choices', '2': 'Tacky/Loud', '3': 'Low-Brow Media', '4': 'Immoral', '5': 'Poor Manners'},
    'Good_Ex': {'-1': 'Outlier', '0': 'Fashion/Decor', '1': 'Refinement/Art', '2': 'Best Friends', '3': 'Musical Knowledge', '4': 'Film/TV', '5': 'Obamas/Class', '6': 'Partners/Food'},
    'Bad_Ex': {'-1': 'Outlier', '0': 'Unspecified/Music', '1': 'Poor Fashion', '2': 'Derivative Media', '3': 'Trump/Vulgarity'}
}

# We will drop outliers for the chi-square analysis
for col in ['Good_Def', 'Bad_Def', 'Good_Ex', 'Bad_Ex']:
    df = df[df[col] != '-1']
    df[col] = df[col].map(topic_names[col])

domains = ['Good_Def', 'Bad_Def', 'Good_Ex', 'Bad_Ex']
for d1, d2 in itertools.combinations(domains, 2):
    print(f"\n======================================")
    print(f"Crosstab: {d1} vs {d2}")
    ct = pd.crosstab(df[d1], df[d2])
    print(ct)
    
    # Only compute chi-square if the table is at least 2x2
    if ct.shape[0] > 1 and ct.shape[1] > 1:
        chi2, p, dof, ex = chi2_contingency(ct)
        print(f"\nChi-square: {chi2:.2f}, p-value: {p:.4f}, df: {dof}")
    else:
        print("Not enough variation for Chi-square.")
