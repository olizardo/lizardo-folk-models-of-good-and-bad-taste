import pandas as pd
import json

df = pd.read_csv("data/FolkTaste_CleanData.csv")
if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
    df = df.drop(0).reset_index(drop=True)

df = df[df['Taste_Possibility'] == 'YES'].copy()

probs = pd.read_csv("bad_taste_nmf_k4/document_topic_probabilities.csv")
merged = pd.merge(df, probs, left_index=True, right_on="original_index")

# Topic 3 is Care-Really-Look
t3_docs = merged[merged.iloc[:, -4:].idxmax(axis=1) == '3'].sort_values('3', ascending=False)
for d in t3_docs['BadTaste_Def'].head(15):
    print("-", d)
