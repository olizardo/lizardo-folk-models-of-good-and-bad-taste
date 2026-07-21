import pandas as pd
import numpy as np
import json
import os
from sklearn.feature_extraction.text import TfidfVectorizer, ENGLISH_STOP_WORDS
from sklearn.decomposition import NMF

def evaluate_nmf(docs, name, min_k=2, max_k=10):
    domain_stops = ["don", "dont", "t", "didn", "didnt", "doesn", "doesnt", "ve", "ll", "re", "m", "isn", "isnt", "aren", "arent", "wasn", "wasnt", "weren", "werent", "haven", "havent", "hasn", "hasnt", "hadn", "hadnt", "won", "wont", "wouldn", "wouldnt", "can", "cant", "couldn", "couldnt", "shouldn", "shouldnt", "mightn", "mightnt", "mustn", "mustnt", "do", "does", "did", "doing", "done", "don't", "doesn't", "didn't", "taste", "good", "bad", "people", "person", "think", "things", "just", "like", "mean", "means"]
    custom_stop_words = list(ENGLISH_STOP_WORDS.union(domain_stops))
    
    tfidf_vectorizer = TfidfVectorizer(stop_words=custom_stop_words, max_df=0.95, min_df=2)
    tfidf = tfidf_vectorizer.fit_transform(docs)
    
    errors = []
    k_values = list(range(min_k, max_k + 1))
    
    for k in k_values:
        nmf = NMF(n_components=k, random_state=42, init='nndsvd')
        nmf.fit(tfidf)
        errors.append(nmf.reconstruction_err_)
        
    res_df = pd.DataFrame({'k': k_values, 'reconstruction_error': errors})
    os.makedirs('Tabs', exist_ok=True)
    res_df.to_csv(f'Tabs/nmf_reconstruction_{name}.csv', index=False)

def run_nmf_k4(docs, indices, model_prefix):
    domain_stops = ["don", "dont", "t", "didn", "didnt", "doesn", "doesnt", "ve", "ll", "re", "m", "isn", "isnt", "aren", "arent", "wasn", "wasnt", "weren", "werent", "haven", "havent", "hasn", "hasnt", "hadn", "hadnt", "won", "wont", "wouldn", "wouldnt", "can", "cant", "couldn", "couldnt", "shouldn", "shouldnt", "mightn", "mightnt", "mustn", "mustnt", "do", "does", "did", "doing", "done", "don't", "doesn't", "didn't", "taste", "good", "bad", "people", "person", "think", "things", "just", "like", "mean", "means"]
    custom_stop_words = list(ENGLISH_STOP_WORDS.union(domain_stops))
    
    tfidf_vectorizer = TfidfVectorizer(stop_words=custom_stop_words, max_df=0.95, min_df=2)
    tfidf = tfidf_vectorizer.fit_transform(docs)
    feature_names = tfidf_vectorizer.get_feature_names_out()
    
    k = 4
    nmf = NMF(n_components=k, random_state=42, init='nndsvd', max_iter=500)
    W = nmf.fit_transform(tfidf)
    H = nmf.components_
    
    out_dir = f"{model_prefix}_nmf_k4"
    os.makedirs(out_dir, exist_ok=True)
    
    probs_df = pd.DataFrame(W)
    probs_df.columns = [str(c) for c in probs_df.columns]
    probs_df.insert(0, 'original_index', indices)
    probs_file = os.path.join(out_dir, "document_topic_probabilities.csv")
    probs_df.to_csv(probs_file, index=False)
    
    topic_info_data = []
    primary_topics = np.argmax(W, axis=1)
    
    for topic_idx in range(k):
        top_indices = H[topic_idx].argsort()[:-8:-1]
        top_words = [feature_names[i] for i in top_indices]
        topic_name = f"{topic_idx}_" + "_".join(top_words[:3])
        count = np.sum(primary_topics == topic_idx)
        
        topic_info_data.append({
            "Topic": topic_idx,
            "Count": count,
            "Name": topic_name,
            "Representation": "['" + "', '".join(top_words) + "']"
        })
        
    topic_info_df = pd.DataFrame(topic_info_data)
    topic_info_df.to_csv(os.path.join(out_dir, "topic_info.csv"), index=False)
    
    return out_dir

def main():
    data_path = "data/FolkTaste_CleanData.csv"
    df = pd.read_csv(data_path)
    
    if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
        df = df.drop(0).reset_index(drop=True)
        
    df = df[df['Taste_Possibility'] == 'YES'].copy()
    
    good_series = df['GoodTaste_Def'].fillna('').str.strip()
    good_mask = good_series != ''
    good_docs = good_series[good_mask].tolist()
    good_indices = df[good_mask].index.tolist()
    
    evaluate_nmf(good_docs, "good_taste")
    good_out = run_nmf_k4(good_docs, good_indices, "good_taste")
    
    bad_series = df['BadTaste_Def'].fillna('').str.strip()
    bad_mask = bad_series != ''
    bad_docs = bad_series[bad_mask].tolist()
    bad_indices = df[bad_mask].index.tolist()
    
    evaluate_nmf(bad_docs, "bad_taste")
    bad_out = run_nmf_k4(bad_docs, bad_indices, "bad_taste")
    
    with open("config.json", "w") as f:
        json.dump({
            "good_model": good_out,
            "bad_model": bad_out
        }, f, indent=4)
        
if __name__ == '__main__':
    main()
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
import pandas as pd
import re
import json
import os

with open("config.json", "r") as f:
    config = json.load(f)

def make_summary_table(csv_path, out_tex, label):
    df = pd.read_csv(csv_path)
    df = df[df['Topic'] != -1]
    
    # Clean the name provided by BERTopic/NMF (e.g. "0_taste_good" -> "Taste-Good")
    df['Name'] = df['Name'].apply(lambda x: " ".join(x.split('_')[1:]).title())
    
    name_map = {
        "Similar Say Usually": "Shared Taste",
        "Art Appreciate Quality": "Cultivated Discernment",
        "Look Appealing Clothing": "Personal Style",
        "Pleasing Choices Aesthetically": "Aesthetic Curation",
        "Likes Different Opinion": "Relational Distance",
        "Choices Music Clothing": "Questionable Consumption",
        "Usually Quality Ugly": "Lack of Refinement",
        "Care Really Look": "Aesthetic Neglect"
    }
    df['Name'] = df['Name'].replace(name_map)
    
    # Clean representation array string
    def get_words(r):
        words = re.findall(r"'([^']+)'", r)
        return ", ".join(words)[:60] + "..."
        
    df['Representation'] = df['Representation'].apply(get_words)
    
    df = df[['Topic', 'Count', 'Name', 'Representation']]
    
    tex_str = "\\begin{table}[htpb]\n\\centering\n\\resizebox{\\textwidth}{!}{\n\\begin{tabular}{llrl}\n\\toprule\n"
    tex_str += "Topic & Count & Name & Representation \\\\\n\\midrule\n"
    
    for _, row in df.iterrows():
        name = str(row['Name']).replace('&', '\\&')
        rep = str(row['Representation']).replace('&', '\\&')
        tex_str += f"{row['Topic']} & {row['Count']} & {name} & {rep} \\\\\n"
        
    tex_str += "\\bottomrule\n\\end{tabular}\n}\n"
    tex_str += f"\\caption{{{label.replace('_', ' ').title()}}}\n"
    tex_str += f"\\label{{tab:{label}}}\n\\end{{table}}\n"
    
    os.makedirs(os.path.dirname(out_tex), exist_ok=True)
    with open(out_tex, "w") as f:
        f.write(tex_str)

make_summary_table(f"{config['good_model']}/topic_info.csv", "Tabs/good_taste_def_summary_table.tex", "good_taste_def_summary")
make_summary_table(f"{config['bad_model']}/topic_info.csv", "Tabs/bad_taste_def_summary_table.tex", "bad_taste_def_summary")
print("Successfully generated summary tables from config.")
