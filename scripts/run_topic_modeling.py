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
    os.makedirs('report/Tabs', exist_ok=True)
    res_df.to_csv(f'report/Tabs/nmf_reconstruction_{name}.csv', index=False)

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