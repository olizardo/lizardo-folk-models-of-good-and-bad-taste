import pandas as pd
import numpy as np
import json
import os
from bertopic import BERTopic
from sklearn.feature_extraction.text import CountVectorizer, ENGLISH_STOP_WORDS

def run_and_save_bertopic(docs, indices, model_prefix, min_topic_sizes=[5, 10, 15, 20]):
    """
    Run BERTopic with different min_topic_size values as a robustness check.
    Saves the models and document-topic probability matrices.
    """
    results = []
    
    # Initialize CountVectorizer with expanded stop words
    domain_stops = ["good", "bad", "taste", "people", "person", "think", "means", "just", "like", "things", "someone", "really", "dont", "don't", "do not", "doesnt", "doesn't"]
    custom_stop_words = list(ENGLISH_STOP_WORDS.union(domain_stops))
    vectorizer_model = CountVectorizer(stop_words=custom_stop_words)
    
    for size in min_topic_sizes:
        print(f"Running BERTopic for {model_prefix} with min_topic_size={size}...")
        
        # Initialize BERTopic model
        model = BERTopic(
            language="english",
            vectorizer_model=vectorizer_model,
            min_topic_size=size,
            calculate_probabilities=True,
            verbose=True
        )
        
        # Fit model
        topics, probs = model.fit_transform(docs)
        
        # Create output directory
        out_dir = f"{model_prefix}_model_min{size}"
        os.makedirs(out_dir, exist_ok=True)
        
        # Save model
        model.save(out_dir, serialization="safetensors", save_ctfidf=True)
        
        # Save the document-topic probabilities matrix
        probs_df = pd.DataFrame(probs)
        probs_df.insert(0, 'original_index', indices)
        probs_file = os.path.join(out_dir, "document_topic_probabilities.csv")
        probs_df.to_csv(probs_file, index=False)
        print(f"Saved document-topic probabilities to {probs_file}")
        
        # Record number of topics (excluding outlier topic -1)
        topic_info = model.get_topic_info()
        num_topics = len(topic_info[topic_info['Topic'] != -1])
        results.append({"min_topic_size": size, "num_topics": num_topics})
        
        # Save topic info summary
        topic_info.to_csv(os.path.join(out_dir, "topic_info.csv"), index=False)
        
    return results

def process_column(df, col_name, model_prefix):
    series = df[col_name].fillna('').str.strip()
    mask = series != ''
    docs = series[mask].tolist()
    indices = df[mask].index.tolist()
    
    print(f"\n--- Processing {col_name} ---")
    print(f"Loaded {len(docs)} valid documents (removed {len(df) - len(docs)} empty).")
    
    if len(docs) == 0:
        print(f"No documents found for {col_name}, skipping.")
        return []
        
    return run_and_save_bertopic(docs, indices, model_prefix)

def main():
    # Load data
    data_path = "data/FolkTaste_CleanData.csv"
    df = pd.read_csv(data_path)
    
    if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
        df = df.drop(0).reset_index(drop=True)
        
    # Restrict to participants who answered YES to Taste_Possibility
    original_len = len(df)
    df = df[df['Taste_Possibility'] == 'YES'].copy()
    print(f"Filtered dataset from {original_len} to {len(df)} participants (Taste_Possibility == 'YES').")
    
    summary = {}
    
    # 1. Good Taste Definitions
    summary["good_taste_def"] = process_column(df, 'GoodTaste_Def', 'good_taste_def')
    
    # 2. Bad Taste Definitions
    summary["bad_taste_def"] = process_column(df, 'BadTaste_Def', 'bad_taste_def')
    
    # 3. Good Taste Examples
    summary["good_taste_example"] = process_column(df, 'GoodTaste_Example', 'good_taste_example')
    
    # 4. Bad Taste Examples
    summary["bad_taste_example"] = process_column(df, 'BadTaste_Example', 'bad_taste_example')
    
    with open("topic_robustness_summary_four_models.json", "w") as f:
        json.dump(summary, f, indent=4)
        
    print("\nAll four models processed. Summary saved to topic_robustness_summary_four_models.json.")

if __name__ == "__main__":
    main()