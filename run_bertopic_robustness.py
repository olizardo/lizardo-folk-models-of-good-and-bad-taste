import pandas as pd
import numpy as np
import json
import os
from bertopic import BERTopic
from sklearn.feature_extraction.text import CountVectorizer, ENGLISH_STOP_WORDS

def run_and_save_bertopic(docs, taste_type, min_topic_sizes=[5, 10, 15, 20]):
    """
    Run BERTopic with different min_topic_size values as a robustness check.
    Saves the models and document-topic probability matrices.
    """
    results = []
    
    # Initialize CountVectorizer with expanded stop words
    domain_stops = ["good", "bad", "taste", "people", "person", "think", "means", "just", "like", "things", "someone", "really"]
    custom_stop_words = list(ENGLISH_STOP_WORDS.union(domain_stops))
    vectorizer_model = CountVectorizer(stop_words=custom_stop_words)
    
    for size in min_topic_sizes:
        print(f"Running BERTopic for {taste_type} taste with min_topic_size={size}...")
        
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
        out_dir = f"{taste_type}_taste_model_min{size}"
        os.makedirs(out_dir, exist_ok=True)
        
        # Save model
        model.save(out_dir, serialization="safetensors", save_ctfidf=True)
        
        # Save the document-topic probabilities matrix
        # docs x topics
        probs_df = pd.DataFrame(probs)
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

def main():
    # Load data
    data_path = "data/FolkTaste_CleanData.csv"
    df = pd.read_csv(data_path, skiprows=[1]) # skip question row if it's there
    
    # We need to extract the docs carefully.
    # The first row is headers, second is question text.
    # We should skip the second row.
    df = pd.read_csv(data_path)
    # drop the question row (index 0)
    df = df.drop(0).reset_index(drop=True)
    
    good_taste_docs = df['GoodTaste_Def'].dropna().astype(str).tolist()
    bad_taste_docs = df['BadTaste_Def'].dropna().astype(str).tolist()
    
    print(f"Loaded {len(good_taste_docs)} 'Good Taste' definitions.")
    print(f"Loaded {len(bad_taste_docs)} 'Bad Taste' definitions.")
    
    print("\n--- Running Robustness Checks for Good Taste ---")
    good_results = run_and_save_bertopic(good_taste_docs, "good")
    
    print("\n--- Running Robustness Checks for Bad Taste ---")
    bad_results = run_and_save_bertopic(bad_taste_docs, "bad")
    
    # Save robustness summary
    summary = {
        "good_taste_robustness": good_results,
        "bad_taste_robustness": bad_results
    }
    with open("topic_robustness_summary.json", "w") as f:
        json.dump(summary, f, indent=4)
        
    print("Robustness checks complete. Summary saved to topic_robustness_summary.json.")

if __name__ == "__main__":
    main()
