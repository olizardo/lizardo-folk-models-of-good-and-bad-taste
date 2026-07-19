import pandas as pd
import numpy as np
import json
import os
from bertopic import BERTopic
from sklearn.feature_extraction.text import CountVectorizer, ENGLISH_STOP_WORDS

def run_and_save_bertopic(docs, indices, taste_type, min_topic_sizes=[5, 10, 15, 20]):
    """
    Run BERTopic with different min_topic_size values as a robustness check.
    Saves the models and document-topic probability matrices.
    """
    results = []
    
    # Initialize CountVectorizer with expanded stop words
    domain_stops = ["don", "dont", "t", "didn", "didnt", "doesn", "doesnt", "ve", "ll", "re", "m", "isn", "isnt", "aren", "arent", "wasn", "wasnt", "weren", "werent", "haven", "havent", "hasn", "hasnt", "hadn", "hadnt", "won", "wont", "wouldn", "wouldnt", "can", "cant", "couldn", "couldnt", "shouldn", "shouldnt", "mightn", "mightnt", "mustn", "mustnt", "i've", "don't", "doesn't", "didn't", "isn't", "aren't", "wasn't", "weren't", "haven't", "hasn't", "hadn't", "won't", "wouldn't", "can't", "couldn't", "shouldn't", "mightn't", "mustn't"]
    custom_stop_words = list(ENGLISH_STOP_WORDS.union(domain_stops))
    vectorizer_model = CountVectorizer(stop_words=custom_stop_words)
    
    for size in min_topic_sizes:
        print(f"Running BERTopic for {taste_type} taste (combined) with min_topic_size={size}...")
        
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
        out_dir = f"{taste_type}_taste_combined_model_min{size}"
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

def main():
    # Load data
    data_path = "data/FolkTaste_CleanData.csv"
    df = pd.read_csv(data_path)
    # drop the question row (index 0) if it exists, wait clean data usually doesn't have it, but original script did drop(0)
    # Let's check if index 0 is questions in CleanData
    if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
        df = df.drop(0).reset_index(drop=True)
    
    # Concatenate Def and Example columns
    # Fill NA with empty strings so concatenation works
    df['GoodTaste_Def'] = df['GoodTaste_Def'].fillna('')
    df['GoodTaste_Example'] = df['GoodTaste_Example'].fillna('')
    df['BadTaste_Def'] = df['BadTaste_Def'].fillna('')
    df['BadTaste_Example'] = df['BadTaste_Example'].fillna('')
    
    # Create combined documents. We include a space between them.
    good_taste_combined = (df['GoodTaste_Def'] + " " + df['GoodTaste_Example']).str.strip()
    bad_taste_combined = (df['BadTaste_Def'] + " " + df['BadTaste_Example']).str.strip()
    
    # Filter out empty documents
    good_mask = good_taste_combined != ''
    bad_mask = bad_taste_combined != ''
    
    good_taste_docs = good_taste_combined[good_mask].tolist()
    good_indices = df[good_mask].index.tolist()
    
    bad_taste_docs = bad_taste_combined[bad_mask].tolist()
    bad_indices = df[bad_mask].index.tolist()
    
    print(f"Loaded {len(good_taste_docs)} valid 'Good Taste' combined documents (removed {len(df) - len(good_taste_docs)} empty).")
    print(f"Loaded {len(bad_taste_docs)} valid 'Bad Taste' combined documents (removed {len(df) - len(bad_taste_docs)} empty).")
    
    print("\n--- Running Robustness Checks for Combined Good Taste ---")
    good_results = run_and_save_bertopic(good_taste_docs, good_indices, "good")
    
    print("\n--- Running Robustness Checks for Combined Bad Taste ---")
    bad_results = run_and_save_bertopic(bad_taste_docs, bad_indices, "bad")
    
    # Save robustness summary
    summary = {
        "good_taste_combined_robustness": good_results,
        "bad_taste_combined_robustness": bad_results
    }
    with open("topic_robustness_summary_combined.json", "w") as f:
        json.dump(summary, f, indent=4)
        
    print("Robustness checks complete. Summary saved to topic_robustness_summary_combined.json.")

if __name__ == "__main__":
    main()