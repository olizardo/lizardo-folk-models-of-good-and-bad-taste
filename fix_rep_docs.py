import csv
import json

with open("config.json", "r") as f:
    config = json.load(f)

# Read raw data
raw_docs = {}
with open("data/FolkTaste_CleanData.csv", "r", encoding="utf-8") as f:
    reader = csv.reader(f)
    header = next(reader)
    q_idx = header.index("Taste_Possibility")
    g_idx = header.index("GoodTaste_Def")
    b_idx = header.index("BadTaste_Def")
    
    # Check if first data row is the question row
    first_row = next(reader)
    if "What do you mean" in first_row[g_idx]:
        pass # dropped
    else:
        # if for some reason it's not the question row, we process it
        if first_row[q_idx].strip().upper() == "YES":
            raw_docs[0] = {
                "GoodTaste_Def": first_row[g_idx],
                "BadTaste_Def": first_row[b_idx]
            }
            
    row_idx = 1
    for row in reader:
        # We store every row that has YES, indexed by row_idx 
        # (which matches the dataframe index in run_topic_modeling after drop(0))
        if row[q_idx].strip().upper() == "YES":
            raw_docs[row_idx] = {
                "GoodTaste_Def": row[g_idx],
                "BadTaste_Def": row[b_idx]
            }
        row_idx += 1

def generate_rep_docs(model_dir, text_col, out_tex, caption, label):
    # read probabilities
    prob_header = []
    prob_rows = []
    with open(f"{model_dir}/document_topic_probabilities.csv", "r", encoding="utf-8") as f:
        reader = csv.reader(f)
        prob_header = next(reader)
        for r in reader:
            prob_rows.append(r)
            
    # read topic info
    topic_names = {}
    with open(f"{model_dir}/topic_info.csv", "r", encoding="utf-8") as f:
        reader = csv.reader(f)
        t_header = next(reader)
        for r in reader:
            t_id = int(r[0])
            name = " ".join(r[2].split("_")[1:]).title()
            topic_names[t_id] = name
            
    tex_str = "\\begin{table}[htpb]\n\\centering\n\\footnotesize\n\\begin{tabular}{lp{3.5cm}p{10cm}}\n\\toprule\n"
    tex_str += "Topic & Name & Representative Document \\\\\n\\midrule\n"
    
    topic_cols = [c for c in prob_header if c != 'original_index']
    idx_idx = prob_header.index('original_index')
    
    for t in topic_cols:
        t_int = int(t)
        t_idx_in_row = prob_header.index(t)
        
        # find row with max prob
        max_prob = -1
        best_orig_idx = -1
        
        for row in prob_rows:
            p = float(row[t_idx_in_row])
            if p > max_prob:
                max_prob = p
                best_orig_idx = int(float(row[idx_idx]))
                
        # get document
        doc = raw_docs[best_orig_idx][text_col]
        doc = doc.replace('%', '\\%').replace('&', '\\&').replace('$', '\\$').replace('_', '\\_').replace('#', '\\#')
        # fix line breaks if any
        doc = doc.replace('\n', ' ').replace('\r', '')
        
        t_name = topic_names[t_int]
        
        tex_str += f"{t_int} & {t_name} & ``{doc}'' \\\\\n"
        
    tex_str += "\\bottomrule\n\\end{tabular}\n"
    tex_str += f"\\caption{{{caption}}}\n\\label{{tab:{label}}}\n\\end{{table}}\n"
    
    with open(out_tex, "w", encoding="utf-8") as f:
        f.write(tex_str)

generate_rep_docs(config['good_model'], "GoodTaste_Def", "report/Tabs/good_taste_rep_docs.tex", "Representative Quotes for Good Taste Cultural Models", "good_taste_rep_docs")
generate_rep_docs(config['bad_model'], "BadTaste_Def", "report/Tabs/bad_taste_rep_docs.tex", "Representative Quotes for Bad Taste Cultural Models", "bad_taste_rep_docs")
print("Done.")
