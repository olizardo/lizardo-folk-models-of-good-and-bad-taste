import csv
import json
import ast
import os

with open("config.json", "r") as f:
    config = json.load(f)

def clean_name(name):
    return " ".join(name.split("_")[1:]).title()

def make_rep_docs_table(csv_path, out_tex, caption, label):
    with open(csv_path, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = [r for r in reader if r['Topic'] != '-1']
        
    tex_str = "\\begin{table}[htpb]\n\\centering\n"
    tex_str += "\\footnotesize\n\\begin{tabular}{lp{3.5cm}p{10cm}}\n\\toprule\n"
    tex_str += "Topic & Name & Representative Document \\\\\n\\midrule\n"
    
    for row in rows:
        name = clean_name(row['Name']).replace("&", "\\&")
        try:
            docs = ast.literal_eval(row['Representative_Docs'])
            doc = docs[0] if docs else ""
        except:
            doc = row['Representative_Docs'][:100]
        
        # Clean latex characters
        doc = doc.replace('%', '\\%').replace('&', '\\&').replace('$', '\\$').replace('_', '\\_').replace('#', '\\#')
        
        tex_str += f"{row['Topic']} & {name} & ``{doc}'' \\\\\n"
        
    tex_str += "\\bottomrule\n\\end{tabular}\n"
    tex_str += f"\\caption{{{caption}}}\n\\label{{tab:{label}}}\n\\end{{table}}\n"
    
    with open(out_tex, "w", encoding="utf-8") as f:
        f.write(tex_str)

make_rep_docs_table(f"{config['good_model']}/topic_info.csv", "report/Tabs/good_taste_rep_docs.tex", "Representative Quotes for Good Taste Cultural Models", "good_taste_rep_docs")
make_rep_docs_table(f"{config['bad_model']}/topic_info.csv", "report/Tabs/bad_taste_rep_docs.tex", "Representative Quotes for Bad Taste Cultural Models", "bad_taste_rep_docs")
