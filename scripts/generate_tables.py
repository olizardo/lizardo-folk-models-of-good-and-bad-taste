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
    df['Name'] = df['Name'].apply(lambda x: "-".join(x.split('_')[1:]).title())
    
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

make_summary_table(f"{config['good_model']}/topic_info.csv", "report/Tabs/good_taste_def_summary_table.tex", "good_taste_def_summary")
make_summary_table(f"{config['bad_model']}/topic_info.csv", "report/Tabs/bad_taste_def_summary_table.tex", "bad_taste_def_summary")
print("Successfully generated summary tables from config.")