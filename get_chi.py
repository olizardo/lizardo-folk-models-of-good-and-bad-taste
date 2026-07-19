import csv
from collections import defaultdict

def get_primary(prob_file):
    with open(prob_file, 'r') as f:
        reader = csv.reader(f)
        header = next(reader)
        # assuming 'original_index' is one of the columns
        topic_cols = [c for c in header if c != 'original_index']
        idx_idx = header.index('original_index') if 'original_index' in header else -1
        
        primary = {}
        for row in reader:
            idx = row[idx_idx] if idx_idx != -1 else row[0]
            vals = [float(row[header.index(c)]) for c in topic_cols]
            max_val = max(vals)
            max_col = topic_cols[vals.index(max_val)]
            primary[idx] = max_col
    return primary

g = get_primary('good_taste_nmf_k4/document_topic_probabilities.csv')
b = get_primary('bad_taste_nmf_k4/document_topic_probabilities.csv')

common = set(g.keys()).intersection(b.keys())

g_counts = defaultdict(int)
b_counts = defaultdict(int)
joint_counts = defaultdict(lambda: defaultdict(int))

for k in common:
    g_counts[g[k]] += 1
    b_counts[b[k]] += 1
    joint_counts[g[k]][b[k]] += 1

total = len(common)

chi2 = 0
for gk in g_counts:
    for bk in b_counts:
        expected = (g_counts[gk] * b_counts[bk]) / total
        observed = joint_counts[gk][bk]
        if expected > 0:
            chi2 += ((observed - expected) ** 2) / expected

print(f"Chi-square: {chi2:.2f}, N: {total}")
print("Joint counts:")
for gk in g_counts:
    for bk in b_counts:
        print(f"Good: {gk}, Bad: {bk}, Obs: {joint_counts[gk][bk]}, Exp: {(g_counts[gk]*b_counts[bk])/total:.1f}")
