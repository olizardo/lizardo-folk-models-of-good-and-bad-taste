import pandas as pd
df = pd.read_csv('data/FolkTaste_BruteData.csv')
print(df['Political'].value_counts(dropna=False))
