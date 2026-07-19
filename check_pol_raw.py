import pandas as pd
df = pd.read_csv('data/FolkTaste_BruteData.csv')
print(df[['Political']].head(5))
