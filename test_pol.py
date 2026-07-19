import pandas as pd
df = pd.read_csv("data/FolkTaste_CleanData.csv", low_memory=False)
if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
    df = df.drop(0).reset_index(drop=True)
print(df['Political'].unique())
print(df['Political'].head())
