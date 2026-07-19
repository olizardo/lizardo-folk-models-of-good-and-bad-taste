import pandas as pd
df = pd.read_csv('data/FolkTaste_CleanData.csv')
if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
    df = df.iloc[1:]
df['Age'] = pd.to_numeric(df['Age'], errors='coerce')
print(df[df['Age'] < 18]['Age'].value_counts())
