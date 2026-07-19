import pandas as pd
df = pd.read_csv('data/FolkTaste_CleanData.csv')
if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
    df = df.iloc[1:]

print("Data type:", df['Age'].dtype)
print("\nSample values:")
print(df['Age'].head(10).tolist())
print("\nUnique values count:", df['Age'].nunique())
print("\nMin/Max (if numeric):")
df['Age'] = pd.to_numeric(df['Age'], errors='coerce')
print("Min:", df['Age'].min(), "Max:", df['Age'].max())
