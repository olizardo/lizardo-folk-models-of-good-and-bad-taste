import pandas as pd
df = pd.read_csv('data/FolkTaste_CleanData.csv')
if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
    df = df.iloc[1:]

res = df[['EducationLevel', 'EducationLevel_Coded']].drop_duplicates().sort_values('EducationLevel_Coded')
print(res)
