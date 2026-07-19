import pandas as pd
df = pd.read_csv('data/FolkTaste_CleanData.csv')
# drop the first row if it's the question text
if "What do you mean" in str(df['GoodTaste_Def'].iloc[0]):
    df = df.iloc[1:]

res = df.groupby(['EducationLevel', 'EducationLevel_Coded']).size().reset_index(name='Count')
print(res)
