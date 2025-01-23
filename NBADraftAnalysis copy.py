import pandas as pd
import seaborn as sb
import numpy as nump
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression

data = pd.read_csv('/Users/jordan/Desktop/NBA Draft Analysis/draft-data-20-years.csv')

def move_draft_year(data):
    columns = data.columns.tolist()
    columns.remove('DraftYr')
    columns.insert(columns.index('College')+1, 'DraftYr')
    return data[columns]

data = data.pipe(move_draft_year)

data = pd.DataFrame(data)

data['PickType'] = 'Non-Lottery'

for index, row in data.iterrows():
  if (row['DraftYr'] >= 2004 and row['Pk'] < 15) or (row['DraftYr'] < 2004 and row['Pk'] < 14):
    data.at[index, 'PickType'] = 'Lottery'

def move_lottery_selection(data):
    columns = data.columns.tolist()
    columns.remove('PickType')
    columns.insert(columns.index('Pk')+1, 'PickType')
    return data[columns]

data = data.pipe(move_lottery_selection)

data = data[(data['G'] >= 41) & (data['DraftYr'] != 2021)]

data['College'].fillna('International/High School', inplace = True)

data['3P%'] = data['3P%'].fillna(0.0)



data.to_csv('/Users/jordan/Desktop/NBA Draft Analysis/draft-data-20-years.csv', index = False)