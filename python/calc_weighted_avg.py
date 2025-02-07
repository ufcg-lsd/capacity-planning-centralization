import pandas as pd

data = pd.read_csv("../data/mapped_percentage_monthly_2013_2023.csv")

data['month'] = pd.to_datetime(data['month'])
data['year'] = data['month'].dt.year

yearly_data = data.groupby('year').apply(
    lambda group: pd.Series({
        'total_weight': group['weight'].sum(),
        'avg_tagged_percentage': (group['tagged_percentage'] * group['weight']).sum() / group['weight'].sum()
    })
).reset_index()

yearly_data['rolling_weighted_avg'] = yearly_data['avg_tagged_percentage'].rolling(window=3).apply(
    lambda x: sum(x * yearly_data.loc[x.index, 'total_weight']) / yearly_data.loc[x.index, 'total_weight'].sum()
)

yearly_data['time_window'] = yearly_data['year'].apply(
    lambda x: f"{x-2}-{x}" if x >= 3 else None
)

rolling_data = yearly_data.dropna(subset=['rolling_weighted_avg'])

rolling_data[['time_window', 'rolling_weighted_avg']].to_csv(
    "../data/rolling_weighted_avg.csv", index=False
)
