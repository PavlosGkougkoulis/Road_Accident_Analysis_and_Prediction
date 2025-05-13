import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression

# Load datasets
file_path = "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/yearly_indicators.csv"
df = pd.read_csv(file_path)
accidents_file_path = "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/yearly_accidents.csv"
df_accidents = pd.read_csv(accidents_file_path)

# Ensure 'year' is sorted
df = df.sort_values(by='year')

# Merge accident data (to get fatalities and vehicles for calculation)
df = df.merge(df_accidents, on='year', how='left')

# Convert '0' values to NaN where appropriate (assuming missing values might be stored as 0)
columns_to_fill = [
    'environmental_taxes', 'petrol_cons', 'ng_cons', 'el_cons',
    'speed_infr', 'drink_infr', 'belt_infr', 'helmet_infr', 'gini_index'
]
df[columns_to_fill] = df[columns_to_fill].replace(0, np.nan)


def fill_missing_values(df, column_name):
    """ Fills missing values using Linear Regression based on existing data trends. """
    known_years = df[df[column_name].notna()]['year'].values.reshape(-1, 1)
    known_values = df[df[column_name].notna()][column_name].values.reshape(-1, 1)

    if len(known_years) > 1:
        model = LinearRegression()
        model.fit(known_years, known_values)

        missing_years = df[df[column_name].isna()]['year'].values.reshape(-1, 1)
        df.loc[df[column_name].isna(), column_name] = model.predict(missing_years)


        df[column_name] = df[column_name].apply(lambda x: max(x, 0) if pd.notna(x) else x)


for col in columns_to_fill:
    if col in df.columns:
        fill_missing_values(df, col)


decimal_places = {}
for col in columns_to_fill:
    first_non_null = df[col].dropna().astype(str).str.split('.').str[1].dropna()
    if not first_non_null.empty:
        decimal_places[col] = len(first_non_null.iloc[0])
    else:
        decimal_places[col] = 2


for col, dec in decimal_places.items():
    if col in df.columns:
        df[col] = df[col].round(dec)


df['fatal_per_million_veh'] = (df['fatalities'] / df['vehicles']) * 1_000_000
df.to_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/final_corrected_yearly_indicators.xlsx", index=False)
print("Updated dataset saved as 'final_corrected_yearly_indicators.xlsx'.")
