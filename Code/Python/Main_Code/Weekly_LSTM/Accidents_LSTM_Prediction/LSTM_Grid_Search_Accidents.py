from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
import numpy as np
import pandas as pd
import itertools
from datetime import datetime, timedelta
import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset
from torch.nn import HuberLoss
from torch.nn import MSELoss
import random
import matplotlib.pyplot as plt
import seaborn as sns

random.seed(1312)

print("CUDA Available:", torch.cuda.is_available())
print("Current Device:", torch.cuda.current_device())
print("GPU Name:", torch.cuda.get_device_name(0) if torch.cuda.is_available() else "No GPU")


##Importing Dataset##


acc_w_ind_y_mod = pd.read_csv("E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/Dfs_LSTM/Weekly_Accident_Dataset_LSTM.csv")


#Fill NA and make a copy

acc_w_ind_y_mod['accidents'] = acc_w_ind_y_mod['accidents'].fillna(0)
acc_w_ind_y_mod['fatalities'] = acc_w_ind_y_mod['fatalities'].fillna(0)
df = acc_w_ind_y_mod.copy()

#Filter base weeks

base_weeks = df[df['week'] <= 4].copy()

#Count base weeks per

base_weeks['n_base_weeks'] = base_weeks.groupby(['year', 'month'])['week'].transform('count')

#Aggregate extra week values

extra_all = (
    df[df['week'] > 4]
    .groupby(['year', 'month'], as_index=False)
    .agg(
        extra_acc=('accidents', 'sum'),
        extra_fat=('fatalities', 'sum')
    )
)

#Merge the extra sums with base weeks

base_weeks = base_weeks.merge(extra_all, on=['year', 'month'], how='left')
base_weeks['extra_acc'] = base_weeks['extra_acc'].fillna(0)
base_weeks['extra_fat'] = base_weeks['extra_fat'].fillna(0)

#Redistribute values

base_weeks['acc_base_add'] = (base_weeks['extra_acc'] // base_weeks['n_base_weeks']).astype(int)
base_weeks['acc_remainder'] = (base_weeks['extra_acc'] % base_weeks['n_base_weeks']).astype(int)
base_weeks['fat_base_add'] = (base_weeks['extra_fat'] // base_weeks['n_base_weeks']).astype(int)
base_weeks['fat_remainder'] = (base_weeks['extra_fat'] % base_weeks['n_base_weeks']).astype(int)
base_weeks['min_week'] = base_weeks.groupby(['year', 'month'])['week'].transform('min')

#Redistribute to base weeks

base_weeks['accidents'] = (
    base_weeks['accidents'] +
    base_weeks['acc_base_add'] +
    np.where(base_weeks['week'] == base_weeks['min_week'], base_weeks['acc_remainder'], 0)
)
base_weeks['fatalities'] = (
    base_weeks['fatalities'] +
    base_weeks['fat_base_add'] +
    np.where(base_weeks['week'] == base_weeks['min_week'], base_weeks['fat_remainder'], 0)
)

#Drop columns

base_weeks = base_weeks.drop(columns=[
    'n_base_weeks', 'extra_acc', 'extra_fat',
    'acc_base_add', 'acc_remainder',
    'fat_base_add', 'fat_remainder', 'min_week'
])

#Add weekly index per year

base_weeks = base_weeks.sort_values(by=['year', 'month', 'week'])
base_weeks['week_index'] = base_weeks.groupby('year').cumcount() + 1

#Assign Back

acc_w_ind_y_mod = base_weeks.copy()


#Keep only selected columns

acc_w_ind_y_mod = acc_w_ind_y_mod[[
    'year', 'month', 'week',
    'accidents', 'fatalities',
    'total_gdp', 'gdp_per_capita', 'unemployment',
    'population', 'vehicles', 'inflation_rate', 'week_index'
]].copy()

#Quarter

acc_w_ind_y_mod['quarter'] = ((acc_w_ind_y_mod['month'] - 1) // 3) + 1

#Map seasons

def get_season_code(month):
    if month in [12, 1, 2]:
        return 1  # Winter
    elif month in [3, 4, 5]:
        return 2  # Spring
    elif month in [6, 7, 8]:
        return 3  # Summer
    else:
        return 4  # Autumn

#Apply season code

acc_w_ind_y_mod['season'] = acc_w_ind_y_mod['month'].apply(get_season_code)

#Set is_summer and is_winter flags as 1/0

acc_w_ind_y_mod['is_summer'] = (acc_w_ind_y_mod['season'] == 3).astype(int)
acc_w_ind_y_mod['is_winter'] = (acc_w_ind_y_mod['season'] == 1).astype(int)
def compute_week_date(row):
    day = {1: 1, 2: 8, 3: 15, 4: 22}.get(int(row['week']), 1)
    return pd.Timestamp(year=int(row['year']), month=int(row['month']), day=day)

#Apply to Df

acc_w_ind_y_mod['week_date'] = acc_w_ind_y_mod.apply(compute_week_date, axis=1)

#Easter

def compute_orthodox_easter(year):
    a = year % 19
    b = year % 4
    c = year % 7
    d = (19 * a + 15) % 30
    e = (2 * b + 4 * c + 6 * d + 6) % 7
    f = d + e
    return datetime(year, 4, 4) + timedelta(days=f)

#Other Holidays

def mark_greek_holidays(df, date_col='week_date'):
    df = df.copy()
    df['year'] = df[date_col].dt.year
    stationary_holidays = [
        "-01-01",  # New Year's Day
        "-01-06",  # Epiphany
        "-03-25",  # Independence Day
        "-05-01",  # Labor Day
        "-08-15",  # Assumption of Mary
        "-10-28",  # Oxi Day
        "-12-24",  # Christmas Eve
        "-12-25",  # Christmas Day
    ]
    df['is_holiday'] = df[date_col].dt.strftime("-%m-%d").isin(stationary_holidays).astype(int)
    holiday_flags = []
    for idx, row in df.iterrows():
        date = row[date_col]
        easter = compute_orthodox_easter(row['year'])

        movable_holidays = {
            easter - timedelta(days=48),  # Clean Monday
            easter - timedelta(days=3),   # Holy Friday
            easter - timedelta(days=2),   # Holy Saturday
            easter,                       # Easter Sunday
            easter + timedelta(days=1),   # Easter Monday
            easter + timedelta(days=50),  # Pentecost
        }
        holiday_flags.append(int(date in movable_holidays))
    df['is_holiday'] = np.maximum(df['is_holiday'], holiday_flags)
    return df
acc_w_ind_y_mod = mark_greek_holidays(acc_w_ind_y_mod, date_col='week_date')

#Lags

lag_weeks = [1,2, 3, 4, 6, 7, 8, 12, 24, 52]
for lag in lag_weeks:
    acc_w_ind_y_mod[f'Accidents_Lag{lag}'] = acc_w_ind_y_mod['accidents'].shift(lag)
    acc_w_ind_y_mod[f'Fatalities_Lag{lag}'] = acc_w_ind_y_mod['fatalities'].shift(lag)

#Rolling Averages

rolling_windows = [4, 7, 12, 24, 52]
for w in rolling_windows:
    acc_w_ind_y_mod[f'Rolling_Avg_{w}w_a'] = acc_w_ind_y_mod['accidents'].rolling(window=w, min_periods=1).mean()
for w in rolling_windows:
    acc_w_ind_y_mod[f'Rolling_Avg_{w}w_f'] = acc_w_ind_y_mod['fatalities'].rolling(window=w, min_periods=1).mean()

#Rolling Sums

rolling_sum_windows = [4, 8, 12, 24]
for w in rolling_sum_windows:
    acc_w_ind_y_mod[f'RollingSum_{w}w_a'] = acc_w_ind_y_mod['accidents'].rolling(window=w, min_periods=1).sum()
for w in rolling_sum_windows:
    acc_w_ind_y_mod[f'RollingSum_{w}w_f'] = acc_w_ind_y_mod['fatalities'].rolling(window=w, min_periods=1).sum()

#Cumulative Sums

acc_w_ind_y_mod['CumulativeAccidentsY'] = acc_w_ind_y_mod.groupby('year')['accidents'].cumsum()
acc_w_ind_y_mod['CumulativeFatalitiesY'] = acc_w_ind_y_mod.groupby('year')['fatalities'].cumsum()
acc_w_ind_y_mod['CumulativeAccidents'] = acc_w_ind_y_mod['accidents'].cumsum()
acc_w_ind_y_mod['CumulativeFatalities'] = acc_w_ind_y_mod['fatalities'].cumsum()

##Other Indexes


#Growth and Change Rates

df = acc_w_ind_y_mod.copy()
df['AccidentGrowthRate'] = (df['accidents'] - df['accidents'].shift(1)) / df['accidents'].shift(1)
df['FatalityGrowthRate'] = (df['fatalities'] - df['fatalities'].shift(1)) / df['fatalities'].shift(1)
df['FatalityRate'] = df['fatalities'] / df['accidents']
df['AccidentChangeRate'] = (df['accidents'] - df['Accidents_Lag1']) / df['Accidents_Lag1']
df['FatalityChangeRate'] = (df['fatalities'] - df['Fatalities_Lag1']) / df['Fatalities_Lag1']

# Per Capita and Per Vehicle

df['AccidentsPerCapita'] = df['accidents'] / df['population']
df['FatalitiesPerCapita'] = df['fatalities'] / df['population']
df['AccidentsPerVehicle'] = df['accidents'] / df['vehicles']

#Interaction variables

df['AccidentsUnemployment'] = df['accidents'] * df['unemployment']
df['FatalitiesInflation'] = df['fatalities'] * df['inflation_rate']

#Economic crisis dummy

df['economic_crisis'] = np.where((df['year'] >= 2009) & (df['year'] <= 2018), 1, 0)

# COVID dummy

df['week_date'] = pd.to_datetime(df['week_date'])
covid_start = pd.to_datetime("2020-03-01")
covid_end = pd.to_datetime("2022-05-31")
df['covid'] = np.where((df['week_date'] >= covid_start) & (df['week_date'] <= covid_end), 1, 0)

#New highways dummy

df['new_highways'] = np.where(df['year'] >= 2017, 1, 0)

#Policy dummies

df['speed_limits']    = np.where(df['year'] >= 1999, 1, 0)
df['seat_belt']       = np.where(df['year'] >= 1999, 1, 0)
df['helmet']          = np.where(df['year'] >= 1999, 1, 0)
df['alcohol']         = np.where(df['year'] >= 1999, 1, 0)
df['phone']           = np.where(df['year'] >= 1999, 1, 0)
df['licensing']       = np.where(df['year'] >= 1999, 1, 0)
df['airbags']         = np.where(df['year'] >= 2009, 1, 0)
df['abs']             = np.where(df['year'] >= 2009, 1, 0)
df['kid_seat']        = np.where(df['year'] >= 1999, 1, 0)
df['fines']           = np.where(df['year'] >= 2007, 1, 0)
df['pedestrians']     = np.where(df['year'] >= 2011, 1, 0)
df['kteo']            = np.where(df['year'] >= 1999, 1, 0)
df['point_system']    = np.where(df['year'] >= 2007, 1, 0)

#Seasons Dummies

df = pd.get_dummies(df, columns=['season'], prefix='season')
df = df.astype({col: 'int' for col in df.columns if col.startswith('season_')})
df = df.fillna(0)

#Check

acc_w_ind_y_mod = df
acc_w_ind_y_mod.to_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Python/Main_Code/Weekly_LSTM/Accidents_LSTM_Prediction/Accident_Forecast_DFs/Accidents_Forecast_Df.xlsx", index=False)

#Hyperparameter Grid

time_steps_list = [1]
lstm_units_list = [64, 128, 256]
dense_units_list = [64, 128, 256]
dropout_list = [0.2, 0.3]
loss_list = ['mse', 'huber']
learning_rates = [0.001, 0.005]
batch_size_list = [16, 32, 64]
epoch_list = [50, 100, 200]
results = []

#Evaluation Metrics

def smape(y_true, y_pred):
    return 100 * np.mean(2 * np.abs(y_pred - y_true) / (np.abs(y_pred) + np.abs(y_true) + 1e-8))
def mape(y_true, y_pred):
    return 100 * np.mean(np.abs((y_true - y_pred) / (y_true + 1e-8)))
def mase(y_true, y_pred, y_train):
    naive_forecast = np.mean(np.abs(np.diff(y_train)))
    return np.mean(np.abs(y_true - y_pred)) / (naive_forecast + 1e-8)
def wape(y_true, y_pred):
    return 100 * np.sum(np.abs(y_true - y_pred)) / (np.sum(np.abs(y_true)) + 1e-8)

#LSTM Model

class LSTMModel(nn.Module):
    def __init__(self, input_dim, lstm_units, dense_units, dropout):
        super(LSTMModel, self).__init__()
        self.lstm1 = nn.LSTM(input_dim, lstm_units, batch_first=True)
        self.dropout1 = nn.Dropout(dropout)
        self.lstm2 = nn.LSTM(lstm_units, lstm_units, batch_first=True)
        self.dropout2 = nn.Dropout(dropout)
        self.fc1 = nn.Linear(lstm_units, dense_units)
        self.relu1 = nn.ReLU()
        self.fc2 = nn.Linear(dense_units, dense_units // 2)
        self.relu2 = nn.ReLU()
        self.fc3 = nn.Linear(dense_units // 2, 1)
    def forward(self, x):
        x, _ = self.lstm1(x)
        x = self.dropout1(x)
        x, _ = self.lstm2(x)
        x = self.dropout2(x)
        x = x[:, -1, :]  # Get last time step
        x = self.relu1(self.fc1(x))
        x = self.relu2(self.fc2(x))
        x = self.fc3(x)
        return x

##Data Prep


# Exclude certain columns

df = acc_w_ind_y_mod.copy()
drop_cols = ['week_date', 'year', 'month', 'week', 'week_index']
target_col = 'accidents'
feature_cols = [col for col in df.columns if col not in drop_cols + [target_col]]

# Separate binary columns (0–1) to skip scaling

binary_cols = [col for col in feature_cols if df[col].dropna().isin([0,1]).all()]
to_scale_cols = [col for col in feature_cols if col not in binary_cols]
df = df.sort_values(by=["year", "month", "week"]).reset_index(drop=True)

# Split based on year

train_df = df[df["year"] <= 2017].copy()
test_df = df[df["year"] >= 2018].copy()

# Scale features (exclude binary)

scaler_X = StandardScaler()
train_df[to_scale_cols] = scaler_X.fit_transform(train_df[to_scale_cols])
test_df[to_scale_cols] = scaler_X.transform(test_df[to_scale_cols])

# Scale target

scaler_y = StandardScaler()
train_df[target_col] = scaler_y.fit_transform(train_df[[target_col]])
test_df[target_col] = scaler_y.transform(test_df[[target_col]])

#Grid Search

for combo in itertools.product(time_steps_list, lstm_units_list, dense_units_list,
                               dropout_list, loss_list, learning_rates, batch_size_list, epoch_list):
    time_steps, lstm_units, dense_units, dropout, loss_fn_str, lr, batch_size, epochs = combo
    def create_sequences(data_df, time_steps):
        data = data_df[feature_cols + [target_col]].values
        X, y = [], []
        if time_steps == 1:
            for i in range(len(data)):
                X.append(data[i:i + 1, :-1])
                y.append(data[i, -1])
        else:
            for i in range(time_steps, len(data)):
                X.append(data[i - time_steps:i, :-1])
                y.append(data[i, -1])
        return np.array(X), np.array(y)
    X_train, y_train = create_sequences(train_df, time_steps)
    X_test, y_test = create_sequences(test_df, time_steps)
    train_targets = scaler_y.inverse_transform(y_train.reshape(-1, 1)).flatten()

    #Torch datasets

    train_ds = TensorDataset(torch.tensor(X_train, dtype=torch.float32), torch.tensor(y_train, dtype=torch.float32))
    test_ds = TensorDataset(torch.tensor(X_test, dtype=torch.float32), torch.tensor(y_test, dtype=torch.float32))
    train_loader = DataLoader(train_ds, batch_size=batch_size, shuffle=False)
    test_loader = DataLoader(test_ds, batch_size=batch_size, shuffle=False)

    #Model

    model = LSTMModel(input_dim=X_train.shape[2], lstm_units=lstm_units, dense_units=dense_units, dropout=dropout)
    model = model.cuda() if torch.cuda.is_available() else model
    optimizer = optim.Adam(model.parameters(), lr=lr)
    criterion = MSELoss() if loss_fn_str == 'mse' else HuberLoss()

    #Training loop

    for epoch in range(epochs):
        print(f"  Epoch {epoch + 1}/{epochs}")
        model.train()
        for xb, yb in train_loader:
            xb, yb = xb.cuda(), yb.cuda() if torch.cuda.is_available() else (xb, yb)
            optimizer.zero_grad()
            preds = model(xb).squeeze()
            loss = criterion(preds, yb)
            loss.backward()
            optimizer.step()

    #Evaluation

    model.eval()
    preds_list, targets_list = [], []
    with torch.no_grad():
        for xb, yb in test_loader:
            xb = xb.cuda() if torch.cuda.is_available() else xb
            preds = model(xb).squeeze().cpu().numpy()
            targets = yb.numpy()
            preds_list.extend(preds)
            targets_list.extend(targets)

    # Convert predictions & targets back to original scale

    preds = scaler_y.inverse_transform(np.array(preds_list).reshape(-1, 1)).flatten()
    targets = scaler_y.inverse_transform(np.array(targets_list).reshape(-1, 1)).flatten()


    #Metrics
    mae = mean_absolute_error(targets, preds)
    rmse = np.sqrt(mean_squared_error(targets, preds))
    r2 = r2_score(targets, preds)
    mape_val = mape(targets, preds)
    smape_val = smape(targets, preds)
    mase_val = mase(targets, preds, train_targets)
    wape_val = wape(targets, preds)
    print("\n----------------------------------")
    print(f"Params → Time Steps: {time_steps}, LSTM: {lstm_units}, Dense: {dense_units}, Dropout: {dropout}, Loss: {loss_fn_str}, LR: {lr}, Batch: {batch_size}, Epochs: {epochs}")
    print(f"MAE: {mae:.4f}, RMSE: {rmse:.4f}, R²: {r2:.4f}")
    print(f"MAPE: {mape_val:.2f}%, SMAPE: {smape_val:.2f}%, MASE: {mase_val:.4f}, WAPE: {wape_val:.2f}%")
    print("----------------------------------")
    results.append({
        "Time Steps": time_steps,
        "LSTM Units": lstm_units,
        "Dense Units": dense_units,
        "Dropout": dropout,
        "Loss Function": loss_fn_str,
        "Learning Rate": lr,
        "Batch Size": batch_size,
        "Epochs": epochs,
        "MAE": mae,
        "RMSE": rmse,
        "R2": r2,
        "MAPE": mape_val,
        "SMAPE": smape_val,
        "MASE": mase_val,
        "WAPE": wape_val
    })

#Print Results

results_df = pd.DataFrame(results)
results_df.to_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Python/Main_Code/Weekly_LSTM/Accidents_LSTM_Prediction/LSTM_Hparams_Evaluation_Accidents/LSTM_Gridsearch_Results_Accidents.xlsx", index=False)
print("📁 Grid search results saved as 'LSTM_Gridsearch_Results_Accidents.xlsx'")

