import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset
from sklearn.preprocessing import StandardScaler
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random

random.seed(1312)

#Config

TARGET = 'accidents'
TIME_STEPS = 48
LSTM_UNITS = 256
DENSE_UNITS = 64
DROPOUT = 0.2
LOSS_FN = nn.MSELoss()
LR = 0.001
BATCH_SIZE = 64
EPOCHS = 200

#Load Dfs (Frozen Df Creation also with Frozen values of 2022, then fill the Frozen Df)

df_base = pd.read_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Python/Main_Code/Monthly_LSTM/Accidents_LSTM_Prediction/Accident_Forecast_DFs/Accidents_Forecast_Df.xlsx")
df_frozen = pd.read_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Python/Main_Code/Monthly_LSTM/Accidents_LSTM_Prediction/Accident_Forecast_DFs/Accidents_Forecast_Df_Frozen.xlsx")

#Drop non-feature columns

drop_cols = ['month_date', 'year', 'month']
feature_cols = [col for col in df_base.columns if col not in drop_cols + [TARGET]]
binary_cols = [col for col in feature_cols if df_base[col].dropna().isin([0, 1]).all()]
to_scale_cols = [col for col in feature_cols if col not in binary_cols]

# Train data up to 2022

df_train = df_base[df_base["year"] <= 2022].copy().sort_values(by=["year", "month"]).reset_index(drop=True)

#Scale features

scaler_X = StandardScaler()
scaler_y = StandardScaler()
df_train[to_scale_cols] = scaler_X.fit_transform(df_train[to_scale_cols])
df_train[TARGET] = scaler_y.fit_transform(df_train[[TARGET]])

# Create sequences

X = df_train[feature_cols].values
y = df_train[TARGET].values
X_seq = []
y_seq = []
for i in range(len(X) - TIME_STEPS):
    X_seq.append(X[i:i+TIME_STEPS])
    y_seq.append(y[i+TIME_STEPS])
X_seq = np.array(X_seq)
y_seq = np.array(y_seq)

# Torch dataset

train_ds = TensorDataset(torch.tensor(X_seq, dtype=torch.float32),
                         torch.tensor(y_seq, dtype=torch.float32))
train_loader = DataLoader(train_ds, batch_size=BATCH_SIZE, shuffle=False)

#Define Model

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
        x = x[:, -1, :]
        x = self.relu1(self.fc1(x))
        x = self.relu2(self.fc2(x))
        x = self.fc3(x)
        return x

#Train Model

model = LSTMModel(input_dim=X.shape[1], lstm_units=LSTM_UNITS,
                  dense_units=DENSE_UNITS, dropout=DROPOUT)
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model = model.to(device)
optimizer = optim.Adam(model.parameters(), lr=LR)
for epoch in range(EPOCHS):
    model.train()
    for xb, yb in train_loader:
        xb, yb = xb.to(device), yb.to(device)
        optimizer.zero_grad()
        preds = model(xb).squeeze()
        loss = LOSS_FN(preds, yb)
        loss.backward()
        optimizer.step()
    if (epoch + 1) % 20 == 0:
        print(f"Epoch {epoch+1}/{EPOCHS} - Loss: {loss.item():.4f}")

#Recursive Prediction Loop

df_frozen = df_frozen.sort_values(by=["year", "month"]).reset_index(drop=True)
last_known_index = df_frozen[df_frozen['year'] == 2022].index.max()
for i in range(last_known_index + 1, len(df_frozen)):
    row = df_frozen.iloc[i]
    for lag in [1,2,3,4,6,7,8,12,24]:
        df_frozen[f'Accidents_Lag{lag}'] = df_frozen['accidents'].shift(lag)
    for w in [4, 7, 12]:
        df_frozen[f'Rolling_Avg_{w}m_a'] = df_frozen['accidents'].rolling(window=w, min_periods=1).mean()
    for w in [3, 6, 12, 24]:
        df_frozen[f'RollingSum_{w}m_a'] = df_frozen['accidents'].rolling(window=w, min_periods=1).sum()
    df_frozen['CumulativeAccidents'] = df_frozen['accidents'].cumsum()
    df_frozen['CumulativeAccidentsY'] = df_frozen.groupby('year')['accidents'].cumsum()
    df_frozen['AccidentGrowthRate'] = (df_frozen['accidents'] - df_frozen['accidents'].shift(1)) / (df_frozen['accidents'].shift(1) + 1e-8)
    df_frozen['AccidentChangeRate'] = (df_frozen['accidents'] - df_frozen['Accidents_Lag1']) / (df_frozen['Accidents_Lag1'] + 1e-8)
    df_frozen['AccidentsPerCapita'] = df_frozen['accidents'] / df_frozen['population']
    df_frozen['AccidentsPerVehicle'] = df_frozen['accidents'] / df_frozen['vehicles']
    df_frozen['AccidentsUnemployment'] = df_frozen['accidents'] * df_frozen['unemployment']
    if i < TIME_STEPS:
        print(f"⛔ Not enough history to predict for index {i}")
        continue
    else:
        window = df_frozen.iloc[i - TIME_STEPS:i].copy()
        window[feature_cols] = window[feature_cols].fillna(0)
        window[to_scale_cols] = scaler_X.transform(window[to_scale_cols])
        X_input = window[feature_cols].values.reshape(1, TIME_STEPS, -1)
        X_input_tensor = torch.tensor(X_input, dtype=torch.float32).to(device)
        model.eval()
        with torch.no_grad():
            pred_scaled = model(X_input_tensor).cpu().numpy().flatten()
        pred_original = scaler_y.inverse_transform(pred_scaled.reshape(-1, 1)).flatten()[0]
        df_frozen.at[i, 'accidents'] = pred_original
        print(f"✅ Month {i + 1}/{len(df_frozen)} — {int(row['year'])}-W{int(row['month'])}: {pred_original:.2f}")

#Save final forecast

df_frozen.to_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Python/Main_Code/Monthly_LSTM/Accidents_LSTM_Prediction/Recursive_Prediction_Accidents/Recursive_Forecast_Accidents.xlsx", index=False)
print("📁 Saved full recursive forecast to 'Recursive_Forecast_Accidents.xlsx'")


# Step 1: Load actual data and sort

df_actual = pd.read_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Python/Main_Code/Monthly_LSTM/Accidents_LSTM_Prediction/Accident_Forecast_DFs/Accidents_Forecast_Df.xlsx")
df_actual = df_actual[df_actual["year"] <= 2022].copy()
df_actual = df_actual.sort_values(by=["year", "month"]).reset_index(drop=True)

# Step 2: Use Frozen Df with Forecasts for 2023–2026

df_forecast = df_frozen[df_frozen["year"] >= 2023].copy()
df_forecast = df_forecast.sort_values(by=["year", "month"]).reset_index(drop=True)

#Generate synthetic monthly dates for forecast

last_date = df_actual["month_date"].iloc[-1]
n_months_forecast = len(df_forecast)
future_dates = pd.date_range(start=last_date + pd.DateOffset(months=1), periods=n_months_forecast, freq="MS")

# Assign synthetic dates to forecast rows

df_forecast["month_date"] = future_dates

#Combine actual + forecast

df_all = pd.concat([df_actual, df_forecast], ignore_index=True)

#Plot

plt.figure(figsize=(16, 6))
plt.plot(df_all["month_date"], df_all["accidents"], label="Accidents (Actual + Forecast)", color='blue')
plt.axvline(x=last_date, color='red', linestyle='--', label="Forecast Start (2023)")
plt.title("Monthly Accidents in Greece (1996–2026)", fontsize=14)
plt.xlabel("Month Date", fontsize=12)
plt.ylabel("Accidents", fontsize=12)
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.xticks(rotation=45)
plt.show()
