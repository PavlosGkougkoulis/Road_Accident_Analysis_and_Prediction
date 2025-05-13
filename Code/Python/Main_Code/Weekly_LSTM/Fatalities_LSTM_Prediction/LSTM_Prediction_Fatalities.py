import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset
from torch.nn import HuberLoss, MSELoss
from sklearn.preprocessing import StandardScaler
import pandas as pd
import numpy as np
import random

random.seed(1312)

#Load preprocessed dataset

df = pd.read_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Python/Main_Code/Weekly_LSTM/Fatalities_LSTM_Prediction/Fatalities_Forecast_DFs/Fatalities_Forecast_Df.xlsx")

#Settings

target_col = 'fatalities'
drop_cols = ['week_date', 'year', 'month', 'week', 'week_index']
feature_cols = [col for col in df.columns if col not in drop_cols + [target_col]]

# Separate columns to scale

binary_cols = [col for col in feature_cols if df[col].dropna().isin([0, 1]).all()]
to_scale_cols = [col for col in feature_cols if col not in binary_cols]

#Filter training data

train_df = df[df["year"] <= 2022].copy()
scaler_X = StandardScaler()
scaler_y = StandardScaler()
train_df[to_scale_cols] = scaler_X.fit_transform(train_df[to_scale_cols])
train_df[target_col] = scaler_y.fit_transform(train_df[[target_col]])

#Create sequences

def create_sequences(data_df):
    data = data_df[feature_cols + [target_col]].values
    X, y = [], []
    for i in range(len(data)):
        X.append(data[i:i+1, :-1])
        y.append(data[i, -1])
    return np.array(X), np.array(y)
X_train, y_train = create_sequences(train_df)

#Define LSTM Model

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
        x = x[:, -1, :]  # last timestep
        x = self.relu1(self.fc1(x))
        x = self.relu2(self.fc2(x))
        x = self.fc3(x)
        return x

#Train Model

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model = LSTMModel(input_dim=X_train.shape[2], lstm_units=256, dense_units=64, dropout=0.2).to(device)
optimizer = optim.Adam(model.parameters(), lr=0.001)
criterion = MSELoss()
train_loader = DataLoader(TensorDataset(
    torch.tensor(X_train, dtype=torch.float32),
    torch.tensor(y_train, dtype=torch.float32)
), batch_size=16, shuffle=False)
print("\n🧠 Training LSTM Model...\n")
model.train()
for epoch in range(50):
    epoch_losses = []
    for xb, yb in train_loader:
        xb, yb = xb.to(device), yb.to(device)
        optimizer.zero_grad()
        preds = model(xb).squeeze()
        loss = criterion(preds, yb)
        loss.backward()
        optimizer.step()
        epoch_losses.append(loss.item())
    avg_loss = np.mean(epoch_losses)
    print(f"Epoch {epoch+1:3d}/200 — Loss: {avg_loss:.4f}")

#Predict Week 1 of 2023

last_X = torch.tensor(X_train[-1:], dtype=torch.float32).to(device)
model.eval()
with torch.no_grad():
    pred_scaled = model(last_X).cpu().numpy().flatten()
    pred_actual = scaler_y.inverse_transform(pred_scaled.reshape(-1, 1)).flatten()[0]
print(f"\n🚗 Predicted Fatalities for Week 1 of 2023: {pred_actual:.2f}")
