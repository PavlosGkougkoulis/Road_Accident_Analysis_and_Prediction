import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset
from torch.nn import HuberLoss
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random

random.seed(1312)

#Load Preprocessed Data

df = pd.read_excel("E:/University/Thesis/Thesis_Main/Thesis_Code/Python/Main_Code/Monthly_LSTM/Fatalities_LSTM_Prediction/Fatalities_Forecast_DFs/Fatalities_Forecast_Df.xlsx")

#Hyperparameters (Change with Random Seed)

time_steps = 1
lstm_units = 256
dense_units = 256
dropout = 0.2
loss_fn_str = "huber"
lr = 0.001
batch_size = 64
epochs = 200
target_col = 'fatalities'
drop_cols = ['month_date', 'year', 'month']
feature_cols = [col for col in df.columns if col not in drop_cols + [target_col]]

#Custom Metrics

def smape(y_true, y_pred):
    return 100 * np.mean(2 * np.abs(y_pred - y_true) / (np.abs(y_pred) + np.abs(y_true) + 1e-8))
def mape(y_true, y_pred):
    return 100 * np.mean(np.abs((y_true - y_pred) / (y_true + 1e-8)))
def mase(y_true, y_pred, y_train):
    naive_forecast = np.mean(np.abs(np.diff(y_train)))
    return np.mean(np.abs(y_true - y_pred)) / (naive_forecast + 1e-8)
def wape(y_true, y_pred):
    return 100 * np.sum(np.abs(y_true - y_pred)) / (np.sum(np.abs(y_true)) + 1e-8)

#Split & Scale

train_df = df[df["year"] <= 2017].copy()
test_df = df[df["year"] >= 2018].copy()
binary_cols = [col for col in feature_cols if df[col].dropna().isin([0, 1]).all()]
to_scale_cols = [col for col in feature_cols if col not in binary_cols]
scaler_X = StandardScaler()
train_df[to_scale_cols] = scaler_X.fit_transform(train_df[to_scale_cols])
test_df[to_scale_cols] = scaler_X.transform(test_df[to_scale_cols])
scaler_y = StandardScaler()
train_df[target_col] = scaler_y.fit_transform(train_df[[target_col]])
test_df[target_col] = scaler_y.transform(test_df[[target_col]])

#Create Sequences

def create_sequences(data_df):
    data = data_df[feature_cols + [target_col]].values
    X, y = [], []
    for i in range(len(data)):
        X.append(data[i:i+1, :-1])
        y.append(data[i, -1])
    return np.array(X), np.array(y)
X_train, y_train = create_sequences(train_df)
X_test, y_test = create_sequences(test_df)
train_targets_orig = scaler_y.inverse_transform(y_train.reshape(-1, 1)).flatten()

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

#Train

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model = LSTMModel(input_dim=X_train.shape[2], lstm_units=lstm_units, dense_units=dense_units, dropout=dropout).to(device)
optimizer = optim.Adam(model.parameters(), lr=lr)
criterion = HuberLoss() if loss_fn_str == "huber" else nn.MSELoss()
train_loader = DataLoader(TensorDataset(
    torch.tensor(X_train, dtype=torch.float32),
    torch.tensor(y_train, dtype=torch.float32)
), batch_size=batch_size, shuffle=False)
print("\n🧠 Training LSTM model...\n")
model.train()
for epoch in range(epochs):
    epoch_losses = []
    for xb, yb in train_loader:
        xb, yb = xb.to(device), yb.to(device)
        optimizer.zero_grad()
        preds = model(xb).squeeze()
        loss = criterion(preds, yb)
        loss.backward()
        optimizer.step()
        epoch_losses.append(loss.item())
    print(f"Epoch {epoch + 1:3d}/{epochs} — Loss: {np.mean(epoch_losses):.4f}")

#Evaluate

model.eval()
preds_list, targets_list = [], []
with torch.no_grad():
    for i in range(len(X_test)):
        x = torch.tensor(X_test[i:i+1], dtype=torch.float32).to(device)
        pred = model(x).cpu().numpy().flatten()[0]
        preds_list.append(pred)
        targets_list.append(y_test[i])

#Inverse Scale

preds = scaler_y.inverse_transform(np.array(preds_list).reshape(-1, 1)).flatten()
targets = scaler_y.inverse_transform(np.array(targets_list).reshape(-1, 1)).flatten()

#Metrics

mae = mean_absolute_error(targets, preds)
rmse = np.sqrt(mean_squared_error(targets, preds))
r2 = r2_score(targets, preds)
mape_val = mape(targets, preds)
smape_val = smape(targets, preds)
mase_val = mase(targets, preds, train_targets_orig)
wape_val = wape(targets, preds)
print("\n" + "="*60)
print("🔧 Model Parameters")
print("-" * 60)
print(f"Time Steps     : {time_steps}")
print(f"LSTM Units     : {lstm_units}")
print(f"Dense Units    : {dense_units}")
print(f"Dropout        : {dropout}")
print(f"Loss Function  : {loss_fn_str}")
print(f"Learning Rate  : {lr}")
print(f"Batch Size     : {batch_size}")
print(f"Epochs         : {epochs}")
print("\n📊 Evaluation Metrics")
print("-" * 60)
print(f"MAE            : {mae:.4f}")
print(f"RMSE           : {rmse:.4f}")
print(f"R²             : {r2:.4f}")
print(f"MAPE           : {mape_val:.2f}%")
print(f"SMAPE          : {smape_val:.2f}%")
print(f"MASE           : {mase_val:.4f}")
print(f"WAPE           : {wape_val:.2f}%")
print("="*60 + "\n")

#Plot Actual vs Predicted

plt.figure(figsize=(12, 5))
plt.plot(targets, label="Actual", marker='o')
plt.plot(preds, label="Predicted", marker='x')
plt.title("Actual vs Predicted Fatalities (2018+)")
plt.xlabel("Months (Test Set)")
plt.ylabel("Fatalities")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
residuals = targets - preds
plt.figure(figsize=(10, 4))
plt.plot(residuals, label="Residuals", color="purple")
plt.axhline(0, linestyle="--", color="gray")
plt.title("Residuals Over Time (Actual - Predicted)")
plt.xlabel("Months (Test Set)")
plt.ylabel("Error")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
plt.figure(figsize=(8, 4))
plt.hist(residuals, bins=30, color="skyblue", edgecolor="black")
plt.title("Histogram of Residuals")
plt.xlabel("Error")
plt.ylabel("Frequency")
plt.tight_layout()
plt.show()
plt.figure(figsize=(6, 6))
plt.scatter(targets, preds, alpha=0.7, color="green")
plt.plot([min(targets), max(targets)], [min(targets), max(targets)], 'r--')  # perfect prediction line
plt.xlabel("Actual Fatalities")
plt.ylabel("Predicted Fatalities")
plt.title("Predicted vs Actual (Scatter)")
plt.grid(True)
plt.tight_layout()
plt.show()
rolling_error = pd.Series(np.abs(residuals)).rolling(window=4).mean()
plt.figure(figsize=(10, 4))
plt.plot(rolling_error, label="4-Months Rolling MAE", color="orange")
plt.title("Rolling Mean Absolute Error (4-month)")
plt.xlabel("Months (Test Set)")
plt.ylabel("Absolute Error")
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

