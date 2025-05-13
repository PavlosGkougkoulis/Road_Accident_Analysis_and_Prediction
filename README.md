# Road Accident Analysis and Prediction

## Description
This repository contains the full pipeline for preprocessing, feature engineering, ARIMA/SARIMA statistical modeling, and deep learning with LSTM for accident prediction. Includes recursive forecasting and performance evaluation across weekly and monthly data.

## Methodology:

### Overview
The core methodology of this thesis revolves around building and evaluating time series forecasting models for predicting road accidents and traffic-related fatalities in Greece. The models were designed using a dataset covering the period from 1996 to 2022, at both weekly and monthly levels. A systematic, multi-stage pipeline was implemented—starting from database construction and preprocessing, continuing through extensive feature engineering, and culminating in the development of both classical statistical models (ARIMA/SARIMA) and machine learning models (LSTM neural networks).

### Data Preprocessing and Feature Engineering
Initial data was collected from the SANTRA accident database and augmented with a wide range of external variables, including macroeconomic indicators, fuel prices, seasonal markers, holidays, and traffic policy flags. The preprocessing phase involved restructuring raw time-indexed data into a clean tabular format suitable for time series modeling. This included the transformation of date fields, the creation of lagged variables (week/month/year lags), rolling means, cumulative sums, and binary indicators for legislative changes. These operations were performed using R scripts and exported from a PostgreSQL relational schema designed for structured query efficiency.

### Classical Time Series Modeling (ARIMA / SARIMA)
Statistical models were constructed using the ARIMA and SARIMA families to account for both trend and seasonality in the time series. A hybrid model selection strategy was followed: initial parameter ranges were determined through manual ACF/PACF analysis and differencing, while model tuning was further optimized using auto.arima and exhaustive grid search techniques. Residual diagnostics (Ljung-Box tests, AIC/BIC analysis, residual normality checks) ensured robustness in the selected configurations. Separate models were fit for accident and fatality counts across both weekly and monthly series, and log transformations were applied when appropriate to stabilize variance.

### LSTM Neural Networks Implementation
The most advanced part of the methodology involved implementing Long Short-Term Memory (LSTM) neural networks for multi-feature, multi-horizon forecasting. The networks were trained in Python using TensorFlow and PyTorch, with a standardized pipeline that normalized numerical features and structured input-output sequences for one-step predictions. Hyperparameter tuning was performed using grid search over time steps, LSTM units, dense layers, dropout rates, loss functions, and learning rates. The model was evaluated across several forecast horizons and retrained using recursive prediction to simulate long-term accident evolution.

### Recursive Forecasting Strategy
A recursive forecasting mechanism was developed to simulate weekly and monthly accident/fatality predictions beyond the available dataset, projecting values up to 2026. After each predicted time step, the resulting forecast was fed back into the input window, with rolling features and cumulative indicators being recomputed dynamically using updated values. This recursive loop allowed the LSTM model to maintain temporal continuity and preserve internal feature dynamics during long-term prediction, mimicking real-world deployment scenarios where future inputs are not available.

### Comparative Evaluation
All models—both classical and neural—were assessed using a unified evaluation framework across standard forecasting metrics (MAE, RMSE, MAPE, SMAPE, R², WAPE, and MASE). The evaluation was performed independently on the same hold-out test sets, ensuring fair comparisons. LSTM models consistently achieved higher accuracy, particularly in scenarios involving multi-variable inputs and short-term trends. The recursive setup also allowed for stress-testing the model's behavior under compounding uncertainty, demonstrating its ability to maintain predictive stability over extended horizons.
