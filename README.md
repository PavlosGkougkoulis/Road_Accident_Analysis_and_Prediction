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
The most advanced part of the methodology involved implementing Long Short-Term Memory (LSTM) neural networks for multi-feature, multi-horizon forecasting. The networks were trained in Python using PyTorch, with a standardized pipeline that normalized numerical features and structured input-output sequences for one-step predictions. Hyperparameter tuning was performed using grid search over time steps, LSTM units, dense layers, dropout rates, loss functions, and learning rates. The model was evaluated across several forecast horizons and retrained using recursive prediction to simulate long-term accident evolution.

### Recursive Forecasting Strategy
A recursive forecasting mechanism was developed to simulate weekly and monthly accident/fatality predictions beyond the available dataset, projecting values up to 2026. After each predicted time step, the resulting forecast was fed back into the input window, with rolling features and cumulative indicators being recomputed dynamically using updated values. This recursive loop allowed the LSTM model to maintain temporal continuity and preserve internal feature dynamics during long-term prediction, mimicking real-world deployment scenarios where future inputs are not available.

### Comparative Evaluation
All models—both classical and neural—were assessed using a unified evaluation framework across standard forecasting metrics (MAE, RMSE, MAPE, SMAPE, R², WAPE, and MASE). The evaluation was performed independently on the same hold-out test sets, ensuring fair comparisons. LSTM models consistently achieved higher accuracy, particularly in scenarios involving multi-variable inputs and short-term trends. The recursive setup also allowed for stress-testing the model's behavior under compounding uncertainty, demonstrating its ability to maintain predictive stability over extended horizons.

## Configuring DB and Running Code in R and Python

### DB Creation with pgAdmin4 and PostgreSQL:
To replicate the database used in the Thesis, follow the steps below to install the necessary tools and restore the database from the provided backup:

**1. Install PostgreSQL and pgAdmin4**  
First, download and install the latest versions of [PostgreSQL](https://www.postgresql.org/download/) and [pgAdmin4](https://www.pgadmin.org/download/), which is a graphical interface for managing PostgreSQL databases.

**2. Launch pgAdmin4 and Create a New Database**  
After installation, open pgAdmin4, connect to your PostgreSQL server, and create a new empty database

**3. Restore the Database from Backup**  
- Right-click on the new database you created in pgAdmin.
- Select Restore.
- Under Format, choose Tar.
- Locate the backup file from the repository.
- Click Restore to populate the database with all tables, relationships, and data.

### R:  
The R script in this Repository already has the install.packages() and library() functions at the top, so anything needed to simulate the results in terms of libraries will be installed when you run the acoording code blocks.
The only change that will be needed is to replace the file paths of the datasets accordingly.
Also, [R](https://cran.r-project.org/) and [RStudio](https://posit.co/products/open-source/rstudio/) will be needed to simulate the Thesis' environment

### Python:  
To run the Python forecasting models in this Thesis, you will need to prepare a Python environment with the required dependencies.

**1. Install Python 3**  
Install the newest version of Python from the official [Python website](https://www.python.org/downloads/)

**2. Create a Virtual Environment**  
Create a virtual environment to isolate the project dependencies, using any Python IDE or from terminal like this:
<pre lang="markdown">#Create a virtual environment 
    
python -m venv venv 
    
#Activate the environment 
    
source venv/bin/activate # On Windows use: venv\Scripts\activate  
</pre>

**3. Install Required Python Packages**  
With your virtual environment activated, install all dependencies using the requirements.txt file provided:

<pre lang="markdown">pip install -r requirements.txt
</pre>

**4. Modify the Scripts file paths**  
To run the scripts on a different system, modify the file paths (e.g., dataset locations or database URLs) inside the Python scripts to match your local directory structure.


## Repository Structure and Files

```
Code/
    ├── Dfs/
        ├── Dfs_Extraction/
            ├── Monthly/
                ├── Csv/
                    └── Monthly_Accidents.csv
                ├── Excel/
                    └── Monthly_Accidents.xlsx
                ├── Available_Cols_Monthly.txt
                └── Cols_Included_Monthly.txt
            ├── Weekly/
                ├── Csv/
                    └── Weekly_Accidents.csv
                ├── Excel/
                    └── Weekly_Accidents.xlsx
                ├── Available_Cols_Weekly.txt
                └── Cols_Included_Weekly.txt
            └── Yearly/
                ├── Csv/
                    └── Yearly_Accidents.csv
                ├── Excel/
                    └── Yearly_Accidents.xlsx
                ├── Available_Cols_Yearly.txt
                └── Cols_Included_Yearly.txt
        ├── Dfs_for_Python/
            ├── acc.m.ind.y_mod.csv
            ├── acc.w.ind.y_mod.csv
            └── acc.y.ind.y_mod.csv
        ├── Dfs_LSTM/
            ├── Monthly_Accident_Dataset_LSTM.csv
            └── Weekly_Accident_Dataset_LSTM.csv
        ├── .RData
        ├── .Rhistory
        ├── ~$Desc_Stats_Acc_Monthly.xlsx
        ├── ~$Desc_Stats_Acc_Weekly.xlsx
        ├── ~$Desc_Stats_Acc_Yearly.xlsx
        ├── ARIMA_Acc_Monthly_Preds_GS.xlsx
        ├── ARIMA_Acc_Monthly_Preds.xlsx
        ├── ARIMA_Acc_Weekly_Preds.xlsx
        ├── ARIMA_Fat_Monthly_Preds.xlsx
        ├── ARIMA_Fat_Weekly_Preds.xlsx
        ├── Desc_Stats_Acc_Monthly.xlsx
        ├── Desc_Stats_Acc_Weekly.xlsx
        ├── Desc_Stats_Acc_Yearly.xlsx
        ├── monthly_accidents_yearly_indicators_filled.csv
        ├── monthly_accidents_yearly_indicators.csv
        ├── monthly_accidents.csv
        ├── SARIMA_Acc_Monthly_Preds.xlsx
        ├── SARIMA_Acc_Weekly_Preds.xlsx
        ├── SARIMA_Fat_Monthly_Preds_GS.xlsx
        ├── SARIMA_Fat_Monthly_Preds.xlsx
        ├── SARIMA_Fat_Weekly_Preds_GS.xlsx
        ├── SARIMA_Fat_Weekly_Preds.xlsx
        ├── weekly_accidents_yearly_indicators_filled.csv
        ├── weekly_accidents_yearly_indicators.csv
        ├── weekly_accidents.csv
        ├── yearly_accidents_yearly_indicators_filled.csv
        ├── yearly_accidents_yearly_indicators.csv
        ├── yearly_accidents.csv
        ├── yearly_indicators_filled.csv
        └── yearly_indicators.csv
    ├── Python/
        ├── Main_Code/
            ├── Monthly_LSTM/
                ├── Accidents_LSTM_Prediction/
                    ├── Accident_Forecast_DFs/
                        ├── Accidents_Forecast_Df_Frozen.xlsx
                        └── Accidents_Forecast_Df.xlsx
                    ├── LSTM_Hparams_Evaluation_Accidents/
                        └── LSTM_Gridsearch_Results_Accidents.xlsx
                    ├── Recursive_Prediction_Accidents/
                        └── Recursive_Forecast_Accidents.xlsx
                    ├── LSTM_Grid_Search_Accidents.py
                    ├── LSTM_Prediction_Accidents.py
                    ├── LSTM_Recursive_Accidents.py
                    └── LSTM_Training_Accidents.py
                └── Fatalities_LSTM_Prediction/
                    ├── Fatalities_Forecast_DFs/
                        ├── Fatalities_Forecast_Df_Frozen.xlsx
                        └── Fatalities_Forecast_Df.xlsx
                    ├── LSTM_Hparams_Evaluation_Fatalities/
                        └── LSTM_Gridsearch_Results_Fatalities.xlsx
                    ├── Recursive_Prediction_Fatalities/
                        └── Recursive_Forecast_Fatalities.xlsx
                    ├── LSTM_Grid_Search_Fatalities.py
                    ├── LSTM_Prediction_Fatalities.py
                    ├── LSTM_Recursive_Fatalities.py
                    └── LSTM_Training_Fatalities.py
            └── Weekly_LSTM/
                ├── Accidents_LSTM_Prediction/
                    ├── Accident_Forecast_DFs/
                        ├── Accidents_Forecast_Df_Frozen.xlsx
                        └── Accidents_Forecast_Df.xlsx
                    ├── LSTM_Hparams_Evaluation_Accidents/
                        └── LSTM_Gridsearch_Results_Accidents.xlsx
                    ├── Recursive_Prediction_Accidents/
                        └── Recursive_Forecast_Accidents.xlsx
                    ├── lstm_architecture
                    ├── LSTM_Grid_Search_Accidents.py
                    ├── LSTM_Prediction_Accidents.py
                    ├── LSTM_Recursive_Accidents.py
                    └── LSTM_Training_Accidents.py
                └── Fatalities_LSTM_Prediction/
                    ├── Fatalities_Forecast_DFs/
                        ├── Fatalities_Forecast_Df_Frozen.xlsx
                        └── Fatalities_Forecast_Df.xlsx
                    ├── LSTM_Hparams_Evaluation_Fatalities/
                        └── LSTM_Gridsearch_Results_Fatalities.xlsx
                    ├── Recursive_Prediction_Fatalities/
                        └── Recursive_Forecast_Fatalities.xlsx
                    ├── LSTM_Grid_Search_Fatalities.py
                    ├── LSTM_Prediction_Fatalities.py
                    ├── LSTM_Recursive_Fatalities.py
                    └── LSTM_Training_Fatalities.py
        ├── Other_Code/
            ├── accidents_pivot_tables_1996_2014.xlsx
            ├── Corr_Matrix_Yearly_Indicators.py
            ├── Excel_Preprocessing.py
            ├── fatalities_pivot_tables_1996_2014.xlsx
            └── Yearly_Indicators_Fill_Linear_Regression.py
        └── requirements.txt
    └── R/
        ├── Models/
            ├── Manual_Model_Selection_ARIMA_SARIMA.xlsx
        └── Thesis_R_Code.R
Data/
    ├── Main_Data/
        ├── Accidents_1996-2022_MODDED.xlsx
        ├── Accidents-1996-2022-2.xlsx
        └── Yearly_Indicators.csv
    └── Other_Data/
        ├── Elstat_Data/
            ├── Arithmos_Odikwn_Atyxhmatwn_kai_pathontwn_proswpwn_1991_2022_Ethsio.xlsx
            ├── Deikths_Kyklou_Ergasiwn_Ston_Tomea_Metaforwn.xlsx
            ├── Environmental_Taxes_2012_2022.xlsx
            ├── GDP_Ethsio_1995_2023.xlsx
            ├── GDP_Per_Capita_1995_2023.xlsx
            ├── Inflation_Rate.xls
            ├── Nekroi_Apo_Odika_Troxaia_Atyxhmata_2007_2022_Ethsio.xlsx
            ├── Odika_Troxaia_Atyxhmata_Avgoustos_2024.pdf
            ├── Odika_Troxaia_Atyxhmata_Vasika_Xarakthristika_2007_2022_Ethsio.xlsx
            ├── Population_1991_2011.pdf
            ├── Troxaia_Odika_Atyxhmata_kai_Pathonta_Proswpa_Ianouarios_2010_Avgoustos_2024_Mhniaio.xlsx
            ├── Unemployment_1983_2023.xls
            └── Vehicles_Existing_1985_2023.xlsx
        ├── Eurostat_Data/
            ├── All_Gas_Types_Consumption_and_Stats.xlsx
            ├── Electricity.xlsx
            ├── GDP.xlsx
            ├── Healthy_Life_Years.xlsx
            ├── MotorCycles_Per_Fuel.xlsx
            ├── Natural_Gas.xlsx
            ├── Newly_Registered_Cars_Per_Fuel_Type.xlsx
            ├── Newly_Registered_MotorCycles_Per_Fuel_Type.xlsx
            ├── Passenger_Cars_Per_Fuel.xlsx
            ├── Persons_killed_in_road_accidents_by_age_sex_and_category_of_persons_involved.xlsx
            ├── Petrol_Consumption.xlsx
            ├── Population.xlsx
            ├── Road_Freight_Transport_By_Type_of_Operation_and_Type_of_Transport.xlsx
            ├── Total_Road_Length_2022.xlsx
            ├── Transport_Environmental_Taxes.xlsx
            └── Unemployment_Rate.xlsx
        └── NTUA_Data/
            └── Basic_Road_Safety_Figures_pdf.pdf
DB/
    ├── Backup/
        └── RoadAccidentsDBBackup.tar
    ├── DB_Data/
        ├── DB_Data_Outputs/
            ├── monthly_accidents_yearly_indicators_filled.csv
            ├── monthly_accidents_yearly_indicators.csv
            ├── monthly_accidents.csv
            ├── weekly_accidents_yearly_indicators_filled.csv
            ├── weekly_accidents_yearly_indicators.csv
            ├── weekly_accidents.csv
            ├── yearly_accidents_yearly_indicators_filled.csv
            ├── yearly_accidents_yearly_indicators.csv
            ├── yearly_accidents.csv
            ├── yearly_indicators_filled.csv
            └── yearly_indicators.csv
        └── DB_Data_Populated/
            ├── Main_Gathered/
                ├── Accidents_1996-2022_MODDED.xlsx
                └── Yearly_Indicators.csv
            ├── Basic_Road_Safety_Figures_pdf.pdf
            ├── Electricity.xlsx
            ├── Environmental_Taxes_2012_2022.xlsx
            ├── GDP_Ethsio_1995_2023.xlsx
            ├── GDP_Per_Capita_1995_2023.xlsx
            ├── Natural_Gas.xlsx
            ├── Petrol_Consumption.xlsx
            ├── Population_1991_2011.pdf
            ├── Unemployment_1983_2023.xls
            └── Vehicles_Existing_1985_2023.xlsx
    ├── DB_Details/
        └── ER_RoadAccidentsDB.png
    └── Queries/
        ├── DB_CSV_Output_Queries/
            ├── Monthly_Accidents_Output_Query.sql
            ├── Monthly_Accidents_Yearly_Indicators_Filled_Output_Query.sql
            ├── Monthly_Accidents_Yearly_Indicators_Output_Query.sql
            ├── Weekly_Accidents_Output_Query.sql
            ├── Weekly_Accidents_Yearly_Indicators_Filled_Output_Query.sql
            ├── Weekly_Accidents_Yearly_Indicators_Output_Query.sql
            ├── Yearly_Accidents_Output_Query.sql
            ├── Yearly_Accidents_Yearly_Indicators_Filled_Output_Query.sql
            ├── Yearly_Accidents_Yearly_Indicators_Output_Query.sql
            ├── Yearly_Indicators_Filled_Output_Query.sql
            └── Yearly_Indicators_Output_Query.sql
        ├── DB_Population_Queries/
            ├── Population_Query_Monthly.sql
            ├── Population_Query_Weekly.sql
            ├── Population_Query_Yearly.sql
            ├── Test_Query_Weekly.sql
            └── Test_Query_Yearly.sql
        └── Yearly_Indicators_Fill/
            ├── Yearly_Indicators_Filled_Copy.sql
            └── Yearly_Indicators_Filled_Csv_Import.sql
Papers/
    ├── Circulars_Clarification_Draft laws/
        ├── apofasi_oeek-2017-5-2007.pdf
        ├── egk_%C13-61100-9707-2014.pdf
        ├── egk_127829-2020.pdf
        ├── egk_128553-2020.pdf
        ├── egk_1297-449-2005.pdf
        ├── egk_13-137-3902-2007.pdf
        ├── egk_13007-1390-2011.pdf
        ├── egk_13596-1288-2011.pdf
        ├── egk_14623-1351-2011.pdf
        ├── egk_17427-2253-2002.pdf
        ├── egk_18465-1013-2000.pdf
        ├── egk_1876-55-2009.pdf
        ├── egk_20423-2185-2003.pdf
        ├── egk_22523-2102-2001.pdf
        ├── egk_2258-183-2011.pdf
        ├── egk_22732-2107-2001.pdf
        ├── egk_25464-3759-2005.pdf
        ├── egk_27323-3618-2013.pdf
        ├── egk_27986-3593-2002.pdf
        ├── egk_2849-438-2013.pdf
        ├── egk_31462-3991-2013.pdf
        ├── egk_35674-2585-2007.pdf
        ├── egk_38745-4324-2005.pdf
        ├── egk_39258-4232-2004.pdf
        ├── egk_45428-5913-2010.pdf
        ├── egk_46-2007.pdf
        ├── egk_46166-6038-2010.pdf
        ├── egk_47990-3580-2011.pdf
        ├── egk_48842-5961-2009.pdf
        ├── egk_49377-6021-2009.pdf
        ├── egk_50362-6533-2010.pdf
        ├── egk_51895-7157-2008.pdf
        ├── egk_54077-8008-2012.pdf
        ├── egk_5463-703-2010.pdf
        ├── egk_5795-740-2010.pdf
        ├── egk_58413-257-1998.pdf
        ├── egk_62617-7658-2009.pdf
        ├── egk_68123-1065-1999.pdf
        ├── egk_68128-7632-2003.pdf
        ├── egk_68632-9139-2008.pdf
        ├── egk_69347-2021.pdf
        ├── egk_69659-7591-2005.pdf
        ├── egk_71358-9562-08-2009.pdf
        ├── egk_76529-1998.pdf
        ├── egk_76970-8618-2004.pdf
        ├── egk_8201-30-109588-2023.pdf
        ├── egk_8980-821-2011.pdf
        ├── egk_91491-2020.pdf
        ├── egk_9527-535-2009.pdf
        ├── egk_a_32654_2548_2014.pdf
        ├── egk_a3-10323-1408-2012.pdf
        ├── egk_a3-165-251-2013.pdf
        ├── egk_a3-18079-1976-2019.pdf
        ├── egk_a3-18539-2218-2020.pdf
        ├── egk_a3-18813-2233-2020.pdf
        ├── egk_a3-19023-2039-2019.pdf
        ├── egk_a3-23659-2693-2020.pdf
        ├── egk_a3-24698-3917-2013.pdf
        ├── egk_a3-25449-2819-2020.pdf
        ├── egk_a3-26013-2872-2020.pdf
        ├── egk_a3-2888-498-2013.pdf
        ├── egk_a3-3174-466-2013.pdf
        ├── egk_a3-35683-4122-2016.pdf
        ├── egk_a3-5871-960-2013.pdf
        ├── egk_a3-59480-7072-2019.pdf
        ├── egk_a3-63032-7900-2016.pdf
        ├── egk_a3-63853-9748-2013.pdf
        ├── egk_a3-69071-10314-2013.pdf
        ├── egk_a3-7167-902-2020.pdf
        ├── egk_a3-73316-7044-2018.pdf
        ├── egk_a3-81139-7800-2018.pdf
        ├── egk_a3-84726-8036-2018.pdf
        ├── egk_ap-7335-2020.pdf
        ├── egk_b1-47832-4533-2010.pdf
        ├── egk_b2-78557-7644-2018.pdf
        ├── egk_b3-11307-1578-2012.pdf
        ├── egk_b3-55445-5730-2012.pdf
        ├── egk_d30-%C13-119364-2024.pdf
        ├── egk_d30-%C13-97516-2024.pdf
        ├── egk_d30-%C24-288562-2021.pdf
        ├── egk_d30-a1-111149-2020.pdf
        ├── egk_d30-a3-113159-2020.pdf
        ├── egk_d30-a3-114685-2020.pdf
        ├── egk_d30-a3-118384-2021.pdf
        ├── egk_d30-a3-129703-2020.pdf
        ├── egk_d30-a3-160986.pdf
        ├── egk_d30-a3-176728-2023.pdf
        ├── egk_d30-a3-209255-2023.pdf
        ├── egk_d30-a3-240154-2023.pdf
        ├── egk_d30-a3-51508-2021.pdf
        ├── egk_d30-a3-5599-2023.pdf
        ├── egk_d30-a3-63171-2020.pdf
        ├── egk_d30-a3-90777-2020.pdf
        ├── egk_d30-a3-93594-2020.pdf
        ├── egk_d30-b3-330905-2023.pdf
        ├── egk_f106-10007-1312.pdf
        ├── egk_g454-19716-1780-2009.pdf
        ├── egk_g454-8699-894-2011.pdf
        ├── egk_p110-41561-4065-2011.pdf
        ├── egk_p110-55513-5734.pdf
        ├── eopp_apof_2384-2011.pdf
        ├── oaee_f1-11-59829-2009.pdf
        ├── oaee_f1-4-22701-2009.pdf
        ├── pol_1083_2003.pdf
        ├── pol_1084_2016.pdf
        ├── pol_1093_2016.pdf
        ├── pol_1131_2008.pdf
        ├── pol_1131-2015.pdf
        ├── pol_1135_2010.pdf
        ├── pol_1150-2014.pdf
        ├── pol_1163-2013.pdf
        ├── protasi_anamorfosis.pdf
        └── protasi_sxediou_nomou_electronics.pdf
    ├── Common_Ministerial_Decisions/
        ├── kya_12066-1989.pdf
        ├── kya_125494-2009.pdf
        ├── kya_12651-1984.pdf
        ├── kya_16085-2009.pdf
        ├── kya_16229-2012.pdf
        ├── kya_21504-2601-2007.pdf
        ├── kya_22172-1385-2006.pdf
        ├── kya_29240-3729-2010.pdf
        ├── kya_3019-st-0967-f911-2011.pdf
        ├── kya_3021-19-53-2005.pdf
        ├── kya_38350-4644-2009.pdf
        ├── kya_43206-6028-2008.pdf
        ├── kya_43500-5691-2002.pdf
        ├── kya_43917-5066-2006.pdf
        ├── kya_52526-6904-2007.pdf
        ├── kya_5535-459-1999.pdf
        ├── kya_58215-187-1992.pdf
        ├── kya_64094-119-2010.pdf
        ├── kya_64834-5491-2000.pdf
        ├── kya_74379-365003-2004.pdf
        ├── kya_a-80410-5610-2001.pdf
        ├── kya_d1a-23983-2022.pdf
        ├── kya_d1a-24489-2021.pdf
        ├── kya_d1a-3060-2021.pdf
        ├── kya_d1a-46819-2021.pdf
        ├── kya_d1a-gp-71342-2020.pdf
        ├── kya_d1a-gp-80189-2020.pdf
        ├── kya_d1a-gp.oik.21268-2020.pdf
        ├── kya_d30-a3-143433-2023.pdf
        ├── kya_d30-a3-190875_2022.pdf
        ├── kya_d30-a3-222261-2024.pdf
        ├── kya_d30-a3-276110-2021.pdf
        ├── kya_d30-a3-29801_2022.pdf
        ├── kya_d30-a3-314952-2021.pdf
        ├── kya_ddo-a-89819-1372-f4_2020.pdf
        ├── kya_defk%205006718ex2001-2011.pdf
        ├── kya_g5-29480-2304-2001.pdf
        └── kya_k3-1563-2000.pdf
    ├── European_Directives/
        ├── 2000-53-ek.pdf
        ├── 2000-56-ek.pdf
        ├── 2003-0252-ek.pdf
        ├── 2003-59-ek.pdf
        ├── 2006-103-ek.pdf
        ├── 2006-126-ek.pdf
        ├── 2008-65-ek.pdf
        ├── 2012-36-ee.pdf
        ├── 2020-612-ee.pdf
        ├── 76-914-eok.pdf
        ├── 91-439-eok.pdf
        ├── 95-46-ek.pdf
        └── 96-26-ek.pdf
    ├── Laws_Legislative_Regulations/
        ├── n_1264-1982.pdf
        ├── n_1959-1991.pdf
        ├── n_2218-1994.pdf
        ├── n_2472-1997.pdf
        ├── n_2671-1998.pdf
        ├── n_2773-1999.pdf
        ├── n_2881-2001.pdf
        ├── n_2963-2001.pdf
        ├── n_3109-2003.pdf
        ├── n_3345-2005.pdf
        ├── n_3431-2006.pdf
        ├── n_3446-2006.pdf
        ├── n_3463-2006.pdf
        ├── n_3471-2006.pdf
        ├── n_3542-2007.pdf
        ├── n_3697-2008.pdf
        ├── n_3710-2008.pdf
        ├── n_3763-2009.pdf
        ├── n_3801-2009.pdf
        ├── n_3842-2010.pdf
        ├── n_3887-2010.pdf
        ├── n_3897_%202010.pdf
        ├── n_3899_%202010.pdf
        ├── n_3917-2011.pdf
        ├── n_3919-2011.pdf
        ├── n_3986-2011.pdf
        ├── n_4070-2012.pdf
        ├── n_4155-2013.pdf
        ├── n_4199-2013.pdf
        ├── n_4233-2014.pdf
        ├── n_4250-2014.pdf
        ├── n_4313-2014.pdf
        ├── n_4336-2015.pdf
        ├── n_4370-2016.pdf
        ├── n_4402-2016.pdf
        ├── n_4413-2016.pdf
        ├── n_4529-2018.pdf
        ├── n_4530-2018.pdf
        ├── n_4568-2018.pdf
        ├── n_4599-2019.pdf
        ├── n_4625_2019.pdf
        ├── n_4657-2020.pdf
        ├── n_4663-2020.pdf
        ├── n_4784-2021.pdf
        ├── n_4811-2021.pdf
        ├── n_4850-2021.pdf
        └── n_5039_2023.pdf
    ├── Ministerial_Decisions/
        ├── aade-ap_1011-2020.pdf
        ├── aade-ap_1100-2020.pdf
        ├── ya_16703-716-2001.pdf
        ├── ya_18586-698-2000.pdf
        ├── ya_21504-2601-2007.pdf
        ├── ya_24324-2884-2009.pdf
        ├── ya_24325-2885-2009.pdf
        ├── ya_24391-2987-2009.pdf
        ├── ya_24688-1892-2006.pdf
        ├── ya_2805_2021.pdf
        ├── ya_28366-2098-2006.pdf
        ├── ya_29515-3856-2013.pdf
        ├── ya_29898-3378-2016.pdf
        ├── ya_30063-4796-2013.pdf
        ├── ya_30510-3941-2002.pdf
        ├── ya_32496-1558-2003.pdf
        ├── ya_33243-4894-2012.pdf
        ├── ya_35364-4046-2006.pdf
        ├── ya_36927-4751-2002.pdf
        ├── ya_374362_2021.pdf
        ├── ya_3900_555-2012.pdf
        ├── ya_40250-5807-2015.pdf
        ├── ya_4100-4504-2005.pdf
        ├── ya_42485-5569-2002.pdf
        ├── ya_42558-5591-2002.pdf
        ├── ya_43221-6038-2008.pdf
        ├── ya_45282-5867-2010.pdf
        ├── ya_45892-2010_kallikratis.pdf
        ├── ya_45990-5584-2009.pdf
        ├── ya_47272-5763-2009.pdf
        ├── ya_48461-4454-2018.pdf
        ├── ya_49522-6046-2009.pdf
        ├── ya_4993-395-2003.pdf
        ├── ya_50292-3549-08-2009.pdf
        ├── ya_51420-2811-2016.pdf
        ├── ya_51573-6746-2008.pdf
        ├── ya_52526-6904-2007.pdf
        ├── ya_552-88-2013.pdf
        ├── ya_57216-7383-2002.pdf
        ├── ya_58185-2474-1991.pdf
        ├── ya_58413-7516-2002.pdf
        ├── ya_58930-480-1999.PDF
        ├── ya_60864-6794-2003.pdf
        ├── ya_63557-4698-2010.pdf
        ├── ya_64885-8388-2008.pdf
        ├── ya_65581-2023.pdf
        ├── ya_6776-734-2003.pdf
        ├── ya_68831-9254-2008.pdf
        ├── ya_72048-9666-08.pdf
        ├── ya_78404-6024-2003.pdf
        ├── ya_79400-2490-1989.pdf
        ├── ya_a-10852-754-2002.pdf
        ├── ya_a-79574-5488-16-2017.pdf
        ├── ya_a3-1517-264-2020.pdf
        ├── ya_a3-15664-2548-2013.pdf
        ├── ya_a3-16110-1967-2020.pdf
        ├── ya_a3-49645-7730-2013.pdf
        ├── ya_a3-50984-7947-2013.pdf
        ├── ya_a3-64720-6117-2018.pdf
        ├── ya_d30-a3-113358-2021.pdf
        ├── ya_d30-a3-347507_2021.pdf
        ├── ya_d30-a3-71830-2024.pdf
        ├── ya_f23-24327-2887-2009.pdf
        ├── ya_f5-58038-7569-2010.pdf
        └── ya_f50-48597-5875-2009.pdf
    ├── Presidential_Decisions/
        ├── pd_116-2004.pdf
        ├── pd_19-1995.PDF
        ├── pd_208-2002.pdf
        ├── pd_241-2005.pdf
        ├── pd_337-2003.pdf
        ├── pd_355-1994.pdf
        ├── pd_404-1996.pdf
        ├── pd_482-1987.pdf
        ├── pd_51-2012.pdf
        ├── pd_66-2010.pdf
        └── pd_74-2008.pdf
    ├── 015022_1_5.0251493.pdf
    ├── 0405050v1.pdf
    ├── 1-s2.0-S0001457512004101-main.pdf
    ├── 1-s2.0-S0022437524000793-main.pdf
    ├── 1-s2.0-S0925753510000731-main.pdf
    ├── 1-s2.0-S1366554524001546-main.pdf
    ├── 1-s2.0-S1877050924027649-main.pdf
    ├── 1-s2.0-S2046043016301332-main.pdf
    ├── 1-s2.0-S2214785321040153-main.pdf
    ├── 1-s2.0-S2352146516302265-main.pdf
    ├── 1-s2.0-S235214651630299X-main.pdf
    ├── 1-s2.0-S2590198223000611-main.pdf
    ├── 13. 2021-22civil13.pdf
    ├── 19f4f742bdd8b775a6950787f0f6ab41754a.pdf
    ├── 2000-53-ek.pdf
    ├── 2000-56-ek.pdf
    ├── 2003-0252-ek.pdf
    ├── 2003-59-ek.pdf
    ├── 2006-103-ek.pdf
    ├── 2006-126-ek.pdf
    ├── 2008-65-ek.pdf
    ├── 2012-36-ee.pdf
    ├── 20191211040818922.pdf
    ├── 2020-612-ee.pdf
    ├── 2406.13968v1.pdf
    ├── 2409.11929v1.pdf
    ├── 3383972.3384034.pdf
    ├── 6026.pdf
    ├── 76-914-eok.pdf
    ├── 91-439-eok.pdf
    ├── 95-46-ek.pdf
    ├── 96-26-ek.pdf
    ├── A_Critical_Review_of_Road_Accident_Prediction_and_Analysis_Techniques_Data_Sources_Challenges_and_Opportunities_in_Future_Developments.pdf
    ├── A_Time-Series_Analysis_of_Traffic_Crashes_in_New_York_City.pdf
    ├── a-statistical-analysis-of-road-accident-fatalities-in-malaysia.pdf
    ├── Accident_Prediction_and_Analysis_using_Machine_Learning_models.pdf
    ├── algorithms-16-00257.pdf
    ├── An analysis of the characteristics of road traffic injuries and a prediction of fatalities in China from 1996 to 2015.pdf
    ├── An_Effective_Prediction_Modelling_of_Traffic_Accidents_based_on_Categorical_Boosting_with_Grid_Search.pdf
    ├── Analysis_and_Prediction_of_Road_Accidents_in_Nepal_by_Grey_System_Theory.pdf
    ├── Analysis_and_Predictive_Modeling_of_Traffic_Incidents_in_Karachi_using_Machine_Learning.pdf
    ├── Analysis_of_road_traffic_fatal_accidents_using_data_mining_techniques.pdf
    ├── applsci-12-00828.pdf
    ├── Athens_Traffic_Mode_Choice.pdf
    ├── atr-05-03-36570.pdf
    ├── burns_v6_1_111.pdf
    ├── computers-10-00157-v2.pdf
    ├── Daktylios_2023.pdf
    ├── Detection_of_traffic_incidents_using_non.pdf
    ├── Digital_Reinforcement_Road_Safety.pdf
    ├── document.pdf
    ├── Driver_Behavior.pdf
    ├── Driving_with_Insurance_Legislation.pdf
    ├── Effective_Road_Measures_Greece.pdf
    ├── file.pdf
    ├── fin_irjmets1653072961.pdf
    ├── Forecasting deaths of road traffic injuries in China using an artificial neural network.pdf
    ├── Forecasting_of_Road_Accident_in_Kerala_A_Case_Study.pdf
    ├── geyannis-pc302.pdf
    ├── geyannis-pc378.pdf
    ├── geyannis-pj133m.pdf
    ├── Gini_Index.pdf
    ├── Improvements_in_Medical_Technology_and_Health_Care_Greece.pdf
    ├── JDAIP_2019030416531017.pdf
    ├── JESAUN_Volume 45_Issue No 1_Pages 28-44.pdf
    ├── Journal of Control Science and Engineering - 2020 - Zhang - Traffic Accident Prediction Based on LSTM‐GBRT Model.pdf
    ├── JTTs_2017050510235847.pdf
    ├── Modelling_the_effects_of_fuel_price_changes_on_road_traffic_collisions_in_the_European_Union_using_panel_data.pdf
    ├── National_Road_Safety_Strategic_Plan_Greece.pdf
    ├── PIIS2405844023097529.pdf
    ├── PIIS2405844024017419.pdf
    ├── Predictive_analytics_of_road_accidents_in_Oman_using_machine_learning_approach.pdf
    ├── Predictive_Analytics_of_Road_Accidents_Using_Machine_Learning.pdf
    ├── Presentation_Cameras_and_Fines.ppt
    ├── Presentation_YPTP.pptx
    ├── raeside-white-2004-predicting-casualty-numbers-in-great-britain.pdf
    ├── Road_Accident_Analysis_and_Prediction_of_Accident_Severity_by_Using_Machine_Learning_in_Bangladesh.pdf
    ├── Road_Accident_Analysis_using_Machine_Learning.pdf
    ├── Road_Accident_Prediction_and_Feature_Analysis_By_Using_Deep_Learning.pdf
    ├── Road_Accident_Prediction_using_Machine_Learning_Approaches.pdf
    ├── Road_Accident_Prediction_Using_Machine_Learning.pdf
    ├── Road_Safety_Country_Overview_Greece.pdf
    ├── Road_Traffic_Code.pdf
    ├── s00500-023-08001-6.pdf
    ├── s00521-019-04695-8.pdf
    ├── s12544-011-0055-4.pdf
    ├── s40537-021-00493-z.pdf
    ├── s40621-020-00294-7.pdf
    ├── s42452-020-3125-1.pdf
    ├── Safety_Belt_Law_EU_1.pdf
    ├── Safety_Belt_Law_EU_2.pdf
    ├── Safety_Belt_Law_EU_3.pdf
    ├── Safety_Belt_Law_GR.pdf
    ├── SCCE_Volume 8_Issue 4_Pages 27-53.pdf
    ├── SKG_Traffic_Mode_Choice.pdf
    ├── ssrn-3550462.pdf
    ├── ssrn-4460883.pdf
    ├── sustainability-12-00395-v2.pdf
    ├── sustainability-14-02851.pdf
    ├── sustainability-14-04403.pdf
    ├── sustainability-15-05939-v3.pdf
    ├── Time series and support vector machines to predict powered-two-wheeler accident risk and accident type propensity  A combined approach.pdf
    ├── Time_series_count_data_models_An_empirical_application_to_traffic_accidents.pdf
    ├── Total_Distance_Travelled_Vkm.pdf
    ├── TOTJ-13-134.pdf
    ├── TRA2020_29102019_safer_africa_3_author.pdf
    ├── Traffic Accidents Analysis & Prediction in UAE.pdf
    ├── Traffic_Emissions_Congestion_GR.pdf
    ├── Trend analysis and fatality causes in Kenyan roads  A review of road traffic accident data between 2015 and 2020.pdf
    └── Vehicles_on_European_Roads.pdf
.gitattributes
LICENSE
README.md
```
