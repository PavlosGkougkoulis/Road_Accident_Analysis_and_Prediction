####Libraries Installation####




install.packages("Metrics")
install.packages("tsibble")
install.packages("fable")
install.packages("feasts")
install.packages("urca")
install.packages("tseries")
install.packages("lmtest")
install.packages("FinTS")
install.packages("writexl")
install.packages("openxlsx")
install.packages("ROCR")
install.packages("reshape2")
install.packages("gridExtra")
install.packages("moments")
install.packages("BayesianTools")
install.packages("xgboost")
install.packages("randomForest")
install.packages("conflicted")
install.packages("caret")
install.packages("MASS")
install.packages("cluster")
install.packages("factoextra")
install.packages("FSA")
install.packages("stats")
install.packages("e1071")
install.packages("matrixStats")
install.packages("ggplot2")
install.packages("zoo")
install.packages("forecast")
install.packages("tseries")
install.packages("tidyr")
install.packages("purrr")
install.packages("timeDate")
install.packages("dplyr")
install.packages("lubridate")
install.packages("readxl")
library(lubridate)
library(readxl)
library(Metrics)
library(tsibble)
library(fable)
library(feasts)
library(urca)
library(tseries)
library(lmtest)
library(FinTS)
library(lubridate)
library(timeDate)
library(purrr)
library(tidyr)
library(tseries)
library(forecast)
library(zoo)
library(ggplot2)
library(e1071)
library(matrixStats)
library(stats)
library(FSA)
library(caret)
library(cluster)
library(factoextra)
library(dplyr)
library(randomForest)
library(xgboost)
library(BayesianTools)
library(moments)
library(gridExtra)
library(reshape2)
library(ROCR)
library(writexl)
library(openxlsx)





####Data Input####




###Working Directory###



setwd("E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs") #Path Here#



###Load Dfs###



acc.w <- read.csv("weekly_accidents.csv")
acc.m <- read.csv("monthly_accidents.csv")
acc.y <- read.csv("yearly_accidents.csv")
ind.y <- read.csv("yearly_indicators.csv")
acc.w.ind.y <- read.csv("weekly_accidents_yearly_indicators.csv")
acc.m.ind.y <- read.csv("monthly_accidents_yearly_indicators.csv")
acc.y.ind.y <- read.csv("yearly_accidents_yearly_indicators.csv")



####Preprocessing the Datasets####




###Weekly Accidents Dataset Preprocessing###



acc.w$accidents <- as.numeric(acc.w$accidents)
acc.w$fatalities <- as.numeric(acc.w$fatalities)
acc.w$year <- factor(acc.w$year, levels = 1996:2022)
acc.w$month <- factor(acc.w$month, levels = 1:12)
acc.w$week <- factor(acc.w$week, levels = 1:6)



##Weekly Accidents One Column Mod##


acc.w_mod <- acc.w %>%
  mutate(
    week = as.numeric(as.character(week)),
    start_of_month = as.Date(paste(year, month, "01", sep = "-")),
    first_sunday = if_else(
      wday(start_of_month) == 1, 
      start_of_month, 
      start_of_month + (7 - wday(start_of_month) + 1) %% 7
    ),
    week_date = case_when(
      week == 1 ~ start_of_month,                                
      week > 1 & week < 6 ~ first_sunday + (week - 2) * 7,       
      week == 6 ~ pmin(                                           
        first_sunday + (week - 2) * 7, 
        ceiling_date(start_of_month, "month") - 1
      ),
      TRUE ~ NA_Date_                                          
    ),
    week_end_date = pmin(
      week_date + 6, 
      ceiling_date(start_of_month, "month") - 1                 
    )
  ) %>%
  select(-start_of_month, -first_sunday, -week_end_date)


###Monthly Accidents Dataset Preprocessing###



acc.m$accidents <- as.numeric(acc.m$accidents)
acc.m$fatalities <- as.numeric(acc.m$fatalities)
acc.m$year <- factor(acc.m$year, levels = 1996:2022)
acc.m$month <- factor(acc.m$month, levels = 1:12)



##Monthly Accidents One Column Mod##


acc.m_mod <- acc.m %>%
  mutate(
    month_date = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  select(year, month, month_date, accidents, fatalities)


###Yearly Accidents Preprocessing###



acc.y$accidents <- as.numeric(acc.y$accidents)
acc.y$fatalities <- as.numeric(acc.y$fatalities)
acc.y$year <- factor(acc.y$year, levels = 1996:2022)



##Yearly Accidents One Column Mod##


acc.y_mod <- acc.y %>%  
  mutate(
    year_date = as.Date(paste(year, "01", "01", sep = "-"))
  ) %>%
  select(year, year_date, accidents, fatalities)


###Yearly Indicators Dataset Preprocessing###



ind.y$year <- factor(ind.y$year, levels = 1996:2022)
num_cols_ind_y <- setdiff(colnames(ind.y), "year")
ind.y[num_cols_ind_y] <- lapply(ind.y[num_cols_ind_y], as.numeric)



##Yearly Indicators One Column Mod##


ind.y_mod <- ind.y %>%
  filter(!is.na(year)) %>%
  mutate(
    year = as.numeric(as.character(year)),
    year_date = as.Date(paste(year, "01", "01", sep = "-"))
  ) %>%
  select(year, year_date, total_gdp, gdp_per_capita, unemployment, population, environmental_taxes, vehicles,
         petrol_cons, ng_cons, el_cons, fatal_per_million_veh, speed_infr, drink_infr, belt_infr,
         helmet_infr, inflation_rate, gini_index)


###Weekly Accidents & Yearly Indicators Dataset Preprocessing###



acc.w.ind.y$accidents <- as.numeric(acc.w.ind.y$accidents)
acc.w.ind.y$fatalities <- as.numeric(acc.w.ind.y$fatalities)
acc.w.ind.y$year <- factor(acc.w.ind.y$year, levels = 1996:2022)
acc.w.ind.y$month <- factor(acc.w.ind.y$month, levels = 1:12)
acc.w.ind.y$week <- factor(acc.w.ind.y$week, levels = 1:6)
indicator_columns_weekly <- setdiff(colnames(acc.w.ind.y), c("accidents", "fatalities", "year", "month", "week"))
acc.w.ind.y[indicator_columns_weekly] <- lapply(acc.w.ind.y[indicator_columns_weekly], as.numeric)



##Weekly Accidents & Yearly Indicators One Column Mod##


acc.w.ind.y_mod <- acc.w.ind.y %>%
  mutate(
    week = as.numeric(as.character(week)),
    start_of_month = as.Date(paste(year, month, "01", sep = "-")),
    first_sunday = if_else(
      wday(start_of_month) == 1, 
      start_of_month, 
      start_of_month + (7 - wday(start_of_month) + 1) %% 7
    ),
    week_date = case_when(
      week == 1 ~ start_of_month,                                
      week > 1 & week < 6 ~ first_sunday + (week - 2) * 7,       
      week == 6 ~ pmin(                                           
        first_sunday + (week - 2) * 7, 
        ceiling_date(start_of_month, "month") - 1
      ),
      TRUE ~ NA_Date_                                          
    ),
    week_end_date = pmin(
      week_date + 6, 
      ceiling_date(start_of_month, "month") - 1                 
    )
  ) %>%
  select(-start_of_month, -first_sunday, -week_end_date)


###Monthly Accidents & Yearly Indicators Dataset Preprocessing###



acc.m.ind.y$accidents <- as.numeric(acc.m.ind.y$accidents)
acc.m.ind.y$fatalities <- as.numeric(acc.m.ind.y$fatalities)
acc.m.ind.y$year <- factor(acc.m.ind.y$year, levels = 1996:2022)
acc.m.ind.y$month <- factor(acc.m.ind.y$month, levels = 1:12)
indicator_columns_monthly <- setdiff(colnames(acc.m.ind.y), c("accidents", "fatalities", "year", "month"))
acc.m.ind.y[indicator_columns_monthly] <- lapply(acc.m.ind.y[indicator_columns_monthly], as.numeric)



##Monthly Accidents & Yearly Indicators One Column Mod##


acc.m.ind.y_mod <- acc.m.ind.y %>%
  mutate(
    month_date = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  select(year, month, month_date, accidents, fatalities, total_gdp, gdp_per_capita, unemployment, population, environmental_taxes, vehicles,
         petrol_cons, ng_cons, el_cons, fatal_per_million_veh, speed_infr, drink_infr, belt_infr,
         helmet_infr, inflation_rate, gini_index)


###Yearly Accidents & Yearly Indicators Dataset Preprocessing###



acc.y.ind.y$accidents <- as.numeric(acc.y.ind.y$accidents)
acc.y.ind.y$fatalities <- as.numeric(acc.y.ind.y$fatalities)
acc.y.ind.y$year <- factor(acc.y.ind.y$year, levels = 1996:2022)
indicator_columns_yearly <- setdiff(colnames(acc.y.ind.y), c("accidents", "fatalities", "year"))
acc.y.ind.y[indicator_columns_yearly] <- lapply(acc.y.ind.y[indicator_columns_yearly], as.numeric)



##Yearly Accidents & Yearly Indicators One Column Mod##


acc.y.ind.y_mod <- acc.y.ind.y %>%  
  mutate(
    year_date = as.Date(paste(year, "01", "01", sep = "-"))
  ) %>%
  select(year, year_date, accidents, fatalities, total_gdp, gdp_per_capita, unemployment, population, environmental_taxes, vehicles,
         petrol_cons, ng_cons, el_cons, fatal_per_million_veh, speed_infr, drink_infr, belt_infr,
         helmet_infr, inflation_rate, gini_index)


###Checks###



summary(acc.w)
summary(acc.m)
summary(acc.y)
summary(ind.y)
summary(acc.w.ind.y)
summary(acc.m.ind.y)
summary(acc.y.ind.y)
summary(acc.w_mod)
summary(acc.m_mod)
summary(acc.y_mod)
summary(ind.y_mod)
summary(acc.w.ind.y_mod)
summary(acc.m.ind.y_mod)
summary(acc.y.ind.y_mod)
str(acc.w)
str(acc.m)
str(acc.y)
str(ind.y)
str(acc.w.ind.y)
str(acc.m.ind.y)
str(acc.y.ind.y)
str(acc.w_mod)
str(acc.m_mod)
str(acc.y_mod)
str(ind.y_mod)
str(acc.w.ind.y_mod)
str(acc.m.ind.y_mod)
str(acc.y.ind.y_mod)
head(acc.w)
head(acc.m)
head(acc.y)
head(ind.y)
head(acc.w.ind.y)
head(acc.m.ind.y)
head(acc.y.ind.y)
head(acc.w_mod)
head(acc.m_mod)
head(acc.y_mod)
head(ind.y_mod)
head(acc.w.ind.y_mod)
head(acc.m.ind.y_mod)
head(acc.y.ind.y_mod)
tail(acc.w)
tail(acc.m)
tail(acc.y)
tail(ind.y)
tail(acc.w.ind.y)
tail(acc.m.ind.y)
tail(acc.y.ind.y)
tail(acc.w_mod)
tail(acc.m_mod)
tail(acc.y_mod)
tail(ind.y_mod)
tail(acc.w.ind.y_mod)
tail(acc.m.ind.y_mod)
tail(acc.y.ind.y_mod)



####Exploratory Data Analysis####

                                                                          ###NOTICE!!!###
          ###(Datasets used are:acc.w_mod, acc.m_mod, acc.y_mod, acc.w.ind.y_mod, acc.m.ind.y_mod, acc.y.ind.y_mod)###

###Feature Engineering###



##Weekly Accidents Dataset##


#Season, Quarter, IsSummer, IsWinter#

acc.w_mod <- acc.w_mod %>% drop_na()
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Autumn")
  }
}
acc.w_mod <- acc.w_mod %>%
  mutate(
    quarter = case_when(
      month %in% c(1, 2, 3) ~ 1,
      month %in% c(4, 5, 6) ~ 2,
      month %in% c(7, 8, 9) ~ 3,
      month %in% c(10, 11, 12) ~ 4
    ),
    season = sapply(month, get_season),
    is_summer = ifelse(season == "Summer", 1, 0),
    is_winter = ifelse(season == "Winter", 1, 0)
  )

#Holidays and Special Events#

compute_stationary_holidays <- function(df) {
  date_column <- if ("week_date" %in% colnames(df)) "week_date" else "month_date"
  
  df <- df %>%
    mutate(
      date_col = as.Date(!!sym(date_column)), 
      year = as.numeric(format(date_col, "%Y"))  
    ) %>%
    rowwise() %>%
    mutate(
      is_holiday = as.integer(any(
        as.Date(c(
          paste0(year, "-01-01"),  # New Year's Day
          paste0(year, "-01-06"),  # Epiphany
          paste0(year, "-03-25"),  # Independence Day
          paste0(year, "-05-01"),  # Labor Day
          paste0(year, "-08-15"),  # Assumption of Mary
          paste0(year, "-10-28"),  # Oxi Day
          paste0(year, "-12-24"),  # Christmas Eve
          paste0(year, "-12-25")   # Christmas Day
        )) == date_col
      ))
    ) %>%
    ungroup() %>%
    select(-date_col)  
  return(df)
}
acc.w_mod <- compute_stationary_holidays(acc.w_mod)
compute_orthodox_easter <- function(year) {
  a <- year %% 19
  b <- year %% 4
  c <- year %% 7
  d <- (19 * a + 15) %% 30
  e <- (2 * b + 4 * c + 6 * d + 6) %% 7
  f <- d + e
  return(as.Date(paste0(year, "-04-04")) + f)
}
compute_movable_holidays <- function(df) {
  date_column <- if ("week_date" %in% colnames(df)) "week_date" else "month_date"
  df <- df %>%
    mutate(
      date_col = as.Date(!!sym(date_column)),
      year = as.numeric(format(date_col, "%Y")),
      orthodox_easter = as.Date(sapply(year, compute_orthodox_easter)),
      clean_monday = orthodox_easter - 48,
      easter_monday = orthodox_easter + 1,
      pentecost_sunday = orthodox_easter + 50,
      carnival_weekend = clean_monday - 3,
      good_friday = orthodox_easter - 2,
      holy_saturday = orthodox_easter - 1
    ) %>%
    mutate(
      is_holiday = ifelse(
        date_col %in% c(clean_monday, orthodox_easter, easter_monday, 
                        pentecost_sunday, carnival_weekend, good_friday, 
                        holy_saturday), 1, is_holiday
      )
    ) %>%
    select(-orthodox_easter, -clean_monday, -easter_monday, -pentecost_sunday,
           -carnival_weekend, -good_friday, -holy_saturday, -date_col)  
  return(df)
}
acc.w_mod <- compute_movable_holidays(acc.w_mod)

#Week Lags#

lag_weeks <- c(1, 2, 3, 4, 6, 7, 8, 12, 24, 52)
acc.w_mod <- acc.w_mod %>%
  arrange(year, month, week) 
for (lag_val in lag_weeks) {
  acc.w_mod <- acc.w_mod %>%
    mutate(
      !!paste0("Accidents_Lag", lag_val) := lag(accidents, lag_val),
      !!paste0("Fatalities_Lag", lag_val) := lag(fatalities, lag_val)
    )
}

#Week Rolling Averages#

acc.w_mod <- acc.w_mod %>%
  arrange(year, month, week) %>%
  mutate(
    Rolling_Avg_4w_a = rollmean(accidents, k = 4, fill = NA, align = "right"),
    Rolling_Avg_7w_a = rollmean(accidents, k = 7, fill = NA, align = "right"),
    Rolling_Avg_12w_a = rollmean(accidents, k = 12, fill = NA, align = "right"),
    Rolling_Avg_24w_a = rollmean(accidents, k = 24, fill = NA, align = "right"),
    Rolling_Avg_52w_a = rollmean(accidents, k = 52, fill = NA, align = "right")
  )
acc.w_mod <- acc.w_mod %>%
  arrange(year, month, week) %>%
  mutate(
    Rolling_Avg_4w_f = rollmean(fatalities, k = 4, fill = NA, align = "right"),
    Rolling_Avg_7w_f = rollmean(fatalities, k = 7, fill = NA, align = "right"),
    Rolling_Avg_12w_f = rollmean(fatalities, k = 12, fill = NA, align = "right"),
    Rolling_Avg_24w_f = rollmean(fatalities, k = 24, fill = NA, align = "right"),
    Rolling_Avg_52w_f = rollmean(fatalities, k = 52, fill = NA, align = "right")
  )

#Indexes#

acc.w_mod <- acc.w_mod %>%
  mutate(
    AccidentGrowthRate = (accidents - lag(accidents)) / lag(accidents),
    FatalityGrowthRate = (fatalities - lag(fatalities)) / lag(fatalities),
    FatalityRate = fatalities / accidents,
    AccidentChangeRate = (accidents - Accidents_Lag1) / Accidents_Lag1,
    FatalityChangeRate = (fatalities - Fatalities_Lag1) / Fatalities_Lag1
  )

#Cumulative Features#

acc.w_mod <- acc.w_mod %>%
  mutate(
    CumulativeAccidents = cumsum(accidents),
    CumulativeFatalities = cumsum(fatalities),
    RollingSum_4w_a = rollsum(accidents, k = 4, fill = NA, align = "right"),
    RollingSum_8w_a = rollsum(accidents, k = 8, fill = NA, align = "right"),
    RollingSum_12w_a = rollsum(accidents, k = 12, fill = NA, align = "right"),
    RollingSum_24w_a = rollsum(accidents, k = 24, fill = NA, align = "right"),
    RollingSum_4w_f = rollsum(fatalities, k = 4, fill = NA, align = "right"),
    RollingSum_8w_f = rollsum(fatalities, k = 8, fill = NA, align = "right"),
    RollingSum_12w_f = rollsum(fatalities, k = 12, fill = NA, align = "right"),
    RollingSum_24w_f = rollsum(fatalities, k = 24, fill = NA, align = "right")
  )

##Monthly Accidents Dataset##


acc.m_mod <- acc.m_mod %>%
  arrange(year, month) %>%
  mutate(
    quarter = case_when(
      month %in% c(1, 2, 3) ~ 1,
      month %in% c(4, 5, 6) ~ 2,
      month %in% c(7, 8, 9) ~ 3,
      month %in% c(10, 11, 12) ~ 4
    ),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    ),
    is_summer = ifelse(season == "Summer", 1, 0),
    is_winter = ifelse(season == "Winter", 1, 0)
  )


#Holidays and Special Events#

acc.m_mod <- compute_stationary_holidays(acc.m_mod)
acc.m_mod <- compute_movable_holidays(acc.m_mod)

#Month Lags#

lag_months <- c(1, 2, 3, 6, 12, 24)
acc.m_mod <- acc.m_mod %>%
  arrange(year, month)
for (lag_val in lag_months) {
  acc.m_mod <- acc.m_mod %>%
    mutate(
      !!paste0("Accidents_Lag", lag_val) := lag(accidents, lag_val),
      !!paste0("Fatalities_Lag", lag_val) := lag(fatalities, lag_val)
    )
}

#Month Rolling Averages#

acc.m_mod <- acc.m_mod %>%
  arrange(year, month) %>%
  mutate(
    Rolling_Avg_3m_a = rollmean(accidents, k = 3, fill = NA, align = "right"),
    Rolling_Avg_6m_a = rollmean(accidents, k = 6, fill = NA, align = "right"),
    Rolling_Avg_12m_a = rollmean(accidents, k = 12, fill = NA, align = "right"),
    Rolling_Avg_3m_f = rollmean(fatalities, k = 3, fill = NA, align = "right"),
    Rolling_Avg_6m_f = rollmean(fatalities, k = 6, fill = NA, align = "right"),
    Rolling_Avg_12m_f = rollmean(fatalities, k = 12, fill = NA, align = "right")
  )

#Indexes#

acc.m_mod <- acc.m_mod %>%
  mutate(
    AccidentGrowthRate = (accidents - lag(accidents)) / lag(accidents),
    FatalityGrowthRate = (fatalities - lag(fatalities)) / lag(fatalities),
    FatalityRate = fatalities / accidents,
    AccidentChangeRate = (accidents - Accidents_Lag1) / Accidents_Lag1,
    FatalityChangeRate = (fatalities - Fatalities_Lag1) / Fatalities_Lag1
  )

#Cumulative Features#

acc.m_mod <- acc.m_mod %>%
  mutate(
    CumulativeAccidents = cumsum(accidents),
    CumulativeFatalities = cumsum(fatalities),
    RollingSum_3m_a = rollsum(accidents, k = 3, fill = NA, align = "right"),
    RollingSum_6m_a = rollsum(accidents, k = 6, fill = NA, align = "right"),
    RollingSum_12m_a = rollsum(accidents, k = 12, fill = NA, align = "right"),
    RollingSum_24m_a = rollsum(accidents, k = 24, fill = NA, align = "right"),
    RollingSum_3m_f = rollsum(fatalities, k = 3, fill = NA, align = "right"),
    RollingSum_6m_f = rollsum(fatalities, k = 6, fill = NA, align = "right"),
    RollingSum_12m_f = rollsum(fatalities, k = 12, fill = NA, align = "right"),
    RollingSum_24m_f = rollsum(fatalities, k = 24, fill = NA, align = "right")
  )

##Yearly Accidents Dataset##


acc.y_mod <- acc.y_mod %>%
  arrange(year)


#Year Lags#

lag_years <- c(1, 2, 3, 5, 10)  
for (lag_val in lag_years) {
  acc.y_mod <- acc.y_mod %>%
    mutate(
      !!paste0("Accidents_Lag", lag_val) := lag(accidents, lag_val),
      !!paste0("Fatalities_Lag", lag_val) := lag(fatalities, lag_val)
    )
}

#Year Rolling Averages#

acc.y_mod <- acc.y_mod %>%
  mutate(
    Rolling_Avg_3y_a = rollmean(accidents, k = 3, fill = NA, align = "right"),
    Rolling_Avg_5y_a = rollmean(accidents, k = 5, fill = NA, align = "right"),
    Rolling_Avg_10y_a = rollmean(accidents, k = 10, fill = NA, align = "right"),
    Rolling_Avg_3y_f = rollmean(fatalities, k = 3, fill = NA, align = "right"),
    Rolling_Avg_5y_f = rollmean(fatalities, k = 5, fill = NA, align = "right"),
    Rolling_Avg_10y_f = rollmean(fatalities, k = 10, fill = NA, align = "right")
  )

#Indexes#

acc.y_mod <- acc.y_mod %>%
  mutate(
    AccidentGrowthRate = (accidents - lag(accidents)) / lag(accidents),
    FatalityGrowthRate = (fatalities - lag(fatalities)) / lag(fatalities),
    FatalityRate = fatalities / accidents,
    AccidentChangeRate = (accidents - Accidents_Lag1) / Accidents_Lag1,
    FatalityChangeRate = (fatalities - Fatalities_Lag1) / Fatalities_Lag1
  )

#Cumulative Features#

acc.y_mod <- acc.y_mod %>%
  mutate(
    CumulativeAccidents = cumsum(accidents),
    CumulativeFatalities = cumsum(fatalities),
    RollingSum_3y_a = rollsum(accidents, k = 3, fill = NA, align = "right"),
    RollingSum_5y_a = rollsum(accidents, k = 5, fill = NA, align = "right"),
    RollingSum_10y_a = rollsum(accidents, k = 10, fill = NA, align = "right"),
    RollingSum_3y_f = rollsum(fatalities, k = 3, fill = NA, align = "right"),
    RollingSum_5y_f = rollsum(fatalities, k = 5, fill = NA, align = "right"),
    RollingSum_10y_f = rollsum(fatalities, k = 10, fill = NA, align = "right")
  )

##Weekly Accidents & Yearly Indicators Dataset##


acc.w.ind.y_mod <- acc.w.ind.y_mod %>% select(-c(
  environmental_taxes, 
  petrol_cons, 
  ng_cons, 
  el_cons, 
  fatal_per_million_veh, 
  speed_infr, 
  drink_infr, 
  belt_infr, 
  helmet_infr, 
  gini_index
))



acc.w.ind.y_mod <- acc.w.ind.y_mod %>% drop_na()

#Assign Season, Quarter, IsSummer, IsWinter#

acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  mutate(
    quarter = case_when(
      month %in% c(1, 2, 3) ~ 1,
      month %in% c(4, 5, 6) ~ 2,
      month %in% c(7, 8, 9) ~ 3,
      month %in% c(10, 11, 12) ~ 4
    ),
    season = sapply(month, get_season),
    is_summer = ifelse(season == "Summer", 1, 0),
    is_winter = ifelse(season == "Winter", 1, 0)
  )

#Holidays and Special Events#

acc.w.ind.y_mod <- compute_stationary_holidays(acc.w.ind.y_mod)
acc.w.ind.y_mod <- compute_movable_holidays(acc.w.ind.y_mod)

#Week Lags#

lag_weeks <- c(1, 2, 3, 4, 6, 7, 8, 12, 24, 52)
acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  arrange(year, month, week)
for (lag_val in lag_weeks) {
  acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
    mutate(
      !!paste0("Accidents_Lag", lag_val) := lag(accidents, lag_val),
      !!paste0("Fatalities_Lag", lag_val) := lag(fatalities, lag_val)
    )
}

#Weekly Rolling Averages#

acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  arrange(year, month, week) %>%
  mutate(
    Rolling_Avg_4w_a = rollmean(accidents, k = 4, fill = NA, align = "right"),
    Rolling_Avg_7w_a = rollmean(accidents, k = 7, fill = NA, align = "right"),
    Rolling_Avg_12w_a = rollmean(accidents, k = 12, fill = NA, align = "right"),
    Rolling_Avg_24w_a = rollmean(accidents, k = 24, fill = NA, align = "right"),
    Rolling_Avg_52w_a = rollmean(accidents, k = 52, fill = NA, align = "right")
  )
acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  arrange(year, month, week) %>%
  mutate(
    Rolling_Avg_4w_f = rollmean(fatalities, k = 4, fill = NA, align = "right"),
    Rolling_Avg_7w_f = rollmean(fatalities, k = 7, fill = NA, align = "right"),
    Rolling_Avg_12w_f = rollmean(fatalities, k = 12, fill = NA, align = "right"),
    Rolling_Avg_24w_f = rollmean(fatalities, k = 24, fill = NA, align = "right"),
    Rolling_Avg_52w_f = rollmean(fatalities, k = 52, fill = NA, align = "right")
  )


#Indexes#

acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  mutate(
    AccidentGrowthRate = (accidents - lag(accidents)) / lag(accidents),
    FatalityGrowthRate = (fatalities - lag(fatalities)) / lag(fatalities),
    FatalityRate = fatalities / accidents,
    AccidentChangeRate = (accidents - Accidents_Lag1) / Accidents_Lag1,
    FatalityChangeRate = (fatalities - Fatalities_Lag1) / Fatalities_Lag1
  )

#Additional Features#

acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  mutate(
    AccidentsPerCapita = accidents / population,
    FatalitiesPerCapita = fatalities / population,
    AccidentsPerVehicle = accidents / vehicles,
    AccidentsUnemployment = accidents * unemployment,
    FatalitiesInflation = fatalities * inflation_rate
  )
acc.w.ind.y_mod$economic_crisis <- ifelse(acc.w.ind.y_mod$year >= 2009 & acc.w.ind.y_mod$year <= 2018, 1, 0)
covid_start.acc.w <- as.Date("2020-03-01")
covid_end.acc.w <- as.Date("2022-05-31")
acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  mutate(covid = ifelse(week_date >= covid_start.acc.w & week_date <= covid_end.acc.w, 1, 0))
acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  mutate(new_highways = ifelse(year >= 2017, 1, 0))
acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  mutate(
    speed_limits  = ifelse(year >= 1999, 1, 0),
    seat_belt     = ifelse(year >= 1999, 1, 0),
    helmet        = ifelse(year >= 1999, 1, 0),
    alcohol       = ifelse(year >= 1999, 1, 0),
    phone         = ifelse(year >= 1999, 1, 0),
    licensing     = ifelse(year >= 1999, 1, 0),
    airbags       = ifelse(year >= 2009, 1, 0),
    abs           = ifelse(year >= 2009, 1, 0),
    kid_seat      = ifelse(year >= 1999, 1, 0),
    fines         = ifelse(year >= 2007, 1, 0),
    pedestrians   = ifelse(year >= 2011, 1, 0),
    kteo          = ifelse(year >= 1999, 1, 0),
    point_system  = ifelse(year >= 2007, 1, 0)
  )

#Cumulative Features#

acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  mutate(
    CumulativeAccidents = cumsum(accidents),
    CumulativeFatalities = cumsum(fatalities),
    RollingSum_4w_a = rollsum(accidents, k = 4, fill = NA, align = "right"),
    RollingSum_8w_a = rollsum(accidents, k = 8, fill = NA, align = "right"),
    RollingSum_12w_a = rollsum(accidents, k = 12, fill = NA, align = "right"),
    RollingSum_24w_a = rollsum(accidents, k = 24, fill = NA, align = "right"),
    RollingSum_4w_f = rollsum(fatalities, k = 4, fill = NA, align = "right"),
    RollingSum_8w_f = rollsum(fatalities, k = 8, fill = NA, align = "right"),
    RollingSum_12w_f = rollsum(fatalities, k = 12, fill = NA, align = "right"),
    RollingSum_24w_f = rollsum(fatalities, k = 24, fill = NA, align = "right")
  )

##Monthly Accidents & Yearly Indicators Dataset##


acc.m.ind.y_mod <- acc.m.ind.y_mod %>% select(-c(
  environmental_taxes, 
  petrol_cons, 
  ng_cons, 
  el_cons, 
  fatal_per_million_veh, 
  speed_infr, 
  drink_infr, 
  belt_infr, 
  helmet_infr, 
  gini_index
))


acc.m.ind.y_mod <- acc.m.ind.y_mod %>% drop_na()


#Assign Season, Quarter, IsSummer, IsWinter#

acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  mutate(
    quarter = case_when(
      month %in% c(1, 2, 3) ~ 1,
      month %in% c(4, 5, 6) ~ 2,
      month %in% c(7, 8, 9) ~ 3,
      month %in% c(10, 11, 12) ~ 4
    ),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    ),
    is_summer = ifelse(season == "Summer", 1, 0),
    is_winter = ifelse(season == "Winter", 1, 0)
  )

#Holidays and Special Events#

acc.m.ind.y_mod <- compute_stationary_holidays(acc.m.ind.y_mod)
acc.m.ind.y_mod <- compute_movable_holidays(acc.m.ind.y_mod)

#Month Lags#

lag_months <- c(1, 2, 3, 6, 12, 24)
acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  arrange(year, month)
for (lag_val in lag_months) {
  acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
    mutate(
      !!paste0("Accidents_Lag", lag_val) := lag(accidents, lag_val),
      !!paste0("Fatalities_Lag", lag_val) := lag(fatalities, lag_val)
    )
}

#Monthly Rolling Averages#

acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  arrange(year, month) %>%
  mutate(
    Rolling_Avg_3m_a = rollmean(accidents, k = 3, fill = NA, align = "right"),
    Rolling_Avg_6m_a = rollmean(accidents, k = 6, fill = NA, align = "right"),
    Rolling_Avg_12m_a = rollmean(accidents, k = 12, fill = NA, align = "right"),
    Rolling_Avg_3m_f = rollmean(fatalities, k = 3, fill = NA, align = "right"),
    Rolling_Avg_6m_f = rollmean(fatalities, k = 6, fill = NA, align = "right"),
    Rolling_Avg_12m_f = rollmean(fatalities, k = 12, fill = NA, align = "right")
  )

#Indexes#

acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  mutate(
    AccidentGrowthRate = (accidents - lag(accidents)) / lag(accidents),
    FatalityGrowthRate = (fatalities - lag(fatalities)) / lag(fatalities),
    FatalityRate = fatalities / accidents,
    AccidentChangeRate = (accidents - Accidents_Lag1) / Accidents_Lag1,
    FatalityChangeRate = (fatalities - Fatalities_Lag1) / Fatalities_Lag1,
    AccidentsPerCapita = accidents / population,
    FatalitiesPerCapita = fatalities / population,
    AccidentsPerVehicle = accidents / vehicles,
    AccidentsUnemployment = accidents * unemployment,
    FatalitiesInflation = fatalities * inflation_rate,
  )
acc.m.ind.y_mod$economic_crisis <- ifelse(acc.m.ind.y_mod$year >= 2009 & acc.m.ind.y_mod$year <= 2018, 1, 0)
covid_start.acc.m <- as.Date("2020-03-01")
covid_end.acc.m <- as.Date("2022-05-31")
acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  mutate(covid = ifelse(month_date >= covid_start.acc.m & month_date <= covid_end.acc.m, 1, 0))
acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  mutate(new_highways = ifelse(year >= 2017, 1, 0))
acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  mutate(
    speed_limits  = ifelse(year >= 1999, 1, 0),
    seat_belt     = ifelse(year >= 1999, 1, 0),
    helmet        = ifelse(year >= 1999, 1, 0),
    alcohol       = ifelse(year >= 1999, 1, 0),
    phone         = ifelse(year >= 1999, 1, 0),
    licensing     = ifelse(year >= 1999, 1, 0),
    airbags       = ifelse(year >= 2009, 1, 0),
    abs           = ifelse(year >= 2009, 1, 0),
    kid_seat      = ifelse(year >= 1999, 1, 0),
    fines         = ifelse(year >= 2007, 1, 0),
    pedestrians   = ifelse(year >= 2011, 1, 0),
    kteo          = ifelse(year >= 1999, 1, 0),
    point_system  = ifelse(year >= 2007, 1, 0)
  )

#Cumulative Features#

acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  mutate(
    CumulativeAccidents = cumsum(accidents),
    CumulativeFatalities = cumsum(fatalities),
    RollingSum_3m_a = rollsum(accidents, k = 3, fill = NA, align = "right"),
    RollingSum_6m_a = rollsum(accidents, k = 6, fill = NA, align = "right"),
    RollingSum_12m_a = rollsum(accidents, k = 12, fill = NA, align = "right"),
    RollingSum_24m_a = rollsum(accidents, k = 24, fill = NA, align = "right"),
    RollingSum_3m_f = rollsum(fatalities, k = 3, fill = NA, align = "right"),
    RollingSum_6m_f = rollsum(fatalities, k = 6, fill = NA, align = "right"),
    RollingSum_12m_f = rollsum(fatalities, k = 12, fill = NA, align = "right"),
    RollingSum_24m_f = rollsum(fatalities, k = 24, fill = NA, align = "right")
  )

##Yearly Accidents & Yearly Indicators Dataset##

acc.y.ind.y_mod <- acc.y.ind.y_mod %>% select(-c(
  environmental_taxes, 
  petrol_cons, 
  ng_cons, 
  el_cons, 
  fatal_per_million_veh, 
  speed_infr, 
  drink_infr, 
  belt_infr, 
  helmet_infr, 
  gini_index
))

acc.y.ind.y_mod <- acc.y.ind.y_mod %>% drop_na()

acc.y.ind.y_mod <- acc.y.ind.y_mod %>%
  arrange(year)


#Year Lags#

lag_years <- c(1, 2, 3, 5, 10)
for (lag_val in lag_years) {
  acc.y.ind.y_mod <- acc.y.ind.y_mod %>%
    mutate(
      !!paste0("Accidents_Lag", lag_val) := lag(accidents, lag_val),
      !!paste0("Fatalities_Lag", lag_val) := lag(fatalities, lag_val)
    )
}

#Yearly Rolling Averages#

acc.y.ind.y_mod <- acc.y.ind.y_mod %>%
  mutate(
    Rolling_Avg_3y_a = rollmean(accidents, k = 3, fill = NA, align = "right"),
    Rolling_Avg_5y_a = rollmean(accidents, k = 5, fill = NA, align = "right"),
    Rolling_Avg_10y_a = rollmean(accidents, k = 10, fill = NA, align = "right"),
    Rolling_Avg_3y_f = rollmean(fatalities, k = 3, fill = NA, align = "right"),
    Rolling_Avg_5y_f = rollmean(fatalities, k = 5, fill = NA, align = "right"),
    Rolling_Avg_10y_f = rollmean(fatalities, k = 10, fill = NA, align = "right")
  )

#Indexes#

acc.y.ind.y_mod <- acc.y.ind.y_mod %>%
  mutate(
    AccidentGrowthRate = (accidents - lag(accidents)) / lag(accidents),
    FatalityGrowthRate = (fatalities - lag(fatalities)) / lag(fatalities),
    FatalityRate = fatalities / accidents,
    AccidentChangeRate = (accidents - Accidents_Lag1) / Accidents_Lag1,
    FatalityChangeRate = (fatalities - Fatalities_Lag1) / Fatalities_Lag1,
    AccidentsPerCapita = accidents / population,
    FatalitiesPerCapita = fatalities / population,
    AccidentsPerVehicle = accidents / vehicles,
    AccidentsUnemployment = accidents * unemployment,
    FatalitiesInflation = fatalities * inflation_rate
  )
acc.y.ind.y_mod$year <- as.numeric(as.character(acc.y.ind.y_mod$year))
acc.y.ind.y_mod$economic_crisis <- ifelse(acc.y.ind.y_mod$year >= 2009 & acc.y.ind.y_mod$year <= 2018, 1, 0)
covid_start.acc.y <- as.Date("2020-03-01")
covid_end.acc.y <- as.Date("2022-05-31")
acc.y.ind.y_mod <- acc.y.ind.y_mod %>%
  mutate(covid = ifelse(year_date >= covid_start.acc.y & year_date <= covid_end.acc.y, 1, 0))
acc.y.ind.y_mod <- acc.y.ind.y_mod %>%
  mutate(new_highways = ifelse(year >= 2017, 1, 0))
acc.y.ind.y_mod <- acc.y.ind.y_mod %>%
  mutate(
    speed_limits  = ifelse(year >= 1999, 1, 0),
    seat_belt     = ifelse(year >= 1999, 1, 0),
    helmet        = ifelse(year >= 1999, 1, 0),
    alcohol       = ifelse(year >= 1999, 1, 0),
    phone         = ifelse(year >= 1999, 1, 0),
    licensing     = ifelse(year >= 1999, 1, 0),
    airbags       = ifelse(year >= 2009, 1, 0),
    abs           = ifelse(year >= 2009, 1, 0),
    kid_seat      = ifelse(year >= 1999, 1, 0),
    fines         = ifelse(year >= 2007, 1, 0),
    pedestrians   = ifelse(year >= 2011, 1, 0),
    kteo          = ifelse(year >= 1999, 1, 0),
    point_system  = ifelse(year >= 2007, 1, 0)
  )
acc.y.ind.y_mod$year <- as.factor(acc.y.ind.y_mod$year)

#Cumulative Features#

acc.y.ind.y_mod <- acc.y.ind.y_mod %>%
  mutate(
    CumulativeAccidents = cumsum(accidents),
    CumulativeFatalities = cumsum(fatalities),
    RollingSum_3y_a = rollsum(accidents, k = 3, fill = NA, align = "right"),
    RollingSum_5y_a = rollsum(accidents, k = 5, fill = NA, align = "right"),
    RollingSum_10y_a = rollsum(accidents, k = 10, fill = NA, align = "right"),
    RollingSum_3y_f = rollsum(fatalities, k = 3, fill = NA, align = "right"),
    RollingSum_5y_f = rollsum(fatalities, k = 5, fill = NA, align = "right"),
    RollingSum_10y_f = rollsum(fatalities, k = 10, fill = NA, align = "right")
  )

###Dfs Extract###



##Weekly##


cols_to_keep_w <- c(
  "year", "month", "week", "accidents", "fatalities", "total_gdp",
  "gdp_per_capita", "unemployment", "population", "vehicles", "inflation_rate",
  "quarter", "season", "is_summer", "is_winter", "is_holiday",
  "AccidentGrowthRate", "FatalityGrowthRate", "FatalityRate",
  "AccidentChangeRate", "FatalityChangeRate",
  "AccidentsPerCapita", "FatalitiesPerCapita", "AccidentsPerVehicle",
  "economic_crisis", "covid", "new_highways", "speed_limits", "seat_belt",
  "helmet", "alcohol", "phone", "licensing", "airbags", "abs", "kid_seat",
  "fines", "pedestrians", "kteo", "point_system"
)
acc_selected_w <- acc.w.ind.y_mod[, cols_to_keep_w]
write.csv(acc_selected_w, "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/Dfs_Extraction/Weekly/Csv/Weekly_Accidents.csv", row.names = FALSE)
write_xlsx(acc_selected_w, "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/Dfs_Extraction/Weekly/Excel/Weekly_Accidents.xlsx")


##Monthly##


cols_to_keep_m <- c(
  "year", "month", "accidents", "fatalities", "total_gdp",
  "gdp_per_capita", "unemployment", "population", "vehicles", "inflation_rate",
  "quarter", "season", "is_summer", "is_winter", "is_holiday",
  "AccidentGrowthRate", "FatalityGrowthRate", "FatalityRate",
  "AccidentChangeRate", "FatalityChangeRate",
  "AccidentsPerCapita", "FatalitiesPerCapita", "AccidentsPerVehicle",
  "economic_crisis", "covid", "new_highways", "speed_limits", "seat_belt",
  "helmet", "alcohol", "phone", "licensing", "airbags", "abs", "kid_seat",
  "fines", "pedestrians", "kteo", "point_system"
)
acc_selected_m <- acc.m.ind.y_mod[, cols_to_keep_m]
write.csv(
  acc_selected_m,
  "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/Dfs_Extraction/Monthly/Csv/Monthly_Accidents.csv",
  row.names = FALSE
)
write_xlsx(
  acc_selected_m,
  "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/Dfs_Extraction/Monthly/Excel/Monthly_Accidents.xlsx"
)


##Yearly##


cols_to_keep_y <- c(
  "year", "accidents", "fatalities", "total_gdp",
  "gdp_per_capita", "unemployment", "population", "vehicles", "inflation_rate",
  "AccidentGrowthRate", "FatalityGrowthRate", "FatalityRate",
  "AccidentChangeRate", "FatalityChangeRate",
  "AccidentsPerCapita", "FatalitiesPerCapita", "AccidentsPerVehicle",
  "economic_crisis", "covid", "new_highways", "speed_limits", "seat_belt",
  "helmet", "alcohol", "phone", "licensing", "airbags", "abs", "kid_seat",
  "fines", "pedestrians", "kteo", "point_system"
)
acc_selected_y <- acc.y.ind.y_mod[, cols_to_keep_y]
write.csv(
  acc_selected_y,
  "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/Dfs_Extraction/Yearly/Csv/Yearly_Accidents.csv",
  row.names = FALSE
)
write_xlsx(
  acc_selected_y,
  "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/Dfs_Extraction/Yearly/Excel/Yearly_Accidents.xlsx"
)


                                                                          ###NOTICE!!###
                                ###(Datasets used are: acc.w.ind.y_mod,acc.m.ind.y_mod, acc.m.ind.y_mod ARE USED)###

###Descriptive Statistics###



##Function##


compute_statistics <- function(df) {
  numeric_df <- df %>% select(where(is.numeric))  
  stats <- data.frame(
    Variable = colnames(numeric_df),
    Std_Dev = apply(numeric_df, 2, sd, na.rm = TRUE),
    Count = colSums(!is.na(numeric_df)),  
    Length = nrow(numeric_df),  
    Max = apply(numeric_df, 2, max, na.rm = TRUE),
    Min = apply(numeric_df, 2, min, na.rm = TRUE),
    Mode = apply(numeric_df, 2, function(x) {
      ux <- unique(na.omit(x))
      ux[which.max(tabulate(match(x, ux)))]
    }),
    Median = apply(numeric_df, 2, median, na.rm = TRUE),
    Range = apply(numeric_df, 2, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    Variance = apply(numeric_df, 2, var, na.rm = TRUE),
    IQR = apply(numeric_df, 2, IQR, na.rm = TRUE),
    CV = apply(numeric_df, 2, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)), 
    Q1 = apply(numeric_df, 2, function(x) quantile(x, 0.25, na.rm = TRUE)),  
    Q2 = apply(numeric_df, 2, function(x) quantile(x, 0.50, na.rm = TRUE)),  
    Q3 = apply(numeric_df, 2, function(x) quantile(x, 0.75, na.rm = TRUE)),  
    Skewness = apply(numeric_df, 2, function(x) skewness(x, na.rm = TRUE)),
    Kurtosis = apply(numeric_df, 2, function(x) kurtosis(x, na.rm = TRUE)),
    p10 = apply(numeric_df, 2, function(x) quantile(x, 0.10, na.rm = TRUE)),  
    p25 = apply(numeric_df, 2, function(x) quantile(x, 0.25, na.rm = TRUE)),  
    p50 = apply(numeric_df, 2, function(x) quantile(x, 0.50, na.rm = TRUE)), 
    p75 = apply(numeric_df, 2, function(x) quantile(x, 0.75, na.rm = TRUE)),  
    p90 = apply(numeric_df, 2, function(x) quantile(x, 0.90, na.rm = TRUE))   
  )
  return(stats)
}


##Weekly Accidents & Yearly Indicators Dataset##


acc.w.ind.y_mod <- acc.w.ind.y_mod %>%
  mutate(
    season_numeric = case_when(
      season == "Winter" ~ 1,
      season == "Spring" ~ 2,
      season == "Summer" ~ 3,
      season == "Autumn" ~ 4
    )
  )
statistics_table_acc.w <- compute_statistics(acc.w.ind.y_mod)
statistics_df_acc.w <- as.data.frame(statistics_table_acc.w)
print(statistics_table_acc.w)
statistics_df_acc.w
write.xlsx(statistics_df_acc.w, "Desc_Stats_Acc_Weekly.xlsx")

##Monthly Accidents & Yearly Indicators Dataset##


acc.m.ind.y_mod <- acc.m.ind.y_mod %>%
  mutate(
    season_numeric = case_when(
      season == "Winter" ~ 1,
      season == "Spring" ~ 2,
      season == "Summer" ~ 3,
      season == "Autumn" ~ 4
    )
  )
statistics_table_acc.m <- compute_statistics(acc.m.ind.y_mod)
statistics_df_acc.m <- as.data.frame(statistics_table_acc.m)
print(statistics_table_acc.m)
statistics_df_acc.m
write.xlsx(statistics_df_acc.m, "Desc_Stats_Acc_Monthly.xlsx")


##Yearly Accidents & Yearly Indicators Dataset##


statistics_table_acc.y <- compute_statistics(acc.y.ind.y_mod)
statistics_df_acc.y <- as.data.frame(statistics_table_acc.y)
print(statistics_table_acc.y)
statistics_df_acc.y
write.xlsx(statistics_df_acc.y, "Desc_Stats_Acc_Yearly.xlsx")


###Inferential Statistics###



##Weekly Accidents & Yearly Indicators Dataset##


numeric_columns.acc.w <- acc.w.ind.y_mod %>% select(where(is.numeric))
shapiro_results.acc.w <- apply(numeric_columns.acc.w, 2, function(x) if(length(x) <= 5000) shapiro.test(x)$p.value else NA)
ks_results.acc.w <- apply(numeric_columns.acc.w, 2, function(x) ks.test(x, "pnorm", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))$p.value)
t_test_one_sample.acc.w <- t.test(numeric_columns.acc.w$accidents, mu = 200)
t_test_two_sample.acc.w <- t.test(accidents ~ is_summer, data = acc.w.ind.y_mod)
anova_test.acc.w <- aov(accidents ~ as.factor(season_numeric), data = acc.w.ind.y_mod)
anova_results.acc.w <- summary(anova_test.acc.w)
wilcox.test.acc.w <- wilcox.test(numeric_columns.acc.w$accidents, numeric_columns.acc.w$fatalities, paired = TRUE)
kruskal_test.acc.w <- kruskal.test(accidents ~ as.factor(season_numeric), data = acc.w.ind.y_mod)
fisher_test.acc.w <- fisher.test(table(acc.w.ind.y_mod$is_summer, acc.w.ind.y_mod$is_holiday))
list(
  Shapiro_Wilk.acc.w = shapiro_results.acc.w,
  Kolmogorov_Smirnov.acc.w = ks_results.acc.w,
  One_Sample_T_Test.acc.w = t_test_one_sample.acc.w,
  Two_Sample_T_Test.acc.w = t_test_two_sample.acc.w,
  ANOVA_Test.acc.w = anova_results.acc.w,
  Wilcoxon_Test.acc.w = wilcox.test.acc.w,
  Kruskal_Wallis_Test.acc.w = kruskal_test.acc.w,
  Fishers_Exact_Test.acc.w = fisher_test.acc.w
)
acc.w.ind.y_mod$season_numeric <- as.factor(acc.w.ind.y_mod$season_numeric)
tukey_results.acc.w <- TukeyHSD(anova_test.acc.w)
print(tukey_results.acc.w)
plot(tukey_results.acc.w)
dunn_results.acc.w <- dunnTest(acc.w.ind.y_mod$accidents, acc.w.ind.y_mod$season_numeric, method = "bonferroni")
print(dunn_results.acc.w)
dunn_table.acc.w <- dunn_results.acc.w$res
print(dunn_table.acc.w)
pairwise.wilcox.test.acc.w <- pairwise.wilcox.test(acc.w.ind.y_mod$accidents, acc.w.ind.y_mod$season_numeric, p.adjust.method = "bonferroni")
pairwise.wilcox.test.acc.w


##Monthly Accidents & Yearly Indicators Dataset##


numeric_columns.acc.m <- acc.m.ind.y_mod %>% select(where(is.numeric))
shapiro_results.acc.m <- apply(numeric_columns.acc.m, 2, function(x) if(length(x) <= 5000) shapiro.test(x)$p.value else NA)
ks_results.acc.m <- apply(numeric_columns.acc.m, 2, function(x) ks.test(x, "pnorm", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))$p.value)
t_test_one_sample.acc.m <- t.test(numeric_columns.acc.m$accidents, mu = 200)
t_test_two_sample.acc.m <- t.test(accidents ~ is_summer, data = acc.m.ind.y_mod)
anova_test.acc.m <- aov(accidents ~ as.factor(season_numeric), data = acc.m.ind.y_mod)
anova_results.acc.m <- summary(anova_test.acc.m)
wilcox.test.acc.m <- wilcox.test(numeric_columns.acc.m$accidents, numeric_columns.acc.m$fatalities, paired = TRUE)
kruskal_test.acc.m <- kruskal.test(accidents ~ as.factor(season_numeric), data = acc.m.ind.y_mod)
fisher_test.acc.m <- fisher.test(table(acc.m.ind.y_mod$is_summer, acc.m.ind.y_mod$is_holiday))
list(
  Shapiro_Wilk.acc.m = shapiro_results.acc.m,
  Kolmogorov_Smirnov.acc.m = ks_results.acc.m,
  One_Sample_T_Test.acc.m = t_test_one_sample.acc.m,
  Two_Sample_T_Test.acc.m = t_test_two_sample.acc.m,
  ANOVA_Test.acc.m = anova_results.acc.m,
  Wilcoxon_Test.acc.m = wilcox.test.acc.m,
  Kruskal_Wallis_Test.acc.m = kruskal_test.acc.m,
  Fishers_Exact_Test.acc.m = fisher_test.acc.m
)
acc.m.ind.y_mod$season_numeric <- as.factor(acc.m.ind.y_mod$season_numeric)
tukey_results.acc.m <- TukeyHSD(anova_test.acc.m)
print(tukey_results.acc.m)
plot(tukey_results.acc.m)
dunn_results.acc.m <- dunnTest(acc.m.ind.y_mod$accidents, acc.m.ind.y_mod$season_numeric, method = "bonferroni")
print(dunn_results.acc.m)
dunn_table.acc.m <- dunn_results.acc.m$res
print(dunn_table.acc.m)
pairwise.wilcox.test.acc.m <- pairwise.wilcox.test(acc.m.ind.y_mod$accidents, acc.m.ind.y_mod$season_numeric, p.adjust.method = "bonferroni")
pairwise.wilcox.test.acc.m


##Yearly Accidents & Yearly Indicators Dataset##


numeric_columns.acc.y <- acc.y.ind.y_mod %>% select(where(is.numeric))
shapiro_results.acc.y <- apply(numeric_columns.acc.y, 2, function(x) if(length(x) <= 5000) shapiro.test(x)$p.value else NA)
ks_results.acc.y <- apply(numeric_columns.acc.y, 2, function(x) ks.test(x, "pnorm", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))$p.value)
t_test_one_sample.acc.y <- t.test(numeric_columns.acc.y$accidents, mu = 200)
wilcox.test.acc.y <- wilcox.test(numeric_columns.acc.y$accidents, numeric_columns.acc.y$fatalities, paired = TRUE)
list(
  Shapiro_Wilk.acc.m = shapiro_results.acc.m,
  Kolmogorov_Smirnov.acc.m = ks_results.acc.m,
  One_Sample_T_Test.acc.m = t_test_one_sample.acc.m,
  Wilcoxon_Test.acc.m = wilcox.test.acc.m
)


###Other Statistics###



##Weekly Accidents & Yearly Indicators Dataset##


numeric_df.acc.w <- acc.w.ind.y_mod %>% select_if(is.numeric)
numeric_df_clean.acc.w <- numeric_df.acc.w %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_all(~ ifelse(is.infinite(.), mean(., na.rm = TRUE), .)) %>%
  select_if(~ var(., na.rm = TRUE) > 0)
cor_pearson.acc.w <- cor(numeric_df.acc.w, use = "complete.obs", method = "pearson")
cor_spearman.acc.w <- cor(numeric_df.acc.w, use = "complete.obs", method = "spearman")
cor_kendall.acc.w <- cor(numeric_df.acc.w, use = "complete.obs", method = "kendall")
cov_matrix.acc.w <- cov(numeric_df.acc.w, use = "complete.obs")
print("Pearson Correlation Matrix:")
print(cor_pearson.acc.w)
print("Spearman Correlation Matrix:")
print(cor_spearman.acc.w)
print("Kendall Correlation Matrix:")
print(cor_kendall.acc.w)
print("Covariance Matrix:")
print(cov_matrix.acc.w)
simple_lm.acc.w <- lm(accidents ~ vehicles, data = acc.w.ind.y_mod)
print(summary(simple_lm.acc.w))
multi_lm.acc.w <- lm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.w.ind.y_mod)
print(summary(multi_lm.acc.w))
par(mfrow = c(2, 2))
plot(multi_lm.acc.w) 
pca_model.acc.w <- prcomp(numeric_df_clean.acc.w, center = TRUE, scale. = TRUE)
print(summary(pca_model.acc.w))
fviz_eig(pca_model.acc.w)
fviz_pca_biplot(pca_model.acc.w, repel = TRUE)
set.seed(42)
kmeans_result.acc.w <- kmeans(numeric_df_clean.acc.w, centers = 3) 
acc.w.ind.y_mod$cluster <- as.factor(kmeans_result.acc.w$cluster)
fviz_cluster(kmeans_result.acc.w, data = numeric_df_clean.acc.w, stand = FALSE)
acc.w.ind.y_mod$z_score <- scale(acc.w.ind.y_mod$accidents)
outliers.acc.w <- acc.w.ind.y_mod %>% filter(abs(z_score) > 3)
print("Detected Outliers based on Z-Score:")
print(outliers.acc.w)
rf_model.acc.w <- randomForest(accidents ~ ., data = numeric_df_clean.acc.w, importance = TRUE)
varImpPlot(rf_model.acc.w)
xgb_model.acc.w <- xgboost(data = as.matrix(numeric_df_clean.acc.w), label = numeric_df_clean.acc.w$accidents, 
                     nrounds = 100, objective = "reg:squarederror")
xgb_model.acc.w
anova_season.acc.w <- aov(accidents ~ as.factor(season_numeric), data = acc.w.ind.y_mod)
summary(anova_season.acc.w)
anova_holiday.acc.w <- aov(accidents ~ is_holiday, data = acc.w.ind.y_mod)
summary(anova_holiday.acc.w)
kruskal_gdp.acc.w <- kruskal.test(accidents ~ as.factor(cut(gdp_per_capita, breaks = 3)), data = acc.w.ind.y_mod)
kruskal_unemployment.acc.w <- kruskal.test(accidents ~ as.factor(cut(unemployment, breaks = 3)), data = acc.w.ind.y_mod)
kruskal_gdp.acc.w
kruskal_unemployment.acc.w
adf_test_acc.w <- adf.test(acc.w.ind.y_mod$accidents)
adf_test_fatal.w <- adf.test(acc.w.ind.y_mod$fatalities)
adf_test_acc.w
adf_test_fatal.w
poisson_model.acc.w <- glm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.w.ind.y_mod, family = poisson)
summary(poisson_model.acc.w)
par(mfrow = c(1,1))
acf(acc.w.ind.y_mod$accidents, lag.max = 30)
pacf(acc.w.ind.y_mod$accidents, lag.max = 30)
acf(acc.w.ind.y_mod$fatalities, lag.max = 30)
pacf(acc.w.ind.y_mod$fatalities, lag.max = 30)


##Monthly Accidents & Yearly Indicators Dataset##


numeric_df.acc.m <- acc.m.ind.y_mod %>% select_if(is.numeric)
numeric_df_clean.acc.m <- numeric_df.acc.m %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_all(~ ifelse(is.infinite(.), mean(., na.rm = TRUE), .)) %>%
  select_if(~ var(., na.rm = TRUE) > 0)
cor_pearson.acc.m <- cor(numeric_df.acc.m, use = "complete.obs", method = "pearson")
cor_spearman.acc.m <- cor(numeric_df.acc.m, use = "complete.obs", method = "spearman")
cor_kendall.acc.m <- cor(numeric_df.acc.m, use = "complete.obs", method = "kendall")
cov_matrix.acc.m <- cov(numeric_df.acc.m, use = "complete.obs")
print("Pearson Correlation Matrix:")
print(cor_pearson.acc.m)
print("Spearman Correlation Matrix:")
print(cor_spearman.acc.m)
print("Kendall Correlation Matrix:")
print(cor_kendall.acc.m)
print("Covariance Matrix:")
print(cov_matrix.acc.m)
simple_lm.acc.m <- lm(accidents ~ vehicles, data = acc.m.ind.y_mod)
print(summary(simple_lm.acc.m))
multi_lm.acc.m <- lm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.m.ind.y_mod)
print(summary(multi_lm.acc.m))
par(mfrow = c(2, 2))
plot(multi_lm.acc.m) 
acc.m.ind.y_mod$fatal_binary <- ifelse(acc.m.ind.y_mod$fatalities > 0, 1, 0)
logistic_model.acc.m <- glm(fatal_binary ~ vehicles + unemployment + inflation_rate, 
                            data = acc.m.ind.y_mod, family = binomial)
print(summary(logistic_model.acc.m))
pca_model.acc.m <- prcomp(numeric_df_clean.acc.m, center = TRUE, scale. = TRUE)
print(summary(pca_model.acc.m))
fviz_eig(pca_model.acc.m)
fviz_pca_biplot(pca_model.acc.m, repel = TRUE)
set.seed(42)
kmeans_result.acc.m <- kmeans(numeric_df_clean.acc.m, centers = 3) 
acc.m.ind.y_mod$cluster <- as.factor(kmeans_result.acc.m$cluster)
fviz_cluster(kmeans_result.acc.m, data = numeric_df_clean.acc.m, stand = FALSE)
acc.m.ind.y_mod$z_score <- scale(acc.m.ind.y_mod$accidents)
outliers.acc.m <- acc.m.ind.y_mod %>% filter(abs(z_score) > 3)
print("Detected Outliers based on Z-Score:")
print(outliers.acc.m)
rf_model.acc.m <- randomForest(accidents ~ ., data = numeric_df_clean.acc.m, importance = TRUE)
varImpPlot(rf_model.acc.m)
xgb_model.acc.m <- xgboost(data = as.matrix(numeric_df_clean.acc.m), label = numeric_df_clean.acc.m$accidents, 
                           nrounds = 100, objective = "reg:squarederror")
xgb_model.acc.m
anova_season.acc.m <- aov(accidents ~ as.factor(season_numeric), data = acc.m.ind.y_mod)
summary(anova_season.acc.m)
anova_holiday.acc.m <- aov(accidents ~ is_holiday, data = acc.m.ind.y_mod)
summary(anova_holiday.acc.m)
kruskal_gdp.acc.m <- kruskal.test(accidents ~ as.factor(cut(gdp_per_capita, breaks = 3)), data = acc.m.ind.y_mod)
kruskal_unemployment.acc.m <- kruskal.test(accidents ~ as.factor(cut(unemployment, breaks = 3)), data = acc.m.ind.y_mod)
kruskal_gdp.acc.m
kruskal_unemployment.acc.m
cor_macro.acc.m <- cor(acc.m.ind.y_mod[, c("accidents", "fatalities", "gdp_per_capita", "unemployment", "inflation_rate")], method = "pearson")
cor_macro.acc.m
adf_test_acc.m <- adf.test(acc.m.ind.y_mod$accidents)
adf_test_fatal.m <- adf.test(acc.m.ind.y_mod$fatalities)
adf_test_acc.m
adf_test_fatal.m
poisson_model.acc.m <- glm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.m.ind.y_mod, family = poisson)
summary(poisson_model.acc.m)
par(mfrow = c(1,1))
acf(acc.m.ind.y_mod$accidents, lag.max = 30)
pacf(acc.m.ind.y_mod$accidents, lag.max = 30)
acf(acc.m.ind.y_mod$fatalities, lag.max = 30)
pacf(acc.m.ind.y_mod$fatalities, lag.max = 30)


##Yearly Accidents & Yearly Indicators Dataset##


numeric_df.acc.y <- acc.y.ind.y_mod %>% select_if(is.numeric)
numeric_df_clean.acc.y <- numeric_df.acc.y %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_all(~ ifelse(is.infinite(.), mean(., na.rm = TRUE), .)) %>%
  select_if(~ var(., na.rm = TRUE) > 0)
cor_pearson.acc.y <- cor(numeric_df.acc.y, use = "complete.obs", method = "pearson")
cor_spearman.acc.y <- cor(numeric_df.acc.y, use = "complete.obs", method = "spearman")
cor_kendall.acc.y <- cor(numeric_df.acc.y, use = "complete.obs", method = "kendall")
cov_matrix.acc.y <- cov(numeric_df.acc.y, use = "complete.obs")
print("Pearson Correlation Matrix:")
print(cor_pearson.acc.y)
print("Spearman Correlation Matrix:")
print(cor_spearman.acc.y)
print("Kendall Correlation Matrix:")
print(cor_kendall.acc.y)
print("Covariance Matrix:")
print(cov_matrix.acc.y)
simple_lm.acc.y <- lm(accidents ~ vehicles, data = acc.y.ind.y_mod)
print(summary(simple_lm.acc.y))
multi_lm.acc.y <- lm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.y.ind.y_mod)
print(summary(multi_lm.acc.y))
par(mfrow = c(2, 2))
plot(multi_lm.acc.y) 
acc.y.ind.y_mod$fatal_binary <- ifelse(acc.y.ind.y_mod$fatalities > 0, 1, 0)
logistic_model.acc.y <- glm(fatal_binary ~ vehicles + unemployment + inflation_rate, 
                            data = acc.y.ind.y_mod, family = binomial)
print(summary(logistic_model.acc.y))
pca_model.acc.y <- prcomp(numeric_df_clean.acc.y, center = TRUE, scale. = TRUE)
print(summary(pca_model.acc.y))
fviz_eig(pca_model.acc.y)
fviz_pca_biplot(pca_model.acc.y, repel = TRUE)
set.seed(42)
kmeans_result.acc.y <- kmeans(numeric_df_clean.acc.y, centers = 3) 
acc.y.ind.y_mod$cluster <- as.factor(kmeans_result.acc.y$cluster)
fviz_cluster(kmeans_result.acc.y, data = numeric_df_clean.acc.y, stand = FALSE)
acc.y.ind.y_mod$z_score <- scale(acc.y.ind.y_mod$accidents)
outliers.acc.y <- acc.y.ind.y_mod %>% filter(abs(z_score) > 3)
print("Detected Outliers based on Z-Score:")
print(outliers.acc.y)
rf_model.acc.y <- randomForest(accidents ~ ., data = numeric_df_clean.acc.y, importance = TRUE)
varImpPlot(rf_model.acc.y)
xgb_model.acc.y <- xgboost(data = as.matrix(numeric_df_clean.acc.y), label = numeric_df_clean.acc.y$accidents, 
                           nrounds = 100, objective = "reg:squarederror")
xgb_model.acc.y
kruskal_gdp.acc.y <- kruskal.test(accidents ~ as.factor(cut(gdp_per_capita, breaks = 3)), data = acc.y.ind.y_mod)
kruskal_unemployment.acc.y <- kruskal.test(accidents ~ as.factor(cut(unemployment, breaks = 3)), data = acc.y.ind.y_mod)
kruskal_gdp.acc.y
kruskal_unemployment.acc.y
cor_macro.acc.y <- cor(acc.y.ind.y_mod[, c("accidents", "fatalities", "gdp_per_capita", "unemployment", "inflation_rate")], method = "pearson")
cor_macro.acc.y
adf_test_acc.y <- adf.test(acc.y.ind.y_mod$accidents)
adf_test_fatal.y <- adf.test(acc.y.ind.y_mod$fatalities)
adf_test_acc.y
adf_test_fatal.y
poisson_model.acc.y <- glm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.y.ind.y_mod, family = poisson)
summary(poisson_model.acc.y)
par(mfrow = c(1,1))
acf(acc.y.ind.y_mod$accidents, lag.max = 30)
pacf(acc.y.ind.y_mod$accidents, lag.max = 30)
acf(acc.y.ind.y_mod$fatalities, lag.max = 30)
pacf(acc.y.ind.y_mod$fatalities, lag.max = 30)


###Plots###



##Weekly Accidents and Yearly Indicators Dataset##


#Time Series Plots of Accidents and Fatalities through the Weeks#

ggplot(acc.w.ind.y_mod, aes(x = week_date, y = accidents)) +
  geom_line(color = "#0072B2", size = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#0072B2", linetype = "solid") +
  labs(title = "Weekly Road Accidents in Greece",
       subtitle = "Trends in Weekly Accidents Over the Years",
       x = "Year",
       y = "Accidents") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = week_date, y = fatalities)) +
  geom_line(color = "#D55E00", size = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#D55E00", linetype = "solid") +
  labs(title = "Weekly Road Fatalities in Greece",
       subtitle = "Trends in Weekly Fatalities Over the Years",
       x = "Year",
       y = "Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Box Plots of Accidents and Fatalities through the Weeks#

ggplot(acc.w.ind.y_mod, aes(x = as.factor(month), y = accidents, fill = "Accidents")) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  stat_summary(aes(color = "Accidents"), fun = mean, geom = "point", size = 2.5, shape = 21, fill = "white") +
  labs(title = "Monthly Distribution of Road Traffic Accidents",
       subtitle = "Box Plot Showing the Spread of Road Traffic Accidents Per Month",
       x = "Month",
       y = "Accidents") +
  scale_fill_manual(values = c("Accidents" = "#0072B2")) +
  scale_color_manual(values = c("Accidents" = "#0072B2")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = as.factor(month), y = fatalities, fill = "Fatalities")) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  stat_summary(aes(color = "Fatalities"), fun = mean, geom = "point", size = 2.5, shape = 21, fill = "white") +
  labs(title = "Monthly Distribution of Road Traffic Fatalities",
       subtitle = "Box Plot Showing the Spread of Road Traffic Fatalities Per Month",
       x = "Year",
       y = "Fatalities") +
  scale_fill_manual(values = c("Fatalities" = "#D55E00")) +
  scale_color_manual(values = c("Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Histograms of Accidents  and Fatalities Count through the Weeks#

ggplot(acc.w.ind.y_mod, aes(x = accidents)) +
  geom_histogram(bins = 50, fill = "#0072B2", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Weekly Road Accidents",
       subtitle = "Distribution of Weekly Accident Counts",
       x = "Weekly Accidents",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = fatalities)) +
  geom_histogram(bins = 50, fill = "#D55E00", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Weekly Road Fatalities",
       subtitle = "Distribution of Weekly Fatality Counts",
       x = "Weekly Fatalities",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Max Min Mean Mode Median and Range of Accidents and Fatalities per Week through the Years#

ggplot(acc.w.ind.y_mod, aes(x = week)) +
  stat_summary(aes(y = accidents, color = "Max Accidents"), fun = max, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Max Fatalities"), fun = max, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Max Accidents"), fun = max, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Max Fatalities"), fun = max, geom = "point", size = 2) +
  labs(title = "Maximum Weekly Accidents and Fatalities",
       subtitle = "Highest Recorded Accidents and Fatalities Per Week Across All Years",
       x = "Week of the Year",
       y = "Maximum Count",
       color = "Legend") +
  scale_color_manual(values = c("Max Accidents" = "#0072B2", "Max Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = week)) +
  stat_summary(aes(y = accidents, color = "Min Accidents"), fun = min, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Min Fatalities"), fun = min, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Min Accidents"), fun = min, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Min Fatalities"), fun = min, geom = "point", size = 2) +
  labs(title = "Minimum Weekly Accidents and Fatalities",
       subtitle = "Lowest Recorded Accidents and Fatalities Per Week Across All Years",
       x = "Week of the Year",
       y = "Minimum Count",
       color = "Legend") +
  scale_color_manual(values = c("Min Accidents" = "#0072B2", "Min Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = week)) +
  stat_summary(aes(y = accidents, color = "Mean Accidents"), fun = mean, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Mean Fatalities"), fun = mean, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Mean Accidents"), fun = mean, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Mean Fatalities"), fun = mean, geom = "point", size = 2) +
  labs(title = "Mean Weekly Accidents and Fatalities",
       subtitle = "Average Accidents and Fatalities Per Week Across All Years",
       x = "Week of the Year",
       y = "Mean Count",
       color = "Legend") +
  scale_color_manual(values = c("Mean Accidents" = "#0072B2", "Mean Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
mode_func <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
ggplot(acc.w.ind.y_mod, aes(x = week)) +
  stat_summary(aes(y = accidents, color = "Mode Accidents"), fun = mode_func, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Mode Fatalities"), fun = mode_func, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Mode Accidents"), fun = mode_func, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Mode Fatalities"), fun = mode_func, geom = "point", size = 2) +
  labs(title = "Mode of Weekly Accidents and Fatalities",
       subtitle = "Most Commonly Occurring Weekly Accident and Fatality Counts",
       x = "Week of the Year",
       y = "Mode Count",
       color = "Legend") +
  scale_color_manual(values = c("Mode Accidents" = "#0072B2", "Mode Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = week)) +
  stat_summary(aes(y = accidents, color = "Median Accidents"), fun = median, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Median Fatalities"), fun = median, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Median Accidents"), fun = median, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Median Fatalities"), fun = median, geom = "point", size = 2) +
  labs(title = "Median Weekly Accidents and Fatalities",
       subtitle = "Middle Value of Accidents and Fatalities Per Week Across All Years",
       x = "Week of the Year",
       y = "Median Count",
       color = "Legend") +
  scale_color_manual(values = c("Median Accidents" = "#0072B2", "Median Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
range_func <- function(x) { max(x, na.rm = TRUE) - min(x, na.rm = TRUE) }
ggplot(acc.w.ind.y_mod, aes(x = week)) +
  stat_summary(aes(y = accidents, color = "Range Accidents"), fun = range_func, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Range Fatalities"), fun = range_func, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Range Accidents"), fun = range_func, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Range Fatalities"), fun = range_func, geom = "point", size = 2) +
  labs(title = "Range of Weekly Accidents and Fatalities",
       subtitle = "Difference Between Maximum and Minimum Accidents and Fatalities Per Week",
       x = "Week of the Year",
       y = "Range",
       color = "Legend") +
  scale_color_manual(values = c("Range Accidents" = "#0072B2", "Range Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Scatter Plot of Accidents and Fatalities through the Weeks#

ggplot(acc.w.ind.y_mod, aes(x = accidents, y = fatalities)) +
  geom_point(alpha = 0.5, color = "#0072B2") +  
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +  
  labs(title = "Scatter Plot of Weekly Accidents vs. Fatalities",
       subtitle = "Examining the Relationship Between Weekly Accidents and Fatalities",
       x = "Weekly Accidents",
       y = "Weekly Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Plots of Sums of Accidents and Fatalities through the Weeks#

ggplot(acc.w.ind.y_mod, aes(x = year)) +
  stat_summary(aes(y = accidents), fun = sum, geom = "line", color = "#0072B2", size = 1.2) +
  stat_summary(aes(y = accidents), fun = sum, geom = "point", color = "#0072B2", size = 2) +
  labs(title = "Total Yearly Road Accidents",
       subtitle = "Summed Weekly Accidents for Each Year",
       x = "Year",
       y = "Total Accidents") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = year)) +
  stat_summary(aes(y = fatalities), fun = sum, geom = "line", color = "#D55E00", size = 1.2) +
  stat_summary(aes(y = fatalities), fun = sum, geom = "point", color = "#D55E00", size = 2) +
  labs(title = "Total Yearly Road Fatalities",
       subtitle = "Summed Weekly Fatalities for Each Year",
       x = "Year",
       y = "Total Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Time Series Plots of Rolling Averages, Rolling Sums and Lagged Values of Weekly Accidents#

ggplot(acc.w.ind.y_mod, aes(x = week_date)) +
  geom_line(aes(y = Rolling_Avg_4w_a, color = "4-Week Rolling Avg"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_7w_a, color = "7-Week Rolling Avg"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_12w_a, color = "12-Week Rolling Avg"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_24w_a, color = "24-Week Rolling Avg"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_52w_a, color = "52-Week Rolling Avg"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Averages of Weekly Accidents",
       subtitle = "4, 7, 12, 24, and 52 Week Moving Averages",
       x = "Week",
       y = "Rolling Average",
       color = "Legend") +
  scale_color_manual(values = c("4-Week Rolling Avg" = "#0072B2", 
                                "7-Week Rolling Avg" = "#E69F00",
                                "12-Week Rolling Avg" = "#D55E00",
                                "24-Week Rolling Avg" = "#009E73",
                                "52-Week Rolling Avg" = "#CC79A7")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = week_date)) +
  geom_line(aes(y = RollingSum_4w_a, color = "4-Week Rolling Sum"), size = 1, alpha = 0.8) +
  geom_line(aes(y = RollingSum_8w_a, color = "8-Week Rolling Sum"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = RollingSum_12w_a, color = "12-Week Rolling Sum"), size = 1, alpha = 0.8) +
  geom_line(aes(y = RollingSum_24w_a, color = "24-Week Rolling Sum"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Sums of Weekly Accidents",
       subtitle = "4, 8, 12 and 24 Week Rolling Sums",
       x = "Week",
       y = "Rolling Sum",
       color = "Legend") +
  scale_color_manual(values = c("4-Week Rolling Sum" = "#0072B2", 
                                "8-Week Rolling Sum" = "#E69F00",
                                "12-Week Rolling Sum" = "#D55E00",
                                "24-Week Rolling Sum" = "#FFC0CB"))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = week_date)) +
  geom_line(aes(y = Accidents_Lag1, color = "Lag 1 Week"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag2, color = "Lag 2 Weeks"), size = 1, alpha = 0.6) +  
  geom_line(aes(y = Accidents_Lag3, color = "Lag 3 Weeks"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag4, color = "Lag 4 Weeks"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag6, color = "Lag 6 Weeks"), size = 1, alpha = 0.6, linetype = "dashed") +
  geom_line(aes(y = Accidents_Lag7, color = "Lag 7 Weeks"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag8, color = "Lag 8 Weeks"), size = 1, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag12, color = "Lag 12 Weeks"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag24, color = "Lag 24 Weeks"), size = 1, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag52, color = "Lag 52 Weeks"), size = 1, alpha = 0.6) +
  labs(title = "Lagged Weekly Accidents",
       subtitle = "Comparing Different Lag Periods",
       x = "Week",
       y = "Lagged Accidents",
       color = "Legend") +
  scale_color_manual(values = c("Lag 1 Week" = "#0072B2", 
                                "Lag 2 Weeks" = "#E69F00",
                                "Lag 3 Weeks" = "#44cb2f",
                                "Lag 4 Weeks" = "#173cc4",
                                "Lag 6 Weeks" = "#dc6bcb",
                                "Lag 7 Weeks" = "#c1260d",
                                "Lag 8 Weeks" = "#D55E00",
                                "Lag 12 Weeks" = "#009E73",
                                "Lag 24 Weeks" = "#CC79A7",
                                "Lag 52 Weeks" = "#F0E442")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Time Series Plots of Rolling Averages, Rolling Sums and Lagged Values of Weekly Fatalities#

ggplot(acc.w.ind.y_mod, aes(x = week_date)) +
  geom_line(aes(y = Rolling_Avg_4w_f, color = "4-Week Rolling Avg"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_7w_f, color = "7-Week Rolling Avg"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_12w_f, color = "12-Week Rolling Avg"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_24w_f, color = "24-Week Rolling Avg"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_52w_f, color = "52-Week Rolling Avg"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Averages of Weekly Fatalities",
       subtitle = "4, 7, 12, 24, and 52 Week Moving Averages",
       x = "Week",
       y = "Rolling Average",
       color = "Legend") +
  scale_color_manual(values = c("4-Week Rolling Avg" = "#0072B2", 
                                "7-Week Rolling Avg" = "#E69F00",
                                "12-Week Rolling Avg" = "#D55E00",
                                "24-Week Rolling Avg" = "#009E73",
                                "52-Week Rolling Avg" = "#CC79A7")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = week_date)) +
  geom_line(aes(y = RollingSum_4w_f, color = "4-Week Rolling Sum"), size = 1, alpha = 0.8) +
  geom_line(aes(y = RollingSum_8w_f, color = "8-Week Rolling Sum"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = RollingSum_12w_f, color = "12-Week Rolling Sum"), size = 1, alpha = 0.8) +
  geom_line(aes(y = RollingSum_24w_f, color = "24-Week Rolling Sum"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Sums of Weekly Fatalities",
       subtitle = "4, 8, 12 and 24 Week Rolling Sums",
       x = "Week",
       y = "Rolling Sum",
       color = "Legend") +
  scale_color_manual(values = c("4-Week Rolling Sum" = "#0072B2", 
                                "8-Week Rolling Sum" = "#E69F00",
                                "12-Week Rolling Sum" = "#D55E00",
                                "24-Week Rolling Sum" = "#FFC0CB"))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.w.ind.y_mod, aes(x = week_date)) +
  geom_line(aes(y = Fatalities_Lag1, color = "Lag 1 Week"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag2, color = "Lag 2 Weeks"), size = 1, alpha = 0.6) +  
  geom_line(aes(y = Fatalities_Lag3, color = "Lag 3 Weeks"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag4, color = "Lag 4 Weeks"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag6, color = "Lag 6 Weeks"), size = 1, alpha = 0.6, linetype = "dashed") +
  geom_line(aes(y = Fatalities_Lag7, color = "Lag 7 Weeks"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag8, color = "Lag 8 Weeks"), size = 1, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag12, color = "Lag 12 Weeks"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag24, color = "Lag 24 Weeks"), size = 1, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag52, color = "Lag 52 Weeks"), size = 1, alpha = 0.6) +
  labs(title = "Lagged Weekly Fatalities",
       subtitle = "Comparing Different Lag Periods",
       x = "Week",
       y = "Lagged Fatalities",
       color = "Legend") +
  scale_color_manual(values = c("Lag 1 Week" = "#0072B2", 
                                "Lag 2 Weeks" = "#E69F00",
                                "Lag 3 Weeks" = "#44cb2f",
                                "Lag 4 Weeks" = "#173cc4",
                                "Lag 6 Weeks" = "#dc6bcb",
                                "Lag 7 Weeks" = "#c1260d",
                                "Lag 8 Weeks" = "#D55E00",
                                "Lag 12 Weeks" = "#009E73",
                                "Lag 24 Weeks" = "#CC79A7",
                                "Lag 52 Weeks" = "#F0E442")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Max, Mean, Min, Mode, Median and Range of Accidents Per Season Per Week through the Years#

ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = season)) +
  stat_summary(fun = max, geom = "line", size = 1.2) +
  labs(title = "Maximum Weekly Accidents Per Season",
       subtitle = "Highest Recorded Weekly Accidents Across Different Seasons",
       x = "Week of the Year",
       y = "Maximum Weekly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = season)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Mean Weekly Accidents Per Season",
       subtitle = "Average Weekly Accidents Across Different Seasons",
       x = "Week of the Year",
       y = "Mean Weekly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = season)) +
  stat_summary(fun = min, geom = "line", size = 1.2) +
  labs(title = "Minimum Weekly Accidents Per Season",
       subtitle = "Lowest Recorded Weekly Accidents Across Different Seasons",
       x = "Week of the Year",
       y = "Minimum Weekly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = season)) +
  stat_summary(fun = mode_func, geom = "line", size = 1.2) +
  labs(title = "Mode of Weekly Accidents Per Season",
       subtitle = "Most Frequent Weekly Accidents Across Different Seasons",
       x = "Week of the Year",
       y = "Mode of Weekly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = season)) +
  stat_summary(fun = median, geom = "line", size = 1.2) +
  labs(title = "Median Weekly Accidents Per Season",
       subtitle = "Middle Value of Weekly Accidents Across Different Seasons",
       x = "Week of the Year",
       y = "Median Weekly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = season)) +
  stat_summary(fun = range_func, geom = "line", size = 1.2) +
  labs(title = "Range of Weekly Accidents Per Season",
       subtitle = "Difference Between Max and Min Accidents Per Week Across Different Seasons",
       x = "Week of the Year",
       y = "Range of Weekly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Max, Mean, Min, Mode, Median and Range of Fatalities Per Season Per Week through the Years#

ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = season)) +
  stat_summary(fun = max, geom = "line", size = 1.2) +
  labs(title = "Maximum Weekly Fatalities Per Season",
       subtitle = "Highest Recorded Weekly Fatalities Across Different Seasons",
       x = "Week of the Year",
       y = "Maximum Weekly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = season)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Mean Weekly Fatalities Per Season",
       subtitle = "Average Weekly Fatalities Across Different Seasons",
       x = "Week of the Year",
       y = "Mean Weekly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = season)) +
  stat_summary(fun = min, geom = "line", size = 1.2) +
  labs(title = "Minimum Weekly Fatalities Per Season",
       subtitle = "Lowest Recorded Weekly Fatalities Across Different Seasons",
       x = "Week of the Year",
       y = "Minimum Weekly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = season)) +
  stat_summary(fun = mode_func, geom = "line", size = 1.2) +
  labs(title = "Mode of Weekly Fatalities Per Season",
       subtitle = "Most Frequent Weekly Fatalities Across Different Seasons",
       x = "Week of the Year",
       y = "Mode of Weekly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = season)) +
  stat_summary(fun = median, geom = "line", size = 1.2) +
  labs(title = "Median Weekly Fatalities Per Season",
       subtitle = "Middle Value of Weekly Fatalities Across Different Seasons",
       x = "Week of the Year",
       y = "Median Weekly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = season)) +
  stat_summary(fun = range_func, geom = "line", size = 1.2) +
  labs(title = "Range of Weekly Fatalities Per Season",
       subtitle = "Difference Between Max and Min Fatalities Per Week Across Different Seasons",
       x = "Week of the Year",
       y = "Range of Weekly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Max, Mean, Min, Mode, Median and Range of Accidents Per Quarter Per Week through the Years#

ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(quarter))) +
  stat_summary(fun = max, geom = "line", size = 1.2) +
  labs(title = "Maximum Weekly Accidents Per Quarter",
       subtitle = "Highest Recorded Weekly Accidents Per Quarter",
       x = "Week of the Year",
       y = "Max Weekly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(quarter))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Mean Weekly Accidents Per Quarter",
       subtitle = "Average Weekly Accidents Per Quarter",
       x = "Week of the Year",
       y = "Mean Weekly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(quarter))) +
  stat_summary(fun = min, geom = "line", size = 1.2) +
  labs(title = "Minimum Weekly Accidents Per Quarter",
       subtitle = "Lowest Recorded Weekly Accidents Per Quarter",
       x = "Week of the Year",
       y = "Min Weekly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(quarter))) +
  stat_summary(fun = mode_func, geom = "line", size = 1.2) +
  labs(title = "Mode of Weekly Accidents Per Quarter",
       subtitle = "Most Frequent Weekly Accidents Per Quarter",
       x = "Week of the Year",
       y = "Mode of Weekly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(quarter))) +
  stat_summary(fun = median, geom = "line", size = 1.2) +
  labs(title = "Median Weekly Accidents Per Quarter",
       subtitle = "Middle Value of Weekly Accidents Per Quarter",
       x = "Week of the Year",
       y = "Median Weekly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(quarter))) +
  stat_summary(fun = range_func, geom = "line", size = 1.2) +
  labs(title = "Range of Weekly Accidents Per Quarter",
       subtitle = "Difference Between Max and Min Weekly Accidents Per Quarter",
       x = "Week of the Year",
       y = "Range of Weekly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Max, Mean, Min, Mode, Median and Range of Fatalities Per Quarter Per Week through the Years#

ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(quarter))) +
  stat_summary(fun = max, geom = "line", size = 1.2) +
  labs(title = "Maximum Weekly Fatalities Per Quarter",
       subtitle = "Highest Recorded Weekly Fatalities Per Quarter",
       x = "Week of the Year",
       y = "Max Weekly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(quarter))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Mean Weekly Fatalities Per Quarter",
       subtitle = "Average Weekly Fatalities Per Quarter",
       x = "Week of the Year",
       y = "Mean Weekly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(quarter))) +
  stat_summary(fun = min, geom = "line", size = 1.2) +
  labs(title = "Minimum Weekly Fatalities Per Quarter",
       subtitle = "Lowest Recorded Weekly Fatalities Per Quarter",
       x = "Week of the Year",
       y = "Min Weekly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(quarter))) +
  stat_summary(fun = mode_func, geom = "line", size = 1.2) +
  labs(title = "Mode of Weekly Fatalities Per Quarter",
       subtitle = "Most Frequent Weekly Fatalities Per Quarter",
       x = "Week of the Year",
       y = "Mode of Weekly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(quarter))) +
  stat_summary(fun = median, geom = "line", size = 1.2) +
  labs(title = "Median Weekly Fatalities Per Quarter",
       subtitle = "Middle Value of Weekly Fatalities Per Quarter",
       x = "Week of the Year",
       y = "Median Weekly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(quarter))) +
  stat_summary(fun = range_func, geom = "line", size = 1.2) +
  labs(title = "Range of Weekly Fatalities Per Quarter",
       subtitle = "Difference Between Max and Min Weekly Fatalities Per Quarter",
       x = "Week of the Year",
       y = "Range of Weekly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Plots of Accidents Per Week when it is Summer, Winter, Holidays vs NOT#

ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(is_summer))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Weekly Accidents: Summer (1 vs 0)",
       subtitle = "Comparison of Weekly Accidents When is_summer is 1 vs 0",
       x = "Week of the Year",
       y = "Mean Weekly Accidents",
       color = "Summer Indicator") +
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#E69F00"),
                     labels = c("Not Summer", "Summer")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(is_winter))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Weekly Accidents: Winter (1 vs 0)",
       subtitle = "Comparison of Weekly Accidents When is_winter is 1 vs 0",
       x = "Week of the Year",
       y = "Mean Weekly Accidents",
       color = "Winter Indicator") +
  scale_color_manual(values = c("0" = "#D55E00", "1" = "#0072B2"),
                     labels = c("Not Winter", "Winter")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = accidents, color = as.factor(is_holiday))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Weekly Accidents: Holiday (1 vs 0)",
       subtitle = "Comparison of Weekly Accidents When is_holiday is 1 vs 0",
       x = "Week of the Year",
       y = "Mean Weekly Accidents",
       color = "Holiday Indicator") +
  scale_color_manual(values = c("0" = "#009E73", "1" = "#CC79A7"),
                     labels = c("Not Holiday", "Holiday")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Plots of Fatalities Per Week when it is Summer, Winter, Holidays vs NOT#

ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(is_summer))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Weekly Fatalities: Summer (1 vs 0)",
       subtitle = "Comparison of Weekly Fatalities When is_summer is 1 vs 0",
       x = "Week of the Year",
       y = "Mean Weekly Fatalities",
       color = "Summer Indicator") +
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#E69F00"),
                     labels = c("Not Summer", "Summer")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(is_winter))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Weekly Fatalities: Winter (1 vs 0)",
       subtitle = "Comparison of Weekly Fatalities When is_winter is 1 vs 0",
       x = "Week of the Year",
       y = "Mean Weekly Fatalities",
       color = "Winter Indicator") +
  scale_color_manual(values = c("0" = "#D55E00", "1" = "#0072B2"),
                     labels = c("Not Winter", "Winter")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.w.ind.y_mod, aes(x = week, y = fatalities, color = as.factor(is_holiday))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Weekly Fatalities: Holiday (1 vs 0)",
       subtitle = "Comparison of Weekly Fatalities When is_holiday is 1 vs 0",
       x = "Week of the Year",
       y = "Mean Weekly Fatalities",
       color = "Holiday Indicator") +
  scale_color_manual(values = c("0" = "#009E73", "1" = "#CC79A7"),
                     labels = c("Not Holiday", "Holiday")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Plots of Weekly Accident Growth Rate, Fatality Growth Rate, Fatality Rate, Accidents Change Rate, Fatalities Change Rate, Fatalities Per Capita, Accidents Per Vehicle#

ggplot(acc.w.ind.y_mod, aes(x = week_date, y = AccidentGrowthRate)) +
  geom_line(color = "#0072B2", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#0072B2", fill = "#0072B2", alpha = 0.2, size = 1.2) +
  labs(title = "Weekly Accident Growth Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Accident Growth Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.w.ind.y_mod, aes(x = week_date, y = FatalityGrowthRate)) +
  geom_line(color = "#D55E00", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#D55E00", fill = "#D55E00", alpha = 0.2, size = 1.2) +
  labs(title = "Weekly Fatality Growth Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Fatality Growth Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.w.ind.y_mod, aes(x = week_date, y = FatalityRate)) +
  geom_line(color = "#009E73", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#009E73", fill = "#009E73", alpha = 0.2, size = 1.2) +
  labs(title = "Weekly Fatality Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Fatality Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.w.ind.y_mod, aes(x = week_date, y = AccidentChangeRate)) +
  geom_line(color = "#E69F00", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E69F00", fill = "#E69F00", alpha = 0.2, size = 1.2) +
  labs(title = "Weekly Accident Change Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Accident Change Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.w.ind.y_mod, aes(x = week_date, y = FatalityChangeRate)) +
  geom_line(color = "#CC79A7", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#CC79A7", fill = "#CC79A7", alpha = 0.2, size = 1.2) +
  labs(title = "Weekly Fatality Change Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Fatality Change Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.w.ind.y_mod, aes(x = week_date, y = FatalitiesPerCapita)) +
  geom_line(color = "#56B4E9", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#56B4E9", fill = "#56B4E9", alpha = 0.2, size = 1.2) +
  labs(title = "Weekly Fatalities Per Capita Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Fatalities Per Capita") +
  theme_minimal(base_size = 14)
ggplot(acc.w.ind.y_mod, aes(x = week_date, y = AccidentsPerVehicle)) +
  geom_line(color = "#009E73", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E69F00", fill = "#E69F00", alpha = 0.2, size = 1.2) +
  labs(title = "Weekly Accidents Per Vehicle Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Accidents Per Vehicle") +
  theme_minimal(base_size = 14)

#Histograms for All Variables#

numeric_data.acc.w.hplots <- acc.w.ind.y_mod %>% select(where(is.numeric))
long_data.acc.w.hplots <- numeric_data.acc.w.hplots %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
var_names.acc.w.hplots <- unique(long_data.acc.w.hplots$Variable)
var_groups.acc.w.hplots <- split(var_names.acc.w.hplots, ceiling(seq_along(var_names.acc.w.hplots) / 8))
plot_histograms <- function(var_subset) {
  ggplot(long_data.acc.w.hplots %>% filter(Variable %in% var_subset), aes(x = Value)) +
    geom_histogram(bins = 50, fill = "#0072B2", color = "black", alpha = 0.7) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) +  
    labs(title = "Histograms of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Value",
         y = "Frequency") +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
histogram_plots.acc.w <- lapply(var_groups.acc.w.hplots, plot_histograms)
histogram_plots.acc.w

#Box Plots for All Variables#

numeric_data.acc.w.bplots <- acc.w.ind.y_mod %>%
  select(week, where(is.numeric)) %>%  
  mutate(week = as.factor(week))  
long_data.acc.w.bplots <- numeric_data.acc.w.bplots %>%
  pivot_longer(cols = -week, names_to = "Variable", values_to = "Value")
var_names.acc.w.bplots <- unique(long_data.acc.w.bplots$Variable)
var_groups.acc.w.bplots <- split(var_names.acc.w.bplots, ceiling(seq_along(var_names.acc.w.bplots) / 8))
plot_boxplots <- function(var_subset) {
  ggplot(long_data.acc.w.bplots %>% filter(Variable %in% var_subset), aes(x = week, y = Value)) +
    geom_boxplot(fill = "#0072B2", color = "black", alpha = 0.6, outlier.shape = NA) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) +  
    labs(title = "Weekly Box Plots of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Week of the Year",
         y = "Value") +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
boxplot_plots.acc.w.bplots <- lapply(var_groups.acc.w.bplots, plot_boxplots)
boxplot_plots.acc.w.bplots

#Plots of Means of All Variables#

numeric_data.acc.w.mplots <- acc.w.ind.y_mod %>%
  select(week, where(is.numeric)) %>%
  mutate(week = as.factor(week))
long_data.acc.w.mplots <- numeric_data.acc.w.mplots %>%
  pivot_longer(cols = -week, names_to = "Variable", values_to = "Value")
var_names.acc.w.mplots <- unique(long_data.acc.w.mplots$Variable)
var_groups.acc.w.mplots <- split(var_names.acc.w.mplots, ceiling(seq_along(var_names.acc.w.mplots) / 8))
plot_means <- function(var_subset) {
  ggplot(long_data.acc.w.mplots %>% filter(Variable %in% var_subset), aes(x = week, y = Value, group = Variable, color = Variable)) +
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) + 
    labs(title = "Weekly Mean of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Week of the Year",
         y = "Mean Value") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
mean_plots.acc.w.mplots <- lapply(var_groups.acc.w.mplots, plot_means)
mean_plots.acc.w.mplots

#Time Series Plots of All variables#

numeric_data.acc.w.tsplots <- acc.w.ind.y_mod %>%
  select(week_date, where(is.numeric)) %>%
  mutate(week_date = as.Date(week_date))
long_data.acc.w.tsplots <- numeric_data.acc.w.tsplots %>%
  pivot_longer(cols = -week_date, names_to = "Variable", values_to = "Value")
var_names.acc.w.tsplots <- unique(long_data.acc.w.tsplots$Variable)
var_groups.acc.w.tsplots <- split(var_names.acc.w.tsplots, ceiling(seq_along(var_names.acc.w.tsplots) / 8))
plot_time_series <- function(var_subset) {
  ggplot(long_data.acc.w.tsplots %>% filter(Variable %in% var_subset), aes(x = week_date, y = Value, color = Variable)) +
    geom_line(size = 0.8, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, size = 1, alpha = 0.5) +  
    facet_wrap(~ Variable, scales = "free", ncol = 4) + 
    labs(title = "Time Series of Numeric Variables Through the Weeks",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Week",
         y = "Value") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
time_series_plots.acc.w.tsplots <- lapply(var_groups.acc.w.tsplots, plot_time_series)
time_series_plots.acc.w.tsplots

#Skewness Plot of Accidents and Fatalities Per Week through the Years#

skewness_data.acc.w <- acc.w.ind.y_mod %>%
  group_by(week) %>%
  summarise(
    Skew_Accidents = skewness(accidents, na.rm = TRUE),
    Skew_Fatalities = skewness(fatalities, na.rm = TRUE)
  )
ggplot(skewness_data.acc.w, aes(x = week)) +
  geom_line(aes(y = Skew_Accidents, color = "Accidents"), size = 1.2) +
  geom_line(aes(y = Skew_Fatalities, color = "Fatalities"), size = 1.2) +
  labs(title = "Weekly Skewness of Accidents and Fatalities",
       subtitle = "Shows Asymmetry in Weekly Data",
       x = "Week",
       y = "Skewness",
       color = "Metric") +
  scale_color_manual(values = c("Accidents" = "#0072B2", "Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14)

#Kurtosis Plot of Accidents and Fatalities Per Week through the Years#

kurtosis_data.acc.w <- acc.w.ind.y_mod %>%
  group_by(week) %>%
  summarise(
    Kurt_Accidents = kurtosis(accidents, na.rm = TRUE),
    Kurt_Fatalities = kurtosis(fatalities, na.rm = TRUE)
  )
ggplot(kurtosis_data.acc.w, aes(x = week)) +
  geom_line(aes(y = Kurt_Accidents, color = "Accidents"), size = 1.2) +
  geom_line(aes(y = Kurt_Fatalities, color = "Fatalities"), size = 1.2) +
  labs(title = "Weekly Kurtosis of Accidents and Fatalities",
       subtitle = "Measures Tail Heaviness of Weekly Data",
       x = "Week",
       y = "Kurtosis",
       color = "Metric") +
  scale_color_manual(values = c("Accidents" = "#0072B2", "Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14)

#Density Plot of Weekly Accidents and Fatalities#

ggplot(acc.w.ind.y_mod) +
  geom_density(aes(x = accidents, fill = "Accidents"), alpha = 0.5, color = "black") +
  geom_density(aes(x = fatalities, fill = "Fatalities"), alpha = 0.5, color = "black") +
  labs(title = "Density Plot of Weekly Accidents and Fatalities",
       subtitle = "Shows the Probability Distribution of Weekly Data",
       x = "Count",
       y = "Density",
       fill = "Metric") +
  scale_fill_manual(values = c("Accidents" = "#0072B2", "Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14)

#Percentiles Plots of Accidents and Fatalities per Week through the Years#

percentile_data.acc.w <- acc.w.ind.y_mod %>%
  group_by(week) %>%
  summarise(
    P10_Accidents = quantile(accidents, probs = 0.10, na.rm = TRUE),
    P25_Accidents = quantile(accidents, probs = 0.25, na.rm = TRUE),
    P50_Accidents = quantile(accidents, probs = 0.50, na.rm = TRUE),  
    P75_Accidents = quantile(accidents, probs = 0.75, na.rm = TRUE),
    P90_Accidents = quantile(accidents, probs = 0.90, na.rm = TRUE),
    P10_Fatalities = quantile(fatalities, probs = 0.10, na.rm = TRUE),
    P25_Fatalities = quantile(fatalities, probs = 0.25, na.rm = TRUE),
    P50_Fatalities = quantile(fatalities, probs = 0.50, na.rm = TRUE),  
    P75_Fatalities = quantile(fatalities, probs = 0.75, na.rm = TRUE),
    P90_Fatalities = quantile(fatalities, probs = 0.90, na.rm = TRUE)
  )
ggplot(percentile_data.acc.w, aes(x = week)) +
  geom_line(aes(y = P10_Accidents, color = "P10 Accidents"), size = 1) +
  geom_line(aes(y = P50_Accidents, color = "P50 Accidents (Median)"), size = 1.2) +
  geom_line(aes(y = P90_Accidents, color = "P90 Accidents"), size = 1) +
  geom_line(aes(y = P10_Fatalities, color = "P10 Fatalities"), size = 1, linetype = "dashed") +
  geom_line(aes(y = P50_Fatalities, color = "P50 Fatalities (Median)"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = P90_Fatalities, color = "P90 Fatalities"), size = 1, linetype = "dashed") +
  labs(title = "Weekly Percentiles of Accidents and Fatalities",
       subtitle = "Comparison of 10th, 50th, and 90th Percentiles Over the Weeks",
       x = "Week",
       y = "Percentile Values",
       color = "Percentile") +
  scale_color_manual(values = c(
    "P10 Accidents" = "#0072B2",
    "P50 Accidents (Median)" = "#005293",
    "P90 Accidents" = "#003366",
    "P10 Fatalities" = "#D55E00",
    "P50 Fatalities (Median)" = "#A53D00",
    "P90 Fatalities" = "#732000"
  )) +
  theme_minimal(base_size = 14)

#CV Plots for All Variables#

numeric_data.acc.w.cvplots <- acc.w.ind.y_mod %>%
  select(week, where(is.numeric)) %>%
  mutate(week = as.factor(week)) 
cv_data.acc.w.cvplots  <- numeric_data.acc.w.cvplots  %>%
  group_by(week) %>%
  summarise(across(everything(), ~ sd(., na.rm = TRUE) / mean(., na.rm = TRUE), .names = "CV_{.col}")) %>%
  pivot_longer(cols = -week, names_to = "Variable", values_to = "CV_Value")
var_names.acc.w.cvplots  <- unique(cv_data.acc.w.cvplots $Variable)
var_groups.acc.w.cvplots  <- split(var_names.acc.w.cvplots , ceiling(seq_along(var_names.acc.w.cvplots ) / 8))
plot_cv <- function(var_subset) {
  ggplot(cv_data.acc.w.cvplots %>% filter(Variable %in% var_subset), aes(x = week, y = CV_Value, group = 1, color = Variable)) +
    geom_line(size = 1.2, alpha = 0.8) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) +  
    labs(title = "Weekly Coefficient of Variation (CV) of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Week",
         y = "CV (Standard Deviation / Mean)") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1) 
    )
}
cv_plots.acc.w.cvplots  <- lapply(var_groups.acc.w.cvplots , plot_cv)
cv_plots.acc.w.cvplots 

#Range and Variance Plots for All Variables#

numeric_data.acc.w.rvplots <- acc.w.ind.y_mod %>%
  select(week, where(is.numeric)) %>%
  mutate(week = as.factor(week))  
range_variance_data.acc.w.rvplots <- numeric_data.acc.w.rvplots %>%
  group_by(week) %>%
  summarise(across(everything(), 
                   list(Range = ~ max(., na.rm = TRUE) - min(., na.rm = TRUE),
                        Variance = ~ var(., na.rm = TRUE)), 
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -week, names_to = "Metric", values_to = "Value")
range_data.acc.w.rvplots <- range_variance_data.acc.w.rvplots %>% filter(grepl("_Range$", Metric))
variance_data.acc.w.rvplots <- range_variance_data.acc.w.rvplots %>% filter(grepl("_Variance$", Metric))
range_vars.acc.w.rvplots <- unique(range_data.acc.w.rvplots$Metric)
variance_vars.acc.w.rvplots <- unique(variance_data.acc.w.rvplots$Metric)
range_groups.acc.w.rvplots <- split(range_vars.acc.w.rvplots, ceiling(seq_along(range_vars.acc.w.rvplots) / 8))
variance_groups.acc.w.rvplots <- split(variance_vars.acc.w.rvplots, ceiling(seq_along(variance_vars.acc.w.rvplots) / 8))
plot_range <- function(var_subset) {
  ggplot(range_data.acc.w.rvplots %>% filter(Metric %in% var_subset), aes(x = week, y = Value, group = 1, color = Metric)) +
    geom_line(size = 1.2, alpha = 0.8) +
    facet_wrap(~ Metric, scales = "free", ncol = 4) +  
    labs(title = "Weekly Range of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Week",
         y = "Range (Max - Min)") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    )
}
plot_variance <- function(var_subset) {
  ggplot(variance_data.acc.w.rvplots %>% filter(Metric %in% var_subset), aes(x = week, y = Value, group = 1, color = Metric)) +
    geom_line(size = 1.2, alpha = 0.8) +
    facet_wrap(~ Metric, scales = "free", ncol = 4) + 
    labs(title = "Weekly Variance of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Week",
         y = "Variance") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    )
}
range_plots.acc.w.rvplots <- lapply(range_groups.acc.w.rvplots, plot_range)
variance_plots.acc.w.rvplots <- lapply(variance_groups.acc.w.rvplots, plot_variance)
range_plots.acc.w.rvplots
variance_plots.acc.w.rvplots

#Simple and Multiple Linear Regression Plots#

simple_lm.acc.w <- lm(accidents ~ vehicles, data = acc.w.ind.y_mod)
ggplot(acc.w.ind.y_mod, aes(x = vehicles, y = accidents)) +
  geom_point(alpha = 0.5, color = "#0072B2") +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.3) +  # Regression line with confidence interval
  labs(title = "Simple Linear Regression: Accidents vs Vehicles",
       subtitle = "Fitted Line with Confidence Interval",
       x = "Number of Vehicles",
       y = "Number of Accidents") +
  theme_minimal(base_size = 14)
plot_multi_lm <- function(var) {
  ggplot(acc.w.ind.y_mod, aes_string(x = var, y = "accidents")) +
    geom_point(alpha = 0.5, color = "#D55E00") +  # Scatter points
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.3) +  # Regression line with confidence interval
    labs(title = paste("Multiple Regression: Accidents vs", var),
         subtitle = "Fitted Line with Confidence Interval",
         x = var,
         y = "Number of Accidents") +
    theme_minimal(base_size = 14)
}
plot_vehicles.acc.w <- plot_multi_lm("vehicles")
plot_unemployment.acc.w <- plot_multi_lm("unemployment")
plot_inflation.acc.w <- plot_multi_lm("inflation_rate")
plot_vehicles.acc.w
plot_unemployment.acc.w
plot_inflation.acc.w
simple_lm.acc.w <- lm(accidents ~ vehicles, data = acc.w.ind.y_mod)
multi_lm.acc.w <- lm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.w.ind.y_mod)
plot_diagnostics.lm <- function(model, model_name) {
  diagnostics <- fortify(model)
  p1 <- ggplot(diagnostics, aes(.fitted, .resid)) +
    geom_point(color = "#0072B2", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste(model_name, "- Residuals vs Fitted"),
         x = "Fitted Values",
         y = "Residuals") +
    theme_minimal()
  p2 <- ggplot(diagnostics, aes(sample = .stdresid)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste(model_name, "- Q-Q Plot"),
         x = "Theoretical Quantiles",
         y = "Standardized Residuals") +
    theme_minimal()
  p3 <- ggplot(diagnostics, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point(color = "#E69F00", alpha = 0.5) +
    geom_smooth(method = "loess", color = "red") +
    labs(title = paste(model_name, "- Scale-Location Plot"),
         x = "Fitted Values",
         y = "√|Standardized Residuals|") +
    theme_minimal()
  p4 <- ggplot(diagnostics, aes(.hat, .stdresid)) +
    geom_point(color = "#D55E00", alpha = 0.5) +
    geom_smooth(method = "loess", color = "red") +
    labs(title = paste(model_name, "- Residuals vs Leverage"),
         x = "Leverage",
         y = "Standardized Residuals") +
    theme_minimal()
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}
plot_diagnostics.lm(simple_lm.acc.w, "Simple LM (Accidents ~ Vehicles)")
plot_diagnostics.lm(multi_lm.acc.w, "Multiple LM (Accidents ~ Vehicles + Unemployment + Inflation)")

#Correlation Matrices#

cor_data.acc.w.cm.short<- acc.w.ind.y_mod %>%
  select(year, month, week, accidents, fatalities, total_gdp, gdp_per_capita, unemployment, 
         population, vehicles, inflation_rate, week_date, quarter, 
         season, is_summer, is_winter, is_holiday, AccidentGrowthRate, FatalityGrowthRate, FatalityRate,
         AccidentChangeRate, FatalityChangeRate, AccidentsPerCapita, FatalitiesPerCapita, AccidentsPerVehicle, AccidentsUnemployment, FatalitiesInflation,
         CumulativeAccidents, CumulativeFatalities, season_numeric)
numeric_data.acc.w.cm.short <- cor_data.acc.w.cm.short %>% select(where(is.numeric))
cor_pearson.acc.w.cm.short <- cor(numeric_data.acc.w.cm.short, use = "pairwise.complete.obs", method = "pearson")
cor_spearman.acc.w.cm.short <- cor(numeric_data.acc.w.cm.short, use = "pairwise.complete.obs", method = "spearman")
cor_kendall.acc.w.cm.short <- cor(numeric_data.acc.w.cm.short, use = "pairwise.complete.obs", method = "kendall")
cov_matrix.acc.w.cm.short <- cov(numeric_data.acc.w.cm.short, use = "pairwise.complete.obs")
melted_cor_pearson.acc.w.cm.short <- melt(cor_pearson.acc.w.cm.short)
ggplot(melted_cor_pearson.acc.w.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Pearson Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cor_spearman.acc.w.cm.short <- melt(cor_spearman.acc.w.cm.short)
ggplot(melted_cor_spearman.acc.w.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Spearman Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cor_kendall.acc.w.cm.short <- melt(cor_kendall.acc.w.cm.short)
ggplot(melted_cor_kendall.acc.w.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Kendall Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cov.acc.w.cm.short <- melt(cov_matrix.acc.w.cm.short)
ggplot(melted_cov.acc.w.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Covariance Matrix (Accidents Data)", x = "", y = "", fill = "Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
numeric_data.acc.w.cm <- acc.w.ind.y_mod %>%
  select(where(is.numeric))
cor_pearson.acc.w.cm  <- cor(numeric_data.acc.w.cm , use = "pairwise.complete.obs", method = "pearson")
cor_spearman.acc.w.cm  <- cor(numeric_data.acc.w.cm , use = "pairwise.complete.obs", method = "spearman")
cor_kendall.acc.w.cm  <- cor(numeric_data.acc.w.cm , use = "pairwise.complete.obs", method = "kendall")
cov_matrix.acc.w.cm  <- cov(numeric_data.acc.w.cm , use = "pairwise.complete.obs")
melted_cor_pearson.acc.w.cm  <- melt(cor_pearson.acc.w.cm )
melted_cor_spearman.acc.w.cm  <- melt(cor_spearman.acc.w.cm )
melted_cor_kendall.acc.w.cm  <- melt(cor_kendall.acc.w.cm )
melted_cov.acc.w.cm  <- melt(cov_matrix.acc.w.cm )
plot_heatmap <- function(data, title) {
  ggplot(data, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab") +
    labs(title = title, x = "", y = "", fill = "Value") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}
plot_heatmap(melted_cor_pearson.acc.w.cm , "Pearson Correlation Matrix")
plot_heatmap(melted_cor_spearman.acc.w.cm , "Spearman Correlation Matrix")
plot_heatmap(melted_cor_kendall.acc.w.cm , "Kendall Correlation Matrix")
plot_heatmap(melted_cov.acc.w.cm , "Covariance Matrix")

#Shapiro Wilk and Kolmogorov-Smirnov Test#

normality_data.acc.w.swplot <- data.frame(
  Variable = names(shapiro_results.acc.w),
  Shapiro_P = shapiro_results.acc.w,
  KS_P = ks_results.acc.w
) %>%
  pivot_longer(cols = -Variable, names_to = "Test", values_to = "P_Value")
ggplot(normality_data.acc.w.swplot, aes(x = Test, y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Shapiro-Wilk & KS Test P-Values",
       x = "Test",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14)

#Boxplot of Two Sample T test#

ggplot(acc.w.ind.y_mod, aes(x = as.factor(is_summer), y = accidents, fill = as.factor(is_summer))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Two-Sample T-Test: Accidents in Summer vs Non-Summer",
       x = "Summer (1 = Yes, 0 = No)",
       y = "Number of Accidents") +
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00")) +
  theme_minimal(base_size = 14)

#Density Plot of One Sample T Test#

ggplot(acc.w.ind.y_mod, aes(x = accidents)) +
  geom_density(fill = "#0072B2", alpha = 0.6, color = "black") +
  geom_vline(xintercept = 200, color = "red", linetype = "dashed", size = 1.2) +
  labs(title = "One-Sample T-Test: Accidents Distribution",
       subtitle = "Dashed Line = Mean Hypothesis (200)",
       x = "Number of Accidents",
       y = "Density") +
  theme_minimal(base_size = 14)

#Boxplot of Wilcoxon Test#

ggplot(acc.w.ind.y_mod, aes(x = "Accidents vs Fatalities", y = accidents - fatalities)) +
  geom_boxplot(fill = "#E69F00", alpha = 0.6) +
  labs(title = "Wilcoxon Test: Accidents vs Fatalities",
       y = "Difference (Accidents - Fatalities)") +
  theme_minimal(base_size = 14)

#Boxplot of Kruskal Wallis Test#

ggplot(acc.w.ind.y_mod, aes(x = as.factor(season_numeric), y = accidents, fill = as.factor(season_numeric))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Kruskal-Wallis Test: Accidents by Season",
       x = "Season",
       y = "Number of Accidents") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#E69F00")) +
  theme_minimal(base_size = 14)

#Heatmap of Fisher's Exact Test#

fisher_data.acc.w <- as.data.frame(table(acc.w.ind.y_mod$is_summer, acc.w.ind.y_mod$is_holiday))
ggplot(fisher_data.acc.w, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Fisher's Exact Test Heatmap",
       x = "Summer (1 = Yes, 0 = No)",
       y = "Holiday (1 = Yes, 0 = No)",
       fill = "Frequency") +
  theme_minimal(base_size = 14)

#Turkey Post Hoc Test Results#

tukey_results.acc.w <- TukeyHSD(anova_test.acc.w)
if (!is.null(tukey_results.acc.w$`as.factor(season_numeric)`)) {
  tukey_df.acc.w <- as.data.frame(tukey_results.acc.w$`as.factor(season_numeric)`)
} else {
  stop("TukeyHSD test did not return valid results. Check ANOVA model.")
}
if (!is.null(tukey_results.acc.w$`as.factor(season_numeric)`)) {
  tukey_df.acc.w <- as.data.frame(tukey_results.acc.w$`as.factor(season_numeric)`)
  colnames(tukey_df.acc.w) <- c("Diff", "Lwr", "Upr", "P_adj") 
  tukey_df.acc.w$Comparison <- rownames(tukey_df.acc.w)  
  ggplot(tukey_df.acc.w, aes(x = Comparison, y = Diff, fill = Comparison)) +
    geom_col(alpha = 0.7) +
    geom_errorbar(aes(ymin = Lwr, ymax = Upr), width = 0.2) +
    labs(title = "Tukey HSD Post-hoc Test Results",
         x = "Season Comparisons",
         y = "Mean Difference") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
} else {
  print("Error: TukeyHSD results are NULL. Check ANOVA model before running TukeyHSD.")
}

#Dunn Test Hetmap Results#

str(dunn_table.acc.w)
if (!"P.adj" %in% colnames(dunn_table.acc.w)) {
  print("Error: 'P.adjusted' column not found in Dunn test results. Check column names with str(dunn_table.acc.w).")
} else {
  dunn_df.acc.w <- dunn_table.acc.w
  ggplot(dunn_df.acc.w, aes(x = Comparison, y = "Dunn Test", fill = P.adj)) +
    geom_tile(color = "black") +
    geom_text(aes(label = round(P.adj, 3)), color = "white", size = 5) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Dunn Post-hoc Test Heatmap",
         x = "Comparison",
         y = "",
         fill = "Adjusted P-Value") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#Pairwise Wilcoxon Test Results Heatmap#

pairwise_wilcox_df.acc.w <- as.data.frame(as.table(as.matrix(pairwise.wilcox.test.acc.w$p.value)))
colnames(pairwise_wilcox_df.acc.w) <- c("Group1", "Group2", "P_Value")
pairwise_wilcox_df.acc.w <- na.omit(pairwise_wilcox_df.acc.w)
ggplot(pairwise_wilcox_df.acc.w, aes(x = Group1, y = Group2, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Pairwise Wilcoxon Test Heatmap",
       x = "Group 1",
       y = "Group 2",
       fill = "P-Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Scree Plot and Biplot#

fviz_eig(pca_model.acc.w, addlabels = TRUE, ylim = c(0, 100)) +
  labs(title = "PCA Scree Plot",
       x = "Principal Components",
       y = "Explained Variance (%)")
fviz_pca_biplot(pca_model.acc.w, repel = TRUE) +
  labs(title = "PCA Biplot",
       subtitle = "Projection of Variables and Observations")

#K-Means Clustering Plot#

fviz_cluster(kmeans_result.acc.w, data = numeric_df_clean.acc.w, stand = FALSE) +
  labs(title = "K-Means Clustering Visualization",
       subtitle = "3 Cluster Solution")

#Outlier Detection Plot#

ggplot(acc.w.ind.y_mod, aes(x = z_score)) +
  geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "black", alpha = 0.7) +
  geom_vline(xintercept = c(-3, 3), linetype = "dashed", color = "red", size = 1) +
  labs(title = "Outlier Detection using Z-Score",
       x = "Z-Score",
       y = "Frequency") +
  theme_minimal(base_size = 14)

#Random Forest Feature Importance Plot#

varImpPlot(rf_model.acc.w, main = "Random Forest Feature Importance")

#Boxplots of Kruskal Wallis tests#

plot_kruskal <- function(var, var_name) {
  ggplot(acc.w.ind.y_mod, aes(x = as.factor(cut(!!sym(var), breaks = 3)), y = accidents, fill = as.factor(cut(!!sym(var), breaks = 3)))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Kruskal-Wallis Test: Accidents by", var_name),
         x = var_name,
         y = "Number of Accidents") +
    scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_gdp.acc.w <- plot_kruskal("gdp_per_capita", "GDP Per Capita")
plot_unemployment.acc.w <- plot_kruskal("unemployment", "Unemployment Rate")
plot_gdp.acc.w
plot_unemployment.acc.w
kruskal_results.acc.w <- data.frame(
  Variable = c("GDP Per Capita", "Unemployment"),
  P_Value = c(kruskal_gdp.acc.w$p.value,
              kruskal_unemployment.acc.w$p.value)
)
ggplot(kruskal_results.acc.w, aes(x = "", y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Kruskal-Wallis Test p-Values",
       x = "",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())

#Time Series Indicating Plots and Poisson Model#

adf_results.acc.w <- data.frame(
  Variable = c("Accidents", "Fatalities"),
  P_Value = c(adf_test_acc.w$p.value, adf_test_fatal.w$p.value)
)
ggplot(adf_results.acc.w, aes(x = "", y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "ADF Test Results (Stationarity Check)",
       x = "",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())
acc.w.ind.y_mod$predicted_poisson <- predict(poisson_model.acc.w, type = "response")
ggplot(acc.w.ind.y_mod, aes(x = predicted_poisson, y = accidents)) +
  geom_point(alpha = 0.5, color = "#0072B2") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Poisson Regression: Predicted vs Actual Accidents",
       x = "Predicted Accidents",
       y = "Actual Accidents") +
  theme_minimal(base_size = 14)
residuals_poisson.acc.w <- data.frame(
  Fitted = poisson_model.acc.w$fitted.values,
  Residuals = residuals(poisson_model.acc.w, type = "pearson")
)
ggplot(residuals_poisson.acc.w, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "#D55E00") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Poisson Regression: Residuals vs Fitted",
       x = "Fitted Values",
       y = "Pearson Residuals") +
  theme_minimal(base_size = 14)
acf_plot <- ggAcf(acc.w.ind.y_mod$accidents, lag.max = 30) +
  ggtitle("Autocorrelation Function (ACF) of Accidents") +
  theme_minimal(base_size = 14)
acf_plot
pacf_plot <- ggPacf(acc.w.ind.y_mod$accidents, lag.max = 30) +
  ggtitle("Partial Autocorrelation Function (PACF) of Accidents") +
  theme_minimal(base_size = 14)
pacf_plot

##Monthly Accidents and Yearly Indicators Dataset##


#Time Series Plots of Accidents and Fatalities through the Months#

ggplot(acc.m.ind.y_mod, aes(x = month_date, y = accidents)) +
  geom_line(color = "#0072B2", size = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#0072B2", linetype = "solid") +
  labs(title = "Monthly Road Accidents in Greece",
       subtitle = "Trends in Monthly Accidents Over the Years",
       x = "Year",
       y = "Accidents") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = month_date, y = fatalities)) +
  geom_line(color = "#D55E00", size = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#D55E00", linetype = "solid") +
  labs(title = "Monthly Road Fatalities in Greece",
       subtitle = "Trends in Monthly Fatalities Over the Years",
       x = "Year",
       y = "Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Box Plots of Accidents and Fatalities through the Months#

ggplot(acc.m.ind.y_mod, aes(x = as.factor(month), y = accidents, fill = "Accidents")) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  stat_summary(aes(color = "Accidents"), fun = mean, geom = "point", size = 2.5, shape = 21, fill = "white") +
  labs(title = "Monthly Distribution of Accidents",
       subtitle = "Box Plot Showing the Spread of Monthly Accidents",
       x = "Month",
       y = "Accidents") +
  scale_fill_manual(values = c("Accidents" = "#0072B2")) +
  scale_color_manual(values = c("Accidents" = "#0072B2")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = as.factor(month), y = fatalities, fill = "Fatalities")) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  stat_summary(aes(color = "Fatalities"), fun = mean, geom = "point", size = 2.5, shape = 21, fill = "white") +
  labs(title = "Monthly Distribution of Fatalities",
       subtitle = "Box Plot Showing the Spread of Monthly Fatalities",
       x = "Month",
       y = "Fatalities") +
  scale_fill_manual(values = c("Fatalities" = "#D55E00")) +
  scale_color_manual(values = c("Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Histograms of Accidents  and Fatalities Count through the Months#

ggplot(acc.m.ind.y_mod, aes(x = accidents)) +
  geom_histogram(bins = 50, fill = "#0072B2", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Monthly Road Accidents",
       subtitle = "Distribution of Monthly Accident Counts",
       x = "Monthly Accidents",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = fatalities)) +
  geom_histogram(bins = 50, fill = "#D55E00", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Monthly Road Fatalities",
       subtitle = "Distribution of Monthly Fatality Counts",
       x = "Monthly Fatalities",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Max Min Mean Mode Median and Range of Accidents and Fatalities per Month through the Years#

ggplot(acc.m.ind.y_mod, aes(x = month)) +
  stat_summary(aes(y = accidents, color = "Max Accidents", group = 1), fun = max, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Max Fatalities", group = 1), fun = max, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Max Accidents"), fun = max, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Max Fatalities"), fun = max, geom = "point", size = 2) +
  labs(title = "Maximum Monthly Accidents and Fatalities",
       subtitle = "Highest Recorded Accidents and Fatalities Per Month Across All Years",
       x = "Month",
       y = "Maximum Count",
       color = "Legend") +
  scale_color_manual(values = c("Max Accidents" = "#0072B2", "Max Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = month)) +
  stat_summary(aes(y = accidents, color = "Min Accidents", group = 1), fun = min, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Min Fatalities", group = 1), fun = min, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Min Accidents"), fun = min, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Min Fatalities"), fun = min, geom = "point", size = 2) +
  labs(title = "Minimum Monthly Accidents and Fatalities",
       subtitle = "Lowest Recorded Accidents and Fatalities Per Month Across All Years",
       x = "Month",
       y = "Minimum Count",
       color = "Legend") +
  scale_color_manual(values = c("Min Accidents" = "#0072B2", "Min Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = month)) +
  stat_summary(aes(y = accidents, color = "Mean Accidents", group = 1), fun = mean, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Mean Fatalities", group = 1), fun = mean, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Mean Accidents"), fun = mean, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Mean Fatalities"), fun = mean, geom = "point", size = 2) +
  labs(title = "Mean Monthly Accidents and Fatalities",
       subtitle = "Average Accidents and Fatalities Per Month Across All Years",
       x = "Month",
       y = "Mean Count",
       color = "Legend") +
  scale_color_manual(values = c("Mean Accidents" = "#0072B2", "Mean Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
mode_func2 <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
ggplot(acc.m.ind.y_mod, aes(x = month)) +
  stat_summary(aes(y = accidents, color = "Mode Accidents", group = 1), fun = mode_func2, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Mode Fatalities", group = 1), fun = mode_func2, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Mode Accidents"), fun = mode_func2, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Mode Fatalities"), fun = mode_func2, geom = "point", size = 2) +
  labs(title = "Mode of Monthly Accidents and Fatalities",
       subtitle = "Most Commonly Occurring Monthly Accident and Fatality Counts",
       x = "Month",
       y = "Mode Count",
       color = "Legend") +
  scale_color_manual(values = c("Mode Accidents" = "#0072B2", "Mode Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
range_func2 <- function(x) { max(x, na.rm = TRUE) - min(x, na.rm = TRUE) }
ggplot(acc.m.ind.y_mod, aes(x = month)) +
  stat_summary(aes(y = accidents, color = "Range Accidents", group = 1), fun = range_func2, geom = "line", size = 1.2) +
  stat_summary(aes(y = fatalities, color = "Range Fatalities", group = 1), fun = range_func2, geom = "line", size = 1.2) +
  stat_summary(aes(y = accidents, color = "Range Accidents"), fun = range_func2, geom = "point", size = 2) +
  stat_summary(aes(y = fatalities, color = "Range Fatalities"), fun = range_func2, geom = "point", size = 2) +
  labs(title = "Range of Monthly Accidents and Fatalities",
       subtitle = "Difference Between Maximum and Minimum Accidents and Fatalities Per Month",
       x = "Month",
       y = "Range",
       color = "Legend") +
  scale_color_manual(values = c("Range Accidents" = "#0072B2", "Range Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Scatter Plot of Accidents and Fatalities through the Months#

ggplot(acc.m.ind.y_mod, aes(x = accidents, y = fatalities)) +
  geom_point(alpha = 0.5, color = "#0072B2") +  
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +  
  labs(title = "Scatter Plot of Monthly Accidents vs. Fatalities",
       subtitle = "Examining the Relationship Between Monthly Accidents and Fatalities",
       x = "Monthly Accidents",
       y = "Monthly Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Plots of Sums of Accidents and Fatalities through the Months#

ggplot(acc.m.ind.y_mod, aes(x = year)) +
  stat_summary(aes(y = accidents), fun = sum, geom = "line", color = "#0072B2", size = 1.2) +
  stat_summary(aes(y = accidents), fun = sum, geom = "point", color = "#0072B2", size = 2) +
  labs(title = "Total Yearly Road Accidents (Monthly Data)",
       subtitle = "Summed Monthly Accidents for Each Year",
       x = "Year",
       y = "Total Accidents") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = year)) +
  stat_summary(aes(y = fatalities), fun = sum, geom = "line", color = "#D55E00", size = 1.2) +
  stat_summary(aes(y = fatalities), fun = sum, geom = "point", color = "#D55E00", size = 2) +
  labs(title = "Total Yearly Road Fatalities (Monthly Data)",
       subtitle = "Summed Monthly Fatalities for Each Year",
       x = "Year",
       y = "Total Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Time Series Plots of Rolling Averages, Rolling Sums and Lagged Values of Monthly Accidents#

ggplot(acc.m.ind.y_mod, aes(x = month_date)) +
  geom_line(aes(y = Rolling_Avg_3m_a, color = "3-Month Rolling Avg"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_6m_a, color = "6-Month Rolling Avg"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_12m_a, color = "12-Month Rolling Avg"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Averages of Monthly Accidents",
       subtitle = "3, 6, and 12 Month Moving Averages",
       x = "Month",
       y = "Rolling Average",
       color = "Legend") +
  scale_color_manual(values = c("3-Month Rolling Avg" = "#0072B2", 
                                "6-Month Rolling Avg" = "#E69F00",
                                "12-Month Rolling Avg" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = month_date)) +
  geom_line(aes(y = RollingSum_3m_a, color = "3-Month Rolling Sum"), size = 1, alpha = 0.8) +
  geom_line(aes(y = RollingSum_6m_a, color = "6-Month Rolling Sum"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = RollingSum_12m_a, color = "12-Month Rolling Sum"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Sums of Monthly Accidents",
       subtitle = "3, 6, and 12 Month Rolling Sums",
       x = "Month",
       y = "Rolling Sum",
       color = "Legend") +
  scale_color_manual(values = c("3-Month Rolling Sum" = "#0072B2", 
                                "6-Month Rolling Sum" = "#E69F00",
                                "12-Month Rolling Sum" = "#D55E00"))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = month_date)) +
  geom_line(aes(y = Accidents_Lag1, color = "Lag 1 Month"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag2, color = "Lag 2 Months"), size = 1, alpha = 0.6) +  
  geom_line(aes(y = Accidents_Lag3, color = "Lag 3 Months"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag6, color = "Lag 6 Months"), size = 1, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag12, color = "Lag 12 Months"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag24, color = "Lag 24 Months"), size = 1, linetype = "dashed", alpha = 0.6) +
  labs(title = "Lagged Monthly Accidents",
       subtitle = "Comparing Different Monthly Lag Periods",
       x = "Month",
       y = "Lagged Accidents",
       color = "Legend") +
  scale_color_manual(values = c("Lag 1 Month" = "#0072B2", 
                                "Lag 2 Months" = "#E69F00",
                                "Lag 3 Months" = "#44cb2f",
                                "Lag 6 Months" = "#dc6bcb",
                                "Lag 12 Months" = "#009E73",
                                "Lag 24 Months" = "#CC79A7")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Time Series Plots of Rolling Averages, Rolling Sums and Lagged Values of Monthly Fatalities#

ggplot(acc.m.ind.y_mod, aes(x = month_date)) +
  geom_line(aes(y = Rolling_Avg_3m_f, color = "3-Month Rolling Avg"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_6m_f, color = "6-Month Rolling Avg"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_12m_f, color = "12-Month Rolling Avg"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Averages of Monthly Fatalities",
       subtitle = "3, 6, and 12 Month Moving Averages",
       x = "Month",
       y = "Rolling Average",
       color = "Legend") +
  scale_color_manual(values = c("3-Month Rolling Avg" = "#0072B2", 
                                "6-Month Rolling Avg" = "#E69F00",
                                "12-Month Rolling Avg" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = month_date)) +
  geom_line(aes(y = RollingSum_3m_f, color = "3-Month Rolling Sum"), size = 1, alpha = 0.8) +
  geom_line(aes(y = RollingSum_6m_f, color = "6-Month Rolling Sum"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = RollingSum_12m_f, color = "12-Month Rolling Sum"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Sums of Monthly Fatalities",
       subtitle = "3, 6, and 12 Month Rolling Sums",
       x = "Month",
       y = "Rolling Sum",
       color = "Legend") +
  scale_color_manual(values = c("3-Month Rolling Sum" = "#0072B2", 
                                "6-Month Rolling Sum" = "#E69F00",
                                "12-Month Rolling Sum" = "#D55E00"))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.m.ind.y_mod, aes(x = month_date)) +
  geom_line(aes(y = Fatalities_Lag1, color = "Lag 1 Month"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag2, color = "Lag 2 Months"), size = 1, alpha = 0.6) +  
  geom_line(aes(y = Fatalities_Lag3, color = "Lag 3 Months"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag6, color = "Lag 6 Months"), size = 1, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag12, color = "Lag 12 Months"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag24, color = "Lag 24 Months"), size = 1, linetype = "dashed", alpha = 0.6) +
  labs(title = "Lagged Monthly Fatalities",
       subtitle = "Comparing Different Monthly Lag Periods",
       x = "Month",
       y = "Lagged Fatalities",
       color = "Legend") +
  scale_color_manual(values = c("Lag 1 Month" = "#0072B2", 
                                "Lag 2 Months" = "#E69F00",
                                "Lag 3 Months" = "#44cb2f",
                                "Lag 6 Months" = "#dc6bcb",
                                "Lag 12 Months" = "#009E73",
                                "Lag 24 Months" = "#CC79A7")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
colnames(acc.m.ind.y_mod)

#Max, Mean, Min, Mode, Median and Range of Accidents Per Season Per Month through the Years#

acc.m.ind.y_mod$season <- as.factor(acc.m.ind.y_mod$season)
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = season, group = season)) +
  stat_summary(fun = max, geom = "line", size = 1.2) +
  labs(title = "Maximum Monthly Accidents Per Season",
       subtitle = "Highest Recorded Monthly Accidents Across Different Seasons",
       x = "Month",
       y = "Maximum Monthly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = season, group = season)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Mean Monthly Accidents Per Season",
       subtitle = "Average Monthly Accidents Across Different Seasons",
       x = "Month",
       y = "Mean Monthly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = season, group = season)) +
  stat_summary(fun = min, geom = "line", size = 1.2) +
  labs(title = "Minimum Monthly Accidents Per Season",
       subtitle = "Lowest Recorded Monthly Accidents Across Different Seasons",
       x = "Month",
       y = "Minimum Monthly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = season, group = season)) +
  stat_summary(fun = mode_func2, geom = "line", size = 1.2) +
  labs(title = "Mode of Monthly Accidents Per Season",
       subtitle = "Most Frequent Monthly Accidents Across Different Seasons",
       x = "Month",
       y = "Mode of Monthly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = season, group = season)) +
  stat_summary(fun = median, geom = "line", size = 1.2) +
  labs(title = "Median Monthly Accidents Per Season",
       subtitle = "Middle Value of Monthly Accidents Across Different Seasons",
       x = "Month",
       y = "Median Monthly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = season, group = season)) +
  stat_summary(fun = range_func2, geom = "line", size = 1.2) +
  labs(title = "Range of Monthly Accidents Per Season",
       subtitle = "Difference Between Max and Min Accidents Per Month Across Different Seasons",
       x = "Month",
       y = "Range of Monthly Accidents",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Max, Mean, Min, Mode, Median and Range of Fatalities Per Season Per Month through the Years#

acc.m.ind.y_mod$season <- as.factor(acc.m.ind.y_mod$season)
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = season, group = season)) +
  stat_summary(fun = max, geom = "line", size = 1.2) +
  labs(title = "Maximum Monthly Fatalities Per Season",
       subtitle = "Highest Recorded Monthly Fatalities Across Different Seasons",
       x = "Month",
       y = "Maximum Monthly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = season, group = season)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Mean Monthly Fatalities Per Season",
       subtitle = "Average Monthly Fatalities Across Different Seasons",
       x = "Month",
       y = "Mean Monthly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = season, group = season)) +
  stat_summary(fun = min, geom = "line", size = 1.2) +
  labs(title = "Minimum Monthly Fatalities Per Season",
       subtitle = "Lowest Recorded Monthly Fatalities Across Different Seasons",
       x = "Month",
       y = "Minimum Monthly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = season, group = season)) +
  stat_summary(fun = mode_func2, geom = "line", size = 1.2) +
  labs(title = "Mode of Monthly Fatalities Per Season",
       subtitle = "Most Frequent Monthly Fatalities Across Different Seasons",
       x = "Month",
       y = "Mode of Monthly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = season, group = season)) +
  stat_summary(fun = median, geom = "line", size = 1.2) +
  labs(title = "Median Monthly Fatalities Per Season",
       subtitle = "Middle Value of Monthly Fatalities Across Different Seasons",
       x = "Month",
       y = "Median Monthly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = season, group = season)) +
  stat_summary(fun = range_func2, geom = "line", size = 1.2) +
  labs(title = "Range of Monthly Fatalities Per Season",
       subtitle = "Difference Between Max and Min Fatalities Per Month Across Different Seasons",
       x = "Month",
       y = "Range of Monthly Fatalities",
       color = "Season") +
  scale_color_manual(values = c("Winter" = "#0072B2", 
                                "Spring" = "#E69F00",
                                "Summer" = "#D55E00",
                                "Autumn" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Max, Mean, Min, Mode, Median and Range of Accidents Per Quarter Per Month through the Years#

acc.m.ind.y_mod$quarter <- as.factor(acc.m.ind.y_mod$quarter)
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = quarter, group = quarter)) +
  stat_summary(fun = max, geom = "line", size = 1.2) +
  labs(title = "Maximum Monthly Accidents Per Quarter",
       subtitle = "Highest Recorded Monthly Accidents Per Quarter",
       x = "Month",
       y = "Max Monthly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = quarter, group = quarter)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Mean Monthly Accidents Per Quarter",
       subtitle = "Average Monthly Accidents Per Quarter",
       x = "Month",
       y = "Mean Monthly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = quarter, group = quarter)) +
  stat_summary(fun = min, geom = "line", size = 1.2) +
  labs(title = "Minimum Monthly Accidents Per Quarter",
       subtitle = "Lowest Recorded Monthly Accidents Per Quarter",
       x = "Month",
       y = "Min Monthly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = quarter, group = quarter)) +
  stat_summary(fun = mode_func2, geom = "line", size = 1.2) +
  labs(title = "Mode of Monthly Accidents Per Quarter",
       subtitle = "Most Frequent Monthly Accidents Per Quarter",
       x = "Month",
       y = "Mode of Monthly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = quarter, group = quarter)) +
  stat_summary(fun = median, geom = "line", size = 1.2) +
  labs(title = "Median Monthly Accidents Per Quarter",
       subtitle = "Middle Value of Monthly Accidents Per Quarter",
       x = "Month",
       y = "Median Monthly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = quarter, group = quarter)) +
  stat_summary(fun = range_func2, geom = "line", size = 1.2) +
  labs(title = "Range of Monthly Accidents Per Quarter",
       subtitle = "Difference Between Max and Min Monthly Accidents Per Quarter",
       x = "Month",
       y = "Range of Monthly Accidents",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Max, Mean, Min, Mode, Median and Range of Fatalities Per Quarter Per Month through the Years#

acc.m.ind.y_mod$quarter <- as.factor(acc.m.ind.y_mod$quarter)
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = quarter, group = quarter)) +
  stat_summary(fun = max, geom = "line", size = 1.2) +
  labs(title = "Maximum Monthly Fatalities Per Quarter",
       subtitle = "Highest Recorded Monthly Fatalities Per Quarter",
       x = "Month",
       y = "Max Monthly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = quarter, group = quarter)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Mean Monthly Fatalities Per Quarter",
       subtitle = "Average Monthly Fatalities Per Quarter",
       x = "Month",
       y = "Mean Monthly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = quarter, group = quarter)) +
  stat_summary(fun = min, geom = "line", size = 1.2) +
  labs(title = "Minimum Monthly Fatalities Per Quarter",
       subtitle = "Lowest Recorded Monthly Fatalities Per Quarter",
       x = "Month",
       y = "Min Monthly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = quarter, group = quarter)) +
  stat_summary(fun = mode_func2, geom = "line", size = 1.2) +
  labs(title = "Mode of Monthly Fatalities Per Quarter",
       subtitle = "Most Frequent Monthly Fatalities Per Quarter",
       x = "Month",
       y = "Mode of Monthly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = quarter, group = quarter)) +
  stat_summary(fun = median, geom = "line", size = 1.2) +
  labs(title = "Median Monthly Fatalities Per Quarter",
       subtitle = "Middle Value of Monthly Fatalities Per Quarter",
       x = "Month",
       y = "Median Monthly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = quarter, group = quarter)) +
  stat_summary(fun = range_func2, geom = "line", size = 1.2) +
  labs(title = "Range of Monthly Fatalities Per Quarter",
       subtitle = "Difference Between Max and Min Monthly Fatalities Per Quarter",
       x = "Month",
       y = "Range of Monthly Fatalities",
       color = "Quarter") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Plots of Accidents Per Month when it is Summer, Winter, Holidays vs NOT#

acc.m.ind.y_mod$is_summer <- as.factor(acc.m.ind.y_mod$is_summer)
acc.m.ind.y_mod$is_winter <- as.factor(acc.m.ind.y_mod$is_winter)
acc.m.ind.y_mod$is_holiday <- as.factor(acc.m.ind.y_mod$is_holiday)
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = is_summer, group = is_summer)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Monthly Accidents: Summer (1 vs 0)",
       subtitle = "Comparison of Monthly Accidents When is_summer is 1 vs 0",
       x = "Month",
       y = "Mean Monthly Accidents",
       color = "Summer Indicator") +
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#E69F00"),
                     labels = c("Not Summer", "Summer")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = is_winter, group = is_winter)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Monthly Accidents: Winter (1 vs 0)",
       subtitle = "Comparison of Monthly Accidents When is_winter is 1 vs 0",
       x = "Month",
       y = "Mean Monthly Accidents",
       color = "Winter Indicator") +
  scale_color_manual(values = c("0" = "#D55E00", "1" = "#0072B2"),
                     labels = c("Not Winter", "Winter")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = accidents, color = is_holiday, group = is_holiday)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Monthly Accidents: Holiday (1 vs 0)",
       subtitle = "Comparison of Monthly Accidents When is_holiday is 1 vs 0",
       x = "Month",
       y = "Mean Monthly Accidents",
       color = "Holiday Indicator") +
  scale_color_manual(values = c("0" = "#009E73", "1" = "#CC79A7"),
                     labels = c("Not Holiday", "Holiday")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Plots of Fatalities Per Month when it is Summer, Winter, Holidays vs NOT#

acc.m.ind.y_mod$is_summer <- as.factor(acc.m.ind.y_mod$is_summer)
acc.m.ind.y_mod$is_winter <- as.factor(acc.m.ind.y_mod$is_winter)
acc.m.ind.y_mod$is_holiday <- as.factor(acc.m.ind.y_mod$is_holiday)
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = is_summer, group = is_summer)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Monthly Fatalities: Summer (1 vs 0)",
       subtitle = "Comparison of Monthly Fatalities When is_summer is 1 vs 0",
       x = "Month",
       y = "Mean Monthly Fatalities",
       color = "Summer Indicator") +
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#E69F00"),
                     labels = c("Not Summer", "Summer")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = is_winter, group = is_winter)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Monthly Fatalities: Winter (1 vs 0)",
       subtitle = "Comparison of Monthly Fatalities When is_winter is 1 vs 0",
       x = "Month",
       y = "Mean Monthly Fatalities",
       color = "Winter Indicator") +
  scale_color_manual(values = c("0" = "#D55E00", "1" = "#0072B2"),
                     labels = c("Not Winter", "Winter")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggplot(acc.m.ind.y_mod, aes(x = month, y = fatalities, color = is_holiday, group = is_holiday)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Monthly Fatalities: Holiday (1 vs 0)",
       subtitle = "Comparison of Monthly Fatalities When is_holiday is 1 vs 0",
       x = "Month",
       y = "Mean Monthly Fatalities",
       color = "Holiday Indicator") +
  scale_color_manual(values = c("0" = "#009E73", "1" = "#CC79A7"),
                     labels = c("Not Holiday", "Holiday")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#Plots of Monthly Accident Growth Rate, Fatality Growth Rate, Fatality Rate, Accidents Change Rate, Fatalities Change Rate, Fatalities Per Capita, Accidents Per Vehicle#

ggplot(acc.m.ind.y_mod, aes(x = month_date, y = AccidentGrowthRate)) +
  geom_line(color = "#0072B2", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#0072B2", fill = "#0072B2", alpha = 0.2, size = 1.2) +
  labs(title = "Monthly Accident Growth Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Accident Growth Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.m.ind.y_mod, aes(x = month_date, y = FatalityGrowthRate)) +
  geom_line(color = "#D55E00", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#D55E00", fill = "#D55E00", alpha = 0.2, size = 1.2) +
  labs(title = "Monthly Fatality Growth Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Fatality Growth Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.m.ind.y_mod, aes(x = month_date, y = FatalityRate)) +
  geom_line(color = "#009E73", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#009E73", fill = "#009E73", alpha = 0.2, size = 1.2) +
  labs(title = "Monthly Fatality Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Fatality Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.m.ind.y_mod, aes(x = month_date, y = AccidentChangeRate)) +
  geom_line(color = "#E69F00", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E69F00", fill = "#E69F00", alpha = 0.2, size = 1.2) +
  labs(title = "Monthly Accident Change Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Accident Change Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.m.ind.y_mod, aes(x = month_date, y = FatalityChangeRate)) +
  geom_line(color = "#CC79A7", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#CC79A7", fill = "#CC79A7", alpha = 0.2, size = 1.2) +
  labs(title = "Monthly Fatality Change Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Fatality Change Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.m.ind.y_mod, aes(x = month_date, y = FatalitiesPerCapita)) +
  geom_line(color = "#56B4E9", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#56B4E9", fill = "#56B4E9", alpha = 0.2, size = 1.2) +
  labs(title = "Monthly Fatalities Per Capita Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Fatalities Per Capita") +
  theme_minimal(base_size = 14)
ggplot(acc.m.ind.y_mod, aes(x = month_date, y = AccidentsPerVehicle)) +
  geom_line(color = "#009E73", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E69F00", fill = "#E69F00", alpha = 0.2, size = 1.2) +
  labs(title = "Monthly Accidents Per Vehicle Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Date",
       y = "Accidents Per Vehicle") +
  theme_minimal(base_size = 14)

#Histograms for All Variables#

numeric_data.acc.m.hplots <- acc.m.ind.y_mod %>% select(where(is.numeric))
long_data.acc.m.hplots <- numeric_data.acc.m.hplots %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
var_names.acc.m.hplots <- unique(long_data.acc.m.hplots$Variable)
var_groups.acc.m.hplots <- split(var_names.acc.m.hplots, ceiling(seq_along(var_names.acc.m.hplots) / 8))
plot_histograms2 <- function(var_subset) {
  ggplot(long_data.acc.m.hplots %>% filter(Variable %in% var_subset), aes(x = Value)) +
    geom_histogram(bins = 50, fill = "#0072B2", color = "black", alpha = 0.7) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) +  
    labs(title = "Histograms of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Value",
         y = "Frequency") +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
histogram_plots.acc.m <- lapply(var_groups.acc.m.hplots, plot_histograms2)
histogram_plots.acc.m

#Box Plots for All Variables#

numeric_data.acc.m.bplots <- acc.m.ind.y_mod %>%
  select(month, where(is.numeric)) %>%  
  mutate(month = as.factor(month))  
long_data.acc.m.bplots <- numeric_data.acc.m.bplots %>%
  pivot_longer(cols = -month, names_to = "Variable", values_to = "Value")
var_names.acc.m.bplots <- unique(long_data.acc.m.bplots$Variable)
var_groups.acc.m.bplots <- split(var_names.acc.m.bplots, ceiling(seq_along(var_names.acc.m.bplots) / 8))
plot_boxplots2 <- function(var_subset) {
  ggplot(long_data.acc.m.bplots %>% filter(Variable %in% var_subset), aes(x = month, y = Value)) +
    geom_boxplot(fill = "#0072B2", color = "black", alpha = 0.6, outlier.shape = NA) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) +  
    labs(title = "Monthly Box Plots of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Month",
         y = "Value") +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
boxplot_plots.acc.m.bplots <- lapply(var_groups.acc.m.bplots, plot_boxplots2)
boxplot_plots.acc.m.bplots

#Plots of Means of All Variables#

numeric_data.acc.m.mplots <- acc.m.ind.y_mod %>%
  select(month, where(is.numeric)) %>%
  mutate(month = as.factor(month))
long_data.acc.m.mplots <- numeric_data.acc.m.mplots %>%
  pivot_longer(cols = -month, names_to = "Variable", values_to = "Value")
var_names.acc.m.mplots <- unique(long_data.acc.m.mplots$Variable)
var_groups.acc.m.mplots <- split(var_names.acc.m.mplots, ceiling(seq_along(var_names.acc.m.mplots) / 8))
plot_means2 <- function(var_subset) {
  ggplot(long_data.acc.m.mplots %>% filter(Variable %in% var_subset), aes(x = month, y = Value, group = Variable, color = Variable)) +
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) + 
    labs(title = "Monthly Mean of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Month",
         y = "Mean Value") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
mean_plots.acc.m.mplots <- lapply(var_groups.acc.m.mplots, plot_means2)
mean_plots.acc.m.mplots

#Time Series Plots of All variables#

numeric_data.acc.m.tsplots <- acc.m.ind.y_mod %>%
  select(month_date, where(is.numeric)) %>%
  mutate(month_date = as.Date(month_date))
long_data.acc.m.tsplots <- numeric_data.acc.m.tsplots %>%
  pivot_longer(cols = -month_date, names_to = "Variable", values_to = "Value")
var_names.acc.m.tsplots <- unique(long_data.acc.m.tsplots$Variable)
var_groups.acc.m.tsplots <- split(var_names.acc.m.tsplots, ceiling(seq_along(var_names.acc.m.tsplots) / 8))
plot_time_series2 <- function(var_subset) {
  ggplot(long_data.acc.m.tsplots %>% filter(Variable %in% var_subset), aes(x = month_date, y = Value, color = Variable)) +
    geom_line(size = 0.8, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, size = 1, alpha = 0.5) +  
    facet_wrap(~ Variable, scales = "free", ncol = 4) + 
    labs(title = "Time Series of Numeric Variables Through the Months",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Month",
         y = "Value") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
time_series_plots.acc.m.tsplots <- lapply(var_groups.acc.m.tsplots, plot_time_series2)
time_series_plots.acc.m.tsplots

#Skewness Plot of Accidents and Fatalities Per Month through the Years#

skewness_data.acc.m <- acc.m.ind.y_mod %>%
  group_by(month) %>%
  summarise(
    Skew_Accidents = skewness(accidents, na.rm = TRUE),
    Skew_Fatalities = skewness(fatalities, na.rm = TRUE)
  ) %>%
  mutate(across(Skew_Accidents:Skew_Fatalities, ~ ifelse(is.nan(.) | is.infinite(.), 0, .)))  # Replace NaN/Inf with 0
ggplot(skewness_data.acc.m, aes(x = month)) +
  geom_line(aes(y = Skew_Accidents, color = "Accidents", group = 1), size = 1.2) +  # Add group = 1
  geom_line(aes(y = Skew_Fatalities, color = "Fatalities", group = 1), size = 1.2) +  # Add group = 1
  labs(title = "Monthly Skewness of Accidents and Fatalities",
       subtitle = "Shows Asymmetry in Monthly Data",
       x = "Month",
       y = "Skewness",
       color = "Metric") +
  scale_color_manual(values = c("Accidents" = "#0072B2", "Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14)

#Kurtosis Plot of Accidents and Fatalities Per Month through the Years#

kurtosis_data.acc.w <- acc.w.ind.y_mod %>%
  group_by(week) %>%
  summarise(
    Kurt_Accidents = kurtosis(accidents, na.rm = TRUE),
    Kurt_Fatalities = kurtosis(fatalities, na.rm = TRUE)
  )
ggplot(kurtosis_data.acc.w, aes(x = week)) +
  geom_line(aes(y = Kurt_Accidents, color = "Accidents"), size = 1.2) +
  geom_line(aes(y = Kurt_Fatalities, color = "Fatalities"), size = 1.2) +
  labs(title = "Weekly Kurtosis of Accidents and Fatalities",
       subtitle = "Measures Tail Heaviness of Weekly Data",
       x = "Week",
       y = "Kurtosis",
       color = "Metric") +
  scale_color_manual(values = c("Accidents" = "#0072B2", "Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14)

#Density Plot of Monthly Accidents and Fatalities#

ggplot(acc.m.ind.y_mod) +
  geom_density(aes(x = accidents, fill = "Accidents"), alpha = 0.5, color = "black") +
  geom_density(aes(x = fatalities, fill = "Fatalities"), alpha = 0.5, color = "black") +
  labs(title = "Density Plot of Monthly Accidents and Fatalities",
       subtitle = "Shows the Probability Distribution of Monthly Data",
       x = "Count",
       y = "Density",
       fill = "Metric") +
  scale_fill_manual(values = c("Accidents" = "#0072B2", "Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14)

#Percentiles Plots of Accidents and Fatalities per Month through the Years#

percentile_data.acc.m <- acc.m.ind.y_mod %>%
  group_by(month) %>%
  summarise(
    P10_Accidents = quantile(accidents, probs = 0.10, na.rm = TRUE),
    P25_Accidents = quantile(accidents, probs = 0.25, na.rm = TRUE),
    P50_Accidents = quantile(accidents, probs = 0.50, na.rm = TRUE),  
    P75_Accidents = quantile(accidents, probs = 0.75, na.rm = TRUE),
    P90_Accidents = quantile(accidents, probs = 0.90, na.rm = TRUE),
    P10_Fatalities = quantile(fatalities, probs = 0.10, na.rm = TRUE),
    P25_Fatalities = quantile(fatalities, probs = 0.25, na.rm = TRUE),
    P50_Fatalities = quantile(fatalities, probs = 0.50, na.rm = TRUE),  
    P75_Fatalities = quantile(fatalities, probs = 0.75, na.rm = TRUE),
    P90_Fatalities = quantile(fatalities, probs = 0.90, na.rm = TRUE)
  )
ggplot(percentile_data.acc.m, aes(x = month)) +
  geom_line(aes(y = P10_Accidents, color = "P10 Accidents", group = 1), size = 1) +
  geom_line(aes(y = P50_Accidents, color = "P50 Accidents (Median)", group = 1), size = 1.2) +
  geom_line(aes(y = P90_Accidents, color = "P90 Accidents", group = 1), size = 1) +
  geom_line(aes(y = P10_Fatalities, color = "P10 Fatalities", group = 1), size = 1, linetype = "dashed") +
  geom_line(aes(y = P50_Fatalities, color = "P50 Fatalities (Median)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = P90_Fatalities, color = "P90 Fatalities", group = 1), size = 1, linetype = "dashed") +
  labs(title = "Monthly Percentiles of Accidents and Fatalities",
       subtitle = "Comparison of 10th, 50th, and 90th Percentiles Over the Months",
       x = "Month",
       y = "Percentile Values",
       color = "Percentile") +
  scale_color_manual(values = c(
    "P10 Accidents" = "#0072B2",
    "P50 Accidents (Median)" = "#005293",
    "P90 Accidents" = "#003366",
    "P10 Fatalities" = "#D55E00",
    "P50 Fatalities (Median)" = "#A53D00",
    "P90 Fatalities" = "#732000"
  )) +
  theme_minimal(base_size = 14)

#CV Plots for All Variables#

numeric_data.acc.m.cvplots <- acc.m.ind.y_mod %>%
  select(month, where(is.numeric)) %>%
  mutate(month = as.factor(month)) 
cv_data.acc.m.cvplots  <- numeric_data.acc.m.cvplots  %>%
  group_by(month) %>%
  summarise(across(everything(), ~ sd(., na.rm = TRUE) / mean(., na.rm = TRUE), .names = "CV_{.col}")) %>%
  pivot_longer(cols = -month, names_to = "Variable", values_to = "CV_Value")
var_names.acc.m.cvplots  <- unique(cv_data.acc.m.cvplots$Variable)
var_groups.acc.m.cvplots  <- split(var_names.acc.m.cvplots, ceiling(seq_along(var_names.acc.m.cvplots) / 8))
plot_cv_monthly <- function(var_subset) {
  ggplot(cv_data.acc.m.cvplots %>% filter(Variable %in% var_subset), aes(x = month, y = CV_Value, group = 1, color = Variable)) +
    geom_line(size = 1.2, alpha = 0.8) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) +  
    labs(title = "Monthly Coefficient of Variation (CV) of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Month",
         y = "CV (Standard Deviation / Mean)") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
cv_plots.acc.m.cvplots  <- lapply(var_groups.acc.m.cvplots, plot_cv_monthly)
cv_plots.acc.m.cvplots 

#Range and Variance Plots for All Variables#

numeric_data.acc.m.rvplots <- acc.m.ind.y_mod %>%
  select(month, where(is.numeric)) %>%
  mutate(month = as.factor(month)) 
range_variance_data.acc.m.rvplots <- numeric_data.acc.m.rvplots %>%
  group_by(month) %>%
  summarise(across(everything(), 
                   list(Range = ~ max(., na.rm = TRUE) - min(., na.rm = TRUE),
                        Variance = ~ var(., na.rm = TRUE)), 
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -month, names_to = "Metric", values_to = "Value")
range_data.acc.m.rvplots <- range_variance_data.acc.m.rvplots %>% filter(grepl("_Range$", Metric))
variance_data.acc.m.rvplots <- range_variance_data.acc.m.rvplots %>% filter(grepl("_Variance$", Metric))
range_vars.acc.m.rvplots <- unique(range_data.acc.m.rvplots$Metric)
variance_vars.acc.m.rvplots <- unique(variance_data.acc.m.rvplots$Metric)
range_groups.acc.m.rvplots <- split(range_vars.acc.m.rvplots, ceiling(seq_along(range_vars.acc.m.rvplots) / 8))
variance_groups.acc.m.rvplots <- split(variance_vars.acc.m.rvplots, ceiling(seq_along(variance_vars.acc.m.rvplots) / 8))
plot_range_monthly <- function(var_subset) {
  ggplot(range_data.acc.m.rvplots %>% filter(Metric %in% var_subset), aes(x = month, y = Value, group = 1, color = Metric)) +
    geom_line(size = 1.2, alpha = 0.8) +
    facet_wrap(~ Metric, scales = "free", ncol = 4) +  
    labs(title = "Monthly Range of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Month",
         y = "Range (Max - Min)") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    )
}
plot_variance_monthly <- function(var_subset) {
  ggplot(variance_data.acc.m.rvplots %>% filter(Metric %in% var_subset), aes(x = month, y = Value, group = 1, color = Metric)) +
    geom_line(size = 1.2, alpha = 0.8) +
    facet_wrap(~ Metric, scales = "free", ncol = 4) + 
    labs(title = "Monthly Variance of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Month",
         y = "Variance") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
    )
}
range_plots.acc.m.rvplots <- lapply(range_groups.acc.m.rvplots, plot_range_monthly)
variance_plots.acc.m.rvplots <- lapply(variance_groups.acc.m.rvplots, plot_variance_monthly)
range_plots.acc.m.rvplots
variance_plots.acc.m.rvplots

#Simple and Multiple Linear Regression Plots#

simple_lm.acc.m <- lm(accidents ~ vehicles, data = acc.m.ind.y_mod)
ggplot(acc.m.ind.y_mod, aes(x = vehicles, y = accidents)) +
  geom_point(alpha = 0.5, color = "#0072B2") +  
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.3) +  
  labs(title = "Simple Linear Regression: Accidents vs Vehicles (Monthly)",
       subtitle = "Fitted Line with Confidence Interval",
       x = "Number of Vehicles",
       y = "Number of Accidents") +
  theme_minimal(base_size = 14)
plot_multi_lm_monthly <- function(var) {
  ggplot(acc.m.ind.y_mod, aes_string(x = var, y = "accidents")) +
    geom_point(alpha = 0.5, color = "#D55E00") + 
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.3) + 
    labs(title = paste("Multiple Regression: Accidents vs", var, "(Monthly)"),
         subtitle = "Fitted Line with Confidence Interval",
         x = var,
         y = "Number of Accidents") +
    theme_minimal(base_size = 14)
}
plot_vehicles.acc.m <- plot_multi_lm_monthly("vehicles")
plot_unemployment.acc.m <- plot_multi_lm_monthly("unemployment")
plot_inflation.acc.m <- plot_multi_lm_monthly("inflation_rate")
plot_vehicles.acc.m
plot_unemployment.acc.m
plot_inflation.acc.m
multi_lm.acc.m <- lm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.m.ind.y_mod)
plot_diagnostics_lm_monthly <- function(model, model_name) {
  diagnostics <- fortify(model)
  p1 <- ggplot(diagnostics, aes(.fitted, .resid)) +
    geom_point(color = "#0072B2", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste(model_name, "- Residuals vs Fitted (Monthly)"),
         x = "Fitted Values",
         y = "Residuals") +
    theme_minimal()
  p2 <- ggplot(diagnostics, aes(sample = .stdresid)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste(model_name, "- Q-Q Plot (Monthly)"),
         x = "Theoretical Quantiles",
         y = "Standardized Residuals") +
    theme_minimal()
  p3 <- ggplot(diagnostics, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point(color = "#E69F00", alpha = 0.5) +
    geom_smooth(method = "loess", color = "red") +
    labs(title = paste(model_name, "- Scale-Location Plot (Monthly)"),
         x = "Fitted Values",
         y = "√|Standardized Residuals|") +
    theme_minimal()
  p4 <- ggplot(diagnostics, aes(.hat, .stdresid)) +
    geom_point(color = "#D55E00", alpha = 0.5) +
    geom_smooth(method = "loess", color = "red") +
    labs(title = paste(model_name, "- Residuals vs Leverage (Monthly)"),
         x = "Leverage",
         y = "Standardized Residuals") +
    theme_minimal()
  
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}
plot_diagnostics_lm_monthly(simple_lm.acc.m, "Simple LM (Accidents ~ Vehicles) (Monthly)")
plot_diagnostics_lm_monthly(multi_lm.acc.m, "Multiple LM (Accidents ~ Vehicles + Unemployment + Inflation) (Monthly)")

#Correlation Matrices#

cor_data.acc.m.cm.short<- acc.m.ind.y_mod %>%
  select(year, month, accidents, fatalities, total_gdp, gdp_per_capita, unemployment, 
         population, vehicles, inflation_rate, quarter, 
         season, is_summer, is_winter, is_holiday, AccidentGrowthRate, FatalityGrowthRate, FatalityRate,
         AccidentChangeRate, FatalityChangeRate, AccidentsPerCapita, FatalitiesPerCapita, AccidentsPerVehicle, AccidentsUnemployment, FatalitiesInflation, 
         CumulativeAccidents, CumulativeFatalities, season_numeric)
numeric_data.acc.m.cm.short <- cor_data.acc.m.cm.short %>% select(where(is.numeric))
cor_pearson.acc.m.cm.short <- cor(numeric_data.acc.m.cm.short, use = "pairwise.complete.obs", method = "pearson")
cor_spearman.acc.m.cm.short <- cor(numeric_data.acc.m.cm.short, use = "pairwise.complete.obs", method = "spearman")
cor_kendall.acc.m.cm.short <- cor(numeric_data.acc.m.cm.short, use = "pairwise.complete.obs", method = "kendall")
cov_matrix.acc.m.cm.short <- cov(numeric_data.acc.m.cm.short, use = "pairwise.complete.obs")
melted_cor_pearson.acc.m.cm.short <- melt(cor_pearson.acc.m.cm.short)
ggplot(melted_cor_pearson.acc.m.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Pearson Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cor_spearman.acc.m.cm.short <- melt(cor_spearman.acc.m.cm.short)
ggplot(melted_cor_spearman.acc.m.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Spearman Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cor_kendall.acc.m.cm.short <- melt(cor_kendall.acc.m.cm.short)
ggplot(melted_cor_kendall.acc.m.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Kendall Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cov.acc.m.cm.short <- melt(cov_matrix.acc.m.cm.short)
ggplot(melted_cov.acc.m.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Covariance Matrix (Accidents Data)", x = "", y = "", fill = "Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
numeric_data.acc.m.cm <- acc.m.ind.y_mod %>%
  select(where(is.numeric))
cor_pearson.acc.m.cm  <- cor(numeric_data.acc.m.cm , use = "pairwise.complete.obs", method = "pearson")
cor_spearman.acc.m.cm  <- cor(numeric_data.acc.m.cm , use = "pairwise.complete.obs", method = "spearman")
cor_kendall.acc.m.cm  <- cor(numeric_data.acc.m.cm , use = "pairwise.complete.obs", method = "kendall")
cov_matrix.acc.m.cm  <- cov(numeric_data.acc.m.cm , use = "pairwise.complete.obs")
melted_cor_pearson.acc.m.cm  <- melt(cor_pearson.acc.m.cm)
melted_cor_spearman.acc.m.cm  <- melt(cor_spearman.acc.m.cm)
melted_cor_kendall.acc.m.cm  <- melt(cor_kendall.acc.m.cm)
melted_cov.acc.m.cm  <- melt(cov_matrix.acc.m.cm)
plot_heatmap_monthly <- function(data, title) {
  ggplot(data, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab") +
    labs(title = title, x = "", y = "", fill = "Value") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}
plot_heatmap_monthly(melted_cor_pearson.acc.m.cm , "Pearson Correlation Matrix (Monthly)")
plot_heatmap_monthly(melted_cor_spearman.acc.m.cm , "Spearman Correlation Matrix (Monthly)")
plot_heatmap_monthly(melted_cor_kendall.acc.m.cm , "Kendall Correlation Matrix (Monthly)")
plot_heatmap_monthly(melted_cov.acc.m.cm , "Covariance Matrix (Monthly)")

#Shapiro Wilk and Kolmogorov-Smirnov Test#

numeric_data.acc.m.swplot <- acc.m.ind.y_mod %>% select(where(is.numeric))
numeric_data.acc.m.swplot <- numeric_data.acc.m.swplot %>%
  select(where(~ length(unique(na.omit(.))) > 1))
shapiro_results.acc.m <- sapply(numeric_data.acc.m.swplot, function(x) {
  if (length(x) <= 5000) {
    if (length(unique(na.omit(x))) > 1) {
      shapiro.test(x)$p.value
    } else {
      NA  
    }
  } else {
    NA  
  }
})
ks_results.acc.m <- sapply(numeric_data.acc.m.swplot, function(x) {
  if (length(unique(na.omit(x))) > 1) {
    ks.test(x, "pnorm", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))$p.value
  } else {
    NA  
  }
})
normality_data.acc.m.swplot <- data.frame(
  Variable = names(shapiro_results.acc.m),
  Shapiro_P = shapiro_results.acc.m,
  KS_P = ks_results.acc.m
) %>%
  pivot_longer(cols = -Variable, names_to = "Test", values_to = "P_Value")
ggplot(normality_data.acc.m.swplot, aes(x = Test, y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Shapiro-Wilk & KS Test P-Values (Monthly Data)",
       x = "Test",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14)

#Boxplot of Two Sample T test#

ggplot(acc.m.ind.y_mod, aes(x = as.factor(is_summer), y = accidents, fill = as.factor(is_summer))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Two-Sample T-Test: Accidents in Summer vs Non-Summer (Monthly)",
       x = "Summer (1 = Yes, 0 = No)",
       y = "Number of Accidents") +
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00")) +
  theme_minimal(base_size = 14)
ggplot(acc.m.ind.y_mod, aes(x = as.factor(is_winter), y = accidents, fill = as.factor(is_winter))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Two-Sample T-Test: Accidents in Winter vs Non-Winter (Monthly)",
       x = "Winter (1 = Yes, 0 = No)",
       y = "Number of Accidents") +
  scale_fill_manual(values = c("0" = "#E69F00", "1" = "#0072B2")) +
  theme_minimal(base_size = 14)
ggplot(acc.m.ind.y_mod, aes(x = as.factor(is_holiday), y = accidents, fill = as.factor(is_holiday))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Two-Sample T-Test: Accidents on Holidays vs Non-Holidays (Monthly)",
       x = "Holiday (1 = Yes, 0 = No)",
       y = "Number of Accidents") +
  scale_fill_manual(values = c("0" = "#009E73", "1" = "#CC79A7")) +
  theme_minimal(base_size = 14)

#Density Plot of One Sample T Test#

ggplot(acc.m.ind.y_mod, aes(x = accidents)) +
  geom_density(fill = "#0072B2", alpha = 0.6, color = "black") +
  geom_vline(xintercept = 200, color = "red", linetype = "dashed", size = 1.2) +
  labs(title = "One-Sample T-Test: Accidents Distribution (Monthly)",
       subtitle = "Dashed Line = Mean Hypothesis (200)",
       x = "Number of Accidents",
       y = "Density") +
  theme_minimal(base_size = 14)

#Boxplot of Wilcoxon Test#

ggplot(acc.m.ind.y_mod, aes(x = "Accidents vs Fatalities", y = accidents - fatalities)) +
  geom_boxplot(fill = "#E69F00", alpha = 0.6) +
  labs(title = "Wilcoxon Test: Accidents vs Fatalities (Monthly)",
       y = "Difference (Accidents - Fatalities)") +
  theme_minimal(base_size = 14)

#Boxplot of Kruskal Wallis Test#

ggplot(acc.m.ind.y_mod, aes(x = as.factor(season_numeric), y = accidents, fill = as.factor(season_numeric))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Kruskal-Wallis Test: Accidents by Season (Monthly)",
       x = "Season",
       y = "Number of Accidents") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#E69F00")) +
  theme_minimal(base_size = 14)

#Heatmap of Fisher's Exact Test#

fisher_data.acc.m <- as.data.frame(table(acc.m.ind.y_mod$is_summer, acc.m.ind.y_mod$is_holiday))
ggplot(fisher_data.acc.m, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Fisher's Exact Test Heatmap (Monthly Data)",
       x = "Summer (1 = Yes, 0 = No)",
       y = "Holiday (1 = Yes, 0 = No)",
       fill = "Frequency") +
  theme_minimal(base_size = 14)

#Turkey Post Hoc Test Results#

anova_test.acc.m <- aov(accidents ~ as.factor(season_numeric), data = acc.m.ind.y_mod)
tukey_results.acc.m <- TukeyHSD(anova_test.acc.m)
if (!is.null(tukey_results.acc.m$`as.factor(season_numeric)`)) {
  tukey_df.acc.m <- as.data.frame(tukey_results.acc.m$`as.factor(season_numeric)`)
  colnames(tukey_df.acc.m) <- c("Diff", "Lwr", "Upr", "P_adj")
  tukey_df.acc.m$Comparison <- rownames(tukey_df.acc.m)
  
  ggplot(tukey_df.acc.m, aes(x = Comparison, y = Diff, fill = Comparison)) +
    geom_col(alpha = 0.7) +
    geom_errorbar(aes(ymin = Lwr, ymax = Upr), width = 0.2) +
    labs(title = "Tukey HSD Post-hoc Test Results (Monthly)",
         x = "Season Comparisons",
         y = "Mean Difference") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  print("Error: TukeyHSD results are NULL. Check ANOVA model before running TukeyHSD.")
}

#Dunn Test Hetmap Results#

dunn_results.acc.m <- dunnTest(acc.m.ind.y_mod$accidents, acc.m.ind.y_mod$season_numeric, method = "bonferroni")
dunn_table.acc.m <- dunn_results.acc.m$res
if (!"P.adj" %in% colnames(dunn_table.acc.m)) {
  print("Error: 'P.adjusted' column not found in Dunn test results. Check column names with str(dunn_table.acc.m).")
} else {
  dunn_df.acc.m <- dunn_table.acc.m
  
  ggplot(dunn_df.acc.m, aes(x = Comparison, y = "Dunn Test", fill = P.adj)) +
    geom_tile(color = "black") +
    geom_text(aes(label = round(P.adj, 3)), color = "white", size = 5) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Dunn Post-hoc Test Heatmap (Monthly Data)",
         x = "Comparison",
         y = "",
         fill = "Adjusted P-Value") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#Pairwise Wilcoxon Test Results Heatmap#

pairwise_wilcox_test.acc.m <- pairwise.wilcox.test(acc.m.ind.y_mod$accidents, acc.m.ind.y_mod$season_numeric, p.adjust.method = "bonferroni")
pairwise_wilcox_df.acc.m <- as.data.frame(as.table(as.matrix(pairwise_wilcox_test.acc.m$p.value)))
colnames(pairwise_wilcox_df.acc.m) <- c("Group1", "Group2", "P_Value")
pairwise_wilcox_df.acc.m <- na.omit(pairwise_wilcox_df.acc.m)
ggplot(pairwise_wilcox_df.acc.m, aes(x = Group1, y = Group2, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Pairwise Wilcoxon Test Heatmap (Monthly Data)",
       x = "Group 1",
       y = "Group 2",
       fill = "P-Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Scree Plot and Biplot#

###pca_model.acc.m <- prcomp(select(acc.m.ind.y_mod, where(is.numeric)), center = TRUE, scale. = TRUE)
###fviz_eig(pca_model.acc.m, addlabels = TRUE, ylim = c(0, 100)) +
  ###labs(title = "PCA Scree Plot (Monthly Data)",
      ### x = "Principal Components",
      ### y = "Explained Variance (%)")
###fviz_pca_biplot(pca_model.acc.m, repel = TRUE) +
  ###labs(title = "PCA Biplot (Monthly Data)",
       ###subtitle = "Projection of Variables and Observations")

#K-Means Clustering Plot#

set.seed(42)
numeric_data.acc.m.kmeans <- acc.m.ind.y_mod %>%
  select(where(is.numeric))
numeric_data.acc.m.kmeans <- numeric_data.acc.m.kmeans %>%
  select(where(~ length(unique(na.omit(.))) > 1))
numeric_data.acc.m.kmeans <- numeric_data.acc.m.kmeans %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), mean(., na.rm = TRUE), .)))
if (ncol(numeric_data.acc.m.kmeans) == 0) {
  stop("Error: No valid numeric columns remaining after filtering constant values.")
}
kmeans_result.acc.m <- kmeans(numeric_data.acc.m.kmeans, centers = 3)
fviz_cluster(kmeans_result.acc.m, data = numeric_data.acc.m.kmeans, stand = TRUE) +
  labs(title = "K-Means Clustering Visualization (Monthly Data)",
       subtitle = "3 Cluster Solution")

#Outlier Detection Plot#

acc.m.ind.y_mod$z_score <- scale(acc.m.ind.y_mod$accidents)
ggplot(acc.m.ind.y_mod, aes(x = z_score)) +
  geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "black", alpha = 0.7) +
  geom_vline(xintercept = c(-3, 3), linetype = "dashed", color = "red", size = 1) +
  labs(title = "Outlier Detection using Z-Score (Monthly Data)",
       x = "Z-Score",
       y = "Frequency") +
  theme_minimal(base_size = 14)

#Random Forest Feature Importance Plot#

set.seed(42)
numeric_data.acc.m.rf <- acc.m.ind.y_mod %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), mean(., na.rm = TRUE), .)))  # Replace NA/Inf with mean
if (ncol(numeric_data.acc.m.rf) == 0) {
  stop("Error: No valid numeric columns remaining after cleaning.")
}
target_column <- "accidents" 
features <- setdiff(names(numeric_data.acc.m.rf), target_column)
rf_model.acc.m <- randomForest(as.formula(paste(target_column, "~ .")), data = numeric_data.acc.m.rf, importance = TRUE)
varImpPlot(rf_model.acc.m, main = "Random Forest Feature Importance (Monthly Data)")

#XGBoost Feature Importance Plot#

xgb_data.acc.m <- as.matrix(numeric_data.acc.m.rf[, features])
xgb_label.acc.m <- numeric_data.acc.m.rf[[target_column]]
xgb_model.acc.m <- xgboost(data = xgb_data.acc.m, label = xgb_label.acc.m, nrounds = 100, objective = "reg:squarederror")
importance_matrix.acc.m <- xgb.importance(model = xgb_model.acc.m)
xgb.plot.importance(importance_matrix.acc.m, main = "XGBoost Feature Importance (Monthly Data)")

#Boxplots of Kruskal Wallis tests#

plot_kruskal_monthly <- function(var, var_name) {
  ggplot(acc.m.ind.y_mod, aes(x = as.factor(cut(!!sym(var), breaks = 3)), y = accidents, fill = as.factor(cut(!!sym(var), breaks = 3)))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Kruskal-Wallis Test: Accidents by", var_name, "(Monthly)"),
         x = var_name,
         y = "Number of Accidents") +
    scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_gdp.acc.m <- plot_kruskal_monthly("gdp_per_capita", "GDP Per Capita")
plot_unemployment.acc.m <- plot_kruskal_monthly("unemployment", "Unemployment Rate")
plot_gdp.acc.m
plot_unemployment.acc.m
kruskal_gdp.acc.m <- kruskal.test(accidents ~ cut(gdp_per_capita, breaks = 3), data = acc.m.ind.y_mod)
kruskal_unemployment.acc.m <- kruskal.test(accidents ~ cut(unemployment, breaks = 3), data = acc.m.ind.y_mod)
kruskal_results.acc.m <- data.frame(
  Variable = c("GDP Per Capita", "Unemployment"),
  P_Value = c(kruskal_gdp.acc.m$p.value,
              kruskal_unemployment.acc.m$p.value)
)
ggplot(kruskal_results.acc.m, aes(x = "", y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Kruskal-Wallis Test p-Values (Monthly Data)",
       x = "",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())

#Time Series Indicating Plots and Poisson Model#

adf_test_acc.m <- adf.test(acc.m.ind.y_mod$accidents)
adf_test_fatal.m <- adf.test(acc.m.ind.y_mod$fatalities)
adf_results.acc.m <- data.frame(
  Variable = c("Accidents", "Fatalities"),
  P_Value = c(adf_test_acc.m$p.value, adf_test_fatal.m$p.value)
)
ggplot(adf_results.acc.m, aes(x = "", y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "ADF Test Results (Stationarity Check - Monthly Data)",
       x = "",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())
poisson_model.acc.m <- glm(accidents ~ vehicles + unemployment + inflation_rate, 
                           data = acc.m.ind.y_mod, family = poisson)
acc.m.ind.y_mod$predicted_poisson <- predict(poisson_model.acc.m, type = "response")
ggplot(acc.m.ind.y_mod, aes(x = predicted_poisson, y = accidents)) +
  geom_point(alpha = 0.5, color = "#0072B2") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Poisson Regression: Predicted vs Actual Accidents (Monthly Data)",
       x = "Predicted Accidents",
       y = "Actual Accidents") +
  theme_minimal(base_size = 14)
residuals_poisson.acc.m <- data.frame(
  Fitted = poisson_model.acc.m$fitted.values,
  Residuals = residuals(poisson_model.acc.m, type = "pearson")
)
ggplot(residuals_poisson.acc.m, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "#D55E00") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Poisson Regression: Residuals vs Fitted (Monthly Data)",
       x = "Fitted Values",
       y = "Pearson Residuals") +
  theme_minimal(base_size = 14)
acf_plot <- ggAcf(acc.m.ind.y_mod$accidents, lag.max = 30) +
  ggtitle("Autocorrelation Function (ACF) of Accidents (Monthly Data)") +
  theme_minimal(base_size = 14)
pacf_plot <- ggPacf(acc.m.ind.y_mod$accidents, lag.max = 30) +
  ggtitle("Partial Autocorrelation Function (PACF) of Accidents (Monthly Data)") +
  theme_minimal(base_size = 14)
acf_plot
pacf_plot

##Yearly Accidents and Yearly Indicators Dataset##


#Time Series Plots of Accidents and Fatalities through the Years#

ggplot(acc.y.ind.y_mod, aes(x = year_date, y = accidents)) +
  geom_line(color = "#0072B2", size = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#0072B2", linetype = "solid") +
  labs(title = "Yearly Road Accidents in Greece",
       subtitle = "Trends in Yearly Accidents Over the Years",
       x = "Year",
       y = "Accidents") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.y.ind.y_mod, aes(x = year_date, y = fatalities)) +
  geom_line(color = "#D55E00", size = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "#D55E00", linetype = "solid") +
  labs(title = "Yearly Road Fatalities in Greece",
       subtitle = "Trends in Yearly Fatalities Over the Years",
       x = "Year",
       y = "Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Histograms of Accidents and Fatalities Count through the Years#

ggplot(acc.y.ind.y_mod, aes(x = accidents)) +
  geom_histogram(bins = 50, fill = "#0072B2", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Yearly Road Accidents",
       subtitle = "Distribution of Yearly Accident Counts",
       x = "Yearly Accidents",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.y.ind.y_mod, aes(x = fatalities)) +
  geom_histogram(bins = 50, fill = "#D55E00", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Yearly Road Fatalities",
       subtitle = "Distribution of Yearly Fatality Counts",
       x = "Yearly Fatalities",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Scatter Plot of Accidents and Fatalities through the Years#

ggplot(acc.y.ind.y_mod, aes(x = accidents, y = fatalities)) +
  geom_point(alpha = 0.5, color = "#0072B2") +  
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +  
  labs(title = "Scatter Plot of Yearly Accidents vs. Fatalities",
       subtitle = "Examining the Relationship Between Yearly Accidents and Fatalities",
       x = "Yearly Accidents",
       y = "Yearly Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Plots of Sums of Accidents and Fatalities through the Years#

ggplot(acc.y.ind.y_mod, aes(x = year, y = accidents, group = 1)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_point(color = "#0072B2", size = 2) +
  labs(title = "Total Yearly Road Accidents (Yearly Data)",
       subtitle = "Summed Yearly Accidents for Each Year",
       x = "Year",
       y = "Total Accidents") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.y.ind.y_mod, aes(x = year, y = fatalities, group = 1)) +
  geom_line(color = "#D55E00", size = 1.2) +
  geom_point(color = "#D55E00", size = 2) +
  labs(title = "Total Yearly Road Fatalities (Yearly Data)",
       subtitle = "Summed Yearly Fatalities for Each Year",
       x = "Year",
       y = "Total Fatalities") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Time Series Plots of Rolling Averages, Rolling Sums and Lagged Values of Yearly Accidents#

ggplot(acc.y.ind.y_mod, aes(x = year_date)) +
  geom_line(aes(y = Rolling_Avg_3y_a, color = "3-Year Rolling Avg"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_5y_a, color = "5-Year Rolling Avg"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_10y_a, color = "10-Year Rolling Avg"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Averages of Yearly Accidents",
       subtitle = "3, 5, and 10 Year Moving Averages",
       x = "Year",
       y = "Rolling Average",
       color = "Legend") +
  scale_color_manual(values = c("3-Year Rolling Avg" = "#0072B2", 
                                "5-Year Rolling Avg" = "#E69F00",
                                "10-Year Rolling Avg" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.y.ind.y_mod, aes(x = year_date)) +
  geom_line(aes(y = RollingSum_3y_a, color = "3-Year Rolling Sum"), size = 1, alpha = 0.8) +
  geom_line(aes(y = RollingSum_5y_a, color = "5-Year Rolling Sum"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = RollingSum_10y_a, color = "10-Year Rolling Sum"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Sums of Yearly Accidents",
       subtitle = "3, 5, and 10 Year Rolling Sums",
       x = "Year",
       y = "Rolling Sum",
       color = "Legend") +
  scale_color_manual(values = c("3-Year Rolling Sum" = "#0072B2", 
                                "5-Year Rolling Sum" = "#E69F00",
                                "10-Year Rolling Sum" = "#D55E00"))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.y.ind.y_mod, aes(x = year_date)) +
  geom_line(aes(y = Accidents_Lag1, color = "Lag 1 Year"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag2, color = "Lag 2 Years"), size = 1, alpha = 0.6) +  
  geom_line(aes(y = Accidents_Lag3, color = "Lag 3 Years"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag5, color = "Lag 5 Years"), size = 1, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(y = Accidents_Lag10, color = "Lag 10 Years"), size = 1, alpha = 0.6) +
  labs(title = "Lagged Yearly Accidents",
       subtitle = "Comparing Different Yearly Lag Periods",
       x = "Year",
       y = "Lagged Accidents",
       color = "Legend") +
  scale_color_manual(values = c("Lag 1 Year" = "#0072B2", 
                                "Lag 2 Years" = "#E69F00",
                                "Lag 3 Years" = "#44cb2f",
                                "Lag 5 Years" = "#dc6bcb",
                                "Lag 10 Years" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Time Series Plots of Rolling Averages, Rolling Sums and Lagged Values of Yearly Fatalities#

ggplot(acc.y.ind.y_mod, aes(x = year_date)) +
  geom_line(aes(y = Rolling_Avg_3y_f, color = "3-Year Rolling Avg"), size = 1, alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_5y_f, color = "5-Year Rolling Avg"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Rolling_Avg_10y_f, color = "10-Year Rolling Avg"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Averages of Yearly Fatalities",
       subtitle = "3, 5, and 10 Year Moving Averages",
       x = "Year",
       y = "Rolling Average",
       color = "Legend") +
  scale_color_manual(values = c("3-Year Rolling Avg" = "#0072B2", 
                                "5-Year Rolling Avg" = "#E69F00",
                                "10-Year Rolling Avg" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.y.ind.y_mod, aes(x = year_date)) +
  geom_line(aes(y = RollingSum_3y_f, color = "3-Year Rolling Sum"), size = 1, alpha = 0.8) +
  geom_line(aes(y = RollingSum_5y_f, color = "5-Year Rolling Sum"), size = 1, linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = RollingSum_10y_f, color = "10-Year Rolling Sum"), size = 1, alpha = 0.8) +
  labs(title = "Rolling Sums of Yearly Fatalities",
       subtitle = "3, 5, and 10 Year Rolling Sums",
       x = "Year",
       y = "Rolling Sum",
       color = "Legend") +
  scale_color_manual(values = c("3-Year Rolling Sum" = "#0072B2", 
                                "5-Year Rolling Sum" = "#E69F00",
                                "10-Year Rolling Sum" = "#D55E00"))+
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )
ggplot(acc.y.ind.y_mod, aes(x = year_date)) +
  geom_line(aes(y = Fatalities_Lag1, color = "Lag 1 Year"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag2, color = "Lag 2 Years"), size = 1, alpha = 0.6) +  
  geom_line(aes(y = Fatalities_Lag3, color = "Lag 3 Years"), size = 1, alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag5, color = "Lag 5 Years"), size = 1, linetype = "dashed", alpha = 0.6) +
  geom_line(aes(y = Fatalities_Lag10, color = "Lag 10 Years"), size = 1, alpha = 0.6) +
  labs(title = "Lagged Yearly Fatalities",
       subtitle = "Comparing Different Yearly Lag Periods",
       x = "Year",
       y = "Lagged Fatalities",
       color = "Legend") +
  scale_color_manual(values = c("Lag 1 Year" = "#0072B2", 
                                "Lag 2 Years" = "#E69F00",
                                "Lag 3 Years" = "#44cb2f",
                                "Lag 5 Years" = "#dc6bcb",
                                "Lag 10 Years" = "#009E73")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

#Plots of Yearly Accident Growth Rate, Fatality Growth Rate, Fatality Rate, Accidents Change Rate, Fatalities Change Rate, Fatalities Per Capita, Accidents Per Vehicle#

ggplot(acc.y.ind.y_mod, aes(x = year_date, y = AccidentGrowthRate)) +
  geom_line(color = "#0072B2", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#0072B2", fill = "#0072B2", alpha = 0.2, size = 1.2) +
  labs(title = "Yearly Accident Growth Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Year",
       y = "Accident Growth Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.y.ind.y_mod, aes(x = year_date, y = FatalityGrowthRate)) +
  geom_line(color = "#D55E00", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#D55E00", fill = "#D55E00", alpha = 0.2, size = 1.2) +
  labs(title = "Yearly Fatality Growth Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Year",
       y = "Fatality Growth Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.y.ind.y_mod, aes(x = year_date, y = FatalityRate)) +
  geom_line(color = "#009E73", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#009E73", fill = "#009E73", alpha = 0.2, size = 1.2) +
  labs(title = "Yearly Fatality Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Year",
       y = "Fatality Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.y.ind.y_mod, aes(x = year_date, y = AccidentChangeRate)) +
  geom_line(color = "#E69F00", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E69F00", fill = "#E69F00", alpha = 0.2, size = 1.2) +
  labs(title = "Yearly Accident Change Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Year",
       y = "Accident Change Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.y.ind.y_mod, aes(x = year_date, y = FatalityChangeRate)) +
  geom_line(color = "#CC79A7", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#CC79A7", fill = "#CC79A7", alpha = 0.2, size = 1.2) +
  labs(title = "Yearly Fatality Change Rate Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Year",
       y = "Fatality Change Rate") +
  theme_minimal(base_size = 14)
ggplot(acc.y.ind.y_mod, aes(x = year_date, y = FatalitiesPerCapita)) +
  geom_line(color = "#56B4E9", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#56B4E9", fill = "#56B4E9", alpha = 0.2, size = 1.2) +
  labs(title = "Yearly Fatalities Per Capita Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Year",
       y = "Fatalities Per Capita") +
  theme_minimal(base_size = 14)
ggplot(acc.y.ind.y_mod, aes(x = year_date, y = AccidentsPerVehicle)) +
  geom_line(color = "#009E73", size = 0.8, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E69F00", fill = "#E69F00", alpha = 0.2, size = 1.2) +
  labs(title = "Yearly Accidents Per Vehicle Over Multiple Years",
       subtitle = "Raw Data & Smoothed Trend",
       x = "Year",
       y = "Accidents Per Vehicle") +
  theme_minimal(base_size = 14)

#Histograms for All Variables#

numeric_data.acc.y.hplots <- acc.y.ind.y_mod %>% select(where(is.numeric))
long_data.acc.y.hplots <- numeric_data.acc.y.hplots %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
var_names.acc.y.hplots <- unique(long_data.acc.y.hplots$Variable)
var_groups.acc.y.hplots <- split(var_names.acc.y.hplots, ceiling(seq_along(var_names.acc.y.hplots) / 8))
plot_histograms3 <- function(var_subset) {
  ggplot(long_data.acc.y.hplots %>% filter(Variable %in% var_subset), aes(x = Value)) +
    geom_histogram(bins = 50, fill = "#0072B2", color = "black", alpha = 0.7) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) +  
    labs(title = "Histograms of Numeric Variables (Yearly Data)",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Value",
         y = "Frequency") +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
histogram_plots.acc.y <- lapply(var_groups.acc.y.hplots, plot_histograms3)
histogram_plots.acc.y

#Plots of Means of All Variables#

numeric_data.acc.y.mplots <- acc.y.ind.y_mod %>%
  select(year, where(is.numeric)) %>%
  mutate(year = as.factor(year))
long_data.acc.y.mplots <- numeric_data.acc.y.mplots %>%
  pivot_longer(cols = -year, names_to = "Variable", values_to = "Value")
var_names.acc.y.mplots <- unique(long_data.acc.y.mplots$Variable)
var_groups.acc.y.mplots <- split(var_names.acc.y.mplots, ceiling(seq_along(var_names.acc.y.mplots) / 8))
plot_means3 <- function(var_subset) {
  ggplot(long_data.acc.y.mplots %>% filter(Variable %in% var_subset), aes(x = year, y = Value, group = Variable, color = Variable)) +
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    facet_wrap(~ Variable, scales = "free", ncol = 4) + 
    labs(title = "Yearly Mean of Numeric Variables",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Year",
         y = "Mean Value") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
mean_plots.acc.y.mplots <- lapply(var_groups.acc.y.mplots, plot_means3)
mean_plots.acc.y.mplots

#Time Series Plots of All variables#

numeric_data.acc.y.tsplots <- acc.y.ind.y_mod %>%
  select(year_date, where(is.numeric)) %>%
  mutate(year_date = as.Date(year_date))
long_data.acc.y.tsplots <- numeric_data.acc.y.tsplots %>%
  pivot_longer(cols = -year_date, names_to = "Variable", values_to = "Value")
var_names.acc.y.tsplots <- unique(long_data.acc.y.tsplots$Variable)
var_groups.acc.y.tsplots <- split(var_names.acc.y.tsplots, ceiling(seq_along(var_names.acc.y.tsplots) / 8))
plot_time_series3 <- function(var_subset) {
  ggplot(long_data.acc.y.tsplots %>% filter(Variable %in% var_subset), aes(x = year_date, y = Value, color = Variable)) +
    geom_line(size = 0.8, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, size = 1, alpha = 0.5) +  
    facet_wrap(~ Variable, scales = "free", ncol = 4) + 
    labs(title = "Time Series of Numeric Variables Through the Years",
         subtitle = paste("Variables:", paste(var_subset, collapse = ", ")),
         x = "Year",
         y = "Value") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",  
      strip.text = element_text(size = 10, face = "bold"),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1)  
    )
}
time_series_plots.acc.y.tsplots <- lapply(var_groups.acc.y.tsplots, plot_time_series3)
time_series_plots.acc.y.tsplots

#Density Plot of Monthly Accidents and Fatalities#

ggplot(acc.y.ind.y_mod) +
  geom_density(aes(x = accidents, fill = "Accidents"), alpha = 0.5, color = "black") +
  geom_density(aes(x = fatalities, fill = "Fatalities"), alpha = 0.5, color = "black") +
  labs(title = "Density Plot of Yearly Accidents and Fatalities",
       subtitle = "Shows the Probability Distribution of Yearly Data",
       x = "Count",
       y = "Density",
       fill = "Metric") +
  scale_fill_manual(values = c("Accidents" = "#0072B2", "Fatalities" = "#D55E00")) +
  theme_minimal(base_size = 14)

#Percentiles Plots of Accidents and Fatalities per Year#

percentile_data.acc.y <- acc.y.ind.y_mod %>%
  group_by(year) %>%
  summarise(
    P10_Accidents = quantile(accidents, probs = 0.10, na.rm = TRUE),
    P25_Accidents = quantile(accidents, probs = 0.25, na.rm = TRUE),
    P50_Accidents = quantile(accidents, probs = 0.50, na.rm = TRUE),  
    P75_Accidents = quantile(accidents, probs = 0.75, na.rm = TRUE),
    P90_Accidents = quantile(accidents, probs = 0.90, na.rm = TRUE),
    P10_Fatalities = quantile(fatalities, probs = 0.10, na.rm = TRUE),
    P25_Fatalities = quantile(fatalities, probs = 0.25, na.rm = TRUE),
    P50_Fatalities = quantile(fatalities, probs = 0.50, na.rm = TRUE),  
    P75_Fatalities = quantile(fatalities, probs = 0.75, na.rm = TRUE),
    P90_Fatalities = quantile(fatalities, probs = 0.90, na.rm = TRUE)
  )
ggplot(percentile_data.acc.y, aes(x = year)) +
  geom_line(aes(y = P10_Accidents, color = "P10 Accidents", group = 1), size = 1) +
  geom_line(aes(y = P50_Accidents, color = "P50 Accidents (Median)", group = 1), size = 1.2) +
  geom_line(aes(y = P90_Accidents, color = "P90 Accidents", group = 1), size = 1) +
  geom_line(aes(y = P10_Fatalities, color = "P10 Fatalities", group = 1), size = 1, linetype = "dashed") +
  geom_line(aes(y = P50_Fatalities, color = "P50 Fatalities (Median)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = P90_Fatalities, color = "P90 Fatalities", group = 1), size = 1, linetype = "dashed") +
  labs(title = "Yearly Percentiles of Accidents and Fatalities",
       subtitle = "Comparison of 10th, 50th, and 90th Percentiles Over the Years",
       x = "Year",
       y = "Percentile Values",
       color = "Percentile") +
  scale_color_manual(values = c(
    "P10 Accidents" = "#0072B2",
    "P50 Accidents (Median)" = "#005293",
    "P90 Accidents" = "#003366",
    "P10 Fatalities" = "#D55E00",
    "P50 Fatalities (Median)" = "#A53D00",
    "P90 Fatalities" = "#732000"
  )) +
  theme_minimal(base_size = 14)

#Simple and Multiple Linear Regression Plots#

simple_lm.acc.y <- lm(accidents ~ vehicles, data = acc.y.ind.y_mod)
ggplot(acc.y.ind.y_mod, aes(x = vehicles, y = accidents)) +
  geom_point(alpha = 0.5, color = "#0072B2") +  
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.3) +  
  labs(title = "Simple Linear Regression: Accidents vs Vehicles (Yearly)",
       subtitle = "Fitted Line with Confidence Interval",
       x = "Number of Vehicles",
       y = "Number of Accidents") +
  theme_minimal(base_size = 14)
plot_multi_lm_yearly <- function(var) {
  ggplot(acc.y.ind.y_mod, aes_string(x = var, y = "accidents")) +
    geom_point(alpha = 0.5, color = "#D55E00") +  
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.3) +  
    labs(title = paste("Multiple Regression: Accidents vs", var, "(Yearly)"),
         subtitle = "Fitted Line with Confidence Interval",
         x = var,
         y = "Number of Accidents") +
    theme_minimal(base_size = 14)
}
plot_vehicles.acc.y <- plot_multi_lm_yearly("vehicles")
plot_unemployment.acc.y <- plot_multi_lm_yearly("unemployment")
plot_inflation.acc.y <- plot_multi_lm_yearly("inflation_rate")
plot_vehicles.acc.y
plot_unemployment.acc.y
plot_inflation.acc.y
multi_lm.acc.y <- lm(accidents ~ vehicles + unemployment + inflation_rate, data = acc.y.ind.y_mod)
plot_diagnostics_lm_yearly <- function(model, model_name) {
  diagnostics <- fortify(model)
  p1 <- ggplot(diagnostics, aes(.fitted, .resid)) +
    geom_point(color = "#0072B2", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste(model_name, "- Residuals vs Fitted (Yearly)"),
         x = "Fitted Values",
         y = "Residuals") +
    theme_minimal()
  p2 <- ggplot(diagnostics, aes(sample = .stdresid)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste(model_name, "- Q-Q Plot (Yearly)"),
         x = "Theoretical Quantiles",
         y = "Standardized Residuals") +
    theme_minimal()
  p3 <- ggplot(diagnostics, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point(color = "#E69F00", alpha = 0.5) +
    geom_smooth(method = "loess", color = "red") +
    labs(title = paste(model_name, "- Scale-Location Plot (Yearly)"),
         x = "Fitted Values",
         y = "√|Standardized Residuals|") +
    theme_minimal()
  p4 <- ggplot(diagnostics, aes(.hat, .stdresid)) +
    geom_point(color = "#D55E00", alpha = 0.5) +
    geom_smooth(method = "loess", color = "red") +
    labs(title = paste(model_name, "- Residuals vs Leverage (Yearly)"),
         x = "Leverage",
         y = "Standardized Residuals") +
    theme_minimal()
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}
plot_diagnostics_lm_yearly(simple_lm.acc.y, "Simple LM (Accidents ~ Vehicles) (Yearly)")
plot_diagnostics_lm_yearly(multi_lm.acc.y, "Multiple LM (Accidents ~ Vehicles + Unemployment + Inflation) (Yearly)")

#Correlation Matrices#

cor_data.acc.y.cm.short<- acc.y.ind.y_mod %>%
  select(year,accidents, fatalities, total_gdp, gdp_per_capita, unemployment, 
         population, vehicles, inflation_rate, 
          AccidentGrowthRate, FatalityGrowthRate, FatalityRate,
         AccidentChangeRate, FatalityChangeRate, AccidentsPerCapita, FatalitiesPerCapita, AccidentsPerVehicle, AccidentsUnemployment, FatalitiesInflation, 
         CumulativeAccidents, CumulativeFatalities)
numeric_data.acc.y.cm.short <- cor_data.acc.y.cm.short %>% select(where(is.numeric))
cor_pearson.acc.y.cm.short <- cor(numeric_data.acc.y.cm.short, use = "pairwise.complete.obs", method = "pearson")
cor_spearman.acc.y.cm.short <- cor(numeric_data.acc.y.cm.short, use = "pairwise.complete.obs", method = "spearman")
cor_kendall.acc.y.cm.short <- cor(numeric_data.acc.y.cm.short, use = "pairwise.complete.obs", method = "kendall")
cov_matrix.acc.y.cm.short <- cov(numeric_data.acc.y.cm.short, use = "pairwise.complete.obs")
melted_cor_pearson.acc.y.cm.short <- melt(cor_pearson.acc.y.cm.short)
ggplot(melted_cor_pearson.acc.y.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Pearson Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cor_spearman.acc.y.cm.short <- melt(cor_spearman.acc.y.cm.short)
ggplot(melted_cor_spearman.acc.y.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Spearman Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cor_kendall.acc.y.cm.short <- melt(cor_kendall.acc.y.cm.short)
ggplot(melted_cor_kendall.acc.y.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Kendall Correlation Matrix (Accidents Data)", x = "", y = "", fill = "Correlation") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
melted_cov.acc.y.cm.short <- melt(cov_matrix.acc.y.cm.short)
ggplot(melted_cov.acc.y.cm.short, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Covariance Matrix (Accidents Data)", x = "", y = "", fill = "Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
numeric_data.acc.y.cm <- acc.y.ind.y_mod %>%
  select(where(is.numeric))
cor_pearson.acc.y.cm  <- cor(numeric_data.acc.y.cm , use = "pairwise.complete.obs", method = "pearson")
cor_spearman.acc.y.cm  <- cor(numeric_data.acc.y.cm , use = "pairwise.complete.obs", method = "spearman")
cor_kendall.acc.y.cm  <- cor(numeric_data.acc.y.cm , use = "pairwise.complete.obs", method = "kendall")
cov_matrix.acc.y.cm  <- cov(numeric_data.acc.y.cm , use = "pairwise.complete.obs")
melted_cor_pearson.acc.y.cm  <- melt(cor_pearson.acc.y.cm)
melted_cor_spearman.acc.y.cm  <- melt(cor_spearman.acc.y.cm)
melted_cor_kendall.acc.y.cm  <- melt(cor_kendall.acc.y.cm)
melted_cov.acc.y.cm  <- melt(cov_matrix.acc.y.cm)
plot_heatmap_yearly <- function(data, title) {
  ggplot(data, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab") +
    labs(title = title, x = "", y = "", fill = "Value") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}
plot_heatmap_yearly(melted_cor_pearson.acc.y.cm , "Pearson Correlation Matrix (Yearly)")
plot_heatmap_yearly(melted_cor_spearman.acc.y.cm , "Spearman Correlation Matrix (Yearly)")
plot_heatmap_yearly(melted_cor_kendall.acc.y.cm , "Kendall Correlation Matrix (Yearly)")
plot_heatmap_yearly(melted_cov.acc.y.cm , "Covariance Matrix (Yearly)")

#Shapiro Wilk and Kolmogorov-Smirnov Test#

numeric_data.acc.y.swplot <- acc.y.ind.y_mod %>% select(where(is.numeric))
numeric_data.acc.y.swplot <- numeric_data.acc.y.swplot %>%
  select(where(~ length(unique(na.omit(.))) > 1))
shapiro_results.acc.y <- sapply(numeric_data.acc.y.swplot, function(x) {
  if (length(x) <= 5000) {
    if (length(unique(na.omit(x))) > 1) {
      shapiro.test(x)$p.value
    } else {
      NA  
    }
  } else {
    NA  
  }
})
ks_results.acc.y <- sapply(numeric_data.acc.y.swplot, function(x) {
  if (length(unique(na.omit(x))) > 1) {
    ks.test(x, "pnorm", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))$p.value
  } else {
    NA  
  }
})
normality_data.acc.y.swplot <- data.frame(
  Variable = names(shapiro_results.acc.y),
  Shapiro_P = shapiro_results.acc.y,
  KS_P = ks_results.acc.y
) %>%
  pivot_longer(cols = -Variable, names_to = "Test", values_to = "P_Value")
ggplot(normality_data.acc.y.swplot, aes(x = Test, y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Shapiro-Wilk & KS Test P-Values (Yearly Data)",
       x = "Test",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14)

#Density Plot of One Sample T Test#

ggplot(acc.m.ind.y_mod, aes(x = accidents)) +
  geom_density(fill = "#0072B2", alpha = 0.6, color = "black") +
  geom_vline(xintercept = 1500, color = "red", linetype = "dashed", size = 1.2) +
  labs(title = "One-Sample T-Test: Accidents Distribution (Monthly)",
       subtitle = "Dashed Line = Mean Hypothesis (200)",
       x = "Number of Accidents",
       y = "Density") +
  theme_minimal(base_size = 14)

#Boxplot of Wilcoxon Test#

ggplot(acc.y.ind.y_mod, aes(x = "Accidents vs Fatalities", y = accidents - fatalities)) +
  geom_boxplot(fill = "#E69F00", alpha = 0.6) +
  labs(title = "Wilcoxon Test: Accidents vs Fatalities (Yearly)",
       y = "Difference (Accidents - Fatalities)") +
  theme_minimal(base_size = 14)

#Scree Plot and Biplot#

###pca_model.acc.y <- prcomp(select(acc.y.ind.y_mod, where(is.numeric)), center = TRUE, scale. = TRUE)
###fviz_eig(pca_model.acc.y, addlabels = TRUE, ylim = c(0, 100)) +
###  labs(title = "PCA Scree Plot (Yearly Data)",
###       x = "Principal Components",
###       y = "Explained Variance (%)")
###fviz_pca_biplot(pca_model.acc.y, repel = TRUE) +
###  labs(title = "PCA Biplot (Yearly Data)",
###       subtitle = "Projection of Variables and Observations")

#K-Means Clustering Plot#

set.seed(42)
numeric_data.acc.y.kmeans <- acc.y.ind.y_mod %>%
  select(where(is.numeric))
numeric_data.acc.y.kmeans <- numeric_data.acc.y.kmeans %>%
  select(where(~ length(unique(na.omit(.))) > 1))
numeric_data.acc.y.kmeans <- numeric_data.acc.y.kmeans %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), mean(., na.rm = TRUE), .)))
if (ncol(numeric_data.acc.y.kmeans) == 0) {
  stop("Error: No valid numeric columns remaining after filtering constant values.")
}
kmeans_result.acc.y <- kmeans(numeric_data.acc.y.kmeans, centers = 3)
fviz_cluster(kmeans_result.acc.y, data = numeric_data.acc.y.kmeans, stand = TRUE) +
  labs(title = "K-Means Clustering Visualization (Yearly Data)",
       subtitle = "3 Cluster Solution")

#Outlier Detection Plot#

acc.y.ind.y_mod$z_score <- scale(acc.y.ind.y_mod$accidents)
ggplot(acc.y.ind.y_mod, aes(x = z_score)) +
  geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "black", alpha = 0.7) +
  geom_vline(xintercept = c(-3, 3), linetype = "dashed", color = "red", size = 1) +
  labs(title = "Outlier Detection using Z-Score (Yearly Data)",
       x = "Z-Score",
       y = "Frequency") +
  theme_minimal(base_size = 14)

#Random Forest Feature Importance Plot#

set.seed(42)
numeric_data.acc.y.rf <- acc.y.ind.y_mod %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), mean(., na.rm = TRUE), .)))  # Replace NA/Inf with mean
if (ncol(numeric_data.acc.y.rf) == 0) {
  stop("Error: No valid numeric columns remaining after cleaning.")
}
target_column <- "accidents" 
features <- setdiff(names(numeric_data.acc.y.rf), target_column)
rf_model.acc.y <- randomForest(as.formula(paste(target_column, "~ .")), data = numeric_data.acc.y.rf, importance = TRUE)
varImpPlot(rf_model.acc.y, main = "Random Forest Feature Importance (Yearly Data)")

#XGBoost Feature Importance Plot#

xgb_data.acc.y <- as.matrix(numeric_data.acc.y.rf[, features])
xgb_label.acc.y <- numeric_data.acc.y.rf[[target_column]]
xgb_model.acc.y <- xgboost(data = xgb_data.acc.y, label = xgb_label.acc.y, nrounds = 100, objective = "reg:squarederror")
importance_matrix.acc.y <- xgb.importance(model = xgb_model.acc.y)
xgb.plot.importance(importance_matrix.acc.y, main = "XGBoost Feature Importance (Yearly Data)")


#Boxplots of Kruskal Wallis tests#

plot_kruskal_yearly <- function(var, var_name) {
  ggplot(acc.y.ind.y_mod, aes(x = as.factor(cut(!!sym(var), breaks = 3)), y = accidents, fill = as.factor(cut(!!sym(var), breaks = 3)))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Kruskal-Wallis Test: Accidents by", var_name, "(Yearly)"),
         x = var_name,
         y = "Number of Accidents") +
    scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_gdp.acc.y <- plot_kruskal_yearly("gdp_per_capita", "GDP Per Capita")
plot_unemployment.acc.y <- plot_kruskal_yearly("unemployment", "Unemployment Rate")
plot_gdp.acc.y
plot_unemployment.acc.y
kruskal_gdp.acc.y <- kruskal.test(accidents ~ cut(gdp_per_capita, breaks = 3), data = acc.y.ind.y_mod)
kruskal_unemployment.acc.y <- kruskal.test(accidents ~ cut(unemployment, breaks = 3), data = acc.y.ind.y_mod)
kruskal_results.acc.y <- data.frame(
  Variable = c("GDP Per Capita", "Unemployment"),
  P_Value = c(kruskal_gdp.acc.y$p.value,
              kruskal_unemployment.acc.y$p.value)
)
ggplot(kruskal_results.acc.y, aes(x = "", y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Kruskal-Wallis Test p-Values (Yearly Data)",
       x = "",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())

#Time Series Indicating Plots and Poisson Model#

adf_test_acc.y <- adf.test(acc.y.ind.y_mod$accidents)
adf_test_fatal.y <- adf.test(acc.y.ind.y_mod$fatalities)
adf_results.acc.y <- data.frame(
  Variable = c("Accidents", "Fatalities"),
  P_Value = c(adf_test_acc.y$p.value, adf_test_fatal.y$p.value)
)
ggplot(adf_results.acc.y, aes(x = "", y = Variable, fill = P_Value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(P_Value, 3)), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "ADF Test Results (Stationarity Check - Yearly Data)",
       x = "",
       y = "Variable",
       fill = "P-Value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())
poisson_model.acc.y <- glm(accidents ~ vehicles + unemployment + inflation_rate, 
                           data = acc.y.ind.y_mod, family = poisson)
acc.y.ind.y_mod$predicted_poisson <- predict(poisson_model.acc.y, type = "response")
ggplot(acc.y.ind.y_mod, aes(x = predicted_poisson, y = accidents)) +
  geom_point(alpha = 0.5, color = "#0072B2") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Poisson Regression: Predicted vs Actual Accidents (Yearly Data)",
       x = "Predicted Accidents",
       y = "Actual Accidents") +
  theme_minimal(base_size = 14)
residuals_poisson.acc.y <- data.frame(
  Fitted = poisson_model.acc.y$fitted.values,
  Residuals = residuals(poisson_model.acc.y, type = "pearson")
)
ggplot(residuals_poisson.acc.y, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "#D55E00") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Poisson Regression: Residuals vs Fitted (Yearly Data)",
       x = "Fitted Values",
       y = "Pearson Residuals") +
  theme_minimal(base_size = 14)
acf_plot <- ggAcf(acc.y.ind.y_mod$accidents, lag.max = 30) +
  ggtitle("Autocorrelation Function (ACF) of Accidents (Yearly Data)") +
  theme_minimal(base_size = 14)
pacf_plot <- ggPacf(acc.y.ind.y_mod$accidents, lag.max = 30) +
  ggtitle("Partial Autocorrelation Function (PACF) of Accidents (Yearly Data)") +
  theme_minimal(base_size = 14)
acf_plot
pacf_plot

####Prediction Models of ARIMA and SARIMA in the Weekly Dataset####




#Data Prep#

week_fixes <- c(
  `1232` = 1, `1233` = 2, `1234` = 3, `1235` = 4,
  `1511` = 1, `1512` = 2, `1513` = 3, `1514` = 4,
  `1650` = 1, `1651` = 2, `1652` = 3, `1653` = 4,
  `1654` = 5
)
acc.w.ind.y_mod$week[as.integer(names(week_fixes))] <- week_fixes
acc.w.ind.y_mod
base_weeks <- acc.w.ind.y_mod %>%
  filter(week <= 4) %>%
  group_by(year, month) %>%
  mutate(n_base_weeks = n()) %>%
  ungroup()
extra_all <- acc.w.ind.y_mod %>%
  filter(week > 4) %>%
  group_by(year, month) %>%
  summarise(
    extra_acc = sum(accidents),
    extra_fat = sum(fatalities),
    .groups = "drop"
  )
acc.w.ind.y_4wmod <- base_weeks %>%
  left_join(extra_all, by = c("year", "month")) %>%
  mutate(
    extra_acc = replace_na(extra_acc, 0),
    extra_fat = replace_na(extra_fat, 0),
    acc_base_add = extra_acc %/% n_base_weeks,
    acc_remainder = extra_acc %% n_base_weeks,
    fat_base_add = extra_fat %/% n_base_weeks,
    fat_remainder = extra_fat %% n_base_weeks,
    accidents = accidents + acc_base_add + if_else(week == min(week), acc_remainder, 0L),
    fatalities = fatalities + fat_base_add + if_else(week == min(week), fat_remainder, 0L)
  ) %>%
  select(year, month, week, accidents, fatalities)
acc.w.ind.y_4wmod <- acc.w.ind.y_4wmod %>%
  group_by(year) %>%
  arrange(year, month, week) %>%
  mutate(week_index = row_number()) %>%
  ungroup()

##ARIMA on Regular Accidents##


#Time Series Object Creation#

acc_w_ts <- ts(acc.w.ind.y_4wmod$accidents, start = c(1996, 1), frequency = 48)

#Time Series Plots#

plot(acc_w_ts, col = "steelblue", lwd = 2,
     main = "Weekly Road Accidents (1996–2022)",
     ylab = "Accidents", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(acc_w_ts, lag.max = 48, main = "ACF - Normal Accidents")
Pacf(acc_w_ts, lag.max = 48, main = "PACF - Normal Accidents")

#ADF test#

adf_acc.w <- adf.test(acc_w_ts, alternative = "stationary")
adf_acc.w

#KPSS test#

summary(ur.kpss(acc_w_ts, type = "mu"))

#Normal Differencing Since ADF and KPSS tests disagree (d = 1)#

acc_w_ts_diff <- diff(acc_w_ts, differences = 1)

#Plotting the Differenced Series#

plot(acc_w_ts_diff, col = "steelblue", main = "1st Difference of Normal Accidents")

#ADF test After Differencing#

adf.test(acc_w_ts_diff, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(acc_w_ts_diff, type = "mu"))

#Decompose the Original Series#

plot(decompose(acc_w_ts))
par(mfrow = c(1, 1))  

#Plotting ACF and PACF of the Differenced Time Series#

Acf(acc_w_ts_diff, lag.max = 48, main = "ACF - Normal Accidents (Differenced)")
Pacf(acc_w_ts_diff, lag.max = 48, main = "PACF - Normal Accidents (Differenced)")

#Trying some possible ARIMA Combinations limited to max(p,q) = 3#

train_ts_arima_acc_w <- window(acc_w_ts, end = c(2017, 29))
test_ts_arima_acc_w  <- window(acc_w_ts, start = c(2017, 30))
model_arima_acc_w <- Arima(train_ts_arima_acc_w, order = c(3,1,2))
fc_arima_acc_w <- forecast(model_arima_acc_w, h = length(test_ts_arima_acc_w))
rmse_val_arima_acc_w <- rmse(test_ts_arima_acc_w, fc_arima_acc_w$mean)
mae_val_arima_acc_w  <- mae(test_ts_arima_acc_w, fc_arima_acc_w$mean)
mape_val_arima_acc_w <- mape(test_ts_arima_acc_w, fc_arima_acc_w$mean) * 100
summary(model_arima_acc_w)
cat("AIC:", AIC(model_arima_acc_w), "\n")
cat("BIC:", BIC(model_arima_acc_w), "\n")
cat("RMSE:", rmse_val_arima_acc_w, "\n")
cat("MAE:", mae_val_arima_acc_w, "\n")
cat("MAPE:", mape_val_arima_acc_w, "%\n")
AIC(model_arima_acc_w)
BIC(model_arima_acc_w)

#Best for now is ARIMA(3,1,2), lets see with Auto.Arima#

model_auto_arima_acc_w <- auto.arima(
  train_ts_arima_acc_w,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_arima_acc_w <- forecast(model_auto_arima_acc_w, h = length(test_ts_arima_acc_w))
summary(fc_auto_arima_acc_w)
summary(model_auto_arima_acc_w)
rmse_val_auto_arima_acc_w  <- rmse(test_ts_arima_acc_w, fc_auto_arima_acc_w$mean)
mae_val_auto_arima_acc_w   <- mae(test_ts_arima_acc_w, fc_auto_arima_acc_w$mean)
mape_val_auto_arima_acc_w <- mape(test_ts_arima_acc_w, fc_auto_arima_acc_w$mean) * 100
cat("AIC:", AIC(model_auto_arima_acc_w), "\n")
cat("BIC:", BIC(model_auto_arima_acc_w), "\n")
cat("RMSE:", rmse_val_auto_arima_acc_w, "\n")
cat("MAE:", mae_val_auto_arima_acc_w, "\n")
cat("MAPE:", mape_val_auto_arima_acc_w, "%\n")

#Best for Auto.Arima is ARIMA(4,1,1), lets try both#

model_final_arima_manual_acc_w <- Arima(
  acc_w_ts,               
  order = c(3,1,2),
  include.drift = FALSE    
)
fc_final_arima_manual_acc_w  <- forecast(model_final_arima_manual_acc_w , h = 192)
summary(fc_final_arima_manual_acc_w)
summary(model_final_arima_manual_acc_w)
plot(fc_final_arima_manual_acc_w , main = "Forecast of Accidents until 2026")
forecast_values_arima_manual_acc_w  <- data.frame(fc_final_arima_manual_acc_w)
head(forecast_values_arima_manual_acc_w)
model_final_arima_auto_acc_w <- Arima(
  acc_w_ts,               
  order = c(4, 1, 1),
  include.drift = FALSE    
)
fc_final_arima_auto_acc_w <- forecast(model_final_arima_auto_acc_w, h = 192)
summary(fc_final_arima_auto_acc_w)
summary(model_final_arima_auto_acc_w)
plot(fc_final_arima_auto_acc_w, main = "Forecast of Accidents until 2026")
forecast_values_arima_auto_acc_w <- data.frame(fc_final_arima_auto_acc_w)
head(forecast_values_arima_auto_acc_w)

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_arima_manual_acc_w), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_manual_acc_w$coef))
Box.test(residuals(model_final_arima_manual_acc_w), lag=12)
checkresiduals(model_final_arima_manual_acc_w) 
Box.test(residuals(model_final_arima_auto_acc_w), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_auto_acc_w$coef))
checkresiduals(model_final_arima_auto_acc_w) 
Box.test(residuals(model_final_arima_auto_acc_w), lag=12)

##ARIMA on Regular Fatalities##


#Time Series Object Creation#

fat_w_ts <- ts(acc.w.ind.y_4wmod$fatalities, start = c(1996, 1), frequency = 48)

#Time Series Plot#

plot(fat_w_ts, col = "darkred", lwd = 2,
     main = "Weekly Road Fatalities (1996–2022)",
     ylab = "Fatalities", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(fat_w_ts, lag.max = 48, main = "ACF - Normal Fatalities")
Pacf(fat_w_ts, lag.max = 48, main = "PACF - Normal Fatalities")

#ADF test#

adf_fat.w <- adf.test(fat_w_ts, alternative = "stationary")
adf_fat.w

#KPSS test#

summary(ur.kpss(fat_w_ts, type = "mu"))

#Normal Differencing Since ADF and KPSS tests disagree (d = 1)#

fat_w_ts_diff <- diff(fat_w_ts, differences = 1)

#Plotting the Differenced Series#

plot(fat_w_ts_diff, col = "darkred", main = "1st Difference of Normal Fatalities")

#ADF test After Differencing#

adf.test(fat_w_ts_diff, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(fat_w_ts_diff, type = "mu"))

#Decompose the Original Series#

plot(decompose(fat_w_ts))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(fat_w_ts_diff, lag.max = 48, main = "ACF - Normal Fatalities (differenced)")
Pacf(fat_w_ts_diff, lag.max = 48, main = "PACF - Normal Fatalities (differenced)")

#Trying some possible ARIMA Combinations limited to max(p,q) = 4#

train_ts_arima_fat_w <- window(fat_w_ts, end = c(2017, 29))
test_ts_arima_fat_w  <- window(fat_w_ts, start = c(2017, 30))
model_arima_fat_w <- Arima(train_ts_arima_fat_w, order = c(1,1,2))
fc_arima_fat_w <- forecast(model_arima_fat_w, h = length(test_ts_arima_fat_w))
rmse_val_arima_fat_w <- rmse(test_ts_arima_fat_w, fc_arima_fat_w$mean)
mae_val_arima_fat_w  <- mae(test_ts_arima_fat_w, fc_arima_fat_w$mean)
mape_val_arima_fat_w <- mape(test_ts_arima_fat_w, fc_arima_fat_w$mean) * 100
summary(model_arima_fat_w)
cat("AIC:", AIC(model_arima_fat_w), "\n")
cat("BIC:", BIC(model_arima_fat_w), "\n")
cat("RMSE:", rmse_val_arima_fat_w, "\n")
cat("MAE:", mae_val_arima_fat_w, "\n")
cat("MAPE:", mape_val_arima_fat_w, "%\n")
AIC(model_arima_fat_w)
BIC(model_arima_fat_w)

#Best for now is ARIMA(1,1,2), lets see with Auto.Arima#

model_auto_arima_fat_w <- auto.arima(
  train_ts_arima_fat_w,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_arima_fat_w <- forecast(model_auto_arima_fat_w, h = length(test_ts_arima_fat_w))
summary(fc_auto_arima_fat_w)
summary(model_auto_arima_fat_w)
rmse_val_auto_arima_fat_w  <- rmse(test_ts_arima_fat_w, fc_auto_arima_fat_w$mean)
mae_val_auto_arima_fat_w   <- mae(test_ts_arima_fat_w, fc_auto_arima_fat_w$mean)
mape_val_auto_arima_fat_w <- mape(test_ts_arima_fat_w, fc_auto_arima_fat_w$mean) * 100
cat("AIC:", AIC(model_auto_arima_fat_w), "\n")
cat("BIC:", BIC(model_auto_arima_fat_w), "\n")
cat("RMSE:", rmse_val_auto_arima_fat_w, "\n")
cat("MAE:", mae_val_auto_arima_fat_w, "\n")
cat("MAPE:", mape_val_auto_arima_fat_w, "%\n")

#Best for Auto.Arima is ARIMA(4,1,1) with drift, lets try both#

model_final_arima_manual_fat_w <- Arima(
  fat_w_ts,               
  order = c(1,1,2),
  include.drift = FALSE    
)
fc_final_arima_manual_fat_w  <- forecast(model_final_arima_manual_fat_w , h = 192)
summary(fc_final_arima_manual_fat_w)
summary(model_final_arima_manual_fat_w)
plot(fc_final_arima_manual_fat_w , main = "Forecast of Fatalities until 2026")
forecast_values_arima_manual_fat_w  <- data.frame(fc_final_arima_manual_fat_w)
head(forecast_values_arima_manual_fat_w)
model_final_arima_auto_fat_w <- Arima(
  acc_w_ts,               
  order = c(4, 1, 1),
  include.drift = TRUE    
)
fc_final_arima_auto_fat_w <- forecast(model_final_arima_auto_fat_w, h = 192)
summary(fc_final_arima_auto_fat_w)
summary(model_final_arima_auto_fat_w)
plot(fc_final_arima_auto_fat_w, main = "Forecast of Fatalities until 2026")
forecast_values_arima_auto_fat_w <- data.frame(fc_final_arima_auto_fat_w)
head(forecast_values_arima_auto_fat_w)

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_arima_manual_fat_w), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_manual_fat_w$coef))
checkresiduals(model_final_arima_manual_fat_w) 
Box.test(residuals(model_final_arima_manual_fat_w), lag=12)
Box.test(residuals(model_final_arima_auto_fat_w), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_auto_fat_w$coef))
checkresiduals(model_final_arima_auto_fat_w) 
Box.test(residuals(model_final_arima_auto_fat_w), lag=12)

##ARIMA on Log Accidents##


#Time Series Object Creation#

acc_w_ts_log <- log1p(acc_w_ts)

#Time Series Plot#

plot(acc_w_ts_log, col = "steelblue", lwd = 2,
     main = "Weekly Road Accidents (1996–2022)",
     ylab = "Accidents", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(acc_w_ts_log, lag.max = 48, main = "ACF - Log Accidents")
Pacf(acc_w_ts_log, lag.max = 48, main = "PACF - Log Accidents")

#ADF test#

adf_acc.w_log <- adf.test(acc_w_ts_log, alternative = "stationary")
adf_acc.w_log

#KPSS test#

summary(ur.kpss(acc_w_ts_log, type = "mu"))

#Normal Differencing Since ADF and KPSS tests disagree (d = 1)#

acc_w_ts_diff_log <- diff(acc_w_ts_log, differences = 1)

#Plotting the Differenced Series#

plot(acc_w_ts_diff_log, col = "steelblue", main = "1st Difference of Log Accidents")

#ADF test After Differencing#

adf.test(acc_w_ts_diff_log, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(acc_w_ts_diff_log, type = "mu"))

#Decompose the Original Series#

plot(decompose(acc_w_ts_log))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(acc_w_ts_diff_log, lag.max = 48, main = "ACF - Log Accidents (differenced)")
Pacf(acc_w_ts_diff_log, lag.max = 48, main = "PACF - Log Accidents (differenced)")

#Trying some possible ARIMA Combinations limited to max(p,q) = 3#

train_ts_arima_acc_w_log <- window(acc_w_ts_log, end = c(2017, 29))
test_ts_arima_acc_w_log  <- window(acc_w_ts_log, start = c(2017, 30))
model_arima_acc_w_log <- Arima(train_ts_arima_acc_w_log, order = c(2, 1, 1))
fc_arima_acc_w_log <- forecast(model_arima_acc_w_log, h = length(test_ts_arima_acc_w_log))
fc_arima_acc_w_log_back <- expm1(fc_arima_acc_w_log$mean)
test_ts_arima_acc_w_log_back <- expm1(test_ts_arima_acc_w_log)
rmse_val_arima_acc_w_log <- rmse(test_ts_arima_acc_w_log_back, fc_arima_acc_w_log_back)
mae_val_arima_acc_w_log  <- mae(test_ts_arima_acc_w_log_back, fc_arima_acc_w_log_back)
mape_val_arima_acc_w_log <- mape(test_ts_arima_acc_w_log_back, fc_arima_acc_w_log_back) * 100
summary(model_arima_acc_w_log)
cat("AIC:", AIC(model_arima_acc_w_log), "\n")
cat("BIC:", BIC(model_arima_acc_w_log), "\n")
cat("RMSE:", rmse_val_arima_acc_w_log, "\n")
cat("MAE:", mae_val_arima_acc_w_log, "\n")
cat("MAPE:", mape_val_arima_acc_w_log, "%\n")
AIC(model_arima_acc_w_log)
BIC(model_arima_acc_w_log)

#Best for now is ARIMA(2,1,1), lets see with Auto.Arima#

model_auto_arima_acc_w_log <- auto.arima(
  train_ts_arima_acc_w_log,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_arima_acc_w_log <- forecast(model_auto_arima_acc_w_log, h = length(test_ts_arima_acc_w_log))
summary(model_auto_arima_acc_w_log)
fc_auto_arima_acc_w_log_back <- expm1(fc_auto_arima_acc_w_log$mean)
rmse_val_auto_arima_acc_w_log  <- rmse(test_ts_arima_acc_w_log_back, fc_auto_arima_acc_w_log_back)
mae_val_auto_arima_acc_w_log   <- mae(test_ts_arima_acc_w_log_back, fc_auto_arima_acc_w_log_back)
mape_val_auto_arima_acc_w_log <- mape(test_ts_arima_acc_w_log_back, fc_auto_arima_acc_w_log_back) * 100
cat("AIC:", AIC(model_auto_arima_acc_w_log), "\n")
cat("BIC:", BIC(model_auto_arima_acc_w_log), "\n")
cat("RMSE:", rmse_val_auto_arima_acc_w_log, "\n")
cat("MAE:", mae_val_auto_arima_acc_w_log, "\n")
cat("MAPE:", mape_val_auto_arima_acc_w_log, "%\n")

#Best for Auto.Arima is ARIMA(4,1,1) with drift, lets try both#

model_final_arima_manual_acc_w_log <- Arima(
  acc_w_ts_log,               
  order = c(2,1,1),
  include.drift = FALSE    
)
fc_final_arima_manual_acc_w_log <- forecast(model_final_arima_manual_acc_w_log, h = 192)
summary(fc_final_arima_manual_acc_w_log)
summary(model_final_arima_manual_acc_w_log)
plot(fc_final_arima_manual_acc_w_log, main = "Forecast of Accidents until 2026")
forecast_values_arima_manual_acc_w_log <- data.frame(fc_final_arima_manual_acc_w_log)
head(forecast_values_arima_manual_acc_w_log)
forecast_values_arima_manual_acc_w_log_back <- expm1(forecast_values_arima_manual_acc_w_log)
forecast_values_arima_manual_acc_w_log_back
last_week_date_arima_acc_w_manual <- max(acc.w.ind.y_mod$week_date)  
forecast_values_arima_manual_acc_w_log_back$Date <- seq(from = last_week_date_arima_acc_w_manual + 7, by = "7 days", length.out = nrow(forecast_values_arima_manual_acc_w_log_back))
ggplot() +
  geom_line(data = forecast_values_arima_manual_acc_w_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_arima_manual_acc_w_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_arima_manual_acc_w_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = accidents), color = "red", linetype = "solid") +
  labs(title = "Forecast of Weekly Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Accidents") +
  theme_minimal()
model_final_arima_auto_acc_w_log <- Arima(
  acc_w_ts_log,               
  order = c(4, 1, 1),
  include.drift = TRUE    
)
fc_final_arima_auto_acc_w_log <- forecast(model_final_arima_auto_acc_w_log, h = 192)
summary(fc_final_arima_auto_acc_w_log)
summary(model_final_arima_auto_acc_w_log)
plot(fc_final_arima_auto_acc_w_log, main = "Forecast of Accidents until 2026")
forecast_values_arima_auto_acc_w_log <- data.frame(fc_final_arima_auto_acc_w_log)
head(forecast_values_arima_auto_acc_w_log)
forecast_values_arima_auto_acc_w_log_back <- expm1(forecast_values_arima_auto_acc_w_log)
forecast_values_arima_auto_acc_w_log_back
last_week_date_arima_acc_w_auto <- max(acc.w.ind.y_mod$week_date)  
forecast_values_arima_auto_acc_w_log_back$Date <- seq(from = last_week_date_arima_acc_w_auto + 7, by = "7 days", length.out = nrow(forecast_values_arima_auto_acc_w_log_back))
ggplot() +
  geom_line(data = forecast_values_arima_auto_acc_w_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_arima_auto_acc_w_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_arima_auto_acc_w_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = accidents), color = "red", linetype = "solid") +
  labs(title = "Forecast of Weekly Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Accidents") +
  theme_minimal()

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_arima_manual_acc_w_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_manual_acc_w_log$coef))
checkresiduals(model_final_arima_manual_acc_w_log) 
Box.test(residuals(model_final_arima_manual_acc_w_log), lag=12)
Box.test(residuals(model_final_arima_auto_acc_w_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_auto_acc_w_log$coef))
checkresiduals(model_final_arima_auto_acc_w_log) 
Box.test(residuals(model_final_arima_auto_acc_w_log), lag=12)

##ARIMA on Log Fatalities##


#Time Series Object Creation#

fat_w_ts_log <- log1p(fat_w_ts)

#Time Series Plot#

plot(fat_w_ts_log, col = "steelblue", lwd = 2,
     main = "Weekly Road Fatalities (1996–2022)",
     ylab = "Fatalities", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(fat_w_ts_log, lag.max = 48, main = "ACF - Log Fatalities")
Pacf(fat_w_ts_log, lag.max = 48, main = "PACF - Log Fatalities")

#ADF test#

adf_fat.w_log <- adf.test(fat_w_ts_log, alternative = "stationary")
adf_fat.w_log

#KPSS test#

summary(ur.kpss(fat_w_ts_log, type = "mu"))

#Normal Differencing Since ADF and KPSS tests disagree (d = 1)#

fat_w_ts_diff_log <- diff(fat_w_ts_log, differences = 1)

#Plotting the Differenced Series#

plot(fat_w_ts_diff_log, col = "steelblue", main = "1st Difference of Log Fatalities")

#ADF test After Differencing#

adf.test(fat_w_ts_diff_log, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(fat_w_ts_diff_log, type = "mu"))

#Decompose the Original Series#

plot(decompose(fat_w_ts_log))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(fat_w_ts_diff_log, lag.max = 48, main = "ACF - Log Fatalities (differenced)")
Pacf(fat_w_ts_diff_log, lag.max = 48, main = "PACF - Log Fatalities (differenced)")

#Trying some possible ARIMA Combinations limited to max(p,q) = 3#

train_ts_arima_fat_w_log <- window(fat_w_ts_log, end = c(2017, 29))
test_ts_arima_fat_w_log  <- window(fat_w_ts_log, start = c(2017, 30))
model_arima_fat_w_log <- Arima(train_ts_arima_fat_w_log, order = c(2,1,1))
fc_arima_fat_w_log <- forecast(model_arima_fat_w_log, h = length(test_ts_arima_fat_w_log))
fc_arima_fat_w_log_back <- expm1(fc_arima_fat_w_log$mean)
test_ts_arima_fat_w_log_back <- expm1(test_ts_arima_fat_w_log)
rmse_val_arima_fat_w_log <- rmse(test_ts_arima_fat_w_log_back, fc_arima_fat_w_log_back)
mae_val_arima_fat_w_log  <- mae(test_ts_arima_fat_w_log_back, fc_arima_fat_w_log_back)
mape_val_arima_fat_w_log <- mape(test_ts_arima_fat_w_log_back, fc_arima_fat_w_log_back) * 100
summary(model_arima_fat_w_log)
cat("AIC:", AIC(model_arima_fat_w_log), "\n")
cat("BIC:", BIC(model_arima_fat_w_log), "\n")
cat("RMSE:", rmse_val_arima_fat_w_log, "\n")
cat("MAE:", mae_val_arima_fat_w_log, "\n")
cat("MAPE:", mape_val_arima_fat_w_log, "%\n")
AIC(model_arima_fat_w_log)
BIC(model_arima_fat_w_log)

#Best for now is ARIMA(2,1,1), lets see with Auto.Arima#

model_auto_arima_fat_w_log <- auto.arima(
  train_ts_arima_fat_w_log,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_arima_fat_w_log <- forecast(model_auto_arima_fat_w_log, h = length(test_ts_arima_fat_w_log))
summary(model_auto_arima_fat_w_log)
fc_auto_arima_fat_w_log_back <- expm1(fc_auto_arima_fat_w_log$mean)
rmse_val_auto_arima_fat_w_log  <- rmse(test_ts_arima_fat_w_log, fc_auto_arima_fat_w_log_back)
mae_val_auto_arima_fat_w_log   <- mae(test_ts_arima_fat_w_log, fc_auto_arima_fat_w_log_back)
mape_val_auto_arima_fat_w_log <- mape(test_ts_arima_fat_w_log, fc_auto_arima_fat_w_log_back) * 100
cat("AIC:", AIC(model_auto_arima_fat_w_log), "\n")
cat("BIC:", BIC(model_auto_arima_fat_w_log), "\n")
cat("RMSE:", rmse_val_auto_arima_fat_w_log, "\n")
cat("MAE:", mae_val_auto_arima_fat_w_log, "\n")
cat("MAPE:", mape_val_auto_arima_fat_w_log, "%\n")

#Best for Auto.Arima is ARIMA(4,1,1) with drift, lets try both#

model_final_arima_manual_fat_w_log <- Arima(
  fat_w_ts_log,               
  order = c(2,1,1),
  include.drift = FALSE    
)
fc_final_arima_manual_fat_w_log <- forecast(model_final_arima_manual_fat_w_log, h = 192)
summary(fc_final_arima_manual_fat_w_log)
summary(model_final_arima_manual_fat_w_log)
plot(fc_final_arima_manual_fat_w_log, main = "Forecast of Accidents until 2026")
forecast_values_arima_manual_fat_w_log <- data.frame(fc_final_arima_manual_fat_w_log)
head(forecast_values_arima_manual_fat_w_log)
forecast_values_arima_manual_fat_w_log_back <- expm1(forecast_values_arima_manual_fat_w_log)
forecast_values_arima_manual_fat_w_log_back
last_week_date_arima_fat_w_manual <- max(acc.w.ind.y_mod$week_date)  
forecast_values_arima_manual_fat_w_log_back$Date <- seq(from = last_week_date_arima_fat_w_manual + 7, by = "7 days", length.out = nrow(forecast_values_arima_manual_fat_w_log_back))
ggplot() +
  geom_line(data = forecast_values_arima_manual_fat_w_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_arima_manual_fat_w_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_arima_manual_fat_w_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = accidents), color = "red", linetype = "solid") +
  labs(title = "Forecast of Weekly Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Accidents") +
  theme_minimal()
model_final_arima_auto_fat_w_log <- Arima(
  fat_w_ts_log,               
  order = c(4, 1, 1),
  include.drift = TRUE    
)
fc_final_arima_auto_fat_w_log <- forecast(model_final_arima_auto_fat_w_log, h = 192)
summary(fc_final_arima_auto_fat_w_log)
summary(model_final_arima_auto_fat_w_log)
plot(fc_final_arima_auto_fat_w_log, main = "Forecast of Accidents until 2026")
forecast_values_arima_auto_fat_w_log <- data.frame(fc_final_arima_auto_fat_w_log)
head(forecast_values_arima_auto_fat_w_log)
forecast_values_arima_auto_fat_w_log_back <- expm1(forecast_values_arima_auto_fat_w_log)
forecast_values_arima_auto_fat_w_log_back
last_week_date_arima_fat_w_auto <- max(acc.w.ind.y_mod$week_date)  
forecast_values_arima_auto_fat_w_log_back$Date <- seq(from = last_week_date_arima_fat_w_auto + 7, by = "7 days", length.out = nrow(forecast_values_arima_auto_fat_w_log_back))
ggplot() +
  geom_line(data = forecast_values_arima_auto_fat_w_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_arima_auto_fat_w_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_arima_auto_fat_w_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = accidents), color = "red", linetype = "solid") +
  labs(title = "Forecast of Weekly Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Accidents") +
  theme_minimal()

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_arima_manual_fat_w_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_manual_fat_w_log$coef))
checkresiduals(model_final_arima_manual_fat_w_log) 
Box.test(residuals(model_final_arima_manual_fat_w_log), lag=12)
Box.test(residuals(model_final_arima_auto_fat_w_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_auto_fat_w_log$coef))
checkresiduals(model_final_arima_auto_fat_w_log) 
Box.test(residuals(model_final_arima_auto_fat_w_log), lag=12)

##SARIMA on Regular Accidents##


#Time Series Object Creation#

acc_w_ts_sa <- ts(acc.w.ind.y_4wmod$accidents, start = c(1996, 1), frequency = 48)

#Time Series Plots#

plot(acc_w_ts_sa, col = "steelblue", lwd = 2,
     main = "Weekly Road Accidents (1996–2022)",
     ylab = "Accidents", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(acc_w_ts_sa, lag.max = 48, main = "ACF - Normal Accidents")
Pacf(acc_w_ts_sa, lag.max = 48, main = "PACF - Normal Accidents")

#ADF test#

adf_acc.w_sa <- adf.test(acc_w_ts_sa, alternative = "stationary")
adf_acc.w_sa

#KPSS test#

summary(ur.kpss(acc_w_ts_sa, type = "mu"))

#Seasonal Differencing Since ADF and KPSS tests disagree (D = 1)#

acc_w_ts_seasonal_diff_sa <- diff(acc_w_ts_sa, lag = 48)

#Plotting the Differenced Series#

plot(acc_w_ts_seasonal_diff_sa, col = "steelblue", main = "1st Difference of Normal Accidents")

#ADF test After Differencing#

adf.test(acc_w_ts_seasonal_diff_sa, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(acc_w_ts_seasonal_diff_sa, type = "mu"))

#Decompose the Original Series#

plot(decompose(acc_w_ts_sa))
par(mfrow = c(1, 1))  

#Plotting ACF and PACF of the Differenced Time Series#

Acf(acc_w_ts_seasonal_diff_sa, lag.max = 48, main = "ACF - Normal Accidents (Differenced)")
Pacf(acc_w_ts_seasonal_diff_sa, lag.max = 48, main = "PACF - Normal Accidents (Differenced)")

#Trying some possible SARIMA Combinations#

train_ts_sarima_acc_w <- window(acc_w_ts_sa, end = c(2017, 29))
test_ts_sarima_acc_w  <- window(acc_w_ts_sa, start = c(2017, 30))
model_sarima_acc_w <- Arima(train_ts_sarima_acc_w, order = c(2,1,1), seasonal = list(order = c(0,1,1), period = 48))
fc_sarima_acc_w <- forecast(model_sarima_acc_w, h = length(test_ts_sarima_acc_w))
rmse_val_sarima_acc_w <- rmse(test_ts_sarima_acc_w, fc_sarima_acc_w$mean)
mae_val_sarima_acc_w  <- mae(test_ts_sarima_acc_w, fc_sarima_acc_w$mean)
mape_val_sarima_acc_w <- mape(test_ts_sarima_acc_w, fc_sarima_acc_w$mean) * 100
summary(model_sarima_acc_w)
cat("AIC:", AIC(model_sarima_acc_w), "\n")
cat("BIC:", BIC(model_sarima_acc_w), "\n")
cat("RMSE:", rmse_val_sarima_acc_w, "\n")
cat("MAE:", mae_val_sarima_acc_w, "\n")
cat("MAPE:", mape_val_sarima_acc_w, "%\n")
AIC(model_sarima_acc_w)
BIC(model_sarima_acc_w)

#Best for now is SARIMA(2,1,1)(0,1,1)[48], lets see with Auto.Arima#

model_auto_sarima_acc_w <- auto.arima(
  train_ts_sarima_acc_w,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_sarima_acc_w <- forecast(model_auto_sarima_acc_w, h = length(test_ts_sarima_acc_w))
summary(fc_auto_sarima_acc_w)
summary(model_auto_sarima_acc_w)
rmse_val_auto_sarima_acc_w  <- rmse(test_ts_sarima_acc_w, fc_auto_sarima_acc_w$mean)
mae_val_auto_sarima_acc_w   <- mae(test_ts_sarima_acc_w, fc_auto_sarima_acc_w$mean)
mape_val_auto_sarima_acc_w <- mape(test_ts_sarima_acc_w, fc_auto_sarima_acc_w$mean) * 100
cat("AIC:", AIC(model_auto_sarima_acc_w), "\n")
cat("BIC:", BIC(model_auto_sarima_acc_w), "\n")
cat("RMSE:", rmse_val_auto_sarima_acc_w, "\n")
cat("MAE:", mae_val_auto_sarima_acc_w, "\n")
cat("MAPE:", mape_val_auto_sarima_acc_w, "%\n")

#Best for Auto.Arima is SARIMA(1,0,0)(0,1,1)[48] with drift, lets try with both

model_final_sarima_manual_acc_w <- Arima(
  acc_w_ts_sa,               
  order = c(2, 1, 1),
  seasonal = list(order = c(0,1,1), period = 48),
  include.drift = FALSE
)
fc_final_sarima_manual_acc_w  <- forecast(model_final_sarima_manual_acc_w , h = 192)
summary(fc_final_sarima_manual_acc_w)
summary(model_final_sarima_manual_acc_w)
plot(fc_final_sarima_manual_acc_w , main = "Forecast of Fatalities until 2026")
forecast_values_sarima_manual_acc_w  <- data.frame(fc_final_sarima_manual_acc_w)
head(forecast_values_sarima_manual_acc_w)
model_final_sarima_auto_acc_w <- Arima(
  acc_w_ts_sa,               
  order = c(1,0,0),
  seasonal = list(order = c(0,1,1), period = 48),
  include.drift = TRUE
)
fc_final_sarima_auto_acc_w <- forecast(model_final_sarima_auto_acc_w, h = 192)
summary(fc_final_sarima_auto_acc_w)
summary(model_final_sarima_auto_acc_w)
plot(fc_final_sarima_auto_acc_w, main = "Forecast of Fatalities until 2026")
forecast_values_sarima_auto_acc_w <- data.frame(fc_final_sarima_auto_acc_w)
head(forecast_values_sarima_auto_acc_w)

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_sarima_manual_acc_w), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_manual_acc_w$coef))
checkresiduals(model_final_sarima_manual_acc_w) 
Box.test(residuals(model_final_sarima_manual_acc_w), lag=12)
Box.test(residuals(model_final_sarima_auto_acc_w), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_auto_acc_w$coef))
checkresiduals(model_final_sarima_auto_acc_w) 
Box.test(residuals(model_final_sarima_auto_acc_w), lag=12)

##SARIMA on Regular Fatalities##


#Time Series Object Creation#

fat_w_ts_sa <- ts(acc.w.ind.y_4wmod$fatalities, start = c(1996, 1), frequency = 48)

#Time Series Plot#

plot(fat_w_ts_sa, col = "darkred", lwd = 2,
     main = "Weekly Road Fatalities (1996–2022)",
     ylab = "Fatalities", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(fat_w_ts_sa, lag.max = 48, main = "ACF - Normal Fatalities")
Pacf(fat_w_ts_sa, lag.max = 48, main = "PACF - Normal Fatalities")

#ADF test#

adf_fat.w_sa <- adf.test(fat_w_ts_sa, alternative = "stationary")
adf_fat.w_sa

#KPSS test#

summary(ur.kpss(fat_w_ts_sa, type = "mu"))

#Seasonal Differencing Since ADF and KPSS tests disagree (D = 1)#

fat_w_ts_seasonal_diff_sa <- diff(fat_w_ts_sa, lag = 12)

#Plotting the Differenced Series#

plot(fat_w_ts_seasonal_diff_sa, col = "darkred", main = "1st Difference of Normal Fatalities")

#ADF test After Differencing#

adf.test(fat_w_ts_seasonal_diff_sa, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(fat_w_ts_seasonal_diff_sa, type = "mu"))

#Decompose the Original Series#

plot(decompose(fat_w_ts_sa))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(fat_w_ts_seasonal_diff_sa, lag.max = 48, main = "ACF - Normal Fatalities (differenced)")
Pacf(fat_w_ts_seasonal_diff_sa, lag.max = 48, main = "PACF - Normal Fatalities (differenced)")

#Trying some possible SARIMA Combinations#

train_ts_sarima_fat_w <- window(fat_w_ts_sa, end = c(2017, 29))
test_ts_sarima_fat_w  <- window(fat_w_ts_sa, start = c(2017, 30))
model_sarima_fat_w <- Arima(train_ts_sarima_fat_w, order = c(3,1,1), seasonal = list(order = c(1,1,2), period = 48))
fc_sarima_fat_w <- forecast(model_sarima_fat_w, h = length(test_ts_sarima_fat_w))
rmse_val_sarima_fat_w <- rmse(test_ts_sarima_fat_w, fc_sarima_fat_w$mean)
mae_val_sarima_fat_w  <- mae(test_ts_sarima_fat_w, fc_sarima_fat_w$mean)
mape_val_sarima_fat_w <- mape(test_ts_sarima_fat_w, fc_sarima_fat_w$mean) * 100
summary(model_sarima_fat_w)
cat("AIC:", AIC(model_sarima_fat_w), "\n")
cat("BIC:", BIC(model_sarima_fat_w), "\n")
cat("RMSE:", rmse_val_sarima_fat_w, "\n")
cat("MAE:", mae_val_sarima_fat_w, "\n")
cat("MAPE:", mape_val_sarima_fat_w, "%\n")
AIC(model_sarima_fat_w)
BIC(model_sarima_fat_w)

#Best for now is SARIMA(3,1,1)(1,1,2)[48], lets see with Auto.Arima#

model_auto_sarima_fat_w <- auto.arima(
  train_ts_sarima_fat_w,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_sarima_fat_w <- forecast(model_auto_sarima_fat_w, h = length(test_ts_sarima_fat_w))
summary(fc_auto_sarima_fat_w)
summary(model_auto_sarima_fat_w)
rmse_val_auto_sarima_fat_w  <- rmse(test_ts_sarima_fat_w, fc_auto_sarima_fat_w$mean)
mae_val_auto_sarima_fat_w   <- mae(test_ts_sarima_fat_w, fc_auto_sarima_fat_w$mean)
mape_val_auto_sarima_fat_w <- mape(test_ts_sarima_fat_w, fc_auto_sarima_fat_w$mean) * 100
cat("AIC:", AIC(model_auto_sarima_fat_w), "\n")
cat("BIC:", BIC(model_auto_sarima_fat_w), "\n")
cat("RMSE:", rmse_val_auto_sarima_fat_w, "\n")
cat("MAE:", mae_val_auto_sarima_fat_w, "\n")
cat("MAPE:", mape_val_auto_sarima_fat_w, "%\n")

#Best for Auto.Arima is SARIMA(0,1,3)(1,0,0)[48] with drift, lets try both#

model_final_sarima_manual_fat_w <- Arima(
  fat_w_ts_sa,               
  order = c(3,1,1),
  seasonal = list(order = c(1,1,2), period = 48),
  include.drift = FALSE
)
fc_final_sarima_manual_fat_w  <- forecast(model_final_sarima_manual_fat_w , h = 192)
summary(fc_final_sarima_manual_fat_w)
summary(model_final_sarima_manual_fat_w)
plot(fc_final_sarima_manual_fat_w , main = "Forecast of Fatalities until 2026")
forecast_values_sarima_manual_fat_w  <- data.frame(fc_final_sarima_manual_fat_w)
head(forecast_values_sarima_manual_fat_w)
model_final_sarima_auto_fat_w <- Arima(
  fat_w_ts_sa,               
  order = c(0,1,3),
  seasonal = list(order = c(1,0,0), period = 48),
  include.drift = TRUE
)
fc_final_sarima_auto_fat_w <- forecast(model_final_sarima_auto_fat_w, h = 192)
summary(fc_final_sarima_auto_fat_w)
summary(model_final_sarima_auto_fat_w)
plot(fc_final_sarima_auto_fat_w, main = "Forecast of Fatalities until 2026")
forecast_values_sarima_auto_fat_w <- data.frame(fc_final_sarima_auto_fat_w)
head(forecast_values_sarima_auto_fat_w)

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_sarima_manual_fat_w), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_manual_fat_w$coef))
checkresiduals(model_final_sarima_manual_fat_w) 
Box.test(residuals(model_final_sarima_manual_fat_w), lag=12)
Box.test(residuals(model_final_sarima_auto_fat_w), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_auto_fat_w$coef))
checkresiduals(model_final_sarima_auto_fat_w) 
Box.test(residuals(model_final_sarima_auto_fat_w), lag=12)

##SARIMA on Log Accidents##


#Time Series Object Creation#

acc_w_ts_log_sa <- log1p(acc_w_ts_sa)

#Time Series Plot#

plot(acc_w_ts_log_sa, col = "steelblue", lwd = 2,
     main = "Weekly Road Accidents (1996–2022)",
     ylab = "Accidents", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(acc_w_ts_log_sa, lag.max = 48, main = "ACF - Log Accidents")
Pacf(acc_w_ts_log_sa, lag.max = 48, main = "PACF - Log Accidents")

#ADF test#

adf_acc.w_log_sa <- adf.test(acc_w_ts_log_sa, alternative = "stationary")
adf_acc.w_log_sa

#KPSS test#

summary(ur.kpss(acc_w_ts_log_sa, type = "mu"))

#Seasonal Differencing Since ADF and KPSS tests disagree (D = 1)#

acc_w_ts_seasonal_diff_log_sa <- diff(acc_w_ts_sa, lag = 12)

#Plotting the Differenced Series#

plot(acc_w_ts_seasonal_diff_log_sa, col = "steelblue", main = "1st Difference of Log Accidents")

#ADF test After Differencing#

adf.test(acc_w_ts_seasonal_diff_log_sa, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(acc_w_ts_seasonal_diff_log_sa, type = "mu"))

#Decompose the Original Series#

plot(decompose(acc_w_ts_log_sa))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(acc_w_ts_seasonal_diff_log_sa, lag.max = 192, main = "ACF - Log Accidents (differenced)")
Pacf(acc_w_ts_seasonal_diff_log_sa, lag.max = 192, main = "PACF - Log Accidents (differenced)")

#Trying some possible SARIMA Combinations#

train_ts_sarima_acc_w_log <- window(acc_w_ts_log_sa, end = c(2017, 29))
test_ts_sarima_acc_w_log  <- window(acc_w_ts_log_sa, start = c(2017, 30))
model_sarima_acc_w_log <- Arima(train_ts_sarima_acc_w_log, order = c(2, 1, 1), seasonal = list(order = c(0, 1, 1), period = 48))
fc_sarima_acc_w_log <- forecast(model_sarima_acc_w_log, h = length(test_ts_sarima_acc_w_log))
fc_sarima_acc_w_log_back <- expm1(fc_sarima_acc_w_log$mean)
test_ts_sarima_acc_w_log_back <- expm1(test_ts_sarima_acc_w_log)
rmse_val_sarima_acc_w_log <- rmse(test_ts_sarima_acc_w_log_back, fc_sarima_acc_w_log_back)
mae_val_sarima_acc_w_log  <- mae(test_ts_sarima_acc_w_log_back, fc_sarima_acc_w_log_back)
mape_val_sarima_acc_w_log <- mape(test_ts_sarima_acc_w_log_back, fc_sarima_acc_w_log_back) * 100
summary(model_sarima_acc_w_log)
cat("AIC:", AIC(model_sarima_acc_w_log), "\n")
cat("BIC:", BIC(model_sarima_acc_w_log), "\n")
cat("RMSE:", rmse_val_sarima_acc_w_log, "\n")
cat("MAE:", mae_val_sarima_acc_w_log, "\n")
cat("MAPE:", mape_val_sarima_acc_w_log, "%\n")
AIC(model_sarima_acc_w_log)
BIC(model_sarima_acc_w_log)

#Best for now is SARIMA(2,1,1)(0,1,1)[48], lets see with Auto.Arima#

model_auto_sarima_acc_w_log <- auto.arima(
  train_ts_sarima_acc_w_log,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_sarima_acc_w_log <- forecast(model_auto_sarima_acc_w_log, h = length(test_ts_sarima_acc_w_log))
summary(model_auto_sarima_acc_w_log)
fc_auto_sarima_acc_w_log_back <- expm1(fc_auto_sarima_acc_w_log$mean)
rmse_val_auto_sarima_acc_w_log  <- rmse(test_ts_sarima_acc_w_log_back, fc_auto_sarima_acc_w_log_back)
mae_val_auto_sarima_acc_w_log   <- mae(test_ts_sarima_acc_w_log_back, fc_auto_sarima_acc_w_log_back)
mape_val_auto_sarima_acc_w_log <- mape(test_ts_sarima_acc_w_log_back, fc_auto_sarima_acc_w_log_back) * 100
cat("AIC:", AIC(model_auto_sarima_acc_w_log), "\n")
cat("BIC:", BIC(model_auto_sarima_acc_w_log), "\n")
cat("RMSE:", rmse_val_auto_sarima_acc_w_log, "\n")
cat("MAE:", mae_val_auto_sarima_acc_w_log, "\n")
cat("MAPE:", mape_val_auto_sarima_acc_w_log, "%\n")

#Best for Auto.Arima is SARIMA(0,0,3)(2,1,0)[48] with drift, lets try both#

model_final_sarima_manual_acc_w_log <- Arima(
  acc_w_ts_log_sa,               
  order = c(2,1,1),
  seasonal = list(order = c(0,1,1), period = 48),
  include.drift = FALSE    
)
fc_final_sarima_manual_acc_w_log  <- forecast(model_final_sarima_manual_acc_w_log , h = 192)
summary(fc_final_sarima_manual_acc_w_log)
summary(model_final_sarima_manual_acc_w_log)
plot(fc_final_sarima_manual_acc_w_log, main = "Forecast of Accidents until 2026")
forecast_values_sarima_manual_acc_w_log <- data.frame(fc_final_sarima_manual_acc_w_log)
head(forecast_values_sarima_manual_acc_w_log)
forecast_values_sarima_manual_acc_w_log_back <- expm1(forecast_values_sarima_manual_acc_w_log)
forecast_values_sarima_manual_acc_w_log_back
last_week_date_sarima_acc_w_manual <- max(acc.w.ind.y_mod$week_date)  
forecast_values_sarima_manual_acc_w_log_back$Date <- seq(from = last_week_date_sarima_acc_w_manual + 7, by = "7 days", length.out = nrow(forecast_values_sarima_manual_acc_w_log_back))
ggplot() +
  geom_line(data = forecast_values_sarima_manual_acc_w_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_sarima_manual_acc_w_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_sarima_manual_acc_w_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = accidents), color = "red", linetype = "solid") +
  labs(title = "Forecast of Weekly Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Accidents") +
  theme_minimal()
model_final_sarima_auto_acc_w_log <- Arima(
  acc_w_ts_log_sa,               
  order = c(0,0,3),
  seasonal = list(order = c(2,1,0), period = 48),
  include.drift = TRUE    
)
fc_final_sarima_auto_acc_w_log <- forecast(model_final_sarima_auto_acc_w_log, h = 192)
summary(fc_final_sarima_auto_acc_w_log)
summary(model_final_sarima_auto_acc_w_log)
plot(fc_final_sarima_auto_acc_w_log, main = "Forecast of Accidents until 2026")
forecast_values_sarima_auto_acc_w_log <- data.frame(fc_final_sarima_auto_acc_w_log)
head(forecast_values_sarima_auto_acc_w_log)
forecast_values_sarima_auto_acc_w_log_back <- expm1(forecast_values_sarima_auto_acc_w_log)
forecast_values_sarima_auto_acc_w_log_back
last_week_date_sarima_acc_w_auto <- max(acc.w.ind.y_mod$week_date)  
forecast_values_sarima_auto_acc_w_log_back$Date <- seq(from = last_week_date_sarima_acc_w_auto + 7, by = "7 days", length.out = nrow(forecast_values_sarima_auto_acc_w_log_back))
ggplot() +
  geom_line(data = forecast_values_sarima_auto_acc_w_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_sarima_auto_acc_w_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_sarima_auto_acc_w_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = accidents), color = "red", linetype = "solid") +
  labs(title = "Forecast of Weekly Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Accidents") +
  theme_minimal()

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_sarima_manual_acc_w_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_manual_acc_w_log$coef))
checkresiduals(model_final_sarima_manual_acc_w_log) 
Box.test(residuals(model_final_sarima_manual_acc_w_log), lag=12)
Box.test(residuals(model_final_sarima_auto_acc_w_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_auto_acc_w_log$coef))
checkresiduals(model_final_sarima_auto_acc_w_log) 
Box.test(residuals(model_final_sarima_auto_acc_w_log), lag=12)

##SARIMA on Log Fatalities##


#Time Series Object Creation#

fat_w_ts_log_sa <- log1p(fat_w_ts_sa)

#Time Series Plot#

plot(fat_w_ts_log_sa, col = "steelblue", lwd = 2,
     main = "Weekly Road Fatalities (1996–2022)",
     ylab = "Fatalities", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(fat_w_ts_log_sa, lag.max = 48, main = "ACF - Log Fatalities")
Pacf(fat_w_ts_log_sa, lag.max = 48, main = "PACF - Log Fatalities")

#ADF test#

adf_fat.w_log_sa <- adf.test(fat_w_ts_log_sa, alternative = "stationary")
adf_fat.w_log_sa

#KPSS test#

summary(ur.kpss(fat_w_ts_log_sa, type = "mu"))

#Seasonal Differencing Since ADF and KPSS tests disagree (D = 1)#

fat_w_ts_seasonal_diff_log_sa <- diff(fat_w_ts_sa, lag = 12)

#Plotting the Differenced Series#

plot(fat_w_ts_seasonal_diff_log_sa, col = "steelblue", main = "1st Difference of Log Fatalities")

#ADF test After Differencing#

adf.test(fat_w_ts_seasonal_diff_log_sa, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(fat_w_ts_seasonal_diff_log_sa, type = "mu"))

#Decompose the Original Series#

plot(decompose(fat_w_ts_log_sa))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(fat_w_ts_seasonal_diff_log_sa, lag.max = 48, main = "ACF - Log Fatalities (differenced)")
Pacf(fat_w_ts_seasonal_diff_log_sa, lag.max = 48, main = "PACF - Log Fatalities (differenced)")

#Trying some possible SARIMA Combinations#

train_ts_sarima_fat_w_log <- window(fat_w_ts_log_sa, end = c(2017, 29))
test_ts_sarima_fat_w_log  <- window(fat_w_ts_log_sa, start = c(2017, 30))
model_sarima_fat_w_log <- Arima(train_ts_sarima_fat_w_log, order = c(1,1,2), seasonal = list(order = c(1,1,0), period = 48))
fc_sarima_fat_w_log <- forecast(model_sarima_fat_w_log, h = length(test_ts_sarima_fat_w_log))
fc_sarima_fat_w_log_back <- expm1(fc_sarima_fat_w_log$mean)
test_ts_sarima_fat_w_log_back <- expm1(test_ts_sarima_fat_w_log)
rmse_val_sarima_fat_w_log <- rmse(test_ts_sarima_fat_w_log_back, fc_sarima_fat_w_log_back)
mae_val_sarima_fat_w_log  <- mae(test_ts_sarima_fat_w_log_back, fc_sarima_fat_w_log_back)
mape_val_sarima_fat_w_log <- mape(test_ts_sarima_fat_w_log_back, fc_sarima_fat_w_log_back) * 100
summary(model_sarima_fat_w_log)
cat("AIC:", AIC(model_sarima_fat_w_log), "\n")
cat("BIC:", BIC(model_sarima_fat_w_log), "\n")
cat("RMSE:", rmse_val_sarima_fat_w_log, "\n")
cat("MAE:", mae_val_sarima_fat_w_log, "\n")
cat("MAPE:", mape_val_sarima_fat_w_log, "%\n")
AIC(model_sarima_fat_w_log)
BIC(model_sarima_fat_w_log)

#Best for now is SARIMA(1,1,2)(1,1,0)[48], lets see with Auto.Arima#

model_auto_sarima_fat_w_log <- auto.arima(
  train_ts_sarima_fat_w_log,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_sarima_fat_w_log <- forecast(model_auto_sarima_fat_w_log, h = length(test_ts_sarima_fat_w_log))
summary(model_auto_sarima_fat_w_log)
fc_auto_sarima_fat_w_log_back <- expm1(fc_auto_sarima_fat_w_log$mean)
rmse_val_auto_sarima_fat_w_log  <- rmse(test_ts_sarima_fat_w_log_back, fc_auto_sarima_fat_w_log_back)
mae_val_auto_sarima_fat_w_log   <- mae(test_ts_sarima_fat_w_log_back, fc_auto_sarima_fat_w_log_back)
mape_val_auto_sarima_fat_w_log <- mape(test_ts_sarima_fat_w_log_back, fc_auto_sarima_fat_w_log_back) * 100
cat("AIC:", AIC(model_auto_sarima_fat_w_log), "\n")
cat("BIC:", BIC(model_auto_sarima_fat_w_log), "\n")
cat("RMSE:", rmse_val_auto_sarima_fat_w_log, "\n")
cat("MAE:", mae_val_auto_sarima_fat_w_log, "\n")
cat("MAPE:", mape_val_auto_sarima_fat_w_log, "%\n")

#Best for Auto.Arima is SARIMA(0,1,2)(1,0,0)[48] with drift, lets try both#

model_final_sarima_manual_fat_w_log <- Arima(
  fat_w_ts_log_sa,               
  order = c(1,1,2),
  seasonal = list(order = c(1,1,0), period = 48),
  include.drift = FALSE    
)
fc_final_sarima_manual_fat_w_log  <- forecast(model_final_sarima_manual_fat_w_log , h = 192)
summary(fc_final_sarima_manual_fat_w_log)
summary(model_final_sarima_manual_fat_w_log)
plot(fc_final_sarima_manual_fat_w_log , main = "Forecast of Fatalities until 2026")
forecast_values_sarima_manual_fat_w_log  <- data.frame(fc_final_sarima_manual_fat_w_log)
head(forecast_values_sarima_manual_fat_w_log)
forecast_values_sarima_manual_fat_w_log_back <- expm1(forecast_values_sarima_manual_fat_w_log)
forecast_values_sarima_manual_fat_w_log_back
forecast_values_sarima_manual_fat_w_log_back$Date <- rownames(forecast_values_sarima_manual_fat_w_log_back)
rownames(forecast_values_sarima_manual_fat_w_log_back) <- NULL
forecast_values_sarima_manual_fat_w_log_back$Date <- trimws(forecast_values_sarima_manual_fat_w_log_back$Date)
forecast_values_sarima_manual_fat_w_log_back$Date <- dmy(paste("01", forecast_values_sarima_manual_fat_w_log_back$Date))
model_final_sarima_auto_fat_w_log <- Arima(
  fat_w_ts_log_sa,               
  order = c(0,1,2),
  seasonal = list(order = c(1,0,0), period = 48),
  include.drift = FALSE    
)
fc_final_sarima_auto_fat_w_log <- forecast(model_final_sarima_auto_fat_w_log, h = 192)
summary(fc_final_sarima_auto_fat_w_log)
summary(model_final_sarima_auto_fat_w_log)
plot(fc_final_sarima_auto_fat_w_log, main = "Forecast of Fatalities until 2026")
forecast_values_sarima_auto_fat_w_log <- data.frame(fc_final_sarima_auto_fat_w_log)
head(forecast_values_sarima_auto_fat_w_log)
forecast_values_sarima_auto_fat_w_log_back <- expm1(forecast_values_sarima_auto_fat_w_log)
forecast_values_sarima_auto_fat_w_log_back
forecast_values_sarima_auto_fat_w_log_back$Date <- rownames(forecast_values_sarima_auto_fat_w_log_back)
rownames(forecast_values_sarima_auto_fat_w_log_back) <- NULL
forecast_values_sarima_auto_fat_w_log_back$Date <- trimws(forecast_values_sarima_auto_fat_w_log_back$Date)
forecast_values_sarima_auto_fat_w_log_back$Date <- dmy(paste("01", forecast_values_sarima_auto_fat_w_log_back$Date))

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_sarima_manual_fat_w_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_manual_fat_w_log$coef))
checkresiduals(model_final_sarima_manual_fat_w_log) 
Box.test(residuals(model_final_sarima_manual_fat_w_log), lag=12)
Box.test(residuals(model_final_sarima_auto_fat_w_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_auto_fat_w_log$coef))
checkresiduals(model_final_sarima_auto_fat_w_log) 
Box.test(residuals(model_final_sarima_auto_fat_w_log), lag=12)

####Prediction Models of ARIMA and SARIMA in the Monthly Dataset####




##ARIMA on Regular Accidents##


#Time Series Object Creation#

acc_m_ts <- ts(acc.m.ind.y_mod$accidents, start = c(1996, 1), frequency = 12)

#Time Series Plots#

plot(acc_m_ts, col = "steelblue", lwd = 2,
     main = "Monthly Road Accidents (1996–2022)",
     ylab = "Accidents", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(acc_m_ts, lag.max = 48, main = "ACF - Normal Accidents")
Pacf(acc_m_ts, lag.max = 48, main = "PACF - Normal Accidents")

#ADF test#

adf_acc.m <- adf.test(acc_m_ts, alternative = "stationary")
adf_acc.m

#KPSS test#

summary(ur.kpss(acc_m_ts, type = "mu"))

#Normal Differencing Since ADF and KPSS tests disagree (d = 1)#

acc_m_ts_diff <- diff(acc_m_ts, differences = 1)

#Plotting the Differenced Series#

plot(acc_m_ts_diff, col = "steelblue", main = "1st Difference of Normal Accidents")

#ADF test After Differencing#

adf.test(acc_m_ts_diff, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(acc_m_ts_diff, type = "mu"))

#Decompose the Original Series#

plot(decompose(acc_m_ts))
par(mfrow = c(1, 1))  

#Plotting ACF and PACF of the Differenced Time Series#

Acf(acc_m_ts_diff, lag.max = 48, main = "ACF - Normal Accidents (Differenced)")
Pacf(acc_m_ts_diff, lag.max = 48, main = "PACF - Normal Accidents (Differenced)")

#Trying some possible ARIMA Combinations limited to max(p,q) = 3#

train_ts_arima_acc_m <- window(acc_m_ts, end = c(2017, 7))
test_ts_arima_acc_m  <- window(acc_m_ts, start = c(2017, 8))
model_arima_acc_m <- Arima(train_ts_arima_acc_m, order = c(3, 1, 2))
fc_arima_acc_m <- forecast(model_arima_acc_m, h = length(test_ts_arima_acc_m))
rmse_val_arima_acc_m <- rmse(test_ts_arima_acc_m, fc_arima_acc_m$mean)
mae_val_arima_acc_m  <- mae(test_ts_arima_acc_m, fc_arima_acc_m$mean)
mape_val_arima_acc_m <- mape(test_ts_arima_acc_m, fc_arima_acc_m$mean) * 100
summary(model_arima_acc_m)
cat("AIC:", AIC(model_arima_acc_m), "\n")
cat("BIC:", BIC(model_arima_acc_m), "\n")
cat("RMSE:", rmse_val_arima_acc_m, "\n")
cat("MAE:", mae_val_arima_acc_m, "\n")
cat("MAPE:", mape_val_arima_acc_m, "%\n")
AIC(model_arima_acc_m)
BIC(model_arima_acc_m)

#Best for now is ARIMA(3,1,2), lets see with Auto.Arima#

model_auto_arima_acc_m <- auto.arima(
  train_ts_arima_acc_m,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  max.p = 3,
  max.q = 3,
  max.d = 3,
  max.order = 10,   
  trace = TRUE
)
fc_auto_arima_acc_m <- forecast(model_auto_arima_acc_m, h = length(test_ts_arima_acc_m))
summary(fc_auto_arima_acc_m)
summary(model_auto_arima_acc_m)
rmse_val_auto_arima_acc_m  <- rmse(test_ts_arima_acc_m, fc_auto_arima_acc_m$mean)
mae_val_auto_arima_acc_m   <- mae(test_ts_arima_acc_m, fc_auto_arima_acc_m$mean)
mape_val_auto_arima_acc_m <- mape(test_ts_arima_acc_m, fc_auto_arima_acc_m$mean) * 100
cat("AIC:", AIC(model_auto_arima_acc_m), "\n")
cat("BIC:", BIC(model_auto_arima_acc_m), "\n")
cat("RMSE:", rmse_val_auto_arima_acc_m, "\n")
cat("MAE:", mae_val_auto_arima_acc_m, "\n")
cat("MAPE:", mape_val_auto_arima_acc_m, "%\n")

#Best for Auto.Arima is ARIMA(3,1,1) with drift, lets try both#

model_final_arima_manual_acc_m <- Arima(
  acc_m_ts,               
  order = c(3, 1, 2),
  include.drift = FALSE    
)
fc_final_arima_manual_acc_m  <- forecast(model_final_arima_manual_acc_m , h = 48)
summary(fc_final_arima_manual_acc_m)
summary(model_final_arima_manual_acc_m)
plot(fc_final_arima_manual_acc_m , main = "Forecast of Accidents until 2026")
forecast_values_arima_manual_acc_m  <- data.frame(fc_final_arima_manual_acc_m)
head(forecast_values_arima_manual_acc_m)
model_final_arima_auto_acc_m <- Arima(
  acc_m_ts,               
  order = c(3, 1, 1),
  include.drift = TRUE    
)
fc_final_arima_auto_acc_m <- forecast(model_final_arima_auto_acc_m, h = 48)
summary(fc_final_arima_auto_acc_m)
summary(model_final_arima_auto_acc_m)
plot(fc_final_arima_auto_acc_m, main = "Forecast of Accidents until 2026")
forecast_values_arima_auto_acc_m <- data.frame(fc_final_arima_auto_acc_m)
head(forecast_values_arima_auto_acc_m)

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_arima_manual_acc_m), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_manual_acc_m$coef))
checkresiduals(model_final_arima_manual_acc_m) 
Box.test(residuals(model_final_arima_manual_acc_m), lag=12)
Box.test(residuals(model_final_arima_auto_acc_m), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_auto_acc_m$coef))
checkresiduals(model_final_arima_auto_acc_m) 
Box.test(residuals(model_final_arima_auto_acc_m), lag=12)

##ARIMA on Regular Fatalities##


#Time Series Object Creation#

fat_m_ts <- ts(acc.m.ind.y_mod$fatalities, start = c(1996, 1), frequency = 12)

#Time Series Plot#

plot(fat_m_ts, col = "darkred", lwd = 2,
     main = "Monthly Road Fatalities (1996–2022)",
     ylab = "Fatalities", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(fat_m_ts, lag.max = 48, main = "ACF - Normal Fatalities")
Pacf(fat_m_ts, lag.max = 48, main = "PACF - Normal Fatalities")

#ADF test#

adf_fat.m <- adf.test(fat_m_ts, alternative = "stationary")
adf_fat.m

#KPSS test#

summary(ur.kpss(fat_m_ts, type = "mu"))

#Normal Differencing Since ADF and KPSS tests disagree (d = 1)#

fat_m_ts_diff <- diff(fat_m_ts, differences = 1)

#Plotting the Differenced Series#

plot(fat_m_ts_diff, col = "darkred", main = "1st Difference of Normal Fatalities")

#ADF test After Differencing#

adf.test(fat_m_ts_diff, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(fat_m_ts_diff, type = "mu"))

#Decompose the Original Series#

plot(decompose(fat_m_ts))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(fat_m_ts_diff, lag.max = 48, main = "ACF - Normal Fatalities (differenced)")
Pacf(fat_m_ts_diff, lag.max = 48, main = "PACF - Normal Fatalities (differenced)")

#Trying some possible ARIMA Combinations limited to max(p,q) = 3#

train_ts_arima_fat_m <- window(fat_m_ts, end = c(2017, 7))
test_ts_arima_fat_m  <- window(fat_m_ts, start = c(2017, 8))
model_arima_fat_m <- Arima(train_ts_arima_fat_m, order = c(3, 1, 3))
fc_arima_fat_m <- forecast(model_arima_fat_m, h = length(test_ts_arima_fat_m))
rmse_val_arima_fat_m <- rmse(test_ts_arima_fat_m, fc_arima_fat_m$mean)
mae_val_arima_fat_m  <- mae(test_ts_arima_fat_m, fc_arima_fat_m$mean)
mape_val_arima_fat_m <- mape(test_ts_arima_fat_m, fc_arima_fat_m$mean) * 100
summary(model_arima_fat_m)
cat("AIC:", AIC(model_arima_fat_m), "\n")
cat("BIC:", BIC(model_arima_fat_m), "\n")
cat("RMSE:", rmse_val_arima_fat_m, "\n")
cat("MAE:", mae_val_arima_fat_m, "\n")
cat("MAPE:", mape_val_arima_fat_m, "%\n")
AIC(model_arima_fat_m)
BIC(model_arima_fat_m)

#Best for now is ARIMA(3,1,3), lets see with Auto.Arima#

model_auto_arima_fat_m <- auto.arima(
  train_ts_arima_fat_m,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  max.p = 3,
  max.q = 3,
  max.d = 3,
  max.order = 10,   
  trace = TRUE
)
fc_auto_arima_fat_m <- forecast(model_auto_arima_fat_m, h = length(test_ts_arima_fat_m))
summary(fc_auto_arima_fat_m)
summary(model_auto_arima_fat_m)
rmse_val_auto_arima_fat_m  <- rmse(test_ts_arima_fat_m, fc_auto_arima_fat_m$mean)
mae_val_auto_arima_fat_m   <- mae(test_ts_arima_fat_m, fc_auto_arima_fat_m$mean)
mape_val_auto_arima_fat_m <- mape(test_ts_arima_fat_m, fc_auto_arima_fat_m$mean) * 100
cat("AIC:", AIC(model_auto_arima_fat_m), "\n")
cat("BIC:", BIC(model_auto_arima_fat_m), "\n")
cat("RMSE:", rmse_val_auto_arima_fat_m, "\n")
cat("MAE:", mae_val_auto_arima_fat_m, "\n")
cat("MAPE:", mape_val_auto_arima_fat_m, "%\n")

#Best for Auto.Arima is ARIMA(3,1,3) with drift, lets try both#

model_final_arima_manual_fat_m <- Arima(
  fat_m_ts,               
  order = c(3, 1, 3),
  include.drift = FALSE    
)
fc_final_arima_manual_fat_m  <- forecast(model_final_arima_manual_fat_m , h = 48)
summary(fc_final_arima_manual_fat_m)
summary(model_final_arima_manual_fat_m)
plot(fc_final_arima_manual_fat_m , main = "Forecast of Fatalities until 2026")
forecast_values_arima_manual_fat_m  <- data.frame(fc_final_arima_manual_fat_m)
head(forecast_values_arima_manual_fat_m)
model_final_arima_auto_fat_m <- Arima(
  acc_m_ts,               
  order = c(3, 1, 3),
  include.drift = TRUE    
)
fc_final_arima_auto_fat_m <- forecast(model_final_arima_auto_fat_m, h = 48)
summary(fc_final_arima_auto_fat_m)
summary(model_final_arima_auto_fat_m)
plot(fc_final_arima_auto_fat_m, main = "Forecast of Fatalities until 2026")
forecast_values_arima_auto_fat_m <- data.frame(fc_final_arima_auto_fat_m)
head(forecast_values_arima_auto_fat_m)

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_arima_manual_fat_m), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_manual_fat_m$coef))
checkresiduals(model_final_arima_manual_fat_m) 
Box.test(residuals(model_final_arima_manual_fat_m), lag=12)
Box.test(residuals(model_final_arima_auto_fat_m), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_auto_fat_m$coef))
checkresiduals(model_final_arima_auto_fat_m) 
Box.test(residuals(model_final_arima_auto_fat_m), lag=12)

##ARIMA on Log Accidents##


#Time Series Object Creation#

acc_m_ts_log <- log1p(acc_m_ts)

#Time Series Plot#

plot(acc_m_ts_log, col = "steelblue", lwd = 2,
     main = "Monthly Road Accidents (1996–2022)",
     ylab = "Accidents", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(acc_m_ts_log, lag.max = 48, main = "ACF - Log Accidents")
Pacf(acc_m_ts_log, lag.max = 48, main = "PACF - Log Accidents")

#ADF test#

adf_acc.m_log <- adf.test(acc_m_ts_log, alternative = "stationary")
adf_acc.m_log

#KPSS test#

summary(ur.kpss(acc_m_ts_log, type = "mu"))

#Normal Differencing Since ADF and KPSS tests disagree (d = 1)#

acc_m_ts_diff_log <- diff(acc_m_ts_log, differences = 1)

#Plotting the Differenced Series#

plot(acc_m_ts_diff_log, col = "steelblue", main = "1st Difference of Log Accidents")

#ADF test After Differencing#

adf.test(acc_m_ts_diff_log, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(acc_m_ts_diff_log, type = "mu"))

#Decompose the Original Series#

plot(decompose(acc_m_ts_log))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(acc_m_ts_diff_log, lag.max = 48, main = "ACF - Log Accidents (differenced)")
Pacf(acc_m_ts_diff_log, lag.max = 48, main = "PACF - Log Accidents (differenced)")

#Trying some possible ARIMA Combinations limited to max(p,q) = 3#

train_ts_arima_acc_m_log <- window(acc_m_ts_log, end = c(2017, 7))
test_ts_arima_acc_m_log  <- window(acc_m_ts_log, start = c(2017, 8))
model_arima_acc_m_log <- Arima(train_ts_arima_acc_m_log, order = c(2, 1, 1))
fc_arima_acc_m_log <- forecast(model_arima_acc_m_log, h = length(test_ts_arima_acc_m_log))
fc_arima_acc_m_log_back <- expm1(fc_arima_acc_m_log$mean)
test_ts_arima_acc_m_log_back <- expm1(test_ts_arima_acc_m_log)
rmse_val_arima_acc_m_log <- rmse(test_ts_arima_acc_m_log_back, fc_arima_acc_m_log_back)
mae_val_arima_acc_m_log  <- mae(test_ts_arima_acc_m_log_back, fc_arima_acc_m_log_back)
mape_val_arima_acc_m_log <- mape(test_ts_arima_acc_m_log_back, fc_arima_acc_m_log_back) * 100
summary(model_arima_acc_m_log)
cat("AIC:", AIC(model_arima_acc_m_log), "\n")
cat("BIC:", BIC(model_arima_acc_m_log), "\n")
cat("RMSE:", rmse_val_arima_acc_m_log, "\n")
cat("MAE:", mae_val_arima_acc_m_log, "\n")
cat("MAPE:", mape_val_arima_acc_m_log, "%\n")
AIC(model_arima_acc_m_log)
BIC(model_arima_acc_m_log)

#Best for now is ARIMA(2,1,1), lets see with Auto.Arima#

model_auto_arima_acc_m_log <- auto.arima(
  train_ts_arima_acc_m_log,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  max.p = 3,
  max.q = 3,
  max.d = 3,
  max.order = 10,   
  trace = TRUE
)
fc_auto_arima_acc_m_log <- forecast(model_auto_arima_acc_m_log, h = length(test_ts_arima_acc_m_log))
summary(model_auto_arima_acc_m_log)
fc_auto_arima_acc_m_log_back <- expm1(fc_auto_arima_acc_m_log$mean)
rmse_val_auto_arima_acc_m_log  <- rmse(test_ts_arima_acc_m_log_back, fc_auto_arima_acc_m_log_back)
mae_val_auto_arima_acc_m_log   <- mae(test_ts_arima_acc_m_log_back, fc_auto_arima_acc_m_log_back)
mape_val_auto_arima_acc_m_log <- mape(test_ts_arima_acc_m_log_back, fc_auto_arima_acc_m_log_back) * 100
cat("AIC:", AIC(model_auto_arima_acc_m_log), "\n")
cat("BIC:", BIC(model_auto_arima_acc_m_log), "\n")
cat("RMSE:", rmse_val_auto_arima_acc_m_log, "\n")
cat("MAE:", mae_val_auto_arima_acc_m_log, "\n")
cat("MAPE:", mape_val_auto_arima_acc_m_log, "%\n")

#Best for Auto.Arima is ARIMA(3,1,2) with drift, lets try both#

model_final_arima_manual_acc_m_log <- Arima(
  acc_m_ts_log,               
  order = c(2, 1, 1),
  include.drift = FALSE    
)
fc_final_arima_manual_acc_m_log  <- forecast(model_final_arima_manual_acc_m_log , h = 48)
summary(fc_final_arima_manual_acc_m_log)
summary(model_final_arima_manual_acc_m_log)
plot(fc_final_arima_manual_acc_m_log , main = "Forecast of Accidents until 2026")
forecast_values_arima_manual_acc_m_log  <- data.frame(fc_final_arima_manual_acc_m_log)
head(forecast_values_arima_manual_acc_m_log)
forecast_values_arima_manual_acc_m_log_back <- expm1(forecast_values_arima_manual_acc_m_log)
forecast_values_arima_manual_acc_m_log_back
forecast_values_arima_manual_acc_m_log_back$Date <- rownames(forecast_values_arima_manual_acc_m_log_back)
rownames(forecast_values_arima_manual_acc_m_log_back) <- NULL
forecast_values_arima_manual_acc_m_log_back$Date <- trimws(forecast_values_arima_manual_acc_m_log_back$Date)
forecast_values_arima_manual_acc_m_log_back$Date <- dmy(paste("01", forecast_values_arima_manual_acc_m_log_back$Date))
ggplot() +
  geom_line(data = forecast_values_arima_manual_acc_m_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_arima_manual_acc_m_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_arima_manual_acc_m_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = `accidents`), color = "red", linetype = "solid") +
  labs(title = "Forecast of Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Values") +
  theme_minimal()
model_final_arima_auto_acc_m_log <- Arima(
  acc_m_ts_log,               
  order = c(3, 1, 2),
  include.drift = TRUE    
)
fc_final_arima_auto_acc_m_log <- forecast(model_final_arima_auto_acc_m_log, h = 48)
summary(fc_final_arima_auto_acc_m_log)
summary(model_final_arima_auto_acc_m_log)
plot(fc_final_arima_auto_acc_m_log, main = "Forecast of Accidents until 2026")
forecast_values_arima_auto_acc_m_log <- data.frame(fc_final_arima_auto_acc_m_log)
head(forecast_values_arima_auto_acc_m_log)
forecast_values_arima_auto_acc_m_log_back <- expm1(forecast_values_arima_auto_acc_m_log)
forecast_values_arima_auto_acc_m_log_back
forecast_values_arima_auto_acc_m_log_back$Date <- rownames(forecast_values_arima_auto_acc_m_log_back)
rownames(forecast_values_arima_auto_acc_m_log_back) <- NULL
forecast_values_arima_auto_acc_m_log_back$Date <- trimws(forecast_values_arima_auto_acc_m_log_back$Date)
forecast_values_arima_auto_acc_m_log_back$Date <- dmy(paste("01", forecast_values_arima_auto_acc_m_log_back$Date))
ggplot() +
  geom_line(data = forecast_values_arima_auto_acc_m_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_arima_auto_acc_m_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_arima_auto_acc_m_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = `accidents`), color = "red", linetype = "solid") +
  labs(title = "Forecast of Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Values") +
  theme_minimal()

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_arima_manual_acc_m_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_manual_acc_m_log$coef))
checkresiduals(model_final_arima_manual_acc_m_log) 
Box.test(residuals(model_final_arima_manual_acc_m_log), lag=12)
Box.test(residuals(model_final_arima_auto_acc_m_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_auto_acc_m_log$coef))
checkresiduals(model_final_arima_auto_acc_m_log) 
Box.test(residuals(model_final_arima_auto_acc_m_log), lag=12)

##ARIMA on Log Fatalities##


#Time Series Object Creation#

fat_m_ts_log <- log1p(fat_m_ts)

#Time Series Plot#

plot(fat_m_ts_log, col = "steelblue", lwd = 2,
     main = "Monthly Road Fatalities (1996–2022)",
     ylab = "Fatalities", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(fat_m_ts_log, lag.max = 48, main = "ACF - Log Fatalities")
Pacf(fat_m_ts_log, lag.max = 48, main = "PACF - Log Fatalities")

#ADF test#

adf_fat.m_log <- adf.test(fat_m_ts_log, alternative = "stationary")
adf_fat.m_log

#KPSS test#

summary(ur.kpss(fat_m_ts_log, type = "mu"))

#Normal Differencing Since ADF and KPSS tests disagree (d = 1)#

fat_m_ts_diff_log <- diff(fat_m_ts_log, differences = 1)

#Plotting the Differenced Series#

plot(fat_m_ts_diff_log, col = "steelblue", main = "1st Difference of Log Fatalities")

#ADF test After Differencing#

adf.test(fat_m_ts_diff_log, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(fat_m_ts_diff_log, type = "mu"))

#Decompose the Original Series#

plot(decompose(fat_m_ts_log))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(fat_m_ts_diff_log, lag.max = 48, main = "ACF - Log Fatalities (differenced)")
Pacf(fat_m_ts_diff_log, lag.max = 48, main = "PACF - Log Fatalities (differenced)")

#Trying some possible ARIMA Combinations limited to max(p,q) = 3#

train_ts_arima_fat_m_log <- window(fat_m_ts_log, end = c(2017, 7))
test_ts_arima_fat_m_log  <- window(fat_m_ts_log, start = c(2017, 8))
model_arima_fat_m_log <- Arima(train_ts_arima_fat_m_log, order = c(3, 1, 1))
fc_arima_fat_m_log <- forecast(model_arima_fat_m_log, h = length(test_ts_arima_fat_m_log))
fc_arima_fat_m_log_back <- expm1(fc_arima_fat_m_log$mean)
test_ts_arima_fat_m_log_back <- expm1(test_ts_arima_fat_m_log)
rmse_val_arima_fat_m_log <- rmse(test_ts_arima_fat_m_log_back, fc_arima_fat_m_log_back)
mae_val_arima_fat_m_log  <- mae(test_ts_arima_fat_m_log_back, fc_arima_fat_m_log_back)
mape_val_arima_fat_m_log <- mape(test_ts_arima_fat_m_log_back, fc_arima_fat_m_log_back) * 100
summary(model_arima_fat_m_log)
cat("AIC:", AIC(model_arima_fat_m_log), "\n")
cat("BIC:", BIC(model_arima_fat_m_log), "\n")
cat("RMSE:", rmse_val_arima_fat_m_log, "\n")
cat("MAE:", mae_val_arima_fat_m_log, "\n")
cat("MAPE:", mape_val_arima_fat_m_log, "%\n")
AIC(model_arima_fat_m_log)
BIC(model_arima_fat_m_log)

#Best for now is ARIMA(3,1,1), lets see with Auto.Arima#

model_auto_arima_fat_m_log <- auto.arima(
  train_ts_arima_fat_m_log,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  max.p = 3,
  max.q = 3,
  max.d = 3,
  max.order = 10,   
  trace = TRUE
)
fc_auto_arima_fat_m_log <- forecast(model_auto_arima_fat_m_log, h = length(test_ts_arima_fat_m_log))
summary(model_auto_arima_fat_m_log)
fc_auto_arima_fat_m_log_back <- expm1(fc_auto_arima_fat_m_log$mean)
rmse_val_auto_arima_fat_m_log  <- rmse(test_ts_arima_fat_m_log_back, fc_auto_arima_fat_m_log_back)
mae_val_auto_arima_fat_m_log   <- mae(test_ts_arima_fat_m_log_back, fc_auto_arima_fat_m_log_back)
mape_val_auto_arima_fat_m_log <- mape(test_ts_arima_fat_m_log_back, fc_auto_arima_fat_m_log_back) * 100
cat("AIC:", AIC(model_auto_arima_fat_m_log), "\n")
cat("BIC:", BIC(model_auto_arima_fat_m_log), "\n")
cat("RMSE:", rmse_val_auto_arima_fat_m_log, "\n")
cat("MAE:", mae_val_auto_arima_fat_m_log, "\n")
cat("MAPE:", mape_val_auto_arima_fat_m_log, "%\n")

#Best for Auto.Arima is ARIMA(3,1,2) with drift, lets try both#

model_final_arima_manual_fat_m_log <- Arima(
  fat_m_ts_log,               
  order = c(3, 1, 1),
  include.drift = FALSE    
)
fc_final_arima_manual_fat_m_log  <- forecast(model_final_arima_manual_fat_m_log , h = 48)
summary(fc_final_arima_manual_fat_m_log)
summary(model_final_arima_manual_fat_m_log)
plot(fc_final_arima_manual_fat_m_log , main = "Forecast of Fatalities until 2026")
forecast_values_arima_manual_fat_m_log  <- data.frame(fc_final_arima_manual_fat_m_log)
head(forecast_values_arima_manual_fat_m_log)
forecast_values_arima_manual_fat_m_log_back <- expm1(forecast_values_arima_manual_fat_m_log)
forecast_values_arima_manual_fat_m_log_back
forecast_values_arima_manual_fat_m_log_back$Date <- rownames(forecast_values_arima_manual_fat_m_log_back)
rownames(forecast_values_arima_manual_fat_m_log_back) <- NULL
forecast_values_arima_manual_fat_m_log_back$Date <- trimws(forecast_values_arima_manual_fat_m_log_back$Date)
forecast_values_arima_manual_fat_m_log_back$Date <- dmy(paste("01", forecast_values_arima_manual_fat_m_log_back$Date))
ggplot() +
  geom_line(data = forecast_values_arima_manual_fat_m_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_arima_manual_fat_m_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_arima_manual_fat_m_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = `fatalities`), color = "red", linetype = "solid") +
  labs(title = "Forecast of Fatalities with 80% and 95% Confidence Intervals",
       x = "Date", y = "Values") +
  theme_minimal()
model_final_arima_auto_fat_m_log <- Arima(
  fat_m_ts_log,               
  order = c(3, 1, 2),
  include.drift = TRUE    
)
fc_final_arima_auto_fat_m_log <- forecast(model_final_arima_auto_fat_m_log, h = 48)
summary(fc_final_arima_auto_fat_m_log)
summary(model_final_arima_auto_fat_m_log)
plot(fc_final_arima_auto_fat_m_log, main = "Forecast of Fatalities until 2026")
forecast_values_arima_auto_fat_m_log <- data.frame(fc_final_arima_auto_fat_m_log)
head(forecast_values_arima_auto_fat_m_log)
forecast_values_arima_auto_fat_m_log_back <- expm1(forecast_values_arima_auto_fat_m_log)
forecast_values_arima_auto_fat_m_log_back
forecast_values_arima_auto_fat_m_log_back$Date <- rownames(forecast_values_arima_auto_fat_m_log_back)
rownames(forecast_values_arima_auto_fat_m_log_back) <- NULL
forecast_values_arima_auto_fat_m_log_back$Date <- trimws(forecast_values_arima_auto_fat_m_log_back$Date)
forecast_values_arima_auto_fat_m_log_back$Date <- dmy(paste("01", forecast_values_arima_auto_fat_m_log_back$Date))
ggplot() +
  geom_line(data = forecast_values_arima_auto_fat_m_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_arima_auto_fat_m_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_arima_auto_fat_m_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = `fatalities`), color = "red", linetype = "solid") +
  labs(title = "Forecast of Fatalities with 80% and 95% Confidence Intervals",
       x = "Date", y = "Values") +
  theme_minimal()

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_arima_manual_fat_m_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_manual_fat_m_log$coef))
checkresiduals(model_final_arima_manual_fat_m_log) 
Box.test(residuals(model_final_arima_manual_fat_m_log), lag=12)
Box.test(residuals(model_final_arima_auto_fat_m_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_arima_auto_fat_m_log$coef))
checkresiduals(model_final_arima_auto_fat_m_log) 
Box.test(residuals(model_final_arima_auto_fat_m_log), lag=12)

##SARIMA on Regular Accidents##


#Time Series Object Creation#

acc_m_ts_sa <- ts(acc.m.ind.y_mod$accidents, start = c(1996, 1), frequency = 12)

#Time Series Plots#

plot(acc_m_ts_sa, col = "steelblue", lwd = 2,
     main = "Monthly Road Accidents (1996–2022)",
     ylab = "Accidents", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(acc_m_ts_sa, lag.max = 48, main = "ACF - Normal Accidents")
Pacf(acc_m_ts_sa, lag.max = 48, main = "PACF - Normal Accidents")

#ADF test#

adf_acc.m_sa <- adf.test(acc_m_ts_sa, alternative = "stationary")
adf_acc.m_sa

#KPSS test#

summary(ur.kpss(acc_m_ts_sa, type = "mu"))

#Seasonal Differencing Since ADF and KPSS tests disagree (D = 1)#

acc_m_ts_seasonal_diff_sa <- diff(acc_m_ts_sa, lag = 12)

#Plotting the Differenced Series#

plot(acc_m_ts_seasonal_diff_sa, col = "steelblue", main = "1st Difference of Normal Accidents")

#ADF test After Differencing#

adf.test(acc_m_ts_seasonal_diff_sa, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(acc_m_ts_seasonal_diff_sa, type = "mu"))

#Decompose the Original Series#

plot(decompose(acc_m_ts_sa))
par(mfrow = c(1, 1))  

#Plotting ACF and PACF of the Differenced Time Series#

Acf(acc_m_ts_seasonal_diff_sa, lag.max = 48, main = "ACF - Normal Accidents (Differenced)")
Pacf(acc_m_ts_seasonal_diff_sa, lag.max = 48, main = "PACF - Normal Accidents (Differenced)")

#Trying some possible SARIMA Combinations#

train_ts_sarima_acc_m <- window(acc_m_ts_sa, end = c(2017, 7))
test_ts_sarima_acc_m  <- window(acc_m_ts_sa, start = c(2017, 8))
model_sarima_acc_m <- Arima(train_ts_sarima_acc_m, order = c(1, 0, 2), seasonal = list(order = c(0, 1, 1), period = 12))
fc_sarima_acc_m <- forecast(model_sarima_acc_m, h = length(test_ts_sarima_acc_m))
rmse_val_sarima_acc_m <- rmse(test_ts_sarima_acc_m, fc_sarima_acc_m$mean)
mae_val_sarima_acc_m  <- mae(test_ts_sarima_acc_m, fc_sarima_acc_m$mean)
mape_val_sarima_acc_m <- mape(test_ts_sarima_acc_m, fc_sarima_acc_m$mean) * 100
summary(model_sarima_acc_m)
cat("AIC:", AIC(model_sarima_acc_m), "\n")
cat("BIC:", BIC(model_sarima_acc_m), "\n")
cat("RMSE:", rmse_val_sarima_acc_m, "\n")
cat("MAE:", mae_val_sarima_acc_m, "\n")
cat("MAPE:", mape_val_sarima_acc_m, "%\n")
AIC(model_sarima_acc_m)
BIC(model_sarima_acc_m)

#Best for now is SARIMA(1,0,2)(0,1,1)[12], lets see with Auto.Arima#

model_auto_sarima_acc_m <- auto.arima(
  train_ts_sarima_acc_m,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_sarima_acc_m <- forecast(model_auto_sarima_acc_m, h = length(test_ts_sarima_acc_m))
summary(fc_auto_sarima_acc_m)
summary(model_auto_sarima_acc_m)
rmse_val_auto_sarima_acc_m  <- rmse(test_ts_sarima_acc_m, fc_auto_sarima_acc_m$mean)
mae_val_auto_sarima_acc_m   <- mae(test_ts_sarima_acc_m, fc_auto_sarima_acc_m$mean)
mape_val_auto_sarima_acc_m <- mape(test_ts_sarima_acc_m, fc_auto_sarima_acc_m$mean) * 100
cat("AIC:", AIC(model_auto_sarima_acc_m), "\n")
cat("BIC:", BIC(model_auto_sarima_acc_m), "\n")
cat("RMSE:", rmse_val_auto_sarima_acc_m, "\n")
cat("MAE:", mae_val_auto_sarima_acc_m, "\n")
cat("MAPE:", mape_val_auto_sarima_acc_m, "%\n")

#Best for Auto.Arima is SARIMA(1,0,2)(0,1,1)[12] also#

model_final_sarima_acc_m <- Arima(
  acc_m_ts_sa,               
  order = c(1, 0, 2),
  seasonal = list(order = c(0,1,1), period = 12)
)
fc_final_sarima_acc_m  <- forecast(model_final_sarima_acc_m , h = 48)
summary(fc_final_sarima_acc_m)
summary(model_final_sarima_acc_m)
plot(fc_final_sarima_acc_m , main = "Forecast of Accidents until 2026")
forecast_values_sarima_acc_m  <- data.frame(fc_final_sarima_acc_m)
head(forecast_values_sarima_acc_m)

#Checking Residuals for the model after fitting#

Box.test(residuals(model_final_sarima_acc_m), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_acc_m$coef))
checkresiduals(model_final_sarima_acc_m) 
Box.test(residuals(model_final_sarima_acc_m), lag=12)

##SARIMA on Regular Fatalities##


#Time Series Object Creation#

fat_m_ts_sa <- ts(acc.m.ind.y_mod$fatalities, start = c(1996, 1), frequency = 12)

#Time Series Plot#

plot(fat_m_ts_sa, col = "darkred", lwd = 2,
     main = "Monthly Road Fatalities (1996–2022)",
     ylab = "Fatalities", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(fat_m_ts_sa, lag.max = 48, main = "ACF - Normal Fatalities")
Pacf(fat_m_ts_sa, lag.max = 48, main = "PACF - Normal Fatalities")

#ADF test#

adf_fat.m_sa <- adf.test(fat_m_ts_sa, alternative = "stationary")
adf_fat.m_sa

#KPSS test#

summary(ur.kpss(fat_m_ts_sa, type = "mu"))

#Seasonal Differencing Since ADF and KPSS tests disagree (D = 1)#

fat_m_ts_seasonal_diff_sa <- diff(fat_m_ts_sa, lag = 12)

#Plotting the Differenced Series#

plot(fat_m_ts_seasonal_diff_sa, col = "darkred", main = "1st Difference of Normal Fatalities")

#ADF test After Differencing#

adf.test(fat_m_ts_seasonal_diff_sa, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(fat_m_ts_seasonal_diff_sa, type = "mu"))

#Decompose the Original Series#

plot(decompose(fat_m_ts_sa))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(fat_m_ts_seasonal_diff_sa, lag.max = 48, main = "ACF - Normal Fatalities (differenced)")
Pacf(fat_m_ts_seasonal_diff_sa, lag.max = 48, main = "PACF - Normal Fatalities (differenced)")

#Trying some possible SARIMA Combinations#

train_ts_sarima_fat_m <- window(fat_m_ts_sa, end = c(2017, 7))
test_ts_sarima_fat_m  <- window(fat_m_ts_sa, start = c(2017, 8))
model_sarima_fat_m <- Arima(train_ts_sarima_fat_m, order = c(1, 0, 1), seasonal = list(order = c(1, 1, 1), period = 12))
fc_sarima_fat_m <- forecast(model_sarima_fat_m, h = length(test_ts_sarima_fat_m))
rmse_val_sarima_fat_m <- rmse(test_ts_sarima_fat_m, fc_sarima_fat_m$mean)
mae_val_sarima_fat_m  <- mae(test_ts_sarima_fat_m, fc_sarima_fat_m$mean)
mape_val_sarima_fat_m <- mape(test_ts_sarima_fat_m, fc_sarima_fat_m$mean) * 100
summary(model_sarima_fat_m)
cat("AIC:", AIC(model_sarima_fat_m), "\n")
cat("BIC:", BIC(model_sarima_fat_m), "\n")
cat("RMSE:", rmse_val_sarima_fat_m, "\n")
cat("MAE:", mae_val_sarima_fat_m, "\n")
cat("MAPE:", mape_val_sarima_fat_m, "%\n")
AIC(model_sarima_fat_m)
BIC(model_sarima_fat_m)

#Best for now is SARIMA(1,0,1)(1,1,1)[12], lets see with Auto.Arima#

model_auto_sarima_fat_m <- auto.arima(
  train_ts_sarima_fat_m,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_sarima_fat_m <- forecast(model_auto_sarima_fat_m, h = length(test_ts_sarima_fat_m))
summary(fc_auto_sarima_fat_m)
summary(model_auto_sarima_fat_m)
rmse_val_auto_sarima_fat_m  <- rmse(test_ts_sarima_fat_m, fc_auto_sarima_fat_m$mean)
mae_val_auto_sarima_fat_m   <- mae(test_ts_sarima_fat_m, fc_auto_sarima_fat_m$mean)
mape_val_auto_sarima_fat_m <- mape(test_ts_sarima_fat_m, fc_auto_sarima_fat_m$mean) * 100
cat("AIC:", AIC(model_auto_sarima_fat_m), "\n")
cat("BIC:", BIC(model_auto_sarima_fat_m), "\n")
cat("RMSE:", rmse_val_auto_sarima_fat_m, "\n")
cat("MAE:", mae_val_auto_sarima_fat_m, "\n")
cat("MAPE:", mape_val_auto_sarima_fat_m, "%\n")

#Best for Auto.Arima is SARIMA(1,0,1)(1,1,1)[12] with drift, lets try both#

model_final_sarima_manual_fat_m <- Arima(
  fat_m_ts_sa,               
  order = c(1, 0, 1),
  seasonal = list(order = c(1,1,1), period = 12),
  include.drift = FALSE
)
fc_final_sarima_manual_fat_m  <- forecast(model_final_sarima_manual_fat_m , h = 48)
summary(fc_final_sarima_manual_fat_m)
summary(model_final_sarima_manual_fat_m)
plot(fc_final_sarima_manual_fat_m , main = "Forecast of Fatalities until 2026")
forecast_values_sarima_manual_fat_m  <- data.frame(fc_final_sarima_manual_fat_m)
head(forecast_values_sarima_manual_fat_m)
model_final_sarima_auto_fat_m <- Arima(
  fat_m_ts_sa,               
  order = c(1, 0, 1),
  seasonal = list(order = c(1,1,1), period = 12),
  include.drift = TRUE
)
fc_final_sarima_auto_fat_m <- forecast(model_final_sarima_auto_fat_m, h = 48)
summary(fc_final_sarima_auto_fat_m)
summary(model_final_sarima_auto_fat_m)
plot(fc_final_sarima_auto_fat_m, main = "Forecast of Fatalities until 2026")
forecast_values_sarima_auto_fat_m <- data.frame(fc_final_sarima_auto_fat_m)
head(forecast_values_sarima_auto_fat_m)

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_sarima_manual_fat_m), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_manual_fat_m$coef))
checkresiduals(model_final_sarima_manual_fat_m) 
Box.test(residuals(model_final_sarima_manual_fat_m), lag=12)
Box.test(residuals(model_final_sarima_auto_fat_m), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_auto_fat_m$coef))
checkresiduals(model_final_sarima_auto_fat_m) 
Box.test(residuals(model_final_sarima_auto_fat_m), lag=12)

##SARIMA on Log Accidents##


#Time Series Object Creation#

acc_m_ts_log_sa <- log1p(acc_m_ts_sa)

#Time Series Plot#

plot(acc_m_ts_log_sa, col = "steelblue", lwd = 2,
     main = "Monthly Road Accidents (1996–2022)",
     ylab = "Accidents", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(acc_m_ts_log_sa, lag.max = 48, main = "ACF - Log Accidents")
Pacf(acc_m_ts_log_sa, lag.max = 48, main = "PACF - Log Accidents")

#ADF test#

adf_acc.m_log_sa <- adf.test(acc_m_ts_log_sa, alternative = "stationary")
adf_acc.m_log_sa

#KPSS test#

summary(ur.kpss(acc_m_ts_log_sa, type = "mu"))

#Seasonal Differencing Since ADF and KPSS tests disagree (D = 1)#

acc_m_ts_seasonal_diff_log_sa <- diff(acc_m_ts_sa, lag = 12)

#Plotting the Differenced Series#

plot(acc_m_ts_seasonal_diff_log_sa, col = "steelblue", main = "1st Difference of Log Accidents")

#ADF test After Differencing#

adf.test(acc_m_ts_seasonal_diff_log_sa, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(acc_m_ts_seasonal_diff_log_sa, type = "mu"))

#Decompose the Original Series#

plot(decompose(acc_m_ts_log_sa))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(acc_m_ts_seasonal_diff_log_sa, lag.max = 48, main = "ACF - Log Accidents (differenced)")
Pacf(acc_m_ts_seasonal_diff_log_sa, lag.max = 48, main = "PACF - Log Accidents (differenced)")

#Trying some possible SARIMA Combinations#

train_ts_sarima_acc_m_log <- window(acc_m_ts_log_sa, end = c(2017, 7))
test_ts_sarima_acc_m_log  <- window(acc_m_ts_log_sa, start = c(2017, 8))
model_sarima_acc_m_log <- Arima(train_ts_sarima_acc_m_log, order = c(2, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
fc_sarima_acc_m_log <- forecast(model_sarima_acc_m_log, h = length(test_ts_sarima_acc_m_log))
fc_sarima_acc_m_log_back <- expm1(fc_sarima_acc_m_log$mean)
test_ts_sarima_acc_m_log_back <- expm1(test_ts_sarima_acc_m_log)
rmse_val_sarima_acc_m_log <- rmse(test_ts_sarima_acc_m_log_back, fc_sarima_acc_m_log_back)
mae_val_sarima_acc_m_log  <- mae(test_ts_sarima_acc_m_log_back, fc_sarima_acc_m_log_back)
mape_val_sarima_acc_m_log <- mape(test_ts_sarima_acc_m_log_back, fc_sarima_acc_m_log_back) * 100
summary(model_sarima_acc_m_log)
cat("AIC:", AIC(model_sarima_acc_m_log), "\n")
cat("BIC:", BIC(model_sarima_acc_m_log), "\n")
cat("RMSE:", rmse_val_sarima_acc_m_log, "\n")
cat("MAE:", mae_val_sarima_acc_m_log, "\n")
cat("MAPE:", mape_val_sarima_acc_m_log, "%\n")
AIC(model_sarima_acc_m_log)
BIC(model_sarima_acc_m_log)

#Best for now is SARIMA(2,1,1)(0,1,1)[12], lets see with Auto.Arima#

model_auto_sarima_acc_m_log <- auto.arima(
  train_ts_sarima_acc_m_log,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_sarima_acc_m_log <- forecast(model_auto_sarima_acc_m_log, h = length(test_ts_sarima_acc_m_log))
summary(model_auto_sarima_acc_m_log)
fc_auto_sarima_acc_m_log_back <- expm1(fc_auto_sarima_acc_m_log$mean)
rmse_val_auto_sarima_acc_m_log  <- rmse(test_ts_sarima_acc_m_log_back, fc_auto_sarima_acc_m_log_back)
mae_val_auto_sarima_acc_m_log   <- mae(test_ts_sarima_acc_m_log_back, fc_auto_sarima_acc_m_log_back)
mape_val_auto_sarima_acc_m_log <- mape(test_ts_sarima_acc_m_log_back, fc_auto_sarima_acc_m_log_back) * 100
cat("AIC:", AIC(model_auto_sarima_acc_m_log), "\n")
cat("BIC:", BIC(model_auto_sarima_acc_m_log), "\n")
cat("RMSE:", rmse_val_auto_sarima_acc_m_log, "\n")
cat("MAE:", mae_val_auto_sarima_acc_m_log, "\n")
cat("MAPE:", mape_val_auto_sarima_acc_m_log, "%\n")

#Best for Auto.Arima is SARIMA(1,0,2)(0,1,1)[12] lets try both#

model_final_sarima_manual_acc_m_log <- Arima(
  acc_m_ts_log_sa,               
  order = c(2, 1, 1),
  seasonal = list(order = c(0,1,1), period = 12),
  include.drift = FALSE    
)
fc_final_sarima_manual_acc_m_log  <- forecast(model_final_sarima_manual_acc_m_log , h = 48)
summary(fc_final_sarima_manual_acc_m_log)
summary(model_final_sarima_manual_acc_m_log)
plot(fc_final_sarima_manual_acc_m_log , main = "Forecast of Accidents until 2026")
forecast_values_sarima_manual_acc_m_log  <- data.frame(fc_final_sarima_manual_acc_m_log)
head(forecast_values_sarima_manual_acc_m_log)
forecast_values_sarima_manual_acc_m_log_back <- expm1(forecast_values_sarima_manual_acc_m_log)
forecast_values_sarima_manual_acc_m_log_back
forecast_values_sarima_manual_acc_m_log_back$Date <- rownames(forecast_values_sarima_manual_acc_m_log_back)
rownames(forecast_values_sarima_manual_acc_m_log_back) <- NULL
forecast_values_sarima_manual_acc_m_log_back$Date <- trimws(forecast_values_sarima_manual_acc_m_log_back$Date)
forecast_values_sarima_manual_acc_m_log_back$Date <- dmy(paste("01", forecast_values_sarima_manual_acc_m_log_back$Date))
ggplot() +
  geom_line(data = forecast_values_sarima_manual_acc_m_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_sarima_manual_acc_m_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_sarima_manual_acc_m_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = `accidents`), color = "red", linetype = "solid") +
  labs(title = "Forecast of Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Values") +
  theme_minimal()
model_final_sarima_auto_acc_m_log <- Arima(
  acc_m_ts_log_sa,               
  order = c(1, 0, 2),
  seasonal = list(order = c(0,1,1), period = 12),
  include.drift = FALSE    
)
fc_final_sarima_auto_acc_m_log <- forecast(model_final_sarima_auto_acc_m_log, h = 48)
summary(fc_final_sarima_auto_acc_m_log)
summary(model_final_sarima_auto_acc_m_log)
plot(fc_final_sarima_auto_acc_m_log, main = "Forecast of Accidents until 2026")
forecast_values_sarima_auto_acc_m_log <- data.frame(fc_final_sarima_auto_acc_m_log)
head(forecast_values_sarima_auto_acc_m_log)
forecast_values_sarima_auto_acc_m_log_back <- expm1(forecast_values_sarima_auto_acc_m_log)
forecast_values_sarima_auto_acc_m_log_back
forecast_values_sarima_auto_acc_m_log_back$Date <- rownames(forecast_values_sarima_auto_acc_m_log_back)
rownames(forecast_values_sarima_auto_acc_m_log_back) <- NULL
forecast_values_sarima_auto_acc_m_log_back$Date <- trimws(forecast_values_sarima_auto_acc_m_log_back$Date)
forecast_values_sarima_auto_acc_m_log_back$Date <- dmy(paste("01", forecast_values_sarima_auto_acc_m_log_back$Date))
ggplot() +
  geom_line(data = forecast_values_sarima_auto_acc_m_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_sarima_auto_acc_m_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_sarima_auto_acc_m_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = `accidents`), color = "red", linetype = "solid") +
  labs(title = "Forecast of Accidents with 80% and 95% Confidence Intervals",
       x = "Date", y = "Values") +
  theme_minimal()

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_sarima_manual_acc_m_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_manual_acc_m_log$coef))
checkresiduals(model_final_sarima_manual_acc_m_log) 
Box.test(residuals(model_final_sarima_manual_acc_m_log), lag=12)
Box.test(residuals(model_final_sarima_auto_acc_m_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_auto_acc_m_log$coef))
checkresiduals(model_final_sarima_auto_acc_m_log) 
Box.test(residuals(model_final_sarima_auto_acc_m_log), lag=12)

##SARIMA on Log Fatalities##


#Time Series Object Creation#

fat_m_ts_log_sa <- log1p(fat_m_ts_sa)

#Time Series Plot#

plot(fat_m_ts_log_sa, col = "steelblue", lwd = 2,
     main = "Monthly Road Fatalities (1996–2022)",
     ylab = "Fatalities", xlab = "Time")

#ACF and PACF plots of Original Time Series#

Acf(fat_m_ts_log_sa, lag.max = 48, main = "ACF - Log Fatalities")
Pacf(fat_m_ts_log_sa, lag.max = 48, main = "PACF - Log Fatalities")

#ADF test#

adf_fat.m_log_sa <- adf.test(fat_m_ts_log_sa, alternative = "stationary")
adf_fat.m_log_sa

#KPSS test#

summary(ur.kpss(fat_m_ts_log_sa, type = "mu"))

#Seasonal Differencing Since ADF and KPSS tests disagree (D = 1)#

fat_m_ts_seasonal_diff_log_sa <- diff(fat_m_ts_sa, lag = 12)

#Plotting the Differenced Series#

plot(fat_m_ts_seasonal_diff_log_sa, col = "steelblue", main = "1st Difference of Log Fatalities")

#ADF test After Differencing#

adf.test(fat_m_ts_seasonal_diff_log_sa, alternative = "stationary")

#KPSS test After Differencing#

summary(ur.kpss(fat_m_ts_seasonal_diff_log_sa, type = "mu"))

#Decompose the Original Series#

plot(decompose(fat_m_ts_log_sa))

#Plotting ACF and PACF of the Differenced Time Series#

Acf(fat_m_ts_seasonal_diff_log_sa, lag.max = 48, main = "ACF - Log Fatalities (differenced)")
Pacf(fat_m_ts_seasonal_diff_log_sa, lag.max = 48, main = "PACF - Log Fatalities (differenced)")

#Trying some possible SARIMA Combinations#

train_ts_sarima_fat_m_log <- window(fat_m_ts_log_sa, end = c(2017, 7))
test_ts_sarima_fat_m_log  <- window(fat_m_ts_log_sa, start = c(2017, 8))
model_sarima_fat_m_log <- Arima(train_ts_sarima_fat_m_log, order = c(2, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))
fc_sarima_fat_m_log <- forecast(model_sarima_fat_m_log, h = length(test_ts_sarima_fat_m_log))
fc_sarima_fat_m_log_back <- expm1(fc_sarima_fat_m_log$mean)
test_ts_sarima_fat_m_log_back <- expm1(test_ts_sarima_fat_m_log)
rmse_val_sarima_fat_m_log <- rmse(test_ts_sarima_fat_m_log_back, fc_sarima_fat_m_log_back)
mae_val_sarima_fat_m_log  <- mae(test_ts_sarima_fat_m_log_back, fc_sarima_fat_m_log_back)
mape_val_sarima_fat_m_log <- mape(test_ts_sarima_fat_m_log_back, fc_sarima_fat_m_log_back) * 100
summary(model_sarima_fat_m_log)
cat("AIC:", AIC(model_sarima_fat_m_log), "\n")
cat("BIC:", BIC(model_sarima_fat_m_log), "\n")
cat("RMSE:", rmse_val_sarima_fat_m_log, "\n")
cat("MAE:", mae_val_sarima_fat_m_log, "\n")
cat("MAPE:", mape_val_sarima_fat_m_log, "%\n")
AIC(model_sarima_fat_m_log)
BIC(model_sarima_fat_m_log)

#Best for now is SARIMA(2,1,1)(1,1,1)[12], lets see with Auto.Arima#

model_auto_sarima_fat_m_log <- auto.arima(
  train_ts_sarima_fat_m_log,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
fc_auto_sarima_fat_m_log <- forecast(model_auto_sarima_fat_m_log, h = length(test_ts_sarima_fat_m_log))
summary(model_auto_sarima_fat_m_log)
fc_auto_sarima_fat_m_log_back <- expm1(fc_auto_sarima_fat_m_log$mean)
rmse_val_auto_sarima_fat_m_log  <- rmse(test_ts_sarima_fat_m_log_back, fc_auto_sarima_fat_m_log_back)
mae_val_auto_sarima_fat_m_log   <- mae(test_ts_sarima_fat_m_log_back, fc_auto_sarima_fat_m_log_back)
mape_val_auto_sarima_fat_m_log <- mape(test_ts_sarima_fat_m_log_back, fc_auto_sarima_fat_m_log_back) * 100
cat("AIC:", AIC(model_auto_sarima_fat_m_log), "\n")
cat("BIC:", BIC(model_auto_sarima_fat_m_log), "\n")
cat("RMSE:", rmse_val_auto_sarima_fat_m_log, "\n")
cat("MAE:", mae_val_auto_sarima_fat_m_log, "\n")
cat("MAPE:", mape_val_auto_sarima_fat_m_log, "%\n")

#Best for Auto.Arima is SARIMA(0,1,1)(1,1,1)[12] lets try both#

model_final_sarima_manual_fat_m_log <- Arima(
  fat_m_ts_log_sa,               
  order = c(2, 1, 1),
  seasonal = list(order = c(1,1,1), period = 12),
  include.drift = FALSE    
)
fc_final_sarima_manual_fat_m_log  <- forecast(model_final_sarima_manual_fat_m_log , h = 48)
summary(fc_final_sarima_manual_fat_m_log)
summary(model_final_sarima_manual_fat_m_log)
plot(fc_final_sarima_manual_fat_m_log , main = "Forecast of Fatalities until 2026")
forecast_values_sarima_manual_fat_m_log  <- data.frame(fc_final_sarima_manual_fat_m_log)
head(forecast_values_sarima_manual_fat_m_log)
forecast_values_sarima_manual_fat_m_log_back <- expm1(forecast_values_sarima_manual_fat_m_log)
forecast_values_sarima_manual_fat_m_log_back
forecast_values_sarima_manual_fat_m_log_back$Date <- rownames(forecast_values_sarima_manual_fat_m_log_back)
rownames(forecast_values_sarima_manual_fat_m_log_back) <- NULL
forecast_values_sarima_manual_fat_m_log_back$Date <- trimws(forecast_values_sarima_manual_fat_m_log_back$Date)
forecast_values_sarima_manual_fat_m_log_back$Date <- dmy(paste("01", forecast_values_sarima_manual_fat_m_log_back$Date))
ggplot() +
  geom_line(data = forecast_values_sarima_manual_fat_m_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_sarima_manual_fat_m_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_sarima_manual_fat_m_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = `fatalities`), color = "red", linetype = "solid") +
  labs(title = "Forecast of Fatalities with 80% and 95% Confidence Intervals",
       x = "Date", y = "Values") +
  theme_minimal()
model_final_sarima_auto_fat_m_log <- Arima(
  fat_m_ts_log_sa,               
  order = c(0, 1, 1),
  seasonal = list(order = c(1,1,1), period = 12),
  include.drift = FALSE    
)
fc_final_sarima_auto_fat_m_log <- forecast(model_final_sarima_auto_fat_m_log, h = 48)
summary(fc_final_sarima_auto_fat_m_log)
summary(model_final_sarima_auto_fat_m_log)
plot(fc_final_sarima_auto_fat_m_log, main = "Forecast of Fatalities until 2026")
forecast_values_sarima_auto_fat_m_log <- data.frame(fc_final_sarima_auto_fat_m_log)
head(forecast_values_sarima_auto_fat_m_log)
forecast_values_sarima_auto_fat_m_log_back <- expm1(forecast_values_sarima_auto_fat_m_log)
forecast_values_sarima_auto_fat_m_log_back
forecast_values_sarima_auto_fat_m_log_back$Date <- rownames(forecast_values_sarima_auto_fat_m_log_back)
rownames(forecast_values_sarima_auto_fat_m_log_back) <- NULL
forecast_values_sarima_auto_fat_m_log_back$Date <- trimws(forecast_values_sarima_auto_fat_m_log_back$Date)
forecast_values_sarima_auto_fat_m_log_back$Date <- dmy(paste("01", forecast_values_sarima_auto_fat_m_log_back$Date))
ggplot() +
  geom_line(data = forecast_values_sarima_auto_fat_m_log_back, aes(x = Date, y = Point.Forecast), color = "blue") +
  geom_ribbon(data = forecast_values_sarima_auto_fat_m_log_back, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = forecast_values_sarima_auto_fat_m_log_back, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = `fatalities`), color = "red", linetype = "solid") +
  labs(title = "Forecast of Fatalities with 80% and 95% Confidence Intervals",
       x = "Date", y = "Values") +
  theme_minimal()

#Checking Residuals for both models after fitting#

Box.test(residuals(model_final_sarima_manual_fat_m_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_manual_fat_m_log$coef))
checkresiduals(model_final_sarima_manual_fat_m_log) 
Box.test(residuals(model_final_sarima_manual_fat_m_log), lag=12)
Box.test(residuals(model_final_sarima_auto_fat_m_log), lag = 10, type = 'Ljung-Box',fitdf = length(model_final_sarima_auto_fat_m_log$coef))
checkresiduals(model_final_sarima_auto_fat_m_log) 
Box.test(residuals(model_final_sarima_auto_fat_m_log), lag=12)

####Grid Searches####




###WEEKLY###



###ARIMA###



##REGULAR ACCIDENTS##


#Prepare time series#

acc_w_ts_gs <- ts(acc.w.ind.y_4wmod$accidents, start = c(1996, 1), frequency = 48)
train_ts_arima_acc_w_gs <- window(acc_w_ts_gs, end = c(2017, 29))
test_ts_arima_acc_w_gs  <- window(acc_w_ts_gs, start = c(2017, 30))
p_gs_acc <- 0:3; d_gs_acc <- 0:3; q_gs_acc <- 0:3
results_gs_arima_acc <- list()
tried_models_gs_arima_acc <- 0
total_combinations_gs_arima_acc <- length(p_gs_acc) * length(d_gs_acc) * length(q_gs_acc)
cat("🔄 Total combinations to try:", total_combinations_gs_arima_acc, "\n")
for (i in p_gs_acc) {
  for (j in d_gs_acc) {
    for (k in q_gs_acc) {
      tried_models_gs_arima_acc <- tried_models_gs_arima_acc + 1
      order <- c(i, j, k)
      
      cat("\nTrying model", tried_models_gs_arima_acc, "of", total_combinations_gs_arima_acc,
          "-> ARIMA(", paste(order, collapse = ","), ")\n")
      
      tryCatch({
        fit <- Arima(train_ts_arima_acc_w_gs,
                     order = order,
                     method = "ML")
        
        h <- length(test_ts_arima_acc_w_gs)
        fc <- forecast(fit, h = h)
        mape <- accuracy(fc, test_ts_arima_acc_w_gs)["Test set", "MAPE"]
        lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = length(fit$coef))
        bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
        if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
          cat("✅ Model passed residual tests. Saving...\n")
          results_gs_arima_acc <- append(results_gs_arima_acc, list(list(
            order = paste0("(", paste(order, collapse = ","), ")"),
            AIC = AIC(fit),
            BIC = BIC(fit),
            MAPE = mape,
            LB_p = lb_test$p.value,
            BP_p = bp_test$p.value
          )))
        } else {
          cat("❌ Residuals failed white noise tests.\n")
        }
      }, error = function(e) {
        message("⚠️ Error for ARIMA(", paste(order, collapse = ","), "): ", e$message)
      })
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_arima_acc), "\n")
if (length(results_gs_arima_acc) > 0) {
  results_df_gs_arima_acc <- do.call(rbind, lapply(results_gs_arima_acc, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_arima_acc <- as.data.frame(results_df_gs_arima_acc, stringsAsFactors = FALSE)
  results_df_gs_arima_acc$MAPE  <- as.numeric(results_df_gs_arima_acc$MAPE)
  results_df_gs_arima_acc$AIC   <- as.numeric(results_df_gs_arima_acc$AIC)
  results_df_gs_arima_acc$BIC   <- as.numeric(results_df_gs_arima_acc$BIC)
  results_df_gs_arima_acc$LB_p  <- as.numeric(results_df_gs_arima_acc$LB_p)
  results_df_gs_arima_acc$BP_p  <- as.numeric(results_df_gs_arima_acc$BP_p)
  results_df_sorted_gs_arima_acc <- results_df_gs_arima_acc[order(results_df_gs_arima_acc$MAPE), ]
  rownames(results_df_sorted_gs_arima_acc) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_arima_acc))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##REGULAR FATALITIES##


#Prepare the time series#

fat_w_ts_gs <- ts(acc.w.ind.y_4wmod$fatalities, start = c(1996, 1), frequency = 48)
train_ts_arima_fat_w_gs <- window(fat_w_ts_gs, end = c(2017, 29))
test_ts_arima_fat_w_gs  <- window(fat_w_ts_gs, start = c(2017, 30))
p_gs_fat <- 0:3; d_gs_fat <- 0:3; q_gs_fat <- 0:3
results_gs_arima_fat <- list()
tried_models_gs_arima_fat <- 0
total_combinations_gs_arima_fat <- length(p_gs_fat) * length(d_gs_fat) * length(q_gs_fat)
cat("🔄 Total combinations to try:", total_combinations_gs_arima_fat, "\n")
for (i in p_gs_fat) {
  for (j in d_gs_fat) {
    for (k in q_gs_fat) {
      tried_models_gs_arima_fat <- tried_models_gs_arima_fat + 1
      order <- c(i, j, k)
      cat("\nTrying model", tried_models_gs_arima_fat, "of", total_combinations_gs_arima_fat,
          "-> ARIMA(", paste(order, collapse = ","), ")\n")
      tryCatch({
        fit <- Arima(train_ts_arima_fat_w_gs,
                     order = order,
                     method = "ML")
        h <- length(test_ts_arima_fat_w_gs)
        fc <- forecast(fit, h = h)
        mape <- accuracy(fc, test_ts_arima_fat_w_gs)["Test set", "MAPE"]
        
        lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = length(fit$coef))
        bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
        if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
          cat("✅ Model passed residual tests. Saving...\n")
          results_gs_arima_fat <- append(results_gs_arima_fat, list(list(
            order = paste0("(", paste(order, collapse = ","), ")"),
            AIC = AIC(fit),
            BIC = BIC(fit),
            MAPE = mape,
            LB_p = lb_test$p.value,
            BP_p = bp_test$p.value
          )))
        } else {
          cat("❌ Residuals failed white noise tests.\n")
        }
      }, error = function(e) {
        message("⚠️ Error for ARIMA(", paste(order, collapse = ","), "): ", e$message)
      })
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_arima_fat), "\n")
if (length(results_gs_arima_fat) > 0) {
  results_df_gs_arima_fat <- do.call(rbind, lapply(results_gs_arima_fat, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_arima_fat <- as.data.frame(results_df_gs_arima_fat, stringsAsFactors = FALSE)
  
  results_df_gs_arima_fat$MAPE  <- as.numeric(results_df_gs_arima_fat$MAPE)
  results_df_gs_arima_fat$AIC   <- as.numeric(results_df_gs_arima_fat$AIC)
  results_df_gs_arima_fat$BIC   <- as.numeric(results_df_gs_arima_fat$BIC)
  results_df_gs_arima_fat$LB_p  <- as.numeric(results_df_gs_arima_fat$LB_p)
  results_df_gs_arima_fat$BP_p  <- as.numeric(results_df_gs_arima_fat$BP_p)
  
  results_df_sorted_gs_arima_fat <- results_df_gs_arima_fat[order(results_df_gs_arima_fat$MAPE), ]
  rownames(results_df_sorted_gs_arima_fat) <- NULL
  
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_arima_fat))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##LOG ACCIDENTS##


#Prepare time series#

acc_w_ts_gs_log <- log1p(ts(acc.w.ind.y_4wmod$accidents, start = c(1996, 1), frequency = 48))
train_ts_arima_acc_w_gs_log <- window(acc_w_ts_gs_log, end = c(2017, 29))
test_ts_arima_acc_w_gs_log  <- window(acc_w_ts_gs_log, start = c(2017, 30))
p_gs_acc_log <- 0:3; d_gs_acc_log <- 0:3; q_gs_acc_log <- 0:3
results_gs_arima_acc_log <- list()
tried_models_gs_arima_acc_log <- 0
total_combinations_gs_arima_acc_log <- length(p_gs_acc_log) * length(d_gs_acc_log) * length(q_gs_acc_log)
cat("🔄 Total combinations to try:", total_combinations_gs_arima_acc_log, "\n")
for (i in p_gs_acc_log) {
  for (j in d_gs_acc_log) {
    for (k in q_gs_acc_log) {
      tried_models_gs_arima_acc_log <- tried_models_gs_arima_acc_log + 1
      order <- c(i, j, k)
      cat("\nTrying model", tried_models_gs_arima_acc_log, "of", total_combinations_gs_arima_acc_log,
          "-> ARIMA(", paste(order, collapse = ","), ")\n")
      tryCatch({
        fit <- Arima(train_ts_arima_acc_w_gs_log,
                     order = order,
                     method = "ML")
        h <- length(test_ts_arima_acc_w_gs_log)
        fc <- forecast(fit, h = h)
        fc_original <- expm1(fc$mean)
        test_original <- expm1(test_ts_arima_acc_w_gs_log)
        mape <- accuracy(fc_original, test_original)["Test set", "MAPE"]
        lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = length(fit$coef))
        bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
        if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
          cat("✅ Model passed residual tests. Saving...\n")
          results_gs_arima_acc_log <- append(results_gs_arima_acc_log, list(list(
            order = paste0("(", paste(order, collapse = ","), ")"),
            AIC = AIC(fit),
            BIC = BIC(fit),
            MAPE = mape,
            LB_p = lb_test$p.value,
            BP_p = bp_test$p.value
          )))
        } else {
          cat("❌ Residuals failed white noise tests.\n")
        }
      }, error = function(e) {
        message("⚠️ Error for ARIMA(", paste(order, collapse = ","), "): ", e$message)
      })
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_arima_acc_log), "\n")
if (length(results_gs_arima_acc_log) > 0) {
  results_df_gs_arima_acc_log <- do.call(rbind, lapply(results_gs_arima_acc_log, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_arima_acc_log <- as.data.frame(results_df_gs_arima_acc_log, stringsAsFactors = FALSE)
  results_df_gs_arima_acc_log$MAPE  <- as.numeric(results_df_gs_arima_acc_log$MAPE)
  results_df_gs_arima_acc_log$AIC   <- as.numeric(results_df_gs_arima_acc_log$AIC)
  results_df_gs_arima_acc_log$BIC   <- as.numeric(results_df_gs_arima_acc_log$BIC)
  results_df_gs_arima_acc_log$LB_p  <- as.numeric(results_df_gs_arima_acc_log$LB_p)
  results_df_gs_arima_acc_log$BP_p  <- as.numeric(results_df_gs_arima_acc_log$BP_p)
  results_df_sorted_gs_arima_acc_log <- results_df_gs_arima_acc_log[order(results_df_gs_arima_acc_log$MAPE), ]
  rownames(results_df_sorted_gs_arima_acc_log) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_arima_acc_log))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##LOG FATALITIES##


#Prepare the time series#

fat_w_ts_gs_log <- log1p(ts(acc.w.ind.y_4wmod$fatalities, start = c(1996, 1), frequency = 48))
train_ts_arima_fat_w_gs_log <- window(fat_w_ts_gs_log, end = c(2017, 29))
test_ts_arima_fat_w_gs_log  <- window(fat_w_ts_gs_log, start = c(2017, 30))
p_gs_fat_log <- 0:3; d_gs_fat_log <- 0:3; q_gs_fat_log <- 0:3
results_gs_arima_fat_log <- list()
tried_models_gs_arima_fat_log <- 0
total_combinations_gs_arima_fat_log <- length(p_gs_fat_log) * length(d_gs_fat_log) * length(q_gs_fat_log)
cat("🔄 Total combinations to try:", total_combinations_gs_arima_fat_log, "\n")
for (i in p_gs_fat_log) {
  for (j in d_gs_fat_log) {
    for (k in q_gs_fat_log) {
      tried_models_gs_arima_fat_log <- tried_models_gs_arima_fat_log + 1
      order <- c(i, j, k)
      cat("\nTrying model", tried_models_gs_arima_fat_log, "of", total_combinations_gs_arima_fat_log,
          "-> ARIMA(", paste(order, collapse = ","), ")\n")
      tryCatch({
        fit <- Arima(train_ts_arima_fat_w_gs_log,
                     order = order,
                     method = "ML")
        h <- length(test_ts_arima_fat_w_gs_log)
        fc <- forecast(fit, h = h)
        fc_original <- expm1(fc$mean)
        test_original <- expm1(test_ts_arima_fat_w_gs_log)
        mape <- accuracy(fc_original, test_original)["Test set", "MAPE"]
        lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = length(fit$coef))
        bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
        if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
          cat("✅ Model passed residual tests. Saving...\n")
          results_gs_arima_fat_log <- append(results_gs_arima_fat_log, list(list(
            order = paste0("(", paste(order, collapse = ","), ")"),
            AIC = AIC(fit),
            BIC = BIC(fit),
            MAPE = mape,
            LB_p = lb_test$p.value,
            BP_p = bp_test$p.value
          )))
        } else {
          cat("❌ Residuals failed white noise tests.\n")
        }
      }, error = function(e) {
        message("⚠️ Error for ARIMA(", paste(order, collapse = ","), "): ", e$message)
      })
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_arima_fat_log), "\n")
if (length(results_gs_arima_fat_log) > 0) {
  results_df_gs_arima_fat_log <- do.call(rbind, lapply(results_gs_arima_fat_log, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_arima_fat_log <- as.data.frame(results_df_gs_arima_fat_log, stringsAsFactors = FALSE)
  results_df_gs_arima_fat_log$MAPE  <- as.numeric(results_df_gs_arima_fat_log$MAPE)
  results_df_gs_arima_fat_log$AIC   <- as.numeric(results_df_gs_arima_fat_log$AIC)
  results_df_gs_arima_fat_log$BIC   <- as.numeric(results_df_gs_arima_fat_log$BIC)
  results_df_gs_arima_fat_log$LB_p  <- as.numeric(results_df_gs_arima_fat_log$LB_p)
  results_df_gs_arima_fat_log$BP_p  <- as.numeric(results_df_gs_arima_fat_log$BP_p)
  results_df_sorted_gs_arima_fat_log <- results_df_gs_arima_fat_log[order(results_df_gs_arima_fat_log$MAPE), ]
  rownames(results_df_sorted_gs_arima_fat_log) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_arima_fat_log))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

###SARIMA###


##REGULAR ACCIDENTS##


#Prepare time series#

p_gs_acc <- 0:3; d_gs_acc <- 0:3; q_gs_acc <- 0:3
P_gs_acc <- 0:3; D_gs_acc <- 0:3; Q_gs_acc <- 0:3
s_gs_acc <- 48  
results_gs_sarima_acc <- list()
tried_models_gs_sarima_acc <- 0
total_combinations_gs_sarima_acc <- length(p_gs_acc) * length(d_gs_acc) * length(q_gs_acc) * length(P_gs_acc) * length(D_gs_acc) * length(Q_gs_acc)
cat("🔄 Total combinations to try:", total_combinations_gs_sarima_acc, "\n")
for (i in p_gs_acc) {
  for (j in d_gs_acc) {
    for (k1 in q_gs_acc) {
      for (i2 in P_gs_acc) {
        for (j2 in D_gs_acc) {
          for (k2 in Q_gs_acc) {
            tried_models_gs_sarima_acc <- tried_models_gs_sarima_acc + 1
            order <- c(i, j, k1)
            seasonal <- c(i2, j2, k2)
            cat("\nTrying model", tried_models_gs_sarima_acc, "of", total_combinations_gs_sarima_acc,
                "-> ARIMA(", paste(order, collapse = ","), ") x (", 
                paste(seasonal, collapse = ","), ")[", s_gs_acc, "]\n")
            tryCatch({
              fit <- Arima(train_ts_arima_acc_w_gs,
                           order = order,
                           seasonal = list(order = seasonal, period = s_gs_acc),
                           method = "ML")
              h <- length(test_ts_arima_acc_w_gs)
              fc <- forecast(fit, h = h)
              mape <- accuracy(fc, test_ts_arima_acc_w_gs)["Test set", "MAPE"]
              lb_test <- Box.test(residuals(fit), lag = 12, type = "Ljung-Box", fitdf = length(fit$coef))
              bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
              if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
                cat("✅ Model passed residual tests. Saving...\n")
                results_gs_sarima_acc <- append(results_gs_sarima_acc, list(list(
                  order = paste0("(", paste(order, collapse = ","), ")"),
                  seasonal = paste0("(", paste(seasonal, collapse = ","), ")[", s_gs_acc, "]"),
                  AIC = AIC(fit),
                  BIC = BIC(fit),
                  MAPE = mape,
                  LB_p = lb_test$p.value,
                  BP_p = bp_test$p.value
                )))
              } else {
                cat("❌ Residuals failed white noise tests.\n")
              }
            }, error = function(e) {
              message("⚠️ Error for ARIMA(", paste(order, collapse = ","), ") x (", 
                      paste(seasonal, collapse = ","), ")[", s_gs_acc, "]: ", e$message)
            })
          }
        }
      }
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_sarima_acc), "\n")
if (length(results_gs_sarima_acc) > 0) {
  results_df_gs_sarima_acc <- do.call(rbind, lapply(results_gs_sarima_acc, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_sarima_acc <- as.data.frame(results_df_gs_sarima_acc, stringsAsFactors = FALSE)
  results_df_gs_sarima_acc$MAPE  <- as.numeric(results_df_gs_sarima_acc$MAPE)
  results_df_gs_sarima_acc$AIC   <- as.numeric(results_df_gs_sarima_acc$AIC)
  results_df_gs_sarima_acc$BIC   <- as.numeric(results_df_gs_sarima_acc$BIC)
  results_df_gs_sarima_acc$LB_p  <- as.numeric(results_df_gs_sarima_acc$LB_p)
  results_df_gs_sarima_acc$BP_p  <- as.numeric(results_df_gs_sarima_acc$BP_p)
  results_df_sorted_gs_sarima_acc <- results_df[order(results_df_gs_sarima_acc$MAPE), ]
  rownames(results_df_sorted_gs_sarima_acc) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_sarima_acc))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##REGULAR FATALITIES##


#Prepare time series#

p_gs_fat <- 0:3; d_gs_fat <- 0:3; q_gs_fat <- 0:3
P_gs_fat <- 0:3; D_gs_fat <- 0:3; Q_gs_fat <- 0:3
s_gs_fat <- 48  
results_gs_sarima_fat <- list()
tried_models_gs_sarima_fat <- 0
total_combinations_gs_sarima_fat <- length(p_gs_fat) * length(d_gs_fat) * length(q_gs_fat) * length(P_gs_fat) * length(D_gs_fat) * length(Q_gs_fat)
cat("🔄 Total combinations to try:", total_combinations_gs_sarima_fat, "\n")
for (i in p_gs_fat) {
  for (j in d_gs_fat) {
    for (k1 in q_gs_fat) {
      for (i2 in P_gs_fat) {
        for (j2 in D_gs_fat) {
          for (k2 in Q_gs_fat) {
            tried_models_gs_sarima_fat <- tried_models_gs_sarima_fat + 1
            order <- c(i, j, k1)
            seasonal <- c(i2, j2, k2)
            cat("\nTrying model", tried_models_gs_sarima_fat, "of", total_combinations_gs_sarima_fat,
                "-> ARIMA(", paste(order, collapse = ","), ") x (", 
                paste(seasonal, collapse = ","), ")[", s_gs_fat, "]\n")
            tryCatch({
              fit <- Arima(train_ts_arima_fat_w_gs,
                           order = order,
                           seasonal = list(order = seasonal, period = s_gs_fat),
                           method = "ML")
              h <- length(test_ts_arima_fat_w_gs)
              fc <- forecast(fit, h = h)
              mape <- accuracy(fc, test_ts_arima_fat_w_gs)["Test set", "MAPE"]
              lb_test <- Box.test(residuals(fit), lag = 12, type = "Ljung-Box", fitdf = length(fit$coef))
              bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
              if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
                cat("✅ Model passed residual tests. Saving...\n")
                results_gs_sarima_fat <- append(results_gs_sarima_fat, list(list(
                  order = paste0("(", paste(order, collapse = ","), ")"),
                  seasonal = paste0("(", paste(seasonal, collapse = ","), ")[", s_gs_fat, "]"),
                  AIC = AIC(fit),
                  BIC = BIC(fit),
                  MAPE = mape,
                  LB_p = lb_test$p.value,
                  BP_p = bp_test$p.value
                )))
              } else {
                cat("❌ Residuals failed white noise tests.\n")
              }
            }, error = function(e) {
              message("⚠️ Error for ARIMA(", paste(order, collapse = ","), ") x (", 
                      paste(seasonal, collapse = ","), ")[", s_gs_fat, "]: ", e$message)
            })
          }
        }
      }
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_sarima_fat), "\n")
if (length(results_gs_sarima_fat) > 0) {
  results_df_gs_sarima_fat <- do.call(rbind, lapply(results_gs_sarima_fat, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_sarima_fat <- as.data.frame(results_df_gs_sarima_fat, stringsAsFactors = FALSE)
  results_df_gs_sarima_fat$MAPE  <- as.numeric(results_df_gs_sarima_fat$MAPE)
  results_df_gs_sarima_fat$AIC   <- as.numeric(results_df_gs_sarima_fat$AIC)
  results_df_gs_sarima_fat$BIC   <- as.numeric(results_df_gs_sarima_fat$BIC)
  results_df_gs_sarima_fat$LB_p  <- as.numeric(results_df_gs_sarima_fat$LB_p)
  results_df_gs_sarima_fat$BP_p  <- as.numeric(results_df_gs_sarima_fat$BP_p)
  results_df_sorted_gs_sarima_fat <- results_df[order(results_df_gs_sarima_fat$MAPE), ]
  rownames(results_df_sorted_gs_sarima_fat) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_sarima_fat))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##LOG ACCIDENTS##


#Prepare time series#

train_ts_sarima_acc_w_gs_log <- window(acc_w_ts_gs_log, end = c(2017, 29))
test_ts_sarima_acc_w_gs_log  <- window(acc_w_ts_gs_log, start = c(2017, 30))
p_gs_acc_log <- 0:3; d_gs_acc_log <- 0:3; q_gs_acc_log <- 0:3
P_gs_acc_log <- 0:3; D_gs_acc_log <- 0:3; Q_gs_acc_log <- 0:3
s_gs_acc_log <- 48  
results_gs_sarima_acc_log <- list()
tried_models_gs_sarima_acc_log <- 0
total_combinations_gs_sarima_acc_log <- length(p_gs_acc_log) * length(d_gs_acc_log) * length(q_gs_acc_log) * length(P_gs_acc_log) * length(D_gs_acc_log) * length(Q_gs_acc_log)
cat("🔄 Total combinations to try:", total_combinations_gs_sarima_acc_log, "\n")
for (i in p_gs_acc_log) {
  for (j in d_gs_acc_log) {
    for (k1 in q_gs_acc_log) {
      for (i2 in P_gs_acc_log) {
        for (j2 in D_gs_acc_log) {
          for (k2 in Q_gs_acc_log) {
            tried_models_gs_sarima_acc_log <- tried_models_gs_sarima_acc_log + 1
            order <- c(i, j, k1)
            seasonal <- c(i2, j2, k2)
            cat("\nTrying model", tried_models_gs_sarima_acc_log, "of", total_combinations_gs_sarima_acc_log,
                "-> ARIMA(", paste(order, collapse = ","), ") x (", 
                paste(seasonal, collapse = ","), ")[", s_gs_acc_log, "]\n")
            tryCatch({
              fit <- Arima(train_ts_sarima_acc_w_gs_log,
                           order = order,
                           seasonal = list(order = seasonal, period = s_gs_acc_log),
                           method = "ML")
              h <- length(test_ts_sarima_acc_w_gs_log)
              fc <- forecast(fit, h = h)
              fc_original <- expm1(fc$mean)
              test_original <- expm1(test_ts_sarima_acc_w_gs_log)
              mape <- accuracy(fc_original, test_original)["Test set", "MAPE"]
              lb_test <- Box.test(residuals(fit), lag = 12, type = "Ljung-Box", fitdf = length(fit$coef))
              bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
              if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
                cat("✅ Model passed residual tests. Saving...\n")
                results_gs_sarima_acc_log <- append(results_gs_sarima_acc_log, list(list(
                  order = paste0("(", paste(order, collapse = ","), ")"),
                  seasonal = paste0("(", paste(seasonal, collapse = ","), ")[", s_gs_acc_log, "]"),
                  AIC = AIC(fit),
                  BIC = BIC(fit),
                  MAPE = mape,
                  LB_p = lb_test$p.value,
                  BP_p = bp_test$p.value
                )))
              } else {
                cat("❌ Residuals failed white noise tests.\n")
              }
            }, error = function(e) {
              message("⚠️ Error for ARIMA(", paste(order, collapse = ","), ") x (", 
                      paste(seasonal, collapse = ","), ")[", s_gs_acc_log, "]: ", e$message)
            })
          }
        }
      }
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_sarima_acc_log), "\n")
if (length(results_gs_sarima_acc_log) > 0) {
  results_df_gs_sarima_acc_log <- do.call(rbind, lapply(results_gs_sarima_acc_log, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_sarima_acc_log <- as.data.frame(results_df_gs_sarima_acc_log, stringsAsFactors = FALSE)
  results_df_gs_sarima_acc_log$MAPE  <- as.numeric(results_df_gs_sarima_acc_log$MAPE)
  results_df_gs_sarima_acc_log$AIC   <- as.numeric(results_df_gs_sarima_acc_log$AIC)
  results_df_gs_sarima_acc_log$BIC   <- as.numeric(results_df_gs_sarima_acc_log$BIC)
  results_df_gs_sarima_acc_log$LB_p  <- as.numeric(results_df_gs_sarima_acc_log$LB_p)
  results_df_gs_sarima_acc_log$BP_p  <- as.numeric(results_df_gs_sarima_acc_log$BP_p)
  results_df_sorted_gs_sarima_acc_log <- results_df[order(results_df_gs_sarima_acc_log$MAPE), ]
  rownames(results_df_sorted_gs_sarima_acc_log) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_sarima_acc_log))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##LOG FATALITIES##


#Prepare time series#

train_ts_sarima_fat_w_gs_log <- window(fat_w_ts_gs_log, end = c(2017, 29))
test_ts_sarima_fat_w_gs_log  <- window(fat_w_ts_gs_log, start = c(2017, 30))
p_gs_fat_log <- 0:3; d_gs_fat_log <- 0:3; q_gs_fat_log <- 0:3
P_gs_fat_log <- 0:3; D_gs_fat_log <- 0:3; Q_gs_fat_log <- 0:3
s_gs_fat_log <- 48  
results_gs_sarima_fat_log <- list()
tried_models_gs_sarima_fat_log <- 0
total_combinations_gs_sarima_fat_log <- length(p_gs_fat_log) * length(d_gs_fat_log) * length(q_gs_fat_log) * length(P_gs_fat_log) * length(D_gs_fat_log) * length(Q_gs_fat_log)
cat("🔄 Total combinations to try:", total_combinations_gs_sarima_fat_log, "\n")
for (i in p_gs_fat_log) {
  for (j in d_gs_fat_log) {
    for (k1 in q_gs_fat_log) {
      for (i2 in P_gs_fat_log) {
        for (j2 in D_gs_fat_log) {
          for (k2 in Q_gs_fat_log) {
            tried_models_gs_sarima_fat_log <- tried_models_gs_sarima_fat_log + 1
            order <- c(i, j, k1)
            seasonal <- c(i2, j2, k2)
            cat("\nTrying model", tried_models_gs_sarima_fat_log, "of", total_combinations_gs_sarima_fat_log,
                "-> ARIMA(", paste(order, collapse = ","), ") x (", 
                paste(seasonal, collapse = ","), ")[", s_gs_fat_log, "]\n")
            tryCatch({
              fit <- Arima(train_ts_sarima_fat_w_gs_log,
                           order = order,
                           seasonal = list(order = seasonal, period = s_gs_fat_log),
                           method = "ML")
              h <- length(test_ts_sarima_fat_w_gs_log)
              fc <- forecast(fit, h = h)
              fc_original <- expm1(fc$mean)
              test_original <- expm1(test_ts_sarima_fat_w_gs_log)
              mape <- accuracy(fc_original, test_original)["Test set", "MAPE"]
              lb_test <- Box.test(residuals(fit), lag = 12, type = "Ljung-Box", fitdf = length(fit$coef))
              bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
              if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
                cat("✅ Model passed residual tests. Saving...\n")
                results_gs_sarima_fat_log <- append(results_gs_sarima_fat_log, list(list(
                  order = paste0("(", paste(order, collapse = ","), ")"),
                  seasonal = paste0("(", paste(seasonal, collapse = ","), ")[", s_gs_fat_log, "]"),
                  AIC = AIC(fit),
                  BIC = BIC(fit),
                  MAPE = mape,
                  LB_p = lb_test$p.value,
                  BP_p = bp_test$p.value
                )))
              } else {
                cat("❌ Residuals failed white noise tests.\n")
              }
            }, error = function(e) {
              message("⚠️ Error for ARIMA(", paste(order, collapse = ","), ") x (", 
                      paste(seasonal, collapse = ","), ")[", s_gs_fat_log, "]: ", e$message)
            })
          }
        }
      }
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_sarima_fat_log), "\n")
if (length(results_gs_sarima_fat_log) > 0) {
  results_df_gs_sarima_fat_log <- do.call(rbind, lapply(results_gs_sarima_fat_log, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_sarima_fat_log <- as.data.frame(results_df_gs_sarima_fat_log, stringsAsFactors = FALSE)
  results_df_gs_sarima_fat_log$MAPE  <- as.numeric(results_df_gs_sarima_fat_log$MAPE)
  results_df_gs_sarima_fat_log$AIC   <- as.numeric(results_df_gs_sarima_fat_log$AIC)
  results_df_gs_sarima_fat_log$BIC   <- as.numeric(results_df_gs_sarima_fat_log$BIC)
  results_df_gs_sarima_fat_log$LB_p  <- as.numeric(results_df_gs_sarima_fat_log$LB_p)
  results_df_gs_sarima_fat_log$BP_p  <- as.numeric(results_df_gs_sarima_fat_log$BP_p)
  results_df_sorted_gs_sarima_fat_log <- results_df[order(results_df_gs_sarima_fat_log$MAPE), ]
  rownames(results_df_sorted_gs_sarima_fat_log) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_sarima_fat_log))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

###MONTHLY###



###ARIMA###



##REGULAR ACCIDENTS##


#Prepare time series#

acc_m_ts_gs <- ts(acc.m.ind.y_mod$accidents, start = c(1996, 1), frequency = 12)
train_ts_arima_acc_m_gs <- window(acc_m_ts_gs, end = c(2017, 7))
test_ts_arima_acc_m_gs  <- window(acc_m_ts_gs, start = c(2017, 8))
p_gs_acc <- 0:3; d_gs_acc <- 0:3; q_gs_acc <- 0:3
results_gs_arima_acc <- list()
tried_models_gs_arima_acc <- 0
total_combinations_gs_arima_acc <- length(p_gs_acc) * length(d_gs_acc) * length(q_gs_acc)
cat("🔄 Total combinations to try:", total_combinations_gs_arima_acc, "\n")
for (i in p_gs_acc) {
  for (j in d_gs_acc) {
    for (k in q_gs_acc) {
      tried_models_gs_arima_acc <- tried_models_gs_arima_acc + 1
      order <- c(i, j, k)
      
      cat("\nTrying model", tried_models_gs_arima_acc, "of", total_combinations_gs_arima_acc,
          "-> ARIMA(", paste(order, collapse = ","), ")\n")
      
      tryCatch({
        fit <- Arima(train_ts_arima_acc_m_gs,
                     order = order,
                     method = "ML")
        
        h <- length(test_ts_arima_acc_m_gs)
        fc <- forecast(fit, h = h)
        mape <- accuracy(fc, test_ts_arima_acc_m_gs)["Test set", "MAPE"]
        lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = length(fit$coef))
        bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
        if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
          cat("✅ Model passed residual tests. Saving...\n")
          results_gs_arima_acc <- append(results_gs_arima_acc, list(list(
            order = paste0("(", paste(order, collapse = ","), ")"),
            AIC = AIC(fit),
            BIC = BIC(fit),
            MAPE = mape,
            LB_p = lb_test$p.value,
            BP_p = bp_test$p.value
          )))
        } else {
          cat("❌ Residuals failed white noise tests.\n")
        }
      }, error = function(e) {
        message("⚠️ Error for ARIMA(", paste(order, collapse = ","), "): ", e$message)
      })
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_arima_acc), "\n")
if (length(results_gs_arima_acc) > 0) {
  results_df_gs_arima_acc <- do.call(rbind, lapply(results_gs_arima_acc, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_arima_acc <- as.data.frame(results_df_gs_arima_acc, stringsAsFactors = FALSE)
  results_df_gs_arima_acc$MAPE  <- as.numeric(results_df_gs_arima_acc$MAPE)
  results_df_gs_arima_acc$AIC   <- as.numeric(results_df_gs_arima_acc$AIC)
  results_df_gs_arima_acc$BIC   <- as.numeric(results_df_gs_arima_acc$BIC)
  results_df_gs_arima_acc$LB_p  <- as.numeric(results_df_gs_arima_acc$LB_p)
  results_df_gs_arima_acc$BP_p  <- as.numeric(results_df_gs_arima_acc$BP_p)
  results_df_sorted_gs_arima_acc <- results_df_gs_arima_acc[order(results_df_gs_arima_acc$MAPE), ]
  rownames(results_df_sorted_gs_arima_acc) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_arima_acc))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##REGULAR FATALITIES##


#Prepare the time series#

fat_m_ts_gs <- ts(acc.m.ind.y_mod$fatalities, start = c(1996, 1), frequency = 12)
train_ts_arima_fat_m_gs <- window(fat_m_ts_gs, end = c(2017, 7))
test_ts_arima_fat_m_gs  <- window(fat_m_ts_gs, start = c(2017, 8))
p_gs_fat <- 0:3; d_gs_fat <- 0:3; q_gs_fat <- 0:3
results_gs_arima_fat <- list()
tried_models_gs_arima_fat <- 0
total_combinations_gs_arima_fat <- length(p_gs_fat) * length(d_gs_fat) * length(q_gs_fat)
cat("🔄 Total combinations to try:", total_combinations_gs_arima_fat, "\n")
for (i in p_gs_fat) {
  for (j in d_gs_fat) {
    for (k in q_gs_fat) {
      tried_models_gs_arima_fat <- tried_models_gs_arima_fat + 1
      order <- c(i, j, k)
      cat("\nTrying model", tried_models_gs_arima_fat, "of", total_combinations_gs_arima_fat,
          "-> ARIMA(", paste(order, collapse = ","), ")\n")
      tryCatch({
        fit <- Arima(train_ts_arima_fat_m_gs,
                     order = order,
                     method = "ML")
        h <- length(test_ts_arima_fat_m_gs)
        fc <- forecast(fit, h = h)
        mape <- accuracy(fc, test_ts_arima_fat_m_gs)["Test set", "MAPE"]
        
        lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = length(fit$coef))
        bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
        if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
          cat("✅ Model passed residual tests. Saving...\n")
          results_gs_arima_fat <- append(results_gs_arima_fat, list(list(
            order = paste0("(", paste(order, collapse = ","), ")"),
            AIC = AIC(fit),
            BIC = BIC(fit),
            MAPE = mape,
            LB_p = lb_test$p.value,
            BP_p = bp_test$p.value
          )))
        } else {
          cat("❌ Residuals failed white noise tests.\n")
        }
      }, error = function(e) {
        message("⚠️ Error for ARIMA(", paste(order, collapse = ","), "): ", e$message)
      })
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_arima_fat), "\n")

if (length(results_gs_arima_fat) > 0) {
  results_df_gs_arima_fat <- do.call(rbind, lapply(results_gs_arima_fat, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_arima_fat <- as.data.frame(results_df_gs_arima_fat, stringsAsFactors = FALSE)
  
  results_df_gs_arima_fat$MAPE  <- as.numeric(results_df_gs_arima_fat$MAPE)
  results_df_gs_arima_fat$AIC   <- as.numeric(results_df_gs_arima_fat$AIC)
  results_df_gs_arima_fat$BIC   <- as.numeric(results_df_gs_arima_fat$BIC)
  results_df_gs_arima_fat$LB_p  <- as.numeric(results_df_gs_arima_fat$LB_p)
  results_df_gs_arima_fat$BP_p  <- as.numeric(results_df_gs_arima_fat$BP_p)
  
  results_df_sorted_gs_arima_fat <- results_df_gs_arima_fat[order(results_df_gs_arima_fat$MAPE), ]
  rownames(results_df_sorted_gs_arima_fat) <- NULL
  
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_arima_fat))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##LOG ACCIDENTS##


#Prepare time series#

acc_m_ts_gs_log <- log1p(ts(acc.m.ind.y_mod$accidents, start = c(1996, 1), frequency = 12))
train_ts_arima_acc_m_gs_log <- window(acc_m_ts_gs_log, end = c(2017, 7))
test_ts_arima_acc_m_gs_log  <- window(acc_m_ts_gs_log, start = c(2017, 8))
p_gs_acc_log <- 0:3; d_gs_acc_log <- 0:3; q_gs_acc_log <- 0:3
results_gs_arima_acc_log <- list()
tried_models_gs_arima_acc_log <- 0
total_combinations_gs_arima_acc_log <- length(p_gs_acc_log) * length(d_gs_acc_log) * length(q_gs_acc_log)
cat("🔄 Total combinations to try:", total_combinations_gs_arima_acc_log, "\n")
for (i in p_gs_acc_log) {
  for (j in d_gs_acc_log) {
    for (k in q_gs_acc_log) {
      tried_models_gs_arima_acc_log <- tried_models_gs_arima_acc_log + 1
      order <- c(i, j, k)
      cat("\nTrying model", tried_models_gs_arima_acc_log, "of", total_combinations_gs_arima_acc_log,
          "-> ARIMA(", paste(order, collapse = ","), ")\n")
      tryCatch({
        fit <- Arima(train_ts_arima_acc_m_gs_log,
                     order = order,
                     method = "ML")
        h <- length(test_ts_arima_acc_m_gs_log)
        fc <- forecast(fit, h = h)
        fc_original <- expm1(fc$mean)
        test_original <- expm1(test_ts_arima_acc_m_gs_log)
        mape <- accuracy(fc_original, test_original)["Test set", "MAPE"]
        lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = length(fit$coef))
        bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
        if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
          cat("✅ Model passed residual tests. Saving...\n")
          results_gs_arima_acc_log <- append(results_gs_arima_acc_log, list(list(
            order = paste0("(", paste(order, collapse = ","), ")"),
            AIC = AIC(fit),
            BIC = BIC(fit),
            MAPE = mape,
            LB_p = lb_test$p.value,
            BP_p = bp_test$p.value
          )))
        } else {
          cat("❌ Residuals failed white noise tests.\n")
        }
      }, error = function(e) {
        message("⚠️ Error for ARIMA(", paste(order, collapse = ","), "): ", e$message)
      })
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_arima_acc_log), "\n")
if (length(results_gs_arima_acc_log) > 0) {
  results_df_gs_arima_acc_log <- do.call(rbind, lapply(results_gs_arima_acc_log, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_arima_acc_log <- as.data.frame(results_df_gs_arima_acc_log, stringsAsFactors = FALSE)
  results_df_gs_arima_acc_log$MAPE  <- as.numeric(results_df_gs_arima_acc_log$MAPE)
  results_df_gs_arima_acc_log$AIC   <- as.numeric(results_df_gs_arima_acc_log$AIC)
  results_df_gs_arima_acc_log$BIC   <- as.numeric(results_df_gs_arima_acc_log$BIC)
  results_df_gs_arima_acc_log$LB_p  <- as.numeric(results_df_gs_arima_acc_log$LB_p)
  results_df_gs_arima_acc_log$BP_p  <- as.numeric(results_df_gs_arima_acc_log$BP_p)
  results_df_sorted_gs_arima_acc_log <- results_df_gs_arima_acc_log[order(results_df_gs_arima_acc_log$MAPE), ]
  rownames(results_df_sorted_gs_arima_acc_log) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_arima_acc_log))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##LOG FATALITIES##


#Prepare the time series#

fat_m_ts_gs_log <- log1p(ts(acc.m.ind.y_mod$fatalities, start = c(1996, 1), frequency = 12))
train_ts_arima_fat_m_gs_log <- window(fat_m_ts_gs_log, end = c(2017, 7))
test_ts_arima_fat_m_gs_log  <- window(fat_m_ts_gs_log, start = c(2017, 8))
p_gs_fat_log <- 0:3; d_gs_fat_log <- 0:3; q_gs_fat_log <- 0:3
results_gs_arima_fat_log <- list()
tried_models_gs_arima_fat_log <- 0
total_combinations_gs_arima_fat_log <- length(p_gs_fat_log) * length(d_gs_fat_log) * length(q_gs_fat_log)
cat("🔄 Total combinations to try:", total_combinations_gs_arima_fat_log, "\n")
for (i in p_gs_fat_log) {
  for (j in d_gs_fat_log) {
    for (k in q_gs_fat_log) {
      tried_models_gs_arima_fat_log <- tried_models_gs_arima_fat_log + 1
      order <- c(i, j, k)
      cat("\nTrying model", tried_models_gs_arima_fat_log, "of", total_combinations_gs_arima_fat_log,
          "-> ARIMA(", paste(order, collapse = ","), ")\n")
      tryCatch({
        fit <- Arima(train_ts_arima_fat_m_gs_log,
                     order = order,
                     method = "ML")
        h <- length(test_ts_arima_fat_m_gs_log)
        fc <- forecast(fit, h = h)
        fc_original <- expm1(fc$mean)
        test_original <- expm1(test_ts_arima_fat_m_gs_log)
        mape <- accuracy(fc_original, test_original)["Test set", "MAPE"]
        lb_test <- Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = length(fit$coef))
        bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
        if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
          cat("✅ Model passed residual tests. Saving...\n")
          results_gs_arima_fat_log <- append(results_gs_arima_fat_log, list(list(
            order = paste0("(", paste(order, collapse = ","), ")"),
            AIC = AIC(fit),
            BIC = BIC(fit),
            MAPE = mape,
            LB_p = lb_test$p.value,
            BP_p = bp_test$p.value
          )))
        } else {
          cat("❌ Residuals failed white noise tests.\n")
        }
      }, error = function(e) {
        message("⚠️ Error for ARIMA(", paste(order, collapse = ","), "): ", e$message)
      })
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_arima_fat_log), "\n")
if (length(results_gs_arima_fat_log) > 0) {
  results_df_gs_arima_fat_log <- do.call(rbind, lapply(results_gs_arima_fat_log, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_arima_fat_log <- as.data.frame(results_df_gs_arima_fat_log, stringsAsFactors = FALSE)
  results_df_gs_arima_fat_log$MAPE  <- as.numeric(results_df_gs_arima_fat_log$MAPE)
  results_df_gs_arima_fat_log$AIC   <- as.numeric(results_df_gs_arima_fat_log$AIC)
  results_df_gs_arima_fat_log$BIC   <- as.numeric(results_df_gs_arima_fat_log$BIC)
  results_df_gs_arima_fat_log$LB_p  <- as.numeric(results_df_gs_arima_fat_log$LB_p)
  results_df_gs_arima_fat_log$BP_p  <- as.numeric(results_df_gs_arima_fat_log$BP_p)
  results_df_sorted_gs_arima_fat_log <- results_df_gs_arima_fat_log[order(results_df_gs_arima_fat_log$MAPE), ]
  rownames(results_df_sorted_gs_arima_fat_log) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_arima_fat_log))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

###SARIMA###


##REGULAR ACCIDENTS##


#Prepare time series#

p_gs_acc <- 0:3; d_gs_acc <- 0:3; q_gs_acc <- 0:3
P_gs_acc <- 0:3; D_gs_acc <- 0:3; Q_gs_acc <- 0:3
s_gs_acc <- 12  
results_gs_sarima_acc <- list()
tried_models_gs_sarima_acc <- 0
total_combinations_gs_sarima_acc <- length(p_gs_acc) * length(d_gs_acc) * length(q_gs_acc) * length(P_gs_acc) * length(D_gs_acc) * length(Q_gs_acc)
cat("🔄 Total combinations to try:", total_combinations_gs_sarima_acc, "\n")
for (i in p_gs_acc) {
  for (j in d_gs_acc) {
    for (k1 in q_gs_acc) {
      for (i2 in P_gs_acc) {
        for (j2 in D_gs_acc) {
          for (k2 in Q_gs_acc) {
            tried_models_gs_sarima_acc <- tried_models_gs_sarima_acc + 1
            order <- c(i, j, k1)
            seasonal <- c(i2, j2, k2)
            cat("\nTrying model", tried_models_gs_sarima_acc, "of", total_combinations_gs_sarima_acc,
                "-> ARIMA(", paste(order, collapse = ","), ") x (", 
                paste(seasonal, collapse = ","), ")[", s_gs_acc, "]\n")
            tryCatch({
              fit <- Arima(train_ts_arima_acc_m_gs,
                           order = order,
                           seasonal = list(order = seasonal, period = s_gs_acc),
                           method = "ML")
              h <- length(test_ts_arima_acc_m_gs)
              fc <- forecast(fit, h = h)
              mape <- accuracy(fc, test_ts_arima_acc_m_gs)["Test set", "MAPE"]
              lb_test <- Box.test(residuals(fit), lag = 12, type = "Ljung-Box", fitdf = length(fit$coef))
              bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
              if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
                cat("✅ Model passed residual tests. Saving...\n")
                results_gs_sarima_acc <- append(results_gs_sarima_acc, list(list(
                  order = paste0("(", paste(order, collapse = ","), ")"),
                  seasonal = paste0("(", paste(seasonal, collapse = ","), ")[", s_gs_acc, "]"),
                  AIC = AIC(fit),
                  BIC = BIC(fit),
                  MAPE = mape,
                  LB_p = lb_test$p.value,
                  BP_p = bp_test$p.value
                )))
              } else {
                cat("❌ Residuals failed white noise tests.\n")
              }
            }, error = function(e) {
              message("⚠️ Error for ARIMA(", paste(order, collapse = ","), ") x (", 
                      paste(seasonal, collapse = ","), ")[", s_gs_acc, "]: ", e$message)
            })
          }
        }
      }
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_sarima_acc), "\n")
if (length(results_gs_sarima_acc) > 0) {
  results_df_gs_sarima_acc <- do.call(rbind, lapply(results_gs_sarima_acc, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_sarima_acc <- as.data.frame(results_df_gs_sarima_acc, stringsAsFactors = FALSE)
  results_df_gs_sarima_acc$MAPE  <- as.numeric(results_df_gs_sarima_acc$MAPE)
  results_df_gs_sarima_acc$AIC   <- as.numeric(results_df_gs_sarima_acc$AIC)
  results_df_gs_sarima_acc$BIC   <- as.numeric(results_df_gs_sarima_acc$BIC)
  results_df_gs_sarima_acc$LB_p  <- as.numeric(results_df_gs_sarima_acc$LB_p)
  results_df_gs_sarima_acc$BP_p  <- as.numeric(results_df_gs_sarima_acc$BP_p)
  results_df_sorted_gs_sarima_acc <- results_df_gs_sarima_acc[order(results_df_gs_sarima_acc$MAPE), ]
  rownames(results_df_sorted_gs_sarima_acc) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_sarima_acc))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##REGULAR FATALITIES##


#Prepare time series#

p_gs_fat <- 0:3; d_gs_fat <- 0:3; q_gs_fat <- 0:3
P_gs_fat <- 0:3; D_gs_fat <- 0:3; Q_gs_fat <- 0:3
s_gs_fat <- 12  
results_gs_sarima_fat <- list()
tried_models_gs_sarima_fat <- 0
total_combinations_gs_sarima_fat <- length(p_gs_fat) * length(d_gs_fat) * length(q_gs_fat) * length(P_gs_fat) * length(D_gs_fat) * length(Q_gs_fat)
cat("🔄 Total combinations to try:", total_combinations_gs_sarima_fat, "\n")
for (i in p_gs_fat) {
  for (j in d_gs_fat) {
    for (k1 in q_gs_fat) {
      for (i2 in P_gs_fat) {
        for (j2 in D_gs_fat) {
          for (k2 in Q_gs_fat) {
            tried_models_gs_sarima_fat <- tried_models_gs_sarima_fat + 1
            order <- c(i, j, k1)
            seasonal <- c(i2, j2, k2)
            cat("\nTrying model", tried_models_gs_sarima_fat, "of", total_combinations_gs_sarima_fat,
                "-> ARIMA(", paste(order, collapse = ","), ") x (", 
                paste(seasonal, collapse = ","), ")[", s_gs_fat, "]\n")
            tryCatch({
              fit <- Arima(train_ts_arima_fat_m_gs,
                           order = order,
                           seasonal = list(order = seasonal, period = s_gs_fat),
                           method = "ML")
              h <- length(test_ts_arima_fat_m_gs)
              fc <- forecast(fit, h = h)
              mape <- accuracy(fc, test_ts_arima_fat_m_gs)["Test set", "MAPE"]
              lb_test <- Box.test(residuals(fit), lag = 12, type = "Ljung-Box", fitdf = length(fit$coef))
              bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
              if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
                cat("✅ Model passed residual tests. Saving...\n")
                results_gs_sarima_fat <- append(results_gs_sarima_fat, list(list(
                  order = paste0("(", paste(order, collapse = ","), ")"),
                  seasonal = paste0("(", paste(seasonal, collapse = ","), ")[", s_gs_fat, "]"),
                  AIC = AIC(fit),
                  BIC = BIC(fit),
                  MAPE = mape,
                  LB_p = lb_test$p.value,
                  BP_p = bp_test$p.value
                )))
              } else {
                cat("❌ Residuals failed white noise tests.\n")
              }
            }, error = function(e) {
              message("⚠️ Error for ARIMA(", paste(order, collapse = ","), ") x (", 
                      paste(seasonal, collapse = ","), ")[", s_gs_fat, "]: ", e$message)
            })
          }
        }
      }
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_sarima_fat), "\n")
if (length(results_gs_sarima_fat) > 0) {
  results_df_gs_sarima_fat <- do.call(rbind, lapply(results_gs_sarima_fat, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_sarima_fat <- as.data.frame(results_df_gs_sarima_fat, stringsAsFactors = FALSE)
  results_df_gs_sarima_fat$MAPE  <- as.numeric(results_df_gs_sarima_fat$MAPE)
  results_df_gs_sarima_fat$AIC   <- as.numeric(results_df_gs_sarima_fat$AIC)
  results_df_gs_sarima_fat$BIC   <- as.numeric(results_df_gs_sarima_fat$BIC)
  results_df_gs_sarima_fat$LB_p  <- as.numeric(results_df_gs_sarima_fat$LB_p)
  results_df_gs_sarima_fat$BP_p  <- as.numeric(results_df_gs_sarima_fat$BP_p)
  results_df_sorted_gs_sarima_fat <- results_df_gs_sarima_fat[order(results_df_gs_sarima_fat$MAPE), ]
  rownames(results_df_sorted_gs_sarima_fat) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_sarima_fat))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##LOG ACCIDENTS##


#Prepare time series#

train_ts_sarima_acc_m_gs_log <- window(acc_m_ts_gs_log, end = c(2017, 7))
test_ts_sarima_acc_m_gs_log  <- window(acc_m_ts_gs_log, start = c(2017, 8))
p_gs_acc_log <- 0:3; d_gs_acc_log <- 0:3; q_gs_acc_log <- 0:3
P_gs_acc_log <- 0:3; D_gs_acc_log <- 0:3; Q_gs_acc_log <- 0:3
s_gs_acc_log <- 12  
results_gs_sarima_acc_log <- list()
tried_models_gs_sarima_acc_log <- 0
total_combinations_gs_sarima_acc_log <- length(p_gs_acc_log) * length(d_gs_acc_log) * length(q_gs_acc_log) * length(P_gs_acc_log) * length(D_gs_acc_log) * length(Q_gs_acc_log)
cat("🔄 Total combinations to try:", total_combinations_gs_sarima_acc_log, "\n")
for (i in p_gs_acc_log) {
  for (j in d_gs_acc_log) {
    for (k1 in q_gs_acc_log) {
      for (i2 in P_gs_acc_log) {
        for (j2 in D_gs_acc_log) {
          for (k2 in Q_gs_acc_log) {
            tried_models_gs_sarima_acc_log <- tried_models_gs_sarima_acc_log + 1
            order <- c(i, j, k1)
            seasonal <- c(i2, j2, k2)
            cat("\nTrying model", tried_models_gs_sarima_acc_log, "of", total_combinations_gs_sarima_acc_log,
                "-> ARIMA(", paste(order, collapse = ","), ") x (", 
                paste(seasonal, collapse = ","), ")[", s_gs_acc_log, "]\n")
            tryCatch({
              fit <- Arima(train_ts_sarima_acc_m_gs_log,
                           order = order,
                           seasonal = list(order = seasonal, period = s_gs_acc_log),
                           method = "ML")
              h <- length(test_ts_sarima_acc_m_gs_log)
              fc <- forecast(fit, h = h)
              fc_original <- expm1(fc$mean)
              test_original <- expm1(test_ts_sarima_acc_m_gs_log)
              mape <- accuracy(fc_original, test_original)["Test set", "MAPE"]
              lb_test <- Box.test(residuals(fit), lag = 12, type = "Ljung-Box", fitdf = length(fit$coef))
              bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
              if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
                cat("✅ Model passed residual tests. Saving...\n")
                results_gs_sarima_acc_log <- append(results_gs_sarima_acc_log, list(list(
                  order = paste0("(", paste(order, collapse = ","), ")"),
                  seasonal = paste0("(", paste(seasonal, collapse = ","), ")[", s_gs_acc_log, "]"),
                  AIC = AIC(fit),
                  BIC = BIC(fit),
                  MAPE = mape,
                  LB_p = lb_test$p.value,
                  BP_p = bp_test$p.value
                )))
              } else {
                cat("❌ Residuals failed white noise tests.\n")
              }
            }, error = function(e) {
              message("⚠️ Error for ARIMA(", paste(order, collapse = ","), ") x (", 
                      paste(seasonal, collapse = ","), ")[", s_gs_acc_log, "]: ", e$message)
            })
          }
        }
      }
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_sarima_acc_log), "\n")
if (length(results_gs_sarima_acc_log) > 0) {
  results_df_gs_sarima_acc_log <- do.call(rbind, lapply(results_gs_sarima_acc_log, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_sarima_acc_log <- as.data.frame(results_df_gs_sarima_acc_log, stringsAsFactors = FALSE)
  results_df_gs_sarima_acc_log$MAPE  <- as.numeric(results_df_gs_sarima_acc_log$MAPE)
  results_df_gs_sarima_acc_log$AIC   <- as.numeric(results_df_gs_sarima_acc_log$AIC)
  results_df_gs_sarima_acc_log$BIC   <- as.numeric(results_df_gs_sarima_acc_log$BIC)
  results_df_gs_sarima_acc_log$LB_p  <- as.numeric(results_df_gs_sarima_acc_log$LB_p)
  results_df_gs_sarima_acc_log$BP_p  <- as.numeric(results_df_gs_sarima_acc_log$BP_p)
  results_df_sorted_gs_sarima_acc_log <- results_df_gs_sarima_acc_log[order(results_df_gs_sarima_acc_log$MAPE), ]
  rownames(results_df_sorted_gs_sarima_acc_log) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_sarima_acc_log))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

##LOG FATALITIES##


#Prepare time series#

train_ts_sarima_fat_m_gs_log <- window(fat_m_ts_gs_log, end = c(2017, 7))
test_ts_sarima_fat_m_gs_log  <- window(fat_m_ts_gs_log, start = c(2017, 8))
p_gs_fat_log <- 0:3; d_gs_fat_log <- 0:3; q_gs_fat_log <- 0:3
P_gs_fat_log <- 0:3; D_gs_fat_log <- 0:3; Q_gs_fat_log <- 0:3
s_gs_fat_log <- 12  
results_gs_sarima_fat_log <- list()
tried_models_gs_sarima_fat_log <- 0
total_combinations_gs_sarima_fat_log <- length(p_gs_fat_log) * length(d_gs_fat_log) * length(q_gs_fat_log) * length(P_gs_fat_log) * length(D_gs_fat_log) * length(Q_gs_fat_log)
cat("🔄 Total combinations to try:", total_combinations_gs_sarima_fat_log, "\n")
for (i in p_gs_fat_log) {
  for (j in d_gs_fat_log) {
    for (k1 in q_gs_fat_log) {
      for (i2 in P_gs_fat_log) {
        for (j2 in D_gs_fat_log) {
          for (k2 in Q_gs_fat_log) {
            tried_models_gs_sarima_fat_log <- tried_models_gs_sarima_fat_log + 1
            order <- c(i, j, k1)
            seasonal <- c(i2, j2, k2)
            cat("\nTrying model", tried_models_gs_sarima_fat_log, "of", total_combinations_gs_sarima_fat_log,
                "-> ARIMA(", paste(order, collapse = ","), ") x (", 
                paste(seasonal, collapse = ","), ")[", s_gs_fat_log, "]\n")
            tryCatch({
              fit <- Arima(train_ts_sarima_fat_m_gs_log,
                           order = order,
                           seasonal = list(order = seasonal, period = s_gs_fat_log),
                           method = "ML")
              h <- length(test_ts_sarima_fat_m_gs_log)
              fc <- forecast(fit, h = h)
              fc_original <- expm1(fc$mean)
              test_original <- expm1(test_ts_sarima_fat_m_gs_log)
              mape <- accuracy(fc_original, test_original)["Test set", "MAPE"]
              lb_test <- Box.test(residuals(fit), lag = 12, type = "Ljung-Box", fitdf = length(fit$coef))
              bp_test <- Box.test(residuals(fit), lag = 12, type = "Box-Pierce", fitdf = length(fit$coef))
              if (lb_test$p.value > 0.05 && bp_test$p.value > 0.05) {
                cat("✅ Model passed residual tests. Saving...\n")
                results_gs_sarima_fat_log <- append(results_gs_sarima_fat_log, list(list(
                  order = paste0("(", paste(order, collapse = ","), ")"),
                  seasonal = paste0("(", paste(seasonal, collapse = ","), ")[", s_gs_fat_log, "]"),
                  AIC = AIC(fit),
                  BIC = BIC(fit),
                  MAPE = mape,
                  LB_p = lb_test$p.value,
                  BP_p = bp_test$p.value
                )))
              } else {
                cat("❌ Residuals failed white noise tests.\n")
              }
            }, error = function(e) {
              message("⚠️ Error for ARIMA(", paste(order, collapse = ","), ") x (", 
                      paste(seasonal, collapse = ","), ")[", s_gs_fat_log, "]: ", e$message)
            })
          }
        }
      }
    }
  }
}

#Check results#

cat("\n✅ Models saved:", length(results_gs_sarima_fat_log), "\n")
if (length(results_gs_sarima_fat_log) > 0) {
  results_df_gs_sarima_fat_log <- do.call(rbind, lapply(results_gs_sarima_fat_log, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  results_df_gs_sarima_fat_log <- as.data.frame(results_df_gs_sarima_fat_log, stringsAsFactors = FALSE)
  results_df_gs_sarima_fat_log$MAPE  <- as.numeric(results_df_gs_sarima_fat_log$MAPE)
  results_df_gs_sarima_fat_log$AIC   <- as.numeric(results_df_gs_sarima_fat_log$AIC)
  results_df_gs_sarima_fat_log$BIC   <- as.numeric(results_df_gs_sarima_fat_log$BIC)
  results_df_gs_sarima_fat_log$LB_p  <- as.numeric(results_df_gs_sarima_fat_log$LB_p)
  results_df_gs_sarima_fat_log$BP_p  <- as.numeric(results_df_gs_sarima_fat_log$BP_p)
  results_df_sorted_gs_sarima_fat_log <- results_df_gs_sarima_fat_log[order(results_df_gs_sarima_fat_log$MAPE), ]
  rownames(results_df_sorted_gs_sarima_fat_log) <- NULL
  cat("\n🎉 Grid search complete! Top models by MAPE:\n")
  print(head(results_df_sorted_gs_sarima_fat_log))
} else {
  cat("⚠️ No valid models were saved. Check error messages above.\n")
}

####Final Selection of ARIMA and SARIMA Models for Accidents (Before GS)####




###MONTHLY###



###ARIMA###


##Time Series Preparation##


train_ts_acc_arima_monthly <- window(acc_m_ts, end = c(2017, 7))
test_ts_acc_arima_monthly  <- window(acc_m_ts, start = c(2017, 8))


##Fit ARIMA Model on Training Data##


model_acc_arima_monthly <- Arima(train_ts_acc_arima_monthly, order = c(3, 1, 2))
summary(model_acc_arima_monthly)


##Forecast on Test Set and Back-transform##


fc_test_acc_arima_monthly <- forecast(model_acc_arima_monthly, h = length(test_ts_acc_arima_monthly))


##Evaluate Forecast##


rmse_val_arima_acc_monthly <- rmse(test_ts_acc_arima_monthly, fc_test_acc_arima_monthly$mean)
mae_val_arima_acc_monthly  <- mae(test_ts_acc_arima_monthly, fc_test_acc_arima_monthly$mean)
mape_val_arima_acc_monthly <- mape(test_ts_acc_arima_monthly, fc_test_acc_arima_monthly$mean) * 100
cat("AIC:", AIC(model_acc_arima_monthly), "\n")
cat("BIC:", BIC(model_acc_arima_monthly), "\n")
cat("RMSE:", rmse_val_arima_acc_monthly, "\n")
cat("MAE:", mae_val_arima_acc_monthly, "\n")
cat("MAPE:", mape_val_arima_acc_monthly, "%\n")


##Fit Final Model on Full Dataset##


model_final_arima_acc_monthly <- Arima(acc_m_ts, order = c(3, 1, 2), include.drift = FALSE)
fc_final_arima_acc_monthly <- forecast(model_final_arima_acc_monthly, h = 48)
summary(model_final_arima_acc_monthly)
summary(fc_final_arima_acc_monthly)
fc_final_arima_acc_monthly <- as.data.frame(fc_final_arima_acc_monthly)
write_xlsx(fc_final_arima_acc_monthly, "ARIMA_Acc_Monthly_Preds.xlsx")

##Add Dates##


fc_df_arima_acc_monthly <- data.frame(fc_final_arima_acc_monthly)
last_date_arima_acc_monthly <- max(acc.m.ind.y_mod$month_date)
fc_df_arima_acc_monthly$Date <- seq(from = last_date_arima_acc_monthly %m+% months(1),
                                          by = "1 month",
                                          length.out = nrow(fc_df_arima_acc_monthly))


##Plot: Actual vs Predicted (Test Set)##


n_pred_arima_acc_monthly <- length(fc_test_acc_arima_monthly$mean)
test_dates_arima_acc_monthly <- tail(acc.m.ind.y_mod$month_date, n_pred_arima_acc_monthly)
predicted_df_arima_acc_monthly <- data.frame(
  Date = test_dates_arima_acc_monthly,
  Actual = test_ts_acc_arima_monthly,
  Predicted = as.numeric(fc_test_acc_arima_monthly$mean)
)
ggplot(predicted_df_arima_acc_monthly, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Monthly Accidents (Back-transformed)",
       x = "Month Index", y = "Accidents") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_arima_acc_monthly, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_arima_acc_monthly, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_arima_acc_monthly, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = accidents), color = "red", size = 1) +
  labs(title = "Forecast of Monthly Accidents (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Accidents") +
  theme_minimal()


##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_arima_acc_monthly), lag = 10, type = "Ljung-Box", fitdf = length(model_final_arima_acc_monthly$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_arima_acc_monthly), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_arima_acc_monthly)


###SARIMA###


##Time Series Preparation##


train_ts_acc_sarima_monthly <- window(acc_m_ts_log, end = c(2017, 7))
test_ts_acc_sarima_monthly  <- window(acc_m_ts_log, start = c(2017, 8))


##Fit ARIMA Model on Training Data##


model_acc_sarima_monthly <- Arima(train_ts_acc_sarima_monthly, order = c(2, 1, 1),  seasonal = list(order = c(0,1,1), period = 12))
summary(model_acc_sarima_monthly)


##Forecast on Test Set and Back-transform##


fc_test_acc_sarima_monthly <- forecast(model_acc_sarima_monthly, h = length(test_ts_acc_sarima_monthly))
fc_test_back_acc_sarima_monthly <- expm1(fc_test_acc_sarima_monthly$mean)
test_back_acc_sarima_monthly <- expm1(test_ts_acc_sarima_monthly)
fc_test_back_acc_sarima_monthly <- as.data.frame(fc_test_back_acc_sarima_monthly)
write_xlsx(fc_test_back_acc_sarima_monthly, "SARIMA_Acc_Monthly_Preds.xlsx")

##Evaluate Forecast##


rmse_val_sarima_acc_monthly <- rmse(test_back_acc_sarima_monthly, fc_test_back_acc_sarima_monthly)
mae_val_sarima_acc_monthly  <- mae(test_back_acc_sarima_monthly, fc_test_back_acc_sarima_monthly)
mape_val_sarima_acc_monthly <- mape(test_back_acc_sarima_monthly, fc_test_back_acc_sarima_monthly) * 100
cat("AIC:", AIC(model_acc_sarima_monthly), "\n")
cat("BIC:", BIC(model_acc_sarima_monthly), "\n")
cat("RMSE:", rmse_val_sarima_acc_monthly, "\n")
cat("MAE:", mae_val_sarima_acc_monthly, "\n")
cat("MAPE:", mape_val_sarima_acc_monthly, "%\n")


##Fit Final Model on Full Dataset##


model_final_sarima_acc_monthly <- Arima(acc_m_ts_log, order = c(2, 1, 1),  seasonal = list(order = c(0,1,1), period = 12), include.drift = FALSE)
fc_final_sarima_acc_monthly <- forecast(model_final_sarima_acc_monthly, h = 48)
summary(model_final_sarima_acc_monthly)
summary(fc_final_sarima_acc_monthly)


##Back-transform Forecast##


fc_df_sarima_acc_monthly <- data.frame(fc_final_sarima_acc_monthly)
fc_df_back_sarima_acc_monthly <- expm1(fc_df_sarima_acc_monthly)
last_date_sarima_acc_monthly <- max(acc.m.ind.y_mod$month_date)
fc_df_back_sarima_acc_monthly$Date <- seq(from = last_date_sarima_acc_monthly %m+% months(1),
                                          by = "1 month",
                                          length.out = nrow(fc_df_back_sarima_acc_monthly))
fc_df_back_sarima_acc_monthly <- as.data.frame(fc_df_back_sarima_acc_monthly)
write_xlsx(fc_df_back_sarima_acc_monthly, "SARIMA_Acc_Monthly_Preds.xlsx")


##Plot: Actual vs Predicted (Test Set)##


n_pred_sarima_acc_monthly <- length(fc_test_back_acc_sarima_monthly)
test_dates_sarima_acc_monthly <- tail(acc.m.ind.y_mod$month_date, n_pred_sarima_acc_monthly)
predicted_df_sarima_acc_monthly <- data.frame(
  Date = test_dates_sarima_acc_monthly,
  Actual = test_back_acc_sarima_monthly,
  Predicted = as.numeric(fc_test_back_acc_sarima_monthly)
)
ggplot(predicted_df_sarima_acc_monthly, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Monthly Accidents (Back-transformed)",
       x = "Month Index", y = "Accidents") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_sarima_acc_monthly, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_sarima_acc_monthly, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_sarima_acc_monthly, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = accidents), color = "red", size = 1) +
  labs(title = "Forecast of Monthly Accidents (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Accidents") +
  theme_minimal()


##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_sarima_acc_monthly), lag = 10, type = "Ljung-Box", fitdf = length(model_final_sarima_acc_monthly$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_sarima_acc_monthly), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_sarima_acc_monthly)


###WEEKLY###



###ARIMA###



##Time Series Preparation##


train_ts_acc_arima_weekly <- window(acc_w_ts_log, end = c(2017, 29))
test_ts_acc_arima_weekly  <- window(acc_w_ts_log, start = c(2017, 30))


##Fit ARIMA Model on Training Data##


model_acc_arima_weekly <- Arima(train_ts_acc_arima_weekly, order = c(4, 1, 1), include.drift = TRUE)
summary(model_acc_arima_weekly)


##Forecast on Test Set and Back-transform##


fc_test_acc_arima_weekly <- forecast(model_acc_arima_weekly, h = length(test_ts_acc_arima_weekly))
fc_test_back_acc_arima_weekly <- expm1(fc_test_acc_arima_weekly$mean)
test_back_acc_arima_weekly <- expm1(test_ts_acc_arima_weekly)


##Evaluate Forecast##


rmse_val_arima_acc_weekly <- rmse(test_back_acc_arima_weekly, fc_test_back_acc_arima_weekly)
mae_val_arima_acc_weekly  <- mae(test_back_acc_arima_weekly, fc_test_back_acc_arima_weekly)
mape_val_arima_acc_weekly <- mape(test_back_acc_arima_weekly, fc_test_back_acc_arima_weekly) * 100
cat("AIC:", AIC(model_acc_arima_weekly), "\n")
cat("BIC:", BIC(model_acc_arima_weekly), "\n")
cat("RMSE:", rmse_val_arima_acc_weekly, "\n")
cat("MAE:", mae_val_arima_acc_weekly, "\n")
cat("MAPE:", mape_val_arima_acc_weekly, "%\n")


##Fit Final Model on Full Dataset##


model_final_arima_acc_weekly <- Arima(acc_w_ts_log, order = c(4, 1, 1), include.drift = TRUE)
fc_final_arima_acc_weekly <- forecast(model_final_arima_acc_weekly, h = 192)
summary(model_final_arima_acc_weekly)
summary(fc_final_arima_acc_weekly)


##Back-transform Forecast##


fc_df_arima_acc_weekly <- data.frame(fc_final_arima_acc_weekly)
fc_df_back_arima_acc_weekly <- expm1(fc_df_arima_acc_weekly)
last_date_arima_acc_weekly <- max(acc.w.ind.y_mod$week_date)
fc_df_back_arima_acc_weekly$Date <- seq(from = last_date_arima_acc_weekly + 7, by = "7 days", length.out = nrow(fc_df_back_arima_acc_weekly))
fc_df_back_arima_acc_weekly <- as.data.frame(fc_df_back_arima_acc_weekly)
write_xlsx(fc_df_back_arima_acc_weekly, "ARIMA_Acc_Weekly_Preds.xlsx")


##Plot: Actual vs Predicted (Test Set)##


n_pred_arima_acc_weekly <- length(fc_test_back_acc_arima_weekly)
test_dates_arima_acc_weekly <- tail(acc.w.ind.y_4wmod$week_index, n_pred_arima_acc_weekly)
predicted_df_arima_acc_weekly <- data.frame(
  Date = test_dates_arima_acc_weekly,
  Actual = test_back_acc_arima_weekly,
  Predicted = as.numeric(fc_test_back_acc_arima_weekly)
)
ggplot(predicted_df_arima_acc_weekly, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Weekly Accidents (Back-transformed)",
       x = "Week Index", y = "Accidents") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_arima_acc_weekly, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_arima_acc_weekly, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_arima_acc_weekly, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = accidents), color = "red", size = 1) +
  labs(title = "Forecast of Weekly Accidents (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Accidents") +
  theme_minimal()

##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_arima_acc_weekly), lag = 10, type = "Ljung-Box", fitdf = length(model_final_arima_acc_weekly$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_arima_acc_weekly), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_arima_acc_weekly)


###SARIMA###



##Time Series Preparation##


train_ts_acc_sarima_weekly <- window(acc_w_ts_log, end = c(2017, 29))
test_ts_acc_sarima_weekly  <- window(acc_w_ts_log, start = c(2017, 30))


##Fit ARIMA Model on Training Data##


model_acc_sarima_weekly <- Arima(
  train_ts_acc_sarima_weekly,
  order = c(2, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 48)
)
summary(model_acc_sarima_weekly)

##Forecast on Test Set and Back-transform##


fc_test_acc_sarima_weekly <- forecast(model_acc_sarima_weekly, h = length(test_ts_acc_sarima_weekly))
fc_test_back_acc_sarima_weekly <- expm1(fc_test_acc_sarima_weekly$mean)
test_back_acc_sarima_weekly <- expm1(test_ts_acc_sarima_weekly)


##Evaluate Forecast##


rmse_val_sarima_acc_weekly <- rmse(test_back_acc_sarima_weekly, fc_test_back_acc_sarima_weekly)
mae_val_sarima_acc_weekly  <- mae(test_back_acc_sarima_weekly, fc_test_back_acc_sarima_weekly)
mape_val_sarima_acc_weekly <- mape(test_back_acc_sarima_weekly, fc_test_back_acc_sarima_weekly) * 100
cat("AIC:", AIC(model_acc_sarima_weekly), "\n")
cat("BIC:", BIC(model_acc_sarima_weekly), "\n")
cat("RMSE:", rmse_val_sarima_acc_weekly, "\n")
cat("MAE:", mae_val_sarima_acc_weekly, "\n")
cat("MAPE:", mape_val_sarima_acc_weekly, "%\n")


##Fit Final Model on Full Dataset##


model_final_sarima_acc_weekly <- Arima(acc_w_ts_log, order = c(2, 1, 1),  seasonal = list(order = c(0, 1, 1), period = 48), include.drift = FALSE)
fc_final_sarima_acc_weekly <- forecast(model_final_sarima_acc_weekly, h = 192)
summary(model_final_sarima_acc_weekly)
summary(fc_final_sarima_acc_weekly)


##Back-transform Forecast##


fc_df_sarima_acc_weekly <- data.frame(fc_final_sarima_acc_weekly)
fc_df_back_sarima_acc_weekly <- expm1(fc_df_sarima_acc_weekly)
last_date_sarima_acc_weekly <- max(acc.w.ind.y_mod$week_date)
fc_df_back_sarima_acc_weekly$Date <- seq(from = last_date_sarima_acc_weekly + 7, by = "7 days", length.out = nrow(fc_df_back_sarima_acc_weekly))
fc_df_back_sarima_acc_weekly <- as.data.frame(fc_df_back_sarima_acc_weekly)
write_xlsx(fc_df_back_sarima_acc_weekly, "SARIMA_Acc_Weekly_Preds.xlsx")

##Plot: Actual vs Predicted (Test Set)##


n_pred_sarima_acc_weekly <- length(fc_test_back_acc_sarima_weekly)
test_dates_sarima_acc_weekly <- tail(acc.w.ind.y_4wmod$week_index, n_pred_sarima_acc_weekly)
predicted_df_sarima_acc_weekly <- data.frame(
  Date = test_dates_sarima_acc_weekly,
  Actual = test_back_acc_sarima_weekly,
  Predicted = as.numeric(fc_test_back_acc_sarima_weekly)
)
ggplot(predicted_df_sarima_acc_weekly, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Weekly Accidents (Back-transformed)",
       x = "Week Index", y = "Accidents") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_sarima_acc_weekly, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_sarima_acc_weekly, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_sarima_acc_weekly, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = accidents), color = "red", size = 1) +
  labs(title = "Forecast of Weekly Accidents (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Accidents") +
  theme_minimal()

##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_sarima_acc_weekly), lag = 10, type = "Ljung-Box", fitdf = length(model_final_sarima_acc_weekly$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_sarima_acc_weekly), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_sarima_acc_weekly)


####Final Selection of ARIMA and SARIMA Models for Fatalities (Before GS)####




###MONTHLY###



###ARIMA###


##Time Series Preparation##


train_ts_fat_arima_monthly <- window(fat_m_ts_log, end = c(2017, 7))
test_ts_fat_arima_monthly  <- window(fat_m_ts_log, start = c(2017, 8))


##Fit ARIMA Model on Training Data##


model_fat_arima_monthly <- Arima(train_ts_fat_arima_monthly, order = c(3, 1, 2), include.drift = TRUE)
summary(model_fat_arima_monthly)


##Forecast on Test Set and Back-transform##


fc_test_fat_arima_monthly <- forecast(model_fat_arima_monthly, h = length(test_ts_fat_arima_monthly))
fc_test_back_fat_arima_monthly <- expm1(fc_test_fat_arima_monthly$mean)
test_back_fat_arima_monthly <- expm1(test_ts_fat_arima_monthly)


##Evaluate Forecast##


rmse_val_arima_fat_monthly <- rmse(test_back_fat_arima_monthly, fc_test_back_fat_arima_monthly)
mae_val_arima_fat_monthly  <- mae(test_back_fat_arima_monthly, fc_test_back_fat_arima_monthly)
mape_val_arima_fat_monthly <- mape(test_back_fat_arima_monthly, fc_test_back_fat_arima_monthly) * 100
cat("AIC:", AIC(model_fat_arima_monthly), "\n")
cat("BIC:", BIC(model_fat_arima_monthly), "\n")
cat("RMSE:", rmse_val_arima_fat_monthly, "\n")
cat("MAE:", mae_val_arima_fat_monthly, "\n")
cat("MAPE:", mape_val_arima_fat_monthly, "%\n")


##Fit Final Model on Full Dataset##


model_final_arima_fat_monthly <- Arima(fat_m_ts_log, order = c(3, 1, 2), include.drift = TRUE)
fc_final_arima_fat_monthly <- forecast(model_final_arima_fat_monthly, h = 48)
summary(model_final_arima_fat_monthly)
summary(fc_final_arima_fat_monthly)


##Back-transform Forecast##


fc_df_arima_fat_monthly <- data.frame(fc_final_arima_fat_monthly)
fc_df_back_arima_fat_monthly <- expm1(fc_df_arima_fat_monthly)
last_date_arima_fat_monthly <- max(acc.m.ind.y_mod$month_date)
fc_df_back_arima_fat_monthly$Date <- seq(from = last_date_arima_fat_monthly %m+% months(1),
                                          by = "1 month",
                                          length.out = nrow(fc_df_back_arima_fat_monthly))
fc_df_back_arima_fat_monthly <- as.data.frame(fc_df_back_arima_fat_monthly)
write_xlsx(fc_df_back_arima_fat_monthly, "ARIMA_Fat_Monthly_Preds.xlsx")

##Plot: Actual vs Predicted (Test Set)##


n_pred_arima_fat_monthly <- length(fc_test_back_fat_arima_monthly)
test_dates_arima_fat_monthly <- tail(acc.m.ind.y_mod$month_date, n_pred_arima_fat_monthly)
predicted_df_arima_fat_monthly <- data.frame(
  Date = test_dates_arima_fat_monthly,
  Actual = test_back_fat_arima_monthly,
  Predicted = as.numeric(fc_test_back_fat_arima_monthly)
)
ggplot(predicted_df_arima_fat_monthly, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Monthly Fatalities (Back-transformed)",
       x = "Month Index", y = "Fatalities") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_arima_fat_monthly, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_arima_fat_monthly, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_arima_fat_monthly, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = fatalities), color = "red", size = 1) +
  labs(title = "Forecast of Monthly Fatalities (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Fatalities") +
  theme_minimal()


##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_arima_fat_monthly), lag = 10, type = "Ljung-Box", fitdf = length(model_final_arima_fat_monthly$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_arima_fat_monthly), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_arima_fat_monthly)


###SARIMA###


##Time Series Preparation##


train_ts_fat_sarima_monthly <- window(fat_m_ts_log, end = c(2017, 7))
test_ts_fat_sarima_monthly  <- window(fat_m_ts_log, start = c(2017, 8))


##Fit ARIMA Model on Training Data##


model_fat_sarima_monthly <- Arima(train_ts_fat_sarima_monthly, order = c(2, 1, 1),  seasonal = list(order = c(1,1,1), period = 12))
summary(model_fat_sarima_monthly)


##Forecast on Test Set and Back-transform##


fc_test_fat_sarima_monthly <- forecast(model_fat_sarima_monthly, h = length(test_ts_fat_sarima_monthly))
fc_test_back_fat_sarima_monthly <- expm1(fc_test_fat_sarima_monthly$mean)
test_back_fat_sarima_monthly <- expm1(test_ts_fat_sarima_monthly)


##Evaluate Forecast##


rmse_val_sarima_fat_monthly <- rmse(test_back_fat_sarima_monthly, fc_test_back_fat_sarima_monthly)
mae_val_sarima_fat_monthly  <- mae(test_back_fat_sarima_monthly, fc_test_back_fat_sarima_monthly)
mape_val_sarima_fat_monthly <- mape(test_back_fat_sarima_monthly, fc_test_back_fat_sarima_monthly) * 100
cat("AIC:", AIC(model_fat_sarima_monthly), "\n")
cat("BIC:", BIC(model_fat_sarima_monthly), "\n")
cat("RMSE:", rmse_val_sarima_fat_monthly, "\n")
cat("MAE:", mae_val_sarima_fat_monthly, "\n")
cat("MAPE:", mape_val_sarima_fat_monthly, "%\n")


##Fit Final Model on Full Dataset##


model_final_sarima_fat_monthly <- Arima(fat_m_ts_log, order = c(2, 1, 1),  seasonal = list(order = c(1,1,1), period = 12), include.drift = FALSE)
fc_final_sarima_fat_monthly <- forecast(model_final_sarima_fat_monthly, h = 48)
summary(model_final_sarima_fat_monthly)
summary(fc_final_sarima_fat_monthly)


##Back-transform Forecast##


fc_df_sarima_fat_monthly <- data.frame(fc_final_sarima_fat_monthly)
fc_df_back_sarima_fat_monthly <- expm1(fc_df_sarima_fat_monthly)
last_date_sarima_fat_monthly <- max(acc.m.ind.y_mod$month_date)
fc_df_back_sarima_fat_monthly$Date <- seq(from = last_date_sarima_fat_monthly %m+% months(1),
                                          by = "1 month",
                                          length.out = nrow(fc_df_back_sarima_fat_monthly))
fc_df_back_sarima_fat_monthly <- as.data.frame(fc_df_back_sarima_fat_monthly)
write_xlsx(fc_df_back_sarima_fat_monthly, "SARIMA_Fat_Monthly_Preds.xlsx")

##Plot: Actual vs Predicted (Test Set)##


n_pred_sarima_fat_monthly <- length(fc_test_back_fat_sarima_monthly)
test_dates_sarima_fat_monthly <- tail(acc.m.ind.y_mod$month_date, n_pred_sarima_fat_monthly)
predicted_df_sarima_fat_monthly <- data.frame(
  Date = test_dates_sarima_fat_monthly,
  Actual = test_back_fat_sarima_monthly,
  Predicted = as.numeric(fc_test_back_fat_sarima_monthly)
)
ggplot(predicted_df_sarima_fat_monthly, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Monthly Fatalities (Back-transformed)",
       x = "Month Index", y = "Fatalities") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_sarima_fat_monthly, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_sarima_fat_monthly, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_sarima_fat_monthly, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = fatalities), color = "red", size = 1) +
  labs(title = "Forecast of Monthly Fatalities (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Fatalities") +
  theme_minimal()


##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_sarima_fat_monthly), lag = 10, type = "Ljung-Box", fitdf = length(model_final_sarima_fat_monthly$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_sarima_fat_monthly), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_sarima_fat_monthly)


###WEEKLY###



###ARIMA###



##Time Series Preparation##


train_ts_fat_arima_weekly <- window(fat_w_ts_log, end = c(2017, 29))
test_ts_fat_arima_weekly  <- window(fat_w_ts_log, start = c(2017, 30))


##Fit ARIMA Model on Training Data##


model_fat_arima_weekly <- Arima(train_ts_fat_arima_weekly, order = c(4, 1, 1), include.drift = TRUE)
summary(model_fat_arima_weekly)


##Forecast on Test Set and Back-transform##


fc_test_fat_arima_weekly <- forecast(model_fat_arima_weekly, h = length(test_ts_fat_arima_weekly))
fc_test_back_fat_arima_weekly <- expm1(fc_test_fat_arima_weekly$mean)
test_back_fat_arima_weekly <- expm1(test_ts_fat_arima_weekly)


##Evaluate Forecast##


rmse_val_arima_fat_weekly <- rmse(test_back_fat_arima_weekly, fc_test_back_fat_arima_weekly)
mae_val_arima_fat_weekly  <- mae(test_back_fat_arima_weekly, fc_test_back_fat_arima_weekly)
mape_val_arima_fat_weekly <- mape(test_back_fat_arima_weekly, fc_test_back_fat_arima_weekly) * 100
cat("AIC:", AIC(model_fat_arima_weekly), "\n")
cat("BIC:", BIC(model_fat_arima_weekly), "\n")
cat("RMSE:", rmse_val_arima_fat_weekly, "\n")
cat("MAE:", mae_val_arima_fat_weekly, "\n")
cat("MAPE:", mape_val_arima_fat_weekly, "%\n")


##Fit Final Model on Full Dataset##


model_final_arima_fat_weekly <- Arima(fat_w_ts_log, order = c(4, 1, 1), include.drift = TRUE)
fc_final_arima_fat_weekly <- forecast(model_final_arima_fat_weekly, h = 192)
summary(model_final_arima_fat_weekly)
summary(fc_final_arima_fat_weekly)


##Back-transform Forecast##


fc_df_arima_fat_weekly <- data.frame(fc_final_arima_fat_weekly)
fc_df_back_arima_fat_weekly <- expm1(fc_df_arima_fat_weekly)
last_date_arima_fat_weekly <- max(acc.w.ind.y_mod$week_date)
fc_df_back_arima_fat_weekly$Date <- seq(from = last_date_arima_fat_weekly + 7, by = "7 days", length.out = nrow(fc_df_back_arima_fat_weekly))
fc_df_back_arima_fat_weekly <- as.data.frame(fc_df_back_arima_fat_weekly)
write_xlsx(fc_df_back_arima_fat_weekly, "ARIMA_Fat_Weekly_Preds.xlsx")


##Plot: Actual vs Predicted (Test Set)##


n_pred_arima_fat_weekly <- length(fc_test_back_fat_arima_weekly)
test_dates_arima_fat_weekly <- tail(acc.w.ind.y_4wmod$week_index, n_pred_arima_fat_weekly)
predicted_df_arima_fat_weekly <- data.frame(
  Date = test_dates_arima_fat_weekly,
  Actual = test_back_fat_arima_weekly,
  Predicted = as.numeric(fc_test_back_fat_arima_weekly)
)
ggplot(predicted_df_arima_fat_weekly, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Weekly Fatalities (Back-transformed)",
       x = "Week Index", y = "Fatalities") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_arima_fat_weekly, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_arima_fat_weekly, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_arima_fat_weekly, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = fatalities), color = "red", size = 1) +
  labs(title = "Forecast of Weekly Fatalities (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Fatalities") +
  theme_minimal()

##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_arima_fat_weekly), lag = 10, type = "Ljung-Box", fitdf = length(model_final_arima_fat_weekly$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_arima_fat_weekly), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_arima_fat_weekly)


###SARIMA###



##Time Series Preparation##


train_ts_fat_sarima_weekly <- window(fat_w_ts_log, end = c(2017, 29))
test_ts_fat_sarima_weekly  <- window(fat_w_ts_log, start = c(2017, 30))


##Fit ARIMA Model on Training Data##


model_fat_sarima_weekly <- Arima(
  train_ts_fat_sarima_weekly,
  order = c(1, 1, 2),
  seasonal = list(order = c(1, 1, 0), period = 48)
)
summary(model_fat_sarima_weekly)

##Forecast on Test Set and Back-transform##


fc_test_fat_sarima_weekly <- forecast(model_fat_sarima_weekly, h = length(test_ts_fat_sarima_weekly))
fc_test_back_fat_sarima_weekly <- expm1(fc_test_fat_sarima_weekly$mean)
test_back_fat_sarima_weekly <- expm1(test_ts_fat_sarima_weekly)


##Evaluate Forecast##


rmse_val_sarima_fat_weekly <- rmse(test_back_fat_sarima_weekly, fc_test_back_fat_sarima_weekly)
mae_val_sarima_fat_weekly  <- mae(test_back_fat_sarima_weekly, fc_test_back_fat_sarima_weekly)
mape_val_sarima_fat_weekly <- mape(test_back_fat_sarima_weekly, fc_test_back_fat_sarima_weekly) * 100
cat("AIC:", AIC(model_fat_sarima_weekly), "\n")
cat("BIC:", BIC(model_fat_sarima_weekly), "\n")
cat("RMSE:", rmse_val_sarima_fat_weekly, "\n")
cat("MAE:", mae_val_sarima_fat_weekly, "\n")
cat("MAPE:", mape_val_sarima_fat_weekly, "%\n")


##Fit Final Model on Full Dataset##


model_final_sarima_fat_weekly <- Arima(fat_w_ts_log, order = c(1, 1, 2),  seasonal = list(order = c(1, 1, 0), period = 48), include.drift = FALSE)
fc_final_sarima_fat_weekly <- forecast(model_final_sarima_fat_weekly, h = 192)
summary(model_final_sarima_fat_weekly)
summary(fc_final_sarima_fat_weekly)


##Back-transform Forecast##


fc_df_sarima_fat_weekly <- data.frame(fc_final_sarima_fat_weekly)
fc_df_back_sarima_fat_weekly <- expm1(fc_df_sarima_fat_weekly)
last_date_sarima_fat_weekly <- max(acc.w.ind.y_mod$week_date)
fc_df_back_sarima_fat_weekly$Date <- seq(from = last_date_sarima_fat_weekly + 7, by = "7 days", length.out = nrow(fc_df_back_sarima_fat_weekly))
fc_df_back_sarima_fat_weekly <- as.data.frame(fc_df_back_sarima_fat_weekly)
write_xlsx(fc_df_back_sarima_fat_weekly, "SARIMA_Fat_Weekly_Preds.xlsx")

##Plot: Actual vs Predicted (Test Set)##


n_pred_sarima_fat_weekly <- length(fc_test_back_fat_sarima_weekly)
test_dates_sarima_fat_weekly <- tail(acc.w.ind.y_4wmod$week_index, n_pred_sarima_fat_weekly)
predicted_df_sarima_fat_weekly <- data.frame(
  Date = test_dates_sarima_fat_weekly,
  Actual = test_back_fat_sarima_weekly,
  Predicted = as.numeric(fc_test_back_fat_sarima_weekly)
)
ggplot(predicted_df_sarima_fat_weekly, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Weekly Fatalities (Back-transformed)",
       x = "Week Index", y = "Fatalities") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_sarima_fat_weekly, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_sarima_fat_weekly, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_sarima_fat_weekly, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.w.ind.y_mod, aes(x = week_date, y = fatalities), color = "red", size = 1) +
  labs(title = "Forecast of Weekly Fatalities (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Fatalities") +
  theme_minimal()

##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_sarima_fat_weekly), lag = 10, type = "Ljung-Box", fitdf = length(model_final_sarima_fat_weekly$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_sarima_fat_weekly), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_sarima_fat_weekly)


####Final Selection of ARIMA and SARIMA Models for Accidents (After GS)####




###MONTHLY###



###ARIMA###



##NO GS RESULTS##


###SARIMA###



##Time Series Preparation##


train_ts_acc_sarima_monthly_GS <- window(acc_m_ts_log, end = c(2017, 7))
test_ts_acc_sarima_monthly_GS  <- window(acc_m_ts_log, start = c(2017, 8))


##Fit ARIMA Model on Training Data##


model_acc_sarima_monthly_GS <- Arima(train_ts_acc_sarima_monthly_GS, order = c(1, 0, 2),  seasonal = list(order = c(1,0,3), period = 12))
summary(model_acc_sarima_monthly_GS)


##Forecast on Test Set and Back-transform##


fc_test_acc_sarima_monthly_GS <- forecast(model_acc_sarima_monthly_GS, h = length(test_ts_acc_sarima_monthly_GS))
fc_test_back_acc_sarima_monthly_GS <- expm1(fc_test_acc_sarima_monthly_GS$mean)
test_back_acc_sarima_monthly_GS <- expm1(test_ts_acc_sarima_monthly_GS)


##Evaluate Forecast##


rmse_val_sarima_acc_monthly_GS <- rmse(test_back_acc_sarima_monthly_GS, fc_test_back_acc_sarima_monthly_GS)
mae_val_sarima_acc_monthly_GS  <- mae(test_back_acc_sarima_monthly_GS, fc_test_back_acc_sarima_monthly_GS)
mape_val_sarima_acc_monthly_GS <- mape(test_back_acc_sarima_monthly_GS, fc_test_back_acc_sarima_monthly_GS) * 100
cat("AIC:", AIC(model_acc_sarima_monthly_GS), "\n")
cat("BIC:", BIC(model_acc_sarima_monthly_GS), "\n")
cat("RMSE:", rmse_val_sarima_acc_monthly_GS, "\n")
cat("MAE:", mae_val_sarima_acc_monthly_GS, "\n")
cat("MAPE:", mape_val_sarima_acc_monthly_GS, "%\n")


##Fit Final Model on Full Dataset##


model_final_sarima_acc_monthly_GS <- Arima(acc_m_ts_log, order = c(1, 0, 2),  seasonal = list(order = c(1,0,3), period = 12), include.drift = FALSE)
fc_final_sarima_acc_monthly_GS <- forecast(model_final_sarima_acc_monthly_GS, h = 48)
summary(model_final_sarima_acc_monthly_GS)
summary(fc_final_sarima_acc_monthly_GS)


##Back-transform Forecast##


fc_df_sarima_acc_monthly_GS <- data.frame(fc_final_sarima_acc_monthly_GS)
fc_df_back_sarima_acc_monthly_GS <- expm1(fc_df_sarima_acc_monthly_GS)
last_date_sarima_acc_monthly_GS <- max(acc.m.ind.y_mod$month_date)
fc_df_back_sarima_acc_monthly_GS$Date <- seq(from = last_date_sarima_acc_monthly_GS %m+% months(1),
                                          by = "1 month",
                                          length.out = nrow(fc_df_back_sarima_acc_monthly_GS))
fc_df_back_sarima_acc_monthly_GS <- as.data.frame(fc_df_back_sarima_acc_monthly_GS)
write_xlsx(fc_df_back_sarima_acc_monthly_GS, "ARIMA_Acc_Monthly_Preds_GS.xlsx")

##Plot: Actual vs Predicted (Test Set)##


n_pred_sarima_acc_monthly_GS <- length(fc_test_back_acc_sarima_monthly_GS)
test_dates_sarima_acc_monthly_GS <- tail(acc.m.ind.y_mod$month_date, n_pred_sarima_acc_monthly_GS)
predicted_df_sarima_acc_monthly_GS <- data.frame(
  Date = test_dates_sarima_acc_monthly_GS,
  Actual = test_back_acc_sarima_monthly_GS,
  Predicted = as.numeric(fc_test_back_acc_sarima_monthly_GS)
)
ggplot(predicted_df_sarima_acc_monthly_GS, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Monthly Accidents (Back-transformed)",
       x = "Month Index", y = "Accidents") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_sarima_acc_monthly_GS, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_sarima_acc_monthly_GS, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_sarima_acc_monthly_GS, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = accidents), color = "red", size = 1) +
  labs(title = "Forecast of Monthly Accidents (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Accidents") +
  theme_minimal()


##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_sarima_acc_monthly_GS), lag = 10, type = "Ljung-Box", fitdf = length(model_final_sarima_acc_monthly_GS$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_sarima_acc_monthly_GS), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_sarima_acc_monthly_GS)


###WEEKLY###



###ARIMA###



##NO GS RESULTS##


###SARIMA###



##NO GS RESULTS##


####Final Selection of ARIMA and SARIMA Models for Fatalities (After GS)####




###MONTHLY###



###ARIMA###



##NO GS RESULTS


###SARIMA###



train_ts_fat_sarima_monthly_GS <- window(fat_m_ts_log, end = c(2017, 7))
test_ts_fat_sarima_monthly_GS  <- window(fat_m_ts_log, start = c(2017, 8))


##Fit ARIMA Model on Training Data##


model_fat_sarima_monthly_GS <- Arima(train_ts_fat_sarima_monthly_GS, order = c(1, 0, 2),  seasonal = list(order = c(1,1,1), period = 12))
summary(model_fat_sarima_monthly_GS)


##Forecast on Test Set and Back-transform##


fc_test_fat_sarima_monthly_GS <- forecast(model_fat_sarima_monthly_GS, h = length(test_ts_fat_sarima_monthly_GS))
fc_test_back_fat_sarima_monthly_GS <- expm1(fc_test_fat_sarima_monthly_GS$mean)
test_back_fat_sarima_monthly_GS <- expm1(test_ts_fat_sarima_monthly_GS)


##Evaluate Forecast##


rmse_val_sarima_fat_monthly_GS <- rmse(test_back_fat_sarima_monthly_GS, fc_test_back_fat_sarima_monthly_GS)
mae_val_sarima_fat_monthly_GS  <- mae(test_back_fat_sarima_monthly_GS, fc_test_back_fat_sarima_monthly_GS)
mape_val_sarima_fat_monthly_GS <- mape(test_back_fat_sarima_monthly_GS, fc_test_back_fat_sarima_monthly_GS) * 100
cat("AIC:", AIC(model_fat_sarima_monthly_GS), "\n")
cat("BIC:", BIC(model_fat_sarima_monthly_GS), "\n")
cat("RMSE:", rmse_val_sarima_fat_monthly_GS, "\n")
cat("MAE:", mae_val_sarima_fat_monthly_GS, "\n")
cat("MAPE:", mape_val_sarima_fat_monthly_GS, "%\n")


##Fit Final Model on Full Dataset##


model_final_sarima_fat_monthly_GS <- Arima(fat_m_ts_log, order = c(1, 0, 2),  seasonal = list(order = c(1,1,1), period = 12), include.drift = FALSE)
fc_final_sarima_fat_monthly_GS <- forecast(model_final_sarima_fat_monthly_GS, h = 48)
summary(model_final_sarima_fat_monthly_GS)
summary(fc_final_sarima_fat_monthly_GS)


##Back-transform Forecast##


fc_df_sarima_fat_monthly_GS <- data.frame(fc_final_sarima_fat_monthly_GS)
fc_df_back_sarima_fat_monthly_GS <- expm1(fc_df_sarima_fat_monthly_GS)
last_date_sarima_fat_monthly_GS <- max(acc.m.ind.y_mod$month_date)
fc_df_back_sarima_fat_monthly_GS$Date <- seq(from = last_date_sarima_fat_monthly_GS %m+% months(1),
                                          by = "1 month",
                                          length.out = nrow(fc_df_back_sarima_fat_monthly_GS))
fc_df_back_sarima_fat_monthly_GS <- as.data.frame(fc_df_back_sarima_fat_monthly_GS)
write_xlsx(fc_df_back_sarima_fat_monthly_GS, "SARIMA_Fat_Monthly_Preds_GS.xlsx")


##Plot: Actual vs Predicted (Test Set)##


n_pred_sarima_fat_monthly_GS <- length(fc_test_back_fat_sarima_monthly_GS)
test_dates_sarima_fat_monthly_GS <- tail(acc.m.ind.y_mod$month_date, n_pred_sarima_fat_monthly_GS)
predicted_df_sarima_fat_monthly_GS <- data.frame(
  Date = test_dates_sarima_fat_monthly_GS,
  Actual = test_back_fat_sarima_monthly_GS,
  Predicted = as.numeric(fc_test_back_fat_sarima_monthly_GS)
)
ggplot(predicted_df_sarima_fat_monthly_GS, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Monthly Fatalities (Back-transformed)",
       x = "Month Index", y = "Fatalities") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_sarima_fat_monthly_GS, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_sarima_fat_monthly_GS, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_sarima_fat_monthly_GS, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = fatalities), color = "red", size = 1) +
  labs(title = "Forecast of Monthly Fatalities (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Fatalities") +
  theme_minimal()


##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_sarima_fat_monthly_GS), lag = 10, type = "Ljung-Box", fitdf = length(model_final_sarima_fat_monthly_GS$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_sarima_fat_monthly_GS), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_sarima_fat_monthly_GS)


###WEEKLY###



###ARIMA###



##NO GS RESULTS##


###SARIMA###


train_ts_fat_sarima_weekly_GS <- window(fat_w_ts_log, end = c(2017, 29))
test_ts_fat_sarima_weekly_GS  <- window(fat_w_ts_log, start = c(2017, 30))


##Fit ARIMA Model on Training Data##


model_fat_sarima_weekly_GS <- Arima(train_ts_fat_sarima_weekly_GS, order = c(3, 0, 3),  seasonal = list(order = c(0,1,3), period = 48))
summary(model_fat_sarima_weekly_GS)


##Forecast on Test Set and Back-transform##


fc_test_fat_sarima_weekly_GS <- forecast(model_fat_sarima_weekly_GS, h = length(test_ts_fat_sarima_weekly_GS))
fc_test_back_fat_sarima_weekly_GS <- expm1(fc_test_fat_sarima_weekly_GS$mean)
test_back_fat_sarima_weekly_GS <- expm1(test_ts_fat_sarima_weekly_GS)


##Evaluate Forecast##


rmse_val_sarima_fat_weekly_GS <- rmse(test_back_fat_sarima_weekly_GS, fc_test_back_fat_sarima_weekly_GS)
mae_val_sarima_fat_weekly_GS  <- mae(test_back_fat_sarima_weekly_GS, fc_test_back_fat_sarima_weekly_GS)
mape_val_sarima_fat_weekly_GS <- mape(test_back_fat_sarima_weekly_GS, fc_test_back_fat_sarima_weekly_GS) * 100
cat("AIC:", AIC(model_fat_sarima_weekly_GS), "\n")
cat("BIC:", BIC(model_fat_sarima_weekly_GS), "\n")
cat("RMSE:", rmse_val_sarima_fat_weekly_GS, "\n")
cat("MAE:", mae_val_sarima_fat_weekly_GS, "\n")
cat("MAPE:", mape_val_sarima_fat_weekly_GS, "%\n")


##Fit Final Model on Full Dataset##


model_final_sarima_fat_weekly_GS <- Arima(fat_w_ts_log, order = c(3, 0, 3),  seasonal = list(order = c(0,1,3), period = 48), include.drift = FALSE)
fc_final_sarima_fat_weekly_GS <- forecast(model_final_sarima_fat_weekly_GS, h = 192)
summary(model_final_sarima_fat_weekly_GS)
summary(fc_final_sarima_fat_weekly_GS)


##Back-transform Forecast##


fc_df_sarima_fat_weekly_GS <- data.frame(fc_final_sarima_fat_weekly_GS)
fc_df_back_sarima_fat_weekly_GS <- expm1(fc_df_sarima_fat_weekly_GS)
last_date_sarima_fat_weekly_GS <- max(acc.w.ind.y_mod$week_date)
fc_df_back_sarima_fat_weekly_GS$Date <- seq(from = last_date_sarima_fat_weekly_GS + 7, by = "7 days", length.out = nrow(fc_df_back_sarima_fat_weekly_GS))
fc_df_back_sarima_fat_weekly_GS <- as.data.frame(fc_df_back_sarima_fat_weekly_GS)
write_xlsx(fc_df_back_sarima_fat_weekly_GS, "SARIMA_Fat_Weekly_Preds_GS.xlsx")


##Plot: Actual vs Predicted (Test Set)##


n_pred_sarima_fat_weekly_GS <- length(fc_test_back_fat_sarima_weekly_GS)
test_dates_sarima_fat_weekly_GS <- tail(acc.w.ind.y_4wmod$week_index, n_pred_sarima_fat_weekly_GS)
predicted_df_sarima_fat_weekly_GS <- data.frame(
  Date = test_dates_sarima_fat_weekly_GS,
  Actual = test_back_fat_sarima_weekly_GS,
  Predicted = as.numeric(fc_test_back_fat_sarima_weekly_GS)
)
ggplot(predicted_df_sarima_fat_weekly_GS, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Weekly Fatalities (Back-transformed)",
       x = "Week Index", y = "Fatalities") +
  theme_minimal()


##Plot: Forecast to 2026 with Confidence Intervals##


ggplot() +
  geom_line(data = fc_df_back_sarima_fat_weekly_GS, aes(x = Date, y = Point.Forecast), color = "blue", size = 1) +
  geom_ribbon(data = fc_df_back_sarima_fat_weekly_GS, aes(x = Date, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = fc_df_back_sarima_fat_weekly_GS, aes(x = Date, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.1) +
  geom_line(data = acc.m.ind.y_mod, aes(x = month_date, y = fatalities), color = "red", size = 1) +
  labs(title = "Forecast of Weekly Fatalities (Back-transformed) with 80% and 95% CI",
       x = "Date", y = "Fatalities") +
  theme_minimal()


##Residual Diagnostics##


cat("\nLjung-Box Test (lag = 10):\n")
print(Box.test(residuals(model_final_sarima_fat_weekly_GS), lag = 10, type = "Ljung-Box", fitdf = length(model_final_sarima_fat_weekly_GS$coef)))
cat("\nBox-Pierce Test (lag = 12):\n")
print(Box.test(residuals(model_final_sarima_fat_weekly_GS), lag = 12, type = "Box-Pierce"))
checkresiduals(model_final_sarima_fat_weekly_GS)
