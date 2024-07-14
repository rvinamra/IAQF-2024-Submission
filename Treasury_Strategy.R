library(PerformanceAnalytics)
library(tidyverse)
library(readxl)
library(lubridate)
library(gridExtra)
library(lmtest)
library(sandwich)
library(caret)
library(stargazer)

set.seed(123)

#read in Fed Data
mpd_bond_data <- read_excel("mpd_stats_bond_data.xlsx")

#grab the 10 year data, format and group by monthly mean skew
mpd_bond_data <- mpd_bond_data |> 
  filter(market == 'tr10yr') |> 
  mutate(idt = ymd(idt)) |> 
  mutate(year = year(idt)) |>
  mutate(month = month(idt)) |>
  group_by(market, year, month) |>
  summarise(monthly_skew = mean(skew, na.rm = TRUE), monthly_kurt = mean(kurt, na.rm = TRUE), .groups = "drop")


#Grab WRDS bond return data
monthly_bond_returns <- read_excel("Bond Return Data 2.xlsx")

#turn them into excess returns by subtracting 30 day bill returns
excess_bond_return <- monthly_bond_returns |> 
  mutate(across(-c(B30D), ~ . - B30D))

#turn into xts object
excess_xts <- xts(excess_bond_return[,-1], order.by = (excess_bond_return$`Calendar_Date`))

#grab TLT index data for an idea that did not end up getting significant results
tlt_index_data <- read_excel('TLT Index Data.xlsx', sheet = 'Values')

#filter by date to coincide with Fed Data
tlt_index_data <- tlt_index_data |> 
  filter(Dates >= "2012-11-30") |>
  mutate(across(-Dates, ~as.numeric(as.character(.))))

#create xts object
tlt_index_xts <- xts(tlt_index_data[,2:ncol(tlt_index_data)], order.by = tlt_index_data$Dates)

#calculate returns and lagged 3 month returns
tlt_index_returns <- Return.calculate(tlt_index_xts[,1:4])
tlt_index_3m_returns <- rollapply(tlt_index_returns, width = 3, FUN = Return.cumulative, by.column = TRUE, align = "right")
tlt_index_3m_returns_lag <- lag.xts(tlt_index_3m_returns, k = -3)

#turn into dataframes for merging
tlt_index_return_df <- data.frame(date = index(tlt_index_returns), coredata(tlt_index_returns)) |> 
  na.omit() |> 
  mutate(year = year(date)) |>
  mutate(month = month(date)) 

tlt_index_3m_return_df <- data.frame(date = index(tlt_index_3m_returns_lag), coredata(tlt_index_3m_returns_lag)) |> 
  na.omit() |> 
  mutate(year = year(date)) |>
  mutate(month = month(date))


#calculate three month excess returns and lagged 3 month excess returns
roll_3m_excess <- rollapply(excess_xts, width = 3, FUN = Return.cumulative, by.column = TRUE, align = "right")
roll_3m_excess_lag <- lag.xts(roll_3m_excess, k = -3)
roll_3m_excess_lag_df <- as.data.frame(roll_3m_excess_lag)
roll_3m_excess_lag_df$month_end_date <- rownames(roll_3m_excess_lag_df)
roll_3m_excess_lag_df$month_end_date <- as.Date(roll_3m_excess_lag_df$month_end_date)

roll_3m_excess_lag_df <- roll_3m_excess_lag_df |>
  mutate(year = year(month_end_date)) |>
  mutate(month = month(month_end_date))

monthly_bond_returns <- monthly_bond_returns |> 
  mutate(DATE = ymd(Calendar_Date)) |> 
  mutate(year = year(DATE)) |>
  mutate(month = month(DATE))


#merging the various data frames
merged_tlt_data <- merge(tlt_index_return_df, mpd_bond_data, by.x = c("year", "month"), by.y = c("year", "month")) |> 
  select(-market)

merged_tlt_data <- merge(merged_tlt_data, tlt_index_3m_return_df, by = c("year", "month")) 

#incorporating the PCA data
PCA_data <- read.csv('PCA_export.csv')
PCA_data <- PCA_data |> 
  mutate(DATE = ymd(DATE)) |> 
  mutate(year = year(DATE)) |>
  mutate(month = month(DATE))


merged_tlt_data <- merge(merged_tlt_data, PCA_data, by = c("year", "month")) |> 
  select(-DATE)

merged_tlt_data <- merge(merged_tlt_data, monthly_bond_returns, by = c("year", "month")) |> 
  select(-DATE)

merged_tlt_data <- merge(merged_tlt_data, roll_3m_excess_lag_df, by = c("year", "month")) |> 
  select(-month_end_date)


#various CBOE indexes on TLT and option writing strategy overlays
return_columns <- c('BXTBW.Index.x',	'TLT.US.Equity.x',	'BXTW.Index.x',	'PTLT.Index.x')

merged_tlt_data <- merged_tlt_data |> 
  mutate(across(all_of(return_columns), 
                ~ lead(.x, n = 1), 
                .names = "lagged_{.col}")) |> 
  na.omit()
  

# List of dependent variables of lagged returns
dependent_vars <- c('B10Y.y',	'B5Y.y', 	'B2Y.y')

# Initialize an empty list to store models
models <- list()
model_summaries <- list()
train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE) # setting 5 fold cross-validation

#Loop through the dependent variables to train the models
for (var in dependent_vars) {
  formula <- as.formula(paste(var, "~ monthly_skew + Level + Slope + Curvature"))
  
  # Train the model with cross-validation
  model <- train(formula,
                 data = merged_tlt_data,
                 method = "lm",
                 trControl = train_control)
  
  # Store the trained model
  models[[var]] <- model
  
  # Extract and print the cross-validation summary for each model
  cat("Model for", var, "\n")
  print(model$results) # This shows the performance metric(s) used during training
  cat("-------------------------------------------------\n")
  
  # Optional: Store the summary for later use or for aggregating results
  model_summaries[[var]] <- model$results
}

# Loop through the models list to print out the R-squared for each model
for (var in dependent_vars) {
  model_summary <- summary(models[[var]])
  cat("R-squared for", var, ":", model_summary$r.squared, "\n")
}


summary(models[['B10Y.y']])
summary(models[['B5Y.y']])
summary(models[['B2Y.y']])


#grab only the quarterly data
merged_tlt_data <- merged_tlt_data |> 
  filter(month %in% c(3, 6, 9, 12))


merged_tlt_data$B10Y_predict <- predict(models[["B10Y.y"]], newdata = merged_tlt_data)
merged_tlt_data$B5Y_predict <- predict(models[["B5Y.y"]], newdata = merged_tlt_data)
merged_tlt_data$B2Y_predict <- predict(models[["B2Y.y"]], newdata = merged_tlt_data)



ratio = 1/3


merged_tlt_data$Weight_2_Year_Bond <- ifelse(merged_tlt_data$B2Y_predict > 0, ratio, -ratio)
merged_tlt_data$Weight_5_Year_Bond <- ifelse(merged_tlt_data$B5Y_predict > 0, ratio, -ratio)
merged_tlt_data$Weight_10_Year_Bond <- ifelse(merged_tlt_data$B10Y_predict > 0, ratio, -ratio)


merged_tlt_data$net_position <- rowSums(merged_tlt_data[,c('Weight_2_Year_Bond', 'Weight_5_Year_Bond', 'Weight_10_Year_Bond')])

merged_tlt_data$return_2Y <- merged_tlt_data$Weight_2_Year_Bond * merged_tlt_data$B2Y.y
merged_tlt_data$return_5Y <- merged_tlt_data$Weight_5_Year_Bond * merged_tlt_data$B5Y.y
merged_tlt_data$return_10Y <- merged_tlt_data$Weight_10_Year_Bond * merged_tlt_data$B10Y.y


merged_tlt_data$Monthly_Total_Portfolio_Return <- with(merged_tlt_data,
                                                       return_2Y +
                                                         return_5Y +
                                                         return_10Y 
)

merged_tlt_data$Bond_index <- with(merged_tlt_data,
                                   (B30Y.y/3) +
                                     (B5Y.y/3) +
                                     (B2Y.y/3)) 



tlt_port_xts <- xts(merged_tlt_data$Monthly_Total_Portfolio_Return, order.by = merged_tlt_data$date.x)


tlt_index_return <- xts(merged_tlt_data$Bond_index, order.by = merged_tlt_data$date.x)

scale = 4 #for quarterly returns


#performance charts
SharpeRatio.annualized(tlt_port_xts, scale = scale)
table.AnnualizedReturns(tlt_port_xts, scale = scale)
chart.Drawdown(tlt_port_xts, scale = scale)
charts.PerformanceSummary(tlt_port_xts, main="Performance Summary of Portfolio Strategy")
table.Drawdowns(tlt_port_xts, top = 5, longs = TRUE, legend.loc = "bottomright")

port_decomp_return <- xts(merged_tlt_data[,c('return_2Y', 'return_5Y', 'return_10Y', 'Monthly_Total_Portfolio_Return')], order.by = merged_tlt_data$date.x)
colnames(port_decomp_return) <- c('2Y', '5Y', '10Y', 'Strategy')



table.AnnualizedReturns(port_decomp_return, scale = scale)
table.CaptureRatios(port_decomp_return, tlt_index_return)
table.InformationRatio(port_decomp_return, tlt_index_return, scale = scale)

chart.CumReturns(port_decomp_return, main="Cumulative Returns of Portfolio Strategy", legend.loc ='top', ylog = TRUE)
chart.CumReturns(port_decomp_return, main="Cumulative Returns of Portfolio Strategy", legend.loc ='top', ylog = FALSE)
chart.Drawdown(port_decomp_return, scale = scale, legend.loc ='bottom')
charts.PerformanceSummary(port_decomp_return, main="Performance Summary", legend.loc ='top', ylog = TRUE)

table.AnnualizedReturns(tlt_index_return, scale = scale)
chart.CumReturns(tlt_index_return, main="Cumulative Returns of TLT Index", col="blue")
chart.Drawdown(tlt_index_return, scale = scale)
table.Drawdowns(tlt_index_return, top = 5, longs = TRUE, legend.loc = "bottomright")



write.csv(merged_tlt_data, "merged_tlt_data.csv")



#creating monthly plot to make chart show more detailed volatility (something private equity won't do)
monthly_plot <- excess_bond_return |> 
  mutate(idt = as.Date(Calendar_Date)) |> 
  mutate(year = year(idt)) |>
  mutate(month = month(idt))


merge_monthly_plot <- merge(monthly_plot, merged_tlt_data, by = c("year", "month"), all = TRUE) 

merge_monthly_plot$Weight_2_Year_Bond <- na.locf(merge_monthly_plot$Weight_2_Year_Bond, na.rm = FALSE)
merge_monthly_plot$Weight_5_Year_Bond <- na.locf(merge_monthly_plot$Weight_5_Year_Bond, na.rm = FALSE)
merge_monthly_plot$Weight_10_Year_Bond <- na.locf(merge_monthly_plot$Weight_10_Year_Bond, na.rm = FALSE)

merge_monthly_plot$return_2Y <- merge_monthly_plot$Weight_2_Year_Bond * merge_monthly_plot$B2Y
merge_monthly_plot$return_5Y <- merge_monthly_plot$Weight_5_Year_Bond * merge_monthly_plot$B5Y
merge_monthly_plot$return_10Y <- merge_monthly_plot$Weight_10_Year_Bond * merge_monthly_plot$B10Y
merge_monthly_plot$Monthly_Total_Portfolio_Return <- with(merge_monthly_plot,
                                                       return_2Y +
                                                         return_5Y +
                                                         return_10Y 
)

data <- merged_tlt_data

data <- data %>%
  mutate(date.x = ymd(date.x)) %>%
  arrange(date.x)

#log returns 
data2 <- merge_monthly_plot %>%
  mutate(return_2Y = log(1 + return_2Y),
         return_5Y = log(1 + return_5Y),
         return_10Y = log(1 + return_10Y),
         Monthly_Total_Portfolio_Return = log(1 + Monthly_Total_Portfolio_Return)) 

data2$Calendar_Date.x <- format(data2$Calendar_Date.x, "%Y-%m-%d")
data2$Calendar_Date.x <- as.Date(data2$Calendar_Date.x)



data_xts_2 <- xts(data2[,c('return_2Y', 'return_5Y', 'return_10Y', 'Monthly_Total_Portfolio_Return')], order.by = data2$Calendar_Date.x)
colnames(data_xts_2) <- c('2 Year', '5 Year', '10 Year', 'Combined Strategy')
data_xts_subset <- window(data_xts_2, start=as.Date("2013-11-30"), end=as.Date("2023-07-30"))

data_xts_subset <- data_xts_subset |> 
  na.omit()

table.Drawdowns(data_xts_subset$`Combined Strategy`, top = 5, longs = TRUE, legend.loc = "bottomright")

index(data_xts_subset) <- as.yearmon(index(data_xts_subset))



write.csv(data2, "data_xts_subset.csv")



chart.CumReturns(data_xts_subset, main="Cumulative Returns", legend.loc ='top', log = TRUE)
table.AnnualizedReturns(data_xts_subset, scale = 12)





