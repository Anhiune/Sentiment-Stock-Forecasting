options(scipen=10)
library(ggplot2)

project_dir = "C:\\Users\\hoang\\OneDrive - University of St. Thomas\\Forecasting Spring 2025\\project_data\\"
earnings_csv = "earnings_TSM.csv"
price_csv = "price_TSM.csv"
eps_source = paste0(project_dir, earnings_csv)
price_source = paste0(project_dir, price_csv)

### Read Data ###
earnings_df = read.csv(eps_source)
n_eps = length(earnings_df[,"period"])
price_df = read.csv(price_source)
n_price = length(price_df[,"period"])

### Data Manipulation – Simple Moving Average ###
for(t in 4:n_eps) 
  {
  earnings_df[t, "sma4_actual_eps"] = mean(earnings_df[(t-3):t, "actual_eps"])
}

### Merge Data ###
combined_df = merge(earnings_df, price_df[, c("date", "adj_close")], by="date")
n = length(combined_df[,"period"])

### Data Manipulation – Percentage Change ###
percentage = function(temp_df, column_name, n, k)
{
  
  column_label = paste0("p", k,"_",column_name)
  
  for(t in (1+k):n)
  {
    temp_df[t,column_label] = (temp_df[t, column_name] - temp_df[t-k,column_name])/temp_df[t-k,column_name]
  }
  
  percentage = temp_df
}

### Calling Function 'percentage' ### 
k = 1
combined_df = percentage(combined_df, "sma4_actual_eps", n, k)
combined_df = percentage(combined_df, "adj_close", n, k)

### Regression Model: Autoregression ###
p1_close_lm = lm(p1_adj_close ~ p1_sma4_actual_eps, data = combined_df)
summary(p1_close_lm)

# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)          0.1123    with the P-value is  0.42672   
# p1_sma4_actual_eps   0.8518   with the P-value is 0.00509 **
# The "p1_sma4_actual_eps" is statistically significant at the 0.05 level
# Multiple R-squared:  0.01249,	Adjusted R-squared:  -0.01283 
# F-statistic: 0.4933 on 1 and 39 DF,  p-value: 0.4866

### Model Fit – Percentage Change ###
for (t in 5:(n-1))
{
  combined_df[, "forecast_p1_adj_close"] = 0.1123 + 0.8518*combined_df[t, "p1_sma4_actual_eps"]
  }


### Plot: Price vs EPS ###
scatter_path = paste0(project_dir, "scatter_TSM.png")
scatter_plot = ggplot(combined_df) +
  geom_point(aes(x = p1_sma4_actual_eps, y = p1_adj_close), color = "blue") +
  geom_point(aes(x = p1_sma4_actual_eps, y = forecast_p1_adj_close), color = "red")
ggsave(filename = scatter_path, plot = scatter_plot, width = 10, height = 5)

### Read Holt Data ###
holts_path = paste0(project_dir, "holt_earnings_TSM_Anh_Bui.csv")
holt_earnings_df = read.csv(holts_path)
n_holt = length(holt_earnings_df[, "period"])

# Modify Holt’s DF (from pervious section)
holt_earnings_df = percentage(holt_earnings_df, "forecast_0.05_0.95", n, k)
holt_earnings_df = merge(holt_earnings_df, combined_df[, c("period", "adj_close")], by="period", all=TRUE)
holt_earnings_df = holt_earnings_df[order(holt_earnings_df[, "period"]),]

### Model Fit – Level Forecast ###
holt_earnings_df[, "forecast_p1_adj_close"] = 0.1123 + 0.8518*combined_df[t, "forecast_0.05_0.95"]
holt_earnings_df[, "level_adjusted_close"] = holt_earnings_df[, "forecast_p1_adj_close"]

### Future Level Forecast ###
for(t in (n_holt + 1):(n_holt + 4)) 
  {
  holt_earnings_df[t, "period"] = t
  holt_earnings_df[t, "level_adjusted_close"] = holt_earnings_df[n_holt, "level_adjusted_close"]
}

### Plot: Level Forecast ###
level_path = paste0(project_dir, "level_TSM.png")
level_plot = ggplot(holt_earnings_df) +
  geom_line(aes(x = period, y = adj_close), color = "blue") +
  geom_point(aes(x = period, y = adj_close), color = "darkmagenta") +
  geom_line(aes(x = period, y = level_adjusted_close), color = "red")
ggsave(filename = level_path, plot = level_plot, width = 10, height = 5)

### Write CSV ###
results_path = paste0(project_dir, "price_TSM_Anh_Bui.csv")
write.csv(holt_earnings_df, results_path, row.names = FALSE)