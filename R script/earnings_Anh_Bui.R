options(scipen=10)
library(ggplot2)

project_dir = "C:\\Users\\hoang\\OneDrive - University of St. Thomas\\Forecasting Spring 2025\\project_data\\"
earnings_csv = "earnings_TSM.csv"
data_source = paste0(project_dir, earnings_csv)

###Read in data###
earnings_df = read.csv(data_source)
n = length(earnings_df[,"period"])

### Import Functions ###
# Holt Exponential Smoothing function
### Define Holt Function ###
start_row = 5
holt = function(temp_df, column_name, a, b, n, k, start_row)
{
  ### Column Labels ### 
  smooth_col = paste0("smooth_", a, "_", b)
  trend_col = paste0("trend_", a, "_", b)
  forecast_col = paste0("forecast_", a, "_", b)
  residual_col = paste0("residual_", a, "_", b)
  
  ### Initial Conditions ### 
  temp_df[start_row, smooth_col] = temp_df[start_row, column_name]
  temp_df[start_row, trend_col] = (temp_df[n, column_name] - temp_df[start_row, column_name])/(n-start_row)
  
  ### Iterate Over Data ###
  for(t in (start_row+1):n)
  {
    temp_df[t, smooth_col] = a*temp_df[t-1, smooth_col] + (1-a)*temp_df[t, column_name]
    temp_df[t, trend_col] = b*(temp_df[t, smooth_col] - temp_df[t-1, smooth_col]) + (1-b)*temp_df[t-1, trend_col]
    temp_df[t, forecast_col] = temp_df[t-1,smooth_col] + temp_df[t, trend_col]
    temp_df[t, residual_col] = temp_df[t, column_name] - temp_df[t, forecast_col]
  }
  
  ### Future Forecast ### 
  for(t in (n+1):(n+k))
  {
    temp_df[t,"period"] = t
    temp_df[t, forecast_col] = temp_df[n, smooth_col] + temp_df[n, trend_col]*(t-n)
  }
  
  holt = temp_df
}

### Defining Function 'sma' ###
sma = function(temp_df, column_name, n, k)
{
  
  column_label = paste0("sma", k,"_",column_name)
  
  for(t in (1+k):n)
  {
    temp_df[t,column_label] = mean(temp_df[(t-k):(t-1),column_name])
  }
  
  sma = temp_df
}


### Data Manipulation – Simple Moving Average ###
for(t in 4:n) 
  {
  earnings_df[t, "sma4_actual_eps"] = mean(earnings_df[(t-3):t, "actual_eps"])
}

### Optimal Alpha, Beta Grid Search ###
rmse_df = data.frame()
row_number = 1

for(a in 1:19) {
  temp_alpha = a/20
  for(b in 1:19) {
    temp_beta = b/20
    earnings_df = holt(earnings_df, "sma4_actual_eps", temp_alpha, temp_beta, 42, 4, 5)
    residuals = na.omit(earnings_df[, paste0("residual_", temp_alpha, "_", temp_beta)])
    rmse = sqrt(mean(residuals^2))
    rmse_df[row_number, "temp_alpha"] = temp_alpha
    rmse_df[row_number, "temp_beta"] = temp_beta
    rmse_df[row_number, "rmse"] = rmse
    row_number = row_number + 1
    # Print the correct RMSE value
    print(paste0("alpha: ", temp_alpha, " beta: ", temp_beta," rmse: ", rmse))
  }
}

# * Optimal Alpha and Beta

# "alpha: 0.05 beta: 0.95 rmse: 0.006054083"

### Clear Data ###
earnings_df = read.csv(data_source)

for(t in 4:n) 
{
  earnings_df[t, "sma4_actual_eps"] = mean(earnings_df[(t-3):t, "actual_eps"])
}

### Call Holt ###
earnings_df = holt(earnings_df, "sma4_actual_eps", .05, .95, n, 4, 5)



### Plot: Holt ###
earnings_path = paste0(project_dir, "earnings_TSM.png")
earnings_plot = ggplot(earnings_df) +
  geom_line(aes(x = period, y = sma4_actual_eps), color = "blue") +
  geom_point(aes(x = period, y = sma4_actual_eps), color = "darkmagenta") +
  geom_line(aes(x = period, y = forecast_0.05_0.95), color = "red")
ggsave(filename = earnings_path, plot = earnings_plot, width = 10, height = 5)

### Write CSV ###
results_path = paste0(project_dir, "holt_earnings_TSM_Anh_Bui.csv")
write.csv(earnings_df, results_path, row.names = FALSE)
