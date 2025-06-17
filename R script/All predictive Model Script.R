#“I (Anh Bui) solemnly swear the work below is my work and my work alone. I did not consult with any other individuals, AI search
#engines, or anything besides my brain and the class notes in the creaƟon of my midterm. I recognize abiding by this statement reflects
#my integrity.”

options(scipen=10)
library(ggplot2)

exam2_dir = "C:\\Users\\hoang\\OneDrive - University of St. Thomas\\Forecasting Spring 2025\\exam2_Anh_Bui\\"
dis_csv = "dis_earnings.csv"

dis_source = paste0(exam2_dir, dis_csv)
dis_df = read.csv(data_source)
n = length(dis_df[,"period"])

for(t in 1:n)
{
  dis_df[t, "residuals"] = dis_df[t, "actual_eps"] - dis_df[t, "forecast_eps"]
}

lags = function(temp_df, column_name, n, k)
{
  column_label = paste0("l", k, "_", column_name)
  for(t in (1+k):n)
  {
    temp_df[t, column_label] = temp_df[t-k, column_name]
  }
  lags = temp_df
}

dis_df = lags(dis_df, "residuals", n, 1)
dis_df = lags(dis_df, "residuals", n, 4)

### Linear Regression Model ###
earnings_lm = lm(actual_eps ~ forecast_eps + l1_residuals + l4_residuals, dis_df)
summary(earnings_lm)

# (Intercept)  -0.018031  With P-value   0.390    
# forecast_eps  1.055149   With P-value   <2e-16 ***
# l1_residuals  0.008715 With P-value  0.976    
# l4_residuals  0.272686  With P-value  0.158    
# The variable significnat at the 0.05 value is forecast_eps
# Multiple R-squared:  0.9936,	Adjusted R-squared:  0.9928 
# Mathematical formula: -0.018031 + 1.055149*dis_df[t, "forecast_eps"] + 0.008715*dis_df[t, "l1_residuals"] + 0.272686*dis_df[t, "l4_residuals"]

### Model Fit and Residuals ###
for(t in 1:n)
{
  dis_df[t, "adj_forecast"] = -0.018031 + 1.055149*dis_df[t, "forecast_eps"] + 0.008715*dis_df[t, "l1_residuals"] + 0.272686*dis_df[t, "l4_residuals"]
  dis_df[t, "adj_residuals"] = dis_df[t, "actual_eps"] - dis_df[t, "adj_forecast"]
}

adj_estimate = -0.018031 + 1.055149*1.00 + 0.008715*dis_df[t, "l1_residuals"] + 0.272686*dis_df[t, "l4_residuals"]
print(adj_estimate)

# Hypothetical Forecast
# Your boss says, “Gee golly whiz, they’ve done it again. Disney gave us an ‘earnings forecast’ of $1.00/share for
# next quarter. What does your model estimate their true earnings will be?”
# *What is your adjusted earnings estimate? Show work in R-syntax 
# adjusted earnings estimate is 1.062357 with forecast_eps = 1

### Plotting ###
library(ggplot2)
disney_path = paste0(exam2_dir, "disney_Anh_Bui.png")
disney_plot = ggplot(dis_df)
disney_plot = disney_plot + geom_line(aes(x=period, y=actual_eps), color="darkgrey")
disney_plot = disney_plot + geom_point(aes(x=period, y=actual_eps), color="darkmagenta")
disney_plot = disney_plot + geom_line(aes(x=period, y=forecast_eps), color="blue")
disney_plot = disney_plot + geom_line(aes(x=period, y=adj_forecast), color="red")
disney_plot = disney_plot + labs(x="Period", y="Cash Money (Dollars)", title="Adjusted Disney Earnings")
ggsave(filename = disney_path, plot = disney_plot, width = 10, height = 5)

### Export CSV ###
disney_path = paste0(exam2_dir, "earnings_Anh_Bui.csv")
write.csv(dis_df, disney_path)

### Model II - Pomme De Terre ARIMA ###

options(scipen=10)
library(ggplot2)

### Paths ###
exam2_dir = "C:\\Users\\hoang\\OneDrive - University of St. Thomas\\Forecasting Spring 2025\\exam2_Anh_Bui\\"
pomme_csv = "pomme_de_terre.csv"
pomme_source = paste0(exam2_dir, pomme_csv)

arima_df = read.csv(pomme_source)
n_arima = length(arima_df[,"period"])

difference = function(temp_df, column_name, n, k)
{
  column_label = paste0("d", k,"_",column_name)
  for(t in (1+k):n)
  {
    temp_df[t,column_label] = temp_df[t, column_name] - temp_df[t-k, column_name]
  }
  difference = temp_df
}

lags = function(temp_df, column_name, n, k)
{
  column_label = paste0("l", k,"_",column_name)
  for(t in (1+k):(n+k))
  {
    temp_df[t,column_label] = temp_df[t-k, column_name]
  }
  lags = temp_df
}

arima_df = difference(arima_df, "total_units", n_arima, 1)
arima_df = lags(arima_df, "d1_total_units", n_arima, 1)
arima_df = lags(arima_df, "d1_total_units", n_arima, 12)

ari_lm = lm(d1_total_units ~ l1_d1_total_units + l12_d1_total_units, arima_df)
summary(ari_lm)

# (Intercept)        -2408.29283 With P-value is   0.917    
# l1_d1_total_units     -0.11484   With P-value is   0.223    
# l12_d1_total_units     1.11457  With P-value is 0.00000000725 ***
# Multiple R-squared:  0.8235,	Adjusted R-squared:  0.8059 
# The variable that statistically significant at the 0.05 level is l12_d1_total_units
# The mathematical formula is: arima_forecast = 


for(t in 1:n_arima)
{
  arima_df[t,"ari_forecast"] = -2408.29283 + -0.11484*arima_df[t,"l1_d1_total_units"] + 1.11457*arima_df[t,"l12_d1_total_units"]
  arima_df[t,"ari_residual"] = arima_df[t,"d1_total_units"] - arima_df[t,"ari_forecast"]
}


# Future Forcast 
k = 1
for(t in (n_arima + 1):(n_arima + k))
{
  arima_df[t, "period"] = t
  
  # Lags for AR model input
  arima_df[t, "l1_d1_total_units"] = arima_df[t - 1, "d1_total_units"]
  arima_df[t, "l12_d1_total_units"] = arima_df[t - 12, "d1_total_units"]
  
  # Forecast using model coefficients
  arima_df[t,"ari_forecast"] = -2408.29283 + -0.11484*arima_df[t,"l1_d1_total_units"] + 1.11457*arima_df[t,"l12_d1_total_units"]
  arima_df[t, "ari_residual"] = arima_df[t, "d1_total_units"] - arima_df[t, "ari_forecast"]
}

arima_df = lags(arima_df, "ari_residual", n_arima, 1)
arima_df = lags(arima_df, "ari_residual", n_arima, 2)

# Regression ARIMA
arima_lm = lm(d1_total_units ~ ari_forecast + l1_ari_residual + l2_ari_residual, arima_df)
summary(arima_lm) 

# (Intercept)      6376.84078 With P-value is  0.7725    
# ari_forecast        1.04787  With P-value is 0.0000000071 ***
#  l1_ari_residual    -0.49646 With P-value is    0.0692 .  
# l2_ari_residual    -0.07552 With P-value is   0.7442 
# Multiple R-squared:  0.8695,	Adjusted R-squared:  0.8464 
# The variable that statiscally significant is ari_forecast, l1_ari_residual
# The mathematical model is d1_total_units = 6376.84078 + 1.04787*ari_forecast + -0.49646*l1_ari_residual + -0.07552*l2_ari_residual

# Model Fit (Difference Forecast) 
for(t in 1:n_arima)
{
  arima_df[t,"arima_forecast"] = 6376.84078 + 1.04787*arima_df[t,"ari_forecast"] + -0.49646*arima_df[t,"l1_ari_residual"] + -0.07552*arima_df[t,"l2_ari_residual"]
  arima_df[t,"arima_residual"] = arima_df[t,"d1_total_units"] - arima_df[t,"arima_forecast"]
}
rmse_arima = sqrt(mean(arima_df[,"arima_residual"]^2, na.rm=TRUE))
# 87852.51

# Model Fit (Level Forecast)
for(t in 2:n_arima)
{
  arima_df[t, "level_forecast"] = arima_df[t - 1, "total_units"] + arima_df[t, "arima_forecast"]
  arima_df[t, "level_residual"] = arima_df[t, "total_units"] - arima_df[t, "level_forecast"]
}

rmse_level = sqrt(mean(arima_df[,"level_residual"]^2, na.rm=TRUE))
# 87852.51

# Future Forecast
k = 1
for(t in (n_arima + 1):(n_arima + k))
{
  arima_df[t, "period"] = t
  
  arima_df[t, "l1_d1_total_units"] = arima_df[t - 1, "d1_total_units"]
  arima_df[t, "l12_d1_total_units"] = arima_df[t - 12, "d1_total_units"]
  
  arima_df[t, "l1_ari_residual"] = arima_df[t - 1, "ari_residual"]
  arima_df[t, "l2_ari_residual"] = arima_df[t - 2, "ari_residual"]

  arima_df[t, "ari_forecast"] = -2408.29283 +
    -0.11484 * arima_df[t, "l1_d1_total_units"] +

  arima_df[t, "arima_forecast"] = 6376.84078 +
    1.04787 * arima_df[t, "ari_forecast"] +
    -0.49646 * arima_df[t, "l1_ari_residual"] +
    -0.07552 * arima_df[t, "l2_ari_residual"]

  arima_df[t, "level_forecast"] = arima_df[t - 1, "total_units"] + arima_df[t, "arima_forecast"]
}

print(arima_df[n_arima + 1, "arima_forecast"])
# -880904.5
print(arima_df[n_arima + 1, "level_forecast"])
# 1179261

difference_path = paste0(exam2_dir, "difference_Anh_Bui.png")
difference_plot = ggplot(arima_df)
difference_plot = difference_plot + geom_line(aes(x = period, y = d1_total_units), color = "darkgrey")
difference_plot = difference_plot + geom_point(aes(x = period, y = d1_total_units), color = "darkmagenta")
difference_plot = difference_plot + geom_line(aes(x = period, y = arima_forecast), color = "red")
difference_plot = difference_plot + labs(x = "Period", y = "Units", title = "Pomme De Terre: Difference Forecast")
ggsave(difference_path, difference_plot, width = 10, height = 5)

level_path = paste0(exam2_dir, "level_Anh_Bui.png")
level_plot = ggplot(arima_df)
level_plot = level_plot + geom_line(aes(x = period, y = total_units), color = "darkgrey")
level_plot = level_plot + geom_point(aes(x = period, y = total_units), color = "darkmagenta")
level_plot = level_plot + geom_line(aes(x = period, y = level_forecast), color = "red")
level_plot = level_plot + labs(x = "Period", y = "Units", title = "Pomme De Terre: Level Forecast")
ggsave(level_path, level_plot, width = 10, height = 5)

arima_path = paste0(exam2_dir, "arima_Anh_Bui.csv")
write.csv(arima_df, arima_path)

# Model III - Winter Exponential Smoothing ###

options(scipen=10)
library(ggplot2)

exam2_dir = "C:\\Users\\hoang\\OneDrive - University of St. Thomas\\Forecasting Spring 2025\\exam2_Anh_Bui\\"
pomme_csv = "pomme_de_terre.csv"
pomme_source = paste0(exam2_dir, pomme_csv)
winter_df = read.csv(pomme_source)
n_winter = length(winter_df[,"period"])
c = 12
full = n/c
# There are 3 full cycles in the dataframe

### Winter ES function ###
winter_es = function(temp_df, column_name, a, b, g, c, n, k) {
  smooth_col = paste0("smooth_", a, "_", b, "_", g)
  trend_col = paste0("trend_", a, "_", b, "_", g)
  forecast_col = paste0("forecast_", a, "_", b, "_", g)
  residual_col = paste0("residual_", a, "_", b, "_", g)
  seasonal_col = paste0("seasonal_", a, "_", b, "_", g)
  
  for(t in 1:c) {
    temp_df[t, smooth_col] = temp_df[t, column_name]
    temp_df[t, trend_col] = (temp_df[n, column_name] - temp_df[1, column_name]) / (n - 1)
    temp_df[t, seasonal_col] = temp_df[t, column_name] / mean(temp_df[1:c, column_name])
  }
  
  for(t in (c+1):n) {
    temp_df[t, smooth_col] = a * (temp_df[t, column_name] / temp_df[t - c, seasonal_col]) + (1 - a) * (temp_df[t - 1, smooth_col] + temp_df[t - 1, trend_col])
    temp_df[t, trend_col] = b * (temp_df[t, smooth_col] - temp_df[t - 1, smooth_col]) + (1 - b) * temp_df[t - 1, trend_col]
    temp_df[t, seasonal_col] = g * (temp_df[t, column_name] / temp_df[t, smooth_col]) + (1 - g) * temp_df[t - c, seasonal_col]
    temp_df[t, forecast_col] = (temp_df[t - 1, smooth_col] + temp_df[t - 1, trend_col]) * temp_df[t - c, seasonal_col]
    temp_df[t, residual_col] = temp_df[t, column_name] - temp_df[t, forecast_col]
  }
  
  for(t in (n + 1):(n + k)) {
    temp_df[t, "period"] = t
    temp_df[t, forecast_col] = (temp_df[n, smooth_col] + (t - n) * temp_df[n, trend_col]) * temp_df[t - c, seasonal_col]
  }
  
  return(temp_df)
}

### Apply three test models ###
winter_df = winter_es(winter_df, "total_units", 0.25, 0.25, 0.25, c, n_winter, 12)
winter_df = winter_es(winter_df, "total_units", 0.5, 0.5, 0.5, c, n_winter, 12)
winter_df = winter_es(winter_df, "total_units", 0.75, 0.75, 0.75, c, n_winter, 12)

### RMSE for those models ###
rmse_025 = sqrt(mean(na.omit(winter_df[,"residual_0.25_0.25_0.25"])^2))
# 207594.9
rmse_050 = sqrt(mean(na.omit(winter_df[,"residual_0.5_0.5_0.5"])^2))
# 161125.3
rmse_075 = sqrt(mean(na.omit(winter_df[,"residual_0.75_0.75_0.75"])^2))
# 165841.7

### Grid search ###
rmse_df = data.frame()
row_number = 1

for(a in 1:9) {
  temp_alpha = a/10
  for(b in 1:3) {
    temp_beta = b/4
    for(g in 1:4) {
      temp_gamma = g/5
      winter_df = winter_es(winter_df, "total_units", a, b, g, c, n_winter, 12)
      residuals = na.omit(winter_df[, paste0("residual_", a, "_", b, "_", g)])
      rmse = sqrt(mean(residuals^2))
      rmse_df[row_number, "alpha"] = a
      rmse_df[row_number, "beta"] = b
      rmse_df[row_number, "gamma"] = g
      rmse_df[row_number, "rmse"] = rmse
      row_number = row_number + 1
      # Print the correct RMSE value
      print(paste0("alpha: ", temp_alpha, " beta: ", temp_beta, " gamma: ", temp_gamma, " rmse: ", rmse))
    }
  }
}

# Optimal Alpha and Beta
# alpha: 0.1 beta: 0.25 gamma: 0.2 rmse: 246358.544873204

winter_df = winter_es(winter_df, "total_units", 0.1, 0.25, 0.2, c, n_winter, 12)

### Period and Profit Analysis ###
total_25_36 = sum(winter_df[25:36, "total_units"])
# 15732205
forecast_37_48 = sum(winter_df[37:48, paste0("forecast_", 0.1, "_", 0.25, "_", 0.2)])
# 17068955
diff_units = forecast_37_48 - total_25_36
# 1336751
pct_change_units = (forecast_37_48 - total_25_36) / total_25_36 * 100
# 8.496906

profit_actual = total_25_36 * 40
# 629288190
profit_forecast = forecast_37_48 * 50
# 853447771
diff_profit = profit_forecast - profit_actual
# 224159580
pct_change_profit = (profit_forecast - profit_actual) / profit_actual * 100
# 35.62113

### Plot ###
optimal_path = paste0(exam2_dir, "optimal_Anh_Bui.png")
winter_plot = ggplot(winter_df)
winter_plot = winter_plot + geom_line(aes(x = period, y = total_units), color = "darkgrey")
winter_plot = winter_plot + geom_point(aes(x = period, y = total_units), color = "darkmagenta")
winter_plot = winter_plot + geom_line(aes(x = period, y = winter_df[, paste0("forecast_", 0.1, "_", 0.25, "_", 0.2)]), color = "red")
winter_plot = winter_plot + labs(x = "Period", y = "Units", title = "Winter Forecast: Pomme de Terre")
ggsave(optimal_path, winter_plot, width = 10, height = 5)

### Export CSV ###
winter_path = paste0(exam2_dir, "winter_Anh_Bui.csv")
write.csv(winter_df, winter_path)

# Three things that I'm greatful for: 
# this is my last final and this school yer is ending
# I'm getting 2 research grants for the summer
# I'm getting an intership for the summer

# Good and bad Sandwich
# good: I think I learn a lots of R code in this class, I did use what I learn in class in my interview
# bad: I think the reading is so long for each chapter, I wish it a bit shorter and more concise
# good: I get to work in groups with good teammates