#gas price data
library(quantmod)
library(zoo)
library(xts)
library(TTR)
library(tidyverse)
p_gas <- getSymbols("TTF=F", src="yahoo", from = "2000-09-01", to = "2023-06-02", auto.assign = FALSE)

#tests for g
library(stats)
library(tseries)

#TESTS WITH ACTUAL PRICES
#box Pierce to test autocorrelation - evidence of autocorrelation
box_Pierce_test_g <- Box.test(as.numeric(price_gas, lag = 1, type = "Ljung-Box"))
print(box_Pierce_test_g)

#non-stationary
library(aTSA)
kpss_test_result_g <- kpss.test(as.numeric(price_gas))

print(kpss_test_result_g)

#non-stationary 
adf_result_g <- adf.test(as.numeric(price_gas))
print(adf_result_g)

#Not normally distributed
jarque_bera_result_g <- jarque.bera.test(as.numeric(na.omit(price_gas)))

# Print the test result
print(jarque_bera_result_g)









#TESTS WITH NORMAL RETURNS GAS
daily_normal_returns_gas <- dailyReturn(p_gas$`TTF=F.Close`)
plot(daily_normal_returns_gas)

#There is autocorrelation (WHAT TO USE AS LAG)
box_jenkins_test_norm_gas <- Box.test(daily_normal_returns_gas, lag = 1, type = "Ljung-Box")
print(box_jenkins_test_p)


#type 3 is significant indicating stationarity for drift & trend
library(aTSA)
kpss_test_result_norm_gas <- kpss.test(daily_normal_returns_gas)

print(kpss_test_result_norm_gas)

#test for stationarity - non-stationary 
adf_result_norm_gas <- adf.test((daily_normal_returns_gas))
print(adf_result_norm_gas)

#Jarque-Bera to test data for normal distribution - Not normally distributed
jarque_bera_result_norm_gas <- jarque.bera.test(as.numeric(daily_normal_returns_gas))

# Print the test result
print(jarque_bera_result_norm_gas)












#TESTS WITH LOG RETURNS GAS
daily_log_returns_gas <- dailyReturn(p_gas$`TTF=F.Close`, type="log")
plot(daily_log_returns_gas)

#There is autocorrelation 
box_jenkins_test_log_gas <- Box.test(daily_log_returns_gas, lag = 1, type = "Ljung-Box")
print(box_jenkins_test_log_gas)


#type 3 is significant indicating stationarity for drift & trend, same as with regular returns
library(aTSA)
kpss_test_result_log_gas <- kpss.test(daily_log_returns_gas)

print(kpss_test_result_log_gas)

#test for stationarity - non-stationary 
adf_result_log_gas <- adf.test((daily_log_returns_gas))
print(adf_result_log_gas)

#Jarque-Bera to test data for normal distribution - Not normally distributed
jarque_bera_result_log_gas <- jarque.bera.test(as.numeric(daily_log_returns_gas))

# Print the test result
print(jarque_bera_result_log_gas)






#TESTS WITH DIFFERENCES GAS








#TESTS FOR P ACTUAL PRICES
#box jenkins to test autocorrelation - There is autocorrelation (WHAT TO USE AS LAG)
box_jenkins_test_p <- Box.test(p_phosphorus_3_T, lag = 1, type = "Ljung-Box")
print(box_jenkins_test_p)
plot(p_phosphorus_3_T)
#test for stationarity - data is non-stationary 
library(aTSA)
kpss_test_result_p <- kpss.test(p_phosphorus_3_T)

print(kpss_test_result_p)

#test for stationarity - non-stationary
adf_result_p <- adf.test((p_phosphorus_3_T))
print(adf_result_p)

#Not normally distributed
jarque_bera_result_p <- jarque.bera.test(as.numeric(p_phosphorus_3_T))

# Print the test result
print(jarque_bera_result_p)











#TESTS FOR P RETURNS
quarterly_normal_return_p <- diff(p_phosphorus_3_T)
plot(quarterly_normal_return_p)
#box jenkins to test autocorrelation - There is autocorrelation (WHAT TO USE AS LAG)
box_jenkins_test_p_norm <- Box.test(quarterly_normal_return_p, lag = 1, type = "Ljung-Box")
print(box_jenkins_test_p_norm)

#test for stationarity - data is non-stationary
library(aTSA)
kpss_test_result_p_norm <- kpss.test(quarterly_normal_return_p)

print(kpss_test_result_p_norm)

#test for stationarity - stationary & could follow a random walk ?????
adf_result_p_norm <- adf.test((quarterly_normal_return_p))
print(adf_result_p_norm)

#Not normally distributed
jarque_bera_result_p_norm <- jarque.bera.test(as.numeric(quarterly_normal_return_p))

# Print the test result
print(jarque_bera_result_p_norm)












#TESTS FOR P LOG RETURNS
quarterly_log_return_p <- diff(log(p_phosphorus_3_T))
plot(quarterly_log_return_p)
#box jenkins to test autocorrelation - There is autocorrelation (WHAT TO USE AS LAG)
box_jenkins_test_p_log <- Box.test(quarterly_log_return_p, lag = 1, type = "Ljung-Box")
print(box_jenkins_test_p_log)

#test for stationarity - data is non-stationary
library(aTSA)
kpss_test_result_p_log <- kpss.test(quarterly_log_return_p)

print(kpss_test_result_p_log)

#test for stationarity - stationary & could follow a random walk ?????
adf_result_p_log <- adf.test((quarterly_log_return_p))
print(adf_result_p_log)

#Not normally distributed
jarque_bera_result_p_log <- jarque.bera.test(as.numeric(quarterly_log_return_p))

# Print the test result
print(jarque_bera_result_p_log)














#TESTS P DIFFERENCES









#test for correlation: Make p and g the same length of timeseries

#Quarterly data for Gas
p_gas_correlation_test <- getSymbols("TTF=F", src="yahoo", from = "2000-09-01", to = "2023-01-01", auto.assign = FALSE)
plot(p_gas_correlation_test)
p_gas_correlation_test_quarterly <- to.quarterly(p_gas_correlation_test$`TTF=F.Close`, indexAt = "lastof", OHLC=FALSE)
print(p_gas_correlation_test_quarterly)
p_gas_correlation_test_quarterly_ts <- ts(p_gas_correlation_test_quarterly, start=c(2017,4), end=c(2022, 4), frequency = 4)
print(p_gas_correlation_test_quarterly_ts)

#Quarterly data for p
p_phosphorus_correlation_test <- read_excel("C:\\Users\\stefp\\Documents\\Price_path_p_correlation_test.xlsx",  col_names = FALSE)

col_names <- as.character(p_phosphorus_correlation_test[1, ])
colnames(p_phosphorus_correlation_test) <- col_names

p_phosphorus_correlation_path <- p_phosphorus_correlation_test[-1, ]

p_phosphorus_correlation_T <- t(p_phosphorus_correlation_path)

p_phosphorus_correlation_T_num<- as.numeric(p_phosphorus_correlation_T)
print(p_phosphorus_correlation_T_num)
plot(p_phosphorus_correlation_T_num)

p_phosphorus_correlation_test_ts <- ts(p_phosphorus_correlation_T_num, start=c(2017,4), end=c(2022,4), frequency=4)
print(p_phosphorus_correlation_test_ts)


#CORRELATION TEST with data as used for NPV

Correlation_p_g <- cor.test(p_gas_correlation_test_quarterly_ts, p_phosphorus_correlation_test_ts)
print(Correlation_p_g)

correlation_p_g_indexed <- cor.test(price_gas, price_phosphorus)
print(correlation_p_g_indexed)

#plotting against eachother

start_date <- as.Date("2017-10-01")  # Assuming the start is in October 2017 (Q4)
end_date <- as.Date("2021-01-01")    # Assuming the end is in December 2022 (Q4)
dates <- seq(start_date, end_date, by = "quarter")

price_gas <- c(100,	92.88990826,	111.4933741,	139.2966361,	112.0285423,	72.45158002,	51.55453619,	83.53720693,	61.41692151,	35.16819572,	31.42201835,	67.71151886,	97.47706422,	96.81447503		
)
price_phosphorus <- c(99.7,	102.7668,	98.6072,	102.8892,	112.0648,	113.8999,	107.2935,	107.9052,	107.9052,	105.336,	97.1391,	93.5912,	93.95824,	104.8466
)


#last 7 g years 176.4627931 498.3384302,	358.5270133,	641.7227319,	736.5647299,	962.2833843,	388.9653415
# last 7 p years 121.6074 127.1128,	234.7733,	266.7044,	293.9865,	309.4016,	306.2207
df <- data.frame(dates, price_gas, price_phosphorus)
ggplot(df, aes(x = dates)) +
  geom_line(aes(y = price_gas, color = "gas")) +
  geom_line(aes(y = price_phosphorus, color = "phosphorus")) +
  labs(x = "Time", y = "Value", title = "Time Series Comparison") +
  scale_color_manual(values = c("gas" = "blue", "phosphorus" = "red")) 

ggplot(df, aes(x = price_gas, y = price_phosphorus)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "indexed gas", y = "indexed phosphorus", title = "Time Series Comparison")

#get regression & find significance
regression_model <- lm(price_phosphorus ~ price_gas)
print(regression_model)
summary(regression_model)

#TEST CORRELATION WITHOUT CRISIS
start_date_old <- as.Date("31-12-2004")  # Assuming the start is in October 2017 (Q4)
end_date_old <- as.Date("31-12-2017")    # Assuming the end is in December 2022 (Q4)
dates_old <- seq(start_date_old, end_date_old, by = "year")

price_gas_older <- c(39.99280058,	47.80417567,	56.47948164,	60.04319654,	61.66306695,	62.67098632,	60.43916487,	62.49100072,	63.53491721,	70.30237581,	70.23038157,	68.57451404,	67.92656587,	65.95572354
)
price_phosphorus_older <- c(93.30,	94.45,	95.75,	82.50,	182.85,	184.00,	122.95,	148.60,	161.58,	159.74,	140.79,	143.43,	150.27,	153.68
)




df_old <- data.frame(dates_old, price_gas_older, price_phosphorus_older)
View(df_old)
ggplot(df_old, aes(x = dates_old)) +
  geom_line(aes(y = price_gas_older, color = "gas")) +
  geom_line(aes(y = price_phosphorus_older, color = "phosphorus")) +
  labs(x = "Time", y = "Value", title = "Time Series Comparison") +
  scale_color_manual(values = c("gas" = "blue", "phosphorus" = "red")) 

ggplot(df_old, aes(x = price_gas_older, y = price_phosphorus_older)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "gas", y = "phosphorus", title = "Time Series Comparison, no crisis")

#get regression & find significance
regression_model <- lm(price_gas_older ~ price_phosphorus_older)
print(regression_model)
summary(regression_model)



#INDEXED

start_date_indexed <- as.Date("31-12-2004")  # Assuming the start is in October 2017 (Q4)
end_date_indexed <- as.Date("31-12-2017")    # Assuming the end is in December 2022 (Q4)
dates_indexed <- seq(start_date_indexed, end_date_indexed, by = "year")

price_gas_indexed <- c(100.0000014,	119.5319549,	141.2241244,	150.1350157,	154.1854208,	156.7056728,	151.1251147,	156.2556278,	158.8658889,	175.7875813,	175.6075633,	171.4671492,	169.8469871,	164.9189943
                     
                     
                     
)
price_phosphorus_indexed <- c(100,	101.2325831,	102.6259378,	88.4244373,	195.9807074,	197.2132905,	131.7792069,	159.2711683,	173.1832797,	171.2111468,	150.9003215,	153.7299035,	161.0610932,	164.71597
                            
                            
)




#last 6 g years 498.3384302,	358.5270133,	641.7227319,	736.5647299,	962.2833843,	388.9653415
# last 6 p years 127.1128,	234.7733,	266.7044,	293.9865,	309.4016,	306.2207
df_indexed <- data.frame(dates_indexed, price_gas_indexed, price_phosphorus_indexed)
View(df_indexed)
ggplot(df_indexed, aes(x = dates_indexed)) +
  geom_line(aes(y = price_gas_indexed, color = "gas")) +
  geom_line(aes(y = price_phosphorus_indexed, color = "phosphorus")) +
  labs(x = "Time", y = "Value", title = "indexed") +
  scale_color_manual(values = c("gas" = "blue", "phosphorus" = "red")) 

ggplot(df_indexed, aes(x = price_gas_indexed, y = price_phosphorus_indexed)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "gas", y = "phosphorus", title = "Time Series Comparison, no crisis & indexed")

#get regression & find significance
regression_model_indexed <- lm(price_gas_indexed ~ price_phosphorus_indexed)
print(regression_model_indexed)
summary(regression_model_indexed)



