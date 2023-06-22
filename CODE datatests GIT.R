#gas price data
library(quantmod)
library(zoo)
library(xts)
library(TTR)

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
end_date <- as.Date("2022-12-31")    # Assuming the end is in December 2022 (Q4)
dates <- seq(start_date, end_date, by = "quarter")

price_gas <- c(100,	92.88990826,	111.4933741,	139.2966361,	112.0285423,	72.45158002,	51.55453619,	83.53720693,	61.41692151,	35.16819572,	31.42201835,	67.71151886,	97.47706422,	96.81447503, 176.4627931, 498.3384302,	358.5270133,	641.7227319,	736.5647299,	962.2833843,	388.9653415
)
price_phosphorus <- c(99.7,	102.7668,	98.6072,	102.8892,	112.0648,	113.8999,	107.2935,	107.9052,	107.9052,	105.336,	97.1391,	93.5912,	93.95824,	104.8466, 121.6074, 127.1128,	234.7733,	266.7044,	293.9865,	309.4016,	306.2207
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

#get regression & find significance regular prices
regression_model <- lm(price_phosphorus ~ price_gas)
print(regression_model)
summary(regression_model)

spearman_rank_regression <- cor.test(price_gas, price_phosphorus,  method = "spearman")
print(spearman_rank_regression)

#get log returns & calculate correlation again with these results NO CORRELATION WHEN USING LOG RETURNS
price_gas_log_crisis <- c(2,	1.967968534,	2.047249059,	2.143940629,	2.049328685,	1.860047861,	1.712266884,	1.921879951,	1.788288044,	1.546150088,	1.497234078,	1.830662556,	1.988902441,	1.985940295,	2.246653149,	2.69752438,	2.554521883,	2.807347424,	2.867210919,	2.983302987,	2.589910906
)
price_phosphorus_log_crisis <- c(2,	2.013157675,	1.995213469,	2.013674632,	2.050774062,	2.057828184	,2.031878254,	2.034347216,	2.034347216,	2.023881664,	1.988698917,	1.972539857,	1.974239715,	2.021859193,	2.086264845,	2.105494127,	2.371953546,	2.427335022,	2.46963223,	2.491827397,	2.487339387

)

price_gas_log_return <- diff(price_gas_log_crisis)
print(price_gas_log_return)
price_phosphorus_log_return <- diff(price_phosphorus_log_crisis)
print(price_phosphorus_log_return)


regression_log_crisis <- lm(price_gas_log_return ~ price_phosphorus_log_return)
print(regression_log_crisis)
summary(regression_log_crisis)


#TEST CORRELATION older data WITHOUT CRISIS
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
ggplot(df_indexed, aes(x = dates_indexed)) +
  geom_line(aes(y = price_gas_indexed, color = "gas")) +
  geom_line(aes(y = price_phosphorus_indexed, color = "phosphorus")) +
  labs(x = "Time", y = "Value", title = "indexed") +
  scale_color_manual(values = c("gas" = "blue", "phosphorus" = "red")) 

ggplot(df_indexed, aes(x = price_gas_indexed, y = price_phosphorus_indexed)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "gas", y = "phosphorus", title = "Time Series Comparison, no crisis & indexed")

#get regression & find significance FOR REGULAR PRICES
regression_model_indexed <- lm(price_gas_indexed ~ price_phosphorus_indexed)
print(regression_model_indexed)
summary(regression_model_indexed)


#INDEXED & CORRECT LOGARITHMIC


start_date_indexed <- as.Date("31-12-2004")  # Assuming the start is in October 2017 (Q4)
end_date_indexed <- as.Date("31-12-2017")    # Assuming the end is in December 2022 (Q4)
dates_indexed <- seq(start_date_indexed, end_date_indexed, by = "year")

price_gas_indexed_log <- c(2.000000006,	2.077484022,	2.149908891,	2.176481994,	2.18804331,	2.195084718,	2.179336643,	2.193835668,	2.201030657,	2.244988191,	2.244543217,	2.234180927,	2.230057847,	2.217270678

)
price_phosphorus_indexed_log <- c(2,	2.005320319,	2.011257139,	1.946572305,	2.292213321,	2.294936179,	2.119846889,	2.202137166,	2.23850596,	2.233532036,	2.178690165,	2.186758355,	2.206990643,	2.216735708

)
#NO CORRELATION WHEN USING LOG RETURNS
price_gas_indexed_log_returns<- diff(price_gas_indexed_log)
print(price_gas_indexed_log_returns)
price_phosphorus_indexed_log_returns<- diff(price_phosphorus_indexed_log)
print(price_phosphorus_indexed_log_returns)
price_log_correlation <- lm(price_gas_indexed_log_returns ~ price_phosphorus_indexed_log_returns)
print(price_log_correlation)
summary(price_log_correlation)


#get regression & find significance FOR REGULAR PRICES
regression_model_indexed_log <- lm(price_gas_indexed_log ~ price_phosphorus_indexed_log)
print(regression_model_indexed_log)
summary(regression_model_indexed_log)



#NEW REGRESSION TEST with longer DATASET & TEST FROM HARDAKER
price_gas_1985_2004 <- c(28.08143998,	29.17763998,	20.97719998,	16.85447999,	17.42867999,	21.11543998,	24.86303998,	25.63739998,	24.72767998,	23.88347998,	24.39647998,	24.43139998,	24.65963998,	24.24815998,	23.95295998,	23.70671998,	22.96439998,	23.17319998,	24.48503998,	25.21511998,	24.26939998,	24.15563998,	22.25339998,	22.40063998, 23.81795998,	29.37671998,	35.34047997,	36.35279997,	34.34399997, 30.85199998,	34.45199997,	35.70479997,	33.76439997,	35.92799997,	38.62799997,	42.22799997,	49.53599996,	52.30799996,	58.42799995, 50.54399996


                           
)
price_phosphate_1985_2004 <- c(77.8065,	77.9901,	79.0441,	75.3464,	70.3388,	69.2094,	70.5779, 70.0761,	71.5791,	72.2725,	73.8931,	67.8057,	68.7943,	69.3482,	69.8719,	67.4308,	70.7602,	72.1986,	73.3679,	74.3665,	74.5561,	75.4610,	76.1213,	76.8205,	76.6864,	76.7712,	78.0012,	79.4973,	79.5295,	77.8596,	78.8125,	80.7230,	85.7418,	91.9495,	89.7800,	88.0100,	90.0000,	90.6700,	91.9900,	94.2100

                                  
)
#quick test for regression
regression_1985_2004 <- lm(price_gas_1985_2004~price_phosphate_1985_2004)
print(regression_1985_2004)
summary(regression_1985_2004)

#test for regression as displayed in hardaker chapter 4
#first determine covariance
covariance_gas_phosphate <- cov(price_gas_1985_2004, price_phosphate_1985_2004)
print(covariance_gas_phosphate)
#then determine sqrt of variance of gas * variance of phosphate
variance_gas <- var(price_gas_1985_2004)
print(variance_gas)
variance_phosphate <-var(price_phosphate_1985_2004)
print(variance_phosphate)
#lastly divide cov by the sqrt of both variances multiplied and correlation is obtained
correlation_gas_phosphate <- covariance_gas_phosphate*(variance_gas*variance_phosphate)^-0.5
print(correlation_gas_phosphate)