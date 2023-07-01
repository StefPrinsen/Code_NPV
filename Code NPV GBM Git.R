#import excel file of NPV variables
library(readxl)

p_HTC <- 1367228               # in €
p_precipitation <- 422030      # in €
p_sodium_hydroxide <- 0.509    # € per kg
q_sodium_hydroxide <- 0.00395  # kg
p_magnesium_chloride <- 0.37   # € per kg
q_magnesium_chloride <- 0.07007# kg required per kg of sludge
p_phosphoric_acid <- 1.264     # € per kg
q_phosphoric_acid <- 0.00719   # kg required per kg of sludge
p_oxalate <- 0.927             # € per kg
q_oxalate <-0.0473             # kg required per kg of sludge
p_disposal_saving <-0.245  # € per kg
discount_rate <- 0.03  # %
q_gas <- 0.000988              # in MWH per kg of processed sludge for a temperature of 200 C
#p_gas_simulated               #simulated with GBM
produced_milk <-100000         # tons
milk_to_sludge <- 10           # amount of sludge in kg per ton of milk. 10 kg of sludge per ton of milk
p_content_sludge <-0.057       # kilo of phosphorus per kg of sludge
recovery_rate <-0.946          # in %
#p_phosphorus_simulated        # simulated with GBM
p_recovered <- produced_milk*milk_to_sludge*p_content_sludge*recovery_rate/1000 #ton recovered phosphorus
print(p_recovered)

#gas price data
library(quantmod)
library(zoo)
library(xts)
library(TTR)
p_gas <- getSymbols("TTF=F", src="yahoo", from = "2000-09-01", to = "2023-06-02", auto.assign = FALSE)

#tests for g
library(stats)
library(tseries)

#calculate returns
p_gas_daily_returns <- dailyReturn(p_gas$`TTF=F.Close`, type ="log")
p_gas_daily_returns_regular <- dailyReturn(p_gas$`TTF=F.Close`)
plot(p_gas_daily_returns_regular)

p_gas_average_returns <- mean(p_gas_daily_returns)*252
print(p_gas_average_returns)

#determine drift rate for p_gas -> is drift rate the mean of the daily returns?

#determine volatility for p_gas
p_gas_volatility <-sd(p_gas_daily_returns)*sqrt(252)

print(p_gas_volatility)

#phosphorus price data

column_types <- c("text", "numeric")

p_phosphorus <- read_excel("C:\\Users\\stefp\\Documents\\P_phosphorus_NL.xlsx", sheet = "P_phosphorus_NL", col_names = FALSE)
View(p_phosphorus)

library(stats)

col_names <- as.character(p_phosphorus[1, ])
colnames(p_phosphorus) <- col_names

p_phosphorus_3 <- p_phosphorus[-1, ]
View(p_phosphorus_3)
#transpose 

p_phosphorus_3_T <- t(p_phosphorus_3)
library(data.table)

#plot price path in NL
library(ggplot2)
plot(p_phosphorus_3_T)

lines(p_phosphorus_3_T)

p_phosphorus_3_T<-as.numeric(p_phosphorus_3_T)

#logreturns Phosphorus
p_phosphorus_quarterly_return <- diff(log(p_phosphorus_3_T))
plot(p_phosphorus_quarterly_return)
lines(p_phosphorus_quarterly_return)
print(p_phosphorus_quarterly_return)

p_phosphorus_average_returns <- mean(p_phosphorus_quarterly_return)*4
print(p_phosphorus_average_returns)

p_phosphorus_volatility <- sd(p_phosphorus_quarterly_return)*sqrt(4)
print(p_phosphorus_volatility)

#GBMS IN 1 LOOP
nsim <- 1000
S0_g <- 37.65 # 01-07-2023
mu_g <- p_gas_average_returns
sigma_g <- p_gas_volatility
t <- 15

S0_p <- 500 # 12-06-2023 If i use similar method to eurostat before, price is 640
mu_p <- p_phosphorus_average_returns
sigma_p <- p_phosphorus_volatility
t_p <- 15
correlation <- correlation_gas_phosphate

gbm_g <- matrix(ncol = nsim, nrow = t)
gbm_p <- matrix(ncol = nsim, nrow = t_p)
set.seed(254)

for (simu in 1:nsim) {
  dt_g <- 1 / t
  dt_p <- 1 / t_p
  gbm_g[1, simu] <- S0_g
  gbm_p[1, simu] <- S0_p
  
  epsilon_g <- rnorm(t)
  epsilon_p <- rnorm(t_p)
  
  for (year in 2:t) {
    gbm_g[year, simu] <- exp((mu_g - sigma_g * sigma_g / 2) * dt_g + sigma_g * epsilon_g[year] * sqrt(dt_g))
  }
  
  for (year in 2:t_p) {
    gbm_p[year, simu] <- exp((mu_p - sigma_p * sigma_p / 2) * dt_p + sigma_p * (correlation * sqrt(dt_g) * epsilon_g[year] + sqrt(1 - correlation^2) * sqrt(dt_p) * epsilon_p[year]))
  }
}

gbm_g <- apply(gbm_g, 2, cumprod)
gbm_p <- apply(gbm_p, 2, cumprod)
View(gbm_g)
View(gbm_p)

ts.plot(gbm_g, gpars = list(col = rainbow(10)), ylim = c (0,800), main = "GBM Gas", ylab = "Price in €/MWh", xlab = "Time in years")
ts.plot(gbm_p, gpars = list(col = rainbow(10)), ylim = c(0,800), main = "GBM Phosphorus", ylab = "Price in €/ton", xlab = "Time in years")

gbm_g_t <- t(gbm_g)

gbm_p_t <- t(gbm_p)
View(gbm_p_t)
View(gbm_g_t)

#NPV method 2

cashflows <- matrix(0, nrow = nsim, ncol = t)
for (i in 1:nsim) {
  for (j in 1:t) {
    # Calculate cashflow
    cashflows <- gbm_p_t*p_recovered + milk_to_sludge*produced_milk*p_disposal_saving - gbm_g_t*q_gas*produced_milk*milk_to_sludge - produced_milk*milk_to_sludge*p_sodium_hydroxide*q_sodium_hydroxide - produced_milk*milk_to_sludge*q_magnesium_chloride*p_magnesium_chloride - produced_milk*milk_to_sludge*p_phosphoric_acid*q_phosphoric_acid-produced_milk*milk_to_sludge*p_oxalate*q_oxalate
    
  }
}
p_revenue<-gbm_p_t*p_recovered
View(p_revenue)
costs_of_gas<-gbm_g_t*q_gas*produced_milk*milk_to_sludge
View(costs_of_gas)

View(cashflows)
# Calculate NPV for each simulation
npv <- numeric(nsim)
for (i in 1:nsim) {
  npv[i] <- sum(cashflows[i, ] / (1 + discount_rate)^(1:t)) - p_HTC - p_precipitation
}


# Print npv
print(sd(npv))
variance_NPV<-var(npv)
print(sd(npv)^2)
print(min(npv))
print(max(npv))
print(mean(npv))

install.packages(scales)
library(scales)

# Plot the probability density function of NPV
density <- density(npv)

# Plot the PDF
ggplot(data.frame(x = density$x, y = density$y), aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(data = subset(data.frame(x = density$x, y = density$y), x < 0),
              aes(ymax = y, ymin = 0), fill = "red", alpha = 0.5) + geom_ribbon(data = subset(data.frame(x = density$x, y = density$y), x > 0),
                                                                                aes(ymax = y, ymin = 0), fill = "green", alpha = 0.5) +
  labs(x = "Net Present Value", y = "Probability Density") +
  ggtitle("Probability Density Function of Net Present Value") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) 
  
# Start a loop to compute certainty equivalents for all risk aversion coefficients
for(r in c(-1e-01, -1e-02, -1e-03, -1e-04, -1e-05, -1e-06, 0,
           1e-06,  1e-05, 1e-04, 1e-03, 1e-02, 1e-01)){
  Certainty_equivalent_NPV <- mean(npv) - variance_NPV * r/2
  assign(paste("CE_NPV", r, sep = ""), Certainty_equivalent_NPV)
  
}

Certainty_equivalent_NPV <- c(`CE_NPV-0.1`,`CE_NPV-0.01`, `CE_NPV-0.001`, `CE_NPV-1e-04`,
                              `CE_NPV-1e-05`, `CE_NPV-1e-06`,`CE_NPV0`, `CE_NPV1e-06`,
                              `CE_NPV1e-05`, `CE_NPV1e-04`, `CE_NPV0.001`, `CE_NPV0.01`, `CE_NPV0.1`
                              )

risk_aversion_coefficient <- c(-1e-01,-1e-02, -1e-03, -1e-04, -1e-05, -1e-06, 0, 1e-06,
                               1e-05, 1e-04, 1e-03, 1e-02, 1e-01)
print(Certainty_equivalent_NPV)

as.numeric(Certainty_equivalent_NPV)

plot(risk_aversion_coefficient, Certainty_equivalent_NPV)
lines(risk_aversion_coefficient, Certainty_equivalent_NPV)
plot(risk_aversion_coefficient, Certainty_equivalent_NPV, log = "y", type = "o", xlab = "Risk Aversion Coefficient", ylab = "Certainty Equivalent NPV", main = "Logarithmic Scale Plot")

plot(Certainty_equivalent_NPV)
lines(Certainty_equivalent_NPV)
#see what percentage of outcomes is positive
percentage_above_zero <- 100 * sum(npv > 0) / length(npv)

print(percentage_above_zero)



