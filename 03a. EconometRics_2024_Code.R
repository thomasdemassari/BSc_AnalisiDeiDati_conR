# ECONOMETRICS WITH R - EconometRics 
# Owner: Thomas De Massari
# email: thomas.demassari@gmail.com
# LinkedIn: www.linkedin.com/in/thomasdemassari
# GitHub:  https://github.com/thomasdemassari

# During the last semester of my BSc, I attended the Econometrics course taught by Professor Carlo Fezzi 
# at the University of Trento. In class and on my own, I wrote this script to apply the theoretical
# concepts learned during the course. For any questions, please contact me at thomas.demassari@gmail.com

# Main topics:
# - OLS regression
# - Omitted variables bias and 2SLS
# - Panel data analysis
# - Time series analysis

# Table of Contents of the file:
# 1. Libraries
# 2. Monte Carlo Simulation for estimating distribution of OLS betas in a sample of 300 observations
# 3. Omitted Variable Bias
# 4. Demonstration of measurement error on X and Y
# 5. Nonlinear model with piecewise and continuous curve
# 6. An empirical application of a nonlinear relationship analysis
# 7. Demonstration of consistency of HC0 (White Estimator for standard error)
# 8. Two Stage Least Squares
# 9. Demonstration of unbiasedness of OLS with time-invariant omitted variables with panel data
# 10. An empirical application of panel data analysis
# 11. Monte Carlo simulation with time series
# 12. An empirical application of CAPM (Sharpe, 1963) and Portfolio Selection (Markowitz, 1952)

# NB: The datasets used are uploaded on my GitHub page
rm(list=ls())
setwd("/Users/thomasdemassari/Library/CloudStorage/OneDrive-Personal/Università/BSc/Econometria/Datasets/")

# Libraries ----
library(moments)    # Kurtosis and Skewness
library(lmtest)     # HC estimators
library(whitestrap) # White Test
library(wooldridge) # Data of Wooldridge, Introductory Econometrics
library(lmtest)     # White Estimator for s.e.
library(sandwich)   # White Estimator for s.e.
library(car)        # Linear Hypothesis Test
library(AER)        # 2SLS
library(timeSeries) # Returns
library(quantmod)   # Data from Yahoo Finance
library(lubridate)  # To take dates from quantmod data

# Monte Carlo Simulation for estimating distribution of OLS betas in a sample of 300 observations ----
x = runif(10^6)
real_beta = 3
reps = 10^3 # It will be better to use a larger number of repetitions, but it would take too long on my PC...
obs = 300
betas = NULL

for (i in 1:reps){
  x_sample = sample(x, obs)
  y = 2 + (real_beta*x_sample) + 1.5*rnorm(obs)
  ols = lm(y ~ x_sample)
  betas[i] = summary(ols)$coeff[2]
}

hist(betas, main = "Distribution of estimate of beta")
result = cbind(mean(betas), kurtosis(betas)+3, skewness(betas))
colnames(result) = c("Mean", "Kurtosis", "Skewness")
print(result)

# Omitted Variable Bias -----
# The goal is to evaluate the impact of the mother’s cigarette smoking on the baby's birth weight.
# We have cross-sectional data from one year.

# Theoritical model
# List of variables that might have an impact on the baby’s birth weight (y):
# 1. Mother’s habits (alcohol, exercise, diet, etc.)
# 2. Genetics
# 3. Availability of healthcare
# 4. Income
# 5. Pollution
# 6. Baby’s sex
# 7. Gestational duration

# Variables that might be correlated with x: 1, 3, 4, 5, 7
# Our goal is to maintain the exogeneity assumption.

df = read.csv("bweight.csv", sep = ";", header = TRUE)
head(df)
hist(df$bwght)
plot(df$cigs, df$bwght) 

betas_of_cigs = matrix(0, ncol = 1, nrow = 4)                 # Where I will save betas of df$cigs
colnames(betas_of_cigs) = "betas_of_cigs"
row_names = NULL

# Weight and cigs
lm1 = lm(df$bwght ~ I(df$cigs/100))
betas_of_cigs[1] = summary(lm1)$coef[2]
row_names[1] = "w~(100)cig"
# NB: In this case, as we have divide x by 100, beta indicates the average effect of 100 more cigs

# Log-linear model
lm2 = lm(log(df$bwght) ~ df$cigs)
betas_of_cigs[2] = summary(lm2)$coef[2]
row_names[2] = "w~cig(%)"
# NB: In this case, beta indicates the percentage average effect of 1 more cigarette 

# According to the theoretical model, exogeneity assumption is violated, thus the parameters are biased
# Considering the income
cor(df$inc, df$cigs)
lm3 = lm(df$bwght ~ df$cigs + df$inc)
betas_of_cigs[3] = summary(lm3)$coef[2]
row_names[3] = "w~cig + inc"

# Instead of number of cigs smocked, we can use a dummy (smoker or not smoker)
lm4 = lm(df$bwght ~ df$smoker + df$inc)
summary(lm4)
betas_of_cigs[4] = summary(lm4)$coef[2]
row_names[4] = "w~cig_dummy + inc"

# It's strange that R2 increases if we consider the dummy smoker instead of number of cigs, because
# dummy contains less information than number of cigs. It could be due to an measurement error (data
# are self-reported) or due to a non-linear effect.

rownames(betas_of_cigs) = row_names
print(betas_of_cigs)

# Demonstration of measurement error on X and Y ----
result = matrix(NA, 5, 4)
colnames(result) = c("b0", "s.e. b0", "b1", "s.e. b1")
rownames(result) = c("ok", "CEV X", "Fake X", "CEV Y", "Fake Y")

N = 10^4
u = rnorm(N)
x = runif(N, 100, 200)
y = 2 + 3*x + u

# X is correctly reported
lm_ok = lm(y ~ x)
result[1, 1] = coefficients(summary(lm_ok))[1, 1]
result[1, 2] = coefficients(summary(lm_ok))[1, 2]
result[1, 3] = coefficients(summary(lm_ok))[2, 1]
result[1, 4] = coefficients(summary(lm_ok))[2, 2]

# CEV on X
x_cev = x + 200*rnorm(N) 
lm_xcev = lm(y ~ x_cev)
result[2, 1] = coefficients(summary(lm_xcev))[1, 1]
result[2, 2] = coefficients(summary(lm_xcev))[1, 2]
result[2, 3] = coefficients(summary(lm_xcev))[2, 1]
result[2, 4] = coefficients(summary(lm_xcev))[2, 2]

# Fake data on X
possible_error = sample(runif(N, min = 50, max = 250), N/2)
indices_of_fake_x = sample(c(1:N), N/2)
x_fake = x

for (i in 1:length(x)){
  if (i %in% indices_of_fake_x){
    x_fake[i] = x[i] + possible_error[i]
  } 
}
  

lm_xfake = lm(y ~ x_fake)
summary(lm_xfake)
result[3, 1] = coefficients(summary(lm_xfake))[1, 1]
result[3, 2] = coefficients(summary(lm_xfake))[1, 2]
result[3, 3] = coefficients(summary(lm_xfake))[2, 1]
result[3, 4] = coefficients(summary(lm_xfake))[2, 2]

# CEV on Y
y_cev = y + 100*rnorm(N)
lm_ycev = lm(y_cev ~ x)
result[4, 1] = coefficients(summary(lm_ycev))[1, 1]
result[4, 2] = coefficients(summary(lm_ycev))[1, 2]
result[4, 3] = coefficients(summary(lm_ycev))[2, 1]
result[4, 4] = coefficients(summary(lm_ycev))[2, 2]

# Fake data on Y
y_fake = y + 88
lm_yfake = lm(y_fake ~ x)
summary(lm_yfake)
result[5, 1] = coefficients(summary(lm_yfake))[1, 1]
result[5, 2] = coefficients(summary(lm_yfake))[1, 2]
result[5, 3] = coefficients(summary(lm_yfake))[2, 1]
result[5, 4] = coefficients(summary(lm_yfake))[2, 2]

print(result)


# Nonlinear model with piecewise and continuous curve ----
# 01. Piecewise curve
set.seed(226091)
n = 2000
x = runif(n)

# Dummy variable
d = numeric(n)
d[x>0.5] = 1   

y = 1 - 2*x + 3*x*d + 0.1*rnorm(n) - 1.5*d 

plot(x,y)

lm1 = lm(y ~ x*d)
summary(lm1)

# Estimation of the model in the first part of the curve (dummy == 0)
lm1.1 = lm(y[x<=0.5] ~ x[x<=0.5]) 
summary(lm1.1)
abline(coef(lm1.1), col = "red")
# Estimation of the model in the second part of the curve (dummy == 1)
lm1.2 = lm(y[x>0.5] ~ x[x>0.5]) 
summary(lm1.2)
abline(coef(lm1.2), col = "blue")


# 02. Continuous curve
lm2 = lm(y ~ x + I(d*(x-0.5))) 
summary(lm2)
points(x, predict(lm2), col = "green")

# Comparison between models
anova(lm2, lm1)

# An empirical application of a nonlinear relationship analysis ----
df = read.csv("cars.csv")
head(df)

# Explanation of variables:
# 1. MPG = miles per gallon
# 2. cylind = number of cylinders
# 3. disp = engine displacement (100 cubic inches)
# 4. HP = horsepower 
# 5. weight = car weight in 1000lb
# 6. seconds = seconds from 0 to 60
# 7. year	
# 8. origin = 1 is USA, 2 = Europe, 3 = Japan

# Graphical analysis
hist(df$MPG)
boxplot(df$MPG ~ df$year)
boxplot(df$MPG ~ df$origin)

# Estimation of the linear model
# MPG ~ weight
plot(df$weight, df$MPG)
lm.0 = lm(df$MPG ~ df$weight)
summary(lm.0)
abline(lm.0, col = "red")
# This model assume that efficiency is constant, but if we analyszise the residuals we can see
# that it isn't
boxplot(resid(lm.0) ~ df$year)
abline(h = 0, col = "black", lty = "dashed")
# The model underestimate the efficiency in the first part, and overestimate its in the second part

# Including the time, in a linear way
lm.1 = lm(df$MPG ~ df$weight + df$year)
summary(lm.1)
# Including the time, in a nonlinear way (using a n-1 dummy)
lm.2 = lm(df$MPG ~ df$weight + factor(df$year))
summary(lm.2)

# Comparison between models
anova(lm.1, lm.2)
# Refuting the null hypothesis, so the efficiency is not costant over time. 

# Including other variables 
lm.3 = lm(df$MPG ~ df$weight + df$year + df$cylind + df$disp + df$HP + df$seconds)
summary(lm.3)

lm.4 = lm(df$MPG ~ df$weight + df$year + df$cylind + df$disp + df$HP)
summary(lm.4)

lm.5 = lm(df$MPG ~ df$weight + df$year + df$cylind + df$disp)
summary(lm.5)

lm.6 = lm(df$MPG ~ df$weight + df$year + df$cylind)
summary(lm.6)

lm.7 = lm(df$MPG ~ df$weight + df$year)
summary(lm.7)

# Comparison between models
anova(lm.4, lm.3) 
anova(lm.5, lm.4) 
anova(lm.6, lm.5) 
anova(lm.7, lm.6) 
anova(lm.7, lm.3) 
# The best model is lm.7

# Including the time, in a nonlinear way (using a n dummy)
lm.8 = lm(df$MPG ~ df$weight + factor(df$year) - 1)
summary(lm.8)

# Including the origin of cars
# With the intercept
lm.9 = lm(df$MPG ~ df$weight + df$year + factor(df$origin))
summary(lm.9)
# Without the intercept
lm.10 = lm(df$MPG ~ df$weight + df$year + factor(df$origin) - 1)
summary(lm.10)

# In different regions, has the passage of time had varying effects?
lm.11 = lm(df$MPG ~ df$weight + factor(df$origin)*df$year)
summary(lm.11)

anova(lm.10, lm.11)
# Yes, the time had varying effects in different regions

# Considering a nonlinear effect of weigth
# Polynomial model
lm.12 = lm(df$MPG ~ df$weight + I(df$weight^2) + df$year)
summary(lm.12)

plot(df$year, df$weight)
cor(df$weight, df$weight^2)
# Multicollinearity problem 

# Piecewise curve
d25 = rep(0, length(df$year))
d25[df$weight >= 2.5] = 1
lm.13 = lm(df$MPG ~ d25*df$weight + I(df$year-70))
summary(lm.13)

# Graphical representation of nonlinear relationship 
origin = 1
weight = seq(min(df$weight), max(df$weight), 0.01)
year = 75

pred = data.frame(weight, origin, year)
pred$d25 = 0
pred$d25[pred$weight > 2.5] = 1

y_hat = predict(lm.12, newdata = pred, interval = "confidence") 
plot(df$weight, df$MPG)
lines(df$weight, y_hat[,2], type = "l", lty = 2)
lines(df$weight, y_hat[,3], type = "l", lty = 2)

# Demonstration of consistency of HC0 (White Estimator for standard error) ----
set.seed(226091)
n = 500
reps = 10^3 # It will be better to use a larger number of repetitions, but it would take too long on my PC...

betas = numeric(n)
se_ols = numeric(n)
se_white = numeric(n)

for (i in 1:reps){
  x = runif(n, -1, 2)
  u = rnorm(500)
  u = 2*u*(x^2)
  y = 1 + 5*x + u
  
  model = lm(y ~ x)
  betas[i] = coef(model)[2]
  se_ols[i] = summary(model)$coef[2,2]
  se_white[i] = coeftest(model, vcov = vcovHC(model, "HC0"))[2,2]
}

hist(betas)
hist(se_ols)
hist(se_white)

mean(se_ols)
mean(se_white)

sd(betas) / mean(se_ols)                                 # OLS underestimate s.e.
sd(betas) / mean(se_white)                               # White Estimator estimate correctly s.e.

# Two Stage Least Squares ----
data(wage2)                                                                 # Data from Wooldridge
data = wage2

# Descriptive statistics
# Wage
hist(data$wage, main = "Wage")
hist(log(data$wage), main = "Log wage")
# Hourly wage
data$hwage = data$wage / (data$hours * 4.33)                               
hist(log(data$hwage), main = "Hourly wage")
# Education
summary(data$educ)
hist(data$educ)
# Experience
hist(data$exper)

# Scatter plot
plot(data$educ, log(data$hwage))                             # It seems there is not a relationship
boxplot(log(data$hwage) ~ data$educ) 
plot(data$educ, data$exper)  # There is a relationship between education and experience (r = -0.45)
cor(data$educ, data$exper)

# Ability of workers could be an omitted variable (positively correlated with education and hourly wage,
# so models without ability will overestimate the beta)

# ESTIMATION
output = matrix(NA, 7, 2)     # Matrix where I will save the betas of education in different models
colnames(output) = c("coef (*100)", "sd.err (*100)")
rownames(output) = c("log(hwage) ~ edu", "log(hwage) ~ edu + exp", "log(hwage) ~ ...", "with proxy (IQ)", 
                     "White", "No NA", "2SLS")

# Log-linear model without experience
m00 = lm(log(hwage) ~ educ, data = data)
summary(m00)
output[1,] = coef(summary(m00))[2,1:2] * 100
# This model does not consider the work experience. The impact of education on hourly wage is 
# underestimate, as the work experience is positively correlated with hourly wage, but negatively correlated
# with education. 

# Log-linear model with experience
m01 = lm(log(hwage) ~ educ + exper, data = data)
summary(m01)
output[2,] = coef(summary(m01))[2,1:2] * 100
# As expected, b_educ is higher than previous estimate

# Adding more regressors 
m02 = lm(log(hwage) ~ educ + exper + black + tenure + south + urban, data = data)
summary(m02)
output[3,] = coef(summary(m02))[2,1:2]*100

# Testing the hypothesis that one additional educational year has the same impact of one additional
# experience year in the same company.
# H0: b_educ = b_tenure + b_experience 
# NB: tenure is the number of years spend to work in the same company
linearHypothesis(m02, c("educ = exper + tenure"),test="F")
# or, without using linearHypothesis function:
m02r = lm(log(hwage) ~ I(educ + exper) + I(educ + tenure) + black + south + urban, data=data)
anova(m02,m02r)
# We refuse the null hypothesis, so one additional educational year has not the same impact of one 
# additional experience year in the same company

# Using a proxy variable of ability: IQ
# Theoretically, b_educ will be lower because the proxy variable reduce, but not delete, the bias 
# (unless, cor(IQ, ability) == 1, but it is not the case)
m03 = lm(log(hwage) ~ educ + exper + black + tenure + south + urban + IQ, data=data)
summary(m03)
output[4,] <-coef(summary(m03))[2,1:2] * 100

# Testing and correcting for heteroskedasticity 
white = white_test(m03)
# The p-value is very close to 0.05, it would be better to correct for heteroskedasticity
coeftest(m03, vcov = vcovHC(m03, "HC1"))
output[5,] <-coeftest(m03, vcov = vcovHC(m03, "HC1"))[2,1:2] * 100

# 2SLS
# Using feduc (education of father) and meduc (education of mother) as instrument variables 
datas = data[is.na(data$feduc)==0,]
datas = datas[is.na(datas$meduc)==0,]

# Try to estimate again m03 without NA
m03 = lm(log(hwage) ~ educ + exper + black + tenure + south + urban + IQ, data=data)
summary(m03)
output[6,] <-coef(summary(m03))[2,1:2] * 100
# Does not change

# First stage of 2SLS
stage1 = lm(educ ~ exper + black + tenure + south + urban + feduc + meduc, data = datas) 
# Are feduc and meduc weak IV?
stage1r = lm(educ ~ exper + black + tenure + south + urban, data = datas)
anova(stage1, stage1r)
# No, they are not weak IV
# Second stage of 2SLS
datas$educ_hat = predict(stage1)
stage2 = lm(log(hwage) ~ educ_hat + exper + black + tenure + south + urban, data=datas)
summary(stage2)
# In this case (as we have compute "manually" 2SLS) we have to correct the s.e. as R compute these
# referring to datas$educ_hat = predict(stage1), but we know that s.e. are refered to data$educ
# In addition, we note that b_educ is bigger than previous estimates, but we have said the beta is 
# overestimate. The IV are strong, but this discordance could be due to correlation between residuals 
# and IV (income of parents could be correlated with the education of parents)
# So, unless the IVs are strong, they are not good instrument as correlation between them and the
# residuals is not zero. Right now, we cannot find another instrument. 

# Estimating again 2SLS regression using IVREG function
# In this case s.e. compute by R are correct. 
twosls = ivreg(log(hwage) ~  educ + exper + black + tenure + south + urban | 
                 feduc + meduc + exper + black + tenure + south + urban, 
               data = datas) 
summary(twosls)
output[7,] <-coef(summary(twosls))[2,1:2] * 100
# The consideration made above are valid also here

# Demonstration of the unbiasedness of OLS with time-invariant omitted variables in panel data ----
# Theoretical model: y = b0 + (b1)x + (b2)z + e
# where Z is the omitted variable, negatively correlated with X.
set.seed(226091)
N = 6                                                       # Number of units in the panel
t = 4                                                       # Number of observation for each units
id = rep(1:N, each = t)                                     # ID of units

z = rep(runif(N, min = 10, max = 30)*5, each = t)
x = 4*(runif(t*N, min = 10, max = 30)) - z
e = rnorm(N*t)
y = 10 + 3*x + 15*z + 2*e
cor(x, z)

cbind(id, y, x, z)                                          # Panel

estimate = matrix(NA, 3, 2)
colnames(estimate) = c("beta_x", "s.e.")
rownames(estimate) = c("y ~ x + z", "y ~ x", "LSDV")

# OLS with z
ols_yesZ = lm(y ~ x + z)
summary(ols_yesZ)
estimate[1,] = coef(summary(ols_yesZ))[2,1:2]
# OLS is BLUE 

# OLS without z
ols_noZ = lm(y ~ x)
summary(ols_noZ)
estimate[2,] = coef(summary(ols_noZ))[2,1:2]
# OLS is biased

# Plot
plot(x, y)
plot(x, z)
# We can see (and we know) that for each unity the value of z is fixed and, for each unity the 
# relationship between x and y is positive. We can use this feature to solve the problem of 
# omitted variable
lsdv = lm(y ~ x + factor(id))
summary(lsdv)
estimate[3,] = coef(summary(lsdv))[2,1:2]
# The estimate of beta_x is correct. 

# An empirical application of panel data analysis ----
df_l16 = read.csv("farms.csv")
table(df_l16$year)

# Variables:
# 1. year = sampling year
# 2. fid = farm ID
# 3. county, region
# 4. wheat_share = share of farm allocated to wheat
# 5. wheat_price = price index of wheat
# 6. temp / precip = average temperature and precipitation on the farm
# 7. slope = average slope on the farm
# 8. ph = soil ph
# 8. coarse / med / fin = share of soil type

# Research question: the impact of prices and climate on agricultural wheat production in the UK
# Fundamental hypothesis: agricultural firms operate in perfect competition

# We are trying to estimate a production function. This can be estimated using the method:
# - Primal: all inputs are expressed in terms of quantity
# - Dual: all inputs are expressed in terms of price
# We are estimating a dual production function

# Variables that influence prices (and their volatility) x_{jt}:
# 1. Wheat quality {j}
# 2. Global demand and supply. We consider this price variability as exogenous since it is not 
#    determined by the individual farmer in the UK. {jt}
# 3. Distance affecting transportation costs {j}
# 4. Global demand and supply {jt}. We consider this price variability as exogenous since it is not 
#    determined by the individual farmer in the UK.

# Variables affecting y_{jt}:
# 1. Technology {t}
# 2. Policy {jt} (varying in j and in t, because local policies affect only certain farmers)

# Ordering the data
o = order(df_l16$fid)
df_l16_order = df_l16[o,]

# Here, we use wheat_price as an approximation of the production function. We are thus saying that 
# the production function is of the type f(x) = global market wheat price.
# In other words, we use wheat_share as the dependent variable, meaning we estimate the amount of 
# wheat produced based on the percentage of wheat allocated to wheat cultivation.

# Graphical analysis
hist(df_l16$wheat_share)
# From the histogram, we can see that there is a problem of censoring (the share is between 0% and 100%).

plot(df_l16$wheat_price, df_l16$wheat_share)
plot(df_l16$temp, df_l16$wheat_share)

boxplot(df_l16$wheat_price ~ df_l16$year)
# We observe that we have both variability between firms and temporal variability

boxplot(df_l16$wheat_share ~ df_l16$year)
# Small variation over time. The main variation is between units

# Modelling (using from specific to general approach)
output = matrix(NA, 10, 2)
colnames(output) = c("Beta of wheat_price", "Std. err.")
rownames(output) = c("Basic model", "Basic model with temp and precip (^1, ^2, I)", 
                     "Basic model with temp (^1) and precip (^1, ^2)", "All environment features", 
                     "Farm FE (with NA)","Farm FE (correct)", "Country FE", 
                     "Temperature FE", "Farm FE with trend", "Time FE")


# Basic model
m0 = lm(wheat_share ~ wheat_price, data = df_l16)
summary(m0)
output[1,] = coef(summary(m0))[2, 1:2]

# Adding temp and precip, both in linear and in quadratic form
m1 = lm(wheat_share ~ wheat_price + temp + precip + I(precip^2) + I(temp^2) + I(temp*precip), data = df_l16)
summary(m1)
output[2,] = coef(summary(m1))[2, 1:2]
cor(df_l16$temp, I(df_l16$temp^2))
cor(df_l16$prec, I(df_l16$prec^2))
cor(df_l16$temp, df_l16$prec)
# Multicollinenarity problem

# Removing temp^2 and interaction between temp and precip
m2 = lm(wheat_share ~ wheat_price + temp + I(precip^2) + precip, data = df_l16)
summary(m2)
output[3,] = coef(summary(m2))[2, 1:2]
# Multicollinenarity problem persist. but we have lots of observations, so it is necessary a little
# part of variability of X to explain Y

# Including all environment features
m3= lm(wheat_share ~ wheat_price + temp + I(precip^2) + precip + ph + slope + coarse + med + fine,
       data = df_l16)
summary(m3)
output[4,] = coef(summary(m3))[2, 1:2]

# So far we have assumed that quality is constant over time and between farms. Now we assume that 
# quality varies only between farms and not over time, using Fixed Effects model (with LSDV regression)
m4 = lm(wheat_share ~ wheat_price + factor(fid) + temp + precip + I(precip^2) + slope + ph + coarse + 
          med + fine, data = df_l16)
options(max.print = 10000)
summary(m4)
output[5,] = coef(summary(m4))[2,1:2]
# The R2 is now very high (the fixed effects capture all the variability between farms). However, 
# the coefficients of the variables become NA because all time-invariant variables are removed
# and thus cannot be estimated (temperature, precipitation, and all other factors assumed to be constant 
# over time). The price parameter is estimated correctly, though (as well as its standard errors).

# FE estimator correct
m5 = lm(wheat_share ~ factor(fid) + wheat_price, data = df_l16)
summary(m5)
output[6,] = coef(summary(m5))[length(coef(summary(m5))[,1]),1:2]

# Fixed effects can be applied both to individual entities and to groups of entities. For example,
# if we want to estimate the effects of climate, we can include fixed effects for groups of entities
# (e.g., by adding fixed effects for regions such as provinces). By doing this, we assume that the quality
# is the same within each province
# We refer to this as country fixed effects rather than farm fixed effects
m6 = lm(wheat_share ~ factor(county) + wheat_price, data = df_l16)
summary(m6)
output[7,] = coef(summary(m6))[71,1:2]
# We find a result that is intermediate between the two, so we can say that there appears to be some
# variability in quality even within the province

# Temperature FE
m7 = lm(wheat_share ~ wheat_price + factor(temp), data = df_l16)
summary(m7)
output[8,] <-coef(summary(m7))[2,1:2]
# There is a slight reduction in bias compared to the previous model. It seems that we have captured 
# most of the variability in quality, but some bias remains. The choice depends on the analysis we 
# want to perform (whether we want to focus on price, price and climate, or climate)

# Including a trend (in farm FE model)
# Creating the trend
df_l16$trend = df_l16$year - 2005
m8 = lm(wheat_share ~ wheat_price + factor(fid) + trend, data = df_l16)
summary(m8)
output[9,] = coef(summary(m8))[2,1:2]
# We find that the impact of the initial price remains the same, and the impact of the trend is 
# significant and positive. This means that, on average, the wheat share increases by 0.36% each year.

# Time FE
m9 = lm(wheat_share ~ wheat_price + factor(year), data = df_l16)
summary(m9)
output[10,] = coef(summary(m9))[2,1:2]

# Monte Carlo simulation with time series ----
set.seed(226091)
t = 100

# White noise
WN = rnorm(t)
plot(WN, type ="l")

# Correlation for Two Nonstationary Processes in Levels and First Differences
R = 100
y = numeric(t)
x = numeric(t)

corlevels = numeric(R)
cordiff = numeric(R)

for(i in 1:R) {
  # y
  WN = rnorm(t)
  y[1] = WN[1]
  for (t in 2:t) {
    y[t] <- y[t-1] + WN[t]
  }
  
  # x
  WN = rnorm(t)
  x[1] = WN[1]
  for (t in 2:t) {
    x[t] = x[t-1] + WN[t]
  }
  
  # First difference
  dx = x - c(NA,x[-t])
  dy = y - c(NA,y[-t])
  
  # Saving the correlations
  cordiff[i] = cor(dx[-1],dy[-1])
  corlevels[i] = cor(x,y)
}

hist(corlevels)
summary(corlevels)
hist(cordiff)
summary(cordiff)

# Bias Estimation in AR(1) coefficients
rm(list=ls())
set.seed(226091)
R = 1000
t = 100

beta = numeric(R)

times = c(10,20,30,40,50,75,100,150,200,500) 
bias = numeric(length(times))
j = 1
phi = 0.9

for(t in times) {
  for(i in 1:R) {
    y = numeric(t)
    WN = rnorm(t)
    y[1] = WN[1]
    for (t in 2:t) {
      y[t] = phi*y[t-1] + WN[t]
    }
    
    lagy = c(NA,y[-t])
    beta[i] = coef(lm(y ~ lagy))[2]
  }
  
  bias[j] = mean(beta) - phi	
  j = j+1
}

hist(beta)
summary(beta)

plot(times, bias, type ="b")

# An empirical application of CAPM (Sharpe, 1963) and Portfolio Selection (Markowitz, 1952) -----
# The data of stocks are downloaded through the package "quantmod"
# SP500
sp500 = new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo", from = as.Date("2013-01-02"), to = as.Date("2023-12-31"))
sp500_data = sp500$GSPC
# AAPL
aapl = new.env()
getSymbols("AAPL", env = aapl, src = "yahoo", from = as.Date("2013-01-02"), to = as.Date("2023-12-31"))
aapl_data = aapl$AAPL
# BA
ba = new.env()
getSymbols("BA", env = ba, src = "yahoo", from = as.Date("2013-01-02"), to = as.Date("2023-12-31"))
ba_data = ba$BA
# XOM
xom = new.env()
getSymbols("XOM", env = xom, src = "yahoo", from = as.Date("2013-01-02"), to = as.Date("2023-12-31"))
xom_data = xom$XOM

data = data.frame(sp500_data$GSPC.Close, aapl_data$AAPL.Close, ba_data$BA.Close, xom_data$XOM.Close)
colnames(data) = c("sp500", "aapl", "ba", "xom")

dates = index(sp500_data)
month = month(dates)
data$month = month

# GRAPHICAL ANALYSIS
# APPL
# Prices
plot(data$aapl, ylab ="Apple price ($)", main = "Apple price", type = "l")
chartSeries(aapl_data)
# Distribution
hist(data$aapl, ylab ="Apple price ($)", main = "Apple price")
kurtosis(data$aapl) + 3
skewness(data$aapl)
# (P)ACF
acf(data$aapl)
pacf(data$aapl)
# AR(1) with a trend
data$trend = 1:nrow(data)
arima(data$aapl, c(1,0,0), xreg = data$trend)

# Returns
r_aapl = returns(data$aapl)
plot(r_aapl, ylab ="Apple return (%)", main = "Apple return", type = "l")
# Distribution
hist(r_aapl, ylab ="Apple return (%)", main = "Apple return")
skewness(as.numeric(r_aapl), na.rm = TRUE)
kurtosis(as.numeric(r_aapl), na.rm = TRUE) + 3
# AR(1) with trend
arima(r_aapl, c(1,0,0), xreg = data$trend)

# BA
# Prices
plot(data$ba, ylab ="Boeing price ($)", main = "Boeing price", type = "l")
chartSeries(ba_data)
# Distribution
hist(data$ba, ylab = "Boeing price ($)",  main = "Boeing price")
kurtosis(data$ba) + 3
skewness(data$ba)
# (P)ACF
acf(data$ba)
pacf(data$ba)
# AR(1) with a trend
arima(data$ba, c(1,0,0), xreg = data$trend)

# Returns
r_ba = returns(data$ba)
plot(r_ba, ylab = "Boeing return (%)", main = "Boeing return", type = "l")
# Distribution
hist(r_ba, ylab = "Boeing return (%)", main = "Boeing return")
skewness(as.numeric(r_ba), na.rm = TRUE)
kurtosis(as.numeric(r_ba), na.rm = TRUE) + 3
# AR(1) with trend
arima(r_ba, c(1,0,0), xreg = data$trend)

# XOM
# Prices
plot(data$xom, ylab ="Exxon Mobil price ($)", main = "Exxon Mobil price", type = "l")
chartSeries(xom_data)
# Distribution
hist(data$xom, ylab = "Exxon Mobil price ($)",  main = "Exxon Mobil price")
kurtosis(data$xom) + 3
skewness(data$xom)
# (P)ACF
acf(data$xom)
pacf(data$xom)
# AR(1) with a trend
arima(data$xom, c(1,0,0), xreg = data$trend)

# Returns
r_xom = returns(data$xom)
plot(r_xom, ylab = "Exxon Mobil return (%)", main = "Exxon Mobil return", type = "l")
# Distribution
hist(r_xom, ylab = "Exxon Mobil return (%)", main = "Exxon Mobil return")
skewness(as.numeric(r_xom), na.rm = TRUE)
kurtosis(as.numeric(r_xom), na.rm = TRUE) + 3
# AR(1) with trend
arima(r_xom, c(1,0,0), xreg = data$trend)


# SP500
# Prices
plot(data$sp500, ylab ="SP500 price ($)", main = "SP500 price", type = "l")
chartSeries(sp500_data)
# Distribution
hist(data$sp500, ylab = "SP500 price ($)",  main = "SP500 price")
kurtosis(data$sp500) + 3
skewness(data$sp500)
# (P)ACF
acf(data$sp500)
pacf(data$sp500)
# AR(1) with a trend
arima(data$sp500, c(1,0,0), xreg = data$trend)

# Returns
r_sp500 = returns(data$sp500)
plot(r_sp500, ylab = "SP500 return (%)", main = "SP500 return", type = "l")
# Distribution
hist(r_sp500, ylab = "SP500 return (%)", main = "SP500 return")
skewness(as.numeric(r_sp500), na.rm = TRUE)
kurtosis(as.numeric(r_sp500), na.rm = TRUE) + 3
# AR(1) with trend
arima(r_ba, c(1,0,0), xreg = data$trend)


# MARKET MODEL
market_model_aapl = lm(r_aapl ~ r_sp500, data = data)
summary(market_model_aapl)
# beta_aapl = 1.1659046

market_model_ba = lm(r_ba ~ r_sp500, data = data)
summary(market_model_ba)
# beta_ba = 1.3789165

market_model_xom = lm(r_xom ~ r_sp500, data = data)
summary(market_model_xom)
# beta_xom = 0.8962688

# Does the beta vary across different quarters?
# Creating the filters 
data$Q = 1
data$Q[data$month == 4 | data$month== 5 | data$month== 6] = 2
data$Q[data$month== 7 | data$month== 8 | data$month== 9] = 3
data$Q[data$month== 10 | data$month== 11 | data$month== 12] = 4

beta_quaterly_aapl = lm(r_aapl ~ r_sp500*factor(Q), data = data)
summary(beta_quaterly_aapl)
# Yes, the beta of Apple vary across different quarters (3rd and 4th)

beta_quaterly_ba = lm(r_ba ~ r_sp500*factor(Q), data = data)
summary(beta_quaterly_ba)
# Yes, the beta of Boeing vary across different quarters (3rd and 4th)

beta_quaterly_xom = lm(r_xom ~ r_sp500*factor(Q), data = data)
summary(beta_quaterly_xom)
# No, the beta of Exxon Mobil does not vary across different quarters


# Portfolio Selection (Markowitz 1962) with XOM and BA
# NB: I wrote a more general function to implement Portfolio Selection in Python, available in 
# the Project section of my LinkedIn profile in the file "Some fun Python scripts involving XOM 
# and JPM stocks".
weigths_ba = NULL
weigths_xom = NULL
portfolio_returns = NULL
portfolio_sds = NULL

potential_weigths = seq(0, 1, by = 0.001)

for (i in 1:1000){
  weigth_ba_tmp = sample(potential_weigths, 1)
  weigth_xom_tmp = 1 - weigth_ba_tmp
  
  r_ba_clean = na.omit(r_ba)
  r_xom_clean = na.omit(r_xom)
  
  portfolio_return_tmp = (mean(r_ba_clean)*weigth_ba_tmp) + (mean(r_xom_clean)*weigth_xom_tmp)
  portfolio_sd_tmp = cov(r_ba_clean, r_xom_clean)*weigth_ba_tmp*weigth_xom_tmp
  
  weigths_ba[i] = round(weigth_ba_tmp, 5) 
  weigths_xom[i] = round(weigth_xom_tmp, 5)
  portfolio_returns[i] = portfolio_return_tmp
  portfolio_sds[i] = sqrt(weigth_ba_tmp^2 * var(r_ba_clean) + weigth_xom_tmp^2 * var(r_xom_clean) 
                          + 2 * weigth_ba_tmp * weigth_xom_tmp * cov(r_ba_clean, r_xom_clean))
}
# Dataframe
markowitz = data.frame(weigths_ba, weigths_xom, portfolio_returns, portfolio_sds)

# Plot
sd = markowitz$portfolio_sds
r = markowitz$portfolio_returns
plot(sd, r, main = "Portfolios with BA and XOM", xlab = "Portfolio standard deviation (%)", 
     ylab = "Portfolio return (%)")

abline(h = markowitz$portfolio_returns[which.min(markowitz$portfolio_sds)], col = "black", lty = 2) 

min_sd_y = markowitz$portfolio_returns[which.min(markowitz$portfolio_sds)]
three_quarters_sd = 0.75 * max(markowitz$portfolio_sds)

text(x = three_quarters_sd, y = min_sd_y, labels = "Efficient Portfolios", pos = 3, 
     col = "black")
text(x = three_quarters_sd, y = min_sd_y, labels = "Inefficient Portfolios", pos = 1, 
     col = "black")

# Ordering the dataframe
markowitz = markowitz[order(markowitz$weigths_ba), ]
markowitz = markowitz*100
colnames(markowitz) = c("weight_BA", "weigh_XOM", "return", "sd")
