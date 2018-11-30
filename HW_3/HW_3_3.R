# Identify outliers using Cooks Distance

library(readr)
INFECTION <- read_csv("INFECTION.csv")

Rate = INFECTION$RATE
Estimation = INFECTION$EST

quad.fit = lm(Rate~Estimation + I(Estimation^2))
summary(quad.fit)

n = length(Rate)
errors = quad.fit$residuals             # Get errors from model
hi = lm.influence(quad.fit)$hat         # Calculate hi for Cooks Distance
k = 2                                   # Number of beta excluding beta_0
s = sigma(quad.fit)                     # Standard error of our estimate
MSE = s^2                               # Calculate Mean Squared Error
Di<-errors^2*hi/((k+1)*MSE*(1-hi)^2)   # Calculate Cooks Distance 
df1<-k+1                                # Calculate 1st Degree of Freedom
df2<-n-k-1                              # Calculate 2nd Degree of Freedom
F<-qf(0.5,df1,df2)                      # Calculate F 50% value for Cooks


# Build dataframe for Cooks
data.frame(Rate, Estimation, Cook.Distance=Di, F.50thpercentile=F)


plot(cooks.distance(quad.fit))