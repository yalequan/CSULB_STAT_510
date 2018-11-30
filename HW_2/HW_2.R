# Read in data and assign variables
library(readr)
LungCapData <- read_csv("LungCapData.csv")

y <- LungCapData$LungCap
x <- LungCapData$Age

# Given the LungCap data, using only the first column (LungCap) and the second column (Age), assume that errors are independent and normally distributed
# 1A) Compute the 90% confidence limits for Beta1
# 1B) Compute the 90% confidence limits for Sigma^2

# SLR model & Estimators
alpha<-0.05                            	 	  # Set the significance level of Alpha
n<-length(y)			                          # Determine sample size
lm.model<-lm(y~x)			                      # Fit linear model
se<-sqrt(diag(vcov(lm.model)))          		# Standard errors
y.fitted<-lm.model$fitted.values 		        # Extract the fitted values of y
b0<-lm.model$coefficients[1]            		# Est. Coefficient of Beta0
b1<-lm.model$coefficients[2]            		# Est. Coefficient of Beta1
sig.b0<-se[1]                           		# Beta0 Standard Deviation
sig.b1<-se[2]                          		  # Beta1 Standard Deviation
ssx<-sum(x^2)                           		# Sum of squares 
ssxx<-sum((x-mean(x))^2)                		# Sum of squares of x
sse<-sum((y - y.fitted)^2)  		            # Sum of squares error
mse<-sse/(n - 2)            		            # Mean squared error
sig.hat<-sqrt(mse)                      		# Est. sigma
t.val<-qt(1-alpha/2,n-2)                		# Critical value of t
chisq.left<-qchisq(0.025, n-2) 		          # Critical value of chi-square left
chisq.right<-qchisq(0.975,n-2)		          # Critical value of chi-square right

# Confindence limits for SLR parameters
b0.lower<-b0-t.val*sig.b0               		# Est. lower limit of beta0
b0.upper<-b0+t.val*sig.b0              	 	  # Est. upper limit of beta0
b1.lower<-b1-t.val*sig.b1		                # Est. uower limit of beta1
b1.upper<-b1+t.val*sig.b1               		# Est. upper limit of beta1
sigma.lower<-(n-2)*mse/chisq.right    		  # Est. lower limit of sigma square
sigma.upper<-(n-2)*mse/chisq.left     		  # Est. upper limit of sigma square

# Build data.frame of upper and lower limits calculated above
lower<-data.frame(rbind(b0.lower,b1.lower,sigma.lower))
upper<-data.frame(rbind(b0.upper,b1.upper,sigma.upper))
fit<-data.frame(rbind(b0,b1,sig.hat))

# Collect all into data.frame and rename columns
results <- data.frame(cbind(lower, upper, fit), row.names = c('Beta0', 'Beta1', 'Sigma^2'))
colnames(results)<-c('Lower','Upper','Est.Par')

results                                   # Print Results

# 1C) Perform a size Alpha = .10 test ofr H0: Beta1 = 1 vs HA: Beta1 =/ 1

summary(lm.model)

df <- n-2                                 # Calculate degrees of freedom
t_stat <- (b1 - 1)/(sig.b1)               # Calcuate T_Statistic
p_value <- 2*pt(t_stat, df)               # Calcuate P_Value
p_value                                   # Print p_value

#2A) Find and interpret the values of r and r^2 for the simple linear regression relating the proportion 
# of names recalled (y) and position (order) of the student (x) during the ‘‘name game.’’

# Read in dataset

NAMEGAME2 <- read.delim("~/Documents/School/CSULB/CSULB - M.A.S./STAT 510 - Regression/HW_2/NAMEGAME2.txt")

# Calculate r and r^2 value

Name_Recall <- NAMEGAME2$RECALL
Name_Position <- NAMEGAME2$POSITION

name_fit <- lm( Name_Recall~ Name_Position)
summary(name_fit)

# Find a 99% confidence interval for the mean recall proportion for students in the fifth position during the ‘‘name game.’’ Interpret the result.

new.dat <- data.frame(Name_Position = 5)

predict(name_fit, newdata = new.dat, interval = 'confidence', level = 0.99)
predict(name_fit, newdata = new.dat, interval = 'predict', level = 0.99)
