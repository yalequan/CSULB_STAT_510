# Import dataset
library(readr)
HW3_Data = read_csv("Documents/School/CSULB/CSULB - M.A.S./STAT 510 - Regression/HW_3/HW3_Data.csv")

# Create Matrix
X_Matrix = cbind(HW3_Data$X_1, HW3_Data$X_2, HW3_Data$X_3)
Y_Matrix = cbind(HW3_Data$Y)

# View Matrices
X_Matrix
Y_Matrix

# Find Beta_Hat Least Square Estimator
Beta_Hat = solve(t(X_Matrix)%*%X_Matrix)%*%t(X_Matrix)%*%Y_Matrix

# View Beta_Hat Least Square Estimator
Beta_Hat

# Solving for S Squared the estimate of Sigma Squared
n = length(HW3_Data$X_1)
k = 2
S_Squared = (t(Y_Matrix-X_Matrix%*%Beta_Hat)%*%(Y_Matrix-X_Matrix%*%Beta_Hat))/(n-k-1)

# View S_Squared
S_Squared

# Find variance-covariance Matrix of Beta_Hat
Beta_Cov_Matrix = as.numeric(S_Squared[1]) * solve(t(X_Matrix)%*%X_Matrix)

# View the Cov Matrix
Beta_Cov_Matrix

# Calculate Hat Matrix
Hat_Matrix = X_Matrix %*% solve(t(X_Matrix)%*%X_Matrix) %*% t(X_Matrix)

# View Hat Matrix
Hat_Matrix

# Covariance of Fitted Values
Fitted_Cov_Matrix = as.numeric(S_Squared[1]) * Hat_Matrix

# View Covariance of Fitted Values
Fitted_Cov_Matrix

# Varince Covariance Matrix of Residuals
Residual_Cov_Matrix = as.numeric(S_Squared[1]) * (diag(9) - Hat_Matrix)

# View Residual Covariance Matrix
Residual_Cov_Matrix