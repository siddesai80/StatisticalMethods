########################################################
#           Importing all the data files               #
########################################################

library(ggplot2)

X = read.csv("X.csv")
y = read.csv("y.csv")
time = read.csv("time.csv")

# Combining all the objects into a data frame
data = cbind(X, y, time)

# Verifying if the data frame is created appropriately
head(data)

########################################################
#         Task 1: Preliminary data analysis            #
########################################################

## Time series plots (of audio and EEG signals)

# Time series plot of EEG signal 'x1'
ggplot(data, aes(x = time, y = x1)) + 
  geom_line(color = "blue", linewidth = 0.5) + 
  ggtitle("Time series plot of EEG signal 'x1'") +
  xlab("Time(s)") +
  ylab("'x1' Signal(Hz)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

# Time series plot of EEG signal 'x2'
ggplot(data, aes(x = time, y = x2))  +
  geom_line(color = "blue", linewidth = 0.5) + 
  ggtitle("Time series plot of EEG signal 'x2'") +
  xlab("Time(s)") +
  ylab("'x2' Signal(Hz)") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

# Time series plot of sound signal 'y'
ggplot(data, aes(x = time, y = y))  +
  geom_line(color = "black", linewidth = 0.5) + 
  ggtitle("Time series plot of sound signal 'y'") +
  xlab("Time(s)") +
  ylab("Sound signal 'y'(Hz)") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

## Distribution for each signal

# Histogram of EEG signal 'x1'
ggplot(data, aes(x = x1)) + 
  geom_histogram(binwidth = 0.2, fill = "black", color = "white") +
  xlab("EEG signal 'x1'") +
  ylab("Frequency") +
  ggtitle("Histogram of EEG signal 'x1'") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

# Histogram of EEG signal 'x2'
ggplot(data, aes(x = x2)) + 
  geom_histogram(binwidth = 0.2, fill = "black", color = "white") +
  xlab("EEG signal 'x2'") +
  ylab("Frequency") +
  ggtitle("Histogram of EEG signal 'x2'") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

# Histogram of sound signal 'y'
ggplot(data, aes(x = y)) + 
  geom_histogram(binwidth = 4, fill = "black", color = "white") +
  xlab("Sound signal 'y'") +
  ylab("Frequency") +
  ggtitle("Histogram of sound signal 'y'") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

## Correlation and scatter plots (between the audio and brain signals)

# Compute correlation of 'x1' to 'y'
cor_val1 = round(cor(data$x1, data$y), 2)

# Scatter plot between 'x1' brain signal and 'y' sound signal
ggplot(data, aes(x = x1, y = y)) +
  geom_point() +
  xlab("'x1' Signal(Hz)") +
  ylab("Sound signal 'y'(Hz)") +
  ggtitle("Scatter plot between 'x1' brain signal and 'y' sound signal") +
  annotate("text", x = max(data$x1), y = max(data$y), label = paste0("correlation = ", cor_val1), hjust = 3.5, vjust = 1) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

# Compute correlation of 'x2' to 'y'
cor_val2 = round(cor(data$x2, data$y), 2)

# Scatter plot between 'x2' brain signal and 'y' sound signal
ggplot(data, aes(x = x2, y = y)) +
  geom_point() +
  xlab("'x2' Signal(Hz)") +
  ylab("Sound signal 'y'(Hz)") +
  ggtitle("Scatter plot between 'x2' brain signal and 'y' sound signal") +
  annotate("text", x = max(data$x2), y = max(data$y), label = paste0("correlation = ", cor_val2), hjust = 1, vjust = 1) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

#####################################################################################################
#         Task 2: Regression – modelling the relationship between audio and EEG signals             #
#####################################################################################################

# Converting objects into matrix
X = data.matrix(X)
y = data.matrix(y)
time = data.matrix(time)

# Creating ones matrix
ones = matrix(1 , length(X[,1]),1)

## Task 2.1: Estimating model parameters 𝜽={𝜃1,𝜃2,⋯,𝜃𝑏𝑖𝑎𝑠}𝑇 for every candidate model using Least Squares (𝜽̂= (𝐗𝑇𝐗)−1𝐗𝑇𝐲)

# Creating design matrix for each candidate model
model1 = cbind(X[,1]^3, X[,2]^5, ones)
model2 = cbind(X[,1]^4, X[,2]^2, ones)
model3 = cbind(X[,1]^3, X[,2], X[,1], ones)
model4 = cbind(X[,1], X[,1]^2, X[,1]^3, X[,2]^3, ones)
model5 = cbind(X[,1]^3, X[,1]^4, X[,2], ones)

# Estimate model parameters using least squares
thetaHat1 = solve(t(model1) %*% model1) %*% t(model1) %*% y
thetaHat2 = solve(t(model2) %*% model2) %*% t(model2) %*% y
thetaHat3 = solve(t(model3) %*% model3) %*% t(model3) %*% y
thetaHat4 = solve(t(model4) %*% model4) %*% t(model4) %*% y
thetaHat5 = solve(t(model5) %*% model5) %*% t(model5) %*% y

# Print the estimated model parameters for each candidate model

# Model 1
row_names = c("x1^3","x2^5","Bias")
row.names(thetaHat1) = row_names
thetaHat1 = round(thetaHat1, 3)
print(thetaHat1)

# Model 2
row_names = c("x1^4","x2^2","Bias")
row.names(thetaHat2) = row_names
thetaHat2 = round(thetaHat2, 3)
print(thetaHat2)

# Model 3
row_names = c("x1^3","x2","x1","Bias")
row.names(thetaHat3) = row_names
thetaHat3 = round(thetaHat3, 3)
print(thetaHat3)

# Model 4
row_names = c("x1","x1^2","x1^3","x2^3","Bias")
row.names(thetaHat4) = row_names
thetaHat4 = round(thetaHat4, 3)
print(thetaHat4)

# Model 5
row_names = c("x1^3","x1^4","x2","Bias")
row.names(thetaHat5) = row_names
thetaHat5 = round(thetaHat5, 3)
print(thetaHat5)

## Task 2.2: Computing the model residual (error) sum of squared errors (RSS)

# Creating a vector of predicted values
y_Hat1 = model1 %*% thetaHat1
y_Hat2 = model2 %*% thetaHat2
y_Hat3 = model3 %*% thetaHat3
y_Hat4 = model4 %*% thetaHat4
y_Hat5 = model5 %*% thetaHat5

# Computing error or residual between the true response variable values (y) and the predicted values
error1 = y - y_Hat1
error2 = y - y_Hat2
error3 = y - y_Hat3
error4 = y - y_Hat4
error5 = y - y_Hat5

# Calculating the residual sum of squares (RSS)
rss1 = sum(error1^2)
rss2 = sum(error2^2)
rss3 = sum(error3^2)
rss4 = sum(error4^2)
rss5 = sum(error5^2)

# Printing residual sum of squares (RSS)
print(paste("rss1:", toString(round(rss1, 3))))
print(paste("rss2:", toString(round(rss2, 3))))
print(paste("rss3:", toString(round(rss3, 3))))
print(paste("rss4:", toString(round(rss4, 3))))
print(paste("rss5:", toString(round(rss5, 3))))

## Task 2.3: Computing the log-likelihood function for every candidate model

# Calculating the sample variance of x1
sigma_1 = rss1/(length(data$x1)-1)
sigma_2 = rss2/(length(data$x1)-1)
sigma_3 = rss3/(length(data$x1)-1)
sigma_4 = rss4/(length(data$x1)-1)
sigma_5 = rss5/(length(data$x1)-1)

# Calculating log-likelihood
logLik1 = -(length(data$x1)/2) * log(2 * pi) - (length(data$x1)/2) * log(sigma_1) - (1/(2 * sigma_1)) * rss1
logLik2 = -(length(data$x1)/2) * log(2 * pi) - (length(data$x1)/2) * log(sigma_2) - (1/(2 * sigma_2)) * rss2
logLik3 = -(length(data$x1)/2) * log(2 * pi) - (length(data$x1)/2) * log(sigma_3) - (1/(2 * sigma_3)) * rss3
logLik4 = -(length(data$x1)/2) * log(2 * pi) - (length(data$x1)/2) * log(sigma_4) - (1/(2 * sigma_4)) * rss4
logLik5 = -(length(data$x1)/2) * log(2 * pi) - (length(data$x1)/2) * log(sigma_5) - (1/(2 * sigma_5)) * rss5

# Printing sample variance and log-likelihood
print(paste("sigma_1:", toString(round(sigma_1, 3)), ", logLik1:", toString(round(logLik1, 3)), sep = " "))
print(paste("sigma_2:", toString(round(sigma_2, 3)), ", logLik2:", toString(round(logLik2, 3)), sep = " "))
print(paste("sigma_3:", toString(round(sigma_3, 3)), ", logLik3:", toString(round(logLik3, 3)), sep = " "))
print(paste("sigma_4:", toString(round(sigma_4, 3)), ", logLik4:", toString(round(logLik4, 3)), sep = " "))
print(paste("sigma_5:", toString(round(sigma_5, 3)), ", logLik5:", toString(round(logLik5, 3)), sep = " "))

## Task 2.4: Computing the Akaike information criterion (AIC) and Bayesian information criterion (BIC) for every candidate model

# Akaike information criterion (AIC)
aic1 = 2 * length(thetaHat1) - 2 * logLik1
aic2 = 2 * length(thetaHat2) - 2 * logLik2
aic3 = 2 * length(thetaHat3) - 2 * logLik3
aic4 = 2 * length(thetaHat4) - 2 * logLik4
aic5 = 2 * length(thetaHat5) - 2 * logLik5

# Bayesian information criterion (BIC)
bic1 = 3 * log(length(data$x1)) - 2 * logLik1
bic2 = 3 * log(length(data$x1)) - 2 * logLik2
bic3 = 4 * log(length(data$x1)) - 2 * logLik3
bic4 = 5 * log(length(data$x1)) - 2 * logLik4
bic5 = 4 * log(length(data$x1)) - 2 * logLik5

# Printing Akaike information criterion (AIC) and Bayesian information criterion (BIC) values
print(paste("AIC_1:", toString(round(aic1, 3)), ", BIC_1:", toString(round(bic1, 3)), sep = " "))
print(paste("AIC_2:", toString(round(aic2, 3)), ", BIC_2:", toString(round(bic2, 3)), sep = " "))
print(paste("AIC_3:", toString(round(aic3, 3)), ", BIC_3:", toString(round(bic3, 3)), sep = " "))
print(paste("AIC_4:", toString(round(aic4, 3)), ", BIC_4:", toString(round(bic4, 3)), sep = " "))
print(paste("AIC_5:", toString(round(aic5, 3)), ", BIC_5:", toString(round(bic5, 3)), sep = " "))

## Task 2.5: Distribution of model prediction errors (residuals) for each candidate model

# Generate QQ plot with error1 data for Model 1
qqnorm(error1, main = "Q-Q Plot for Model 1") 
# Add a horizontal line indicating the expected values
qqline(error1)

# Generate QQ plot with error2 data for Model 2
qqnorm(error2, main = "Q-Q Plot for Model 2") 
# Add a horizontal line indicating the expected values
qqline(error2)

# Generate QQ plot with error3 data for Model 3
qqnorm(error3, main = "Q-Q Plot for Model 3") 
# Add a horizontal line indicating the expected values
qqline(error3)

# Generate QQ plot with error4 data for Model 4
qqnorm(error4, main = "Q-Q Plot for Model 4") 
# Add a horizontal line indicating the expected values
qqline(error4)

# Generate QQ plot with error5 data for Model 5
qqnorm(error5, main = "Q-Q Plot for Model 5") 
# Add a horizontal line indicating the expected values
qqline(error5)

## Task 2.7

# Reading the data files
X = read.csv("X.csv")
y = read.csv("y.csv")

# Creating a dataframe by binding both the datasets
dataframe = cbind(X,y)

# Create a vector of indices corresponding to each observation in your dataset
row_length = 1:nrow(dataframe)

# Use the sample function to randomly select 70% of the indices for the training set
training_rows = sample(row_length, floor(0.7 * length(row_length)))

# Use the setdiff function to select the remaining 30% of indices for the test set
test_rows = setdiff(row_length, training_rows)

# Use the training_indices and test_indices vectors to create your training and test datasets
training_set = dataframe[training_rows, ]
test_set = dataframe[test_rows, ]

# Printing the dimensions of training_set and test_set
dim(training_set)
dim(test_set)

# Splitting the data into training data set
x1_train = training_set$x1
x2_train = training_set$x2
y_train = training_set$y

# Splitting the data into testing data set
x1_test = test_set$x1
x2_test = test_set$x2
y_test = test_set$y

# Creating ones matrix for training and testing data set
ones_train =  matrix(1 , length(training_rows),1)
ones_test = matrix(1 , length(test_rows),1)

## Task 2.7.1: Estimating model parameters using the training dataset

# Train the model 
train_model = cbind(x1_train^3, x2_train, x1_train, ones_train)
train_thetaHat = solve(t(train_model) %*% train_model) %*% t(train_model) %*% y_train
row_names = c("x1^3","x2","x1","Bias")
row.names(train_thetaHat) = row_names
train_thetaHat

# Testing and predicting the values
test_model = cbind(x1_test^3, x2_test, x1_test, ones_test)
test_thetaHat = solve(t(test_model) %*% test_model) %*% t(test_model) %*% y_test
row_names = c("x1^3","x2","x1","Bias")
row.names(test_thetaHat) = row_names
test_thetaHat

## Task 2.7.2: Computing the model’s output/prediction on the testing data

# Calculating the predicted values
y_hat = test_model %*% train_thetaHat

# Calculating the difference between the actual values and the predicted values
error = y_test - y_hat

# Calculating the sum of squared errors (SSE) between the predicted values and the actual values of the test dataset
sse = norm(error , type = "2")^2

# Printing the value of sum of squared errors (SSE)
print(paste("Sum of Squared Errors (SSE):", toString(round(sse, 3))))

## Task 2.7.3: Computing the 95% (model prediction) confidence intervals and plotting them (with error bars) together with the model prediction, as well as the testing data samples

# Storing number of rows of y_hat
n = nrow(y_hat)

# Computing sample variance
variance = sse/(n-1)

# Computing the inverse of the cross-product
ctheta = (solve(t(test_model) %*% test_model))

# Creating a zero matrix 
zero_hat = matrix(0, n , 1)

# Computing predicted variance
for (i in 1:n) {
  model_data = matrix(train_model[i, ], 1, 4)
  zero_hat[i,1] = model_data %*% ctheta %*% t(model_data)
}

# Calculating the Confidence Interval 
CI = 2 * sqrt(zero_hat)
print(paste("Confidence Interval:", toString(round(CI, 3))))

# Calculating the upper and lower limits of the Confidence Interval
upper = y_hat + CI
lower = y_hat - CI

# Creating data frame combining all the values 
df = data.frame(x = 1:length(y_test), y_test = y_test, y_hat = y_hat, 
                 lower_bound = lower, upper_bound = upper)

# Plotting model prediction with error bars
ggplot(df, aes(x = x)) +
  geom_line(aes(y = y_test)) +
  geom_point(aes(y = y_hat), color = "blue") +
  geom_segment(aes(x = x, y = lower, xend = x, yend = upper), 
               size = 10) +
  ylim(range(c(y_test, y_hat, upper, lower))) +
  ggtitle("95% (model prediction) Confidence Intervals with error bars") +
  xlab("Time(s)") +
  ylab("y prediction") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

###################################################################
#         Task 3: Approximate Bayesian Computation (ABC)          #
###################################################################

## Task 3.1

# Combining X and y objects into a data frame
data = cbind(X,y)

# Extracting values from thetaHat3
theta2 = thetaHat3[2,]
theta3 = thetaHat3[3,]
theta_bias = thetaHat3[4,]

# Task 3.2

# Creating random numbers from a uniform distribution
theta3_prior = runif(1000, min = 0.5 * theta3, max = 1 * theta3)
theta_bias_prior = runif(1000, min = -0.5 * abs(theta_bias), max = 1 * abs(theta_bias))

# Creating empty vectors
accepted_samples_3 = c()
accepted_samples_bias = c()

# Task 3.3

# Compute predicted values and error
for (i in 1:length(theta3_prior)) {
  y_pred = theta3_prior[i] * data$x1 ^ 3 + theta2 * data$x2 + theta3_prior[i] * data$x1 + theta_bias_prior[i]
  pred_error = data$y - y_pred
  rss = sum(pred_error ^ 2)
  if (rss < 300000) {
    accepted_samples_3 = c(accepted_samples_3, theta3_prior[i])
    accepted_samples_bias = c(accepted_samples_bias, theta_bias_prior[i])
  }
}

# Defining a function to removes missing values (NA's) 
remove_NA = function(x) {
  x[!is.na(x)]
}

# Remove missing values from accepted samples vectors
accepted_samples_3 = remove_NA(accepted_samples_3)
accepted_samples_bias = remove_NA(accepted_samples_bias)

# Task 3.4

# Plotting Joint Density of Theta3 and Theta_bias
ggplot(data = data.frame(theta3_prior, theta_bias_prior)) +
  geom_density_2d_filled(mapping = aes(x = theta3_prior, y = theta_bias_prior)) +
  geom_point(mapping = aes(x = theta3_prior, y = theta_bias_prior)) + 
  ggtitle("Joint Density of Theta3 and Theta_bias") +
  xlab("Theta3") +
  ylab("Theta_bias") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

# Create histogram of marginal distribution of theta_3
ggplot(data = data.frame(theta3 = accepted_samples_3)) +
  geom_histogram(mapping = aes(x = theta3), bins = 20, color = "white", fill = "black", alpha = 0.7) +
  xlab("Theta3") +
  ylab("Density") +
  ggtitle("Histogram of Marginal Distribution of Theta3") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))

# Create histogram of marginal distribution of theta_bias
ggplot(data = data.frame(theta_bias = accepted_samples_bias)) +
  geom_histogram(mapping = aes(x = theta_bias), bins = 20, color = "white", fill = "black", alpha = 0.7) +
  xlab("Theta_bias") +
  ylab("Density") +
  ggtitle("Histogram of Marginal Distribution of Theta_bias") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white"))
