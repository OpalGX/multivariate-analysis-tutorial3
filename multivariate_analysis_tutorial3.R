setwd()
data <- as.data.frame(read.csv('/Users/gongxue/Documents/R Project/salary.csv'))

#1. Calculate the (sample) means and variances for X1, X2, ???, X16 and Y
lapply(data, mean)
lapply(data, var)

  
#2. Scatter plot the data pairwise using function pairs(???)  in R
pairs(data)

#3. Calculate covariance matrix of X1, X2, ???, X16
cor(data)

#4. Calculate covariance matrix of X1, X2, ???, X16 
cor(data)

#Partition the samples into two equal parts according to the value of Y. 
# Sample fixed number per group
sampleRow <- sample(as.numeric(rownames(data)), floor(0.5*nrow(data)), replace = FALSE)
partition1 <- data[sampleRow,]
partition2 <- data[-sampleRow,]

# Test whether means of X1 in these two parts are equal or not at significance level 5%. 
#Do the same test for each of the other covariates X2, ???, X16.
library(Hotelling)
out = hotelling.test(partition1, partition2)

#5. Estimate the following regression model
#Y = a + b1*X1 + ??? + b16*X16 + 
#  Find the R-square of the regression model, using all the 337 observations.
model <- lm(y ~ ., data = data)
summary(model)$r.squared


#6. Find a linear combination,   with , such that it has the largest value of correlation coefficient with Y. 
#Compare this value with the R-square in 5.
#(Hint:   For part 5, you need to use function lm(???) in R; 
#for part 6, please use the first eigenvalue for the maximization)   
library('Matrix')
vec <- qr.solve(data[, -1], data$y)
normVec <- vec / sqrt(sum(vec^2)) 
fitted <- (as.matrix(data[, -1]) %*%as.vector(vec))
residualSquare <- sum((data$y - fitted)^2)
SSTO <- sum((data$y - mean(data$y))^2)
SSE <- sum((data$y - fitted)^2)
RSquare <- 1- SSE/SSTO
