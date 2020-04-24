## Table 1 in Thorndike, 1904
library(pastecs)
library(stringr)
setwd("/Users/blakefinnegan/Documents/Programming/R/Edward Thorndike - An Introduction to the Theory of Mental and Social Measurements/")

data <- read.csv("table_1_spelling.csv")
descriptives <- data.frame(stat.desc(data))


# Non-significant difference between boys and girls
boxplot(data$boys, data$girls, main="Boys and Girls Spelling Errors", xlab="Boy, Girl", ylab="Errors")
t.test(data$boys, data$girls) 

# Calculatings a few new variables
data$word <- as.character(data$word)
word_length <- str_length(data$word)
data$word_length <- word_length
data$mean_errors <- (data$girls + data$boys) / 2
data$error_deviation <- data$mean_errors - mean(data$mean_errors)
data$squared_deviation <- data$error_deviation^2

## Fake Regression Assumption Check
random = rchisq(nrow(data), 7)
fake = lm(random~mean_errors+word_length, data=data)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

## Linearity
qqnorm(standardized)
abline(0,1, col = "red")

## Normality
hist(standardized)

## Homogeneity
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

# Positive Relationship between Word Length and Mean Errors
model_1 <- lm(mean_errors ~ word_length, data)
summary(model_1)
plot(data$word_length, data$mean_errors)
abline(model_1, col = "red")

# Adding Regression Lines for Girls and Boys
plot(data$word_length, data$mean_errors)
abline(model_1, col = "red")
abline(lm(mean_errors ~ girls, data), col = "blue")
abline(lm(mean_errors ~ boys, data), col = "green")
