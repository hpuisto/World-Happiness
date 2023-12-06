##########################################################
# HarvardX PH125.9x Data Science: Capstone
# Project 2: World Happiness
# Author: Hannah Puisto
# Date: December 2023
# Github: https://github.com/hpuisto/World-Happiness/
##########################################################

##########################################################
# Import data and create datasets
#
# World Happiness Ranks dataset found here:
# Data downloaded for 2016-2019 from: https://www.kaggle.com/unsdsn/world-happiness
##########################################################

# Import libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(DescTools)
library(data.table)
library(readxl)

# Set your computer file location here for where you stored the files
setwd("C:/EdX/World Happiness")

# Import datasets
data15 <- read.csv("wh2015.csv")
data16 <- read.csv("wh2016.csv")
data17 <- read.csv("wh2017.csv")
data18 <- read.csv("wh2018.csv")
data19 <- read.csv("wh2019.csv")

#Make all columns in each dataset have the same names
data15 <- data15 %>% 
  rename(
    rank = Happiness.Rank,
    country = Country,
    score = Happiness.Score,
    gdp_per_capita = Economy..GDP.per.Capita.,
    social_support = Family,
    life_exp = Health..Life.Expectancy.,
    freedom = Freedom,
    gov_trust = Trust..Government.Corruption.,
    generosity = Generosity,
    dyst_res = Dystopia.Residual
  )
data16 <- data16 %>% 
  rename(
    rank = Happiness.Rank,
    country = Country,
    score = Happiness.Score,
    gdp_per_capita = Economy..GDP.per.Capita.,
    social_support = Family,
    life_exp = Health..Life.Expectancy.,
    freedom = Freedom,
    gov_trust = Trust..Government.Corruption.,
    generosity = Generosity,
    dyst_res = Dystopia.Residual
  )
data17 <- data17 %>% 
  rename(
    rank = Happiness.Rank,
    country = Country,
    score = Happiness.Score,
    gdp_per_capita = Economy..GDP.per.Capita.,
    social_support = Family,
    life_exp = Health..Life.Expectancy.,
    freedom = Freedom,
    gov_trust = Trust..Government.Corruption.,
    generosity = Generosity,
    dyst_res = Dystopia.Residual
  )
data18 <- data18 %>% 
  rename(
    rank = Overall.rank,
    country = Country.or.region,
    score = Score,
    gdp_per_capita = GDP.per.capita,
    social_support = Social.support,
    life_exp = Healthy.life.expectancy,
    freedom = Freedom.to.make.life.choices,
    gov_trust = Perceptions.of.corruption,
    generosity = Generosity
  )
data19 <- data19 %>% 
  rename(
    rank = Overall.rank,
    country = Country.or.region,
    score = Score,
    gdp_per_capita = GDP.per.capita,
    social_support = Social.support,
    life_exp = Healthy.life.expectancy,
    freedom = Freedom.to.make.life.choices,
    gov_trust = Perceptions.of.corruption,
    generosity = Generosity
  )

# Add a column to designate the year
data15['year'] = 2015
data16['year'] = 2016
data17['year'] = 2017
data18['year'] = 2018
data19['year'] = 2019

# Only keep specific columns from each dataset so all have the same variables
final_data15 = subset(data15, select = -c(Region, Standard.Error, dyst_res) )
final_data16 = subset(data16, select = -c(Region, Lower.Confidence.Interval, Upper.Confidence.Interval, dyst_res) )
final_data17 = subset(data17, select = -c(Whisker.high, Whisker.low, dyst_res) )
final_data18 <- data18
final_data19 <- data19

# Fix 2018 file so the perceptions of corruption variable is numeric
final_data18$gov_trust = as.numeric(final_data18$gov_trust)

# Create one large dataset
full_data <-  rbind(final_data15, final_data16, final_data17, final_data18, final_data19)

##########################################################
# Initial data exploration
##########################################################

# The RMSE function that will be used in this project is:
RMSE <- function(true_score, predicted_score, na.rm = TRUE) {
  sqrt(mean((true_score - predicted_score)^2, na.rm = na.rm))
}

##########################################################
# Number of columns and rows in large dataset
dim(full_data)

# Show the structure of the training dataset
str(full_data)

# Show the descriptive statistics of each category
summary(full_data)

# Create histograms of scores for each year’s data and the full combined list
hist_15 <- hist(final_data15$score, freq=TRUE, col="black", border="white", 
                main="2015 Happiness Scores", xlab="Score", ylab="Count")
hist_16 <- hist(final_data16$score, freq=TRUE, col="black", border="white", 
                main="2016 Happiness Scores", xlab="Score", ylab="Count")
hist_17 <- hist(final_data17$score, freq=TRUE, col="black", border="white", 
                main="2017 Happiness Scores", xlab="Score", ylab="Count")
hist_18 <- hist(final_data18$score, freq=TRUE, col="black", border="white", 
                main="2018 Happiness Scores", xlab="Score", ylab="Count")
hist_19 <- hist(final_data19$score, freq=TRUE, col="black", border="white", 
                main="2019 Happiness Scores", xlab="Score", ylab="Count")
hist_all <- hist(full_data$score, freq=TRUE, col="black", border="white", 
                 main="Happiness Scores for All Years", xlab="Score", ylab="Count")

##########################################################
# Explore the correlation between the predictors and happiness score with scatterplots
# Plot gdp_per_capita vs score
ggplot(data = full_data, aes(x = score, y = gdp_per_capita)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# Plot social_support vs score
ggplot(data = full_data, aes(x = score, y = social_support)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# Plot life_exp vs score
ggplot(data = full_data, aes(x = score, y = life_exp)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# Plot freedom vs score
ggplot(data = full_data, aes(x = score, y = freedom)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# Plot gov_trust vs score
ggplot(data = full_data, aes(x = score, y = gov_trust)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# Plot generosity vs score
ggplot(data = full_data, aes(x = score, y = generosity)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# Create training and testing datasets using p=0.80
train_index <- createDataPartition(full_data$score, times=1, p=0.80, list=FALSE)
train <- full_data[train_index,]
test <- full_data[-train_index,]


##########################################################
##########################################################
# Begin Analysis
##########################################################
##########################################################

##########################################################
# Method 1: Add six key variables together to predict scores, including dystopia constant
##########################################################
# Predict score by sum method
sumvar_model <- full_data %>%
  mutate(pred_score = gdp_per_capita + social_support + life_exp + freedom + gov_trust + generosity + 1.85,
         RMSE = RMSE(score, pred_score))

# Calculate the RMSE
sum_rmse = RMSE(sumvar_model$score, sumvar_model$pred_score)

# Remove generosity since little to no correlation to score
# Predict score by sum method without generosity
sumvar_nogen_model <- full_data %>% mutate(pred_score = gdp_per_capita + social_support + life_exp + freedom + gov_trust + 1.85,
                                       RMSE = RMSE(score, pred_score))

# Calculate the RMSE
sum_nogen_rmse = RMSE(sumvar_nogen_model$score, sumvar_nogen_model$pred_score)

##########################################################
# Method 2: Use the Generalized Linear Model
##########################################################
# Predict score using GLM
data_fit <- glm(score ~ gdp_per_capita + social_support + life_exp + freedom + gov_trust + generosity,
                data = train)

# Add predicted scores to test data frame
results <- test %>% mutate(pred_score = as.numeric(predict(data_fit, test, type = "response")))

results <- test %>% 
  mutate(pred_score = as.numeric(predict.glm(data_fit, newdata=test)),
         RMSE = RMSE(score, pred_score))

# Calculate the RMSE
glm_rmse = RMSE(results$score, results$pred_score)

# Plot predicted scores vs actual scores with x=y line
ggplot(data = results, aes(score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='blue')

# print coefficients of fitted model
data_fit$coefficients

# Remove generosity since little to no correlation to score
# Predict score using GLM
data_fit_nogen <- glm(score ~ gdp_per_capita + social_support + life_exp + freedom + gov_trust, 
                      data = train)

# Add predicted scores to test data frame
results_nogen <- test %>% 
  mutate(pred_score = as.numeric(predict.glm(data_fit_nogen, newdata=test)),
         RMSE = RMSE(score, pred_score))

# Calculate the RMSE
glm_nogen_rmse = RMSE(results_nogen$score, results_nogen$pred_score)

# plot predicted scores vs actual scores
# also plot 1 to 1 line
ggplot(data = results_nogen, aes(score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='blue')

# print coefficients of fitted model
data_fit_nogen$coefficients

##########################################################
##########################################################
# Begin Results
##########################################################
##########################################################
# Print all RMSEs to choose best one
sum_rmse
sum_nogen_rmse
glm_rmse
glm_nogen_rmse
