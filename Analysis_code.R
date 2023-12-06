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
RMSE <- function(true_score = NULL, predicted_score = NULL) {
  sqrt(mean((true_score - predicted_score)^2))
}

##########################################################
# Number of columns and rows in training and testing datasets
dim(full_data)

# Show an organized section of training dataset
glimpse(full_data)

# Show the structure of the training dataset
str(full_data)

# Show the descriptive statistics of each category
summary(full_data)

# Create histograms of scores for each year’s data and the full combined list
hist_scores15 <- final_data15 %>%
  count(country) %>%
  ggplot(aes(n)) +
  geom_histogram(fill = 'black') +
  labs(
    title = '2015 Happiness Scores',
    x = 'Score', y = 'Count', fill = element_blank()
  ) +
  theme_classic()
hist_scores15

hist_scores16 <- final_data16 %>%
  count(country) %>%
  ggplot(aes(n)) +
  geom_histogram(fill = 'black') +
  labs(
    title = '2016 Happiness Scores',
    x = 'Score', y = 'Count', fill = element_blank()
  ) +
  theme_classic()
hist_scores16

hist_scores17 <- final_data17 %>%
  count(country) %>%
  ggplot(aes(n)) +
  geom_histogram(fill = 'black') +
  labs(
    title = '2017 Happiness Scores',
    x = 'Score', y = 'Count', fill = element_blank()
  ) +
  theme_classic()
hist_scores17

hist_scores18 <- final_data18 %>%
  count(country) %>%
  ggplot(aes(n)) +
  geom_histogram(fill = 'black') +
  labs(
    title = '2018 Happiness Scores',
    x = 'Score', y = 'Count', fill = element_blank()
  ) +
  theme_classic()
hist_scores18

hist_scores19 <- final_data19 %>%
  count(country) %>%
  ggplot(aes(n)) +
  geom_histogram(fill = 'black') +
  labs(
    title = '2019 Happiness Scores',
    x = 'Score', y = 'Count', fill = element_blank()
  ) +
  theme_classic()
hist_scores19

hist_scores <- full_data %>%
  count(country) %>%
  ggplot(aes(n)) +
  geom_histogram(fill = 'black') +
  labs(
    title = 'Happiness Scores for All Years',
    x = 'Score', y = 'Count', fill = element_blank()
  ) +
  theme_classic()
hist_scores
