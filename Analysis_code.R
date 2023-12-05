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
    score = Happiness.Score,
    gdp_per_capita = Economy..GDP.per.Capita.,
    social_support = Family,
    life_exp = Health..Life.Expectancy.,
    gov_trust = Trust..Government.Corruption.,
    dyst_res = Dystopia.Residual
  )
data16 <- data16 %>% 
  rename(
    score = Happiness.Score,
    gdp_per_capita = Economy..GDP.per.Capita.,
    social_support = Family,
    life_exp = Health..Life.Expectancy.,
    gov_trust = Trust..Government.Corruption.,
    dyst_res = Dystopia.Residual
  )
data17 <- data17 %>% 
  rename(
    score = Happiness.Score,
    gdp_per_capita = Economy..GDP.per.Capita.,
    social_support = Family,
    life_exp = Health..Life.Expectancy.,
    gov_trust = Trust..Government.Corruption.,
    dyst_res = Dystopia.Residual
  )
data18 <- data18 %>% 
  rename(
    country = Country.or.region,
    gdp_per_capita = GDP.per.capita,
    social_support = Social.support,
    life_exp = Healthy.life.expectancy,
    freedom = Freedom.to.make.life.choices,
    gov_trust = Perceptions.of.corruption
  )
data19 <- data19 %>% 
  rename(
    country = Country.or.region,
    gdp_per_capita = GDP.per.capita,
    social_support = Social.support,
    life_exp = Healthy.life.expectancy,
    freedom = Freedom.to.make.life.choices,
    gov_trust = Perceptions.of.corruption
  )

# Add a column to designate the year
data15['year'] = 2015
data16['year'] = 2016
data17['year'] = 2017
data18['year'] = 2018
data19['year'] = 2019

# Only keep specific columns from each dataset so all have the same variables
final_data15 = subset(data15, select = -c(Region, Happiness.Rank, Standard.Error) )
final_data16 = subset(data16, select = -c(Region, Happiness.Rank, Lower.Confidence.Interval, Upper.Confidence.Interval) )
final_data17 = subset(data17, select = -c(Happiness.Rank, Whisker.high, Whisker.low) )
final_data18 = subset(data18, select = -c(Overall.rank) )
final_data19 = subset(data19, select = -c(Overall.rank) )

# Fix 2018 file so the perceptions of corruption variable is numeric
final_data18$gov_trust = as.numeric(final_data18$gov_trust)

# Add dyst_res to 2018 and 2019 files as the difference between the score and the sum of the remaining fields
final_data18 <- final_data18 %>% mutate(dyst_res = (Score-gdp_per_capita-social_support-life_exp-freedom-Generosity-gov_trust))
final_data19 <- final_data19 %>% mutate(dyst_res = (Score-gdp_per_capita-social_support-life_exp-freedom-Generosity-gov_trust))
