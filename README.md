# EdX Capstone Project 2
## World-Happiness
This is the second project in the HarvardX Data Science Capstone course. The goal is to choose and use a publicly available dataset to solve the problem of our choice by applying machine learning techniques that go beyond standard linear regression. We are then to clearly communicate the process and insights gained from the analysis in a report. The dataset chosen is about global happiness scores based on societal factors from the World Happiness Report published annually. This project employs regression analysis in order to study the happiness of countries.

The source for the data files is Kaggle: https://www.kaggle.com/datasets/unsdsn/world-happiness/. Only data from 2015 to 2019 is available on this platform. The original data comes from the World Happiness Report website (https://worldhappiness.report/data/), however, the data for 2020-2023 is either formatted differently or is not available. The 2022 file doesn't exists on its own, and the Life Expectancy variable in the 2020, 2021, and 2023 file is the actual age instead of being rated like in the 2015-2019 files. Therefore, we only use data from 2015 to 2019 for consistency.

## Important files in this repository
1. `Analysis_code.R` imports and saves the data we need to perform our analysis, splits it into a "training" set and a "validation" set, and then proceeds with our exploration and analysis. The last portion of this script applies the final model to the "validation" set for a final measure of accuracy.
2. `report.Rmd` is an R-markdown file that generates a final report.
3. `Report.pdf` is the final report generated for this project.
