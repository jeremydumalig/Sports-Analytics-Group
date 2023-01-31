# Set working directory
setwd("/Users/jeremydumalig/Documents/GitHub/Sports-Analytics-Group/Linear Regression")

# Install modelr package
# install.packages("modelr")

# Import tidyverse and ggplot2 packages
library(tidyverse)
library(ggplot2)
library(modelr)

# Import dataset, add points column
nba <- 
  read_csv("nba2022.csv") %>%
  mutate(PTS = FTM + 3*`3PM` + 2*(FGM - `3PM`))

# Visualize data
ggplot(data=nba, aes(x=FTA, y=PTS)) +
  geom_point()

# Find correlation between two variables
?cor
cor(nba$FTA, nba$PTS)

# Fit  data and find linear regression coefficients
# Note that fit$coefficients and fit[["coefficients"]] return the same output
?lm
fit <- lm(data=nba, PTS ~ FTA)
coef <- fit$coefficients
coef <- fit[["coefficients"]] # returns the same output as above

# Add predictions to nba dataframe
# Note that add_predictions() effectively does the same as mutate()
?add_predictions
nba <- 
  nba %>%
  add_predictions(fit) %>%
  mutate(Residual = PTS - pred) %>%
  select(PLAYER, FTA, PTS, pred, Residual)

# Visualize data and regression fit
ggplot(data=nba, aes(x=FTA, y=PTS)) +
  geom_abline(intercept=coef['(Intercept)'], slope=coef['FTA'], color="blue") +
  geom_segment(aes(x=FTA, xend=FTA, y=PTS, yend=pred), color = "light blue") +
  geom_point()
