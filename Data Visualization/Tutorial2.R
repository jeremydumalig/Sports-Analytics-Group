# Optional: clear environment
rm(list = ls())

# Set working directory
setwd("/Users/jeremydumalig/Documents/GitHub/Sports-Analytics-Group")

# Install ggplot2 package
# install.packages("ggplot2")

# Import tidyverse AND ggplot2 packages
library(tidyverse)
library(ggplot2)

# Import dataset
nba <- read_csv("nba2022.csv")


# Pipe function
nba %>%
  filter(TEAM == "GSW") %>%
  arrange(desc(`3PM`)) %>%
  select(PLAYER, `3PM`) %>%
  head(5)

# Group by
nba %>%
  group_by(TEAM) %>%
  summarize(`Total 3PM` = sum(`3PM`)) %>%
  ungroup()

# Conditional mutate()
nba %>%
  mutate(`3P%` = `3PM` / `3PA`,
         `Shooter?` = case_when((`3P%` >= 0.4) ~ "Shooter",
                                (`3P%` < 0.4) ~ "Non-Shooter"))
