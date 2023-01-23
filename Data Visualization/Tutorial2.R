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
nba <- read_csv("Data Visualization/nba2022.csv")



# Of players who made at least 82 three-pointers last season, who took the
# highest percentage of their shots from three-point range?

# Pipe function
nba %>%
  filter(`3PM` >= 82) %>%
  mutate(`3PR` = `3PA` / FGA) %>%
  arrange(desc(`3PR`)) %>%
  head(5)

# Conditional mutate()
mutate(dataframe, 
       col_name = case_when((condition1) ~ "First Category",
                            (condition2) ~ "Second Category",
                            TRUE ~ "Third Category"))
nba %>%
  mutate(`3P%` = `3PM` / `3PA`,
         `Shooter?` = case_when((`3P%` >= 0.4) ~ "Shooter",
                                (`3P%` < 0.4) ~ "Non-Shooter"))

# Group by
teams <- 
  nba %>%
  group_by(TEAM) %>%
  summarize(FGA = sum(FGA),
            `3PM` = sum(`3PM`),
            `3PA` = sum(`3PA`),
            `3P%` = 100 * `3PM` / `3PA`) %>%
  ungroup()
teams



# Histograms
ggplot(data=nba, aes(x=`3PA`)) +
  geom_histogram(bins=20, 
                 fill="light blue",
                 color="blue")

# Barchart
teams %>%
  filter(TEAM %in% c("GSW", "LAL", "LAC", "PHX", "SAC")) %>%
  ggplot(aes(x=fct_reorder(TEAM, `3PA`), 
             y=`3PA`,
             fill=TEAM)) +
  geom_bar(stat="identity")

# Scatterplot
ggplot(data=teams, aes(x=`3PA`, 
                       y=`3P%`)) +
  geom_point()

teams %>%
  filter(TEAM %in% c("GSW", "LAL", "LAC", "PHX", "SAC")) %>%
  ggplot(aes(x=`3PA`, 
             y=`3P%`,
             color=TEAM)) +
  geom_point(size=5) +
  labs(x="3-Point Attempts (3PA)",
       y="3-Point Percentage (3P%)",
       title="Pacific Division: 3PA x 3P%") +
  theme_bw() # https://ggplot2.tidyverse.org/reference/ggtheme.html

