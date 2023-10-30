# Set working directory
setwd("/Users/jeremydumalig/Documents/GitHub/Sports-Analytics-Group/2023-24 Intro to R")

# Install required packages
install.packages("tidyverse")
install.packages("ggplot2")

# Import required packages
library(tidyverse)
library(ggplot2)

# Import dataset
nba_totals <- read_csv("nba_totals_2023.csv")

# View dataset
colnames(nba_totals)
nrow(nba_totals)
summary(nba_totals)

View(nba_totals)

head(nba_totals, 5)
tail(nba_totals, 5)

# Select or deselect by column
select(nba_totals, Player)
select(nba_totals, Player, Pos, Age)
select(nba_totals, -GS)

# Condition rows
filter(nba_totals, Player == "LeBron James")
filter(nba_totals, `3P` >= 200)
filter(nba_totals, `3P` >= 50, Tm == "GSW")

# Sort rows
arrange(nba_totals, `3P`)
arrange(nba_totals, desc(`3P`))
arrange(nba_totals, Tm, desc(`3P`))

View( arrange(nba_totals, Tm, desc(`3P`)) )

# Create new columns based on existing data
nba_totals <- mutate(nba_totals, 
                     `3P%` = `3P` / `3PA`,
                     PTS = 1*FT + 2*`2P` + 3*`3P`,
                     Shooter = case_when((`3P%` >= 0.4) ~ "Elite",
                                          (`3P%` >= 0.35) ~ "Above Average",
                                          TRUE ~ "Below Average"))
select(nba_totals,
       Player, `FG%`, PTS)

# Compute summary statistics
nba_freethrows <- summarize(nba_totals, 
                            FT = sum(FT),
                            FTA = sum(FTA))
nba_freethrows <- mutate(nba_freethrows,
                         `FT%` = FT / FTA)
nba_freethrows

# Want to find each team's free-throw percentage
team_freethrows <- nba_totals

team_freethrows <- group_by(team_freethrows, Tm)

team_freethrows <- summarize(team_freethrows, 
                             FT = sum(FT),
                             FTA = sum(FTA))

team_freethrows <- ungroup(team_freethrows)

team_freethrows <- mutate(team_freethrows,
                         `FT%` = FT / FTA)

team_freethrows <- arrange(team_freethrows, desc(`FT%`))
team_freethrows


nba_totals %>%
  group_by(Tm) %>%
  summarize(FT = sum(FT),
            FTA = sum(FTA)) %>%
  ungroup() %>%
  mutate(`FT%` = FT / FTA) %>%
  arrange(desc(`FT%`))

# nba_totals <- mutate(nba_totals, `3P%` = `3P` / `3PA`)
ggplot(data=nba_totals, 
       aes(x=`3PA`, 
           y=`3P%`,
           color=Tm)) +
  geom_point()

nba_totals %>%
  filter(Tm == "CHI") %>%
  ggplot(aes(x=`3PA`, 
             y=`3P%`)) +
  geom_point() +
  geom_label(aes(label=Player))


