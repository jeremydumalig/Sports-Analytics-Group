# Optional: clear environment
rm(list = ls())

# Set working directory
setwd("/Users/jeremydumalig/Downloads")

# Install tidyverse package
# install.packages("tidyverse")

# Import tidyverse package
library(tidyverse)

# Import dataset
nba <- read_csv("nba2022.csv")

# View dataset
View(nba)
head(nba, 5)
tail(nba, 5)


# Select or deselect by column
select(nba, PLAYER, PTS)
select(nba, -GP)

# Condition rows
filter(nba, PLAYER == "LeBron James")
filter(nba, `3PM` >= 200)
filter(nba, `3PM` >= 50, TEAM == "GSW")

# Sort rows
arrange(nba, FTM)
arrange(nba, desc(FTM))
arrange(nba, TEAM, desc(GP))

# Create new columns based on existing data
mutate(nba, 
       `FG%` = FGM / FGA,
       `2PM` = FGM - `3PM`,
       PTS = 2*`2PM` + 3*`3PM` + FTM,
       `TS%` = PTS / (2*(FGA + 0.44*FTA)))

# Compute summary statistics
summarize(nba, n(), sum(`3PM`))


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
