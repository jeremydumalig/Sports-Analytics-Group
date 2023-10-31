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

first5 <- head(nba_totals, 5)
tail(nba_totals, 5)

# Select or deselect by column
select(nba_totals, Player)
select(nba_totals, Player, `3P`, Age)
select(nba_totals, -`3P`)

# Condition rows
lebron <- filter(nba_totals, Player == "LeBron James")
filter(nba_totals, `3P` >= 200)
warriors <- filter(nba_totals, `3P` >= 50, Tm == "GSW")
select(warriors, Player, `3P`, `3PA`)

# Sort rows
arrange(nba_totals, `3P`)
mostThrees <- arrange(nba_totals, desc(`3P`))
View(arrange(nba_totals, Tm))
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
       Player, `3P%`, PTS)

# Compute summary statistics
nba_freethrows <- summarize(nba_totals, 
                            FT_sum = sum(FT),
                            FT_average = mean(FT),
                            FTA = sum(FTA))
nba_freethrows

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

# Of all points guards who played at least 65 games, who averaged the most assists?


# point_guards <- filter(nba_totals, ((Pos == "PG") | (Pos == "PG-SG") | (Pos == "SG-PG")), G >= 65)
point_guards <- filter(nba_totals, str_detect(Pos, "PG"), G >= 65)
point_guards <- mutate(point_guards, APG = AST / G)
point_guards <- arrange(point_guards, desc(APG))
point_guards <- select(point_guards, Player, G, AST, APG)
head(point_guards, 5)

nba_totals %>%
  filter(str_detect(Pos, "PG"), G >= 65) %>%
  mutate(APG = AST / G) %>%
  arrange(desc(APG)) %>%
  select(Player, G, AST, APG) %>%
  head(5) %>%
  View()

View(nba_totals)


# Any player who averages 25+ more points = star
# Which 5 teams have the most stars?

nba_totals %>%
  mutate(PTS = FT + 2*`2P` + 3*`3P`,
         PPG = PTS / G,
         Star = case_when((PPG >= 20) ~ TRUE,
                          TRUE ~ FALSE)) %>%
  select(Tm, Player, PTS, PPG, Star) %>%
  filter(Star) %>%
  group_by(Tm) %>%
  summarize(Stars = sum(Star)) %>%
  ungroup() %>%
  arrange(desc(Stars)) %>%
  View()




nba_totals %>%
  group_by(Tm) %>%
  summarize(FT = sum(FT),
            FTA = sum(FTA)) %>%
  ungroup() %>%
  mutate(`FT%` = FT / FTA) %>%
  arrange(desc(`FT%`))

nba_totals <- mutate(nba_totals, `3P%` = `3P` / `3PA`)
ggplot(nba_totals, 
       aes(x=`3PA`, 
           y=`3P%`,
           color=Pos)) +
  geom_point()

nba_totals %>%
  mutate(`3P%` = `3P` / `3PA`) %>%
  ggplot(aes(x=`3PA`, 
           y=`3P%`,
           color=Pos)) +
  geom_point()

nba_totals %>%
  mutate(`3P%` = `3P` / `3PA`) %>%
  ggplot(aes(x=`3PA`, 
             y=`3P%`,
             color=Pos)) +
  geom_point() +
  facet_wrap(~Pos)

nba_totals %>%
  ggplot(aes(x=`3P`)) +
  geom_histogram()
