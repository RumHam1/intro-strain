library(tidyverse)
library(here)

pass_rush <- read_csv(here("data", "pass_rush.csv.gz"))

get_avg_strain <- pass_rush |> 
  filter(team == defensiveTeam, !is.infinite(strain)) |> 
  group_by(gameId, playId, nflId) |> 
  summarize(avg_strain = mean(strain, na.rm = TRUE)) |>
  ungroup()

games <- read_csv(here("data", "games.csv"))
plays <- read_csv(here("data", "plays.csv"))



#table(plays$down)

get_avg_strain |> 
  left_join(plays) |> 
  mutate(down = factor(down)) |> 
  ggplot(aes(down, avg_strain)) +
  geom_boxplot()

get_avg_strain |> 
  left_join(plays) |> 
  mutate(down = factor(down)) |> 
  ggplot(aes(avg_strain, down)) +
  ggridges::geom_density_ridges(scale = 1, quantile_lines = TRUE, quantiles = 0.5)


get_avg_strain |> 
  left_join(plays) |> 
  ggplot(aes(yardsToGo, avg_strain, color = factor(down))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ down, nrow = 1)

get_avg_strain |> 
  left_join(plays) |> 
  ggplot(aes(absoluteYardlineNumber, avg_strain)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")


plays |> 
  count(yardlineNumber)




