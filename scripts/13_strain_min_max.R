library(tidyverse)
library(here)

games <- read_csv(here("data", "games.csv"))
source(here("scripts", "04_fig_cor_pressure.R"))

min_100_snaps <- test |> 
  select(nflId, team, displayName, officialPosition) |> 
  distinct() |> 
  right_join(test_summed) |> 
  filter(n_plays >= 100 & !is.infinite(strain)) |> 
  pull(displayName)

strain_first <- test |>
  left_join(select(games, gameId, week)) |> 
  filter(displayName %in% min_100_snaps, week %in% 1:4, !is.infinite(strain)) |> 
  group_by(nflId) |>
  summarize(mean_strain = mean(strain),
            min_strain = min(strain),
            max_strain = max(strain))

strain_last <- test |>
  left_join(select(games, gameId, week)) |> 
  filter(displayName %in% min_100_snaps, week %in% 5:8, !is.infinite(strain)) |> 
  group_by(nflId) |>
  summarize(mean_strain = mean(strain),
            min_strain = min(strain),
            max_strain = max(strain))


cor(strain_first$mean_strain, strain_last$mean_strain)
cor(strain_first$min_strain, strain_last$min_strain)
cor(strain_first$max_strain, strain_last$max_strain)
