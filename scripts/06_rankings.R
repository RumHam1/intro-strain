library(tidyverse)
library(here)

pass_rush <- read_csv(here("data", "pass_rush.csv.gz"))

test <- pass_rush |> 
  filter(team == defensiveTeam & !is.na(strain))
test$strain <- test$slope_distance_from_QB/test$distance_from_QB
test_summed <- test |> 
  group_by(nflId) |>
  summarize(
    strain = sum(strain),
    name = head(displayName, 1),
    pos = head(officialPosition, 1),
    n = n(),
    n_plays = length(unique(playId))
  ) |>
  mutate(strain_rate = 10 * strain / n) |> 
  arrange(-strain_rate) |> 
  ungroup()

info <- test |> 
  ungroup() |> 
  select(nflId, team, displayName, officialPosition) |> 
  distinct()

out <- test_summed |> 
  left_join(info, by = "nflId") |> 
  filter(n_plays >= 100 & !is.infinite(strain)) |> 
  mutate(rank = row_number()) |> 
  filter(strain_rate > 0) |> 
  select(rank, displayName, 
         officialPosition,
         team, 
         n_plays, 
         strain, 
         strain_rate)

library(xtable)
# edge 
edge <- out |>
  filter(officialPosition %in% c("OLB", "DE")) |> 
  mutate(rank = row_number()) |> 
  select(Rank = rank, 
         Player = displayName, 
         Team = team, 
         Position = officialPosition,
         Snaps = n_plays, 
         "Average STRAIN" = strain_rate) |> 
  slice_head(n = 15)

print(xtable(edge), include.rownames = FALSE)

# interior
interior <- out |>
  filter(officialPosition %in% c("DT", "NT")) |> 
  mutate(rank = row_number()) |> 
  select(Rank = rank, 
         Player = displayName, 
         Team = team, 
         Position = officialPosition,
         Snaps = n_plays, 
         "Average STRAIN" = strain_rate) |> 
  slice_head(n = 15)

print(xtable(interior), include.rownames = FALSE)
