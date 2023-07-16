library(tidyverse)
theme_set(theme_light())
library(here)

pff_full <- read_csv(here("data", "pffScoutingData.csv"))

# pff_full |> 
#   filter(pff_role == "Pass Rush") |> 
#   nrow()

pass_rushers_count <- pff_full |> 
  filter(pff_role == "Pass Rush") |> 
  count(gameId, playId) |> 
  count(n) |> 
  ggplot(aes(n, nn)) +
  geom_col(fill = "gray") +
  scale_x_continuous(breaks = 1:8) +
  labs(x = "Number of pass rushers",
       y = "Count")

pass_blockers_count <- pff_full |> 
  filter(pff_role == "Pass Block") |> 
  count(gameId, playId) |> 
  count(n) |> 
  ggplot(aes(n, nn)) +
  geom_col() +
  scale_x_continuous(breaks = 1:8) +
  labs(x = "Number of pass blockers",
       y = "Count")


rushers_blockers <- pff_full |> 
  filter(pff_role == "Pass Rush") |> 
  count(gameId, playId, name = "n_rushers") |> 
  full_join(
    pff_full |> 
      filter(pff_role == "Pass Block") |> 
      count(gameId, playId, name = "n_blockers")
  )

rushers_blockers |> 
  select(contains("n_")) |> 
  drop_na() |> 
  cor()

rushers_blockers |> 
  ggplot(aes(n_rushers, n_blockers)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


rushers_blockers |> 
  count(n_rushers, n_blockers, sort = TRUE) |> 
  ggplot(aes(n_rushers, n_blockers)) +
  geom_tile(aes(fill = n), color = "white") +
  coord_fixed()
