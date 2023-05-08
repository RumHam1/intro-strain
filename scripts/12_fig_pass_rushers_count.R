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
  geom_col() +
  scale_x_continuous(breaks = 1:8) +
  labs(x = "Number of pass rushers",
       y = "Count")
