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
  summarize(
    strain = sum(strain),
    name = head(displayName, 1),
    pos = head(officialPosition, 1),
    n = n()
  ) |>
  mutate(strain_rate = 10 * strain / n) |> 
  filter(pos %in% c("OLB", "DE", "DT", "NT")) |> 
  select(name, pos, first_strain_rate = strain_rate)

strain_last <- test |>
  left_join(select(games, gameId, week)) |> 
  filter(displayName %in% min_100_snaps, week %in% 5:8, !is.infinite(strain)) |> 
  group_by(nflId) |>
  summarize(
    strain = sum(strain),
    name = head(displayName, 1),
    pos = head(officialPosition, 1),
    n = n()
  ) |>
  mutate(strain_rate = 10 * strain / n) |> 
  filter(pos %in% c("OLB", "DE", "DT", "NT")) |> 
  arrange(-strain_rate) |> 
  select(name, pos, last_strain_rate = strain_rate)

fig_stability <- strain_first |> 
  full_join(strain_last) |>
  mutate(pos = factor(pos, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(first_strain_rate, last_strain_rate)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = pos, group = name), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  labs(x = "Average STRAIN (first 4 weeks)",
       y = "Average STRAIN (last 4 weeks)",
       color = "Position") +
    theme_light() +
    theme(axis.title = element_text(size = rel(1)),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(0.8)),
          axis.text = element_text(size = rel(0.8)),
          panel.grid.minor = element_blank())

fig_stability_bw <- strain_first |> 
  full_join(strain_last) |>
  mutate(pos = factor(pos, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(first_strain_rate, last_strain_rate)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = pos, group = name, fill = pos, shape = pos), size = 2, alpha = 0.8) +
  #scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  scale_color_manual(values = c("gray20", "gray20", "gray80", "gray80")) +
  scale_fill_manual(values = c("gray20", "gray20", "gray80", "gray80")) +
  scale_shape_manual(values = c(21, 24, 22, 25)) +
  labs(x = "Average STRAIN (first 4 weeks)",
       y = "Average STRAIN (last 4 weeks)",
       color = "Position",
       fill = "Position",
       shape = "Position") +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        panel.grid.minor = element_blank())

