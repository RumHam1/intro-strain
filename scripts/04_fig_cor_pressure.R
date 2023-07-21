library(tidyverse)
library(here)

pass_rush <- read_csv(here("data", "pass_rush.csv.gz"))
hsh <- pass_rush |>
  distinct(nflId, gameId, pff_hurry, pff_sack, pff_hit) |>
  group_by(nflId) |>
  summarize(hurries = sum(pff_hurry, na.rm = TRUE),
            sacks = sum(pff_sack, na.rm = TRUE),
            hits = sum(pff_hit, na.rm = TRUE))

test <- pass_rush |> 
  filter(team == defensiveTeam & !is.na(strain))
test$strain <- test$slope_distance_from_QB/test$distance_from_QB
test_summed <- test |> 
  filter(!is.infinite(strain)) |> 
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

players <- read_csv(here("data", "players.csv"))

fig_cor_pressure <- test_summed |> 
  left_join(hsh) |> 
  filter(n_plays >= 100 & !is.infinite(strain)) |> 
  mutate(pres = (hurries + sacks + hits) / n_plays) |>
  mutate(pos = factor(pos, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(strain_rate, pres)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = pos, group = name), size = 2, alpha = 0.8) +
  expand_limits(x = 2.7, y = c(0.02, 0.12)) +
  scale_x_continuous(breaks = seq(0.5, 2.5, 1)) +
  scale_y_continuous(breaks = seq(0, 0.12, 0.04)) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  labs(y = "Pressure rate",
       x = "Average STRAIN",
       color = "Position") +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        panel.grid.minor = element_blank())

fig_cor_pressure_bw <- test_summed |> 
  left_join(hsh) |> 
  filter(n_plays >= 100 & !is.infinite(strain)) |> 
  mutate(pres = (hurries + sacks + hits) / n_plays) |>
  mutate(pos = factor(pos, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(strain_rate, pres)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = pos, group = name, shape = pos, fill = pos), size = 2, alpha = 0.8) +
  expand_limits(x = 2.7, y = c(0.02, 0.12)) +
  scale_x_continuous(breaks = seq(0.5, 2.5, 1)) +
  scale_y_continuous(breaks = seq(0, 0.12, 0.04)) +
  #scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  scale_color_manual(values = rev(c("gray20", "gray20", "gray80", "gray80"))) +
  scale_fill_manual(values = rev(c("gray20", "gray20", "gray80", "gray80"))) +
  scale_shape_manual(values = c(21, 24, 22, 25)) +
  labs(y = "Pressure rate",
       x = "Average STRAIN",
       color = "Position",
       fill = "Position",
       shape = "Position") +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        panel.grid.minor = element_blank())
