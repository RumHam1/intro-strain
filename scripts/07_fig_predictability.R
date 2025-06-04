library(tidyverse)
library(here)


games <- read_csv(here("data", "games.csv"))
players <- read_csv(here("data", "players.csv"))
source(here("scripts", "05_fig_stability.R"))


min_100_snaps_id <- test |> 
  select(nflId, team, displayName, officialPosition) |> 
  distinct() |> 
  right_join(test_summed) |> 
  filter(n_plays >= 100 & !is.infinite(strain)) |> 
  pull(nflId)

hsh <- pass_rush |>
  distinct(nflId, gameId, pff_hurry, pff_sack, pff_hit) |> 
  left_join(select(games, gameId, week))


hsh_first <- hsh |> 
  filter(nflId %in% min_100_snaps_id, week %in% 1:4) |> 
  group_by(nflId) |>
  summarize(hurries = sum(pff_hurry, na.rm = TRUE),
            sacks = sum(pff_sack, na.rm = TRUE),
            hits = sum(pff_hit, na.rm = TRUE)) |> 
  full_join(
    test |>
      left_join(select(games, gameId, week)) |> 
      filter(displayName %in% min_100_snaps, week %in% 1:4) |> 
      group_by(nflId) |>
      summarize(n_plays = length(unique(playId)))
  ) |>
  # pressure rate = (hurries + sacks + hits) / plays
  transmute(nflId, first_hsh = (hurries + sacks + hits) / n_plays)

hsh_last <- hsh |> 
  filter(nflId %in% min_100_snaps_id, week %in% 5:8) |> 
  group_by(nflId) |>
  summarize(hurries = sum(pff_hurry, na.rm = TRUE),
            sacks = sum(pff_sack, na.rm = TRUE),
            hits = sum(pff_hit, na.rm = TRUE)) |> 
  full_join(
    test |>
      left_join(select(games, gameId, week)) |> 
      filter(displayName %in% min_100_snaps, week %in% 5:8) |> 
      group_by(nflId) |>
      summarize(n_plays = length(unique(playId)))
  ) |>
  # pressure rate = (hurries + sacks + hits) / plays
  transmute(nflId, last_hsh = (hurries + sacks + hits) / n_plays)



#0.09651732
hsh_hsh <- hsh_first |> 
  full_join(hsh_last) |>
  left_join(select(players, nflId, officialPosition)) |>
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(first_hsh, last_hsh)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = officialPosition, group = nflId), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  labs(x = "Pressure rate (first 4 weeks)",
       y = "Pressure rate (last 4 weeks)",
       color = "Position") +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)),
        legend.position = "none",
        axis.text = element_text(size = rel(0.8)),
        panel.grid.minor = element_blank())


#0.3216974

strain_hsh <- hsh_last |> 
  left_join(select(players, nflId, officialPosition, displayName)) |> 
  select(name = displayName, pos = officialPosition, last_hsh) |> 
  full_join(strain_first) |>
  mutate(pos = factor(pos, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(first_strain_rate, last_hsh)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = pos, group = name), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  labs(x = "Average STRAIN (first 4 weeks)",
       # y = "Pressure rate (last 4 weeks)",
       y = NULL,
       color = "Position") +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        axis.text.y = element_blank(),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        panel.grid.minor = element_blank())

fig_predictability <- cowplot::plot_grid(hsh_hsh, strain_hsh, rel_widths = c(2.1, 2.4))


# bw ----------------------------------------------------------------------

hsh_hsh_bw <- hsh_first |> 
  full_join(hsh_last) |>
  left_join(select(players, nflId, officialPosition)) |>
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(first_hsh, last_hsh)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = officialPosition, group = nflId, fill = officialPosition, shape = officialPosition), size = 2, alpha = 0.8) +
  #scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  scale_color_manual(values = rev(c("gray20", "gray20", "gray80", "gray80"))) +
  scale_fill_manual(values = rev(c("gray20", "gray20", "gray80", "gray80"))) +
  scale_shape_manual(values = c(21, 24, 22, 25)) +
  labs(x = "Pressure rate (first 4 weeks)",
       y = "Pressure rate (last 4 weeks)",
       color = "Position",
       fill = "Position",
       shape = "Position") +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)),
        legend.position = "none",
        axis.text = element_text(size = rel(0.8)),
        panel.grid.minor = element_blank())


#0.3216974

strain_hsh_bw <- hsh_last |> 
  left_join(select(players, nflId, officialPosition, displayName)) |> 
  select(name = displayName, pos = officialPosition, last_hsh) |> 
  full_join(strain_first) |>
  mutate(pos = factor(pos, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(first_strain_rate, last_hsh)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = pos, group = name, fill = pos, shape = pos), size = 2, alpha = 0.8) +
  #scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  scale_color_manual(values = rev(c("gray20", "gray20", "gray80", "gray80"))) +
  scale_fill_manual(values = rev(c("gray20", "gray20", "gray80", "gray80"))) +
  scale_shape_manual(values = c(21, 24, 22, 25)) +
  labs(x = "Average STRAIN (first 4 weeks)",
       # y = "Pressure rate (last 4 weeks)",
       y = NULL,
       color = "Position",
       fill = "Position",
       shape = "Position") +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        axis.text.y = element_blank(),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        panel.grid.minor = element_blank())

fig_predictability_bw <- cowplot::plot_grid(hsh_hsh_bw, strain_hsh_bw, rel_widths = c(2.1, 2.4))
