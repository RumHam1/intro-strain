library(here)
source(here("scripts", "fig_field.R"))
pff <- read_csv(here("data", "pffScoutingData.csv"))

crosby <- play_lv_den |> 
  ungroup() |> 
  filter(nflId == 47889) |>
  select(frameId_snap_corrected, distance_from_QB, slope_distance_from_QB, strain)


fig_crosby_curves <- play_lv_den |> 
  left_join(pff) |>
  filter(pff_role == "Pass Rush", nflId != 47889) |> 
  select(name = displayName, frameId_snap_corrected, value = strain) |> 
  mutate(name = str_c("strain_", name)) |> 
  bind_rows(pivot_longer(crosby, !frameId_snap_corrected)) |> 
  mutate(
    name = case_when(
      name == "distance_from_QB" ~ "Distance from QB (yards)",
      name == "slope_distance_from_QB" ~ "Velocity (yards/second)",
      name == "strain" ~ "STRAIN (1/second)",
      TRUE ~ as.character(name)
    ),
    name_fct = ifelse(str_detect(name, "strain_"), "STRAIN of other rushers", name),
    name_fct = factor(
      name_fct,
      levels = c(
        "Distance from QB (yards)",
        "Velocity (yards/second)",
        "STRAIN (1/second)",
        "STRAIN of other rushers"
      )
    )
  ) |> 
  ggplot() +
  geom_line(aes(frameId_snap_corrected, value, color = name_fct, alpha = name_fct, group = name), linewidth = 1.2) +
  labs(x = "Time since snap (seconds)",
       y = "Feature value") +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  scale_color_manual(values = c("red", "#FFCC33" , "#1143E2", "#CDCDCD"),
                     guide = guide_legend(order = 1)) +
  scale_alpha_manual(values = c(1, 1, 1, 0.4),
                     guide = guide_legend(order = 1)) +
  labs(color = NULL,
       alpha = NULL) +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1)))
