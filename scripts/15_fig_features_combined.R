library(here)
source(here("scripts", "01_fig_field.R"))
pff <- read_csv(here("data", "pffScoutingData.csv"))
play_unsucess <- read_csv(here("data", "play_unsucess.csv.gz"))


crosby_sack <- play_lv_den |> 
  ungroup() |> 
  filter(nflId == 47889) |>
  select(frameId_snap_corrected, distance_from_QB, slope_distance_from_QB, strain)

feat_sack <- play_lv_den |> 
  left_join(pff) |>
  filter(pff_role == "Pass Rush", nflId != 47889) |> 
  select(name = displayName, frameId_snap_corrected, value = strain) |> 
  mutate(name = str_c("strain_", name)) |> 
  bind_rows(pivot_longer(crosby_sack, !frameId_snap_corrected)) |> 
  mutate(play = "Successful pass rush")



crosby_unsuccess <- play_unsucess |> 
  filter(nflId == 47889) |>
  select(frameId_snap_corrected, distance_from_QB, slope_distance_from_QB, strain)

feat_unsuccess <- play_unsucess |> 
  left_join(pff) |>
  filter(pff_role == "Pass Rush", nflId != 47889) |> 
  select(name = displayName, frameId_snap_corrected, value = strain) |> 
  mutate(name = str_c("strain_", name)) |> 
  bind_rows(pivot_longer(crosby_unsuccess, !frameId_snap_corrected)) |> 
  mutate(play = "Unsuccessful pass rush")


fig_features_combined <- feat_sack |> 
  bind_rows(feat_unsuccess) |> 
  mutate(
    name = case_when(
      name == "distance_from_QB" ~ "Distance from QB\n(yards)",
      name == "slope_distance_from_QB" ~ "Velocity\n(yards/second)",
      name == "strain" ~ "STRAIN\n(1/second)",
      TRUE ~ as.character(name)
    ),
    name_fct = ifelse(str_detect(name, "strain_"), "STRAIN of\nother rushers", name),
    name_fct = factor(
      name_fct,
      levels = c(
        "Distance from QB\n(yards)",
        "Velocity\n(yards/second)",
        "STRAIN\n(1/second)",
        "STRAIN of\nother rushers"
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
  facet_wrap(~ play, scales = "free_x") +
  labs(color = NULL,
       alpha = NULL) +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        strip.text = element_text(size = rel(1)),
        legend.position = "bottom",
        legend.text = element_text(size = rel(0.9)),
        panel.spacing = unit(2, "lines"),
        legend.box.spacing = unit(0.25, "lines"))






fig_features_combined_bw <- feat_sack |> 
  bind_rows(feat_unsuccess) |> 
  mutate(
    name = case_when(
      name == "distance_from_QB" ~ "Distance from QB\n(yards)",
      name == "slope_distance_from_QB" ~ "Velocity\n(yards/second)",
      name == "strain" ~ "STRAIN\n(1/second)",
      TRUE ~ as.character(name)
    ),
    name_fct = ifelse(str_detect(name, "strain_"), "STRAIN of\nother rushers", name),
    name_fct = factor(
      name_fct,
      levels = c(
        "Distance from QB\n(yards)",
        "Velocity\n(yards/second)",
        "STRAIN\n(1/second)",
        "STRAIN of\nother rushers"
      )
    )
  ) |> 
  ggplot() +
  geom_line(aes(frameId_snap_corrected, value, alpha = name_fct, group = name, linetype = name_fct), linewidth = 1.2) +
  labs(x = "Time since snap (seconds)",
       y = "Feature value") +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  # scale_color_manual(values = c("red", "#FFCC33" , "#1143E2", "#CDCDCD"),
  scale_linetype_manual(values = c("dashed", "dotted" , "solid", "solid"),
                        guide = guide_legend(order = 1)) +
  scale_alpha_manual(values = c(1, 1, 1, 0.1),
                     guide = guide_legend(order = 1)) +
  facet_wrap(~ play, scales = "free_x") +
  labs(linetype = NULL,
       alpha = NULL) +
  theme_light() +
  theme(axis.title = element_text(size = rel(0.9)),
        axis.text = element_text(size = rel(0.7)),
        strip.text = element_text(size = rel(0.9)),
        legend.position = "bottom",
        legend.text = element_text(size = rel(0.75)),
        panel.spacing = unit(2, "lines"),
        legend.box.spacing = unit(0.25, "lines"),
        legend.key.width = unit(1.5, "cm"))
