
pass_rush


pff <- read_csv(here("data", "pffScoutingData.csv"))

outcomes <- pff |> 
  group_by(gameId, playId, nflId) |> 
  summarize(is_hit = sum(pff_hit, na.rm = TRUE) > 0,
            is_hurry = sum(pff_hurry, na.rm = TRUE) > 0 ,
            is_sack = sum(pff_sack, na.rm = TRUE) > 0)

outcomes |> 
  count(gameId, playId, nflId, sort = TRUE)


hit <- pass_rush |> 
  inner_join(outcomes |> 
               filter(is_hit == TRUE)) |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(frameId_snap_corrected, mn, 
             color = officialPosition, 
             group = officialPosition)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40"), name = "Position") +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  scale_linetype_manual(values = c(rep("solid", 4), "dotted"), name = "Position") +
  expand_limits(y = 2.3) +
  labs(y = "STRAIN",
       x = "Time since snap (seconds)",
       title = "Hit") +
  theme_light() +
  theme(legend.key.width = unit(0.9, "cm"),
        legend.position = "none",
        axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)))

sack <- pass_rush |> 
  inner_join(outcomes |> 
               filter(is_sack == TRUE)) |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(frameId_snap_corrected, mn, 
             color = officialPosition, 
             group = officialPosition)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40"), name = "Position") +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  scale_linetype_manual(values = c(rep("solid", 4), "dotted"), name = "Position") +
  expand_limits(y = 2.3) +
  labs(y = "STRAIN",
       x = "Time since snap (seconds)",
       title = "Sack") +
  theme_light() +
  theme(legend.key.width = unit(0.9, "cm"),
        legend.position = "none",
        axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)))


hurry <- pass_rush |> 
  inner_join(outcomes |> 
               filter(is_hurry == TRUE)) |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(frameId_snap_corrected, mn, 
             color = officialPosition, 
             group = officialPosition)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40"), name = "Position") +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  scale_linetype_manual(values = c(rep("solid", 4), "dotted"), name = "Position") +
  expand_limits(y = 2.3) +
  labs(y = "STRAIN",
       x = "Time since snap (seconds)",
       title = "Hurry") +
  theme_light() +
  theme(legend.key.width = unit(0.9, "cm"),
        legend.position = "none",
        axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)))

none <- pass_rush |> 
  inner_join(
    outcomes |> 
      filter(is_hit + is_hurry + is_sack == 0)) |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "DT", "NT"))) |> 
  ggplot(aes(frameId_snap_corrected, mn, 
             color = officialPosition, 
             group = officialPosition)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40"), name = "Position") +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  scale_linetype_manual(values = c(rep("solid", 4), "dotted"), name = "Position") +
  expand_limits(y = 2.3) +
  labs(y = "STRAIN",
       x = "Time since snap (seconds)",
       title = "None") +
  theme_light() +
  theme(legend.key.width = unit(0.9, "cm"),
        legend.position = c(0.8, 0.8),
        axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)))


cowplot::plot_grid(hit, sack, hurry, none, nrow = 1)

