library(tidyverse)
library(here)

pass_rush <- read_csv(here("data", "pass_rush.csv.gz"))


pff <- read_csv(here("data", "pffScoutingData.csv"))

outcomes <- pff |> 
  group_by(gameId, playId, nflId) |> 
  summarize(is_hit = sum(pff_hit, na.rm = TRUE) > 0,
            is_hurry = sum(pff_hurry, na.rm = TRUE) > 0 ,
            is_sack = sum(pff_sack, na.rm = TRUE) > 0)


hit <- pass_rush |> 
  inner_join(outcomes |> 
               filter(is_hit == TRUE)) |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  mutate(outcome = "Hit")

sack <- pass_rush |> 
  inner_join(outcomes |> 
               filter(is_sack == TRUE)) |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  mutate(outcome = "Sack")


hurry <- pass_rush |> 
  inner_join(outcomes |> 
               filter(is_hurry == TRUE)) |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  mutate(outcome = "Hurry")

none <- pass_rush |> 
  inner_join(
    outcomes |> 
      filter(is_hit + is_hurry + is_sack == 0)) |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  mutate(outcome = "None")


fig_outcome_curves <- hit |> 
  bind_rows(sack) |> 
  bind_rows(hurry) |> 
  bind_rows(none) |> 
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "DT", "NT")),
         outcome = factor(outcome, levels = c("Sack", "Hit", "Hurry", "None"))) |> 
  ggplot(aes(frameId_snap_corrected, mn, 
             color = officialPosition, 
             group = officialPosition)) +
  geom_smooth(se = FALSE, span = 0.4, linewidth = 1) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40"), name = "Position") +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  scale_linetype_manual(values = c(rep("solid", 4), "dotted"), name = "Position") +
  expand_limits(y = 2.3) +
  labs(y = "STRAIN",
       x = "Time since snap (seconds)") +
  theme_light() +
  facet_wrap(~ outcome, nrow = 1) +
  theme(legend.key.width = unit(0.9, "cm"),
        legend.position = "bottom",
        axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)))


