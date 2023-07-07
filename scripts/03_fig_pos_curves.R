library(tidyverse)
library(here)

# df_cleaned <- read_csv(here("data", "df_cleaned.csv.gz"))

pff <- read_csv(here("data", "pffScoutingData.csv"))

# pass_rush <- df_cleaned |> 
#   left_join(pff) |>
#   filter(pff_role == "Pass Rush")
# write_csv(pass_rush, here("data", "pass_rush.csv.gz"))

pass_rush <- read_csv(here("data", "pass_rush.csv.gz"))

avg_strain_all <- pass_rush |> 
  filter(team == defensiveTeam,
         frameId_snap_corrected <= 40) |> 
  group_by(frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE))


fig_pos_curves <- pass_rush |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  bind_rows(mutate(avg_strain_all, officialPosition = "Average")) |>
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "DT", "NT", "Average"))) |> 
  ggplot(aes(frameId_snap_corrected, mn, 
             color = officialPosition, 
             group = officialPosition, 
             linetype = officialPosition)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "gray"), name = "Position") +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  scale_linetype_manual(values = c(rep("solid", 4), "dotted"), name = "Position") +
  labs(y = "STRAIN",
       x = "Time since snap (seconds)") +
  theme_light() +
  theme(legend.key.width = unit(0.9, "cm"),
        axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)))

fig_pos_curves_bw <- pass_rush |> 
  filter(team == defensiveTeam,
         officialPosition %in% c("OLB", "NT", "DE", "DT"),
         frameId_snap_corrected <= 40) |> 
  group_by(officialPosition, frameId_snap_corrected) |> 
  summarise(mn = mean(strain, na.rm = TRUE)) |> 
  bind_rows(mutate(avg_strain_all, officialPosition = "Average")) |>
  mutate(officialPosition = factor(officialPosition, levels = c("OLB", "DE", "Average", "DT", "NT"))) |> 
  ggplot(aes(frameId_snap_corrected, mn, 
             color = officialPosition, 
             group = officialPosition, 
             linetype = officialPosition)) +
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = 0:4) +
  scale_linetype_manual(values = c("solid", "dashed", "twodash", "dotdash", "dotted"), name = "Position") +
  scale_color_manual(values = c("black", "black", "gray90", "gray60", "gray60"), name = "Position") +
  labs(y = "STRAIN",
       x = "Time since snap (seconds)") +
  theme_light() +
  theme(legend.key.width = unit(2, "cm"),
        axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)))
