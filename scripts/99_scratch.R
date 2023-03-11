
# correlation between weight and average STRAIN
# 0.-8185309
test_summed |> 
  filter(n_plays >= 100) |> 
  left_join(select(players, nflId, weight)) |>
  mutate(pos = factor(pos, levels = c("OLB", "DE", "DT", "NT")),
         class = ifelse(pos == "DT" | pos == "NT", "Interior", "Edge")) |> 
  ggplot(aes(weight, strain_rate)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black", alpha = 0.5) +
  geom_point(aes(color = pos, group = name), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#FFC107", "#004D40")) +
  labs(y = "Average STRAIN across all frames",
       x = "Weight (lbs)",
       color = "Position") +
  facet_wrap(~ class, scales = "free") +
  theme_light() +
  theme(axis.title = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        panel.grid.minor = element_blank())
