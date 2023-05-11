
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

# defense formation
tibble(x = c(3:5, 3:6 - 0.5, c(2, 3.7, 4.3, 6)),
       y = c(1, 1, 1, 1.01, 1.01, 1.01, 1.01, 1.02, 1.02, 1.02, 1.02),
       pos = c("DE", "NT", "DE", "OLB", "ILB", "ILB", "OLB",
               "CB", "S", "S", "CB"),
       d = "3–4 Defense") |> 
  bind_rows(
    tibble(x = c(3:6, c(2.5, 4.5, 6.5), c(2, 4, 5, 7)),
           y = c(1, 1, 1, 1, 1.01, 1.01, 1.01, 1.02, 1.02, 1.02, 1.02),
           pos = c("DE", "DT", "DT", "DE", "OLB", "MLB", "OLB",
                   "CB", "S", "S", "CB"),
           d = "4-3 Defense") 
  ) |> 
  mutate(d = fct_relevel(d, "4-3 Defense")) |> 
  ggplot(aes(x, y)) +
  geom_tile(fill = "white") +
  geom_label(aes(label = pos), size = rel(4)) +
  geom_hline(yintercept = 0.997, linetype = "dashed") +
  facet_wrap(~ d, scales = "free", ncol = 1) +
  coord_cartesian(ylim = c(0.99, 1.03)) +
  theme_light() +
  theme(strip.text = element_text(size = rel(1.2)),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

