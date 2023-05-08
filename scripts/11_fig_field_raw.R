library(tidyverse)
library(here)

play_lv_den <- read_csv(here("data", "play_lv_den.csv.gz"))

# play_lv_den |> filter(nflId == 47889) |>
#   select(frameId, jerseyNumber, team, x:event) |>
#   filter(frameId %in% c(7, 8, 50)) |>
#   xtable::xtable()

plays <- read_csv(here("data", "plays.csv"))
desc <- plays |>
  filter(gameId == 2021101709 & playId == 1444) |>
  pull(playDescription) |> 
  str_replace("\\)\\.", "\\)")

plot_field_raw <- function(frame) {
  plot_df <- filter(play_lv_den, frameId_snap_corrected == frame)
  field <- ggplot() + 
    annotate("text", 
             x = seq(40, 80, 10),
             y = 12,
             color = "#bebebe",
             size = rel(3),
             label = 10 * c(3:5, 4:3)) +
    annotate("text", 
             x = seq(40, 80, 10),
             y = 38.35,
             color = "#bebebe",
             size = rel(3),
             angle = 180,
             label = 10 * c(3:5, 4:3)) +
    annotate("segment", 
             x = setdiff(seq(35, 85, 1), seq(35, 85, 5)),
             xend = setdiff(seq(35, 85, 1), seq(35, 85, 5)),
             y = -Inf,
             yend = 0,
             color = "#bebebe") +
    annotate("segment", 
             x = setdiff(seq(35, 85, 1), seq(35, 85, 5)),
             xend = setdiff(seq(35, 85, 1), seq(35, 85, 5)),
             y = 48.7,
             yend = Inf,
             color = "#bebebe") +
    annotate("text", 
             x = setdiff(seq(35, 85, 1), seq(35, 85, 5)),
             y = 23.36667,
             color = "#bebebe",
             label = "|",
             size = rel(2)) +
    annotate("text", 
             x = setdiff(seq(35, 85, 1), seq(35, 85, 5)),
             y = 29.96667,
             color = "#bebebe",
             label = "|",
             size = rel(2)) +
    annotate("segment", 
             x = 35,
             xend = 85,
             y = c(-Inf, Inf),
             yend = c(-Inf, Inf),
             color = "#bebebe") +
    geom_vline(xintercept = seq(35, 85, 5), color = "#bebebe") +
    geom_point(aes(x = x, y = y),
               size = 0.5,
               color = "black",
               data = filter(plot_df, team == "LV" & nflId != 47889)) +
    geom_point(aes(x = x, y = y),
               color = "#1143E2",
               size = 0.5,
               data = filter(plot_df, team == "LV" & nflId == 47889)) +
    geom_point(aes(x = x, y = y),
               color = "#FB4F14",
               size = 0.5,
               data = filter(plot_df, team == "DEN")) +
    geom_point(aes(x = x, y = y),
               color = "#803621",
               size = 0.5,
               shape = 20,
               data = filter(plot_df, team == "football")) +
    scale_size_identity() +
    labs(title = str_c(frame/10, "s since snap")) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          plot.title = element_text(size = rel(0.8), face = "bold", hjust = 0.5),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0.25, 0, 0, -0.25), "cm"))
  return(field)
}


library(cowplot)
fig_field_raw <- plot_grid(
  plot_field_raw(10),
  plot_field_raw(20),
  plot_field_raw(30),
  plot_field_raw(40)
)

