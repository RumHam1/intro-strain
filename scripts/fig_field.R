library(tidyverse)
library(here)

# We have a cleaned data file from BDB
# This data file is too large to push to github
# So I stored it on dropbox (as a zipped file)
# Download and save it in the "data" folder
# https://www.dropbox.com/s/haojedm4kgq3atu/df_cleaned.csv.gz?dl=1
# What's great about readr::read_csv() is that
# it allows direct importing of .csv.gz

# df_cleaned <- read_csv(here("data", "df_cleaned.csv.gz"))

# play_lv_den <- df_cleaned |>
#   filter(gameId == 2021101709 & playId == 1444)
# write_csv(play_lv_den, here("data", "play_lv_den.csv.gz"))

play_lv_den <- read_csv(here("data", "play_lv_den.csv.gz"))

plays <- read_csv(here("data", "plays.csv"))
desc <- plays |>
  filter(gameId == 2021101709 & playId == 1444) |>
  pull(playDescription) |> 
  str_replace("\\)\\.", "\\)")

lv_strain_range <- play_lv_den |> 
  filter(team == "LV") |> 
  pull(strain) |> 
  range()

play_lv_den <- play_lv_den |>
  mutate(point_size = ifelse(
    team != "LV",
    1.5,
    scales::rescale(strain, to = c(1.5, 7.5), from = lv_strain_range)
  ))

plot_field <- function(frame) {
  plot_df <- filter(play_lv_den, frameId_snap_corrected == frame)
  field <- ggplot() + 
    annotate("text", 
             x = seq(40, 80, 10),
             y = 12,
             color = "#bebebe",
             size = rel(4),
             label = 10 * c(3:5, 4:3)) +
    annotate("text", 
             x = seq(40, 80, 10),
             y = 38.35,
             color = "#bebebe",
             size = rel(4),
             label = 10 * c(3:5, 4:3)) +
    annotate("text", 
             x = setdiff(seq(35, 85, 1), seq(35, 85, 5)),
             y = 0,
             color = "#bebebe",
             label = "|",
             vjust = 1.5) +
    annotate("text", 
             x = setdiff(seq(35, 85, 1), seq(35, 85, 5)),
             y = 48.7,
             color = "#bebebe",
             label = "∣",
             vjust = -0.5) +
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
    geom_point(aes(x = x, y = y, size = point_size),
               color = "black",
               data = filter(plot_df, team == "LV" & nflId != 47889)) +
    geom_point(aes(x = x, y = y, size = point_size),
               color = "#1143E2",
               data = filter(plot_df, team == "LV" & nflId == 47889)) +
    geom_point(aes(x = x, y = y, size = point_size),
               color = "#FB4F14",
               data = filter(plot_df, team == "DEN")) +
    geom_point(aes(x = x, y = y, size = point_size),
               color = "#803621",
               shape = 20,
               data = filter(plot_df, team == "football")) +
    scale_size_identity() +
    labs(title = str_c(frame/10, " seconds since snap")) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          plot.title = element_text(size = rel(1), face = "bold", hjust = 0.5),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
  return(field)
}


library(cowplot)
fig_field <- plot_grid(
  plot_field(20),
  plot_field(30),
  plot_field(40),
  ncol = 1
)

fig_field
