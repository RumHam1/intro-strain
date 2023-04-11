library(tidyverse)
library(here)

# df_cleaned <- read_csv(here("data", "df_cleaned.csv.gz"))
# 
# pass_rush_snaps <- df_cleaned |>
#   left_join(pff) |>
#   filter(pff_role == "Pass Rush") |> 
#   group_by(nflId) |>
#   summarize(n_plays = n_distinct(playId))
# write_rds(pass_rush_snaps, here("data", "pass_rush_snaps.rds"))
# 
# pass_snaps <- df_cleaned |>
#   left_join(pff) |>
#   filter(pff_role == "Pass") |> 
#   group_by(nflId) |>
#   summarize(n_plays = n_distinct(playId))
# write_rds(pass_snaps, here("data", "pass_snaps.rds"))
# 
# pass_block_snaps <- df_cleaned |>
#   left_join(pff) |>
#   filter(pff_role == "Pass Block") |> 
#   group_by(nflId) |>
#   summarize(n_plays = n_distinct(playId))
# write_rds(pass_block_snaps, here("data", "pass_block_snaps.rds"))

plays <- read_csv(here("data", "plays.csv"))
players <- read_csv(here("data", "players.csv"))
pff <- read_csv(here("data", "pffScoutingData.csv"))

# snap counts
pass_rush_snaps <- read_rds(here("data", "pass_rush_snaps.rds"))
pass_snaps <- read_rds(here("data", "pass_snaps.rds"))
pass_block_snaps <- read_rds(here("data", "pass_block_snaps.rds"))

# df_cleaned |>
#   filter(event == "ball_snap") |>
#   write_csv(here("data", "ball_snap.csv.gz"))

ball_snap <- read_csv(here("data", "ball_snap.csv.gz"))

pass_rush <- read_csv(here("data", "pass_rush.csv.gz"))

# check distribution of average strain
# 6 obs with avg strain > 2, else pretty normal
get_avg_strain <- pass_rush |> 
  filter(team == defensiveTeam, !is.infinite(strain)) |> 
  group_by(gameId, playId, nflId) |> 
  summarize(avg_strain = mean(strain, na.rm = TRUE)) |>
  ungroup()

# get_avg_strain |> 
#   ggplot(aes(avg_strain)) +
#   geom_histogram()

# get qb and play info from nflfastR
d <- nflfastR::load_pbp(2021)

pass_plays <- plays |> 
  transmute(game_play_id = str_c(gameId, playId, sep = "_")) |> 
  pull()

get_qb <- d |> 
  select(gameId = old_game_id, playId = play_id, passer, half_seconds_remaining,
         yardline_100, down, ydstogo, posteam_timeouts_remaining, defteam_timeouts_remaining) |> 
  filter(str_c(gameId, playId, sep = "_") %in% pass_plays) |> 
  mutate(gameId = as.double(gameId))

get_play_len <- pass_rush |> 
  distinct(gameId, playId, snap_frame, end_frame) |> 
  transmute(gameId, playId, play_len = end_frame - snap_frame)


# all game-plays
all_game_plays <- unique(str_c(pass_rush$gameId, pass_rush$playId, sep = "_"))

set.seed(100)
mod_df <- pff |> 
  filter(pff_role == "Pass Block") |> 
  select(gameId,
         playId,
         blocker_id = nflId,
         nflId = pff_nflIdBlockedPlayer) |> 
  filter(!is.na(nflId)) |>
  left_join(get_qb) |> 
  left_join(get_play_len) |> 
  left_join(get_avg_strain) |> 
  filter(str_c(gameId, playId, sep = "_") %in% all_game_plays) |> 
  left_join(select(players, nflId, rush_pos = officialPosition)) |> 
  filter(!rush_pos %in% c("RB", "G")) |> 
  mutate(rush_pos = ifelse(rush_pos %in% c("CB", "FS", "SS"), "secondary", rush_pos)) |> 
  mutate(rush_pos = ifelse(rush_pos %in% c("ILB", "MLB"), "interiorLB", rush_pos)) |> 
  mutate(rush_pos = ifelse(rush_pos == "LB", "OLB", rush_pos)) |> 
  left_join(select(players, blocker_id = nflId, block_pos = officialPosition)) |> 
  mutate(block_pos = ifelse(!block_pos %in% c("C", "G", "T"), "other", block_pos))

# check to make sure
# mod_df |> 
#   count(rush_pos)
# 
# mod_df |> 
#   count(block_pos)

# some rushers are assigned 4 blockers 
n_blockers <- mod_df |> 
  count(gameId, playId, nflId, name = "n_blockers")

loc_rush <- mod_df |> 
  select(gameId, playId, nflId) |> 
  left_join(select(ball_snap, gameId:nflId, x:y))

loc_block <- mod_df |> 
  select(gameId, playId, nflId = blocker_id) |> 
  left_join(select(ball_snap, gameId:nflId, x:y))

# tie breaker: closest blocker at ball snap
mod_df_final <- mod_df |> 
  mutate(x_rush = loc_rush$x,
         y_rush = loc_rush$y,
         x_block = loc_block$x,
         y_block = loc_block$y,
         dis = sqrt((x_rush - x_block) ^ 2 + (y_rush - y_block) ^ 2)) |>
  group_by(gameId, playId, nflId) |> 
  slice_min(dis, n = 1) |> 
  left_join(n_blockers)


library(lme4)  
strain_fit <- lmer(
  avg_strain ~ (1 | nflId) + (1 | blocker_id) + (1 | passer) + play_len + rush_pos + block_pos + n_blockers,
  data = mod_df_final
)


# fit_out_rush <- strain_fit |> 
#   ranef() |> 
#   pluck("nflId")
# 
# tibble(nflId = as.double(rownames(fit_out_rush)), 
#        intercept = fit_out_rush$`(Intercept)`) |> 
#   arrange(-intercept) |> 
#   left_join(players) |> 
#   filter(nflId %in% filter(pass_rush_snaps, n_plays >= 100)$nflId) |> 
#   filter(officialPosition == "OLB")
# 
# 
# fit_out_qb <- strain_fit |> 
#   ranef() |> 
#   pluck("passer")
# 
# get_passer_id <- players |> 
#   filter(officialPosition == "QB") |> 
#   separate(displayName, into = c("fn", "ln")) |> 
#   transmute(passer = str_c(str_sub(fn, 1, 1), ".", ln),
#             nflId)
# 
# tibble(passer = rownames(fit_out_qb), 
#        intercept = fit_out_qb$`(Intercept)`) |>
#   arrange(-intercept) |> 
#   left_join(get_passer_id) |> 
#   filter(nflId %in% filter(pass_snaps, n_plays >= 100)$nflId)
  
# https://cran.r-project.org/web/packages/merTools/index.html

# fit_out_block <- strain_fit |> 
#   ranef() |> 
#   pluck("blocker_id")
# 
# tibble(nflId = as.double(rownames(fit_out_block)), 
#        intercept = fit_out_block$`(Intercept)`) |> 
#   arrange(intercept) |> 
#   left_join(players) |> 
#   filter(nflId %in% filter(pass_block_snaps, n_plays >= 100)$nflId) |> 
#   filter(officialPosition == "C")


# fixed eff
# var partition

summary(strain_fit)$coef
broom.mixed::tidy(strain_fit, conf.int=TRUE)

strain_fit |> 
  VarCorr() |> 
  as_tibble() |> 
  mutate(icc = vcov / sum(vcov)) |> 
  select(grp, icc)

strain_eff <- merTools::REsim(strain_fit, n.sims = 10000, seed = 101)

# strain_eff |> 
#   merTools::plotREsim()

fig_rankings_boot <- strain_eff |> 
  as_tibble() |> 
  filter(groupFctr == "nflId") |> 
  mutate(nflId = as.double(groupID)) |> 
  left_join(players) |> 
  filter(nflId %in% filter(pass_rush_snaps, n_plays >= 100)$nflId) |> 
  filter(officialPosition %in% c("DE", "OLB", "DT", "NT")) |> 
  group_by(officialPosition) |> 
  arrange(desc(mean)) |>
  slice(1:10) |>
  ggplot(aes(x = reorder(displayName, mean))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - 2 * sd,
                    ymax = mean + 2 * sd)) +
  facet_wrap(~officialPosition, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_light() +
  labs(x = NULL, y = "intercept")
