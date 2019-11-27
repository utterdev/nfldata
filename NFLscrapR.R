# from https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701#file-nflscrapr-md

library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))

pbp %>% select(posteam, defteam, desc, play_type) %>% head

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp %>% select(posteam, desc, play_type) %>% head

pbp_rp %>% filter(play_type=="no_play") %>% select(desc, rush_attempt, pass_attempt)  %>% head

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp %>% filter(play_type=="no_play") %>% select(pass, rush, desc)  %>% head

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp %>%
  filter(posteam == "LA", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)

schotty <- pbp_rp %>%
  filter(wp>.20 & wp<.80 & down<=2 & qtr<=2 & half_seconds_remaining>120) %>%
  group_by(posteam) %>%
  summarize(mean_pass=mean(pass), plays=n()) %>%
  arrange(mean_pass)
schotty

ggplot(schotty, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam))

ggsave('FILENAME.png', dpi=1000)

chart_data <- pbp_rp %>%
  filter(pass==1) %>%
  group_by(posteam) %>%
  summarise(
    num_db = n(),
    epa_per_db = sum(epa) / num_db,
    success_rate = sum(epa > 0) / num_db
  )

nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))

chart %>%
  ggplot(aes(x = success_rate, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "Dropback success rate & EPA/play",
       subtitle = "2018") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('FILENAME.png', dpi=1000)

chart_data <- pbp_rp %>%
  group_by(posteam) %>%
  filter(down<=2) %>%
  summarise(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    epa_per_db = sum(epa * pass) / n_dropbacks,
    epa_per_rush = sum(epa * rush) / n_rush,
    success_per_db = sum(success * pass) / n_dropbacks,
    success_per_rush = sum(success * rush) / n_rush
  )

chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))

chart %>%
  ggplot(aes(x = epa_per_rush, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush EPA/play",
       y = "Pass EPA/play",
       caption = "Data from nflscrapR",
       title = "Early-down rush and pass EPA/play",
       subtitle = "2018") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('FILENAME.png', dpi=1000)

chart %>%
  ggplot(aes(x = success_per_rush, y = success_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush success rate",
       y = "Pass success rate",
       caption = "Data from nflscrapR",
       title = "Early-down rush and pass success rate",
       subtitle = "2018") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('FILENAME.png', dpi=1000)

pbp_rp %>% filter(play_type=="no_play") %>% select(desc, pass, passer_player_name, rusher_player_name, receiver_player_name) %>% head()


pbp_players <- pbp_rp %>% 
  mutate(
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name)
  )

qbs <- pbp_players %>% 
  mutate(
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    rusher = rusher_player_name,
    receiver = receiver_player_name,
    play = 1
  ) %>%
  group_by(name, posteam) %>%
  summarize (
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = sum(play),
    epa_per_play = sum(epa)/n_plays,
    success_per_play =sum(success)/n_plays
  ) %>%
  filter(n_dropbacks>=100)

library(ggrepel)

qbs %>%
  ggplot(aes(x = success_per_play, y = epa_per_play)) +
  geom_hline(yintercept = mean(qbs$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(qbs$success_per_play), color = "red", linetype = "dashed") +
  geom_point(color = ifelse(qbs$posteam == "SF", "red", "black"), cex=qbs$n_plays/60, alpha=1/4) +
  geom_text_repel(aes(label=name),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "QB success rate and EPA/play",
       subtitle = "2018, min 100 pass attempts, includes all QB's rush and pass plays") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))

ggsave('FILENAME.png', dpi=1000)

first <- 2009 #first season to grab. min available=2009
last <- 2019 # most recent season

datalist = list()
for (yr in first:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards,-blocked_player_id,-fumble_recovery_2_player_id)
  datalist[[yr]] <- pbp # add it to your list
}

pbp_all <- dplyr::bind_rows(datalist)

pbp_all %>% group_by(home_team) %>%summarize(n=n(), seasons=n_distinct(season), minyr=min(season), maxyr=max(season)) %>% 
  arrange(seasons)

pbp_all <- pbp_all %>% 
  mutate_at(vars(home_team, away_team, posteam, defteam), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  ))) 

saveRDS(pbp_all, file="NFLscrapRdata.rds")
pbp_all <- readRDS("NFLscrapRdata.rds")

pbp_all_rp <- pbp_all %>%
  filter(!is_na(epa), !is_na(posteam), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0),
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		      (up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
    play=1
  ) %>%
  filter(pass==1 | rush==1)

pbp_all_rp %>% filter(pass==1 & !is.na(passer_player_name))%>% mutate(
  arod = if_else(posteam=="GB"&passer_player_name=="A.Rodgers",1,0),
  early = if_else(season<=2014,1,0)
) %>%
  group_by(arod,early) %>%
  summarize(mean_epa=mean(epa), ypp=mean(yards_gained, na.rm = TRUE)) %>% arrange(-early)

saveRDS(pbp_all_rp, file="NFLscrapRdata.rds")
pbp_all_rp <- readRDS("NFLscrapRdata.rds")

draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
# we do a left join here because the names won't always match but don't want to lose any nflscrapR rows
pbp_all_rp <- pbp_all_rp %>%
  left_join(draft_picks,by=c("posteam"="team","name"="name"))

draft_values <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_values.csv")
draft_picks <- draft_picks %>%
  inner_join(draft_values,by=c("pick"="pick"))

games <- read_csv("http://www.habitatring.com/games.csv")
pbp_all_rp <- pbp_all_rp %>%
  inner_join(games,by=c("game_id"="game_id","away_team"="away_team","home_team"="home_team"))

logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv")
pbp_all_rp <- pbp_all_rp %>%
  inner_join(logos,by=c("posteam"="team"))

rosters <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/rosters.csv")
# we do a left join here because the names won't always match but don't want to lose any nflscrapR rows
pbp_all_rp <- pbp_all_rp %>%
  left_join(rosters,by=c("season"="season","posteam"="team","name"="name"))

standings <- read_csv("http://www.habitatring.com/standings.csv")
pbp_all_rp <- pbp_all_rp %>%
  inner_join(standings,by=c("season"="season","posteam"="team"))



# cally <- pbp_rp %>%
#   filter(game_date>= as.Date("2019-10-07") & down<=4 & qtr<=4 & half_seconds_remaining>120) %>%
#   nflypr = sum(rush*yards_gained)/ sum(rush)
#   nflypp = sum(pass*yards_gained)/ sum(pass)
#   group_by(posteam) %>%
#   # summarize(sumrush = sum(rush), sumpass = sum(pass)) %>%
#   summarize(ypr = sum(rush*yards_gained)/ sum(rush), ypp = sum(pass*yards_gained)/ sum(pass), pvr = ypp-ypr) %>%
#   arrange(pvr) 
# cally

