library(tidyverse)
library(gt)


pbp <- readRDS(url('https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2020.rds?raw=true')) %>%
    filter(week == 20, !is.na(epa))

td <- pbp %>%
    filter(down == 1, ydstogo == 10, score_differential <= 14, rush == 1, game_seconds_remaining > 120) %>%
    group_by(posteam) %>%
    summarise(
        yds = sum(yards_gained),
        n = n(),
        avg = mean(yards_gained),
        med = median(yards_gained),
        p25 = quantile(yards_gained, 0.25),
        p75 = quantile(yards_gained, 0.75),
        p90 = quantile(yards_gained, 0.9),
        epa_per = mean(epa),
        epa_total = sum(epa)
        ) %>%
    ungroup()

td %>%
    rename(
        Team=posteam, TotalYards=yds, Plays=n, Avg=avg, Median=med,
        '25%'=p25, '75%'=p75, '90%'=p90, 'EPA/play'=epa_per, 'Total EPA'=epa_total
        ) %>%
    gt() %>%
    gt::tab_header('Runs on 1/10 during Championship Round', subtitle = 'Half of the runs gained at most 3 yards') %>%
    gt::tab_source_note('(1) When score differential is at most 14') %>%
    gt::tab_source_note('(2) At least 120s remaining on the game') %>%
    gt::tab_source_note(md('<br>Author: **@gppeixoto** | Data: nflfastR')) %>%
    gt::fmt_number(columns=vars(Avg, 'EPA/play', 'Total EPA'), decimals = 3)
