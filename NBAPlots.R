library(nbastatR)
library(tidyverse)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(paletteer)
library(viridis)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 3)

NBA2022 <- bref_players_stats(seasons = 2023, tables = c("advanced", "totals"),
                              include_all_nba = F, only_totals = TRUE, nest_data = FALSE,
                              assign_to_environment = TRUE, widen_data = TRUE, join_data = TRUE,
                              return_message = TRUE) 



Advanced <- dataBREFPlayerAdvanced 
Advanced <- Advanced %>%
  select(slugPlayerSeason, namePlayer:countGames, idPlayerNBA, minutes:ratioVORP) %>%
  rename(Position = 'slugPosition', Player = namePlayer, Age = agePlayer, Games = countGames,
         Minutes = minutes, Team = slugTeamBREF, 'Win Share Ratio Per 48' = ratioWSPer48, 'VORP Ratio' = ratioVORP) %>%
  mutate(Position = ifelse(Position %in% c("C", "PF", "SF", "PF-SF", "PF-C", "SF-SG"), "Frontcourt", "Backcourt"))

Advanced <- Advanced %>%
  filter(Minutes >= 500) 

top10 <-  Advanced %>%
  #sort(WS, decreasing = TRUE) %>%
  group_by(Position) %>%
  summarize(top = top_n(Advanced,n=10,wt=`VORP Ratio`)) 

top10 <- top10$top$Player


Test <- Advanced %>%
  ggplot(aes(`Win Share Ratio Per 48`, `VORP Ratio`, text = paste("Player: ", Player), color = Minutes)) +
  geom_point(size  = 2) +
  scale_x_continuous(breaks = c(seq(-.05,.35, .1)), limits = c(-.05,.35)) +
  scale_y_continuous(limits = c(-.5,9), breaks = c(seq(-.5,9,1))) +
  scale_color_viridis(option = "plasma", labels = scales::comma) +
  labs(title = "VORP Ratio vs Win Share (Per 48) Ratio", subtitle = "Players with <500 minutes logged (as of 1.8.23) are omitted") +
  theme_fivethirtyeight(base_size = 40, base_family = 'serif') +
  theme(axis.title = element_text(family = 'serif', size = 50), 
        axis.title.y.right = element_text(vjust=unit(1.5, "cm")), axis.text = element_text(size = 20),
        legend.text = element_text(size = 20), legend.title = element_text(size = 40), legend.position = "right", legend.direction = "vertical",
        legend.key.height = unit(3, "cm"), legend.key.width =  unit(3, "cm")) +
  facet_grid(~Position) +
  gghighlight::gghighlight(Player %in% top10, label_key = Player,
                           label_params = list(fill = ggplot2::alpha("white", 0.8),
                                               box.padding = 0,
                                               family = "Roboto Condensed"))

Test

ggplotly(Test, tooltip = c('text', 'x', 'y', 'color'))

ggplotly(Test, tooltip = c('text', 'x', 'y', 'color')) %>%
  layout(xaxis = list(autorange = FALSE),
         yaxis = list(autorange = TRUE))

Advanced %>%
  group_by(Position) %>%
  do(p=plot_ly(., x = ~`Win Share Ratio Per 48`, y = ~`VORP Ratio`, color = ~Minutes, type = "scatter",)) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>%
  layout(theme = "plotly_dark")



