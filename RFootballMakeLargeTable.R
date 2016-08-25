rm(list = ls())
setwd("C:/Users/mbarger/Documents/RFootball")
load("england.rda")
load("germany.rda")
load("holland.rda")
load("italy.rda")
load("spain.rda")
england <- england %>% mutate(Country = "ENG")
germany <- germany %>% mutate(Country = "GER")
holland <- holland %>% mutate(Country = "NED")
italy <- italy %>% mutate(Country = "ITA")
spain <- spain %>% mutate(Country = "ESP")
england <- england %>% select(Date, Season, home, visitor, FT, hgoal, vgoal, tier, Country)
spain <- spain %>% select(Date, Season, home, visitor, FT, hgoal, vgoal, tier, Country)
euro <- tbl_df(rbind(england,germany,holland,italy,spain))
euro <- euro %>% 
          filter(Season >= 1996, tier == 1) %>%
          select(-tier)
home <- euro %>%
          select(Date, Season, Country, Team = home, Opp = visitor, GF = hgoal, GA = vgoal) %>%
          mutate(GD = GF - GA) %>%
          mutate(HA = "Home")
away <- euro %>%
          select(Date, Season, Country, Team = visitor, Opp = home, GF = vgoal, GA = hgoal) %>%
          mutate(GD = GF - GA) %>%
          mutate(HA = "Away")
euro <- rbind(home, away)
rm(home,away,england,germany,holland,italy,spain)
euro.tbl <- euro %>%
              group_by(Season,Country,Team) %>%
                summarise(GP = sum(GD<=100),
                          W = sum(GD > 0),
                          L = sum(GD < 0),
                          D = sum(GD == 0),
                          GF = sum(GF),
                          GA = sum(GA),
                          GD = sum(GD)
                ) %>%
                mutate(PTS = W * 3 + D) %>%
                arrange(-PTS, -GD, -GF)
                