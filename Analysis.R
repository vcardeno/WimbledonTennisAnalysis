library(readr)
library(dplyr)
library(tidyverse)
library(naniar)

#read in data
m2015 <- read_csv('wimbledonmatches2015.csv')
p2015 <- read_csv('wimbledonpoints2015.csv')
m2016 <- read_csv('wimbledonmatches2016.csv')
p2016 <- read_csv('wimbledonpoints2016.csv')
m2017 <- read_csv('wimbledonmatches2017.csv')
p2017 <- read_csv('wimbledonpoints2017.csv')
m2018 <- read_csv('wimbledonmatches2018.csv')
p2018 <- read_csv('wimbledonpoints2018.csv')
m2019 <- read_csv('wimbledonmatches2019.csv')
p2019 <- read_csv('wimbledonpoints2019.csv')
m2021 <- read_csv('wimbledonmatches2021.csv')
p2021 <- read_csv('wimbledonpoints2021.csv')
m2022 <- read_csv('wimbledonmatches2022.csv')
p2022 <- read_csv('wimbledonpoints2022.csv')

#join data for year on match_id
t2015 <- merge(m2015, p2015, by = "match_id")
t2016 <- merge(m2016, p2016, by = "match_id")
t2017 <- merge(m2017, p2017, by = "match_id")
t2018 <- merge(m2018, p2018, by = "match_id")
t2019 <- merge(m2019, p2019, by = "match_id")
t2021 <- merge(m2021, p2021, by = "match_id")
t2022 <- merge(m2022, p2022, by = "match_id")


#calculate winner 
calculatewinner <- function(dataset){
  temp <- dataset %>% group_by(match_id, SetWinner) %>% summarise(n = sum(SetWinner))
  temp$n[temp$SetWinner == 2] <- temp$n[temp$SetWinner == 2] / 2
  temp <- temp %>% filter(SetWinner != 0)
  temp <- temp %>% group_by(match_id) %>% arrange(desc(n)) %>% filter(row_number() == 1) %>% mutate(MatchWinner = SetWinner) %>% select(match_id, MatchWinner)
  new <- merge(dataset, temp, by = "match_id") 
  new$wonby <- ifelse(new$MatchWinner == 1, new$player1, new$player2)
  return(new)
}

t2015 <- calculatewinner(t2015)
t2016 <- calculatewinner(t2016)
t2017 <- calculatewinner(t2017)
t2018 <- calculatewinner(t2018)
t2019 <- calculatewinner(t2019)
t2021 <- calculatewinner(t2021)
t2022 <- calculatewinner(t2022)


#now you have to aggregate
aggregate <- function(data){
  data <- data %>% filter(PointNumber != '0X' & PointNumber != '0Y')
  serve_speed <- data %>% mutate(Speed_MPH = na_if(Speed_MPH, 0)) %>% group_by(match_id, PointServer, ServeNumber) %>% mutate(avg_serve_speed = mean(Speed_MPH, na.rm = TRUE)) %>% select(match_id, PointServer, avg_serve_speed) %>% distinct() %>% filter(ServeNumber != 0 & PointServer != 0) %>% spread(ServeNumber, avg_serve_speed) %>% rename(avg_first_serve_speed = `1`, avg_second_serve_speed = `2`)  
  num_aces <- data %>% group_by(match_id, PointServer) %>% summarise(aces = sum(P1Ace) + sum(P2Ace)) %>% filter(PointServer != 0)
  num_dfs <- data %>% group_by(match_id, PointServer) %>% summarise(dfs = sum(P1DoubleFault) + sum(P2DoubleFault)) %>% filter(PointServer != 0)
  num_unforced <- data %>% group_by(match_id) %>% summarise(p1_unf = sum(P1UnfErr), p2_unf = sum(P2UnfErr)) %>% pivot_longer(cols = c(p1_unf, p2_unf), names_to = "PointServer", values_to = "unforced_errors") %>% mutate(PointServer = replace(PointServer, PointServer == 'p1_unf', 1)) %>% mutate(PointServer = replace(PointServer, PointServer == 'p2_unf', 2)) %>% mutate_at('PointServer', as.numeric)
  net_perc <- data %>% group_by(match_id) %>% summarise(p1_net_perc = sum(P1NetPointWon) / sum(P1NetPoint), p2_net_perc = sum(P2NetPointWon) / sum(P2NetPoint)) %>% pivot_longer(cols = c(p1_net_perc, p2_net_perc), names_to = "PointServer", values_to = "net_perc") %>% mutate(PointServer = replace(PointServer, PointServer == 'p1_net_perc', 1)) %>% mutate(PointServer = replace(PointServer, PointServer == 'p2_net_perc', 2)) %>% mutate_at('PointServer', as.numeric)
  bp_perc <- data %>% group_by(match_id) %>% summarise(p1_bp_perc = sum(P1BreakPointWon) / sum(P1BreakPoint), p2_bp_perc = sum(P2BreakPointWon) / sum(P2BreakPoint)) %>% pivot_longer(cols = c(p1_bp_perc, p2_bp_perc), names_to = "PointServer", values_to = "bp_perc") %>% mutate(PointServer = replace(PointServer, PointServer == 'p1_bp_perc', 1)) %>% mutate(PointServer = replace(PointServer, PointServer == 'p2_bp_perc', 2)) %>% mutate_at('PointServer', as.numeric)
  #agg <- data %>% group_by(match_id, PointWinner, WinnerShotType) %>% summarise(num_winners = sum(P1Winner) + sum(P2Winner))
  temp <- data %>% group_by(match_id, PointServer) %>% summarise(total = n_distinct(PointNumber))
  temp2 <- data %>% group_by(match_id, PointServer, ServeNumber) %>% summarise(serve = n_distinct(PointNumber)) %>% filter(ServeNumber == 1)
  temp3 <- merge(temp, temp2, by = c("match_id", "PointServer"))
  first_serve_perc <- temp3 %>% group_by(match_id, PointServer) %>% summarise(first_serve_perc = serve / total)
  temp <- data %>% group_by(match_id, PointServer, ServeNumber, PointWinner) %>% summarise(n = n_distinct(PointNumber))
  temp2 <- data %>% group_by(match_id, PointServer, ServeNumber) %>% summarise(n2 = n_distinct(PointNumber))
  temp3 <- merge(temp, temp2, by = c("match_id", "PointServer", "ServeNumber"))
  first_serve_won_perc <- temp3 %>% mutate(first_serve_won_perc = n / n2) %>% filter(PointServer == 1 | PointServer == 2) %>% filter(ServeNumber == 1) %>% filter(PointServer == PointWinner) %>% select(match_id, PointServer, first_serve_won_perc)
  second_serve_won_perc <- temp3 %>% mutate(second_serve_won_perc = n / n2) %>% filter(PointServer == 1 | PointServer == 2) %>% filter(ServeNumber == 2) %>% filter(PointServer == PointWinner) %>% select(match_id, PointServer, second_serve_won_perc)
  num_winners <- data %>% group_by(match_id) %>% summarise(p1_winn = sum(P1Winner), p2_winn = sum(P2Winner)) %>% pivot_longer(cols = c(p1_winn, p2_winn), names_to = "PointServer", values_to = "winners") %>% mutate(PointServer = replace(PointServer, PointServer == 'p1_winn', 1)) %>% mutate(PointServer = replace(PointServer, PointServer == 'p2_winn', 2)) %>% mutate_at('PointServer', as.numeric)
  num_winners_bh <- data %>% filter(WinnerShotType == 'B') %>% group_by(match_id) %>% summarise(p1_winn_bh = sum(P1Winner), p2_winn_bh = sum(P2Winner)) %>% pivot_longer(cols = c(p1_winn_bh, p2_winn_bh), names_to = "PointServer", values_to = "winners_bh") %>% mutate(PointServer = replace(PointServer, PointServer == 'p1_winn_bh', 1)) %>% mutate(PointServer = replace(PointServer, PointServer == 'p2_winn_bh', 2)) %>% mutate_at('PointServer', as.numeric)
  num_winners_fh <- data %>% filter(WinnerShotType == 'F') %>% group_by(match_id) %>% summarise(p1_winn_fh = sum(P1Winner), p2_winn_fh = sum(P2Winner)) %>% pivot_longer(cols = c(p1_winn_fh, p2_winn_fh), names_to = "PointServer", values_to = "winners_fh") %>% mutate(PointServer = replace(PointServer, PointServer == 'p1_winn_fh', 1)) %>% mutate(PointServer = replace(PointServer, PointServer == 'p2_winn_fh', 2)) %>% mutate_at('PointServer', as.numeric)
  distance_run <- data %>% mutate(P1DistanceRun = na_if(P1DistanceRun, 0)) %>% mutate(P2DistanceRun = na_if(P2DistanceRun, 0)) %>% group_by(match_id) %>% summarise(p1_avg_dist = mean(P1DistanceRun, na.rm = TRUE), p2_avg_dist = mean(P2DistanceRun, na.rm = TRUE)) %>% pivot_longer(cols = c(p1_avg_dist, p2_avg_dist), names_to = "PointServer", values_to = "avg_dist_run") %>% mutate(PointServer = replace(PointServer, PointServer == 'p1_avg_dist', 1)) %>% mutate(PointServer = replace(PointServer, PointServer == 'p2_avg_dist', 2)) %>% mutate_at('PointServer', as.numeric)
  avg_rally_length_p1serve <- data %>% filter(PointServer == 1) %>% group_by(match_id) %>% summarise(p1_serve_avg_rally_length = mean(RallyCount), p2_return_avg_rally_length = mean(RallyCount))  
  avg_rally_length_p2serve <- data %>% filter(PointServer == 2) %>% group_by(match_id) %>% summarise(p2_serve_avg_rally_length = mean(RallyCount), p1_return_avg_rally_length = mean(RallyCount))
  avg_rally_length <- merge(avg_rally_length_p1serve, avg_rally_length_p2serve, by = "match_id")
  avg_rally_length_p1 <- avg_rally_length %>% select(match_id, p1_serve_avg_rally_length, p1_return_avg_rally_length) %>% mutate(PointServer = 1)
  avg_rally_length_p2 <- avg_rally_length %>% select(match_id, p2_serve_avg_rally_length, p2_return_avg_rally_length) %>% mutate(PointServer = 2)
  avg_rally_length_final <- union_all(avg_rally_length_p1, avg_rally_length_p2) %>% replace_with_na_at(., c("p1_serve_avg_rally_length", "p2_serve_avg_rally_length", "p1_return_avg_rally_length", "p2_return_avg_rally_length"), condition = ~.x < 0.5)
  perc_serve_direc_first <- data %>% group_by(match_id, PointServer, ServeNumber, ServeWidth) %>% summarise(count = n_distinct(PointNumber)) %>% filter(PointServer != 0 & ServeNumber != 0) %>% mutate(perc = count/sum(count)) %>% spread(ServeWidth, perc) %>% filter(ServeNumber == 1) %>% rename(perc_first_body = `B`, perc_first_body_center = `BC`, perc_first_body_wide = `BW`, perc_first_tee = `C`, perc_first_wide = `W`) %>% ungroup() %>% select(-c(ServeNumber, count, `<NA>`)) %>% group_by(match_id, PointServer) %>% summarise(perc_first_body = max(perc_first_body, na.rm = TRUE), perc_first_body_center = max(perc_first_body_center, na.rm = TRUE), perc_first_body_wide = max(perc_first_body_wide, na.rm = TRUE), perc_first_tee = max(perc_first_tee, na.rm = TRUE), perc_first_wide = max(perc_first_wide, na.rm = TRUE)) %>% na_if(., -Inf)
  perc_serve_direc_second <- data %>% group_by(match_id, PointServer, ServeNumber, ServeWidth) %>% summarise(count = n_distinct(PointNumber)) %>% mutate(perc = count/sum(count)) %>% filter(PointServer != 0 & ServeNumber != 0) %>% spread(ServeWidth, perc) %>% filter(ServeNumber == 2) %>% rename(perc_second_body = `B`, perc_second_body_center = `BC`, perc_second_body_wide = `BW`, perc_second_tee = `C`, perc_second_wide = `W`) %>% ungroup() %>% select(-c(ServeNumber, count, `<NA>`)) %>% group_by(match_id, PointServer) %>% summarise(perc_second_body = max(perc_second_body, na.rm = TRUE), perc_second_body_center = max(perc_second_body_center, na.rm = TRUE), perc_second_body_wide = max(perc_second_body_wide, na.rm = TRUE), perc_second_tee = max(perc_second_tee, na.rm = TRUE), perc_second_wide = max(perc_second_wide, na.rm = TRUE)) %>% na_if(., -Inf)
  perc_serve_ctl <- data %>% group_by(match_id, PointServer, ServeDepth) %>% summarise(count = n_distinct(PointNumber)) %>% mutate(perc_ctl = count/sum(count)) %>% filter(ServeDepth == 'CTL') %>% select(match_id, PointServer, perc_ctl)
  perc_return_deep <- data %>% group_by(match_id, PointServer, ReturnDepth) %>% summarise(count = n_distinct(PointNumber)) %>% mutate(perc_deep = count/sum(count)) %>% filter(ReturnDepth == 'D') %>% mutate(PointReturner = ifelse(PointServer == '1', '2', '1')) %>% select(match_id, PointReturner, perc_deep) %>% ungroup() %>% select(-c(PointServer)) %>% rename(PointServer = PointReturner) %>% mutate_at('PointServer', as.numeric)
  #now have to join all of above
  all <- list(serve_speed, num_aces, num_dfs, num_unforced, net_perc, bp_perc, first_serve_perc, first_serve_won_perc, second_serve_won_perc, num_winners, num_winners_bh, num_winners_fh, distance_run, avg_rally_length_final, perc_serve_direc_first, perc_serve_direc_second, perc_serve_ctl, perc_return_deep) %>% reduce(left_join, by = c("match_id", "PointServer"))
  winner <- data %>% group_by(match_id) %>% summarise(wonby = max(wonby))
  all <- merge(all, winner, by = "match_id")
  player <- data %>% group_by(match_id) %>% summarise(player1 = max(player1), player2 = max(player2))
  all <- merge(all, player, by = "match_id")
  all$player <- ifelse(all$PointServer == 1, all$player1, all$player2)
  all <- all %>% select(-c(player1, player2))
  #all$avg_rally_length_p1serve <- ifelse(is.na(all$perc_first_body) & is.na(all$perc_first_body_center) &  is.na(all$perc_first_body_wide) & is.na(all$perc_first_tee) & is.na(all$perc_first_wide), NA, all$avg_rally_length_p1serve)
  all <- all %>% mutate(p1_serve_avg_rally_length = ifelse(is.na(perc_first_body) & is.na(perc_first_body_center) &  is.na(perc_first_body_wide) & is.na(perc_first_tee) & is.na(perc_first_wide), NA, p1_serve_avg_rally_length)) %>% mutate(p2_serve_avg_rally_length = ifelse(is.na(perc_first_body) & is.na(perc_first_body_center) &  is.na(perc_first_body_wide) & is.na(perc_first_tee) & is.na(perc_first_wide), NA, p2_serve_avg_rally_length)) %>% mutate(p1_return_avg_rally_length = ifelse(is.na(perc_first_body) & is.na(perc_first_body_center) &  is.na(perc_first_body_wide) & is.na(perc_first_tee) & is.na(perc_first_wide), NA, p1_return_avg_rally_length)) %>% mutate(p2_return_avg_rally_length = ifelse(is.na(perc_first_body) & is.na(perc_first_body_center) &  is.na(perc_first_body_wide) & is.na(perc_first_tee) & is.na(perc_first_wide), NA, p2_return_avg_rally_length))
  #all$avg_rally_length_p1return <- ifelse(is.na(all$perc_first_body) & is.na(all$perc_first_body_center) &  is.na(all$perc_first_body_wide) & is.na(all$perc_first_tee) & is.na(all$perc_first_wide), NA, all$avg_rally_length_p1return)
  #all$avg_rally_length_p2serve <- ifelse(is.na(all$perc_first_body) & is.na(all$perc_first_body_center) &  is.na(all$perc_first_body_wide) & is.na(all$perc_first_tee) & is.na(all$perc_first_wide), NA, all$avg_rally_length_p2serve)
  #all$avg_rally_length_p2return <- ifelse(is.na(all$perc_first_body) & is.na(all$perc_first_body_center) &  is.na(all$perc_first_body_wide) & is.na(all$perc_first_tee) & is.na(all$perc_first_wide), NA, all$avg_rally_length_p2return)
  return(all)
  }



f2015 <- aggregate(t2015)
f2016 <- aggregate(t2016)
f2017 <- aggregate(t2017)
f2018 <- aggregate(t2018)
f2019 <- aggregate(t2019)
f2021 <- aggregate(t2021)
f2022 <- aggregate(t2022)


  

f2016_mens <- subset(f2016, substr(f2016$match_id, 16, 16) == "1")
f2016_womens <- subset(f2016, substr(f2016$match_id, 16, 16) == "2")
f2017_mens <- subset(f2017, substr(f2017$match_id, 16, 16) == "1")
f2017_womens <- subset(f2017, substr(f2017$match_id, 16, 16) == "2")
f2018_mens <- subset(f2018, substr(f2018$match_id, 16, 16) == "1")
f2018_womens <- subset(f2018, substr(f2018$match_id, 16, 16) == "2")
f2019_mens <- subset(f2019, substr(f2019$match_id, 16, 16) == "1")
f2019_womens <- subset(f2019, substr(f2019$match_id, 16, 16) == "2")
f2021_mens <- subset(f2021, substr(f2021$match_id, 16, 16) == "1")
f2021_womens <- subset(f2021, substr(f2021$match_id, 16, 16) == "2")
f2022_mens <- subset(f2022, substr(f2022$match_id, 16, 16) == "1")
f2022_womens <- subset(f2022, substr(f2022$match_id, 16, 16) == "2")


wimbledon_mens <- rbind(f2016_mens, f2017_mens, f2018_mens, f2019_mens, f2021_mens, f2022_mens)

write_csv(wimbledon_mens, "wimbledon_mens_v2.csv")


t2021 %>% filter(match_id == '2021-wimbledon-1108') %>% View(.)


table(wimbledon_mens$p1_serve_avg_rally_length)

substr(f2016$match_id, 16, 16)

View(serve_speed)











