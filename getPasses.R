library(XML)
library(dplyr)
require(plyr)

getPasses <- function(url) {
  
  xml <- xml <- xmlParse(url)
  
  # extract values from XML doc
  df <- xml %>% 
    xpathSApply("//*/all_passes//time_slice//event", dumFun) %>%
    t %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    sapply(unlist) %>% 
    plyr::ldply(rbind) %>%
    as.data.frame

  # get team and player names
  playerdf <- getPlayerNames(xml)
  teamdf <- getTeamNames(xml)
  df$team <- playerdf$team[match(df$team_id, playerdf$team_id)]
  df$player <- playerdf$player[match(df$player_id, playerdf$player_id)]
  
  # get start and end x,y-coords
  df$x.start <- sub(",.*", "", df$start)
  df$y.start <- sub(".*,", "", df$start)
  df$x.end <- sub(",.*", "", df$end)
  df$y.end <- sub(".*,", "", df$end)

  # convert classes
  df[,c("mins","secs","x.start","x.end","y.start","y.end")] <- as.numeric(as.character(unlist(df[,c("mins","secs","x.start","x.end","y.start","y.end")])))
  df$assists <- as.factor(ifelse(is.na(df$assists), 0, 1))
  df$k <- as.factor(ifelse(is.na(df$k), 0, 1))
  df$headed <- as.factor(ifelse(is.na(df$headed), 0, 1))
  df$injurytime_play <- as.factor(ifelse(is.na(df$injurytime_play), 0, 1))
  
  df %>%
    mutate(time = as.numeric(as.character(minsec))) %>%
    arrange(time) %>%
    mutate(receiver = lead(player)) %>%
    mutate(completed = if_else(type=="completed", 1, 0)) %>%
    select(team, passer = player, receiver, time, mins, secs, x.start, y.start, x.end, y.end, completed, assist = assists, key = k, headed, throw = throw_ins)
}

getPlayerNames <- function(xml) {
  players.xml  <- xpathSApply(xml, "//players//player")
  
  lapply(1:length(players.xml), function(x) {
    players.xname <- xmlName(players.xml[[x]])
    players.xattrs <- xmlAttrs(players.xml[[x]])
    
    df <- c(sapply(xmlChildren(players.xml[[x]]), xmlValue), name = players.xname, players.xattrs)
    data.frame(player_id = as.numeric(as.character(df["id"])), player = as.character(df["name"]), team_id = as.numeric(as.character(df["team_id"])), team = as.character(df["team_name"]))
  }) %>%
    plyr::rbind.fill()
}

getTeamNames <- function(xml) {
  team_id <- xpathSApply(xml, "//team/@id")
  team <- xpathSApply(xml, "//team//long_name", xmlValue)
  data.frame(team_id, team)
}

url <- "http://s3-irl-epl.squawka.com/dp/ingame/32991"
dat <- getPasses(url)
