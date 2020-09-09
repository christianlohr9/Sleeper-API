# Get all Players from Sleeper API

#######################
# Player Pool
#######################

#players <- readRDS("Z:/Podcast/Sleeper API/players_2020.rds")

get_players <- "https://api.sleeper.app/v1/players/nfl"

players <- httr::GET(get_players)

jsontext <- content(players, as = "text")

players_df <- jsontext %>%
  gather_object("player_id") %>%              # collapses a JSON array into index-value pairs
  spread_values(firstname = jstring("first_name"),
                lastname = jstring("last_name"),
                birthdate = jstring("birth_date"),
                birth_country = jstring("birth_country"),
                birth_state =jstring("birth_state"),
                birth_city = jstring("birth_city"),
                pos = jstring("position"),
                team = jstring("team"),
                number = jstring("number"),
                status = jstring("status"),
                depth_chart = jstring("depth_chart_order"),
                height = jstring("height"),
                weight = jstring("weight"),
                highschool = jstring("high_school"),
                college = jstring("college"),
                experience = jstring("years_exp"),
                practice = jstring("practice_participation"),
                injurystatus = jstring("injury_status"),
                injurybodypart = jstring("injury_body_part"),
                injurystart = jstring("injury_start_date"),
                espn_id = jstring("espn_id"),
                fantasy_data_id = jstring("fantasy_data_id"),
                gsis_id = jstring("gsis_id"),
                pandascore_id = jstring("pandascore_id"),
                rotowire_id = jstring("rotowire_id"),
                rotoworld_id = jstring("rotoworld_id"),
                sportsradar_id = jstring("sportradar_id"),
                stats_id = jstring("stats_id"),
                yahoo_id = jstring("yahoo_id")
                )     # extract OwnerIDs

players_df$document.id <- NULL
players_df$..JSON <- NULL
players_df <- as.data.frame(players_df)

# change experience to "in league since":
players_df <- as.data.frame(players_df) %>%
  mutate(act_year = as.numeric(lubridate::year(Sys.Date())),
         inleaguesince = act_year - as.numeric(experience))

# write_rds(players_df, "Z:/Podcast/Sleeper API/players_2020.rds")
# write.csv2(players_df, "Z:/Podcast/Sleeper API/players_2020.csv")


#######################
# Transactions
#######################
weeks <- 1:16

leagues <- c("597038861834567680", "597041380413464576",
             "597040014550945792", "597034773214453760",
             "597043384380612608", "597041707346874368",
             "597040711145152512", "597041969868361728",
             "597038205220470784")
url <- "https://api.sleeper.app/v1/league/"

get_transactions <- function(leagues,week){
  transactions <- httr::GET(paste0(url,leagues,"/transactions/",week))
  
  # Wir ben?tigen den "JSON"-Text:
  jsontext <- content(transactions, as = "text")
  
  # Im ersten Schritt geht es an die grundlegenden Daten der Transaktionen
  transactions_df <- jsontext %>% 
    gather_array %>%            
    spread_values(type = jstring("type"),
                  transaction_id = jstring("transaction_id"),
                  status = jstring("status"),
                  creator = jstring("creator")) %>%
    enter_object("roster_ids") %>% 
      gather_array("child.num") %>% 
      append_values_string("roster_id") %>% 
    select(-document.id, -array.index)
  transactions_df$..JSON <- NULL
  
  # da scheinbar nicht in zwei Objekte reingegangen werden kann muss hier das
  # Objekt "adds" nochmal gesondert ausgewertet werden. Später joinen wir auf
  # Basis der roster- und transaction-ID

  transactions_df_temp <- jsontext %>%
    gather_array %>%
    spread_values(transaction_id = jstring("transaction_id")) %>%
  enter_object("adds") %>%
      gather_object("player_id") %>%
      append_values_string("roster_id") %>%
  select(-document.id, -array.index)
  transactions_df_temp$..JSON <- NULL

  transactions_df <- transactions_df %>%
    left_join(transactions_df_temp, by = c("transaction_id" = "transaction_id",
                                       "roster_id" = "roster_id")) %>%
    rename(adds = player_id)

  # dasselbe wie mit den "adds" nun auch für die "drops" inklusive Join

  transactions_df_temp <- jsontext %>%
    gather_array %>%
    spread_values(transaction_id = jstring("transaction_id")) %>%
    enter_object("drops") %>%
    gather_object("player_id") %>%
    append_values_string("roster_id") %>%
    select(-document.id, -array.index)
  transactions_df_temp$..JSON <- NULL

  transactions_df <- transactions_df %>%
    left_join(transactions_df_temp, by = c("transaction_id" = "transaction_id",
                                           "roster_id" = "roster_id")) %>%
    rename(drops = player_id)

  # dasselbe wie mit den "adds" und "drops" nun auch für die "settings"
  # um waiver bids zu bekommen. Inklusive Join

  transactions_df_temp <- jsontext %>%
    gather_array %>%
    spread_values(transaction_id = jstring("transaction_id")) %>%
    enter_object("settings") %>%
    gather_object("waiver_bid") %>%
    append_values_string("waiver_bid") %>%
    select(-document.id, -array.index)
  transactions_df_temp$..JSON <- NULL

  transactions_df <- transactions_df %>%
    left_join(transactions_df_temp, by = c("transaction_id" = "transaction_id"))


# Clean-Up:
  
  transactions_df <- as.data.frame(transactions_df) %>% 
    mutate(wk = week,
           league_id = leagues)
  
# Get users to join user_id with names and ava (for 'get_users' take a look
# at 'sleeper_users-rosters-record.r')
  all_rosters <- purrr::map_dfr(leagues, get_rosters)
  
  transactions_df <- all_rosters %>% 
    select(league_id, roster_id) %>% 
    left_join(transactions_df, by = c("league_id" = "league_id",
                                      "roster_id" = "roster_id"))
  
  return(transactions_df)
}

all_transactions <- purrr::pmap_dfr(purrr::transpose(
  purrr::cross2(leagues, weeks)), get_transactions)

