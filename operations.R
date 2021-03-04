library(rvest)
library(tidyverse)
library(glue)

seed_crawler <- function(url){
  base_url <- "https://www.sherdog.com/fighter/"
  fighter_url <- glue(base_url,"{url}")
  
  tmp_seed<-read_html(fighter_url) %>% 
    html_nodes("td:nth-child(2) a , .cnaccept") %>% 
    html_attr(.,"href") %>% as.data.frame() %>% 
    unique()
}

# Update with pauses and sessions
fighter_crawler <- function(url){
  base_url <- "https://www.sherdog.com"
  fighter_url <- glue(base_url,"{url}")
  
  tmp <-read_html(fighter_url) %>% html_nodes("td:nth-child(2) a , .cnaccept") %>% html_attr(.,"href") %>% as.data.frame()
  nam<-paste0(fighter_url,"_")
  assign(nam,tmp)
}

# update with pauses and sessions
url_crawler <- function(fighter){
  
  base_url <- "https://www.sherdog.com"
  full_url <- glue(base_url,"{fighter}")
  
  read_html(full_url) %>% 
    html_nodes("body > div.container > div:nth-child(3) > div.col_left > section:nth-child(4) > div > div.content.table") %>% html_children() %>% 
    html_table(header = TRUE, fill=TRUE) %>% 
    as.data.frame()
}


# provide initial seed
url <- "Ricky-Simon-111209"

# modded seed function
seed_crawler_mod <- function(url){
  
  curr_session<- html_session("https://www.sherdog.com/events")
  base_url <- "https://www.sherdog.com/fighter/"
  
  print(curr_session$url)
  
  fighter_url <- glue(base_url,"{url}")
  
  print(fighter_url)
  
  t0 <- Sys.time()
  
  curr_session %>% jump_to(fighter_url) %>% 
    read_html() %>% html_nodes("td:nth-child(2) a , .cnaccept") %>% 
    html_attr(.,"href") %>% as.data.frame() %>% 
    unique() -> tmp_seed
  
  t1 <- Sys.time()
  print(t1-t0)
  
  Sys.sleep(2*(t1-t0))
  
  return(tmp_seed)
  
}

seed_crawler_mod(url) -> out_tmp
names(out_tmp)[1]<-"fighterName"
rownames(out_tmp)<-str_split(out_tmp$fighterName,"/",simplify = TRUE)[,3]


# modded fighter function
fighter_crawler_mod <- function(url){
  
  curr_session<- html_session("https://www.sherdog.com/events")
  base_url <- "https://www.sherdog.com"
  fighter_url <- glue(base_url,"{url}")
  
  #print(fighter_url)
  
  t0 <- Sys.time()
  
  curr_session %>% jump_to(fighter_url) %>% 
    read_html(fighter_url) %>% 
    html_nodes("td:nth-child(2) a , .cnaccept") %>% 
    html_attr(.,"href") %>% as.data.frame() ->tmp
  
  nam <-paste0(fighter_url,"_")
  assign(nam,tmp)
  
  t1 <- Sys.time()
  print(paste0(fighter_url," - ",t1-t0))
  
  Sys.sleep(2*(t1-t0))
  
  return(tmp)
  
}


map_df(out_tmp$fighterName,fighter_crawler_mod) %>% unique %>% rowid_to_column() -> div_tmp 
names(div_tmp)[2]<-"fighterName"

library(httr)

# modded results crawler with ua
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
uaa<-ua$options$useragent

results_crawler <- function(url,uaa){
  
  curr_session<- html_session("https://www.sherdog.com/events",uaa)
  base_url <- "https://www.sherdog.com"
  full_url <- glue(base_url,"{url}")
  
  t0 <- Sys.time()
  
  curr_session %>% jump_to(full_url) %>% 
  read_html(full_url) %>% 
  html_nodes("body > div.container > div:nth-child(3) > div.col_left > section:nth-child(4) > div > div.content.table") %>% 
  html_children() %>% html_table(header = TRUE, fill=TRUE) %>% 
  as.data.frame() -> tmp
  
  t1 <- Sys.time()
  print(paste0(full_url," - ",t1-t0))
  
  Sys.sleep(5*(t1-t0))
  
  return(tmp)
  
}


map2_df(div_tmp$fighterName,uaa,results_crawler,.id = "rowid") -> results_tmp


results_tmp ->results_135

## save division results

setwd("C:/Users/eoedd/Desktop/locstore/projects/_octagon/data")
write.csv(results_135,"results_135.csv")


library(stringr)


div_tmp %>%  
  mutate(Opponent = str_trim(str_replace_all(str_remove_all(str_split(fighterName,"fighter/",simplify = TRUE)[,2],"[0-9]"),"-"," "))) -> div_135

write.csv(div_135,"div_135.csv")


## DATA PROCESSING

results_135 -> working_copy


working_copy %>% select(rowid,Event,Result,Fighter,Method.Referee,R,Time) %>% 
  mutate(Method=str_c(str_split(Method.Referee,"[)]",simplify = TRUE)[,1],")")) %>% 
  select(-Method.Referee) %>% rename(Round = R) %>% na.omit() ->working_copy


# Extract Dates of Events
working_copy %>% mutate(DateEvent= as.Date(str_replace_all(str_replace_all(str_sub(working_copy$Event,start = -15),"/","-")," ",""),format = "%b-%d-%Y")) -> working_copy

str_sub(working_copy$Event,start=-15) <-""


# Calculate Streaks - Prepare
working_copy[order(working_copy$DateEvent),] %>% 
  group_by(rowid) %>% 
  mutate(Outcome = case_when(Result=="win" ~ 1, TRUE ~0)) %>% 
  mutate(lagged = lag(Outcome)) %>% mutate(start=(Outcome != lagged)) -> working_copy

working_copy %>% mutate(start=replace_na(start,TRUE)) -> working_copy

working_copy %>% mutate(streak_id = cumsum(start)) ->working_copy


# Calculate Streak Length and Arrange
working_copy %>%  group_by(rowid,streak_id) %>% 
  mutate(streak=row_number()) %>% 
  ungroup() %>% 
  arrange(.,rowid,DateEvent) %>% 
  rename(PreviousOutcome=lagged) -> working_copy

# Previous Result
working_copy %>% group_by(rowid) %>% mutate(StreakLength = lag(streak)) %>% 
  mutate(StreakLength = case_when(PreviousOutcome == 0 ~ -StreakLength, TRUE ~ StreakLength)) -> working_copy


# Win Ratio
working_copy %>% ungroup() %>% group_by(rowid) %>%  
  mutate(WinRatio = lag(cumsum(Outcome))/(row_number()-1)) %>% 
  mutate(CountFights =row_number()-1) ->working_copy


# Finish Ratio and Finish Previous
working_copy %>% mutate(FinishRatio = lag(!str_detect(Method,"Decision") & Outcome == 1)) %>%
  mutate(FinishRatio=replace_na(FinishRatio,FALSE)) %>% 
  mutate(FinishRatio = cumsum(FinishRatio)/(CountFights)) %>% 
  mutate(FinishPrevious = case_when(!str_detect(Method,"Decision") & Outcome == 1 ~TRUE, TRUE ~ FALSE)) %>% 
  mutate(FinishPrevious=lag(FinishPrevious)) -> working_copy


# Finished Ratio and Finished Previous
working_copy %>% mutate(FinishedRatio = lag(!str_detect(Method,"Decision") & Outcome == 0)) %>% mutate(FinishedRatio = replace_na(FinishedRatio,FALSE)) %>% 
  mutate(FinishedRatio = cumsum(FinishedRatio)/(CountFights)) %>% 
  mutate(FinishedPrevious = case_when(!str_detect(Method,"Decision") & Outcome == 0 ~TRUE, TRUE ~ FALSE)) %>% 
  mutate(FinishedPrevious = lag(FinishedPrevious)) -> working_copy


working_copy %>% mutate(FinishedTrue = lag(!str_detect(Method,"Decision") & Outcome == 0)) %>% mutate(FinishedTrue = replace_na(FinishedTrue,FALSE)) %>% 
  mutate(FinishedCount=cumsum(FinishedTrue)) %>% 
  mutate(FinishTrue = lag(!str_detect(Method,"Decision") & Outcome == 1)) %>% mutate(FinishTrue = replace_na(FinishTrue,FALSE)) %>% 
  mutate(FinishCount=cumsum(FinishTrue)) %>% 
  mutate(DamageDiff = FinishCount/(FinishedCount+1)) %>% select(c(-FinishedTrue,-FinishTrue)) -> working_copy


write.csv(working_copy,"result-metrics_135.csv")


# BUILD RESULTS MATRIX

# Join to Get Fighter Name
working_copy %>% mutate(rowid = as.integer(rowid)) -> working_copy_x

div_135 %>% rename(Fighter = Opponent) -> div_135

working_copy_x %>% left_join(div_135,by="rowid") ->working_copy_x

working_copy_x %>% rename(Fighter=Fighter.y, Opponent=Fighter.x) -> working_copy_x


# Reset for Opponent Id
div_135 %>% rename(Opponent = Fighter) -> div_135


#Join For Opponent Id
working_copy_x %>% left_join(div_135,by="Opponent") -> working_copy_x

working_copy_x %>% rename(FighterId = rowid.x, OpponentId = rowid.y) -> working_copy_x

#Remove Unnecessary Columns
working_copy_x %>% select(-fighterName.x,-fighterName.y) -> working_copy_x


#Join For Composite
working_copy_x %>% 
  mutate(FighterComposite = str_c(working_copy_x$FighterId,
                                  working_copy_x$Opponent,
                                  working_copy_x$DateEvent,sep = "_"),
         OpponentComposite = str_c(working_copy_x$OpponentId,
                                   working_copy_x$Fighter,
                                   working_copy_x$DateEvent,sep = "_")) ->working_copy_x 

# Make Copies for Self Join
working_copy_x -> working_copy_y


# Join for Result Set
working_copy_x %>% left_join(working_copy_y,by=c("FighterComposite"="OpponentComposite")) -> raw_event_results


## Unduplicate resultset
## Derive a column to further distinguish outcomes on events

# the same for both rows
# ads uniqueness to results which finish in the same way on the sam e card i.e. decisions
raw_event_results %>% mutate(IdKey = FighterId.x+FighterId.y) -> raw_event_results

raw_event_results[!duplicated(raw_event_results[c("Event.x","Round.x","Time.x","Method.x","IdKey")]),] -> unique_event_results


# Filter down to reflect only complete cases
unique_event_results %>% filter(complete.cases(.)) -> complete_event_results_135
write.csv(complete_event_results_135,"complete-event-results_135.csv")


# Derived Additional Model Parameters
complete_event_results_135 %>% mutate(delta_FP=FinishPrevious.x-FinishPrevious.y,
                                  delta_FIP=FinishedPrevious.x-FinishedPrevious.y,
                                  delta_FC=FinishCount.x-FinishCount.y,
                                  delta_FIC=FinishedCount.x-FinishedCount.y) ->modelset_derived_135


# odds estimate
modelset_derived_135 %>%  
  mutate(Odds.x = 2-(WinRatio.x-WinRatio.y)-0.1*(PreviousOutcome.x-PreviousOutcome.y))  %>% 
  mutate(Odds.x.revised = case_when(Odds.x <1.0 ~ 1.1, TRUE ~ Odds.x)) -> modelset_derived_135

write.csv(modelset_derived_135,"modelset_derived_135.csv")


# choose modelset params
modelset_derived_135 %>% select(Result.x,
                            Method.x,
                            Fighter.x,
                            Opponent.x,
                            CountFights.x,
                            PreviousOutcome.x,
                            StreakLength.x,
                            WinRatio.x,
                            FinishRatio.x,
                            FinishPrevious.x,
                            FinishedRatio.x,
                            FinishPrevious.x,
                            FinishCount.x,
                            FinishedCount.x,
                            DamageDiff.x,
                            CountFights.y,
                            PreviousOutcome.y,
                            StreakLength.y,
                            WinRatio.y,
                            FinishRatio.y,
                            FinishPrevious.y,
                            FinishedRatio.y,
                            FinishPrevious.y,
                            FinishCount.y,
                            FinishedCount.y,
                            DamageDiff.y,
                            delta_FP,
                            delta_FIP,
                            delta_FC,
                            delta_FIC,
                            Odds.x.revised) ->modelset_selected_135

write.csv(modelset_selected_135,"modelset_selected_135.csv")
         

