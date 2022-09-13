library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(writexl)
library(plm)
library(lmtest)
library(rdrobust)
library(stargazer)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####2020 presidential election

#li's list and code

lilist_2020 <- read.csv("data/2020election/elbase.csv", header = FALSE, colClasses = c('character'))

lilist_2020 <- lilist_2020 %>%
  filter(!is.na(V5)) %>%
  filter(V5 != "0")

multi_li <- read.csv("data/2020election/2020投票所跨村里表.csv", header = FALSE, colClasses = "character", fileEncoding = "big-5")

lilist_2020[, c(1:5)] <- apply(lilist_2020[, c(1:5)], 2, function(x) as.character(x))
lilist_2020 <- bind_rows(lilist_2020, multi_li)

lilist_2020$V1[nchar(lilist_2020$V1) == 1] <- paste0("0", lilist_2020$V1[nchar(lilist_2020$V1) == 1])

lilist_2020$V2[nchar(lilist_2020$V2) == 1] <- paste0("00", lilist_2020$V2[nchar(lilist_2020$V2) == 1])
lilist_2020$V2[nchar(lilist_2020$V2) == 2] <- paste0("0", lilist_2020$V2[nchar(lilist_2020$V2) == 2])

lilist_2020$V4[nchar(lilist_2020$V4) == 2] <- paste0("0", lilist_2020$V4[nchar(lilist_2020$V4) == 2])

lilist_2020$V5[nchar(lilist_2020$V5) == 1] <- paste0("00", lilist_2020$V5[nchar(lilist_2020$V5) == 1])
lilist_2020$V5[nchar(lilist_2020$V5) == 2] <- paste0("0", lilist_2020$V5[nchar(lilist_2020$V5) == 2])

lilist_2020$li_id <- case_when(substr(lilist_2020$V1,1,1) == "6"~paste0(lilist_2020$V1, "0",lilist_2020$V4, "0-", lilist_2020$V5),
                               TRUE ~ paste0(lilist_2020$V1,lilist_2020$V2, substr(lilist_2020$V4,1,2), "-", lilist_2020$V5))



#Li's basic information

profile_2020 <- read.csv("data/2020election/elprof.csv", header = FALSE, colClasses = c('character'))

profile_2020$V5[substr(profile_2020$V5, 1, 1) == 0] <- substr(profile_2020$V5[substr(profile_2020$V5, 1, 1)==0], 2, 4)

profile_2020 <- profile_2020 %>%
  filter(V5 != "000") %>%
  filter(V6 != "0000")

profile_2020$li_id <- case_when(substr(profile_2020$V1, 1, 1) == "6" ~ paste0(profile_2020$V1, "0", profile_2020$V4, "0-", profile_2020$V5),
                                TRUE ~ paste0(profile_2020$V1, profile_2020$V2, substr(profile_2020$V4,1,2), "-", profile_2020$V5))

profile_2020[, 7:20] <- apply(profile_2020[, 7:20], 2, function(x) as.numeric(x))

profile_2020 <- profile_2020 %>%
  group_by(li_id) %>%
  dplyr::summarise(valid_vote = sum(V7),
                   invalid_vote = sum(V8),
                   turnout = sum(V9),
                   num_voter = sum(V10))

#profile_2020[,1:5] <- apply(profile_2020[,1:5], 2, function(x) as.character(x))

#Candidate's information

cand_2020 <- read.csv("data/2020election/elcand.csv", header = FALSE, colClasses = c('character'))

#colnames(cand_2020) <- gsub("Column", "", colnames(cand_2020))

#colnames(cand_2020) <- paste0("V", colnames(cand_2020))

cand_2020 <- cand_2020 %>%
  mutate(party = case_when(V8 ==  "1" ~ "KMT",
                           V8 == "16" ~ "DPP",
                           TRUE ~ "PFP"))

cand_2020[,1:5] <- apply(cand_2020[, 1:5], 2, function(x) as.character(x))

cand_2020 <- cand_2020 %>%
  filter(V16 != "Y")

#Election results

result_2020 <- read.csv("data/2020election/elctks.csv", header = FALSE, colClasses = c('character'))

#colnames(result_2020) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")

result_2020[,c(1:5)] <- apply(result_2020[, c(1:5)], 2, function(x) as.character(x))

result_2020$V1 <- gsub(" ", "", result_2020$V1)
result_2020$V2 <- gsub(" ", "", result_2020$V2)
result_2020$V4 <- gsub(" ", "", result_2020$V4)

result_2020$V1[nchar(result_2020$V1) == 1] <- paste0("0", result_2020$V1[nchar(result_2020$V1) == 1])
result_2020$V2[nchar(result_2020$V2) == 1] <- paste0("00", result_2020$V2[nchar(result_2020$V2) == 1])
result_2020$V2[nchar(result_2020$V2) == 2] <- paste0("0", result_2020$V2[nchar(result_2020$V2) == 2])
result_2020$V4[nchar(result_2020$V4) == 2] <- paste0("0", result_2020$V4[nchar(result_2020$V4) == 2])

result_2020$V5[substr(result_2020$V5, 1, 1) == "0"] <- substr(result_2020$V5[substr(result_2020$V5, 1, 1)==0], 2, 4)

result_2020 <- result_2020 %>%
  filter(V5 != "000") %>%
  filter(V6 != "0")

result_2020$li_id <- case_when(substr(result_2020$V1, 1, 1) == "6" ~ paste0(result_2020$V1, "0", result_2020$V4, "0-", result_2020$V5),
                               TRUE ~ paste0(result_2020$V1, result_2020$V2, substr(result_2020$V4, 1, 2), "-", result_2020$V5))

#result_2020 <- result_2020 %>%
#  group_by(V7, li_id) %>%
#  summarise(vote = sum(V8, na.rm = TRUE))

#Merge the election result and candidate data
result_2020 <- result_2020 %>%
  left_join(cand_2020[, c(6, 7, 17)], by = c("V7" = "V6"))



result_2020 <- result_2020 %>%
  group_by(li_id, party) %>%
  dplyr::summarise(vote = sum(as.numeric(V8), na.rm = TRUE)) %>%
  spread(party, vote)

#Merge the election result and Li's data

result_2020 <- result_2020 %>%
  left_join(profile_2020, by = "li_id")


colnames(result_2020)[c(2:8)] <- paste0(colnames(result_2020)[c(2:8)], "_", "2020")

result_2020[, 2:8] <- apply(result_2020[, 2:8], 2, function(x) as.numeric(x))

