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

fongshan_2010 <- read.csv("data/fongshan_2010.csv", stringsAsFactors = FALSE)
fongshan_2014 <- read.csv("data/fongshan_2014.csv", stringsAsFactors = FALSE)

#Haifeng (6401200-023) was merged into Haiguang (6401200-022) 

fongshan_2010[fongshan_2010$li_id == "6401200-022", c(5:11)]

fongshan_2010[fongshan_2010$li_id == "6401200-022", c(5:11)] + fongshan_2010[fongshan_2010$li_id == "6401200-023", c(5:11)]

fongshan_2010[fongshan_2010$li_id == "6401200-022", c(5:11)] <- 
  fongshan_2010[fongshan_2010$li_id == "6401200-022", c(5:11)] + 
  fongshan_2010[fongshan_2010$li_id == "6401200-023", c(5:11)]

fongshan_2010 <- fongshan_2010 %>%
  filter(li_id != "6401200-023")

#Chengzheng (6401200-052) was split and then merged into Shengming (6401200-045) and Chengzhi (6401200-062)

fongshan_2010[fongshan_2010$li_id == "6401200-045", c(5:11)] <- 

fongshan_2010[fongshan_2010$li_id == "6401200-052", c(5:11)] +
fongshan_2010[fongshan_2010$li_id == "6401200-045", c(5:11)] +
fongshan_2010[fongshan_2010$li_id == "6401200-062", c(5:11)]

fongshan_2010 <- fongshan_2010 %>%
  filter(!li_id %in% c("6401200-052", "6401200-062"))

fongshan_2014[fongshan_2014$li_id == "6401200-045", c(2:9)] <- 

fongshan_2014[fongshan_2014$li_id == "6401200-062", c(2:9)] +
fongshan_2014[fongshan_2014$li_id == "6401200-045", c(2:9)]

fongshan_2014 <- fongshan_2014 %>%
  filter(li_id != "6401200-062")

fongshan <- fongshan_2010 %>%
  left_join(fongshan_2014, by = "li_id")



