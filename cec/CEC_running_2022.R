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
library(jsonlite)


options(warn = -1)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


####

startTime <- as.POSIXct("2020-01-10 10:15:00")

while(TRUE) {
  
  Sys.sleep(1)
  
  if(Sys.time() >= startTime) {
    
    destname <- paste0(substr(startTime, 12, 13), substr(startTime, 15, 16), ".json")
    
    download.file("https://download.2020dl.nat.gov.tw/running_0110.json", 
                  destfile = destname, 
                  method = "wget", 
                  extra = "--no-check-certificate --http-user=DL000034 --http-password=QTWF12Q") #They are invaild username and password.
    
    running <- fromJSON(destname)
    president <- running$P1
    
    cad_info <- ldply(president$candTksInfo, function(x){
      
      spread(as.data.frame(x)[1:2], candNo, tks)
      
    })
    
    colnames(cad_info) <- paste0("candno_", colnames(cad_info))
    
    feja_time <- startTime - 43200
    
    feja_time <- gsub("-", "/", feja_time)
    feja_time <- gsub(" ", " 下午 ", feja_time)
    
    result <- data.frame(time = feja_time, capture = substr(destname, 1, 4), cand_3 = cad_info$candno_3[1], cand_2 = cad_info$candno_2[1], cand_1 = cad_info$candno_1[1])
    
    write.table(result, paste0("feja/running-1090102", substr(destname, 1, 4), ".txt"), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
    
    startTime <- startTime + 600 #get a running file every 10 mins
    
  } else {
    
    print(paste(Sys.time(), "I'm still waiting...", sep = " "))
    
  }
  
}


###final file

final_2022 <- fromJSON("final.json")

council_2022 <- final_2022$T1

council_2022 <- council_2022 %>%
  filter(is.na(tboxNo) == FALSE) %>%
  rename(valid_vote = prof1,
         invalid_vote = prof2,
         turnout = prof3,
         num_voter = prof7)

council_2022$dist_id <- paste0(council_2022$prvCode, council_2022$cityCode, council_2022$deptCode, council_2022$tboxNo)


#Get vote number

vote_info <- ldply(council_2022$candTksInfo, function(x){
  
  spread(as.data.frame(x)[1:2], candNo, tks)
  
})

vote_info$dist_id <- council_2022$dist_id

vote_info <- vote_info %>%
  gather(candno, vote, 1:25)

#Get victory information

vic_info <- ldply(council_2022$candTksInfo, function(x){
  
  spread(as.data.frame(x)[,c(1, 3)], candNo, candVictor)
  
})

#colnames(vic_info) <- paste0("candno_", colnames(vic_info))

vic_info$dist_id <- council_2022$dist_id

vic_info <- vic_info %>%
  gather(candno, victor, 1:25)

vote_info <- vote_info %>%
  left_join(vic_info, by = c("dist_id", "candno"))

vote_info <- vote_info %>%
  filter(is.na(victor) == FALSE)


#Which tboxs belong to their li?: From the village chief result

li_tb <- final_2022$TV %>%
  filter(is.na(tboxNo) == FALSE) %>%
  dplyr::select(1:6)

li_tb$li_id <- case_when(substr(li_tb$prvCode, 1, 1) == "6" ~ paste0(li_tb$prvCode, substr(li_tb$cityCode, 1, 1), li_tb$deptCode, "0-", li_tb$liCode),
                         TRUE ~ paste0(li_tb$prvCode, li_tb$cityCode, substr(li_tb$deptCode, 1, 2), "-", li_tb$liCode))

#In Lienchiang County, several lis belong to one tbox. Therefore, we can't get these lis' individual voting results. We have to merge them and reserve one of them.
li_tb <- li_tb %>%
  filter(!li_id %in% c("0900701-003", "0900701-006", "0900701-007", "0900701-008", "0900701-009",
                       "0900702-002", "0900702-004", "0900702-005", "0900702-006",
                       "0900703-002", "0900703-003", "0900703-005", "0900704-002"))

li_tb$dist_id <- paste0(li_tb$prvCode, li_tb$cityCode, li_tb$deptCode, li_tb$tboxNo)

vote_info <- vote_info %>%
  left_join(li_tb[,7:8], by = "dist_id")

vote_sum <- vote_info %>%
  group_by(li_id, candno) %>%
  summarise(vote = sum(vote))

vic_uni <- distinct(vote_info[,c(2, 4, 5)])


vote_sum <- vote_sum %>%
  left_join(vic_uni, by = c("li_id", "candno"))

#Merge the vote and victory results into the council table.

council_2022 <- council_2022 %>%
  left_join(li_tb[, 7:8], by = "dist_id")

council_2022 <- council_2022 %>%
  group_by(li_id) %>%
  summarize(valid_vote = sum(valid_vote),
            invalid_vote = sum(invalid_vote),
            turnout = sum(turnout),
            num_voter = sum(num_voter))

council_2022 <- council_2022 %>%
  left_join(vote_sum, by = "li_id")

council_2022$rate <- council_2022$vote / council_2022$valid_vote

#How to get li's names? Using web scraping from CEC website.

li_df_adjusted <- read_xlsx("lilist_2022_adjusted.xlsx")

council_2022 <- council_2022 %>%
  left_join(li_df_adjusted, by = "li_id")


#tbox

prv <- fromJSON("https://www.cec.gov.tw/data/json/dist/prvCityDept.json")

countycode <- prv$prvs$prvCityCode


li_df <- data.frame()

for (i in 1:22) {
  

    tbox_json <- fromJSON(paste0("https://www.cec.gov.tw/data/json/tbox/",
                                 prv$prvs$prvCityCode[[i]], "/01",
                                 prv$prvs$prvCityCode[[i]], ".json"))
    
    
    for (l in 1:length(tbox_json$depts$lis)) {
      
      tbox_json$depts$deptCode[[l]]
      
      tbox_json$depts$lis[[l]]$liCode
      tbox_json$depts$lis[[l]]$liName
      
      li_temp <- data.frame(county = prv$prvs$prvName[[i]],
                          county_code = prv$prvs$prvCityCode[[i]],
                          district_code = tbox_json$depts$deptCode[[l]],
                          li_name = tbox_json$depts$lis[[l]]$liName,
                          li_code = tbox_json$depts$lis[[l]]$liCode)
      
      li_df <- rbind(li_df, li_temp)
      
      
    }
    

}


county_df <- data.frame()

for (i in 1:22) {
  
  county_temp <- prv$prvs$depts[[i]]
  
  county_temp$county <- prv$prvs$prvName[[i]]
  county_temp$county_code <- prv$prvs$prvCityCode[[i]]
  
  county_df <- rbind(county_df, county_temp)
  
}

county_df <- county_df %>%
  rename(district = deptName,
         district_code = deptCode) %>%
  dplyr::select(county, county_code, everything())


li_df <- li_df %>%
  left_join(county_df[, -1], by = c("county_code", "district_code"))

li_df <- li_df %>%
  dplyr::select(county, county_code, district, everything())


li_df$li_id <- case_when(substr(li_df$county_code, 1, 1) == "6" ~ paste0(substr(li_df$county_code, 1, 3), li_df$district_code, "0-", li_df$li_code),
                                   TRUE ~ paste0(li_df$county_code, substr(li_df$district_code, 1, 2), "-", li_df$li_code))


write_xlsx(li_df, "lilist_2022.xlsx")

li_df_adjusted <- li_df
  
li_df_adjusted$li_name[li_df_adjusted$li_id == "0900701-002"] <- "復興村;福沃村"
li_df_adjusted$li_name[li_df_adjusted$li_id == "0900701-004"] <- "清水村;珠螺村"
li_df_adjusted$li_name[li_df_adjusted$li_id == "0900701-005"] <- "仁愛村;津沙村;馬祖村;四維村"
li_df_adjusted$li_name[li_df_adjusted$li_id == "0900702-001"] <- "塘岐村;后沃村"
li_df_adjusted$li_name[li_df_adjusted$li_id == "0900702-003"] <- "橋仔村;芹壁村;坂里村;白沙村"
li_df_adjusted$li_name[li_df_adjusted$li_id == "0900703-001"] <- "青帆村;田沃村;西坵村"
li_df_adjusted$li_name[li_df_adjusted$li_id == "0900703-004"] <- "福正村;大坪村"
li_df_adjusted$li_name[li_df_adjusted$li_id == "0900704-001"] <- "中柳村;樂華村"

li_df_adjusted <- li_df_adjusted %>%
  filter(!li_id %in% c("0900701-003", "0900701-006", "0900701-007", "0900701-008", "0900701-009",
                       "0900702-002", "0900702-004", "0900702-005", "0900702-006",
                       "0900703-002", "0900703-003", "0900703-005", "0900704-002"))

write_xlsx(li_df_adjusted, "lilist_2022_adjusted.xlsx")
