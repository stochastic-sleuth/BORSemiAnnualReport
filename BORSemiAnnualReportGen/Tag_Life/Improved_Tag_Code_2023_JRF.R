library(tidyverse) # Data manipulations
library(dplyr)
library(ggplot2)
library(data.table) #To use setDT function
library(grid) #Allows me to put text onto GGplot using textGrob()
library(rerddap) # Getting data from ERDDAP
library(data.table)
library(lubridate)
library(plotly)
library(tictoc)

#
# Read in and format RT Files - Only do once (theoretically...) NOT WORKING!!! ------------------------------

#Read in and format the RT files, do it in 5 separate chunks since the files are so large. Change this using the path folder

ATS_all_files <- data.frame(x=dir(pattern = ".csv", 
                                  path = "~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/ReceiverFiles", full.names = T))

ATS_all_files <- cbind(ATS_all_files, data.frame(shortname=dir(pattern = ".csv", path = "~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/ReceiverFiles", full.names = F)))

ATS_all_files$hour <- as.POSIXct(substr(ATS_all_files$shortname, 9,21),format= "%y%m%d_%H%M%S", tz =  "Etc/GMT+8")

## Only select hourly files since setup of tag life tank

ATS_files <- ATS_all_files[ATS_all_files$hour > as.POSIXct("2022-12-09"),]

## Remove files with "(1)" in their shortnames from ATS_files
ATS_files <- ATS_files[!grepl(pattern = "\\(1\\)", ATS_files$shortname),]     #%>% 
#filter(!grepl("(1)", shortnam"e))

for (i in 1:nrow(ATS_files)) {
  ## First, put in error catch for files that are completely empty
  print(i)
  
  if (file.info(ATS_files[i,"x"])$size >0){
    # if (substr(read.csv(ATS_files[i,"x"], nrows = 1, header = F)[,1] , 1,4) == "Site") {
    #   test <- read.csv(ATS_files[i,"x"], stringsAsFactors = F, skip = 9, header = T,  row.names=NULL)
    # }else if (substr(read.csv(ATS_files[i,"x"], nrows = 1, header = F)[,1] , 1,4) == " Num") {
    #   test <- read.csv(ATS_files[i,"x"], stringsAsFactors = F, skip = 16, header = T,  row.names=NULL)
    # }else if (substr(read.csv(ATS_files[i,"x"], nrows = 1, header = F, skip = 27)[,1] , 1,8) == "Internal") {
    #   test <- read.csv(ATS_files[i,"x"], stringsAsFactors = F, skip = 27, header = T,  row.names=NULL)
    # }else{
    #   test <- read.csv(ATS_files[i,"x"], stringsAsFactors = F, skip = 7, header = T,  row.names=NULL)
    # }
    if (any(grep("File Start", readLines(ATS_files[i,"x"])))) {
      test <- read.csv(ATS_files[i,"x"], stringsAsFactors = F, skip = grep("File Start", readLines(ATS_files[i,"x"]))[1], header = T,  row.names=NULL)
    } else {
      test <- read.csv(ATS_files[i,"x"], stringsAsFactors = F, skip = grep("Log Resume", readLines(ATS_files[i,"x"]))[1], header = T,  row.names=NULL)
    }  
  }
}




RT_files<-test

str(RT_files)
head(RT_files)

RT_Files2<- RT_files %>%
 dplyr::rename(DateTime = X.1,
         TagID_Hex = DateTime) %>%
  select(DateTime, TagID_Hex) %>%
  mutate(TagID_Hex = substr(TagID_Hex, 5, 8),
         DateTime = mdy_hms(DateTime, tz = "Etc/GMT+8"))

#write.csv(RT_Files2, "C:\\Users\\rhold\\Documents\\Autonomous Receivers\\Tag Life\\2023 Tag Life Receiver\\Receiver Files\\RT_Detects_Raw_Cyrils_Method.csv")

# 
# tic()
# setwd("~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/ReceiverFiles\\Folder4")
# RT_files <-
#   list.files(pattern = "*.csv") %>% 
#   map_df(~fread(.,fill = TRUE, select = 4:10, blank.lines.skip = TRUE))
# toc()
# 
# tic()
# test<-RT_files %>%
#   mutate(DateTime = paste(V7, V8, sep = " ")) %>%
#  dplyr::rename(TagCode = V9) %>%
#   mutate(DateTime = mdy_hms(DateTime, tz = "Etc/GMT+8"),
#          TagCode = substr(TagCode, 5, 8)) %>%
#   filter(!is.na(DateTime),
#          TagCode %in% startup2$TagID_Hex) %>%
#   mutate(recv = 19030) %>%
#  dplyr::rename(DateTime_PST = DateTime) %>%
#   select(TagCode, DateTime_PST, recv)
# toc()
# #write.csv(test, "C:\\Users\\rhold\\Documents\\Autonomous Receivers\\Tag Life\\2023 Tag Life Receiver\\Receiver Files\\RT_Folder4.csv")
# 
# 
# 
# 
# 
# #
# #
# #


# Plot Tag Life Studies ---------------------------------------------------

#Set your wd to the file containing the 5 PRI filtered files
setwd("~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/PRI_5_FilterFiles/")
#Read in 5 or 3 PRI files (Only read in the TagCode and DateTime columns)
Filt_Files_5PRI <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.,select = c("TagCode","DateTime_PST","recv"))) %>%
  mutate(PRI_r = 5)

#Set your wd to the file containing the 8 PRI filtered files (for the CDFW tags)
setwd("~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/PRI_8_FilterFiles/")
Filt_Files_8PRI <-
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.,select = c("TagCode","DateTime_PST", "recv"))) %>%
  mutate(PRI_r = 8)

Filt_Files<-rbind(Filt_Files_5PRI, Filt_Files_8PRI)

#Some tags are missing a leading 0 and all have a space in front of them, remove the space and add the leading zero
#Only do this for the Lotek detections, not the RT receiver detections (once these are included...)

Filt_Files2<-Filt_Files %>%
  mutate(TagCode = ifelse(recv == 19030, TagCode, substr(TagCode, 3, 6)),
         TagCode = ifelse(substr(TagCode, 1,1)==" ", paste("0", substr(TagCode, 2, 5), sep = ""), TagCode))



#Read in startup files with the file name as a column (study id)
wd<-setwd("~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/StartUpSpecSheets/")

# list the folders in this file
files <- list.files("~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/StartUpSpecSheets/")

#Number of columns and column names must match exactly for this to work
data2<-lapply(files, read.table, header=TRUE, sep=",")
for (i in 1:length(data2)){data2[[i]]<-cbind(data2[[i]],files[i])}
startup <- do.call("rbind", data2) 

startup2<-startup %>%
  dplyr::rename(studyid = `files[i]`) %>%
  mutate(Date = as.Date(DateTime, format = "%m/%d/%Y"),
         studyid = sub("\\..*", "", studyid),
         PRI_r = round(PRI, digits=0)) %>%
  select(-c(Notes, PRI))

#Create warranty life table with all the options for tag model and PRI
TagModel<-c("SS300","SS400","SS300 bat 392", "SS400 2 bat 379","SS300","SS400","SS300 bat 392", "SS400 2 bat 379","SS400","SS300","SS400","SS300 bat 392", "SS400 2 bat 379")
PRI_r<-c(3,3,3,3,5,5,5,5,8,10,10,10,10)
warranty_life<-c(23, 48, 79, 108, 37, 71, 128, 159, 90, 68, 111, 238, 247)
warranty<-data.frame(TagModel, PRI_r, warranty_life)

#Add warranty life and warranty date to startup table
startup3<-left_join(startup2, warranty) %>%
  mutate(warranty_date = Date + warranty_life) %>%
  dplyr::rename(start_date = Date)


write.csv(startup3, "~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/Tag_Lifestartup.csv")



###Get date dead for each tag by selecting max day a tag was detected on two consecutive days, with at least 100 detections on each day
#Only do this for autonomous receiver files, not RT files
rem<-Filt_Files2 %>%
  group_by(recv, PRI_r, TagCode) %>%
  count(TagCode, recv, as.Date(DateTime_PST)) %>%
  mutate(diff = NA,
         diff = as.numeric(diff),
         n = as.numeric(n)) %>%
 dplyr::rename(Date2 = `as.Date(DateTime_PST)`,
         TagID_Hex = TagCode) %>%
  filter(Date2 > "2022-01-01") %>% #There were some weird detections in 2012, remove these
  mutate(remove = ifelse(recv!=19030 & n < 100, "Y",
                  ifelse(recv==19030 & n < 20, "Y", "N"))) %>% 
  filter(remove == "N") %>%
  arrange(recv, TagID_Hex, PRI_r, Date2)


#For each tag code, get the difference between date and previous day detected
for(i in 2:nrow(rem)) {
  if(rem[i, 'recv'] == rem[i - 1, 'recv'] & rem[i, 'TagID_Hex'] == rem[i - 1, 'TagID_Hex']) {
    rem[i, 'diff'] <- as.numeric(difftime(rem$Date2[i],rem$Date2[i - 1], units = "day"))
  }
}


test<-rem[which(rem$recv==19030),]


#When getting date dead for each tag, take the maximum day that a tag was detected on two consecutive days with at least 100 detections on both days
date_dead<- rem %>%
 dplyr::rename(date_dead = Date2) %>%
  group_by(TagID_Hex, PRI_r) %>%
  summarize(date_dead = max(date_dead))


#Add date dead and days on to the startup dataframe
startup4<-left_join(startup3, date_dead) %>%
  mutate(days_on = as.numeric(difftime(date_dead, start_date)))


#Add startup data to the detection file, remove tagIDs that aren't in the startup file
rem2<-left_join(rem, startup4) %>%
 dplyr::rename(Date = Date2) %>%
  filter(!is.na(studyid)) %>%
  mutate(detect = "Y") %>%
  select(TagID_Hex, PRI_r, Date, studyid, start_date, detect, diff, n, TagModel) %>%
  ungroup()




###Next find and expand date ranges where tags were not detected

#Find date ranges where tags were not detected
no_det<-rem2 %>%
  mutate(diff = ifelse(is.na(diff), 0, diff)) %>%
  filter(diff > 1) %>%
  mutate(first = Date - 1,
         last = Date - (diff-1))


#Expand on those date ranges
no_det2<-setDT(no_det)[ , list(recv = recv, TagID_Hex = TagID_Hex, PRI_r = PRI_r, day = seq(last, first, by = "day")), by = 1:nrow(no_det)]

no_det3<-no_det2 %>%
  mutate(detect = "N") %>%
 dplyr::rename(Date = day) %>%
  select(TagID_Hex, PRI_r, Date, detect, recv)

no_det4<-left_join(no_det3, startup4) %>%
  mutate(diff = as.numeric(NA),
         n = as.numeric(NA)) %>%
  select(TagID_Hex, PRI_r, Date, studyid, start_date, detect, diff, n, TagModel, recv) %>%
  ungroup()

rem3<-rbind(rem2, no_det4) %>%
  mutate(days_on = as.numeric(difftime(Date, start_date), units = "days")) %>%
  filter(days_on > 0) %>%
  group_by(studyid) %>%
  arrange(TagID_Hex)

  
#If there were no days without detections, need to run the line with only black as the specified color
rem3 %>% filter(PRI_r == 5) %>%
ggplot(aes(days_on, TagID_Hex, group = TagID_Hex, color = detect)) +
  geom_line() +
  scale_color_manual(values = c("white", "black")) +
  #scale_color_manual(values = "black") +
  ylab("Tag ID (Hex)") + xlab("Days On") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
                     panel.grid.major=element_blank(), panel.grid.minor=element_blank())



###Create plot that shows percent tags remaining by studyid (currently plotting all studies, can filter for specific studies)
unique(startup4$studyid)
#Add release week to the startup table
startup5<-startup4 %>%
  mutate(across(studyid, str_replace, "Butte_2023_Tag_Life", "Butte_Creek_2023"),
         across(studyid, str_replace, "CDFW_BattLife_20230601", "CDFW"),
         across(studyid, str_replace, "Seasonal_Survival_Wk1_4", "Seasonal_Survival"),
         across(studyid, str_replace, "SJ_Wild_all", "SJ_Steelhead"),
         across(studyid, str_replace, "Spring_Pulse_Wk1_5", "Spring_Pulse")) %>%
  group_by(studyid) %>%
  arrange(studyid, start_date, .by_group = T) %>%
  mutate(NameID = match(start_date, unique(start_date))) %>%
  mutate(week = paste(studyid, NameID, sep = "-"))

#Get number of tags by study week
tags_by_study<-startup5 %>%
  group_by(week) %>%
  filter(!is.na(date_dead)) %>%
  summarise(count = n_distinct(TagID_Hex), max_day = max(days_on), start_date_min = min(start_date), start_day_max = max(start_date))

dates_exp<-setDT(tags_by_study)[ , list(week = week, days_on = seq(0, max_day)), by = 1:nrow(tags_by_study)]

#Create column for number of tags dead by study week
startup6<-left_join(dates_exp, startup5) %>%
  mutate(dead = ifelse(is.na(TagID_Hex), 0, 1)) %>%
  group_by(week) %>%
  arrange(days_on, .by_group = TRUE) %>%
  mutate(cum_count = cumsum(dead))

percent_tags<-left_join(startup6, tags_by_study) %>%
  mutate(dead_percent = (cum_count/count)*100) %>%
  mutate(percent_alive = 100 - dead_percent) %>%
  mutate(studyid = sub("-.*", "", week)) %>%
  select(c(TagID_Hex, studyid, week, dead_percent, percent_alive, start_date, date_dead, days_on, max_day, TagModel))


ggplot(percent_tags, aes(days_on, percent_alive, group = week, color = studyid)) +
  geom_line(linewidth=1.2) +
  scale_x_continuous(name = "Days Since Start of Tags", breaks = seq(0,max(percent_tags$days_on)+10, by=20)) +
  scale_y_continuous(name = "Percent Tags Remaining", breaks = seq(0,100, by=5)) +
  theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())


# Create Plots for Each Study ---------------------------------------------
unique(percent_tags$studyid)

#Seasonal Survival - Good to go
SSTLplot<-percent_tags %>% filter(studyid == "Seasonal_Survival") %>%
ggplot(aes(days_on, percent_alive, group = week, color = week)) +
  geom_line(linewidth=1.2) +
  scale_color_discrete(name = "Study Week",
                     breaks = c("Seasonal_Survival-1", "Seasonal_Survival-2", "Seasonal_Survival-3", "Seasonal_Survival-4"),
                     labels = c("One","Two","Three","Four")) +
  geom_segment(aes(x = 71, y = 0, xend = 71, yend = 100), linetype = "longdash", color = "black", linewidth = 1) +
  scale_x_continuous(name = "Days Since Start of Tags", breaks = seq(0,max(percent_tags$days_on)+10, by=20)) +
  scale_y_continuous(name = "Percent Tags Remaining", breaks = seq(0,100, by=5)) +
  theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) 

SSTLplot

dev.copy(png,filename="SSTLplot");
dev.off ();

#Spring Pulse - Plot is good, need to re run with RT files since some tags "died" when the receiver died
SPTLplot<- percent_tags %>% filter(studyid == "Spring_Pulse") %>%
  mutate(TagModel = coalesce(TagModel, "NA")) %>%
  ggplot(aes(days_on, percent_alive, group = week)) +
  geom_line(linewidth=1.2, aes(color = week)) +
  geom_point(aes(color = TagModel, alpha = TagModel), size = 2) +
  geom_segment(aes(x = 71, y = 0, xend = 71, yend = 100), linetype = "longdash", color = "blue", size = 1) +
  geom_segment(aes(x = 37, y = 0, xend = 37, yend = 100), linetype = "longdash", color = "red", size = 1) +
  scale_color_manual(name = "Week / Tag Model",
                     breaks = c("Spring_Pulse-1","Spring_Pulse-2","Spring_Pulse-3","Spring_Pulse-4","Spring_Pulse-5","SS300","SS400","NA"),
                     labels = c("One","Two","Three","Four","Five","SS300","SS400",""),
                     values = c("orange1","cyan3","darkorchid1","darkgreen", "gold1","red1","blue1","white")) +
  scale_alpha_manual(values = c(0,1,1),
                     guide = "none") + 
  scale_x_continuous(name = "Days Since Start of Tags", breaks = seq(0,max(percent_tags$days_on)+10, by=20)) +
  scale_y_continuous(name = "Percent Tags Remaining", breaks = seq(0,100, by=5)) +
  theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

SPTLplot
dev.copy(png,filename="SPTLplot");
dev.off ();

#Butte - Issues because the Lotek receiver was dead when the tags started to die. 
#Need to include RT detections to know when they actually died!
ButteTLplot<- percent_tags %>% filter(studyid == "Butte_Creek_2023") %>%
  ggplot(aes(days_on, percent_alive)) +
  geom_line(linewidth=1.2) +
  geom_segment(aes(x = 37, y = 0, xend = 37, yend = 100), linetype = "longdash", color = "black", size = 1) +
  scale_x_continuous(name = "Days Since Start of Tags", breaks = seq(0,max(percent_tags$days_on)+10, by=20)) +
  scale_y_continuous(name = "Percent Tags Remaining", breaks = seq(0,100, by=5)) +
  theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())


ButteTLplot
dev.copy(png,filename="ButteTLplot");
dev.off ();
graphics.off()

###SJ Steelhead - Not past estimated run time yet
SJTLplot<- percent_tags %>% filter(studyid == "SJ_Steelhead") %>%
  ggplot(aes(days_on, percent_alive)) +
  geom_line(linewidth=1.2) +
  geom_segment(aes(x = 159, y = 0, xend = 159, yend = 100), linetype = "longdash", color = "black", size = 1) +
  scale_x_continuous(name = "Days Since Start of Tags", breaks = seq(0,max(percent_tags$days_on)+10, by=20)) +
  scale_y_continuous(name = "Percent Tags Remaining", breaks = seq(0,100, by=5)) +
  theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

SJTLplot
dev.copy(png,filename="SJTLplot");
dev.off ();

ggsave("SJTLplot.png")
graphics.off()

###CDFW - Not past estimated run time yet
CDFWTLpercent_tags %>% filter(studyid == "CDFW") %>%
  ggplot(aes(days_on, percent_alive)) +
  geom_line(linewidth=1.2) +
  geom_segment(aes(x = 90, y = 0, xend = 90, yend = 100), linetype = "longdash", color = "black", size = 1) +
  scale_x_continuous(name = "Days Since Start of Tags", breaks = seq(0,max(percent_tags$days_on)+10, by=20)) +
  scale_y_continuous(name = "Percent Tags Remaining", breaks = seq(0,100, by=5)) +
  theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())



# Create Summary Tables ---------------------------------------------------

#Get min and max days on / date dead for all studies
startup5 %>%
  group_by(week) %>%
  select(week, days_on, date_dead) %>%
  filter(!is.na(date_dead)) %>%
  summarize(min_days = min(days_on), first_dead = min(date_dead), max_day = max(days_on), last_dead = max(date_dead))


#Table based on tagging week
table_weexk<-startup5 %>%
  group_by(week) %>%
  summarize(Tag_Count = n_distinct(TagID_Hex), Start_Date = unique(start_date), Study = unique(studyid),
            Avg_Run_Time = mean(days_on), Min_Run_Time = min(days_on), Max_Run_Time = max(days_on)) %>%
  mutate(Run_Time_Range = paste(Min_Run_Time, Max_Run_Time, sep = " - "),
         across(Avg_Run_Time, round, 0)) %>%
  select(-c(Min_Run_Time, Max_Run_Time))

#For studies that used multiple tag models, paste these into one row
tag_model_by_week<-aggregate(TagModel ~ week, unique(startup5[,c("TagModel","week")]), paste, collapse = ", ")

#Add in tag model and rename some columns
unique(table_week$Study)

SPTLtable<-left_join(table_week, tag_model_by_week) %>%
  filter(Study == "Spring_Pulse") %>%
 dplyr::rename(Study_ID = week,
         Tag_Model = TagModel) %>%
  select(Study_ID, Tag_Model, Tag_Count, Start_Date, Avg_Run_Time, Run_Time_Range)

CDFWTLtable<-left_join(table_week, tag_model_by_week) %>%
  filter(Study == "CDFW") %>%
  dplyr::rename(Study_ID = week,
                Tag_Model = TagModel) %>%
  select(Study_ID, Tag_Model, Tag_Count, Start_Date, Avg_Run_Time, Run_Time_Range)

SJTLtable<-left_join(table_week, tag_model_by_week) %>%
  filter(Study == "SJ_Steelhead") %>%
  dplyr::rename(Study_ID = week,
                Tag_Model = TagModel) %>%
  select(Study_ID, Tag_Model, Tag_Count, Start_Date, Avg_Run_Time, Run_Time_Range)

SSTLtable<-left_join(table_week, tag_model_by_week) %>%
  filter(Study == "Seasonal_Survival") %>%
  dplyr::rename(Study_ID = week,
                Tag_Model = TagModel) %>%
  select(Study_ID, Tag_Model, Tag_Count, Start_Date, Avg_Run_Time, Run_Time_Range)

BCTLtable<-left_join(table_week, tag_model_by_week) %>%
  filter(Study == "Butte_Creek_2023") %>%
  dplyr::rename(Study_ID = week,
                Tag_Model = TagModel) %>%
  select(Study_ID, Tag_Model, Tag_Count, Start_Date, Avg_Run_Time, Run_Time_Range)

#CDFW has a tag with no date dead, must be wrong tag code. Need to figure out the appropriate code and re filter the files

write.csv(SPTLtable, "~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/Tables/SPTLtable.csv")
write.csv(CDFWTLtable, "~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/Tables/CDFWTLtable.csv")
write.csv(SJTLtable, "~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/Tables/SJTLtable.csv")
write.csv(SSTLtable, "~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/Tables/SSTLtable.csv")
write.csv(BCTLtable, "~/Desktop/BORSemiAnnualReportQ3Q4DESK/Tag_Life/Tables/BCTLtable.csv")








