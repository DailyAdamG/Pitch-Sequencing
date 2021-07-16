#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/Pitch-Sequencing")

#Import data

data_first_08 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2008 First Half Raw.csv")
data_last_08 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2008 Second Half Raw.csv")
data_first_09 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2009 First Half Raw.csv")
data_last_09 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2009 Second Half Raw.csv")
data_first_10 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2010 First Half Raw.csv")
data_last_10 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2010 Second Half Raw.csv")
data_first_11 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2011 First Half Raw.csv")
data_last_11 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2011 Second Half Raw.csv")
data_first_12 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2012 First Half Raw.csv")
data_last_12 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2012 Second Half Raw.csv")
data_first_13 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2013 First Half Raw.csv")
data_last_13 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2013 Second Half Raw.csv")
data_first_14 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2014 First Half Raw.csv")
data_last_14 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2014 Second Half Raw.csv")
data_first_15 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2015 First Half Raw.csv")
data_last_15 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2015 Second Half Raw.csv")
data_first_16 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2016 First Half Raw.csv")
data_last_16 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2016 Second Half Raw.csv")
data_first_17 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2017 First Half Raw.csv")
data_last_17 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2017 Second Half Raw.csv")
data_first_18 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2018 First Half Raw.csv")
data_last_18 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2018 Second Half Raw.csv")
data_first_19 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2019 First Half Raw.csv")
data_last_19 <- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2019 Second Half Raw.csv")
data_20<- read.csv("C:/Users/daily/Desktop/Baseball Savant CSVs/2020 Season Raw.csv")

#Load libraries

library(tidyverse)
library(splitstackshape)

#Combine data

data <- bind_rows(data_first_08, data_last_08,
                  data_first_09, data_last_09,
                  data_first_10, data_last_10,
                  data_first_11, data_last_11,
                  data_first_12, data_last_12,
                  data_first_13, data_last_13,
                  data_first_14, data_last_14,
                  data_first_15, data_last_15,
                  data_first_16, data_last_16,
                  data_first_17, data_last_17,
                  data_first_18, data_last_18,
                  data_first_19, data_last_19,
                  data_20)

#Select only the columns I need

data <- select(data, pitch_type, game_date, pitcher, batter, events, stand, p_throws, type, bb_type,
                plate_x, plate_z, sz_top, sz_bot, game_pk, woba_value, at_bat_number,
                pitch_number, pitch_name)

#Create my own pitch type classification

data <- data %>%
  mutate(my_pitch_type = ifelse(pitch_type == "FA" | pitch_type == "FF" | pitch_type == "FT" | pitch_type == "SI", "FB",
                                ifelse(pitch_type == "CU" | pitch_type == "KC" | pitch_type == "EP", "CB",
                                       ifelse(pitch_type == "SL" | pitch_type == "FC", "SL",
                                              ifelse(pitch_type == "CH" | pitch_type == "FO" | pitch_type == "FS" | pitch_type == "SC", "CH", "XX")))))

#Create inside/outside zones

data <- data %>%
  mutate(horizontal = ifelse(stand == "R" & p_throws == "R" & plate_x <= -0.28, "I", 
                             ifelse(stand == "R" & p_throws == "R" & plate_x >= 0.28, "O",
                                    ifelse(stand == "L" & p_throws == "R" & plate_x <= -0.28, "O",
                                           ifelse(stand == "L" & p_throws == "R" & plate_x >= 0.28, "I",
                                                  ifelse(stand == "R" & p_throws == "L" & plate_x >= 0.28, "O",
                                                         ifelse(stand == "R" & p_throws == "L" & plate_x <= -0.28, "I",
                                                                ifelse(stand == "L" & p_throws == "L" & plate_x <= -0.28, "O",
                                                                       ifelse(stand == "L" & p_throws == "L" & plate_x >= 0.28, "I",
                                                                              ifelse(plate_x <= 0.27 & plate_x >= -0.27, "M", "X"))))))))))

#Fill null values with Xs in column

data$horizontal[is.na(data$horizontal)] <- "X"

#Create borders for vertical zones

data <- data %>%
  mutate(sz_thirds = (sz_top - sz_bot) / 3) %>%
  mutate(sz_top_lb = sz_top - sz_thirds) %>%
  mutate(sz_bot_ub = sz_bot + sz_thirds)

#Create high and low zones

data <- data %>%
  mutate(vertical = ifelse(plate_z > sz_top_lb, "H",
                           ifelse(plate_z < sz_bot_ub, "L", "M")))

#Fill null values with Xs in column

data$vertical[is.na(data$vertical)] <- "X"

#Create Platoon Advantage Column

data <- data %>%
  mutate(PlatoonAdvantage = ifelse(stand == p_throws, 1, 0))

#Concatenate pitch type and locations

data <- data %>%
  mutate(pitch_type_location = paste(my_pitch_type, vertical, horizontal, sep = "-"))

#Group pitches into sequences

abs <-  data %>%
  arrange(game_date, game_pk, at_bat_number, pitch_number) %>%
  group_by(game_pk, at_bat_number) %>%
  summarise(Pitch_Sequence = paste(pitch_type_location, collapse = ","),
            PlatoonAdv = first(PlatoonAdvantage),
            Result = last(events),
            Batted_Ball_Type = last(bb_type),
            wOBA = last(woba_value))

#Remove any sequences with missing data

clean_abs <- abs[- grep("X", abs$Pitch_Sequence),]

#Remove any sequences that are not considered PA

clean_abs <- clean_abs %>%
  filter(Result != "") %>%
  filter(Result != "catcher_interf") %>%
  filter(Result != "caught_stealing_2b") %>%
  filter(Result != "caught_stealing_3b") %>%
  filter(Result != "caught_stealing_home") %>%
  filter(Result != "ejection") %>%
  filter(Result != "game_advisory") %>%
  filter(Result != "other_advance") %>%
  filter(Result != "passed_ball") %>%
  filter(Result != "pickoff_1b") %>%
  filter(Result != "pickoff_2b") %>%
  filter(Result != "pickoff_3b") %>%
  filter(Result != "pickoff_caught_stealing_2b") %>%
  filter(Result != "pickoff_caught_stealing_3b") %>%
  filter(Result != "pickoff_caught_stealing_home") %>%
  filter(Result != "pickoff_error_2b") %>%
  filter(Result != "stolen_base_2b") %>%
  filter(Result != "stolen_base_3b") %>%
  filter(Result != "stolen_base_home") %>%
  filter(Result != "wild_pitch") %>%
  filter(Result != "runner_double_play") %>%
  filter(Result != "other_out")

clean_abs <- cSplit(clean_abs, 'Pitch_Sequence', sep=",", type.convert=FALSE)

#Create Boolean columns

clean_abs <- clean_abs %>%
  mutate(BBE = ifelse(Batted_Ball_Type != "", 1, 0)) %>%
  mutate(Fly_Ball = ifelse(Batted_Ball_Type == "fly_ball", 1, 0)) %>%
  mutate(Ground_Ball = ifelse(Batted_Ball_Type == "ground_ball", 1, 0)) %>%
  mutate(Line_Drive = ifelse(Batted_Ball_Type == "line_drive", 1, 0)) %>%
  mutate(Pop_Up = ifelse(Batted_Ball_Type == "popup", 1, 0)) %>%
  mutate(Strikeout = ifelse(Result == "strikeout" | Result == "strikeout_double_play", 1, 0)) %>%
  mutate(Walk = ifelse(Result == "walk", 1, 0)) %>%
  mutate(HR = ifelse(Result == "home_run", 1, 0))

#Split the data into platoon advantages and disadvantages

PlatoonAdv_ABs <- clean_abs %>%
  filter(PlatoonAdv == 1)

PlatoonDis_ABs <- clean_abs %>%
  filter(PlatoonAdv != 1)

#Create a summary table for each platoon and first pitch of sequence

PlatoonAdvSummary_FirstPitch <- PlatoonAdv_ABs %>%
  group_by(Pitch_Sequence_01) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

PlatoonDisSummary_FirstPitch <- PlatoonDis_ABs %>%
  group_by(Pitch_Sequence_01) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

#Create a summary table for each platoon and first & second pitch of sequence

PlatoonAdvSummary_FirstTwoPitches <- PlatoonAdv_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

PlatoonDisSummary_FirstTwoPitches <- PlatoonDis_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

#Create a summary table for each platoon and first three pitches of sequence

PlatoonAdvSummary_FirstThreePitches <- PlatoonAdv_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

PlatoonDisSummary_FirstThreePitches <- PlatoonDis_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

#Create a summary table for each platoon and first four pitches of sequence

PlatoonAdvSummary_FirstFourPitches <- PlatoonAdv_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03, Pitch_Sequence_04) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

PlatoonDisSummary_FirstFourPitches <- PlatoonDis_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03, Pitch_Sequence_04) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

#Create a summary table for each platoon and first five pitches of sequence

PlatoonAdvSummary_FirstFivePitches <- PlatoonAdv_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03, Pitch_Sequence_04, Pitch_Sequence_05) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

PlatoonDisSummary_FirstFivePitches <- PlatoonDis_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03, Pitch_Sequence_04, Pitch_Sequence_05) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

#Create a summary table for each platoon and first six pitches of sequence

PlatoonAdvSummary_FirstSixPitches <- PlatoonAdv_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03, Pitch_Sequence_04, Pitch_Sequence_05, Pitch_Sequence_06) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

PlatoonDisSummary_FirstSixPitches <- PlatoonDis_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03, Pitch_Sequence_04, Pitch_Sequence_05, Pitch_Sequence_06) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

#Create a summary table for each platoon and first seven pitches of sequence

PlatoonAdvSummary_FirstSevenPitches <- PlatoonAdv_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03, Pitch_Sequence_04, Pitch_Sequence_05, Pitch_Sequence_06, Pitch_Sequence_07) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

PlatoonDisSummary_FirstSevenPitches <- PlatoonDis_ABs %>%
  group_by(Pitch_Sequence_01, Pitch_Sequence_02, Pitch_Sequence_03, Pitch_Sequence_04, Pitch_Sequence_05, Pitch_Sequence_06, Pitch_Sequence_07) %>%
  summarise(PA = n(),
            BBE = sum(BBE),
            Fly_Ball = sum(Fly_Ball),
            Ground_Ball = sum(Ground_Ball),
            Line_Drive = sum(Line_Drive),
            Pop_Up = sum(Pop_Up),
            Strikeout = sum(Strikeout),
            Walk = sum(Walk),
            HR = sum(HR),
            Avg_wOBA = mean(wOBA),
            Sum_wOBA = sum(wOBA))

#Export files to excel

write.csv(PlatoonAdvSummary_FirstPitch, "First Pitch Summary with Platoon Advantage.csv")
write.csv(PlatoonDisSummary_FirstPitch, "First Pitch Summary with Platoon Disadvantage.csv")
write.csv(PlatoonAdvSummary_FirstTwoPitches, "First Two Pitches Summary with Platoon Advantage.csv")
write.csv(PlatoonDisSummary_FirstTwoPitches, "First Two Pitches Summary with Platoon Disadvantage.csv")
write.csv(PlatoonAdvSummary_FirstThreePitches, "First Three Pitches Summary with Platoon Advantage.csv")
write.csv(PlatoonDisSummary_FirstThreePitches, "First Three Pitches Summary with Platoon Disadvantage.csv")
write.csv(PlatoonAdvSummary_FirstFourPitches, "First Four Pitches Summary with Platoon Advantage.csv")
write.csv(PlatoonDisSummary_FirstFourPitches, "First Four Pitches Summary with Platoon Disadvantage.csv")
write.csv(PlatoonAdvSummary_FirstFivePitches, "First Five Pitches Summary with Platoon Advantage.csv")
write.csv(PlatoonDisSummary_FirstFivePitches, "First Five Pitches Summary with Platoon Disadvantage.csv")
write.csv(PlatoonAdvSummary_FirstSixPitches, "First Six Pitches Summary with Platoon Advantage.csv")
write.csv(PlatoonDisSummary_FirstSixPitches, "First Six Pitches Summary with Platoon Disadvantage.csv")
write.csv(PlatoonAdvSummary_FirstSevenPitches, "First Seven Pitches Summary with Platoon Advantage.csv")
write.csv(PlatoonDisSummary_FirstSevenPitches, "First Seven Pitches Summary with Platoon Disadvantage.csv")