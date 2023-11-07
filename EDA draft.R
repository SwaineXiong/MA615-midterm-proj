## Import packages
library(ggplot2)
library(readr)
library(dplyr)


## Load data
DeclarationDenials <- read_csv("DeclarationDenials (1).csv")
#View(DeclarationDenials)

DisasterDeclarationsSummaries <- read_csv("DisasterDeclarationsSummaries.csv")
#View(DisasterDeclarationsSummaries)

FemaWebDisasterDeclarations <- read_csv("FemaWebDisasterDeclarations.csv")
#View(FemaWebDisasterDeclarations)

FemaWebDisasterSummaries <- read_csv("FemaWebDisasterSummaries.csv")
#View(FemaWebDisasterSummaries)

population_2020 <- read_csv("ACSDP5Y2020.DP05-Data.csv")
#View(population_2020)

population_2021 <- read_csv("ACSDP5Y2021.DP05-Data.csv")
#View(population_2021)

house_2020 <- read_csv("ACSDT5Y2020.B25001-Data.csv")
#View(house_2020)

house_2021 <- read_csv("ACSDT5Y2021.B25001-Data.csv")
#View(house_2021)

poverty_2020 <- read_csv("ACSST5Y2020.S1701-Data.csv")
#View(poverty_2020)

poverty_2021 <- read_csv("ACSST5Y2021.S1701-Data.csv")
#View(poverty_2021)

MissionAssignments <- read_csv("MissionAssignments.csv")
#View(MissionAssignments)

StormEvents_details_2020 <- read_csv("StormEvents_details-ftp_v1.0_d2020_c20230927.csv")
#View(StormEvents_details_ftp_v1_0_d2020_c20230927)

StormEvents_details_2021 <- read_csv("StormEvents_details-ftp_v1.0_d2021_c20231017.csv")
#View(StormEvents_details_ftp_v1_0_d2021_c20231017)

fatalities_2020 <- read_csv("StormEvents_fatalities-ftp_v1.0_d2020_c20230927.csv")
#View(StormEvents_fatalities_ftp_v1_0_d2020_c20230927)

fatalities_2021 <- read_csv("StormEvents_fatalities-ftp_v1.0_d2021_c20231017.csv")
View(StormEvents_fatalities_ftp_v1_0_d2021_c20231017)




## Questions 
#How dangerous are floods? 
#How expensive? 
#Is there any pattern to the kinds of communities that suffer losses from floods?



## Data cleaning

# Census data cleaning: Drop null columns
cols_to_keep <- c("GEO_ID", "NAME")

#Population 2020
population_2020_clean <- population_2020
population_2020_clean[, setdiff(names(population_2020), cols_to_keep)] <- lapply(population_2020[, setdiff(names(population_2020), cols_to_keep)], as.numeric)
population_2020_clean <- population_2020_clean[, colSums(is.na(population_2020_clean)) < nrow(population_2020_clean)]
population_2020_clean <- population_2020_clean[-1,]
population_2020_clean <- population_2020_clean %>%
  mutate(state = str_extract(NAME, "(?<=,\\s)[^,]+$"))
population_2020_clean1 <- population_2020_clean[, c(1,2,dim(population_2020_clean)[2],seq(3,dim(population_2020_clean)[2],2))] 

#Population 2021
population_2021_clean <- population_2021
population_2021_clean[, setdiff(names(population_2021), cols_to_keep)] <- lapply(population_2021[, setdiff(names(population_2021), cols_to_keep)], as.numeric)
population_2021_clean <- population_2021_clean[, colSums(is.na(population_2021_clean)) < nrow(population_2021_clean)]
population_2021_clean <- population_2021_clean[-1,]
population_2021_clean <- population_2021_clean %>%
  mutate(state = str_extract(NAME, "(?<=,\\s)[^,]+$"))
population_2021_clean1 <- population_2021_clean[, c(1,2,dim(population_2021_clean)[2],seq(3,dim(population_2021_clean)[2],2))] 



#House 2020
house_2020_clean <- house_2020
house_2020_clean[, setdiff(names(house_2020), cols_to_keep)] <- lapply(house_2020[, setdiff(names(house_2020), cols_to_keep)], as.numeric)
house_2020_clean <- house_2020_clean[, colSums(is.na(house_2020_clean)) < nrow(house_2020_clean)]
house_2020_clean <- house_2020_clean[-1,]
house_2020_clean <- house_2020_clean %>%
  mutate(state = str_extract(NAME, "(?<=,\\s)[^,]+$"))

#House 2021
house_2021_clean <- house_2021
house_2021_clean[, setdiff(names(house_2021), cols_to_keep)] <- lapply(house_2021[, setdiff(names(house_2021), cols_to_keep)], as.numeric)
house_2021_clean <- house_2021_clean[, colSums(is.na(house_2021_clean)) < nrow(house_2021_clean)]
house_2021_clean <- house_2021_clean[-1,]
house_2021_clean <- house_2021_clean %>%
  mutate(state = str_extract(NAME, "(?<=,\\s)[^,]+$"))



#Poverty 2020
poverty_2020_clean <- poverty_2020
poverty_2020_clean[, setdiff(names(poverty_2020), cols_to_keep)] <- lapply(poverty_2020[, setdiff(names(poverty_2020), cols_to_keep)], as.numeric)
poverty_2020_clean <- poverty_2020_clean[, colSums(is.na(poverty_2020_clean)) < nrow(poverty_2020_clean)]
poverty_2020_clean <- poverty_2020_clean[-1,]
poverty_2020_clean <- poverty_2020_clean %>%
  mutate(state = str_extract(NAME, "(?<=,\\s)[^,]+$"))
poverty_2020_clean1 <- poverty_2020_clean[, c(1,2,dim(poverty_2020_clean)[2],seq(3,dim(poverty_2020_clean)[2],2))] 



#Poverty 2021
poverty_2021_clean <- poverty_2021
poverty_2021_clean[, setdiff(names(poverty_2021), cols_to_keep)] <- lapply(poverty_2021[, setdiff(names(poverty_2021), cols_to_keep)], as.numeric)
poverty_2021_clean <- poverty_2021_clean[, colSums(is.na(poverty_2021_clean)) < nrow(poverty_2021_clean)]
poverty_2021_clean <- poverty_2021_clean[-1,]
poverty_2021_clean <- poverty_2021_clean %>%
  mutate(state = str_extract(NAME, "(?<=,\\s)[^,]+$"))
poverty_2021_clean1 <- poverty_2021_clean[, c(1,2,dim(poverty_2021_clean)[2],seq(3,dim(poverty_2021_clean)[2],2))] 




# Merge populaion, house, poverty
Merged_census_2020 <- merge(merge(population_2020_clean, house_2020_clean, by = "NAME", all = TRUE),poverty_2020_clean,by = "NAME", all = TRUE)
Merged_census_2020 <- Merged_census_2020 |>
  select(GEO_ID,NAME,state,DP05_0001E,B25001_001E,S1701_C01_001E)


Merged_census_2021 <- merge(merge(population_2021_clean, house_2021_clean, by = "NAME", all = TRUE),poverty_2021_clean,by = "NAME", all = TRUE)
Merged_census_2021 <- Merged_census_2021 |>
  select(GEO_ID,NAME,state,DP05_0001E,B25001_001E,S1701_C01_001E)



# Declaration Denials cleaning: Select requested incident types == Flood
DeclarationDenials_clean <- DeclarationDenials |>
  filter(requestedIncidentTypes == "Flood")
# Select data between 2020/01/01 - 2021/12/31
DeclarationDenials_clean <- DeclarationDenials_clean %>%
  filter(incidentBeginDate > as.POSIXct("2020-01-11 00:00:00") & incidentBeginDate < as.POSIXct("2021-12-31 23:59:59"))


# Disaster Declarations Summaries cleaning: Select incident types == Flood
DisasterDeclarationsSummaries_clean <- DisasterDeclarationsSummaries |>
  filter(incidentType == "Flood")|>
  rename(stateAbbreviation= state)
# Merge Declaration with state
state_abb <- distinct(DeclarationDenials[c("stateAbbreviation","state")])
DisasterDeclarationsSummaries_with_state <- left_join(DisasterDeclarationsSummaries_clean,state_abb,by = "stateAbbreviation")
# Select data between 2020/01/01 - 2021/12/31
DisasterDeclarationsSummaries_with_state_clean <- DisasterDeclarationsSummaries_with_state %>%
  filter(incidentBeginDate > as.POSIXct("2020-01-11 00:00:00") & incidentBeginDate < as.POSIXct("2021-12-31 23:59:59"))



# Merge Declaration with state

# Fema Web Disaster Declarations cleaning: Select incident types == Flood
FemaWebDisasterDeclarations_clean <- FemaWebDisasterDeclarations |>
  filter(incidentType == "Flood")
# Select data between 2020/01/01 - 2021/12/31
FemaWebDisasterDeclarations_clean <- FemaWebDisasterDeclarations_clean %>%
  filter(incidentBeginDate > as.POSIXct("2020-01-11 00:00:00") & incidentBeginDate < as.POSIXct("2021-12-31 23:59:59"))


#Fema Web Disaster Summaries cleaning: merge to FemaWebDisasterDeclarations_clean by disater number
dstr_num <- FemaWebDisasterDeclarations_clean$disasterNumber
FemaWebDisasterSummaries_clean <- FemaWebDisasterSummaries |>
  filter(disasterNumber %in% dstr_num)



#Mission assignment
# Select data between 2020/01/01 - 2021/12/31
MissionAssignments_clean <- MissionAssignments %>%
  filter(dateRequested > as.POSIXct("2020-01-11 00:00:00") & dateRequested < as.POSIXct("2021-12-31 23:59:59"))%>%
  rename(stateAbbreviation= stateorTribe)
MissionAssignments_clean <- left_join(MissionAssignments_clean,state_abb,by = "stateAbbreviation")
MissionAssignments_clean <- MissionAssignments_clean |>
  filter(disasterDescription == "Flood")

##details cleaning: select flood cases
StormEvents_details_2020_clean <- StormEvents_details_2020[!is.na(StormEvents_details_2020$FLOOD_CAUSE), ]
StormEvents_details_2021_clean <- StormEvents_details_2021[!is.na(StormEvents_details_2021$FLOOD_CAUSE), ]

##fatalities cleaning:
fatalities_with_detail_2020 <- left_join(StormEvents_details_2020_clean,fatalities_2020,by = "EVENT_ID")
fatalities_with_detail_2021 <- left_join(StormEvents_details_2021_clean,fatalities_2021,by = "EVENT_ID")



## Questions 
#How dangerous are floods? 
#How expensive? 
#Is there any pattern to the kinds of communities that suffer losses from floods?


# EDA 
#1. Dangerous: flood number vs population in different age groups

#flood frequency by state based on NOAA data
dstr_freq3 <- DisasterDeclarationsSummaries_with_state_clean %>%
  group_by(state) %>%
  summarize(Count = n())

#flood frequency based on FEMA data
# dstr_freq2 <- FemaWebDisasterDeclarations_clean %>%
#   group_by(stateName) %>%
#   summarize(Count = n())

#flood frequency based on mission assignment data
dstr_freq <- MissionAssignments_clean %>%
  group_by(state) %>%
  summarize(Count = n())




obligation_sum <- MissionAssignments_clean |>
  group_by(state) |>
  summarise(obligation_total = sum(obligationAmount))



census_sum_2020 <- Merged_census_2020 %>%
  group_by(state) %>%
  summarize(population2020 = sum(DP05_0001E), house2020 = sum(B25001_001E), poverty2020 = sum(S1701_C01_001E))

census_sum_2021 <- Merged_census_2021 %>%
  group_by(state) %>%
  summarize(population2021 = sum(DP05_0001E), house2021 = sum(B25001_001E), poverty2021 = sum(S1701_C01_001E))

merge_flood_census <- left_join(left_join(dstr_freq,census_sum_2020,by = "state"),census_sum_2021,by = "state")
tmp1 <- right_join(right_join(dstr_freq,census_sum_2020,by = "state"),census_sum_2021,by = "state")
tmp2 <- right_join(dstr_freq,obligation_sum,by = "state")
house_per_person2020 <- tmp1$house2020/tmp1$population2020
house_per_person2021 <- tmp1$house2021/tmp1$population2021

#state vs obligation to see the expense
ggplot(tmp2) +
  aes(x = state, y = obligation_total, colour = Count) +
  geom_point(shape = "circle", size = 4L) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(
    x = "State",
    y = "Obligation Amount",
    title = "Obligation amount in different states",
    color = "Flood Frequency"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



#3. disater vs poverty in different races
poverty_rate_race_2020 <- subset(poverty_2020_clean1, select = grep("_C03_", names(poverty_2020_clean1)))[,seq(13,21)]
poverty_rate_2020 <- subset(poverty_2020_clean1, select = grep("_C03_", names(poverty_2020_clean1)))[,1]
colnames(poverty_rate_race_2020) <- c("White Alone","Black or African American alone","American Indian and Alaska Native alone","Asian alone","Native Hawaiian and Other Pacific Islander alone","Some other race alone","Two or more races","Hispanic or Latino origin (of any race)","White alone, not Hispanic or Latino")

poverty_rate_race_2020 <- poverty_rate_race_2020 |>
  mutate(state = poverty_2020_clean1$state)
poverty_rate_2020 <- poverty_rate_2020 |>
  mutate(state = poverty_2020_clean1$state)

poverty_rate_race_2021 <- subset(poverty_2021_clean1, select = grep("_C03_", names(poverty_2021_clean1)))[,seq(13,21)]
poverty_rate_2021 <- subset(poverty_2021_clean1, select = grep("_C03_", names(poverty_2021_clean1)))[,1]
colnames(poverty_rate_race_2021) <- c("White Alone","Black or African American alone","American Indian and Alaska Native alone","Asian alone","Native Hawaiian and Other Pacific Islander alone","Some other race alone","Two or more races","Hispanic or Latino origin (of any race)","White alone, not Hispanic or Latino")

poverty_rate_race_2021 <- poverty_rate_race_2021 |>
  mutate(state = poverty_2021_clean1$state)
poverty_rate_2021 <- poverty_rate_2021 |>
  mutate(state = poverty_2021_clean1$state)


avg_rate_state_2020 <- poverty_rate_race_2020 |>
  group_by(state) |>
  summarize(across(!contains("state"), mean))

avg_whole_rate_state_2020 <- poverty_rate_2020 |>
  group_by(state) |>
  summarize(across(!contains("state"), mean))


avg_rate_state_2021 <- poverty_rate_race_2021 |>
  group_by(state) |>
  summarize(across(!contains("state"), mean))

avg_whole_rate_state_2021 <- poverty_rate_2021 |>
  group_by(state) |>
  summarize(across(!contains("state"), mean))


floodstate_poverty_rate <- left_join(left_join(dstr_freq,avg_whole_rate_state_2020, by = "state"),avg_whole_rate_state_2021, by = "state")
floodstate_poverty_rate["difference"] <- floodstate_poverty_rate$S1701_C03_001E.y-floodstate_poverty_rate$S1701_C03_001E.x
esquisser(floodstate_poverty_rate)

#plot of Poverty rate difference vs flood frequency in each state
ggplot(floodstate_poverty_rate) +
  aes(x = state, y = difference, colour = Count) +
  geom_point(shape = "circle", size = 4L) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(
    x = "State",
    y = "Poverty rate difference",
    title = "Poverty rate difference vs flood frequency in each state",
    color = "Flood frequency count"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


##房子的量差值by state
census_sum <- left_join(census_sum_2020,census_sum_2021,by = "state")
census_sum["difference"] <- census_sum$house2021 - census_sum$house2020
house_flood <- left_join(dstr_freq,census_sum, by = "state")
esquisser(house_flood)

ggplot(house_flood) +
  aes(x = state, y = difference, colour = Count) +
  geom_point(shape = "circle", size = 4L) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(
    x = "State",
    y = "House count difference",
    title = "The amount of houses vs frequency of flood",
    color = "Flood frequency count"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


##death place caused by flood in 2020 and 2021
tmp3 <- na.omit(fatalities_with_detail_2020$FATALITY_LOCATION)

tmp4 <- as.data.frame(table(tmp3))

colnames(tmp4) <- c("location","freq")

tmp5 <- na.omit(fatalities_with_detail_2021$FATALITY_LOCATION)

tmp6 <- as.data.frame(table(tmp5))

colnames(tmp6) <- c("location","freq")
tmp7 <- rbind(tmp4,tmp6)


ggplot(tmp7) +
  aes(x = reorder(location,-freq), y = freq) +
  geom_col(fill = "#112446") +
  labs(
    x = "Fatality location",
    y = "Count",
    title = "Fatality location count"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
fatalities_with_detail <- rbind(fatalities_with_detail_2020,fatalities_with_detail_2021)


# floods caused death rate in each state
tmp8 <- fatalities_with_detail
tmp8["fata_binary"] <- ifelse(is.na(tmp8$FATALITY_ID) == FALSE, 1,0)
tmp8["flood_binary"] <- rep(1,dim(tmp8)[1])
tmp9 <- tmp8 |>
  group_by(STATE)|>
  summarise(sum(fata_binary)/sum(flood_binary))
tmp9 <- tmp9[tmp9$`sum(fata_binary)/sum(flood_binary)` != 0, ]


ggplot(tmp9) +
  aes(x = reorder(STATE,-`sum(fata_binary)/sum(flood_binary)`), y = `sum(fata_binary)/sum(flood_binary)`) +
  geom_col(fill = "#4682B4") +
  labs(
    x = "State",
    y = "Proportion of floods cause fatality",
    title = "Proportion of floods result in fatality in each state"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#各个州的洪水次数
tmp11 <- fatalities_with_detail %>% distinct(END_YEARMONTH, END_DAY, STATE)
tmp11["flood_binary"] <- rep(1,dim(tmp11)[1])
tmp12 <- tmp11 |>
  group_by(STATE)|>
  summarise(sum(flood_binary))


##洪水次数和季节
# 加载 lubridate 包
library(lubridate)

# 将日期转换为月份
DisasterDeclarationsSummaries_with_state_clean$Month <- month(DisasterDeclarationsSummaries_with_state_clean$incidentBeginDate)

# 创建一个函数来将月份映射到季节
get_season <- function(month) {
  if (month %in% 3:5) {
    return("Spring")
  } else if (month %in% 6:8) {
    return("Summer")
  } else if (month %in% 9:11) {
    return("Autumn")
  } else {
    return("Winter")
  }
}

# 应用函数来获取季节
DisasterDeclarationsSummaries_with_state_clean$Season <- sapply(DisasterDeclarationsSummaries_with_state_clean$Month, get_season)
esquisser(DisasterDeclarationsSummaries_with_state_clean)

#flood count in different season
ggplot(DisasterDeclarationsSummaries_with_state_clean) +
  aes(x = Season) +
  geom_bar(fill = "#112446") +
  labs(title = "Flood count in different season") +
  theme_minimal()

#flood count in different season in different states
ggplot(DisasterDeclarationsSummaries_with_state_clean) +
  aes(x = state, fill = Season) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = "Flood count in different season in each state") +
  theme_minimal()





