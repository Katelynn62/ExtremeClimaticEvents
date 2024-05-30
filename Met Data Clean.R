###############################################################################
# QA/QC High Frequency Met Data 
###############################################################################
library(tidyverse)
library(lubridate)

###############################################################################

# Load in the files

MB1 <- read_csv(file = "Raw Data/Met/MB_2018_2021_2022_03_29_23_05_05_EDT_1.csv")
MB1 <- MB1[,c(1:3, 24:32)] %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2`,
         PAR = `PAR (S-LIA 10979908:10962917-1), uE, Bree_met_2`,
         Wind_Speed = `Wind Speed (S-WSB 10979908:10964411-1), m/s, Bree_met_2`,
         Gust_Speed = `Gust Speed (S-WSB 10979908:10964411-2), m/s, Bree_met_2`, 
         Solar_Radiation = `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2`,
         Pressure = `Pressure (S-BPB 10979908:10972864-1), mbar, Bree_met_2`, 
         Temperature = `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2`, 
         RH = `RH (S-THB 10979908:10974881-2), %, Bree_met_2`,
         Dew_Point = `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2`) %>%
  select(-`Line#`) %>%
  mutate(Date=mdy(Date))
MB2 <- read_csv(file = "Raw Data/Met/MB_2018_2021_2022_03_29_23_05_05_EDT_2.csv")
MB2 <- MB2[,c(1:3, 24:32)] %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2`,
         PAR = `PAR (S-LIA 10979908:10962917-1), uE, Bree_met_2`,
         Wind_Speed = `Wind Speed (S-WSB 10979908:10964411-1), m/s, Bree_met_2`,
         Gust_Speed = `Gust Speed (S-WSB 10979908:10964411-2), m/s, Bree_met_2`, 
         Solar_Radiation = `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2`,
         Pressure = `Pressure (S-BPB 10979908:10972864-1), mbar, Bree_met_2`, 
         Temperature = `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2`, 
         RH = `RH (S-THB 10979908:10974881-2), %, Bree_met_2`,
         Dew_Point = `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2`) %>%
  select(-`Line#`) %>%
  mutate(Date=mdy(Date))
MB3 <- read_csv(file = "Raw Data/Met/MB_2018_2021_2022_03_29_23_05_05_EDT_3.csv")
MB3 <- MB3[,c(1:3, 24:32)] %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2`,
         PAR = `PAR (S-LIA 10979908:10962917-1), uE, Bree_met_2`,
         Wind_Speed = `Wind Speed (S-WSB 10979908:10964411-1), m/s, Bree_met_2`,
         Gust_Speed = `Gust Speed (S-WSB 10979908:10964411-2), m/s, Bree_met_2`, 
         Solar_Radiation = `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2`,
         Pressure = `Pressure (S-BPB 10979908:10972864-1), mbar, Bree_met_2`, 
         Temperature = `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2`, 
         RH = `RH (S-THB 10979908:10974881-2), %, Bree_met_2`,
         Dew_Point = `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2`) %>%
  select(-`Line#`) %>%
  mutate(Date=mdy(Date))
MB4 <- read_csv(file = "Raw Data/Met/MB_2018_2021_2022_03_29_23_05_05_EDT_4.csv")
MB4 <- MB4[,c(1:3, 24:32)] %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2`,
         PAR = `PAR (S-LIA 10979908:10962917-1), uE, Bree_met_2`,
         Wind_Speed = `Wind Speed (S-WSB 10979908:10964411-1), m/s, Bree_met_2`,
         Gust_Speed = `Gust Speed (S-WSB 10979908:10964411-2), m/s, Bree_met_2`, 
         Solar_Radiation = `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2`,
         Pressure = `Pressure (S-BPB 10979908:10972864-1), mbar, Bree_met_2`, 
         Temperature = `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2`, 
         RH = `RH (S-THB 10979908:10974881-2), %, Bree_met_2`,
         Dew_Point = `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2`) %>%
  select(-`Line#`) %>%
  mutate(Date=mdy(Date))
MB5 <- read_csv(file = "Raw Data/Met/MB_2018_2021_2022_03_29_23_05_05_EDT_5.csv")


MB5.1 <- MB5 %>%
  select(Date, Time, `Wind Speed (S-WSB 10979908:10964410-1), m/s, Bree_met_2 (Missisquoi Bay)`) %>%
  na.omit() %>%
  rename(Wind_Speed = `Wind Speed (S-WSB 10979908:10964410-1), m/s, Bree_met_2 (Missisquoi Bay)`) %>%
  mutate(Date = mdy(Date)) %>%
  filter(Wind_Speed > -500)

MB5.2 <- MB5 %>%
  select(Date, Time, `Gust Speed (S-WSB 10979908:10964410-2), m/s, Bree_met_2 (Missisquoi Bay)`) %>%
  na.omit()%>%
  rename(Gust_Speed = `Gust Speed (S-WSB 10979908:10964410-2), m/s, Bree_met_2 (Missisquoi Bay)`)%>%
  mutate(Date = mdy(Date))

MB5.3 <- MB5 %>%
  select(Date, Time, `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2 (Missisquoi Bay)...6`) %>%
  na.omit()  %>%
  rename(Solar_Radiation = `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2 (Missisquoi Bay)...6`)%>%
  mutate(Date = mdy(Date))

MB5.4 <- MB5 %>%
  select(Date, Time, `Wind Direction (S-WDA 10979908:10969970-1), *, Bree_met_2 (Missisquoi Bay)`) %>%
  na.omit() %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10969970-1), *, Bree_met_2 (Missisquoi Bay)`)%>%
  mutate(Date = mdy(Date))

MB5.5 <- MB5 %>%
  select(Date, Time, `Pressure (S-BPB 10979908:10972864-1), mbar, Bree_met_2 (Missisquoi Bay)`) %>%
  na.omit() %>%
  rename(Pressure = `Pressure (S-BPB 10979908:10972864-1), mbar, Bree_met_2 (Missisquoi Bay)`)%>%
  mutate(Date = mdy(Date))

MB5.6 <- MB5 %>%
  select(Date, Time, `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2 (Missisquoi Bay)...9`) %>%
  na.omit() %>%
  rename(Temperature = `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2 (Missisquoi Bay)...9`)%>%
  mutate(Date = mdy(Date))

MB5.7 <- MB5 %>%
  select( Date, Time, `RH (S-THB 10979908:10974881-2), %, Bree_met_2 (Missisquoi Bay)`) %>%
  na.omit() %>%
  rename(RH = `RH (S-THB 10979908:10974881-2), %, Bree_met_2 (Missisquoi Bay)`)%>%
  mutate(Date = mdy(Date))

MB5.8 <- MB5 %>%
  select( Date, Time, `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2 (Missisquoi Bay)...11`) %>%
  na.omit() %>%
  rename(Dew_Point = `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2 (Missisquoi Bay)...11`)%>%
  mutate(Date = mdy(Date))

MB5.9 <- MB5 %>%
  select( Date, Time, `PAR (S-LIA 10979908:10962917-1), uE, Bree_met_2 (Missisquoi Bay)`) %>%
  na.omit() %>%
  rename(PAR = `PAR (S-LIA 10979908:10962917-1), uE, Bree_met_2 (Missisquoi Bay)`)%>%
  mutate(Date = mdy(Date))

MB5.10 <- MB5 %>%
  select( Date, Time, `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2 (Missisquoi Bay)...13`) %>%
  na.omit() %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2 (Missisquoi Bay)...13`)%>%
  mutate(Date = mdy(Date))

MB5.11 <- MB5 %>%
  select( Date, Time, `Wind Speed (S-WSB 10979908:10964411-1), m/s, Bree_met_2 (Missisquoi Bay)`) %>%
  na.omit() %>%
  rename(Wind_Speed = `Wind Speed (S-WSB 10979908:10964411-1), m/s, Bree_met_2 (Missisquoi Bay)`)%>%
  mutate(Date = mdy(Date)) %>%
  filter(Wind_Speed > -500)

MB5.12 <- MB5 %>%
  select( Date, Time, `Gust Speed (S-WSB 10979908:10964411-2), m/s, Bree_met_2 (Missisquoi Bay)`) %>%
  na.omit() %>%
  rename(Gust_Speed = `Gust Speed (S-WSB 10979908:10964411-2), m/s, Bree_met_2 (Missisquoi Bay)`)%>%
  mutate(Date = mdy(Date))

MB5.13 <- MB5 %>%
  select( Date, Time, `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2 (Missisquoi Bay)...16`) %>%
  na.omit() %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2 (Missisquoi Bay)...16`)%>%
  mutate(Date = mdy(Date))

MB5.14 <- MB5 %>%
  select( Date, Time, `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2 (Missisquoi Bay)...17`) %>%
  na.omit() %>%
  rename(Solar_Radiation = `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2 (Missisquoi Bay)...17`)%>%
  mutate(Date = mdy(Date))

MB5.15 <- MB5 %>%
  select( Date, Time, `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2 (Missisquoi Bay)...18`) %>%
  na.omit() %>%
  rename(Temperature = `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2 (Missisquoi Bay)...18`)%>%
  mutate(Date = mdy(Date))

MB5.16 <- MB5 %>%
  select( Date, Time, `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2 (Missisquoi Bay)...19`) %>%
  na.omit() %>%
  rename(Dew_Point = `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2 (Missisquoi Bay)...19`)%>%
  mutate(Date = mdy(Date))

MB5.17 <- MB5 %>%
  select( Date, Time, `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2 (Missisquoi Bay)...20`) %>%
  na.omit() %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2 (Missisquoi Bay)...20`)%>%
  mutate(Date = mdy(Date))

MB5.18 <- MB5 %>%
  select( Date, Time, `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2 (Missisquoi Bay)...21`) %>%
  na.omit() %>%
  rename(Solar_Radiation = `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2 (Missisquoi Bay)...21`)%>%
  mutate(Date = mdy(Date))

MB5.19 <- MB5 %>%
  select( Date, Time, `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2 (Missisquoi Bay)...22`) %>%
  na.omit() %>%
  rename(Temperature = `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2 (Missisquoi Bay)...22`)%>%
  mutate(Date = mdy(Date))

MB5.20 <- MB5 %>%
  select( Date, Time, `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2 (Missisquoi Bay)...23`) %>%
  na.omit() %>%
  rename(Dew_Point = `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2 (Missisquoi Bay)...23`)%>%
  mutate(Date = mdy(Date))


MB5.21 <- MB5 %>%
  select( Date, Time, `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2`) %>%
  na.omit() %>%
  rename(Wind_Direction = `Wind Direction (S-WDA 10979908:10955776-1), *, Bree_met_2`)%>%
  mutate(Date = mdy(Date))

MB5.22 <- MB5 %>%
  select( Date, Time, `PAR (S-LIA 10979908:10962917-1), uE, Bree_met_2`) %>%
  na.omit() %>% 
  rename(PAR = `PAR (S-LIA 10979908:10962917-1), uE, Bree_met_2`)%>%
  mutate(Date = mdy(Date))

MB5.23 <- MB5 %>%
  select( Date, Time, `Wind Speed (S-WSB 10979908:10964411-1), m/s, Bree_met_2`) %>%
  na.omit() %>%
  rename(Wind_Speed = `Wind Speed (S-WSB 10979908:10964411-1), m/s, Bree_met_2`)%>%
  mutate(Date = mdy(Date))

MB5.24 <- MB5 %>%
  select( Date, Time, `Gust Speed (S-WSB 10979908:10964411-2), m/s, Bree_met_2`) %>%
  na.omit() %>%
  rename(Gust_Speed = `Gust Speed (S-WSB 10979908:10964411-2), m/s, Bree_met_2`)%>%
  mutate(Date = mdy(Date))

MB5.25 <- MB5 %>%
  select( Date, Time, `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2`) %>%
  na.omit() %>%
  rename(Solar_Radiation = `Solar Radiation (S-LIB 10979908:10969906-1), W/m^2, Bree_met_2`)%>%
  mutate(Date = mdy(Date))

MB5.26 <- MB5 %>%
  select( Date, Time, `Pressure (S-BPB 10979908:10972864-1), mbar, Bree_met_2`) %>%
  na.omit() %>%
  rename(Pressure = `Pressure (S-BPB 10979908:10972864-1), mbar, Bree_met_2`)%>%
  mutate(Date = mdy(Date))

MB5.27 <- MB5 %>%
  select( Date, Time, `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2`) %>%
  na.omit() %>%
  rename(Temperature = `Temperature (S-THB 10979908:10974881-1), *C, Bree_met_2`)%>%
  mutate(Date = mdy(Date))

MB5.28 <- MB5 %>%
  select( Date, Time, `RH (S-THB 10979908:10974881-2), %, Bree_met_2`) %>%
  na.omit()  %>%
  rename(RH = `RH (S-THB 10979908:10974881-2), %, Bree_met_2`)%>%
  mutate(Date = mdy(Date))

MB5.29 <- MB5 %>%
  select( Date, Time, `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2`) %>%
  na.omit() %>%
  rename(Dew_Point = `Dew Point (S-THB 10979908:10974881-3), *C, Bree_met_2`)%>%
  mutate(Date = mdy(Date))


## Wind Speed

MB5.1.1 <- full_join(MB5.1, MB5.11) %>%
  full_join(MB5.23) 

# Wind Direction 

MB5.1.2 <- full_join(MB5.4, MB5.10) %>%
  full_join(MB5.13) %>%
  full_join(MB5.17) %>%
  full_join(MB5.21) 

# Gust Speed

MB5.1.3 <- full_join(MB5.2, MB5.12) %>%
  full_join(MB5.24) 


# Solar radiation 

MB5.1.4 <- full_join(MB5.3, MB5.14) %>%
  full_join(MB5.18) %>%
  full_join(MB5.25)

# Pressure
MB5.1.5 <- full_join(MB5.5, MB5.26) 

# DP 
MB5.1.6 <- full_join(MB5.8, MB5.16) %>%
  full_join(., MB5.20) %>%
  full_join(., MB5.29)

# SR 

MB5.1.7 <- full_join(MB5.3, MB5.14) %>%
  full_join(., MB5.18) %>%
  full_join(., MB5.25)

# PAR

MB5.1.8 <- full_join(MB5.9, MB5.22) 


# Temp 

MB5.1.9 <- full_join(MB5.6, MB5.15) %>%
  full_join(., MB5.19) %>%
  full_join(., MB5.27)


# RH

MB5.1.10 <- full_join(MB5.7, MB5.28) 


## Join all togetjer


MB5 <- left_join(MB5.1.9, MB5.1.10, by=c("Date", "Time")) %>%
  left_join(., MB5.1.8) %>%
  left_join(., MB5.1.7) %>%
  left_join(., MB5.1.6) %>%
  left_join(., MB5.1.5) %>%
  left_join(., MB5.1.4) %>%
  left_join(., MB5.1.3) %>%
  left_join(., MB5.1.2) %>%
  left_join(., MB5.1.1) 




MB_Met <- full_join(MB1, MB2) %>%
  full_join(., MB3) %>%
  full_join(., MB4) %>%
  full_join(., MB5) %>%
  mutate(Month = month(Date),
         Day = day(Date),
         Year = year(Date), 
         Hour = hour(Time),
         Minute = minute(Time),
         Second = second(Time), 
         Datetime = make_datetime(Year, Month, Day, Hour, Minute, Second))

### St. Albans Data

str(SA_Met)
SA_Met <- read_csv(file = "Raw Data/Met/STA_met_data_2021.csv") %>%
  mutate(PAR = `PAR.PAR@StAlbans_Met`,
         Wind_Dir = `Wind Dir.Wind_Direction@StAlbans_Met`,
         Wind_Speed_m.s = `Wind Vel.Wind_Speed@StAlbans_Met`,
         Gust_Speed_m.s = `Wind Gust Vel.Wind_Gust@StAlbans_Met`,
         Solar_Radiation = `Solar Rad.Solar_Radiation@StAlbans_Met`,
         Pressure = `Atmos Pres.ATM_Pressure@StAlbans_Met`,
         Temp = `Air Temp.Air_Temp@StAlbans_Met`,
         RH = `Rel Humidity.Relative_Humidity@StAlbans_Met`,
         Dew_Point = `Temperature.Dew_Point@StAlbans_Met`,
         Datetime = ymd_hms(Timestamp),
         Year = year(Timestamp),
         Month = month(Timestamp),
         Day = day(Timestamp),
         Date = make_date(Year, Month, Day)) %>%
  select(Datetime, Date, Year, Month, Day, Wind_Speed_m.s, Temp, Wind_Dir) %>%
  na.omit()

write_csv(SA_Met, "Clean Data/SA_Met_Clean.csv")


### Trying to model Venice Bay Wind Data for Missisquoi Bay 2017 Wind Speed.


### Venice Data

Venice <- read.table("Raw Data/Met/Venice_2018.txt", header=F) %>%
  select(V1, V2, V3, V4, V5, V6, V7) %>%
  rename(Year = V1, 
         Month = V2, 
         Day = V3, 
         Hour = V4, 
         Minute = V5,
         Wind_Dir = V6,
         Wind_Speed_m.s = V7) %>%
  mutate(Datetime = make_datetime(Year, Month, Day, Hour, Minute),
         Date = make_date(Year, Month, Day)) %>%
  select(Date, Wind_Speed_m.s, Wind_Dir) %>%
  group_by(Date) %>%
  summarize(Avg.Wind = mean(Wind_Speed_m.s),
            Avg.Dir = mean(Wind_Dir)) %>%
  mutate(Station = "Venice")


  
####### Logistic regression for MB 21 Missing Temp. 

MB1821_Temp <- MB_Met %>%
  filter(Temperature >= -5 & Temperature <= 35) %>%
  filter(Year == 2018 | Year == 2021) %>%
  select(Date, Temperature) %>%
  mutate(Station = "MB") %>%
  rename(Temp = Temperature)

SA18 <- SA_Met %>%
  select(Temp, Date, Year) %>%
  filter(Year == 2018) %>% 
  mutate(Station = "SA")


SA21 <- SA_Met %>%
  select(Temp, Date, Year) %>%
  filter(Year == 2021) %>% 
  mutate(Station = "SA")


Temp1821 <- full_join(MB1821_Temp, SA18) %>%
  full_join(., SA21) %>%
  group_by(Date, Station) %>%
  summarise(Temp = mean(Temp)) %>%
  pivot_wider(names_from = "Station", values_from = "Temp") %>%
  na.omit()

Fig <- Temp1821 %>%
  ggplot(aes(x=MB, y=SA)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method="lm") +
  ylab("St. Albans Daily Temperature (C)")+
  xlab("Missiqoui Bay Daily Temperature (C)")

test <- lm(MB ~ SA, data=Temp1821)
summary(test)


tiff("..//Figures/SAvMBTemp.tiff", width = 5, height = 5, units = "in", res = 600, bg = "white")

Fig

dev.off()



MB21_Temp <- 1.01901*SA21$Temp - 0.26659

## Rsquared == 0.9744

MB21_NewTemp <- as.data.frame(cbind(SA21, MB21_Temp)) %>%
  select(Date, MB21_Temp) %>%
  group_by(Date) %>%
  summarize(NewTemp = mean(MB21_Temp))

MB_Met_Temp <- MB_Met %>%
  mutate(Year = year(Date))  %>%
  group_by(Date, Year) %>%
  summarize(Temp = mean(Temperature)) %>%
  full_join(., MB21_NewTemp, by=c("Date")) %>%
  mutate(Year = year(Date))

MB_Met_Temp$Temp <- replace_na(MB_Met_Temp$Temp, -500)  

MB_Met_Temp <- MB_Met_Temp %>%  
  mutate(Remove = if_else(Temp < 0 | Temp > 35, "Yes", "No")) %>%
  mutate(Temp1 = if_else(Year == 2021 & Remove =="Yes", NewTemp, Temp))


### Figure for MB21 Temp and the New Temp

MB_Met_Fig <- MB_Met_Temp %>%
  select(-NewTemp) %>%
  na.omit() %>%
  pivot_longer(cols=c(Temp, Temp1), names_to = "Type", values_to = "Temp")

MB_Met_Fig %>%
  mutate(Temp = if_else(Temp < -10 | Temp > 35, 0, Temp)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month  = month(Date)) %>%
  filter(Year == 2021) %>%
  filter(Month < 11) %>%
  ggplot(aes(x=Date, y=Temp, color=Type, group=Type))+
  geom_line(linewidth=1.25)+
  geom_point(size=2) +
  scale_color_manual(values=c("black","dodgerblue3"))+
  theme_classic() +
  ylim(0,35)

### Join this new temperature column to MB Met data.

MB_Met <- full_join(MB_Met, MB_Met_Temp, by=c("Date", "Year"))

write.csv(MB_Met, "Clean Data/MB_Met_Clean.csv")

#####################


MB18_Wind <- MB_Met %>%
  select(Wind_Speed_m.s, Wind_Dir, Date, Year) %>%
  filter(Year == 2018) %>% 
  group_by(Date) %>%
  summarize(Avg.Wind = mean(Wind_Speed_m.s),
            Avg.Dir = mean(Wind_Dir))  %>%
  mutate(Station = "MB")

Wind18 <- full_join(MB18_Wind, Venice) %>%
  pivot_wider(names_from = "Station", values_from = "Avg.Wind") %>%
  na.omit()


Wind18 %>%
  ggplot(aes(x=MB, y=Venice)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method="lm") +
  ylab("Venice Daily Wind")+
  xlab("Missiqoui Bay Daily Wind")


Wind18_glm <- glm()
## Well this is just awful (:


### MB vs. SA Temperature




################################################################################
# Temperature QAQC
################################################################################

MB21 <-  MB_Met %>%
  select(Temp, Date, Time, Year) %>%
  filter(Year == "2021") %>%
  mutate(Lag = lag(Temp)) %>%
  mutate(Perc = (Lag - Temp)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

MB18 %>%
  ggplot(aes(x=Datetime, y=Temp, color=Flag)) +
  geom_point()



SA21 <-  SA_Met %>%
  select(Wind_Speed_m.s, Datetime, Year) %>%
  filter(Year == "2021") %>%
  mutate(Lag = lag(Wind_Speed_m.s)) %>%
  mutate(Perc = (Lag - Wind_Speed_m.s)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

SA21 %>%
  ggplot(aes(x=Datetime, y=Wind_Speed_m.s)) +
  geom_line()

SA18 <-  SA_Met %>%
  select(Temp, Datetime, Year) %>%
  filter(Year == "2017") %>%
  mutate(Lag = lag(Temp)) %>%
  mutate(Perc = (Lag - Temp)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

SA18 %>%
  ggplot(aes(x=Datetime, y=Temp)) +
  geom_line()

