###############################################################################
# QA/QC High Frequency Data - MB 2018
###############################################################################
library(tidyverse)
library(lubridate)

###############################################################################

Profiler_MB18 <- read_csv("Raw Data/Profiler/Missisquoi_2018_Profiler.csv") %>%
  mutate(datetime = mdy_hm(TIMESTAMP)) %>%
  mutate(Year = year(datetime)) %>%
  mutate(Month = month(datetime)) %>%
  mutate(Day = day(datetime)) %>%
  mutate (Hour = hour(datetime)) %>%
  mutate(Minute = 0) %>%
  mutate(datetime = make_datetime(Year, Month, Day, Hour, Minute)) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  mutate(Depth = Depth_meters) %>%
  mutate(Depth = if_else(Depth <=0.8, 0.5, Depth)) %>%
  mutate(Depth = if_else(Depth >0.8 & Depth <=1.3, 1.0, Depth)) %>%
  mutate(Depth = if_else(Depth >1.3 & Depth <=1.8, 1.5, Depth)) %>%
  mutate(Depth = if_else(Depth >1.8 & Depth <=2.3, 2.0, Depth)) %>%
  mutate(Depth = if_else(Depth >2.3 & Depth <=2.8, 2.5, Depth)) %>%
  mutate(Depth = if_else(Depth >2.8 & Depth <=3.3, 3.0, Depth)) %>%
  mutate(ODOmgL = `ODO_mg/L`) %>%
  mutate(ODOsat = `ODOsat_%`) %>%
  mutate(PC_RFU = `BGA_PC_RFU`) %>%
  mutate(Chl_RFU = `Chl_RFU`) %>%
  mutate(Turbid = `Turbid+_NTU`) %>%
  select(datetime, Month, Day, Year, Hour, Depth, Temp_C, ODOsat, PC_RFU, Chl_RFU, pH, Turbid) %>%
  mutate(Remove = if_else(Month == 7 & Depth == 1 & PC_RFU > 60, "Yes", "No"),
         Remove = if_else(Month == 10 & Depth == 1 & PC_RFU > 40, "Yes", Remove)) %>%
  mutate(PC_RFU = if_else(PC_RFU < 0, 0, PC_RFU)) %>%
  mutate(Chl_RFU = if_else(Chl_RFU < 0, 0, Chl_RFU)) %>%
  mutate(Remove = if_else(Depth == 0.5 & Chl_RFU > 20, "Yes", Remove),
         Remove = if_else(Depth == 0.5 & Turbid > 250, "Yes", Remove),
         Remove = if_else(Depth == 2 & Turbid > 200, "Yes", Remove)) %>%
  filter(Remove == "No")

### Need to note that when doing 02 Analysis we need to remove the flat lines. 
################################################################################

## Depth ##

###############################################################################

# Take a look at 0.5 surface readings. My assumptions are that this is wonky.

Profiler_MB18 %>%
  ggplot(aes(x=datetime, y=Depth)) +
  geom_point()

### The The lake started off deep, but got very shallow - use only 0-2m

Profiler_MB18 <- Profiler_MB18 %>%
  filter(Depth < 2.5)

unique(Profiler_MB18$Depth) # make sure the appropriate depths were removed.


########################## Identifying Outliers ################################


################################################################################

## Phycocyanin ##

################################################################################

##### Looks like there are some outliers in the data, or perhaps points where the sensor just passed over a colony

### The best way to filter it out would be to separate each depth, and clear out any points that were above x percentage 
##### of the previous reading at that depth.

D05 <-  Profiler_MB18 %>%
  select(PC_RFU, datetime, Depth) %>%
  filter(Depth == "0.5") %>%
  mutate(Lag = lag(PC_RFU)) %>%
  mutate(Perc = (Lag - PC_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D05 %>%
  ggplot(aes(x=datetime, y=PC_RFU, color=Flag)) +
  geom_point()

################################################################################


D1 <-  Profiler_MB18 %>%
  select(PC_RFU, datetime, Depth) %>%
  filter(Depth == "1") %>%
  mutate(Lag = lag(PC_RFU)) %>%
  mutate(Perc = (Lag - PC_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D1 %>%
  ggplot(aes(x=datetime, y=PC_RFU, color=Flag)) +
  geom_point()

## Outliers in July and October

D1.5 <-  Profiler_MB18 %>%
  select(PC_RFU, datetime, Depth) %>%
  filter(Depth == "1.5") %>%
  mutate(Lag = lag(PC_RFU)) %>%
  mutate(Perc = (Lag - PC_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D1.5 %>%
  ggplot(aes(x=datetime, y=PC_RFU, color=Flag)) +
  geom_point()

#### I think most of these look pkay, and I will keep them. 

D2 <-  Profiler_MB18 %>%
  select(PC_RFU, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(PC_RFU)) %>%
  mutate(Perc = (Lag - PC_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=PC_RFU, color=Flag)) +
  geom_point()

### One outlier occures in October. I will manually remove that point.

D2 <- D2 %>%
  filter(PC_RFU < 15)

D2 %>%
  ggplot(aes(x=datetime, y=PC_RFU, color=Flag)) +
  geom_point()


### Remove anything below 0 here.


################################################################################

## Chlorophyll ##

################################################################################

D05 <-  Profiler_MB18 %>%
  select(Chl_RFU, datetime, Depth) %>%
  filter(Depth == "0.5") %>%
  mutate(Lag = lag(Chl_RFU)) %>%
  mutate(Perc = (Lag - Chl_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D05 %>%
  ggplot(aes(x=datetime, y=Chl_RFU, color=Flag)) +
  geom_point()

#### Remove that one point. 


D1 <-  Profiler_MB18 %>%
  select(Chl_RFU, datetime, Depth) %>%
  filter(Depth == "1") %>%
  mutate(Lag = lag(Chl_RFU)) %>%
  mutate(Perc = (Lag - Chl_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D1 %>%
  ggplot(aes(x=datetime, y=Chl_RFU, color=Flag)) +
  geom_point()

## Remove the first flagged point - that looks like an outlier...rest ok.

D1.5 <-  Profiler_MB18 %>%
  select(Chl_RFU, datetime, Depth) %>%
  filter(Depth == "1.5") %>%
  mutate(Lag = lag(Chl_RFU)) %>%
  mutate(Perc = (Lag - Chl_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D1.5 %>%
  ggplot(aes(x=datetime, y=Chl_RFU, color=Flag)) +
  geom_point()

####  Two outliers in August. 

D2 <-  Profiler_MB18 %>%
  select(Chl_RFU, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(Chl_RFU)) %>%
  mutate(Perc = (Lag - Chl_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=Chl_RFU, color=Flag)) +
  geom_point()


################################################################################

## Temperature ##

################################################################################

D05 <-  Profiler_MB18 %>%
  select(Temp_C, datetime, Depth) %>%
  filter(Depth == "0.5") %>%
  mutate(Lag = lag(Temp_C)) %>%
  mutate(Perc = (Lag - Temp_C)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D05 %>%
  ggplot(aes(x=datetime, y=Temp_C, color=Flag)) +
  geom_point()

##
D1 <-  Profiler_MB18 %>%
  select(Temp_C, datetime, Depth) %>%
  filter(Depth == "1") %>%
  mutate(Lag = lag(Temp_C)) %>%
  mutate(Perc = (Lag - Temp_C)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D1 %>%
  ggplot(aes(x=datetime, y=Temp_C, color=Flag)) +
  geom_point()

## These look okay

D1.5 <-  Profiler_MB18 %>%
  select(Temp_C, datetime, Depth) %>%
  filter(Depth == "1.5") %>%
  mutate(Lag = lag(Temp_C)) %>%
  mutate(Perc = (Lag - Temp_C)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D1.5 %>%
  ggplot(aes(x=datetime, y=Temp_C, color=Flag)) +
  geom_point()

####  These look okay

D2 <-  Profiler_MB18 %>%
  select(Temp_C, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(Temp_C)) %>%
  mutate(Perc = (Lag - Temp_C)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=Temp_C, color=Flag)) +
  geom_point()


## These look okay.

################################################################################

## O2 ##

################################################################################

D05 <-  Profiler_MB18 %>%
  select(ODOsat, datetime, Depth) %>%
  filter(Depth == "0.5") %>%
  mutate(Lag = lag(ODOsat)) %>%
  mutate(Perc = (Lag - ODOsat)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D05 %>%
  ggplot(aes(x=datetime, y=ODOsat, color=Flag)) +
  geom_point()

### .....Are the flat line 0 okay? No. there should be a lot of 02 in the surface.

D1 <-  Profiler_MB18 %>%
  select(ODOsat, datetime, Depth) %>%
  filter(Depth == "1") %>%
  mutate(Lag = lag(ODOsat)) %>%
  mutate(Perc = (Lag - ODOsat)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D1 %>%
  ggplot(aes(x=datetime, y=ODOsat, color=Flag)) +
  geom_point()

## These look okay

D1.5 <-  Profiler_MB18 %>%
  select(ODOsat, datetime, Depth) %>%
  filter(Depth == "1.5") %>%
  mutate(Lag = lag(ODOsat)) %>%
  mutate(Perc = (Lag - ODOsat)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D1.5 %>%
  ggplot(aes(x=datetime, y=ODOsat, color=Flag)) +
  geom_point()

####  These look okay

D2 <-  Profiler_MB18 %>%
  select(ODOsat, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(ODOsat)) %>%
  mutate(Perc = (Lag - ODOsat)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=ODOsat, color=Flag)) +
  geom_point()

################################################################################

## pH ##

################################################################################

D05 <-  Profiler_MB18 %>%
  select(pH, datetime, Depth) %>%
  filter(Depth == "0.5") %>%
  mutate(Lag = lag(pH)) %>%
  mutate(Perc = (Lag - pH)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D05 %>%
  ggplot(aes(x=datetime, y=pH, color=Flag)) +
  geom_point()

D1 <-  Profiler_MB18 %>%
  select(pH, datetime, Depth) %>%
  filter(Depth == "1") %>%
  mutate(Lag = lag(pH)) %>%
  mutate(Perc = (Lag - pH)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D1 %>%
  ggplot(aes(x=datetime, y=pH, color=Flag)) +
  geom_point()

## These look okay

D1.5 <-  Profiler_MB18 %>%
  select(pH, datetime, Depth) %>%
  filter(Depth == "1.5") %>%
  mutate(Lag = lag(pH)) %>%
  mutate(Perc = (Lag - pH)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D1.5 %>%
  ggplot(aes(x=datetime, y=pH, color=Flag)) +
  geom_point()

####  These look okay

D2 <-  Profiler_MB18 %>%
  select(pH, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(pH)) %>%
  mutate(Perc = (Lag - pH)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=pH, color=Flag)) +
  geom_point()

################################################################################

## Turbidity ##

################################################################################

D05 <-  Profiler_MB18 %>%
  select(Turbid, datetime, Depth) %>%
  filter(Depth == "0.5") %>%
  mutate(Lag = lag(Turbid)) %>%
  mutate(Perc = (Lag - Turbid)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D05 %>%
  ggplot(aes(x=datetime, y=Turbid, color=Flag)) +
  geom_point()

# Remove the point that is above 250.
D1 <-  Profiler_MB18 %>%
  select(Turbid, datetime, Depth) %>%
  filter(Depth == "1") %>%
  mutate(Lag = lag(Turbid)) %>%
  mutate(Perc = (Lag - Turbid)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D1 %>%
  ggplot(aes(x=datetime, y=Turbid, color=Flag)) +
  geom_point()

# June Turbidity and Ocotber, 

## These look okay

D1.5 <-  Profiler_MB18 %>%
  select(Turbid, datetime, Depth) %>%
  filter(Depth == "1.5") %>%
  mutate(Lag = lag(Turbid)) %>%
  mutate(Perc = (Lag - Turbid)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D1.5 %>%
  ggplot(aes(x=datetime, y=Turbid, color=Flag)) +
  geom_point()

####  These look okay

D2 <-  Profiler_MB18 %>%
  select(Turbid, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(Turbid)) %>%
  mutate(Perc = (Lag - Turbid)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=Turbid, color=Flag)) +
  geom_point()

### These look okay

D2.5 <-  Profiler_MB18 %>%
  select(Turbid, datetime, Depth) %>%
  filter(Depth == "2.5") %>%
  mutate(Lag = lag(Turbid)) %>%
  mutate(Perc = (Lag - Turbid)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D2.5 %>%
  ggplot(aes(x=datetime, y=Turbid, color=Flag)) +
  geom_point()


### Write clean CSV file.

write.csv(Profiler_MB18, file="MB_Profiler_2018_Clean.csv")
