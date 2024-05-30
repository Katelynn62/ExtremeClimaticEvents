###############################################################################
# QA/QC High Frequency Data - MB 2021
###############################################################################

Profiler_MB21 <- read_csv("../Raw Data/Profiler/Missisquoi_2021_Profiler.csv") %>%
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
  mutate(Turbid = `Turbid_NTU`) %>%
  select(datetime, Month, Day, Year, Hour, Depth, Temp_C, ODOsat, ODOmgL, PC_RFU, Chl_RFU, pH, Turbid, fDOM_RFU) %>%
  mutate(Remove = if_else(Month == 9 & Depth == 2 & PC_RFU > 15, "Yes", "No")) %>% # Removing an outlier in the PC Code. 
  mutate(Remove = if_else(Month == 7 & Depth == 1.0 & Chl_RFU > 4.5, "Yes", Remove),
         Remove = if_else(Month == 8 & Depth == 1.5 & Chl_RFU > 7.5, "Yes", Remove),
         Remove = if_else(Month == 6 & Depth == 1 & Turbid > 25, "Yes", Remove),
         Remove = if_else(Month == 10 & Depth == 1 & Turbid > 25, "Yes", Remove)) %>%
  filter(Remove == "No")


################################################################################

## Depth ##

###############################################################################

# Take a look at 0.5 surface readings. My assumptions are that this is wonky.

Profiler_MB21 %>%
  ggplot(aes(x=datetime, y=Depth)) +
  geom_point()

### The 0.5m profiler only worked for the beginning portion of the summer...so we
### will get rid of that, and the depth of the lake got shallower in late June, 
### so we will omit anything > 3m. 

Profiler_MB21 <- Profiler_MB21 %>%
  filter(Depth != "0.5" & Depth < 3.0)

unique(Profiler_MB21$Depth) # make sure the appropriate depths were removed.

### Conclusion: Surface readings stopped working, so calculations should only be done with 1-2.5m



########################## Identifying Outliers ################################


################################################################################

## Phycocyanin ##

################################################################################

##### Looks like there are some outliers in the data, or perhaps points where the sensor just passed over a colony

### The best way to filter it out would be to separate each depth, and clear out any points that were above x percentage 
##### of the previous reading at that depth.

D1 <-  Profiler_MB21 %>%
  select(PC_RFU, datetime, Depth) %>%
  filter(Depth == "1") %>%
  mutate(Lag = lag(PC_RFU)) %>%
  mutate(Perc = (Lag - PC_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D1 %>%
  ggplot(aes(x=datetime, y=PC_RFU, color=Flag)) +
  geom_point()

## I think most of these are okay, most outliers occur during periods of rapid growth

D1.5 <-  Profiler_MB21 %>%
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

D2 <-  Profiler_MB21 %>%
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

D2.5 <-  Profiler_MB21 %>%
  select(PC_RFU, datetime, Depth) %>%
  filter(Depth == "2.5") %>%
  mutate(Lag = lag(PC_RFU)) %>%
  mutate(Perc = (Lag - PC_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D2.5 %>%
  ggplot(aes(x=datetime, y=PC_RFU, color=Flag)) +
  geom_point()

### I think most of these look fine as well. 

### Rather than going through and joining the dataframe, I'm just going to remove the point manually in the above code. 


################################################################################

## Chlorophyll ##

################################################################################

D1 <-  Profiler_MB21 %>%
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

D1.5 <-  Profiler_MB21 %>%
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

D2 <-  Profiler_MB21 %>%
  select(Chl_RFU, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(Chl_RFU)) %>%
  mutate(Perc = (Lag - Chl_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=Chl_RFU, color=Flag)) +
  geom_point()

### These look okay

D2 %>%
  ggplot(aes(x=datetime, y=Chl_RFU, color=Flag)) +
  geom_point()

D2.5 <-  Profiler_MB21 %>%
  select(Chl_RFU, datetime, Depth) %>%
  filter(Depth == "2.5") %>%
  mutate(Lag = lag(Chl_RFU)) %>%
  mutate(Perc = (Lag - Chl_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 100, "Yes", "No")) %>%
  na.omit()

D2.5 %>%
  ggplot(aes(x=datetime, y=Chl_RFU, color=Flag)) +
  geom_point()

## These look okay.


################################################################################

## Temperature ##

## Need to modify what we flag these as. 

################################################################################

D1 <-  Profiler_MB21 %>%
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

D1.5 <-  Profiler_MB21 %>%
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

D2 <-  Profiler_MB21 %>%
  select(Temp_C, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(Temp_C)) %>%
  mutate(Perc = (Lag - Temp_C)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=Temp_C, color=Flag)) +
  geom_point()

### These look okay

D2.5 <-  Profiler_MB21 %>%
  select(Temp_C, datetime, Depth) %>%
  filter(Depth == "2.5") %>%
  mutate(Lag = lag(Temp_C)) %>%
  mutate(Perc = (Lag - Temp_C)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2.5 %>%
  ggplot(aes(x=datetime, y=Temp_C, color=Flag)) +
  geom_point()

## These look okay.

################################################################################

## O2 ##

################################################################################

D1 <-  Profiler_MB21 %>%
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

D1.5 <-  Profiler_MB21 %>%
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

D2 <-  Profiler_MB21 %>%
  select(ODOsat, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(ODOsat)) %>%
  mutate(Perc = (Lag - ODOsat)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=ODOsat, color=Flag)) +
  geom_point()

### These look okay

D2.5 <-  Profiler_MB21 %>%
  select(ODOsat, datetime, Depth) %>%
  filter(Depth == "2.5") %>%
  mutate(Lag = lag(ODOsat)) %>%
  mutate(Perc = (Lag - ODOsat)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2.5 %>%
  ggplot(aes(x=datetime, y=ODOsat, color=Flag)) +
  geom_point()

################################################################################

## FDOM ##

################################################################################

D1 <-  Profiler_MB21 %>%
  select(fDOM_RFU, datetime, Depth) %>%
  filter(Depth == "1") %>%
  mutate(Lag = lag(fDOM_RFU)) %>%
  mutate(Perc = (Lag - fDOM_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D1 %>%
  ggplot(aes(x=datetime, y=fDOM_RFU, color=Flag)) +
  geom_point()

## These look okay

D1.5 <-  Profiler_MB21 %>%
  select(fDOM_RFU, datetime, Depth) %>%
  filter(Depth == "1.5") %>%
  mutate(Lag = lag(fDOM_RFU)) %>%
  mutate(Perc = (Lag - fDOM_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D1.5 %>%
  ggplot(aes(x=datetime, y=fDOM_RFU, color=Flag)) +
  geom_point()

####  These look okay

D2 <-  Profiler_MB21 %>%
  select(fDOM_RFU, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(fDOM_RFU)) %>%
  mutate(Perc = (Lag - fDOM_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=fDOM_RFU, color=Flag)) +
  geom_point()

### These look okay

D2.5 <-  Profiler_MB21 %>%
  select(fDOM_RFU, datetime, Depth) %>%
  filter(Depth == "2.5") %>%
  mutate(Lag = lag(fDOM_RFU)) %>%
  mutate(Perc = (Lag - fDOM_RFU)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2.5 %>%
  ggplot(aes(x=datetime, y=fDOM_RFU, color=Flag)) +
  geom_point()

## For DOM, Only use 1-2m in analyses. 

################################################################################

## pH ##

################################################################################

D1 <-  Profiler_MB21 %>%
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

D1.5 <-  Profiler_MB21 %>%
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

D2 <-  Profiler_MB21 %>%
  select(pH, datetime, Depth) %>%
  filter(Depth == "2") %>%
  mutate(Lag = lag(pH)) %>%
  mutate(Perc = (Lag - pH)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2 %>%
  ggplot(aes(x=datetime, y=pH, color=Flag)) +
  geom_point()

### These look okay

D2.5 <-  Profiler_MB21 %>%
  select(pH, datetime, Depth) %>%
  filter(Depth == "2.5") %>%
  mutate(Lag = lag(pH)) %>%
  mutate(Perc = (Lag - pH)/Lag * 100) %>%
  mutate(Flag = if_else(abs(Perc) >= 10, "Yes", "No")) %>%
  na.omit()

D2.5 %>%
  ggplot(aes(x=datetime, y=pH, color=Flag)) +
  geom_point()

################################################################################

## Turbidity ##

################################################################################

D1 <-  Profiler_MB21 %>%
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

D1.5 <-  Profiler_MB21 %>%
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

D2 <-  Profiler_MB21 %>%
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

D2.5 <-  Profiler_MB21 %>%
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

write.csv(Profiler_MB21, file="../Clean Data/MB_Profiler_2021_Clean.csv")
