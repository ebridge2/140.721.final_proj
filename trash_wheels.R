library(dplyr)
library(ggplot2)
library(chron)
library(lubridate)
# read in data
# filter by just street trash call types
raw_dt = read.csv("311_Customer_Service_Requests.csv")%>%
dt =  filter(raw_dt, SRType %in% c("SW-Cleaning","SW-Dirty Street Proactive",
                                  "SW-Municipal Trash Can Concern","HCD-Illegal Dumping",
                                  "SW-Clean Up","SW-Dirty Alley","SW-Dirty Alley Proactive",
                                  "SW-Public (Corner) Trash Can Issue",
                                  "SW-Trash Can/Recycling Container Complaint",
                                  "SW-Public (Corner) Trash Can Request/Removal","SW-Park Cleaning"
                                  ))%>%
  filter(Agency == "Solid Waste")

# only include valid vizposed
dt = dt[which(nchar(as.character(dt$ZipCode))>4),]
dt$ZipCode = as.numeric(substr(as.character(dt$ZipCode),1,5))

# create year column for year-specific analysis
dt$year = as.numeric(substr(dt$ServiceRequestNum,1,2))+2000


# plot calls by zipcode and year
dt%>% 
  filter(ZipCode %in% names(sort(table(dt$ZipCode), decreasing = T))[1:15]) %>%
  ggplot(aes(as.factor(ZipCode), fill = as.factor(year))) + geom_bar(stat = "count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(year~.)+
  labs(x = "Zip Code", y = "311 Complaints")+
  theme(legend.position = "none")


# make data set with only top zipcode

trash_data =  dt%>% 
  filter(ZipCode %in% names(sort(table(dt$ZipCode), decreasing = T))[1:5])%>%
  filter(!is.na(Longitude)&!is.na(Latitude))%>%
  filter(Longitude!=-77.0)

# plot
trash_data %>%
  mutate(ZipCode = as.character(ZipCode))%>%
  ggplot(aes(x = Longitude, y = Latitude, color = ZipCode))+
  geom_point(size = 1, alpha = .5)+
  theme_classic()+
  theme(legend.position = "none")

# choose top neighborhoods
nbhds = names(sort(table(trash_data$Neighborhood), decreasing = T))[1:10]

# plot 311 calls by neighborhood
trash_data %>%
  filter(Neighborhood %in% nbhds)%>%
  ggplot(aes(x= Neighborhood, fill = as.factor(year))) + geom_bar(stat = "count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 6))+
  facet_grid(year~.)+
  labs(x = "Neighborhood", y = "311 Complaints")+
  theme(legend.position = "none")


## convert createddate column to time
# add 4*60*60 to convert from Coordinated Universal Time
trash_data %>%
  filter(Neighborhood %in% nbhds[c(1,2,4,5,10)])%>%
  mutate(CreatedDate = mdy_hms(CreatedDate))%>%
  mutate(timeofday = as.POSIXct(strftime(CreatedDate, format="%H:%M:%S"), format="%H:%M:%S")+4*60*60)%>%
  mutate(weekday = weekdays(CreatedDate+4*60*60))%>%
  ggplot(aes(x = timeofday, color = weekday))+geom_line(stat = "density")+
  theme_bw()+
  labs(title = "311 Call By Time of Day", x = "Time of Day")+
  scale_x_datetime(date_labels = "%H:%M")+
  scale_color_discrete(name = "Day of Week")
  
  

  
  
  
