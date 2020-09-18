##Where should I buy my next house?
#comparing various cities in the raleigh metro area
#assuming I want highest possible appreciation
#based on current growth trajectories

#downloaded this data from:
#https://www.zillow.com/research/data/ 
#Geography set to "City"
  
#more about ZHVI
#https://www.zillow.com/research/why-zillow-home-value-index-better-17742/
#the one-sentence explanation is that Zillow takes all estimated home values 
#for a given region and month (Zestimates), takes a median of these values, 
#applies some adjustments to account for seasonality or errors in individual 
#home estimates, and then does the same across all months over the past 20 years 
#and for many different geography levels (ZIP, neighborhood, city, county, 
#metro, state, and country)

library(readr)
library(dpyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)

setwd('~/Documents/Job Search/Data Science/projects/Raleigh_RealEstate')

##All Homes
#includes both Single Family Residences, Condos and Coops
#based on the name, seems to be the middle tier which makes sense as it should be the median of all


######
#first look around
all_homes_city <- read_csv("City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv") 
head(all_homes_city)
str(all_homes_city)

all_nc_city <- all_homes_city %>% filter(State == "NC")
str(all_nc_city)

dates_only <- all_nc_city[,c(9:301)]
concise_nc <- all_nc_city %>% select(RegionName, Metro)
concise_nc <- cbind(concise_nc,dates_only)

#look at what is within raleigh, and durham-chapel hill
raleigh_towns <- concise_nc %>%
  filter(Metro == "Raleigh")
unique(raleigh_towns$RegionName)
#ok i want to look at cary, apex, garner, holly springs, morrisville

dch_towns <- concise_nc %>%
  filter(Metro == "Durham-Chapel Hill")
unique(dch_towns$RegionName)
#LOL a lot less there, let's look at everything

tri_cities <- concise_nc %>% 
  filter(RegionName == "Raleigh" | RegionName == "Cary" | RegionName == "Morrisville" | RegionName == "Holly Springs" | RegionName == "Apex" | RegionName == "Garner" | RegionName == 
           "Durham" | RegionName == "Chapel Hill" | RegionName == "Carrboro") 

#ok getting the dates in shape for ggplot
tri_long <- tri_cities %>% 
  gather("Date", "ZHVI", -RegionName, -Metro)
str(tri_long)
tri_long$Date <- as.Date(tri_long$Date)
str(tri_long)

ggplot(tri_long, aes(x=Date, y=ZHVI, color = RegionName)) +
  geom_line() +
  facet_wrap(~Metro) +
  scale_y_continuous(labels = comma)

#observations in no particular order
#Chapel Hill is the wealthiest and looks like it grew insanely over the time period (eg230K-400K = +170K)
#Cary is the next wealthiest
#by comparison, Raleigh grew from ~150-240K over the same period = +90K
#just by eyeballing, many spots have similar rate of growth incl Durham, Raleigh, Morrisville, Holly Springs
#Garner is the lowest! 




#########let's check out all the data

#all homes in Raleigh 
all_homes <- raleigh_towns %>% 
  filter(RegionName == "Raleigh" | RegionName == "Cary" | RegionName == "Morrisville" | RegionName == "Holly Springs" | RegionName == "Apex" | RegionName == "Garner") 
all_homes$Metro <- NULL #dont need this anymore
all_homes$Type <- "All Homes"

#all single family residences
sfr_city <- read_csv("City_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv")
sfr_city$Type <- "Single Family Residence"

#all condos and coops
condos_city <- read_csv("City_zhvi_uc_condo_tier_0.33_0.67_sm_sa_mon.csv")
condos_city$Type <- "Condos"

#four bedroom houses
four_bdrm_city <- read_csv("City_zhvi_bdrmcnt_4_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv")
four_bdrm_city$Type <- "Four Bedroom House"

#top tier (65th-95th percentile housing value)
#csv name suggests it is 67-100 percentile
top_tier_city <- read_csv("City_zhvi_uc_sfrcondo_tier_0.67_1.0_sm_sa_mon.csv")
top_tier_city$Type <- "Top Tier Value"

#bottom tier (5th-35th percentile housing value)
#csv name suggests it is 0-33 percentile
bot_tier_city <- read_csv("City_zhvi_uc_sfrcondo_tier_0.0_0.33_sm_sa_mon.csv")
bot_tier_city$Type <- "Bottom Tier Value"


#put them together! 
most_data <- rbind(sfr_city, condos_city, four_bdrm_city, top_tier_city, bot_tier_city)

#get data for NC, get dates, city names and the type of data
most_nc_data <- most_data %>% filter(State == "NC")
dates <- most_nc_data[,c(9:301)]
concise_nc_data <- most_nc_data %>% select(RegionName, Type)
most_concise_nc <- cbind(concise_nc_data, dates)

# get cities of interest
#i think faster to get cities of interest in first step before gathering
most_raleigh_cities <- most_concise_nc %>%
  filter(RegionName == "Raleigh" | RegionName == "Cary" | RegionName == "Morrisville" | RegionName == "Holly Springs" | RegionName == "Apex" | RegionName == "Garner")


all_raleigh_cities <- rbind(all_homes, most_raleigh_cities)
#quick check
unique(all_raleigh_cities$RegionName)
unique(all_raleigh_cities$Type)

#gather eg make long form
all_raleigh_long <- all_raleigh_cities %>% 
  gather("Date", "ZHVI", -Type, -RegionName) %>%
  arrange(Type, RegionName) #organize

all_raleigh_long$Date <- as.Date(all_raleigh_long$Date)

#set order of facets in advance
all_raleigh_long$Type <- factor(all_raleigh_long$Type, levels = c("Single Family Residence", "Condos", "Four Bedroom House", "Bottom Tier Value", "All Homes", "Top Tier Value"))
str(all_raleigh_long)
levels(all_raleigh_long$Type)

#ok first viz attempt
ggplot(all_raleigh_long, aes(x=Date, y=ZHVI, color = RegionName)) +
  geom_line() +
  facet_wrap(~Type)


#write
write.csv(all_raleigh_long, file = "all_raleigh_long.csv")


#Start visualizing

all_raleigh_long %>%
  filter(Type == "Single Family Residence" | Type == "Condos" | Type == "Bottom Tier Value" | Type == 
           "Top Tier Value") %>%
  ggplot(aes(x=Date, y=ZHVI, color = RegionName)) +
  geom_line() +
  facet_wrap(~Type) +
  scale_y_continuous(labels = comma)

all_raleigh_long %>%
  filter(Type == "Top Tier Value") %>%
  ggplot(aes(x=Date, y=ZHVI, color = RegionName)) +
  geom_line() +
  ggtitle("Top Tier: median of 65-95th Percentile") +
  scale_y_continuous(labels = comma)

#what did we learn today?
#THE RICH GET RICHER! 

#Cary is the wealthiest of the Raleigh towns, even when accounting for condos (none in Cary, fair amount in Raleigh)
#the top tier homes in Raleigh have appreciated better than other homes, and are more comparable to Cary than any other categories

#Condos did not appreciate as well as SFRs over the period 2000-2020
#condo 2000-2020 145->225 (80K diff)
#SFR 2000-2020 175->290 (115K diff)
#in recent times, appreciation was comparable
#~150->220 for a condo in Raleigh for 2015-2020 (70K)
#~225->290 for a SFR in Raleigh for 2015-2020 (65K)
#BUT this could be due to increase in luxury condos built pulling up the median, 
#rather than all condos increasing in value

#Top Tier
#appreciation for Cary vs Raleigh somewhat comparable in top tier
#but it is clear upon closer inspection that the angle of the slope is higher for Cary

all_raleigh_long %>%
  filter(Type == "Top Tier Value") %>%
  filter(RegionName == "Raleigh" | RegionName == "Cary" | RegionName == "Garner") %>%
  ggplot(aes(x=Date, y=ZHVI, color = RegionName)) +
  geom_point() +
  geom_smooth(method = lm, aes(color = RegionName)) +
  ggtitle("Top Tier: median of 65-95th Percentile") +
  scale_y_continuous(labels = comma)


#okay lets look at what factors are most associated with the value

#set factor, levels so that all compares to Raleigh
all_raleigh_long$RegionName <- factor(all_raleigh_long$RegionName, levels = c("Raleigh", "Cary", "Apex", "Morrisville", "Holly Springs", "Garner"))

#tryna get the dates as ordinal numbers
all_raleigh_long$DateSince1970 <- as.numeric(all_raleigh_long$Date)
head(all_raleigh_long$DateSince1970)
all_raleigh_long$OrdinalDay <- all_raleigh_long$DateSince1970 - 9525
head(all_raleigh_long$OrdinalDay)
#all_raleigh_long$Year <- all_raleigh_long$OrdinalDay / 365 #As necessary

#crank some models
#raleigh
all_homes_raleigh_long <- all_raleigh_long %>%
  filter(Type == "All Homes" & RegionName == "Raleigh")

mod_time <- glm(ZHVI ~ Year, data = all_homes_raleigh_long)
summary(mod_time)

#cary
all_homes_cary_long <- all_raleigh_long %>%
  filter(Type == "All Homes" & RegionName == "Cary")

mod_time_cary <- glm(ZHVI ~ Year, data = all_homes_cary_long)
summary(mod_time_cary)

#quick Raleigh vs Cary viz
raleigh_cary_long <- rbind(all_homes_raleigh_long, all_homes_cary_long)

ggplot(raleigh_cary_long, aes(x=Year+1996, y=ZHVI, color = RegionName)) +
  geom_line() +
  geom_smooth(method = lm) +
  xlab("Year") +
  ggtitle("Median home values (ZHVI) for Raleigh and Cary") +
  scale_y_continuous(labels = comma)

#I dont think I'm actually supposed to compare top vs median vs bottom tier?
#like are these really separate effects? (eg not correlated?)
##I dont really need the below since it takes all the disparate groups together... 
# mod_time <- glm(ZHVI ~ Year, data = all_raleigh_long)
# summary(mod_time)
# 
# mod_time <- glm(ZHVI ~ Ordinal, data = all_raleigh_long)
# summary(mod_time)
# 
# #for all this data, all predictors
# mod_city <- glm(ZHVI ~ OrdinalDay + RegionName, data = all_raleigh_long)
# summary(mod_city)
#woah
#SFR significant no effect?
#ignoring that the stats arent perfect:
#suggests that only Garner has a negative effect on house value as compared to Raleigh

#I'm guessing it is probably better to fit individual models for all of these 
#and compare the slopes separately, not together

#CARY
sfr_cary <- all_raleigh_long %>%
  filter(Type == "Single Family Residence" & RegionName == "Cary")
sfr_cary$Year <- sfr_cary$OrdinalDay / 365

#model over entire period, but note that post 2012 the curve is steeper
mod_sfr_cary <- glm(ZHVI ~ Year, data = sfr_cary)
summary(mod_sfr_cary)
#1 year translates to increase of $6300?
#10 years therefore = ~63000
#20 years ~+120000
ggplot(sfr_cary, aes(x=Year+1996, y=ZHVI)) +
  geom_line() +
  geom_smooth(method = lm) +
  xlab("Year") +
  ggtitle("Change in home value for SFR in Cary") +
  scale_y_continuous(labels = comma)


#RALEIGH
sfr_raleigh <- all_raleigh_long %>%
  filter(Type == "Single Family Residence" & RegionName == "Raleigh")
sfr_raleigh$Year <- sfr_raleigh$OrdinalDay / 365

mod_sfr_raleigh <- glm(ZHVI ~ Year, data = sfr_raleigh)
summary(mod_sfr_raleigh)
#1 year translates to increase of $4200?
#10 years therefore = ~42000
#20 years ~+84000
ggplot(sfr_raleigh, aes(x=Year+1996, y=ZHVI)) +
  geom_line() +
  geom_smooth(method = lm) +
  xlab("Year") +
  ggtitle("Change in home value for SFR in Raleigh") +
  scale_y_continuous(labels = comma)

#condos?
condo_raleigh <- all_raleigh_long %>%
  filter(Type == "Condos" & RegionName == "Raleigh")
condo_raleigh$Year <- condo_raleigh$OrdinalDay / 365
mod_condo_raleigh <- glm(ZHVI ~ Year, data = condo_raleigh)
summary(mod_condo_raleigh)
#condos gaining 2500 per year based on 20 year prediction
#10 years woudl be 25000, 20 years 50000

ggplot(condo_raleigh, aes(x=Year+1996, y=ZHVI)) +
  geom_line() +
  geom_smooth(method = lm) +
  xlab("Year") +
  ggtitle("Change in home value for condos in Raleigh") +
  scale_y_continuous(labels = comma)


###RALEIGH COMPARISION - comparing condo vs sfr appreciation
#should have done this one in the first place
comp_raleigh <- all_raleigh_long %>%
  filter(RegionName == "Raleigh" & Type == "Single Family Residence" | RegionName == "Raleigh" & Type == "Condos")
comp_raleigh$Year <- comp_raleigh$OrdinalDay / 365
#still unclear how kosher the below is
mod_comp_raleigh <- glm(ZHVI ~ Year + Type, data = comp_raleigh)
summary(mod_comp_raleigh)

ggplot(comp_raleigh, aes(x=Year+1996, y=ZHVI, color = Type)) +
  geom_line() +
  geom_smooth(method = lm) +
  xlab("Year") +
  ggtitle("Change in home value in Raleigh") +
  scale_y_continuous(labels = comma)


###CARY COMPARISION - comparing house value appreciation (bottom, median, top tier)
top_cary <- all_raleigh_long %>%
  filter(Type == "Top Tier Value" & RegionName == "Cary")
#fit model
mod_top_cary <- glm(ZHVI ~ Year, data = top_cary )
summary(mod_top_cary )

mid_cary <- all_raleigh_long %>%
  filter(Type == "All Homes" & RegionName == "Cary")
#rename this to fit with the others
mid_cary$Type <- "Mid Tier Value"
#fit model
mod_mid_cary <- glm(ZHVI ~ Year, data = mid_cary )
summary(mod_mid_cary )

bot_cary <- all_raleigh_long %>%
  filter(Type == "Bottom Tier Value" & RegionName == "Cary")
#fit model
mod_bot_cary <- glm(ZHVI ~ Year, data = bot_cary )
summary(mod_bot_cary )



comp_cary <- all_raleigh_long %>%
  filter(RegionName == "Cary" & Type == "All Homes" | RegionName == "Cary" & Type == "Top Tier Value" | RegionName == "Cary" & Type == "Bot Tier Value")
comp_cary$Year <- comp_cary$OrdinalDay / 365

#still unclear how kosher the below is
mod_comp_cary <- glm(ZHVI ~ Year + Type, data = comp_cary)
summary(mod_comp_cary)

ggplot(comp_cary, aes(x=Year+1996, y=ZHVI, color = Type)) +
  geom_line() +
  geom_smooth(method = lm) +
  xlab("Year") +
  ggtitle("Change in home value in Cary") +
  scale_y_continuous(labels = comma)

