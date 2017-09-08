#add libraries
library(plyr)
library(dplyr)
library(tidyr)
library(rpart)
library(ggplot2)
library(lubridate)

#set wd
setwd("C:/Users/mlisaius/Desktop/DigitalGreen_Wheat2017")

#data <- read.csv("bihar_classification_ee2.csv", header = TRUE)
df = read.csv('bihar_classification_ee2.csv', header=TRUE) %>% 
  mutate(date=as.Date.POSIXct(mstimestamp/1000, '1970-1-1', tz='UTC')) %>% 
  mutate(month=month(date),year=year(date)) %>% 
  mutate(count=as.numeric(B1_count))

bands= c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'NDVI', 'GCVI')
variables = c(paste(bands,'mean',sep='_'),paste(bands,'variance',sep='_'),paste(bands,'count',sep='_'), 'yield', 'yieldLevel100Break')

df.grouped.ID = df %>% group_by(ID)

df.grouped = df %>% 
  group_by(year,month,district) %>% 
  summarise_each_(funs(weighted.mean(.,count, na.rm=TRUE)), variables)

df.grouped.gat = df.grouped %>%
  gather(BAND, VALUE, B1_mean:NDVI_mean) %>%
  filter(BAND %in% paste(bands,'mean',sep='_'))

df.grouped.max1 = df %>% 
  group_by(ID) %>% 
  summarise_each_(funs(max(., na.rm=TRUE)), variables)

df.helper = data.frame(df$GCVI_mean, df$date, df$ID, df$month, df$district, df$noTimesIrrigated, df$noTimesWeeded, df$seedSource,
                       df$area, df$chemical, df$isFertilizedType, df$isIntercropping, df$intercroppingPercent)
colnames(df.helper) <- c("GCVI_mean", "date", "ID","month","district","noTimesIrrigated","noTimesWeeded","seedSource",
                         "area","chemical","isFertilizedType","isIntercropping","intercroppingPercent")

df.grouped.max = merge(df.grouped.max1,df.helper,by=c("ID", "GCVI_mean"))


df.grouped2 = df %>% 
  group_by(year,month,gpsfileid) %>% 
  summarise_each_(funs(weighted.mean(.,count, na.rm=TRUE)), variables)


#GCVI time series
gp = ggplot(data=df.grouped)+
  geom_line(aes(x=month, y=GCVI_mean,group=as.factor(district),colour=as.factor(district)))+
  ggtitle("Mean GCVI by Region by Date")
print(gp)
ggsave(filename = "GCVIvRegion.png", plot = gp, device='png',
       path = NULL, width = 5, height =3, units = 'in', 
       dpi = 600)


#GCVI v Yield combinations of interest: area, chemical (noChemicalFertApp), isFertilizedType,intercroppingPercent, isWeeded,
#noTimesIrrigated, noTimesWeeded, seedSource, sowdate(??)

#max GCVI v Yield ///date
gy1 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(date)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))
print(gy1)


#max GCVI v Yield ///district
gy2 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(district)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))
print(gy2)


#max GCVI v Yield ///noTimesIrrigated
gy3 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(noTimesIrrigated)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))
print(gy3)


#max GCVI v Yield ///noTimesWeeded
gy4 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(noTimesWeeded)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))
print(gy4)


#max GCVI v Yield ///seedSource
gy5 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(seedSource)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))
print(gy5)


#max GCVI v Yield ///chem fert apps
gy6 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(chemical)))+
  ggtitle('Max GCVI vs yield (Color: No. Chemical Fert Applications)') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))
print(gy6)


#max GCVI v Yield ///fert type
gy7 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(isFertilizedType)))+
  ggtitle('Max GCVI vs yield (Color: Fertilizer Type)') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))
print(gy7)


#max GCVI v Yield ///chem fert applications
gy8 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(intercroppingPercent)))+
  ggtitle('Max GCVI vs yield (Color: InterCroppingPercent)') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))
print(gy8)


######################################################### lol Maddy's trolling


#max GCVI v Yield ///date
gy1_2 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(date)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield))+
  facet_wrap(~ district, ncol = 2)
print(gy1_2)
ggsave(filename = "GCVIvYield_date_fw.png", plot = gy1_2, device='png',
       path = NULL, width = 7, height =7, units = 'in', 
       dpi = 600)


#max GCVI v Yield ///noTimesIrrigated
gy3_2 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(noTimesIrrigated)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield)) +
  facet_wrap(~ district, ncol = 2)
print(gy3_2)
ggsave(filename = "GCVIvYield_noTimesIrrigated_fw.png", plot = gy3_2, device='png',
       path = NULL, width = 7, height =7, units = 'in', 
       dpi = 600)


#max GCVI v Yield ///noTimesWeeded
gy4_2 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(noTimesWeeded)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield)) +
  facet_wrap(~ district, ncol = 2)
print(gy4_2)
ggsave(filename = "GCVIvYield_noTimesWeeded_fw.png", plot = gy4_2, device='png',
       path = NULL, width = 7, height =7, units = 'in', 
       dpi = 600)


#max GCVI v Yield ///seedSource
gy5_2 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(seedSource)))+
  ggtitle('Max GCVI vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield)) +
  facet_wrap(~ district, ncol = 2)
print(gy5_2)
ggsave(filename = "GCVIvYield_seedSource_fw.png", plot = gy5_2, device='png',
       path = NULL, width = 7, height =7, units = 'in', 
       dpi = 600)


#max GCVI v Yield ///chem fert apps
gy6_2 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(chemical)))+
  ggtitle('Max GCVI vs yield (Color: No. Chemical Fert Applications)') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield)) +
  facet_wrap(~ district, ncol = 2)
print(gy6_2)
ggsave(filename = "GCVIvYield_chemFertApps_fw.png", plot = gy6_2, device='png',
       path = NULL, width = 7, height =7, units = 'in', 
       dpi = 600)


#max GCVI v Yield ///fert type
gy7_2 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(isFertilizedType)))+
  ggtitle('Max GCVI vs yield (Color: Fertilizer Type)') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield)) +
  facet_wrap(~ district, ncol = 2)
print(gy7_2)
ggsave(filename = "GCVIvYield_fertType_fw.png", plot = gy7_2, device='png',
       path = NULL, width = 7, height =7, units = 'in', 
       dpi = 600)


#max GCVI v Yield ///chem fert applications
gy8_2 = ggplot(data=df.grouped.max)+
  geom_point(aes(x=GCVI_mean, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(intercroppingPercent)))+
  ggtitle('Max GCVI vs yield (Color: InterCroppingPercent)') +
  geom_smooth(method='lm',formula=y~x, aes(x=GCVI_mean, y=yield)) +
  facet_wrap(~ district, ncol = 2)
print(gy8_2)
ggsave(filename = "GCVIvYield_noChemFertApps_fw.png", plot = gy8_2, device='png',
       path = NULL, width = 7, height =7, units = 'in', 
       dpi = 600)


#############################################################

#fert v yield
yieldVfert <- lm(yield~a7, data=df.grouped.ID)
summary(yieldVfert)
gp4 = ggplot(data=df.grouped.ID)+
  geom_point(aes(x=a7, y=yield,size=GCVI_count,alpha=0.4))+
  ggtitle('Fertilizer Use vs yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=a7, y=yield))
print(gp4)

#GCVI time series
df.grouped$monthOrder = mapvalues(df.grouped$month, from = c("12","1","2","3","7"), to = c("0 (Dec)","1 (Jan)","2 (Feb)",
                                                                                              "3 (Mar)", "7 (July)"))
gp1 = ggplot(data=df.grouped)+
  geom_line(aes(x=monthOrder, y=GCVI_mean,group=as.factor(district),colour=as.factor(district)))+
  ggtitle('GCVI by Month by Region') 
print(gp1)

#fert v yield
df.grouped.ID$fertUsed = mapvalues(df.grouped.ID$a7, from = c("1","2","3","4"), to = c("Chemical","Organic","Both", "Neither"))
yieldVfert <- lm(yield~fertUsed, data=df.grouped.ID)
summary(yieldVfert)
gp4 = ggplot(data=df.grouped.ID)+
  geom_point(aes(x=fertUsed, y=yield,size=GCVI_count,alpha=0.4))+
  ggtitle('Fertilizer Use vs Yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=fertUsed, y=yield))
print(gp4)


#weeded v yield
df.grouped.ID$weeded = mapvalues(df.grouped.ID$a6, from = c("1","2"), to = c("yes","no"))
yieldVweeded <- lm(yield~weeded, data=df.grouped.ID)
summary(yieldVweeded)
gp5 = ggplot(data=df.grouped.ID)+
  geom_point(aes(x=weeded, y=yield,size=GCVI_count,alpha=0.4))+
  ggtitle('Is Weeded vs Yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=weeded, y=yield))
print(gp5)


#no weedings v yield
yieldVnoweedings <- lm(yield~noTimesWeeded, data=df.grouped.ID)
summary(yieldVnoweedings)
gp6 = ggplot(data=df.grouped.ID)+
  geom_point(aes(x=noTimesWeeded, y=yield,size=GCVI_count,alpha=0.4))+
  ggtitle('No. Times Weeded vs Yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=noTimesWeeded, y=yield))
print(gp6)


#seedSource v yield
yieldVseedSource <- lm(yield~seedSource, data=df.grouped.ID)
summary(yieldVseedSource)
gp7 = ggplot(data=df.grouped.ID)+
  geom_point(aes(x=seedSource, y=yield,size=GCVI_count,alpha=0.4,colour=as.factor(district)))+
  ggtitle('Seed Source vs Yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=seedSource, y=yield))
print(gp7)


#No Times Irrigated v yield
yieldVirr <- lm(yield~noTimesIrrigated, data=df.grouped.ID)
summary(yieldVirr)
gp8 = ggplot(data=df.grouped.ID)+
  geom_point(aes(x=noTimesIrrigated, y=yield,size=GCVI_count,alpha=0.4))+
  ggtitle('No Times Irrigated vs Yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=noTimesIrrigated, y=yield))
print(gp8)


#district v yield
yieldVdistrict <- lm(yield~district, data=df.grouped.ID)
summary(yieldVdistrict )
gp9 = ggplot(data=df.grouped.ID)+
  geom_point(aes(x=district, y=yield,size=GCVI_count,alpha=0.4))+
  ggtitle('District vs Yield') +
  geom_smooth(method='lm',formula=y~x, aes(x=district, y=yield))
print(gp9)


test <-lm(yield~district+noTimesIrrigated+seedSource, df.grouped.ID)
coefficients(test)
anova(test)


