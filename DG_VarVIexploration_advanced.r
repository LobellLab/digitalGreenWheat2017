library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(reshape2)
library(GGally)
library(Hmisc)
library(lubridate)

#define dataset
setwd("C:/Users/mlisaius/Desktop/DigitalGreen_Wheat2017/")
df = read.csv('DG_VIexport_20170719.csv', header=TRUE) %>% 
  mutate(date=as.Date.POSIXct(mstimestamp/1000, '1970-1-1', tz='UTC')) %>% 
  mutate(month_R=month(date),yearR=year(date)) %>% 
  mutate(count=as.numeric(B1_count))


bands= c('CRC', 'EVI', 'GCVI_mine','GCVI_george', 'NDVI_mine','NDVI_george', 'MTCI', 'NBR1', 'NBR2', 'NDTI', 'RDGCVI', 'RDNDVI',
         'RDTCI','SNDVI','STI','TVI')
VIs = c(paste(bands,'mean',sep='_'))
VIs.var = c(paste(bands,'variance',sep='_'))
GCVI = c("GCVI_mine_mean")

df.grouped.ID = df %>%
  group_by(ID) 

#group by ID and summarise by max GCVI for field (ID) over season
df.grouped.GCVI = df %>% 
  group_by(ID) %>% 
  summarise_each_(funs(max(., na.rm=TRUE)), GCVI)

#group by ID, summarise for max variance accross season for multiple VIs
df.grouped.variance = df %>% 
  group_by(ID) %>% 
  summarise_each_(funs(max(., na.rm=TRUE)), VIs.var)


#use helper to combine a grouped/summarised dataset with other variables
df.helper = data.frame(df$ID,df$GCVI_mine_mean,df$isIntercropping,df$area,df$yield,df$noTimesIrrigated,df$noTimesWeeded, 
                       df$chemical, df$swi)
colnames(df.helper) <- c("ID","GCVI_mine_mean","isIntercropping","area","yield","noTimesIrrigated","noTimesWeeded",
                         "chemical", "swi")
df.grouped.max = merge(df.grouped.GCVI,df.helper,by=c("ID","GCVI_mine_mean"))


#use helper to join GCVI max across season for each field to management variables for each field
df.helper.all = data.frame(df$ID,df$GCVI_mine_mean,df$count,df$intercroppingCrop,df$intercroppingPercent,df$isFertilizedType,
                           df$isIntercropping,df$isWeeded,df$lodging,df$noTimesIrrigated,df$noTimesWeeded,df$seedSource,
                           df$seedVariety,df$weedCoverPercent,df$area,df$yield,df$district,df$a2,df$a5_2, df$swi)
colnames(df.helper.all) <- c("ID","GCVI_mine_mean","count","intercroppingCrop","intercroppingPercent","isFertilizedType",
                             "isIntercropping","isWeeded","lodging","noTimesIrrigated","noTimesWeeded","seedSource",
                             "seedVariety","weedCoverPercent","area","yield", "district","dateSowing","a5_2","swi")
df.grouped.max.all = merge(df.grouped.GCVI,df.helper.all,by=c("ID","GCVI_mine_mean"))


#simple lm to explore initial relationships
vartest = lm(yield~intercroppingCrop+intercroppingPercent+isFertilizedType+isIntercropping+isWeeded+
               noTimesIrrigated+noTimesWeeded+seedSource+seedVariety+weedCoverPercent, data=df.grouped.max.all)
test = summary(vartest)




####################################################################################################################
################################## fertilizer management var testing ###############################################
####################################################################################################################

#use helper to join GCVI max across season for each field to all fertilizer related vars
df.helper.fert = data.frame(df$ID,df$GCVI_mine_mean,df$isFertilizedType,df$a7_1, df$chemical,df$a7_1_1_r1,df$a7_1_1_r2,
                            df$a7_1_1_r3,df$a7_1_1_r4,df$a7_2,df$a7_2_1_r1,df$a7_2_1_r2,df$a7_2_1_r3,df$a7_2_1_r4,
                            df$isIntercropping, df$yield, df$district,df$count,df$a2,df$a5_2)
colnames(df.helper.fert) <- c("ID","GCVI_mine_mean","isFertilizedType","chemFertApps","chemical","chem_fert_R1","chem_fert_R2",
                              "chem_fert_R3","chem_fert_R4","manure","man_fert_R1","man_fert_R2","man_fert_R3","man_fert_R4",
                              "isIntercropping","yield", "district","count","dateSowing","a5_2")
df.grouped.max.fert = merge(df.grouped.GCVI,df.helper.fert,by=c("ID","GCVI_mine_mean"))

#explore how fertilizer use connects to yield distribution
#min low-hinge med up-hinge max
fivenum(data.frame(filter(df.grouped.max.fert,isFertilizedType =="YES Both"))$yield)
fivenum(data.frame(filter(df.grouped.max.fert,isFertilizedType =="YES Chemical Fertilizer"))$yield)
fivenum(data.frame(filter(df.grouped.max.fert,isFertilizedType =="YES Manure"))$yield)



############################ for fields only fertilized with chemical fert
#filter fert dataset for chemical fert
df.grouped.max.fert.chem = df.grouped.max.fert %>% filter(isFertilizedType == "YES Chemical Fertilizer")
#filter fert dataset for chemical fert and purestand fields only
df.grouped.max.fert.chem.pure = df.grouped.max.fert.chem %>% filter (isIntercropping == "NO")

#explore potential relationships betwen series of chemical fert applications and yield
yieldVchemical = lm(yield~chem_fert_R1+chem_fert_R2+chem_fert_R3+chem_fert_R4, df.grouped.max.fert.chem)
summary(yieldVchemical)
yieldVchemical.pure = lm(yield~chem_fert_R1+chem_fert_R2+chem_fert_R3+chem_fert_R4, df.grouped.max.fert.chem.pure)
summary(yieldVchemical.pure)

#yield ~ chemical fert, purestand only, scatterplot
yieldVchemical.pure.pplot = ggplot(data=df.grouped.max.fert.chem.pure)+
  geom_point(aes(x=yield, y=chem_fert_R1))+
  ggtitle('Yield vs first chem app - purestands only') 
print(yieldVchemical.pure.pplot )



############################ for fields only fertilized with manure - note low count of fields
#filter fert dataset for manure
df.grouped.max.fert.man = df.grouped.max.fert %>% filter(isFertilizedType == "YES Manure")
#filter fert dataset for manure and purestand fields only
df.grouped.max.fert.man.pure = df.grouped.max.fert.man %>% filter (isIntercropping == "NO")

#explore potential relationships betwen series of manure applications and yield
yieldVmanure = lm(yield~man_fert_R1+man_fert_R2+man_fert_R3+man_fert_R4, df.grouped.max.fert.man)
summary(yieldVmanure)
yieldVmanure.pure = lm(yield~man_fert_R1+man_fert_R2+man_fert_R3+man_fert_R4, df.grouped.max.fert.chem.pure)
summary(yieldVmanure.pure)



############################ for fields only fertilized with both manure and chemical fert
#filter fert dataset for plots with both chem fert and manure
df.grouped.max.fert.both = df.grouped.max.fert 
#mutate new both fert dataset to include chemical fert percentages (i.e. percent of total fert over growing
#season applied in each round of fert application, plus cumulative percent applied with each round)
df.grouped.max.fert.both.fertperc = df.grouped.max.fert.both %>% mutate(totalfert=chem_fert_R1+chem_fert_R2+
                              chem_fert_R3+chem_fert_R4+man_fert_R1+man_fert_R2+man_fert_R3+man_fert_R4) %>%
                              mutate(manure=man_fert_R1+man_fert_R2+man_fert_R3+man_fert_R4) %>%
                              mutate(chem_fert_R1_perc=chem_fert_R1/chemical) %>%
                              mutate(chem_fert_R2_perc=chem_fert_R2/chemical) %>%
                              mutate(chem_fert_R3_perc=chem_fert_R3/chemical) %>%
                              mutate(chem_fert_R4_perc=chem_fert_R4/chemical) %>%
                              mutate(chem_fert_R2_cumperc=chem_fert_R1_perc+chem_fert_R2_perc) %>%
                              mutate(chem_fert_R3_cumperc=chem_fert_R2_cumperc+chem_fert_R3_perc) %>%
                              mutate(chem_fert_R4_cumperc=chem_fert_R3_cumperc+chem_fert_R4_perc) %>%
                              filter(count>3)
#filter for plots with both chem fert and maure for purestand fields only
df.grouped.max.fert.both.pure = df.grouped.max.fert.both.fertperc %>% filter (isIntercropping == "NO")

#explore potential relationships between yield and fertilizer applications
yieldVfertboth = lm(yield~chem_fert_R1+chem_fert_R2+chem_fert_R3+chem_fert_R4+
                      man_fert_R1+man_fert_R2+man_fert_R3+man_fert_R4, df.grouped.max.fert.both)
summary(yieldVfertboth)
yieldVfertboth.pure = lm(chem_fert_R1+chem_fert_R2+chem_fert_R3+chem_fert_R4+
                       yield~man_fert_R1+man_fert_R2+man_fert_R3+man_fert_R4, df.grouped.max.fert.both.pure)
summary(yieldVfertboth.pure)



#yield~total chem fert R1
yieldVchemical.R1.plot = ggplot(data=df.grouped.max.fert.both.fertperc)+
  geom_point(aes(y=yield, x=chem_fert_R1, colour=as.factor(count), size=chem_fert_R1_perc))+
  xlim(0,6)+
  ggtitle('Yield vs Chemical fert R1 kg/kattha - all fields')+
  geom_smooth(method='lm',formula=y~x, aes(x=chem_fert_R1, y=yield))+
  facet_wrap(~ district, ncol = 2)
print(yieldVchemical.R1.plot)

#yield~total chem fert R1 perc
yieldVchemical.R1perc.plot = ggplot(data=df.grouped.max.fert.both.fertperc)+
  geom_point(aes(y=yield, x=chem_fert_R1_perc, colour=as.factor(district), size=chemical))+
  xlim(0.25,1)+
  ggtitle('Yield vs Chemical R1 perc - all fields')+
  geom_smooth(method='lm',formula=y~x, aes(x=chem_fert_R1_perc, y=yield))+
  facet_wrap(~ district, ncol = 2)
print(yieldVchemical.R1perc.plot)

#yield~total chem fert R2
yieldVchemical.R2.plot = ggplot(data=df.grouped.max.fert.both.fertperc)+
  geom_point(aes(y=yield, x=chem_fert_R2, colour=as.factor(district), size=chemical))+
  xlim(0,4)+
  ggtitle('Yield vs Chemical fert R2 kg/kattha - all fields')+
  geom_smooth(method='lm',formula=y~x, aes(x=chem_fert_R2, y=yield))+
  facet_wrap(~ district, ncol = 2)
print(yieldVchemical.R2.plot)

#yield~total weight fert
yieldVtotalFert.plot = ggplot(data=df.grouped.max.fert.both.fertperc)+
  geom_point(aes(y=yield, x=totalfert, colour=as.factor(district), size=count))+
  xlim(0,35)+
  ggtitle('Yield vs TOTAL fert kg/kattha - all fields')+
  geom_smooth(method='lm',formula=y~x, aes(x=totalfert, y=yield))+
  facet_wrap(~ district, ncol = 2)
print(yieldVtotalFert.plot)

#yield~total weight chemical fert
yieldVtotalChem.plot = ggplot(data=df.grouped.max.fert.both.fertperc)+
  geom_point(aes(y=yield, x=chemical, colour=as.factor(chemFertApps), size=count))+
  xlim(0,12)+
  ylim(0,500)+
  ggtitle('Yield vs TOTAL Chemical fert kg/kattha - all fields')+
  geom_smooth(method='lm',formula=y~x, aes(x=chemical, y=yield))+
  facet_wrap(~ district, ncol = 2)
print(yieldVtotalChem.plot)

#yield~total weight manure
yieldVtotalMan.plot = ggplot(data=df.grouped.max.fert.both.fertperc)+
  geom_point(aes(y=yield, x=manure, colour=as.factor(district), size=count))+
  xlim(0,26)+
  ggtitle('Yield vs manure kg/kattha - all fields')+
  geom_smooth(method='lm',formula=y~x, aes(x=manure, y=yield))+
  facet_wrap(~ district, ncol = 2)
print(yieldVtotalMan.plot)

#yield~fertType
yieldVfertType.barplot = ggplot(data=df.grouped.max.fert.both.fertperc)+
  geom_bar(aes(x=yield, group=as.factor(isFertilizedType), colour=as.factor(isFertilizedType),fill=as.factor(isFertilizedType)),binwidth=100,alpha=0.6, position = "identity")+
  ggtitle('Yield Grouped by Fert Type')#+
#facet_wrap(~ district, ncol = 2)
print(yieldVfertType.barplot)

#yield~chemfertapps
yieldVchemApps.barplot = ggplot(data=df.grouped.max.fert.both.fertperc)+
  geom_bar(aes(x=yield, group=as.factor(chemFertApps), colour=as.factor(chemFertApps),fill=as.factor(chemFertApps)),binwidth=100,alpha=0.6, position = "identity")+
  ggtitle('Yield Grouped by No.Chem Fert Applications')#+
#facet_wrap(~ district, ncol = 2)
print(yieldVchemApps.barplot)

summary(lm(yield~isFertilizedType,df.grouped.max.all))




####################################################################################################################
################################## other management var testing ####################################################
####################################################################################################################

#yield~notimesirr
yieldVnoTimesIrr.plot = ggplot(data=df.grouped.max.all)+
  geom_point(aes(y=yield, x=noTimesIrrigated, colour=as.factor(isFertilizedType), size=count, alpha=0.5))+
  xlim(0,6)+
  ggtitle('Yield vs No. Times Irrigated - all fields')+
  geom_smooth(method='lm',formula=y~x, aes(x=noTimesIrrigated, y=yield))+
  facet_wrap(~ district, ncol = 2)
print(yieldVnoTimesIrr.plot)

#yield~seedVariety
yieldVseedVariety.plot = ggplot(data=df.grouped.max.all)+
  geom_bar(aes(x=yield, group=as.factor(seedVariety), colour=as.factor(seedVariety),fill=as.factor(seedVariety)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('Yield Grouped by Seed Variety')+
  facet_wrap(~ district, ncol = 2)
print(yieldVseedVariety.plot)

#yield~did you weed
yieldVisWeeded.plot = ggplot(data=df.grouped.max.all)+
  geom_bar(aes(x=yield, group=as.factor(isWeeded), colour=as.factor(isWeeded),fill=isWeeded),binwidth=100,alpha=0.3,position = "identity")+
  ggtitle('Yield Grouped by if Weeded')#+
  #facet_wrap(~ district, ncol = 2)
print(yieldVisWeeded.plot)

#min low-hinge med up-hinge max
fivenum(data.frame(filter(df.grouped.max.all,isWeeded =="YES"))$yield)
fivenum(data.frame(filter(df.grouped.max.all,isWeeded =="NO"))$yield)

#yield~weed perc
yieldVweedPerc.plot = ggplot(data=df.grouped.max.all)+
  geom_freqpoly(aes(yield, ..density.., colour=as.factor(weedCoverPercent)), binwidth = 200)+
  #geom_bar(aes(x=yield, group=as.factor(weedCoverPercent), colour=as.factor(weedCoverPercent),fill=weedCoverPercent),binwidth=100,position=position_dodge())+
  ggtitle('Yield Grouped by Weed Cover Percent')#+
  #facet_wrap(~ district, ncol = 2)
print(yieldVweedPerc.plot)

#GCVI~weed perc
yieldVweedPerc.plot = ggplot(data=df.grouped.max.all)+
  geom_bar(aes(x=GCVI_mine_mean, group=as.factor(weedCoverPercent), colour=as.factor(weedCoverPercent),fill=weedCoverPercent),binwidth=100,position=position_dodge())+
  ggtitle('GCVI Grouped by Weed Cover Percent')#+
#facet_wrap(~ district, ncol = 2)
print(GCVIVweedPerc.plot)

#seedSource
#yield~seedSource
yieldVseedSource.plot = ggplot(data=df.grouped.max.all)+
  geom_bar(aes(x=yield, group=as.factor(seedSource), colour=as.factor(seedSource),fill=seedSource),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('Yield Grouped by Seed Source')#+
  #facet_wrap(~ district, ncol = 2)
print(yieldVseedSource.plot)

#irr
#yield~noTimesIrrigated
yieldVnoTimesIrr.plot.bar = ggplot(data=df.grouped.max.all)+
  geom_bar(aes(x=yield, group=as.factor(noTimesIrrigated), colour=as.factor(noTimesIrrigated),fill=as.factor(noTimesIrrigated)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('Yield Grouped by No.Times Irrigated')#+
  #facet_wrap(~ district, ncol = 2)
print(yieldVnoTimesIrr.plot.bar )


#sowingdate
df.grouped.max.all.sowing =df.grouped.max.all
var.dateSowing = df.grouped.max.all.sowing$dateSowing
df.grouped.max.all.sowing$yearSowed=substr(as.character(var.dateSowing),nchar(as.character(var.dateSowing))-3,nchar(as.character(var.dateSowing)))
df.grouped.max.all.sowing$monthSowed=substr(as.character(var.dateSowing),nchar(as.character(var.dateSowing))-5,nchar(as.character(var.dateSowing))-4)
df.grouped.max.all.sowing$daySowed=substr(as.character(var.dateSowing),1,nchar(as.character(var.dateSowing))-6)

#yield~sowingdate
yieldVsowingDate.plot = ggplot(data=df.grouped.max.all.sowing)+
  geom_bar(aes(x=yield, group=as.factor(monthSowed), colour=as.factor(monthSowed),fill=as.factor(monthSowed)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('Yield Grouped by MonthSown')+
  facet_wrap(~ district, ncol = 2)
print(yieldVsowingDate.plot)

#yield~sowingdate WITH VI
GCVIVsowingDate.plot = ggplot(data=df.grouped.max.all.sowing)+
  geom_bar(aes(x=GCVI_mine_mean, group=as.factor(monthSowed), colour=as.factor(monthSowed),fill=as.factor(monthSowed)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('GCVI Grouped by MonthSown')+
  facet_wrap(~ district, ncol = 2)
print(GCVIVsowingDate.plot)

#yield~intercropping WITH VI
GCVIVintercropping.plot = ggplot(data=df.grouped.max.all)+
  geom_freqpoly(aes(GCVI_mine_mean, ..density.., colour=as.factor(isIntercropping)), binwidth = 200)+
  #geom_bar(aes(x=GCVI_mine_mean, group=as.factor(isIntercropping), colour=as.factor(isIntercropping),fill=as.factor(isIntercropping)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('GCVI max Grouped by if Intercropping')#+
#facet_wrap(~ district, ncol = 2)
print(GCVIVintercropping.plot)

#yield~swi
yieldVSWI.plot = ggplot(data=df.grouped.max.all)+
  geom_freqpoly(aes(yield, ..density.., colour=as.factor(swi)), binwidth = 100, na.rm=TRUE)+
  #geom_bar(aes(x=yield, group=as.factor(swi), colour=as.factor(swi),fill=as.factor(swi)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('Yield Grouped by SWI Adoptation')#+
#facet_wrap(~ district, ncol = 2)
print(yieldVSWI.plot)
summary(lm(yield~as.factor(swi),df.grouped.max.all))

#GCVI~swi
GCVIVSWI.plot = ggplot(data=df.grouped.max.all)+
  geom_freqpoly(aes(GCVI_mine_mean, ..density.., colour=as.factor(swi)), binwidth = 200, na.rm=TRUE)+
  #geom_bar(aes(x=yield, group=as.factor(swi), colour=as.factor(swi),fill=as.factor(swi)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('GCVI Grouped by SWI Adoptation')#+
#facet_wrap(~ district, ncol = 2)
print(GCVIVSWI.plot)

#GCVI~lodging
GCVIVlodging.plot = ggplot(data=df.grouped.max.all)+
  geom_freqpoly(aes(GCVI_mine_mean, ..density.., colour=as.factor(lodging)), binwidth = 200, na.rm=TRUE)+
  ggtitle('GCVI Grouped by lodging')#+
#facet_wrap(~ district, ncol = 2)
print(GCVIVlodging.plot)

#yield~intercropping crop
yieldVintercroppingCrop.plot = ggplot(data=df.grouped.max.all)+
  geom_bar(aes(x=yield, group=as.factor(intercroppingCrop), colour=as.factor(intercroppingCrop),fill=as.factor(intercroppingCrop)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('Yield Grouped by Intercropping Crop')#+
#facet_wrap(~ district, ncol = 2)
print(yieldVintercroppingCrop.plot)

yieldVintercroppingPerc.plot = ggplot(data=df.grouped.max.all)+
  geom_bar(aes(x=yield, group=as.factor(intercroppingPercent), colour=as.factor(intercroppingPercent),fill=as.factor(intercroppingPercent)),binwidth=100,alpha=0.3, position = "identity")+
  ggtitle('Yield Grouped by Intercropping Percent')#+
#facet_wrap(~ district, ncol = 2)
print(yieldVintercroppingPerc.plot)


df.grouped.max.swi1 = filter(df.grouped.max.all, df.grouped.max.all$swi == 1)
df.grouped.max.swi0 = filter(df.grouped.max.all, df.grouped.max.all$swi == 0)

summary(df.grouped.max.swi1$yield)
summary(df.grouped.max.swi0$yield)

