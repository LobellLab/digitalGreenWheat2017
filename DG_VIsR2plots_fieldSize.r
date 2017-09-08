library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(reshape2)
library(GGally)
library(Hmisc)
library(lubridate)


bands= c('GCVI_mine', 'NDVI_mine', 'MTCI', 'NBR1', 'NBR2', 'NDTI', 'RDGCVI', 'RDNDVI') #'CRC', 'EVI', ,'GCVI_george','RDTCI','SNDVI','STI','TVI'
VIs = c(paste(bands,'mean',sep='_'))
VIs.var = c(paste(bands,'variance',sep='_'))

setwd("C:/Users/mlisaius/Desktop/DigitalGreen_Wheat2017/")
df = read.csv('DG_VIexport_20170719.csv', header=TRUE) %>% 
  mutate(date=as.Date.POSIXct(mstimestamp/1000, '1970-1-1', tz='UTC')) %>% 
  mutate(month_R=month(date),yearR=year(date)) %>% 
  mutate(count=as.numeric(B1_count))

hist = hist(df$area, breaks=10)
df.split = split(df, cut2(df$area, g=8))

df.grouped = df %>% 
  group_by(ID) %>% 
  summarise_each_(funs(max(., na.rm=TRUE)), VIs)

df.grouped.variance = df %>% 
  group_by(ID) %>% 
  summarise_each_(funs(max(., na.rm=TRUE)), VIs.var)

df.helper = data.frame(df$ID,df$GCVI_mine_mean,df$area,df$isIntercropping,df$yield)
colnames(df.helper) <- c("ID","GCVI_mine_mean","area","isIntercropping","yield")
df.grouped.max = merge(df.grouped,df.helper,by=c("ID","GCVI_mine_mean"))


####################################################################################################
# Correlation
macre<-seq(0, 2000, by=200)
n_ps<-n_all<-hold_r2_ps<-hold_r2_all<-c()
lst_macre<-list()

for(vv in 1:length(VIs)){
  for(kk in 1:length(macre)){
    tmp_all<-df.grouped.max %>% filter(area >= macre[kk])
    tmp_ps<-df.grouped.max %>% filter(area >= macre[kk]) %>% filter(isIntercropping != 'YES')
    
    mod_all<-lm(yield~tmp_all[,1+vv], tmp_all, na.action=na.exclude)
    mod_ps<-lm(yield~tmp_ps[,1+vv], tmp_ps, na.action=na.exclude)
  
    n_all[kk]<-sum(!is.na(predict(mod_all)))
    n_ps[kk]<-sum(!is.na(predict(mod_ps)))
    
    hold_r2_all[kk]<-max(summary(mod_all)$adj.r.squared, 0)
    hold_r2_ps[kk]<-max(summary(mod_ps)$adj.r.squared, 0)
  }
  lst_macre[[vv]]<-data.frame('VI'=VIs[vv], macre, n_all, hold_r2_all, n_ps, hold_r2_ps)
}

df_macre<-do.call('rbind', lst_macre)

p.VIs<-ggplot(data=df_macre)+
  geom_line(aes(x=macre, y=hold_r2_all, col="All fields"))+
  geom_line(aes(x=macre, y=hold_r2_ps, col="Purestand"))+
  geom_text(aes(x=macre, y=1.0, label=n_all), col=4)+
  geom_text(aes(x=macre, y=0.95, label=n_ps), col=2)+
  scale_color_manual(values=c("All fields"=4, "Purestand"=2), guide=guide_legend(title=""))+
  labs(x="Minimal field size m2", y="adjusted R2")+
  coord_cartesian(xlim=c(0,2000), ylim=c(0,1.0))+
  scale_x_continuous(breaks=c(0, 500, 1000, 1500))+
  facet_wrap(~VI, ncol=2)+
  theme_bw()+
  theme(plot.margin=unit(c(2, 1, 2, 1),"cm"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text=element_text(size=14),
        legend.justification=c(0,1),legend.position=c(0.02,0.95),
        legend.background=element_rect(fill="transparent", colour="transparent"),
        legend.key.size=unit(1.0, "cm"), legend.text=element_text(size=14))

png("R2-compare_VIs_2.png", width=10, height=20, units='in', res=300)
p.VIs
dev.off()