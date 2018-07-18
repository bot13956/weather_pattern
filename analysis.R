#IMPORT NECESSARY LIBRARY AND THE DATASET

library(tidyverse)
library(readr)
df<-read.csv("weather_data.csv")

#DATA PREPARATION AND ANALYSIS

#convert temperature from tenths of degree C to degree C
df$Data_Value = 0.1*df$Data_Value

#functions to split date

split_function<-function(x)unlist(strsplit(x,'-'))
year_function<-function(x)split_function(x)[1]
day_function<-function(x)paste(split_function(x)[2],split_function(x)[3],sep='-')

#create Day and Year columns

day<-sapply(as.vector(df$Date),day_function)
year<-sapply(as.vector(df$Date),year_function)
df<-df%>%mutate(Day=day,Year=year )

#filter leap year and select 10 year observation: 2005-2014

df_2005_to_2014<-df%>%filter((df$Day!='02-29')&(df$Year!='2015'))
df_2015<-df%>%filter((df$Day!='02-29')&(df$Year=='2015'))

#record min and max for each day of the year for the 2005-2014 period

record_max<-df_2005_to_2014%>%group_by(Day)%>%summarize(Max = max(Data_Value),Min=min(Data_Value))%>%.$Max
record_min<-df_2005_to_2014%>%group_by(Day)%>%summarize(Max = max(Data_Value),Min=min(Data_Value))%>%.$Min

#record min and max for each day of the year for 2015

record_2015_max<-df_2015%>%group_by(Day)%>%summarize(Max = max(Data_Value),Min=min(Data_Value))%>%.$Max
record_2015_min<-df_2015%>%group_by(Day)%>%summarize(Max = max(Data_Value),Min=min(Data_Value))%>%.$Min


#PREPARE DATA FOR VISUALIZATION

#data frame for the 2005-2014 temperatures

y<-c(seq(1,1,length=365),seq(2,2,length=365))
y<-replace(replace(y, seq(1,365),'max'),seq(366,730),'min')
values<-data.frame(day=c(seq(1,365), seq(1,365)),element=sort(y),Temp=c(record_max,record_min))
q<-values%>%mutate(element=factor(element))

#data frame for the 2015 temperatures

max_index<-which(record_2015_max>record_max)
min_index<-which(record_2015_min < record_min)
dat15_max<-data.frame(max_index=max_index, Tmax=record_2015_max[max_index])
dat15_min<-data.frame(min_ndex=min_index, Tmin=record_2015_min[min_index])

#GENERATE DATA VISUALIZATION

q%>%ggplot(aes(day,Temp,color=element))+geom_line(size=1,show.legend = TRUE)+
  geom_point(data=dat15_max,aes(max_index,Tmax,color='max_index'),size=2,show.legend = TRUE)+
  geom_point(data=dat15_min,aes(min_index,Tmin,color='min_index'),size=2,show.legend = TRUE)+
  scale_colour_manual(labels=c("record high","2015 break high","record low","2015 break low"),values=c('red','purple','blue','green'))+
  xlab('Month')+ ylab('Temperature (C)')+
  scale_x_continuous(breaks = as.integer(seq(1,335,length=12)), labels = c('Jan','Feb', 'Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
 ggtitle("Record temperatures between 2005-2014")+
  theme(
    plot.title = element_text(color="black", size=12, hjust=0.5, face="bold"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold"),
    legend.title = element_blank()
  )
