########################################
# daily plot
rm(list = ls())
library(ggplot2)
#d=read.delim("cases_202004090001AM.txt",sep=",",header = TRUE)
d=read.delim("cases_20200410_0338PM.txt",sep=",",header = TRUE)
dim(d)
# number of states needed 56
sort(unique(d$state_name))
state_ls = sort(unique(d$state_name))
state_ls = state_ls[!state_ls %in% 
                      c("Wuhan Evacuee",
                        "Diamond Princess", 
                        "Grand Princess", 
                        "Military",
                        )]
length(state_ls)
# number of days needed
max(as.Date(d$confirmed_date))-min(as.Date(d$confirmed_date))
d_state = list()
date_ls = seq(min(as.Date(d$confirmed_date)), max(as.Date(d$confirmed_date)), by = "day")
date_unique_ls=sort(as.Date(unique(d$confirmed_date)))
names(d)
library(dplyr)
d_agg = d %>%
  group_by(confirmed_date, state_name) %>% 
  summarise(confirmed_count = sum(confirmed_count))
library(reshape)
d_daily_tab=cast(d_agg, confirmed_date ~ state_name)
d_daily_tab[is.na(d_daily_tab)]=0
d_total_tab=data.frame(confirmed_date = d_daily_tab[,1], apply(d_daily_tab, 2, cumsum))
tail(d_total_tab)
US_xy=data.frame(total_case = rowSums(d_total_tab[,-1]),
                daily_case = rowSums(d_daily_tab[,-1]))
plot(log10(US_xy$total_case),log10(US_xy$daily_case),type="l")
US_xy_reg=US_xy[(US_xy$total_case>1)&(US_xy$daily_case>1),]
abline(lm(log10(US_xy_reg$daily_case)~log10(US_xy_reg$total_case)))

OH_xy=d_daily_tab[,c("confirmed_date","OH")]
OH_xyz=cbind(OH_xy,cumsum(OH_xy$OH))
names(OH_xyz)[3]="total_case"
plot(OH_xy$confirmed_date,OH_xy$OH,col="blue")
plot(log10(OH_xyz$total_case),log10(OH_xyz$OH),type="l")

library(zoo)
par(mfrow=c(1,1))
for (date_idx in 10:length(date_unique_ls))
{
  jpeg(paste(date_unique_ls[date_idx],"rplot.jpg",sep=""), 
       width = 1600, height = 900)
  par(mfrow=c(1,1))
  par(mar=c(10,10,3,3),mgp=c(5,2,0))
  p = plot(as.Date(date_ls),rep(1,length(date_ls)),type = "n",
           xlim=c(as.Date("2020-03-01"),as.Date("2020-04-07")),
           ylim=c(0,4.5),
           cex.lab=3,cex.axis=3,
           xlab="Date",
           ylab="Daily New Case (average of last 3 days)",
           yaxt="n",xaxt="n")
  axis(side=2, at=c(0,1,2,3,4,5), 
       labels = c(1,10,100,1000,10000,100000),
       cex.lab=3,cex.axis=3)
  axis(side=1, at=seq(min(as.Date(date_ls)),
                        max(as.Date(date_ls)),length.out=15),
       labels = as.character(seq(min(as.Date(date_ls)),
                        max(as.Date(date_ls)),length.out=15)),
       cex.lab=3,cex.axis=3)
  for (state_idx in 1:length(state_ls))
  {
    # if (state_ls[state_idx] %in% c("MT","ME","HI",
    #                                #"ND","SD","GU","AK",
    #                                #"WV","VT","NH",
    #                                "DE",
    #                                #"KY","ID",
    #                                #"RI","UT","AZ","WI",
    #                                #"NC","VA","TN",
    #                                "OH",
    #                                #"TX","CT","PA","FL","IN","MI","LA"
    #                                "NY","NJ","CA","WA"))
    {
          xy=data.frame(
            d_total_tab[1:date_idx,as.character(state_ls[state_idx])],
            d_daily_tab[1:date_idx,as.character(state_ls[state_idx])],
            d_daily_tab[1:date_idx,1])
    y=rollapply(xy[,2], 4, mean)
    x=xy[-seq(1,3),1]
    z=xy[-seq(1,3),3]
    #y=xy[,2]
    #x=xy[,1]
    #z=xy[,3]
    points(as.Date(z),log10(y),type="l",col=rgb(0,0,1,0.1),lwd=3)
    points(as.Date(z),log10(y),col=rgb(0,0,1,0.1),pch=20)
    points(as.Date(z)[length(z)],log10(y)[length(z)],pch=20,cex=1.5)
    text(  as.Date(z)[length(z)]+1,
           log10(y)[length(z)], 
           labels = state_ls[state_idx],
           cex=1.5)

    # points(log10(x),log10(y),type="l",col=rgb(0,0,1,0.1),lwd=3)
    # points(log10(x),log10(y),col=rgb(0,0,1,0.1),pch=20)
    # points(log10(x)[length(x)],log10(y)[length(x)],pch=20,cex=1.5)
    # text(  log10(x)[length(x)]+0.05,
    #        log10(y)[length(x)]-0.05, 
    #        labels = state_ls[state_idx],
    #        cex=1.5)
   text( as.Date(date_ls[80]),200, labels = date_unique_ls[date_idx], cex=5,col=rgb(0.5,0,0,0.1))
    }
  }
  #abline(a=-0.1,b=1)
  #x <- c(0+0.5,0-0.1,8-0.1,8+0.5)
  #y <- c(0    ,0    ,8    ,8)
  #polygon(x,y,col=rgb(0.1,0,0,0.1),border=rgb(0.1,0,0,0))
  dev.off()
}



for (state_idx2 in 1:length(state_ls))
{
  jpeg(paste("state",state_ls[state_idx2],".jpg",sep=""), 
       width = 1600, height = 900)
  par(mfrow=c(1,1))
  par(mar=c(10,10,3,3),mgp=c(5,2,0))
  p = plot(as.Date(date_ls),rep(1,length(date_ls)),type = "n",
           xlim=c(as.Date("2020-03-01"),as.Date("2020-04-07")),
           ylim=c(0,4.5),
           cex.lab=3,cex.axis=3,
           xlab="Date",
           ylab="Daily New Case (average of last 3 days)",
           yaxt="n",xaxt="n")
  axis(side=2, at=c(0,1,2,3,4,5), 
       labels = c(1,10,100,1000,10000,100000),
       cex.lab=3,cex.axis=3)
  axis(side=1, at=seq(min(as.Date(date_ls)),
                        max(as.Date(date_ls)),length.out=15),
       labels = as.character(seq(min(as.Date(date_ls)),
                        max(as.Date(date_ls)),length.out=15)),
       cex.lab=3,cex.axis=3)
  for (state_idx in 1:length(state_ls))
  {
          xy=data.frame(
            d_total_tab[1:date_idx,as.character(state_ls[state_idx])],
            d_daily_tab[1:date_idx,as.character(state_ls[state_idx])],
            d_daily_tab[1:date_idx,1])
    y=rollapply(xy[,2], 4, mean)
    x=xy[-seq(1,3),1]
    z=xy[-seq(1,3),3]
    #y=xy[,2]
    #x=xy[,1]
    #z=xy[,3]
    points(as.Date(z),log10(y),type="l",col=rgb(0,0,1,0.1),lwd=3)
    points(as.Date(z),log10(y),col=rgb(0,0,1,0.1),pch=20)
    points(as.Date(z)[length(z)],log10(y)[length(z)],pch=20,
           col=rgb(0,0,1,0.1),cex=1.5)
    text(  as.Date(z)[length(z)]+1,
           log10(y)[length(z)], 
           labels = state_ls[state_idx],
           col=rgb(0,0,1,0.1),
           cex=1.5)

    # points(log10(x),log10(y),type="l",col=rgb(0,0,1,0.1),lwd=3)
    # points(log10(x),log10(y),col=rgb(0,0,1,0.1),pch=20)
    # points(log10(x)[length(x)],log10(y)[length(x)],pch=20,cex=1.5)
    # text(  log10(x)[length(x)]+0.05,
    #        log10(y)[length(x)]-0.05, 
    #        labels = state_ls[state_idx],
    #        cex=1.5)
   text( as.Date(date_ls[80]),200, labels = date_unique_ls[date_idx], cex=5,col=rgb(0.5,0,0,0.1))
  }

  xy=data.frame(
          d_total_tab[1:date_idx,as.character(state_ls[state_idx2])],
          d_daily_tab[1:date_idx,as.character(state_ls[state_idx2])],
          d_daily_tab[1:date_idx,1])
  y=rollapply(xy[,2], 4, mean)
  x=xy[-seq(1,3),1]
  z=xy[-seq(1,3),3]
  #y=xy[,2]
  #x=xy[,1]
  #z=xy[,3]
  points(as.Date(z),log10(y),type="l",col=rgb(0,0,1,1),lwd=3)
  points(as.Date(z),log10(y),col=rgb(0,0,1,1),pch=20)
  points(as.Date(z)[length(z)],log10(y)[length(z)],pch=20,cex=1.5)
  text(  as.Date(z)[length(z)]+1,
         log10(y)[length(z)], 
         labels = state_ls[state_idx2],
         cex=1.5)
  dev.off()

}















require(scales)
mb <- as.numeric(1:10 %o% 10 ^ (0:5))
p <- ggplot() + 
  scale_x_continuous(trans='log10',minor_breaks = mb,labels = comma) + 
  scale_y_log10(minor_breaks = mb,labels = comma) 
# + coord_cartesian(xlim = c(0, 100), ylim = c(10, 20))
d_label=data.frame(x=numeric(),y=numeric(),state=character(),stringsAsFactors=FALSE)
for (state_idx in 1:length(state_ls))
{
  y=rollapply(d_state[[state_idx]][,3], 4, sum)
  x=d_state[[state_idx]][-seq(1,3),2]
  d_ggplot=as.data.frame(cbind(x,y))
  d_label[state_idx,1:2]=d_ggplot[dim(d_ggplot)[1],]
  d_label[state_idx,3]=as.character(state_ls[state_idx])
  p = p + geom_line(data = d_ggplot, aes(x = x, y = y), color = "blue", alpha = 0.2)
  p = p + geom_point(data = d_ggplot, aes(x = x, y = y), color = "blue", alpha = 0.2,size = 0.3)
}
p = p + 
  geom_point(data = d_label, aes(x = x, y = y),color = "blue", alpha = 0.7,size = 1) +
  geom_text(data = d_label, aes(x = x, y = y,label = state),nudge_y = -0.05,angle = -45,size=3)
p <- p + xlab('Dates') + ylab('percent.change')
p



####################################################### 
# geofacet

library(data.table)
library(tidyverse)
#install.packages("geofacet")
library(geofacet)
dt <- fread("http://www.freddiemac.com/fmac-resources/research/docs/fmhpi_master_file.csv")

dt[,date:=as.Date(ISOdate(Year,Month,1))
   ][,hpa:=Index_SA/lag(Index_SA,12)-1,by=.(GEO_Type,GEO_Name,GEO_Code)]


# function to compute bivariate (state-to-state) correlation in 12-month house price appreciation rate

corf <- function(s1="CA",s2="AZ"){
  cor(dt[GEO_Name==s1&!is.na(hpa) & Year>1989,]$hpa,dt[GEO_Name==s2 &!is.na(hpa) & Year>1989,]$hpa)
}


# create correlation matrix

df <- 
  expand_grid(state1=unique(dt[GEO_Type=="State",]$GEO_Name),
              state2=unique(dt[GEO_Type=="State",]$GEO_Name)) %>%
  mutate(correlation=map2(state1,state2,corf)) %>%
  unnest(correlation)

df <-left_join(df,us_state_grid1,by=c("state1"="code"))
ggplot(data=filter(dt,GEO_Type=="State",Year>1989), 
       aes(x=date,y=hpa))+
  geom_line()+
  geom_area(fill="dodgerblue",alpha=0.25)+
  theme_minimal()+
  scale_y_continuous(labels=scales::percent)+
  scale_x_date(guide=guide_axis(check.overlap = TRUE))+
  facet_geo(~GEO_Name,grid = mygrid)+
  theme(plot.title=element_text(face="bold",size=rel(1.2)),
        plot.caption=element_text(hjust=0))+
  labs(x="",y="",
       subtitle="12-month percent change in house price index",
       title="State house price growth rate Jan 1990 - Jan 2020",
       caption="@lenkiefer Source: Freddie Mac House Price Index")

mygrid <- data.frame(
  row = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 8),
  col = c(13, 12, 11, 1, 11, 13, 4, 12, 10, 8, 5, 3, 4, 6, 2, 1, 10, 9, 6, 7, 5, 4, 8, 3, 2, 1, 11, 10, 9, 13, 3, 4, 7, 5, 6, 2, 8, 1, 10, 7, 2, 5, 8, 6, 3, 4, 9, 8, 5, 4, 1),
  code = c("ME", "NH", "VT", "AK", "CT", "MA", "ND", "RI", "NY", "MI", "MN", "MT", "SD", "WI", "ID", "WA", "NJ", "PA", "IL", "IN", "IA", "NE", "OH", "WY", "NV", "OR", "DE", "MD", "VA", "DC", "CO", "KS", "KY", "MO", "TN", "UT", "WV", "CA", "NC", "AL", "AZ", "AR", "GA", "MS", "NM", "OK", "SC", "FL", "LA", "TX", "HI"),
  name = c("Maine", "New Hampshire", "Vermont", "Alaska", "Connecticut", "Massachusetts", "North Dakota", "Rhode Island", "New York", "Michigan", "Minnesota", "Montana", "South Dakota", "Wisconsin", "Idaho", "Washington", "New Jersey", "Pennsylvania", "Illinois", "Indiana", "Iowa", "Nebraska", "Ohio", "Wyoming", "Nevada", "Oregon", "Delaware", "Maryland", "Virginia", "District of Columbia", "Colorado", "Kansas", "Kentucky", "Missouri", "Tennessee", "Utah", "West Virginia", "California", "North Carolina", "Alabama", "Arizona", "Arkansas", "Georgia", "Mississippi", "New Mexico", "Oklahoma", "South Carolina", "Florida", "Louisiana", "Texas", "Hawaii"),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid)
grid_preview(mygrid)




####################################
rm(list = ls())
library(ggplot2)
library(data.table)
library(tidyverse)
#install.packages("geofacet")
library(geofacet)
d=read.delim("cases_20200409_0249PM.txt",sep=",",header = TRUE,stringsAsFactors = FALSE)
dim(d)
# number of states needed 56
state_ls = sort(unique(d$state_name))
state_ls
state_ls = state_ls[!state_ls %in% 
                      c("Wuhan Evacuee",
                        "Diamond Princess", 
                        "Grand Princess", 
                        "Military",
                        "VI",
                        "PR",
                        "NN",
                        "MP",
                        "GU"
                        )]
length(state_ls)
# number of days needed
max(as.Date(d$confirmed_date))-min(as.Date(d$confirmed_date))
date_ls = seq(min(as.Date(d$confirmed_date)), max(as.Date(d$confirmed_date)), by = "day")
date_unique_ls=sort(as.Date(unique(d$confirmed_date)))

names(d)
library(dplyr)
d_agg = d %>%
  group_by(confirmed_date, state_name) %>% 
  summarise(confirmed_count = sum(confirmed_count))
d_agg_order=d_agg
d_agg_order$confirmed_date=as.Date(d_agg_order$confirmed_date)
d_agg_order$log_confirmed_count=log10(d_agg_order$confirmed_count)
d_agg_order$log_confirmed_count[is.infinite(d_agg_order$log_confirmed_count)]=-0.01
state_case_start_date=as.Date(rep(NA,length(state_ls)))
for (state_idx in 1:length(state_ls))
{
  state_case_start_date[state_idx]=
    as.Date(min(as.Date(
      d_agg_order$confirmed_date[d_agg_order$state_name==state_ls[state_idx]])))
  print(state_ls[state_idx])
  print(diff(sort(unique(d_agg_order$confirmed_date[d_agg_order$state_name==state_ls[state_idx]]))))
}
table(d_agg_order$state_name)

mygrid2 <- data.frame(
  row = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7),
  col = c(1, 11, 12, 13, 4, 11, 12, 13, 10, 8, 6, 5, 4, 3, 2, 1, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 11, 10, 8, 7, 6, 5, 4, 3, 2, 1, 9, 12, 10, 9, 8, 7, 6, 5, 4, 3, 2, 8, 5, 4, 1),
  code = c("AK", "VT", "NH", "ME", "ND", "CT", "RI", "MA", "NY", "MI", "WI", "MN", "SD", "MT", "ID", "WA", "NJ", "PA", "OH", "IN", "IL", "IA", "NE", "WY", "NV", "OR", "DE", "MD", "WV", "KY", "TN", "MO", "KS", "CO", "UT", "CA", "VA", "DC", "NC", "SC", "GA", "AL", "MS", "AR", "OK", "NM", "AZ", "FL", "LA", "TX", "HI"),
  name = c("Alaska", "Vermont", "New Hampshire", "Maine", "North Dakota", "Connecticut", "Rhode Island", "Massachusetts", "New York", "Michigan", "Wisconsin", "Minnesota", "South Dakota", "Montana", "Idaho", "Washington", "New Jersey", "Pennsylvania", "Ohio", "Indiana", "Illinois", "Iowa", "Nebraska", "Wyoming", "Nevada", "Oregon", "Delaware", "Maryland", "West Virginia", "Kentucky", "Tennessee", "Missouri", "Kansas", "Colorado", "Utah", "California", "Virginia", "District of Columbia", "North Carolina", "South Carolina", "Georgia", "Alabama", "Mississippi", "Arkansas", "Oklahoma", "New Mexico", "Arizona", "Florida", "Louisiana", "Texas", "Hawaii"),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid2)
ggplot(data=d_agg_order, 
       aes(x=confirmed_date,y=confirmed_count))+
  geom_line()+
  #geom_area(fill="dodgerblue",alpha=0.25)+
  facet_geo(~state_name,grid = mygrid2, scales = "free_y")+
  #theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.text.x = element_text(size = 15)) +
  scale_x_date(date_labels = "%m/%d",limit=c(as.Date("2020-03-01"),as.Date("2020-04-07")))+
  #scale_y_continuous(labels=scales::percent)+
  #scale_x_date(guide=guide_axis(check.overlap = TRUE))+
  
  #theme(plot.title=element_text(face="bold",size=rel(1.2)),
  #      plot.caption=element_text(hjust=0))+
  labs(x="",y="",
       subtitle="March 2020 - April 2020",
       title="State Confirmed COVID-19 Cases Growth",
       caption="By Apiu \n
       Date Source: https://coronavirus.1point3acres.com/")

state_ls
for (date_idx in 19:length(date_unique_ls)){
  d_agg_order_this=
    d_agg_order[d_agg_order$confirmed_date<=date_unique_ls[date_idx],]
  ggplot(data=d_agg_order_this, 
       aes(x=confirmed_date,y=log_confirmed_count))+
    geom_line()+
    geom_area(fill="dodgerblue",alpha=0.25)+
    facet_geo(~state_name,grid = mygrid2)+
    theme(plot.title = element_text(size=25),
          axis.text.x = element_text(angle=60, hjust=1),
          strip.text.x = element_text(size = 15)#,
          #panel.spacing = unit(0.1, "lines"),
          #axis.title.y = element_blank(size = 15),
          #axis.text.y = element_blank(),
          #axis.ticks.y = element_blank()
        ) +
    scale_x_date(date_labels = "%m/%d",
                 breaks = seq(min(as.Date(d_agg$confirmed_date)),
                              max(as.Date(d_agg$confirmed_date)),
                              length.out = 15),
                 labels=  as.character(seq(min(as.Date(d_agg$confirmed_date)),
                                           max(as.Date(d_agg$confirmed_date)),
                                           length.out = 15)))+
    # scale_y_continuous(labels=scales::comma,
    #                    guide=guide_axis(check.overlap = TRUE))+
    scale_y_continuous(breaks = c(-0.01,1,2,3,4),labels=c("0","10","100","1000","10000")) + 
    coord_cartesian(ylim = c(-0.01, 4),xlim=c(as.Date("2020-02-28"),as.Date("2020-04-07"))) +
    #scale_y_continuous(labels=scales::percent)+
    #scale_x_date(guide=guide_axis(check.overlap = TRUE))+
    #theme(plot.title=element_text(face="bold",size=rel(1.2)),
    #      plot.caption=element_text(hjust=0))+
    labs(x="Date",y="Daily New Cases Count",
         subtitle="",
         title=paste("Daily New Confirmed COVID-19 Cases Growth by U.S. State as of ",
                     date_unique_ls[date_idx],
                     "\nBy Apiu, data source: https://coronavirus.1point3acres.com/",sep=""))
  ggsave(paste("geo",date_unique_ls[date_idx],".jpg",sep=""), width = 16, height = 9, units = "in")
}
