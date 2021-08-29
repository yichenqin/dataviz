# coronavirus display
install.packages("usmap")
library(usmap)
library(ggplot2)


d=read.delim("cases.txt",sep=",",header = TRUE)
dim(d)
names(d)
length(unique(d$county_name))
length(unique(paste(d$state_name,d$county_name)))
head(d)
d$county_name=paste(d$county_name,"County")
write.csv("d","cases_kelly.csv")



aggregate(d$confirmed_count, by=list(Category=d$state_name), FUN=sum)

d0401=d[d$confirmed_date=="2020-04-01",]
d0401$state_county=paste(d0401$state_name,d0401$county_name)


d0401_input=aggregate(d0401$confirmed_count, by=list(state_county=d0401$state_county), FUN=sum)
names(d0401_input)[2]="dailycase"
head(d0401_input)


e=read.delim("County_and_State_update.csv",sep=",",
             header = TRUE,
             colClasses=c('character','character','character'))
names(e)=c("fips","county","state")
for (i in 1:nrow(e)){
  if(nchar(e$fips[i])<5){
    e$fips[i]<- paste0('0',e$fips[i],sep="")
  }
}
e$state_county=paste(e$state,e$county)
e

f=merge(d0401_input,e,by="state_county")
f=f[,c("fips","dailycase")]
head(f)
as.tibble(f)

head(countydaily)
g=merge(countydaily,f,by="fips",all=TRUE)
head(g)
#g[is.na(g)]=0
g$logdailycase=log10(g$dailycase)

plot_usmap(data = g, values = "logdailycase",color="white") +
  scale_fill_continuous(name = "Daily Cases",
                        label = scales::comma,
                        limits=c(0,3),
                        high = "#132B43", low = "#56B1F7") +
  theme(legend.position = "right")



plot_usmap(regions = "counties") +
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") +
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))


plot_usmap(include = "NY",
           data = countypop, values = "pop_2015", color = "red") +
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) +
  theme(legend.position = "right")

countydaily=countypop
names(countydaily)
countydaily$dailycase=sample(1:10,dim(countydaily)[1],replace = TRUE)

plot_usmap(data = countydaily, values = "dailycase", color = "red") +
  scale_fill_continuous(name = "Daily Cases", label = scales::comma) +
  theme(legend.position = "right")


########################################3
# daily plot
rm(list = ls())
library(usmap)
library(ggplot2)
d=read.delim("cases.txt",sep=",",header = TRUE)
d$county_name=paste(d$county_name,"County")
d$state_county=as.character(paste(d$state_name,d$county_name))
extra_state=read.csv("extra_counties.csv",header = TRUE)
extra_state[1:50,]
for (replace_idx in 1:dim(extra_state)[1])
{
  if (extra_state[replace_idx,3] !="" )
  {
    print(extra_state[replace_idx,3])
    d$state_county[d$state_county==extra_state[replace_idx,2]] =
      as.character(extra_state[replace_idx,3])
  }
}


date_ls=seq(as.Date("2020-01-01"), as.Date("2020-04-03"), by = "day")
date_idx=1
for (date_idx in 1:length(date_ls))
{
  d_this=d[as.Date(d$confirmed_date)<=date_ls[date_idx],]
  daily_new=sum(d_this$confirmed_count[as.Date(d_this$confirmed_date)==date_ls[date_idx]])
  total_case=sum(d_this$confirmed_count)

  d_this=d[as.Date(d$confirmed_date)==date_ls[date_idx],]
  print(date_ls[date_idx])
  print(dim(d_this))
  if (dim(d_this)[1]==0)
  {
    next
  }
  d_this=aggregate(d_this$confirmed_count,
                   by=list(state_county=d_this$state_county),
                   FUN=sum)
  head(d_this)
  names(d_this)[2]="dailycase"
  f=countypop
  f$state_county=paste(f$abbr,f$county)
  head(f)
  g=merge(f,d_this,by="state_county",all=TRUE)
  head(g)
  g$dailycase[g$dailycase>999]=999
  #g[is.na(g)]=0
  g$logdailycase=log10(g$dailycase)

  p <- plot_usmap(#include=c("VA"),
             data = g, values = "logdailycase",color="white",size=0.1) +
    scale_fill_continuous(name = "Daily Cases",
                          #label = scales::comma,
                          limits=c(0,3.1),
                          breaks=c(0,1,2,3),
                          labels=c("1","10","100","1000+"),
                          high = "#132B43", low = "#56B1F7") +
    theme(legend.position = "right")
  p <- p + labs(title=paste("Daily New COVID-19 Cases by County on", date_ls[date_idx],
                            "(by Apiu, data source at https://coronavirus.1point3acres.com/) \n
    Daily Total New Cases: ",format(daily_new,big.mark=","),
                            "\n    All Cases:",format(total_case,big.mark=",")))
  p
  ggsave(paste("dailycase_", date_idx, ".jpeg", sep=""), width = 30, height = 20, units = "cm")

}


#############################
# cumulative daily case USA

date_ls=seq(as.Date("2020-01-01"), as.Date("2020-04-03"), by = "day")
date_idx=94
for (date_idx in 1:length(date_ls))
{
  d_this=d[as.Date(d$confirmed_date)<=date_ls[date_idx],]
  daily_new=sum(d_this$confirmed_count[as.Date(d_this$confirmed_date)==date_ls[date_idx]])
  total_case=sum(d_this$confirmed_count)
  print(date_ls[date_idx])
  print(dim(d_this))
  if (dim(d_this)[1]==0)
  {
    next
  }
  d_this=aggregate(d_this$confirmed_count,
                   by=list(state_county=d_this$state_county),
                   FUN=sum)
  head(d_this)
  names(d_this)[2]="dailycase"
  f=countypop
  f$state_county=paste(f$abbr,f$county)
  head(f)
  g=merge(f,d_this,by="state_county",all=TRUE)
  head(g)
  g$dailycase[g$dailycase>=10000]=10000
  #g[is.na(g)]=0
  g$logdailycase=log10(g$dailycase)

  p <- plot_usmap(#include=c("OH"),
    data = g, values = "logdailycase",color="white",size=0.1) +
    scale_fill_continuous(name = " ",
                          #label = scales::comma,
                          limits=c(0,4.1),
                          breaks=c(0,1,2,3,4),
                          labels=c("1","10","100","1000","10000+"),
                          high = "#132B43", low = "#56B1F7") +
    theme(legend.position = "right")
  p <- p + labs(title=paste("Total COVID-19 Cases by County as of", date_ls[date_idx],
    "(figure by Apiu, data source at https://coronavirus.1point3acres.com/en) \n
    Daily New Cases: ",format(daily_new,big.mark=","),
    "\n    Total Cases:",format(total_case,big.mark=",")))
  p
  ggsave(paste("OH_cumcase_", date_idx, ".jpeg", sep=""), width = 30, height = 20, units = "cm")

}

#############################
# cumulative daily case Ohio

date_ls=seq(as.Date("2020-01-01"), as.Date("2020-04-03"), by = "day")
date_idx=94
for (date_idx in 1:length(date_ls))
{
  d_this=d[as.Date(d$confirmed_date)<=date_ls[date_idx],]
  daily_new=sum(d_this$confirmed_count[
    (as.Date(d_this$confirmed_date)==date_ls[date_idx]
     )&(
      d_this$state_name=="OH"
     )
    ])
  total_case=sum(d_this$confirmed_count[d_this$state_name=="OH"])
  print(date_ls[date_idx])
  print(dim(d_this))
  if (dim(d_this)[1]==0)
  {
    next
  }
  d_this=aggregate(d_this$confirmed_count,
                   by=list(state_county=d_this$state_county),
                   FUN=sum)
  head(d_this)
  names(d_this)[2]="dailycase"
  f=countypop
  f$state_county=paste(f$abbr,f$county)
  head(f)
  g=merge(f,d_this,by="state_county",all=TRUE)
  head(g)
  g$dailycase[g$dailycase>=10000]=10000
  #g[is.na(g)]=0
  g$logdailycase=log10(g$dailycase)

  p <- plot_usmap(include=c("OH"),
                  data = g, values = "logdailycase",color="white",size=0.1) +
    scale_fill_continuous(name = " ",
                          #label = scales::comma,
                          limits=c(0,3.1),
                          breaks=c(0,1,2,3),
                          labels=c("1","10","100","1000+"),
                          high = "#132B43", low = "#56B1F7") +
    theme(legend.position = "right")
  p <- p + labs(title=paste("Total Ohio COVID-19 Cases by County as of", date_ls[date_idx],
                            "(figure by Apiu, data source at https://coronavirus.1point3acres.com/en) \n
    Ohio Daily New Cases: ",format(daily_new,big.mark=","),
                            "\n    Total Ohio Cases:",format(total_case,big.mark=",")))
  p
  ggsave(paste("OH_cumcase_", date_idx, ".jpeg", sep=""), width = 30, height = 20, units = "cm")

}


#############################
# ohio case and death for kelly

total_case_daily=rep(NA,length(date_ls))
total_death_daily=rep(NA,length(date_ls))
for (date_idx in 1:length(date_ls))
{
  d_this=d[as.Date(d$confirmed_date)<=date_ls[date_idx],]
  total_case_daily[date_idx]=sum(
    d_this$confirmed_count[d_this$state_name=="OH"]
    )
  total_death_daily[date_idx]=sum(
    d_this$death_count[d_this$state_name=="OH"]
  )
}
ggplot(as.data.frame(cbind(date_ls,total_case_daily,total_death_daily)),
       aes(x=date_ls, y=total_case_daily))+geom_point()
ggplot(as.data.frame(cbind(date_ls,total_case_daily,total_death_daily)),
       aes(x=date_ls, y=total_death_daily))+geom_point()
output_data=data.frame(date=as.character(date_ls),
                          case=total_case_daily,
                          death=total_death_daily)
write.csv(output_data,"output_data.csv")


#############################
# cumulative daily case in various states

#state_ls=c("NY")
#state_ls=c("CA")
#state_ls=c("FL")
#state_ls=c("CA","OR","WA","NV","ID")
#state_ls=c("CT","RI","MA","NH","VT","ME")
#state_ls=c("NY","NJ","PA","CT","RI","MA","NH","VT","DE","ME")
#state_ls=c("OH","IN","IL","MI","WI","MN","ND","SD","NE","KS","IA","MO") #midwest

date_ls=seq(as.Date("2020-01-01"), as.Date("2020-04-03"), by = "day")
date_idx=94
for (date_idx in 1:length(date_ls))
{
  d_this=d[as.Date(d$confirmed_date)<=date_ls[date_idx],]

  daily_new=sum(d_this$confirmed_count[
    (as.Date(d_this$confirmed_date)==date_ls[date_idx]
    )&(
      d_this$state_name %in% state_ls
    )
    ])
  total_case=sum(d_this$confirmed_count[d_this$state_name %in% state_ls])
  print(date_ls[date_idx])
  print(dim(d_this))
  d_this=aggregate(d_this$confirmed_count,
                   by=list(state_county=d_this$state_county),
                   FUN=sum)
  names(d_this)[2]="dailycase"
  head(d_this)
  if (dim(d_this)[1]==0)
  {
    next
  }
  # fix for NYC
  NYC_case = d_this$dailycase[d_this$state_county=="NY New York County"]
  if (length(NYC_case)==0)
  {
    NYC_case=NA
  }
  Bronx <- data.frame("NY Bronx County", NYC_case)
  Richmond <- data.frame("NY Richmond County", NYC_case)
  Kings <- data.frame("NY Kings County", NYC_case)
  Queens <- data.frame("NY Queens County", NYC_case)
  names(Bronx)=names(Richmond)=names(Kings)=names(Queens)=names(d_this)
  d_this = rbind(d_this,Bronx,Richmond,Kings,Queens)

  # fix for MA Dukes and Nantucket
  nantucket_case = d_this$dailycase[d_this$state_county=="MA Nantucket County"]
  if (length(nantucket_case)==0)
  {
    nantucket_case=NA
  }
  MADukes <- data.frame("MA Dukes County", nantucket_case)
  names(MADukes)=names(d_this)
  d_this = rbind(d_this,MADukes)


  f=countypop
  f$state_county=paste(f$abbr,f$county)
  head(f)
  g=merge(f,d_this,by="state_county",all=TRUE)
  head(g)
  g$dailycase[g$dailycase>=10000]=10000
  #g[is.na(g)]=0
  g$logdailycase=log10(g$dailycase)

  p <- plot_usmap(#include = .midwest_region,
                  include = state_ls,
                  labels = TRUE,
                  data = g, values = "logdailycase",color="white",size=0.1) +
    scale_fill_continuous(name = " ",
                          #label = scales::comma,
                          limits=c(0,4.5),
                          breaks=c(0,1,2,3,4),
                          labels=c("1","10","100","1000","10000+"),
                          high = "#132B43", low = "#56B1F7") +
    theme(legend.position = "right")
  p <- p + labs(title=paste("Total Regional COVID-19 Cases by County as of", date_ls[date_idx],
                            "\n    Figure by Apiu, data source at https://coronavirus.1point3acres.com/",
                            "\n    Daily New Cases: ",format(daily_new,big.mark=","),
                            "\n    Total Regional Cases:",format(total_case,big.mark=",")))
  p <- p + theme(plot.title = element_text(size=20))
  p
  ggsave(paste("Midwest_cumcase_", date_idx, ".jpeg", sep=""), width = 30, height = 20, units = "cm")
}

###################
# checking state county names

d_agg=aggregate(d$confirmed_count,
                by=list(state_county=d$state_county),
                FUN=sum)
template=countypop
template$state_county=paste(template$abbr,template$county)
head(template)
head(d_agg)
total=merge(template,d_agg,by="state_county",all=TRUE)
dim(total)
dim(template)
dim(d_agg)


sum(!(template$state_county %in% d_agg$state_county))
sum(!(d_agg$state_county %in% template$state_county))
d_agg$state_county[!(d_agg$state_county %in% template$state_county)]

# NY city area fix
cbind(d_agg$state_county[substr(d_agg$state_county, start = 1, stop = 2)=="NY"]
      ,countypop$county[countypop$abbr=="NY"])
cbind(d_agg$state_county[substr(d_agg$state_county, start = 1, stop = 2)=="PA"]
      ,countypop$county[countypop$abbr=="PA"])
cbind(d_agg$state_county[substr(d_agg$state_county, start = 1, stop = 2)=="MA"]
      ,countypop$county[countypop$abbr=="MA"])

g[which(g$abbr=="NY"),]



#############################
# cumulative daily case in USA

date_ls=seq(as.Date("2020-01-01"), as.Date("2020-04-03"), by = "day")
date_idx=94
for (date_idx in 1:length(date_ls))
{
  d_this=d[as.Date(d$confirmed_date)<=date_ls[date_idx],]
  daily_new=sum(d_this$confirmed_count[
    as.Date(d_this$confirmed_date)==date_ls[date_idx]
    ])
  total_case=sum(d_this$confirmed_count)
  print(date_ls[date_idx])
  print(dim(d_this))
  d_this=aggregate(d_this$confirmed_count,
                   by=list(state_county=d_this$state_county),
                   FUN=sum)
  names(d_this)[2]="dailycase"
  head(d_this)
  if (dim(d_this)[1]==0)
  {
    next
  }
  # fix for NYC
  NYC_case = d_this$dailycase[d_this$state_county=="NY New York County"]
  if (length(NYC_case)==0)
  {
    NYC_case=NA
  }
  Bronx <- data.frame("NY Bronx County", NYC_case)
  Richmond <- data.frame("NY Richmond County", NYC_case)
  Kings <- data.frame("NY Kings County", NYC_case)
  Queens <- data.frame("NY Queens County", NYC_case)
  names(Bronx)=names(Richmond)=names(Kings)=names(Queens)=names(d_this)
  d_this = rbind(d_this,Bronx,Richmond,Kings,Queens)

  # fix for MA Dukes and Nantucket
  nantucket_case = d_this$dailycase[d_this$state_county=="MA Nantucket County"]
  if (length(nantucket_case)==0)
  {
    nantucket_case=NA
  }
  MADukes <- data.frame("MA Dukes County", nantucket_case)
  names(MADukes)=names(d_this)
  d_this = rbind(d_this,MADukes)


  f=countypop
  f$state_county=paste(f$abbr,f$county)
  head(f)
  g=merge(f,d_this,by="state_county",all=TRUE)
  head(g)
  g$dailycase[g$dailycase>=20000]=20000
  #g[is.na(g)]=0
  g$logdailycase=log10(g$dailycase)

  p <- plot_usmap(#include = .midwest_region,
    #include = state_ls,
    labels = TRUE,
    data = g, values = "logdailycase",color="white",size=0.1) +
    scale_fill_continuous(name = " ",
                          #label = scales::comma,
                          limits=c(0,4.5),
                          breaks=c(0,1,2,3,4,4.5),
                          labels=c("1","10","100","1000","10000","10000+"),
                          high = "#132B43", low = "#56B1F7") +
    theme(legend.position = "right")
  p <- p + labs(title=paste("Total Regional COVID-19 Cases by County as of", date_ls[date_idx],
                            "\n    Figure by Apiu, data source at https://coronavirus.1point3acres.com/",
                            "\n    Daily New Cases: ",format(daily_new,big.mark=","),
                            "\n    Total Regional Cases:",format(total_case,big.mark=",")))
  p <- p + theme(plot.title = element_text(size=20))
  p
  ggsave(paste("USA_cumcase_", date_idx, ".jpeg", sep=""), width = 30, height = 20, units = "cm")
}
