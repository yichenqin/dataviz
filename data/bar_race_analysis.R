########################################3
# daily plot
rm(list = ls())
library(ggplot2)
d=read.delim("cases_20200407_0540AM.txt",sep=",",header = TRUE)
dim(d)
# number of states needed 56
sort(unique(d$state_name))
state_ls = sort(unique(d$state_name))
#state_ls = state_ls[!state_ls %in% c("Wuhan Evacuee","Diamond Princess", "Grand Princess", "Military")]
length(state_ls)
# number of days needed
max(as.Date(d$confirmed_date))-min(as.Date(d$confirmed_date))
d_state = list()
date_ls = seq(min(as.Date(d$confirmed_date)), max(as.Date(d$confirmed_date)), by = "day")
names(d)
library(dplyr)
d_agg = d %>%
  group_by(confirmed_date, state_name) %>% 
  summarise(confirmed_count = sum(confirmed_count))

library(reshape)

d_daily_tab=cast(d_agg, state_name ~ confirmed_date)
d_daily_tab[is.na(d_daily_tab)]=0
d_total_tab=t(apply(d_daily_tab, 1, cumsum))
d_total_tab=cbind(state_name=rownames(d_total_tab),d_total_tab)

state_region=read.table("state_region.txt",sep = ",",header=TRUE,stringsAsFactors = FALSE)
names(state_region)[2]="state_name"
head(state_region)
d_total_tab=merge(state_region,d_total_tab,by="state_name",all = TRUE)
d_total_tab[is.na(d_total_tab)]=as.character("Others")
write.csv(d_total_tab,"bar_race.csv")
