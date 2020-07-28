library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(hrbrthemes)

options(scipen=999)

setwd("~/OneDrive/Desktop/MP")
dt<-read_xlsx('SYSTEMIC BANKING CRISES DATABASE_2018 (2).xlsx',sheet=2)
dt<-dt[-1,]
dt[is.na(dt)]<-"NA"

# data for each

banking<-dt[,c(1,2)]
currency<-dt[,c(1,3)]
debt<-dt[,c(1,4)]

names(banking)<-names(currency)<-names(debt)<-c("country","year")


# convert table

banking1<-banking%>%separate(year,c("1","2","3","4","5","6","7"),sep=",")%>%
  gather(key="key",value="year",-country,na.rm = TRUE)
currency1<-currency%>%separate(year,c("1","2","3","4","5","6","7"),sep=",")%>%
  gather(key="key",value="year",-country,na.rm = TRUE)
debt1<-debt%>%separate(year,c("1","2","3","4","5","6","7"),sep=",")%>%
  gather(key="key",value="year",-country,na.rm = TRUE)

country_num<-length(unique(dt$Country))

banking1$year<-as.numeric(gsub("NA","",banking1$year))
currency1$year<-as.numeric(gsub("NA","",currency1$year))
debt1$year<-as.numeric(gsub("NA","",debt1$year))
year

banking_omitted<-na.omit(banking1)
currency_omitted<-na.omit(currency1)
debt_omitted<-na.omit(debt1)

banking_crisis_num<-length(unique(banking_omitted$country))
currency_crisis_num<-length(unique(currency_omitted$country))
debt_crisis_num<-length(unique(debt_omitted$country))

dt4<-dt3%>%group_by(cyear)%>%tally()

banking2<-banking_omitted%>%group_by(year)%>%tally()
currency2<-currency_omitted%>%group_by(year)%>%tally()
debt2<-debt_omitted%>%group_by(year)%>%tally()

frequency<-as.data.frame(c(1970:2017))
names(frequency)<-"year"

banking3<-left_join(frequency,banking2)
banking3[is.na(banking3)]<-0


currency3<-left_join(frequency,currency2)
currency3[is.na(currency3)]<-0

debt3<-left_join(frequency,debt2)
debt3[is.na(debt3)]<-0

crisis_number<-left_join(banking3,currency3,by="year")%>%
  left_join(debt3,by="year")
names(crisis_number)<-c("year","Systemic Banking Crisis","Currency Crisis","Sovereign Debt Crisis")

crisis_graph<-crisis_number%>%gather(key="type",value="number",-year)

names(dt)

ggplot(data=crisis_graph,aes(x=year,y=number,group=type))+
  theme_ipsum(base_size=10,axis_title_just = "mc",plot_title_size = 14,axis_title_size = 10,axis_text_size=7,caption_margin =20)+
  theme(plot.caption = element_text(hjust=0),
        panel.background = element_blank(),
        plot.background = element_blank())+
  geom_line(aes(color=type),size=0.5)+
  geom_point(aes(shape=type,color=type),size=1)+
  xlab("Years") + ylab("Number of Crises")+
  labs(caption = "Data source:  Laeven, L., & Valencia , F. (2018). Systemic Banking Crises Revisited. IMF.",
       color="Crisis type",shape="Crisis type")+
  scale_color_viridis_d(end=0.93)+
  theme(legend.position="right")



ggsave("crisis.png",dpi=1200,width =7,height = 4)



freedom<-read_xlsx('freedom.xlsx',sheet=2)

unique<-read_xlsx("unique.xlsx")
wi<-unique$crisis
re<-unique$freedom
for (i in 1:22){
  freedom$Countries<-gsub(paste0("^",re[i],"$"),wi[i],freedom$Countries)
        }



year<-seq(from = 1970, to = 2015, by = 5)
country<-dt$Country


a<-list(banking_omitted,currency_omitted,debt_omitted)

for(j in 1:3){
  a[[j]]$c<-0
  for (i in 1:nrow(a[[j]])){
  if (a[[j]]$year[i]>2015){
    a[[j]]$c[i]<-2015
  }
  else if(a[[j]]$year[i]>2010){
    a[[j]]$c[i]<-2010
  }
  else if(a[[j]]$year[i]>2005){
    a[[j]]$c[i]<-2005
  }
  else if(a[[j]]$year[i]>2000){
    a[[j]]$c[i]<-2000
  }
  else if(a[[j]]$year[i]>1995){
    a[[j]]$c[i]<-1995
  }
  else if(a[[j]]$year[i]>1990){
    a[[j]]$c[i]<-1990
  }
  else if(a[[j]]$year[i]>1985){
    a[[j]]$c[i]<-1985
  }
  else if(a[[j]]$year[i]>1980){
    a[[j]]$c[i]<-1980
  }
  else if(a[[j]]$year[i]>1975){
    a[[j]]$c[i]<-1975
  }
  else if(a[[j]]$year[i]>1970){
    a[[j]]$c[i]<-1970
  }
}
}

banking_cor<-a[[1]]
currency_cor<-a[[2]]
debt_cor<-a[[3]]

names(freedom)
names(banking_cor)

c<-as.data.frame(country)
g<-NULL
for(i in 1:nrow(c)){
  g0<-cbind(c[i,],year)
  g<-as.data.frame(rbind(g,g0))
}

names(g)<-c("country","year")
g$year<-as.numeric(g$year)

banking_cor$bank<-1
currency_cor$currency<-1
debt_cor$debt<-1

banking_cor$key<-currency_cor$key<-debt_cor$key<-NULL
names(banking_cor)<-c("country","cyear","year","bank")
names(currency_cor)<-c("country","cyear","year","currency")
names(debt_cor)<-c("country","cyear","year","debt")

final0<-left_join(g,banking_cor, by = c("country", "year"))%>%left_join(currency_cor,by = c("country", "year"))%>%left_join(debt_cor,by = c("country", "year"))
final0[is.na(final0)]<-0

names(freedom)[1]<-"year"
names(freedom)[3]<-"country"

final1<-left_join(final0,freedom,by = c("country", "year"))
names(final1)
names(final1)[11:15]<-c("gov_size","leg_prop","sound_mon","trade_freed","regul")
final1<-final1[-336,]


############### TRADE ###################



###trade<-read.csv("trade.csv")
###trade[,c(7,9:12)]<-NULL
###names(trade)<-c("country","code","ccountry","ccode","year","exp","imp")
###replace<-read_xlsx("asd.xlsx")
###r<-replace$with
###w<-replace$replace
###for(j in c(1,3)){
###        for (i in 1:47){
###                trade[,j]<-gsub(r[i],w[i],trade[,j])
###        }
###}
###library(utils)
###write.csv(trade, file = "trade_replaced.csv")

trade<-read.csv("trade_replaced.csv")

trade[is.na(trade)]<-0
trade1<-trade%>%group_by(country,ccountry,year)%>%summarise(trade=sum(exp,imp))

trade1<-trade1%>%group_by(country,year)%>%mutate(share=trade/sum(trade))


tr1<-left_join(trade1,banking_cor[,-2],by=c("ccountry"="country","year"))%>%left_join(currency_cor[,-2],by=c("ccountry"="country","year"))%>%left_join(debt_cor[,-2],by=c("ccountry"="country","year"))
tr1$bank[is.na(tr1$bank)]<-0
tr1$currency[is.na(tr1$currency)]<-0
tr1$debt[is.na(tr1$debt)]<-0


trade2<-tr1%>%filter(share>0.05)

bank_partner<-trade2%>%group_by(country,year)%>%filter(bank==1)%>%tally()%>%mutate(bank_partner=1)
bank_partner$n<-NULL
currency_partner<-trade2%>%group_by(country,year)%>%filter(currency==1)%>%tally()%>%mutate(currency_partner=1)
currency_partner$n<-NULL
debt_partner<-trade2%>%group_by(country,year)%>%filter(debt==1)%>%tally()%>%mutate(debt_partner=1)
debt_partner$n<-NULL





final2<-left_join(final1,bank_partner,by=c("country","year"))%>%
  left_join(currency_partner,by=c("country","year"))%>%
  left_join(debt_partner,by=c("country","year"))
final2$bank_partner[is.na(final2$bank_partner)]<-0
final2$currency_partner[is.na(final2$currency_partner)]<-0
final2$debt_partner[is.na(final2$debt_partner)]<-0

##################################

freedom_graph1<-final1%>%group_by(year)%>%summarise(EFW=mean(EFW,na.rm=TRUE))


ggplot(data=freedom_graph1,aes(x=year,y=EFW))+
  theme_ipsum(base_size=10,axis_title_just = "mc",plot_title_size = 14,axis_title_size = 10,axis_text_size=7,caption_margin =20)+
  theme(plot.caption = element_text(hjust=0))+
  geom_line( color="#440154FF") +
  geom_point(shape=24, color="#440154FF", fill="#440154FF", size=1.5)+
  geom_point(aes())
  xlab("Years") + ylab("Freedom Score")+
  coord_cartesian(ylim = c(3,9))+
  labs(title = "Figure 2: The Average Economic Freedom by Year",
       caption = "Data source:  Fraser Institute Economic Freedom (2019)")


ggplot()+
  theme_ipsum(base_size=9,axis_title_just = "mc",plot_title_size = 13,axis_title_size = 9,axis_text_size=6,caption_margin =20)+
  theme(plot.caption = element_text(hjust=0))+

  geom_point(data=final2,aes(year,EFW),alpha=0.2, color="#238A8DFF",size=1)+
xlab("Years") + ylab("Freedom Score")+

  geom_line(data=freedom_graph1,aes(x=year,y=EFW),color="black") +

  geom_point(data=freedom_graph1,aes(x=year,y=EFW),shape=21, fill="black",color="black", size=1.5,alpha=0.7)+

  coord_cartesian(ylim = c(0,10))+
  labs(
       caption = "Data source:  Fraser Institute Economic Freedom (2019)")

ggsave("freedom.png",dpi=1200,width =7,height = 4)




freedom_graph0<-final1%>%group_by(year)%>%summarise(gov_size=mean(gov_size,na.rm=TRUE),
                                                    leg_prop=mean(leg_prop,na.rm=TRUE),
                                                    sound_mon=mean(sound_mon,na.rm=TRUE),
                                                    trade_freed=mean(trade_freed,na.rm=TRUE),
                                                    regul=mean(regul,na.rm=TRUE))
names(freedom_graph0)<-c("year","Size of Government","Legal System & Property Rights","Sound Money"  ,"Trade Freedom","Regulation")
freedom_graph<-freedom_graph0%>%gather(key="Components",value="score",-year)



ggplot(data=freedom_graph,aes(x=year,y=score,group=Components))+
  theme_ipsum(base_size=10,axis_title_just = "mc",plot_title_size = 14,axis_title_size = 10,axis_text_size=7,caption_margin =20)+
  theme(plot.caption = element_text(hjust=0))+
  geom_line(aes(color=Components),size=0.4,alpha=0.9)+
  geom_point(aes(shape=Components,color=Components),size=1,alpha=0.9)+
  xlab("Years") + ylab("Freedom Score")+
  scale_color_viridis_d()+
  theme(legend.position="right")+
  coord_cartesian(ylim = c(3,9))+
  labs(title = "Figure 3: The Average Economic Freedom by Year and Component",
       caption = "Data source:  Fraser Institute Economic Freedom (2019)",
       color="Components",shape="Components")

ggsave("freedom components.png",dpi=1200,width =7,height = 4)




graph_banking<-final1%>% filter(bank==1)
graph_banking<-na.omit(graph_banking)
graph_banking$graph<-"Low"

for (i in 1:103){
  if (graph_banking$EFW[i]>mean(final1$EFW,na.rm=TRUE)){
    graph_banking$graph[i]<-"High"
  }
}

graph_banking1<-graph_banking%>%group_by(graph)%>%tally()%>%cbind(c("Systemic Banking"))
names(graph_banking1)[3]<-"type"

graph_currency<-final1%>% filter(currency==1)
graph_currency<-na.omit(graph_currency)
graph_currency$graph<-"Low"
for (i in 1:126){
  if (graph_currency$EFW[i]>mean(final1$EFW,na.rm=TRUE)){
    graph_currency$graph[i]<-"High"
  }
}
graph_currency1<-graph_currency%>%group_by(graph)%>%tally()%>%cbind(c("Currency"))
names(graph_currency1)[3]<-"type"


graph_debt<-final1%>% filter(debt==1)
graph_debt<-na.omit(graph_debt)
graph_debt$graph<-"Low"
for (i in 1:40){
  if (graph_debt$EFW[i]>mean(final1$EFW,na.rm=TRUE)){
    graph_debt$graph[i]<-"High"
  }
}
graph_debt1<-graph_debt%>%group_by(graph)%>%tally()%>%cbind(c("Sovereign Debt"))
names(graph_debt1)[3]<-"type"

barplot<-rbind(graph_banking1,graph_currency1,graph_debt1)



ggplot(data=barplot,aes(x=type,y=n,fill=factor(graph)))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme_ipsum(base_size=10,axis_title_just = "mc",plot_title_size = 14,axis_title_size = 10,axis_text_size=7,caption_margin =20)+
  theme(plot.caption = element_text(hjust=0),
        axis.title.x = element_blank())+
  labs(fill="Overall Economic Freedom")+
  ylab("Number of Crises")+
  coord_cartesian(ylim = c(0,120))+
  scale_fill_viridis_d(begin=0.4,end=0.1)+
  scale_x_discrete(limits=c("Systemic Banking", "Currency", "Sovereign Debt"))+
  geom_text(aes(label=n), vjust=1.2, color="white",
            position = position_dodge(0.9), size=3)+
  labs(title = "Figure 4:The Number of Crises by Type of Crises and \n                Level of Economic Freedom",
       caption = "Data sources:  Fraser Institute Economic Freedom (2019),\n                        Laeven, L., & Valencia , F. (2018). Systemic Banking Crises Revisited. IMF.",
       color="Components",shape="Components")

ggsave("graph.png",dpi=1200,width =7,height = 4)




banking.ncountries<-length(unique(banking_omitted$country))
currency.ncountries<-length(unique(currency_omitted$country))
debt.ncountries<-length(unique(debt_omitted$country))


country<-as.data.frame(country)







################### Deposit #####################

library(readxl)
deposit<-read_xlsx("deposit_insurence.xlsx")
deposit2<-deposit[-192,c(1,6,9,15)]
names(deposit2)<-c("country","deposit","year","public_deposit")

unique<-read_xlsx("unique.xlsx")
wi<-unique$crisis
re<-unique$freedom
for (i in 1:22){
  deposit2$country<-gsub(paste0("^",re[i],"$"),wi[i],deposit2$country)
}
replace<-read_xlsx("names1.xlsx")
r<-replace$with
w<-replace$replace
for (i in 1:47){
  deposit2$country<-gsub(paste0("^",r[i],"$"),w[i],deposit2$country)
}
rep<-read.csv("names2.csv")
r<-rep$replace
w<-rep$with
for (i in 1:7){
  deposit2$country<-gsub(paste0("^",r[i],"$"),w[i],deposit2$country)
}

deposit1<-deposit2[,c(1,3,4)]
deposit1$public_deposit[is.na(deposit1$public_deposit)]<-0


for (i in 1:nrow(deposit1)){
  if (is.na(deposit1$year[i]==TRUE)){
    deposit1$c[i]<-2020
  }
  else if(deposit1$year[i]<1970){
    deposit1$c[i]<-1970
  }
  else if(deposit1$year[i]<1975){
    deposit1$c[i]<-1975
  }
  else if(deposit1$year[i]<1980){
    deposit1$c[i]<-1980
  }
  else if(deposit1$year[i]<1985){
    deposit1$c[i]<-1985
  }
  else if(deposit1$year[i]<1990){
    deposit1$c[i]<-1990
  }
  else if(deposit1$year[i]<1995){
    deposit1$c[i]<-1995
  }
  else if(deposit1$year[i]<2000){
    deposit1$c[i]<-2000
  }
  else if(deposit1$year[i]<2005){
    deposit1$c[i]<-2005
  }
  else if(deposit1$year[i]<2010){
    deposit1$c[i]<-2010
  }
  else if(deposit1$year[i]<2015){
    deposit1$c[i]<-2015
  }
}

depcountry<-as.data.frame(unique(deposit1$country))
dep<-NULL
for(i in 1:nrow(depcountry)){
  dep0<-cbind(depcountry[i,],year)
  dep<-as.data.frame(rbind(dep,dep0))
}

names(dep)<-c("country","year")
dep$year<-as.numeric(dep$year)

deposit3<-left_join(dep,deposit1[,c(1,3,4)],by=c("country",'year'='c'))
deposit3$public_deposit[is.na(deposit3$public_deposit)]<-0
deposit3<-na.omit(deposit3)
k0<-NULL
for(j in 1:length(unique(deposit3$country))){
  k<-deposit3%>%filter(deposit3$country==unique(deposit3$country)[j])
  for(i in 1:10){
    if(k$public_deposit[i]==1){
      if (i+1>10) {
        break
      }
      else{k$public_deposit[i+1]<-1}

    }
  }
  k0<-rbind(k0,k)
}

final2<-left_join(final2,k0,by=c("country","year"))

####################################################


#################### past crisis ###################
final2$bank_past<-0
final2$currency_past<-0
final2$debt_past<-0


p0<-NULL
for(j in 1:length(unique(final2$country))){
  p<-final2%>%filter(final2$country==unique(final2$country)[j])
  for(i in 1:10){
    if(p$bank[i]==1){
      if (i+1>10) {
        break
      }
      else{p$bank_past[i+1]<-1}

    }
  }

  for(i in 1:10){
    if(p$currency[i]==1){
      if (i+1>10) {
        break
      }
      else{p$currency_past[i+1]<-1}

    }
  }

  for(i in 1:10){
    if(p$debt[i]==1){
      if (i+1>10) {
        break
      }
      else{p$debt_past[i+1]<-1}

    }

  }
  for(i in 1:10){
    if(p$debt[i]==1){
      if (i+2>10) {
        break
      }
      else{p$debt_past[i+2]<-1}

    }

  }
  p0<-rbind(p0,p)
}
final2<-p0
######################################

######### GDP ########################

gdp<-read_xls("GDP_per_cap.xls")
gdp1<-gdp%>%gather(key="year",value="gdp_per_cap",5:63)
gdp1$year<-as.numeric(gdp1$year)

final2<-final2%>%left_join(gdp1[,c(2,5,6)],by=c("ISO_Code"="Country Code","year"))


######################################

############# DEBT ###################



debt_to_gdp<-read_xls("debt.xls")

names(debt_to_gdp)[1]<-"country"

for (i in 1:22){
  debt_to_gdp$country<-gsub(paste0("^",re[i],"$"),wi[i],debt_to_gdp$country)
}
replace<-read_xlsx("names1.xlsx")
r<-replace$with
w<-replace$replace
for (i in 1:47){
  debt_to_gdp$country<-gsub(paste0("^",r[i],"$"),w[i],debt_to_gdp$country)
}
rep<-read.csv("names2.csv")
repl<-rep$replace
with<-rep$with
for (i in 1:7){
  debt_to_gdp$country<-gsub(paste0("^",repl[i],"$"),with[i],debt_to_gdp$country)
}

write.csv(unique(debt_to_gdp$country),"debt_to.csv")

repla<-read.csv("debt_to.csv")
replac<-repla$replace
wit<-repla$with
for (i in 1:7){
  debt_to_gdp$country<-gsub(paste0("^",replac[i],"$"),wit[i],debt_to_gdp$country)
}

debt_to_gdp1<-debt_to_gdp%>%gather(value="debt_to_gdp",key="year",-1)
debt_to_gdp1$year<-as.numeric(debt_to_gdp1$year)

final2<-final2%>%left_join(debt_to_gdp1,by=c("country","year"))
