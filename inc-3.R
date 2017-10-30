#Set workspace and intake data.
setwd("~/Desktop/data")
feb<-xlsx::read.xlsx("February 2017 Final.xlsx",1)[,c(1:5,7,9)]
mar<-xlsx::read.xlsx("March 2017 CCC Data.xlsx",1)[,c(1:5,7,9)]
apr<-xlsx::read.xlsx("April 2017.xlsx",1)[,c(1:5,7,9)]
may<-xlsx::read.xlsx("Crowd Estimates May 2017.xlsx",2)[,c(1:5,7,9)]
jun<-xlsx::read.xlsx("Crowd Estimates June 2017.xlsx",2,colIndex = c(1:5,7,9))
jul<-xlsx::read.xlsx("Crowd Estimates July 2017.xlsx",1)[,c(1:5,7,9)]
aug<-xlsx::read.xlsx("Crowd Estimates August 2017(3).xlsx",2)[,c(1:5,7,9)]
#Combine all data sets and clean up.
count<-as.data.frame(rbind(feb,mar,apr,may,jun,jul,aug))
count$StateTerritory<-trimws(count$StateTerritory)
#Remove entries with significant missing data as well as outliers.
count<-subset(count,!(is.na(count$StateTerritory)))
count$StateTerritory[4093]<-'KS';count<-count[-3662,];list<-ls()
count<-subset(count,!is.na(count$Date))
count<-count[-2820,]
#Import population data
link<-'//*[@id="mw-content-text"]/div/table[1]'
require(rvest)
url <- "http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"
population <- url %>%
  read_html() %>%
  html_nodes(xpath=link) %>%
  html_table()
population <- population[[1]]
population<-population[1:53,c(3,4)]
vc<-c('CA','TX','FL','NY','IL','PA','OH','GA','NC','MI','NJ','VA','WA','AZ','MA','TN','IN','MO','MD','WI','CO','MN','SC','AL','LA','KY','OR','OK','CT','PR','IA','UT','MI','AR','NV','KS','NM','NE','WV','ID','HI','NH','ME','RI','MT','DE','SD','ND','AK','DC','VT','WY','GU')
pop<-cbind(vc,population);pop<-pop[order(pop$vc),]
pop[,3]<-gsub(',','',pop[,3]);pop[,3]<-as.numeric(pop[,3])
#Set up subsets by state,territory
x<-sort(unique(count$StateTerritory));u<-vector('numeric',length(x))
for(i in 1:length(x)){
  ls[[i]]<-subset(count,count$StateTerritory==x[i])
  u[i]<-mean(ls[[i]]$EstimateLow,na.rm = T)/pop[i,3]
}
#First Plot
plot(u,main='Mean crowd size relative to population');text(u,labels=x,cex=0.7,pos=1)
#Set up subsets by date.
y<-unique(count$Date);lt<-list();v<-vector('numeric',length(y))
for(i in 1:length(y)){
  lt[[i]]<-subset(count,count$Date==y[i])
  v[i]<-mean(lt[[i]]$EstimateLow,na.rm = T)
}

#Second Plot
which(v>2000)->z;y0<-y[-z];v0<-v[-z]
plot(y0,v0,type='h',main='Daily mean crowd size')


