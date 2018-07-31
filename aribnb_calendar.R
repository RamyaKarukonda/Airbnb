#Which time of the year will the prices change? 

#using calender dataset

rm(list=ls())

setwd("C:/Users/Ramya/Downloads")
c <- read.csv("testcalendar.csv", header= TRUE, stringsAsFactors = F)

c$Price<-sub("$","",c$price,fixed=TRUE)

c$Price<-sub(",","",c$Price,fixed=TRUE)

c$Price<-as.numeric(c$Price)

names(c)[4]<-paste("p")

c$newdate<-sub("-","/",c$date,fixed=TRUE)
c$newdate<-sub("-","/",c$newdate,fixed=TRUE)
head(c$newdate)

library(lubridate)
library(dplyr)




c$newdate = as.POSIXlt(c$newdate, format="%d/%m/%Y")

str(c)



c$month<-as.Date(cut(c$newdate,breaks="month"))

c$week<-as.Date(cut(c$newdate,breaks="week",start.on.monday = FALSE)) #Changes start point to Sunday

require(ggplot2)

ggplot(data=c,aes(month,Price))+stat_summary(fun.y = mean,geom="bar")
#+ scale_x_date(labels=date_format("%Y-%m"),breaks="1 month")

ggplot(data=c,aes(x=month,y=Price,color=available))+stat_summary(fun.y = mean,geom="point")

ggplot(data=c,aes(week,Price))+stat_summary(fun.y = mean,geom="bar")#+scale_x_date(labels = date_format("%Y-%m-%d"),breaks = "1 week")

cd<-c[c$listing_id==3379116,]

tail(cd)

head(cd$date)

ggplot(data=cd,aes(date,Price))+geom_point()


c$newdate<-as.Date(c$newdate)

weekday<-wday(c$newdate,label=TRUE)

wkagg<-aggregate(Price~weekday, FUN=mean, data=c)

qplot(Price~weekday,data=wkagg)



ggplot(data=wkagg, aes(weekday,Price))+stat_summary(geom="bar")
qplot(Price, weekday, data=wkagg, geom=c("bar", "smooth"))

#tab<-table(c$listing_id,c$available)
#class(tab)

#tb<-table(listing_id=c$listing_id,c$available) 


#tbdf<-as.data.frame(tb)
#head(tbdf$listing_id)

rm(list=ls())

listings<-read.csv("listings.csv", header=TRUE, stringsAsFactors = FALSE  )

ca <- read.csv("calendar.csv", header= TRUE, stringsAsFactors = F)

reviews

#ca$Price<-sub("$","",ca$price,fixed=TRUE)

#ca$Price<-sub(",","",ca$Price,fixed=TRUE)

#ca$Price<-as.numeric(ca$Price)
head(ca)

#names(ca)[4]<-paste("p")
names(ca)[1]<-paste("id")

#mg1<-merge(ca,listings,by="id")
#head(mg1)

mg<-merge(ca[,c("id","price")],listings[,c("id","reviews_per_month")])

head(mg)

summary(lm(price~reviews_per_month,data = mg))

#library(sqldf)

#head(c)
#head(listings$id)

#lis<-aggregate(reviews_per_month~id,FUN=mean,data=listings)
#head(lis)
#lis[lis$listing_id == 241032,]
#length(listings$id)
#length(lis$id)

#head(lis)

calendaragg<-aggregate(Price~listing_id,FUN=mean,data=c)
head(c)
#head(calendaragg)
#head(listings)
#class(calendaragg)

#class(lis$id)

#class(calendaragg$id)
colnames(lis)[1] = "listing_id"
colnames(lis)
names(calendaragg)[1]<-paste("id")

mge<-merge(lis,calendaragg, by="listing_id")

head(mge)

str(mg)


head(c)
rc<-sqldf("select listing_id,Price from c")

rsub<-sqldf("select id, reviews_per_month from listings")

head(c)

mr<-sqldf("select c.listing_id, c.Price, rsub.reviews_per_month
          from c left join rsub on c.listing_id=rsub.listing_id")

reviews<-read.csv("reviews.csv")


