rm(list = ls())

library(readr)
library(stringr)
library(dplyr)
library(tidytext)
dat =  read.csv("C://Users//Ramya Karukonda//Downloads//reviews.csv", header=TRUE, stringsAsFactors = FALSE)
list2 = list1[,c("id","review_scores_rating")]
colnames(list2)[1] <- "listing_id"
head(list2)
list2 = na.omit(list2)

def = merge(x = dat,y= list2, by = "listing_id")
head(def, 50)

head(dat)

?unnest_tokens

review_words <- def %>%
  select(listing_id,id,date,reviewer_id, reviewer_name, review_scores_rating,comments) %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))
head(review_words,100)

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

AFINN

reviews_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(listing_id) %>%
  summarize(sentiment = mean(afinn_score))

reviews_sentiment
colnames(reviews_sentiment)[1] <- "id"

list1 =  read.csv("C://Users//Aman Jain//Downloads//listings.csv", header=TRUE, stringsAsFactors = FALSE)

abc = merge(x = reviews_sentiment,y= list1, by = "id")
str(abc)

abc$price = sub("$","",abc$price, fixed = TRUE)
abc$price = as.numeric(sub(",","",abc$price, fixed = TRUE))
head(abc)

myabc = subset(abc, sentiment > 0 &  sentiment < 4)

fgh = merge(x = reviews_sentiment,y= myabc, by = "id")
head(fgh)
reg = lm(review_scores_rating~sentiment, data = myabc)
summary(reg)

require(ggplot2)
qplot(sentiment.x, review_scores_rating,data= fgh) +
  xlab("sentiment") + ylab("Review")+ theme_bw() + geom_abline(intercept=82.49,slope = 5.883, color="blue")
  

ggplot( data=myabc, aes( x=sentiment, y=review_scores_rating ))+
  geom_point( alpha=.08 )+
  xlab( "Weight (carats)" )+
  ylab( "Price (dollars)" )+
  theme_bw()
#---------------------------------------------------------------------------------------------------------------

review_words_counted <- review_words %>%
  count(listing_id,id,date,reviewer_id, reviewer_name,review_scores_rating, word) %>%
  ungroup()

tail(review_words_counted)


word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(listingss = n_distinct(listing_id),
            reviews = n(),
            uses = sum(n),
            avg_rating = mean(review_scores_rating)) %>%
  ungroup()

head(word_summaries,50)


word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 200, listingss >= 10)

word_summaries_filtered

ggplot(word_summaries_filtered, aes(reviews, avg_rating)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(fgh$review_scores_rating), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average rating")


words_afinn <- word_summaries_filtered %>%
  inner_join(AFINN)

words_afinn


ggplot(words_afinn, aes(afinn_score, avg_rating)) + 
  geom_smooth(method="lm", se=FALSE, show.legend=FALSE) +
  geom_text(aes(label = word, size = NULL), check_overlap = TRUE, vjust=1, hjust=1) +
  geom_point() +
  scale_x_continuous(limits = c(-6,6)) +
  xlab("Sentimental score") +
  ylab("Average Rating Score")


ggplot(words_afinn, aes(reviews, avg_rating, color = afinn_score)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(fgh$review_scores_rating), color = "red", lty = 2) +
  scale_colour_gradient2("AFINN", low = "red", mid = "white", high = "blue", limits = c(-5,5)) +
  xlab("# of reviews") +
  ylab("Average Rating Score")

#####################################################################################################
# second library
library(rJava)
library(RSentiment)

comm = dat$comments
comm = na.omit(comm)
abdd = head(comm,3)

sent = rep(NA,length(comm))
length(sent)
sent = na.omit(sent)
sent1 = sent
head(sent)
length(sent)
for (i in 76940:length(comm)){
  xx = calculate_score(comm[i])
  sent[i] = xx
  gc()
}
sent
length(sent)
tail(sent1, 20)
head(sent,20)
sent = sent[12:length(sent)]
sentiment_score = c(sent1,sent)


head(fgh)

nm = dat[,"listing_id"]
nm = as.data.frame(cbind(nm,sentiment_score))
colnames(nm)[1] <- "listing_id" 
head(nm)


aks <- aggregate(sentiment_score ~ listing_id, data = nm, FUN = mean)
head(aks)

list2 = list1[,c("id","review_scores_rating","price","zipcode","security_deposit","number_of_reviews","host_response_time","host_response_rate","host_has_profile_pic","host_identity_verified","property_type","room_type","bed_type","instant_bookable","require_guest_profile_picture","require_guest_phone_verification")]                         
colnames(list2)[1] <- "listing_id"
price_list = subset(list2, !is.na(price))
rating_list = subset(list2, !is.na(review_scores_rating))  #Subsetting imdb dataset where values in Budget column is not NA
head(list2)
hjjj = subset(merge_rating, sentiment_score <20 & review_scores_rating > 50)
hppp = subset(merge_price, sentiment_score<20 & price<400)

merge_price = merge(x = aks,y= price_list, by = "listing_id")
merge_rating = merge(x = aks,y= rating_list, by = "listing_id")
head(vb, 50)

merge_price$price = sub("$","",merge_price$price, fixed = TRUE)
merge_price$price = as.numeric(sub(",","",merge_price$price, fixed = TRUE))

merge_rating$price = sub("$","",merge_rating$price, fixed = TRUE)
merge_rating$price = as.numeric(sub(",","",merge_rating$price, fixed = TRUE))

qplot(sentiment_score, review_scores_rating,data= hjjj,color =zipcode) +
  xlab("Sentiment_score") + ylab("Review_Score")+ theme_bw() + geom_abline(intercept=92.06130,slope = 0.45466, color="blue")

qplot(sentiment_score, price,data= hppp, color = room_type) +
  xlab("Sentiment_score") + ylab("Price") + theme_bw() +geom_abline(intercept=117.8838,slope = 0.1834, color="green",size = 2)

mm = lm(review_scores_rating~sentiment_score, data = hjjj)
summary(mm)


head(merge_price, 20)
#-------------------------------------------------------------------------------------------------------------
#MAPS
list1$price = sub("$","",list1$price, fixed = TRUE)
list1$price = as.numeric(sub(",","",list1$price, fixed = TRUE))
us.city.map = map_data('county')
ggplot(list1, aes(x = longitude, y = latitude, group=zipcode, fill=price))+ geom_polygon(colour = "white", size = 0.1)

obesity_map <- data.frame(state_names=counties$region, 
                          county_names=counties$subregion, 
                          obesity= runif(nrow(counties), min=0, max=100))

library(data.table)   # use data table merge - it's *much* faster
obesity_map <- data.frame(state_names=list1$city, 
                          county_names=list1$zipcode, 
                          obesity= list1$price)

map.county <- data.table(map_data('county'))
setkey(map.county,region,subregion)
obesity_map <- data.table(obesity_map)
setkey(obesity_map,state_names,county_names)
map.df      <- map.county[obesity_map]

ggplot(map.df, aes(x=long, y=lat, group=group, fill=obesity)) + 
  geom_polygon()+coord_map()



#----------------------------------------------------------------------------------------------

library(XLConnect)    # for loadWorkbook(...) and readWorksheet(...)
library(rgdal)        # for readOGR(...)
library(RColorBrewer) # for brewer.pal(...)
library(data.table)

setwd(" < directory with all your files > ")
wb <- loadWorkbook("DataDownload.xls")   # from the USDA website
df <- readWorksheet(wb,"HEALTH")         # this sheet has the obesity data

US.counties <- readOGR(dsn=".",layer="gz_2010_us_050_00_5m")
#leave out AK, HI, and PR (state FIPS: 02, 15, and 72)
US.counties <- US.counties[!(US.counties$STATE %in% c("02","15","72")),]  
county.data <- US.counties@data
county.data <- cbind(id=rownames(county.data),county.data)
county.data <- data.table(county.data)
county.data[,FIPS:=paste0(STATE,COUNTY)] # this is the state + county FIPS code
setkey(county.data,FIPS)      
obesity.data <- data.table(df)
setkey(obesity.data,FIPS)
county.data[obesity.data,obesity:=PCT_OBESE_ADULTS10]

map.df <- data.table(fortify(US.counties))
setkey(map.df,id)
setkey(county.data,id)
map.df[county.data,obesity:=obesity]

ggplot(map.df, aes(x=long, y=lat, group=group, fill=obesity)) +
  scale_fill_gradientn("",colours=brewer.pal(9,"YlOrRd"))+
  geom_polygon()+coord_map()+
  labs(title="2010 Adult Obesity by Country, percent",x="",y="")+
  theme_bw()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=====

calendar =  read.csv("C://Users//Aman Jain//Downloads//calendar.csv", header=TRUE, stringsAsFactors = FALSE)

dt<-(subset(calendar, available!= "t"))
head(dt)

colnames(dt)[1] <- "listing_id"
numberoflistings<-table(listing_id = dt$listing_id,dt$available)
head(numberoflistings)

Mergedlistingcountstays<- merge(merge_price, numberoflistings, by="listing_id")
head(Mergedlistingcountstays)


qplot(sentiment_score, Freq,data= Mergedlistingcountstays) +
  xlab("sentiment_score") + ylab("Freq")+ theme_bw() + geom_abline(intercept=117.88,slope = 0.1834, color="blue")

#_------------------------------------------------------------------------------------


names(calendar)[1]<-paste("id")

mg1<-merge(calendar[,c("id","price")],list1[,c("id","reviews_per_month")],by="id")
head(mg1)
tail(mg1,50)
mg1[mg1$id == 241032,]
