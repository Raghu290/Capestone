#install.packages("xlsx")
library("xlsx")
#install.packages('reshape2')
library("reshape2")
#install.packages('ggplot2')
library("ggplot2")
#install.packages('docxtractr')
library(docxtractr)
# install.packages("lubridate")
library(lubridate)
#install.packages("tidyr")
library(tidyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("MASS")
library("MASS")
#install.packages("zoo")
library(zoo)
#install.packages("car")
library(car)
#install.packages("DataCombine")
library(DataCombine)
#install.packages("plyr", dependencies = TRUE)
library(plyr)

############  product List sheet   #####################
product_list_df<-read.xlsx("Media data and other information.xlsx", sheetName = "Product List", header=TRUE, colClasses=NA)
colnames(product_list_df)<-c("Product","Frequency","Percent")

#--- EDA and data cleaning---##
product_list_df$Percent <-format(round(product_list_df$Percent, 2), nsmall = 2)
product_list_df$Percent<-as.numeric(product_list_df$Percent)
#Removing the last row which is summary in excel.
product_list_df<-product_list_df[-which(product_list_df$Percent == 100.00),]
str(product_list_df)
#checking if any NA in df
sum(is.na(product_list_df)) #0 no NA's

###Plotting first few Products having highest Frequency of adversiting
head(product_list_df[order(-product_list_df$Frequency),])
qplot(data = head(product_list_df[order(-product_list_df$Frequency),]), x = Product,y=Frequency) + ylab("Frequency")
#Clearly LaptopSpeaker and MobileSpeaker have highest
###############################################################################################################################

####################################### EDA of Media Investment sheet ##################################################
media_investement_df<-read.xlsx("Media data and other information.xlsx", sheetName = "Media Investment",startRow=3, header=TRUE, colClasses=NA)

### Data cleaning  ####
media_investement_df$Content.Marketing <-format(round(media_investement_df$Content.Marketing, 2), nsmall = 2)
media_investement_df$Content.Marketing <-as.numeric(media_investement_df$Content.Marketing)
media_investement_df[is.na(media_investement_df)] <- 0

##PLotting the monthly total investemnt
ggplot(data = media_investement_df, aes(x=reorder(Month,Year),y= Total.Investment)) + 
  xlab("Month")+
  ylab("Total.Investment in Crores")+ geom_bar(stat="identity")+aes(fill=as.factor(Year))

#Understad how melting works#
#when you melt essentially you create only one column with the value and one column with the variable i.e. your x,y,z
#ggplot it. x axis will be m, y will be the value and fill will be essentially your x,y,z

#Melting whole df for each ADV investment plot
media_investement_df ->melting_df
melting_df[,-c(3)]->melting_df
melting_df <- melt(melting_df, id.vars=c('Month','Year'))   
#media wise investment monthly bases
ggplot(melting_df, aes(x=reorder(Month,Year), y=value, fill=variable)) + geom_bar(stat='identity')
ggplot(melting_df, aes(x=reorder(Month,Year), y=value, fill=variable)) + geom_bar(stat='identity',position='dodge')
#clearly above plots show sponsorship is having high investment
###############################################################################################################################

#############################################EDA product details with frequency of adv#######################################################

#Reading Product category docx
docx_obj<-read_docx("Product Details.docx")
docx_df<-docx_extract_all_tbls(docx_obj,guess_header = T,trim = T)
product_category_df<-as.data.frame(docx_df)
colnames(product_category_df) <- c("super_category","category","sub_category","Product")
product_category_df <- product_category_df[-1,]
View(product_category_df)

sum(is.na(product_category_df))

product_category_df$Product<-toupper(product_category_df$Product)
product_list_df$Product<-toupper(product_list_df$Product)
#Combine 2 data frames to map products and its categorys (SKU)
setdiff(product_list_df$Product,product_category_df$Product)
product_list_df <-merge(product_list_df,product_category_df,by="Product")

#Clearly Speaker subcategory has highest frequency of adv
ggplot(data = product_list_df, aes(x=sub_category,y= Frequency)) + 
  xlab("category")+
  ylab("Frequency of adv")+ geom_bar(stat="identity")

###############################################################################################################################

######################################## EDA Consumer Transaction df #####################################################
#Consumer Transaction dataframe
consumer_df<-read.csv("ConsumerElectronics.csv",stringsAsFactors = F,header = T)
str(consumer_df)
consumer_df_bckp<-consumer_df


####### outlier treatment function by capping of Continous variables ###########

outlier_adjustment <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  x
}

outlier_adjustment_fordays <- function(x){
  qnt <- quantile(x, probs=c(.25, .90), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  x
}

##### EDA and Data Cleaning #####

#1. Check NA's
sum(is.na(consumer_df)) #14712 are NA
sapply(consumer_df, function(x) length(which(is.na(x))))
#gmv   cust_id  pincode
#4904  4904     4904
#Checking percentage of na in total data. it is very low so we can remove those rows
colMeans(is.na(consumer_df)) # this is very less 0.3 percent of whole data so we can remove them.
#Lets removes these rows
consumer_df <- consumer_df %>% drop_na()

# Blank Values
sapply(consumer_df, function(x) length(which(x == ""))) #No blank values

#GMV
consumer_df <- filter(consumer_df,consumer_df$gmv!=0)
# Year and Month columns (As per given info data should be from july 2015 to june 2016)
consumer_df$Year <- as.numeric(consumer_df$Year)
consumer_df$Month <- as.numeric(consumer_df$Month)

consumer_df <- filter(consumer_df, (consumer_df$Year==2015 & consumer_df$Month %in% c(7,8,9,10,11,12)) | (consumer_df$Year==2016 & consumer_df$Month %in% c(1,2,3,4,5,6)))

# order_id and order_item_id columns. Not negative values. Looks fine.
summary(consumer_df$order_id)
summary(consumer_df$order_item_id)

# units column. # checking if any outliers in Units
consumer_df$units <- as.numeric(consumer_df$units)
summary(consumer_df$units)   # No negative values
ggplot(consumer_df, aes(x= "", y= units)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "units") + ggtitle("Boxplot of Units")


####
# deliverybday
consumer_df[consumer_df$deliverybdays=="\\N",9]<-0
consumer_df[consumer_df$deliverybdays < 0,9]<-0
ggplot(consumer_df, aes(x= "", y= deliverybdays)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "deliverybdays") + ggtitle("Boxplot of sla")
consumer_df$deliverybdays<- as.numeric(consumer_df$deliverybdays)
consumer_df$deliverybdays <- outlier_adjustment_fordays(consumer_df$deliverybdays)
# deliverycday
consumer_df[consumer_df$deliverycdays=="\\N",10] <- 0
consumer_df[consumer_df$deliverycdays < 0,10]<-0
consumer_df$deliverycdays<- as.numeric(consumer_df$deliverycdays)
consumer_df$deliverycdays <- outlier_adjustment_fordays(consumer_df$deliverycdays)#capping to 0.90precentile
#. s1_fact.order_payment_type - factor with COD and prepaid mode of payment
consumer_df$s1_fact.order_payment_type <- as.factor(consumer_df$s1_fact.order_payment_type)
str(consumer_df$s1_fact.order_payment_type)

# sla
consumer_df$sla <- as.numeric(consumer_df$sla)
ggplot(consumer_df, aes(x= "", y= sla)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "sla") + ggtitle("Boxplot of sla")
consumer_df$sla <- outlier_adjustment(consumer_df$sla) # capping to .75 percentile. Now it will be from 0 to 11

# product_analytic_super_category - this has only one value CE(removing it from analysis)
consumer_df$product_analytic_super_category <- as.factor(consumer_df$product_analytic_super_category)

# product_analytic_category
consumer_df$product_analytic_category <- as.factor(consumer_df$product_analytic_category)
#Camera    CameraAccessory EntertainmentSmall          GameCDDVD     GamingHardware 
#100898             256706             941288             114174             230245 

# product_analytic_sub_category
consumer_df$product_analytic_sub_category <- as.factor(consumer_df$product_analytic_sub_category)

consumer_df$product_analytic_vertical <- as.factor(consumer_df$product_analytic_vertical)

# product_mrp . It has some data with 0. which we need filter
consumer_df$product_mrp <- as.numeric(consumer_df$product_mrp)
consumer_df<- filter(consumer_df, product_mrp > 0)
# product_procurement_sla - it has negative values
summary(consumer_df$product_procurement_sla)
consumer_df<- filter(consumer_df, product_procurement_sla >= 0)
ggplot(consumer_df, aes(x= "", y= product_procurement_sla)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + scale_y_continuous(name = "product_procurement_sla") + ggtitle("Boxplot of product_procurement_sla")
consumer_df$product_procurement_sla <- outlier_adjustment(consumer_df$product_procurement_sla)
#capping to 5
#pincode and cust_id has no impact. removing them(pincode we already have SLA which will take care of impact).
consumer_df$pincode <- NULL
consumer_df$cust_id <- NULL
#checking if any corrleation between sla and procurement sla. its -0.017 which is very low. So we can have both these for modelling
cor(consumer_df$sla,consumer_df$product_procurement_sla)

#### Since we need to do analyis on weekly bases add week column
# 1.creating the week of year based on order date.
consumer_df$order_date <- parse_date_time(consumer_df$order_date, c("%Y-%m-%d %H:%M:%S"), tz = "")
consumer_df <- consumer_df[order(consumer_df$order_date),]
week <- as.numeric(consumer_df$order_date-consumer_df$order_date[1]) %/% (7*24*60*60)
week <- week+1
week <- as.data.frame(week)
consumer_df$week <- week$week
summary(consumer_df$week)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   16.00   29.00   28.45   40.00   53.00

# do some Univarient nad bivarient analyis here
#PLot weekly revenue of firm
ggplot(data = consumer_df, aes(x=week,y= gmv)) + 
  xlab("Week")+
  ylab("Revenue")+ geom_bar(stat="identity")+aes(fill=as.factor(Year))


##################################### Feature Engineering ############################################
#1. added week column to dataset

# 2.Creating deliveryDelay with addition deliverybdays(warhouse) and deliverycdays(customer primise) values 
consumer_df$deliveryDelay <- consumer_df$deliverybdays + consumer_df$deliverycdays
#Take cut off as 7 days, as we max delay as 13.
consumer_df$delayed <- ifelse(consumer_df$deliveryDelay <= 7,0,1)
consumer_df$deliveryDelay <-NULL

# 3. dayOfWeek
consumer_df$order_date <- date(consumer_df$order_date)
consumer_df$dayOfWeek <- weekdays(consumer_df$order_date)
consumer_df$dayOfWeek <- as.factor(consumer_df$dayOfWeek)

#4. Discount percentage/ promotional percantage offered - and binning it as per discount
#calculation : listprice= gmv/no. of units sold
#discount = mrp-listprice/mrp
consumer_df -> consumer_df_backup
consumer_df$list_price <- consumer_df$gmv / consumer_df$units
consumer_df$discount_offered <- (consumer_df$product_mrp - consumer_df$list_price) / consumer_df$product_mrp
summary(consumer_df$discount_offered)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-7.9965  0.2749  0.4467  0.4395  0.6096  1.0000 
###Bining this discount range into 4 bins ranges
binRange <- c(-Inf, 0, 0.25,0.5,0.75 ,1)
binLabels <- c("NoProfitDiscount","lessThan25pcDiscount","25to50pcDiscount","50to75pcDiscount","75to100pcDiscount")
consumer_df$promotion_range <- cut(consumer_df$discount_offered, breaks = binRange, labels = binLabels)
summary(consumer_df$promotion_range)


#5. delivery perception of customer -total sla bining
consumer_df$customer_slaPerception <- consumer_df$sla + consumer_df$product_procurement_sla
summary(consumer_df$customer_slaPerception ) #based on quartiles, we decide range of bin 0-5,5-10,>10
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   6.000   8.000   8.302  10.000  16.000 
binRange <- c( 0, 5,10,Inf)
binLabels <- c("fastDeliveryPerception","tolerableDeliveryPerception","delayedDeliveryPerception")
consumer_df$deliveryPerception <- cut(consumer_df$customer_slaPerception, breaks = binRange, labels = binLabels)
summary(consumer_df$deliveryPerception)
consumer_df$customer_slaPerception <-NULL

#6. holiday effect - includes special days and general holidays
#Eid & Rathayatra sale (18-19th July)
#Independence Sale (15-17th Aug)
#Rakshabandhan Sale (28-30th Aug)
#Daussera sale (17-15th Oct)
#Big Diwali Sale (7-14th Nov)
#Christmas & New Year Sale (25th Dec'15 - 3rd Jan'16)
#Republic Day (20-22 Jan)
#BED (1-2 Feb)
#FHSD (20-21 Feb)
#Valentine's Day (14-15 Feb)
#BSD-5 (7-9 Mar)
#Pacman (25-27 May)
holiday <- c("2015-07-01", "2015-07-18","2015-07-19",
             "2015-08-15","2015-08-16","2015-08-17" ,"2015-08-28","2015-08-29","2015-08-30",
             "2015-10-15", "2015-10-16", "2015-10-17",
             "2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-11-11","2015-11-12","2015-11-13","2015-11-14",
             "2015-12-25","2015-12-26","2015-12-27","2015-12-28","2015-12-29","2015-12-30","2015-12-31",
             "2016-01-01","2016-01-02","2016-01-03","2016-01-20","2016-01-21","2016-01-22",
             "2016-02-01","2016-02-02","2016-02-14","2016-02-15","2016-02-20","2016-02-21",
             "2016-03-07","2016-03-08","2016-03-09",
             "2016-05-25","2016-05-26","2016-05-27")
holiday_list<- as.Date(holiday)
holiday_list<- parse_date_time(holiday, c("%Y-%m-%d"), tz = "")
holiday_list<- as.data.frame(holiday_list)
week <- as.numeric(holiday_list$holiday_list-holiday_list$holiday_list[1]) %/% (7*24*60*60)
week <- week+1
week <- as.data.frame(week)
week$holidays<-1
week <- aggregate(holidays~week,week,sum)
week<-week[-1,]
consumer_df <- merge(consumer_df,week,by="week",all.x = T)
consumer_df$holidays[which(is.na(consumer_df$holidays))] <- 0


#7. P_tag
#Here wedivide entrie products of product_analytic_vertical category into 3 categories mass moving, premium and medium based on kmeans
#consumer_df$list_price <- consumer_df$gmv / consumer_df$units
cluster <- aggregate(cbind(units,list_price,product_mrp)~product_analytic_vertical,consumer_df,mean)
cluster$list_price1<- scale(cluster$list_price)
cluster$units1<-scale(cluster$units)
cluster$product_mrp1<-scale(cluster$product_mrp)
str(cluster)
k1<-cluster[,-c(1:3)]
clust<-kmeans(k1,centers = 3,iter.max = 50,nstart = 50)
cluster$p_tag <- as.factor(clust$cluster)
cluster <- cluster[,c(1,8)]
consumer_df <- merge(consumer_df,cluster,by= "product_analytic_vertical",all.x=T)
k2<-  plyr::count(consumer_df$p_tag)[2]
levels(consumer_df$p_tag)[which(k2==max(plyr::count(consumer_df$p_tag)[2]))] <- "mass"
levels(consumer_df$p_tag)[which(k2==min(plyr::count(consumer_df$p_tag)[2]))] <- "premium"
levels(consumer_df$p_tag)[which(k2!=max(plyr::count(consumer_df$p_tag)[2]) & k2!=min(plyr::count(consumer_df$p_tag)[2]))] <- "middle"

#8. Moving avg

myfunc1 = function(x) zoo::rollmean(x,k=2,fill=NA,align = "right")
myfunc2 = function(x) zoo::rollmean(x,k=3,fill=NA,align = "right")
myfunc3 = function(x) zoo::rollmean(x,k=4,fill=NA,align = "right")

dataset <- consumer_df[,c("p_tag","week","list_price","discount_offered")]
dataset <-arrange(dataset,p_tag,week)
x1 <- dataset %>% group_by(p_tag) %>% mutate_at(vars(list_price,discount_offered),funs(myfunc1)) %>% data.frame()
x2 <- dataset %>% group_by(p_tag) %>% mutate_at(vars(list_price,discount_offered),funs(myfunc2)) %>% data.frame()
x3 <- dataset %>% group_by(p_tag) %>% mutate_at(vars(list_price,discount_offered),funs(myfunc3)) %>% data.frame()
x1$LP_MA1<- x1$list_price
x2$LP_MA2<- x2$list_price
x3$LP_MA3<- x3$list_price
x1$PO_MA1 <- x1$discount_offered
x2$PO_MA2 <- x2$discount_offered
x3$PO_MA3 <- x3$discount_offered

x4<-cbind(x1[,-c(1:4)],x2[,-c(1:4)],x3[,-c(1:4)])
consumer_df <-arrange(consumer_df,p_tag,week)
consumer_df <- cbind(consumer_df,x4[,c(1,3,5,2,4,6)])
consumer_df<-na.omit(consumer_df)

consumer_df$inc_LP_MA1 <- (consumer_df$list_price - consumer_df$LP_MA1) / consumer_df$LP_MA1
consumer_df$inc_LP_MA2 <- (consumer_df$list_price - consumer_df$LP_MA2) / consumer_df$LP_MA2
consumer_df$inc_LP_MA3 <- (consumer_df$list_price - consumer_df$LP_MA3) / consumer_df$LP_MA3

consumer_df$inc_PO_MA1 <- (consumer_df$discount_offered - consumer_df$PO_MA1) / consumer_df$PO_MA1
consumer_df$inc_PO_MA2 <- (consumer_df$discount_offered - consumer_df$PO_MA2) / consumer_df$PO_MA2
consumer_df$inc_PO_MA3 <- (consumer_df$discount_offered - consumer_df$PO_MA3) / consumer_df$PO_MA3

consumer_df$LP_MA1<- NULL
consumer_df$LP_MA2<- NULL
consumer_df$LP_MA3<- NULL
consumer_df$PO_MA1 <- NULL
consumer_df$PO_MA2 <- NULL
consumer_df$PO_MA3 <- NULL

#9. Lag price

consumer_df <- slide(consumer_df,Var="list_price",GroupVar = "p_tag",slideBy = -1)
consumer_df <- slide(consumer_df,Var="list_price",GroupVar = "p_tag",slideBy = -2)
consumer_df <- slide(consumer_df,Var="list_price",GroupVar = "p_tag",slideBy = -3)

consumer_df$LP_1week <- (consumer_df$list_price - consumer_df$`list_price-1`)/consumer_df$`list_price-1`
consumer_df$LP_2week <- (consumer_df$list_price - consumer_df$`list_price-2`)/consumer_df$`list_price-2`
consumer_df$LP_3week <- (consumer_df$list_price - consumer_df$`list_price-3`)/consumer_df$`list_price-3`

consumer_df$LP_1week <- ifelse(is.na(consumer_df$LP_1week),0,consumer_df$LP_1week)
consumer_df$LP_2week <- ifelse(is.na(consumer_df$LP_2week),0,consumer_df$LP_2week)
consumer_df$LP_3week <- ifelse(is.na(consumer_df$LP_3week),0,consumer_df$LP_3week)

consumer_df$`list_price-1`<-NULL
consumer_df$`list_price-2`<-NULL
consumer_df$`list_price-3`<-NULL
####################################
#10. NPS score
NPS <- c(54.6,60,46.9,44.4,47,45.8,47.1,50.3,49,51.8,47.3,50.5)
NPS <- as.data.frame(NPS)
media_investement_df <- cbind(media_investement_df,NPS)

#################### 11.Adstock of advertising combine with main dataframe ############################

month_week_df<- consumer_df[,c(6,2)]
month_week_df <- unique(month_week_df)

media_investement_df <- merge(media_investement_df,month_week_df,by="Month",all.x = T)
#basically we divide the mnthly spending on weekly bases, based on number of weeks in month.
months <- c(1:12)
for (yearMonth in months){
  media_investement_df[media_investement_df$Month==yearMonth,c(3:12)]/nrow(media_investement_df[media_investement_df$Month==yearMonth,])->
    media_investement_df[media_investement_df$Month==yearMonth,c(3:12)]
}
### Now we weeks in month, this helps to split the adverstise investment accordinly weekly bases for analysis.below is we do as per week
media_investement_df <- ddply(media_investement_df, 'week', summarize, Total.Investment =mean(Total.Investment),
                  TV=mean(TV), Digital =mean(Digital),
                  Sponsorship =mean(Sponsorship),Content.Marketing =mean(Content.Marketing),
                  Online.marketing =mean(Online.marketing),X.Affiliates =mean(X.Affiliates),SEM =mean(SEM)
                  ,Radio =mean(Radio),Other =mean(Other),NPS = mean(NPS))

#########creating the adstock  with 0.5 impact of previeous week
#ad.adstock <- as.numeric(filter(x=ad$ad, filter=.50, method="rec$ursive"))
media_investement_df$Total.Investment <- as.numeric(stats::filter(x=media_investement_df$Total.Investment, filter=.50, method="recursive"))
media_investement_df$TV <- as.numeric(stats::filter(x=media_investement_df$TV, filter=.50, method="recursive"))
media_investement_df$Digital <- as.numeric(stats::filter(x=media_investement_df$Digital, filter=.50, method="recursive"))
media_investement_df$Sponsorship<- as.numeric(stats::filter(x=media_investement_df$Sponsorship, filter=.50, method="recursive"))
media_investement_df$Content.Marketing <- as.numeric(stats::filter(x=media_investement_df$Content.Marketing, filter=.50, method="recursive"))
media_investement_df$Online.marketing <- as.numeric(stats::filter(x=media_investement_df$Online.marketing, filter=.50, method="recursive"))
media_investement_df$X.Affiliates <- as.numeric(stats::filter(x=media_investement_df$X.Affiliates, filter=.50, method="recursive"))
media_investement_df$SEM <- as.numeric(stats::filter(x=media_investement_df$SEM, filter=.50, method="recursive"))
media_investement_df$Radio <- as.numeric(stats::filter(x=media_investement_df$Radio, filter=.50, method="recursive"))
media_investement_df$Other <- as.numeric(stats::filter(x=media_investement_df$Other, filter=.50, method="recursive"))


#Combining the consumer and media investment data frames
consumer_df<-merge(consumer_df,media_investement_df,by="week",sort=F,all.x = T)

################################## preparing for modelling ########################
consumer_df$product_analytic_vertical <-NULL
consumer_df$fsn_id <- NULL
consumer_df$order_date <- NULL
consumer_df$Year <-NULL
consumer_df$Month <- NULL
consumer_df$order_id<- NULL
consumer_df$order_item_id <-NULL
consumer_df$deliverybdays <- NULL
consumer_df$deliverycdays <- NULL
consumer_df$product_analytic_category <- NULL
consumer_df$product_analytic_super_category<- NULL
consumer_df$sla<-NULL
consumer_df$product_procurement_sla<-NULL
consumer_df$holidayweek <- ifelse(consumer_df$holiday > 0,1,0)


camera_df <- filter(consumer_df ,product_analytic_sub_category=='CameraAccessory')
gaming_df <- filter(consumer_df ,product_analytic_sub_category=='GamingAccessory')
homeaudio_df <- filter(consumer_df ,product_analytic_sub_category=='HomeAudio')
camera_df$product_analytic_sub_category <-NULL
gaming_df$product_analytic_sub_category <- NULL
homeaudio_df$product_analytic_sub_category <- NULL

#EDA on each category.
ggplot(data = camera_df, aes(x=week,y= gmv)) + 
  xlab("Week")+
  ylab("camera Accessory Revenue(GMV)")+ geom_bar(stat="identity")+aes(fill=as.factor(holidayweek))

ggplot(data = gaming_df, aes(x=week,y= gmv)) + 
  xlab("Week")+
  ylab("gaming Accessory Revenue(GMV)")+ geom_bar(stat="identity")+aes(fill=as.factor(holidayweek))


ggplot(data = homeaudio_df, aes(x=week,y= gmv)) + 
  xlab("Week")+
  ylab("homeaudio Accessory Revenue(GMV)")+ geom_bar(stat="identity")+aes(fill=as.factor(holidayweek))

#Linear Model data preparation
prepareForLinearAdditiveModel <- function(basic_data_frame){
k<-model.matrix(~basic_data_frame$p_tag)
basic_data_frame <- cbind(basic_data_frame,k[,-1])
basic_data_frame$p_tag<-NULL

k<-model.matrix(~basic_data_frame$deliveryPerception)
basic_data_frame <- cbind(basic_data_frame,k[,-1])
basic_data_frame$deliveryPerception<-NULL

k<-model.matrix(~basic_data_frame$promotion_range)
basic_data_frame <- cbind(basic_data_frame,k[,-1])
basic_data_frame$promotion_range<-NULL

k<-model.matrix(~basic_data_frame$dayOfWeek)
basic_data_frame <- cbind(basic_data_frame,k[,-1])
basic_data_frame$dayOfWeek<-NULL

basic_data_frame$holidays<-as.numeric(basic_data_frame$holidays)
basic_data_frame$s1_fact.order_payment_type <- as.numeric(basic_data_frame$s1_fact.order_payment_type)
basic_data_frame$delayed <- as.numeric(basic_data_frame$delayed)

basic_data_frame$week <- NULL
basic_data_frame$units <- NULL
basic_data_frame$list_price <- NULL
basic_data_frame$product_mrp <- NULL
basic_data_frame$holidayweek <- NULL
basic_data_frame$discount_offered <- NULL
return (basic_data_frame)
}
camera_additive_df <- prepareForLinearAdditiveModel(camera_df)
gaming_additive_df <- prepareForLinearAdditiveModel(gaming_df)
homeaudio_additive_df <- prepareForLinearAdditiveModel(homeaudio_df)

#Linear Model
#camera
set.seed(100)
train_index<-sample(1:nrow(camera_additive_df),0.7*nrow(camera_additive_df))
train<-camera_additive_df[train_index,]
test<-camera_additive_df[-train_index,]
lm(gmv~.,train)->camera_model
summary(camera_model)
#Residual standard error: 169.3 on 162036 degrees of freedom
#Multiple R-squared:  0.7655,	Adjusted R-squared:  0.7655  
step <- stepAIC(camera_model, direction="both")
summary(step)
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3 + 
                LP_1week + LP_2week + LP_3week + Total.Investment + TV + 
                Digital + Sponsorship + Content.Marketing + Online.marketing + 
                X.Affiliates + SEM + Radio + Other + NPS + `basic_data_frame$p_tagpremium` + 
                `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` + `basic_data_frame$dayOfWeekMonday` + 
                `basic_data_frame$dayOfWeekTuesday`, 
              data = train)
summary(model_2)
library(car)
vif(car) 
#This looks small <2 for all. So we try to remove based on P value. Removing `basic_data_frame$promotion_range25to50pcDiscount`
model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3 + 
                LP_1week + LP_2week + LP_3week + Total.Investment + TV + 
                Digital + Sponsorship + Content.Marketing + Online.marketing + 
                X.Affiliates + SEM + Radio + Other +  `basic_data_frame$p_tagpremium` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` + `basic_data_frame$dayOfWeekMonday` + 
                `basic_data_frame$dayOfWeekTuesday`, 
              data = train)
summary(model_3)
#removing `basic_data_frame$dayOfWeekSunday`
model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
                LP_1week + LP_2week + LP_3week + Total.Investment + TV + 
                Digital + Sponsorship + Online.marketing + 
                X.Affiliates + SEM + Radio + Other + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` + `basic_data_frame$dayOfWeekMonday` + 
                `basic_data_frame$dayOfWeekTuesday`, 
              data = train)
summary(model_4)
#Removing complete dayOfweek, no impact on camera sales
model_5 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed +
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
                LP_1week + LP_2week + LP_3week + Total.Investment + TV + 
                Digital + Sponsorship + Online.marketing + 
                X.Affiliates + SEM + Radio + Other +  `basic_data_frame$p_tagpremium` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` , 
              data = train)
summary(model_5)
#No changes in adjusted R square, removing Digital
vif(model_5)
camera_liner_final <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed +
               inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
               LP_1week + LP_2week + LP_3week + Total.Investment + TV + 
                Sponsorship + Online.marketing + 
               X.Affiliates + SEM + Radio + Other +  
               `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
               `basic_data_frame$promotion_range50to75pcDiscount` + 
               `basic_data_frame$promotion_range75to100pcDiscount` , 
             data = train)
summary(camera_liner_final)
#Multiple R-squared:  0.7523,	Adjusted R-squared:  0.7523 
#holidays have no impact on camera sales


#Gaming df
train_index<-sample(1:nrow(gaming_additive_df),0.7*nrow(gaming_additive_df))
train<-gaming_additive_df[train_index,]
test<-gaming_additive_df[-train_index,]
lm(gmv~.,train)->gamming_model
step <- stepAIC(gamming_model, direction="both")
summary(step)
#Multiple R-squared:  0.6071,	Adjusted R-squared:  0.6071 
#removing `basic_data_frame$dayOfWeekSunday` with low p value
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                Radio + Other + NPS + `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                 `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount`  + 
                `basic_data_frame$dayOfWeekTuesday`, data = train)
summary(model_2)
#`basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` is with low value
model_3 <-  lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                 inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + LP_1week + 
                 LP_2week + LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                 Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                 Radio + Other + NPS + 
                 `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                 `basic_data_frame$promotion_range50to75pcDiscount` + 
                 `basic_data_frame$promotion_range75to100pcDiscount`  + 
                 `basic_data_frame$dayOfWeekTuesday`, data = train)
summary(model_3)
vif(model_3)
model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed  + 
                inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                Radio + Other + NPS + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount`  + 
                `basic_data_frame$dayOfWeekTuesday`, data = train)
summary(model_4)
vif(model_4)
#Multiple R-squared:  0.6239,	Adjusted R-squared:  0.6238 
model_5 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed  + 
                inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment +
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount`  + 
                `basic_data_frame$dayOfWeekTuesday`, data = train)
summary(model_5)
vif(model_5)
model_6<- lm(formula = gmv ~ s1_fact.order_payment_type + delayed  + 
                inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment +
                Content.Marketing + SEM + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount`  + 
                `basic_data_frame$dayOfWeekTuesday`, data = train)
summary(model_6)
vif(model_6)
gaming_linear_final<- lm(formula = gmv ~ s1_fact.order_payment_type +
               inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA3 + LP_1week + 
               LP_2week + LP_3week + Total.Investment +
               Content.Marketing + 
               `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
               `basic_data_frame$promotion_range50to75pcDiscount` + 
               `basic_data_frame$promotion_range75to100pcDiscount`  
               , data = train)
summary(gaming_linear_final)
vif(gaming_linear_final)
#Multiple R-squared:  0.6073,	Adjusted R-squared:  0.6073 
#Multiple R-squared:  0.6074,	Adjusted R-squared:  0.6073 

#hoomeaudio df
train_index<-sample(1:nrow(homeaudio_additive_df),0.7*nrow(homeaudio_additive_df))
train<-homeaudio_additive_df[train_index,]
test<-homeaudio_additive_df[-train_index,]
lm(gmv~.,train)->homeaudio_model
step <- stepAIC(homeaudio_model, direction="both")
summary(step)

model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + LP_1week + LP_2week + 
                LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                Radio + Other + NPS + `basic_data_frame$p_tagpremium` + `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` + 
                `basic_data_frame$dayOfWeekTuesday`, data = train)
summary(model_2)
#NPS,Radio
model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + LP_1week + LP_2week + 
                LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                  `basic_data_frame$p_tagpremium` + `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` + 
                `basic_data_frame$dayOfWeekTuesday`, data = train)
summary(model_3)

model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + 
                `basic_data_frame$p_tagpremium` + `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` , 
              data = train)
summary(model_4)
model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + 
                `basic_data_frame$p_tagpremium` + `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` , 
              data = train)
summary(model_4)
#digital and content marketing
model_5 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Sponsorship + 
                 `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` , 
              data = train)
summary(model_5)

model_6 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 +  LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Sponsorship + 
                `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` , 
              data = train)
summary(model_6)
vif(model_6)
#remove content marketing high vif
homeaudio_linear_final <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA3 +  LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Sponsorship + 
                `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` , 
              data = train)
summary(homeaudio_linear_final)
#Multiple R-squared:  0.4958,	Adjusted R-squared:  0.4957 

####################################Multiplicative modelling##############################################

prepareForLinearMultiplicativeModel <- function(basic_data_frame){
#Removing the Lag and moving  varaibles
multi_df <- basic_data_frame[,-c(5:13)]
multi_df$holidayweek <- NULL
#replace 0 with 1q valu
multi_df$Content.Marketing[which(multi_df$Content.Marketing==0)]<-0.01212
multi_df$holidays[which(multi_df$holidays==0)]<-0.00001
multi_df$Radio[which(multi_df$Radio==0)]<-0.00001
multi_df$Other[which(multi_df$Other==0)]<-0.00001
multi_df$list_price[which(multi_df$list_price <= 0)]<-0.00001
multi_df <- multi_df[,-c(16:29)]
multi_df$delayed <- NULL
multi_df <- log(multi_df)
return (multi_df)
}
camera_additive_df$list_price <- camera_df$list_price
camera_additive_df$list_price <- scale(camera_additive_df$list_price )

camera_multi_df <- prepareForLinearMultiplicativeModel(camera_additive_df)
camera_multi_model <- lm(gmv~.,camera_multi_df)
summary(camera_multi_model)
camera_multi_step <- stepAIC(camera_multi_model, direction="both")
summary(camera_multi_step)

vif(camera_multi_step)
#Total.Investment has high vif and less significant
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + 
                Digital + Sponsorship + Content.Marketing + Online.marketing + 
                X.Affiliates + SEM + Radio + Other + NPS + list_price, data = camera_multi_df)
summary(model_2)
vif(model_2)  
#Online marketing
model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type +
                Digital + Sponsorship + Content.Marketing +
                X.Affiliates + SEM + Radio + Other + NPS + list_price, data = camera_multi_df)
summary(model_3)
#Remove Digital
model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type +
                 Sponsorship + Content.Marketing +
                X.Affiliates + SEM + Radio + Other + NPS + list_price, data = camera_multi_df)
summary(model_4)
vif(model_4)
model_5 <- lm(formula = gmv ~ s1_fact.order_payment_type +
                Sponsorship + Content.Marketing +
                X.Affiliates + SEM + Radio + NPS + list_price, data = camera_multi_df)
summary(model_5)
vif(model_5)
#Xaffliates
model_6 <- lm(formula = gmv ~ s1_fact.order_payment_type +
                Sponsorship + Content.Marketing +
                 SEM + Radio + NPS + list_price, data = camera_multi_df)
summary(model_6)
vif(model_6)

#NPS
multiplicative_camera_df<- lm(formula = gmv ~ s1_fact.order_payment_type +
                Sponsorship + Content.Marketing +
                SEM + Radio + list_price, data = camera_multi_df)
summary(multiplicative_camera_df)
#Multiple R-squared:  0.7282,	Adjusted R-squared:  0.7281 

#Multiplicative gaming

gaming_additive_df$list_price <- gaming_df$list_price
gaming_additive_df$list_price <- scale(gaming_additive_df$list_price )

gaming_multi_df <- prepareForLinearMultiplicativeModel(gaming_additive_df)
gaming_multi_model <- lm(gmv~.,gaming_multi_df)
summary(gaming_multi_model)
gaming_multi_step <- stepAIC(gaming_multi_model, direction="both")
summary(gaming_multi_step)

vif(gaming_multi_step)
#removing NPS, least significant and high vif
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + Digital + 
                Sponsorship + Content.Marketing + Online.marketing + X.Affiliates + 
                 list_price, data = gaming_multi_df)

summary(model_2)
#online marketing
model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + Digital + 
                Sponsorship + Content.Marketing + X.Affiliates + 
                list_price, data = gaming_multi_df)
summary(model_3)
#X-affiliates
gaming_multiplicative_final <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + Digital + 
                Sponsorship + Content.Marketing +
                list_price, data = gaming_multi_df)
summary(gaming_multiplicative_final)
vif(gaming_multiplicative_final)
#Multiple R-squared:  0.7226,	Adjusted R-squared:  0.7226 

#Multiplicative homeaudio
homeaudio_additive_df$list_price <- homeaudio_df$list_price
homeaudio_additive_df$list_price <- scale(homeaudio_additive_df$list_price )

homeaudio_multi_df <- prepareForLinearMultiplicativeModel(homeaudio_additive_df)
homeaudio_multi_model <- lm(gmv~.,homeaudio_multi_df)
summary(homeaudio_multi_model)
gaming_multi_step <- stepAIC(homeaudio_multi_model, direction="both")
summary(homeaudio_multi_model)
vif(homeaudio_multi_model)
#Multiple R-squared:  0.5928,	Adjusted R-squared:  0.5927 

#remove Other,radio
model_2 <- lm(gmv ~ s1_fact.order_payment_type + holidays + Total.Investment + 
  Digital + Sponsorship + Online.marketing + X.Affiliates + 
  SEM +  list_price,data = homeaudio_multi_df)
summary(model_2)
#OnlineMarketing
model_3 <- lm(gmv ~ s1_fact.order_payment_type + holidays + Total.Investment + 
                Digital + Sponsorship + X.Affiliates + 
                SEM +  list_price,data = homeaudio_multi_df)
summary(model_3)
#SEM
model_4 <- lm(gmv ~ s1_fact.order_payment_type + holidays + Total.Investment + 
                Digital + Sponsorship + X.Affiliates + 
                 list_price,data = homeaudio_multi_df)
summary(model_4)
vif(model_4)
#Total Investment
homeaudio_multi_final <- lm(gmv ~ s1_fact.order_payment_type + holidays +
                Digital + Sponsorship + X.Affiliates + 
                list_price,data = homeaudio_multi_df)
summary(homeaudio_multi_final)
vif(homeaudio_multi_final)
#Multiple R-squared:  0.5921,	Adjusted R-squared:  0.592 

## Koyck Model

prepareForKoyckModel <- function(basic_data_frame){
  #Removing the and moving  varaibles. Koyck model will have lag for dependent variables but not for independent varaibles
  koyck_df <- basic_data_frame[,-c(8:13)]
  koyck_df$holidayweek <- NULL
  koyck_df$list_price <- NULL
  #replace 0 with 1q value
  koyck_df <- slide(koyck_df,Var="gmv",slideBy = -1)
  koyck_df <- slide(koyck_df,Var="gmv",slideBy = -2)
  koyck_df <- slide(koyck_df,Var="gmv",slideBy = -3)
  
  koyck_df <- na.omit(koyck_df)
 
  return (koyck_df)
}
camera_koyck_df <- prepareForKoyckModel(camera_additive_df)
camera_koyck_model <- lm(gmv~.,camera_koyck_df)
summary(camera_koyck_model)
step <- stepAIC(camera_koyck_model, direction="both")
summary(step)
vif(step)

#Online marketing high vif and low p has no impact
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + Total.Investment + 
                TV + Digital + Sponsorship + Content.Marketing + 
                X.Affiliates + Radio + Other + NPS + `basic_data_frame$p_tagpremium` + 
                `basic_data_frame$p_tagmiddle` + `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` + `basic_data_frame$dayOfWeekSaturday` + 
                `basic_data_frame$dayOfWeekSunday` + `basic_data_frame$dayOfWeekWednesday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = camera_koyck_df)
summary(model_2)

model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + Total.Investment + 
                TV + Digital + Sponsorship + Content.Marketing + 
                X.Affiliates +Other + NPS + `basic_data_frame$p_tagpremium` + 
                `basic_data_frame$p_tagmiddle` +  
                 + `basic_data_frame$dayOfWeekSaturday` + 
                `basic_data_frame$dayOfWeekSunday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = camera_koyck_df)
summary(model_3)
vif(model_3)
model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
                TV + Digital + Sponsorship + Content.Marketing + 
                X.Affiliates +Other + NPS + `basic_data_frame$p_tagpremium` + 
                `basic_data_frame$p_tagmiddle` +  
                + `basic_data_frame$dayOfWeekSaturday` + 
                `basic_data_frame$dayOfWeekSunday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = camera_koyck_df)
summary(model_4)
model_5 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
                TV + Digital  + `basic_data_frame$p_tagpremium` + 
                `basic_data_frame$p_tagmiddle` +  
                + `basic_data_frame$dayOfWeekSaturday` + 
                `basic_data_frame$dayOfWeekSunday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = camera_koyck_df)
summary(model_5)

model_6 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1  + inc_LP_MA3 + 
                TV + Digital  + `basic_data_frame$p_tagpremium` + 
                `basic_data_frame$p_tagmiddle` +  
                + `basic_data_frame$dayOfWeekSaturday` + 
                `basic_data_frame$dayOfWeekSunday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = camera_koyck_df)
summary(model_6)

koyck_camera_final <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1  + inc_LP_MA3 + 
                `basic_data_frame$p_tagmiddle` +  
                + `basic_data_frame$dayOfWeekSaturday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = camera_koyck_df)
summary(koyck_camera_final)
vif(koyck_camera_final)
#Multiple R-squared:  0.4054,	Adjusted R-squared:  0.4053 
#Gamming Koyck model
gaming_koyck_df <- prepareForKoyckModel(gaming_additive_df)
gaming_koyck_model <- lm(gmv~.,gaming_koyck_df)
summary(gaming_koyck_model)
step <- stepAIC(gaming_koyck_model, direction="both")
summary(step)

model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + inc_LP_MA1 + 
                inc_LP_MA2 + inc_LP_MA3 + Total.Investment + TV + Digital + 
                Sponsorship + Content.Marketing + Online.marketing + SEM + 
                Other + NPS + `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                 `basic_data_frame$dayOfWeekSaturday` + 
                 `gmv-1` + `gmv-2` + 
                `gmv-3` , data = gaming_koyck_df)
summary(model_2)

model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + inc_LP_MA1 + 
                inc_LP_MA2 + inc_LP_MA3 + Total.Investment + TV + Digital + 
                Sponsorship + Content.Marketing + Online.marketing + SEM + 
                Other + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `gmv-1` + `gmv-2` + 
                `gmv-3` , data = gaming_koyck_df)
summary(model_3)
vif(model_3)
gaming_koyck_final <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + inc_LP_MA1 + 
                inc_LP_MA2 + inc_LP_MA3 + Total.Investment + TV + 
                Sponsorship + Content.Marketing + Online.marketing + SEM + 
                Other + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `gmv-1` + `gmv-2` + 
                `gmv-3` , data = gaming_koyck_df)
summary(gaming_koyck_final)
#Multiple R-squared:  0.5442,	Adjusted R-squared:  0.5441 

#homeaudio koyck model
haudio_koyck_df <- prepareForKoyckModel(homeaudio_additive_df)
haudio_koyck_model <- lm(gmv~.,haudio_koyck_df)
summary(haudio_koyck_model)
step <- stepAIC(haudio_koyck_model, direction="both")
summary(step)
model_2<-lm(formula = gmv ~ s1_fact.order_payment_type + delayed +  
     inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + Total.Investment + 
     TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
     X.Affiliates + SEM + Radio + Other + `basic_data_frame$p_tagmass` + 
     `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
     `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
     `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
     `basic_data_frame$promotion_range75to100pcDiscount` + `basic_data_frame$dayOfWeekMonday` +
     `gmv-1` + `gmv-2` + `gmv-3`, data = haudio_koyck_df)

homeaudio_koyck_final <-lm(formula = gmv ~ s1_fact.order_payment_type + delayed +  
              inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + Total.Investment + 
              TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
              X.Affiliates + SEM + Radio + Other + `basic_data_frame$p_tagmass` + 
              `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
              `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
              `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
              `basic_data_frame$promotion_range75to100pcDiscount` + 
              `gmv-1` + `gmv-2` + `gmv-3`, data = haudio_koyck_df)
summary(homeaudio_koyck_final)
#Multiple R-squared:  0.6104,	Adjusted R-squared:  0.6103 

#Distributed Lag model
prepareForDistributedLagModel <- function(basic_data_frame){
  # Distributed model will have lag for dependent variables and also independent varaibles
  dist_lag_df <- basic_data_frame
  dist_lag_df$holidayweek <- NULL
  dist_lag_df$list_price <-NULL
  #replace 0 with 1q value
  dist_lag_df <- slide(dist_lag_df,Var="gmv",slideBy = -1)
  dist_lag_df <- slide(dist_lag_df,Var="gmv",slideBy = -2)
  dist_lag_df <- slide(dist_lag_df,Var="gmv",slideBy = -3)
  
  dist_lag_df <- na.omit(dist_lag_df)
  
  return (dist_lag_df)
}
camera_dist_lag_df <- prepareForDistributedLagModel(camera_additive_df)
camera_dist_lag_model <- lm(gmv~.,camera_dist_lag_df)
summary(camera_dist_lag_model)
step <- stepAIC(camera_dist_lag_model, direction="both")
summary(step)
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + 
                inc_LP_MA2 + inc_LP_MA3 + LP_1week + LP_2week + 
                LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                 Other + `basic_data_frame$p_tagpremium` + `basic_data_frame$p_tagmiddle` + 
                `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` + `gmv-1` + `gmv-2` + 
                `gmv-3`, data = camera_dist_lag_df)
summary(model_2)
vif(model_2)
model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + 
                inc_LP_MA2 + inc_LP_MA3 + LP_1week + LP_2week + 
                LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                Other + `basic_data_frame$p_tagpremium` + `basic_data_frame$p_tagmiddle` +
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` + `gmv-1` + `gmv-2` + 
                `gmv-3`, data = camera_dist_lag_df)
summary(model_3)

model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + 
                inc_LP_MA2 + inc_LP_MA3 + LP_1week + LP_2week + 
                LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                Other + `basic_data_frame$p_tagpremium` + `basic_data_frame$p_tagmiddle` +
                `gmv-1` + `gmv-2` + 
                `gmv-3`, data = camera_dist_lag_df)
summary(model_4)
#Total Investemnt
model_5 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + 
                inc_LP_MA2 + inc_LP_MA3 + LP_1week + LP_2week + 
                LP_3week + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                Other + `basic_data_frame$p_tagpremium` + `basic_data_frame$p_tagmiddle` +
                `gmv-1` + `gmv-2` + 
                `gmv-3`, data = camera_dist_lag_df)
summary(model_5)

camera_distributed_final <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + 
                inc_LP_MA2 + inc_LP_MA3 + LP_1week + LP_2week + 
                LP_3week + TV + Digital + Sponsorship + 
                Content.Marketing +  X.Affiliates + SEM + 
                Other + `basic_data_frame$p_tagpremium` + `basic_data_frame$p_tagmiddle` +
                `gmv-1` + `gmv-2` + 
                `gmv-3`, data = camera_dist_lag_df)
summary(camera_distributed_final)
#Multiple R-squared:  0.7894,	Adjusted R-squared:  0.727

gaming_dist_lag_df <- prepareForDistributedLagModel(gaming_additive_df)
gaming_dist_lag_model <- lm(gmv~.,gaming_dist_lag_df)
summary(gaming_dist_lag_model)
step <- stepAIC(gaming_dist_lag_model, direction="both")
summary(step)
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                Radio + Other + NPS + `basic_data_frame$deliveryPerceptiontolerableDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + `basic_data_frame$promotion_range75to100pcDiscount` + 
                 `basic_data_frame$dayOfWeekTuesday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = gaming_dist_lag_df)
summary(model_2)

model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Digital + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                Radio + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + `basic_data_frame$promotion_range75to100pcDiscount` + 
                `basic_data_frame$dayOfWeekTuesday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = gaming_dist_lag_df)
summary(model_3)

model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV  + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + SEM + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + `basic_data_frame$promotion_range75to100pcDiscount` + 
                `basic_data_frame$dayOfWeekTuesday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = gaming_dist_lag_df)
summary(model_4)

model_5 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV  + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + `basic_data_frame$promotion_range75to100pcDiscount` + 
                `basic_data_frame$dayOfWeekTuesday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = gaming_dist_lag_df)
summary(model_5)

model_6 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed +  
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV  + Sponsorship + 
                Content.Marketing + Online.marketing + X.Affiliates + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + `basic_data_frame$promotion_range75to100pcDiscount` + 
                `basic_data_frame$dayOfWeekTuesday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = gaming_dist_lag_df)
summary(model_6)

model_7 <- lm(formula = gmv ~ 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV  + Sponsorship + 
                Content.Marketing + Online.marketing + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range50to75pcDiscount` + `basic_data_frame$promotion_range75to100pcDiscount` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = gaming_dist_lag_df)
summary(model_7)

#Homeauidio- distributed
haudio_dist_lag_df <- prepareForDistributedLagModel(homeaudio_additive_df)
haudio_dist_lag_model <- lm(gmv~.,haudio_dist_lag_df)
summary(haudio_dist_lag_model)
step <- stepAIC(haudio_dist_lag_model, direction="both")
summary(step)
vif(step)
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + LP_1week + 
                LP_2week + LP_3week  +  Digital + Sponsorship + 
                `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$promotion_rangelessThan25pcDiscount` + 
                `basic_data_frame$promotion_range25to50pcDiscount` + `basic_data_frame$promotion_range50to75pcDiscount` + 
                `basic_data_frame$promotion_range75to100pcDiscount` +
                  `basic_data_frame$dayOfWeekWednesday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = haudio_dist_lag_df)
summary(model_2)
vif(model_2)

homeaudio_distributed_final <- lm(formula = gmv ~ s1_fact.order_payment_type + delayed + holidays + 
                inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + LP_1week + 
                LP_2week + LP_3week  +  Digital + Sponsorship + 
                 `basic_data_frame$deliveryPerceptiondelayedDeliveryPerception` + 
                `basic_data_frame$dayOfWeekWednesday` + 
                `gmv-1` + `gmv-2` + `gmv-3`, data = haudio_dist_lag_df)
summary(homeaudio_distributed_final)
#Multiple R-squared:  0.6857,	Adjusted R-squared:  0.6656 


prepareForMultiplicativeDistributedLagModel <- function(dist_lag_data_frame){
  #Removing  moving  varaibles
  multi_df <- dist_lag_data_frame[,-c(5:10)]
  multi_df <- multi_df[,-c(19:32)]
  multi_df$list_price <- NULL
  
  multi_df$LP_1week <- multi_df$LP_1week +1 - min(multi_df$LP_1week)
  multi_df$LP_2week <- multi_df$LP_2week +1 - min(multi_df$LP_2week)
  multi_df$LP_3week <- multi_df$LP_3week +1 - min(multi_df$LP_3week)
 
  #replace 0 with 1q valu
  multi_df$Content.Marketing[which(multi_df$Content.Marketing==0)]<-0.0121
  multi_df$holidays[which(multi_df$holidays==0)]<-0.00001
  multi_df$delayed <- NULL
  multi_df$Other <-NULL
  multi_df$Radio <- NULL
  
  multi_df <- slide(multi_df,Var="gmv",slideBy = -1)
  multi_df <- slide(multi_df,Var="gmv",slideBy = -2)
  multi_df <- slide(multi_df,Var="gmv",slideBy = -3)
  multi_df <- na.omit(multi_df)
  
  
  multi_df <- slide(multi_df,Var="discountOffered",slideBy = -1)
  multi_df <- slide(multi_df,Var="discountOffered",slideBy = -2)
  multi_df <- slide(multi_df,Var="discountOffered",slideBy = -3)
  multi_df <- na.omit(multi_df)
  multi_df$`discountOffered-1` <- multi_df$`discountOffered-1` +1 - min(multi_df$`discountOffered-1`)
  multi_df$`discountOffered-2` <- multi_df$`discountOffered-2` +1 - min(multi_df$`discountOffered-2`)
  multi_df$`discountOffered-3` <- multi_df$`discountOffered-3` +1 - min(multi_df$`discountOffered-3`)
  
  multi_df$discountOffered <-NULL
  multi_df <- na.omit(multi_df)
  multi_df <- log(multi_df)
  return (multi_df)
}

#Multiplicative distributed model
camera_additive_df$discountOffered <- scale(camera_df$discount_offered)
camera_multidist_lag_df <- prepareForMultiplicativeDistributedLagModel(camera_additive_df)
camera_multidist_lag_model <- lm(gmv~.,camera_multidist_lag_df)
summary(camera_multidist_lag_model)
#Multiple R-squared:  0.8954,	Adjusted R-squared:  0.8954 
step <- stepAIC(camera_multidist_lag_model, direction="both")
summary(step)
vif(step)
#total INvestemtn has high vif
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + LP_1week + 
     LP_2week + LP_3week + TV + Digital + Sponsorship + 
     Content.Marketing + Online.marketing + X.Affiliates + SEM + 
     NPS + `gmv-1` + `gmv-2` + `gmv-3` + `discountOffered-1` + 
     `discountOffered-2` + `discountOffered-3`, data = camera_multidist_lag_df)
summary(model_2)
#X-Affiliate high vif has less P values now.
model_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + LP_1week + 
                LP_2week + LP_3week +TV+ Digital + Sponsorship + 
                Content.Marketing + Online.marketing +  SEM + 
                NPS + `gmv-1` + `gmv-2` + `gmv-3` + `discountOffered-1` + 
                `discountOffered-2` + `discountOffered-3`, data = camera_multidist_lag_df)
summary(model_3)
vif(model_3)
#Remove digital
model_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + LP_1week + 
                LP_2week + LP_3week +TV + Sponsorship + 
                Content.Marketing + Online.marketing +  SEM + 
                NPS + `gmv-1` + `gmv-2` + `gmv-3` + `discountOffered-1` + 
                `discountOffered-2` + `discountOffered-3`, data = camera_multidist_lag_df)
summary(model_4)
vif(model_4)
camera_multidist_final <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + LP_1week + 
                LP_2week + LP_3week +TV + Sponsorship + 
                 Online.marketing + 
                NPS + `gmv-1` + `gmv-2` + `gmv-3` + `discountOffered-1` + 
                `discountOffered-2` + `discountOffered-3`, data = camera_multidist_lag_df)
summary(camera_multidist_final)
vif(camera_multidist_final)
#Multiple R-squared:  0.8953,	Adjusted R-squared:  0.8952 

gaming_additive_df$discountOffered <- scale(gaming_df$discount_offered)
gaming_mutlidist_lag_df <- prepareForMultiplicativeDistributedLagModel(gaming_additive_df)
gaming_multidist_lag_model <- lm(gmv~.,gaming_mutlidist_lag_df)
summary(gaming_multidist_lag_model)
#Multiple R-squared:  0.9057,	Adjusted R-squared:  0.9056 
step <- stepAIC(gaming_multidist_lag_model, direction="both")
summary(step)
vif(step)
#remove total investement
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + LP_1week + LP_2week + 
     LP_3week + TV + Digital + Sponsorship + 
     Online.marketing + SEM + NPS + `gmv-1` + `gmv-2` + 
     `gmv-3` + `discountOffered-1` + `discountOffered-2` + `discountOffered-3`, 
   data = gaming_mutlidist_lag_df)
summary(model_2)
#remove SEM with high vif
gaming_multidis_final <- lm(formula = gmv ~ s1_fact.order_payment_type + LP_1week + LP_2week + 
                LP_3week + TV + Digital + Sponsorship + 
                Online.marketing + NPS + `gmv-1` + `gmv-2` + 
                `gmv-3` + `discountOffered-1` + `discountOffered-2` + `discountOffered-3`, 
              data = gaming_mutlidist_lag_df)
summary(gaming_multidis_final)

#homeaudio
homeaudio_additive_df$discountOffered <- scale(homeaudio_df$discount_offered)

haudio_multidist_lag_df <- prepareForMultiplicativeDistributedLagModel(homeaudio_additive_df)
haudio_multidist_lag_model <- lm(gmv~.,haudio_multidist_lag_df)
summary(haudio_multidist_lag_model)
step <- stepAIC(haudio_multidist_lag_model, direction="both")
summary(step)
#Multiple R-squared:  0.9115,	Adjusted R-squared:  0.9115 
vif(step)
#remove online marketing with high vif
model_2 <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + LP_1week + 
     LP_2week + LP_3week + Total.Investment + TV + Digital + Content.Marketing + 
     X.Affiliates + SEM + NPS + `gmv-1` + `gmv-2` + 
     `gmv-3` + `discountOffered-1` + `discountOffered-2` + `discountOffered-3`, 
   data = haudio_multidist_lag_df)
summary(model_2)
vif(model_2)
#remove x-affiliate,SEM
model_3<- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + LP_1week + 
                LP_2week + LP_3week + Total.Investment + TV + Digital + Content.Marketing + 
                `gmv-1` + `gmv-2` + 
                `gmv-3` + `discountOffered-1` + `discountOffered-2` + `discountOffered-3`, 
              data = haudio_multidist_lag_df)
summary(model_3)
#total investment
model_4<- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + LP_1week + 
               LP_2week + LP_3week + Digital + Content.Marketing + 
               `gmv-1` + `gmv-2` + 
               `gmv-3` + `discountOffered-1` + `discountOffered-2` + `discountOffered-3`, 
             data = haudio_multidist_lag_df)
summary(model_4)
vif(model_4)

homeaudio_multidist_final <- lm(formula = gmv ~ s1_fact.order_payment_type + holidays + LP_1week + 
               LP_2week + LP_3week + Content.Marketing + 
               `gmv-1` + `gmv-2` + 
               `gmv-3` +  `discountOffered-2` + `discountOffered-3`, 
             data = haudio_multidist_lag_df)
summary(homeaudio_multidist_final)

#Multiple R-squared:  0.9112,	Adjusted R-squared:  0.902 


