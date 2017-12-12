
rm(list = ls(all =T))
#code to take train data from prod 
library(RODBC)
#install.packages("H:/kukrebh/d backup/R Packages/R Packages/lazyeval_0.2.0.zip", repos = NULL, type = "win.binary")
library(dplyr)
library(cluster)
library(ggplot2)
library(Rtsne)
library(klaR)
library(lazyeval)
library(openssl)
dbhandle <- odbcDriverConnect("dsn=AJ_prod_latest;uid=v683623;pwd=pokemon16;")
# break into 2 data frames
closed_op<- sqlQuery(dbhandle, 'SELECT * FROM SND_OFFSHORE_ANALYTICS.FUNNEL_CLOSED_BASE_SNAP2017NOV')
pending_op <- sqlQuery(dbhandle, 'SELECT * FROM SND_OFFSHORE_ANALYTICS.FUNNEL_PENDING_DELTA_SNAP2017NOV')
#alldata_perm <- sqlQuery(dbhandle, 'SELECT * FROM SND_OFFSHORE_ANALYTICS.FUNNEL_CLOSED_BASE_SNAP2017NOV UNION SELECT * FROM SND_OFFSHORE_ANALYTICS.FUNNEL_PENDING_DELTA_SNAP2017NOV')
 closed_op <- as.data.frame(closed_op)
pending_op <- as.data.frame(pending_op)
alldata_perm <- rbind(closed_op, pending_op)
alldata_perm <- as.data.frame(alldata_perm)
# apac non apac diff data frame
names(alldata_perm)
#unique(alldata_perm$branch_geo_region)
# 6 masked levels

as.character(md5("APAC"))==tolower(as.character('A2F89A300F983190022374848C4353B0'))
# A2F89A300F983190022374848C4353B0 which is level 5 in 6 levels 

# [1] 0F1C6D45B761226679E0927CC47D24D3 (EMEA) 7516FD43ADAA5E0B8A65A672C39845D2(US)
# [3] D41D8CD98F00B204E9800998ECF8427E CF11DA476A9FC9481D298A824F805638
# [5] A2F89A300F983190022374848C4353B0 (APAC) 991D48815C3A68294305AAF78E0BDEEB

  
alldata<- alldata_perm[,c('OPPID','DATE_OPPORTUNITY_CLOSED','CLOSE_MONTH','TERM','PRIMARY_COUNTRY','NUM_OF_PRODUCTS',
                          'NASP_ID','VERTICAL_RPT_DESC2','nasp_type','OPPTYPE','COMPETITOR','CSM_TIER_CODE',
                          'PUSH_COUNTER', 'AGE', 'username', 'forecast_category', 'BOOKING','EXP_SPA_DATE',
                          'STAGE_0_DURATION__C','STAGE_1_DURATION__C','STAGE_2_DURATION__C','STAGE_3_DURATION__C',
                          'STAGE_4_DURATION__C','STAGE_5_DURATION__C','STAGENAME','WON','SITECOUNT',
                          'STAGE_0_TIME__C','STAGE_1_TIME__C','STAGE_2_TIME__C','STAGE_3_TIME__C',
                          'STAGE_4_TIME__C','STAGE_5_TIME__C','branch_geo_region','PRODUCT_FAMILY_PR4','TOTAL_STAGE_DURATION__C')]

alldata <- as.data.frame(alldata)

alldata_apac <- subset( alldata, branch_geo_region == toupper(as.character(md5("APAC")))) 
#View(alldata_perm_non_apac)                                         
alldata_non_apac<- subset( alldata, branch_geo_region != toupper(as.character(md5("APAC"))))
 
## ------------------for APAC------------------------- ##
#Data cleaning
#Adding model indicator 
alldata_apac$MODEL_IND <-'';
alldata_apac = alldata_apac %>% mutate(MODEL_IND = ifelse(TOTAL_STAGE_DURATION__C > 90, 'L', MODEL_IND))
alldata_apac = alldata_apac %>% mutate(MODEL_IND = ifelse(TOTAL_STAGE_DURATION__C < 46, 'S', MODEL_IND))
alldata_apac = alldata_apac %>% mutate(MODEL_IND = ifelse((TOTAL_STAGE_DURATION__C > 45)&(TOTAL_STAGE_DURATION__C < 91), 'M', MODEL_IND))

#Opptype update
alldata_apac$OPPTYPE <- as.character(alldata_apac$OPPTYPE)
alldata_apac$OPPTYPE[alldata_apac$OPPTYPE %in% c('Add to Existing/Upgrade','Change - Other','Upgrade - Service',
                                                 'Conversion/Migration','Upgrade - Same Service',
                                                 'Downgrade - Service','Move','Upsell - Cross Sell','Contract Only')] = 'CHANGE'
alldata_apac$OPPTYPE[alldata_apac$OPPTYPE %in% c('New Business','Existing Business','TOSA')]='EXISTING'
alldata_apac$OPPTYPE[alldata_apac$OPPTYPE %in% c('Renewal Only','Renewal')]='RENEW'
alldata_apac = alldata_apac %>% mutate(OPPTYPE = ifelse(OPPTYPE =='New Customer','NEW',OPPTYPE))
alldata_apac$OPPTYPE <- as.factor(alldata_apac$OPPTYPE)

# UPDATING THE NEW CSM TIERS
alldata_apac$CSM_TIER_CODE[alldata_apac$CSM_TIER_CODE %in% c('CST-1','CST-2')]='CSM-3'
alldata_apac = alldata_apac %>% mutate(CSM_TIER_CODE = ifelse(CSM_TIER_CODE =='Field-1','CSM-1',CSM_TIER_CODE))
alldata_apac = alldata_apac %>% mutate(CSM_TIER_CODE = ifelse(CSM_TIER_CODE =='Field-2','CSM-2',CSM_TIER_CODE))

#Stalled stage update
alldata_apac$STALLED_STAGE <- as.numeric(NA)
alldata_apac$STAGE_0_TIME__C <- as.numeric(alldata_apac$STAGE_0_TIME__C)

alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_0_TIME__C >= 90)&(MODEL_IND == 'L')),0,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_1_TIME__C >= 90)&(MODEL_IND == 'L')),1,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_2_TIME__C >= 90)&(MODEL_IND == 'L')),2,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_3_TIME__C >= 120)&(MODEL_IND == 'L')),3,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND == 'L')),4,STALLED_STAGE))

alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_0_TIME__C >= 30)&(MODEL_IND == 'S')),0,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_1_TIME__C >= 27)&(MODEL_IND =='S')),1,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_2_TIME__C >= 25)&(MODEL_IND == 'S')),2,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_3_TIME__C >= 35)&(MODEL_IND == 'S')),3,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND =='S')),4,STALLED_STAGE))

alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_0_TIME__C >= 40)&(MODEL_IND == 'M')),0,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_1_TIME__C >= 40)&(MODEL_IND == 'M')),1,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_2_TIME__C >= 40)&(MODEL_IND == 'M')),2,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_3_TIME__C >= 60)&(MODEL_IND == 'M')),3,STALLED_STAGE))
alldata_apac = alldata_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND =='M')),4,STALLED_STAGE))

#Vertical update 
alldata_apac$VERTICAL_RPT_DESC2 <- as.character(alldata_apac$VERTICAL_RPT_DESC2)
alldata_apac$VERTICAL_RPT_DESC2 <-gsub( "/.*$", "", alldata_apac$VERTICAL_RPT_DESC2 )
alldata_apac$VERTICAL_RPT_DESC2 <- as.factor(alldata_apac$VERTICAL_RPT_DESC2)


#number of stalls 
alldata_apac$NUM_STALLS <- 0

alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_0_TIME__C >= 90)&(MODEL_IND == 'L')),0,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_1_TIME__C >= 90)&(MODEL_IND == 'L')),1,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_2_TIME__C >= 90)&(MODEL_IND == 'L')),2,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_3_TIME__C >= 120)&(MODEL_IND == 'L')),3,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND == 'L')),4,NUM_STALLS +1))

alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND == 'S')),4,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_3_TIME__C >= 35)&(MODEL_IND == 'S')),3,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_2_TIME__C >= 25)&(MODEL_IND =='S')),2,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_1_TIME__C >= 27)&(MODEL_IND =='S')),1,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_0_TIME__C >= 30)&(MODEL_IND =='S')),0,NUM_STALLS +1))

alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND == 'M')),4,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_3_TIME__C >= 60)&(MODEL_IND == 'M')),3,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_2_TIME__C >= 40)&(MODEL_IND == 'M')),2,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_1_TIME__C >= 40)&(MODEL_IND == 'M')),1,NUM_STALLS +1))
alldata_apac = alldata_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_0_TIME__C >= 40)&(MODEL_IND == 'M')),0,NUM_STALLS +1))

#adding winrates later 
#agg_stage_stall <- aggregate(WON~STALLED_STAGE, data=alldata_apac, FUN=mean)
#names(agg_stage_stall)[names(agg_stage_stall)=="WON"] <- "STALLED_WIN_RATE"
#alldata_apac <- merge(alldata_apac, agg_stage_stall, by="STALLED_STAGE", all.x=TRUE)

#adding stage conversion factors
alldata_apac$STAGENAME <- as.character(alldata_apac$STAGENAME)
alldata_apac$LAST_STAGE <- as.numeric(0)
alldata_apac = alldata_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_0_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),0,LAST_STAGE))
alldata_apac = alldata_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_1_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),1,LAST_STAGE))
alldata_apac = alldata_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_2_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),2,LAST_STAGE))
alldata_apac = alldata_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_3_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),3,LAST_STAGE))
alldata_apac = alldata_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_4_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),4,LAST_STAGE))
alldata_apac$STAGENAME <- as.factor(alldata_apac$STAGENAME)


#Calculating total won oppids and total oppids
alldata_apac_temp <- alldata_apac[alldata_apac$STAGENAME =='5 Closed Won',]
COUNT_WON <-length(unique(alldata_apac_temp$OPPID))
rm('alldata_apac_temp')
COUNT_TOTAL <- length(unique(alldata_apac$OPPID))
alldata_apac$COUNT_WON <- as.numeric(COUNT_WON)
alldata_apac$COUNT_TOTAL <- as.numeric(COUNT_TOTAL)


#lost + disqualified wrt last stage calculation
agg_COUNT_LOST_DISQ <- aggregate(OPPID~LAST_STAGE, data=alldata_apac[alldata_apac$STAGENAME=="5 Closed Disqualified" | alldata_apac$STAGENAME == '5 Closed Lost',], FUN=NROW)
i <- as.numeric(0)
agg_COUNT_LOST_DISQ <- as.data.frame(agg_COUNT_LOST_DISQ)
for (i in (0:4))
{
  x <-paste('COUNT_LOST_DISQ',i, sep="") 
  assign(paste('COUNT_LOST_DISQ',i, sep=""),as.numeric(agg_COUNT_LOST_DISQ[[2]][i+1]))
  b <- mget(x);
  alldata_apac <- cbind(alldata_apac, b);
}

#STAGE_CONVERSION_P Calculation
alldata_apac$STAGE_CONVERSION_P <- as.numeric(0)
alldata_apac = alldata_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 0),(COUNT_WON/COUNT_TOTAL),STAGE_CONVERSION_P))
alldata_apac = alldata_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 1),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0)),STAGE_CONVERSION_P))
alldata_apac = alldata_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 2),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0-COUNT_LOST_DISQ1)),STAGE_CONVERSION_P))
alldata_apac = alldata_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 3),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0-COUNT_LOST_DISQ1-COUNT_LOST_DISQ2)),STAGE_CONVERSION_P))
alldata_apac = alldata_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 4),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0-COUNT_LOST_DISQ1-COUNT_LOST_DISQ2-COUNT_LOST_DISQ3)),STAGE_CONVERSION_P))
alldata_apac = alldata_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 5),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0-COUNT_LOST_DISQ1-COUNT_LOST_DISQ2-COUNT_LOST_DISQ3-COUNT_LOST_DISQ4)),STAGE_CONVERSION_P))


#Dropping unnecessary columns Not correct 
for (i in (0:4))
{
  x <-paste('COUNT_LOST_DISQ',i, sep="")
  b <- mget(x)
  b <-names(b)
  remove(list=c(b))
  alldata_apac[,x] <- NULL
}    
rm('x');
rm('i');
rm('b');
rm('COUNT_TOTAL')
rm('COUNT_WON')

#Stuffing products and grouping by oppid 
PROD_GRP<- alldata_apac[,c('OPPID','PRODUCT_FAMILY_PR4')]
PROD_GRP<- as.data.frame(PROD_GRP)
PROD_GRP <- PROD_GRP[order(PROD_GRP$OPPID, PROD_GRP$PRODUCT_FAMILY_PR4),]
PROD_GRP <- unique(PROD_GRP)
PROD_GRP <- aggregate(PRODUCT_FAMILY_PR4~OPPID,data=PROD_GRP,paste, collapse="|")
names(PROD_GRP)[names(PROD_GRP)=="PRODUCT_FAMILY_PR4"] <- "PRODUCTS"
alldata_apac <- merge(alldata_apac, PROD_GRP, by="OPPID", all.x=TRUE)


#AGGREGATING THE BOOKINGS/OPPORTUNITY
#CLOSE_YEAR

CSM_TIER_CODE_AGGR<- alldata_apac[,c('OPPID','CSM_TIER_CODE')]
CSM_TIER_CODE_AGGR<- as.data.frame(CSM_TIER_CODE_AGGR)
CSM_TIER_CODE_AGGR <- aggregate(CSM_TIER_CODE~OPPID,data=CSM_TIER_CODE_AGGR,min,na.rm=TRUE)
names(CSM_TIER_CODE_AGGR)[names(CSM_TIER_CODE_AGGR)=="CSM_TIER_CODE"] <- "CSM_TIER_CODE_AGGR"
alldata_apac <- merge(alldata_apac, CSM_TIER_CODE_AGGR, by="OPPID", all.x=TRUE)

BOOKING_AGGR<- alldata_apac[,c('OPPID','BOOKING')]
BOOKING_AGGR<- as.data.frame(BOOKING_AGGR)
BOOKING_AGGR <- aggregate(BOOKING~OPPID,data=BOOKING_AGGR,sum, na.rm=TRUE)
names(BOOKING_AGGR)[names(BOOKING_AGGR)=="BOOKING"] <- "BOOKING_AGGR"
alldata_apac <- merge(alldata_apac, BOOKING_AGGR, by="OPPID", all.x=TRUE)

SITECOUNT_AGGR<- alldata_apac[,c('OPPID','SITECOUNT')]
SITECOUNT_AGGR<- as.data.frame(SITECOUNT_AGGR)
SITECOUNT_AGGR <- aggregate(SITECOUNT~OPPID,data=SITECOUNT_AGGR,sum,na.rm=TRUE)
names(SITECOUNT_AGGR)[names(SITECOUNT_AGGR)=="SITECOUNT"] <- "SITECOUNT_AGGR"
alldata_apac <- merge(alldata_apac, SITECOUNT_AGGR, by="OPPID", all.x=TRUE)


#Dropping unnecessary columns and keeping only aggregated view
alldata_apac[,'CSM_TIER_CODE']<-NULL
alldata_apac[,'BOOKING']<-NULL
alldata_apac[,'SITECOUNT']<-NULL
alldata_apac[,'PRODUCT_FAMILY_PR4']<- NULL
alldata_apac[,'COUNT_TOTAL']<- NULL 
alldata_apac[,'COUNT_WON']<- NULL 
alldata_apac <- unique(alldata_apac)
rm(BOOKING_AGGR)
rm(CSM_TIER_CODE_AGGR)
rm(PROD_GRP)
rm(SITECOUNT_AGGR)

#Renaming few columns 
names(alldata_apac)[names(alldata_apac)=="CSM_TIER_CODE_AGGR"] <- "CSM_TIER_CODE"
names(alldata_apac)[names(alldata_apac)=="BOOKING_AGGR"] <- "BOOKING"
names(alldata_apac)[names(alldata_apac)=="SITECOUNT_AGGR"] <- "SITECOUNT"

# taking backup for column feed in table
alldata_apac_back<-alldata_apac
alldata_apac_back <- unique(alldata_apac_back)

#Adding Close year 
alldata_apac$CLOSE_YEAR <- format(as.Date(alldata_apac$DATE_OPPORTUNITY_CLOSED, format="%d/%m/%Y"),"%Y")
alldata_apac[,'DATE_OPPORTUNITY_CLOSED']<-NULL

#length(unique(alldata_apac_perm$OPPID)) discrepency of 161105 - 160884
#write.csv(alldata_apac, file = "C:\\Users\\v683589\\Desktop\\SQL MODEL\\alldata_apac.csv", na="", row.names = FALSE)
#checking required some 200 rows extra 

################Clustering with just PUSH_COUNTER########################

PUSH_COUNTER_DATA<-alldata_apac[,c('PUSH_COUNTER','WON')]
PUSH_COUNTER_DATA <- as.data.frame(PUSH_COUNTER_DATA)
#adding winrates
agg_PUSH_COUNTER <- aggregate(WON~PUSH_COUNTER, data=PUSH_COUNTER_DATA, FUN=mean)
names(agg_PUSH_COUNTER)[names(agg_PUSH_COUNTER)=="WON"] <- "PUSH_COUNTER_WIN_RATE"
PUSH_COUNTER_DATA <- merge(PUSH_COUNTER_DATA, agg_PUSH_COUNTER, by="PUSH_COUNTER", all.x=TRUE)
PUSH_COUNTER_DATA$WON <- NULL 

#Identify ideal number of clusters to be created
wss <- (length(PUSH_COUNTER_DATA)-1)*sum(apply(PUSH_COUNTER_DATA,2,var))
for (i in 1:7) wss[i] <- sum(kmeans(na.omit(PUSH_COUNTER_DATA), centers=i)$withinss)
#plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
for (i in 1:7) 
{
  if (wss[i] == min(wss))
    num_clusters = i
}

#Fit the clustering model through K means
fit<-kmeans(na.omit(PUSH_COUNTER_DATA), as.numeric(num_clusters))

#Check details of the cluster created
#fit
#fit$size
#Check strength of clustering: BSS should be higher and WSS should be lower
#strength<-fit$betweenss/fit$withinss
#strength
#cluster<-fit$cluster
#R Square: Total variance explained by the clustering exercise
#fit$betweenss/fit$totss

#Profiling the clusters and putting it into the table
cluster<-fit$cluster
PUSH_COUNTER_DATA3<-cbind(na.omit(PUSH_COUNTER_DATA), cluster)
PUSH_COUNTER_DATA3 <- as.data.frame(PUSH_COUNTER_DATA3)
PUSH_COUNTER_DATA3$cluster<-as.factor(PUSH_COUNTER_DATA3$cluster)

#To merge Push counter with data  
PUSH_COUNTER_DATA3 <- unique(PUSH_COUNTER_DATA3)
PUSH_COUNTER_DATA3$PUSH_COUNTER_WIN_RATE <- NULL 
alldata_apac <- merge(alldata_apac,PUSH_COUNTER_DATA3 ,by="PUSH_COUNTER", all.x=TRUE)
names(alldata_apac)[names(alldata_apac)=="cluster"] <- "PUSH_COUNTER_CAT"
rm(PUSH_COUNTER_DATA)
rm(PUSH_COUNTER_DATA3)
rm(agg_PUSH_COUNTER)

################Clustering with just SITE_COUNT ########################
SITE_COUNT_DATA<-alldata_apac[,c('SITECOUNT','WON')]
SITE_COUNT_DATA <- as.data.frame(SITE_COUNT_DATA)
SITE_COUNT_DATA<-SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITECOUNT) == TRUE,]
SITE_COUNT_DATA2 <- SITE_COUNT_DATA[((SITE_COUNT_DATA$SITECOUNT >3)),]
a <- boxplot(SITE_COUNT_DATA2$SITECOUNT, data=SITE_COUNT_DATA2)$stats[c(5),1]
SITE_COUNT_DATA[(SITE_COUNT_DATA$SITECOUNT > a),]$SITECOUNT <- a

#adding winrates
agg_SITE_COUNT <- aggregate(WON~SITECOUNT, data=SITE_COUNT_DATA, FUN=mean)
names(agg_SITE_COUNT)[names(agg_SITE_COUNT)=="WON"] <- "SITE_COUNT_WIN_RATE"
SITE_COUNT_DATA <- merge(SITE_COUNT_DATA, agg_SITE_COUNT, by="SITECOUNT", all.x=TRUE)
SITE_COUNT_DATA$WON <- NULL 
SITE_COUNT_DATA <- SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITECOUNT) == TRUE,]
SITE_COUNT_DATA <- SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITE_COUNT_WIN_RATE) == TRUE,]
#nrow(SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITE_COUNT_WIN_RATE) == FALSE,])

#Identify ideal number of clusters to be created
wss <- (length(SITE_COUNT_DATA)-1)*sum(apply(SITE_COUNT_DATA,2,var))
for (i in 1:5) wss[i] <- sum(kmeans(SITE_COUNT_DATA, centers=i)$withinss)
for (i in 1:5) 
{
  if (wss[i] == min(wss))
    num_clusters = i
}

#Fit the clustering model through K means
fit<-kmeans(SITE_COUNT_DATA, as.numeric(num_clusters))

#Profiling the clusters and putting it into the table
cluster<-fit$cluster
SITE_COUNT_DATA3<-cbind(SITE_COUNT_DATA, cluster)
SITE_COUNT_DATA3 <- as.data.frame(SITE_COUNT_DATA3)
SITE_COUNT_DATA3$cluster<-as.factor(SITE_COUNT_DATA3$cluster)

#To merge SITE COUNT with data  
SITE_COUNT_DATA3 <- unique(SITE_COUNT_DATA3)
SITE_COUNT_DATA3$SITE_COUNT_WIN_RATE <- NULL 
alldata_apac[((alldata_apac$SITECOUNT > a)& (is.na(alldata_apac$SITECOUNT)== FALSE)),]$SITECOUNT <- a
alldata_apac <- merge(alldata_apac,SITE_COUNT_DATA3 ,by="SITECOUNT", all.x=TRUE)
names(alldata_apac)[names(alldata_apac)=="cluster"] <- "SITE_COUNT_CAT"

#table(alldata_apac[!is.na(alldata_apac$SITECOUNT) == FALSE,]$SITE_COUNT_CAT)
min_limit <- numeric()
max_limit <- numeric()
for(i in sort(unique(alldata_apac$SITE_COUNT_CAT)))
{
  min_limit[i] <- min(alldata_apac$SITECOUNT[alldata_apac$SITE_COUNT_CAT == i], na.rm = TRUE)
  max_limit[i] <- max(alldata_apac$SITECOUNT[alldata_apac$SITE_COUNT_CAT == i], na.rm = TRUE)
  if(length(alldata_apac[alldata_apac$SITECOUNT>=min_limit[i] & alldata_apac$SITECOUNT<=max_limit[i] & 
                         (!is.na(alldata_apac$SITE_COUNT_CAT) == FALSE) & (!is.na(alldata_apac$SITECOUNT) == TRUE),]$SITECOUNT ) > 0)
  {
    alldata_apac[alldata_apac$SITECOUNT >= min_limit[i] & alldata_apac$SITECOUNT <= max_limit[i] & (!is.na(alldata_apac$SITE_COUNT_CAT) == FALSE) & (!is.na(alldata_apac$SITECOUNT) == TRUE),]$SITE_COUNT_CAT <- i 
  }
}

if (length(alldata_apac[(!is.na(alldata_apac$SITE_COUNT_CAT) == FALSE) & (!is.na(alldata_apac$SITECOUNT) == TRUE),]$SITECOUNT) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$SITE_COUNT_CAT) == FALSE) & (!is.na(alldata_apac$SITECOUNT) == TRUE),]$SITE_COUNT_CAT <- alldata_apac[alldata_apac$SITECOUNT == max(alldata_apac$SITECOUNT, na.rm= TRUE),]$SITE_COUNT_CAT
}
rm(SITE_COUNT_DATA)
rm(SITE_COUNT_DATA2)
rm(SITE_COUNT_DATA3)
rm(agg_SITE_COUNT)

################Clustering with just BOOKING########################
BOOKING_DATA<-alldata_apac[,c('BOOKING','WON')]
BOOKING_DATA <- as.data.frame(BOOKING_DATA)
BOOKING_DATA<-BOOKING_DATA[!is.na(BOOKING_DATA$BOOKING) == TRUE,]
BOOKING_DATA2 <- BOOKING_DATA[((BOOKING_DATA$BOOKING >0)),]
a <- boxplot(BOOKING_DATA2$BOOKING, data=BOOKING_DATA2)$stats[c(5),1]
BOOKING_DATA[(BOOKING_DATA$BOOKING > a),]$BOOKING <- a

#adding winrates
agg_BOOKING <- aggregate(WON~BOOKING, data=BOOKING_DATA, FUN=mean)
names(agg_BOOKING)[names(agg_BOOKING)=="WON"] <- "BOOKING_WIN_RATE"
BOOKING_DATA <- merge(BOOKING_DATA, agg_BOOKING, by="BOOKING", all.x=TRUE)
BOOKING_DATA$WON <- NULL 
BOOKING_DATA <- BOOKING_DATA[!is.na(BOOKING_DATA$BOOKING) == TRUE,]
BOOKING_DATA <- BOOKING_DATA[!is.na(BOOKING_DATA$BOOKING_WIN_RATE) == TRUE,]
#nrow(SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITE_COUNT_WIN_RATE) == FALSE,])

#Identify ideal number of clusters to be created
wss <- (length(BOOKING_DATA)-1)*sum(apply(BOOKING_DATA,2,var))
for (i in 1:5) wss[i] <- sum(kmeans(BOOKING_DATA, centers=i)$withinss)
for (i in 1:5) 
{
  if (wss[i] == min(wss))
    num_clusters = i
}

#Fit the clustering model through K means
fit<-kmeans(BOOKING_DATA, as.numeric(num_clusters))

#Profiling the clusters and putting it into the table
cluster<-fit$cluster
BOOKING_DATA3<-cbind(BOOKING_DATA, cluster)
BOOKING_DATA3 <- as.data.frame(BOOKING_DATA3)
BOOKING_DATA3$cluster<-as.factor(BOOKING_DATA3$cluster)

#To merge SITE COUNT with data  
BOOKING_DATA3 <- unique(BOOKING_DATA3)
BOOKING_DATA3$BOOKING_WIN_RATE <- NULL 
alldata_apac[((alldata_apac$BOOKING > a)& (is.na(alldata_apac$BOOKING)== FALSE)),]$BOOKING <- a
alldata_apac <- merge(alldata_apac,BOOKING_DATA3 ,by="BOOKING", all.x=TRUE)
names(alldata_apac)[names(alldata_apac)=="cluster"] <- "BOOKING_CAT"

#nrow(alldata_apac[!is.na(alldata_apac$BOOKING) == FALSE,])
min_limit <- numeric()
max_limit <- numeric()
for(i in sort(unique(alldata_apac$BOOKING_CAT)))
{
  min_limit[i] <- min(alldata_apac$BOOKING[alldata_apac$BOOKING_CAT == i], na.rm = TRUE)
  max_limit[i] <- max(alldata_apac$BOOKING[alldata_apac$BOOKING_CAT == i], na.rm = TRUE)
  if(length(alldata_apac[alldata_apac$BOOKING>=min_limit[i] & alldata_apac$BOOKING<=max_limit[i] & 
                         (!is.na(alldata_apac$BOOKING_CAT) == FALSE) & (!is.na(alldata_apac$BOOKING) == TRUE),]$BOOKING ) > 0)
  {
    alldata_apac[alldata_apac$BOOKING >= min_limit[i] & alldata_apac$BOOKING <= max_limit[i] & (!is.na(alldata_apac$BOOKING_CAT) == FALSE) & (!is.na(alldata_apac$BOOKING) == TRUE),]$BOOKING_CAT <- i 
  }
}

#if (length(alldata_apac[(!is.na(alldata_apac$BOOKING_CAT) == FALSE) & (!is.na(alldata_apac$BOOKING) == TRUE),]$BOOKING) > 0) 
#{
#  alldata_apac[(!is.na(alldata_apac$BOOKING_CAT) == FALSE) & (!is.na(alldata_apac$BOOKING) == TRUE),]$BOOKING_CAT <- alldata_apac[alldata_apac$BOOKING == max(alldata_apac$BOOKING, na.rm= TRUE),]$BOOKING_CAT
#}
rm(BOOKING_DATA)
rm(BOOKING_DATA2)
rm(BOOKING_DATA3)
rm(agg_BOOKING)

alldata_apac$TOTAL_STAGE <- 0
alldata_apac$TOTAL_STAGE <- apply(alldata_apac[,c('STAGE_5_DURATION__C','STAGE_0_TIME__C','STAGE_1_TIME__C','STAGE_2_TIME__C','STAGE_3_TIME__C',
                                                  'STAGE_4_TIME__C')],1,sum, na.rm= TRUE)

#Breaking in percentiles
set.seed(700)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(temp_alldata_apac$TOTAL_STAGE, probs = seq(0, 1, by = 0.10))), 
      labels=c("Stage_duration_0_10_percentile","Stage_duration_10_20_percentile",
               "Stage_duration_20_30_percentile","Stage_duration_30_40_percentile",
               "Stage_duration_40_50_percentile","Stage_duration_50_60_percentile",
               "Stage_duration_60_70_percentile","Stage_duration_70_80_percentile",
               "Stage_duration_80_90_percentile","Stage_duration_90_100_percentile"
      ), include.lowest=TRUE)
}

#Add the quintile to the dataframe
temp_alldata_apac<- alldata_apac[,c('OPPID','TOTAL_STAGE')]
temp_alldata_apac$TOTAL_STAGE_GRP <- sapply(temp_alldata_apac$TOTAL_STAGE, ApplyQuintiles)
temp_alldata_apac$TOTAL_STAGE <- NULL
alldata_apac <- merge(alldata_apac,temp_alldata_apac ,by="OPPID", all.x=TRUE)
#table(temp_alldata_apac$TOTAL_STAGE_GRP)

#Adding variable GEN_WINRATE
GEN_WINRATE <- mean(alldata_apac$WON ,na.rm = TRUE)

#Adding winrates for each column 
agg_USERNAME <- aggregate(WON~username, data=alldata_apac, FUN=mean)
names(agg_USERNAME)[names(agg_USERNAME)=="WON"] <- "REP_P"
alldata_apac <- merge(alldata_apac, agg_USERNAME, by="username", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$REP_P) == FALSE),]$REP_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$REP_P) == FALSE),]$REP_P <- GEN_WINRATE
}
rm(agg_USERNAME)

agg_NUM_STALLS <- aggregate(WON~NUM_STALLS, data=alldata_apac, FUN=mean)
names(agg_NUM_STALLS)[names(agg_NUM_STALLS)=="WON"] <- "NUM_STALLS_P"
alldata_apac <- merge(alldata_apac, agg_NUM_STALLS, by="NUM_STALLS", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$NUM_STALLS_P) == FALSE),]$NUM_STALLS_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$NUM_OF_STALLS_P) == FALSE),]$NUM_OF_STALLS_P <- GEN_WINRATE
}
rm(agg_NUM_OF_STALLS)

agg_Branch_geo_region <- aggregate(WON~branch_geo_region, data=alldata_apac, FUN=mean)
names(agg_Branch_geo_region)[names(agg_Branch_geo_region)=="WON"] <- "BRANCH_GEO_REGION_P"
alldata_apac <- merge(alldata_apac, agg_Branch_geo_region, by="branch_geo_region", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$BRANCH_GEO_REGION_P) == FALSE),]$BRANCH_GEO_REGION_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$BRANCH_GEO_REGION_P) == FALSE),]$BRANCH_GEO_REGION_P <- GEN_WINRATE
}
rm(agg_Branch_geo_region)

agg_BOOKING <- aggregate(WON~BOOKING_CAT, data=alldata_apac, FUN=mean)
names(agg_BOOKING)[names(agg_BOOKING)=="WON"] <- "BOOKING_P"
alldata_apac <- merge(alldata_apac, agg_BOOKING, by="BOOKING_CAT", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$BOOKING_P) == FALSE),]$BOOKING_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$BOOKING_P) == FALSE),]$BOOKING_P <- GEN_WINRATE
}
rm(agg_BOOKING)

agg_SITECOUNT <- aggregate(WON~SITE_COUNT_CAT, data=alldata_apac, FUN=mean)
names(agg_SITECOUNT)[names(agg_SITECOUNT)=="WON"] <- "SITECOUNT_P"
alldata_apac <- merge(alldata_apac, agg_SITECOUNT, by="SITE_COUNT_CAT", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$SITECOUNT) == FALSE),]$SITECOUNT) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$SITECOUNT) == FALSE),]$SITECOUNT_P <- mean(alldata_apac[(!is.na(alldata_apac$SITECOUNT) == FALSE),]$WON ,na.rm = TRUE)
}
rm(agg_SITECOUNT)

agg_TOTAL_STAGE_GRP <- aggregate(WON~TOTAL_STAGE_GRP, data=alldata_apac, FUN=mean)
names(agg_TOTAL_STAGE_GRP)[names(agg_TOTAL_STAGE_GRP)=="WON"] <- "TOTAL_STAGE_GRP_P"
alldata_apac <- merge(alldata_apac, agg_TOTAL_STAGE_GRP, by="TOTAL_STAGE_GRP", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$TOTAL_STAGE_GRP_P) == FALSE),]$TOTAL_STAGE_GRP_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$TOTAL_STAGE_GRP_P) == FALSE),]$TOTAL_STAGE_GRP_P <- GEN_WINRATE
}
rm(agg_TOTAL_STAGE_GRP)

agg_STALLED_STAGE <- aggregate(WON~STALLED_STAGE, data=alldata_apac, FUN=mean)
names(agg_STALLED_STAGE)[names(agg_STALLED_STAGE)=="WON"] <- "STALL_STAGE_P"
alldata_apac <- merge(alldata_apac, agg_STALLED_STAGE, by="STALLED_STAGE", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$STALL_STAGE_P) == FALSE),]$STALL_STAGE_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$STALL_STAGE_P) == FALSE),]$STALL_STAGE_P <- GEN_WINRATE
}
rm(agg_STALLED_STAGE)

agg_VERTICAL <- aggregate(WON~VERTICAL_RPT_DESC2, data=alldata_apac, FUN=mean)
names(agg_VERTICAL)[names(agg_VERTICAL)=="WON"] <- "VERTICAL_P"
alldata_apac <- merge(alldata_apac, agg_VERTICAL, by="VERTICAL_RPT_DESC2", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$VERTICAL_P) == FALSE),]$VERTICAL_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$VERTICAL_P) == FALSE),]$VERTICAL_P <- GEN_WINRATE
}
rm(agg_VERTICAL)

agg_PRODUCTS <- aggregate(WON~PRODUCTS, data=alldata_apac, FUN=mean)
names(agg_PRODUCTS)[names(agg_PRODUCTS)=="WON"] <- "PRODUCTS_P"
alldata_apac <- merge(alldata_apac, agg_PRODUCTS, by="PRODUCTS", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$PRODUCTS_P) == FALSE),]$PRODUCTS_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$PRODUCTS_P) == FALSE),]$PRODUCTS_P <- GEN_WINRATE
}
rm(agg_PRODUCTS)

agg_PUSH_COUNTER_CAT <- aggregate(WON~PUSH_COUNTER_CAT, data=alldata_apac, FUN=mean)
names(agg_PUSH_COUNTER_CAT)[names(agg_PUSH_COUNTER_CAT)=="WON"] <- "PUSH_COUNTER_CAT_P"
alldata_apac <- merge(alldata_apac, agg_PUSH_COUNTER_CAT, by="PUSH_COUNTER_CAT", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$PUSH_COUNTER_CAT_P) == FALSE),]$PUSH_COUNTER_CAT_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$PUSH_COUNTER_CAT_P) == FALSE),]$PUSH_COUNTER_CAT_P <- GEN_WINRATE
}
rm(agg_PUSH_COUNTER_CAT)

agg_COMPETITOR <- aggregate(WON~COMPETITOR, data=alldata_apac, FUN=mean)
names(agg_COMPETITOR)[names(agg_COMPETITOR)=="WON"] <- "COMPETITOR_P"
alldata_apac <- merge(alldata_apac, agg_COMPETITOR, by="COMPETITOR", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$COMPETITOR_P) == FALSE),]$COMPETITOR_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$COMPETITOR_P) == FALSE),]$COMPETITOR_P <- GEN_WINRATE
}
rm(agg_COMPETITOR)

agg_CSM_TIER_CODE <- aggregate(WON~CSM_TIER_CODE, data=alldata_apac, FUN=mean)
names(agg_CSM_TIER_CODE)[names(agg_CSM_TIER_CODE)=="WON"] <- "CSM_P"
alldata_apac <- merge(alldata_apac, agg_CSM_TIER_CODE, by="CSM_TIER_CODE", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$CSM_P) == FALSE),]$CSM_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$CSM_P) == FALSE),]$CSM_P <- GEN_WINRATE
}
rm(agg_CSM_TIER_CODE)


agg_NASP_TYPE<- aggregate(WON~nasp_type, data=alldata_apac, FUN=mean)
names(agg_NASP_TYPE)[names(agg_NASP_TYPE)=="WON"] <- "NASP_TYPE_P"
alldata_apac <- merge(alldata_apac, agg_NASP_TYPE, by="nasp_type", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$NASP_TYPE_P) == FALSE),]$NASP_TYPE_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$NASP_TYPE_P) == FALSE),]$NASP_TYPE_P <- GEN_WINRATE
}
rm(agg_NASP_TYPE)

agg_NASPID<- aggregate(WON~NASP_ID, data=alldata_apac, FUN=mean)
names(agg_NASPID)[names(agg_NASPID)=="WON"] <- "NASPID_P"
alldata_apac <- merge(alldata_apac, agg_NASPID, by="NASP_ID", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$NASPID_P) == FALSE),]$NASPID_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$NASPID_P) == FALSE),]$NASPID_P <- GEN_WINRATE
}
rm(agg_NASPID)

agg_PROD_COUNT<- aggregate(WON~NUM_OF_PRODUCTS, data=alldata_apac, FUN=mean)
names(agg_PROD_COUNT)[names(agg_PROD_COUNT)=="WON"] <- "PROD_COUNT_P"
alldata_apac <- merge(alldata_apac, agg_PROD_COUNT, by="NUM_OF_PRODUCTS", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$PROD_COUNT_P) == FALSE),]$PROD_COUNT_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$PROD_COUNT_P) == FALSE),]$PROD_COUNT_P <- GEN_WINRATE
}
rm(agg_PROD_COUNT)

agg_PRIMARY_COUNTRY<- aggregate(WON~PRIMARY_COUNTRY, data=alldata_apac, FUN=mean)
names(agg_PRIMARY_COUNTRY)[names(agg_PRIMARY_COUNTRY)=="WON"] <- "PRIMARY_COUNTRY_P"
alldata_apac <- merge(alldata_apac, agg_PRIMARY_COUNTRY, by="PRIMARY_COUNTRY", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$PRIMARY_COUNTRY_P) == FALSE),]$PRIMARY_COUNTRY_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$PRIMARY_COUNTRY_P) == FALSE),]$PRIMARY_COUNTRY_P <- GEN_WINRATE
}
rm(agg_PRIMARY_COUNTRY)


agg_TERM<- aggregate(WON~TERM, data=alldata_apac, FUN=mean)
names(agg_TERM)[names(agg_TERM)=="WON"] <- "TERM_P"
alldata_apac <- merge(alldata_apac, agg_TERM, by="TERM", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$TERM_P) == FALSE),]$TERM_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$TERM_P) == FALSE),]$TERM_P <- GEN_WINRATE
}
rm(agg_TERM)

agg_OPPTYPE<- aggregate(WON~OPPTYPE, data=alldata_apac, FUN=mean)
names(agg_OPPTYPE)[names(agg_OPPTYPE)=="WON"] <- "OPPTYPE_P"
alldata_apac <- merge(alldata_apac, agg_OPPTYPE, by="OPPTYPE", all.x=TRUE)
if (length(alldata_apac[(!is.na(alldata_apac$OPPTYPE_P) == FALSE),]$OPPTYPE_P) > 0) 
{
  alldata_apac[(!is.na(alldata_apac$OPPTYPE_P) == FALSE),]$OPPTYPE_P <- GEN_WINRATE
}
rm(agg_OPPTYPE)

reg_tab<- alldata_apac[,c('OPPID','OPPTYPE_P','TERM_P','PRIMARY_COUNTRY_P','PROD_COUNT_P','NASPID_P','NASP_TYPE_P',
                          'CSM_P','COMPETITOR_P','PUSH_COUNTER_CAT_P','VERTICAL_P','STAGE_CONVERSION_P','PRODUCTS_P', 
                          'BOOKING_P','REP_P','TOTAL_STAGE_GRP_P', 'STALL_STAGE_P', 'WON', 'SITECOUNT_P' , 
                          'BRANCH_GEO_REGION_P','NUM_STALLS_P','MODEL_IND')]
reg_tab <- as.data.frame(reg_tab)

#form train and test data 
Training_Data <- reg_tab[(!is.na(reg_tab$WON) == TRUE),]
Test_Data <- reg_tab[(!is.na(reg_tab$WON) == FALSE),]
dbhandle <- odbcDriverConnect("dsn=AJ_prod_latest;uid=v683623;pwd=pokemon16;")
OPP_CLOSED_SINCE_NOV_END <- sqlQuery(dbhandle, "SELECT OPPID,STAGENAME FROM SND_OFFSHORE_ANALYTICS.FUNNEL_OPPS_CLOSED_SINCE_8NOV2017 where STAGENAME like '%CLOSED%'")

OPP_CLOSED_SINCE_NOV_END <- as.data.frame(OPP_CLOSED_SINCE_NOV_END)
names(OPP_CLOSED_SINCE_NOV_END)[names(OPP_CLOSED_SINCE_NOV_END)=="STAGENAME"] <- "FINAL_STAGE"
Test_Data <- merge(Test_Data,OPP_CLOSED_SINCE_NOV_END ,by="OPPID")

Training_DataS <- Training_Data[Training_Data$MODEL_IND == 'S',];
Training_DataM <- Training_Data[Training_Data$MODEL_IND == 'M',];
Training_DataL <- Training_Data[Training_Data$MODEL_IND == 'L',];
Test_DataS <- Test_Data[Test_Data$MODEL_IND == 'S',];
Test_DataM <- Test_Data[Test_Data$MODEL_IND == 'M',];
Test_DataL <- Test_Data[Test_Data$MODEL_IND == 'L',];

# COMPUTING THE REGRESSION CO-EFFICIENTS
Data_ModelS <- step(glm(WON ~ OPPTYPE_P + TERM_P + PRIMARY_COUNTRY_P + PROD_COUNT_P + NASPID_P + NASP_TYPE_P + CSM_P + COMPETITOR_P + PUSH_COUNTER_CAT_P + VERTICAL_P + PRODUCTS_P + REP_P + TOTAL_STAGE_GRP_P + STAGE_CONVERSION_P + STALL_STAGE_P + SITECOUNT_P + BOOKING_P + NUM_STALLS_P + BRANCH_GEO_REGION_P ,family=binomial, data=Training_DataS))
Data_ModelM <- step(glm(WON ~ OPPTYPE_P + TERM_P + PRIMARY_COUNTRY_P + PROD_COUNT_P + NASPID_P + NASP_TYPE_P + CSM_P + COMPETITOR_P + PUSH_COUNTER_CAT_P + VERTICAL_P + PRODUCTS_P + REP_P + TOTAL_STAGE_GRP_P + STAGE_CONVERSION_P + STALL_STAGE_P + SITECOUNT_P + BOOKING_P + NUM_STALLS_P + BRANCH_GEO_REGION_P ,family=binomial, data=Training_DataM))
Data_ModelL <- step(glm(WON ~ OPPTYPE_P + TERM_P + PRIMARY_COUNTRY_P + PROD_COUNT_P + NASPID_P + NASP_TYPE_P + CSM_P + COMPETITOR_P + PUSH_COUNTER_CAT_P + VERTICAL_P + PRODUCTS_P + REP_P + TOTAL_STAGE_GRP_P + STAGE_CONVERSION_P + STALL_STAGE_P + SITECOUNT_P + BOOKING_P + NUM_STALLS_P + BRANCH_GEO_REGION_P ,family=binomial, data=Training_DataL))

#library(car)
#xyz <-  vif(Data_Model)
#summ <-summary(Data_Model)

vbookS <- data.frame(oppid = Test_DataS$OPPID, oppscore = 1/(1+exp(-(predict(Data_ModelS,Test_DataS)))), final_stage = Test_DataS$FINAL_STAGE) # The prediction is performed on the test data based on the logistic regression formula 1/(1 + e^-f(x)).
vbookM <- data.frame(oppid = Test_DataM$OPPID, oppscore = 1/(1+exp(-(predict(Data_ModelM,Test_DataM)))), final_stage = Test_DataM$FINAL_STAGE) # The prediction is performed on the test data based on the logistic regression formula 1/(1 + e^-f(x)).
vbookL <- data.frame(oppid = Test_DataL$OPPID, oppscore = 1/(1+exp(-(predict(Data_ModelL,Test_DataL)))), final_stage = Test_DataL$FINAL_STAGE) # The prediction is performed on the test data based on the logistic regression formula 1/(1 + e^-f(x)).
vbook_apac <- rbind(vbookS,vbookM,vbookL)

vbook_apac$oppscore_category <-ifelse(vbook_apac$oppscore< 0.35,"Low probability",
                                 ifelse(vbook_apac$oppscore <0.75,"Medium probablity",
                                        ifelse(vbook_apac$oppscore <=1,"High probability","unpredictable")))
count <-xtabs(~vbook_apac$final_stage+vbook_apac$oppscore_category, data=vbook_apac)
total_accuracy <- (count[3,1]+count[1,2]+count[2,2])/(count[3,1]+count[2,1]+count[1,1]+count[1,2]+count[2,2]+count[3,2])
win_accuracy <- (count[3,1])/(count[3,1]+count[2,1]+count[1,1])
loss_accuracy <-(count[1,2]+count[2,2])/(count[1,2]+count[2,2]+count[3,2])
#> total_accuracy
# [1] 0.8181818
# > win_accuracy
# [1] 1
# > loss_accuracy
# [1]0.76
# > 
# add all required columns to vbook_non_apac
#View(alldata_apac_back)
head(alldata_apac_back)
names(reg_tab)
# change name from oppid to OPPID
colnames(vbook_apac)[1] <- "OPPID"
nrow(vbook_apac)
vbook_apac_2<-merge(x = vbook_apac, 
                      y = alldata_apac_back[ , c("BOOKING", "forecast_category","STAGENAME","EXP_SPA_DATE","username","PRODUCTS",
                                                 "nasp_type","VERTICAL_RPT_DESC2","PRIMARY_COUNTRY","CSM_TIER_CODE","OPPID"
                                                 )], by = "OPPID", all.x=TRUE, all.y=FALSE)
nrow(vbook_apac_2)
#duplicate rows because of booking_aggr

vbook_apac_2<-merge(x = vbook_apac_2, 
                    y = reg_tab[ , c("REP_P","OPPID")], by = "OPPID", all.x=TRUE, all.y=FALSE)
nrow(vbook_apac_2)
if (weekdays(Sys.Date())=="Monday")
{
for(row in seq_len(nrow(vbook_apac_2))){
  
  query <- paste0(
    "INSERT INTO SND_OFFSHORE_ANALYTICS.SFA_APAC_TRK_OUTPUT_FINAL
   (RPT_DATE, OPPID, BI_PROB_SCORE, BI_PROB_CATEGORY, 
    BOOKING, FORECAST_CATEGORY, STAGENAME, EXP_SPA_DATE, 
     NASP_TYPE, VERTICAL_RPT_DESC2, 
    PRIMARY_COUNTRY, CSM_TIER_CODE,  NAME,  REP_P, PRODUCTS)
    VALUES ( CURRENT_DATE,
    '", vbook_apac_2$OPPID[row],"', 
    '", vbook_apac_2$oppscore[row],"', 
    '", vbook_apac_2$oppscore_category[row],"', 
    '", vbook_apac_2$BOOKING[row],"', 
    '", vbook_apac_2$forecast_category[row],"', 
    '", vbook_apac_2$STAGENAME[row],"', 
    '", vbook_apac_2$EXP_SPA_DATE[row],"', 
    '", vbook_apac_2$nasp_type[row],"', 
    '", vbook_apac_2$VERTICAL_RPT_DESC2[row],"', 
    '", vbook_apac_2$PRIMARY_COUNTRY[row],"',
    '", vbook_apac_2$CSM_TIER_CODE[row],"', 
    '", vbook_apac_2$username[row],"', 
    '", vbook_apac_2$REP_P[row],"',
    '", vbook_apac_2$PRODUCTS[row],"' 
    )"
  )
  sqlQuery(dbhandle, query)
}
}
#INSERT INTO SND_OFFSHORE_ANALYTICS.SFA_APAC_TRK_OUTPUT_FINAL (RPT_DATE, OPPID, BI_PROB_SCORE, BI_PROB_CATEGORY, 
#BOOKING, FORECAST_CATEGORY, STAGENAME, EXP_SPA_DATE, GVP, SALES_DIRECTOR, NASP_TYPE, VERTICAL_RPT_DESC2, 
#PRIMARY_COUNTRY, CSM_TIER_CODE, B_END, OPPNAME, ACCNAME, NAME, MANAGER, SFDC_OPPORTUNITY_ID, REP_P, PRODUCTS) VALUES();
# ####block insert####
# # change names
# colnames(vbook_apac_2)[colnames(vbook_apac_2)=="oppscore"] <- "BI_PROB_SCORE"
# colnames(vbook_apac_2)[colnames(vbook_apac_2)=="oppscore_category"] <- "BI_PROB_CATEGORY"
# colnames(vbook_apac_2)[colnames(vbook_apac_2)=="forecast_category"] <- "FORECAST_CATEGORY"
# colnames(vbook_apac_2)[colnames(vbook_apac_2)=="nasp_type"] <- "NASP_TYPE"
# colnames(vbook_apac_2)[colnames(vbook_apac_2)=="username"] <- "NAME"
# 
# apac_trk<-vbook_apac_2
# apac_trk$RPT_DATE<-Sys.Date()
# #View(apac_trk)
# #RPT_DATE, OPPID, BI_PROB_SCORE, BI_PROB_CATEGORY, 
# #BOOKING, FORECAST_CATEGORY, STAGENAME, EXP_SPA_DATE, GVP, SALES_DIRECTOR, NASP_TYPE, VERTICAL_RPT_DESC2, 
# #PRIMARY_COUNTRY, CSM_TIER_CODE, B_END, OPPNAME, ACCNAME, NAME, MANAGER, SFDC_OPPORTUNITY_ID, REP_P, PRODUCTS
# apac_trk$GVP   <-  NA
# apac_trk$SALES_DIRECTOR <- NA
# apac_trk$B_END   <-  NA
# apac_trk$OPPNAME <- NA
# apac_trk$ACCNAME   <-  NA
# apac_trk$MANAGER <- NA
# apac_trk$SFDC_OPPORTUNITY_ID   <-  NA
# apac_trk$BI_TOP_RISK_FACTORS_1 <-NA
# apac_trk$BI_TOP_RISK_FACTORS_2  <-NA
# apac_trk$BI_TOP_RISK_FACTORS_3  <-NA
# apac_trk$BI_TOP_RISK_FACTORS_4  <-NA
# apac_trk$BI_TOP_RISK_FACTORS_5  <- NA
# names(apac_trk)
# apac_trk_fill <- apac_trk[, c("RPT_DATE", "OPPID", "BI_PROB_SCORE", "BI_PROB_CATEGORY", 
#                         "BOOKING", "FORECAST_CATEGORY", "STAGENAME", "EXP_SPA_DATE", "GVP","SALES_DIRECTOR",
#                         "NASP_TYPE", "VERTICAL_RPT_DESC2", "PRIMARY_COUNTRY", "CSM_TIER_CODE","B_END",
#                         "OPPNAME", "ACCNAME" ,"NAME", "MANAGER", "SFDC_OPPORTUNITY_ID" , "REP_P","BI_TOP_RISK_FACTORS_1"
#                         , "BI_TOP_RISK_FACTORS_2", "BI_TOP_RISK_FACTORS_3", "BI_TOP_RISK_FACTORS_4" ,"BI_TOP_RISK_FACTORS_5" ,"PRODUCTS")]
#  
# sqlSave(dbhandle, apac_trk_fill, tablename = "SND_OFFSHORE_ANALYTICS.SFA_APAC_TRK_OUTPUT_FINAL",append = TRUE,fast=TRUE)
# 

## ## ------------------for non APAC------------------------- ##
#Data cleaning
#Adding model indicator 

alldata_non_apac$MODEL_IND <-'';
alldata_non_apac = alldata_non_apac %>% mutate(MODEL_IND = ifelse(TOTAL_STAGE_DURATION__C > 90, 'L', MODEL_IND))
alldata_non_apac = alldata_non_apac %>% mutate(MODEL_IND = ifelse(TOTAL_STAGE_DURATION__C < 46, 'S', MODEL_IND))
alldata_non_apac = alldata_non_apac %>% mutate(MODEL_IND = ifelse((TOTAL_STAGE_DURATION__C > 45)&(TOTAL_STAGE_DURATION__C < 91), 'M', MODEL_IND))

#Opptype update
alldata_non_apac$OPPTYPE <- as.character(alldata_non_apac$OPPTYPE)
alldata_non_apac$OPPTYPE[alldata_non_apac$OPPTYPE %in% c('Add to Existing/Upgrade','Change - Other','Upgrade - Service',
                                                         'Conversion/Migration','Upgrade - Same Service',
                                                         'Downgrade - Service','Move','Upsell - Cross Sell','Contract Only')] = 'CHANGE'
alldata_non_apac$OPPTYPE[alldata_non_apac$OPPTYPE %in% c('New Business','Existing Business','TOSA')]='EXISTING'
alldata_non_apac$OPPTYPE[alldata_non_apac$OPPTYPE %in% c('Renewal Only','Renewal')]='RENEW'
alldata_non_apac = alldata_non_apac %>% mutate(OPPTYPE = ifelse(OPPTYPE =='New Customer','NEW',OPPTYPE))
alldata_non_apac$OPPTYPE <- as.factor(alldata_non_apac$OPPTYPE)

# UPDATING THE NEW CSM TIERS
alldata_non_apac$CSM_TIER_CODE[alldata_non_apac$CSM_TIER_CODE %in% c('CST-1','CST-2')]='CSM-3'
alldata_non_apac = alldata_non_apac %>% mutate(CSM_TIER_CODE = ifelse(CSM_TIER_CODE =='Field-1','CSM-1',CSM_TIER_CODE))
alldata_non_apac = alldata_non_apac %>% mutate(CSM_TIER_CODE = ifelse(CSM_TIER_CODE =='Field-2','CSM-2',CSM_TIER_CODE))

#Stalled stage update
alldata_non_apac$STALLED_STAGE <- as.numeric(NA)
alldata_non_apac$STAGE_0_TIME__C <- as.numeric(alldata_non_apac$STAGE_0_TIME__C)

alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_0_TIME__C >= 90)&(MODEL_IND == 'L')),0,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_1_TIME__C >= 90)&(MODEL_IND == 'L')),1,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_2_TIME__C >= 90)&(MODEL_IND == 'L')),2,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_3_TIME__C >= 120)&(MODEL_IND == 'L')),3,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND == 'L')),4,STALLED_STAGE))

alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_0_TIME__C >= 30)&(MODEL_IND == 'S')),0,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_1_TIME__C >= 27)&(MODEL_IND =='S')),1,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_2_TIME__C >= 25)&(MODEL_IND == 'S')),2,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_3_TIME__C >= 35)&(MODEL_IND == 'S')),3,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND =='S')),4,STALLED_STAGE))

alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_0_TIME__C >= 40)&(MODEL_IND == 'M')),0,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_1_TIME__C >= 40)&(MODEL_IND == 'M')),1,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_2_TIME__C >= 40)&(MODEL_IND == 'M')),2,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_3_TIME__C >= 60)&(MODEL_IND == 'M')),3,STALLED_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(STALLED_STAGE = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND =='M')),4,STALLED_STAGE))

#Vertical update 
alldata_non_apac$VERTICAL_RPT_DESC2 <- as.character(alldata_non_apac$VERTICAL_RPT_DESC2)
alldata_non_apac$VERTICAL_RPT_DESC2 <-gsub( "/.*$", "", alldata_non_apac$VERTICAL_RPT_DESC2 )
alldata_non_apac$VERTICAL_RPT_DESC2 <- as.factor(alldata_non_apac$VERTICAL_RPT_DESC2)


#number of stalls 
alldata_non_apac$NUM_STALLS <- 0

alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_0_TIME__C >= 90)&(MODEL_IND == 'L')),0,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_1_TIME__C >= 90)&(MODEL_IND == 'L')),1,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_2_TIME__C >= 90)&(MODEL_IND == 'L')),2,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_3_TIME__C >= 120)&(MODEL_IND == 'L')),3,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND == 'L')),4,NUM_STALLS +1))

alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND == 'S')),4,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_3_TIME__C >= 35)&(MODEL_IND == 'S')),3,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_2_TIME__C >= 25)&(MODEL_IND =='S')),2,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_1_TIME__C >= 27)&(MODEL_IND =='S')),1,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_0_TIME__C >= 30)&(MODEL_IND =='S')),0,NUM_STALLS +1))

alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_4_TIME__C <= 0)&(MODEL_IND == 'M')),4,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_3_TIME__C >= 60)&(MODEL_IND == 'M')),3,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_2_TIME__C >= 40)&(MODEL_IND == 'M')),2,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_1_TIME__C >= 40)&(MODEL_IND == 'M')),1,NUM_STALLS +1))
alldata_non_apac = alldata_non_apac %>% mutate(NUM_STALLS = ifelse(((STAGE_0_TIME__C >= 40)&(MODEL_IND == 'M')),0,NUM_STALLS +1))

#adding winrates later 
#agg_stage_stall <- aggregate(WON~STALLED_STAGE, data=alldata_non_apac, FUN=mean)
#names(agg_stage_stall)[names(agg_stage_stall)=="WON"] <- "STALLED_WIN_RATE"
#alldata_non_apac <- merge(alldata_non_apac, agg_stage_stall, by="STALLED_STAGE", all.x=TRUE)

#adding stage conversion factors
alldata_non_apac$STAGENAME <- as.character(alldata_non_apac$STAGENAME)
alldata_non_apac$LAST_STAGE <- as.numeric(0)
alldata_non_apac = alldata_non_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_0_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),0,LAST_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_1_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),1,LAST_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_2_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),2,LAST_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_3_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),3,LAST_STAGE))
alldata_non_apac = alldata_non_apac %>% mutate(LAST_STAGE = ifelse(((STAGE_4_TIME__C != 0)&(grepl("Closed",STAGENAME, fixed=TRUE)== TRUE)),4,LAST_STAGE))
alldata_non_apac$STAGENAME <- as.factor(alldata_non_apac$STAGENAME)


#Calculating total won oppids and total oppids
alldata_non_apac_temp <- alldata_non_apac[alldata_non_apac$STAGENAME =='5 Closed Won',]
COUNT_WON <-length(unique(alldata_non_apac_temp$OPPID))
rm('alldata_non_apac_temp')
COUNT_TOTAL <- length(unique(alldata_non_apac$OPPID))
alldata_non_apac$COUNT_WON <- as.numeric(COUNT_WON)
alldata_non_apac$COUNT_TOTAL <- as.numeric(COUNT_TOTAL)


#lost + disqualified wrt last stage calculation
agg_COUNT_LOST_DISQ <- aggregate(OPPID~LAST_STAGE, data=alldata_non_apac[alldata_non_apac$STAGENAME=="5 Closed Disqualified" | alldata_non_apac$STAGENAME == '5 Closed Lost',], FUN=NROW)
i <- as.numeric(0)
agg_COUNT_LOST_DISQ <- as.data.frame(agg_COUNT_LOST_DISQ)
for (i in (0:4))
{
  x <-paste('COUNT_LOST_DISQ',i, sep="") 
  assign(paste('COUNT_LOST_DISQ',i, sep=""),as.numeric(agg_COUNT_LOST_DISQ[[2]][i+1]))
  b <- mget(x);
  alldata_non_apac <- cbind(alldata_non_apac, b);
}

#STAGE_CONVERSION_P Calculation
alldata_non_apac$STAGE_CONVERSION_P <- as.numeric(0)
alldata_non_apac = alldata_non_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 0),(COUNT_WON/COUNT_TOTAL),STAGE_CONVERSION_P))
alldata_non_apac = alldata_non_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 1),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0)),STAGE_CONVERSION_P))
alldata_non_apac = alldata_non_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 2),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0-COUNT_LOST_DISQ1)),STAGE_CONVERSION_P))
alldata_non_apac = alldata_non_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 3),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0-COUNT_LOST_DISQ1-COUNT_LOST_DISQ2)),STAGE_CONVERSION_P))
alldata_non_apac = alldata_non_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 4),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0-COUNT_LOST_DISQ1-COUNT_LOST_DISQ2-COUNT_LOST_DISQ3)),STAGE_CONVERSION_P))
alldata_non_apac = alldata_non_apac %>% mutate(STAGE_CONVERSION_P = ifelse((LAST_STAGE == 5),(COUNT_WON/(COUNT_TOTAL-COUNT_LOST_DISQ0-COUNT_LOST_DISQ1-COUNT_LOST_DISQ2-COUNT_LOST_DISQ3-COUNT_LOST_DISQ4)),STAGE_CONVERSION_P))


#Dropping unnecessary columns Not correct 
for (i in (0:4))
{
  x <-paste('COUNT_LOST_DISQ',i, sep="")
  b <- mget(x)
  b <-names(b)
  remove(list=c(b))
  alldata_non_apac[,x] <- NULL
}    
rm('x');
rm('i');
rm('b');
rm('COUNT_TOTAL')
rm('COUNT_WON')

#Stuffing products and grouping by oppid 
PROD_GRP<- alldata_non_apac[,c('OPPID','PRODUCT_FAMILY_PR4')]
PROD_GRP<- as.data.frame(PROD_GRP)
PROD_GRP <- PROD_GRP[order(PROD_GRP$OPPID, PROD_GRP$PRODUCT_FAMILY_PR4),]
PROD_GRP <- unique(PROD_GRP)
PROD_GRP <- aggregate(PRODUCT_FAMILY_PR4~OPPID,data=PROD_GRP,paste, collapse="|")
names(PROD_GRP)[names(PROD_GRP)=="PRODUCT_FAMILY_PR4"] <- "PRODUCTS"
alldata_non_apac <- merge(alldata_non_apac, PROD_GRP, by="OPPID", all.x=TRUE)


#AGGREGATING THE BOOKINGS/OPPORTUNITY
#CLOSE_YEAR

CSM_TIER_CODE_AGGR<- alldata_non_apac[,c('OPPID','CSM_TIER_CODE')]
CSM_TIER_CODE_AGGR<- as.data.frame(CSM_TIER_CODE_AGGR)
CSM_TIER_CODE_AGGR <- aggregate(CSM_TIER_CODE~OPPID,data=CSM_TIER_CODE_AGGR,min,na.rm=TRUE)
names(CSM_TIER_CODE_AGGR)[names(CSM_TIER_CODE_AGGR)=="CSM_TIER_CODE"] <- "CSM_TIER_CODE_AGGR"
alldata_non_apac <- merge(alldata_non_apac, CSM_TIER_CODE_AGGR, by="OPPID", all.x=TRUE)

BOOKING_AGGR<- alldata_non_apac[,c('OPPID','BOOKING')]
BOOKING_AGGR<- as.data.frame(BOOKING_AGGR)
BOOKING_AGGR <- aggregate(BOOKING~OPPID,data=BOOKING_AGGR,sum, na.rm=TRUE)
names(BOOKING_AGGR)[names(BOOKING_AGGR)=="BOOKING"] <- "BOOKING_AGGR"
alldata_non_apac <- merge(alldata_non_apac, BOOKING_AGGR, by="OPPID", all.x=TRUE)

SITECOUNT_AGGR<- alldata_non_apac[,c('OPPID','SITECOUNT')]
SITECOUNT_AGGR<- as.data.frame(SITECOUNT_AGGR)
SITECOUNT_AGGR <- aggregate(SITECOUNT~OPPID,data=SITECOUNT_AGGR,sum,na.rm=TRUE)
names(SITECOUNT_AGGR)[names(SITECOUNT_AGGR)=="SITECOUNT"] <- "SITECOUNT_AGGR"
alldata_non_apac <- merge(alldata_non_apac, SITECOUNT_AGGR, by="OPPID", all.x=TRUE)

#Dropping unnecessary columns and keeping only aggregated view
alldata_non_apac[,'CSM_TIER_CODE']<-NULL
alldata_non_apac[,'BOOKING']<-NULL
alldata_non_apac[,'SITECOUNT']<-NULL
alldata_non_apac[,'PRODUCT_FAMILY_PR4']<- NULL
alldata_non_apac[,'COUNT_TOTAL']<- NULL 
alldata_non_apac[,'COUNT_WON']<- NULL 
alldata_non_apac <- unique(alldata_non_apac)
rm(BOOKING_AGGR)
rm(CSM_TIER_CODE_AGGR)
rm(PROD_GRP)
rm(SITECOUNT_AGGR)

#Renaming few columns 
names(alldata_non_apac)[names(alldata_non_apac)=="CSM_TIER_CODE_AGGR"] <- "CSM_TIER_CODE"
names(alldata_non_apac)[names(alldata_non_apac)=="BOOKING_AGGR"] <- "BOOKING"
names(alldata_non_apac)[names(alldata_non_apac)=="SITECOUNT_AGGR"] <- "SITECOUNT"

# taking backup for column feed in table
alldata_non_apac_back<-alldata_non_apac
alldata_non_apac_back <- unique(alldata_apac_back)
#Adding Close year 
alldata_non_apac$CLOSE_YEAR <- format(as.Date(alldata_non_apac$DATE_OPPORTUNITY_CLOSED, format="%d/%m/%Y"),"%Y")
alldata_non_apac[,'DATE_OPPORTUNITY_CLOSED']<-NULL

#length(unique(alldata_non_apac_perm$OPPID)) discrepency of 161105 - 160884
#write.csv(alldata_non_apac, file = "C:\\Users\\v683589\\Desktop\\SQL MODEL\\alldata_non_apac.csv", na="", row.names = FALSE)
#checking required some 200 rows extra 

################Clustering with just PUSH_COUNTER########################

PUSH_COUNTER_DATA<-alldata_non_apac[,c('PUSH_COUNTER','WON')]
PUSH_COUNTER_DATA <- as.data.frame(PUSH_COUNTER_DATA)
#adding winrates
agg_PUSH_COUNTER <- aggregate(WON~PUSH_COUNTER, data=PUSH_COUNTER_DATA, FUN=mean)
names(agg_PUSH_COUNTER)[names(agg_PUSH_COUNTER)=="WON"] <- "PUSH_COUNTER_WIN_RATE"
PUSH_COUNTER_DATA <- merge(PUSH_COUNTER_DATA, agg_PUSH_COUNTER, by="PUSH_COUNTER", all.x=TRUE)
PUSH_COUNTER_DATA$WON <- NULL 

#Identify ideal number of clusters to be created
wss <- (length(PUSH_COUNTER_DATA)-1)*sum(apply(PUSH_COUNTER_DATA,2,var))
for (i in 1:7) wss[i] <- sum(kmeans(na.omit(PUSH_COUNTER_DATA), centers=i)$withinss)
#plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
for (i in 1:7) 
{
  if (wss[i] == min(wss))
    num_clusters = i
}

#Fit the clustering model through K means
fit<-kmeans(na.omit(PUSH_COUNTER_DATA), as.numeric(num_clusters))

#Check details of the cluster created
#fit
#fit$size
#Check strength of clustering: BSS should be higher and WSS should be lower
#strength<-fit$betweenss/fit$withinss
#strength
#cluster<-fit$cluster
#R Square: Total variance explained by the clustering exercise
#fit$betweenss/fit$totss

#Profiling the clusters and putting it into the table
cluster<-fit$cluster
PUSH_COUNTER_DATA3<-cbind(na.omit(PUSH_COUNTER_DATA), cluster)
PUSH_COUNTER_DATA3 <- as.data.frame(PUSH_COUNTER_DATA3)
PUSH_COUNTER_DATA3$cluster<-as.factor(PUSH_COUNTER_DATA3$cluster)

#To merge Push counter with data  
PUSH_COUNTER_DATA3 <- unique(PUSH_COUNTER_DATA3)
PUSH_COUNTER_DATA3$PUSH_COUNTER_WIN_RATE <- NULL 
alldata_non_apac <- merge(alldata_non_apac,PUSH_COUNTER_DATA3 ,by="PUSH_COUNTER", all.x=TRUE)
names(alldata_non_apac)[names(alldata_non_apac)=="cluster"] <- "PUSH_COUNTER_CAT"
rm(PUSH_COUNTER_DATA)
rm(PUSH_COUNTER_DATA3)
rm(agg_PUSH_COUNTER)

################Clustering with just SITE_COUNT ########################
SITE_COUNT_DATA<-alldata_non_apac[,c('SITECOUNT','WON')]
SITE_COUNT_DATA <- as.data.frame(SITE_COUNT_DATA)
SITE_COUNT_DATA<-SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITECOUNT) == TRUE,]
SITE_COUNT_DATA2 <- SITE_COUNT_DATA[((SITE_COUNT_DATA$SITECOUNT >3)),]
a <- boxplot(SITE_COUNT_DATA2$SITECOUNT, data=SITE_COUNT_DATA2)$stats[c(5),1]
SITE_COUNT_DATA[(SITE_COUNT_DATA$SITECOUNT > a),]$SITECOUNT <- a

#adding winrates
agg_SITE_COUNT <- aggregate(WON~SITECOUNT, data=SITE_COUNT_DATA, FUN=mean)
names(agg_SITE_COUNT)[names(agg_SITE_COUNT)=="WON"] <- "SITE_COUNT_WIN_RATE"
SITE_COUNT_DATA <- merge(SITE_COUNT_DATA, agg_SITE_COUNT, by="SITECOUNT", all.x=TRUE)
SITE_COUNT_DATA$WON <- NULL 
SITE_COUNT_DATA <- SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITECOUNT) == TRUE,]
SITE_COUNT_DATA <- SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITE_COUNT_WIN_RATE) == TRUE,]
#nrow(SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITE_COUNT_WIN_RATE) == FALSE,])

#Identify ideal number of clusters to be created
wss <- (length(SITE_COUNT_DATA)-1)*sum(apply(SITE_COUNT_DATA,2,var))
for (i in 1:5) wss[i] <- sum(kmeans(SITE_COUNT_DATA, centers=i)$withinss)
for (i in 1:5) 
{
  if (wss[i] == min(wss))
    num_clusters = i
}

#Fit the clustering model through K means
fit<-kmeans(SITE_COUNT_DATA, as.numeric(num_clusters))

#Profiling the clusters and putting it into the table
cluster<-fit$cluster
SITE_COUNT_DATA3<-cbind(SITE_COUNT_DATA, cluster)
SITE_COUNT_DATA3 <- as.data.frame(SITE_COUNT_DATA3)
SITE_COUNT_DATA3$cluster<-as.factor(SITE_COUNT_DATA3$cluster)

#To merge SITE COUNT with data  
SITE_COUNT_DATA3 <- unique(SITE_COUNT_DATA3)
SITE_COUNT_DATA3$SITE_COUNT_WIN_RATE <- NULL 
alldata_non_apac[((alldata_non_apac$SITECOUNT > a)& (is.na(alldata_non_apac$SITECOUNT)== FALSE)),]$SITECOUNT <- a
alldata_non_apac <- merge(alldata_non_apac,SITE_COUNT_DATA3 ,by="SITECOUNT", all.x=TRUE)
names(alldata_non_apac)[names(alldata_non_apac)=="cluster"] <- "SITE_COUNT_CAT"

#table(alldata_non_apac[!is.na(alldata_non_apac$SITECOUNT) == FALSE,]$SITE_COUNT_CAT)
min_limit <- numeric()
max_limit <- numeric()
for(i in sort(unique(alldata_non_apac$SITE_COUNT_CAT)))
{
  min_limit[i] <- min(alldata_non_apac$SITECOUNT[alldata_non_apac$SITE_COUNT_CAT == i], na.rm = TRUE)
  max_limit[i] <- max(alldata_non_apac$SITECOUNT[alldata_non_apac$SITE_COUNT_CAT == i], na.rm = TRUE)
  if(length(alldata_non_apac[alldata_non_apac$SITECOUNT>=min_limit[i] & alldata_non_apac$SITECOUNT<=max_limit[i] & 
                             (!is.na(alldata_non_apac$SITE_COUNT_CAT) == FALSE) & (!is.na(alldata_non_apac$SITECOUNT) == TRUE),]$SITECOUNT ) > 0)
  {
    alldata_non_apac[alldata_non_apac$SITECOUNT >= min_limit[i] & alldata_non_apac$SITECOUNT <= max_limit[i] & (!is.na(alldata_non_apac$SITE_COUNT_CAT) == FALSE) & (!is.na(alldata_non_apac$SITECOUNT) == TRUE),]$SITE_COUNT_CAT <- i 
  }
}

if (length(alldata_non_apac[(!is.na(alldata_non_apac$SITE_COUNT_CAT) == FALSE) & (!is.na(alldata_non_apac$SITECOUNT) == TRUE),]$SITECOUNT) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$SITE_COUNT_CAT) == FALSE) & (!is.na(alldata_non_apac$SITECOUNT) == TRUE),]$SITE_COUNT_CAT <- alldata_non_apac[alldata_non_apac$SITECOUNT == max(alldata_non_apac$SITECOUNT, na.rm= TRUE),]$SITE_COUNT_CAT
}
rm(SITE_COUNT_DATA)
rm(SITE_COUNT_DATA2)
rm(SITE_COUNT_DATA3)
rm(agg_SITE_COUNT)

################Clustering with just BOOKING########################
BOOKING_DATA<-alldata_non_apac[,c('BOOKING','WON')]
BOOKING_DATA <- as.data.frame(BOOKING_DATA)
BOOKING_DATA<-BOOKING_DATA[!is.na(BOOKING_DATA$BOOKING) == TRUE,]
BOOKING_DATA2 <- BOOKING_DATA[((BOOKING_DATA$BOOKING >0)),]
a <- boxplot(BOOKING_DATA2$BOOKING, data=BOOKING_DATA2)$stats[c(5),1]
BOOKING_DATA[(BOOKING_DATA$BOOKING > a),]$BOOKING <- a

#adding winrates
agg_BOOKING <- aggregate(WON~BOOKING, data=BOOKING_DATA, FUN=mean)
names(agg_BOOKING)[names(agg_BOOKING)=="WON"] <- "BOOKING_WIN_RATE"
BOOKING_DATA <- merge(BOOKING_DATA, agg_BOOKING, by="BOOKING", all.x=TRUE)
BOOKING_DATA$WON <- NULL 
BOOKING_DATA <- BOOKING_DATA[!is.na(BOOKING_DATA$BOOKING) == TRUE,]
BOOKING_DATA <- BOOKING_DATA[!is.na(BOOKING_DATA$BOOKING_WIN_RATE) == TRUE,]
#nrow(SITE_COUNT_DATA[!is.na(SITE_COUNT_DATA$SITE_COUNT_WIN_RATE) == FALSE,])

#Identify ideal number of clusters to be created
wss <- (length(BOOKING_DATA)-1)*sum(apply(BOOKING_DATA,2,var))
for (i in 1:5) wss[i] <- sum(kmeans(BOOKING_DATA, centers=i)$withinss)
for (i in 1:5) 
{
  if (wss[i] == min(wss))
    num_clusters = i
}

#Fit the clustering model through K means
fit<-kmeans(BOOKING_DATA, as.numeric(num_clusters))

#Profiling the clusters and putting it into the table
cluster<-fit$cluster
BOOKING_DATA3<-cbind(BOOKING_DATA, cluster)
BOOKING_DATA3 <- as.data.frame(BOOKING_DATA3)
BOOKING_DATA3$cluster<-as.factor(BOOKING_DATA3$cluster)

#To merge SITE COUNT with data  
BOOKING_DATA3 <- unique(BOOKING_DATA3)
BOOKING_DATA3$BOOKING_WIN_RATE <- NULL 
alldata_non_apac[((alldata_non_apac$BOOKING > a)& (is.na(alldata_non_apac$BOOKING)== FALSE)),]$BOOKING <- a
alldata_non_apac <- merge(alldata_non_apac,BOOKING_DATA3 ,by="BOOKING", all.x=TRUE)
names(alldata_non_apac)[names(alldata_non_apac)=="cluster"] <- "BOOKING_CAT"

#nrow(alldata_non_apac[!is.na(alldata_non_apac$BOOKING) == FALSE,])
min_limit <- numeric()
max_limit <- numeric()
for(i in sort(unique(alldata_non_apac$BOOKING_CAT)))
{
  min_limit[i] <- min(alldata_non_apac$BOOKING[alldata_non_apac$BOOKING_CAT == i], na.rm = TRUE)
  max_limit[i] <- max(alldata_non_apac$BOOKING[alldata_non_apac$BOOKING_CAT == i], na.rm = TRUE)
  if(length(alldata_non_apac[alldata_non_apac$BOOKING>=min_limit[i] & alldata_non_apac$BOOKING<=max_limit[i] & 
                             (!is.na(alldata_non_apac$BOOKING_CAT) == FALSE) & (!is.na(alldata_non_apac$BOOKING) == TRUE),]$BOOKING ) > 0)
  {
    alldata_non_apac[alldata_non_apac$BOOKING >= min_limit[i] & alldata_non_apac$BOOKING <= max_limit[i] & (!is.na(alldata_non_apac$BOOKING_CAT) == FALSE) & (!is.na(alldata_non_apac$BOOKING) == TRUE),]$BOOKING_CAT <- i 
  }
}

#if (length(alldata_non_apac[(!is.na(alldata_non_apac$BOOKING_CAT) == FALSE) & (!is.na(alldata_non_apac$BOOKING) == TRUE),]$BOOKING) > 0) 
#{
#  alldata_non_apac[(!is.na(alldata_non_apac$BOOKING_CAT) == FALSE) & (!is.na(alldata_non_apac$BOOKING) == TRUE),]$BOOKING_CAT <- alldata_non_apac[alldata_non_apac$BOOKING == max(alldata_non_apac$BOOKING, na.rm= TRUE),]$BOOKING_CAT
#}
rm(BOOKING_DATA)
rm(BOOKING_DATA2)
rm(BOOKING_DATA3)
rm(agg_BOOKING)

alldata_non_apac$TOTAL_STAGE <- 0
alldata_non_apac$TOTAL_STAGE <- apply(alldata_non_apac[,c('STAGE_5_DURATION__C','STAGE_0_TIME__C','STAGE_1_TIME__C','STAGE_2_TIME__C','STAGE_3_TIME__C',
                                                          'STAGE_4_TIME__C')],1,sum, na.rm= TRUE)

#Breaking in percentiles
set.seed(700)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(temp_alldata_non_apac$TOTAL_STAGE, probs = seq(0, 1, by = 0.10))), 
      labels=c("Stage_duration_0_10_percentile","Stage_duration_10_20_percentile",
               "Stage_duration_20_30_percentile","Stage_duration_30_40_percentile",
               "Stage_duration_40_50_percentile","Stage_duration_50_60_percentile",
               "Stage_duration_60_70_percentile","Stage_duration_70_80_percentile",
               "Stage_duration_80_90_percentile","Stage_duration_90_100_percentile"
      ), include.lowest=TRUE)
}

#Add the quintile to the dataframe
temp_alldata_non_apac<- alldata_non_apac[,c('OPPID','TOTAL_STAGE')]
temp_alldata_non_apac$TOTAL_STAGE_GRP <- sapply(temp_alldata_non_apac$TOTAL_STAGE, ApplyQuintiles)
temp_alldata_non_apac$TOTAL_STAGE <- NULL
alldata_non_apac <- merge(alldata_non_apac,temp_alldata_non_apac ,by="OPPID", all.x=TRUE)
#table(temp_alldata_non_apac$TOTAL_STAGE_GRP)

#Adding variable GEN_WINRATE
GEN_WINRATE <- mean(alldata_non_apac$WON ,na.rm = TRUE)

#Adding winrates for each column 
agg_USERNAME <- aggregate(WON~username, data=alldata_non_apac, FUN=mean)
names(agg_USERNAME)[names(agg_USERNAME)=="WON"] <- "REP_P"
alldata_non_apac <- merge(alldata_non_apac, agg_USERNAME, by="username", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$REP_P) == FALSE),]$REP_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$REP_P) == FALSE),]$REP_P <- GEN_WINRATE
}
rm(agg_USERNAME)

agg_NUM_STALLS <- aggregate(WON~NUM_STALLS, data=alldata_non_apac, FUN=mean)
names(agg_NUM_STALLS)[names(agg_NUM_STALLS)=="WON"] <- "NUM_STALLS_P"
alldata_non_apac <- merge(alldata_non_apac, agg_NUM_STALLS, by="NUM_STALLS", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$NUM_STALLS_P) == FALSE),]$NUM_STALLS_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$NUM_OF_STALLS_P) == FALSE),]$NUM_OF_STALLS_P <- GEN_WINRATE
}
rm(agg_NUM_OF_STALLS)

agg_Branch_geo_region <- aggregate(WON~branch_geo_region, data=alldata_non_apac, FUN=mean)
names(agg_Branch_geo_region)[names(agg_Branch_geo_region)=="WON"] <- "BRANCH_GEO_REGION_P"
alldata_non_apac <- merge(alldata_non_apac, agg_Branch_geo_region, by="branch_geo_region", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$BRANCH_GEO_REGION_P) == FALSE),]$BRANCH_GEO_REGION_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$BRANCH_GEO_REGION_P) == FALSE),]$BRANCH_GEO_REGION_P <- GEN_WINRATE
}
rm(agg_Branch_geo_region)

agg_BOOKING <- aggregate(WON~BOOKING_CAT, data=alldata_non_apac, FUN=mean)
names(agg_BOOKING)[names(agg_BOOKING)=="WON"] <- "BOOKING_P"
alldata_non_apac <- merge(alldata_non_apac, agg_BOOKING, by="BOOKING_CAT", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$BOOKING_P) == FALSE),]$BOOKING_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$BOOKING_P) == FALSE),]$BOOKING_P <- GEN_WINRATE
}
rm(agg_BOOKING)

agg_SITECOUNT <- aggregate(WON~SITE_COUNT_CAT, data=alldata_non_apac, FUN=mean)
names(agg_SITECOUNT)[names(agg_SITECOUNT)=="WON"] <- "SITECOUNT_P"
alldata_non_apac <- merge(alldata_non_apac, agg_SITECOUNT, by="SITE_COUNT_CAT", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$SITECOUNT) == FALSE),]$SITECOUNT) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$SITECOUNT) == FALSE),]$SITECOUNT_P <- mean(alldata_non_apac[(!is.na(alldata_non_apac$SITECOUNT) == FALSE),]$WON ,na.rm = TRUE)
}
rm(agg_SITECOUNT)

agg_TOTAL_STAGE_GRP <- aggregate(WON~TOTAL_STAGE_GRP, data=alldata_non_apac, FUN=mean)
names(agg_TOTAL_STAGE_GRP)[names(agg_TOTAL_STAGE_GRP)=="WON"] <- "TOTAL_STAGE_GRP_P"
alldata_non_apac <- merge(alldata_non_apac, agg_TOTAL_STAGE_GRP, by="TOTAL_STAGE_GRP", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$TOTAL_STAGE_GRP_P) == FALSE),]$TOTAL_STAGE_GRP_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$TOTAL_STAGE_GRP_P) == FALSE),]$TOTAL_STAGE_GRP_P <- GEN_WINRATE
}
rm(agg_TOTAL_STAGE_GRP)

agg_STALLED_STAGE <- aggregate(WON~STALLED_STAGE, data=alldata_non_apac, FUN=mean)
names(agg_STALLED_STAGE)[names(agg_STALLED_STAGE)=="WON"] <- "STALL_STAGE_P"
alldata_non_apac <- merge(alldata_non_apac, agg_STALLED_STAGE, by="STALLED_STAGE", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$STALL_STAGE_P) == FALSE),]$STALL_STAGE_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$STALL_STAGE_P) == FALSE),]$STALL_STAGE_P <- GEN_WINRATE
}
rm(agg_STALLED_STAGE)

agg_VERTICAL <- aggregate(WON~VERTICAL_RPT_DESC2, data=alldata_non_apac, FUN=mean)
names(agg_VERTICAL)[names(agg_VERTICAL)=="WON"] <- "VERTICAL_P"
alldata_non_apac <- merge(alldata_non_apac, agg_VERTICAL, by="VERTICAL_RPT_DESC2", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$VERTICAL_P) == FALSE),]$VERTICAL_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$VERTICAL_P) == FALSE),]$VERTICAL_P <- GEN_WINRATE
}
rm(agg_VERTICAL)

agg_PRODUCTS <- aggregate(WON~PRODUCTS, data=alldata_non_apac, FUN=mean)
names(agg_PRODUCTS)[names(agg_PRODUCTS)=="WON"] <- "PRODUCTS_P"
alldata_non_apac <- merge(alldata_non_apac, agg_PRODUCTS, by="PRODUCTS", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$PRODUCTS_P) == FALSE),]$PRODUCTS_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$PRODUCTS_P) == FALSE),]$PRODUCTS_P <- GEN_WINRATE
}
rm(agg_PRODUCTS)

agg_PUSH_COUNTER_CAT <- aggregate(WON~PUSH_COUNTER_CAT, data=alldata_non_apac, FUN=mean)
names(agg_PUSH_COUNTER_CAT)[names(agg_PUSH_COUNTER_CAT)=="WON"] <- "PUSH_COUNTER_CAT_P"
alldata_non_apac <- merge(alldata_non_apac, agg_PUSH_COUNTER_CAT, by="PUSH_COUNTER_CAT", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$PUSH_COUNTER_CAT_P) == FALSE),]$PUSH_COUNTER_CAT_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$PUSH_COUNTER_CAT_P) == FALSE),]$PUSH_COUNTER_CAT_P <- GEN_WINRATE
}
rm(agg_PUSH_COUNTER_CAT)

agg_COMPETITOR <- aggregate(WON~COMPETITOR, data=alldata_non_apac, FUN=mean)
names(agg_COMPETITOR)[names(agg_COMPETITOR)=="WON"] <- "COMPETITOR_P"
alldata_non_apac <- merge(alldata_non_apac, agg_COMPETITOR, by="COMPETITOR", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$COMPETITOR_P) == FALSE),]$COMPETITOR_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$COMPETITOR_P) == FALSE),]$COMPETITOR_P <- GEN_WINRATE
}
rm(agg_COMPETITOR)

agg_CSM_TIER_CODE <- aggregate(WON~CSM_TIER_CODE, data=alldata_non_apac, FUN=mean)
names(agg_CSM_TIER_CODE)[names(agg_CSM_TIER_CODE)=="WON"] <- "CSM_P"
alldata_non_apac <- merge(alldata_non_apac, agg_CSM_TIER_CODE, by="CSM_TIER_CODE", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$CSM_P) == FALSE),]$CSM_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$CSM_P) == FALSE),]$CSM_P <- GEN_WINRATE
}
rm(agg_CSM_TIER_CODE)


agg_NASP_TYPE<- aggregate(WON~nasp_type, data=alldata_non_apac, FUN=mean)
names(agg_NASP_TYPE)[names(agg_NASP_TYPE)=="WON"] <- "NASP_TYPE_P"
alldata_non_apac <- merge(alldata_non_apac, agg_NASP_TYPE, by="nasp_type", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$NASP_TYPE_P) == FALSE),]$NASP_TYPE_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$NASP_TYPE_P) == FALSE),]$NASP_TYPE_P <- GEN_WINRATE
}
rm(agg_NASP_TYPE)

agg_NASPID<- aggregate(WON~NASP_ID, data=alldata_non_apac, FUN=mean)
names(agg_NASPID)[names(agg_NASPID)=="WON"] <- "NASPID_P"
alldata_non_apac <- merge(alldata_non_apac, agg_NASPID, by="NASP_ID", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$NASPID_P) == FALSE),]$NASPID_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$NASPID_P) == FALSE),]$NASPID_P <- GEN_WINRATE
}
rm(agg_NASPID)

agg_PROD_COUNT<- aggregate(WON~NUM_OF_PRODUCTS, data=alldata_non_apac, FUN=mean)
names(agg_PROD_COUNT)[names(agg_PROD_COUNT)=="WON"] <- "PROD_COUNT_P"
alldata_non_apac <- merge(alldata_non_apac, agg_PROD_COUNT, by="NUM_OF_PRODUCTS", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$PROD_COUNT_P) == FALSE),]$PROD_COUNT_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$PROD_COUNT_P) == FALSE),]$PROD_COUNT_P <- GEN_WINRATE
}
rm(agg_PROD_COUNT)

agg_PRIMARY_COUNTRY<- aggregate(WON~PRIMARY_COUNTRY, data=alldata_non_apac, FUN=mean)
names(agg_PRIMARY_COUNTRY)[names(agg_PRIMARY_COUNTRY)=="WON"] <- "PRIMARY_COUNTRY_P"
alldata_non_apac <- merge(alldata_non_apac, agg_PRIMARY_COUNTRY, by="PRIMARY_COUNTRY", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$PRIMARY_COUNTRY_P) == FALSE),]$PRIMARY_COUNTRY_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$PRIMARY_COUNTRY_P) == FALSE),]$PRIMARY_COUNTRY_P <- GEN_WINRATE
}
rm(agg_PRIMARY_COUNTRY)


agg_TERM<- aggregate(WON~TERM, data=alldata_non_apac, FUN=mean)
names(agg_TERM)[names(agg_TERM)=="WON"] <- "TERM_P"
alldata_non_apac <- merge(alldata_non_apac, agg_TERM, by="TERM", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$TERM_P) == FALSE),]$TERM_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$TERM_P) == FALSE),]$TERM_P <- GEN_WINRATE
}
rm(agg_TERM)

agg_OPPTYPE<- aggregate(WON~OPPTYPE, data=alldata_non_apac, FUN=mean)
names(agg_OPPTYPE)[names(agg_OPPTYPE)=="WON"] <- "OPPTYPE_P"
alldata_non_apac <- merge(alldata_non_apac, agg_OPPTYPE, by="OPPTYPE", all.x=TRUE)
if (length(alldata_non_apac[(!is.na(alldata_non_apac$OPPTYPE_P) == FALSE),]$OPPTYPE_P) > 0) 
{
  alldata_non_apac[(!is.na(alldata_non_apac$OPPTYPE_P) == FALSE),]$OPPTYPE_P <- GEN_WINRATE
}
rm(agg_OPPTYPE)

reg_tab<- alldata_non_apac[,c('OPPID','OPPTYPE_P','TERM_P','PRIMARY_COUNTRY_P','PROD_COUNT_P','NASPID_P','NASP_TYPE_P',
                              'CSM_P','COMPETITOR_P','PUSH_COUNTER_CAT_P','VERTICAL_P','STAGE_CONVERSION_P','PRODUCTS_P', 
                              'BOOKING_P','REP_P','TOTAL_STAGE_GRP_P', 'STALL_STAGE_P', 'WON', 'SITECOUNT_P' , 
                              'BRANCH_GEO_REGION_P','NUM_STALLS_P','MODEL_IND')]
reg_tab <- as.data.frame(reg_tab)

#form train and test data 
Training_Data <- reg_tab[(!is.na(reg_tab$WON) == TRUE),]
Test_Data <- reg_tab[(!is.na(reg_tab$WON) == FALSE),]
dbhandle <- odbcDriverConnect("dsn=AJ_prod_latest;uid=v683623;pwd=pokemon16;")
OPP_CLOSED_SINCE_NOV_END <- sqlQuery(dbhandle, "SELECT OPPID,STAGENAME FROM SND_OFFSHORE_ANALYTICS.FUNNEL_OPPS_CLOSED_SINCE_8NOV2017 where STAGENAME like '%CLOSED%'")

OPP_CLOSED_SINCE_NOV_END <- as.data.frame(OPP_CLOSED_SINCE_NOV_END)
names(OPP_CLOSED_SINCE_NOV_END)[names(OPP_CLOSED_SINCE_NOV_END)=="STAGENAME"] <- "FINAL_STAGE"
Test_Data <- merge(Test_Data,OPP_CLOSED_SINCE_NOV_END ,by="OPPID")

Training_DataS <- Training_Data[Training_Data$MODEL_IND == 'S',];
Training_DataM <- Training_Data[Training_Data$MODEL_IND == 'M',];
Training_DataL <- Training_Data[Training_Data$MODEL_IND == 'L',];
Test_DataS <- Test_Data[Test_Data$MODEL_IND == 'S',];
Test_DataM <- Test_Data[Test_Data$MODEL_IND == 'M',];
Test_DataL <- Test_Data[Test_Data$MODEL_IND == 'L',];

# COMPUTING THE REGRESSION CO-EFFICIENTS
Data_ModelS <- step(glm(WON ~ OPPTYPE_P + TERM_P + PRIMARY_COUNTRY_P + PROD_COUNT_P + NASPID_P + NASP_TYPE_P + CSM_P + COMPETITOR_P + PUSH_COUNTER_CAT_P + VERTICAL_P + PRODUCTS_P + REP_P + TOTAL_STAGE_GRP_P + STAGE_CONVERSION_P + STALL_STAGE_P + SITECOUNT_P + BOOKING_P + NUM_STALLS_P + BRANCH_GEO_REGION_P ,family=binomial, data=Training_DataS))
Data_ModelM <- step(glm(WON ~ OPPTYPE_P + TERM_P + PRIMARY_COUNTRY_P + PROD_COUNT_P + NASPID_P + NASP_TYPE_P + CSM_P + COMPETITOR_P + PUSH_COUNTER_CAT_P + VERTICAL_P + PRODUCTS_P + REP_P + TOTAL_STAGE_GRP_P + STAGE_CONVERSION_P + STALL_STAGE_P + SITECOUNT_P + BOOKING_P + NUM_STALLS_P + BRANCH_GEO_REGION_P ,family=binomial, data=Training_DataM))
Data_ModelL <- step(glm(WON ~ OPPTYPE_P + TERM_P + PRIMARY_COUNTRY_P + PROD_COUNT_P + NASPID_P + NASP_TYPE_P + CSM_P + COMPETITOR_P + PUSH_COUNTER_CAT_P + VERTICAL_P + PRODUCTS_P + REP_P + TOTAL_STAGE_GRP_P + STAGE_CONVERSION_P + STALL_STAGE_P + SITECOUNT_P + BOOKING_P + NUM_STALLS_P + BRANCH_GEO_REGION_P ,family=binomial, data=Training_DataL))

#library(car)
#xyz <-  vif(Data_Model)
#summ <-summary(Data_Model)

vbookS <- data.frame(oppid = Test_DataS$OPPID, oppscore = 1/(1+exp(-(predict(Data_ModelS,Test_DataS)))), final_stage = Test_DataS$FINAL_STAGE) # The prediction is performed on the test data based on the logistic regression formula 1/(1 + e^-f(x)).
vbookM <- data.frame(oppid = Test_DataM$OPPID, oppscore = 1/(1+exp(-(predict(Data_ModelM,Test_DataM)))), final_stage = Test_DataM$FINAL_STAGE) # The prediction is performed on the test data based on the logistic regression formula 1/(1 + e^-f(x)).
vbookL <- data.frame(oppid = Test_DataL$OPPID, oppscore = 1/(1+exp(-(predict(Data_ModelL,Test_DataL)))), final_stage = Test_DataL$FINAL_STAGE) # The prediction is performed on the test data based on the logistic regression formula 1/(1 + e^-f(x)).
vbook <- rbind(vbookS,vbookM,vbookL)

vbook$oppscore_category <-ifelse(vbook$oppscore< 0.35,"Low probability",
                                 ifelse(vbook$oppscore <0.75,"Medium probablity",
                                        ifelse(vbook$oppscore <=1,"High probability","unpredictable")))
count <-xtabs(~vbook$final_stage+vbook$oppscore_category, data=vbook)
total_accuracy <- (count[3,1]+count[1,2]+count[2,2])/(count[3,1]+count[2,1]+count[1,1]+count[1,2]+count[2,2]+count[3,2])
win_accuracy <- (count[3,1])/(count[3,1]+count[2,1]+count[1,1])
loss_accuracy <-(count[1,2]+count[2,2])/(count[1,2]+count[2,2]+count[3,2])

# > total_accuracy
# [1] 0.814524
# > win_accuracy
# [1] 0.8414239
# > loss_accuracy
# [1] 0.8028169

# add all required columns to vbook_non_apac
names(alldata_non_apac_back)
names(vbook)
nrow(vbook)
# change name from oppid to OPPID
colnames(vbook)[1] <- "OPPID"
head(alldata_non_apac_back)
vbook_non_apac<-merge(x = vbook, 
                y = alldata_non_apac_back[ , c("BOOKING", "EXP_SPA_DATE","forecast_category","STAGENAME","OPPID")], 
                by = "OPPID", all.x=TRUE)

if (weekdays(Sys.Date())=="Monday")
{
for(row in seq_len(nrow(vbook_non_apac))){
  query <- paste0(
    "INSERT INTO SND_OFFSHORE_ANALYTICS.FUNNEL_TRK_OUTPUT_FINAL
(RPT_DATE, OPPID, BI_PROB_SCORE, BI_PROB_CATEGORY, BOOKING, FORECAST_CATEGORY, STAGENAME, EXP_SPA_DATE)
    VALUES ( CURRENT_DATE,
    '", vbook_non_apac$OPPID[row],"', 
    '", vbook_non_apac$oppscore[row],"', 
    '", vbook_non_apac$oppscore_category[row],"', 
    '", vbook_non_apac$BOOKING[row],"', 
    '", vbook_non_apac$forecast_category[row],"', 
    '", vbook_non_apac$STAGENAME[row],"', 
    '", vbook_non_apac$EXP_SPA_DATE[row],"'
    )"
  )
  sqlQuery(dbhandle, query)
}
}
#View(vbook_non_apac)
sqlQuery(dbhandle, "DELETE FROM SND_OFFSHORE_ANALYTICS.FUNNEL_OUTPUT_FINAL_D")

for(row in seq_len(nrow(vbook_non_apac))){
  query <- paste0(
    "INSERT INTO SND_OFFSHORE_ANALYTICS.FUNNEL_OUTPUT_FINAL_D
    (OPPID, BI_PROB_SCORE, BI_PROB_CATEGORY) VALUES 
    ( 
    '", vbook_non_apac$OPPID[row],"', 
    '", vbook_non_apac$oppscore[row],"', 
    '", vbook_non_apac$oppscore_category[row],"'
    )"
  )
  sqlQuery(dbhandle, query)
}
odbcClose(dbhandle)
# INSERT INTO SND_OFFSHORE_ANALYTICS.FUNNEL_TRK_OUTPUT_FINAL (RPT_DATE, OPPID, BI_PROB_SCORE, BI_PROB_CATEGORY, BOOKING, FORECAST_CATEGORY, STAGENAME. EXP_SPA_DATE) VALUES ();
# 
# DELETE FROM SND_OFFSHORE_ANALYTICS.FUNNEL_OUTPUT_FINAL_D;
# 
# INSERT INTO SND_OFFSHORE_ANALYTICS.FUNNEL_OUTPUT_FINAL_D (OPPID, BI_PROB_SCORE, BI_PROB_CATEGORY, BI_TOP_RISK_FACTORS) VALUES ();