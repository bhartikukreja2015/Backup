# tg 2018 code #
rm(list=ls(all=T))
setwd("H:\\kukrebh\\gola-done\\tg18\\DS")

# ------------step 1 read all training csv files------------#
DEO_B_DATA<-read.csv("Deodorant-B.CSV")
DEO_F_DATA<-read.csv("Deodorant-F.CSV")
DEO_G_DATA<-read.csv("Deodorant-G.CSV")
DEO_H_DATA<-read.csv("Deodorant-H.CSV")
DEO_J_DATA<-read.csv("Deodorant-J.CSV")

NAMES<-as.data.frame(names(DEO_B_DATA))
NAMES$F_NAMES<-names(DEO_F_DATA)
NAMES$G_NAMES<-names(DEO_G_DATA)
NAMES$H_NAMES<-names(DEO_H_DATA)
NAMES$J_NAMES<-names(DEO_J_DATA)

# ------------step 2 join them all in 1 file------------#
# before binding deal with q8 columns
# 1 2 5 6 7 8 9 10  11 12 13 17 18 19 20 
DEO_B_DATA$q8.7 <- '0'
DEO_B_DATA$q8.9 <- '0'
DEO_B_DATA$q8.10 <- '0'
DEO_B_DATA$q8.17 <- '0'
DEO_B_DATA$q8.18 <- '0'

DEO_F_DATA$q8.2 <- '0'
DEO_F_DATA$q8.8 <- '0'
DEO_F_DATA$q8.10 <- '0'
DEO_F_DATA$q8.12 <- '0'
DEO_F_DATA$q8.17 <- '0'

DEO_G_DATA$q8.8 <- '0'
DEO_G_DATA$q8.9 <- '0'
DEO_G_DATA$q8.10 <- '0'
DEO_G_DATA$q8.18 <- '0'
DEO_G_DATA$q8.20 <- '0'

DEO_H_DATA$q8.8 <- '0'
DEO_H_DATA$q8.9 <- '0'
DEO_H_DATA$q8.10 <- '0'
DEO_H_DATA$q8.17 <- '0'
DEO_H_DATA$q8.18 <- '0'

DEO_J_DATA$q8.2 <- '0'
DEO_J_DATA$q8.8 <- '0'
DEO_J_DATA$q8.9 <- '0'
DEO_J_DATA$q8.17 <- '0'
DEO_J_DATA$q8.20 <- '0'


# deal with s13.2,6,8,10
colnames(DEO_B_DATA)[colnames(DEO_B_DATA)=="s13.2"] <- "frag_usd_past_6_mths"
colnames(DEO_F_DATA)[colnames(DEO_F_DATA)=="s13.6"] <- "frag_usd_past_6_mths"
colnames(DEO_G_DATA)[colnames(DEO_G_DATA)=="s13.7"] <- "frag_usd_past_6_mths"
colnames(DEO_H_DATA)[colnames(DEO_H_DATA)=="s13.8"] <- "frag_usd_past_6_mths"
colnames(DEO_J_DATA)[colnames(DEO_J_DATA)=="s13.10"] <- "frag_usd_past_6_mths"

# deal with s13a.b.most.often
colnames(DEO_B_DATA)[colnames(DEO_B_DATA)=="s13a.b.most.often"] <- "most_often"
colnames(DEO_F_DATA)[colnames(DEO_F_DATA)=="s13a.f.most.often"] <- "most_often"
colnames(DEO_G_DATA)[colnames(DEO_G_DATA)=="s13a.g.most.often"] <- "most_often"
colnames(DEO_H_DATA)[colnames(DEO_H_DATA)=="s13a.h.most.often"] <- "most_often"
colnames(DEO_J_DATA)[colnames(DEO_J_DATA)=="s13a.j.most.often"] <- "most_often"

# combine
Complete_DATA<- rbind(DEO_B_DATA,DEO_F_DATA)
Complete_DATA_2<-rbind(Complete_DATA,DEO_G_DATA)
Complete_DATA_3<-rbind(Complete_DATA_2,DEO_H_DATA)
Complete_DATA_4<-rbind(Complete_DATA_3,DEO_J_DATA)
nrow(Complete_DATA_4)

#------------step 3 clean and rename factors------------#
Complete_DATA_4$Respondent_id_1<-as.factor(Complete_DATA_4$Respondent.ID)

Complete_DATA_4$Product_id_2<-as.factor(Complete_DATA_4$Product.ID)

Complete_DATA_4$Instant_liking_target <-as.factor(Complete_DATA_4$Instant.Liking )

library(toOrdinal)
Complete_DATA_4$personal_opinion_3<-  toOrdinal(Complete_DATA_4$q1_1.personal.opinion.of.this.Deodorant )

Complete_DATA_4$all_words_4<-as.factor(Complete_DATA_4$q2_all.words)

Complete_DATA_4$strength_of_deo_5<-  toOrdinal(Complete_DATA_4$q3_1.strength.of.the.Deodorant )

Complete_DATA_4$artifical_chemical_6<-as.factor(Complete_DATA_4$q4_1.artificial.chemical)
Complete_DATA_4$attractive_6<-as.factor(Complete_DATA_4$q4_2.attractive)
Complete_DATA_4$bold_6<-as.factor(Complete_DATA_4$q4_3.bold)
Complete_DATA_4$boring_6<-as.factor(Complete_DATA_4$q4_4.boring)
Complete_DATA_4$casual_6<-as.factor(Complete_DATA_4$q4_5.casual)
Complete_DATA_4$cheap_6<-as.factor(Complete_DATA_4$q4_6.cheap)
Complete_DATA_4$clean_6<-as.factor(Complete_DATA_4$q4_7.clean)
Complete_DATA_4$easy_to_wear_6<-as.factor(Complete_DATA_4$q4_8.easy.to.wear)
Complete_DATA_4$elegant_6<-as.factor(Complete_DATA_4$q4_9.elegant)
Complete_DATA_4$feminine_6<-as.factor(Complete_DATA_4$q4_10.feminine)
Complete_DATA_4$forsomeonelikeme_6<-as.factor(Complete_DATA_4$q4_11.for.someone.like.me)
Complete_DATA_4$heavy_6<-as.factor(Complete_DATA_4$q4_12.heavy)
Complete_DATA_4$high_quality_6<-as.factor(Complete_DATA_4$q4_13.high.quality)
Complete_DATA_4$long_lasting_6<-as.factor(Complete_DATA_4$q4_14.long.lasting)
Complete_DATA_4$masculine_6<-as.factor(Complete_DATA_4$q4_15.masculine)
Complete_DATA_4$memorable_6<-as.factor(Complete_DATA_4$q4_16.memorable)
Complete_DATA_4$natural_6<-as.factor(Complete_DATA_4$q4_17.natural)
Complete_DATA_4$old_fashioned_6<-as.factor(Complete_DATA_4$q4_18.old.fashioned)
Complete_DATA_4$ordinary_6<-as.factor(Complete_DATA_4$q4_19.ordinary)
Complete_DATA_4$forsomeonelikeme_6<-as.factor(Complete_DATA_4$q4_20.overpowering)
Complete_DATA_4$heavy_6<-as.factor(Complete_DATA_4$q4_21.sharp)
Complete_DATA_4$high_quality_6<-as.factor(Complete_DATA_4$q4_22.sophisticated)
Complete_DATA_4$long_lasting_6<-as.factor(Complete_DATA_4$q4_23.upscale)
Complete_DATA_4$masculine_6<-as.factor(Complete_DATA_4$q4_24.well.rounded)

Complete_DATA_4$Deo_is_addictive_7<-  toOrdinal(Complete_DATA_4$q5_1.Deodorant.is.addictive )

Complete_DATA_4$q7_8<-  toOrdinal(Complete_DATA_4$q7)

Complete_DATA_4$q81_9<-as.factor(Complete_DATA_4$q8.1)
Complete_DATA_4$q82_9<-as.factor(Complete_DATA_4$q8.2)
Complete_DATA_4$q85_9<-as.factor(Complete_DATA_4$q8.5)
Complete_DATA_4$q86_9<-as.factor(Complete_DATA_4$q8.6)
Complete_DATA_4$q87_9<-as.factor(Complete_DATA_4$q8.7)
Complete_DATA_4$q88_9<-as.factor(Complete_DATA_4$q8.8)
Complete_DATA_4$q89_9<-as.factor(Complete_DATA_4$q8.9)
Complete_DATA_4$q810_9<-as.factor(Complete_DATA_4$q8.10)
Complete_DATA_4$q811_9<-as.factor(Complete_DATA_4$q8.11)
Complete_DATA_4$q812_9<-as.factor(Complete_DATA_4$q8.12)
Complete_DATA_4$q813_9<-as.factor(Complete_DATA_4$q8.13)
Complete_DATA_4$q817_9<-as.factor(Complete_DATA_4$q8.17)
Complete_DATA_4$q818_9<-as.factor(Complete_DATA_4$q8.18)
Complete_DATA_4$q819_9<-as.factor(Complete_DATA_4$q8.19)
Complete_DATA_4$q820_9<-as.factor(Complete_DATA_4$q8.20)

Complete_DATA_4$how_likely_purchase_10  <-toOrdinal(Complete_DATA_4$q9.how.likely.would.you.be.to.purchase.this.Deodorant)

Complete_DATA_4$prefer_this_or_usual_11  <-toOrdinal(Complete_DATA_4$q10.prefer.this.Deodorant.or.your.usual.Deodorant)

Complete_DATA_4$time_of_day_12 <-as.factor(Complete_DATA_4$q11.time.of.day.would.this.Deodorant.be.appropriate)

Complete_DATA_4$occasions_13 <-as.factor(Complete_DATA_4$q12.which.occasions.would.this.Deodorant.be.appropriate)

Complete_DATA_4$liking_after30min_14 <-toOrdinal(Complete_DATA_4$Q13_Liking.after.30.minutes)

Complete_DATA_4$overall_15 <-toOrdinal(Complete_DATA_4$q14.Deodorant.overall.on.a.scale.from.1.to.10)

Complete_DATA_4$ValSegb_16 <-as.factor(Complete_DATA_4$ValSegb)

Complete_DATA_4$involvement_17 <-toOrdinal(Complete_DATA_4$s7.involved.in.the.selection.of.the.cosmetic.products)

Complete_DATA_4$ethnic_18 <-as.factor(Complete_DATA_4$s8.ethnic.background)

Complete_DATA_4$education_19 <-as.factor(Complete_DATA_4$s9.education)

Complete_DATA_4$income_20 <-toOrdinal(Complete_DATA_4$s10.income)

Complete_DATA_4$m_status_21 <-as.factor(Complete_DATA_4$s11.marital.status)

Complete_DATA_4$w_status_22 <-as.factor(Complete_DATA_4$s12.working.status)

Complete_DATA_4$frag_usd_past_6_mths_23 <-(Complete_DATA_4$frag_usd_past_6_mths)

Complete_DATA_4$most_often_24 <-as.factor(Complete_DATA_4$most_often)

Complete_DATA_4$bottles_own_25 <-toOrdinal(Complete_DATA_4$s13b.bottles.of.Deodorant.do.you.currently.own)

Complete_DATA_4$Product_26 <-(Complete_DATA_4$Product)

#------------step 4 remove old factor columns and filter out 4 training data sets again ---------#
names(Complete_DATA_4)
drops <- c( "Respondent.ID","Product.ID","Product","Instant.Liking","q1_1.personal.opinion.of.this.Deodorant","q2_all.words",                                           
            "q3_1.strength.of.the.Deodorant","q4_1.artificial.chemical","q4_2.attractive","q4_3.bold","q4_4.boring",
            "q4_5.casual" ,"q4_6.cheap","q4_7.clean","q4_8.easy.to.wear","q4_9.elegant","q4_10.feminine","q4_11.for.someone.like.me",
            "q4_12.heavy","q4_13.high.quality","q4_14.long.lasting","q4_15.masculine","q4_16.memorable","q4_17.natural","q4_18.old.fashioned",
            "q4_19.ordinary","q4_20.overpowering","q4_21.sharp","q4_22.sophisticated","q4_23.upscale","q4_24.well.rounded","q5_1.Deodorant.is.addictive",
            "q7","q8.1","q8.2","q8.5","q8.6","q8.8","q8.11","q8.12" ,"q8.13","q8.19","q8.20","q9.how.likely.would.you.be.to.purchase.this.Deodorant",
            "q10.prefer.this.Deodorant.or.your.usual.Deodorant","q11.time.of.day.would.this.Deodorant.be.appropriate","q12.which.occasions.would.this.Deodorant.be.appropriate",
            "Q13_Liking.after.30.minutes","q14.Deodorant.overall.on.a.scale.from.1.to.10","ValSegb","s7.involved.in.the.selection.of.the.cosmetic.products",
            "s8.ethnic.background","s9.education","s10.income","s11.marital.status","s12.working.status","frag_usd_past_6_mths","most_often",
            "s13b.bottles.of.Deodorant.do.you.currently.own","q8.7","q8.9","q8.10","q8.17","q8.18"    
            )

Comp_Data_filtered<-Complete_DATA_4[ , !(names(Complete_DATA_4) %in% drops)]
names(Comp_Data_filtered)

DEO_B_DATA_Filtered<- subset( Comp_Data_filtered, Product_26 == "Deodorant B") 
DEO_F_DATA_Filtered<- subset( Comp_Data_filtered, Product_26 == "Deodorant F")
DEO_G_DATA_Filtered<- subset( Comp_Data_filtered, Product_26 == "Deodorant G")
DEO_H_DATA_Filtered<- subset( Comp_Data_filtered, Product_26 == "Deodorant H") 
DEO_J_DATA_Filtered<- subset( Comp_Data_filtered, Product_26 == "Deodorant J") 

str(Comp_Data_filtered)
Comp_Data_filtered[,'involvement_17']<-NULL
Comp_Data_filtered[,'Respondent_id_1']<-NULL
comp_mod<-glm(Instant_liking_target ~  . ,
             family=binomial, data=Comp_Data_filtered,control = list(maxit = 100))
comp_mod
# load package glmnet in R
#https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
Test_Data<-read.csv("test-data.CSV")
str(Test_Data)
predict(comp_mod,Test_Data)
# drop unnecessary columns 
DEO_B_DATA_Filtered[,'q87_9']<-NULL
DEO_B_DATA_Filtered[,'q89_9']<-NULL
DEO_B_DATA_Filtered[,'q810_9']<-NULL
DEO_B_DATA_Filtered[,'q817_9']<-NULL
DEO_B_DATA_Filtered[,'q818_9']<-NULL

DEO_F_DATA_Filtered[,'q82_9']<-NULL
DEO_F_DATA_Filtered[,'q88_9']<-NULL
DEO_F_DATA_Filtered[,'q810_9']<-NULL
DEO_F_DATA_Filtered[,'q812_9']<-NULL
DEO_F_DATA_Filtered[,'q817_9']<-NULL

DEO_G_DATA_Filtered[,'q88_9']<-NULL
DEO_G_DATA_Filtered[,'q89_9']<-NULL
DEO_G_DATA_Filtered[,'q810_9']<-NULL
DEO_G_DATA_Filtered[,'q818_9']<-NULL
DEO_G_DATA_Filtered[,'q820_9']<-NULL

DEO_H_DATA_Filtered[,'q88_9']<-NULL
DEO_H_DATA_Filtered[,'q89_9']<-NULL
DEO_H_DATA_Filtered[,'q810_9']<-NULL
DEO_H_DATA_Filtered[,'q817_9']<-NULL
DEO_H_DATA_Filtered[,'q818_9']<-NULL

DEO_J_DATA_Filtered[,'q82_9']<-NULL
DEO_J_DATA_Filtered[,'q88_9']<-NULL
DEO_J_DATA_Filtered[,'q89_9']<-NULL
DEO_J_DATA_Filtered[,'q817_9']<-NULL
DEO_J_DATA_Filtered[,'q820_9']<-NULL

# step 5 make model suggested by spss for Deodorant B stepwise logistic
names(DEO_B_DATA_Filtered)
DEO_B_DATA_Train<-DEO_B_DATA_Filtered
DEO_B_DATA_Train[,'Product_id_2']<-NULL
DEO_B_DATA_Train[,'Product_26']<-NULL
DEO_B_DATA_Train[,'involvement_17']<-NULL
DEO_B_MOD_1<- glm(Instant_liking_target ~  . ,
                                 family=binomial, data=DEO_B_DATA_Train)

str(DEO_B_DATA_Train)
# step 6 make model suggested by spss for Deodorant G
# step 7 make model suggested by spss for Deodorant J
# step 8 make model suggested by spss for Deodorant F
# step 9 make model suggested by spss for Deodorant H
# step 10 read test file 
# step 11 score all rows
# step 12  fix categorical new levels in test data
# step 13 get accuracy and other factors
# step 14 put output in submission file

################################################################################################################################






