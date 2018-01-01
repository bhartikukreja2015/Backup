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
names(Complete_DATA_4)
#------------combine test data in training------------#
Test_Data<-read.csv("test-data.CSV")
Test_Data$Instant.Liking <- NA

Complete_DATA_4[,'q8.7']<-NULL
Complete_DATA_4[,'q8.9']<-NULL
Complete_DATA_4[,'q8.10']<-NULL
Complete_DATA_4[,'q8.17']<-NULL
Complete_DATA_4[,'q8.18']<-NULL
colnames(Test_Data)[colnames(Test_Data)=="s13a.b.most.often"] <- "most_often"
colnames(Test_Data)[colnames(Test_Data)=="s13.2"] <- "frag_usd_past_6_mths"
names(Complete_DATA_4)
names(Test_Data)
nrow(Test_Data)

# rearrange columns to combine train and test 
TRAIN_DATA<- Complete_DATA_4[, c("Respondent.ID","Product.ID","Product","q1_1.personal.opinion.of.this.Deodorant","q2_all.words",                                           
                                           "q3_1.strength.of.the.Deodorant","q4_1.artificial.chemical","q4_2.attractive","q4_3.bold","q4_4.boring",
                                           "q4_5.casual" ,"q4_6.cheap","q4_7.clean","q4_8.easy.to.wear","q4_9.elegant","q4_10.feminine","q4_11.for.someone.like.me",
                                           "q4_12.heavy","q4_13.high.quality","q4_14.long.lasting","q4_15.masculine","q4_16.memorable","q4_17.natural","q4_18.old.fashioned",
                                           "q4_19.ordinary","q4_20.overpowering","q4_21.sharp","q4_22.sophisticated","q4_23.upscale","q4_24.well.rounded","q5_1.Deodorant.is.addictive",
                                           "q7","q8.1","q8.2","q8.5","q8.6","q8.8","q8.11","q8.12" ,"q8.13","q8.19","q8.20","q9.how.likely.would.you.be.to.purchase.this.Deodorant",
                                           "q10.prefer.this.Deodorant.or.your.usual.Deodorant","q11.time.of.day.would.this.Deodorant.be.appropriate","q12.which.occasions.would.this.Deodorant.be.appropriate",
                                           "Q13_Liking.after.30.minutes","q14.Deodorant.overall.on.a.scale.from.1.to.10","ValSegb","s7.involved.in.the.selection.of.the.cosmetic.products",
                                           "s8.ethnic.background","s9.education","s10.income","s11.marital.status","s12.working.status","frag_usd_past_6_mths","most_often",
                                           "s13b.bottles.of.Deodorant.do.you.currently.own","Instant.Liking" 
                                            )]

NAMES2<-as.data.frame(names(Complete_DATA_4))
NAMES2$test_names<-names(Test_Data)
NAMES2$train_names<-names(TRAIN_DATA)
nrow(Test_Data)
# 5105
nrow(TRAIN_DATA)
#  2500
total_data<-rbind(TRAIN_DATA,Test_Data)
nrow(total_data)

#------------step 3 clean and rename factors------------#
total_data$Respondent_id_1<-as.factor(total_data$Respondent.ID)

total_data$Product_id_2<-as.factor(total_data$Product.ID)

total_data$Instant_liking_target <-as.factor(total_data$Instant.Liking)

total_data$personal_opinion_3<-  as.ordered(total_data$q1_1.personal.opinion.of.this.Deodorant)

total_data$all_words_4<-as.factor(total_data$q2_all.words)

total_data$strength_of_deo_5<-  as.ordered(total_data$q3_1.strength.of.the.Deodorant)

total_data$artifical_chemical_6<-as.factor(total_data$q4_1.artificial.chemical)
total_data$attractive_6<-as.factor(total_data$q4_2.attractive)
total_data$bold_6<-as.factor(total_data$q4_3.bold)
total_data$boring_6<-as.factor(total_data$q4_4.boring)
total_data$casual_6<-as.factor(total_data$q4_5.casual)
total_data$cheap_6<-as.factor(total_data$q4_6.cheap)
total_data$clean_6<-as.factor(total_data$q4_7.clean)
total_data$easy_to_wear_6<-as.factor(total_data$q4_8.easy.to.wear)
total_data$elegant_6<-as.factor(total_data$q4_9.elegant)
total_data$feminine_6<-as.factor(total_data$q4_10.feminine)
total_data$forsomeonelikeme_6<-as.factor(total_data$q4_11.for.someone.like.me)
total_data$heavy_6<-as.factor(total_data$q4_12.heavy)
total_data$high_quality_6<-as.factor(total_data$q4_13.high.quality)
total_data$long_lasting_6<-as.factor(total_data$q4_14.long.lasting)
total_data$masculine_6<-as.factor(total_data$q4_15.masculine)
total_data$memorable_6<-as.factor(total_data$q4_16.memorable)
total_data$natural_6<-as.factor(total_data$q4_17.natural)
total_data$old_fashioned_6<-as.factor(total_data$q4_18.old.fashioned)
total_data$ordinary_6<-as.factor(total_data$q4_19.ordinary)
total_data$forsomeonelikeme_6<-as.factor(total_data$q4_20.overpowering)
total_data$heavy_6<-as.factor(total_data$q4_21.sharp)
total_data$high_quality_6<-as.factor(total_data$q4_22.sophisticated)
total_data$long_lasting_6<-as.factor(total_data$q4_23.upscale)
total_data$masculine_6<-as.factor(total_data$q4_24.well.rounded)

total_data$Deo_is_addictive_7<-  as.ordered(total_data$q5_1.Deodorant.is.addictive)

total_data$q7_8<-  as.ordered(total_data$q7)

total_data$q81_9<-as.factor(total_data$q8.1)
total_data$q82_9<-as.factor(total_data$q8.2)
total_data$q85_9<-as.factor(total_data$q8.5)
total_data$q86_9<-as.factor(total_data$q8.6)

total_data$q88_9<-as.factor(total_data$q8.8)


total_data$q811_9<-as.factor(total_data$q8.11)
total_data$q812_9<-as.factor(total_data$q8.12)
total_data$q813_9<-as.factor(total_data$q8.13)

total_data$q819_9<-as.factor(total_data$q8.19)
total_data$q820_9<-as.factor(total_data$q8.20)

total_data$how_likely_purchase_10  <-as.ordered(total_data$q9.how.likely.would.you.be.to.purchase.this.Deodorant)

total_data$prefer_this_or_usual_11  <-as.ordered(total_data$q10.prefer.this.Deodorant.or.your.usual.Deodorant)

total_data$time_of_day_12 <-as.factor(total_data$q11.time.of.day.would.this.Deodorant.be.appropriate)

total_data$occasions_13 <-as.factor(total_data$q12.which.occasions.would.this.Deodorant.be.appropriate)

total_data$liking_after30min_14 <-as.ordered(total_data$Q13_Liking.after.30.minutes)

total_data$overall_15 <-as.ordered(total_data$q14.Deodorant.overall.on.a.scale.from.1.to.10)

total_data$ValSegb_16 <-as.factor(total_data$ValSegb)

total_data$involvement_17 <-as.ordered(total_data$s7.involved.in.the.selection.of.the.cosmetic.products)

total_data$ethnic_18 <-as.factor(total_data$s8.ethnic.background)

total_data$education_19 <-as.factor(total_data$s9.education)

total_data$income_20 <-as.ordered(total_data$s10.income)

total_data$m_status_21 <-as.factor(total_data$s11.marital.status)

total_data$w_status_22 <-as.factor(total_data$s12.working.status)

total_data$frag_usd_past_6_mths_23 <-(total_data$frag_usd_past_6_mths)

total_data$most_often_24 <-as.factor(total_data$most_often)

total_data$bottles_own_25 <-as.ordered(total_data$s13b.bottles.of.Deodorant.do.you.currently.own)

total_data$Product_26 <-(total_data$Product)


#------------step 4 remove old factor columns and filter out 4 training data sets again ---------#
names(total_data)
drops <- c( "Respondent.ID","Product.ID","Product","Instant.Liking","q1_1.personal.opinion.of.this.Deodorant","q2_all.words",                                           
            "q3_1.strength.of.the.Deodorant","q4_1.artificial.chemical","q4_2.attractive","q4_3.bold","q4_4.boring",
            "q4_5.casual" ,"q4_6.cheap","q4_7.clean","q4_8.easy.to.wear","q4_9.elegant","q4_10.feminine","q4_11.for.someone.like.me",
            "q4_12.heavy","q4_13.high.quality","q4_14.long.lasting","q4_15.masculine","q4_16.memorable","q4_17.natural","q4_18.old.fashioned",
            "q4_19.ordinary","q4_20.overpowering","q4_21.sharp","q4_22.sophisticated","q4_23.upscale","q4_24.well.rounded","q5_1.Deodorant.is.addictive",
            "q7","q8.1","q8.2","q8.5","q8.6","q8.8","q8.11","q8.12" ,"q8.13","q8.19","q8.20","q9.how.likely.would.you.be.to.purchase.this.Deodorant",
            "q10.prefer.this.Deodorant.or.your.usual.Deodorant","q11.time.of.day.would.this.Deodorant.be.appropriate","q12.which.occasions.would.this.Deodorant.be.appropriate",
            "Q13_Liking.after.30.minutes","q14.Deodorant.overall.on.a.scale.from.1.to.10","ValSegb","s7.involved.in.the.selection.of.the.cosmetic.products",
            "s8.ethnic.background","s9.education","s10.income","s11.marital.status","s12.working.status","frag_usd_past_6_mths","most_often",
            "s13b.bottles.of.Deodorant.do.you.currently.own"   
            )

Comp_Data_filtered<-total_data[ , !(names(total_data) %in% drops)]
names(Comp_Data_filtered)

str(Comp_Data_filtered)
Comp_Data_filtered[,'involvement_17']<-NULL
Comp_Data_filtered[,'Respondent_id_1']<-NULL
Comp_Data_filtered[,'Product_id_2']<-NULL

# -----------------------train and test data split--------------------#
Test_filtered<- subset( Comp_Data_filtered, is.na(Instant_liking_target )) 
Train_filtered<- subset( Comp_Data_filtered, !(is.na(Instant_liking_target )) )
nrow(Test_filtered)
nrow(Train_filtered)
#--------------------modelling------------------------------#
getwd()
comp_mod<-glm(Instant_liking_target ~  . ,
             family=binomial, data=Train_filtered,control = list(maxit = 100))
comp_mod
#Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred
# trying glmnet bcoz glm.fit: fitted probabilities numerically 0 or 1 occurred
# load package glmnet in R glmnet_2.0-13

library(glmnet)
#install.packages("H:/kukrebh/d backup/R Packages/R Packages/iterators_1.0.8.zip", repos = NULL, type = "win.binary")
attach(Train_filtered)
xfactors <- model.matrix( personal_opinion_3~ + all_words_4 + strength_of_deo_5
                         + artifical_chemical_6 + attractive_6 + bold_6 + boring_6 + casual_6 +cheap_6
                         + clean_6 + easy_to_wear_6 + elegant_6 + feminine_6 + forsomeonelikeme_6 +heavy_6
                         + high_quality_6 + long_lasting_6 + masculine_6 + memorable_6 + natural_6 +old_fashioned_6
                         + ordinary_6 + Deo_is_addictive_7 + q7_8 + q81_9 + q82_9 +q85_9+q86_9+q88_9+q811_9+q812_9
                         + q813_9 + q819_9 + q820_9 + how_likely_purchase_10 + prefer_this_or_usual_11 +time_of_day_12
                         + occasions_13 + liking_after30min_14 + overall_15 + ValSegb_16 + ethnic_18 +education_19
                         + income_20 + m_status_21 + w_status_22 + most_often_24 + bottles_own_25 +Product_26)
x        <- as.matrix(data.frame(frag_usd_past_6_mths_23,  xfactors))

# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
glmmod <- glmnet(x, y=as.factor(Instant_liking_target), alpha=1, family="binomial")
glmmod
coef(glmmod)[, 10]
cv.glmmod <- cv.glmnet(x, y=as.double(Instant_liking_target), alpha=1)
plot(cv.glmmod)
(best.lambda <- cv.glmmod$lambda.min)

#now predict
x_testfactors <- model.matrix( Test_filtered$personal_opinion_3 ~ + Test_filtered$all_words_4 + Test_filtered$strength_of_deo_5
                              + Test_filtered$artifical_chemical_6 + Test_filtered$attractive_6 + Test_filtered$bold_6 + Test_filtered$boring_6 + Test_filtered$casual_6 + Test_filtered$cheap_6
                              + Test_filtered$clean_6 + Test_filtered$easy_to_wear_6 + Test_filtered$elegant_6 + Test_filtered$feminine_6 + Test_filtered$forsomeonelikeme_6 + Test_filtered$heavy_6
                              + Test_filtered$high_quality_6 + Test_filtered$long_lasting_6 + Test_filtered$masculine_6 + Test_filtered$memorable_6 + Test_filtered$natural_6 + Test_filtered$old_fashioned_6
                              + Test_filtered$ordinary_6 + Test_filtered$Deo_is_addictive_7 + Test_filtered$q7_8 + Test_filtered$q81_9 + Test_filtered$q82_9 + Test_filtered$q85_9+ Test_filtered$q86_9+ Test_filtered$q88_9+ Test_filtered$q811_9+ Test_filtered$q812_9
                              + Test_filtered$q813_9 + Test_filtered$q819_9 + Test_filtered$q820_9 + Test_filtered$how_likely_purchase_10 + Test_filtered$prefer_this_or_usual_11 + Test_filtered$time_of_day_12
                              + Test_filtered$occasions_13 + Test_filtered$liking_after30min_14 + Test_filtered$overall_15 + Test_filtered$ValSegb_16 + Test_filtered$ethnic_18 + Test_filtered$education_19
                              + Test_filtered$income_20 + Test_filtered$m_status_21 + Test_filtered$w_status_22 + Test_filtered$most_often_24 + Test_filtered$bottles_own_25 + Test_filtered$Product_26)
                            

x_test       <- as.matrix(data.frame(Test_filtered$frag_usd_past_6_mths_23,  x_testfactors))
nrow(x_test)
str(x_test)
str(x)
lasso_pred = predict(glmmod, s = best.lambda, newx = x_test, type="response") # Use best lambda to predict test data
lasso_pred

# another method
names(Train_filtered)
x <- as.matrix(Train_filtered[c(-1)])
y <-as.double(Train_filtered$Instant_liking_target)
library(glmnet)
GLMnet_model_1 <- glmnet(x, y, family="gaussian", alpha=0.755,
                         nlambda=1000, standardize=FALSE, maxit=100000)
# Calculate test MSE
# step: predict test file
#predictions<-predict(comp_mod,Test_Data_filtered)
#unique(Comp_Data_filtered$q7_8)
#unique(Test_Data_filtered$q7_8)

#install.packages("H:/kukrebh/d backup/R Packages/R Packages/glmnet_2.0-13.zip", repos = NULL, type = "win.binary")
#install.packages("H:/kukrebh/d backup/R Packages/R Packages/foreach_1.4.3.zip", repos = NULL, type = "win.binary")

#library(glmnet)
#https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression




# step 11 score all rows
# step 12  fix categorical new levels in test data
# step 13 get accuracy and other factors
# step 14 put output in submission file

################################################################################################################################






