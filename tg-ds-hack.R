# tg 2018 code #
rm(list=ls(all=T))
setwd("H:\\kukrebh\\gola\\tg18\\DS")
# step 1 read all training csv files
#   deo b    #
DEO_B_DATA<-read.csv("Deodorant-B.CSV")
str(DEO_B_DATA)
names(DEO_B_DATA)
#------------clean factors---------#
DEO_B_DATA$Respondent.ID<-as.factor(DEO_B_DATA$Respondent.ID)
DEO_B_DATA$Product.ID<-as.factor(DEO_B_DATA$Product.ID)
DEO_B_DATA$Instant.Liking <-as.factor(DEO_B_DATA$Instant.Liking )
#http://www.dummies.com/education/math/statistics/types-of-statistical-data-numerical-categorical-and-ordinal/   y ordinal
# install.packages("H:/kukrebh/d backup/R Packages/R Packages/toOrdinal_1.0-0.0.zip", repos = NULL, type = "win.binary")
# install.packages("H:/kukrebh/d backup/R Packages/R Packages/testthat_1.0.2.zip", repos = NULL, type = "win.binary")
# install.packages("H:/kukrebh/d backup/R Packages/R Packages/crayon_1.3.1.zip", repos = NULL, type = "win.binary")

library(toOrdinal)
DEO_B_DATA$personal_opinion<-  toOrdinal(DEO_B_DATA$q1_1.personal.opinion.of.this.Deodorant )
DEO_B_DATA$q2_all.words<-as.factor(DEO_B_DATA$q2_all.words)
DEO_B_DATA$strength_of_deo<-  toOrdinal(DEO_B_DATA$q3_1.strength.of.the.Deodorant )


View(DEO_B_DATA)

levels(DEO_B_DATA$q1_1.personal.opinion.of.this.Deodorant)
DEO_B_DATA$q8.2<-as.factor(DEO_B_DATA$q8.2)
DEO_B_DATA$q8.5<-as.factor(DEO_B_DATA$q8.5)
DEO_B_DATA$q8.6<-as.factor(DEO_B_DATA$q8.6)
DEO_B_DATA$q8.8<-as.factor(DEO_B_DATA$q8.8)
DEO_B_DATA$q8.11<-as.factor(DEO_B_DATA$q8.11)
DEO_B_DATA$q8.12<-as.factor(DEO_B_DATA$q8.12)
DEO_B_DATA$q8.13<-as.factor(DEO_B_DATA$q8.13)
DEO_B_DATA$q8.19<-as.factor(DEO_B_DATA$q8.19)
DEO_B_DATA$q8.20<-as.factor(DEO_B_DATA$q8.20)

#------------remove old factor columns ---------#


#   deo g    #
DEO_G_DATA<-read.csv("Deodorant-G.CSV")
str(DEO_G_DATA)
#------------clean factors---------#










# step 2 make model suggested by spss for Deodorant B stepwise logistic
# step 3 make model suggested by spss for Deodorant G
# step 4 make model suggested by spss for Deodorant J
# step 5 make model suggested by spss for Deodorant F
# step 6 make model suggested by spss for Deodorant H
# step 7 read test file 
# step 8 score all rows
#   fix categorical new levels in test data
# step 9 get accuracy and other factors
# step 10 put output in submission file