# neural network using SPSS  try1  
rm(list = ls(all =T))
# import training data with predictions
train <- read.csv("C://Users//kukrebh/Desktop//neural-net//training-output.csv", header = TRUE,  stringsAsFactors = FALSE , quote="\"")
names(train)
xTab <- table(train$X.N.WON, train$WON)
xTab
#//actuals on top
# pred from left side
# 0 is for lost one's 
# 1 is for won
acc<-sum(diag(xTab))/sum(xTab)
acc
# 0.8636632
# when all are win 6501+28838\
# correctly classified as won 28838
# acc= 28838/35339
win_accuracy <- 28838/35339
win_accuracy
# ] 0.8160389
loss_accuracy <-56124/(56124+6991)
#  0.8892339

# import testing data with predictions
test <- read.csv("C://Users//kukrebh/Desktop//neural-net//test_data_res.csv", header = TRUE,  stringsAsFactors = FALSE , quote="\"")
names(test)

xTab2 <- table(test$X.N.WON, test$WON)
xTab2
#//actuals on top
# pred from left side
# 0 is for lost one's 
# 1 is for won
acc2<-sum(diag(xTab2))/sum(xTab2)
acc2
#[1] 0.7959894
win_accuracy2  <- 622/(622+561)
#  0.5257819
lost_accuracy2 <- 4242/(4242+609)
# 0.8744589
# 





# neural network using SPSS  try2  
rm(list = ls(all =T))
# import training data with predictions
train2 <- read.csv("C://Users//kukrebh/Desktop//training-output-try2.csv", header = TRUE,  stringsAsFactors = FALSE , quote="\"")
names(train2)
xTab_2 <- table(train2$X.N.WON, train2$WON)
xTab_2
#//actuals on top
# pred from left side
# 0 is for lost one's 
# 1 is for won
acc_2<-sum(diag(xTab_2))/sum(xTab_2)
acc_2
# 0.8657877
# when all are win 6322+29017
# correctly classified as won 29017

win_accuracy_2 <- 29017/(29017+6322)
win_accuracy_2
#  0.8211042
loss_accuracy_2 <-56154/(56154+6881)
# 0.8908384

# import testing data with predictions
test2 <- read.csv("C://Users//kukrebh/Desktop//test_data_try2.csv", header = TRUE,  stringsAsFactors = FALSE , quote="\"")
names(test2)

xTab2_2 <- table(test2$X.N.WON, test2$WON)
xTab2_2
#//actuals on top
# pred from left side
# 0 is for lost one's 
# 1 is for won
acc2_2<-sum(diag(xTab2_2))/sum(xTab2_2)
acc2_2
#0.7936692
win_accuracy2_2  <- 613/(613+570)
#  0.5181741
lost_accuracy2_2 <- 4219/(4219+632)
# 0.8697176
# 



# neural network using SPSS  try3 
rm(list = ls(all =T))
# import training data with predictions
train3 <- read.csv("C://Users//kukrebh/Desktop//training-output-try3.csv", header = TRUE,  stringsAsFactors = FALSE , quote="\"")
names(train3)
xTab_3 <- table(train3$X.N.WON, train3$WON)
xTab_3
#//actuals on top
# pred from left side
# 0 is for lost one's 
# 1 is for won
acc_3<-sum(diag(xTab_3))/sum(xTab_3)
acc_3
#0.8599325
# when all are win 6322+29017
# correctly classified as won 29017

win_accuracy_3 <- 28829/(28829+6510)
win_accuracy_3
#  0.8157843
loss_accuracy_3 <-55766/(55766+7269)
#  0.8846831

# import testing data with predictions
test3 <- read.csv("C://Users//kukrebh/Desktop//test_data_res.csv", header = TRUE,  stringsAsFactors = FALSE , quote="\"")
names(test3)
View(test3)
xTab2_3 <- table(test3$X.N.WON, test3$WON)
xTab2_3
#//actuals on top
# pred from left side
# 0 is for lost one's 
# 1 is for won
acc2_3<-sum(diag(xTab2_3))/sum(xTab2_3)
acc2_3
# 0.7981439

win_accuracy2_3  <- 614/(614+569)
#  0.5190194
lost_accuracy2_3 <- 4247/(4247+604)
# 0.8754896
# 
