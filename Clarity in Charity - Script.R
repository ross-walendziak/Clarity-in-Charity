RNGversion("3.5.3")
library(tidyr)
library(ggplot2)
library(psych)
library(caret)
library(Hmisc)
library(xlsx)
library(writexl)
library(car)
library(mlogit)
library(MASS)
library(pscl)
library(AER)
library(boot)
library(purrr)
library(pastecs)

charity = read.csv("charity.csv", stringsAsFactors= T)# load the "charity.csv" file
###DATA EXPLORATION HERE###
summary(charity)


data.train[,-1] %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + 
  geom_density() #density plots

factors<-c("reg1","reg2","reg3","reg4","home","chld", "hinc", "genf","wrat")

attach(charity)
data.train[,factors] %>% #super messy if I don't separate categorical variables from others
  gather() %>%
  ggplot(aes(value, fill=donr)) + 
  facet_wrap(~key, scales = "free") + 
  geom_bar(fill="Blue")

par(mfrow= c(1,1))

Neigh <- data.frame(donr,damt, reg1, reg2, reg3, reg4, avhv, incm, inca, plow)
cor.ci(Neigh, method="spearman", cex=2.5)

Don <- data.frame(donr,damt, home, chld, hinc,genf,wrat)
cor.ci(Don, method="spearman", cex=2.5)

Prior <- data.frame(donr,damt, npro, tgif, lgif, rgif, tdon, tlag, agif)
cor.ci(Prior, method="spearman", cex=2.5)

for( x in c("reg1", "reg2" ,"reg3", "reg4", "home", "chld", "hinc", "genf", "wrat" )){
  print(x)
  with(data.train,print(table(donr,get(x))))
}

par(mfrow= c(1,4))

for( x in c("reg1", "reg2" ,"reg3", "reg4")){
  boxplot(damt~get(x),subset(data.train, data.train$donr=="1"),main = paste("damt vs" , x))
}

par(mfrow= c(1,3))

for( x in c("chld", "hinc","wrat")){
  boxplot(damt~get(x),subset(data.train, data.train$donr=="1"),main = paste("damt vs" , x))
}


#***Data Preparation / Transformation Begins here***
#Read in raw data
#charity = read.csv("charity.csv") # load the "charity.csv" file
head(charity)

#Transform the categorical variables to more useful buckets and log quantitative variables to achieve normal distribution

transformVariables = function(df){
  #"bucket" categorical variables, true or 1 is associated with greater donation probabilities
  df$chldLess1 = ifelse(df$chld<=1, 1, 0) #true if chld is 1 child or less
  df$hinc345 = ifelse((df$hinc>=3 & df$hinc<=5), 1, 0) #true for hinc categories 3, 4 and 5
  df$wealthGr6 = ifelse(df$wrat>=6, 1, 0) #true if wrat rating is greater than or equal to 6
  
  #Create derived financial wellness variable that shows most recent gift as a percent of largest gift.
  #0=financial hardship for donor, 1~financial prosperity for donor #returns spectrum of results, not a dummy variable
  df$finWellness = ifelse(df$lgif!=0, (df$rgif/df$lgif), 0) 
  
  #transform continuous variables with log function to remove right skewness from the distribution
  df$avhvLog = log(df$avhv) #log of avg home value in donors neighborhood
  df$incmLog = log(df$incm) #log of median income in donor's neighborhood
  df$incaLog = log(df$inca) #log of avg fam income in donor's neighborhood
  df$tgifLog = log(df$tgif) #log of total dollar amount of lifetime gifts
  df$agifLog = log(df$agif) #log of avg gift amount
  
  #Drop the non-log versions of the quantitative variables
  df = subset(df, select = -c(avhv, incm, inca, tgif, agif))
  
  #Reorder with repose variables at end
  df = subset(df, select=c(ID:tlag, chldLess1:agifLog, donr:part))
  
  return(df)
}

#Pass charlity df to transformVariables Function
charity.t = transformVariables(df=charity)
head(charity.t)
dim(charity.t)

#basic summary stats
#t(summary(charity.t[charity.t$part=="train",]))
#t(summary(charity[charity$part=="train",]))


#Setup Train, Validation and Test data into separate components
data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:25]
c.train <- data.train[,26] # donr
n.train.c <- length(c.train) #3984 obs
y.train <- data.train[c.train==1,27] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995 obs

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:25]
c.valid <- data.valid[,26] # donr
n.valid.c <- length(c.valid) # 2018 obs
y.valid <- data.valid[c.valid==1,27] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999 obs

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:25]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

####################################
##### CLASSIFICATION MODELING ######
####################################

### linear discriminant analysis ###

library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhvLog + incmLog + incaLog + plow + npro + tgifLog + lgif + rgif + tdon + tlag + agifLog, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
#FOR US IT IS 1367 & $11635.5 USING THE LOG TRANSFORMS
#For PROF 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#FOR us         c.valid
#chat.valid.lda1   0   1
#              0 643   8
#              1 376 991
# check n.mail.valid = 376+991 = 1367
# check profit = 14.5*991-2*1367 = 11635.5


#FOR PROF       c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5


### logistic regression ###

model.log <- glm(donr ~.,data.train.std.c, family=binomial("logit"))
summary(model.log)

#Log1
model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhvLog + incmLog + incaLog + plow + npro + tgifLog + lgif + rgif + tdon + tlag + agifLog, 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
#For US   1219   $11627
#For Prof 1291.0 $11642.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table

#For Us        c.valid
#chat.valid.log1   0   1
#              0 770  29
#              1 249 970
# check n.mail.valid = 249+970 = 1219
# check profit = 14.5*970-2*1219 = 11627

#For Prof      c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5

#Log2
model.log2 <- glm(donr ~ reg1 + reg2 +home +tdon +tlag +chldLess1 + hinc345 +wealthGr6 +incmLog +tgifLog, 
                  data.train.std.c, family=binomial("logit"))#potentially remove hinc 
summary(model.log2) #All sig at ***
post.valid.log2 <- predict(model.log2, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log2 <- cumsum(14.5*c.valid[order(post.valid.log2, decreasing=T)]-2)
plot(profit.log2) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log2) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log2)) # report number of mailings and maximum profit
#For Log 2   1367   $11592
#For Log 1   1219   $11627 Log 1 is still better, will keep


### logistic regression GAM###

library(gam)
model.gam <- gam(donr~.+I(hinc^2),data.train.std.c, family = "gaussian")
summary(model.gam)#need to remove points that are not significant

post.valid.gam <- predict(model.gam, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.gam <- cumsum(14.5*c.valid[order(post.valid.gam, decreasing=T)]-2)
plot(profit.gam) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.gam) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.gam)) # report number of mailings and maximum profit
#For GAM 1391 mail $11674.5

cutoff.gam <- sort(post.valid.gam, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.gam <- ifelse(post.valid.gam>cutoff.gam, 1, 0) # mail to everyone above the cutoff
table(chat.valid.gam, c.valid) # classification table

#gam 1
model.gam1 <- gam(donr~reg1+reg2+home+chld+plow+npro+tdon+tlag+chldLess1+hinc345+hinc345+wealthGr6+
                    avhvLog+incmLog+tgifLog+I(hinc^2),data.train.std.c, family = "gaussian")
summary(model.gam1)#need to remove points that are not significant

post.valid.gam1 <- predict(model.gam1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.gam1 <- cumsum(14.5*c.valid[order(post.valid.gam1, decreasing=T)]-2)
plot(profit.gam1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.gam1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.gam1)) # report number of mailings and maximum profit
#For Sign GAM 1288 mail $11663
#For Full GAM 1391 mail $11674.5
#Full GAM is better, will keep

### QDA ###
model.qda <- qda(donr ~.+I(hinc^2), data.train.std.c)
post.valid.qda <- predict(model.qda, data.valid.std.c)$posterior[,2]
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.qda <- cumsum(14.5*c.valid[order(post.valid.qda, decreasing=T)]-2)
plot(profit.qda) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.qda) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.qda)) # report number of mailings and maximum profit
#For Full QDA 1304 mail $11181.5

cutoff.qda <- sort(post.valid.qda, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.qda <- ifelse(post.valid.qda>cutoff.qda, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda, c.valid) # classification table

#QDA Sig
summary(model.qda)
model.qda1 <- qda(donr~reg1+reg2+home+chld+plow+npro+tdon+tlag+chldLess1+hinc345+hinc345+wealthGr6+
                    avhvLog+incmLog+tgifLog+I(hinc^2), data.train.std.c)
post.valid.qda1 <- predict(model.qda1, data.valid.std.c)$posterior[,2]
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.qda1 <- cumsum(14.5*c.valid[order(post.valid.qda1, decreasing=T)]-2)
plot(profit.qda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.qda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.qda1)) # report number of mailings and maximum profit
#For Sign QDA 1293 mail $11261.5
#For Full QDA 1304 mail $11181.5
#Will Use Sign QDA

cutoff.qda1 <- sort(post.valid.qda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.qda1 <- ifelse(post.valid.qda1>cutoff.qda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda1, c.valid) # classification table


###KNN Model###
set.seed(2814) #Earth's sector in GL
model.knn5<-knn(data.train.std.c[,],data.valid.std.c[,],c.train,k = 5)

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.knn5 <- cumsum(14.5*c.valid[order(model.knn5, decreasing=T)]-2)
plot(profit.knn5) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.knn5) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.knn5)) # report number of mailings and maximum profit
#For KNN5 1137 mail $11646

cutoff.knn5 <- sort(model.knn5, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
table(model.knn5, c.valid) 

model.knn = function(K){
  set.seed(2814) #Earth's sector in GL
  model.knn<-knn(data.train.std.c[,],data.valid.std.c[,],c.train,k = K)
  
  # calculate ordered profit function using average donation = $14.50 and mailing cost = $2
  
  profit.knn <- cumsum(14.5*c.valid[order(model.knn, decreasing=T)]-2)
  plot(profit.knn) # see how profits change as more mailings are made
  n.mail.valid <- which.max(profit.knn) # number of mailings that maximizes profits
  c(n.mail.valid, max(profit.knn)) # report number of mailings and maximum profit
  
  cutoff.knn <- sort(model.knn, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
  table(model.knn, c.valid)
  c(n.mail.valid, max(profit.knn))
}
model.knn(1)
#1111 & $11190.5
#values between 1 & 5 are worse
model.knn(5)
#1137 & $11646
#values between 5 & 10 are worse
model.knn(10) 
#1160 & $11643.5
model.knn(20)
#1188 & $11674.5
model.knn(50)
#1218 & $11817.5
model.knn(100)
#1253 & $11863.5

###SVM###
library(e1071)
tune.out <- tune(svm, donr~.+ I(hinc^2), data = data.train.std.c, kernel="linear",ranges=list(cost=c(0.1, 1,5,10,100)))
bestmod=tune.out$best.model
summary(bestmod) #Cost 1, gamma 0.04, radial kernel is best

model.svm = svm(donr~.+ I(hinc^2), data=data.train.std.c, kernel="radial", cost=1, scale=FALSE)
summary(model.svm)

post.valid.svm <- predict(model.svm, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.svm <- cumsum(14.5*c.valid[order(post.valid.svm, decreasing=T)]-2)
plot(profit.svm) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svm) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm)) # report number of mailings and maximum profit
#For Radial   1338   $11635.5

cutoff.svm <- sort(post.valid.svm, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.svm <- ifelse(post.valid.svm>cutoff.svm, 1, 0) # mail to everyone above the cutoff
table(chat.valid.svm, c.valid)


###SUPPORT VECTOR CLASSIFIER###
model.svm1 = svm(donr~.+ I(hinc^2), data=data.train.std.c, kernel="linear", cost=1, scale=FALSE)
summary(model.svm1)

post.valid.svm1 <- predict(model.svm1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.svm1 <- cumsum(14.5*c.valid[order(post.valid.svm1, decreasing=T)]-2)
plot(profit.svm1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svm1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm1)) # report number of mailings and maximum profit
#For Linear   1378   $11599
#For Radial   1338   $11635.5
#Radial is better
###TREE###

library(tree)
#install.packages("rpart")
library(rpart)
model.tree = rpart(donr~.+I(hinc^2), method="class", control=rpart.control(minsplit=100, cp=0.001),
                   data=data.train.std.c)
printcp(model.tree) #chld, home, hinc^2, reg2, tdon, tlag, wrat used
plotcp(model.tree)
summary(model.tree)

valid.tree = predict(model.tree,data.valid.std.c, type="class")

profit.tree <- cumsum(14.5*c.valid[order(valid.tree, decreasing=T)]-2)
plot(profit.tree) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.tree) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree)) 
#1106 and $10649.5 for min 100 and cp 0.01
#1027 and $10720.5 for min 100 and cp 0.001
#changes to minsplit were not beneficial

table(valid.tree, c.valid)

###RANDOM FOREST###
library(randomForest)
set.seed(2814)
model.RandFor = randomForest(as.factor(donr)~.+I(hinc^2), data=data.train.std.c, mtry=1, ntree=1000)

valid.RandFor = predict(model.RandFor,data.valid.std.c, type="class")

profit.RandFor <- cumsum(14.5*c.valid[order(valid.RandFor, decreasing=T)]-2)
plot(profit.RandFor) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.RandFor) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.RandFor)) 
#1056 mail $11184.5 for mtry 15 ntree 100
#1049 mail $11184 for mtry 10 ntree 100
#1066 mail $11222.5 for mtry 5  ntree 100
#1103 mail $11279 for mtry 2 ntree 100
#1106 mail $11345.5 for mtry 2 ntree 400, best of 100-500
#1103 mail $11366 for mtry 2 ntree 1000
#1192 mail $11579.5 for mtry 1 ntree 1000
#mtry 1 had best results with ntree 1000
#ntree over 1000 took a very long time and returned slightly worse results
#After performing various combinations of mtry and ntree, mtry 2 and ntree 1000 had the best results
table(valid.tree, c.valid)


###BOOST###


fit <- trainControl(method="repeatedcv", number=10, repeats=10)
set.seed(2814)
boost.fit = train(as.factor(donr)~.+I(hinc^2),data = data.train.std.c,
                  method = "gbm", trControl = fit, verbose = FALSE)
summary(boost.fit)
boost.fit #The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10
#Had an accuracy of 0.9094137

library(gbm)
set.seed(2814)
model.boost=gbm(donr~.+I(hinc^2), data=data.train.std.c,distribution = "bernoulli",n.trees = 150,
                shrinkage=0.1,n.minobsinnode = 10, interaction.depth = 3)

set.seed(2814)
valid.boost = predict(model.boost,data.valid.std.c, n.trees = 150, type="response")
pred = rep("0", 2018)
pred[valid.boost> .5] = "1"
table(pred,c.valid)

profit.boost <- cumsum(14.5*c.valid[order(valid.boost, decreasing=T)]-2)
plot(profit.boost) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.boost) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.boost)) 
#1218 mail $11948 for ntrees 150, shrinkage 0.1, n.minobsinnode 10, ID 3 which were optimized earlier


### THIS SECTION GOES AFTER ALL MODELS ARE MADE ###
# Results

# n.mail Profit    Model
# 1218   $11948    Boost
# 1253   $11863.5  KNN100
# 1391   $11674.5  GAM
# 1338   $11635.5  SVM (RADIAL)
# 1367   $11635.5  LDA1
# 1219   $11627    Log1
# 1378   $11599    SVC(LINEAR)
# 1192   $11579.5  RandForest (mtry=1 ntree=1000)
# 1293   $11261.5  QDA1
# 1027   $10720.5  Decision Tree


# select model BOOST since it has maximum profit in the validation sample

###THIS SECTION GOES AFTER BEST MODEL IS SELECTED AND THAT MODEL WILL BE USED HERE###

post.test <- predict(model.boost, data.test.std, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.boost)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1717  290
# based on this model we'll mail to the 290 highest posterior probabilities

# See below for saving chat.test into a file for submission

###PREDICTION SECTION (Regression Models)

#***Best subset selection using 10-fold Cross Validation***
#Page 250 in text
library(leaps)
k = 10
set.seed(1)
folds = sample(1:k, nrow(data.train.std.y), replace=TRUE)
cv.errors=matrix(NA, k, 24, dimnames = list(NULL, paste(1:24)))

#page 249 in text, necessary b/c for some reason regsubsets has no prediction method, Very unfortunate!
predict.regsubsets = function(object, newdata, id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}
#page 250 from text
for(j in 1:k){
  best.fit = regsubsets(damt ~., data=data.train.std.y[folds!=j,], nvmax=24)
  
  for (i in 1:24){
    pred = predict(object=best.fit, newdata=data.train.std.y[folds==j,], id=i) #calls the custom prediction method above predict.regsubsets
    cv.errors[j,i] = mean((data.train.std.y$damt[folds==j] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
mean.cv.errors[15]  #MSE in the test data minimized at 1.492059 with 15 variables

tenFoldBestCV = regsubsets(damt ~., data=data.train.std.y, nvmax=24)
coef(tenFoldBestCV, 15) #Find the coefficients associated with the 15 best selected

#Now fit linear model using the 15 coefficients from above from best cross validated subset selection using training data
#necessary b/c lm has a predict method
lmBest = lm(damt ~  reg3+reg4+home+chld+hinc+genf+wrat+plow+rgif+tdon+hinc345+wealthGr6+incmLog+tgifLog+agifLog, data=data.train.std.y)

#predict MSE and SD on validation set
pred.lmBest = predict(lmBest, newdata=data.valid.std.y)
mean((y.valid - pred.lmBest)^2)
#1.67181
sd((y.valid - pred.lmBest)^2) / sqrt(n.valid.y)
#0.1610668


#***Full least squares model using all variables***
lsFull = lm(damt~., data=data.train.std.y) #train full lm using training set
pred.lsFull = predict(lsFull, newdata=data.valid.std.y) #Get predictions using trained model on validation data

mean((y.valid - pred.lsFull)^2)
#1.664029
sd((y.valid - pred.lsFull)^2)/sqrt(n.valid.y)
#0.1616225


#***Ridge Regression***
library(glmnet)
#page 254 in text
x = model.matrix(damt~., data.train.std.y)[,-1]
y= data.train.std.y[,25]

set.seed(1)
cv.out = cv.glmnet(x, y, alpha=0)
bestlam = cv.out$lambda.min  #Best lambda occurs at 0.1088535
bestlam

#page 251 and 253 of text 
grid = 10^seq(10, -2, length=100)
ridge.mod = glmnet(x, y, alpha=0, lambda=grid, thresh=1e-12)

xVal = model.matrix(damt~., data.valid.std.y)[,-1]
yVal = data.valid.std.y[,25]
ridge.pred = predict(ridge.mod, s=bestlam, newx=xVal) #predict using matrix of x values from validation data

mean((ridge.pred - yVal)^2)
#1.677144
sd((ridge.pred - yVal)^2) / sqrt(n.valid.y)
#0.1641831


#*** The Lasso Model*** Note, need to run ridge first b/c it relies on the same matrices of training and validation data as inputs
#page 255 in text
set.seed(1)
cv.out = cv.glmnet(x, y, alpha=1)
bestLam = cv.out$lambda.min  #Best lambda for lasso occurs at 0.002345177
bestLam

lasso.mod = glmnet(x, y, alpha=1, lambda=grid, thresh=1e-12)
lasso.pred = predict(lasso.mod, s=bestLam, newx=xVal)

mean((lasso.pred - yVal)^2)
#1.663785
sd((lasso.pred - yVal)^2) / sqrt(n.valid.y)
#0.1614331
lasso.coef = predict(lasso.mod, type="coefficients", s=bestLam)
lasso.coef

#*** Principle Components Regression (PCR) ***
library(pls)
set.seed(1)
pcr.fit = pcr(damt~., data=data.train.std.y, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP") #MSEP reaches local min at 16 components, with not much improvement thereafter.
summary(pcr.fit)

pcr.pred = predict(pcr.fit, newdata=data.valid.std.y, ncomp=16) #Using 16 components from above

mean((y.valid - pcr.pred)^2)
#1.764425
sd((y.valid - pcr.pred)^2) / sqrt(n.valid.y)
#0.1634824


#*** Partial Least Squares Regression (PLS) ***
set.seed(1)
pls.fit = plsr(damt~., data=data.train.std.y, scale=TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP") #MSEP reaches local min at 5 components, with not much improvement thereafter.
summary(pls.fit)

pls.pred = predict(pls.fit, newdata=data.valid.std.y, ncomp=5) #Using 5 components from above

mean((y.valid - pls.pred)^2)
#1.67588
sd((y.valid - pls.pred)^2) / sqrt(n.valid.y)
#0.1612646


#*** Boosting Regression ***
#page 331 of text
library(gbm)
set.seed(1)
boost.fit = gbm(damt~., data=data.train.std.y, distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(boost.fit)

boost.pred = predict(boost.fit, newdata=data.valid.std.y, n.trees=5000)

mean((y.valid - boost.pred)^2)
#1.719131
sd((y.valid - boost.pred)^2) / sqrt(n.valid.y)
#0.1664692


#*** Random Forest ***
#Page 329 in text
library(randomForest)
set.seed(1)
rf.fit = randomForest(damt~., data=data.train.std.y, mtry=6, importance=TRUE)
pred.rf = predict(rf.fit, newdata=data.valid.std.y)

mean((y.valid - pred.rf)^2)
#1.662664
sd((y.valid - pred.rf)^2)/sqrt(n.valid.y)
#0.1733396
importance(rf.fit)
varImpPlot(rf.fit)

#*** Best Selection with BIC ***
#page 246 in text
library(leaps)
regfit.full = regsubsets(damt~., data=data.train.std.y, nvmax=24)

which.min(summary(regfit.full)$bic) #error rate as per BIC metric minimized with 12 variables
plot(summary(regfit.full)$bic, xlab="Num of Variables", ylab="BIC", type='l')
points(12,summary(regfit.full)$bic[12],col="red",cex=2,pch=20)

coef(regfit.full, 12)

#build lm model with train data based using bic 12 coefficients
#again, is necessary b/c issue with predict method on regsubsets
bic.fit = lm(damt~reg3+reg4+home+chld+hinc+wrat+plow+rgif+wealthGr6+incmLog+tgifLog+agifLog, data=data.train.std.y)

pred.bic = predict(bic.fit, newdata=data.valid.std.y) #predict on validation data

mean((y.valid - pred.bic)^2)
#1.677476
sd((y.valid - pred.bic)^2)/sqrt(n.valid.y)
#0.1611646

#Applying best classification model and prediction model to the test data
###Classification###
post.test <- predict(model.boost, data.test.std, n.trees = 150, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.boost)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set
n.mail.test

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
chat.test[1:20]
#    0    1 
# 1717  290
# based on this model we'll mail to the 290 highest posterior probabilities
summary(chat.test)

###Prediction###
yhat.test <- predict(rf.fit, newdata = data.test.std) # test predictions
mean(yhat.test) #Random Forest Model predicts and average donation of $14.22123 across all prospective donors in the test set

length(yhat.test) # check length = 2007
yhat.test[1:10] # check this consists of plausible predictions of damt

ip = data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="SN_RW.csv", row.names=FALSE) # use your initials for the file name