# VARIABLE DESCRIPTIONS:
#   survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# 
# SPECIAL NOTES:
#   Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# 
# Age is in Years; Fractional if Age less than One (1)
# If the Age is Estimated, it is in the form xx.5
# 
# With respect to the family relation variables (i.e. sibsp and parch)
# some relations were ignored.  The following are the definitions used
# for sibsp and parch.
# 
# Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
# Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
# Parent:   Mother or Father of Passenger Aboard Titanic
# Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic
# 
# Other family relatives excluded from this study include cousins,
# nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
# only with a nanny, therefore parch=0 for them.  As well, some
# travelled with very close friends or neighbors in a village, however,
# the definitions do not support such relations.

# build
library(data.table)
d <- fread("train.csv")
t <- fread("test.csv")

# decision trees
library(rpart)
library(rpart.plot)
subd <- d[!is.na(Age), .(Survived, Sex, Age)]
model <- rpart(Survived ~ Age + Sex, data = subd)
prp(model, type = 3)
predict(model, newdata = data.frame(Sex = 'male', Age = 8))

# test
ans <- as.integer(predict(model, newdata = t) >= .5) # 892 1309
write.csv(data.frame(PassengerId = 892:1309, Survived = ans), file = "ans1.csv", quote = FALSE, row.names = F)

# random forest
features <- list(Pclass, Sex, Age, SibSp, ParCh)
subsetdata <- function(dat) {
  dat$Age[is.na(dat$Age)] <- -1
  dat$Pclass <- as.factor(dat$Pclass)
  dat$Sex <- as.factor(dat$Sex)
  dat[, .(Pclass, Sex, Age, SibSp, Parch)]
}
subsetdata(d)
library(randomForest)
set.seed(1)
rf <- randomForest(subsetdata(d), as.factor(d$Survived), ntree = 100, importance = TRUE)
submission <- data.frame(PassengerId = t$PassengerId)
submission$Survived <- predict(rf, subsetdata(t))
write.csv(submission, file = "random_forest.csv", row.names=FALSE, quote = FALSE)

# importance of features
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

# ts shift + tab
# Mon Dec 12 16:55:32 2016 ------------------------------
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(mytree)

# Wed Dec 14 17:10:32 2016 ------------------------------
data <- d
data$Age[is.na(data$Age)] <- -1
model <- rpart(Survived ~ Age + Sex, data = data, method = "class")
fancyRpartPlot(model)
predict(model, newdata = data.frame(Sex = 'male', Age = 8), type = "prob")

tdata <- t
tdata$Age[is.na(tdata$Age)] <- -1
ans <- predict(model, newdata = tdata, type = "class") # 892 1309
df <- data.frame(PassengerId = tdata$PassengerId, Survived = ans)
write.csv(df, file = "rpart_fill_miss_-1.csv", row.names=FALSE, quote = FALSE)

# Wed Dec 14 17:35:22 2016 ------------------------------
mosaicplot(with(d, table(Embarked, Survived)))
mosaicplot(with(d, table(Pclass, Survived)))
mosaicplot(with(d, table(Sex, Survived)))
mosaicplot(with(d, table(Parch, Survived)))
mosaicplot(with(d, table(SibSp, Survived)))

data <- d
data$Age[is.na(data$Age)] <- -1
model <- rpart(Survived ~ SibSp + Parch + Sex + Pclass + Embarked, data = data, method = "class")
fancyRpartPlot(model)
predict(model, newdata = data.frame(Sex = 'male', Age = 8), type = "prob")

tdata <- t
tdata$Age[is.na(tdata$Age)] <- -1
ans <- predict(model, newdata = tdata, type = "class") # 892 1309
df <- data.frame(PassengerId = tdata$PassengerId, Survived = ans)
write.csv(df, file = "rpart_4.csv", row.names=FALSE, quote = FALSE)

# Thu Dec 15 01:30:10 2016 ------------------------------
subsetdata <- function(df) {
  df$Age[is.na(df$Age)] <- -1
  df$Embarked[df$Embarked == ""] <- "S"
  df$Sex <- as.factor(df$Sex)
  df$Embarked <- as.factor(df$Embarked)
  df$Pclass <- as.factor(df$Pclass)
  df$Salute <- sapply(df$Name, FUN = function(x) strsplit(x, split = "[.,]")[[1]][2])
  #df[, c("SibSp", "Parch", "Sex", "Pclass", "Embarked", "Age")] 
  df
}

nd <- subsetdata(d)
nt <- subsetdata(t)
model <- rpart(Survived ~ Salute + SibSp + Parch + Sex + Pclass + Embarked, data = nd, method = "class")
fancyRpartPlot(model)

nt$Salute <- as.character(nt$Salute)
nt$Salute[nt$Salute == " Dona"] <- " Don"
nt$Salute <- as.factor(nt$Salute)

ans <- predict(model, newdata = nt[, ], type = "class") # 892 1309
df <- data.frame(PassengerId = nt$PassengerId, Survived = ans)
write.csv(df, file = "rpart_salutation.csv", row.names=FALSE, quote = FALSE)

# Thu Dec 15 17:11:23 2016 ------------------------------
subsetdata <- function(df) {
  df$Age[is.na(df$Age)] <- -1
  df$Embarked[df$Embarked == ""] <- "S"
  df$Sex <- as.factor(df$Sex)
  df$Embarked <- as.factor(df$Embarked)
  df$Pclass <- as.factor(df$Pclass)
  df$Salute <- sapply(df$Name, FUN = function(x) strsplit(x, split = "[.,]")[[1]][2])
  #df[, c("SibSp", "Parch", "Sex", "Pclass", "Embarked", "Age")] 
  df$Tshort <- sapply(df$Ticket, FUN = function(x) strsplit(x, split = "")[[1]][1])
  df$Cabin[df$Cabin == ""] <- "Z"
  df$Cabin <- sapply(df$Cabin, FUN = function(x) strsplit(x, split = "")[[1]][1])
  df
}

nd <- subsetdata(d)
nt <- subsetdata(t)

nt$Salute <- as.character(nt$Salute)
nt$Salute[nt$Salute == " Dona"] <- " Don"
nt$Salute <- as.factor(nt$Salute)
nt$Tshort <- as.factor(nt$Tshort)
nt$Cabin <- as.factor(nt$Cabin)

model <- glm(Survived ~ Salute + SibSp + Parch + Sex + 
                 Pclass + Embarked, data = nd)
fancyRpartPlot(model)

ans <- predict(model, newdata = nt) # 892 1309
ans <- sapply(ans, function(x) ifelse(x>=0.3, 1, 0))
df <- data.frame(PassengerId = nt$PassengerId, Survived = ans)
write.csv(df, file = "logistic_0.3.csv", row.names=FALSE, quote = FALSE)
# A  B  C  D  E  F  G  T 
# 15 47 59 33 32 13  4  1 

# Fri Dec 16 23:03:55 2016 ------------------------------
# logistic model

model <- glm(Survived ~ Salute + Sex + Pclass + Embarked, data = nd, 
             family = "binomial")
ans <- predict(model, newdata = nt, type = "response") # 892 1309
#ans <- 1/(1+exp(-ans))
ans <- sapply(ans, function(x) ifelse(x>=0.5, 1, 0))
df <- data.frame(PassengerId = nt$PassengerId, Survived = ans)
write.csv(df, file = "logistic_modified_.5b.csv", row.names=FALSE, quote = FALSE)

# Fri Dec 16 23:58:29 2016 ------------------------------

nd2 <- nd[, .(Pclass, Sex, Age, SibSp, Parch, Embarked, Salute)]
nt2 <- nt[, .(Pclass, Sex, Age, SibSp, Parch, Embarked, Salute)]

nd2s <- sparse.model.matrix(~.-1,data = nd2)
nt2s <- sparse.model.matrix(~.-1,data = nt2)

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta=0.1,
  gamma=0,
  max_depth=6,
  min_child_weight=1,
  subsample=1,
  colsample_bytree=1
)

xgbcv <- xgb.cv(params = params
                ,data = nd2s
                ,label = nd$Survived
                ,nrounds = 100
                ,nfold = 5
                ,showsd = T
                ,stratified = T
                ,print.every.n = 10
                ,early.stop.round = 20
                ,maximize = F
)

xgbm2 <- xgboost(data = nd2s, label = nd$Survived, nrounds = 17,
                 params = params)
ans <- predict(xgbm2, nt2s)

ans <- sapply(ans, function(x) ifelse(x>=0.5, 1, 0))
df <- data.frame(PassengerId = nt$PassengerId, Survived = ans)
write.csv(df, file = "xgbmodel2.csv", row.names=FALSE, quote = FALSE)

# Wed Dec 21 00:35:33 2016 ------------------------------
# learn deeply parameters of xgboost

library(data.table)
d <- fread("train.csv")
t <- fread("test.csv")
subsetdata <- function(df) {
  df$Embarked[df$Embarked == ""] <- "S"
  df$Sex <- as.factor(df$Sex)
  df$Embarked <- as.factor(df$Embarked)
  df$Age[is.na(df$Age)] <- -1
  df$Pclass <- as.factor(df$Pclass)
  df
}


nd <- subsetdata(d)
nt <- subsetdata(t)  

nd2 <- nd[, .(Pclass, Sex, Age, SibSp, Parch, Embarked)]
nt2 <- nt[, .(Pclass, Sex, Age, SibSp, Parch, Embarked)]

nd2s <- model.matrix(~.+0, data = nd2)
nt2s <- model.matrix(~.+0, data = nt2)

dtrain <- xgb.DMatrix(data = nd2s, label = nd$Survived)

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta=0.1,
  gamma=0,
  max_depth=6,
  min_child_weight=1,
  subsample=1,
  colsample_bytree=1
)

xgbcv <- xgb.cv(params = params
                ,data = dtrain
                ,nrounds = 100
                ,nfold = 5
                ,showsd = T
                ,stratified = T
                ,print.every.n = 10
                ,early.stop.round = 20
                ,maximize = F
)

min(xgbcv$test.error.mean)

xgbsm2 <- xgb.train(
  params = params
  ,data = dtrain
  ,nrounds = 49
  ,watchlist = list(train=dtrain)
  ,print.every.n = 10
  ,early.stop.round = 10
  ,maximize = F
  ,eval_metric = "error"
)

xgbpred <- predict(xgbsm2,nt2s)
xgbpred <- ifelse(xgbpred > 0.5,1,0)

df <- data.frame(PassengerId = nt$PassengerId, Survived = xgbpred)
write.csv(df, file = "xgbsimplemodel5.csv", row.names=FALSE, quote = FALSE)

