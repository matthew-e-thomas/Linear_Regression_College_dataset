library(ISLR)
data(College)

#Check acceptance rate against different regressors
College$AccRate <- College$Accept/College$Apps
College$Enroll.Rate <- College$Enroll / College$Accept
College$Selective <- rep(0, nrow(College))
College$Selective[College$AccRate < 0.4] = 1
plot(College$Top10perc, log(College$AccRate/(1-College$AccRate)))
plot(College$Top10perc, College$AccRate)
plot(College$Outstate, log(College$AccRate/(1-College$AccRate)))
plot(College$Outstate, College$AccRate)
plot(PhD, log(AccRate/(1-AccRate)))
plot(PhD, AccRate)

#subsetting
None_small <- subset(College, Enroll > 1000)
result <- glm(None_small$AccRate ~ None_small$Top10perc + None_small$perc.alumni + None_small$PhD, family="binomial", weights=None_small$Apps)
summary(result)

result_small <- lm(Accept ~ Top10perc + Enroll + Outstate + Expend, data=None_small)
summary(result_small) #not that great

plot(College$Grad.Rate, log(College$AccRate/(1-College$AccRate)))
plot(College$Grad.Rate, College$AccRate)

result1 <- glm(College$AccRate ~ College$Top10perc + College$perc.alumni + College$PhD, family="binomial", weights=College$Apps)
summary(result1)
1-pchisq(result$deviance, 170)

preds <- cbind(None_small$Private, None_small$Apps, None_small$Accept, None_small$Enroll, None_small$Top10perc, None_small$Top25perc, None_small$F.Undergrad, None_small$P.Undergrad, None_small$Outstate, None_small$Room.Board, None_small$Books, None_small$Personal, None_small$PhD, None_small$Terminal, None_small$S.F.Ratio, None_small$perc.alumni, None_small$Expend, None_small$Grad.Rate, None_small$AccRate)
cor(preds)

Public <- subset(College, Private == "No")
Public$Private <- NULL


#Check different linear regression models
library(leaps)

regnull1 <- lm(AccRate ~1,data=None_small)
##model with all predictors
regfull1 <- lm(AccRate ~.,data=None_small)

##forward selection, backward elimination, and stepwise regression
step(regnull1, scope=list(lower=regnull1, upper=regfull1), direction="forward")

result2 <- lm(AccRate ~ Top10perc + Enroll + Outstate + 
  Private + P.Undergrad + S.F.Ratio, data=None_small)
summary(result2)

regnull2 <- lm(AccRate ~ 1, data=Public)
regfull2 <- lm(AccRate ~ ., data=Public)
step(regnull2, scope=list(lower=regnull2, upper=regfull2), direction="forward")



#Try all regressions against Acceptances for Public Universities
allreg <- regsubsets(Accept ~ Apps + Enroll + Top25perc + F.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate , data=Public, nbest=9)
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

best[order(best$r2),]
best[order(best$adjr2),]
best[order(best$mse),]
best[order(best$cp),]
best[order(best$bic),]

result_Public <- lm(formula = Accept ~ Apps + Enroll + Top25perc + Outstate + Expend,
                    data = Public)
summary(result_Public) #strong predictors

#try same thing with logistic using acc rate
log_public <- glm(AccRate ~ Enroll + Top25perc  + Outstate + Expend, data=Public,family='binomial', weights=Apps)
summary(log_public)
1-pchisq(log_public$deviance, 207) #doesn't fit well

log_enroll_public <- glm(Enroll.Rate ~ Apps + Top25perc + Outstate + Expend + PhD + perc.alumni  + Grad.Rate, data=Public, family='binomial', weights=Accept)
summary(log_enroll_public)
1-pchisq(log_enroll_public$deviance, 207)

#plot AccRate versus Acceptances for Public
plot(Public$Top25perc, Public$Accept)
plot(Public$Top25perc, log(Public$AccRate/(1-Public$AccRate))) #the correlation looks better but the variance is higher

log_enroll_public <- glm(Enroll.Rate ~ Apps + Top25perc + Outstate + Expend, data=Public, family='binomial', weights=Accept)
summary(log_enroll_public)
1-pchisq(log_enroll_public$deviance, 207)

#Try using 'Selective' predictor
reg_selective <- glm(Selective ~ Top10perc, data=College, family="binomial")
summary(reg_selective)
1-pchisq(reg_selective$deviance, 775)

set.seed(101)

sample<-sample.int(nrow(College), floor(.50*nrow(College)), replace = F)
train<-College[sample, ]
test<-College[-sample, ]

Selective_train <- glm(Selective ~ Top10perc, data=train, family='binomial')

library(ROCR)

preds<-predict(Selective_train,newdata=test, type="response")


rates<-prediction(preds, test$Selective)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Predicting Selective Schools")
lines(x = c(0,1), y = c(0,1), col="red")
auc<-performance(rates, measure = "auc")
auc

table(test$Selective, preds>0.5)

regnull <- glm(AccRate ~1, family="binomial",data=College, weights=Apps)
##model with all predictors
regfull <- glm(AccRate ~., family="binomial",data=College, weights=Apps)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")

summary(regfull)

#All predictors are good predictors of acceptance rate in logistic regression,
# but the residual deviance is still high

preds1 <- cbind(Private, Apps, Accept, Enroll, Top10perc, Top25perc, F.Undergrad, P.Undergrad, Outstate, Room.Board, Books, Personal, PhD, Terminal, S.F.Ratio, perc.alumni, Expend, Grad.Rate, College$AccRate)
cor(preds1)

#regression on enrollment rate
result_enrollment <- lm(College$Enroll.Rate ~ Expend + Top25perc + F.Undergrad + S.F.Ratio + perc.alumni + College$AccRate + Private)
summary(result_enrollment)

b <- c(-Inf, median(Boston$crim), quantile(Boston$crim, .75), Inf)
names <- c("Low", "Medium", "High")
Boston$crim.level <- cut(Boston$crim, breaks = b, labels = names)