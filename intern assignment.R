train1 <- read.csv(file = "Downloads/bank-additional-full.csv", sep = ";")
View(train1)
# Removing unknown values
table(train1$y)
train1[train1=="unknown"]<-NA
train2 <- na.omit(train1)
table(train2$y)
#Releveling poutcome column so as to get nonexistent as the base category
train3 <- within(train2, poutcome <- relevel(poutcome, ref = "nonexistent"))
train3
# Checking whether consumer price and consumer confidence index has high correlation
cor(cbind(train3$cons.price.idx, train3$cons.conf.idx))
# Changing pdays range to ordered levels of 1-5 and assigning Not applicable (999) as 0
table(train3$pdays)
train3$pdays <- ifelse(train3$pdays <= 3, 1, ifelse(train3$pdays <= 7, 2, ifelse(train3$pdays <= 11, 3, 
ifelse(train3$pdays <= 15, 4, ifelse(train3$pdays <= 30, 5, 0)))))
train3$pdays
# Using Logistic regression in Model with all values
train4_glm <- glm(y ~ ., family="binomial", data=train2, na.action=na.omit)
train4_glm
# Selecting ideal columns based on AIC and p values
install.packages("MASS")
library(MASS)
step <- stepAIC(train4_glm, direction="both")
step
# Model based on most significant columns and most information
train4_glm <- glm(y ~ poutcome*previous+job+education+contact+month+day_of_week+campaign+
pdays+emp.var.rate+cons.price.idx+cons.conf.idx+nr.employed, family="binomial", data=train2, na.action=na.omit)
train4_glm
# Residuals and terms
install.packages("ggplot2")
library(ggplot2)
summary(train4_glm)
train5_plot <- train2[, c("poutcome", "previous", "job", "education", "contact", "month", "day_of_week", "duration", "campaign", "pdays", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "nr.employed")]
train5_plot
terms <- predict(train4_glm, type='terms')
terms
partial_resid <- resid(train4_glm) + terms
partial_resid
df <- data.frame(predictor = train5_plot[, 'pdays'], 
                 terms = terms[, "pdays"], 
                 partial_resid = partial_resid[, 'pdays'])
df
ggplot(df, aes(x=predictor, y=partial_resid, solid=FALSE)) +
geom_point(shape=46, alpha=0.4) +
geom_line(aes(x=predictor, y=terms), 
color='red', alpha=0.5, size=1.5) + 
labs(y='Partial Residual', x=colnames(train2)[13])
# Applying splines to the model
install.packages("mgcv")
library(mgcv)
train4_glm <- gam(y ~ poutcome*previous+job+education+contact+month+day_of_week+s(duration)+s(campaign)+
                    pdays+emp.var.rate+cons.price.idx+cons.conf.idx+nr.employed
                  , family="binomial", data=train2, na.action=na.omit)

train4_glm
# Calculating McFaddens pseudo R squared
nullmod <- glm(y ~ 1, family="binomial", data=train1)
nullmod
# R2
1-logLik(train4_glm)/logLik(nullmod)
# Adj R2
(1-logLik(train4_glm)/logLik(nullmod))*((24382-1)/(24382-52-1))
# Graph of prediction probability
predicted.data1 <- data.frame(probability.of.response=train4_glm$fitted.values,response=train2$y)
predicted.data1
predicted.data2 <- predicted.data[order(predicted.data$probability.of.response, decreasing=FALSE),]
predicted.data1$rank <- 1:nrow(predicted.data1)
predicted.data1$rank
install.packages("cowplot")
library(cowplot)
ggplot(data=predicted.data1, aes(x=rank, y=probability.of.response)) +
geom_point(aes(color=resp), alpha=1, shape=4, stroke=2) +
xlab("Index") +
ylab("Predicted probability of response")
# Testing the model
# 5 Fold cross validation
# Splitting train data into 5 folds
ind <- sample(5, nrow(train2), replace=TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
ind
a1 <- train2[ind==1, ]
a1
a2 <- train2[ind==2, ]
a2
a3 <- train2[ind==3, ]
a3
a4 <- train2[ind==4, ]
a4
a5 <- train2[ind==5, ]
a5
# Combining train block for 5th interation
train <- rbind(a5, a2, a3, a4)
train
# Building test model
train6_tst <- gam(y ~ poutcome*previous+job+education+contact+month+day_of_week+s(duration)+s(campaign)+
pdays+emp.var.rate+cons.price.idx+cons.conf.idx+nr.employed, family="binomial", data=train2, na.action=na.omit)
attributes(train6_tst)
train6_tst
prob_pred <- predict(train6_tst, type = 'response', newdata=gp1)
prob_pred
y_pred <- ifelse(prob_pred > 0.5, 'yes', 'no')
y_pred
y_true <- as.numeric(gp1$y)
y_true
# ROC curve for test
attributes(train6_tst)
prob_pred1 <- predict(train6_tst, newdata=gp1)
prob_pred1
y_pred1 <- as.numeric(prob_pred > 0)
y_pred1
y_true1 <- as.numeric(gp1$y == 'yes')
y_true1
true_pos <- (y_true==1) & (y_pred==1)
true_neg <- (y_true==0) & (y_pred==0)
false_pos <- (y_true==0) & (y_pred==1)
false_neg <- (y_true==1) & (y_pred==0)
idx <- order(-prob_pred)
idx
recall <- cumsum(y_true[idx] == 1)/sum(y_true==1)
recall
specificity <- (sum(y_true==0) - cumsum(y_true[idx]==0))/sum(y_true==0)
specificity
roc_df <- data.frame(recall = recall, specificity = specificity)
roc_df
ggplot(roc_df, aes(x=specificity, y=recall)) +
geom_line(color='blue') +
scale_x_reverse(expand=c(0,0)) +
scale_y_continuous(expand=c(0,0)) +
geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x),
linetype='dotted', color='red')
# Calculating AUC
sum(roc_df$recall[-1] * diff(1 - roc_df$specificity))
table(a1$y, y_pred)
# Final model
train4_glm <- gam(y ~ poutcome*previous+job+education+contact+month+day_of_week+s(duration)+s(campaign)+
pdays+emp.var.rate+cons.price.idx+cons.conf.idx+nr.employed, family="binomial", data=train2, na.action=na.omit)
