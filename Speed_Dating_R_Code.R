dating <- read.csv('copy11.csv')

str(dating)
dating$gender <- as.factor(dating$gender)
dating$samerace <- as.factor(dating$samerace)
dating$metp <- as.factor(dating$metp)
dating$decision <- as.factor(dating$decision)

#Higher ambition
dating <- setDT(dating)
dating$higher_amb <- as.numeric(dating[,dating$ambp>dating$ambs])
dating$higher_amb <- as.factor(dating$higher_amb)

#Higher attraction
dating[1:20]
dating$higher_attr <- as.numeric(dating[,dating$attrp>dating$attrs])
dating$higher_attr <- as.factor(dating$higher_attr)

#Higher intelligence
dating$higher_intel <- as.numeric(dating[,dating$intelp>dating$intels])
dating$higher_attr <- as.factor(dating$higher_attr)

#Datatypes
dating$age_p <- as.numeric(dating$age_p)
dating$age_s <- as.numeric(dating$age_s)

dating$attrp <- as.numeric(dating$attrp)
dating$intel <- as.numeric(dating$intel)
dating$funp <- as.numeric(dating$funp)
dating$ambp <- as.numeric(dating$ambp)
dating$sincp <- as.numeric(dating$sincp)
dating$sharep <- as.numeric(dating$sharep)

dating$age_s <- as.numeric(dating$age_s)
dating$age_p <- as.numeric(dating$age_p)

dating1 <- dating

#Histograms

hist(dating$attrp, main = "Attraction")
hist(log(dating$sincp), main = "Sincere - Log Transformation") #no transformation to address this skewness
hist(dating$intelp, main = "Intelligence")
hist(dating$funp, main = "Fun")
hist(dating$ambp, main= "Ambition")
hist(dating$sharep, main = "Shared Interests")
hist(dating$age_p, main="Age of the participants")
hist(scale(dating$age_s), main="Age of the subjects")



# Statistical analysis
summary(dating$attrp)
summary(dating$sincp)
summary(dating$intelp)
summary(dating$funp)
summary(dating$ambp)
summary(dating$sharep)

table(dating$gender)
table(dating$decision)
table(dating$samerace)
table(dating$higher_amb)
table(dating$higher_attr)
table(dating$higher_intel)

#boxplots
plot(dating$decision,dating$attrp,xlab="Decision",ylab="Attraction",main="Attraction Vs Decision to Date - With outliers")

#removing outliers

dating5 <- as.data.frame(dating)
dating5 <- dating5[!(dating5$decision==1 & dating5$attrp<3),]
dating5 <- dating5[!(dating5$decision==0 & dating5$attrp>9),]
dating5 <- dating5[!(dating5$decision==0 & dating5$attrp<1),]
?plot

plot(dating5$decision,dating5$attrp,xlab="Decision",ylab="Attraction",main="Attraction Vs Decision to Date - Without outliers", type="b")

M = tapply(dating5$attrp,
           INDEX = dating5$decision,
           FUN   = mean)

boxplot(dating5$attrp~dating5$decision,
        data=dating5,
        ylab="Attraction", xlab="Decision", main="Attraction vs Decision to Date with outliers")

points(M,
       col="red",
       pch="+",
       cex=2)
try <- sqrt(50-dating$sincp)
hist(try)

plot(dating$decision,dating$sincp)
plot(dating$decision,dating$intelp)
plot(dating$decision, dating$funp)
plot(dating$decision, dating$ambp)
plot(dating$decision, dating$sharep)
plot(dating$decision, dating$higher_amb)

dating_new <- dating
dating_new <- setDT(dating_new)
list <- c("attrp","sincp","funp","intelp","funp","sharep","higher_amb","higher_intel","higher_attr")
dating_new <- dating_new[,c("attrp","sincp","funp","intelp","funp","sharep","higher_amb","higher_intel","higher_attr")]
head(dating_new)
res <- rcorr(as.matrix(dating_new))

#chi-square 
chisq.test(dating$decision,)
fit <- Mclust(dating)
plot(fit)
summary(fit)
table(dating$higher_intel)

dating$intel <- NULL
model_full <- glm(biggest,data=dating, family = binomial)
summary(model_full)
a <-residuals(model_full)
b <- fitted.values(model_full)
plot(a,b)


model_div <- glm(decision~int_corr+samerace+age_s+age_p+attrs+sincs+attrp+sincp+funs+funp+ambs+ambp+sharep+intels+intelp+metp+higher_amb+higher_intel+higher_attr, data=dating, family=binomial)
c <-residuals(model_div, type="deviance")
d <- log(fitted.values(model_div))
glm.diag.plots(model_div)
plot(c,d)
range(a)
graphics.off()



?step
model_reduced <- step(model_full, direction = "forward")
summary(model_reduced)


#model1 <- glm(decision~attrp+sincp+intelp+funp+int_corr, data=dating, family=binomial)
#summary(model1)

#Forward selection

biggest <- decision~gender+samerace+attrp+sincp+funp+ambp+sharep+intelp+higher_amb+higher_intel+higher_attr
model_m <- glm(decision~1, data=dating, family=binomial)
add1(model_m, scope=biggest)
model_m2 <- glm(decision~gender+int_corr+samerace+age_s)


datingf <- dating[dating$gender==0]
datingm <- dating[dating$gender==1]

modelf <- glm(decision~attrp+sincp+intelp+funp+int_corr, data=datingf, family=binomial)
summary(modelf)

#annova
model_full <- glm(decision~gender+samerace+attrp+sincp+funp+ambp+sharep+intelp+higher_amb+higher_intel+higher_attr, data=dating, family=binomial)
model_reduced <- glm(decision~samerace+attrp+sincp+funp+ambp+sharep+intelp+higher_amb+higher_intel+higher_attr, data=dating, family=binomial)
?anova
anova(model_reduced, model_full, test="Chisq")
summary(an)

chisq.test(dating)

#Data Partitioning
dating$male <- dating$gender
datingf <- dating[dating$male==0]
datingm <- dating[dating$male==1]
nrow(datingf)
nrow(datingm)

#Data Split
# 
dating_female <- split_df(datingf, ratio=0.70, seed=123)
nrow(dating_ft) <- dating_female$train
dating_fv <- dating_female$test

# dating_male <- split_df(datingm, ratio=0.70, seed=321)
# dating_mt <- dating_male$train
# dating_mv <- dating_male$test


write.csv(datingf, "datingf.csv", row.names = FALSE)
write.csv(datingm, "datingm.csv", row.names = FALSE)

#For female data
model_full_f <- glm(decision~(samerace+attrp+sincp+funp+ambp+sharep+intelp+higher_amb+higher_intel+higher_attr)^2, data=datingf, family=binomial)
BIC(model_full_f)
model_bestf <- step(model_full_f, type = "backward", k=log(nrow(datingf)))
summary(model_bestf)
BIC(model_bestf)

#for male data
model_full_m <- glm(decision~(samerace+attrp+sincp+funp+ambp+sharep+intelp+higher_amb+higher_intel+higher_attr)^2, data=datingm, family=binomial)
AIC(model_full_m)
model_bestm <- step(model_full_m, type = "backward", k=log(nrow(datingm)))
summary(model_bestm)

biggest <- decision~samerace+attrp+sincp+funp+ambp+sharep+intelp+higher_amb+higher_intel+higher_attr

model_best_f2 <- glm(decision~1, data=dating, family=binomial)
add1(model_best_f2, scope=biggest)
