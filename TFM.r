require('data.table')
require('ggplot2')
require('leaps')
require('rpart')
require('caret')
require('gbm')
require('parallel')
require('foreach')
require('plyr')
require('doParallel')
require('glmnet')
require('RVowpalWabbit')
library(dplyr)
library(caTools)
library(MASS)
library(party)
library(randomForest)
library(class)

rm(list=ls(all=TRUE))



setwd("/home/dsc/Downloads/")
df = read.csv("train_clean.csv",sep=";",nrows=200000)
cols = colnames(df)

weightsTrain <- df$var11
df['fire'] <- ifelse(df$target > 0, 1, 0)
df <- df[ ,!colnames(df) %in% c("dummy",'X','id')]

for (i in cols)
{ # print(i)
  # print( mean(df[,i]) )
  if (  mean(df[,i]) == max(df[,i])  ) {print (i)
    df <- df[,!colnames(df)== i ] }
}




set.seed(999)
randomSubset <- sample.int(nrow(df), nrow(df)) #use this to use full data
#treeModel <- gbm.fit(x = df[randomSubset, c(seq(3, 19), seq(21,393))], y = df$fire[randomSubset],
treeModel <- gbm.fit(x = df[randomSubset, c(seq(1,381))], y = df$fire[randomSubset],
                     w = weightsTrain[randomSubset],
                     distribution = 'bernoulli',
                     n.trees = 1000, verbose = TRUE)


modelprobit = glm(formula = df$fire[randomSubset] ~ ., family = binomial(link = "probit"),  data = df[randomSubset, c(seq(1,381))])



reales <- df$fire
predichos <- predict(treeModel,n.trees = 1000)

# varias maneras de ver el error
sum(reales != predichos)
sum(reales = predichos)
mean(reales != predichos)
table(reales, predichos)



best.iter <- gbm.perf(treeModel)#, method = "test")

GBMClassPredictors <- summary(treeModel)
GBMClassPredictors <- as.character(GBMClassPredictors$var[GBMClassPredictors$rel.inf > 0.5])
allClassPredictors <- union(linearClassPredictors, GBMClassPredictors)


df <- df[ ,!colnames(df) %in% c("dummy",'X','id')]

cols = colnames(df)


for (i in cols)
{ # print(i)
  # print( mean(df[,i]) )
  if (  mean(df[,i])  == 0 ) {print (i)
                              df <- df[,!colnames(df)== i ] }
}



randomSubset <- sample.int(nrow(train), nrow(train)) #use this to use full data
treeModel <- gbm.fit(x = train[randomSubset, c(seq(3, 19), seq(21,302))], y = train$fire[randomSubset],
                     w = weightsTrain[randomSubset],
                     distribution = 'bernoulli', nTrain = floor(nrow(train) * 0.7), 
                     n.trees = 200, verbose = TRUE)
best.iter <- gbm.perf(treeModel, method = "test")


dfpos <-  df[df[1]>0,]

df$pos <-  ifelse(df$target==0,0,1)

head(df)


df[1] <-  sqrt(df[1])
max(df[1])

class(dfpos)


barplot(height='dfpos')

ln(e)


set.seed(1234) 
SAMPLE = sample.split(df, SplitRatio = .60)
DepositosTrain = subset(df, SAMPLE == TRUE)
DepositosValTest = subset(df, SAMPLE == FALSE)

df[df[,df$target==0]]

df2$pos <- as.factor(df2$pos)





arbol <- ctree(pos ~ ., data = df2)

arbol <- ctree(pos ~ var10+var11+var12+var13+var14+var15+var16+var17, data = df2)

var10+var11+var12+var13+var14+var15+var16+var17
plot(arbol)
summary(arbol)


# Ejercicio: repetir el ejercicio tratando de predecir la variable Area

# Â¿CuÃ¡ntos errores cometemos?
reales <- df2$target
predichos <- predict(arbol)

# varias maneras de ver el error
sum(reales != predichos)
sum(reales = predichos)
mean(reales != predichos)
table(reales, predichos)



df2 = sample_n(df,20000)

df3 = df2-df;

df2target = as.data.frame(df2[,1])

df2 = df2[,!colnames(df2)=='target']

df2 = df2[,!colnames(df2)=='pos']

arbol<- ctree(target ~ ., data = df2)


df.pca <- prcomp(df2,
                 center = TRUE,
                 scale. = TRUE) 
print(df.pca)

plot(df.pca, type = "l")

prop_acum    <- summary(df.pca)$importance[3,]

pca_v        <- prop_acum[prop_acum <= 0.999 ]
plot(pca_v)

summary(df.pca)


df.pca.pred <- as.data.frame(predict(df.pca))
df.pca.pred <- df.pca.pred[,1:250]
df.pca.pred$target <- df2target[,1]



head(df.pca.pred)

reg <- lm(target~., data=df.pca.pred)
anova(reg)
summary(reg)

regpos <- lm(target~., data=dfpos)
anova(regpos)
summary(regpos)

df2$target <- df2target[,1] 
df2$fire <- ifelse(df2)

head(df2)


cor(df2$target,df2$var10)


cols = colnames(df)

length(df2$target)
length(df2[df2$target>0,])

cor <- for (i in cols)
{  paste0(print(i),print(cor(df2$target,df2[i])))
 
}

  
reg2 <- lm(target~var15, data=df2)
#anova(reg2)
summary(reg2)

modelo3.2 <- stepAIC(reg2, direction = "both", steps = 2)
summary(modelo3.2)

plot(reg2$residuals)

log(5)
sqrt(sqrt(5))

reg3 <- glm(target~., data=df2)
anova(reg3)
summary(reg3)


reg4pos <- glm(target~., data=dfpos,family=poisson())
anova(reg4pos)
summary(reg4pos)


