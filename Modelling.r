library(caTools)
library(dplyr)
library(randomForest)
library(e1071)
library(ggplot2)



#limpieza de objetos
rm(list=ls(all=TRUE))

setwd("c:/master/")
df = read.csv("clean.csv",sep=";")


#Eliminación de algunas variables que no entraran en el análisis
cols = colnames(df)
df <- df[ ,!colnames(df) %in% c("dummy",'X','id')]


#Eliminación de variables nulas o con un solo valor

cols = colnames(df)
for (i in cols)
{  
  if (  var(df[,i])==0  ) {print (paste("Variable a elminar: ",i))
  df <- df[,!colnames(df)== i ] }
}

#Partición de la muestra en train y validación

set.seed(999)
Sample  <- sample.split(df, SplitRatio = .80)
dfTrain <- subset(df, Sample == TRUE)
dfValid  <- subset(df, Sample == FALSE)

target <- dfTrain$target

###########################################################################################
###########################################################################################
##                        MODELO DE REGRESIÓN LINEAL
###########################################################################################
###########################################################################################

reg <- lm(target~., data=dfTrain)
anova(reg)
summary(reg)

dfreg <- as.data.frame(target)
dfreg$pred   <- predict(reg,data=dfTrain)

# Vamos a valorar la capacidad del modelo en terminos de la variable target 
# como si fuera dicotómica: 0, ó 1 si el target es positivo
# Buscamos un valor limite que maximice el número de éstos aciertos, asignando
# valor cero a la variable target a los que no superen dicho límite

maxprecisionreg <-0
for (i in seq(5000))
{ 
  Precision <- sum(dfreg$target>0 & dfreg$pred>=1/i) + sum(dfreg$target==0 & dfreg$pred<=1/i)
  ifelse(Precision>maxprecisionreg,maxprecisionreg <- 1/i,0)
}

limitreg <- maxprecisionreg

# La nueva predicción en términos 0,1

dfreg$prednew <- ifelse(dfreg$pred<limitreg,0,dfreg$pred)

regplot <- ggplot(dfreg, aes(x = dfreg$target, y = dfreg$prednew))+
  geom_point() +
  xlab("Valor Real") +
  ylab("Predicción") +
  ggtitle("Valor Real Target vs Predicción Reg Target")
regplot

# Número de aciertos en terminos de la variable target (0,1)

aciertos_reg <- sum(dfreg$target==0 & dfreg$prednew==0) + sum(dfreg$target>0 & dfreg$prednew>0)

errores_reg <-  sum(dfreg$target==0 & dfreg$prednew>0) + sum(dfreg$target>0 & dfreg$prednew==0)

Por_aciertos_reg <- aciertos_reg/(aciertos_reg+errores_reg)
Por_aciertos_reg
# Suma de errores como Error  cometido por éste modelo

SumErr_reg <- sum(abs(dfreg$target-dfreg$pred))/count(dfreg)
SumErr_reg

###########################################################################################
###########################################################################################
##                        MODELO RANDOMFOREST
###########################################################################################
###########################################################################################


forest <-randomForest(target ~ ., 
                      data=dfTrain, 
                      ntree=30,     
                      mtry=8,       
                      replace=T)  
summary(forest)
plot(forest)

dfforest <- as.data.frame(target)
dfforest$pred   <- predict(forest,data=dfTrain)


# Vamos a valorar la capacidad del modelo en terminos de la variable target 
# como si fuera dicotómica;0 ó 1 si el target es positivo
# Buscamos un valor limite que maximice el número de éstos aciertos, asignando
# valor cero a la variable target a los que no superen dicho límite

maxprecisionforest <-0
for (i in seq(5000))
{ 
  Precision <- sum(dfforest$target>0 & dfforest$pred>=1/i) + sum(dfforest$target==0 & dfforest$pred<=1/i)
  ifelse(Precision>maxprecisionforest,maxprecisionforest <- 1/i,0)
}

limitforest <- maxprecisionforest

# La nueva predicción en términos 0,1

dfforest$prednew <- ifelse(dfforest$pred<limitforest,0,dfforest$pred)

forestplot <- ggplot(dfforest, aes(x = dfforest$target, y = dfforest$prednew))+
  geom_point() +
  xlab("Valor Real") +
  ylab("Predicción") +
  ggtitle("Valor Real Target vs Predicción forest Target")
forestplot


# Número de aciertos en terminos de la variable target (0,1)

aciertos_forest <- sum(dfforest$target==0 & dfforest$prednew==0) + sum(dfforest$target>0 & dfforest$prednew>0)

errores_forest <-  sum(dfforest$target==0 & dfforest$prednew>0) + sum(dfforest$target>0 & dfforest$prednew==0)

Por_aciertos_forest <- aciertos_forest/(aciertos_forest+errores_forest)
Por_aciertos_forest

# Suma de errores como Error  cometido por éste modelo

SumErr_forest <- sum(abs(dfforest$target-dfforest$pred))/count(dfforest)
SumErr_forest


###########################################################################################
###########################################################################################
##                        MODELO GLM
###########################################################################################
###########################################################################################


glm <- glm(target~., data=dfTrain)
#dfTrain$glmpredict     <- predict(glm,data=dfTrain)
summary(glm)



dfglm <- as.data.frame(target)
dfglm$pred   <- predict(glm,data=dfTrain)


# Vamos a valorar la capacidad del modelo en terminos de la variable target 
# como si fuera dicotómica;0 ó 1 si el target es positivo
# Buscamos un valor limite que maximice el número de éstos aciertos, asignando
# valor cero a la variable target a los que no superen dicho límite

maxprecisionglm <-0
for (i in seq(5000))
{ 
  Precision <- sum(dfglm$target>0 & dfglm$pred>=1/i) + sum(dfglm$target==0 & dfglm$pred<=1/i)
  ifelse(Precision>maxprecisionglm,maxprecisionglm <- 1/i,0)
}

limitglm <- maxprecisionglm

# La nueva predicción en términos 0,1

dfglm$prednew <- ifelse(dfglm$pred<limitglm,0,dfglm$pred)

glmplot <- ggplot(dfglm, aes(x = dfglm$target, y = dfglm$prednew))+
  geom_point() +
  xlab("Valor Real") +
  ylab("Predicción") +
  ggtitle("Valor Real Target vs Predicción glm Target")
glmplot


# Número de aciertos en terminos de la variable target (0,1)

aciertos_glm <- sum(dfglm$target==0 & dfglm$prednew==0) + sum(dfglm$target>0 & dfglm$prednew>0)

errores_glm <-  sum(dfglm$target==0 & dfglm$prednew>0) + sum(dfglm$target>0 & dfglm$prednew==0)

Por_aciertos_glm <- aciertos_glm/(aciertos_glm+errores_glm)
Por_aciertos_glm

# Suma de errores como Error  cometido por éste modelo

SumErr_glm <- sum(abs(dfglm$target-dfglm$pred))/count(dfglm)
SumErr_glm

###########################################################################################
###########################################################################################
##                        MODELO SVM
###########################################################################################
###########################################################################################


svm <- svm(target~., data=dfTrain,kernel="radial")
dfsvm <- as.data.frame(target)
dfsvm$pred   <- predict(svm,data=dfTrain)



summary(svm)
# Vamos a valorar la capacidad del modelo en terminos de la variable target 
# como si fuera dicotómica;0 ó 1 si el target es positivo
# Buscamos un valor limite que maximice el número de éstos aciertos, asignando
# valor cero a la variable target a los que no superen dicho límite

maxprecisionsvm <-0
for (i in seq(5000))
{ 
  Precision <- sum(dfsvm$target>0 & dfsvm$pred>=1/i) + sum(dfsvm$target==0 & dfsvm$pred<=1/i)
  ifelse(Precision>maxprecisionsvm,maxprecisionsvm <- 1/i,0)
}

limitsvm <- maxprecisionsvm

# La nueva predicción en términos 0,1

dfsvm$prednew <- ifelse(dfsvm$pred<limitsvm,0,dfsvm$pred)

svmplot <- ggplot(dfsvm, aes(x = dfsvm$target, y = dfsvm$prednew))+
  geom_point() +
  xlab("Valor Real") +
  ylab("Predicción") +
  ggtitle("Valor Real Target vs Predicción svm Target")
svmplot


# Número de aciertos en terminos de la variable target (0,1)

aciertos_svm <- sum(dfsvm$target==0 & dfsvm$prednew==0) + sum(dfsvm$target>0 & dfsvm$prednew>0)
aciertos_svm
errores_svm <-  sum(dfsvm$target==0 & dfsvm$prednew>0) + sum(dfsvm$target>0 & dfsvm$prednew==0)
errores_svm
Por_aciertos_svm <- aciertos_svm/(aciertos_svm+errores_svm)
Por_aciertos_svm
# Suma de errores como Error  cometido por éste modelo

SumErr_svm <- sum(abs(dfsvm$target-dfsvm$pred))/count(dfsvm)
SumErr_svm

###########################################################################################
## Dada la singularidad de la varaible target, con un 99% de ceros, se plantea un modelo mixto
## partiendo del random forest que acierta en un 79% si la variable target es 0 o positiva,
## y además un modelo que estime el valor de target solo en los casos positivos
###########################################################################################


dfTrainPos <- dfTrain[dfTrain$target>0,]

#Eliminación de variables nulas o con un solo valor
cols = colnames(dfTrainPos)
for (i in cols)
{   if (  var(dfTrainPos[,i])==0  ) {print (paste("Variable a elminar: ",i))
  dfTrainPos <- dfTrainPos[,!colnames(dfTrainPos)== i ] }
}

# regresión lineal
regpos <- lm(target~., data=dfTrainPos)
anova(regpos)
summary(regpos)

# random forest
forestpos <-randomForest(target ~ ., 
                      data=dfTrainPos, 
                      ntree=30,     
                      mtry=8,       
                      replace=T)  
anova(forestpos)
summary(forestpos)

# glm
glmpos <- glm(target~., data=dfTrainPos)
anova(glmpos)
summary(glmpos)

# svm
svmpos <- svm(target~., data=dfTrainPos)
anova(svmpos)
summary(svmpos)

# Errores de previsión de los modelos
errorpred <- sum(abs(predict(regpos,data=dfTrainPos)-dfTrainPos$target))
errorglm  <- sum(abs(predict(glmpos,data=dfTrainPos)-dfTrainPos$target))
errorforest <- sum(abs(predict(forestpos,data=dfTrainPos)-dfTrainPos$target))
errorsvm <- sum(abs(predict(svmpos,data=dfTrainPos)-dfTrainPos$target))

errorpred
errorglm
errorforest
errorsvm



# Aquellos que el modelo forest le daba un valor de target menor que el límite le
# damos directamente cero, y al resto que esté como 1 le damos la previsión del modelo
# específico para valores positivos


predforest <- predict(forest,dfTrain)
predsvm <- predict(svmpos,newdata=dfTrain)


prevmixta <- ifelse(predforest<limitforest,0,1) * ifelse(predsvm<0,0,predsvm)

dfTrain$prevmixta <- prevmixta

errormed_prevmixta <- sum(abs(prevmixta-dfTrain$target))/count(dfTrain)
errormed_prevmixta

# Aplicamos por tanto el Random Forest al dataframe de Test al ser el mejor
# modelo de los probados


predValid <- predict(forest,newdata=dfValid)
errormedvalid <- sum(abs(predValid-dfValid$target))/count(dfValid)
errormedvalid

validplot <- ggplot(dfValid, aes(x = dfValid$target, y = predValid))+
  geom_point() +
  xlab("Valor Real") +
  ylab("Predicción") +
  ggtitle("Valor Real Target vs Predicción Target Validación")
validplot



# Solo queda aplicar el modelo seleccionado al dataframe test

df = read.csv("test_clean.csv",sep=";")


#Eliminación de algunas variables que no entraran en el análisis
cols = colnames(df)
df <- df[ ,!colnames(df) %in% c("dummy",'X','id')]


#Eliminación de variables nulas o con un solo valor

cols = colnames(df)
for (i in cols)
{  
  if (  var(df[,i])==0  ) {print (paste("Variable a elminar: ",i))
    df <- df[,!colnames(df)== i ] }
}

prevtarget_test <- predict(forest,newdata=df)


