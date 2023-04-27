# Análises TCC USP - Rodrigo Napoli
# KNN (K Nearest Neighbour) 
# A variável resposta foi apresentada com 2 opções (leve+moderado vs severo)

########################
##   RESULTADO FINAL  ##
########################
# acc = 44,44%
#https://www.edureka.co/blog/knn-algorithm-in-r/

#bibliotecas
library(class)
library(caret)

#### Step 1: Import the dataset
df <-  readRDS("dados.rds")

#### Step 2: Data Cleaning
#apagando as variáveis que não serão usadas NESTE modelo
df$esteatose <- NULL
df$esteatose_dic2 <- NULL
str(df)
head(df)

#### Step 3: Data Normalization
#Normalization
df$sexo <- as.integer(df$sexo)
df$diabetes <- as.integer(df$diabetes)
df$esteatose_dic <- as.integer(df$esteatose_dic)
head(df)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

df.normal <- as.data.frame(lapply(df[,2:16], normalize))
summary (df.normal)

#### Step 4: Data Splicing
set.seed(134)
dat.d <- sample(1:nrow(df.normal),size=nrow(df.normal)*0.75,replace = FALSE) #random selection of 75% data.

train.df <- df[dat.d,] # 75% training data
test.df <- df[-dat.d,] # remaining 25% test data

#Creating seperate dataframe for 'Esteatose' feature which is our target.
train.df_labels <- df[dat.d,1]
test.df_labels <-df[-dat.d,1]

#### Step 5: Building a Machine Learning model

#Find the number of K (usually it is used by the square root of number of observations)
sqrt(NROW(train.df_labels))
knn9 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=9)
summary(knn9)

#### Step 6: Model Evaluation

# Check prediction against actual value in tabular form for k=26
tab <- table(knn9 ,test.df_labels$esteatose_dic)
tab
acc9 <- (tab [1,1]+tab[2,2])/sum(tab)
acc9

confusionMatrix(table(knn9 ,test.df_labels$esteatose_dic))

#### Step 7: Optimization

knn6 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=6)
knn7 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=7)
knn8 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=8)
knn10 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=10)
knn11 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=11)
knn12 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=12)
knn13 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=13)
knn14 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=14)
knn15 <- knn(train=train.df, test=test.df, cl=train.df_labels$esteatose_dic, k=15)

tab6 <- table(knn6 ,test.df_labels$esteatose_dic)
acc6 <- (tab6 [1,1]+tab6[2,2])/sum(tab6)

tab7 <- table(knn7 ,test.df_labels$esteatose_dic)
acc7 <- (tab7 [1,1]+tab7[2,2])/sum(tab7)

tab8 <- table(knn8 ,test.df_labels$esteatose_dic)
acc8 <- (tab8 [1,1]+tab8[2,2])/sum(tab8)

tab10 <- table(knn10 ,test.df_labels$esteatose_dic)
acc10 <- (tab10 [1,1]+tab10[2,2])/sum(tab10)

tab11 <- table(knn11 ,test.df_labels$esteatose_dic)
acc11 <- (tab11 [1,1]+tab11[2,2])/sum(tab11)

tab12 <- table(knn12 ,test.df_labels$esteatose_dic)
acc12 <- (tab12 [1,1]+tab12[2,2])/sum(tab12)

tab13 <- table(knn13 ,test.df_labels$esteatose_dic)
acc13 <- (tab13 [1,1]+tab13[2,2])/sum(tab13)

tab14 <- table(knn14 ,test.df_labels$esteatose_dic)
acc14 <- (tab14 [1,1]+tab14[2,2])/sum(tab14)

tab15 <- table(knn15 ,test.df_labels$esteatose_dic)
acc15 <- (tab15 [1,1]+tab15[2,2])/sum(tab15)

acc6
acc7
acc8
acc9
acc10
acc11
acc12
acc13
acc14
acc15

confusionMatrix(table(knn11 ,test.df_labels$esteatose_dic))

#ROC
roc_knn_test <- roc(response = test.df$esteatose_dic, predictor =as.numeric(knn11))
roc_knn_test
plot(roc_knn_test)

#######
