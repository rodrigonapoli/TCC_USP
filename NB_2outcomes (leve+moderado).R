# Análises TCC USP - Rodrigo Napoli
# NB (Naive Bayes)
# A variável resposta foi apresentada com 2 opções (leve+moderado vs severo)

# https://www.geeksforgeeks.org/naive-bayes-classifier-in-r-programming/

# Bibliotecase
library(e1071)
library(caTools)
library(caret)

#rodando o script de preparo do banco de dados
setwd("/Users/rodrigonapoli/Dropbox/Cientista de dados/USP/ 00 - TCC/Dados")
dados <- readRDS("dados.rds")
str(dados)

#apagando as variáveis que não serão usadas NESTE modelo
dados$esteatose <- NULL
dados$esteatose_dic2 <- NULL
str(dados)

######################################
# Dividir amostras de treino e teste #
set.seed(09041978)
# Gera 75% de 1´s e 25% de 2´s para separar as amostras
n <- sample(1:2, # vamos amostrar elementos do conjunto c(1,2)
            size=nrow(dados), # O tamanho da amostragem é baseado no tamanho da base dados
            replace=TRUE, # Amostragem com reposição (de c(1,2))
            prob=c(0.75,0.25)) # A probabilidade de ser 1 é 75%, de ser 2 é 25%

table (n)
n

# Amostra de treino: n==1 (os 75%)
treino <- dados[n==1,]
# Amostra de teste: n==2 (os 25%)
teste <- dados[n==2,]
head(treino)

######## NAIVE BAYES
classificador <- naiveBayes(esteatose_dic ~ ., data = treino)
classificador

# Predizendo o dataset de teste
predito <- predict(classificador, newdata = teste)

# Matriz de confusão
tab <- table(teste$esteatose_dic, predito)
tab
confusionMatrix(tab)

#ROC
roc_NB_test <- roc(response = teste$esteatose_dic, predictor =as.numeric(predito))
roc_NB_test
plot(roc_NB_test)

#######

