# Análises TCC USP - Rodrigo Napoli
# SVM (Support Vector Machine)
# A variável resposta foi apresentada com 2 opções (leve+moderado VS severo)

########################
##   RESULTADO FINAL  ##
########################
# acc = 87,5%

# https://odsc.medium.com/build-a-multi-class-support-vector-machine-in-r-abcdd4b7dab6

library(ggplot2) #grafico
library(tidyverse)
library(e1071) #para rodar o SVM
library(dplyr) #para adicinoar linhas no ajuste do gama e custo
library(caret) #para fazer o comando de confusionMatrix

#rodando o script de preparo do banco de dados
setwd("/Users/rodrigonapoli/Dropbox/Cientista de dados/USP/ 00 - TCC/Dados")
dados <- readRDS("dados.rds")

#apagando as variáveis que não serão usadas NESTE modelo
dados$esteatose <- NULL
dados$esteatose_dic2 <- NULL
str(dados)

#padronizando os dados quantitativo
dados2 <- as.data.frame(scale(dados[4:16]))
dados_norm <- cbind(dados[1:3],dados2)



#preparando os grupos de teste e treino
n <- nrow(dados)  #definindo o número total de observações
ntreino <- round(n*0.75)  # tamanho de 75% do N para treinamento
set.seed(090478)    #Para obter resultados replicáveis
index <- sample(n, ntreino)   # criando um sorteio de números da população
traino_dados <- dados_norm[index,]   # criando um df de treino
teste_dados <- dados_norm[-index,]   # criando um df de teste

#definindo os parâmetros para rodar o modelo
g <- 0.1 #gama
c <- 10 #cost

#rodando o modelo de SVM
svm1 <- svm(esteatose_dic~., data=traino_dados, 
              method="C-classification", kernal="radial", 
              gamma=g, cost=c)

summary(svm1)
#svm1$SV

#plot(svm1, train_dados, imc ~ cp, slice=list(tgo=7, tgp=12))

#fazendo as predições do modelo na base de teste
predict <- predict(svm1, teste_dados)

#criando uma matrix de confusão
tab <- table(teste_dados$esteatose_dic, predict)
tab

#medindo a qualidade do modelo
acc <- (tab[1,1]+tab[2,2])/sum(tab) # Calculando a acurácia
acc
confusionMatrix(table(predict ,teste_dados$esteatose_dic))

#ROC
roc_svm_test <- roc(response = teste_dados$esteatose_dic, predictor =as.numeric(predict))
roc_svm_test
plot(roc_svm_test)

#######