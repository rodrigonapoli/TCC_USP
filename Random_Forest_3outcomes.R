# Análises TCC USP - Rodrigo Napoli
# RANDOM FOREST 
# A variável resposta foi apresentada com aS 3 OPCÕES
# Valores NA nas variáveis foram removidos


########################
##   RESULTADO FINAL  ##
########################
# Variáveis preditoras:  tri, vitaminda D, diabetes, ... classifier$importance
# acc = 46,66%


#bibliotecas
library(randomForest)

#rodando o script de preparo do banco de dados
setwd("/Users/rodrigonapoli/Dropbox/Cientista de dados/USP/ 00 - TCC/Dados")
dados <- readRDS("dados.rds")
str(dados)

#apagando as variáveis que não serão usadas NESTE modelo
dados$esteatose_dic <- NULL
dados$esteatose_dic2 <- NULL
str(dados)


######################################
# Dividir amostras de treino e teste #
set.seed(1234)
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

#RANDOM FOREST
treino_rf <-  randomForest(
  esteatose ~ ., 
  data = treino, 
  ntree = 500, # número de árvores desenvolvidas
  mtry = 5, # número de variáveis avaliadas por árvore
  maxnode=12, #número máximo de nodos no final
  importance = T, #registrar a imprtancia das variáveis
  replace = T, #usar reposição no processo de reamostragem
  type = "classification")

print(treino_rf)

#Predizendo os resultados do "teste" com o modelo de Random Forest gerdo
predito = predict(treino_rf, newdata = teste[-1])
predito

#Matriz de confusão - Decision Tree
cm = table(teste$esteatose, predito)
cm
acc <- ((cm[1,1]+cm[2,2]+cm[3,3])/sum(cm))
acc

confusionMatrix(table(predito ,teste$esteatose))


treino_rf$importance
varImpPlot(treino_rf,
           sort=TRUE, n.var=min(30, nrow(treino_rf$importance)),
           type=NULL, class=Accuracy, scale=TRUE, 
           main=deparse(substitute(Importância))) 

#Added bonus let’s visualize the Decsion Trees
plot(treino_rf)


#Multi-ROC
roc_knn_test <- multiclass.roc(response = teste$esteatose, predictor =as.numeric(predito))
roc_knn_test
plot(roc_knn_test)

#######


