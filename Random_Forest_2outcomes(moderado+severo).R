# Análises TCC USP - Rodrigo Napoli
# RANDOM FOREST 
# A variável resposta foi apresentada com aS 2 OPCÕES (Leve VS Moderado+Severo)
# Valores NA nas variáveis foram removidos


########################
##   RESULTADO FINAL  ##
########################
# Variáveis preditoras:  tri, vitaminda D, diabetes, ... classifier$importance
# acc = %


#bibliotecas
library(randomForest)

#rodando o script de preparo do banco de dados
setwd("/Users/rodrigonapoli/Dropbox/Cientista de dados/USP/ 00 - TCC/Dados")
dados <- readRDS("dados.rds")
str(dados)

#apagando as variáveis que não serão usadas NESTE modelo
dados$esteatose <- NULL
dados$esteatose_dic <- NULL
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
  esteatose_dic2 ~ ., 
  data = treino, 
  ntree = 500, # número de árvores desenvolvidas
  mtry = 5, # número de variáveis avaliadas por árvore
  maxnode=12, #número máximo de nodos no final
  importance = T, #registrar a imprtancia das variáveis
  replace = T, #usar reposição no processo de reamostragem
  type = "classification")

print(treino_rf)

#Predizendo os resultados do "teste" com o modelo de Random Forest gerdo
predito = predict(treino_rf, newdata = teste[-1], type = "prob")
predito2 = predict(treino_rf, newdata = teste[-1], type = "response")

#Matriz de confusão - Decision Tree
class2 = base::factor(ifelse(predito[,2] >= 0.5, "2_3", "1"))
head(class2)

tab = table(class2, teste$esteatose_dic2)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab) #acurácia do modelo
sen <- (tab[1,1])/ (tab[1,1] + tab[2,1]) #sensibilidade do modelo (verdadeiro positivo)
esp <- (tab[2,2])/ (tab[1,2] + tab[2,2]) #especificidade do modelo (verdadeiro negativo)
acc
sen
esp

confusionMatrix(table(predito2, teste$esteatose_dic2))

###############################
# Curva ROC                   #
###############################



# Vamos calcular a área da curva ROC com uma função no Caret
aval <- data.frame(obs=teste$esteatose_dic2, 
                   pred=class2,
                   "2_3" = predito[,2],
                   "1" = 1-predito[,2])
str(aval)
head(aval)
caret::twoClassSummary(aval, lev=levels(aval$obs))


# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval, aes(d = obs, m = 3, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - Dados agrupados - Sem NAs")

CurvaROC
calc_auc(CurvaROC)

#ROC
roc_rf_test <- roc(response = teste$esteatose_dic2, predictor =as.numeric(predito2))
roc_rf_test
plot(roc_knn_test)

#Importância das variáveis
treino_rf$importance
varImpPlot(treino_rf,
           sort=TRUE, n.var=min(30, nrow(treino_rf$importance)),
           type=NULL, class=Accuracy, scale=TRUE, 
           main=deparse(substitute(Importância))) 

#Added bonus let’s visualize the Decsion Trees
plot(treino_rf)
