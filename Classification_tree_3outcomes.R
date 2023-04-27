# Análises TCC USP - Rodrigo Napoli
# CLASSIFICATION TREE

########################
##   RESULTADO FINAL  ##
########################

# Os dados apresentados com 3 categorias apresentou um modelo pós-poda inadequado pois
# não foi capaz de prever nenhum caso de estesato severa, desta forma, 
# este modelo foi abandonado

# Instalação de pacotes
pacotes <- c('tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',      # Funções úteis para machine learning
             'plotROC',
             "readxl"      # para abrir arquivos do excel
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#rodando o script de preparo do banco de dados
setwd("/Users/rodrigonapoli/Dropbox/Cientista de dados/USP/ 00 - TCC/Dados")
dados <- readRDS("dados.rds")
str(dados)

#apagando as variáveis que não serão usadas NESTE modelo
dados$esteatose_dic <- NULL
dados$esteatose_dic2 <- NULL
str(dados)

######################################################
# ETAPA 2: CONSTRUINDO A ÁRVORE COM OS DADOS ORIGINAIS

set.seed(09041978)
arvore <- rpart(esteatose ~ .,
                data=dados,
                parms = list(split = 'gini'), # podemos trocar para  'information' ou 'gini'
                method='class') # Essa opção indica que a resposta é qualitativa; se não colocar vai para quantitativo
rpart.plot(arvore)

# Verificando a complexidade da árvore
arvore$frame
ai <- arvore$variable.importance
ai <- as.data.frame(ai)
ai

# para transferir o objeto para o excel
library("writexl") #para graver os objetos em excel
write_xlsx(ai,"TREE_importancia_semNA_agrupado_1_2_3.xlsx", col_names = TRUE, format_headers = TRUE)


#########################
# Visualizando a árvore #
# Plotando a árvore (https://rdrr.io/cran/rpart.plot/man/rpart.plot.html)
prp(arvore, main="",
    extra=104, 
    nn=FALSE, 
    fallen.leaves=TRUE, 
    branch=1, 
    faclen=1, 
    trace=0, 
    shadow.col="gray", 
    split.cex=1, 
    split.suffix="?", 
    split.box.col="lightgray", 
    split.border.col="darkgray", 
    split.round=.5)

##############################
# Avaliação básica da árvore #

# Predizendo com a árvore
# Probabilidade de ter esteatose os diferentes graus de esteatose
prob = predict(arvore, dados) #cria uma tabela com probabilidade de ter esteatose severa
head(prob) #vetor tripo, soma 100%


# Classificação de ter diferentes graus de esteatose
class2 = base::factor(ifelse(prob[,3] > prob[,1] & prob[,3] > prob[,2], "severa", (ifelse(prob[,2]>prob[,1], "moderada", "leve"))))
# como a prevelência de esteatose severa é menor do que leve na população brasielira, obtou-se por considerar "leve" os casos de empate

# Matriz de confusão
tab <- table(class2, dados$esteatose)
tab #aqui vai cruzar quem teve severa de fato com o que o modelo predisse

acc <- (tab[1,1] + tab[1,2] + tab[2,3])/ sum(tab) #acurácia do modelo
sen <- (tab[1,1] + tab[1,2])/ (tab[1,1] + tab [1,2] + tab[2,1] + tab[2,2]) #sensibilidade do modelo (verdadeiro positivo)
esp <- (tab[2,3])/ (tab[1,3] + tab[2,3]) #especificidade do modelo (verdadeiro negativo)
acc
sen
esp

##########################
# pós-poda (Grid Search) #
##########################
#Como o tamanho amostral é pequeno foi usado um método
# de cross-validation Leave One Out (LOOCV)
cp.grid <- expand.grid(.cp = (0:100)*0.01)  #alterar esse parâmetro caso haja necessidade
# Leave one out cross validation - LOOCV 
# Define training control 
train.control.LOOCV <- trainControl(method = "LOOCV") 

# Train the model 
LOOCV <- train(esteatose ~., 
               data = dados,
               method = "rpart",
               trControl = train.control.LOOCV,
               tuneGrid = cp.grid) 
# Summarize the results 
print(LOOCV)

#Criando a árvore de decisão mais apropriada
arvore_poda <- rpart::rpart(esteatose ~ .,
                            data=dados,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = LOOCV$bestTune$cp, #aqui indicar o cp que foi sugerido pelo comando acima
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)
rpart.plot::rpart.plot(arvore_poda)

##############################
# Avaliação da árvore podada#

# Predizendo com a árvore
# Probabilidade de ter esteatose os diferentes graus de esteatose
prob2 = predict(arvore_poda, dados) #cria uma tabela com probabilidade de ter esteatose severa
head(prob2) #vetor tripo, soma 100%


# Classificação de ter diferentes graus de esteatose
class3 = predict(arvore_poda, dados, "class")

# Matriz de confusão
tab2 <- table(class3, dados$esteatose)
tab2 #aqui vai cruzar quem teve severa de fato com o que o modelo predisse

tab2 <- data.frame(leve = c(26,28,0),  
                   moderado = c(3,48,0),  
                   severo = c(1,11,0))
tab2

acc <- (tab2[1,1] + tab2[2,2] + tab2[3,3])/ sum(tab2) #acurácia do modelo
acc

confusionMatrix(table(class3 ,dados$esteatose))

#Multi-ROC
roc_knn_test <- multiclass.roc(response = dados$esteatose, predictor =as.numeric(class3))
roc_knn_test
plot(roc_knn_test)

#######

