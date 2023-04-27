# Análises TCC USP - Rodrigo Napoli
# CLASSIFICATION TREE
# A variável resposta foi apresentada com 2 opções (leve+moderado vs severo)
# Valores NA nas variáveis removidos

########################
##   RESULTADO FINAL  ##
########################

# Os dados apresentados com 2 categorias (agrupadas) não apresentou bons resultados na pré poda:
# Acuidade = 93%; Sensiblidade = 95% e Especificidade = 75%; ROC = 0,5
# O modelo não fez predição de esteatose severa, no pós poda apresentou 100% de leve+moderado




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
dados$esteatose <- NULL
dados$esteatose_dic2 <- NULL
str(dados)

######################################################
# ETAPA 2: CONSTRUINDO A ÁRVORE COM OS DADOS ORIGINAIS

set.seed(1234)
arvore <- rpart(esteatose_dic ~ .,
                data=dados,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class') # Essa opção indica que a resposta é qualitativa; se não colocar vai para quantitativo
rpart.plot(arvore)

# Verificando a complexidade da árvore
arvore$frame
ai <- arvore$variable.importance
ai <- as.data.frame(ai)
ai

#para transferir o objeto para o excel
library("writexl") #para graver os objetos em excel
write_xlsx(ai,"TREE_importancia_semNA_agrupado_1E2_3.xlsx", col_names = TRUE, format_headers = TRUE)

#########################
# Visualizando a árvore #

# Plotando a árvore (https://rdrr.io/cran/rpart.plot/man/rpart.plot.html)
prp(arvore, main="",
    extra=106, 
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
# Probabilidade de ter esteatose severa?
prob = predict(arvore, dados) #cria uma tabela com probabilidade de ter esteatose severa
head(prob) #vetor duplo, soma 100%

# Classificação de ter esteatose severa
class = prob[,2] > 0.5
head(class) #neste caso TRUE é a classificação de que teve bard severa, qualquer valor, na coluna "2" acima de 0.5 vai dar TRUE
sum(class) #predição do número de pessoas com esteatose severa

class2 = base::factor(ifelse(prob[,2] >= 0.5, "3", "1_2"))
head(class2)

# Matriz de confusão
tab <- table(class2, dados$esteatose_dic)
tab #aqui vai cruzar quem teve severa de fato com o que o modelo predisse

acc <- (tab[1,1] + tab[2,2])/ sum(tab) #acurácia do modelo
sen <- (tab[1,1])/ (tab[1,1] + tab[2,1]) #sensibilidade do modelo (verdadeiro positivo)
esp <- (tab[2,2])/ (tab[1,2] + tab[2,2]) #especificidade do modelo (verdadeiro negativo)
acc
sen
esp

###############################
# Curva ROC                   #
###############################

# Vamos calcular a área da curva ROC com uma função no Caret
aval <- data.frame(obs=dados$esteatose_dic, 
                   pred=class2,
                   "3" = prob[,2],
                   "1_2" = 1-prob[,2])

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
LOOCV <- train(esteatose_dic ~., 
               data = dados,
               method = "rpart",
               trControl = train.control.LOOCV,
               tuneGrid = cp.grid) 
# Summarize the results 
print(LOOCV)  

#Criando a árvore de decisão mais apropriada
arvore_poda <- rpart::rpart(esteatose_dic ~ .,
                            data=dados,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = LOOCV$bestTune$cp, #aqui indicar o cp que foi sugerido pelo comando acima
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)
rpart.plot::rpart.plot(arvore_poda)

prob = stats::predict(arvore_poda, dados)
class2 = base::factor(ifelse(prob[,2]>.5, "3", "1_2"))

#####
aval <- data.frame(obs=dados$esteatose_dic, 
                   pred=class2,
                   "3" = prob[,2],
                   "1_2" = 1-prob[,2])
aval
head(aval)
caret::twoClassSummary(aval, lev=levels(aval$obs))
#uma vez que todos os resultados preditos foram todos de leve+moderado o comando
#acima não funciona


# Matriz de confusão
tab <- table(class2, dados$esteatose_dic)
tab #aqui vai cruzar quem sobreviveu de fato com o que o modelo predisse

acc <- (tab[1,1] + tab[2,2])/ sum(tab) #acurácia do modelo
sen <- (tab[1,1])/ (tab[1,1] + tab[2,1]) #sensibilidade do modelo (verdadeiro positivo)
esp <- (tab[2,2])/ (tab[1,2] + tab[2,2]) #especificidade do modelo (verdadeiro negativo)
acc
sen
esp
#uma vez que todos os resultados preditos forma todos de leve+moderado o comando
#acima não funciona

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC2 <- ggplot2::ggplot(aval, aes(d = obs, m = 3, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC")

CurvaROC2
calc_auc(CurvaROC2)
