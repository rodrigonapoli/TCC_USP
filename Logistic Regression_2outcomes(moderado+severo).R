# Análises TCC USP - Rodrigo Napoli
# REGRESSÃO LOGÍSTICA 
# A variável resposta foi apresentada com 2 opções (leve VS moderado + severo)
# Valores NA nas variáveis foram removidos


########################
##   RESULTADO FINAL  ##
########################
# Neste modelo o psudo R2 foi de 17,67% (p<0,001). 
# Variáveis preditoras: TGP e Glicose 
# acc = 65,81%
# sensibilidade = 64,81%
# especificiadad = 66,67%


# Bibliotecas
library("pROC")
library("ggplot2") #gráficos
library("MASS") #para fazer Stepwise 
library ("jtools") 
library ('equatiomatic') #para obter a equação da regressão
library ('kableExtra') # para montar tabelas mais "bonitas"
#library("Hmisc")
#library("reshape2")
#library("cowplot")

#rodando o script de preparo do banco de dados
setwd("/Users/rodrigonapoli/Dropbox/Cientista de dados/USP/ 00 - TCC/Dados")
dados <- readRDS("dados.rds")
str(dados)

#apagando as variáveis que não serão usadas NESTE modelo
dados$esteatose <- NULL
dados$esteatose_dic <- NULL
str(dados)

#MODELO DE REGRESSÃO LOGÍSTICA BINOMIAL
modelo.completo <- glm(esteatose_dic2 ~.,
                       family=binomial(link='logit'),
                       data=dados)

stepmodelo <- stepAIC (modelo.completo,
                       direction = "both",
                       trace=FALSE,
                       k = 3.841458820694)

summary(modelo.completo)
summary(stepmodelo)
export_summs(modelo.completo,stepmodelo,
             model.names = c("completo","stepwise"),
             scale = F, digits = 4)

# Criando predições
probabilities <- stepmodelo %>% predict(dados, type = "response")
predicted.classes <- ifelse(probabilities < 0.5, "1", "2_3")
#### Model accuracy
mean(predicted.classes==dados$esteatose_dic2)
probabilities
predicted.classes

tab <- table(predicted.classes, dados$esteatose_dic2)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab) #acurácia do modelo
sen <- (tab[1,1])/ (tab[1,1] + tab[2,1]) #sensibilidade do modelo (verdadeiro positivo)
esp <- (tab[2,2])/ (tab[1,2] + tab[2,2]) #especificidade do modelo (verdadeiro negativo)
acc
sen
esp

# Calculando o R2 (e valor de p)
ll.null <- stepmodelo$null.deviance/-2
ll.proposed <- stepmodelo$deviance/-2
pseudo_r2 <- (ll.null - ll.proposed)/ll.null #overall effect size
pseudo_r2
p_do_r2 <- 1 - pchisq(2*(ll.proposed - ll.null),
                      df=(length(stepmodelo$coefficients)-1)) # valor de p para o R2, 
p_do_r2

#gráfico de
predito <- data.frame(
  probability.of.esteatose=stepmodelo$fitted.values,
  esteatose_dic2=dados$esteatose_dic2)

predito <- predito[
  order(predito$probability.of.esteatose, decreasing = FALSE),]
predito$rank <- 1:nrow(predito)

ggplot(data=predito, aes(x=rank, y=probability.of.esteatose, color=esteatose_dic2)) + 
  ggtitle("Modelo com 2 outcomes: Leve VS Moderado + Severo") +
  geom_point() +
  labs(x = "Index",
       y = "Probabilidade predita de ter esteatose severa")


#### Calculando a curva ROC
#Desejamos os dados da sensitividade e de especificidade.
predicoes <- prediction(predictions = stepmodelo$fitted.values, 
                        labels = as.factor(dados$esteatose_dic2))
sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 
especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

#função roc do pacote pROC
ROC <- roc(response = dados$esteatose_dic2, 
           predictor = stepmodelo$fitted.values)

#Plotagem da curva ROC propriamente dita
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )

#função 'extract_eq' do pacote 'equatiomatic'
extract_eq(stepmodelo, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)


######


