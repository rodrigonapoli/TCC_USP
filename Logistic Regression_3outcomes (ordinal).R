# Análises TCC USP - Rodrigo Napoli
# REGRESSÃO LOGÍSTICA ORDINAL
# A variável resposta foi apresentada com as 3 opções (leve, moderada, severa)
# https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
# https://bookdown.org/chua/ber642_advanced_regression/ordinal-logistic-regression.html


########################
##   RESULTADO FINAL  ##
########################
# Neste modelo o R2 f de  (p < ). - OVERFITTING? 
# Variáveis preditoras:  
# acc = 54,7%


# Bibliotecas
library("readxl") #para ler o arquivo em excel
library("foreign")
library("ggplot2")
library("MASS")
library("Hmisc")
library("reshape2")
library("cowplot")
library("jtools")
library("caret") #para fazer a matriz de confusão com resultados
library ("pROC") #para calcular a AUC

#rodando o script de preparo do banco de dados
setwd("/Users/rodrigonapoli/Dropbox/Cientista de dados/USP/ 00 - TCC/Dados")
dados <- readRDS("dados.rds")
str(dados)

#apagando as variáveis que não serão usadas NESTE modelo
dados$esteatose_dic <- NULL
dados$esteatose_dic2 <- NULL
str(dados)

dat <- dados


## fit ordered logit model and store results 'm'
modelo <- polr(esteatose ~ ., data = dados, Hess=TRUE, method = "logistic")

stepmodelo <- stepAIC (modelo,
                       direction = "both",
                       trace=FALSE,
                       k = 3.841458820694)


## view a summary of the model
summary(modelo)
summary(stepmodelo)
export_summs(modelo,stepmodelo,
             model.names = c("completo","stepwise"),
             scale = F, digits = 4)


n <- polr(esteatose ~ 1, data = dados) # Run a "only intercept" model
summary(n)
anova(stepmodelo,n) # Compare the our test model with the "Only intercept" model
# neste caso o valor de p é < 0,005, desta forma o modelo proposto é melhor que um modelo
# sem nenhuma variável

# Check the predicted probability for each program
stepmodelo$fitted.values
# We can get the predicted result by using predict function
predict(stepmodelo)
# Test the goodness of fit
chisq.test(dados$esteatose,predict(stepmodelo))
#Compute confusion table and misclassification error
predictor_m <-  predict(stepmodelo,dados)
cTab <- table(dados$esteatose, predictor_m)
cTab
acc <- 1-(mean(as.character(dados$esteatose) != as.character(predictor_m))) 
acc
length(predictor_m)
confusionMatrix(table(predictor_m ,dados$esteatose))

#este modelo acertou em 54,7% das vezes

#Measuring Strength of Association Pseudo R-Square)
library("DescTools")
PseudoR2(stepmodelo, which = c("McFadden"))

mean(as.character(dados$esteatose) != as.character(predictor_m))
#este modelo errou em 45,76% das vezes

## store table
(ctable <- coef(summary(stepmodelo)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
p <- sprintf(p, fmt = '%.4f')
p

## combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(stepmodelo)) # default method gives profiled CIs
confint.default(stepmodelo) # CIs assuming normality

#The CIs for both pared and gpa do not include 0; public does. 
#The estimates in the output are given in units of ordered logits, 
# or ordered log odds. So for pared, we would say that for a one unit 
# increase in pared (i.e., going from 0 to 1), we expect a 1.05 increase 
# in the expected value of apply on the log odds scale, given all of the 
# other variables in the model are held constant. For gpa, we would say 
# that for a one unit increase in gpa, we would expect a 0.62 increase in 
# the expected value of apply in the log odds scale, given that all of the 
# other variables in the model are held constant.


## odds ratios
exp(coef(stepmodelo))

## ODDS RATIO and CI
or <- exp(cbind(OR = coef(stepmodelo), ci))
or2 <- exp(coef(stepmodelo))
p_value <- ctable[, c("p value")]
x <- c(17, 18)
p_value <- p_value[-x]
p_value

resultados <- cbind(or, p_value)
resultados

#ROC
roc_lr_test <- multiclass.roc(response = dados$esteatose, predictor =as.numeric(predictor_m))
roc_lr_test
#plot(roc_lr_test)








