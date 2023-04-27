# TCC_USP
Dados e análises do TCC de Data Science & Analytics da USP/Esalq
Rodrigo Picheth Di Napoli

Estes scripts foram gerados com o intuito de avaliar o melhor modelo de machine learning para predizer o grau de esteatose hepática
O banco de dados é composto por 117 pacientes adultos e obesos portadores de esteatose hepática em 3 graus distintos da doença (leve, severo e moderado).
Uma vez que os dados ainda não foram publicados e para preservar a confidencialidade dos participantes da pesquisa, o banco de dados não foi apresentado aqui.
Assim que o trabalho seja aprovado, corrigido e publicado serão apresentadas mais informações aqui.
Por enquanto disponiblizo o título e resumo do TCC para melhor conhecimento desta pesquisa.


Aprendizado de máquina para predição do grau de esteatose hepática não alcoólica em pacientes obesos

Resumo
A esteatose hepática não alcoólica [EHNA] é a forma inflamatória da doença hepática gordurosa não alcoólica, doença onde ocorre um acumulado excessivo
de gordura no fígado. Seu diagnóstico e avaliação da gravidade é normalmente dada por ultrassonografia abdominal, entretanto, buscam-se alternativas
mais rápidas e baratas baseadas em dados morfológicos e bioquímicos. Desta forma, objetivou-se avaliar diferentes algoritmos de aprendizado de 
máquinas preditores de diferentes graus de EHNA em pacientes obesos candidatos a cirurgia bariátrica. Um banco de dados composto por 15 variáveis 
antropométricas e bioquímicas de 117 pacientes obesos e adultos previamente classificados em três diferentes graus (leve, moderado e severo) foram 
avaliados por seis técnicas de aprendizado de máquina: regressão logística [RL], árvore de decisão [AD], K-Vizinhos Próximos [K-VP], “Naive Bayes” [NB],
“Random Forest” [RF] e “Suport Vector Machine” [SVM]. As avaliações foram feitas com graus de EHNA não agrupados, e agrupados (juntando leve e moderado
e juntando moderado e severo). Resultados de área abaixo da curva [AUC], acurácia, sensibilidade e especificidade foram usados para comparar os modelos.
Os modelos de RL e SVM se mostraram os melhores modelos preditores com AUC de 73,2 e 79,1 (respectivamente) sem agrupamento dos dados, 97,5 e 
81,4 (respectivamente) quando os pacientes com grau leve foram agrupados ao grau moderado e 70,6 e 65,7 (respectivamente) quando os pacientes 
com grau moderado foram agrupados ao grau severo. Estes modelos se mostraram adequados quando o interesse é prever a ocorrência de EHNA severa.



