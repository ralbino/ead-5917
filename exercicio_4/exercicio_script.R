setwd("/Users/raphaelalbino/Dropbox/ead-5917/exercicio_4/")
data_customer_satisfaction <- read.csv2("mobi250.csv",sep=";")

# montando as relacoes do modelo
Image <- c(0,0,0,0,0,0,0)

Expectation <- c(0,0,0,0,0,0,0)

Quality <- c(0,1,0,0,0,0,0)

Value <- c(0,1,1,0,0,0,0)

Satisfaction <- c(1,1,1,1,0,0,0)

Complaints <- c(0,0,0,0,1,0,0)

Loyalty <- c(1,0,0,0,1,1,0)

model_path = rbind(Image,Expectation, Quality, Value, Satisfaction,Complaints,Loyalty)

colnames(model_path) = rownames(model_path)

library("plspm")

# definindo que o modelo ?? reflexivo em todas as variaveis latentes

model_modes = rep("A", 7)

# exibindo o modelo conceitual
innerplot(model_path,box.size = 0.05)

# associando os indicadores a variaveis latentes
model_blocks =list(1:5,6:8,9:15,16:17,18:20,21,22:24)

# gerando o modelo de equacoes estruturais
satisfaction_pls = plspm(data_customer_satisfaction, model_path, model_blocks, modes = model_modes,boot.val=TRUE,br=2000)

# exibindo as cargas fatoriais das relacoes entre as variaveis latentes
plot(satisfaction_pls, what = "loadings")

# analisando a significancia das relacoes entre as variaveis latentes
satisfaction_pls$boot$paths

# analisando a carga fatorial dos indicadores nas variaveis latentes
satisfaction_pls$outer_model

# analisando a media da variancia extraida
satisfaction_pls$inner_summary

# analisando a carga cruzada
satisfaction_pls$crossloadings

# gerando a matrix de correlacao entre os escores
cor(satisfaction_pls$scores)

# analisando a confiabilidade composta
satisfaction_pls$unidim

# modelo com as cargas
plot(satisfaction_pls)

# regressoes para cada uma das variaveis latentes
satisfaction_pls$inner_model

good_rows = c(3:5, 7:15)

path_effs = as.matrix(satisfaction_pls$effects[good_rows, 2:3])

rownames(path_effs) = satisfaction_pls$effects[good_rows, 1]

# analise dos efeitos diretos e indiretos
path_effs

# analise do r-quadrado
satisfaction_pls$inner_summary

# capturando os escores padronizados
matrix_scores <- satisfaction_pls$scores

data_customer_satisfaction$scoreSatisfaction <- matrix_scores[,5]
data_customer_satisfaction$scoreValue <- matrix_scores[,4]
data_customer_satisfaction$scoreExpectation <- matrix_scores[,2]
data_customer_satisfaction$scoreQuality <- matrix_scores[,3]
data_customer_satisfaction$scoreImage <- matrix_scores[,1]

library("car")
# avaliando a multicolinearidade do escore satisfacao a partir dos escores de valor, expectativa e imagem
vif(lm(scoreSatisfaction ~ scoreImage + scoreQuality + scoreValue+scoreExpectation,data=data_customer_satisfaction))

