# Carregando os dados do arquivo csv na vari??vel data_education
data_education <- read.csv2("education.csv", sep=",")

# Levantando o total de observa????es da amostra
summary(data_education)

# Fazendo um cruzamento das vari??veis de perfil g??nero e trabalho
table(data_education$gender,data_education$job)

# Levantando o total de observa????es por g??nero
summary(data_education$gender)

# Instalando a biblioteca de an??lise de componentes principais
#install.packages("plsdepot")

# Carregando a biblioteca de an??lise de componentes principais
library("plsdepot")

# Calculando a correla????o entre os indicadores que comp??e a vari??vel latente suporte
cor(data_education[,2:5])
# Criando a visualiza????o para an??lise de correla????o dos indicadores da vari??vel latente suporte
support_pca = nipals(data_education [,2:5])
plot(support_pca, main = "Indicadores da vari??vel latente suporte (c??rculo de correla????o)", cex.main = 1)

# Calculando a correla????o entre os indicadores que comp??e a vari??vel latente aconselhamento
cor(data_education[,6:9])
# Criando a visualiza????o para an??lise de correla????o dos indicadores da vari??vel latente suporte
support_pca = nipals(data_education[,6:9])
plot(support_pca, main = "Indicadores da vari??vel latente aconselhamento (c??rculo de correla????o)", cex.main = 1)

# Calculando a correla????o entre os indicadores que comp??e a vari??vel latente tutoria
cor(data_education[,10:13])
# Criando a visualiza????o para an??lise de correla????o dos indicadores da vari??vel latente tutoria
support_pca = nipals(data_education[,10:13])
plot(support_pca, main = "Indicadores da vari??vel latente tutoria (c??rculo de correla????o)", cex.main = 1)

# Calculando a correla????o entre os indicadores que comp??e a vari??vel latente valor
cor(data_education[,14:17])
# Criando a visualiza????o para an??lise de correla????o dos indicadores da vari??vel latente valor
support_pca = nipals(data_education[,14:17])
plot(support_pca, main = "Indicadores da vari??vel latente valor (c??rculo de correla????o)", cex.main = 1)

# Calculando a correla????o entre os indicadores que comp??e a vari??vel latente satisfa????o
cor(data_education[,18:20])
# Criando a visualiza????o para an??lise de correla????o dos indicadores da vari??vel latente satisfa????o
support_pca = nipals(data_education[,18:20])
plot(support_pca, main = "Indicadores da vari??vel latente satisfacao (c??rculo de correla????o)", cex.main = 1)

# Calculando a correla????o entre os indicadores que comp??e a vari??vel latente lealdade
cor(data_education[,21:24])
# Criando a visualiza????o para an??lise de correla????o dos indicadores da vari??vel latente lealdade
support_pca = nipals(data_education[,21:24])
plot(support_pca, main = "Indicadores da vari??vel latente lealdade (c??rculo de correla????o)", cex.main = 1)

# Criando os vetores de caminho para cada vari??vel latente do modelo
Support = c(0, 0, 0, 0, 0, 0)
Advising = c(0, 0, 0, 0, 0, 0)
Tutoring = c(0, 0, 0, 0, 0, 0)
Value = c(1, 1, 1, 0, 0, 0)
Satisfaction = c(1, 1, 1, 1, 0, 0)
Loyalty = c(0, 0, 0, 0, 1, 0)

# Definindo a matriz de direcionamento do modelo
model_path = rbind(Support, Advising, Tutoring, Value, Satisfaction, Loyalty)
colnames(model_path) = rownames(model_path)

# install.packages("plspm")
#library("plspm")
innerplot(model_path,box.size = 0.1)

# defini????o dos indicadores
model_blocks = list(2:5, 6:9, 10:13, 14:17, 18:20, 21:24)
# defini????o do tipo de rela????o dos indicadores com as vari??veis latentes - reflexivo - os indicadores geram o construto
model_modes = rep("A", 6)

#modelo de equa????es estruturais
education_pls = plspm(data_education, model_path, model_blocks, modes = model_modes)

# an??lise da qualidade do modelo
education_pls$unidim

# exibindo as carcas nas vari??veis latentes
plot(education_pls, what = "loadings")

# Criando o indicador de aprecia????o
data_education$sup.appre = 8 - data_education$sup.under

# Criando o indicador de honrado
data_education$loy.pleas = 8 - data_education$loy.asha

# nova configura????o dos indicadores
model_blocks_2 = list(c(2,28,4,5), 6:9, 10:13, 14:17, 18:20, c(21,22,29,24))

# novo modelo de equa????es estruturais
education_pls_2 = plspm(data_education, model_path, model_blocks_2, modes = model_modes)

# exibindo as carcas nas vari??veis latentes
plot(education_pls_2, what = "loadings")

# avaliando a carga e comunalidade de cada indicador
education_pls_2$outer_model

# nova configura????o removendo os indicadores honrado e aprecia????o
model_blocks_3 = list(c(2,4,5), 6:9, 10:13, 14:17, 18:20, c(21,22,24))

# modelo final
education_pls_3 = plspm(data_education, model_path, model_blocks_3, modes = model_modes)

# an??lise da qualidade do modelo
education_pls_3$unidim

# avaliando a carga e comunalidade de cada indicador
education_pls_3$outer_model

# modelo final com as cargas
plot(education_pls_3, arr.pos = 0.35)

# an??lise do modelo estrutural
education_pls_3$inner_summary

# indicador de predi????o do modelo
education_pls_3$gof

# executando a valida????o a partir de reamostragem - bootstrap
edu_val = plspm(data_education, model_path, model_blocks_3, modes = model_modes,
                boot.val = TRUE, br = 2000)

# coeficientes das rela????es ap??s a reamostragem
edu_val$boot$paths