install.packages("bnlearn")
library("bnlearn")

# criando um grafo vazio
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))

# visualizando o objeto com a estrutura de grafo vazia e seus atributos
dag

# criando uma rela????o entre idade e educa??ao
dag <- set.arc(dag, from = "A", to = "E")

# criando uma rela????o entre sexo e educa??ao
dag <- set.arc(dag, from = "S", to = "E")

# criando uma rela????o entre educa??ao e cargo
dag <- set.arc(dag, from = "E", to = "O")

# criando uma rela????o entre educa????o e residencia
dag <- set.arc(dag, from = "E", to = "R")

# criando uma rela????o entre cargo e transporte
dag <- set.arc(dag, from = "O", to = "T")

# criando uma rela????o entre residencia e transporte
dag <- set.arc(dag, from = "R", to = "T")

dag

# n??s do grafo
nodes(dag)

# rela??oes do grafo
arcs(dag)

# outra forma de criar o grafo a partir de uma matriz
dag2 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
arc.set <- matrix (c("A","E", "S","E","E","O","E","R","O","T","R","T"), byrow = TRUE, ncol = 2, dimnames = list(NULL, c("from","to")))
arcs(dag2) <- arc.set

# validando se os dois objetos de grafo sao iguais
all.equal(dag, dag2)

# definindo os valores para as variaveis categoricas do grafo

A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")

# definindo as probabilidades das observacoes serem jovem, adulto ou idoso
A.prob <- array(c(0.30, 0.50, 0.20), dim = 3, dimnames = list(A = A.lv))
A.prob

# definindo as probabilidades das observacoes serem homem ou mulher
S.prob <- array(c(0.60, 0.40), dim = 2, dimnames = list(S = S.lv))
S.prob

# cruzamento das probabilidades do cargo com o nivel de educa??ao
O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2, 2), dimnames = list(O = O.lv, E = E.lv))
O.prob

# cruzamento das probabilidades do residencia com o nivel de educa??ao
R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim = c(2, 2), dimnames = list(R = R.lv, E = E.lv))
R.prob

# cruzamento das probabilidades entre educa??ao, idade e sexo
E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,0.36, 0.70, 0.30, 0.90, 0.10), dim = c(2, 3, 2), 
                dimnames = list(E = E.lv, A = A.lv, S = S.lv))

# cruzamento das probabilidades entre viagem, ocupa??ao e residencia
T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                      0.24, 0.18, 0.70, 0.21, 0.09), dim = c(3, 2, 2),
                    dimnames = list(T = T.lv, O = O.lv, R = R.lv))

# combinando o grafo com as distribui??oes de probabilidade
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob, T = T.prob)
bn <- custom.fit(dag, cpt)

setwd("/Users/raphaelalbino/Dropbox/ead-5917/exercicio_5/")

# lendo o arquivo da pesquisa
survey <- read.table("survey.txt", header = TRUE)
head(survey)

# estimando a maxima verossimilhan??a
bn.bayes <- bn.fit(dag, data = survey, method = "bayes", iss = 10)

#exemplo de probabilidade cruzada entre ocupa????o e educa????o
bn.bayes$O

(nlevels(survey[, "T"]) - 1) * (nlevels(survey[, "E"]) - 1) *  +   (nlevels(survey[, "O"]) * nlevels(survey[, "R"]))

ci.test("T", "E", c("O", "R"), test = "mi", data = survey)

ci.test("T", "E", c("O", "R"), test = "x2", data = survey)

# Both tests return very large p-values, indicating that the dependence relationship encoded by E??T is not significant given the current DAG structure.
ci.test("T", "E", c("O", "R"), test = "x2", data = survey)

# What we see from the output above is that all arcs with the exception of O???T have p-values smaller than 0,05 and are well supported by the data.
arc.strength(dag, data = survey, criterion = "x2")

# validando se ha uma liga??ao entre S e R
dsep(dag, x = "S", y = "R")

# validando se ha uma liga??ao entre O e R
dsep(dag, x = "O", y = "R")

# validando se E bloqueia o caminho e se S e R sao independentes
dsep(dag, x = "S", y = "R", z = "E")

# podemos dizer que E ?? influenciado por A e S, isto ??, se soubermos o tipo de educacao da pessoa, algumas combinacoes de idade e sexo serao mais provaveis do que outras
dsep(dag, x = "A", y = "S", z = "E")

# serial connections, since both arcs have the same direction and follow one after the other

# divergent connections , because the two arcs have divergent directions from a central node

# convergent connections, because the two arcs converge to a central node

install.packages("gRain")

source("https://bioconductor.org/biocLite.R")
biocLite("RBGL")
biocLite("Rgraphviz")

library("gRain")
library("Rgraphviz")

junction <- compile(as.grain(bn))

jsex <- setEvidence(junction, nodes = "S", states = "F")
querygrain(jsex, nodes = "T")$T

#This suggests that women show about the same preferences towards car and train use as the interviewees as a whole.
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jres, nodes = "T")$T

#The probability associated with other drops from 0.1573 to 0.099, while the probability associated with train increases from 0.2808 to 0.4170.
# Overall, the combined probability of car and train increases from 0.8426 (for the whole survey sample) to 0.9009 (for people living in small cities). Extending this query to provide the most likely explanation, we conclude that for people living in small cities the car is the preferred means of transport.

# plotando o grafo 
graphviz.plot(dag)

# grafico de barra para as provabilidades condicionais
bn.mle <- bn.fit(dag, data = survey, method = "mle")
bn.fit.barchart(bn.mle$T, main = "Travel", xlab = "Pr(T | R,O)", ylab = "")