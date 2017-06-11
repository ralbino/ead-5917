setwd("/Users/raphaelalbino/Dropbox/ead-5917/trabalho_disciplina/")
nba_data <- read.csv("dados_nba_2016_2006.csv")

# Effective field goal percentage
# EFG = (FGM + 0.5 ?? 3PM)/FGA
nba_data$effective_field_goal_percentage <- nba_data$effective_field_goal_percentage/100
nba_data$opponent_effective_field_goal_percentage <- nba_data$opponent_effective_field_goal_percentage/100

# Free throw rate
# FTR = FTM/FGA

# Turnovers per possession
# TPP = TO/POSS
# POSS = FGAt+0.44??FTAt???OREBt+TOt (formula to calculate possession)
nba_data$turnover_percentage <- nba_data$turnover_percentage/100
nba_data$opponent_turnover_percentage <- nba_data$opponent_turnover_percentage/100


# Offensive rebounding percentage
# ORP = OREBt /(OREB t+ DREBo)
nba_data$offensive_rebound_percentage <- nba_data$offensive_rebound_percentage/100
nba_data$opponent_offensive_rebound_rate <- nba_data$opponent_offensive_rebound_rate/100


# Estatística descritiva
summary(nba_data$win_percentage)
hist(nba_data$win_percentage, ylab="Frequencia",main="Histograma do percentual de vitorias", xlab="Percentual de vitorias")
boxplot(nba_data$win_percentage)

# Análise da dispersão das variáveis independentes quando cruzadas com o percentual de vitória (variável dependente)
par(mfrow=c(2,2)) 
plot(nba_data$win_percentage,nba_data$effective_field_goal_percentage,cex=0.1,pch=19,col="blue")
plot(nba_data$win_percentage,nba_data$free_throw_attempt_rate,cex=0.1,pch=19,col="blue")
plot(nba_data$win_percentage,nba_data$turnover_percentage,cex=0.1,pch=19,col="blue")
plot(nba_data$win_percentage,nba_data$offensive_rebound_percentage,cex=0.1,pch=19,col="blue")

par(mfrow=c(2,2)) 
plot(nba_data$win_percentage,nba_data$opponent_effective_field_goal_percentage,cex=0.1,pch=19,col="red")
plot(nba_data$win_percentage,nba_data$opponent_free_throw_attempted_rate,cex=0.1,pch=19,col="red")
plot(nba_data$win_percentage,nba_data$opponent_turnover_percentage,cex=0.1,pch=19,col="red")
plot(nba_data$win_percentage,nba_data$opponent_offensive_rebound_rate,cex=0.1,pch=19,col="red")

cor.test(nba_data$win_percentage,nba_data$effective_field_goal_percentage)
cor.test(nba_data$win_percentage,nba_data$free_throw_attempt_rate)
cor.test(nba_data$win_percentage,nba_data$turnover_percentage)
cor.test(nba_data$win_percentage,nba_data$offensive_rebound_percentage)
cor.test(nba_data$win_percentage,nba_data$opponent_effective_field_goal_percentage)
cor.test(nba_data$win_percentage,nba_data$opponent_free_throw_attempted_rate)
cor.test(nba_data$win_percentage,nba_data$opponent_turnover_percentage)
cor.test(nba_data$win_percentage,nba_data$opponent_offensive_rebound_rate)

par(mfrow=c(1,1)) 

regressao  <- lm(nba_data$win_percentage ~ nba_data$effective_field_goal_percentage + nba_data$free_throw_attempt_rate + nba_data$turnover_percentage + nba_data$offensive_rebound_percentage + nba_data$opponent_effective_field_goal_percentage+ nba_data$opponent_free_throw_attempted_rate+ nba_data$opponent_turnover_percentage+ nba_data$opponent_offensive_rebound_rate) 
summary(regressao)

anova(regressao)

# Teste de distribuição normal dos erros
shapiro.test(residuals(regressao))

# Suposição erros possuem variância constante, se p-value maior do que 0.05 não se pode negar a hipótese nula, longo não há evidências de falta de homocedasticidade.
qqnorm(residuals(regressao), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(regressao))

median(nba_data$effective_field_goal_percentage)
var.test(residuals(regressao)[nba_data$effective_field_goal_percentage<=0.498],residuals(regressao)
         [nba_data$effective_field_goal_percentage>=0.498])

median(nba_data$free_throw_attempt_rate)
var.test(residuals(regressao)[nba_data$free_throw_attempt_rate<=0.2885],residuals(regressao)
         [nba_data$free_throw_attempt_rate>=0.2885])

median(nba_data$turnover_percentage)
var.test(residuals(regressao)[nba_data$turnover_percentage<=0.1495],residuals(regressao)
         [nba_data$turnover_percentage>=0.1495])

median(nba_data$offensive_rebound_percentage)
var.test(residuals(regressao)[nba_data$offensive_rebound_percentage<=0.2595],residuals(regressao)
         [nba_data$offensive_rebound_percentage>=0.2595])

median(nba_data$opponent_effective_field_goal_percentage)
var.test(residuals(regressao)[nba_data$opponent_effective_field_goal_percentage<=0.5],residuals(regressao)
         [nba_data$opponent_effective_field_goal_percentage>=0.5])

median(nba_data$opponent_free_throw_attempted_rate)
var.test(residuals(regressao)[nba_data$opponent_free_throw_attempted_rate<=0.286],residuals(regressao)
         [nba_data$opponent_free_throw_attempted_rate>=0.286])

median(nba_data$opponent_turnover_percentage)
var.test(residuals(regressao)[nba_data$opponent_turnover_percentage<=0.149],residuals(regressao)
         [nba_data$opponent_turnover_percentage>=0.149])

median(nba_data$opponent_offensive_rebound_rate)
var.test(residuals(regressao)[nba_data$opponent_offensive_rebound_rate<=0.259],residuals(regressao)
         [nba_data$opponent_offensive_rebound_rate>=0.259])

par(mfrow=c(1,1)) 

# modelagem de equações estruturais

# carregando a biblioteca de análise de componentes principais
library("plsdepot")

# qualidade ofensiva
qualidade_ofensiva <- data.frame(nba_data[,c(9,10,13,22)])
pca_ofensivo = nipals(qualidade_ofensiva)
plot(pca_ofensivo, main = "Indicadores de qualidade ofensiva", cex.main = 1)

# controle defensivo
qualidade_defensiva <- data.frame(nba_data[,c(58,59,60,61)])
pca_defensivo = nipals(qualidade_defensiva)
plot(pca_defensivo, main = "Indicadores de qualidade defensiva", cex.main = 1)

# ataque e defesa influenciam o percentual de vitória da equipe
library(plspm)

# construindo o modelo
Ataque = c(0, 0, 0)
Defesa = c(0, 0, 0)
Sucesso = c(1, 1, 0)

nba_path = rbind(Ataque, Defesa, Sucesso)
colnames(nba_path) = rownames(nba_path)

# exibindo o modelo
innerplot(nba_path)

# outer model
nba_blocks = list(c(9,10,13,22), c(58,59,60,61), 7)

# modes (reflective blocks)
nba_modes = rep("A", 3)

# rodando o modelo
nba_pls_modelo = plspm(nba_data, nba_path, nba_blocks, modes = nba_modes,boot.val = TRUE, br = 2000)

# validade convergente
nba_pls_modelo$outer_model
nba_pls_modelo$boot$loadings
nba_pls_modelo$inner_summary
plot(nba_pls_modelo, what = "loadings")

# validade discriminante
nba_pls_modelo$crossloadings
cor(nba_pls_modelo$scores) 

# confiabilidade
nba_pls_modelo$unidim

# coeficientes estruturais
plot(nba_pls_modelo)
nba_pls_modelo$path_coefs
nba_pls_modelo$inner_model

# efeitos diretos
nba_pls_modelo$effects

# r-quadrado
nba_pls_modelo$inner_summary

nba_pls_modelo$gof

# capturando os escores padronizados
matrix_scores <- nba_pls_modelo$scores

nba_data$scoreDefesa <- matrix_scores[,2]
nba_data$scoreAtaque <- matrix_scores[,1]
nba_data$scoreSucesso <- matrix_scores[,3]

# avaliando a multicolinearidade do escore sucesso a partir dos escores de defesa e ataque
library("car")

vif(lm(scoreSucesso ~ scoreDefesa + scoreAtaque,data=nba_data))
