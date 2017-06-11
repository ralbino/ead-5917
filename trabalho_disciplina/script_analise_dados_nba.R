getwd()
directory <- "/Users/raphaelalbino/Dropbox/ead-5917/trabalho_disciplina/"
setwd(directory)
nba_data <- read.csv2("nba_data_2016_2009.csv",sep = ",")

# validando se a relacao entre vitorias e derrotas bate com o valor ja calculado
nba_data$validation <- round(nba_data$wins/nba_data$games, 3)
nba_validate <- nba_data[nba_data$validation != nba_data$wins_per_loses,]
# o valor apresentado no teste deve ser zero
length(nba_validate$id)

# transformando os dados de nivel para numerico
nba_data$field_goals <- as.numeric(as.matrix(nba_data$field_goals))
nba_data$opponent_field_goals <- as.numeric(as.matrix(nba_data$opponent_field_goals))

nba_data$field_goals_attemps <- as.numeric(as.matrix(nba_data$field_goals_attemps))
nba_data$opponent_field_goals_attemps <- as.numeric(as.matrix(nba_data$opponent_field_goals_attemps))

nba_data$X3_points_field_goals <- as.numeric(as.matrix(nba_data$X3_points_field_goals))
nba_data$opponent_3_points_field_goals <- as.numeric(as.matrix(nba_data$opponent_3_points_field_goals))

nba_data$free_throws_field_goals <- as.numeric(as.matrix(nba_data$free_throws_field_goals))
nba_data$opponent_free_throws_field_goals <- as.numeric(as.matrix(nba_data$opponent_free_throws_field_goals))

nba_data$free_throws_goals_attemps <- as.numeric(as.matrix(nba_data$free_throws_goals_attemps))
nba_data$opponent_free_throws_goals_attemps <- as.numeric(as.matrix(nba_data$opponent_free_throws_goals_attemps))

nba_data$offensive_rebounds <- as.numeric(as.matrix(nba_data$offensive_rebounds))
nba_data$defensive_rebounds <- as.numeric(as.matrix(nba_data$defensive_rebounds))
nba_data$opponent_defensive_rebounds <- as.numeric(as.matrix(nba_data$opponent_defensive_rebounds))
nba_data$opponent_offensive_rebounds <- as.numeric(as.matrix(nba_data$opponent_offensive_rebounds))

nba_data$turnovers <- as.numeric(as.matrix(nba_data$turnovers))
nba_data$opponent_turnovers <- as.numeric(as.matrix(nba_data$opponent_turnovers))

nba_data$steals <- as.numeric(as.matrix(nba_data$steals))

nba_data$wins_per_loses <- as.numeric(as.matrix(nba_data$wins_per_loses))

summary(nba_data$field_goals)
summary(nba_data$X3_points_field_goals)
summary(nba_data$field_goals_attemps)
summary(nba_data$free_throws_field_goals)
summary(nba_data$free_throws_goals_attemps)
summary(nba_data$offensive_rebounds)
summary(nba_data$turnovers)
summary(nba_data$opponent_defensive_rebounds)
summary(nba_data$opponent_offensive_rebounds)

# Effective field goal percentage
# EFG = (FGM + 0.5 ?? 3PM)/FGA
nba_data$EFG <- (nba_data$field_goals + 0.5*nba_data$X3_points_field_goals)/nba_data$field_goals_attemps

# Free throw rate
# FTR = FTM/FGA
nba_data$FTR <- nba_data$free_throws_field_goals/nba_data$field_goals_attemps

# Turnovers per possession
# TPP = TO/POSS
# POSS = FGAt+0.44??FTAt???OREBt+TOt (formula to calculate possession)
nba_data$POSS <- nba_data$field_goals_attemps+0.44*nba_data$free_throws_goals_attemps-nba_data$offensive_rebounds+nba_data$turnovers
nba_data$TPP = nba_data$turnovers/nba_data$POSS

# Offensive rebounding percentage
# ORP = OREBt /(OREB t+ DREBo)
nba_data$ORP = nba_data$offensive_rebounds/(nba_data$offensive_rebounds+nba_data$opponent_defensive_rebounds)

# Opponnent offensive rebound
# OORP = OREBo /(OREBo+ DREBt)
nba_data$OORP = nba_data$opponent_offensive_rebounds/(nba_data$opponent_offensive_rebounds+nba_data$defensive_rebounds)

# Opponent turnover
# TPP = TOo/POSSo
nba_data$OPOSS <- nba_data$opponent_field_goals_attemps+0.44*nba_data$opponent_free_throws_goals_attemps-nba_data$opponent_offensive_rebounds+nba_data$opponent_turnovers

nba_data$OTPP = nba_data$opponent_turnovers/nba_data$OPOSS

# Opponent Free throw rate
# OFTR = OFTM/OFGA
nba_data$OFTR <- nba_data$opponent_free_throws_field_goals/nba_data$opponent_field_goals_attemps

# Steals per opponent possession
# SPP = STt/POSS
nba_data$SPP = nba_data$steals/nba_data$OPOSS

# Opponent Effective field goal percentage
# OEFG = (FGMo + 0.5 ?? 3PMo)/FGAo
nba_data$OEFG <- (nba_data$opponent_field_goals + 0.5*nba_data$opponent_3_points_field_goals)/nba_data$opponent_field_goals_attemps

mean(nba_data$EFG)
mean(nba_data$FTR)
mean(nba_data$TPP)
mean(nba_data$ORP)

mean(nba_data$OEFG)
mean(nba_data$SPP)
mean(nba_data$OTPP)
mean(nba_data$OORP)

# Realizando a regressão com os quatro fatores identificados
regressao_percentual_vitoria <- lm(nba_data$wins_per_loses~nba_data$field+nba_data$FTR+nba_data$TPP+nba_data$ORP+nba_data$OEFG+nba_data$OFTR+nba_data$OTPP+nba_data$OORP)
summary(regressao_percentual_vitoria)

# Carregando a biblioteca de análise de componentes principais
library("plsdepot")

# Validando se as variáveis EFG, FTR, TPP e ORP representam a qualidade ofensiva do time
offensive_quality <- data.frame(nba_data[,52:53],nba_data[,55:56])
offensive_pca = nipals(offensive_quality)
plot(offensive_pca, main = "Indicadores de qualidade ofensiva", cex.main = 1)
# A variável apresenta uma correlação invertida, pois, de fato, quanto menor o número de erros ofensivos, melhor será o desempenho ofensivo.

defensive_quality <- data.frame(nba_data[,57],nba_data[,59:61])
defensive_pca = nipals(defensive_quality)
plot(defensive_pca, main = "Indicadores de qualidade defensiva", cex.main = 1)
