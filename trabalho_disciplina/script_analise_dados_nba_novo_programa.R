getwd()
directory <- "/Users/raphaelalbino/Dropbox/ead-5917/trabalho_disciplina/"
setwd(directory)
nba_data <- read.csv2("teste.csv",sep = ",")

#Offensive
nba_data$EFG <- as.numeric(as.matrix(nba_data$EFG))
nba_data$TOV <- as.numeric(as.matrix(nba_data$TOV))
nba_data$ORB <- as.numeric(as.matrix(nba_data$ORB))
nba_data$FT_FGA <- as.numeric(as.matrix(nba_data$FT_FGA))

#Defensive
nba_data$OEFG <- as.numeric(as.matrix(nba_data$OEFG))
nba_data$OTOV <- as.numeric(as.matrix(nba_data$OTOV))
nba_data$DRB <- as.numeric(as.matrix(nba_data$DRB))
nba_data$OFT_FGA <- as.numeric(as.matrix(nba_data$OFT_FGA))

library("plsdepot")

# Validando se as variáveis EFG, FTR, TPP e ORP representam a qualidade ofensiva do time
offensive_quality <- data.frame(nba_data$EFG,nba_data$TOV,nba_data$ORB, nba_data$FT_FGA)
offensive_pca = nipals(offensive_quality)
plot(offensive_pca, main = "Indicadores de qualidade ofensiva", cex.main = 1)
matcor <- cor(offensive_quality)
print(matcor, digits = 2)
