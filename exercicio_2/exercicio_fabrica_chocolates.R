# Somos gestores de uma f??brica de chocolates e queremos avaliar se a eleva????o da 
setwd("/Users/raphaelalbino/Dropbox/doutorado/EAD5917/exercicio_2/")
dados_fabrica <- read.csv2("fabricachocolate.csv")

summary(dados_fabrica)

dados_fabrica$Ci_residuo = (dados_fabrica$Ci - mean(dados_fabrica$Ci))*(dados_fabrica$Ci - mean(dados_fabrica$Ci))
sum(dados_fabrica$Ci_residuo)

custo_indireto <- dados_fabrica$Ci
producao_fabrica <- dados_fabrica$Ton
embalagens <- dados_fabrica$Emb
ferias <- factor(dados_fabrica$ferias)
ano_2008 <- factor(dados_fabrica$Ano2008)
ano_2007 <- factor(dados_fabrica$Ano2007)

par(mfrow=c(1,2))
plot(producao_fabrica, custo_indireto, type="n", xlab="Producao da fabrica",ylab="Custos indiretos")
points(producao_fabrica, custo_indireto, pch=19)

plot(embalagens, custo_indireto, type="n", xlab="Producao de embalagens",ylab="Custos indiretos")
points(embalagens, custo_indireto, pch=19)

equacao <- lm(custo_indireto~producao_fabrica+embalagens)
summary(equacao)

confint(equacao)

anova(equacao)

summary(equacao)

abline(h=0) 
qqnorm(residuals(equacao), ylab="Residuos") 
qqline(residuals(equacao)) 

shapiro.test(residuals(equacao))

median(embalagens)
median(producao_fabrica)

var.test(residuals(equacao)[embalagens<=2074],residuals(equacao)
         [embalagens>=2074])

var.test(residuals(equacao)[producao_fabrica<=50.5],residuals(equacao)
         [producao_fabrica>=50.5])

equacao_nova <- lm(custo_indireto~producao_fabrica+embalagens+ferias+ano_2007+ano_2008)
summary(equacao_nova)

vif(equacao_nova)
sqrt(vif(equacao_nova)) > 2

cor(dados_fabrica)