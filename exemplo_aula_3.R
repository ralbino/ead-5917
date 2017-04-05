install.packages("psych")
install.packages("multilevel")
library(multilevel)
library(psych)
library(ggplot2)

getwd()
MOD <- read.table("Exemplo_Aula2MOD.txt")
describeBy(MOD, MOD$condition)

modelo0 <- lm(MOD$IQ ~ MOD$D1 + MOD$D2)
summary(modelo0)
confint(modelo0)
#dada as condições (categorias) como se comporta a variância do IQ.
modelo0a <- aov(MOD$IQ ~ MOD$condition)
TukeyHSD(modelo0a)

modelo1 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2)
summary(modelo1)

ggplot(MOD, aes(x = WM, y = IQ)) + geom_smooth(method = "lm") + 
  geom_point()

# Criar novas variáveis preditoras
MOD$WM.D1 <- (MOD$WM * MOD$D1)
MOD$WM.D2 <- (MOD$WM * MOD$D2)
View(MOD)

model2 <- lm(MOD$IQ ~ MOD$WM + MOD$D1 + MOD$D2 + MOD$WM.D1 + MOD$WM.D2)
summary(model2)

color <- c("red","green","blue")
ggplot(MOD, aes(x = WM, y = IQ)) + stat_smooth(method="lm", se=F) +
  geom_point(aes(color=condition))
ggplot(MOD, aes(x = WM, y = IQ)) + 
  geom_smooth(aes(group=condition), method="lm", se=T, color="black", fullrange=T) +
  geom_point(aes(color=condition))

anova(modelo1, model2)

# Correlação e Scatter plot
WM.control <- MOD$WM[1:50]
IQ.control <- MOD$IQ[1:50]
WM.threat1 <- MOD$WM[51:100]
IQ.threat1 <- MOD$IQ[51:100]
WM.threat2 <- MOD$WM[101:150]
IQ.threat2 <- MOD$IQ[101:150]

cor(MOD$WM,MOD$IQ)
cor.test(MOD$WM,MOD$IQ)
# Quando não havia ameaça, a capacidade de trabalho não era afetado.
cor(IQ.control, WM.control)

# Quando houve ameaça, a capacidade de trabalho passou a ser afetada.
cor(IQ.threat1,WM.threat1)
cor(IQ.threat2,WM.threat2)

# Leitura dos dados
MED <- read.table("Exemplo_Aula2MED.txt", header = T)

# Para verificar os dados
View(MED)

# Resumo das estatísticas
describeBy(MED, MED$condition)

# A função sobel no pacote multinível executa toda a análise de mediação em uma etapa, mas primeiro vamos fazê-lo com 3 modelos lm
model.YX <- lm(MED$IQ ~ MED$condition)
model.YXM <- lm(MED$IQ ~ MED$condition + MED$WM)
model.MX <- lm(MED$WM ~ MED$condition)

summary(model.YX)
summary(model.YXM)
summary(model.MX)

# Compare os resultados com a saída da função sobel
# Função sobel(x, mediador, y)
model.ALL <- sobel(MED$condition, MED$WM, MED$IQ) 
model.ALL

# $z.value [1] -3.871839 > se z.value for maior do que 1.96 (rejeito a h0 - não há mediação)