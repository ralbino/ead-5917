setwd("/Users/raphaelalbino/Downloads/")
dados <- read.csv2("Escores.csv")
#install.packages("UsingR")
library("UsingR")

# Regressão linear simples
#Criação do gráfico de dispersão
plot(dados$Anos_de_Experiencia, dados$Salario,       
     xlab = "Anos de Experiência",      
     ylab = "Salário",      
     bg = "red",  col = "green", cex = 1.1, pch = 21, frame = FALSE) 
abline(lm(Salario ~ Anos_de_Experiencia, data = dados), lwd = 3)

#Cálculo da correlação entre as variáveis salário e anos de experiência
correlacao <- cor(dados$Salario,dados$Anos_de_Experiencia)

# Teste da significância da correlação
# Ho: correlacao(x,y) = 0
# H1: correlacao(x,y) <> 0
teste_correlacao <- cor.test(dados$Salario,dados$Anos_de_Experiencia)
teste_correlacao
# t = dado estatístico
# df = grau de liberdade (usado para o cálculo das estatísticas)
# Como p_value é menor do que 1% é possível rejeitar H0 (de que a correlação é igual = 0 = não há correlação).
# Com 95%, a correlação está entre os intervalos 0.6641954 0.9414899.


# Calculando a regressão - primeiro a variável dependente e depois a variável independente
equação  <- lm(dados$Salario ~ dados$Anos_de_Experiencia) 
equação

#Exibindo os coeficientes da regressão
coef(equação)

#Teste na regressão linear simples.
#H0: Beta 1 = 0 (variável x não influencia variável y).
#H1: Beta 1 <> 0 (variável x há influencia variável y).
#Se p_value < alfa, rejeita-se H0 ou seja há influência.
#R_quadrado: correlação ao quadrado (variância explicada pela variância total). 73% da variação da variável y é explicado pela variável x.
#R_quadrado_ajustado: variável que deve ser olhado. Correlação ao quadrado (variância explicada pela variância total). 72% da variação da variável y é explicado pela variável x.
#Teste do modelo (teste de ANOVA - variância explicada - quanto do x explica o y): p-value: 1.541e-06
summary(equação)

# Intervalo de confiança dos Betas
confint(equação)

y <- dados$Salario
x <- dados$Anos_de_Experiencia
fit <- lm(y ~ x) 
e <- resid(fit)

plot(x,e)
plot(x,e,xlab="Anos de Experiência",ylab="Resíduos")
abline(h=0)

par(mfrow=c(1,2)) 
plot(fitted(equação),residuals(equação),xlab="Valores Esperados Salários",ylab="Resíduos")
abline(h=0)
plot(dados$Anos_de_Experiencia,residuals(equação),xlab="Anos de Experiência", ylab="Resíduos")
abline(h=0)

median(dados$Anos_de_Experiencia)

# var.test Ho: variância do lado esquerdo é igual a variância do lado direito (homocedasticidade).
# H1: as variâncias são diferentes.
# Não há dados suficientes para dizer que as variâncias são diferentes. Logo, há homocedasticidade.
# Se o teste dos resíduos fosse rejeitado, eu não posso usar regressão linear.
var.test(residuals(equação)[dados$Anos_de_Experiencia>5.5],residuals(equação)
         [dados$Anos_de_Experiencia<5.5])

# Comparação entre a distribuição normal (eixo horizontal) e os erros dos resíduos
qqnorm(residuals(equação), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(equação))

# Teste de normalidade. H0: a variável vem de uma distribuição normal.
shapiro.test(residuals(equação))

require(ggplot2)
dados1 <- data.frame(dados$Anos_de_Experiencia,dados$Salario)
# O ggplot2 exige que os dados estejam em um data.frame
p <- ggplot(dados1, aes(x=x, y=y)) + # Informa os dados a serem utilizados
  geom_point() + # Informa que eu quero um gráfico de dispersão
  xlab("Anos de Experiência") + 
  ylab("Salário")
p
p1 <- p + geom_smooth(method=lm) # Acrescenta a linha de tendência e o intervalo de confiança de predição
p1

# Regressão linear múltipla
dados$Graduacao = factor(dados$Graduacao)
summary(dados)

# Gerando a visualização entre salário comparado com anos e escore.

par(mfrow=c(1,2))
plot(c(65,100), c(20,45), type="n", xlab="Escore Teste",ylab="Salário")
points(dados$Escore_teste[dados$Graduacao== "N\xe3o"], dados$Salario[dados$Graduacao== "N\xe3o"],xlab="Escore Teste",ylab="Salário")
points(dados$Escore_teste[dados$Graduacao=="Sim"], dados$Salario[dados$Graduacao=="Sim"], pch=19)

plot(c(0,11), c(20,45), type="n", xlab="Anos de Experiência",ylab="Salário")
points(dados$Anos_de_Experiencia[dados$Graduacao== "N\xe3o"], dados$Salario[dados$Graduacao== "N\xe3o"]) 
points(dados$ Anos_de_Experiencia[dados$Graduacao== "Sim"], dados$Salario[dados$Graduacao== "Sim"], pch=19)

equation = lm(dados$Salario ~ dados$Anos_de_Experiencia + dados$Escore_teste + dados$Graduacao)
equation

summary(equation)

par(mfrow=c(2,2)) 
boxplot(dados$Salario~ dados$Graduacao, xlab="Graduação", ylab="Salário")
boxplot(dados$Anos_de_Experiencia~ dados$Graduacao, xlab="Graduação", ylab="Anos de Experiencia")
boxplot(dados$Escore_teste~ dados$Graduacao, xlab="Graduação", ylab="Escore Teste")
plot(dados$Anos_de_Experiencia, dados$Escore_teste,xlab="Anos de Experiencia",ylab="Escore Teste") 

#Observar a existência de multicolinearidade. Fazer o teste para confirmação.
require(faraway)
equation = lm(dados$Salario ~ dados$Anos_de_Experiencia + dados$Escore_teste + dados$Graduacao)
vif(equation)
sqrt(vif(equation)) > 2


#Limpa todas as variáveis utilizadas no script
rm(list=ls(all=TRUE))
cat("\014")