setwd("/Users/raphaelalbino/Downloads/")
dados <- read.csv2("Escores.csv")
#install.packages("UsingR")
library("UsingR")

# Regress�o linear simples
#Cria��o do gr�fico de dispers�o
plot(dados$Anos_de_Experiencia, dados$Salario,       
     xlab = "Anos de Experi�ncia",      
     ylab = "Sal�rio",      
     bg = "red",  col = "green", cex = 1.1, pch = 21, frame = FALSE) 
abline(lm(Salario ~ Anos_de_Experiencia, data = dados), lwd = 3)

#C�lculo da correla��o entre as vari�veis sal�rio e anos de experi�ncia
correlacao <- cor(dados$Salario,dados$Anos_de_Experiencia)

# Teste da signific�ncia da correla��o
# Ho: correlacao(x,y) = 0
# H1: correlacao(x,y) <> 0
teste_correlacao <- cor.test(dados$Salario,dados$Anos_de_Experiencia)
teste_correlacao
# t = dado estat�stico
# df = grau de liberdade (usado para o c�lculo das estat�sticas)
# Como p_value � menor do que 1% � poss�vel rejeitar H0 (de que a correla��o � igual = 0 = n�o h� correla��o).
# Com 95%, a correla��o est� entre os intervalos 0.6641954 0.9414899.


# Calculando a regress�o - primeiro a vari�vel dependente e depois a vari�vel independente
equa��o  <- lm(dados$Salario ~ dados$Anos_de_Experiencia) 
equa��o

#Exibindo os coeficientes da regress�o
coef(equa��o)

#Teste na regress�o linear simples.
#H0: Beta 1 = 0 (vari�vel x n�o influencia vari�vel y).
#H1: Beta 1 <> 0 (vari�vel x h� influencia vari�vel y).
#Se p_value < alfa, rejeita-se H0 ou seja h� influ�ncia.
#R_quadrado: correla��o ao quadrado (vari�ncia explicada pela vari�ncia total). 73% da varia��o da vari�vel y � explicado pela vari�vel x.
#R_quadrado_ajustado: vari�vel que deve ser olhado. Correla��o ao quadrado (vari�ncia explicada pela vari�ncia total). 72% da varia��o da vari�vel y � explicado pela vari�vel x.
#Teste do modelo (teste de ANOVA - vari�ncia explicada - quanto do x explica o y): p-value: 1.541e-06
summary(equa��o)

# Intervalo de confian�a dos Betas
confint(equa��o)

y <- dados$Salario
x <- dados$Anos_de_Experiencia
fit <- lm(y ~ x) 
e <- resid(fit)

plot(x,e)
plot(x,e,xlab="Anos de Experi�ncia",ylab="Res�duos")
abline(h=0)

par(mfrow=c(1,2)) 
plot(fitted(equa��o),residuals(equa��o),xlab="Valores Esperados Sal�rios",ylab="Res�duos")
abline(h=0)
plot(dados$Anos_de_Experiencia,residuals(equa��o),xlab="Anos de Experi�ncia", ylab="Res�duos")
abline(h=0)

median(dados$Anos_de_Experiencia)

# var.test Ho: vari�ncia do lado esquerdo � igual a vari�ncia do lado direito (homocedasticidade).
# H1: as vari�ncias s�o diferentes.
# N�o h� dados suficientes para dizer que as vari�ncias s�o diferentes. Logo, h� homocedasticidade.
# Se o teste dos res�duos fosse rejeitado, eu n�o posso usar regress�o linear.
var.test(residuals(equa��o)[dados$Anos_de_Experiencia>5.5],residuals(equa��o)
         [dados$Anos_de_Experiencia<5.5])

# Compara��o entre a distribui��o normal (eixo horizontal) e os erros dos res�duos
qqnorm(residuals(equa��o), ylab="Res�duos",xlab="Quantis te�ricos",main="")
qqline(residuals(equa��o))

# Teste de normalidade. H0: a vari�vel vem de uma distribui��o normal.
shapiro.test(residuals(equa��o))

require(ggplot2)
dados1 <- data.frame(dados$Anos_de_Experiencia,dados$Salario)
# O ggplot2 exige que os dados estejam em um data.frame
p <- ggplot(dados1, aes(x=x, y=y)) + # Informa os dados a serem utilizados
  geom_point() + # Informa que eu quero um gr�fico de dispers�o
  xlab("Anos de Experi�ncia") + 
  ylab("Sal�rio")
p
p1 <- p + geom_smooth(method=lm) # Acrescenta a linha de tend�ncia e o intervalo de confian�a de predi��o
p1

# Regress�o linear m�ltipla
dados$Graduacao = factor(dados$Graduacao)
summary(dados)

# Gerando a visualiza��o entre sal�rio comparado com anos e escore.

par(mfrow=c(1,2))
plot(c(65,100), c(20,45), type="n", xlab="Escore Teste",ylab="Sal�rio")
points(dados$Escore_teste[dados$Graduacao== "N\xe3o"], dados$Salario[dados$Graduacao== "N\xe3o"],xlab="Escore Teste",ylab="Sal�rio")
points(dados$Escore_teste[dados$Graduacao=="Sim"], dados$Salario[dados$Graduacao=="Sim"], pch=19)

plot(c(0,11), c(20,45), type="n", xlab="Anos de Experi�ncia",ylab="Sal�rio")
points(dados$Anos_de_Experiencia[dados$Graduacao== "N\xe3o"], dados$Salario[dados$Graduacao== "N\xe3o"]) 
points(dados$ Anos_de_Experiencia[dados$Graduacao== "Sim"], dados$Salario[dados$Graduacao== "Sim"], pch=19)

equation = lm(dados$Salario ~ dados$Anos_de_Experiencia + dados$Escore_teste + dados$Graduacao)
equation

summary(equation)

par(mfrow=c(2,2)) 
boxplot(dados$Salario~ dados$Graduacao, xlab="Gradua��o", ylab="Sal�rio")
boxplot(dados$Anos_de_Experiencia~ dados$Graduacao, xlab="Gradua��o", ylab="Anos de Experiencia")
boxplot(dados$Escore_teste~ dados$Graduacao, xlab="Gradua��o", ylab="Escore Teste")
plot(dados$Anos_de_Experiencia, dados$Escore_teste,xlab="Anos de Experiencia",ylab="Escore Teste") 

#Observar a exist�ncia de multicolinearidade. Fazer o teste para confirma��o.
require(faraway)
equation = lm(dados$Salario ~ dados$Anos_de_Experiencia + dados$Escore_teste + dados$Graduacao)
vif(equation)
sqrt(vif(equation)) > 2


#Limpa todas as vari�veis utilizadas no script
rm(list=ls(all=TRUE))
cat("\014")