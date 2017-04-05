# Primeiro teste em uma regressão do modelo

(Intercept)                 7.9448     7.3808   1.076   0.2977   

dados$Anos_de_Experiencia   1.1476     0.2976   3.856   0.0014 **

dados$Escore_teste          0.1969     0.0899   2.191   0.0436 * 

dados$GraduacaoSim          2.2804     1.9866   1.148   0.2679   

---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.396 on 16 degrees of freedom
Multiple R-squared:  0.8468,	Adjusted R-squared:  0.8181 
F-statistic: 29.48 on 3 and 16 DF,  p-value: 9.417e-07

## 1. Análise do r_quadrado_ajustado

Combinação das variáveis independentes). 81%. Ajuste do modelo. 
A variância esperada sobre a variância total. Adjusted R-squared:  0.8181

## 2. Análise do modelo como um todo. 

Será que o modelo é significativo? Os valores dos betas são diferentes de 0.

H0: B1 = B2 = B3 = ... = Bn = 0 (não existe regressão).
H1: Existe pelo menos um Beta que é diferente de 0 (existe regressão para pelo menos uma variável independente).

pvalor é aproximadamente zero, o que faz com que seja rejeitado H0. Ou seja, pelo menos uma variável influência salário. p-value: 9.417e-07

## 3. Olhar para cada uma das variáveis

Para cada variável incluída no modelo.
H0: Bi = 0
H1: Bi <> 0

p_valor < 5% 
Variáveis: Anos_de_Experiencia e Escore_teste