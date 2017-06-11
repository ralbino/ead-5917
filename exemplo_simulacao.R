set.seed(1) #definição de semente
Q = runif(1000,8000,12000) #distribuição uniforme: tamanho amostra, mínimo, máximo
hist(Q,nclass=10) #histograma: variável, número de classes~
CV = rnorm(1000,7,2)
CV = ifelse(CV<=1,1,CV)
CV = ifelse(CV>=10,10,CV)
hist(CV,nclass=10)

P = rnorm(1000,10,2)
P = ifelse(P<=3.5,3.5,P)
hist(P,nclass=10)

CF = 5000

L = Q*P - (Q*CV+CF)
hist(L,nclass=10)
contagem = ifelse(L<0,1,0)

sum(contagem)
prob_negativo = sum(contagem)/length(L)
prob_negativo