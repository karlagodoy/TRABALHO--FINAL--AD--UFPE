#####################################
## Trabalho Final de Análise de Dados
## Karla Godoy da Costa Lima
## Data: 26/08/1996
#####################################


# Abrindo pacotes
if(require(magrittr) == F) install.packages('magrittr'); require(magrittr)
if(require(rio) == F) install.packages('rio'); require(rio)
if(require(fields) == F) install.packages('fields'); require(fields)
if(require(car) == F) install.packages('car'); require(car)
if(require(MASS) == F) install.packages('MASS'); require(MASS)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(psych) == F) install.packages('psych'); require(psych)
if(require(sandwich) == F) install.packages('sandwich'); require(sandwich)
if(require(lmtest) == F) install.packages('lmtest'); require(lmtest)
if(require(foreign)== F) install.packages("foreign"); require(foreign)
if(require(nnet)== F) install.packages("nnet"); require(nnet)
if(require(reshape2)==F) install.packages("reshape2"); require (reshape2)
if(require(effects)== F) install.packages("effects"); require(effects)
if (require(tidyverse)== F) install.packages("tidyverse"); require(tidyverse)
if (require (texreg)== F) install.packages("texreg"); require (texreg)
if (require(olsrr)== F) install.packages("olsrr"); require(olsrr)
if(require(dotwhisker)==F)install.packages('dotwhisker');require(dotwhisker)
if(require(broom)==F)install.packages('broom');require(broom)



# Definindo o diretório 

setwd("C:/Users/karla/Desktop/Artigo desenvolvimento sustentavel")



# Abrindo base de dados

ArtK <- read.csv (file="karla-godoy-ad-ufpe-2019.csv", header=TRUE, sep=",")

# Análise descritiva das variáveis

describe(ArtK[c("Factor1","Democ",
                   "GovEffect")])

## Análise gráficas das variáveis


# Grafico de linha das variaveis 

ggplot(ArtK, aes(Democ))+geom_density()

ggplot(ArtK, aes(GovEffect))+geom_density()

ggplot(ArtK, aes (Factor1))+geom_density()


# Observando a distribuicao das variaveis:


Plot(ArtK$Factor1, pch=1, lwd=2,
    main="Fator de Desenvolvimento Sustentável",
    xlab="", ylab="") 

plot(ArtK$GovEffect, pch=1, lwd=2,
     main="Índice Efetividade de Governo",
     xlab="", ylab="") 

plot(ArtK$Democ, pch=1, lwd=2,
     main="Índice Democrático",
     xlab="", ylab="") 


# Visualizando relação entre as variáveis

ggplot(data = ArtK, mapping = aes(Democ, Factor1)) + 
  geom_point() + xlab("Democracia") +
  ylab("Fator de Desenvolvimento Sustentável")

ggplot(data = ArtK, mapping = aes(GovEffect, Factor1)) + 
  geom_point()+ xlab("Efetividade de Governo") +
  ylab("Fator de Desenvolvimento Sustentável")

## Regessão linear bivariada com a variável dependente Factor e vi Democracia

Modelo1 <- lm(Factor1 ~ Democ, data= ArtK)

# Resumo do modelo 1

screenreg(list(Modelo1))

## Gráfico do Modelo 1

dwplot(Modelo1,vline =geom_vline(xintercept =0,colour ="grey60",linetype =2))

## Regessão linear bivariada com a variável dependente Factor e vi Efetividade de Governo

Modelo2 <- lm(Factor1 ~ GovEffect, data= ArtK)

# Resumo do modelo 2

screenreg(list(Modelo2))

## Gráfico do Modelo 2

dwplot(Modelo2,vline =geom_vline(xintercept =0,colour ="grey60",linetype =2))

## Regressão Multivariada 

Modelo3 <- lm(Factor1 ~ Democ + GovEffect, data= ArtK)

# Resumo do modelo 3

screenreg(list(Modelo3))

## Gráfico da regressão 3

dwplot(Modelo3,vline =geom_vline(xintercept =0,colour ="grey60",linetype =2))

## Checando ajuste do modelo

coef(Modelo3)

confint(Modelo3)

## Pressupostos do Modelo

# Multicolinearidade 

vif(Modelo3)

#Valores Ajustados pelos resíduos

# Gráfico de valores ajustados pelos resíduos
ols_plot_resid_qq(Modelo3)

ols_plot_resid_fit(Modelo3)

# Teste de Shapiro para normalidade  

shapiro.test(Modelo3$residuals)


## Comparando Coeficientes

summary(Modelo3)

# padronizando coeficiente Democracy

coef(Modelo3)[2]* (sd(ArtK$Democ)/ sd(ArtK$Factor1)) 

#  Democ 
# 0.074245 

# padronizando coef Governement Effect
coef(Modelo3)[3]* (sd(ArtK$GovEffect)/ sd(ArtK$Factor1))

# GovEffect 
# 0.79031
