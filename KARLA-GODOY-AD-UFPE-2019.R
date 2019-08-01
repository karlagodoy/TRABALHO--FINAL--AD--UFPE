require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(effects)
require(tidyverse)
require(coefplot)
require(readxl)
require(dplyr)
install.packages("texreg")
require(texreg)
install.packages("olsrr")
require(olsrr)
require (fields)
require(lmtest)
require(stats)
install.packages("WriteXLS")
require(WriteXLS)
require(stargazer) 
require(interplot)
require(car)

options(scipen= 999, digits= 5)

## Agrupando dados para formar base de dados 

# Variavel Sustentabilidade

setwd("C:/Users/karla/Desktop/Artigo desenvolvimento sustentavel")

Data_ENVIRONMENTAL <- read_excel("ENVIRONMENTAL_PERFORMANCE_2018.xlsx", sheet = 1)

summary(Data_ENVIRONMENTAL)

Score_Numeric <- as.numeric(as.character(Data_ENVIRONMENTAL$SCORE))

DataEnvi<- add_column(Data_ENVIRONMENTAL, Score_Numeric)

summary(DataEnvi)

str(DataEnvi)


# GPD

setwd("C:/Users/karla/Desktop/Artigo desenvolvimento sustentavel")

Data_GDP<- read_excel("GDP_Percapita_2018.xlsx", sheet = 1)

Data_GDP_2018 <- na.omit (Data_GDP)

Data_GDP_Percapita <- Data_GDP_2018 %>% group_by(`Code`) %>% summarise(`2018 [YR2018]`)

GDP_Numeric<- as.numeric(as.character(Data_GDP_Percapita$`2018 [YR2018]`))

summary(GDP_Numeric)

DataGDP <- add_column(Data_GDP_Percapita, GDP_Numeric)

str(DataGDP)

summary(DataGDP)

# Adicionando a variavel IDH

setwd("C:/Users/karla/Desktop/Artigo desenvolvimento sustentavel")

Data_IDH<- read_excel("IDH_2017.xlsx", sheet = 1)

summary(Data_IDH)

IDH_Numeric<- as.numeric(as.character(Data_IDH$IDH))

summary(IDH_Numeric)

DataIDH <- add_column(Data_IDH, IDH_Numeric)

str(DataIDH)

DataIDH$IDH <- NULL

## Democracia

setwd("C:/Users/karla/Desktop/Artigo desenvolvimento sustentavel")

Data_DEMOCRACY <- read_excel("Democracy_Index_2018.xlsx", sheet = 1)

Data_DEMOCRACY_2018 <- Data_DEMOCRACY %>% group_by(Code) %>% summarise(`Overall score`)

summary(Data_DEMOCRACY_2018)

Overall_Numeric<- as.numeric(as.character(Data_DEMOCRACY_2018$`Overall score`))

DataDemoc <- add_column(Data_DEMOCRACY_2018, Overall_Numeric )

summary(DataDemoc)

str(DataDemoc)


# Capacidade de Governo 

setwd("C:/Users/karla/Desktop/Artigo desenvolvimento sustentavel")

Data_Gov_Eff<- read.delim("Gov_Effect.txt", header = TRUE, sep = "\t", dec = ".")

summary(Data_Gov_Eff)

Estimate_Numeric<- as.numeric(as.character(Data_Gov_Eff$`Estimate`))

summary(Estimate_Numeric)

DataGovEffet <- add_column(Data_Gov_Eff, Estimate_Numeric)

str(DataGovEffet)

summary(DataGovEffet)



# Unindo Sustentabilidade, IDH, GDP 

Env_GDP <- DataEnvi %>% full_join (DataGDP, by = c("Code"))

Env_GDP_IDH <- Env_GDP %>% full_join(DataIDH, by = c("Code"))

Env_GDP_IDH_Democ<- Env_GDP_IDH %>% full_join(DataDemoc, by = c("Code"))
  
Env_GDP_IDH_Democ_Cap <- Env_GDP_IDH_Democ %>% full_join(DataGovEffet, by = c("Code"))

#Excluindo todos os casos que tem NA

Env_GDP_IDH_Democ_Cap.1<- na.omit(Env_GDP_IDH_Democ_Cap)

summary(Env_GDP_IDH_Democ_Cap.1)

# Excluindo colunas em Caracteres

summary(Env_GDP_IDH_Democ_Cap.1)

Env_GDP_IDH_Democ_Cap.1$"Overall score" <- NULL 

Env_GDP_IDH_Democ_Cap.1$SCORE<- NULL

Env_GDP_IDH_Democ_Cap.1$`2018 [YR2018]` <- NULL

Env_GDP_IDH_Democ_Cap.1$country<- NULL

Env_GDP_IDH_Democ_Cap.1$country<- NULL


Env_GDP_IDH_Democ_Cap.1$Estimate<- NULL

summary(Env_GDP_IDH_Democ_Cap.1)


# Mudando o nome das variÃ¡veis 


names(Env_GDP_IDH_Democ_Cap.1)[grep('Score_Numeric', names(Env_GDP_IDH_Democ_Cap.1))] <- 'EnvironPerfor'

names(Env_GDP_IDH_Democ_Cap.1)[grep('IDH_Numeric', names(Env_GDP_IDH_Democ_Cap.1))] <- 'IDH'

names(Env_GDP_IDH_Democ_Cap.1)[grep('GDP_Numeric', names(Env_GDP_IDH_Democ_Cap.1))] <- 'GDP'

names(Env_GDP_IDH_Democ_Cap.1)[grep('Estimate_Numeric', names(Env_GDP_IDH_Democ_Cap.1))] <- 'GovEffect'

names(Env_GDP_IDH_Democ_Cap.1)[grep('Overall_Numeric', names(Env_GDP_IDH_Democ_Cap.1))] <- 'Democ'


names (Env_GDP_IDH_Democ_Cap.1)

# Formando base para Analise Fatorial 

Env_GDP_IDH.2 <- as.data.frame(Env_GDP_IDH_Democ_Cap.1)

summary(Env_GDP_IDH.2)

Env_GDP_IDH.2$Code <- NULL 

Env_GDP_IDH.2$COUNTRY <- NULL 

Env_GDP_IDH.2$REG <- NULL 

Env_GDP_IDH.2$Democ <- NULL 
Env_GDP_IDH.2$GovEffect <- NULL 

names(Env_GDP_IDH.2)

# Rodando Análise Fatorial

# Analisando correlação entre as variaveis do fator 

# Segundo a teoria, para criação de um fator partindo de outras  variaveis é importante que essas variaveis estejam correlacionadas entre sim. 

# Matriz de correlação

MatrizCor <- round(cor(Env_GDP_IDH.2),2)
head(MatrizCor)

# Mapa de Calor da Correlação

library(reshape2)
melted_MatrizCor<- melt(MatrizCor)
head(melted_MatrizCor)

ggplot(data =melted_MatrizCor , aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Grafico de Dispersão para observar variavel GDP e EnvironPerfor

ggplot(data = Env_GDP_IDH.2, mapping = aes(GDP,EnvironPerfor)) + 
  geom_point()


# Rodando o PCA pra determinar o numero de fatores 

Env_GDP_IDH.2.PCA <- princomp(Env_GDP_IDH.2)

summary(Env_GDP_IDH.2.PCA)

plot (Env_GDP_IDH.2.PCA)


# Baseado no PCA, tenho que utilizar 1 fator.

Env_GDP_IDH.2.AF <- factanal(Env_GDP_IDH.2, factors = 1, rotation = "varimax")

Env_GDP_IDH.2.AF

Env_GDP_IDH.AF <- factanal(Env_GDP_IDH.2, factors = 1, rotation = "varimax", scores = "regression")

Env_GDP_IDH.AF

# Fator Gerado

head(Env_GDP_IDH.AF$scores)

# Base de dados final

DesenSust <- add_column(Env_GDP_IDH.AF$scores)

# Salvando base de dados completa

ArtK <- merge(Env_GDP_IDH_Democ_Cap.1, DesenSust, by= 0)

write.csv(ArtK, "C:/Users/karla/Desktop/Artigo desenvolvimento sustentavel/karla-godoy-ad-ufpe-2019.csv",
           row.names = F)


# Abrindo dados

setwd("C:/Users/karla/Desktop/Artigo desenvolvimento sustentavel")

ArtK <- read.csv (file="karla-godoy-ad-ufpe-2019.csv", header=TRUE, sep=",")


# Análise Descritiva dos dados
summary (ArtK)

str(ArtK)

# observando a distribuicao das variaveis:

ggplot(ArtK, aes(Democ))+geom_density()

ggplot(ArtK, aes(GovEffect))+geom_density()

ggplot(ArtK, aes (Factor1))+geom_density()

# Analisando  a dispersao conjunta das variaveis

ggplot(data = ArtK, mapping = aes(Democ, Factor1)) + 
  geom_point()

ggplot(data = ArtK, mapping = aes(GovEffect, Factor1)) + 
  geom_point()

# Observamos que existe uma dispersao normal das variaveis independentes e variavel dependente.

# calculando a correlacao entre elas

cor.test(ArtK$Democ,ArtK$Factor1)

cor.test(ArtK$GovEffect, ArtK$Factor1)


## Regessão linear bivariada com a variável dependente Factor e vi Democracia

Modelo <- lm(Factor1 ~ Democ, data= ArtK)

screenreg(list(Modelo))

# =======================
#  Model 1   
# -----------------------
#  (Intercept)   -1.60 ***
#  (0.18)   
# Democ          0.29 ***
#  (0.03)   
# -----------------------
#  R^2            0.41    
# Adj. R^2       0.41    
# Num. obs.    139       
# RMSE           0.75    
# =======================
#  *** p < 0.001, ** p < 0.01, * p < 0.05

# Podemos observar que o Modelo apresenta um poder de explicação (r^2) de 0.41 da variável dependente. 


## Regressão Multivariada 

Modelo1 <- lm(Factor1 ~ Democ + GovEffect, data= ArtK)


screenreg(list(Modelo1))

# =======================
# Model 1   
# -----------------------
#  (Intercept)   -0.16    
# (0.17)   
# Democ          0.03    
# (0.03)   
# GovEffect      0.76 ***
#  (0.06)   
-----------------------
#  R^2            0.71    
# Adj. R^2       0.71    
# Num. obs.    139       
# RMSE           0.52    
# =======================
#  *** p < 0.001, ** p < 0.01, * p < 0.05

# O Modelo 1 apresenta que com aumento de 1 unidade da variavel Democracia, existe 
# um aumento de 0,03% na variavel dependente. Ja para a variavel GovEffect,podemos observar que 
# o aumento de uma unidade da variavel independente ocorre um aumento de 0.76 na variavel depentente. 
# Enquanto que o poder explicativo do modelo (r^2) é de 0.71 e o RMSE é de 0.52. Contudo, para melhor 
# entermos o melhor parametro para entendimento é o padronização dos coeficientes. 

# Coef

coef(Modelo1)

# (Intercept)       Democ   GovEffect 
# 0.159768   -0.033419   -0.764481

# Para os coefientes estimados de angulação da reta o modelo retorna os valores de 0.159768 para quando X1 e X2 forem 0
# Já para X1 o valor é -0.033419 quando GovEffect é zero. Já para X2, o valor apresentado foi de -0.764481 para quando Democ for 0.


# Confint 

confint(Modelo1)

#   2.5 %    97.5 %
# (Intercept)  0.498753 -0.179216
# Democ        0.025112 -0.091949
# GovEffect   -0.638698 -0.890264

# # Temos 95% de "chance" dos intervalos apresentados conterem o verdadeiro valor da média populacional.


# VIF

# Atraves do Vif e possivel identificar problemas de multicolinearidade com as variáveis independentes.

vif(Modelo1)

#  Democ GovEffect 
# 2.0578    2.0578 

# Os VIFs medem o quanto a variância de um coeficiente de regressão estimado aumenta se seus preditores estão correlacionados. 
# Se todos os VIFs forem 1, não há multicolinearidade, mas se alguns VIFs forem maiores do que 1, os preditores estão correlacionados. 
# Contudo, apenas quando um VIF é 5 > 10 é que o coeficiente de regressão para esse termo não é estimado de maneira apropriada. 
# Dessa forma, podemos crer que o coeficiente de regressao do modelo é estimado de maneira apropriada, apesar de apresentar Vif de 2.058.


## Observando o distribuicao de normalidade, erros e residuos: 

# Gráfico de valores ajustados pelos resíduos
ols_plot_resid_qq(Modelo1)

ols_plot_resid_fit(Modelo1)

# Teste de Shapiro para normalidade  

shapiro.test(Modelo1$residuals)

# Shapiro-Wilk normality test

# data:  Modelo1$residuals
# W = 0.987, p-value = 0.2

#O Teste de Shapiro-Wilk para normalidade avalia a aderencia dos residuos a distribuicao Normal. 
# O p-valor se refere a hipotese de que os residuos seguem de fato uma distribuicao Normal,
# e essa hipotese e rejeitada, de modo geral, quando p valor e menor que 0.05. Neste caso, podemos afirmar
# que os residuos tem distribuicao normal pois o p valor e maior que 0.05.

# Podemos verificar a homecedasticidade dos residuos também a partir do teste de Breush_Pagan. 

bptest(Modelo1)

# studentized Breusch-Pagan test

# data:  Modelo1
# BP = 14.4, df = 2, p-value = 0.00076

# Dado o p-valor (0.00076), podemos dizer que a probabilidade de cometer um erro de 
# tipo I (afirmar que a hipotese nula e falsa, dado que ela e verdadeira) é baixa. 
# Logo, concluimos que ha homocedasticidade nos residuos.


## Comparando Coeficientes

summary(Modelo1)

# padronizando coeficiente Democracy

coef(Modelo1)[2]* (sd(ArtK$Democ)/ sd(ArtK$Factor1)) 

#  Democ 
# 0.074245 

# padronizando coef Governement Effect
coef(Modelo1)[3]* (sd(ArtK$GovEffect)/ sd(ArtK$Factor1))

# GovEffect 
# 0.79031

# Padronizando os coeficientes e possivel constatar que a variavel de Capacidade de Governo 
# apresenta maior efeito/associacao sobre a variavel dependente Desenvolvimento Sustentável.


## Conclusões 

# Ao analisarmos a relação da variavel dependente Fator1, que corresponde ao fator criado para avaliar o 
# desenvolvimento sustentável em 139 países, com as variáveis independentes Democ (Democracia) e GovEffect (Efetividade do Governo) podemos 
# observar os seguintes pontos:

# Quando a analisada de forma individual, as variais independentes tem uma correlação forte e positiva com a variavel dependente.
# A regressão bivariada entre Democ e Factor1 apresenta um p-valor significativo e a variavel democracia explica 0.4% da variação na variavel dependente. 
# Contudo, quando analisado o modelo de regressão multivariada com as variaveis independentes Democracia e Efetividade do Governo, a VI Democracia apresenta um teor explicativo de apenas
# 0.07%, enquanto que a VI Efetividade de Governo explica 0.79% da variação  da variavel dependente, de acordo com a padronização dos coeficientes. 
# Já com relação aos pressupostos e ajustes do modelo, os rsiduos estão dispersos de maneira aleatoria. Existem evisdencias de que os residuos seguem uma distribuicao Normal,
# e dessa forma, existe homocedasticidade nos residuos.


