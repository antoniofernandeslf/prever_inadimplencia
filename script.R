##setwd("C:/Users/anton/Desktop/GitHub/prever_inadimplencia") Configurando Diretorio

##  INSTALANDO PACOTES NECESSÁRIOS
##install.packages("Amelia")
##install.packages("caret")
##install.packages("ggplot2")
##install.packages("dplyr")
##install.packages("reshape")
##install.packages("randomForest")

##  CARREGANDO PACOTES
library(Amelia)
library(caret)
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)

##  CARREGANDO DATASET
dataset <- read.csv("credit-card.csv")
View(dataset)
str(dataset)

## TRANSFORMAÇÃO E LIMPEZA DOS DADOS
dataset$AGE <- cut(dataset$AGE, c(0, 30, 50, 100), c("Jovem", "Adulto", "Idoso")) ##Categoriza os intervalor numéricos em labels
head(dataset$AGE)

dataset$SEX <- cut(dataset$SEX, c(0, 1, 2),labels = c("Masculino", "Feminino")) ##Categoriza os intervalor numéricos em labels PS: Funciona sem o "labels = c(...)
head(dataset$SEX)

dataset$EDUCATION <- cut(dataset$EDUCATION, c(0, 1, 2, 3, 4), c("Pós-Graduação", "Superior", "Ensino Médio", "Outro"))
head(dataset$EDUCATION)

dataset$MARRIAGE <- cut(dataset$MARRIAGE, c(-1, 0, 1, 2, 3), c("Desconhecido(a)", "Casado(a)", "Solteiro(a)", "Outros"))
head(dataset$MARRIAGE)

##  PAGAMENTOS
dataset$PAY_0 <- as.factor(dataset$PAY_0)
dataset$PAY_2 <- as.factor(dataset$PAY_2)
dataset$PAY_3 <- as.factor(dataset$PAY_3)
dataset$PAY_4 <- as.factor(dataset$PAY_4)
dataset$PAY_5 <- as.factor(dataset$PAY_5)
dataset$PAY_6 <- as.factor(dataset$PAY_6)

##  Alterando variavel dependente para o tipo fator
dataset$default.payment.next.month <- as.factor(dataset$default.payment.next.month)

head(dataset)

##RENOMEAR COLUNA
colnames(dataset)     ##Verifica os nomes das colunas
colnames(dataset)[25] <- "Inadimplentes"  ##Altera o nome da coluna com index 25 para inadimplentes
colnames(dataset)

##IMPORTANTE  -   VERIFICAÇÃO DE VALORES MISSING
sapply(dataset, function(x) sum(is.na(x))) ##Contagem de valores missing
missmap(dataset, main = "Valores Missing") ##Mapeamento de valores missing para melhor visualização
dataset <- na.omit(dataset) ##Remove valores missing

##REMOVER COLUNA ID - Não ajuda o modelo preditivo por ser apenas um identificador
dataset$ID <- NULL

##CONTAGEM DE INADIMPLENTES E NÃO INADIMPLENTES
table(dataset$Inadimplentes)

##Plot para vizualização dos dados
##    Eixo (coluna),     dados,    tipo de grafico    tema do grafico     elementos (     90º,    justificado)
qplot(Inadimplentes, data = dataset, geom = "bar") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Valor randomico
set.seed(1234)

##AMOSTRAGEM ESTRATIFICADA - Selecionando linhas de acordo com a variavel inadimplentes (Divisão do dataset em treino e teste)
TrainingDataIndex <- createDataPartition(dataset$Inadimplentes, p = 0.45, list = FALSE)
TrainingDataIndex          ##Indice de linhas que farão parte do dataset de treino

##Criar dados de treino como subconjunto de dados com números de indice de linha.
trainData <- dataset[TrainingDataIndex,] ##Atribuição dos dados de treino em um novo dataset
table(trainData$Inadimplentes)

##Verificar porcentagem de inadimplentes no dataset de treino - 77,8 / 22,2
prop.table(table(trainData$Inadimplentes))
 
##Contagem de linhas no dataset de treino - 13346
nrow(trainData)

##Compara as porcentagens entre as classes de treinamento e dados originais
DistributionCompare <- cbind(prop.table(table(trainData$Inadimplentes)), prop.table(table(dataset$Inadimplentes)))
colnames(DistributionCompare) <- c("Treinamento", "Original")
DistributionCompare


##Converter colunas em linhas - Melt data - Pivot
MeltedComp <- melt(DistributionCompare)
MeltedComp

##Plots para ver a distribuição de treinamento vs original
ggplot(MeltedComp, aes(x = X1, y = value)) + geom_bar(aes(fill = X2), stat = "identity", position = "dodge")


##Criando conjunto de dados de TESTE
testData <- dataset[-TrainingDataIndex]

#Criando paramentro de treinamento com validação cruzada de 10 folds
TrainingParameters <- trainControl(method = "cv", number = 10)



















