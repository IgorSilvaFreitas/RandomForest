#Pacotes
library(caret)
library(randomForest)
library(RANN)

#Criando amostras treino e teste
load("dados_desafioII.rda")

dados <- as.data.frame(dados)
set.seed(100)
totrain <- createDataPartition(y=dados$diagnosis, p=0.70, list=F)
treino <- dados[totrain,]
teste <- dados[-totrain,]


#tratamento para imputação de dados faltantes
preproc_NA <- caret::preProcess(treino,method = "knnImpute", k=5)

treino <- predict(preproc_NA,treino)
teste <- predict(preproc_NA,teste)

#padronização das variáveis
preproc <- caret::preProcess(treino , method = c("center", "scale"))

treino <- predict(preproc, treino)
teste <- predict(preproc,teste)

#normalização das variáveis
preproc2 <- caret::preProcess(treino, method = "YeoJohnson")

treino <- predict(preproc2, treino)
teste <- predict(preproc2,teste)

#verificando se existe variância quase zero nas variáveis
preproc3 <- caret::nearZeroVar(treino,saveMetrics = F,names=T)
preproc3
#nenhum variável a ser removida

#verificando se existe dependência Linear entre as variáveis explicativas
dl <- caret::findLinearCombos(treino[,-1])
dl
#Nenhuma variável a ser removida

#verificando se existe correlação entre as variáveis explicativas
correla <- caret::preProcess(treino, method="corr")
correla

treino <- predict(correla, treino)
teste <- predict(correla,teste)
#10 variáveis removidas

#Verificando melhor valor para o parametro ntree 
modelFit<-randomForest(diagnosis ~ ., data=treino, ntree=2000,
                       proximity=T)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(modelFit$err.rate), times=3),
  Type=rep(c("OOB", "M", "B"), 
           each=nrow(modelFit$err.rate)),
  Error=c(modelFit$err.rate[,"OOB"],
          modelFit$err.rate[,"M"],
          modelFit$err.rate[,"B"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
#ntree = 380 parece adequado

# #pre processamentos necessários juntos para salvar no rdata
# preproc <- preProcess(treino, method = c("knnImpute","center", "scale","YeoJohnson","corr"))
# treino <- predict(preproc,treino)
# teste <- predict(preproc,teste)

#construção do modeolo
set.seed(100)
controle <- caret::trainControl(method="oob")

modelo <- train(diagnosis ~ ., data=treino, method="rf", ntree = 380,
                trControl=controle)
#mtry escolhido pelo modelo foi 2.

#Aplicando o modelo na amostra Teste
preditor <- predict(modelo, teste)

#Estimando o erro fora da amostra
caret::confusionMatrix(preditor, teste$diagnosis)

#accuracy = 0.9606
#kappa = 0.9159
#sensibilidade = 0.9574
#especificidade = 0.9625

save(preproc, controle, modelo, file="IgorFreitas.RData")
