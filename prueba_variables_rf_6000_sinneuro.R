## -----------------------------------------------------------------------------
library(caret)
library(randomForest)
library(doParallel)
library(dplyr)
## -----------------------------------------------------------------------------

categoria_muestras = read.csv(file = "samples.csv", sep = "\t")

variantes_1pc = read.csv2(file = "dataset2_5pc.csv",sep = "\t")
variantes_1pc$grupo = categoria_muestras$class
variantes_1pc = variantes_1pc[variantes_1pc$grupo != "neuro_muscular",]
variantes_1pc$grupo = as.factor(variantes_1pc$grupo)
rownames(variantes_1pc) = variantes_1pc[,1]
variantes_1pc = variantes_1pc[,-1]

variantes_1pc = variantes_1pc %>%
  select(last_col(), everything())


df_hpo = read.csv2("anotacion_hpo.csv", sep = ",")


df_hpo_intersec = df_hpo[df_hpo$name == "Abnormality of the cardiovascular system",]
df_hpo_genes = strsplit(df_hpo_intersec$intersections, split = ",")

genes_hpo = c()

for (i in 1:(length(df_hpo_genes[[1]]))){
  gen = gsub("\\]","",gsub("\\'","",gsub("\\[", "", noquote(df_hpo_genes[[1]][i]))))
  gen = gsub(" ","",gen)
  genes_hpo = c(genes_hpo, gen)
}

genes_hpo = tolower(genes_hpo)

variantes_def = c()

for (i in 1:length(colnames(variantes_1pc[,-1]))){
  for (y in genes_hpo){
    if (y %in% strsplit(colnames(variantes_1pc[i]), split="_")[[1]][1]){
      variantes_def = c(variantes_def,colnames(variantes_1pc[i]))
    }
  }
  
}

variantes_def = unique(variantes_def)

df_modificado = variantes_1pc[,variantes_def]

df_modificado$grupo = categoria_muestras[categoria_muestras$class != "neuro_muscular",]$class
df_modificado$grupo = as.factor(df_modificado$grupo)

df_modificado = df_modificado %>%
  select(last_col(), everything())


resultado_rfe_tbag = readRDS("resultado_rfe_rf_6000_pruebajp")

df_modificado_reducido = df_modificado[,c(predictors(resultado_rfe_tbag),"grupo")]

set.seed(1234)

tbag_particion_2 = createDataPartition(df_modificado_reducido$grupo, p=0.80, list = FALSE)
tbag_train_data = df_modificado_reducido[tbag_particion_2,]
tbag_test_data = df_modificado_reducido[-tbag_particion_2,]



## -----------------------------------------------------------------------------
set.seed(1234)

control.cv.10 = trainControl(method = "repeatedcv", 
                                  number = 5,
                                  repeats = 2,
                                  seeds = NULL, 
                                  returnResamp = "final",
                                  verboseIter = FALSE,
                                  allowParallel = TRUE)


#-----------------------------------------

set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(20)
registerDoParallel(cl)

param.lineal = expand.grid(cost = c(0.001,0.002,0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20))

tbag_training.svm.lineal = train(grupo ~ ., data = tbag_train_data,
                      method = "svmLinear2",
                      tuneGrid = param.lineal,
                      metric = "Accuracy",
                      preProcess = c("center", "scale"),
                      trControl = control.cv.10)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(tbag_training.svm.lineal, "modelo_tbag_sinneuro_svml_var1p_pruebajp")


## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
set.seed(1234)

random.cv.10 = trainControl(method = "repeatedcv", 
                                  number = 5,
                                  repeats = 2,
                                  seeds = NULL, 
                                  returnResamp = "final",
                                  search = "random",
                                  verboseIter = FALSE,
                                  allowParallel = TRUE)


tiempo = Sys.time()

cl = makeCluster(20)
registerDoParallel(cl)

tbag_training.svm.radial = train(grupo ~ ., data = tbag_train_data,
                      method = "svmRadial",
                      tuneLength = 15,
                      metric = "Accuracy",
                      preProcess = c("center", "scale"),
                      trControl = random.cv.10)


tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)


stopCluster(cl)

saveRDS(tbag_training.svm.radial, "modelo_tbag_sinneuro_svmr_var1p_pruebajp")


## -------------------------------------------------------------------------## -----------------------------------------------------------------------------
set.seed(1234)

tiempo = Sys.time()


cl = makeCluster(20)
registerDoParallel(cl)

param.polinomial = expand.grid(C = c(0.01,0.1,0.2,0.5,1,2), degree = c(1,2,3), scale = c(0.01,0.1,1,10))

tbag_training.svm.polinomial = train(grupo ~ ., data = tbag_train_data,
                      method = "svmPoly",
                      tuneGrid = param.polinomial,
                      metric = "Accuracy",
                      preProcess = c("center", "scale"),
                      trControl = control.cv.10)


tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)


stopCluster(cl)

saveRDS(tbag_training.svm.polinomial, "modelo_tbag_sinneuro_svmp_var1p_pruebajp")



## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
raiz_variables = round(sqrt(ncol(tbag_train_data)))

tunegrid.rf.1 = expand.grid(mtry = c(raiz_variables-5,raiz_variables-2 ,raiz_variables,raiz_variables+3, raiz_variables+5, raiz_variables+10, raiz_variables+20),
                            min.node.size = c(1,2,3,4,5),
                            splitrule = c("gini","extratrees","hellinger"))



set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(20)
registerDoParallel(cl)

tbag_training.rf.1 = train(grupo ~ ., data = tbag_train_data,
                      method = "ranger",
                      tuneGrid = tunegrid.rf.1,
                      metric = "Accuracy",
                      preProcess = c("center", "scale"),
                      trControl = control.cv.10)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(tbag_training.rf.1, "modelo_tbag_sinneuro_rf_var1p_pruebajp")



## ------------------------------------------------------------------## -----------------------------------------------------------------------------
grid.diab.1 = expand.grid(laplace = 0:4,
                       usekernel = c(TRUE,FALSE),
                       adjust = 1:4)

set.seed(1234)

cl = makeCluster(20)
registerDoParallel(cl)

tbag_diab.models.1 <- train(grupo ~ ., data = tbag_train_data, 
                 method = "naive_bayes",
                 trControl = control.cv.10,
                 preProcess = c("center", "scale"),
                 tuneGrid=grid.diab.1)

stopCluster(cl)

saveRDS(tbag_diab.models.1, "modelo_tbag_sinneuro_nb_var1p_pruebajp")


