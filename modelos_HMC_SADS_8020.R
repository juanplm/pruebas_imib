library(caret)
library(randomForest)
library(doParallel)
library(dplyr)

muestras = read.csv("samples_anotadas.csv")
variantes = read.csv("dataset2_5pc.csv", sep="\t")
names(muestras)[names(muestras) == "id_sample"] = "sample"

df_completo = merge(variantes, muestras, by = "sample")

df_final = df_completo %>% select(-c(id_origen, afectado, patologia_secundaria, SnomedCT, etiqueta_1, etiqueta_2, etiqueta_3))

df_final = df_final[df_final$patologia_principal %in% c("SADS CI o sosp","Hipertrofica","SADS-corazon normal"),]

df_final$patologia_principal[df_final$patologia_principal == "SADS CI o sosp"] = "SADS"
df_final$patologia_principal[df_final$patologia_principal == "SADS-corazon normal"] = "SADS"

df_final$patologia_principal = as.factor(df_final$patologia_principal)

rownames(df_final) = df_final$sample
df_final = df_final %>% select(-sample)

df_final = df_final %>% select(last_col(), everything())

df_hiper = df_final[df_final$patologia_principal == "Hipertrofica",]
set.seed(1234)
df_hiper_reducido = df_hiper %>% sample_n(80)

df_sads = df_final[df_final$patologia_principal == "SADS",]

df_reducido = rbind(df_hiper_reducido, df_sads)

set.seed(1234)
particion = createDataPartition(df_reducido$patologia_principal, p=0.8, list = FALSE)

train_data = df_reducido[particion,]
test_data = df_reducido[-particion,]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

seeds <- list(
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 200),  # Para la primera partición
  sample.int(1000, 1)   # Semilla final (solo un número)
)

controlcv = trainControl(method = "repeatedcv",
                        repeats = 2,
                        number = 5,
                        allowParallel = TRUE,
                        returnResamp = "all",
                        verboseIter = FALSE,
                        seeds = seeds)

### SVM Lineal ---------------------------------------------------------------

start_time = Sys.time()

cl = makeCluster(15)

registerDoParallel(cl)

param.lineal = expand.grid(cost = c(0.001,0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 20))

modelos.svm.lineal = train(patologia_principal ~ ., data = train_data,
                                 method = "svmLinear2",
                                 tuneGrid = param.lineal,
                                 metric = "Accuracy",
                                 preProcess = c("center", "scale"),
                                 trControl = controlcv)

stopCluster(cl)
end_time = Sys.time()
print(end_time - start_time)

saveRDS(modelos.svm.lineal, "modelos_2fenotipos_svml_8020")


# SVM Radial ----------------------------------------------------------

randomcv = trainControl(method = "repeatedcv",
                            repeats = 2,
                            number = 5,
                            seeds = seeds, 
                            returnResamp = "all",
                            search = "random",
                            verboseIter = FALSE,
                            allowParallel = TRUE)

start_time = Sys.time()

cl = makeCluster(15)

registerDoParallel(cl)

modelos.svm.radial = train(patologia_principal ~ ., data = train_data,
                           method = "svmRadial",
                           tuneLength = 15,
                           metric = "Accuracy",
                           preProcess = c("center", "scale"),
                           trControl = randomcv)

stopCluster(cl)
end_time = Sys.time()
print(end_time - start_time)

saveRDS(modelos.svm.radial, "modelos_2fenotipos_svmr_8020")


#### SVM Polinomial ------------------------------------------------------------

param.polinomial = expand.grid(C = c(0.01,0.1,0.2,0.5,1,2), degree = c(1,2,3), scale = c(0.01,0.1,1,10))

start_time = Sys.time()

cl = makeCluster(15)

registerDoParallel(cl)

modelos.svm.polinomial = train(patologia_principal ~ ., data = train_data,
                               method = "svmPoly",
                               tuneGrid = param.polinomial,
                               metric = "Accuracy",
                               preProcess = c("center", "scale"),
                               trControl = controlcv)

stopCluster(cl)
end_time = Sys.time()
print(end_time - start_time)

saveRDS(modelos.svm.polinomial, "modelos_2fenotipos_svmp_8020")


### Random Forest ------------------------------------------------------------

raiz_variables = round(sqrt(ncol(train_data)))

param.rf = expand.grid(mtry = c(raiz_variables-50,raiz_variables-30 ,raiz_variables-10,raiz_variables, raiz_variables+20, raiz_variables+40, raiz_variables+100,raiz_variables+200,raiz_variables+400),
                            min.node.size = c(1,2,3,4,5),
                            splitrule = c("gini","extratrees","hellinger"))



start_time = Sys.time()

cl = makeCluster(15)
registerDoParallel(cl)

modelos.rf = train(patologia_principal ~ ., data = train_data,
                           method = "ranger",
                           tuneGrid = param.rf,
                           metric = "Accuracy",
                           preProcess = c("center", "scale"),
                           trControl = controlcv)

stopCluster(cl)
end_time = Sys.time()
print(end_time - start_time)

saveRDS(modelos.rf, "modelos_2fenotipos_rf_8020")


### Naive Bayes

parama.nb = expand.grid(laplace = 0:3,
                          usekernel = c(TRUE,FALSE),
                          adjust = 1:3)


start_time = Sys.time()

cl = makeCluster(15)
registerDoParallel(cl)

modelos.nb <- train(patologia_principal ~ ., data = train_data, 
                            method = "naive_bayes",
                            trControl = controlcv,
                            preProcess = c("center", "scale"),
                            tuneGrid=parama.nb)



stopCluster(cl)
end_time = Sys.time()
print(end_time - start_time)

saveRDS(modelos.nb, "modelos_2fenotipos_nb_8020")




### Fase de evaluación y de test

mejor_svml = modelos.svm.lineal$results[order(modelos.svm.lineal$results$Accuracy, decreasing = TRUE),][1,]["Accuracy"]
mejor_svmr = modelos.svm.radial$results[order(modelos.svm.radial$results$Accuracy, decreasing = TRUE),][1,]["Accuracy"]
mejor_svmp = modelos.svm.polinomial$results[order(modelos.svm.polinomial$results$Accuracy, decreasing = TRUE),][1,]["Accuracy"]
mejor_rf = modelos.rf$results[order(modelos.rf$results$Accuracy, decreasing = TRUE),][1,]["Accuracy"]
mejor_nb = modelos.nb$results[order(modelos.nb$results$Accuracy, decreasing = TRUE),][1,]["Accuracy"]


resultados_modelos_entrenamiento = data.frame(Algoritmos = c("RF","SVML","SVMR","SVMP","NB"),
                                     Máx.Accuracy = c(mejor_rf$Accuracy, mejor_svml$Accuracy, mejor_svmr$Accuracy, mejor_svmp$Accuracy, mejor_nb$Accuracy))
resultados_modelos_entrenamiento[order(resultados_modelos_entrenamiento$Máx.Accuracy, decreasing = TRUE),]


### Test SVML

set.seed(1234)

predict_svml = predict(modelos.svm.lineal, newdata = test_data)

confusion_matrix_svml = confusionMatrix(predict_svml, test_data$patologia_principal)
confusion_matrix_svml

### Test SVMR

set.seed(1234)

predict_svmr = predict(modelos.svm.radial, newdata = test_data)

confusion_matrix_svmr = confusionMatrix(predict_svmr, test_data$patologia_principal)
confusion_matrix_svmr

### Test SVMP

set.seed(1234)

predict_svmp = predict(modelos.svm.polinomial, newdata = test_data)

confusion_matrix_svmp = confusionMatrix(predict_svmp, test_data$patologia_principal)
confusion_matrix_svmp

### Test RF

set.seed(1234)

predict_rf = predict(modelos.rf, newdata = test_data)

confusion_matrix_rf = confusionMatrix(predict_rf, test_data$patologia_principal)
confusion_matrix_rf

### Test NB

set.seed(1234)

predict_nb = predict(modelos.nb, newdata = test_data)

confusion_matrix_nb = confusionMatrix(predict_nb, test_data$patologia_principal)
confusion_matrix_nb
