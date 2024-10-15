## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
resultado_modelo_tbag_sinneuro_svml = readRDS("modelo_tbag_sinneuro_svml_var1p")
resultado_modelo_tbag_sinneuro_svmr = readRDS("modelo_tbag_sinneuro_svmr_var1p")
resultado_modelo_tbag_sinneuro_svmp = readRDS("modelo_tbag_sinneuro_svmp_var1p")
resultado_modelo_tbag_sinneuro_rf = readRDS("modelo_tbag_sinneuro_rf_var1p")
resultado_modelo_tbag_sinneuro_nb = readRDS("modelo_tbag_sinneuro_nb_var1p")

resultado_modelo_lda_sinneuro_svml = readRDS("modelo_lda_sinneuro_svml_var1p")
resultado_modelo_lda_sinneuro_svmr = readRDS("modelo_lda_sinneuro_svmr_var1p")
resultado_modelo_lda_sinneuro_svmp = readRDS("modelo_lda_sinneuro_svmp_var1p")
resultado_modelo_lda_sinneuro_rf = readRDS("modelo_lda_sinneuro_rf_var1p")
resultado_modelo_lda_sinneuro_nb = readRDS("modelo_lda_sinneuro_nb_var1p")

resultado_modelo_nb_sinneuro_svml = readRDS("modelo_nb_sinneuro_svml_var1p")
resultado_modelo_nb_sinneuro_svmr = readRDS("modelo_nb_sinneuro_svmr_var1p")
resultado_modelo_nb_sinneuro_svmp = readRDS("modelo_nb_sinneuro_svmp_var1p")
resultado_modelo_nb_sinneuro_rf = readRDS("modelo_nb_sinneuro_rf_var1p")
resultado_modelo_nb_sinneuro_nb = readRDS("modelo_nb_sinneuro_nb_var1p")

resultado_modelo_rf_sinneuro_svml = readRDS("modelo_rf_sinneuro_svml_var1p")
resultado_modelo_rf_sinneuro_svmr = readRDS("modelo_rf_sinneuro_svmr_var1p")
resultado_modelo_rf_sinneuro_svmp = readRDS("modelo_rf_sinneuro_svmp_var1p")
resultado_modelo_rf_sinneuro_rf = readRDS("modelo_rf_sinneuro_rf_var1p")
resultado_modelo_rf_sinneuro_nb = readRDS("modelo_rf_sinneuro_nb_var1p")


## -----------------------------------------------------------------------------
objetos2 = list(
  tbag_sinneuro_svml = resultado_modelo_tbag_sinneuro_svml,
  tbag_sinneuro_svmr = resultado_modelo_tbag_sinneuro_svmr,
  tbag_sinneuro_svmp = resultado_modelo_tbag_sinneuro_svmp,
  tbag_sinneuro_rf = resultado_modelo_tbag_sinneuro_rf,
  tbag_sinneuro_nb = resultado_modelo_tbag_sinneuro_nb,
  
  lda_sinneuro_svml = resultado_modelo_lda_sinneuro_svml,
  lda_sinneuro_svmr = resultado_modelo_lda_sinneuro_svmr,
  lda_sinneuro_svmp = resultado_modelo_lda_sinneuro_svmp,
  lda_sinneuro_rf = resultado_modelo_lda_sinneuro_rf,
  lda_sinneuro_nb = resultado_modelo_lda_sinneuro_nb,
  
  nb_sinneuro_svml = resultado_modelo_nb_sinneuro_svml,
  nb_sinneuro_svmr = resultado_modelo_nb_sinneuro_svmr,
  nb_sinneuro_svmp = resultado_modelo_nb_sinneuro_svmp,
  nb_sinneuro_rf = resultado_modelo_nb_sinneuro_rf,
  nb_sinneuro_nb = resultado_modelo_nb_sinneuro_nb,
  
  rf_sinneuro_svml = resultado_modelo_rf_sinneuro_svml,
  rf_sinneuro_svmr = resultado_modelo_rf_sinneuro_svmr,
  rf_sinneuro_svmp = resultado_modelo_rf_sinneuro_svmp,
  rf_sinneuro_rf = resultado_modelo_rf_sinneuro_rf,
  rf_sinneuro_nb = resultado_modelo_rf_sinneuro_nb
)


## -----------------------------------------------------------------------------

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


resultado_rfe_tbag = readRDS("resultado_rfe_treebag_6000_pruebajp")

df_modificado_reducido = df_modificado[,c(predictors(resultado_rfe_tbag),"grupo")]

set.seed(1234)

tbag_particion_2 = createDataPartition(df_modificado_reducido$grupo, p=0.80, list = FALSE)
tbag_train_data = df_modificado_reducido[tbag_particion_2,]
tbag_test_data = df_modificado_reducido[-tbag_particion_2,]

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
# Crear un data frame vacío para almacenar los resultados
resultados_test_2class <- data_frame(
  Algoritmo = character(),
  Precisión = numeric(),
  Sensibilidad = numeric(),
  Especificidad = numeric(),
  Hiperparametros = character(),
  Condiciones = character()
)

# Verificar que el archivo RDS existe antes de leerlo
for (nombre in names(objetos2)){
  set.seed(1234)
  
  objeto = objetos2[[nombre]]
  
  # Realizar predicción
  set.seed(1234)
  prediccion <- predict(objeto, newdata = tbag_test_data)
  
  # Calcular matriz de confusión
  set.seed(1234)
  matriz_conf <- confusionMatrix(prediccion, tbag_test_data$grupo)
  
  # Combinación de hiperparámetros
  combinacion_hiperparametros <- paste(names(objeto$bestTune), objeto$bestTune, sep = "=", collapse = ", ")
  
  # Métricas de calidad
  metricas_calidad <- c(
    round(as.numeric(matriz_conf$overall["Accuracy"][[1]]), 4),
    round(as.numeric(matriz_conf$byClass["Sensitivity"][[1]]), 4),
    round(as.numeric(matriz_conf$byClass["Specificity"][[1]]), 4)
  )
  
  # Crear fila a insertar
  fila_a_insertar <- c(
    objeto$method,
    metricas_calidad,
    combinacion_hiperparametros,
    nombre
  )
  
  resultados_test_2class <- bind_rows(resultados_test_2class, data_frame(
    Algoritmo = fila_a_insertar[1],
    Precisión = as.numeric(fila_a_insertar[2]),
    Sensibilidad = as.numeric(fila_a_insertar[3]),
    Especificidad = as.numeric(fila_a_insertar[4]),
    Hiperparametros = fila_a_insertar[5],
    Condiciones = fila_a_insertar[6]
  ))
}

saveRDS(resultados_test_2class, "resultados_comparacion_2class_var1p")

write.csv2(resultados_test_2class, "comparación_2clases_var1p_pruebajp.csv")
## -----------------------------------------------------------------------------



