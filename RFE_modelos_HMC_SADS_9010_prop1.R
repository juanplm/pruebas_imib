library(caret)
library(randomForest)
library(doParallel)
library(dplyr)

setwd("~/pruebas_jp/pruebas_imib")

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
df_hiper_reducido = df_hiper %>% sample_n(57)

df_sads = df_final[df_final$patologia_principal == "SADS",]

df_reducido = rbind(df_hiper_reducido, df_sads)

set.seed(1234)
particion = createDataPartition(df_reducido$patologia_principal, p=0.9, list = FALSE)

train_data = df_reducido[particion,]
test_data = df_reducido[-particion,]

x_train_prueba = train_data[, -1]
y_train_prueba = train_data[, 1]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Semillas de aleatorización para la generación en paralelo de modelos

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

###############   Pruebas de RFE  #############################

#### Random Forest

control_rfe_rf = rfeControl(functions = rfFuncs,
                            method = "repeatedcv",
                            repeats = 2,
                            number = 5,
                            allowParallel = TRUE,
                            seeds = seeds)

set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(15)
registerDoParallel(cl)

resultado_rfe_rf = rfe(x = x_train_prueba,
                        y = y_train_prueba,
                        sizes = c(1:20,40,60,80,100,150,200,250,300,350,400,500,600,700,800,900,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4500,5000,5500),
                        rfeControl = control_rfe_rf)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(resultado_rfe_rf, file = "rfe_rf_9010_prop1")


#### Bagged Trees


control_rfe_treebag = rfeControl(functions = treebagFuncs,
                                 method = "repeatedcv",
                                 repeats = 2,
                                 number = 5,
                                 allowParallel = TRUE,
                                 seeds = seeds)


set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(15)
registerDoParallel(cl)

resultado_rfe_treebag = rfe(x = x_train_prueba,
                            y = y_train_prueba,
                            sizes = c(1:20,40,60,80,100,150,200,250,300,350,400,500,600,700,800,900,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4500,5000,5500),
                            rfeControl = control_rfe_treebag)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(resultado_rfe_treebag, file = "rfe_treebag_9010_prop1")


#### Naive Bayes


control_rfe_nb = rfeControl(functions = nbFuncs,
                            method = "repeatedcv",
                            repeats = 2,
                            number = 5,
                            allowParallel = TRUE,
                            seeds = seeds)

set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(15)
registerDoParallel(cl)

resultado_rfe_nb = rfe(x = x_train_prueba,
                       y = y_train_prueba,
                       sizes = c(1:20,40,60,80,100,150,200,250,300,350,400,500,600,700,800,900,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4500,5000,5500),
                       rfeControl = control_rfe_nb)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(resultado_rfe_nb, file = "rfe_nb_9010_prop1")


#### Linear Discriminant Analysis


control_rfe_lda = rfeControl(functions = ldaFuncs,
                             method = "repeatedcv",
                             repeats = 2,
                             number = 5,
                             allowParallel = TRUE,
                             seeds = seeds)

set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(15)
registerDoParallel(cl)

resultado_rfe_lda = rfe(x = x_train_prueba,
                        y = y_train_prueba,
                        sizes = c(1:20,40,60,80,100,150,200,250,300,350,400,500,600,700,800,900,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4500,5000,5500),
                        rfeControl = control_rfe_lda)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(resultado_rfe_lda, file = "rfe_lda_9010_prop1")





