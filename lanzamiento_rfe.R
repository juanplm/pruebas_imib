library(caret)
library(randomForest)
library(doParallel)
library(dplyr)

setwd("C:/Users/Juanpepito/Escritorio/Trabajo/Oferta IMIB Angel/Pruebas clasificador local/")
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

# Mantengo únicamente la fila asociada al fenotipo de interés
df_hpo_intersec = df_hpo[df_hpo$name == "Abnormality of the cardiovascular system",] 

# Separo los genes que hacen intersección
df_hpo_genes = strsplit(df_hpo_intersec$intersections, split = ",")


# Para tener los nombres de los genes en limpio, creo una nueva lista e introduzco los nombres de los genes ya limpios (sin comillas o simbolos)
genes_hpo = c()


# Para cada gen de la lista original, aplico un procesado mediante el cual obtengo finalmente el SYMBOL de cada gen unicamente
for (i in 1:(length(df_hpo_genes[[1]]))){
  gen = gsub("\\]","",gsub("\\'","",gsub("\\[", "", noquote(df_hpo_genes[[1]][i]))))
  gen = gsub(" ","",gen)
  genes_hpo = c(genes_hpo, gen)
}

# Lo paso a minúsculas para mas tarde comparar con el nombre de las variantes de los dfs originales
genes_hpo = tolower(genes_hpo)

variantes_def = c()

# Para cada nombre de variante del df original, me quedo con el symbol de la variante, y compruebo si dicho symbol coincide con alguno
# de los nombres de genes que he extraído anteriormente del df de anotación de HPO. Los casos positivos, guardo el nombre de variante completo
for (i in 1:length(colnames(variantes_1pc[,-1]))){
  for (y in genes_hpo){
    if (y %in% strsplit(colnames(variantes_1pc[i]), split="_")[[1]][1]){
      variantes_def = c(variantes_def,colnames(variantes_1pc[i]))
    }
  }
  
}


# Elimino posibles duplicados de variantes
variantes_def = unique(variantes_def)


# Creo un nuevo df únicamente con las variantes de interés, las ubicadas en genes anotados en HPO con fenotipo = anormalidad del SCV
df_modificado = variantes_1pc[,variantes_def]

df_modificado$grupo = categoria_muestras[categoria_muestras$class != "neuro_muscular",]$class
df_modificado$grupo = as.factor(df_modificado$grupo)

df_modificado = df_modificado %>%
  select(last_col(), everything())


set.seed(1234)

particion = createDataPartition(df_modificado$grupo, p = .80, list = FALSE)

train_prueba = df_modificado[particion, ]
test_prueba = df_modificado[-particion, ]

x_train_prueba = train_prueba[, -1]
y_train_prueba = train_prueba[, 1]

control_rfe_rf = rfeControl(functions = rfFuncs,
                            method = "repeatedcv",
                            repeats = 2,
                            number = 5,
                            allowParallel = TRUE)

set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(4)
registerDoParallel(cl)

resultado_rfe_rf = rfe(x = x_train_prueba,
                       y = y_train_prueba,
                       sizes = c(20,40,60,80,100,150,200,250,300,350,400,450,500,550,600,700,800,900,1000),
                       rfeControl = control_rfe_rf)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(resultado_rfe_rf, file = "resultado_rfe_rf_6000_pruebajp")


control_rfe_treebag = rfeControl(functions = treebagFuncs,
                                 method = "repeatedcv",
                                 repeats = 1,
                                 number = 5,
                                 allowParallel = TRUE)


set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(4)
registerDoParallel(cl)

resultado_rfe_treebag = rfe(x = x_train_prueba,
                            y = y_train_prueba,
                            sizes = c(300,400,500,550,600,700,800,900,1000),
                            rfeControl = control_rfe_treebag)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(resultado_rfe_treebag, file = "resultado_rfe_treebag_6000_pruebajp")

control_rfe_nb = rfeControl(functions = nbFuncs,
                            method = "repeatedcv",
                            repeats = 1,
                            number = 5,
                            allowParallel = TRUE)

set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(4)
registerDoParallel(cl)

resultado_rfe_nb = rfe(x = x_train_prueba,
                       y = y_train_prueba,
                       sizes = c(300,400,500,550,600,700,800,900,1000),
                       rfeControl = control_rfe_nb)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)

stopCluster(cl)

saveRDS(resultado_rfe_nb, file = "resultado_rfe_nb_6000_pruebajp")

control_rfe_lda = rfeControl(functions = ldaFuncs,
                             method = "repeatedcv",
                             repeats = 1,
                             number = 5,
                             allowParallel = TRUE)

set.seed(1234)

tiempo = Sys.time()

cl = makeCluster(4)
registerDoParallel(cl)

resultado_rfe_lda = rfe(x = x_train_prueba,
                        y = y_train_prueba,
                        sizes = c(300,400,500,550,600,700,800,900,1000),
                        rfeControl = control_rfe_lda)

tiempo.fin = Sys.time() - tiempo
print(tiempo.fin)
stopCluster(cl)

saveRDS(resultado_rfe_lda, file = "resultado_rfe_lda_6000_pruebajp")

