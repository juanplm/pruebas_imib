library(caret)
library(randomForest)
library(dplyr)


df_vars = read.csv("~/scratch/Documentacion-TFM/dataset2_5pc_modificado.csv")
rownames(df_vars) = df_vars[,1]
df_vars = df_vars[,-1]

dir_act = getwd()

listado_rds = basename(list.files(path = dir_act, pattern = "^modelo_", full.names = TRUE))

part_datos = 0.7


columnas = c("Metodo","Acc_train", "Kappa_train", "Acc_test", "Kappa_test", "Sens", "Spec", "Precis","F1", "Hiper_comb", "Partic", "RFE", "Vars","Semilla")

df_resultados = data.frame(matrix(nrow = 0, ncol = length(columnas)))
colnames(df_resultados) = columnas


for (modelo in listado_rds){

	if (grepl("1234",modelo) == TRUE){
		semilla = 1234
	}

	if (grepl("1515",modelo) == TRUE){
		semilla = 1515
	}

	if (grepl("5678",modelo) == TRUE){
		semilla = 5678
	}

	set.seed(semilla)

	particion = createDataPartition(df_vars$grupo, p=part_datos, list = FALSE)

	train_data = df_vars[particion,]
	test_data = df_vars[-particion,]


	mod = readRDS(modelo)
	acc_training = mod$results[rownames(mod$results) == rownames(mod$bestTune), ]["Accuracy"][[1]]
	kappa_training = mod$results[rownames(mod$results) == rownames(mod$bestTune), ]["Kappa"][[1]]
	num_vars = length(colnames(mod$trainingData[,-1]))
	comb_hiperpar = paste(names(mod$bestTune), mod$bestTune, sep = "=", collapse = ", ")
	
	metodo = mod$method
	
	df_bucle = df_vars[,c(colnames(mod$trainingData[,-1]), "grupo")]
	vars = length(colnames(df_bucle))	

	train_bucle = train_data[,colnames(df_bucle)]
	test_bucle = test_data[,colnames(df_bucle)]
	
	rfe_mod = sub("modelo_(.*?)_.*", "\\1", modelo)	
	
	test_bucle$grupo = as.factor(test_bucle$grupo)

	set.seed(semilla)
	prediccion = predict(mod, newdata = test_bucle)
	matriz_conf = confusionMatrix(prediccion, test_bucle$grupo)
		
	acc_test = matriz_conf$overall["Accuracy"][[1]]
	kappa_test = matriz_conf$overall["Kappa"][[1]]
	sensib = matriz_conf$byClass["Sensitivity"][[1]]
	spec = matriz_conf$byClass["Specificity"][[1]]
	precis = matriz_conf$byClass["Precision"][[1]]
	f1 = matriz_conf$byClass["F1"][[1]]
	
	nueva_fila = data.frame(Metodo = metodo,
			        Acc_train = acc_training, 
			        Kappa_train = kappa_training, 
			        Acc_test = acc_test,
			        Kappa_test = kappa_test,
			        Sens = sensib, 
			        Spec = spec, 
			        Precis = precis,
			        F1 = f1, 
			        Hiper_comb = comb_hiperpar, 
			        Partic = part_datos,
			        RFE = rfe_mod,
				Vars = vars,
				Semilla = semilla)

	df_resultados = rbind(df_resultados, nueva_fila)

}	

write.csv(df_resultados, paste0("Resultados_", part_datos,"_totales.csv"))

