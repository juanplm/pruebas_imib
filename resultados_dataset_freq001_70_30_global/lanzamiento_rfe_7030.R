library(caret)
library(randomForest)
library(doParallel)
library(dplyr)

variantes_1pc = read.csv("~/scratch/Documentacion-TFM/dataset2_5pc_modificado.csv")
rownames(variantes_1pc) = variantes_1pc[,1]
variantes_1pc = variantes_1pc[,-1]
variantes_1pc$grupo = as.factor(variantes_1pc$grupo)

semillas = c(1234,1515,5678)
rfe_alg = c("tbag","rf","lda","nb")

for (s in semillas){
	for (alg in rfe_alg){
		
		set.seed(s)

		particion = createDataPartition(variantes_1pc$grupo, p = .70, list = FALSE)

		train_prueba = variantes_1pc[particion, ]
		test_prueba = variantes_1pc[-particion, ]

		x_train_prueba = train_prueba[, -1]
		y_train_prueba = train_prueba[, 1]


		set.seed(s)
		seeds = list(
		   sample.int(1000,49),	
		   sample.int(1000,49),
		   sample.int(1000,49),
		   sample.int(1000,49),
		   sample.int(1000,49),
		   sample.int(1000,49),	
		   sample.int(1000,49),
		   sample.int(1000,49),
		   sample.int(1000,49),
		   sample.int(1000,49),
		   sample.int(1000,1)
		)
		
		control_rfe_rf = rfeControl(functions = rfFuncs,
                            method = "repeatedcv",
                            repeats = 2,
                            number = 5,
                            allowParallel = TRUE,
			    seeds = seeds,
			    returnResamp = "all")


		control_rfe_tbag = rfeControl(functions = treebagFuncs,
		                         method = "repeatedcv",
		                         repeats = 2,
		                         number = 5,
		                         allowParallel = TRUE,
					 seeds=seeds,
					 returnResamp = "all")

		control_rfe_nb = rfeControl(functions = nbFuncs,
		                            method = "repeatedcv",
		                            repeats = 2,
		                            number = 5,
		                            allowParallel = TRUE,
					    seeds=seeds,
					    returnResamp = "all")


		control_rfe_lda = rfeControl(functions = ldaFuncs,
		                             method = "repeatedcv",
		                             repeats = 2,
		                             number = 5,
		                             allowParallel = TRUE,
					     seeds=seeds,
					     returnResamp = "all")

		
		if (alg == "tbag"){
			control_rfe = control_rfe_tbag
			nucleos = 5	
		}

		if (alg == "rf"){
			control_rfe = control_rfe_rf
			nucleos = 10	
		}

		if (alg == "lda"){
			control_rfe = control_rfe_lda
			nucleos = 10	
		}

		if (alg == "nb"){
			control_rfe = control_rfe_nb
			nucleos = 10	
		}
		
		set.seed(s)

		tiempo = Sys.time()
		print(paste0("Comienzo RFE: ", alg, " - semilla: ", s))

		cl = makeCluster(nucleos)
		registerDoParallel(cl)

		resultado_rfe_rf = rfe(x = x_train_prueba,
		                       y = y_train_prueba,
		                       sizes = c(1:20,40,60,80,100,150,200,250,300,350,400,500,600,700,800,900,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000),
		                       rfeControl = control_rfe)

		tiempo.fin = Sys.time() - tiempo
		print(paste0("Finalizado, tiempo: ", tiempo.fin))

		stopCluster(cl)

		saveRDS(resultado_rfe_rf, file = paste0("resultado_rfe_",alg,"_7030_",s))




	}
}

print("Finalizado proceso de RFE con todas las semillas y algoritmos")
