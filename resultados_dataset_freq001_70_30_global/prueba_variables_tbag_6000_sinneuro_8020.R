## -----------------------------------------------------------------------------
library(caret)
library(randomForest)
library(doParallel)
library(dplyr)


variantes_5pc = read.csv("~/scratch/Documentacion-TFM/dataset2_5pc_modificado.csv")
rownames(variantes_5pc) = variantes_5pc[,1]
variantes_5pc = variantes_5pc[,-1]
variantes_5pc$grupo = as.factor(variantes_5pc$grupo)


## -----------------------------------------------------------------------------
semillas = c(1234,1515,5678)
alg_rfe = c("tbag","rf","lda","nb")

for (s in semillas){

	set.seed(s)

	seeds = list(
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,200),
	 sample.int(1000,1)
	)
	
	control.cv.10 = trainControl(method = "repeatedcv", 
                                  number = 5,
                                  repeats = 2,
                                  seeds = seeds, 
                                  returnResamp = "all",
                                  verboseIter = FALSE,
                                  allowParallel = TRUE,
				  classProbs = TRUE)
	

	for (alg in alg_rfe){
		archivo_rfe = paste0("resultado_rfe_",alg,"_7030_",s)
		resultado_rfe = readRDS(archivo_rfe)	
		
		variantes_5pc_reducido = variantes_5pc[,c(predictors(resultado_rfe),"grupo")] 
		
		set.seed(s)
		
		particion = createDataPartition(variantes_5pc_reducido$grupo, p=0.70, list = FALSE)
		train_data = variantes_5pc_reducido[particion,]
		test_data = variantes_5pc_reducido[-particion,]		

				## -----------------------------------------------------------------------------
		set.seed(s)
		
		print(paste0("Comienzo algoritmo SVML de ", alg, " y semilla ",s))
		tiempo = Sys.time()

		cl = makeCluster(25)
		registerDoParallel(cl)

		param.lineal = expand.grid(cost = c(0.001, 0.01, 0.1, 10, 100))

		training.svm.lineal = train(grupo ~ ., data = train_data,
		                      method = "svmLinear2",
		                      tuneGrid = param.lineal,
		                      metric = "Accuracy",
		                      preProcess = c("center", "scale"),
		                      trControl = control.cv.10)

		tiempo.fin = Sys.time() - tiempo
		print(tiempo.fin)

		stopCluster(cl)
		
		saveRDS(training.svm.lineal, paste0("modelo_", alg,"_svml_7030_",s))


		## -----------------------------------------------------------------------------
		set.seed(s)
		
		print(paste0("Comienzo algoritmo SVMR de ", alg, " y semilla ",s))
		tiempo = Sys.time()

		cl = makeCluster(25)
		registerDoParallel(cl)
		
		param.radial = expand.grid(sigma = c(0.001, 0.001, 0.01, 0.1, 0.5, 1, 2, 5),
					   C = c(0.01,0.1,1,5,10,20,50,100,200))		

		training.svm.radial = train(grupo ~ ., data = train_data,
		                      method = "svmRadial",
		                      tuneGrid = param.radial,
		                      metric = "Accuracy",
		                      preProcess = c("center", "scale"),
		                      trControl = control.cv.10)


		tiempo.fin = Sys.time() - tiempo
		print(tiempo.fin)


		stopCluster(cl)

		saveRDS(training.svm.radial, paste0("modelo_", alg,"_svmr_7030_",s))
		

		## -----------------------------------------------------------------------------
		set.seed(s)
		print(paste0("Comienzo algoritmo SVMP de ", alg, " y semilla ",s))
		tiempo = Sys.time()


		cl = makeCluster(25)
		registerDoParallel(cl)

		param.polinomial = expand.grid(C = c(0.01,0.05,0.1,0.2,0.5,1,5), degree = c(1,2,3), scale = c(0.01,0.1,1))

		training.svm.polinomial = train(grupo ~ ., data = train_data,
		                      method = "svmPoly",
		                      tuneGrid = param.polinomial,
		                      metric = "Accuracy",
		                      preProcess = c("center", "scale"),
		                      trControl = control.cv.10)


		tiempo.fin = Sys.time() - tiempo
		print(tiempo.fin)


		stopCluster(cl)

		saveRDS(training.svm.polinomial, paste0("modelo_", alg,"_svmp_7030_",s))


		## -----------------------------------------------------------------------------
		raiz_variables = round(sqrt(ncol(train_data)))

		tunegrid.rf.1 = expand.grid(mtry = c(round(raiz_variables*0.1),round(raiz_variables*0.3),round(raiz_variables*0.5),round(raiz_variables*0.7),raiz_variables,round(raiz_variables*1.2),round(raiz_variables*1.5)),
		                            min.node.size = c(1,2,3,4,5),
		                            splitrule = c("gini","extratrees","hellinger"))



		set.seed(s)
		print(paste0("Comienzo algoritmo RF de ", alg, " y semilla ",s))
		tiempo = Sys.time()

		cl = makeCluster(25)
		registerDoParallel(cl)

		training.rf.1 = train(grupo ~ ., data = train_data,
		                      method = "ranger",
		                      tuneGrid = tunegrid.rf.1,
		                      metric = "Accuracy",
		                      preProcess = c("center", "scale"),
		                      trControl = control.cv.10)

		tiempo.fin = Sys.time() - tiempo
		print(tiempo.fin)

		stopCluster(cl)

		saveRDS(training.rf.1, paste0("modelo_", alg,"_rf_7030_",s))


		## -----------------------------------------------------------------------------
		grid.diab.1 = expand.grid(laplace = 0:6,
		                       usekernel = c(TRUE,FALSE),
		                       adjust = 1:6)
		
		set.seed(s)
		
		print(paste0("Comienzo algoritmo NB de ", alg, " y semilla ",s))
		tiempo = Sys.time()
		
		cl = makeCluster(25)
		registerDoParallel(cl)

		training.nb <- train(grupo ~ ., data = train_data, 
		                 method = "naive_bayes",
		                 trControl = control.cv.10,
		                 preProcess = c("center", "scale"),
		                 tuneGrid=grid.diab.1)

		stopCluster(cl)

		saveRDS(training.nb, paste0("modelo_", alg,"_nb_7030_",s))
		tiempo.fin = Sys.time() - tiempo
		print(tiempo.fin)
				
	
	}
}





