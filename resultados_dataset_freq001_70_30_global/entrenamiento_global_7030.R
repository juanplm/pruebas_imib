## -----------------------------------------------------------------------------
library(caret)
library(randomForest)
library(doParallel)
library(dplyr)
library(modelgrid)

variantes_5pc = read.csv("~/scratch/Documentacion-TFM/dataset2_5pc_modificado.csv")
rownames(variantes_5pc) = variantes_5pc[,1]
variantes_5pc = variantes_5pc[,-1]
variantes_5pc$grupo = as.factor(variantes_5pc$grupo)


# Defino el grid de modelos

grid_modelos <- model_grid() 

grid_modelos <- grid_modelos %>%
			share_settings(
			metric = "Accuracy",
			trControl = trainControl(
				    	method = "repeatedcv",
					number = 5,
					repeats = 2,
					returnResamp = "all",
					verboseIter = FALSE,
					allowParallel = TRUE,
					classProbs = TRUE
				    )
		
			)

# Como lo que se va a cambiar en cada iteración es el número de predictores en función del resultado de RFE, ciertas características serán comunes en todos
# los casos, así que eso se puede definir ya fuera del propio bucle.

semillas = c(1234,1515,5678)
rfe_alg = c("tbag","rf","lda","nb")

for (s in semillas){
	for (alg in rfe_alg){
		
		archivo_rfe = paste0("resultado_rfe_",alg,"_7030_",s)
		resultado_rfe = readRDS(archivo_rfe)	
		
		variantes_5pc_reducido = variantes_5pc[,c(predictors(resultado_rfe),"grupo")] 

		set.seed(s)
		
		particion = createDataPartition(variantes_5pc_reducido$grupo, p=0.70, list = FALSE)
		train_data = variantes_5pc_reducido[particion,]
		test_data = variantes_5pc_reducido[-particion,]		
		
		raiz_variables = round(sqrt(ncol(train_data)))
		
		grid_modelos <- grid_modelos %>%
			add_model(
				y = train_data$grupo,
				x = train_data %>% select(-grupo),
				model_name = paste0("SVM Lineal ", alg, " ", s),
				method = "svmLinear2",
				tuneGrid = expand.grid(
					   	cost = c(0.001, 0.01, 0.1, 10, 100)	
					   )
				
			) %>% 
			add_model(
				y = train_data$grupo,
				x = train_data %>% select(-grupo),
				model_name = paste0("SVM Radial ", alg, " ", s),
				method = "svmRadial",
				tuneGrid = expand.grid(
					   	sigma = c(0.001, 0.001, 0.01, 0.1, 0.5, 1, 2, 5),
					   	C = c(0.01,0.1,1,5,10,20,50,100,200)	
					   )
			) %>%
			add_model(
				y = train_data$grupo,
				x = train_data %>% select(-grupo),
				model_name = paste0("SVM Polinomial ", alg, " ", s),
				method = "svmPoly",
				tuneGrid = expand.grid(
					   	C = c(0.01,0.05,0.1,0.2,0.5,1,5), 
						degree = c(1,2,3), 
						scale = c(0.01,0.1,1)
					   )
			) %>% 
			add_model(
				y = train_data$grupo,
				x = train_data %>% select(-grupo),
				model_name = paste0("Random Forest", alg, " ", s),
				method = "ranger",
				tuneGrid = expand.grid(
					    mtry = c(round(raiz_variables*0.1),round(raiz_variables*0.3),round(raiz_variables*0.5),round(raiz_variables*0.7),raiz_variables,round(raiz_variables*1.2),round(raiz_variables*1.5)),
		                            min.node.size = c(1,2,3,4,5),
		                            splitrule = c("gini","extratrees","hellinger")
					   )	
			) %>%
			add_model(
				y = train_data$grupo,
				x = train_data %>% select(-grupo),
				model_name = paste0("Naive Bayes", alg, " ", s),
				method = "naive_bayes",
				tuneGrid = expand.grid(
					   	laplace = 0:6,
		                       		usekernel = c(TRUE,FALSE),
		                       		adjust = 1:6
					   )
			)


		


		cl = makeCluster(15)
		registerDoParallel(cl)
		
		tiempo = Sys.time()
		print(paste0("Comienzo entrenamiento de los modelos de RFE: ", alg, " y semilla: ", s))
		
		grid_modelos <- train(grid_modelos, resample_seed = s)
		
	}
}


saveRDS(grid_modelos, "grid_modelos_7030")
