import pandas as pd
from pymongo import MongoClient
import dotenv
import os
import sys

here="./"
dotenv_path = os.path.join(here, ".env")
config = dotenv.dotenv_values(dotenv_path)




# Conexion a Mongo

mongodb = MongoClient(config["DB_URI"])
try:
    mongodb.server_info()
except Exception:
    print("No hay conexión a MongoDB")
    sys.exit()


db = mongodb.get_database("b") # Accedo a la database "b"
db.list_collection_names()

db_samples = db.get_collection("samples")
db_samples_variants = db.get_collection("samples_variants")
db_variants = db.get_collection("effects")

db_samples.find_one()
db_samples_variants.find_one({"variant":1261586})
db_variants.find_one({"id":1261586},{"SYMBOL":1})

##### Obtención de pacientes de cardio

muestras_iniciales = pd.read_csv("~/forgejo/clasificador-cohortes/samples.csv", sep="\t")

muestras_cardio = muestras_iniciales[muestras_iniciales["class"] == "cardio"]["sample"]

##### Obtencion de variantes del dataset de 6700 variantes

df_variantes = pd.read_csv("~/forgejo/clasificador-cohortes/dataset2_5pc.csv", sep="\t")

#### Obtencion de pacientes de cardio con subfenotipos

df_cardio_especifico = pd.read_excel("pac_cardio.xls")
ids_df_cardio_especifico = df_cardio_especifico["CÓGIDO DE ORIGEN Tecnalia Angel"]
ids_df_cardio_especifico.index = ids_df_cardio_especifico.index + 1

#### Conversión de IDs tipo "A303" a IDs tipo "S34535353"

df_tubos = pd.read_csv("ENVÍO EXOMAS_ASL_01122023.csv")

for sample in muestras_cardio:
    if sample.startswith("A"):
        id_sample = sample.replace("A","")
        print(sample, id_sample)
        muestras_cardio[muestras_cardio == sample] = df_tubos[df_tubos["CODIGO TUBO"] == int(id_sample)]["CÓGIDO DE ORIGEN "].to_list()[0]

muestras_cardio

df_tubos[["CODIGO TUBO","CÓGIDO DE ORIGEN "]]

df_tubos[df_tubos["CODIGO TUBO"] == 346]["CÓGIDO DE ORIGEN "].to_list()

#########
# Asociación de IDs de los pacientes entre diferentes registros

df_tubos[df_tubos["CODIGO TUBO"] == 9]["CÓGIDO DE ORIGEN "].to_list()[0]

dicc_muestra_fenotipo = {}
for sample in muestras_cardio:
    if str(sample).startswith("S"):
        id_original = int(str(sample).replace("S",""))
        id_sample = df_cardio_especifico[df_cardio_especifico["CÓGIDO DE ORIGEN Tecnalia Angel"] == id_original]["IDcorrelativo"]
    elif str(sample).startswith("A"):
        id_sample = int(str(sample).replace("A",""))

    fila_objetivo = df_cardio_especifico[df_cardio_especifico["IDcorrelativo"] == id_original]
    dicc_muestra_fenotipo[sample] = {"patologia_principal":fila_objetivo["Patologia Principal"].to_list()[0],
                                     "afectado":fila_objetivo["Afectado patologia principal"].to_list()[0],
                                     "patologia_secundaria":fila_objetivo["Patologia secundaria tx"].to_list()[0],
                                     "SnomedCT":fila_objetivo["SnomedCT"].to_list()[0],
                                     "etiqueta_1":fila_objetivo["etiqueta general_1"].to_list()[0],
                                     "etiqueta_2":fila_objetivo["etiqueta general_2"].to_list()[0]}
    
    

df_cardio_especifico.query("")

muestras_iniciales.to_dict()

dict_prueba = {}
dict_prueba["juan"] = {"enf":"malito","edad":15}
dict_prueba
df_cardio_especifico[df_cardio_especifico["IDcorrelativo"] == 1]["Patologia Principal"].to_list()[0]