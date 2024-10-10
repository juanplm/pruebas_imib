
from clases_jp import Variante_anotacion
import pandas as pd

import requests
import sys

import gprofiler 




# Anotación de variantes con BBDD en Mongo

dicc_variantes = {}

var1 = Variante_anotacion("var1")
var1.impact = "moderado"
var1.clin_sig = "mala"
var1.symbol = "gen1"
var1.existing_variant = "no"
var1.sift = "sift1"

var_dicc1 = {"_id":"var1", "impact":"moderado","sift":"sift1","symbol":"gen1"}
var_dicc2 = {"_id":"var2", "impact":"moderado","sift":"sift2","symbol":"gen2"}
var_dicc3 = {"_id":"var3", "impact":"moderado","sift":"sift3","symbol":"gen3"}
var_dicc4 = {"_id":"var4", "impact":"moderado","sift":"sift4","symbol":"gen4"}

dicc_variantes[var1._id] = var1

var2 = Variante_anotacion("var2")
var2.impact = "fuerte"
var2.clin_sig = "buena"
var2.symbol = "gen2"
var2.existing_variant = "si"
var2.sift = "sift2"

dicc_variantes[var2._id] = var2

var3 = Variante_anotacion("var3")
var3.impact = "debil"
var3.clin_sig = "mala"
var3.symbol = "gen3"
var3.existing_variant = "no"
var3.sift = "sift3"

dicc_variantes[var3._id] = var3

lista_dics = [var_dicc1, var_dicc2, var_dicc3, var_dicc4]

df_diccs = pd.DataFrame.from_records(lista_dics)
df_diccs.to_csv("csv_diccs.csv", index=False)




# Obtencion genes usados en el modelo final 

df_variants = pd.read_csv("dataset2_5pc.csv", sep="\t")

genes_completos = list(df_variants.columns)
genes_completos = genes_completos[1:]

gene_names = []

for gen in genes_completos:
    gen_partido = gen.split("_")
    gen_name = gen_partido[0].upper()
    gene_names.append(gen_name)

len(gene_names)

gene_names = list(set(gene_names))

len(gene_names)
    
for gen in gene_names:
    print(gen, end=" ")



# Anotacion con G:Profiler con HPO

r = requests.post(
    url='https://biit.cs.ut.ee/gprofiler/api/gost/profile/',
    json={
        'organism':'hsapiens',
        'query':gene_names,
        'sources':["HP"]
    }
    )
r.json()['result']

r = requests.post(
    url='https://biit.cs.ut.ee/gprofiler/api/gost/profile/',
    json={
    'organism':'hsapiens',
    'query':["CASQ2", "CASQ1", "GSTO1", "DMD", "GSTM2"],
    'sources' :['HP'], #only look into Gene Ontology terms.
    'user_threshold':1e-8, #reduce the significance threshold,
    'significance_threshold_method':'bonferroni', #use bonferroni correction instrad of the default 'g_SCS'.
    'no_evidences':True, #skip lookup for evidence codes. Speeds up queries, if there is no interest in evidence codes.
    'no_iea':True, #Ignore electonically annotated GO annotations

    'domain_scope':'custom',#use the genes in the probe as the statistical background.
    'background':'AFFY_HG_U133A'
    },
    headers={
    'User-Agent':'FullPythonRequest'
    }
)

r.json()['result']