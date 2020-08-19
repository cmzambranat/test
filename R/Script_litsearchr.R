#Establecer directorio de trabajo

#setwd("D:/Rwork")

#Alternativa para instalar "litsearchr"
#Se requiere instalar previamente "remotes" y "igraph"

library(remotes)
library(igraph)
library(litsearchr)
library(here)

#Enlance con el repositorio de github por medio de "remotes"


#Una vez ejecutado el comando anterior apararece un men?: seleccionar 1(All) en la consola y dar enter
#Puede aparecer un ERROR por la paqueter?a "backport". Se soluciona eliminando dicha paqueter?a


#Ejecutar comando para activar "litsearchr"



#Ecuaciones de b?squeda inicial para Web of Science (Se pueden utilizar otras bases de datos).
#En el caso de Web of Science descargar metadatos en formato ".bib"

#Malaria
#TS=(malaria AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Fiebre amarilla
#TS=("Yellow fever" OR " Yellow fever virus" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Dengue
#TS=(Dengue OR "Dengue virus" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Zika
#TS=(Zika OR "Zika virus" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Chikungunya
#TS=(Chikungunya OR "Chikungunya virus" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Mayaro
#TS=(Mayaro OR "Mayaro virus" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Leishmaniasis
#TS=(Leishmania* AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Chagas
#TS=(Chagas OR "Chagas disease" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Hantavirus
#TS=(Hantavirus AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Rabia
#TS=(Rabie OR "Rabies virus" OR Lyssavirus AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Ilheus virus
#TS=("Ilheus virus" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Rocio virus
#TS=("Rocio virus" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Oropouche virus
#TS=(Oropouche OR "Oropouche virus" AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Rickettsia
#TS=(Rickettsi* AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Leptospirosis
#TS=(Leptospir* AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)
#Toxoplasmosis
#TS=(Toxoplasm* AND Brazil OR Brasil AND deforestation AND Amazon* AND forest AND incidence AND prevalence)


#Flujo de trabajo por enfermedad

#malaria

#Importar metadatos de los art?culos

results <- litsearchr::import_results(file = c("malaria1.bib","malaria2.bib"))

#Remover art?culos duplicados con base en los t?tulos

rduplicates <- litsearchr::remove_duplicates(results, field="title", method ="exact")

#La funci?n "cleankeywords" se utiliza cuando las palabras clave no est?n separadas por el caracter ";".
#Si el archivo que se utiliza para importar los resultados tiene formato ".bib" no es necesario ejecutarla.
#cleankeywords <- litsearchr::clean_keywords()


#Identificar t?rminos que se repiten en el t?tulo y el abstract por medio del m?todo "fakerake"

fakerake <- litsearchr::extract_terms(text = paste(rduplicates$title,rduplicates$abstract),
                                      method = "fakerake",
                                      min_freq = 2,
                                      ngrams = T,
                                      min_n = 2,
                                      max_n = 5,
                                      stopwords = F,
                                      language = "English")

#Identificar t?rminos que se repiten en las keyword con el m?todo "tagged"

tagged <- litsearchr::extract_terms(keywords = rduplicates$keywords,
                                            method = "tagged",
                                            min_freq = 2,
                                            ngrams = T,
                                            min_n = 2,
                                            max_n = 5, 
                                            stopwords = NULL,
                                            language = "English")

#Uni?n de los t?rminos por ambos m?todos ("fakerake" y "tagged")

all_terms <- unique(append(tagged, fakerake))

#Generar matriz de presencia-ausencia con el objeto "all_terms"

dfmatrix <- litsearchr::create_dfm(elements = paste(rduplicates$title,rduplicates$abstract),
                                   features = all_terms)
#Crear red de todos los t?rminos

netw <- litsearchr::create_network(search_dfm = as.matrix(dfmatrix),
                                   min_studies = 3,
                                   min_occ = 3)

#Visualizar red

plot(netw)

#Evaluar la importancia de los t?rminos con base en las propiedades de la red.
#Ordenar los t?rminos con base en su importancia dada por la fuerza de interacci?n de los nodos (t?rminos)

importance_terms <- make_importance(netw, imp_method  = "strength")

#Recomendaci?n: Visualizar los t?rminos m?s importantes
#Corresponden a las ?ltimas filas del data "importance_terms"
#Aplicar "tail" para visualizar los 30 t?rminos m?s importantes de la red.

tail(importance_terms, n=30)

#Opcional (Exploratorio):
#Para identificar t?rminos que aparecen m?s de dos veces ("ngrams")
#Para identificar t?rminos que aparecen una sola vez ("unigrams")
#ngrams <- select_ngrams(netw, imp_method = "strength")
#unigrams <- select_unigrams(netw, imp_method = "strength")

#Identificar el punto de cambio de la fuerza de los t?rminos

findcutoff <- litsearchr::find_cutoff(netw, method = "cumulative",
                                      percent = 0.8,
                                      imp_method = "strength")

#Pedir el findcutoff (punto de cambio), este valor esta dado por la fuerza de interacci?n.

findcutoff

#Reducir la red a partir del "findcutoff"

reducegraph <- litsearchr::reduce_graph(graph = netw, cutoff_strength = 98, imp_method = "strength")

#Visualizar la red reducida

plot(reducegraph)

#Extraer los t?rminos de la red reducida
#Para tener un df con los t?rminos m?s importantes de la b?squeda inicial.
#Estos t?rminos son los m?s importantes por estar arriba del cuttoff (punto de cambio).

terms <- litsearchr::get_keywords(reducegraph)

#Guardar los t?rminos en un archivo con formato ".csv"

write.csv(terms, "terms_malaria.csv")

# Abrir el archivo csv

df <- read.csv("terms_malaria.csv", header = T)

# Renombrar las columnas del data frame

#Columna 1 (group)

colnames(df)[1] <- "group"

#Columna 2 (term)

colnames(df)[2] <- "term"

#Explorar los t?rminos para tenerlos de referencia.

#Hasta este momento tenemos informaci?n suficiente para elegir los t?rminos.
#Por practicidad, sugerimos utilizar los 30 t?rminos m?s importantes
#Y trabajar con ellos a partir de este punto.

#Retomar el objeto importance_terms

#Hacer un "tail" de los ?ltimos 30 t?rminos (los 30 t?rminos m?s importantes, seg?n la red)

tail(importance_terms, n=30)

#Generar data frame con los 30 t?rminos

dataterms <- tail(importance_terms, n = 30)

#Guardar archivo ".csv" con los 30 t?rminos m?s importantes

write.csv(dataterms, "dataterms.csv")

#IMPORTANTE:
#En estos 30 t?rminos no apareci? ninguno relacionado con la deforestaci?n
#Buscamos en el dataframe de "importance_terms"
#Un t?rmino relacionado con la deforestaci?n arriba del punto de corte.
#Para esto exploramos el archivo "terms_malaria.csv" que contiene todos los t?rminos arriba del cuttoff.

filter(importance_terms, nodename == "deforested areas")

#Abrimos el archivo "dataterms.csv" con el t?rmino que se agreg? y la agrupaci?n por categor?a.
#La agrupaci?n se realiz? por medio del m?todo PECO.
#Para m?s informaci?n del m?todo PECO Consultar el art?culo de Eliza Grames.
#Population, Exposition, Control (No aplica en nuestro caso) y Result

df.dataterms <- read.csv("dataterms.csv", header = T)

#Aplicamos un "grep" a los t?rminos que se consideraron "population"

population_terms <- df.dataterms$term[grep("population", df.dataterms$group)]

#Visualizamos los t?rminos "population"

population_terms

#Aplicamos un "grep" a los t?rminos que se consideraron "exposition"
#En este ejemplo el t?rmino que se agrup? en esta categor?a fue "deforested areas".
#Este t?rmino fue el que se agreg? a la lista de los 30 t?rminos m?s importantes.

exposition_terms <- df.dataterms$term[grep("exposition", df.dataterms$group)]

#Visualizamos los t?rminos "exposition"

exposition_terms

#Aplicamos un "grep" a los t?rminos que se consideraron "result"

result_terms <- df.dataterms$term[grep("result", df.dataterms$group)]

#Visualizamos los t?rminos "result"

result_terms

#Unimos todas las categor?as

population_unique <- unique(append(c("population"), population_terms))

population_unique


exposition_unique <- unique(append(c("exposition"), exposition_terms))

exposition_unique


result_unique <- unique(append(c("result"), result_terms))

result_unique

#Enlistamos las tres categor?as para poder aplicar la funci?n que da la b?squeda sugerida por "litsearchr"

mysearchterms <- list(population_unique, exposition_unique,result_unique)

#Con la funci?n "write_search" obtenemos la ecuaci?n de b?squeda sugerida por "litsearchr" 

systemathic_search <- litsearchr::write_search(groupdata = mysearchterms,
                                               languages = "English",
                                               stemming = T,
                                               closure = "none",
                                               exactphrase = T,
                                               writesearch = F,
                                               verbose = T)

#Guardar la ecuaci?n de b?squeda sugerida por "litsearchr" en un archivo con formato ".txt"

write.table(systemathic_search, "systemathic_search_malaria.txt")

#Con esto terminanos el work flow de "litsearchr" para realizar b?squedas sistem?ticas.
#Actualizado al 18/08/2020
#IMPORTANTE:
#Cuando abran el archivo ".txt" solo copiar a partir del par?ntesis
#y no olvidar borrar las categor?as PECO para buscar en la Web of Science u otra base de datos.

