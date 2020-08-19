#Establecer directorio de trabajo

setwd("D:/Rwork")

#Alternativa para instalar "litsearchr"
#Se requiere instalar previamente "remotes" y "igraph"

library(remotes)
library(igraph)

#Enlance con el repositorio de github por medio de "remotes"

remotes::install_github("luketudge/litsearchr", ref="patch-1")

#Una vez ejecutado el comando anterior apararece un menú: seleccionar 1(All) en la consola y dar enter
#Puede aparecer un ERROR por la paquetería "backport". Se soluciona eliminando dicha paquetería


#Ejecutar comando para activar "litsearchr"

library(litsearchr)

#Ecuaciones de búsqueda inicial para Web of Science (Se pueden utilizar otras bases de datos).
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

#Importar metadatos de los artículos

results <- litsearchr::import_results(file = c("malaria1.bib","malaria2.bib"))

#Remover artículos duplicados con base en los títulos

rduplicates <- litsearchr::remove_duplicates(results, field="title", method ="exact")

#La función "cleankeywords" se utiliza cuando las palabras clave no están separadas por el caracter ";".
#Si el archivo que se utiliza para importar los resultados tiene formato ".bib" no es necesario ejecutarla.
#cleankeywords <- litsearchr::clean_keywords()


#Identificar términos que se repiten en el título y el abstract por medio del método "fakerake"

fakerake <- litsearchr::extract_terms(text = paste(rduplicates$title,rduplicates$abstract),
                                      method = "fakerake",
                                      min_freq = 2,
                                      ngrams = T,
                                      min_n = 2,
                                      max_n = 5,
                                      stopwords = F,
                                      language = "English")

#Identificar términos que se repiten en las keyword con el método "tagged"

tagged <- litsearchr::extract_terms(keywords = rduplicates$keywords,
                                            method = "tagged",
                                            min_freq = 2,
                                            ngrams = T,
                                            min_n = 2,
                                            max_n = 5, 
                                            stopwords = NULL,
                                            language = "English")

#Unión de los términos por ambos métodos ("fakerake" y "tagged")

all_terms <- unique(append(tagged, fakerake))

#Generar matriz de presencia-ausencia con el objeto "all_terms"

dfmatrix <- litsearchr::create_dfm(elements = paste(rduplicates$title,rduplicates$abstract),
                                   features = all_terms)
#Crear red de todos los términos

netw <- litsearchr::create_network(search_dfm = as.matrix(dfmatrix),
                                   min_studies = 3,
                                   min_occ = 3)

#Visualizar red

plot(netw)

#Evaluar la importancia de los términos con base en las propiedades de la red.
#Ordenar los términos con base en su importancia dada por la fuerza de interacción de los nodos (términos)

importance_terms <- make_importance(netw, imp_method  = "strength")

#Recomendación: Visualizar los términos más importantes
#Corresponden a las últimas filas del data "importance_terms"
#Aplicar "tail" para visualizar los 30 términos más importantes de la red.

tail(importance_terms, n=30)

#Opcional (Exploratorio):
#Para identificar términos que aparecen más de dos veces ("ngrams")
#Para identificar términos que aparecen una sola vez ("unigrams")
#ngrams <- select_ngrams(netw, imp_method = "strength")
#unigrams <- select_unigrams(netw, imp_method = "strength")

#Identificar el punto de cambio de la fuerza de los términos

findcutoff <- litsearchr::find_cutoff(netw, method = "cumulative",
                                      percent = 0.8,
                                      imp_method = "strength")

#Pedir el findcutoff (punto de cambio), este valor esta dado por la fuerza de interacción.

findcutoff

#Reducir la red a partir del "findcutoff"

reducegraph <- litsearchr::reduce_graph(graph = netw, cutoff_strength = 98, imp_method = "strength")

#Visualizar la red reducida

plot(reducegraph)

#Extraer los términos de la red reducida
#Para tener un df con los términos más importantes de la búsqueda inicial.
#Estos términos son los más importantes por estar arriba del cuttoff (punto de cambio).

terms <- litsearchr::get_keywords(reducegraph)

#Guardar los términos en un archivo con formato ".csv"

write.csv(terms, "terms_malaria.csv")

# Abrir el archivo csv

df <- read.csv("terms_malaria.csv", header = T)

# Renombrar las columnas del data frame

#Columna 1 (group)

colnames(df)[1] <- "group"

#Columna 2 (term)

colnames(df)[2] <- "term"

#Explorar los términos para tenerlos de referencia.

#Hasta este momento tenemos información suficiente para elegir los términos.
#Por practicidad, sugerimos utilizar los 30 términos más importantes
#Y trabajar con ellos a partir de este punto.

#Retomar el objeto importance_terms

#Hacer un "tail" de los últimos 30 términos (los 30 términos más importantes, según la red)

tail(importance_terms, n=30)

#Generar data frame con los 30 términos

dataterms <- tail(importance_terms, n = 30)

#Guardar archivo ".csv" con los 30 términos más importantes

write.csv(dataterms, "dataterms.csv")

#IMPORTANTE:
#En estos 30 términos no apareció ninguno relacionado con la deforestación
#Buscamos en el dataframe de "importance_terms"
#Un término relacionado con la deforestación arriba del punto de corte.
#Para esto exploramos el archivo "terms_malaria.csv" que contiene todos los términos arriba del cuttoff.

filter(importance_terms, nodename == "deforested areas")

#Abrimos el archivo "dataterms.csv" con el término que se agregó y la agrupación por categoría.
#La agrupación se realizó por medio del método PECO.
#Para más información del método PECO Consultar el artículo de Eliza Grames.
#Population, Exposition, Control (No aplica en nuestro caso) y Result

df.dataterms <- read.csv("dataterms.csv", header = T)

#Aplicamos un "grep" a los términos que se consideraron "population"

population_terms <- df.dataterms$term[grep("population", df.dataterms$group)]

#Visualizamos los términos "population"

population_terms

#Aplicamos un "grep" a los términos que se consideraron "exposition"
#En este ejemplo el término que se agrupó en esta categoría fue "deforested areas".
#Este término fue el que se agregó a la lista de los 30 términos más importantes.

exposition_terms <- df.dataterms$term[grep("exposition", df.dataterms$group)]

#Visualizamos los términos "exposition"

exposition_terms

#Aplicamos un "grep" a los términos que se consideraron "result"

result_terms <- df.dataterms$term[grep("result", df.dataterms$group)]

#Visualizamos los términos "result"

result_terms

#Unimos todas las categorías

population_unique <- unique(append(c("population"), population_terms))

population_unique


exposition_unique <- unique(append(c("exposition"), exposition_terms))

exposition_unique


result_unique <- unique(append(c("result"), result_terms))

result_unique

#Enlistamos las tres categorías para poder aplicar la función que da la búsqueda sugerida por "litsearchr"

mysearchterms <- list(population_unique, exposition_unique,result_unique)

#Con la función "write_search" obtenemos la ecuación de búsqueda sugerida por "litsearchr" 

systemathic_search <- litsearchr::write_search(groupdata = mysearchterms,
                                               languages = "English",
                                               stemming = T,
                                               closure = "none",
                                               exactphrase = T,
                                               writesearch = F,
                                               verbose = T)

#Guardar la ecuación de búsqueda sugerida por "litsearchr" en un archivo con formato ".txt"

write.table(systemathic_search, "systemathic_search_malaria.txt")

#Con esto terminanos el work flow de "litsearchr" para realizar búsquedas sistemáticas.
#Actualizado al 18/08/2020
#IMPORTANTE:
#Cuando abran el archivo ".txt" solo copiar a partir del paréntesis
#y no olvidar borrar las categorías PECO para buscar en la Web of Science u otra base de datos.

