#################################################################################
# Nombre del programa:	1_process_raw.R,                  
# Autor:              	Madeline Swarr 
# Ultima modificacion: 	12 febrero 2022
#       
# Descripcion: 
#	Este programa procesa un fichero de microdatos del INE (md_EPA_2021T1.txt...renombrado a datos_eni_07.txt)
#   a partir de un fichero de metadatos (dr_EPA_2021.xlsx...renombrado a disreg_eni07.xls) que contiene 
#   el diseno de registro del archivo de microdatos. 
#     EPA.: Operacion estadistica
#     2007: Ano(s) de produccion de los datos
#
#     Web: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177005&menu=resultados&idp=1254735573002#!tabs-1254736195389
#
# Entrada:          
#     - Diseno de registro: 	dr_EPA_2021.xlsx...renombrado a datos_eni_07.txt
#     - Archivo de microdatos: 	md_EPA_2021T1.txt...renombrado a disreg_eni07.xls
# Salida: 
#     - Archivo de microdatos en formato data.frame de R: 
#       - all_data (todo)
#       - this_data (filtrado y procesado)
#       
#################################################################################

########### ESPECIFICAMOS EL DIRECTORIO DEL CODIGO ################
head.dir <- head_dir

# Se necesita leer los datos crudos?
readin.raw <- readin_raw

# Descargar libraries 
dependency.dir <- paste0(head.dir, "/Pipeline/dependencies")
key.dir <- paste0(head.dir, "/Pipeline/keys")
source(paste0(dependency.dir, "/get_packages.R"))
get_packages(c("XLConnect", "plyr", "tidyverse", "reshape2", "utile.tools", "hash", "translateR"))

####################    Asignacion de parametros    #######################
fichero_micro <- "datos_eni_07.txt"
fichero_meta  <- "disreg_eni07.xls"
fichero_pib <- "GDPpc_timeseries_worldbank.xls"
fichero_key <- "medeg-thesis-translator-d96fe6bca7c2.json"

#Recogemos la ruta del script que se esta ejecutando
input.dir <- paste0(head.dir, "/Data")
codebook.dir <- paste0(input.dir, "/", fichero_meta)
gdp.dir <- paste0(input.dir, "/", fichero_pib)
output.dir <- paste0(head.dir, "/Output/1_processed")
if (!dir.exists(output.dir)){
  dir.create(output.dir)
}

####################     LEER LOS DATOS CRUDOS     #########################

#Lectura del fichero de metadatos (METAD), Hoja "Diseno" de archivo .xlsx
tryCatch((workBook <- loadWorkbook(paste0(input.dir, "/", fichero_meta))), error=function(e) 
        stop(paste("Error. No se puede abrir el fichero: ", e, fichero_meta,". Saliendo de la ejecucion...", sep = "")))
codebook <- readNamedRegion(workBook, name = "PARAMETROS")

nombresVarbls <- codebook[,1]
descripVarbls <- codebook[,2]
posiciones    <- codebook[,5]
tipo          <- codebook[,4]
keepVarbls    <- codebook[,6] 
tamanio       <- length(nombresVarbls)

if(readin.raw == 1) {
  # Lectura del fichero de microdatos (MICROD), guardar todas como string
  tryCatch((all_data <- read.fortran(file = paste0(input.dir, "/", fichero_micro), format= paste0("A", codebook$Long..Campo))), error=function(e)
    stop(paste("Error. No se encuentra el fichero: ", e, fichero_micro,". Saliendo de la ejecucion...", sep = "")))

  # Capturamos las columnas con su tipo de dato
  for (i in 1:length(codebook$Tipo.variable)){
    este_tipo = codebook$Tipo.variable[i]
    if(identical(este_tipo, "AN")){
      all_data[,i] <- all_data[,i] %>% as.character()
    } else {
      if(identical(este_tipo, "N")){
        all_data[,i] <- all_data[,i] %>% as.numeric()
      }
    }
  }
  
  # Aplicamos los nombres de la cabecera del registro
  names(all_data) <- codebook[,1]
  comment(all_data) <- descripVarbls
  saveRDS(all_data, file = paste0(output.dir, "/datos_eni_07.rds"))
  
  } else {
  all_data <- readRDS(paste0(output.dir, "/datos_eni_07.rds"))
}

######################## FILTRAR Y ASIGNAR INFORMACION ###################

# Filtramos a las variables deseadas
this_data <- all_data[, which(keepVarbls == "Yes")]
names(this_data) <- codebook[,1][which(keepVarbls == "Yes")]
comment(this_data) <- descripVarbls[which(keepVarbls == "Yes")]

# Codificar por NAs
this_data <- this_data %>% mutate(EDAD01 = na_if(EDAD01, 999), 
                                  HORAS2 = na_if(HORAS2, 999),
                                  RECANT = na_if(RECANT, 99999),
                                  ALLE01 = na_if(ALLE01, 0),
                                  ANAC01 = na_if(ANAC01, 0),
                                  OCUP1 = ifelse(str_trim(OCUP1) == "", NA, str_trim(OCUP1)),
                                  OCUP2 = ifelse(str_trim(OCUP2) == "", NA, str_trim(OCUP2)),
                                  SECT1 = ifelse(str_trim(SECT1) == "", NA, str_trim(SECT1)),
                                  SECT2 = ifelse(str_trim(SECT2) == "", NA, str_trim(SECT2)),
                                  TPO1 = na_if(TPO1, 0),
                                  DOCUM = na_if(DOCUM, 0))

######### Hash info #############

## Recogemos los buscadores 
country_codebook <- readWorksheetFromFile(codebook.dir, sheet = 4)
province_codebook <- readWorksheetFromFile(codebook.dir, sheet = 2)
comunidad_codebook <- readWorksheetFromFile(codebook.dir, sheet = 3)
occupation_codebook <- readWorksheetFromFile(codebook.dir, sheet = 12) 
sector_codebook <- readWorksheetFromFile(codebook.dir, sheet = 11, startRow = 4) # Note: made edits to disreg_eni07.xls on "Sector de actividad" tab
# De pais de origen
hash_countries <- hash()
.set(hash_countries, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$LITERAL)
this_data$country <- values(hash_countries, keys = this_data$PNAC01)
clear(hash_countries)
# De pais de origen (shortened)
hash_countries_short <- hash()
.set(hash_countries_short, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$SHORT)
this_data$country_short <- values(hash_countries_short, keys = this_data$PNAC01)
clear(hash_countries_short)
# De provincia en Espana
hash_provinces <- hash()
.set(hash_provinces, keys = province_codebook$CODIGO_PROVINCIA, values = province_codebook$NOMBRE_PROVINCIA)
this_data$province <- values(hash_provinces, keys = this_data$CPRO)
this_data$province <- trimws(this_data$province)
clear(hash_provinces)
# De comunidad en Espana
hash_comunidad <- hash()
.set(hash_comunidad, keys = comunidad_codebook$COD_CCAA, values = comunidad_codebook$NOM_CCAA)
this_data$comunidad <- values(hash_comunidad, keys = this_data$CCAA)
this_data$comunidad <- trimws(this_data$comunidad)
clear(hash_comunidad)
# De ocupación
hash_occupation <- hash()
.set(hash_occupation, keys = occupation_codebook$Código, values = occupation_codebook$Ocupaciones)
this_data$occupation1[!is.na(this_data$OCUP1)] <- values(hash_occupation, keys = this_data$OCUP1[!is.na(this_data$OCUP1)])
this_data$occupation2[!is.na(this_data$OCUP2)] <- values(hash_occupation, keys = this_data$OCUP2[!is.na(this_data$OCUP2)])
clear(hash_occupation)
# De sector
hash_sector <- hash()
.set(hash_sector, keys = sector_codebook$CÓDIGOS.QUE.AGRUPA, values = sector_codebook$CLASIFICACIÓN.ACTIVIDADES)
this_data$sector1[!is.na(this_data$SECT1)] <- values(hash_sector, keys = this_data$SECT1[!is.na(this_data$SECT1)])
this_data$sector2[!is.na(this_data$SECT2)] <- values(hash_sector, keys = this_data$SECT2[!is.na(this_data$SECT2)])
clear(hash_sector)

## Capturar todas las formas de encontrar trabajo
form_data <- this_data %>% select(c(1, grep("FORM", colnames(this_data)))) %>% 
  melt(id.vars = "IDQ") %>% 
  mutate(FORM = ifelse(is.na(variable), NA, ifelse(value == 1, paste0(variable), ""))) %>%
  group_by(IDQ) %>% dplyr::summarize(all_forms = utile.tools::paste0(FORM, collapse = "", na.rm = T)) %>%
  mutate(all_forms = ifelse(all_forms == "", NA, all_forms)) %>%
  mutate(all_forms_any_friends = ifelse(grepl("FORM04", all_forms), "FORM04", all_forms))

## Collect data that indicates “economic immigrants”
# Immigration history indicating movements “por falta de empleo”, “para buscar un empleo mejor”, or “por cambio de destino laboral”
femp_data <- this_data %>% select(nombresVarbls[grepl("DSLA|FEMP|BEMP", nombresVarbls)]) %>% mutate(economic = 0)
# Last column with non-missing data represents decision to move to Spain...make this more efficient
for (row in 1:nrow(femp_data)) {
  this_row = femp_data[row,]
  for (col in 1:(ncol(femp_data)-1)) {
    this_col = ncol(femp_data) - col
    this_point = this_row[this_col]
    if (!is.na(this_point) & this_point == 1 & femp_data[row,]$economic == 0) {femp_data[row,]$economic = 1}
  }
}

# Countries that possessed 70% or more of Spanish GDP per capita in 2007 (World Bank data)
gdp_data <- readWorksheetFromFile(gdp.dir, sheet = 1, startRow = 5)
rich_countries <- gdp_data[which(gdp_data$X2007 >= 0.7*gdp_data[gdp_data$Country.Name=="Spain",]$X2007),]$Country.Name %>% as.data.frame()
paises_ricos <- translate(dataset = rich_countries,
                          content.field = '.', 
                          google.api.key = "d96fe6bca7c2bb5e53fb142c739f27a452b39e20", 
                          source.lang = 'en', 
                          target.lang = 'es')
paises_ricos <- c("Aruba", "Andorra","Emiratos Árabes Unidos","Australia","Austria","Bélgica","Las Bahamas","Bermudas","Brunei Darussalam","Canadá","Suiza","Islas Anglonormandas","Islas Caimán","Chipre","Alemania","Dinamarca","España","Finlandia","Francia","Islas Faroe","Reino Unido","Grecia","Groenlandia","Guam","RAE de Hong Kong", "China","Isla de Man","Irlanda","Islandia","Israel","Italia","Japón","República de Corea – Corea del Sur","Kuwait","Liechtenstein","Luxemburgo","Mónaco","Nueva Caledonia","Países Bajos","Noruega","Nueva Zelanda","Puerto Rico","Qatar","Singapur","San Marino","Eslovenia","Suecia","Islas Turcas y Caicos","Estados Unidos de América","Islas Vírgenes (EE.UU.)")

this_data <- this_data %>% mutate(#COVARIATES
                                  years = ifelse((2006 - ALLE01) > 0, 2006 - ALLE01, 0),
                                  age = EDAD01,
                                  age_arrival = this_data$ALLE01 - this_data$ANAC01,
                                  age_sq = EDAD01^2,
                                  sex = mapvalues(this_data$SEXO01, from = c(1, 2), to = c("Male", "Female")),
                                  education = this_data$MNIV,
                                  spanish = mapvalues(this_data$HAESP, from = c(1, 2, 3, 4), to = c("Muy bien", "Bien", "Suficiente", "Necesita mejorar")),
                                  work_permit = ifelse(DOCUM %in% c(1, 2, 3), 1, ifelse(is.na(DOCUM), NA, 0)),
                                  #POPULATION INDICATORS
                                  economic = femp_data$economic, 
                                  was_working = mapvalues(this_data$TRAB1, from = c(1, 6), to = c(1, 0)),
                                  had_worked = mapvalues(this_data$TRANT, from = c(1, 6), to = c(1, 0)),
                                  ethnic = ifelse(!country %in% these_rich_countries, 1, 0),
                                  #RESPONSE VARS
                                  month_earn = INGRE,
                                  log_month_earn = log(INGRE),
                                  month_hours = HORAS2*4,
                                  hour_earn = month_earn/month_hours,
                                  log_hour_earn = log(month_earn/month_hours),
                                  lt1mo_search = ifelse(TPO1 <= 2, 1, 0),
                                  #SOCIAL VARS
                                  influenced = mapvalues(this_data$INFLU, from = c(1, 6), to = c(1, 0)),
                                  networked = mapvalues(this_data$FORM04, from = c(1, 6), to = c(1, 0)),
                                  first_job_still = mapvalues(this_data$PRIMTR, from = c(1, 6), to = c(1, 0)),
                                  insecurity = mapvalues(this_data$BUSEMP, from = c(1, 6), to = c(1, 0)),
                                  precontract = mapvalues(this_data$PROPTR, from = c(1, 6, 0), to = c(1, 0, NA)),
                                  form = form_data$all_forms,
                                  form_any_friends = form_data$all_forms_any_friends
                                  )


######## Guardamos ##########
saveRDS(this_data, file = paste0(output.dir, "/datos_eni_07_subset.rds"))

## Liberacion de memoria y aclaracion de variables 
# Values
rm(workBook, nombresVarbls, posiciones, tamanio, codebook, all_data, femp_data, form_data, gdp_data, country_codebook, comunidad_codebook, occupation_codebook, province_codebook, sector_codebook)




######## SOCIAL CAPITAL INDEX EXPLORATION ############
# Cuenta de "social ties" tras llegar en España
this_data$LLCONT <- mapvalues(this_data$LLCONT, from = c(1, 6), to = c(1, 0))
this_data$LLFAM <- mapvalues(this_data$LLFAM, from = c(1, 6), to = c(1, 0))
this_data$LLAMIG <- mapvalues(this_data$LLAMIG, from = c(1, 6), to = c(1, 0))
this_data$LLEMPR <- mapvalues(this_data$LLEMPR, from = c(1, 6), to = c(1, 0))
this_data$LLINTR <- mapvalues(this_data$LLINTR, from = c(1, 6), to = c(1, 0))
this_data$LLCONO <- mapvalues(this_data$LLCONO, from = c(1, 6), to = c(1, 0))
this_data$LLOTR <- mapvalues(this_data$LLOTR, from = c(1, 6), to = c(1, 0))
this_data$DINFAM <- mapvalues(this_data$DINFAM, from = c(1, 6), to = c(1, 0))
this_data$CONFAM <- mapvalues(this_data$CONFAM, from = c(1, 6, 2), to = c(1, 0, NA))
this_data <- this_data %>% 
  mutate(support_index = ifelse(!is.na(LLCONT), rowSums(cbind(LLFAM, LLAMIG, LLEMPR, LLINTR, LLCONO, LLOTR), na.rm = TRUE), NA),
         any_support = ifelse(support_index > 0, 1, 0),
         housing_connect = ifelse(TENV %in% c(11, 12, 13, 14), 1, 0))
# Ordén jerárquico empezando con "strong ties" a "weak ties"
#this_data <- this_data %>% mutate(support = ifelse(LLFAM == 1, "Familiares", 
#                                                   ifelse(LLAMIG == 1, "Amigos", 
#                                                        ifelse(LLEMPR == 1, "Empresarios", 
#                                                              ifelse(LLINTR == 1, "Intermediarios", 
#                                                                   ifelse(LLCONO == 1, "Conocidos", 
#                                                                          ifelse(LLOTR == 1, "Otros", NA)))))))

## Cuenta de paricipación en organizaciones, ONGs, etc 
this_data$PXAYU <- mapvalues(this_data$PXAYU, from = c(1, 6), to = c(1, 0))
this_data$PXDEP <- mapvalues(this_data$PXDEP, from = c(1, 6), to = c(1, 0))
this_data$PXEDU <- mapvalues(this_data$PXEDU, from = c(1, 6), to = c(1, 0))
this_data$PXREL <- mapvalues(this_data$PXREL, from = c(1, 6), to = c(1, 0))
this_data$PXSOC <- mapvalues(this_data$PXSOC, from = c(1, 6), to = c(1, 0))
this_data$PNXONG <- mapvalues(this_data$PNXONG, from = c(1, 6), to = c(1, 0))
this_data$PNXPOL <- mapvalues(this_data$PNXPOL, from = c(1, 6), to = c(1, 0))
this_data$PNXREL <- mapvalues(this_data$PNXREL, from = c(1, 6), to = c(1, 0))
this_data$PNXEDU <- mapvalues(this_data$PNXEDU, from = c(1, 6), to = c(1, 0))
this_data$PNXDEP <- mapvalues(this_data$PNXDEP, from = c(1, 6), to = c(1, 0))
this_data$PNXSOC <- mapvalues(this_data$PNXSOC, from = c(1, 6), to = c(1, 0))
this_data$PELEC <- mapvalues(this_data$PELEC, from = c(1, 6), to = c(1, 0))

this_data <- this_data %>% 
  mutate(participation_index = rowSums(cbind(PXAYU, PXDEP, PXEDU, PXREL, PXSOC, PNXONG, PNXPOL, PNXREL, PNXEDU, PNXDEP, PNXSOC, PELEC), na.rm = F),
         participation_indicator = ifelse(participation_index > 0, 1, 0))

# Process data regarding family relationships and whether they live within or outside household
siblings_data <- this_data[grep("DVHE", colnames(this_data))] == 1 | this_data[grep("DVHE", colnames(this_data))] == 2
this_data <- this_data %>% mutate(NUMHH = na_if(NUMHH, 99),
                                  HH_no_convive = rowSums(!is.na(this_data[grep("NOHE", colnames(this_data))]), na.rm = T),
                                  HH_convive = NUMHH - HH_no_convive,
                                  HH_no_convive_espana = rowSums(siblings_data, na.rm = T),
                                  HH_en_espana = HH_convive + HH_no_convive_espana
)