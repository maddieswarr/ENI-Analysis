###########################################################################################################################
# Name of program:	  1_process_raw.R,                  
# Author:             Madeline Swarr 
# Last modification: 	01 July 2022
#       
# Description: 
# This program processes the ENI's raw microdata according the corresponding metadata file which contains 
# the registry design of the microdata.  
#     Organization: INE
#     Statistical operation: EPA
#     Data production year: 2007
#     Data downloaded from: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177005&menu=resultados&idp=1254735573002#!tabs-1254736195389
#
# Input:          
#     - Microdata: 	datos_eni_07.txt
#     - Metadata: 	disreg_eni07.xls
# Output: 
#     - Processed microdata in R dataframe (.RDS): 
#       - all_data (processed, contains all variables)
#       - this_data (processed, filtered to desired keep variables)
#       
# IMPORTANT NOTE: In metadata file disreg_eni07.xls, I added column F named "Keep" where any cell value of "Yes" will signal
# the corresponding variable to be kept in "this_data" dataframe. Edit this column to keep or get rid of certain variables.
#
##################################################################################################################

options(java.parameters = "-Xmx4g" )

########### PARAMETERS AND SWITCHES #########
# Run date, for archiving purposes
run_date <- "27may2022"
# Do you need to read in the raw data?
readin.raw <- 0

################## SET UP ###################
# Specify head directory
head.dir <- "/Volumes/GoogleDrive/My Drive/Thesis/Data Analysis/ENI_ANALYSIS"
# Download and load necessary packages/libraries
dependency.dir <- paste0(head.dir, "/Pipeline/dependencies")
key.dir <- paste0(head.dir, "/Pipeline/keys")
source(paste0(dependency.dir, "/get_packages.R"))
get_packages(c("XLConnect", "openxlsx", "plyr", "tidyverse", "reshape2", "utile.tools", "hash", "translateR"))

# Input data names
data_micro <- "datos_eni_07.txt"
data_meta  <- "disreg_eni07.xls"
data_ppr <- "población_provincia_serie_temporal.xlsx"
# Google Cloud key for access to Translate
key_GoogleTranslate <- "medeg-thesis-translator-d96fe6bca7c2.json"

# Specify other directories
input.dir <- paste0(head.dir, "/Data")
codebook.dir <- paste0(input.dir, "/", data_meta)
padron_province.dir <- paste0(input.dir, "/", data_ppr)
output.dir <- paste0(head.dir, "/Output/", run_date, "/1_processed")
# Create output directory
if (!dir.exists(output.dir)){
  dir.create(output.dir, recursive = T)
}

############## READ IN METADATA AND RAW MICRODATA #########################
# Read in metadata (.xls), worksheet titled "Diseno"
tryCatch((workBook <- XLConnect::loadWorkbook(codebook.dir)), error=function(e) 
        stop(paste("Error. No se puede abrir el fichero: ", e, data_meta,". Saliendo de la ejecucion...", sep = "")))
codebook <- readNamedRegion(workBook, name = "PARAMETROS")

# Gather metadata parameters
varNames <- codebook[,1]
varDescr <- codebook[,2]
positions <- codebook[,5]
keepVars <- codebook[,6] 

# If raw data has not yet been read in, read it in
if(readin.raw == 1) {
  # Read in microdata (.txt), all as character strings
  tryCatch((all_data <- read.fortran(file = paste0(input.dir, "/", data_micro), format= paste0("A", codebook$Long..Campo))), error=function(e)
    stop(paste("Error. No se encuentra el fichero: ", e, data_micro,". Saliendo de la ejecucion...", sep = "")))

  # Capture columns along with data type for each variable
  # AN = string, N = numeric
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
  
  # Add names and attributes to microdata dataframe
  names(all_data) <- codebook[,1]
  comment(all_data) <- varDescr
  # Save the raw data as .RDS
  saveRDS(all_data, file = paste0(input.dir, "/datos_eni_07.rds"))
  
  # If raw data has already been saved as .RDS, read it in...
  } else {
  all_data <- readRDS(paste0(input.dir, "/datos_eni_07.rds"))
}

################################ PROCESS DATA ###################################
# Filter to desired variables
this_data <- all_data[, which(keepVars == "Yes")]
names(this_data) <- codebook[,1][which(keepVars == "Yes")]
comment(this_data) <- varDescr[which(keepVars == "Yes")]

# Define the prefixes of the variables which indicate motivations for migratory movements, used for flagging for economic
# immigrants
motive_var_prefixes <- c("JUB", "DSLA", "FEMP", "BEMP", "POLI", "RELI", "EDUC", "CALV", "REAG", "COST", "CLIM", "TRNS", "OTRS")

###  Since each row in the data represents a household, we must identify and extract the household member of reference ####
this_data <- this_data %>% mutate(EDAD = NA,
                                  SEXO = NA,
                                  PNAC = NA,
                                  ALLE = NA,
                                  ANAC = NA,
                                  NESP = NA,
                                  motive_var_prefixes = NA,
                                  spanish_housemates = ifelse(NPERS > 1, 0, NA)
                                  )

this_data[, paste0("compi_relation", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_age", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_country", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_sex", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_nesp", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_dnac", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_ane", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_arrival_spain", str_pad(1:15, 2, pad = "0"))] = NA

# Vector of potential number of household members
compis <- c(1:15)
# NPELEG represents the household member of reference (takes on value in [1 to 15])
# Age, sex, nationality, year of arrival, amongst other information is collected for each household member
# For example, age information for each member in the household is stored in variables EDAD01, EDAD02, ..., EDAD15 
# If NPELEG = 4, then the age of the immigrant of reference is stored in variable EDAD04. 
for (n in levels(factor(this_data$NPELEG))){
  this_data$EDAD[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$EDAD", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$SEXO[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$SEXO", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$PNAC[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$PNAC", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$ALLE[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$ALLE", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$ANAC[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$ANAC", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$NESP[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$NESP", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  these_compis = compis[!(compis %in% n)]
  for (h in these_compis) {
    this_compi_relation <- eval(parse(text = paste0("this_data$P", str_pad(n, 2, pad = "0"), "_", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_age <- eval(parse(text = paste0("this_data$EDAD", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_country <- eval(parse(text = paste0("this_data$PNAC", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_sex <- eval(parse(text = paste0("this_data$SEXO", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_nesp <- eval(parse(text = paste0("this_data$NESP", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_dnac <- eval(parse(text = paste0("this_data$DNAC", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_ane <- eval(parse(text = paste0("this_data$ANE", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_arrival_spain <- eval(parse(text = paste0("this_data$ALLE", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_spanish_housemates <- this_data$spanish_housemates[which(this_data$NPELEG == n)]
    
    # Determine if an individual shares a house with a working age, Spanish national by birth (country == 108) and count the 
    # number of housemates that meet this requirement
    these_compis_country_age <- data.frame(country = this_compi_country, 
                                           age = this_compi_age,
                                           spanish_housemates = this_compi_spanish_housemates) %>% 
      mutate(spanish_housemates = ifelse(country == "108" & age >= 16 & age < 65, spanish_housemates + 1, spanish_housemates))

    this_data[which(this_data$NPELEG == n),]$spanish_housemates <- these_compis_country_age$spanish_housemates
    this_data[which(this_data$NPELEG == n), paste0("compi_relation", str_pad(h, 2, pad = "0"))] <- this_compi_relation
    this_data[which(this_data$NPELEG == n), paste0("compi_age", str_pad(h, 2, pad = "0"))] <- this_compi_age
    this_data[which(this_data$NPELEG == n), paste0("compi_country", str_pad(h, 2, pad = "0"))] <- this_compi_country
    this_data[which(this_data$NPELEG == n), paste0("compi_sex", str_pad(h, 2, pad = "0"))] <- this_compi_sex
    this_data[which(this_data$NPELEG == n), paste0("compi_nesp", str_pad(h, 2, pad = "0"))] <- this_compi_nesp
    this_data[which(this_data$NPELEG == n), paste0("compi_dnac", str_pad(h, 2, pad = "0"))] <- this_compi_dnac
    this_data[which(this_data$NPELEG == n), paste0("compi_ane", str_pad(h, 2, pad = "0"))] <- this_compi_ane
    this_data[which(this_data$NPELEG == n), paste0("compi_arrival_spain", str_pad(h, 2, pad = "0"))] <- this_compi_arrival_spain
    }
  }

# Set to NA any values that represent missing values
this_data <- this_data %>% mutate(EDAD = na_if(EDAD, 999),
                                  PNAC = na_if(PNAC, "   "),
                                  NESP = na_if(NESP, 0),
                                  HORAS2 = na_if(HORAS2, 999),
                                  RECANT = na_if(RECANT, 99999),
                                  ALLE = na_if(ALLE, 0),
                                  ANAC = na_if(ANAC, 0),
                                  LMAT = na_if(LMAT, 0),
                                  OCUP1 = ifelse(str_trim(CIOCUP1) == "", NA, str_trim(CIOCUP1)),
                                  OCUP2 = ifelse(str_trim(CIOCUP2) == "", NA, str_trim(CIOCUP2)),
                                  OCUP3 = ifelse(str_trim(CIOCUP2) == "", NA, str_trim(CIOCUP3)),
                                  SECT1 = ifelse(str_trim(SECT1) == "" | str_trim(SECT1) == "00", NA, str_trim(SECT1)),
                                  SECT2 = ifelse(str_trim(SECT2) == "", NA, str_trim(SECT2)),
                                  SECT3 = ifelse(str_trim(SECT3) == "", NA, str_trim(SECT3)),
                                  TPO1 = na_if(TPO1, 0),
                                  DOCUM = na_if(DOCUM, 0),
                                  NUMHH = na_if(NUMHH, 99),
                                  TENHH = na_if(TENHH, 0),
                                  ANOCOM = na_if(ANOCOM, 0),
                                  NCONTR = na_if(NCONTR, 99)
)


####  Hash in info from metadata ####
# Collect hash lookups 
country_codebook <- readWorksheetFromFile(codebook.dir, sheet = 4)
language_codebook <- readWorksheetFromFile(codebook.dir, sheet = 7, startRow = 2)
province_codebook <- readWorksheetFromFile(codebook.dir, sheet = 2)
comunidad_codebook <- readWorksheetFromFile(codebook.dir, sheet = 3)
occupation_codebook <- readWorksheetFromFile(codebook.dir, sheet = 16, region = "A2:B22") # Note: made edits to disreg_eni07.xls on "Ocupación_Ampliada" tab...see README for details
sector_codebook <- readWorksheetFromFile(codebook.dir, sheet = 11, startRow = 4) # Note: made edits to disreg_eni07.xls on "Sector de actividad" tab...see README for details
situ_codebook <- readWorksheetFromFile(codebook.dir, sheet = 13, startRow = 2) 
educ_codebook <- readWorksheetFromFile(codebook.dir, sheet = 8, startRow = 2) 

# Hash in country of origin info
hash_countries <- hash()
.set(hash_countries, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$LITERAL)
this_data$country[!is.na(this_data$PNAC)] <- values(hash_countries, keys = this_data$PNAC[!is.na(this_data$PNAC)])
clear(hash_countries)

# Hash in country of origin (shortened name) info
hash_countries_short <- hash()
.set(hash_countries_short, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$SHORT)
this_data$country_short[!is.na(this_data$PNAC)] <- values(hash_countries_short, keys = this_data$PNAC[!is.na(this_data$PNAC)])
clear(hash_countries_short)

# Hash in whether country of origin is an EU member or not
hash_country_class <- hash()
.set(hash_country_class, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$EU)
this_data$country_class[!is.na(this_data$PNAC)] <- values(hash_country_class, keys = this_data$PNAC[!is.na(this_data$PNAC)])
clear(hash_country_class)

# Hash in mother tongue info
hash_language <- hash()
.set(hash_language, keys = language_codebook$CÓDIGO, values = language_codebook$IDIOMA)
this_data$language[!is.na(this_data$LMAT)] <- values(hash_language, keys = this_data$LMAT[!is.na(this_data$LMAT)])
clear(hash_language)

# Hash in indicator of whether mother tongue is Spanish or not
hash_country_spanish <- hash()
.set(hash_country_spanish, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$ESPANOL_LENGUA_MATERNA)
this_data$country_spanish[!is.na(this_data$PNAC)] <- values(hash_country_spanish, keys = this_data$PNAC[!is.na(this_data$PNAC)])
clear(hash_country_class)

# Munge data regarding siblings province of residence
siblings_data_prov <- this_data[grep("PRHE", colnames(this_data))] %>%
  mutate_all(trimws) %>%
  mutate_all(na_if, "") %>%
  mutate_all(na_if, "00")
# Hash in province of residence in Spain info
hash_provinces <- hash()
.set(hash_provinces, keys = province_codebook$CODIGO_PROVINCIA, values = province_codebook$NOMBRE_PROVINCIA)
this_data$province <- values(hash_provinces, keys = this_data$CPRO)
this_data$province <- trimws(this_data$province)
# In which province live the individuals siblings, if they live in Spain as well
this_data$HH_province1[!is.na(siblings_data_prov$PRHE01)] <- values(hash_provinces, keys = siblings_data_prov$PRHE01[!is.na(siblings_data_prov$PRHE01)])
this_data$HH_province2[!is.na(siblings_data_prov$PRHE02)] <- values(hash_provinces, keys = siblings_data_prov$PRHE02[!is.na(siblings_data_prov$PRHE02)])
this_data$HH_province3[!is.na(siblings_data_prov$PRHE03)] <- values(hash_provinces, keys = siblings_data_prov$PRHE03[!is.na(siblings_data_prov$PRHE03)])
this_data$HH_province4[!is.na(siblings_data_prov$PRHE04)] <- values(hash_provinces, keys = siblings_data_prov$PRHE04[!is.na(siblings_data_prov$PRHE04)])
this_data$HH_province5[!is.na(siblings_data_prov$PRHE05)] <- values(hash_provinces, keys = siblings_data_prov$PRHE05[!is.na(siblings_data_prov$PRHE05)])
this_data$HH_province6[!is.na(siblings_data_prov$PRHE06)] <- values(hash_provinces, keys = siblings_data_prov$PRHE06[!is.na(siblings_data_prov$PRHE06)])
this_data$HH_province7[!is.na(siblings_data_prov$PRHE07)] <- values(hash_provinces, keys = siblings_data_prov$PRHE07[!is.na(siblings_data_prov$PRHE07)])
this_data$HH_province8[!is.na(siblings_data_prov$PRHE08)] <- values(hash_provinces, keys = siblings_data_prov$PRHE08[!is.na(siblings_data_prov$PRHE08)])
this_data$HH_province9[!is.na(siblings_data_prov$PRHE09)] <- values(hash_provinces, keys = siblings_data_prov$PRHE09[!is.na(siblings_data_prov$PRHE09)])
this_data$HH_province10[!is.na(siblings_data_prov$PRHE10)] <- values(hash_provinces, keys = siblings_data_prov$PRHE10[!is.na(siblings_data_prov$PRHE10)])
this_data$HH_province11[!is.na(siblings_data_prov$PRHE11)] <- values(hash_provinces, keys = siblings_data_prov$PRHE11[!is.na(siblings_data_prov$PRHE11)])
this_data$HH_province12[!is.na(siblings_data_prov$PRHE12)] <- values(hash_provinces, keys = siblings_data_prov$PRHE12[!is.na(siblings_data_prov$PRHE12)])
clear(hash_provinces)

# Hash in comunidad autónoma of residence info 
hash_comunidad <- hash()
.set(hash_comunidad, keys = comunidad_codebook$COD_CCAA, values = comunidad_codebook$NOM_CCAA)
this_data$comunidad <- values(hash_comunidad, keys = this_data$CCAA)
this_data$comunidad <- trimws(this_data$comunidad)
clear(hash_comunidad)

# Hash in occupation info 
# OCUP1 = last occupation in country of departure
# OCUP2 = current occupation
# OCUP3 = occupation in first job upon arriving in Spain
hash_occupation <- hash()
.set(hash_occupation, keys = occupation_codebook$Código, values = occupation_codebook$Ocupaciones)
this_data$occupation1[!is.na(this_data$OCUP1)] <- values(hash_occupation, keys = this_data$OCUP1[!is.na(this_data$OCUP1)])
this_data$occupation2[!is.na(this_data$OCUP2)] <- values(hash_occupation, keys = this_data$OCUP2[!is.na(this_data$OCUP2)])
this_data$occupation3[!is.na(this_data$OCUP3)] <- values(hash_occupation, keys = this_data$OCUP3[!is.na(this_data$OCUP3)])
clear(hash_occupation)

# Hash in sector of occupation info
# SECT1 = sector of last occupation in country of departure
# SECT2 = sector of current occupation
# SECT3 = sector of occupation in first job upon arriving in Spain
hash_sector <- hash()
.set(hash_sector, keys = sector_codebook$CÓDIGOS.QUE.AGRUPA, values = sector_codebook$CLASIFICACIÓN.ACTIVIDADES)
this_data$sector1[!is.na(this_data$SECT1)] <- values(hash_sector, keys = this_data$SECT1[!is.na(this_data$SECT1)])
this_data$sector2[!is.na(this_data$SECT2)] <- values(hash_sector, keys = this_data$SECT2[!is.na(this_data$SECT2)])
this_data$sector3[!is.na(this_data$SECT3)] <- values(hash_sector, keys = this_data$SECT3[!is.na(this_data$SECT3)])
clear(hash_sector)

# Hash in professional situation info
hash_situ <- hash::hash()
.set(hash_situ, keys = situ_codebook$Código, values = situ_codebook$Situación.profesional)
this_data$situation1[!is.na(this_data$SITU1)] <- values(hash_situ, keys = this_data$SITU1[!is.na(this_data$SITU1)])
this_data$situation2[!is.na(this_data$SITU2)] <- values(hash_situ, keys = this_data$SITU2[!is.na(this_data$SITU2)])
this_data$situation3[!is.na(this_data$SITU3)] <- values(hash_situ, keys = this_data$SITU3[!is.na(this_data$SITU3)])
clear(hash_situ)

# Hash in level of studies completed
hash_educ <- hash::hash()
.set(hash_educ, keys = educ_codebook$Codificación, values = educ_codebook$Estudios.terminados.breve)
this_data$education[!is.na(this_data$MNIV)] <- values(hash_educ, keys = this_data$MNIV[!is.na(this_data$MNIV)])
clear(hash_educ)


#### More complicated processing ####
# Capture all forms of finding a job
form_data <- this_data %>% dplyr::select(c("IDQ", grep("FORM", colnames(this_data)))) %>% 
  melt(id.vars = "IDQ") %>% 
  mutate(FORM = ifelse(is.na(variable), NA, ifelse(value == 1, paste0(variable), ""))) %>%
  group_by(IDQ) %>% dplyr::summarize(all_forms = utile.tools::paste0(FORM, collapse = "", na.rm = T)) %>%
  mutate(all_forms = ifelse(all_forms == "", NA, all_forms)) %>%
  mutate(all_forms_any_friends = ifelse(grepl("FORM04", all_forms), "FORM04", all_forms))

# Collect data that indicates “economic immigrants”
# Immigration history indicating movements “por falta de empleo”, “para buscar un empleo mejor”, or “por cambio de destino laboral”
if (file.exists(file = paste0(output.dir, "/motivos_por_inmigrar.rds"))) {
  motive_data <- readRDS(file = paste0(output.dir, "/motivos_por_inmigrar.rds"))
} else { 
  # Get data on immigration history 
  motive_data <- this_data %>% dplyr::select(starts_with("PTRS")) %>% mutate(spain_num = NA)
  for (row in 1:nrow(motive_data)) {
    this_row = motive_data[row,]
    while (is.na(this_row$spain_num)){
      for (col in 1:(ncol(motive_data)-1)) {
        this_col = ncol(motive_data) - col
        this_point = this_row[this_col]
        # Check that country immigrated to is Spain (108)
        if ((this_point == "108") & is.na(this_row$spain_num)) {
          this_row$spain_num = paste0(str_pad(this_col, 2, pad = "0"))
        }
      }
    }
    motive_data[row,]$spain_num <- this_row$spain_num
  }
  saveRDS(motive_data, file = paste0(output.dir, "/motivos_por_inmigrar.rds"))
}

# Since an individual could have immigrated various times to various countries, need to indicate which movement and 
# corresponding variables related to motivations for moving pertain to the movement to Spain
this_data$motive_var <- motive_data$spain_num
for (m in 1:length(motive_var_prefixes)){
  motive_var_prefix = motive_var_prefixes[m]
  this_data$this_motive = NA
  for (n in 1:length(levels(factor(this_data$motive_var)))){
    this_data$this_motive[which(this_data$motive_var == str_pad(n, 2, pad = "0"))] <- eval(parse(text = paste0("this_data$", motive_var_prefix, str_pad(n, 2, pad = "0"))))[which(this_data$motive_var ==  str_pad(n, 2, pad = "0"))]
  }
  this_data <- this_data %>% 
    mutate(this_motive = ifelse(this_motive == 6, 0, ifelse(this_motive == 1, 1, this_motive))) %>%
    rename(!!motive_var_prefix := this_motive)
}

# Collect data on Spanish language abilities
idioma_data <- this_data %>% select(starts_with(c("IDIO","COMP","HABLA","LEE","ESCRI"))) %>% mutate(COMP = NA,
                                                                                                    HABLA = NA,
                                                                                                    LEE = NA,
                                                                                                    ESCRI = NA)
# idio == 101 means Spanish
for (i in 1:6) {
  idio = eval(parse(text = paste0("idioma_data$IDIO", i)))
  idioma_data$COMP[which(idio == 101)] <- eval(parse(text = paste0("idioma_data$COMP", i)))[which(idio == 101)]
  idioma_data$HABLA[which(idio == 101)] <- eval(parse(text = paste0("idioma_data$HABLA", i)))[which(idio == 101)]
  idioma_data$LEE[which(idio == 101)] <- eval(parse(text = paste0("idioma_data$LEE", i)))[which(idio == 101)]
  idioma_data$ESCRI[which(idio == 101)] <- eval(parse(text = paste0("idioma_data$ESCRI", i)))[which(idio == 101)]
}

idioma_data <- idioma_data %>% select(COMP, HABLA, LEE, ESCRI) %>% mutate(COMP = mapvalues(idioma_data$COMP, from = c(1, 6), to = c(1, 0)),
                                                                                    HABLA = mapvalues(idioma_data$HABLA, from = c(1, 6), to = c(1, 0)),
                                                                                    LEE = mapvalues(idioma_data$LEE, from = c(1, 6), to = c(1, 0)),
                                                                                    ESCRI = mapvalues(idioma_data$ESCRI, from = c(1, 6), to = c(1, 0)),
                                                                                    IDIO = COMP + HABLA + LEE + ESCRI)

#### Process data tracing geographic movements
# Collect data that indicates municipalities lived in/currently living in
muv_data <- this_data %>% dplyr::select(varNames[grepl("MUV_", varNames)]) %>%
  mutate_all(trimws) %>%
  mutate_all(na_if, "")
muv_data$MUV <- muv_data[cbind(1:nrow(muv_data), max.col(!is.na(muv_data), ties.method = 'last'))]

## Collect data that indicates provinces lived in/currently living in
#prov_data <- this_data %>% dplyr::select(starts_with("PRV")) %>% select(-PRVIV) %>%
#  mutate_all(trimws) %>%
#  mutate_all(na_if, "")
#prov_data$PRV <- prov_data[cbind(1:nrow(prov_data), max.col(!is.na(prov_data), ties.method = 'last'))]

# Collect data that indicates how long was lived/currently living in each municipality
anom_data <- this_data %>% dplyr::select(starts_with("ANOM")) %>%
  mutate_all(na_if, 0)
this_data$ANOM <- anom_data[cbind(1:nrow(anom_data), max.col(!is.na(anom_data), ties.method = 'last'))]

this_data$ANOM_min <- ifelse(is.na(this_data$ANOM) & !is.na(this_data$ANOCOM), this_data$ANOCOM, ifelse(!is.na(this_data$ANOCOM) & (this_data$ANOCOM > this_data$ANOM), this_data$ANOCOM, this_data$ANOM))
this_data$ANOM_floor <- ifelse(this_data$ANOM_min < 1998, 1998, this_data$ANOM_min)

# Merge in population data by province and country of origin
padron_province_data <- read.xlsx(padron_province.dir, sheet = 3, startRow = 8, cols = c(2:27)) 
this_data <- merge(x = this_data, y = padron_province_data[, c("Destination", "Origin", "1998":"2007")], by.x = c("province", "country"), by.y = c("Destination", "Origin"), all.x = TRUE)
this_data$eth_dens_prov = NA
for (year in c(1998:2007)) {
  these_obs = which(this_data$ANOM_floor == year)
  this_data$eth_dens_prov[these_obs] <-  eval(parse(text = paste0("this_data$`", year, "`")))[these_obs]
}

################## Social variable exploration ###########################
# Count of social ties after arriving in Spain
this_data$LLCONT <- mapvalues(this_data$LLCONT, from = c(1, 6), to = c(1, 0))
this_data$LLFAM <- mapvalues(this_data$LLFAM, from = c(1, 6), to = c(1, 0))
this_data$LLAMIG <- mapvalues(this_data$LLAMIG, from = c(1, 6), to = c(1, 0))
this_data$LLEMPR <- mapvalues(this_data$LLEMPR, from = c(1, 6), to = c(1, 0))
this_data$LLINTR <- mapvalues(this_data$LLINTR, from = c(1, 6), to = c(1, 0))
this_data$LLCONO <- mapvalues(this_data$LLCONO, from = c(1, 6), to = c(1, 0))
this_data$LLOTR <- mapvalues(this_data$LLOTR, from = c(1, 6), to = c(1, 0))
this_data$DINFAM <- mapvalues(this_data$DINFAM, from = c(1, 6), to = c(1, 0))
this_data$CONFAM <- mapvalues(this_data$CONFAM, from = c(1, 6, 2), to = c(1, 0, NA))

# Ordén jerárquico empezando con "strong ties" a "weak ties"
#this_data <- this_data %>% mutate(support = ifelse(LLFAM == 1, "Familiares", 
#                                                   ifelse(LLAMIG == 1, "Amigos", 
#                                                        ifelse(LLEMPR == 1, "Empresarios", 
#                                                              ifelse(LLINTR == 1, "Intermediarios", 
#                                                                   ifelse(LLCONO == 1, "Conocidos", 
#                                                                          ifelse(LLOTR == 1, "Otros", NA)))))))

# Count of participation in social, religious, education organizations, NGO's, etc.
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

# Data regarding siblings in Spain and where they live
household_data <- this_data %>% select(starts_with("compi"))
siblings_data <- this_data[grep("DVHE", colnames(this_data))]
siblings_data_prov <- this_data[grep("HH_province", colnames(this_data))] %>%
  mutate_all(trimws)
siblings_data_same_prov <- (siblings_data_prov == this_data$province)

############################ Set and specify final processed variables ##########################
study_data <- this_data %>% mutate(#COVARIATES
                                  arrival_spain = ALLE,
                                  years = ifelse((2006 - ALLE) > 0, 2006 - ALLE, 0),
                                  age = EDAD,
                                  age_arrival = this_data$ALLE - this_data$ANAC,
                                  age_sq = EDAD^2,
                                  sex = mapvalues(this_data$SEXO, from = c(1, 2), to = c("Male", "Female")),
                                  education = education,
                                  spanish = ifelse(language == "español", "Nativo", 
                                                   ifelse(HAESP %in% c(1,2) | idioma_data$IDIO >= 3, "Bien", 
                                                          ifelse(HAESP == 3 | idioma_data$IDIO == 2, "Suficiente",
                                                                 ifelse(HAESP == 4 | idioma_data$IDIO < 2, "Necesita mejorar", NA)))),
                                  #spanish = mapvalues(this_data$HAESP, from = c(1, 2, 3, 4), to = c("Muy bien", "Bien", "Suficiente", "Necesita mejorar")),
                                  work_permit = ifelse((DOCUM %in% c(1, 2, 3) | country_class == "Yes"), 1, ifelse(is.na(DOCUM) & is.na(country_class), NA, 0)),
                                  spanish_nat = mapvalues(this_data$NESP, from = c(1, 6), to = c(1, 0)),
                                  country = country,
                                  country_short = country_short,
                                  province = province,
                                  comunidad = comunidad,
                                  occupation1 = occupation1,
                                  occupation2 = occupation2,
                                  occupation3 = occupation3,
                                  sector1 = sector1,
                                  sector2 = sector2,
                                  sector3 = sector3,
                                  situation1 = situation1,
                                  situation2 = situation2,
                                  situation3 = situation3,
                                  municipality = muv_data$MUV,
                                  arrival_mun = ANOM,
                                  part_time = ifelse(HORAS2 < 30, 1, 0),
                                  #POPULATION INDICATORS
                                  economic = ifelse(DSLA + FEMP + BEMP > 0, 1, 0), 
                                  retire = JUB,
                                  political = POLI, 
                                  religious = RELI,
                                  student = EDUC,
                                  quality = CALV,
                                  regroup = REAG,
                                  cheaper = COST,
                                  climate = CLIM,
                                  temporary = TRNS,
                                  other = OTRS,
                                  was_working = mapvalues(this_data$TRAB1, from = c(1, 6), to = c(1, 0)),
                                  had_worked = mapvalues(this_data$TRANT, from = c(1, 6), to = c(1, 0)),
                                  #RESPONSE VARS
                                  month_earn = INGRE,
                                  log_month_earn = log(INGRE),
                                  month_hours = HORAS2*4,
                                  hour_earn = month_earn/month_hours,
                                  log_hour_earn = log(month_earn/month_hours),
                                  lt_search_1mo = ifelse(TPO1 <= 2, 1, 0),
                                  lt_search = TPO1,
                                  remittance = RECANT,
                                  #SOCIAL VARS
                                  eth_dens_prov = eth_dens_prov,
                                  log_eth_dens_prov = log(eth_dens_prov),
                                  eth_dens_prov_sq = eth_dens_prov^2,
                                  eth_dens_prov_2006 = `2006`,
                                  log_eth_dens_prov_2006 = log(`2006`),
                                  eth_dens_prov_2006_sq = `2006`^2,
                                  influenced = mapvalues(this_data$INFLU, from = c(1, 6), to = c(1, 0)),
                                  networked = mapvalues(this_data$FORM04, from = c(1, 6), to = c(1, 0)),
                                  first_job_still = mapvalues(this_data$PRIMTR, from = c(1, 6), to = c(1, 0)),
                                  insecurity = mapvalues(this_data$BUSEMP, from = c(1, 6), to = c(1, 0)),
                                  precontract = mapvalues(this_data$PROPTR, from = c(1, 6, 0), to = c(1, 0, NA)),
                                  form = form_data$all_forms,
                                  form_any_friends = form_data$all_forms_any_friends,
                                  support_index = ifelse(!is.na(LLCONT), rowSums(cbind(LLFAM, LLAMIG, LLCONO), na.rm = TRUE), NA),
                                  any_support = ifelse(support_index > 0, 1, 0),
                                  housing_connect = ifelse(TENV %in% c(11, 12, 13, 14), 1, 0),
                                  participation_index = rowSums(cbind(PXAYU, PXDEP, PXEDU, PXREL, PXSOC, PNXONG, PNXPOL, PNXREL, PNXEDU, PNXDEP, PNXSOC, PELEC), na.rm = F),
                                  participation_indicator = ifelse(participation_index > 0, 1, 0),
                                  num_siblings = ifelse(TENHH == 6, 0, NUMHH),
                                  HH_convive = rowSums(household_data == 7, na.rm = T),
                                  HH_no_convive = rowSums(!is.na(this_data[grep("NOHE", colnames(this_data))]), na.rm = T),
                                  HH_same_prov = rowSums(!is.na(siblings_data_same_prov), na.rm = T) + rowSums(siblings_data == 1, na.rm = T),
                                  HH_same_prov_sq = HH_same_prov^2,
                                  HH_mismo_municipio = rowSums(siblings_data == 1, na.rm = T) + rowSums(household_data == 7, na.rm = T),
                                  HH_otro_municipio = rowSums(siblings_data == 2, na.rm = T),
                                  HH_en_espana = rowSums(siblings_data == 1 | siblings_data == 2, na.rm = T),
                                  HH_total_en_espana = HH_convive + HH_en_espana,
                                  ## ADDITIONAL INFORMATIVE VARIABLES
                                  reference_person = NPELEG
                                  )


# Subset to the processed variables of interest
study_data_subset <- study_data %>% dplyr::select(IDQ, NPERS, arrival_spain, arrival_mun, years, age, age_arrival, 
                                              age_sq, sex, education, spanish, work_permit, spanish_nat, country, country_short,
                                              province, comunidad, occupation1, occupation2, occupation3,sector1, sector2, sector3, 
                                              situation1, situation2, situation3,municipality, starts_with("HH_province"), starts_with("compi"), 
                                              num_siblings, economic, was_working, had_worked, part_time, month_earn, log_month_earn,
                                              month_hours, hour_earn, log_hour_earn, lt_search_1mo, lt_search, remittance,
                                              eth_dens_prov_2006, eth_dens_prov, log_eth_dens_prov, eth_dens_prov_sq, influenced, networked, 
                                              first_job_still, insecurity, precontract, form, form_any_friends,
                                              support_index, any_support, housing_connect, participation_index, participation_indicator,
                                              num_siblings, HH_convive, HH_no_convive, HH_same_prov, HH_same_prov_sq, HH_mismo_municipio, HH_otro_municipio,
                                              HH_en_espana, HH_total_en_espana, reference_person, spanish_housemates, motive_var_prefixes)

######################### Save processed dataframes ##############################
saveRDS(study_data, file = paste0(output.dir, "/datos_eni_07_processed.rds"))
saveRDS(study_data_subset, file = paste0(output.dir, "/datos_eni_07_processed_subset.rds"))

######################### Free up memory #########################################
rm(workBook, varNames, positions, codebook, all_data, this_data, idioma_data, 
   form_data, country_codebook, language_codebook, comunidad_codebook, occupation_codebook, province_codebook, situ_codebook,
   sector_codebook, educ_codebook, padron_province_data, muv_data, anom_data, household_data, siblings_data, siblings_data_prov, siblings_data_same_prov, 
   hash_comunidad, hash_countries, hash_countries_short, hash_country_class, hash_country_spanish, hash_language, hash_occupation, 
   hash_provinces, hash_situ, hash_sector, hash_educ, motive_data, motive_var_prefixes)
