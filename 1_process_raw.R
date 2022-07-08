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
#       - datos_eni_07_processed.rds (processed, contains all variables)
#       - datos_eni_07_processed_subset.rds (processed, filtered to desired keep variables)
#
# IMPORTANT NOTE: In metadata file disreg_eni07.xls, I added column F named "Keep" where any cell value of "Yes" will signal the 
# corresponding variable to be kept in "this_data" data frame. This column needs to be editted to keep or get rid of certain raw 
# variables.
#
# Dependencies (processes additional information):
#     - housemate_demographics.R
#     - sibling_prov_of_residence.R
#     - spanish_ability.R
#
##################################################################################################################
options(java.parameters = "-Xmx4g" )

########### PARAMETERS AND SWITCHES #########
# Run date, for archiving purposes
run_date <- "06jul2022"
# Do you need to read in the raw data?
readin.raw <- 1
# Do you want to housemate demographic information like gender, age, nationality? WARNING: Long processing step
get_housemate_demographics <- 1
# Do you want to get information about where siblings live in relation to reference individual?
get_sibling_residence_info  <- 1
# Do you want to get the individual's reported Spanish level in speaking, reading, writing, and comprehension?
get_spanish_ability <- 1

################## SET UP ###################
# Specify directories
head.dir <- "/Users/papapeach/Documents/UC3M/Thesis/Data Analysis/ENI-Analysis"
dependency.dir <- paste0(head.dir, "/dependencies")
key.dir <- paste0(head.dir, "/keys")
# Source dependencies
source(paste0(dependency.dir, "/get_packages.R"))
# Download and load necessary packages/libraries
get_packages(c("XLConnect", "openxlsx", "plyr", "tidyverse", "reshape2", "utile.tools", "hash", "translateR"))

# Input data names
data_micro <- "datos_eni_07.txt"
data_meta  <- "disreg_eni07.xls"
data_ppr <- "población_provincia_serie_temporal_por_sexo.xlsx"
# Google Cloud key for access to Translate
key_GoogleTranslate <- "medeg-thesis-translator-d96fe6bca7c2.json"

# Specify other directories
input.dir <- paste0(head.dir, "/Data")
codebook.dir <- paste0(input.dir, "/", data_meta)
padron_province.dir <- paste0(input.dir, "/", data_ppr)
output.dir <- paste0(head.dir, "/output/", run_date, "/1_processed")
# Create output directory
if (!dir.exists(output.dir)){
  dir.create(output.dir, recursive = T)
}

############## READ IN METADATA AND RAW MICRODATA #########################
# Read in metadata (.xls), worksheet titled "Diseno"
tryCatch((workBook <- XLConnect::loadWorkbook(codebook.dir)), error=function(e) 
  stop(paste("Error. Can't open the file: ", e, data_meta,". Haltin execution...", sep = "")))
codebook <- readNamedRegion(workBook, name = "PARAMETROS")

# Gather metadata parameters
varNames <- codebook[,1]
varDescr <- codebook[,2]
positions <- codebook[,5]
keepVars <- codebook[,6] 

# If raw data taken directly from INE's website has not yet been read in, read it in...
if(readin.raw == 1) {
  # Read in microdata (.txt), all as character strings
  tryCatch((all_data <- read.fortran(file = paste0(input.dir, "/", data_micro), format= paste0("A", codebook$Long..Campo))), error=function(e)
    stop(paste("Error. Can't find file: ", e, data_micro,". Halting execution...", sep = "")))
  
  # Capture columns and set corresponding data type for each variable
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
# Initiate vector of column names to keep in final processed study dataset
final_vars <- c()
# Initiate vector of object names to clear from memory at end of script
delete_objects <- c()

###  Since each row in the data represents a household, we must identify and extract the household member of reference ####
this_data <- this_data %>% mutate(EDAD = NA,
                                  SEXO = NA,
                                  PNAC = NA,
                                  ALLE = NA,
                                  ANAC = NA,
                                  NESP = NA,
                                  motive_var_prefixes = NA
)

#### Process basic demographic information on household member of reference ####
# NPELEG represents the household member of reference (takes on value in [1 to 15])
# Age, sex, nationality, year of arrival, amongst other information is collected for each household member
# For example, age information for each member in the household is stored in variables EDAD01, EDAD02, ..., EDAD15 
# If NPELEG = 4, then the age of the individual of reference is stored in variable EDAD04. 
for (n in levels(factor(this_data$NPELEG))){
  this_data$EDAD[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$EDAD", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$SEXO[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$SEXO", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$PNAC[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$PNAC", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$ALLE[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$ALLE", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$ANAC[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$ANAC", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
  this_data$NESP[which(this_data$NPELEG == n)] <- eval(parse(text = paste0("this_data$NESP", str_pad(n, 2, pad = "0"))))[which(this_data$NPELEG == n)]
}

####  Process data that indicates “economic immigrants” ####
# Immigration history indicating movements “por falta de empleo”, “para buscar un empleo mejor”, or “por cambio de destino laboral”
# If immigration motivations information has already been processed, load it in...
if (file.exists(file = paste0(output.dir, "/motivos_por_inmigrar.rds"))) {
  motive_data <- readRDS(file = paste0(output.dir, "/motivos_por_inmigrar.rds"))
} else { 
  # Get data on countries to which the individual has immigrated to
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
    # Save which variable index represents the move to Spain
    motive_data[row,]$spain_num <- this_row$spain_num
  }
  saveRDS(motive_data, file = paste0(output.dir, "/motivos_por_inmigrar.rds"))
}

# Since an individual could have immigrated various times to various countries, need to indicate which movement and 
# corresponding variables related to motivations for moving are the ones that pertain to the movement to Spain
this_data$motive_var <- motive_data$spain_num
# Define the prefixes of the variables which indicate motivations for migratory movements, used for flagging for economic
# immigrants
motive_var_prefixes <- c("JUB", "DSLA", "FEMP", "BEMP", "POLI", "RELI", "EDUC", "CALV", "REAG", "COST", "CLIM", "TRNS", "OTRS")
for (m in 1:length(motive_var_prefixes)){
  motive_var_prefix = motive_var_prefixes[m]
  this_data$this_motive = NA
  for (n in 1:length(levels(factor(this_data$motive_var)))){
    this_data$this_motive[which(this_data$motive_var == str_pad(n, 2, pad = "0"))] <- eval(parse(text = paste0("this_data$", motive_var_prefix, str_pad(n, 2, pad = "0"))))[which(this_data$motive_var ==  str_pad(n, 2, pad = "0"))]
  }
  this_data <- this_data %>% 
    mutate(this_motive = ifelse(this_motive == 6, 0, ifelse(this_motive == 1, 1, this_motive))) %>%
    dplyr::rename(!!motive_var_prefix := this_motive)
}


#### Set to NA any values that represent missing values ####
this_data <- this_data %>% mutate(EDAD = na_if(EDAD, 999),
                                  PNAC = ifelse(str_trim(PNAC) == "", NA, str_trim(PNAC)), 
                                  NESP = na_if(NESP, 0),
                                  HORAS2 = na_if(HORAS2, 999),
                                  RECANT = na_if(RECANT, 99999),
                                  ALLE = na_if(ALLE, 0),
                                  ANAC = na_if(ANAC, 0),
                                  LMAT = na_if(LMAT, 0),
                                  OCUP1 = ifelse(str_trim(CIOCUP1) == "", NA, str_trim(CIOCUP1)),
                                  OCUP2 = ifelse(str_trim(CIOCUP2) == "", NA, str_trim(CIOCUP2)),
                                  OCUP3 = ifelse(str_trim(CIOCUP3) == "", NA, str_trim(CIOCUP3)),
                                  SECT1 = ifelse(str_trim(SECT1) == "" | str_trim(SECT1) == "00", NA, str_trim(SECT1)),
                                  SECT2 = ifelse(str_trim(SECT2) == "", NA, str_trim(SECT2)),
                                  SECT3 = ifelse(str_trim(SECT3) == "", NA, str_trim(SECT3)),
                                  TPO1 = na_if(TPO1, 0),
                                  DOCUM = na_if(DOCUM, 0),
                                  NUMHH = na_if(NUMHH, 99),
                                  TENHH = na_if(TENHH, 0),
                                  ANOCOM = na_if(ANOCOM, 0),
                                  NCONTR = na_if(NCONTR, 99),
                                  PR1VI = ifelse(str_trim(PR1VI) == "" | str_trim(PR1VI) == "00", NA, str_trim(PR1VI))
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
this_data$country[!is.na(this_data$PNAC)] <- hash::values(hash_countries, keys = this_data$PNAC[!is.na(this_data$PNAC)])
clear(hash_countries)

# Hash in country of origin (shortened name) info
hash_countries_short <- hash()
.set(hash_countries_short, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$SHORT)
this_data$country_short[!is.na(this_data$PNAC)] <- hash::values(hash_countries_short, keys = this_data$PNAC[!is.na(this_data$PNAC)])
clear(hash_countries_short)

# Hash in whether country of origin is an EU member or not
hash_country_class <- hash()
.set(hash_country_class, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$EU)
this_data$country_class[!is.na(this_data$PNAC)] <- hash::values(hash_country_class, keys = this_data$PNAC[!is.na(this_data$PNAC)])
clear(hash_country_class)

# Hash in mother tongue info
hash_language <- hash()
.set(hash_language, keys = language_codebook$CÓDIGO, values = language_codebook$IDIOMA)
this_data$language[!is.na(this_data$LMAT)] <- hash::values(hash_language, keys = this_data$LMAT[!is.na(this_data$LMAT)])
clear(hash_language)

# Hash in indicator of whether mother tongue is Spanish or not
hash_country_spanish <- hash()
.set(hash_country_spanish, keys = country_codebook$CÓDIGO.DEL.PAÍS.NACIONALIDAD, values = country_codebook$ESPANOL_LENGUA_MATERNA)
this_data$country_spanish[!is.na(this_data$PNAC)] <- hash::values(hash_country_spanish, keys = this_data$PNAC[!is.na(this_data$PNAC)])
clear(hash_country_class)

# Hash in province of residence in Spain info
hash_provinces <- hash()
.set(hash_provinces, keys = province_codebook$CODIGO_PROVINCIA, values = province_codebook$NOMBRE_PROVINCIA)
this_data$province <- hash::values(hash_provinces, keys = this_data$CPRO)
this_data$province <- trimws(this_data$province)
this_data$province_PR1VI[!is.na(this_data$PR1VI)] <- hash::values(hash_provinces, keys = this_data$PR1VI[!is.na(this_data$PR1VI)])
clear(hash_provinces)

# Hash in comunidad autónoma of residence info 
hash_comunidad <- hash()
.set(hash_comunidad, keys = comunidad_codebook$COD_CCAA, values = comunidad_codebook$NOM_CCAA)
this_data$comunidad <- hash::values(hash_comunidad, keys = this_data$CCAA)
this_data$comunidad <- trimws(this_data$comunidad)
clear(hash_comunidad)

# Hash in occupation info 
# OCUP1 = last occupation in country of departure
# OCUP2 = current occupation
# OCUP3 = occupation in first job upon arriving in Spain
hash_occupation <- hash()
.set(hash_occupation, keys = occupation_codebook$Código, values = occupation_codebook$Ocupaciones)
this_data$occupation1[!is.na(this_data$OCUP1)] <- hash::values(hash_occupation, keys = this_data$OCUP1[!is.na(this_data$OCUP1)])
this_data$occupation2[!is.na(this_data$OCUP2)] <- hash::values(hash_occupation, keys = this_data$OCUP2[!is.na(this_data$OCUP2)])
this_data$occupation3[!is.na(this_data$OCUP3)] <- hash::values(hash_occupation, keys = this_data$OCUP3[!is.na(this_data$OCUP3)])
clear(hash_occupation)

# Hash in sector of occupation info
# SECT1 = sector of last occupation in country of departure
# SECT2 = sector of current occupation
# SECT3 = sector of occupation in first job upon arriving in Spain
hash_sector <- hash()
.set(hash_sector, keys = sector_codebook$CÓDIGOS.QUE.AGRUPA, values = sector_codebook$CLASIFICACIÓN.ACTIVIDADES)
this_data$sector1[!is.na(this_data$SECT1)] <- hash::values(hash_sector, keys = this_data$SECT1[!is.na(this_data$SECT1)])
this_data$sector2[!is.na(this_data$SECT2)] <- hash::values(hash_sector, keys = this_data$SECT2[!is.na(this_data$SECT2)])
this_data$sector3[!is.na(this_data$SECT3)] <- hash::values(hash_sector, keys = this_data$SECT3[!is.na(this_data$SECT3)])
clear(hash_sector)

# Hash in professional situation info
hash_situ <- hash::hash()
.set(hash_situ, keys = situ_codebook$Código, values = situ_codebook$Situación.profesional)
this_data$situation1[!is.na(this_data$SITU1)] <- hash::values(hash_situ, keys = this_data$SITU1[!is.na(this_data$SITU1)])
this_data$situation2[!is.na(this_data$SITU2)] <- hash::values(hash_situ, keys = this_data$SITU2[!is.na(this_data$SITU2)])
this_data$situation3[!is.na(this_data$SITU3)] <- hash::values(hash_situ, keys = this_data$SITU3[!is.na(this_data$SITU3)])
clear(hash_situ)

# Hash in level of studies completed
hash_educ <- hash::hash()
.set(hash_educ, keys = educ_codebook$Codificación, values = educ_codebook$Estudios.terminados.breve)
this_data$education[!is.na(this_data$MNIV)] <- hash::values(hash_educ, keys = this_data$MNIV[!is.na(this_data$MNIV)])
clear(hash_educ)


#### Get information on each house member (relation to reference individual, age, country of origin, gender, 
# Spanish nationality, years since arrival in Spain)
if (get_housemate_demographics == 1) {
  source(paste0(dependency.dir, "/housemate_demographics.R"))
  household_data <- this_data %>% dplyr::select(starts_with("compi"))
  this_data <- this_data %>% mutate(HH_convive = rowSums(household_data == 7, na.rm = T))
  # Add processed variables to the keep var list
  final_vars <- c(final_vars, colnames(this_data)[grep("compi_", colnames(this_data))], "spanish_housemates", "HH_convive")
  # Delete intermediate data frame
  rm(household_data)
}

#### Get information about where siblings live (in same household, in same municipality, in same province, in Spain) and count 
# how many siblings fall into each group
if (get_sibling_residence_info == 1) {
  # If siblings live in Spain, get the province in which they live in
  source(paste0(dependency.dir, "/sibling_prov_of_residence.R"))
  # Data representing where siblings live relative to reference individual for those who don't live in the same household
  siblings_data <- this_data[grep("DVHE", colnames(this_data))]
  # Data representing the sibling's province of residence
  siblings_data_prov <- this_data[grep("HH_province", colnames(this_data))] %>%
    mutate_all(trimws)
  # Count number of siblings that don't live in same household (no_convive) and live in same province (mismo_prov) or same municipality (mismo_mun) or in Spain (en_espana)
  this_data <- this_data %>% mutate(HH_no_convive = rowSums(!is.na(this_data[grep("NOHE", colnames(this_data))]), na.rm = T),
                                    HH_mismo_prov = rowSums(!is.na(siblings_data_prov == this_data$province), na.rm = T) + rowSums(siblings_data == 1, na.rm = T),
                                    HH_mismo_mun = rowSums(siblings_data == 1, na.rm = T),
                                    HH_en_espana = rowSums(siblings_data == 1 | siblings_data == 2, na.rm = T))
  # Add processed variables to the keep var list
  final_vars <- c(final_vars, colnames(this_data)[grep("HH_province", colnames(this_data))], "HH_no_convive", "HH_mismo_prov", "HH_mismo_mun", "HH_en_espana")
  # Delete intermediate data frame
  rm(siblings_data, siblings_data_prov)
}

#### Get information about the reference individual's Spanish abilities ####
if (get_spanish_ability == 1) {
  source(paste0(dependency.dir, "/spanish_ability.R"))
  idioma_data <- idioma_data %>% dplyr::select(COMP, HABLA, LEE, ESCRI) %>% mutate(COMP = mapvalues(idioma_data$COMP, from = c(1, 6), to = c(1, 0)),
                                                                            HABLA = mapvalues(idioma_data$HABLA, from = c(1, 6), to = c(1, 0)),
                                                                            LEE = mapvalues(idioma_data$LEE, from = c(1, 6), to = c(1, 0)),
                                                                            ESCRI = mapvalues(idioma_data$ESCRI, from = c(1, 6), to = c(1, 0)),
                                                                            IDIO = COMP + HABLA + LEE + ESCRI)
  # Group Spanish ability according to self-reported ability (HAESP), or in the absence of that information, by reported
  # ability to comprehend, speak, read, and write in Spanish (3 out of 4 abilities = good level, 2 out of 4 = sufficient, 
  # less than 2 = could improve)
  this_data <- this_data %>% mutate(spanish = ifelse(language == "español", "Nativo", 
                                                     ifelse(HAESP %in% c(1,2) | idioma_data$IDIO >= 3, "Bien", 
                                                            ifelse(HAESP == 3 | idioma_data$IDIO == 2, "Suficiente",
                                                                   ifelse(HAESP == 4 | idioma_data$IDIO < 2, "Necesita mejorar", NA)))))
  final_vars = c(final_vars, "spanish")
  # Add intermediate datasets to the delete list to erase from memory at end of script
  rm(idioma_data)
}

################## Processing of potential explanatory variables ###########################
# Collect data that indicates municipalities lived in/currently living in
muv_data <- this_data %>% dplyr::select(varNames[grepl("MUV_", varNames)]) %>%
  mutate_all(trimws) %>%
  mutate_all(na_if, "")
muv_data$MUV <- muv_data[cbind(1:nrow(muv_data), max.col(!is.na(muv_data), ties.method = 'last'))]

# Collect data that indicates how long was lived/currently living in each municipality
anom_data <- this_data %>% dplyr::select(starts_with("ANOM")) %>%
  mutate_all(na_if, 0)
# Last non-missing entry represents the year in which individual started living in their current municipality of residence
this_data$ANOM <- anom_data[cbind(1:nrow(anom_data), max.col(!is.na(anom_data), ties.method = 'last'))]
# If missing start year of municipal residence, take year that current work contract started
# If not missing start year of municipal residence nor year that current work contract started, then take the latter. Otherwise, take the former
this_data$ANOM_min <- ifelse(is.na(this_data$ANOM) & !is.na(this_data$ANOCOM), this_data$ANOCOM, ifelse(!is.na(this_data$ANOCOM) & (this_data$ANOCOM > this_data$ANOM), this_data$ANOCOM, this_data$ANOM))
# If start of residence/contract year was before 1998, set value to 1998 (since we do not have padron data prior to this)
this_data$ANOM_floor <- ifelse(this_data$ANOM_min < 1998, 1998, this_data$ANOM_min)
# For those who are missing start year of municipal residence and year of current work contract, look to other variables
# If individual has never changed addresses since arriving, then take year of arrival as ANOM_floor
this_data$ANOM_floor[which(is.na(this_data$ANOM_floor))] <- ifelse(this_data$CBVIV == 6, this_data$ALLE, ifelse(!is.na(this_data$province_PR1VI), this_data$province_PR1VI, NA))[which(is.na(this_data$ANOM_floor))]
# If individual

# Merge in population data by province and country of origin
padron_province_data <- read.xlsx(padron_province.dir, sheet = 3, startRow = 8, cols = c(2:29)) 
this_data$sex <- mapvalues(this_data$SEXO, from = c(1, 2), to = c("Male", "Female"))
this_data <- merge(x = this_data, y = padron_province_data[, c("Destination", "Origin", "SEXO", "1998":"2007")], by.x = c("province", "country", "sex"), by.y = c("Destination", "Origin", "SEXO"), all.x = TRUE)
this_data$eth_dens_prov = NA
for (year in c(1998:2007)) {
  these_obs = which(this_data$ANOM_floor == year)
  this_data$eth_dens_prov[these_obs] <-  eval(parse(text = paste0("this_data$`", year, "`")))[these_obs]
}

#### Indicators of social ties after arriving in Spain
this_data$LLCONT <- mapvalues(this_data$LLCONT, from = c(1, 6), to = c(1, 0))
this_data$LLFAM <- mapvalues(this_data$LLFAM, from = c(1, 6), to = c(1, 0))
this_data$LLAMIG <- mapvalues(this_data$LLAMIG, from = c(1, 6), to = c(1, 0))
this_data$LLEMPR <- mapvalues(this_data$LLEMPR, from = c(1, 6), to = c(1, 0))
this_data$LLINTR <- mapvalues(this_data$LLINTR, from = c(1, 6), to = c(1, 0))
this_data$LLCONO <- mapvalues(this_data$LLCONO, from = c(1, 6), to = c(1, 0))
this_data$LLOTR <- mapvalues(this_data$LLOTR, from = c(1, 6), to = c(1, 0))
this_data$DINFAM <- mapvalues(this_data$DINFAM, from = c(1, 6), to = c(1, 0))
this_data$CONFAM <- mapvalues(this_data$CONFAM, from = c(1, 6, 2), to = c(1, 0, NA))

#### Indicators of participation in social, religious, education organizations, NGO's, etc. ####
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

#### Capture all forms of finding a job ####
form_data <- this_data %>% dplyr::select(c("IDQ", grep("FORM", colnames(this_data)))) %>% 
  melt(id.vars = "IDQ") %>% 
  mutate(FORM = ifelse(is.na(variable), NA, ifelse(value == 1, paste0(variable), ""))) %>%
  group_by(IDQ) %>% dplyr::summarize(all_forms = utile.tools::paste0(FORM, collapse = "", na.rm = T)) %>%
  mutate(all_forms = ifelse(all_forms == "", NA, all_forms)) %>%
  mutate(all_forms_any_friends = ifelse(grepl("FORM04", all_forms), "FORM04", all_forms))

############################ Set and specify final processed variables ##########################
study_data <- this_data %>% mutate(#### COVARIATES ####
                                    # Year of arrival in Spain
                                    arrival_spain = ALLE,
                                    # How many complete years in Spain
                                    years = ifelse((2006 - ALLE) > 0, 2006 - ALLE, 0),
                                    # Age
                                    age = EDAD,
                                    # Age at arrival to Spain
                                    age_arrival = this_data$ALLE - this_data$ANAC,
                                    # Age squared
                                    age_sq = EDAD^2,
                                    # Sex
                                    sex = sex,
                                    # Highest level of studies achieved
                                    education = education,
                                    # If he/she has Spanish residence (automatic "yes" if country of origin is an EU member)
                                    work_permit = ifelse((DOCUM %in% c(1, 2, 3) | country_class == "Yes"), 1, ifelse(is.na(DOCUM) & is.na(country_class), NA, 0)),
                                    # If he/she Spanish nationality
                                    spanish_nat = mapvalues(this_data$NESP, from = c(1, 6), to = c(1, 0)),
                                    # Country of origin
                                    country = country,
                                    # Country of origin (shortened for plotting and labelling purposes)
                                    country_short = country_short,
                                    # Current province of residence
                                    province = province,
                                    # Current comunidad autónoma of residence
                                    comunidad = comunidad,
                                    # Current municipality of residence
                                    municipality = muv_data$MUV,
                                    # What year did he/she start living in this municipality
                                    arrival_mun = ANOM,
                                    # Last occupation in country of departure
                                    occupation1 = occupation1,
                                    # Current occupation
                                    occupation2 = occupation2,
                                    # First occupation upon arriving in Spain
                                    occupation3 = occupation3,
                                    # Sector of last occupation in country of departure
                                    sector1 = sector1,
                                    # Sector of current occupation
                                    sector2 = sector2,
                                    # Sector of first occupation upon arriving in Spain
                                    sector3 = sector3,
                                    # Professional situation of last occupation in country of departure
                                    situation1 = situation1,
                                    # Professional situation of current occupation
                                    situation2 = situation2,
                                    # Professional situation of first occupation upon arriving in Spain
                                    situation3 = situation3,
                                    # Whether he/she works part-time (less than 30 hours a week)
                                    part_time = ifelse(HORAS2 < 30, 1, 0),
                                   
                                    #### POPULATION INDICATORS ####
                                    # Did he/she immigrate to Spain for economic reasons?
                                    economic = ifelse(DSLA + FEMP + BEMP > 0, 1, 0), 
                                    # For retirement?
                                    retire = JUB,
                                    # For political reasons?
                                    political = POLI, 
                                    # For religious reasons?
                                    religious = RELI,
                                    # To study?
                                    student = EDUC,
                                    # For the quality of life?
                                    quality = CALV,
                                    # For family reunification?
                                    regroup = REAG,
                                    # For cost of living?
                                    cheaper = COST,
                                    # For the climate?
                                    climate = CLIM,
                                    # Temporarily while transitioning to other location?
                                    temporary = TRNS,
                                    # Other reason?
                                    other = OTRS,
                                    # Was he/she working at the time of departure?
                                    was_working = mapvalues(this_data$TRAB1, from = c(1, 6), to = c(1, 0)),
                                    # Had he/she worked before in the country of departure?
                                    had_worked = mapvalues(this_data$TRANT, from = c(1, 6), to = c(1, 0)),
                                   
                                    #### RESPONSE VARS ####
                                    # Money received monthly for work
                                    month_earn = INGRE,
                                    # Log of money received monthly for work
                                    log_month_earn = log(INGRE),
                                    # Hours worked habitually a month (calculated as hours worked habitually a week times 4)
                                    month_hours = HORAS2*4,
                                    # Estimated hourly earnings
                                    hour_earn = month_earn/month_hours,
                                    # Log of estimated hourly earnings
                                    log_hour_earn = log(month_earn/month_hours),
                                    # If time it took to find first job after immigrating was less than a month
                                    lt_search_1mo = ifelse(TPO1 <= 2, 1, 0),
                                    # Time it took to find first job after immigrating, ordinal scale
                                    lt_search = TPO1,
                                    # How much money has he/she sent back as remittances in the last year
                                    remittance = RECANT,
                                   
                                    #### SOCIAL VARS ####
                                    eth_dens_prov = eth_dens_prov,
                                    log_eth_dens_prov = log(eth_dens_prov),
                                    eth_dens_prov_sq = eth_dens_prov^2,
                                    eth_dens_prov_2006 = `2006`,
                                    log_eth_dens_prov_2006 = log(`2006`),
                                    eth_dens_prov_2006_sq = `2006`^2,
                                    # If decision to immigrate was influenced by someone from home country who had already immigrated to Spain
                                    influenced = mapvalues(this_data$INFLU, from = c(1, 6), to = c(1, 0)),
                                    # If still working at the same job as when he/she first arrived in Spain
                                    first_job_still = mapvalues(this_data$PRIMTR, from = c(1, 6), to = c(1, 0)),
                                    # If he/she has ever been unemployed for a month or more and looking for work
                                    insecurity = mapvalues(this_data$BUSEMP, from = c(1, 6), to = c(1, 0)),
                                    # If before immigrating he/she already had a contract lined up before coming
                                    precontract = mapvalues(this_data$PROPTR, from = c(1, 6, 0), to = c(1, 0, NA)),
                                    # Form of searching for first job upon arriving
                                    form = form_data$all_forms,
                                    # If first job was found through friends or relatives
                                    networked = ifelse(form_data$all_forms_any_friends == "FORM04", 1, 0),
                                    # How many people did he/she know (family, friends, conocidos) that he/she could have turned to for support upon arriving?
                                    support_index = ifelse(!is.na(LLCONT), rowSums(cbind(LLFAM, LLAMIG, LLCONO), na.rm = TRUE), NA),
                                    # Did he/she know anyone that he/she could have turned to for support upon arriving?
                                    any_support = ifelse(support_index > 0, 1, 0),
                                    # If he/she is living for free in the house of a family member or friend
                                    housing_connect = ifelse(TENV %in% c(11, 12, 13, 14), 1, 0),
                                    # How many social/educational/religious groups does he/she participate in?
                                    participation_index = rowSums(cbind(PXAYU, PXDEP, PXEDU, PXREL, PXSOC, PNXONG, PNXPOL, PNXREL, PNXEDU, PNXDEP, PNXSOC, PELEC), na.rm = F),
                                    # If he/she participates in any social/education/religious gropus
                                    participation_indicator = ifelse(participation_index > 0, 1, 0),
                                   
                                    #### ADDITIONAL INFORMATIVE VARIABLES ####
                                    # Number of siblings living
                                    num_siblings = ifelse(TENHH == 6, 0, NUMHH),
                                    # The number of the person in the household selected to be surveyed 
                                    reference_person = NPELEG
)


# Subset to the processed variables of interest
study_data_subset <- study_data %>% dplyr::select(IDQ, NPERS, arrival_spain, arrival_mun, years, age, age_arrival, 
                                                  age_sq, sex, education, work_permit, spanish_nat, country, country_short,
                                                  province, comunidad, occupation1, occupation2, occupation3,sector1, sector2, sector3, 
                                                  situation1, situation2, situation3, municipality, 
                                                  num_siblings, economic, was_working, had_worked, part_time, month_earn, log_month_earn,
                                                  month_hours, hour_earn, log_hour_earn, lt_search_1mo, lt_search, remittance,
                                                  eth_dens_prov_2006, eth_dens_prov, log_eth_dens_prov, eth_dens_prov_sq, influenced,
                                                  first_job_still, insecurity, precontract, form, networked,
                                                  support_index, any_support, housing_connect, participation_index, participation_indicator,
                                                  num_siblings, reference_person, motive_var_prefixes, all_of(final_vars))

######################### Save processed data frames ##############################
saveRDS(study_data, file = paste0(output.dir, "/datos_eni_07_processed.rds"))
saveRDS(study_data_subset, file = paste0(output.dir, "/datos_eni_07_processed_subset.rds"))

######################### Free up memory #########################################
rm(workBook, varNames, positions, codebook, all_data, this_data, form_data, country_codebook, language_codebook, comunidad_codebook, 
   occupation_codebook, province_codebook, situ_codebook, sector_codebook, educ_codebook, hash_comunidad, hash_countries, hash_countries_short, 
   hash_country_class, hash_country_spanish, hash_language, hash_occupation, hash_provinces, hash_situ, hash_sector, hash_educ, 
   padron_province_data, muv_data, anom_data,motive_data, motive_var_prefixes)
