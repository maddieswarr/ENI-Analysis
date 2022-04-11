#################################################################################
# Nombre del programa:	3_model.R,
# Autor:              	Madeline Swarr
# Ultima modificacion: 	17 febrero 2022
#
# Descripcion: 
#	Ejecutar regresiones preliminarias
#  
### OBSERVACIONES
# - Para el que llegó sin ninguna propuesta previa de trabajo, tener ya una red 
# social cuando llegó es asociado con más probabilidad de que consiguiese su primer trabajo dentro 
# de 30 días desde cuando empezó a buscarlo
# - Tener alguien al qué pudo dirigirse cuando llegó a España es asociado con menos salario actual
# - Venir a España por la influencia de algún conocido que ya se emigró a España es asociado
# con más probabilidad de que consiguiese su primer trabajo más de 30 días de cuando empezó a 
# buscarlo. 
# - "work_permit" tiene un gran efecto en el bienestar laboral del inmigrante
# 
### TO DO 
# - Habilidades de idioma
# - Modelar "strong and weak ties"
# - Construir un mejor indice de conectividad?
# - Ordinal regression para tiempo en conseguir primer trabajo
# - Clustering analysis by country/province of origin?
#
### VARIABLES ALTERNATIVAS DE RESPUESTA 
# - Consumpción (alquiler)
# - Movimientos laborales (cambios de trabajo)
# - PARO/inseguridad de trabajo (tiempo máximo sin trabajo)
# - Filtrar a los que siguen en su primer trabajo desde su llegada en España
#
### OTROS HILOS DE INVESTIGACIÓN?
# - Qué factores contribuyen al tiempo que tarda en conseguir una autorización de trabajar?
# - Impacto en conectividad social y los flujos de inmigración?
#
#################################################################################

########### ESPECIFICAMOS EL DIRECTORIO DEL CODIGO ################
head.dir <- head_dir

# Descargar libraries 
dependency.dir <- paste0(head.dir, "/Pipeline/dependencies")
source(paste0(dependency.dir, "/get_packages.R"))
get_packages(c("MASS", "foreign", "Hmisc", "reshape2", "dplyr"))

####################    Asignacion de parametros    #######################
# Recogemos la ruta del script que se esta ejecutando
input.dir <- paste0(head.dir, "/Output/1_processed")
output.dir <- paste0(head.dir, "/Output/3_modelled")
if (!dir.exists(output.dir)) {
  dir.create(output.dir)
}

this_data <- readRDS(paste0(input.dir, "/datos_eni_07_subset.rds"))

##############################   Regression   ############################################

######### 1a. Efecto de social networks sobre tiempo que tardó en conseguir primer trabajo 
# Filtrar a los que llegaron sin trabajo pero trabajaban antes de enmigrar
this_data_1a <- this_data %>% filter(precontract == "No" & previously_working == "Sí")
model_1a <- glm(lt1mo_search ~ networking + country_short + years +  age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_1a, family = binomial())
summary(model_1a)

######### 1b. Efecto de social networks sobre tiempo que tardó en conseguir primer trabajo para los que vinieron recientemente (menos "recall bias")
# Filtrar a los que llegaron sin trabajo pero trabajaban antes de enmigrar y que no llegaron antes de 2004
this_data_1b <- this_data %>% filter(precontract == "No" & previously_working == "Sí" & ALLE01 >= 2004)
model_1b <- glm(lt1mo_search ~ networking + country_short + years + age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_1b, family = binomial())
summary(model_1b)

######### 1c. Efecto de social networks sobre tiempo que tardó en conseguir primer trabajo interactuado con pais de origen
# Filtrar a los que llegaron sin trabajo pero trabajaban antes de enmigrar
this_data_filter <- this_data %>% filter(precontract == "No" & previously_working == "Sí")
# De que paises vienen los inmigrantes
countries <- table(this_data_filter$country_short) %>% as.data.frame(responseName = "Freq") %>% dplyr::rename(country_short = Var1) %>%
  arrange(desc(Freq))
these_top_countries <- countries %>% filter(Freq > 100)
this_data_1c <- this_data_filter %>% filter(country_short %in% these_top_countries$country_short)
model_1c <- glm(lt1mo_search ~ networking + country_short + networking*country_short + years +  age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_1c, family = binomial())
summary(model_1c)

######### 1d. Efecto de venir a España por la influencia de algún conocido que ya se emigró a España sobre tiempo que tardó en conseguir primer trabajo
# Filtrar a los que llegaron sin trabajo pero trabajaban antes de enmigrar y que no llegaron antes de 2004
this_data_1d <- this_data %>% filter(precontract == "No" & previously_working == "Sí")
model_1d <- glm(lt1mo_search ~ influencia + country_short + years + age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_1d, family = binomial())
summary(model_1d)

######### 2a. Efecto de tipo de networks sobre tiempo que tardó en conseguir primer trabajo 
# Filtrar a los que llegaron sin trabajo pero trabajaban antes de enmigrar y que encontraron su primer trabajo por solo una uníca manera
this_data_2a <- this_data %>% filter(precontract == "No" & previously_working == "Sí" & (nchar(form) == 6))
model_2a <- glm(lt1mo_search ~ form_any_friends + country_short +  age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_2a, family = binomial())
summary(model_2a)

######### 3a. Efecto de tener un "red social" en salario actual
# Filtrar a los que llegaron sin trabajo
this_data_3a <- this_data %>% filter(precontract == "No")
model_3a <- lm(lwage ~ support_index + country_short + years +  age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_3a)
summary(model_3a)

######### 3b. Efecto de tener un "red social" en salario actual filtrado a los que vinieron recientemente para reducir tiempo entre llegada y ahora
# Filtrar a los que llegaron sin trabajo y que no llegaron antes de 2004
this_data_3b <- this_data %>% filter(precontract == "No" & ALLE01 >= 2004)
model_3b <- lm(lwage ~ support_index + country_short + years +  age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_3b)
summary(model_3b)

######### 4a. Efecto de conseguir primer trabajo por amigo/pariente en salario actual
# Filtrar a los que llegaron sin trabajo
this_data_4a <- this_data %>% filter(precontract == "No")
model_4a <- lm(lwage ~ networking + country_short + years +  age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_4a)
summary(model_4a)

######### 4b. Efecto de conseguir primer trabajo por amigo/pariente en salario actual filtrado a los que vinieron recientemente para reducir tiempo entre llegada y ahora
# Filtrar a los que llegaron sin trabajo y que no llegaron antes de 2004
this_data_4b <- this_data %>% filter(precontract == "No" & ALLE01 >= 2004)
model_4b <- lm(lwage ~ networking + country_short + years +  age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_4b)
summary(model_4b)

######### 4c. Efecto de conseguir primer trabajo por amigo/pariente en salario actual para los que siguen en ese trabajo
# Filtrar a los que llegaron sin trabajo y siguen en el primer trabajo que consiguieron tras llegar
this_data_4c <- this_data %>% filter(precontract == "No" & first_job_still == "Sí")
model_4c <- lm(lwage ~ networking + country_short + years +  age + sex + education + comunidad + OCUP1 + work_permit, data = this_data_4c)
summary(model_4c)


