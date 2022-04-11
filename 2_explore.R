#################################################################################
# Nombre del programa:	2_explore.R,
# Autor:              	Madeline Swarr
# Ultima modificacion: 	11 febrero 2022
#
# Descripcion: 
#	Utiliza los datos de las variables elegidas por el programa 1_process_raw 
# y hace un poco de exploracion de datos.
#
#
# Entrada:
#     - this_data
# Salida: 
#     - unos gráficos
#  
# TO DO:
# - Seguir explorando
# - Definir restricciones
# - Copiar la población final a Excel
#################################################################################

########### ESPECIFICAMOS EL DIRECTORIO DEL CODIGO ################
head.dir <- head_dir

# Descargar libraries 
dependency.dir <- paste0(head.dir, "/Pipeline/dependencies")
source(paste0(dependency.dir, "/get_packages.R"))
get_packages(c("plyr", "tidyverse", "hash", "reshape2", "stringr", "ggplot2"))

####################    Asignacion de parametros    #######################
# Recogemos la ruta del script que se esta ejecutando
input.dir <- paste0(head.dir, "/Output/1_processed")
output.dir <- paste0(head.dir, "/Output/2_explored")
if (!dir.exists(output.dir)) {
  dir.create(output.dir)
}

this_data <- readRDS(paste0(input.dir, "/datos_eni_07_subset.rds"))

####################    Exploramos    #######################
# De que paises vienen los inmigrantes
countries <- table(this_data$country) %>% as.data.frame(responseName = "Freq") %>% dplyr::rename(country = Var1) %>%
  arrange(desc(Freq))
these_top_countries <- countries %>% filter(Freq > 200)

# Distribución de edades al llegar en España
age_upon_arrival <- this_data %>% filter(country %in% these_top_countries$country)  %>% 
  ggplot(aes(age_arrival)) + geom_boxplot() + facet_wrap(~country)

# Histogram de anos en los que llegaron separado por pais y proporción que encontró su primer trabajo por amigos/parientes
year_of_arrival_networking <- this_data %>% filter(country %in% these_top_countries$country) %>% 
  ggplot(aes(ALLE01, fill=networking)) + geom_histogram(stat = "count") + facet_wrap(~country, scales = "free") +
  labs(title = "Counts of immigrants by country of origin who arrived to Spain by year",
       x = "Year of arrival") +
  scale_fill_discrete(name = "First job found through \n friends or relatives?") + 
  theme(legend.position = "bottom")

# Histogram de anos en los que llegaron separado por pais y proporción que participa en un grupo social
year_of_arrival_participation <- this_data %>% filter(country %in% these_top_countries$country) %>% 
  ggplot(aes(ALLE01, fill=participation_indicator)) + geom_histogram(stat = "count") + facet_wrap(~country, scales = "free") +
  labs(title = "Counts of immigrants by country of origin who arrived to Spain by year",
       x = "Year of arrival") +
  scale_fill_discrete(name = "Participates in at least \n one social group currently?") + 
  theme(legend.position = "bottom")

# Histogram de las maneras en las que encontraron su primer trabajo
first_job_networking <- this_data %>% filter(country %in% these_top_countries$country) %>%
  ggplot(aes(form, color = form)) + geom_bar(aes(y = ifelse(..count.. > 5, ..count.., NA)))  +
  labs(title = "") + facet_wrap(~country) + 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(legend.position = "bottom")

# Serie temporal del número bruto de inmigrantes que llegaron por año y la proporción que consiguió su primer trabajo por algún amigo
migration_patterns <- this_data %>% filter(country %in% these_top_countries$country) %>% 
  group_by(country, ALLE01) %>% dplyr::summarise(immigrants = n(), network = sum(networking == "Sí"), influence = sum(influencia == "Sí")) 

patterns_of_migration <- migration_patterns %>% filter(ALLE01 > 1950) %>%
  ungroup() %>% melt(id.vars = c("country", "ALLE01")) %>%
  ggplot(aes(x = ALLE01, y = value, color = variable)) +
  geom_line() + facet_wrap(~country, scales = "free") + 
  theme(legend.position = "bottom")










