#################################################################################
# Nombre del programa:	2_explore_pop.R,
# Autor:              	Madeline Swarr
# Ultima modificacion: 	29 marzo 2022
#
# Descripcion: 
#	Utiliza los datos de las variables elegidas por el programa 1_process_raw 
# y hace un poco de exploracion de datos, restringiendo a la población de interés.
#
#
# Entrada:
#     - this_data
# Salida: 
#     - this_pop
#  
# TO DO:
# - Make femp_data processing more efficient
# - Seguir explorando
# - Concretar las restricciones
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

##################### Characterize independent variables ########################
#### Sibling data
m <- data.frame(convive = c(min(this_data$HH_convive, na.rm = T), quantile(this_data$HH_convive, c(.25, .5, .75), na.rm = T), max(this_data$HH_convive, na.rm = T)))

#### Civic engagement and participation
participation_data <- this_data[, grep(c("PNX|PX|PELEC"), colnames(this_data))] %>% select(-PXNC) %>% filter(complete.cases(.))
no <- colSums(participation_data==0, na.rm = T)
yes <- colSums(participation_data==1, na.rm = T)
total <- nrow(participation_data)
percent <- (yes/total)*100

civic_engagement_stats <- data.frame(yes,
                                     no,
                                     total,
                                     percent)

participation_data_all <- this_data$participation_indicator[!is.na(this_data$participation_index)]
no <- sum(participation_data_all==0)
yes <- sum(participation_data_all==1)
total <- length(participation_data_all)
percent <- (yes/total)*100

civic_engagement_stats_all <- data.frame(yes,
                                         no,
                                         total,
                                         percent)


###### Social network support
support_data <- this_data %>% select(LLCONT, DINFAM, CONFAM, networked, housing_connect)
no <- colSums(support_data==0, na.rm = T)
yes <- colSums(support_data==1, na.rm = T)
total <- no + yes
percent <- (yes/total)*100

support_stats <- data.frame(yes,
                            no,
                            total,
                            percent)


support_data_all <- data.frame(support_index = rowSums(support_data, na.rm = T))
no <- sum(support_data_all==0)
yes <- sum(support_data_all>0)
total <- no + yes
percent <- (yes/total)*100

support_stats_all <- data.frame(yes,
                                no,
                                total,
                                percent)

##################### Explore population restriction attrition #################
restriction_data <- this_data %>% select(economic, ethnic, ALLE01) %>% 
  mutate(arrival_1998 = ifelse(ALLE01 >= 1998, 1, 0),
         arrival_2000 = ifelse(ALLE01 >= 2000, 1, 0),
         arrival_2001 = ifelse(ALLE01 >= 2001, 1, 0),
         arrival_2002 = ifelse(ALLE01 >= 2002, 1, 0),
         arrival_2003 = ifelse(ALLE01 >= 2003, 1, 0),
         arrival_2004 = ifelse(ALLE01 >= 2004, 1, 0),
         economic_1998 = ifelse(economic == 1 & arrival_1998 == 1, 1, 0),
         economic_2003 = ifelse(economic == 1 & arrival_2003 == 1, 1, 0)) %>% select(-ALLE01)

no <- colSums(restriction_data==0, na.rm = T)
yes <- colSums(restriction_data ==1, na.rm = T)
total <- no + yes
percent <- (yes/total)*100

restriction_stats <- data.frame(yes,
                                no,
                                total,
                                percent)

##################### Characterize the entire population ########################
## Which countries are most represented?
countries <- table(this_data$country) %>% as.data.frame(responseName = "Freq") %>% dplyr::rename(country = Var1) %>%
  arrange(desc(Freq))
these_top_countries <- countries %>% filter(Freq > 250)
this_pop <- this_data %>% filter(country %in% these_top_countries$country & !is.na(sector2) & ethnic == 1)
order_countries <- these_top_countries$country[these_top_countries$country %in% levels(factor(this_pop$country))] %>% as.vector()

## Which sectors are most represented by country? Take the top 4 sectors represented for each country
sectors <- ddply(this_pop, c("sector2", "country"), summarise, grp.count=n()) %>% group_by(country) %>%
  slice_max(order_by = grp.count, n = 4) 

this_pop <- inner_join(x = this_pop, y = sectors, by = c("sector2", "country"), keep = FALSE)

#################### Explore response variables ##########################

########### Facetted by country ##############
## Monthly earnings
mu_month_earn <- ddply(this_pop, c("sector2", "country"), summarise, grp.mean=mean(month_earn, na.rm = T)) %>% mutate(across(country, factor, levels=order_countries))
mu_month_earn_all <- ddply(this_pop, c("country"), summarise, grp.mean=mean(month_earn, na.rm = T)) %>% mutate(sector2 = "All") %>% mutate(across(country, factor, levels=order_countries))

## Monthly earnings distribution
gg_month_earn <- this_pop %>% mutate(across(country, factor, levels=order_countries)) %>%
  ggplot(aes(month_earn, fill = sector2, color = sector2)) + geom_histogram(stat = "bin", bins = 40) +
  labs(title = "Counts of immigrants by monthly earnings reported \n Top 4 sectors by country",
       x = "Monthly earnings") +
  geom_vline(data=mu_month_earn_all, aes(xintercept=grp.mean), color = "black", linetype = "solid") +
  geom_vline(data=mu_month_earn, aes(xintercept=grp.mean, color=sector2),
             linetype="dashed") +
  theme(legend.position="right") + 
  facet_grid(country~., scales = "free") +
  scale_x_continuous(limits = c(0, 3000))
gg_month_earn

## Hourly earnings
mu_hour_earn <- ddply(this_pop, c("sector2", "country"), summarise, grp.mean=mean(hour_earn, na.rm = T)) %>% mutate(across(country, factor, levels=order_countries))
mu_hour_earn_all <- ddply(this_pop, c("country"), summarise, grp.mean=mean(hour_earn, na.rm = T)) %>% mutate(sector2 = "All") %>% mutate(across(country, factor, levels=order_countries))

## Hourly earnings distribution
gg_hour_earn <- this_pop %>% mutate(across(country, factor, levels=order_countries)) %>%
  ggplot(aes(hour_earn, fill = sector2, color = sector2)) + geom_histogram(stat = "bin", bins = 40) +
  labs(title = "Counts of immigrants by hourly earnings reported \n Top 4 sectors by country",
       x = "Estimated hourly earnings") +
  geom_vline(data=mu_hour_earn, aes(xintercept=grp.mean, color=sector2),
             linetype="dashed") +
  geom_vline(data=mu_hour_earn_all, aes(xintercept=grp.mean), color = "black", linetype = "solid") +
  theme(legend.position="right") + 
  facet_grid(country~., scales = "free") +
  scale_x_continuous(limits = c(0, 25))
gg_hour_earn

## Log monthly earnings
mu_log_month_earn <- ddply(this_pop, c("sector2", "country"), summarise, grp.mean=mean(log_month_earn, na.rm = T))

## Log monthly earnings distribution
gg_log_monthly_earn <- this_pop %>% 
  ggplot(aes(log_month_earn, fill = sector2, color = sector2)) + geom_histogram(stat = "bin", bins = 40) +
  labs(title = "Counts of immigrants by log(monthly earnings) \n Top 4 sectors by country",
       x = "Log of monthly earnings") +
  geom_vline(data=mu_log_month_earn, aes(xintercept=grp.mean, color=sector2),
             linetype="solid") +
  theme(legend.position="right") + 
  facet_grid(country~., scales = "free") +
  scale_x_continuous(limits = c(4.5,8.5))
gg_log_monthly_earn

########### Faceted by "ethnic" vs. "non-ethnic" ##############
## Which sectors are most represented by country? Take the top 4 sectors represented for each country
sectors_ethnic <- ddply(this_data, c("sector2", "ethnic"), summarise, grp.count=n()) %>% group_by(ethnic) %>%
  slice_max(order_by = grp.count, n = 8) 

this_pop_ethnic <- inner_join(x = this_data, y = sectors_ethnic, by = c("sector2", "ethnic"), keep = FALSE) %>% filter(!is.na(any_support))

# Facet label names for variables
ethnic.labs <- c("Ethnic origin", "Non-ethnic origin")
names(ethnic.labs) <- c(1,0)
network.labs <- c("1st job found through friends", "1st job NOT found thru friends")
names(network.labs) <- c(1, 0)
support.labs <- c("Tenía alguien a quien dirigirse cuando llegó", "No tenía alguien a quien dirigirse cuando llegó")
names(support.labs) <- c(1, 0)

## Monthly earnings
mu_month_earn_ethnic <- ddply(this_pop_ethnic, c("sector2", "ethnic", "any_support"), summarise, grp.mean=mean(month_earn, na.rm = T))
mu_month_earn_ethnic_all <- ddply(this_pop_ethnic, c("ethnic", "any_support"), summarise, grp.mean=mean(month_earn, na.rm = T)) %>% mutate(sector2 = "All") 

## Monthly earnings distribution
gg_month_earn_ethnic <- this_pop_ethnic %>%
  ggplot(aes(month_earn, fill = sector2, color = sector2)) + geom_histogram(stat = "bin", bins = 40) +
  labs(title = "Counts of immigrants by monthly earnings reported \n Top 7 sectors by country",
       x = "Monthly earnings") +
  geom_vline(data=mu_month_earn_ethnic, aes(xintercept=grp.mean, color=sector2),
             linetype="dashed") +
  geom_vline(data=mu_month_earn_ethnic_all, aes(xintercept=grp.mean), color = "black", linetype = "solid") +
  theme(legend.position="bottom") + 
  facet_grid(any_support~ethnic, scales = "free", labeller = labeller(ethnic = ethnic.labs, any_support = support.labs)) +
  scale_x_continuous(limits = c(0, 3000))
gg_month_earn_ethnic


## Hourly earnings
mu_hour_earn_ethnic <- ddply(this_pop_ethnic, c("sector2", "ethnic", "any_support"), summarise, grp.mean=mean(hour_earn, na.rm = T))
mu_hour_earn_ethnic_all <- ddply(this_pop_ethnic, c("ethnic", "any_support"), summarise, grp.mean=mean(hour_earn, na.rm = T)) %>% mutate(sector2 = "All") 

## Hourly earnings distribution
gg_hour_earn_ethnic <- this_pop_ethnic %>% 
  ggplot(aes(hour_earn, fill = sector2, color = sector2)) + geom_histogram(stat = "bin", bins = 40) +
  labs(title = "Counts of immigrants by estimated hourly earnings \n Top 7 sectors by country",
       x = "Estimated hourly earnings") +
  geom_vline(data=mu_hour_earn_ethnic, aes(xintercept=grp.mean, color=sector2),
             linetype="dashed") +
  geom_vline(data=mu_hour_earn_ethnic_all, aes(xintercept=grp.mean), color = "black", linetype = "solid") +
  theme(legend.position="bottom") + 
  facet_grid(any_support~ethnic, scales = "free", labeller = labeller(ethnic = ethnic.labs, any_support = support.labs)) +
  scale_x_continuous(limits = c(0, 25))
gg_hour_earn_ethnic 
