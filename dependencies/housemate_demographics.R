################################################
# Program name: housemate_demographics.R
# Author: Madeline Swarr
# Last modified: 02 July 2022
# Description: Get demographic information on each housemember (relation to reference individual,
# age, country of origin, gender, Spanish nationality, years since arrival in Spain) as well as number
# of house members with Spanish nationality
#################################################

this_data <- this_data %>% mutate(spanish_housemates = ifelse(NPERS > 1, 0, NA))
this_data <- this_data %>% mutate(dependents = ifelse(NPERS > 1, 0, NA))
this_data$spanish_partner = 0
this_data$partner_convive = 0
this_data[, paste0("compi_relation", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_age", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_country", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_sex", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_nesp", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_dnac", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_ane", str_pad(1:15, 2, pad = "0"))] = NA
this_data[, paste0("compi_arrival_spain", str_pad(1:15, 2, pad = "0"))] = NA

# Vector of potential number of household members (max 15)
compis = c(1:15)

# NPELEG represents the household member of reference (takes on value in [1 to 15])
for (n in levels(factor(this_data$NPELEG))){
  these_compis = compis[!(compis %in% n)]
  for (h in these_compis) {
    this_compi_relation = eval(parse(text = paste0("this_data$P", str_pad(n, 2, pad = "0"), "_", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_age = eval(parse(text = paste0("this_data$EDAD", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_country = eval(parse(text = paste0("this_data$PNAC", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_sex = eval(parse(text = paste0("this_data$SEXO", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_nesp = eval(parse(text = paste0("this_data$NESP", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_dnac = eval(parse(text = paste0("this_data$DNAC", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_ane = eval(parse(text = paste0("this_data$ANE", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_arrival_spain = eval(parse(text = paste0("this_data$ALLE", str_pad(h, 2, pad = "0"))))[which(this_data$NPELEG == n)]
    this_compi_spanish_housemates = this_data$spanish_housemates[which(this_data$NPELEG == n)]
    this_compi_dependents = this_data$dependents[which(this_data$NPELEG == n)]
    this_compi_spanish_partner = this_data$spanish_partner[which(this_data$NPELEG == n)]
    this_compi_partner_convive = this_data$partner_convive[which(this_data$NPELEG == n)]
    
    # Determine if an individual shares a house with a working age, Spanish national by birth (country == 108) and count the 
    # number of housemates that meet this requirement
    these_compis_country_age = data.frame(country = this_compi_country, 
                                           age = this_compi_age,
                                           relation = this_compi_relation,
                                           spanish_housemates = this_compi_spanish_housemates,
                                           dependents = this_compi_dependents,
                                           spanish_partner = this_compi_spanish_partner,
                                           partner_convive = this_compi_partner_convive) %>% 
      mutate(spanish_housemates = ifelse(!is.na(country) & !is.na(relation) & relation %in% c(11, 14) & country == "108", spanish_housemates + 1, spanish_housemates),
             dependents = ifelse(!is.na(age) & !is.na(relation) & age < 16 & relation == 5, dependents + 1, dependents),
             spanish_partner = ifelse(!is.na(country) & !is.na(relation) & relation == 1 & country == "108", spanish_partner + 1, spanish_partner),
             partner_convive = ifelse(!is.na(relation) & relation == 1, partner_convive + 1, partner_convive))
    
    this_data[which(this_data$NPELEG == n),]$spanish_housemates <- these_compis_country_age$spanish_housemates
    this_data[which(this_data$NPELEG == n),]$dependents <- these_compis_country_age$dependents
    this_data[which(this_data$NPELEG == n),]$spanish_partner <- these_compis_country_age$spanish_partner
    this_data[which(this_data$NPELEG == n),]$partner_convive <- these_compis_country_age$partner_convive
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

# Delete intermediate data frame
rm(these_compis_country_age, this_compi_relation, this_compi_age, this_compi_country, this_compi_sex, this_compi_ane, 
   this_compi_arrival_spain, this_compi_dependents, this_compi_dnac, this_compi_nesp, this_compi_spanish_housemates,
   this_compi_spanish_partner, this_compi_partner_convive, these_compis)
