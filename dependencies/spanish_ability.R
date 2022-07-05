###########################################################################
# Program name: spanish_ability.R
# Author: Madeline Swarr
# Last modified: 02 July 2022
# Description: Get the individual's reported Spanish level in speaking, reading, 
# writing, and comprehension.
###########################################################################

# Collect data on Spanish language abilities
idioma_data <- this_data %>% dplyr::select(starts_with(c("IDIO","COMP","HABLA","LEE","ESCRI"))) %>% mutate(COMP = NA,
                                                                                                    HABLA = NA,
                                                                                                    LEE = NA,
                                                                                                    ESCRI = NA)
# Six potential languages to collect ability information on
for (i in 1:6) {
  idio = eval(parse(text = paste0("idioma_data$IDIO", i)))
  # idio == 101 means Spanish
  idioma_data$COMP[which(idio == 101)] <- eval(parse(text = paste0("idioma_data$COMP", i)))[which(idio == 101)]
  idioma_data$HABLA[which(idio == 101)] <- eval(parse(text = paste0("idioma_data$HABLA", i)))[which(idio == 101)]
  idioma_data$LEE[which(idio == 101)] <- eval(parse(text = paste0("idioma_data$LEE", i)))[which(idio == 101)]
  idioma_data$ESCRI[which(idio == 101)] <- eval(parse(text = paste0("idioma_data$ESCRI", i)))[which(idio == 101)]
}
