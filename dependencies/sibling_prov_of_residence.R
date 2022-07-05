###########################################################################
# Program name: sibling_prov_of_residence.R
# Author: Madeline Swarr
# Last modified: 02 July 2022
# Description: Hash in province name in which individual's siblings live, if 
# they live in Spain as well.
###########################################################################

# Clean up data relating to siblings province of residence for those siblings that don't live in the same household
siblings_data_prov <- this_data[grep("PRHE", colnames(this_data))] %>%
  mutate_all(trimws) %>%
  mutate_all(na_if, "") %>%
  mutate_all(na_if, "00")

# Hash in province of residence in Spain info
hash_provinces <- hash()
.set(hash_provinces, keys = province_codebook$CODIGO_PROVINCIA, values = province_codebook$NOMBRE_PROVINCIA)
# In which province live the individuals siblings, if they live in Spain as well
this_data$HH_province1[!is.na(siblings_data_prov$PRHE01)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE01[!is.na(siblings_data_prov$PRHE01)])
this_data$HH_province2[!is.na(siblings_data_prov$PRHE02)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE02[!is.na(siblings_data_prov$PRHE02)])
this_data$HH_province3[!is.na(siblings_data_prov$PRHE03)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE03[!is.na(siblings_data_prov$PRHE03)])
this_data$HH_province4[!is.na(siblings_data_prov$PRHE04)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE04[!is.na(siblings_data_prov$PRHE04)])
this_data$HH_province5[!is.na(siblings_data_prov$PRHE05)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE05[!is.na(siblings_data_prov$PRHE05)])
this_data$HH_province6[!is.na(siblings_data_prov$PRHE06)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE06[!is.na(siblings_data_prov$PRHE06)])
this_data$HH_province7[!is.na(siblings_data_prov$PRHE07)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE07[!is.na(siblings_data_prov$PRHE07)])
this_data$HH_province8[!is.na(siblings_data_prov$PRHE08)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE08[!is.na(siblings_data_prov$PRHE08)])
this_data$HH_province9[!is.na(siblings_data_prov$PRHE09)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE09[!is.na(siblings_data_prov$PRHE09)])
this_data$HH_province10[!is.na(siblings_data_prov$PRHE10)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE10[!is.na(siblings_data_prov$PRHE10)])
this_data$HH_province11[!is.na(siblings_data_prov$PRHE11)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE11[!is.na(siblings_data_prov$PRHE11)])
this_data$HH_province12[!is.na(siblings_data_prov$PRHE12)] <- hash::values(hash_provinces, keys = siblings_data_prov$PRHE12[!is.na(siblings_data_prov$PRHE12)])
clear(hash_provinces)

