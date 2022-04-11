#################################################################################
# Nombre del programa:	get_packages.R,
# Autor:              	Madeline Swarr
# Ultima modificacion: 	13 febrero 2022
#
# Descripcion: 
#	Función para comprobar existencia de packages y si no existen, se los descarga.
#################################################################################

get_packages <- function(packages){
  n = length(packages)
  for (i in 1:n) {
    this_package = packages[i]
    if(!this_package %in% installed.packages()){
      install.packages(this_package)
    }
    library(this_package, character.only = TRUE)
  }
  
}

