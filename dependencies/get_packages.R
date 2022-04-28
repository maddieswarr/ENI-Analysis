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
    if(this_package == "XLConnect") {options(java.parameters = "-Xmx1024m")}
    library(this_package, character.only = TRUE)
  }
  
}

