
# Instalando las librerías ------------------------------------------------



install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, jsonlite, leaflet)



# Creamos la variable original del JSON -----------------------------------

ds_raw <- fromJSON("bquxjob_17dd9345_1852b27c607.json") %>% view()


# limpiamos el dataset ----------------------------------------------------

ds_f <- ds_raw %>% clean_names() %>% type_convert(locale=locale(decimal_mark = ","))%>% as_tibble()
 