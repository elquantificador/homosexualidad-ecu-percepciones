# Artículo Percepciones de la Homosexualidad en Ecuador
# Junio 2023
# Autores: Daniel Sánchez y Alonso Quijano Ruiz

# Descarga de la base de datos AmericasBarometer de LAPOP

# Preliminares ------------------------------------------------------------

# Cargar librerías

if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")

# Cargar datos preparados desde el repositorio de GitHub del LIDE para esta base de datos

url <- 'https://raw.githubusercontent.com/laboratoriolide/americas-barometer/main/output/csv/ab_04_09.csv'

download.file(url, here('data/ab_04_19.csv'))

