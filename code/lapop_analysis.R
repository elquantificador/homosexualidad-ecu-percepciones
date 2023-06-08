# Artículo Percepciones de la Homosexualidad en Ecuador
# Junio 2023
# Análisis de la base de datos AmericasBarometer de LAPOP

# Preliminares ------------------------------------------------------------

# Cargar librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")

# Descargar y cargar base de datos

source('code/lapop_download.R')

df <-
  read.csv('data/ab_04_19.csv')

