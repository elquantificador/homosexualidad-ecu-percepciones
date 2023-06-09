# Artículo Percepciones de la Homosexualidad en Ecuador
# Junio 2023
# Analisis de la base de datos de matrimonios Ecuador 2022

# Preliminares ------------------------------------------------------------

# Cargar librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Cargar datos

df22 <-
  read.csv('data/EMA_2022.csv', sep = ';')

df21 <-
  read.csv('data/EMA_2021.csv', sep = ';') %>% 
  mutate(cod_pais1 = as.character(cod_pais1))

df20 <-
  read.csv('data/EMA_2020.csv', sep = ';') 

df19 <-
  read.csv('data/EMA_2019.csv') %>%  # Se tuvo que abrir en excel y guardar en UTF8 puesto que habia tildes
  janitor::clean_names()


# Crear un catalogo de meses en español para que R entienda que meses son en numerico

meses <- data.frame(
  mes_esp = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
  mes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

# Crear opciones estéticas

theme_article_pride <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.background = element_blank())


# Preparación de la base de datos ----------------------------------------------------------------

# Preparar la base de datos para tener una lista de matrimonios del mismo sexo y diferente sexo, por mes por año en Ecu. 
# Lo hacemos por cada año puesto que hay cambios substanciales en cada uno de los años para la base de datos

matrimonios22 <-
  df22 %>% 
  left_join(meses, by = c('mes_insc' = 'mes_esp')) %>% 
  mutate(orientacion = if_else(sexo_1 == sexo_2, 'Mismo sexo', 'Diferente sexo'),
         periodo = paste("01", paste(mes, anio_insc, sep = '-')) %>% dmy()) %>% 
  group_by(periodo, orientacion) %>% 
  summarise(num = n()) %>% 
  ungroup()

matrimonios21 <-
  df21 %>% 
  mutate(mes_insc = str_trim(mes_insc)) %>% 
  left_join(meses, by = c('mes_insc' = 'mes_esp')) %>% 
  mutate(orientacion = if_else(sexo_1 == sexo_2, 'Mismo sexo', 'Diferente sexo'),
         periodo = paste("01", paste(mes, anio_insc, sep = '-')) %>% dmy()) %>% 
  group_by(periodo, orientacion) %>% 
  summarise(num = n()) %>% 
  ungroup()

matrimonios20 <-
  df20 %>%
  mutate(orientacion = if_else(sexo_1 == sexo_2, 'Mismo sexo', 'Diferente sexo'),
         periodo = paste("01", paste(mes_insc, anio_insc, sep = '-')) %>% dmy()) %>% 
  group_by(periodo, orientacion) %>% 
  summarise(num = n()) %>% 
  ungroup()

matrimonios19 <-
  df19 %>%
  mutate(orientacion = if_else(sexo_1 == sexo_2, 'Mismo sexo', 'Diferente sexo'),
         periodo = paste("01", paste(mes_insc, anio_insc, sep = '-')) %>% dmy()) %>% 
  group_by(periodo, orientacion) %>% 
  summarise(num = n()) %>% 
  ungroup()
  
# Create the master dataset

matrimonios <-
  matrimonios19 %>%
  bind_rows(matrimonios20) %>%
  bind_rows(matrimonios21) %>% 
  bind_rows(matrimonios22)
  
# Análisis ----------------------------------------------------------------

# Crear una gráfica de serie de tiempo para matrimonios

grafico_matrimonios <-
  matrimonios %>% 
  # filter(orientacion == 'Mismo sexo') %>% 
  ggplot(aes(periodo, log(num), colour = orientacion)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_date(date_breaks = '6 months', 
               date_labels = '%b-%y') +
  geom_vline(xintercept = as.numeric(as.Date('2016-04-01')),
             colour = 'blue', 
             linetype = 'dashed')+
  theme_article_pride +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

grafico_matrimonios

