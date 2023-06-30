# Artículo Percepciones de la Homosexualidad en Ecuador
# Junio 2023
# Analisis de la base de datos de matrimonios Ecuador 2022

# Preliminares ------------------------------------------------------------

# Cargar librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")

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
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        axis.line = element_line(colour = 'grey60'),
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
  
# Crear la base maestra

matrimonios <-
  matrimonios19 %>%
  bind_rows(matrimonios20) %>%
  bind_rows(matrimonios21) %>% 
  bind_rows(matrimonios22)
  
# Análisis ----------------------------------------------------------------

grafico_matrimonios <-
  matrimonios %>% 
  filter(orientacion == 'Mismo sexo',
         periodo %>%  between(as.Date('2019-07-01'),
                              as.Date('2022-12-01'))) %>%  # Se ignoran matrimonios del mismo sexo registrados antes de la legalizacion
  mutate(cumsum = cumsum(num)) %>% 
  ggplot(aes(periodo, cumsum)) +
  geom_line(colour = '#FFC0CB') +
  geom_point(color = 'black') + 
  scale_x_date(date_breaks = '3 months', 
               date_labels = '%b-%y') +
  scale_y_continuous(breaks = seq(0,1500, 250)) +
  labs(x = '',
       y = 'Número de matrimonios entre el mismo sexo',
       title = 'Matrimonios del mismo sexo en Ecuador 2019-2022',
       subtitle = 'Número de matrimonios del mismo sexo acumulados por cada mes',
       caption = str_wrap('Nota: El número de matrimonios del mismo sexo se calcula como el número de matrimonios donde ambos contrayentes reportan ser del mismo sexo. Fuente: INEC.', 210)) +
  theme_article_pride +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(face = 'bold'))

grafico_matrimonios

ggsave("figures/grafico_matrimonios.png", 
       device = "png", 
       width = 12.5, 
       height = 8.5, 
       dpi = 900)

# Crear un gráfico de número de 

# grafico_matrimonios_comp <-
#   matrimonios %>% 
#   filter(periodo %>%  between(as.Date('2019-07-01'), as.Date('2022-12-01'))) %>%
#   ggplot(aes(periodo, log(num + 1), colour = orientacion)) +
#   geom_line() +
#   geom_point(size = 1) +
#   scale_x_date(date_breaks = '3 months', 
#                date_labels = '%b-%y') +
#   geom_vline(xintercept = as.numeric(as.Date('2016-04-01')),
#              colour = 'blue', 
#              linetype = 'dashed') +
#   labs(x = '',
#        y = 'Ln(x+1) del número de matrimonios',
#        colour = 'Tipo de matrimonio',
#        title = 'Comparación entre matrimonios de diferente sexo y mismo sexo') +
#   theme_article_pride +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#         legend.position = c(0.8,0.7),
#         plot.title = element_text(face = 'bold')) 

# grafico_matrimonios + grafico_matrimonios_comp + 
#   plot_layout(ncol = 2) + 
#   plot_annotation(title = 'Matrimonio Igualitario en Ecuador 2019-2022',
#                   subtitle = 'Datos del Registro de Matrimonios y Divorcios INEC/Registro Civil',
#                   caption = str_wrap('Nota: El número de matrimonios del mismos sexo se calcula como el número de matrimonios donde ambos contrayentes reportan ser del mismo sexo. El panel derecho compara la transformacion ln(x+1) del número de matrimonios.
#                   Fuente: INEC.', 150),
#                   theme = theme(plot.caption = element_text(hjust = 0, face = 'italic'),
#                                 plot.title = element_text(face = 'bold'),
#                                 text = element_text(size = 15)))

