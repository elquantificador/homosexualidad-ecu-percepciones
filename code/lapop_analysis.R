# Artículo Percepciones de la Homosexualidad en Ecuador
# Junio 2023
# Análisis de la base de datos AmericasBarometer de LAPOP

# Preliminares ------------------------------------------------------------

# Cargar librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")

# Cargar datos preparados desde el repositorio de GitHub del LIDE para esta base de datos

url <- 'https://raw.githubusercontent.com/laboratoriolide/americas-barometer/main/output/csv/ab_04_09.csv'

download.file(url, here('data/ab_04_19.csv'))


df <- # Asignar a base de datos
  read.csv('data/ab_04_19.csv')

# Crear diseño muestral mediante survey

ab_des<-svydesign(ids = ~ upm,
                  strata = ~ estratopri, 
                  weights = ~ weight1500, 
                  nest = TRUE,
                  na.action = 'na.exclude',
                  data = df)

# Creamos un theme para los gráficos de ggplot2

theme_article_pride <-
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "grey20"),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey30", hjust = 0, face = 'italic'),
        legend.background = element_blank())

# Análisis ----------------------------------------------------------------

# Utilizar la pregunta d5: "Y ahora, cambiando el tema, y pensando en los homosexuales. ¿Con
# qué firmeza aprueba o desaprueba que estas personas puedan
# postularse para cargos públicos?"

ab_des$variables <-
  ab_des$variables %>% 
  mutate(hmsxl_pol = if_else(d5 >= 6, 1,0),
         hmsxl_marr = if_else(d6 >= 6,1,0))

# Tabulación con pesos de muestra mediante svyby

hsmxl_pol_time <-
  svyby(~ hmsxl_pol,
        ~ year,
        design = ab_des,
        svymean,
        na.rm = T,
        keep.names = F) %>%
  filter(hmsxl_pol != 0)

hsmxl_marr_time <-
  svyby(~ hmsxl_marr,
        ~ year,
        design = ab_des,
        svymean,
        na.rm = T,
        keep.names = F) %>% 
  filter(hmsxl_marr != 0)

caption_grafo1<-
  'Las cifras representan el % de personas que respondieron puntuaciones del 6 al 10 en la pregunta, donde 1 es extrema desaprobación y 10 es extrema aprobación. Las barras representan intervalos de confianza del 95% con errores ajustados por diseño muestral multietapa estratificado. 
  Las encuestas fueron realizadas de enero a marzo de cada año, excepto la ronda 2016, realizada de noviembre 2016 a enero 2017. La encuesta del 2019 se realizó antes de junio 2019, cuando el matrimonio igualitario en Ecuador fue aceptado por la Corte Constitucional. 
Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org.'

grafico2 <- 
  ggplot(hsmxl_marr_time, aes(x = as.factor(year), y = hmsxl_marr, fill = as.factor(year)))+
  geom_col(fill = "#647A8F",
           linewidth = 0.7,
           width = 0.5)+
  geom_errorbar(aes(ymin = hmsxl_marr - 1.96*se,
                    ymax = hmsxl_marr + 1.96*se),
                width = 0.3)+
  geom_text(aes(label = percent(hmsxl_marr, accuracy = 0.1)),
            size = 4,
            vjust = -4.5) +
  scale_y_continuous(limits = c(0, 0.4)) +
  labs(x = '',
       y = '',
       title = 'Aprobación del matrimonio igualitario en Ecuador',
       subtitle = '¿Aprueba que las parejas del mismo sexo puedan tener el derecho a casarse?',
       caption = str_wrap(caption_grafo1, 140)) +
  guides(fill = F) +
  theme_article_pride +
  theme(plot.title = element_text(face = 'bold'),
        plot.caption = element_text(size = 8)); grafico2

ggsave("figures/grafico_lapop_matrimonio.png",plot = grafico2, 
       device = "png", 
       width = 8, 
       height = 6, 
       dpi = 1200)

# Serie de tiempo de aprobación de derechos políticos de homosexuales

caption_grafo2<-
  'Las cifras representan el % de personas que respondieron puntuaciones del 6 al 10 en la pregunta, donde 1 es extrema desaprobación y 10 es extrema aprobación. Las barras representan intervalos de confianza del 95% con errores ajustados por diseño muestral multietapa estratificado. Las encuestas fueron realizadas de enero a marzo de cada año, excepto la ronda 2016, realizada de noviembre 2016 a enero 2017. Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org.'

grafico1 <- 
  ggplot(hsmxl_pol_time, aes(x = as.factor(year), y = hmsxl_pol, fill = as.factor(year)))+
  geom_col(fill = "#647A8F",
           width = 0.5,
           linewidth = 0.7)+
  geom_errorbar(aes(ymin = hmsxl_pol - 1.96*se,
                    ymax = hmsxl_pol + 1.96*se),
                width = 0.3)+
  geom_text(aes(label = percent(hmsxl_pol, accuracy = 0.1)),
            size = 4,
            vjust = -3.2)+
  scale_y_continuous(limits = c(0,0.5)) +
  labs(x = '',
       y = '',
       title = 'Aprobación del derecho a postularse a cargos políticos de la comunidad homosexual en Ecuador',
       subtitle = '¿Aprueba o desaprueba que las personas homosexuales puedan postularse para cargos públicos?',
       caption = str_wrap(caption_grafo2, 160)) +
  guides(fill = F) +
  theme_article_pride +
  theme(plot.title = element_text(face = 'bold')); grafico1

ggsave("figures/grafico_lapop_politicos.png", device = "png", width = 12, height = 6, dpi = 1200)