# Artículo Percepciones de la Homosexualidad en Ecuador
# Junio 2023
# Análisis de la base de datos AmericasBarometer de LAPOP

# Preliminares ------------------------------------------------------------

# Cargar librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")

# Descargar y cargar base de datos

source('code/lapop_download.R') # Ejecutar script de descarga

df <- # Asignar a base de datos
  read.csv('data/ab_04_19.csv')

# Crear diseño muestral mediante survey

ab_des<-svydesign(ids = ~ upm,
                  strata = ~ estratopri, 
                  weights = ~ weight1500, 
                  nest = TRUE,
                  na.action = 'na.exclude',
                  data = df)

# Diseño de imágenes

quant_blue<-'#09A4CC'
quant_grey<-'#5C7C94'
quant_orange<-'#F8754D'
quant_red<-'#F44D54'

# Creamos un theme para los gráficos de ggplot2

theme_article_pride <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
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

# Serie de tiempo de aprobación de derechos políticos de homosexuales

caption_grafo1<-
  'Las cifras representan el % de personas que respondieron puntuaciones del 6 al 10 en la pregunta, donde 1 es extrema desaprobación y 10 es extrema aprobación. Las barras representan intervalos de confianza del 95% con errores ajustados por diseño muestral multietapa estratificado. Las encuestas fueron realizadas de enero a marzo de cada año, excepto la ronda 2016, realizada de noviembre 2016 a enero 2017. Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org. '

grafico1 <- 
  ggplot(hsmxl_pol_time, aes(x = as.factor(year), y = hmsxl_pol))+
  geom_col(fill = quant_blue,
           color = 'black', 
           linewidth = 0.7)+
  geom_errorbar(aes(ymin = hmsxl_pol - 1.96*se,
                    ymax = hmsxl_pol + 1.96*se),
                width = 0.3)+
  geom_text(aes(label = round(hmsxl_pol, 4)*100),
            size = 4,
            vjust = 6.5)+
  labs(x = '',
       y = '% de aprobación',
       title = 'Aprobación del matrimonio igualitario en Ecuador',
       subtitle = '¿Con qué firmeza aprueba o desaprueba que las personas homosexuales puedan postularse para cargos públicos?',
       caption = str_wrap(caption_grafo1, 205))+
  theme_article_pride +
  theme(axis.ticks = element_blank())

ggsave("figures/grafico1_lapop_pride.png", device = "png", width = 12.5, height = 7, dpi = 900)


caption_grafo2<-
  'Las cifras representan el % de personas que respondieron puntuaciones del 6 al 10 en la pregunta, donde 1 es extrema desaprobación y 10 es extrema aprobación. Las barras representan intervalos de confianza del 95% con errores ajustados por diseño muestral multietapa estratificado. Las encuestas fueron realizadas de enero a marzo de cada año, excepto la ronda 2016, realizada de noviembre 2016 a enero 2017. Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org. '

grafico2 <- 
  ggplot(hsmxl_marr_time, aes(x = as.factor(year), y = hmsxl_marr))+
  geom_col(fill = quant_blue,
           color = 'black', 
           linewidth = 0.7)+
  geom_errorbar(aes(ymin = hmsxl_marr - 1.96*se,
                    ymax = hmsxl_marr + 1.96*se),
                width = 0.3)+
  geom_text(aes(label = round(hmsxl_marr, 4)*100),
            size = 4,
            vjust = 6.5)+
  labs(x = '',
       y = '% de aprobación',
       title = 'Aprobación del derecho a postularse a cargos políticos de la comunidad homosexual en Ecuador',
       subtitle = '¿Con qué firmeza aprueba o desaprueba que las personas homosexuales puedan postularse para cargos públicos?',
       caption = str_wrap(caption_grafo1, 205))+
  theme_article_pride +
  theme(axis.ticks = element_blank())

ggsave("figures/grafico2_lapop_pride.png", device = "png", width = 12.5, height = 7, dpi = 900)



