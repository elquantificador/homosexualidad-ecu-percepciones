# Este sctipt analiza las percepciones de los ecuatorianos sobre la homosexualidad usando la 
# Encuesta Mundial de Valores (WVS). Los datos utilizados están almacenados en el GitHub del LIDE
# https://github.com/laboratoriolide/WVSEcuador. Para consultar la fuente original, visita la 
# página de la WVSA www.worldvaluessurvey.org.

# Cargar librerías ----
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(labelled)) install.packages("labelled", repos = "http://cran.us.r-project.org")

# Cargar datos ----
WVSEcuador <- read_dta("https://github.com/laboratoriolide/WVSEcuador/raw/main/data/WVSEcuador.dta")

# Etiquetas de las variables
WVSEcuador_labels <- var_label(WVSEcuador)

# Procesamiento ----
# Año de la encuesta
WVSEcuador$anio <- as_factor(WVSEcuador$S020)

# Sexo de la persona encuestada
WVSEcuador$sexo <- as_factor(WVSEcuador$X001)
WVSEcuador$sexo <- recode_factor(WVSEcuador$sexo, Male = "Hombre", Female = "Mujer")

# Tema de El Quantificador ----
theme_quanti <- function() {
  theme_classic() +
    theme(plot.title = element_text(color = "grey20", size = 14),
          plot.subtitle = element_text(color = "grey30"),
          plot.caption = element_text(color = "grey30", face = 'italic'),
          axis.ticks.x = element_blank(),
          legend.background = element_rect(fill = "white", size=0.5, linetype = "solid", color = "grey30"))
}

# A124_09: Preferiría no tener a un homosexual como vecino ----
# Calcular la media y error estándar por sexo
A124_09 <- WVSEcuador %>% group_by(anio, sexo) %>% 
  summarize(mean = mean(A124_09 == 1, na.rm = TRUE), se = sd(A124_09 == 1, na.rm = TRUE)/sqrt(n()))

# Gráfico 2013
A124_09_2013 <- A124_09 %>% filter (anio == 2013) %>%
  ggplot(aes(x = sexo, y = mean, fill = sexo)) + geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.2) +
  geom_text(aes(label = percent(mean, accuracy = 0.1)), color ="grey20", size = 4, vjust = -3.5) +
  labs(title = "2013", x = "", y = "") + 
  scale_y_continuous(limits = c(0,0.5)) +
  theme_quanti() + scale_fill_manual(values = c("#647A8F", "#FFAC8E")) +
  theme(legend.position  = "none")

# Gráfico 2018
A124_09_2018 <- A124_09 %>% filter (anio == 2018) %>%
  ggplot(aes(x = sexo, y = mean, fill = sexo)) + geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.2) +
  geom_text(aes(label = percent(mean, accuracy = 0.1)), color ="grey20", size = 4, vjust = -3.5) +
  labs(title = "2018", x = "", y = "") + 
  scale_y_continuous(limits = c(0,0.5)) +
  theme_quanti() + scale_fill_manual(values = c("#647A8F", "#FFAC8E"))

# Notas al pie del gráfico
caption_A124_09 <- "Las cifras representan el porcentaje de personas que respondieron que preferiría no tener a un homosexual como vecino. Las barras representan intervalos de confianza del 95%. Fuente: Encuesta Mundial de Valores (WVS) rondas 2013 y 2018. Elaborado por: Laboratorio de Investigación para el Desarrollo del Ecuador (LIDE)."

# Combinar los dos gráficos
plot_WVS_veci_homo <- 
  A124_09_2013 + 
  A124_09_2018 + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Percepciones de los ecuatorianos sobre la homosexualidad",
                  subtitle = "¿Desearía no tener a un homosexual como vecino?",
                  caption = str_wrap(caption_A124_09, 130),
                  theme = theme(plot.title = element_text(hjust = 0, color="grey20", face = "bold", size=14),
                                plot.caption = element_text(hjust = 0, color="grey30", face = 'italic'))); plot_WVS_veci_homo

# Guardar el gráfico
ggsave("figures/plot_WVS_veci_homo.png", plot = plot_WVS_veci_homo, device = "png", width = 8, height = 5, dpi = 900)

# D081: Las parejas homosexuales son tan buenos padres como otras parejas ----
# Renombrar los niveles de la variable
WVSEcuador$D081 <- as_factor(WVSEcuador$D081)
WVSEcuador$D081 <- droplevels(WVSEcuador$D081, exclude = c("Missing; Not available", "Not asked", "Not applicable", "No answer"))
levels(WVSEcuador$D081) <- c("No sabe", "Totalmente de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Totalmente en desacuerdo")

# Calcular la frecuencia relativa (Esta pregunta solo se hizo en el 2018)
D081 <- WVSEcuador %>% filter (anio == 2018, !is.na(D081)) %>% group_by(anio, D081) %>% 
  summarize(n = n()) %>% group_by(anio) %>% mutate(freq = n/sum(n))

# Notas al pie del gráfico
caption_D081 <- 'Las cifras representan los porcentajes de percepción de los ecuatorianos a la afirmación "Las parejas homosexuales son tan buenos padres" Fuente: Encuesta Mundial de Valores (WVS) ronda 2018. Elaborado por: Laboratorio de Investigación para el Desarrollo del Ecuador (LIDE).'

# Gráfico 2018
plot_WVS_padres_homo <- D081 %>%
  ggplot(aes(y = freq, x = D081, fill = D081)) + geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Percepción sobre las parejas homoparentales",
       subtitle = "¿Las parejas homosexuales son tan buenos padres como otras parejas?", x = "", y = "",
       caption = str_wrap(caption_D081, 200)) + theme_quanti() + theme(legend.position = "none") +
  scale_fill_manual(values = c("black", "#16607A", "#7595A6", "#C7CDD1", "#CC724D", "#A53816")) +
  geom_text(aes(label = percent(freq, accuracy = 0.1)), stat = "identity", vjust = -0.5, color = "black", size = 3) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, color="grey30")); plot_WVS_padres_homo

# Guardar el gráfico
ggsave("figures/plot_WVS_padres_homo.png", plot = plot_WVS_padres_homo, device = "png", width = 12.5, height = 7, dpi = 1200)
