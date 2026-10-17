# Ficha del artículo

Completa esta ficha al crear el repositorio y mantenla al día. Es el único
lugar con datos propios del artículo; el README es igual en todos los
repositorios.

## Identificación

- **Título:** Homosexualidad: ¿qué piensan los ecuatorianos?
- **Autoría:** Alonso Quijano Ruiz y Daniel Sánchez Pazmiño
- **Fecha de publicación:** 2023-07-02
- **Artículo publicado:** https://elquantificador.org/post/valores/2023-07-02-homosexualidad-que-piensan-los-ecuatorianos/
- **Categoría:** Valores y comportamiento
- **Edición:** Febrero - Agosto 2023

## Estado

Publicado el 2023-07-02. Datos tomados del sitio; no queda nada pendiente para
cerrar el artículo.

## Pregunta y alcance

El artículo describe qué opinan los ecuatorianos sobre la homosexualidad:
rechazo a tener un vecino homosexual y percepción de las parejas
homoparentales (Encuesta Mundial de Valores, 2013 y 2018), aprobación del
matrimonio igualitario y del derecho a postularse a cargos públicos
(Barómetro de las Américas de LAPOP) y matrimonios del mismo sexo inscritos
entre julio de 2019 y diciembre de 2022 (INEC y Registro Civil). La cobertura
es nacional, y la unidad de análisis es la persona encuestada, salvo en los
registros de matrimonios, donde es el matrimonio inscrito.

## Requisitos

- R: versión no documentada en el repositorio.
- Paquetes de R: haven, here, janitor, labelled, lubridate, patchwork, scales,
  stringr, survey, tidyverse. Los scripts los instalan si faltan. Para
  compilar `report/report.Rmd` también se necesitan knitr y rmarkdown, y
  opcionalmente LaTeX para el PDF.

## Fuentes de datos

| Fuente | Institución | Periodo | Acceso | Script |
|---|---|---|---|---|
| Barómetro de las Américas (AmericasBarometer), base de Ecuador preparada por LIDE | LAPOP, Vanderbilt University | Rondas 2004 a 2019 (el artículo usa las preguntas sobre matrimonio igualitario y cargos públicos) | Descarga automática desde `laboratoriolide/americas-barometer` en GitHub; copia local en `data/ab_04_19.csv`. Fuente original: www.LapopSurveys.org | `code/lapop_analysis.R` |
| Encuesta Mundial de Valores (WVS), base de Ecuador | World Values Survey Association | Rondas 2013 y 2018 | Descarga automática desde `laboratoriolide/WVSEcuador` en GitHub (archivo `.dta`). Fuente original: www.worldvaluessurvey.org | `code/wvs_analysis.R` |
| Estadística de Matrimonios (EMA), registros de matrimonios | INEC y Registro Civil | Inscripciones 2019 a 2022 (el gráfico usa julio de 2019 a diciembre de 2022) | Manual: los cuatro CSV están incluidos en `data/` (`EMA_2019.csv` a `EMA_2022.csv`); el repositorio no documenta la descarga original | `code/matrimonios_analysis.R` |

Los archivos `EMA_*.csv` se leen desde `data/` sin descarga automática. Si
necesitas obtenerlos de nuevo, ubícalos en esa misma carpeta con los mismos
nombres; el de 2019 se guardó en UTF-8 a mano porque tenía tildes mal
codificadas (según un comentario del script). El enlace oficial de INEC para
estos registros no aparece en el repositorio; el sitio de INEC para
matrimonios y divorcios es https://www.ecuadorencifras.gob.ec/matrimonios-divorcios/.

## Salidas

- `figures/plot_WVS_veci_homo.png`: porcentaje que no querría un vecino
  homosexual, por sexo, WVS 2013 y 2018.
- `figures/plot_WVS_padres_homo.png`: acuerdo con que las parejas homosexuales
  son tan buenos padres como otras parejas, WVS 2018.
- `figures/grafico_lapop_matrimonio.png`: aprobación del matrimonio igualitario
  por año, LAPOP.
- `figures/grafico_lapop_politicos.png`: aprobación del derecho de las personas
  homosexuales a postularse a cargos públicos, LAPOP.
- `figures/grafico_matrimonios.png`: matrimonios del mismo sexo acumulados por
  mes, julio de 2019 a diciembre de 2022.
- `figures/grafico1_lapop_pride.png` y `figures/grafico2_lapop_pride.png`:
  figuras antiguas presentes en el repositorio; ningún script de `code/` las
  genera.
- `report/report.html` y `report/report.docx`: versiones renderizadas del
  texto del artículo.

## Cómo replicar en este repositorio

Este repositorio todavía no tiene `MASTER.R`; los pasos salen de su README
anterior y de los scripts.

1. Clona o descarga el repositorio y abre `quantificador-lgbt.Rproj` para que
   las rutas relativas (`data/` y `figures/`) apunten a la raíz del proyecto.
2. Ejecuta `code/lapop_analysis.R`. Descarga la base de LAPOP a
   `data/ab_04_19.csv` (sobrescribe la copia local) y genera
   `figures/grafico_lapop_matrimonio.png` y
   `figures/grafico_lapop_politicos.png`.
3. Ejecuta `code/matrimonios_analysis.R`. Lee `data/EMA_2019.csv` a
   `data/EMA_2022.csv` (ya incluidos) y genera
   `figures/grafico_matrimonios.png`.
4. Ejecuta `code/wvs_analysis.R`. Lee la base de WVS desde GitHub, por lo que
   necesita conexión a internet, y genera `figures/plot_WVS_veci_homo.png` y
   `figures/plot_WVS_padres_homo.png`.
5. Compila `report/report.Rmd` para reproducir el texto del artículo; usa las
   figuras de `figures/`.

El orden de los pasos 2 a 4 sigue el README anterior; los scripts son
independientes entre sí.

## README anterior

Texto del README de este repositorio antes de adoptar el README estándar,
conservado sin cambios de contenido para no perder información.

#### Homosexualidad: ¿qué piensan los ecuatorianos?
Este repositorio contiene el código y los archivos necesarios para reproducir el análisis del artículo de El Quantificador: [Homosexualidad: ¿qué piensan los ecuatorianos?](https://elquantificador.blog/post/valores/2023-07-02-homosexualidad-qu-piensan-los-ecuatorianos/)

#### Requisitos
- R (se recomienda usar RStudio).
- Paquetes de R que se instalarán automáticamente al ejecutar los scripts.
- Opcionalmente LaTeX para compilar el artículo en PDF.

#### Cómo reproducir el análisis
- Clona o descarga este repositorio.
- Ejecuta code/lapop_analysis.R, code/matrimonios_analysis.R y code/wvs_analysis.R.
- Finalmente, puedes ejecutar `report/report.Rmd` para reproducir el texto del artículo.

Para más información, revisa los perfiles de GitHub de los autores [dsanchezp18](https://github.com/elquantificador/homosexualidad-ecu-percepciones/commits?author=dsanchezp18) y [aquijanoruiz](https://github.com/aquijanoruiz).
