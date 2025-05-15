# ---- Librerias ----

library(ggplot2)     # Visualizaciones
library(dplyr)       # Manipulación de datos
library(readxl)      # Lectura de archivos Excel
library(knitr)       # Tablas y reportes
library(gridExtra)   # Manejo de gráficos
library(grid)        # Soporte para gráficos
library(tidyr)       # Transformaciones de datos
library(gtable)      # Tablas gráficas
library(kableExtra)  # Tablas mejoradas para reportes
library(reshape2)
library(purrr)      # Para usar pmap_chr()
library(tibble)

# ---- Archivo de Datos ---- 

ruta_archivo <- "C:/Users/lucia/OneDrive/Escritorio/Asignaturas/Cuarto/TFG/LuciaCalderon_DatosTFG.xlsx"

# ---- ----------- ANÁLISIS DESCRIPTIVO ----------- ---- 

# ---- Funciones Comunes ---- 

# Función para Identificar Outliers por Columna
identificar_outliers <- function(datos, columna) {
  # Calcular cuartiles y rango intercuartil (IQR)
  Q1 <- quantile(datos[[columna]], 0.25, na.rm = TRUE)  # Primer cuartil
  Q3 <- quantile(datos[[columna]], 0.75, na.rm = TRUE)  # Tercer cuartil
  IQR <- Q3 - Q1                                        # Rango intercuartil
  
  # Determinar límites inferior y superior
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  
  # Filtrar los valores atípicos
  outliers <- datos %>% 
    filter(.[[columna]] < limite_inferior | .[[columna]] > limite_superior)
  
  # Retornar los datos atípicos encontrados
  return(outliers)
}

# Recordar sobre la función:
# - `na.rm = TRUE` asegura que se ignoren los valores faltantes al calcular los cuartiles.
# - La salida es un subconjunto de la tabla original que contiene solo los outliers.


# ---- --------- SERIES TEMPORALES ANUALES ------------ ---- 


# ---- PRECIO MERCADO DIARIO ---- 


# 1. Cargar la Hoja del Excel
datosA1 <- read_excel(ruta_archivo, sheet = "A_Pmercdiario_max, min, avg")

# 2. Resumen de los Datos
str(datosA1)  # Estructura del dataset

# Resumen estadístico por columnas
resumen_max <- summary(datosA1$`Precio máximo`)
resumen_min <- summary(datosA1$`Precio mínimo`)
resumen_avg <- summary(datosA1$`Precio medio aritmético`)
desviacion_max <- sd(datosA1$`Precio máximo`, na.rm = TRUE)
desviacion_min <- sd(datosA1$`Precio mínimo`, na.rm = TRUE)
desviacion_avg <- sd(datosA1$`Precio medio aritmético`, na.rm = TRUE)

# Crear un data frame con las estadísticas clave
resumen_df <- data.frame(
  "Mínimo" = c(resumen_max[1], resumen_min[1], resumen_avg[1]),
  "1er Cuartil" = c(resumen_max[2], resumen_min[2], resumen_avg[2]),
  "Mediana" = c(resumen_max[3], resumen_min[3], resumen_avg[3]),
  "Media" = c(resumen_max[4], resumen_min[4], resumen_avg[4]),
  "3er Cuartil" = c(resumen_max[5], resumen_min[5], resumen_avg[5]),
  "Máximo" = c(resumen_max[6], resumen_min[6], resumen_avg[6]),
  "Desv Típica" = c(desviacion_max, desviacion_min, desviacion_avg)  # Añadir columna con desviación típica
)

rownames(resumen_df) <- c("Precio Máximo", "Precio Mínimo", "Precio Promedio")

# Crear tabla con kableExtra
tabla_resumen_A1 <- kable(resumen_df, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resumen_df), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_resumen_A1

# 3. Datos Faltantes
total_na <- sum(is.na(datosA1))  # Total de valores faltantes
total_na
faltantes_por_col <- colSums(is.na(datosA1))  # Valores faltantes por columna
faltantes_por_col

# 4. Identificación de atípicos
outliers_precio_max <- identificar_outliers(datosA1, "Precio máximo")
outliers_precio_min <- identificar_outliers(datosA1, "Precio mínimo")
outliers_precio_avg <- identificar_outliers(datosA1, "Precio medio aritmético")

# Crear una lista con los datos de outliers y sus nombres
lista_outliers_A1 <- list(
  "Máximo" = outliers_precio_max,
  "Mínimo" = outliers_precio_min,
  "Promedio" = outliers_precio_avg
)

# Combinar los resultados de los outliers en un solo data frame
outliers_combined_A1 <- do.call(rbind, lapply(names(lista_outliers_A1), function(var_name) {
  df <- lista_outliers_A1[[var_name]]  # Seleccionar el data frame de la lista
  if (nrow(df) > 0) {  # Verificar si hay filas en el data frame
    df$Variable <- var_name  # Añadir columna con el nombre de la variable
  }
  return(df)
}))

# Tabla de Outliers
tabla_outliers_combined_A1 <- kable(outliers_combined_A1, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(outliers_combined_A1), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_outliers_combined_A1

# 5. Boxplots
tituloA1 <- textGrob("Precio Mercado Diario - Anual(2007 - 2024)", gp = gpar(fontsize = 14, fontface = "bold"))

boxplot_max <- ggplot(datosA1, aes(x = "", y = `Precio máximo`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Máximos") +
  theme_minimal()

boxplot_min <- ggplot(datosA1, aes(x = "", y = `Precio mínimo`)) +
  geom_boxplot(fill = "royalblue") +
  labs(title = "Mínimos") +
  theme_minimal()

boxplot_avg <- ggplot(datosA1, aes(x = "", y = `Precio medio aritmético`)) +
  geom_boxplot(fill = "dodgerblue") +
  labs(title = "Promedio") +
  theme_minimal()

boxplotA1 <- grid.arrange(tituloA1, boxplot_min, boxplot_avg, boxplot_max, 
                          ncol = 3, 
                          layout_matrix = rbind(c(1, 1, 1), c(2, 3, 4)), 
                          heights = c(0.1, 1))

boxplotA1

# 6. Representación de la Serie Temporal
grafico <- ggplot(datosA1, aes(x = Año)) +
  geom_line(aes(y = `Precio máximo`, color = "Máximo"), size = 1) +
  geom_point(aes(y = `Precio máximo`, color = "Máximo"), size = 2) +
  geom_line(aes(y = `Precio mínimo`, color = "Mínimo"), size = 1) +
  geom_point(aes(y = `Precio mínimo`, color = "Mínimo"), size = 2) +
  geom_line(aes(y = `Precio medio aritmético`, color = "Promedio"), size = 1) +
  geom_point(aes(y = `Precio medio aritmético`, color = "Promedio"), size = 2) +
  labs(x = "Año", y = "Precio (€)", color = "Leyenda") +
  scale_color_manual(values = c("Máximo" = "lightblue", "Mínimo" = "royalblue", "Promedio" = "dodgerblue")) +
  scale_x_continuous(breaks = seq(min(datosA1$Año), max(datosA1$Año), by = 1)) +
  theme_minimal()

grafico_sert_A1 <- grid.arrange(tituloA1, grafico, ncol = 1, heights = c(0.1, 0.7))

grafico_sert_A1





# ---- PRECIO FINAL DEMANDA ---- 


# 1. Cargar la Hoja del Excel
datosA2 <- read_excel(ruta_archivo, sheet = "A_Pfdemanda")

# 2. Resumen de los Datos
str(datosA2)  # Revisar estructura del dataset

# Resumen estadístico de las columnas
resumen_precio <- summary(datosA2$`Precio final medio (EUR/MWh)`)
resumen_energia <- summary(datosA2$`Energía (GWh)`)
sd_precio <- sd(datosA2$`Precio final medio (EUR/MWh)`, na.rm = TRUE)
sd_energia <- sd(datosA2$`Energía (GWh)`, na.rm = TRUE)

# Crear un data frame con las estadísticas clave
resumen_df_A2 <- data.frame(
  "Mínimo" = c(resumen_precio[1], resumen_energia[1]),
  "1er Cuartil" = c(resumen_precio[2], resumen_energia[2]),
  "Mediana" = c(resumen_precio[3], resumen_energia[3]),
  "Media" = c(resumen_precio[4], resumen_energia[4]),
  "3er Cuartil" = c(resumen_precio[5], resumen_energia[5]),
  "Máximo" = c(resumen_precio[6], resumen_energia[6]),
  "Desviación Típica" = c(sd_precio, sd_energia) # Añadir la desviación típica
)

rownames(resumen_df_A2) <- c("Precio Final Medio", "Energía")

# Crear tabla con kableExtra
tabla_resumen_A2 <- kable(resumen_df_A2, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resumen_df_A2), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_resumen_A2

# 3. Datos Faltantes
total_na_A2 <- sum(is.na(datosA2))  # Total de valores faltantes
total_na_A2
faltantes_por_col_A2 <- colSums(is.na(datosA2))  # Valores faltantes por columna
faltantes_por_col_A2

# 4. Datos Atípicos
outliers_precio_final <- identificar_outliers(datosA2, "Precio final medio (EUR/MWh)")
outliers_energia <- identificar_outliers(datosA2, "Energía (GWh)")

# Crear una lista con los datos de outliers y sus nombres
lista_outliers_A2 <- list(
  "Precio Final Medio" = outliers_precio_final,
  "Energía" = outliers_energia
)

# Combinar los resultados de los outliers en un solo data frame
outliers_combined_A2 <- do.call(rbind, lapply(names(lista_outliers_A2), function(var_name) {
  df <- lista_outliers_A2[[var_name]]  # Acceder directamente al data frame
  if (nrow(df) > 0) {
    df$Variable <- var_name  # Añadir la columna indicando la variable
  }
  return(df)
}))

# Crear tabla con kableExtra si hay outliers
if (!is.null(outliers_combined_A2) && nrow(outliers_combined_A2) > 0) {
  tabla_outliers_combined_A2 <- kable(outliers_combined_A2, format = "html", row.names = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    column_spec(1:ncol(outliers_combined_A2), color = "black") %>%
    row_spec(0, background = "deepskyblue", color = "white")
  tabla_outliers_combined_A2
} else {
  print("No hay valores atípicos en las variables seleccionadas.")
}


# 5. Boxplots
tituloA2 <- textGrob("Precio Final Demanda y Energía suministrada - Anuales (2009 - 2024)", gp = gpar(fontsize = 14, fontface = "bold"))

boxplot_precio <- ggplot(datosA2, aes(x = "", y = `Precio final medio (EUR/MWh)`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Precio Final Medio") +
  theme_minimal()

boxplot_energia <- ggplot(datosA2, aes(x = "", y = `Energía (GWh)`)) +
  geom_boxplot(fill = "royalblue") +
  labs(title = "Energía") +
  theme_minimal()

boxplotA2 <- grid.arrange(tituloA2, boxplot_precio, boxplot_energia,
                          ncol = 2, 
                          layout_matrix = rbind(c(1, 1), c(2, 3)),
                          heights = c(0.1, 1))

# 6. Representación de la Serie Temporal
factor_escala <- max(datosA2$`Precio final medio (EUR/MWh)`) / max(datosA2$`Energía (GWh)`)

grafico_A2 <- ggplot(datosA2, aes(x = Año)) +
  geom_line(aes(y = `Precio final medio (EUR/MWh)`, color = "Precio Final Medio"), size = 1) +
  geom_point(aes(y = `Precio final medio (EUR/MWh)`, color = "Precio Final Medio"), size = 2) +
  geom_line(aes(y = `Energía (GWh)` * factor_escala, color = "Energía"), size = 1) +
  geom_point(aes(y = `Energía (GWh)` * factor_escala, color = "Energía"), size = 2) +
  labs(x = "Año", y = "Precio Final Medio (EUR/MWh)", color = "Leyenda") +
  scale_color_manual(values = c("Precio Final Medio" = "lightblue", "Energía" = "royalblue")) +
  scale_y_continuous(
    name = "Precio Final Medio (EUR/MWh)", 
    sec.axis = sec_axis(~ . / factor_escala, name = "Energía (GWh)")
  ) +
  scale_x_continuous(breaks = seq(min(datosA2$Año), max(datosA2$Año), by = 1)) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "royalblue"),
    axis.title.y.left = element_text(color = "lightblue")
  )

grafico_sert_A2 <- grid.arrange(tituloA2, grafico_A2, ncol = 1, heights = c(0.1, 0.7))

# Tabla de datos transpuesta
datos_transpuestos_A2 <- t(datosA2 %>% select(`Precio final medio (EUR/MWh)`, `Energía (GWh)`))
datos_transpuestos_A2 <- as.data.frame(datos_transpuestos_A2)

colnames(datos_transpuestos_A2) <- datosA2$Año
rownames(datos_transpuestos_A2) <- c("Precio Final Medio", "Energía")

tabla_datos_A2 <- kable(datos_transpuestos_A2, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(datos_transpuestos_A2), color = "black") %>%  
  row_spec(0, background = "deepskyblue", color = "white")

tabla_datos_A2





# ---- PRODUCCIÓN DE ENERGIA POR TECNOLOGIAS ----


# 1. Cargar la Hoja del Excel
datosA3 <- read_excel(ruta_archivo, sheet = "A_Tecnologia")

# 2. Generar el Resumen de los Datos
# Crear un resumen detallado por tecnología
resumen_df_Tecnologia <- datosA3 %>%
  summarise(across(-Año, list(
    Mínimo = ~ min(., na.rm = TRUE),
    `1er Cuartil` = ~ quantile(., 0.25, na.rm = TRUE),
    Mediana = ~ median(., na.rm = TRUE),
    Media = ~ mean(., na.rm = TRUE),
    `3er Cuartil` = ~ quantile(., 0.75, na.rm = TRUE),
    Máximo = ~ max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Tecnología", "Estadística"),
               names_sep = "_") %>%
  pivot_wider(names_from = Estadística, values_from = value) %>%
  mutate(across(where(is.numeric), ~ round(., 0)))  # Redondear números

# Crear la tabla con kableExtra
tabla_resumen_Tecnologia <- kable(resumen_df_Tecnologia, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resumen_df_Tecnologia), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

# Mostrar la tabla
tabla_resumen_Tecnologia

# 3. Datos Faltantes
total_na_A3 <- sum(is.na(datosA3))  # Total de valores faltantes
total_na_A3
faltantes_por_col_A3 <- colSums(is.na(datosA3))  # Valores faltantes por columna
faltantes_por_col_A3

# 4. Identificar Datos Atípicos
# Identificar outliers en cada columna del dataset
outliers_renovables_cogeneracion <- identificar_outliers(datosA3, "RENOVABLES, COGENERACIÓN Y RESIDUOS")
outliers_nuclear <- identificar_outliers(datosA3, "NUCLEAR")
outliers_internacionales <- identificar_outliers(datosA3, "INTERNACIONALES")
outliers_hidraulica <- identificar_outliers(datosA3, "HIDRÁULICA")
outliers_carbon <- identificar_outliers(datosA3, "CARBÓN")
outliers_ciclo_combinado <- identificar_outliers(datosA3, "CICLO COMBINADO")
outliers_fuel_gas <- identificar_outliers(datosA3, "FUEL-GAS")
outliers_almacenamiento <- identificar_outliers(datosA3, "ALMACENAMIENTO")

# Crear una lista para organizar los resultados
lista_outliers_Tecnologia <- list(
  "RENOVABLES, COGENERACIÓN Y RESIDUOS" = outliers_renovables_cogeneracion,
  "NUCLEAR" = outliers_nuclear,
  "INTERNACIONALES" = outliers_internacionales,
  "HIDRÁULICA" = outliers_hidraulica,
  "CARBÓN" = outliers_carbon,
  "CICLO COMBINADO" = outliers_ciclo_combinado,
  "FUEL-GAS" = outliers_fuel_gas,
  "ALMACENAMIENTO" = outliers_almacenamiento
)

# Combinar los resultados de los outliers en un solo data frame
outliers_combined <- do.call(rbind, lapply(names(lista_outliers_Tecnologia), function(tecnologia) {
  df <- lista_outliers_Tecnologia[[tecnologia]]
  if (nrow(df) > 0) {
    df$Tecnologia <- tecnologia  # Añadir columna con el nombre de la tecnología
  }
  return(df)
}))

# Crear la tabla de outliers con kableExtra
if (!is.null(outliers_combined) && nrow(outliers_combined) > 0) {
  tabla_outliers_combinedA3 <- kable(outliers_combined, format = "html", row.names = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    column_spec(1:ncol(outliers_combined), color = "black") %>%  
    row_spec(0, background = "deepskyblue", color = "white")  # Estilo para la fila de nombres de las tecnologías
  
  tabla_outliers_combinedA3
} else {
  print("No se encontraron datos atípicos en las tecnologías analizadas.")
}

# 5. Representación de la Serie Temporal
# Crear gráfico para todas las tecnologías
tituloA3 <- textGrob("Energía Proporcionada por Tecnología - Anuales (2007 - 2024)", gp = gpar(fontsize = 14, fontface = "bold"))

grafico_Tecnologia <- datosA3 %>%
  pivot_longer(cols = -Año, names_to = "Tecnología", values_to = "Energía") %>%
  ggplot(aes(x = Año, y = Energía, color = Tecnología, group = Tecnología)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Año", y = "Energía (GWh)", color = "Tecnología") +
  scale_color_manual(values = c(
    "RENOVABLES, COGENERACIÓN Y RESIDUOS" = "#4169E1",
    "NUCLEAR" = "#AEC7E8",
    "INTERNACIONALES" = "#D5006D",
    "HIDRÁULICA" = "#9E3D68",
    "CARBÓN" = "#6A51A3",
    "CICLO COMBINADO" = "#B3A0D2",
    "FUEL-GAS" = "#A9A9A9",
    "ALMACENAMIENTO" = "pink"
  )) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )

# Combinar título y gráfico
grafico_Tecnologia <- grid.arrange(tituloA3, grafico_Tecnologia, ncol = 1, heights = c(0.1, 0.9))

# 6. Tabla de Datos Transpuesta
# Transponer los datos
datos_transpuestos_A3 <- t(datosA3 %>% select(-Año))
colnames(datos_transpuestos_A3) <- datosA3$Año
rownames(datos_transpuestos_A3) <- colnames(datosA3)[-1]

# Crear la tabla con kableExtra
tabla_datos_A3 <- kable(datos_transpuestos_A3, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(datos_transpuestos_A3), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

# Mostrar la tabla
tabla_datos_A3






# ---- ------------ CORRELACIONES  SERIES ANUALES ----------- ----

# 1. Identificar el Rango Común de Años
rango_anios_A1 <- range(datosA1$Año)
rango_anios_A2 <- range(datosA2$Año)
rango_anios_A3 <- range(datosA3$Año)

# Obtener el rango común
rango_comun <- max(rango_anios_A1[1], rango_anios_A2[1], rango_anios_A3[1]):min(rango_anios_A1[2], rango_anios_A2[2], rango_anios_A3[2])

# Filtrar las Tablas para el Rango Común
datosA1_filtrado <- datosA1 %>% filter(Año %in% rango_comun)
datosA2_filtrado <- datosA2 %>% filter(Año %in% rango_comun)
datosA3_filtrado <- datosA3 %>% filter(Año %in% rango_comun)

# 2. Unir las Tablas Filtradas
datos_merged <- datosA1_filtrado %>%
  inner_join(datosA2_filtrado, by = "Año") %>%
  inner_join(datosA3_filtrado, by = "Año")
datos_merged


# 3. Selección de Variables
variables_correlacion <- datos_merged %>%
  select_if(is.numeric)  # Selecciona solo columnas numéricas

# 4. Cálculo de Correlaciones
matriz_correlacion <- cor(variables_correlacion, use = "pairwise.complete.obs")
matriz_correlacion
matriz_correlacion_df <- as.data.frame(matriz_correlacion)
matriz_correlacion_df <- matriz_correlacion_df %>%
  rownames_to_column(var = "Variable")

# Creamos la tabla con formato bonito
tabla_matriz_correlacion <- kable(matriz_correlacion_df, format = "html", digits = 3, row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE) %>%
  column_spec(1, bold = TRUE, background = "lightblue") %>%  # Resaltar la primera columna con los nombres
  row_spec(0, background = "deepskyblue", color = "white")  # Estilo para el encabezado

# ---- MOSTRAR LA TABLA ----
tabla_matriz_correlacion

# Convertir la matriz de correlaciones en formato largo
matriz_correlacion_melt <- melt(matriz_correlacion)
colnames(matriz_correlacion_melt) <- c("Variable1", "Variable2", "Correlación")

# Crear el heatmap
heatmap_correlacion <- ggplot(matriz_correlacion_melt, aes(x = Variable1, y = Variable2, fill = Correlación)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
  labs(title = "Correlaciones Series Anuales", x = "Variable", y = "Variable", fill = "Correlación") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

heatmap_correlacion

# 5. Identificar las Principales Correlaciones (Sin Duplicados)
correlaciones_long <- as.data.frame(as.table(matriz_correlacion)) %>%
  filter(Var1 != Var2) %>%
  mutate(Variables = pmap_chr(list(Var1, Var2), ~ paste(sort(c(.x, .y)), collapse = " - "))) %>%
  distinct(Variables, .keep_all = TRUE)

# Verificar el contenido
print(head(correlaciones_long))

# Ordenar por correlación más alta y más baja
correlaciones_principales <- correlaciones_long %>%
  arrange(desc(Freq)) %>%
  head(10) %>%
  bind_rows(
    correlaciones_long %>%
      arrange(Freq) %>%
      head(10)
  )

# Verificar las columnas de correlaciones_principales
print(colnames(correlaciones_principales))

# Crear la tabla si tiene las columnas correctas
if ("Variables" %in% colnames(correlaciones_principales) && "Freq" %in% colnames(correlaciones_principales)) {
  tabla_correlaciones_principales <- kable(correlaciones_principales[, c("Variables", "Freq")], format = "html", row.names = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    column_spec(1:ncol(correlaciones_principales[, c("Variables", "Freq")]), color = "black") %>%
    row_spec(0, background = "deepskyblue", color = "white")
  
  tabla_correlaciones_principales
} else {
  print("La tabla de correlaciones principales no tiene las columnas necesarias.")
}


# 6. Correlaciones con `Precio final medio (EUR/MWh)`

# Filtrar las correlaciones relacionadas con `Precio final medio (EUR/MWh)`
correlaciones_precio_final <- correlaciones_long %>%
  filter(Var1 == "Precio final medio (EUR/MWh)" | Var2 == "Precio final medio (EUR/MWh)") %>%
  mutate(Variable = ifelse(Var1 == "Precio final medio (EUR/MWh)", as.character(Var2), as.character(Var1))) %>%  # Usar nombres completos
  select(Variable, Correlación = Freq) %>%
  arrange(desc(Correlación))  # Ordenar de mayor a menor

# Crear una tabla con las correlaciones específicas
tabla_correlaciones_precio_final <- kable(correlaciones_precio_final, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(correlaciones_precio_final), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_correlaciones_precio_final

# Crear un gráfico de barras horizontales ordenado de mayor a menor
grafico_correlaciones_precio_final <- ggplot(correlaciones_precio_final, aes(x = reorder(Variable, -Correlación), y = Correlación, fill = Correlación)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(Correlación, 2)), hjust = -0.2, size = 4, color = "black") +  # Etiquetas a la derecha
  coord_flip() +  # Rotar para barras horizontales
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1)) +
  labs(
    title = "Correlaciones con Precio Final Medio (EUR/MWh)",
    x = "Variables",
    y = "Correlación",
    fill = "Correlación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )
grafico_correlaciones_precio_final













