#LIBRERIAS NECESARIAS → 

library(ggplot2)
library(dplyr)
library(readxl)
library(knitr)
library(gridExtra)
library(grid)
library(tidyr)
library(gtable)
library(kableExtra)

# ---- Archivo de Datos ---- 

ruta_archivo <- "C:/Users/lucia/OneDrive/Escritorio/Asignaturas/Cuarto/TFG/LuciaCalderon_DatosTFG.xlsx"



# ---- ------------- ANALÍSIS DESCRIPTIVO ------------ ----


# ---- Funciones comunes ---- 

# Identificar datos atípicos por columnas

identificar_outliers <- function(datos, columna) {
  Q1 <- quantile(datos[[columna]], 0.25, na.rm = TRUE)  # Primer cuartil
  Q3 <- quantile(datos[[columna]], 0.75, na.rm = TRUE)  # Tercer cuartil
  IQR <- Q3 - Q1                                        # Rango intercuartil
  
  # Límites inferior y superior
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  
  # Filtrar datos atípicos
  outliers <- datos[datos[[columna]] < limite_inferior | datos[[columna]] > limite_superior, ]
  
  # Retornar resultados
  return(outliers)
}





# ---- PRECIO MERCADO DIARIO ---- 


# 1. CARGAR HOJA DEL EXCEL 
datosD1 <- read_excel(ruta_archivo, sheet = "D_Pmercdiario_max, min, avg")

# 2. RESUMEN DE LOS DATOS 
str(datosD1)

resumen_max <- summary(datosD1$`Precio máximo`)
resumen_min <- summary(datosD1$`Precio mínimo`)
resumen_avg <- summary(datosD1$`Precio medio aritmético`)
sd_max <- sd(datosD1$`Precio máximo`, na.rm = TRUE)
sd_min <- sd(datosD1$`Precio mínimo`, na.rm = TRUE)
sd_avg <- sd(datosD1$`Precio medio aritmético`, na.rm = TRUE)

# Crear un data frame con las estadísticas para cada columna
resumen_df <- data.frame(
  "Mínimo" = c(resumen_max[1], resumen_min[1], resumen_avg[1]),
  "1er Cuartil" = c(resumen_max[2], resumen_min[2], resumen_avg[2]),
  "Mediana" = c(resumen_max[3], resumen_min[3], resumen_avg[3]),
  "Media" = c(resumen_max[4], resumen_min[4], resumen_avg[4]),
  "3er Cuartil" = c(resumen_max[5], resumen_min[5], resumen_avg[5]),
  "Máximo" = c(resumen_max[6], resumen_min[6], resumen_avg[6]),
  "Desv Típica" = c(sd_max, sd_min, sd_avg) # Añadir la desviación típica
)

rownames(resumen_df) <- c("Precio Máximo", "Precio Mínimo", "Precio Promedio")

# Crear la tabla con kableExtra
tabla_resumen_D1 <- kable(resumen_df, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resumen_df), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")
tabla_resumen_D1 

# 3. DATOS FALTANTES 
total_na <- sum(is.na(datosD1))  # Número total de valores faltantes
total_na
faltantes_por_col <- colSums(is.na(datosD1))  # Valores faltantes por columnas
faltantes_por_col

# 4. DATOS ATÍPICOS 
outliers_precio_max <- identificar_outliers(datosD1, "Precio máximo")
outliers_precio_min <- identificar_outliers(datosD1, "Precio mínimo")
outliers_precio_avg <- identificar_outliers(datosD1, "Precio medio aritmético")

# Combinar los resultados de los outliers
lista_outliers_D1 <- list(
  "Precio Máximo" = outliers_precio_max,
  "Precio Mínimo" = outliers_precio_min,
  "Precio Promedio" = outliers_precio_avg
)

outliers_combined_D1 <- do.call(rbind, lapply(names(lista_outliers_D1), function(x) {
  df <- lista_outliers_D1[[x]]
  if (nrow(df) > 0) {
    df$Variable <- x
  }
  return(df)
}))

# Crear tabla de outliers
if (!is.null(outliers_combined_D1) && nrow(outliers_combined_D1) > 0) {
  tabla_outliers_combined_D1 <- kable(outliers_combined_D1, format = "html", row.names = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    column_spec(1:ncol(outliers_combined_D1), color = "black") %>%
    row_spec(0, background = "deepskyblue", color = "white")
  tabla_outliers_combined_D1
} else {
  print("No se encontraron valores atípicos.")
}

# 5. BOXPLOTS 
tituloD1 <- textGrob("Precios Mercado Diario - Diarios (2021 - 2024)", gp = gpar(fontsize = 14, fontface = "bold"))

boxplot_D1 <- datosD1 %>%
  pivot_longer(cols = c(`Precio máximo`, `Precio mínimo`, `Precio medio aritmético`), 
               names_to = "Variable", values_to = "Valor") %>%
  mutate(Variable = factor(Variable, levels = c("Precio mínimo", "Precio medio aritmético", "Precio máximo"))) %>%  # Reordenar los niveles
  ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 2) +
  labs(title = "Boxplots de Precios del Mercado Diario (2021 - 2024)", x = "Variable", y = "Precio (€)") +
  scale_fill_manual(values = c("Precio máximo" = "lightblue", "Precio mínimo" = "royalblue", "Precio medio aritmético" = "dodgerblue")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

boxplot_D1


# 6. REPRESENTACIÓN DE LA SERIE TEMPORAL (SOLO PROMEDIO)
datosD1$Fecha <- as.Date(datosD1$Fecha, format = "%d/%m/%Y")

grafico_promedio_D1 <- ggplot(datosD1, aes(x = Fecha)) +
  geom_line(aes(y = `Precio medio aritmético`, color = "Promedio"), size = 0.8) +
  labs(
    title = "Precio Medio del Mercado Diario (2021 - 2024)",
    x = "Fecha",
    y = "Precio (€)"
  ) +
  scale_color_manual(values = c("Promedio" = "dodgerblue")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Sin leyenda, ya que solo representamos el promedio
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotación de las etiquetas del eje X para mejorar legibilidad
  )

grafico_promedio_D1







# ---- PRECIO COMERCIALIZADORES Y ENERGÍA OFRECIDA ----



# 1. CARGAR HOJA DEL EXCEL
datosD2 <- read_excel(ruta_archivo, sheet = "D_Pcomercial max,min,avg")

# 2. RESUMEN DE LOS DATOS
str(datosD2)

# Resumen estadístico por columnas
resumen_max <- summary(datosD2$`Precio máximo`)
resumen_min <- summary(datosD2$`Precio mínimo`)
resumen_avg <- summary(datosD2$`Precio medio`)
resumen_energia <- summary(datosD2$`Energía (MWh)`)
desviacion_max <- sd(datosD2$`Precio máximo`, na.rm = TRUE)
desviacion_min <- sd(datosD2$`Precio mínimo`, na.rm = TRUE)
desviacion_avg <- sd(datosD2$`Precio medio`, na.rm = TRUE)
desviacion_energia <- sd(datosD2$`Energía (MWh)`, na.rm = TRUE)

# Crear un data frame con las estadísticas para cada columna
resumen_df <- data.frame(
  "Mínimo" = c(resumen_max[1], resumen_min[1], resumen_avg[1], resumen_energia[1]),
  "1er Cuartil" = c(resumen_max[2], resumen_min[2], resumen_avg[2], resumen_energia[2]),
  "Mediana" = c(resumen_max[3], resumen_min[3], resumen_avg[3], resumen_energia[3]),
  "Media" = c(resumen_max[4], resumen_min[4], resumen_avg[4], resumen_energia[4]),
  "3er Cuartil" = c(resumen_max[5], resumen_min[5], resumen_avg[5], resumen_energia[5]),
  "Máximo" = c(resumen_max[6], resumen_min[6], resumen_avg[6], resumen_energia[6]),
  "Desv Típica" = c(desviacion_max, desviacion_min, desviacion_avg, desviacion_energia)  # Añadir desviación estándar
)

rownames(resumen_df) <- c("Precio Máximo", "Precio Mínimo", "Precio Promedio", "Energía (MWh)")

# Crear la tabla con kableExtra
tabla_resumen_D2 <- kable(resumen_df, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resumen_df), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")
tabla_resumen_D2

# 3. DATOS FALTANTES
total_na <- sum(is.na(datosD2))  # Total de valores faltantes
total_na
faltantes_por_col <- colSums(is.na(datosD2))  # Valores faltantes por columna
faltantes_por_col

# 4. DATOS ATÍPICOS
outliers_precio_avg <- identificar_outliers(datosD2, "Precio medio")
outliers_energia <- identificar_outliers(datosD2, "Energía (MWh)")

# Crear una lista con los datos de outliers
lista_outliers_D2 <- list(
  "Outliers Precio Promedio" = outliers_precio_avg,
  "Outliers Energía (MWh)" = outliers_energia
)

# Combinar los resultados de los outliers
outliers_combined_D2 <- do.call(rbind, lapply(names(lista_outliers_D2), function(x) {
  df <- lista_outliers_D2[[x]]
  if (nrow(df) > 0) {
    df$Variable <- x
  }
  return(df)
}))

# Crear tabla de outliers
if (!is.null(outliers_combined_D2) && nrow(outliers_combined_D2) > 0) {
  tabla_outliers_combined_D2 <- kable(outliers_combined_D2, format = "html", row.names = TRUE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    column_spec(1:ncol(outliers_combined_D2), color = "black") %>%
    row_spec(0, background = "deepskyblue", color = "white")
  tabla_outliers_combined_D2
} else {
  print("No se encontraron valores atípicos.")
}

#Numero de outliers por variable 
# Calcular el número de datos atípicos por variable
num_outliers <- sapply(lista_outliers_D2, nrow)

# Crear un data frame con los resultados
tabla_outliers_resumen <- data.frame(
  "Variable" = names(num_outliers),
  "Número_de_Datos_Atípicos" = num_outliers
)

# Crear la tabla con kableExtra
tabla_outliers_resumen_D2 <- kable(tabla_outliers_resumen, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(tabla_outliers_resumen), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")  # Estilo para la fila de nombres de las columnas

tabla_outliers_resumen_D2


# 5. BOXPLOTS

# Gráficos boxplots para identificar valores atípicos
boxplot_max <- ggplot(datosD2, aes(x = "", y = `Precio máximo`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Precio Máximo") +
  ylab(NULL) +
  theme_minimal()

boxplot_min <- ggplot(datosD2, aes(x = "", y = `Precio mínimo`)) +
  geom_boxplot(fill = "royalblue") +
  labs(title = "Precio Mínimo") +
  ylab(NULL) +
  theme_minimal()

boxplot_avg <- ggplot(datosD2, aes(x = "", y = `Precio medio`)) +
  geom_boxplot(fill = "dodgerblue") +
  labs(title = "Precio Promedio") +
  ylab(NULL) +
  theme_minimal()

boxplot_energia <- ggplot(datosD2, aes(x = "", y = `Energía (MWh)`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Energía (MWh)") +
  ylab(NULL) +
  theme_minimal()

tituloD2 <- textGrob("Precio de Comercializadores y Energía Ofrecida - Diarios (2020 - 2024)", gp = gpar(fontsize = 14, fontface = "bold"))

boxplotD2 = grid.arrange(
  tituloD2, boxplot_min, boxplot_avg, boxplot_max, boxplot_energia,
  ncol = 4, 
  layout_matrix = rbind(c(1, 1, 1, 1), c(2, 3, 4, 5)),
  heights = c(0.1, 1)  # Ajusta la altura del título y los gráficos
)

boxplotD2



# 6. REPRESENTACIÓN DE LA SERIE TEMPORAL

# Asegúrate de que 'Fecha' esté en formato Date
datosD2$Fecha <- as.Date(datosD2$Fecha, format = "%d/%m/%Y")

# Gráfico: Precio medio y Energía (con eje secundario)
grafico_avg_energia <- ggplot(datosD2, aes(x = Fecha)) +
  geom_line(aes(y = `Precio medio`, color = "Precio medio"), size = 0.8) +  # Línea más gruesa
  geom_line(aes(y = `Energía (MWh)` / max(`Energía (MWh)`) * max(`Precio medio`), 
                color = "Energía (MWh)"), size = 0.8, linetype = "dashed") +  # Línea discontinua para Energía
  scale_y_continuous(
    name = "Precio medio (EUR/MWh)", 
    sec.axis = sec_axis(~ . * max(datosD2$`Energía (MWh)`) / max(datosD2$`Precio medio`), 
                        name = "Energía (MWh)")
  ) +
  scale_color_manual(values = c("Precio medio" = "navy", "Energía (MWh)" = "purple")) +
  labs(
    title = "Precio Medio de Comercializadores y Energía Ofrecida - Diarios (2020-2024)",
    x = "Fecha"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Mostrar años completos
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "purple"),
    axis.text.y.right = element_text(color = "purple"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(title = "Leyenda"))
grafico_avg_energia





# ---- PRECIO FINAL DE LA DEMANDA  ----

# 1. CARGAR HOJA DEL EXCEL
datosD3 <- read_excel(ruta_archivo, sheet = "D_Pfdemanda")

# Renombrar las columnas: De "1, 2, ..., 24" a "1h, 2h, ..., 24h"
colnames(datosD3) <- c("Fecha", paste0(1:24, "h"))

# Verificar estructura inicial
str(datosD3)

# 2. TRANSFORMACIÓN AL FORMATO LONGITUDINAL
datosD3_long <- datosD3 %>%
  pivot_longer(
    cols = -Fecha,
    names_to = "Hora",
    values_to = "Precio_Demanda"
  ) %>%
  mutate(
    Fecha = as.Date(Fecha),        # Asegurar que la fecha esté en formato Date
    Hora = as.numeric(gsub("h", "", Hora))  # Convertir la hora en numérica
  )

# Verificar la estructura después de la transformación
str(datosD3_long)


# Resumen estadístico general por hora
resumen_df_D3 <- datosD3_long %>%
  group_by(Hora) %>%
  summarise(
    Mínimo = min(Precio_Demanda, na.rm = TRUE),
    `1er Cuartil` = quantile(Precio_Demanda, 0.25, na.rm = TRUE),
    Mediana = median(Precio_Demanda, na.rm = TRUE),
    Media = mean(Precio_Demanda, na.rm = TRUE),
    `3er Cuartil` = quantile(Precio_Demanda, 0.75, na.rm = TRUE),
    Máximo = max(Precio_Demanda, na.rm = TRUE)
  )

# Crear tabla con kableExtra
tabla_resumen_D3 <- kable(resumen_df_D3, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resumen_df_D3), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_resumen_D3

# 4. DATOS FALTANTES
# Calcular el número de datos faltantes por hora
faltantes_por_hora <- datosD3_long %>%
  group_by(Hora) %>%
  summarise(Faltantes = sum(is.na(Precio_Demanda)))

# Crear tabla con los datos faltantes
tabla_faltantes_D3 <- kable(faltantes_por_hora, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(faltantes_por_hora), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_faltantes_D3

# Calcular la media de la columna "24h", excluyendo los valores faltantes
media_24h <- mean(datosD3$`24h`, na.rm = TRUE)

# Sustituir los NA en la columna "24h" por la media calculada
datosD3$`24h`[is.na(datosD3$`24h`)] <- media_24h

# Verificar que los NA han sido reemplazados
sum(is.na(datosD3$`24h`))  # Debe devolver 0


# 5. DATOS ATÍPICOS
# Identificar outliers por hora
outliers_D3 <- datosD3_long %>%
  group_by(Hora) %>%
  summarise(Outliers = nrow(identificar_outliers(pick(Precio_Demanda), "Precio_Demanda")))

# Crear tabla con los datos atípicos por hora
tabla_outliers_D3 <- kable(outliers_D3, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(outliers_D3), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_outliers_D3

# Calcular el total de datos atípicos
total_outliers <- sum(outliers_D3$Outliers)

# Mostrar el total
total_outliers

# 6. VISUALIZACIÓN: BOXPLOTS
# Crear boxplots por hora
boxplot_D3 <- ggplot(datosD3_long, aes(x = factor(Hora), y = Precio_Demanda, fill = factor(Hora))) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 2) +
  labs(
    title = "Distribución del Precio Final de la Demanda por Hora - Intradiarios (2020-2024)",
    x = "Hora",
    y = "Precio de la Demanda (€)"
  ) +
  scale_fill_viridis_d(option = "C") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

boxplot_D3

# 7. VISUALIZACIÓN: SERIE TEMPORAL
# Evolución del precio promedio por día
serie_temporal_D3 <- datosD3_long %>%
  group_by(Fecha) %>%
  summarise(Promedio = mean(Precio_Demanda, na.rm = TRUE)) %>%
  ggplot(aes(x = Fecha, y = Promedio)) +
  geom_line(color = "blue", size = 0.8) +
  labs(
    title = "Evolución del Precio Promedio Diario (2020-2024)",
    x = "Fecha",
    y = "Precio Promedio (€)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

serie_temporal_D3







# ---- DEMANDA DE ENERGÍA DIARIA ----

# 1. CARGAR HOJA DEL EXCEL
datosD4 <- read_excel(ruta_archivo, sheet = "D_Energia")

# Renombrar las columnas: De "1, 2, ..., 24" a "1h, 2h, ..., 24h"
colnames(datosD4) <- c("Fecha", paste0(1:24, "h"))

# Verificar estructura inicial
str(datosD4)

# 2. TRANSFORMACIÓN AL FORMATO LONGITUDINAL
datosD4_long <- datosD4 %>%
  pivot_longer(
    cols = -Fecha,
    names_to = "Hora",
    values_to = "Energia"
  ) %>%
  mutate(
    Fecha = as.Date(Fecha),        # Asegurar que la fecha esté en formato Date
    Hora = as.numeric(gsub("h", "", Hora))  # Convertir la hora en numérica
  )

# Verificar la estructura después de la transformación
str(datosD4_long)

# 3. ANÁLISIS DESCRIPTIVO: RESUMEN ESTADÍSTICO POR HORA
resumen_df_D4 <- datosD4_long %>%
  group_by(Hora) %>%
  summarise(
    Mínimo = min(Energia, na.rm = TRUE),
    `1er Cuartil` = quantile(Energia, 0.25, na.rm = TRUE),
    Mediana = median(Energia, na.rm = TRUE),
    Media = mean(Energia, na.rm = TRUE),
    `3er Cuartil` = quantile(Energia, 0.75, na.rm = TRUE),
    Máximo = max(Energia, na.rm = TRUE)
  )

# Crear tabla con kableExtra
tabla_resumen_D4 <- kable(resumen_df_D4, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resumen_df_D4), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_resumen_D4

# 4. DATOS FALTANTES
faltantes_por_hora <- datosD4_long %>%
  group_by(Hora) %>%
  summarise(Faltantes = sum(is.na(Energia)))

# Crear tabla con los datos faltantes
tabla_faltantes_D4 <- kable(faltantes_por_hora, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(faltantes_por_hora), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_faltantes_D4

# Calcular la media de la columna "24h", excluyendo los valores faltantes
media_24h <- mean(datosD4$`24h`, na.rm = TRUE)

# Sustituir los NA en la columna "24h" por la media calculada
datosD4$`24h`[is.na(datosD4$`24h`)] <- media_24h

# Verificar que los NA han sido reemplazados
sum(is.na(datosD4$`24h`))  # Debe devolver 0

# 5. DATOS ATÍPICOS
outliers_D4 <- datosD4_long %>%
  group_by(Hora) %>%
  summarise(Outliers = nrow(identificar_outliers(pick(Energia), "Energia")))

# Crear tabla con los datos atípicos por hora
tabla_outliers_D4 <- kable(outliers_D4, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(outliers_D4), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_outliers_D4

# Calcular el total de datos atípicos
total_outliers <- sum(outliers_D4$Outliers)
total_outliers

# 6. VISUALIZACIÓN: BOXPLOTS

colores_azules <- colorRampPalette(c("lightblue", "royalblue", "navy"))(24)

# Crear los boxplots con la nueva paleta de colores
boxplot_D4 <- ggplot(datosD4_long, aes(x = factor(Hora), y = Energia, fill = factor(Hora))) +
  geom_boxplot(outlier.colour = "orange", outlier.shape = 1, outlier.size = 2) +
  labs(
    title = "Distribución de Energía Demandada por Hora - Intradiarios (2020-2024)",
    x = "Hora",
    y = "Energía (MWh)"
  ) +
  scale_fill_manual(values = colores_azules) +  # Aplicar paleta personalizada
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

boxplot_D4

# 7. VISUALIZACIÓN: SERIE TEMPORAL
serie_temporal_D4 <- datosD4_long %>%
  group_by(Fecha) %>%
  summarise(Promedio = mean(Energia, na.rm = TRUE)) %>%
  ggplot(aes(x = Fecha, y = Promedio)) +
  geom_line(color = "purple", size = 0.8) +  # Color distinto
  labs(
    title = "Evolución de la Energía Promedio Diaria (2020-2024)",
    x = "Fecha",
    y = "Energía Promedio (MWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

serie_temporal_D4






# ---- ENERGÍA POR TECNOLOGÍAS ----

# 1. CARGAR HOJA DEL EXCEL
datosD5 <- read_excel(ruta_archivo, sheet = "D_Tecnologia")

# 2. RESUMEN DE LOS DATOS
str(datosD5)

# Generar resumen estadístico para cada columna de tecnología
resumen_df_Tecnologia <- datosD5 %>%
  select(-Fecha) %>%
  summarise(across(everything(), list(
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
  pivot_wider(names_from = Estadística, values_from = value)

# Crear tabla con kableExtra
tabla_resumen_D5 <- kable(resumen_df_Tecnologia, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resumen_df_Tecnologia), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_resumen_D5

# 3. DATOS FALTANTES
total_na <- colSums(is.na(datosD5))
total_na

# 4. DATOS ATÍPICOS

# Identificar los outliers por tecnología
lista_outliers_D5 <- datosD5 %>%
  select(-Fecha) %>%
  summarise(across(everything(), ~ nrow(identificar_outliers(datosD5, cur_column()))))

# Crear una tabla con el número de datos atípicos
tabla_outliers_D5 <- data.frame(
  "Tecnología" = colnames(lista_outliers_D5),
  "Número_de_Datos_Atípicos" = unlist(lista_outliers_D5)
)

# Formatear la tabla con kableExtra
tabla_outliers_resumen_D5 <- kable(tabla_outliers_D5, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(tabla_outliers_D5), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_outliers_resumen_D5

total_outliers <- sum(tabla_outliers_D5$`Número_de_Datos_Atípicos`)
total_outliers

# 5. GRÁFICO FINAL
# Convertir los datos a formato largo
datos_long_D5 <- datosD5 %>%
  pivot_longer(
    cols = -Fecha,
    names_to = "Tecnología",
    values_to = "Producción"
  )

datos_long_D5$Fecha <- as.Date(datos_long_D5$Fecha, format = "%d/%m/%Y")

# Crear gráfico
grafico_D5 <- ggplot(datos_long_D5, aes(x = Fecha, y = Producción, color = Tecnología)) +
  geom_line(size = 0.8, alpha = 0.7) +
  labs(
    title = "Producción de Energía por Tecnología (Junio'23 - Diciembre'24)",
    x = "Fecha",
    y = "Producción (MWh)",
    color = "Tecnología"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Mostrar el gráfico
grafico_D5




# ---- ANALISISI CONJUNTO DE SERIES TEMPORALES DIARIAS  ----

# ---- UNION DEMANDA: PRECIO POR HORA - ENERGIA POR HORA ----


# 1.Correlacion Pf Demanda y Demanda de Energia por hora 

# Agrupar por fecha y calcular el promedio diario para cada serie
datosD3_promedio <- datosD3_long %>%
  group_by(Fecha, Hora) %>%
  summarise(PFdemanda_promedio = mean(Precio_Demanda, na.rm = TRUE), .groups = 'drop')

datosD4_promedio <- datosD4_long %>%
  group_by(Fecha, Hora) %>%
  summarise(Energia_promedio = mean(Energia, na.rm = TRUE), .groups = 'drop')

# Unir ambos conjuntos de datos por Fecha y Hora
datos_merged <- merge(datosD3_promedio, datosD4_promedio, by = c("Fecha", "Hora"))

# Calcular la correlación entre Precio Final de Demanda y Energía Demandada por Hora

# ATENCIONNNNNNNN : LA CORRELACION GENERAL DE AQUI ES TENIENDO EN CUENTA FECHA Y HORA - ES DECIR NO ES DE SERIE DIARIA
# PARA SERIE DIARIA MEJOR AL DE LIMPIO CORRELACIONES 
correlaciones_por_hora <- datos_merged %>%
  group_by(Hora) %>%
  summarise(Correlacion = cor(PFdemanda_promedio, Energia_promedio, use = "complete.obs"), .groups = 'drop')

# Calcular la correlación general entre ambas variables
correlacion_general <- cor(datos_merged$PFdemanda_promedio, datos_merged$Energia_promedio, use = "complete.obs")

# Crear tabla para mostrar la correlación general
tabla_correlacion_general <- data.frame(
  Metrica = "Correlación PFinal - Energia Demandada",
  Valor = round(correlacion_general, 4)
)

# Mostrar la tabla estilizada
tabla_correlacion_general_html <- kable(tabla_correlacion_general, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(tabla_correlacion_general), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_correlacion_general_html

# Crear gráfico de correlación por hora
grafico_correlacion_horas <- ggplot(correlaciones_por_hora, aes(x = Hora, y = Correlacion)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Correlacion, 2)), vjust = -0.5, size = 4) +
  labs(
    title = "Correlación entre Precio Final de la Demanda y Energía Demandada por Hora",
    x = "Hora del Día",
    y = "Coeficiente de Correlación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

grafico_correlacion_horas



# 2. Precio y Energia demandada  medido por dia  

# Agrupar por fecha y calcular el promedio diario para cada serie
datosD3_promedio <- datosD3_long %>%
  group_by(Fecha) %>%
  summarise(PFdemanda_promedio = mean(Precio_Demanda, na.rm = TRUE))

datosD4_promedio <- datosD4_long %>%
  group_by(Fecha) %>%
  summarise(Energia_promedio = mean(Energia, na.rm = TRUE))

# Unir ambos conjuntos de datos por Fecha
datos_merged <- merge(datosD3_promedio, datosD4_promedio, by = "Fecha")

# Crear la gráfica con doble eje
grafico_pf_demanda_energia <- ggplot(datos_merged, aes(x = Fecha)) +
  geom_line(aes(y = PFdemanda_promedio, color = "Precio Final Demanda"), size = 1) +
  geom_line(aes(y = Energia_promedio / max(Energia_promedio) * max(PFdemanda_promedio),
                color = "Energía Demandada"), size = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Precio Final Demanda (EUR/MWh)",
    sec.axis = sec_axis(~ . * max(datos_merged$Energia_promedio) / max(datos_merged$PFdemanda_promedio),
                        name = "Energía Demandada (MWh)")
  ) +
  scale_color_manual(values = c("Precio Final Demanda" = "skyblue", "Energía Demandada" = "navy")) +
  labs(
    title = "Precio Final de la Demanda y Energía Demandada - Diarios (2020-2024)",
    x = "Fecha"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "navy"),
    axis.text.y.right = element_text(color = "navy"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "top",
    legend.title = element_blank()
  )

grafico_pf_demanda_energia





#LIMPIO CORRELACIONES 

# ---- AGRUPAR POR FECHA Y CALCULAR PROMEDIOS DIARIOS ----
datosD3_promedio <- datosD3_long %>%
  group_by(Fecha) %>%
  summarise(PFdemanda_promedio = mean(Precio_Demanda, na.rm = TRUE))

datosD4_promedio <- datosD4_long %>%
  group_by(Fecha) %>%
  summarise(Energia_promedio = mean(Energia, na.rm = TRUE))

# ---- UNIR SERIES POR FECHA ----
datos_diarios <- merge(datosD3_promedio, datosD4_promedio, by = "Fecha")

# ---- CALCULAR CORRELACIÓN GENERAL ----
correlacion_general <- cor(datos_diarios$PFdemanda_promedio, datos_diarios$Energia_promedio, use = "complete.obs")

# ---- MOSTRAR RESULTADO EN UNA TABLA ----
tabla_correlacion_general <- data.frame(
  Metrica = "Correlación PFinal - Energia Demandada",
  Valor = round(correlacion_general, 4)
)

tabla_correlacion_general_html <- kable(tabla_correlacion_general, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(tabla_correlacion_general), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_correlacion_general_html

# ---- AGRUPAR POR FECHA Y HORA Y CALCULAR PROMEDIOS ----
datosD3_promedio_hora <- datosD3_long %>%
  group_by(Fecha, Hora) %>%
  summarise(PFdemanda_promedio = mean(Precio_Demanda, na.rm = TRUE), .groups = "drop")

datosD4_promedio_hora <- datosD4_long %>%
  group_by(Fecha, Hora) %>%
  summarise(Energia_promedio = mean(Energia, na.rm = TRUE), .groups = "drop")

# ---- UNIR SERIES POR FECHA Y HORA ----
datos_horarios <- merge(datosD3_promedio_hora, datosD4_promedio_hora, by = c("Fecha", "Hora"))

# ---- CALCULAR CORRELACIONES POR HORA ----
correlaciones_por_hora <- datos_horarios %>%
  group_by(Hora) %>%
  summarise(Correlacion = cor(PFdemanda_promedio, Energia_promedio, use = "complete.obs"), .groups = "drop")

# ---- CREAR GRÁFICO DE CORRELACIONES POR HORA ----
grafico_correlacion_horas <- ggplot(correlaciones_por_hora, aes(x = Hora, y = Correlacion)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Correlacion, 2)), vjust = -0.5, size = 4) +
  labs(
    title = "Correlación entre Precio Final de la Demanda y Energía Demandada por Hora",
    x = "Hora del Día",
    y = "Coeficiente de Correlación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

grafico_correlacion_horas

















