# ---- LIBRERÍAS NECESARIAS ----
library(dplyr)      # Manipulación de datos
library(tidyr)      # Transformación de datos
library(readxl)     # Lectura de archivos Excel
library(tseries)    # Para el Test de Dickey-Fuller
library(ggplot2)    # Visualización gráfica
library(reshape2)   # Transformar la matriz de correlación en formato largo
library(dplyr)      # Manipulación de datos
library(corrplot)   # Para visualizar la matriz de correlación
library(RColorBrewer) # Paleta de colores
library(kableExtra)
library(tibble)

# ---- CARGAR EL ARCHIVO EXCEL ----
ruta_archivo <- "C:/Users/lucia/OneDrive/Escritorio/Asignaturas/Cuarto/TFG/LuciaCalderon_DatosTFG.xlsx"

# ---- CARGAR SERIES TEMPORALES DIARIAS ----
datosD3 <- read_excel(ruta_archivo, sheet = "D_Pfdemanda")   # Precio Final Demanda
datosD4 <- read_excel(ruta_archivo, sheet = "D_Energia")     # Energía Ofrecida
datosD5 <- read_excel(ruta_archivo, sheet = "D_Tecnologia")  # Producción por Tecnología

# ---- AJUSTAR COLUMNAS DE FECHA ----
colnames(datosD3)[1] <- "Fecha"
colnames(datosD4)[1] <- "Fecha"

datosD3$Fecha <- as.Date(datosD3$Fecha, format = "%d/%m/%Y")
datosD4$Fecha <- as.Date(datosD4$Fecha, format = "%d/%m/%Y")
datosD5$Fecha <- as.Date(datosD5$Fecha, format = "%d/%m/%Y")

# ---- CONVERTIR SERIES HORARIAS A DIARIAS ----
# Convertimos D3 (Precio Demanda) en un único valor diario
datosD3_diario <- datosD3 %>%
  pivot_longer(cols = -Fecha, names_to = "Hora", values_to = "Precio_Demanda") %>%
  group_by(Fecha) %>%
  summarise(Precio_Demanda = mean(Precio_Demanda, na.rm = TRUE))

# Convertimos D4 (Energía) en un único valor diario
datosD4_diario <- datosD4 %>%
  pivot_longer(cols = -Fecha, names_to = "Hora", values_to = "Energia") %>%
  group_by(Fecha) %>%
  summarise(Energia = mean(Energia, na.rm = TRUE))

# ---- FILTRAR POR RANGO DE FECHAS (JUNIO 2023 - DICIEMBRE 2024) ----
datos_combinados <- datosD3_diario %>%
  inner_join(datosD4_diario, by = "Fecha") %>%
  inner_join(datosD5, by = "Fecha") %>%
  filter(Fecha >= as.Date("2023-06-01") & Fecha <= as.Date("2024-12-31"))

# ---- CARGAR TABLA COMBINADA ----
#load("datos_combinados_D3_D4_D5.RData")  # Cargamos la tabla combinada

# ---- SELECCIONAR VARIABLES NUMÉRICAS PARA EL TEST ----
variables_numericas <- names(datos_combinados)[sapply(datos_combinados, is.numeric)]

# ---- APLICAR TEST DE DICKEY-FULLER ----
resultados_adf <- lapply(variables_numericas, function(var) {
  serie <- na.omit(datos_combinados[[var]])  # Remover NA
  
  if (length(serie) > 10) {  # Solo aplica el test si hay suficientes datos
    test_adf <- adf.test(serie)
    p_valor <- test_adf$p.value
    estacionaria <- ifelse(p_valor < 0.05, "Sí", "No")
  } else {
    p_valor <- NA
    estacionaria <- "Insuf. datos"
  }
  
  return(data.frame(Variable = var, P_Value = round(p_valor, 4), Stationary = estacionaria))
})

# ---- CREAR TABLA DE RESULTADOS ----
resultados_adf_df <- do.call(rbind, resultados_adf)

# ---- IDENTIFICAR VARIABLES NO ESTACIONARIAS ----
variables_no_estacionarias <- resultados_adf_df %>%
  filter(Stationary == "No") %>%
  pull(Variable)

# ---- APLICAR DIFERENCIAS A LAS SERIES NO ESTACIONARIAS ----
datos_estacionarios <- datos_combinados

for (var in variables_no_estacionarias) {
  datos_estacionarios[[var]] <- c(NA, diff(datos_combinados[[var]]))  # Aplicamos diferencia
}


# ---- SELECCIONAR VARIABLES NUMÉRICAS ----
variables_numericas <- names(datos_estacionarios)[sapply(datos_estacionarios, is.numeric)]

# ---- CALCULAR MATRIZ DE CORRELACIÓN ----
matriz_correlacion <- cor(datos_estacionarios[, variables_numericas], use = "pairwise.complete.obs")

# ---- CREAR HEATMAP DE CORRELACIONES ----
corrplot(matriz_correlacion, method = "color", col = brewer.pal(n = 8, name = "RdBu"),
         type = "upper", tl.col = "black", tl.srt = 45, 
         title = "Heatmap de Correlaciones (Datos Diarios Estacionarios) Junio 2023 - Diciembre 2024", mar = c(0, 0, 2, 0))

# Asegurarnos de que la matriz tiene los nombres exactos
colnames(matriz_correlacion) <- c("Precio Demanda", "Energía", "CARBÓN", "NUCLEAR", "HIDRÁULICA", 
                                  "CICLO COMBINADO", "EÓLICA", "SOLAR TÉRMICA", 
                                  "SOLAR FOTOVOLTAICA", "COGENERACIÓN/RESIDUOS/MINI HIDRA", 
                                  "IMPORTACIÓN INTER.", "IMPORTACIÓN INTER. SIN MIBEL")
rownames(matriz_correlacion) <- colnames(matriz_correlacion)

# Convertimos la matriz de correlación en un data frame para mejor visualización
matriz_correlacion_df <- as.data.frame(matriz_correlacion)
matriz_correlacion_df <- matriz_correlacion_df %>%
  rownames_to_column(var = "Variable")  # Agregar nombres de variables como columna

# Formatear tabla con kableExtra
tabla_matriz_correlacion <- kable(matriz_correlacion_df, format = "html", digits = 3, row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE) %>%
  column_spec(1, bold = TRUE, background = "lightblue") %>%  # Resaltar la primera columna con los nombres
  row_spec(0, background = "deepskyblue", color = "white")  # Estilo para la fila de nombres de las variables
tabla_matriz_correlacion






# ---- CORRELACIONES SIN CORRECIÓN ---- 

# ---- SELECCIONAR VARIABLES NUMÉRICAS DE LOS DATOS ORIGINALES ----
variables_numericas_originales <- names(datos_combinados)[sapply(datos_combinados, is.numeric)]

# ---- CALCULAR MATRIZ DE CORRELACIÓN PARA DATOS ORIGINALES ----
matriz_correlacion_original <- cor(datos_combinados[, variables_numericas_originales], use = "pairwise.complete.obs")

# ---- CREAR HEATMAP DE CORRELACIONES PARA DATOS ORIGINALES ----
library(corrplot)
library(RColorBrewer)

corrplot(matriz_correlacion_original, method = "color", col = brewer.pal(n = 8, name = "RdBu"),
         type = "upper", tl.col = "black", tl.srt = 45, 
         title = "Heatmap de Correlaciones entre Variables (Datos Originales)", mar = c(0, 0, 2, 0))

# ---- HEATMAP DE CORRELACIONES CON AJUSTES ----
corrplot(matriz_correlacion_original, 
         method = "color", 
         col = brewer.pal(n = 8, name = "RdBu"),
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45, 
         tl.cex = 0.8,  # Ajusta el tamaño de las etiquetas
         mar = c(0.5, 0.5, 1.5, 0.2),  # Reduce el espacio del margen
         title = "Heatmap de Correlaciones (Datos Diarios Originales) Junio 2023 - Diciembe2024")



# ---- CREAR TABLA DE CORRELACIONES PARA DATOS ORIGINALES ----
library(kableExtra)
library(tibble)

# Asegurarnos de que la matriz tiene los nombres exactos
colnames(matriz_correlacion_original) <- c("Precio Demanda", "Energía", "CARBÓN", "NUCLEAR", "HIDRÁULICA", 
                                           "CICLO COMBINADO", "EÓLICA", "SOLAR TÉRMICA", 
                                           "SOLAR FOTOVOLTAICA", "COGENERACIÓN/RESIDUOS/MINI HIDRA", 
                                           "IMPORTACIÓN INTER.", "IMPORTACIÓN INTER. SIN MIBEL")
rownames(matriz_correlacion_original) <- colnames(matriz_correlacion_original)

# Convertimos la matriz de correlación en un data frame para mejor visualización
matriz_correlacion_original_df <- as.data.frame(matriz_correlacion_original)
matriz_correlacion_original_df <- matriz_correlacion_original_df %>%
  rownames_to_column(var = "Variable")  # Agregar nombres de variables como columna

# Formatear tabla con kableExtra
tabla_matriz_correlacion_original <- kable(matriz_correlacion_original_df, format = "html", digits = 3, row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE) %>%
  column_spec(1, bold = TRUE, background = "lightblue") %>%  # Resaltar la primera columna con los nombres
  row_spec(0, background = "deepskyblue", color = "white")  # Estilo para la fila de nombres de las variables

# Mostrar la tabla
tabla_matriz_correlacion_original





#---- CORRELACIONES CON EL RETARDO OPTIMO ---- 

# ---- CALCULAR RETARDOS ÓPTIMOS SOBRE DATOS ESTACIONARIOS ----
# Seleccionamos variables numéricas de datos_estacionarios
variables_numericas_estacionarios <- names(datos_estacionarios)[sapply(datos_estacionarios, is.numeric)]

# Calculamos los retardos óptimos para las variables estacionarias
retardos_optimos_estacionarios <- sapply(variables_numericas_estacionarios, function(var) {
  serie <- na.omit(datos_estacionarios[[var]])
  if (length(serie) > 10) {
    modelo <- VARselect(as.matrix(serie), lag.max = 10, type = "const")  # Criterio AIC
    return(modelo$selection["AIC(n)"])
  } else {
    return(NA)  # Si la serie tiene pocos datos, no calculamos retardo
  }
})

# Crear un data frame con los retardos óptimos
retardos_optimos_df_estacionarios <- data.frame(
  Variable = variables_numericas_estacionarios,
  Retardo_Optimo = retardos_optimos_estacionarios
)

# Mostrar los retardos óptimos
print(retardos_optimos_df_estacionarios)


# ---- APLICAR RETARDOS ÓPTIMOS A DATOS ESTACIONARIOS ----
datos_estacionarios_retardos <- datos_estacionarios

# Aplicamos los retardos óptimos a las variables estacionarias
for (row in 1:nrow(retardos_optimos_df_estacionarios)) {
  variable <- retardos_optimos_df_estacionarios$Variable[row]
  retardo <- retardos_optimos_df_estacionarios$Retardo_Optimo[row]
  
  if (!is.na(retardo) && retardo > 0) {
    datos_estacionarios_retardos[[paste0(variable, "_Lag_", retardo)]] <- dplyr::lag(datos_estacionarios[[variable]], n = retardo)
  }
}


# ---- CALCULAR MATRIZ DE CORRELACIÓN CON RETARDOS ÓPTIMOS ----
# Seleccionamos solo las columnas numéricas del nuevo data frame con retardos
variables_numericas_retardos_estacionarios <- names(datos_estacionarios_retardos)[sapply(datos_estacionarios_retardos, is.numeric)]

# Calculamos la matriz de correlación
matriz_correlacion_retardos_estacionarios <- cor(datos_estacionarios_retardos[, variables_numericas_retardos_estacionarios], use = "pairwise.complete.obs")


# ---- CREAR HEATMAP DE CORRELACIONES CON RETARDOS ----
corrplot(matriz_correlacion_retardos_estacionarios, method = "color", col = brewer.pal(n = 8, name = "RdBu"),
         type = "upper", tl.col = "black", tl.srt = 45, 
         title = "Heatmap de Correlaciones con Retardos Óptimos (Datos Estacionarios)", mar = c(0, 0, 2, 0))


# ---- CREAR TABLA DE CORRELACIONES CON RETARDOS ÓPTIMOS ----
# Convertimos la matriz de correlación con retardos en un data frame
matriz_correlacion_retardos_estacionarios_df <- as.data.frame(matriz_correlacion_retardos_estacionarios)
matriz_correlacion_retardos_estacionarios_df <- matriz_correlacion_retardos_estacionarios_df %>%
  rownames_to_column(var = "Variable")  # Agregar nombres de variables como columna

# Ordenamos la tabla por la relación con "Precio Demanda"
tabla_matriz_correlacion_retardos_estacionarios <- matriz_correlacion_retardos_estacionarios_df %>%
  arrange(desc(`Precio_Demanda`))  # Ordenar por correlación con "Precio Demanda"

# Formateamos la tabla ordenada
tabla_matriz_correlacion_retardos_estacionarios_kable <- kable(tabla_matriz_correlacion_retardos_estacionarios, format = "html", digits = 3, row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE) %>%
  column_spec(1, bold = TRUE, background = "lightblue") %>%  # Resaltar la primera columna
  row_spec(0, background = "deepskyblue", color = "white")  # Estilo para el encabezado

# Mostrar la tabla ordenada con retardos
tabla_matriz_correlacion_retardos_estacionarios_kable

# ---- FILTRAR VARIABLES SOLO CON RETARDOS ÓPTIMOS ----
# Seleccionamos solo las columnas que contienen "_Lag_" en el nombre (es decir, las que tienen retardos aplicados)
variables_retardos <- names(datos_estacionarios_retardos)[grepl("_Lag_", names(datos_estacionarios_retardos))]

# ---- CALCULAR MATRIZ DE CORRELACIÓN CON VARIABLES DE RETARDOS ----
# Calculamos la matriz de correlación para las variables con retardos aplicados
matriz_correlacion_retardos <- cor(datos_estacionarios_retardos[, variables_retardos], use = "pairwise.complete.obs")

# ---- CREAR HEATMAP DE CORRELACIONES SOLO CON RETARDOS ----
corrplot(matriz_correlacion_retardos, 
         method = "color", 
         col = brewer.pal(n = 8, name = "RdBu"),
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45, 
         title = "Heatmap de Correlaciones (Datos Diarios: Estacionarios + Retardos Óptimos) Junio 2023 - Diciembre 2024", 
         mar = c(0, 0, 2, 0))

# ---- CREAR TABLA DE CORRELACIONES CON RETARDOS ----
# Convertimos la matriz de correlación en un data frame
matriz_correlacion_retardos_df <- as.data.frame(matriz_correlacion_retardos)
matriz_correlacion_retardos_df <- matriz_correlacion_retardos_df %>%
  rownames_to_column(var = "Variable")  # Agregar nombres de variables como columna

# Ordenamos la tabla por la relación con "Precio Demanda_Lag_X" (ajustar el nombre exacto de la variable)
precio_demanda_variable <- grep("Precio_Demanda_Lag", variables_retardos, value = TRUE)[1]  # Detectar el nombre exacto
tabla_matriz_correlacion_retardos <- matriz_correlacion_retardos_df %>%
  arrange(desc(!!sym(precio_demanda_variable)))  # Ordenar por correlación con la variable de precio demanda lag

# Formateamos la tabla con letras más pequeñas
tabla_matriz_correlacion_retardos_kable <- kable(tabla_matriz_correlacion_retardos, 
                                                 format = "html", 
                                                 digits = 3, 
                                                 row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = 10) %>%  # Reducir tamaño de fuente
  column_spec(1, bold = TRUE, background = "lightblue") %>%  # Resaltar la primera columna
  row_spec(0, background = "deepskyblue", color = "white")  # Estilo para el encabezado

# Mostrar la tabla ordenada
tabla_matriz_correlacion_retardos_kable


# Mostrar la tabla ordenada
tabla_matriz_correlacion_retardos_kable


