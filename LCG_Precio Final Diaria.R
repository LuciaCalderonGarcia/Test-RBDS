# ============================================================================
# ANÁLISIS DE CAMBIOS ESTRUCTURALES EN SERIES TEMPORALES CON TEST BDS RECURSIVO 
#                     APLICACIÓN A DATOS REALES DE DEMANDA
# ============================================================================

# Cargar librerías necesarias
library(ggplot2)      # Para visualización
library(forecast)     # Para análisis de series temporales
library(strucchange)  # Para tests de cambio estructural
library(dplyr)        # Para manipulación de datos
library(kableExtra)   # Para tablas formateadas
library(tseries)      # Para test BDS
library(patchwork)    # Para combinar gráficos
library(furrr)        # Para paralelización
library(future)       # Para configurar la paralelización
library(zoo)          # Para series temporales
library(pracma)       # Para detección de picos
library(gridExtra)    # Para combinar tablas y gráficos
library(grid)         # Para manejo de gráficos avanzados
library(htmltools)    # Para manipular HTML
library(readxl)       # Para leer datos de Excel
library(tidyr)        # Para transformación de datos
library(scales)       # Para formateo de ejes en gráficos

# ---- DATOS ---- 

ruta_archivo <- "C:/Users/lucia/OneDrive/Escritorio/Asignaturas/Cuarto/TFG/LuciaCalderon_DatosTFG.xlsx"
datosD3 <- read_excel(ruta_archivo, sheet = "D_Pfdemanda")

# ============================================================================
# FUNCIÓN: BDS RECURSIVO (VERSIÓN OPTIMIZADA CON PARALELIZACIÓN)
# ============================================================================

bds_recurV3 <- function(xserie, N0 = 50, m = 6, nstd = 0.75, lapso = 5, tracebar = TRUE) {
  # Parámetros:
  # xserie: Serie temporal a analizar
  # N0: Tamaño inicial de la subserie
  # m: Dimensión para el test BDS
  # nstd: Número de veces la desviación estándar para definir el radio
  # lapso: Intervalo de crecimiento en cada iteración
  # tracebar: Mostrar barra de progreso (TRUE/FALSE)
  
  require(tseries)
  require(zoo)
  require(furrr)
  
  # Configurar paralelización
  no_cores <- availableCores() - 1
  plan(multisession, workers = no_cores)
  
  # Calcular número de subconjuntos
  NDimBds <- length(seq(from = N0, to = length(xserie), by = lapso))
  
  cat("\nPreparando datos... \n")
  
  # Crear lista de subseries
  subconjuntos <- future_map(
    seq(from = N0, to = length(xserie), by = lapso), 
    ~ xserie[1:.x],
    .progress = tracebar
  )
  
  cat("\nCalculando BDS recursivo... \n")
  
  # Aplicar test BDS a cada subserie
  bdsout <- future_map(
    subconjuntos, 
    ~ bds.test(.x, m = m, eps = sd(.x) * nstd),
    .progress = tracebar
  )
  
  cat("\nCálculo BDS recursivo completado! ... preparando salida \n")
  
  # Convertir resultados a data frame
  bdsout <- as.data.frame(matrix(unlist(bdsout), nrow = length(bdsout), byrow = TRUE))
  bdsout <- bdsout[, c(1:((m-1)*2))]
  bdsout[] <- lapply(bdsout, as.numeric)
  names(bdsout) <- c(paste0("m", 2:m), paste0("p-value", 2:m))
  bdsout$indice <- seq(from = N0, to = length(xserie), by = lapso)
  bdsout$Fecha <- as.Date(index(xserie[seq(from = N0, to = length(xserie), by = lapso)]))
  bdsout <- bdsout[, c(((m-1)*2+1), ((m-1)*2+2), c(1:((m-1)*2)))]
  
  # Volver al modo secuencial
  plan(sequential)
  
  cat("\nProceso completado!\n")
  return(bdsout)
}

# ============================================================================
# FUNCIÓN: DETECTAR CAMBIOS ESTRUCTURALES EN SERIE BDS
# ============================================================================

detectar_cambios_bds <- function(bdsout, metodo = "gradiente", span_porcentaje = 0.10, umbral = 0.5) {
  # Parámetros:
  # bdsout: Dataframe con resultados del test BDS recursivo
  # metodo: "gradiente" (cambios abruptos) o "picos" (máximos/mínimos locales)
  # span_porcentaje: Porcentaje de datos para considerar un pico como significativo
  # umbral: Umbral de magnitud para considerar un cambio como significativo
  
  # Asegurar que bdsM existe
  if(!"bdsM" %in% colnames(bdsout)) {
    stop("La columna 'bdsM' no existe en el dataframe. Calcúlala primero.")
  }
  
  # Método 1: Detección por cambios en el gradiente
  if(metodo == "gradiente") {
    # Calcular diferencias (gradiente)
    bdsout$gradiente <- c(0, diff(bdsout$bdsM))
    
    # Detectar cambios significativos (mayor que el umbral)
    cambios_sig <- which(abs(bdsout$gradiente) > (umbral * sd(bdsout$gradiente, na.rm = TRUE)))
    
    # Filtrar cambios muy cercanos (tomar el más fuerte en ventanas de span)
    span <- round(nrow(bdsout) * span_porcentaje)
    indices_cambio <- numeric(0)
    
    i <- 1
    while(i <= length(cambios_sig)) {
      ventana <- cambios_sig[which(cambios_sig >= cambios_sig[i] & 
                                     cambios_sig <= cambios_sig[i] + span)]
      
      if(length(ventana) > 0) {
        # Encontrar el cambio más significativo en la ventana
        ventana_abs <- abs(bdsout$gradiente[ventana])
        max_cambio <- ventana[which.max(ventana_abs)]
        indices_cambio <- c(indices_cambio, max_cambio)
        
        # Saltar la ventana
        i <- i + length(ventana)
      } else {
        i <- i + 1
      }
    }
  }
  
  # Método 2: Detección por picos
  if(metodo == "picos") {
    # Identificar picos (máximos locales)
    span <- round(nrow(bdsout) * span_porcentaje)
    maximos <- pracma::findpeaks(bdsout$bdsM, minpeakdistance = span, npeaks = 10)
    minimos <- pracma::findpeaks(-bdsout$bdsM, minpeakdistance = span, npeaks = 10)
    
    # Si no hay picos suficientes, reducir restricciones
    if(is.null(maximos) || nrow(maximos) < 2) {
      maximos <- pracma::findpeaks(bdsout$bdsM, minpeakdistance = round(span/2), npeaks = 10)
    }
    if(is.null(minimos) || nrow(minimos) < 2) {
      minimos <- pracma::findpeaks(-bdsout$bdsM, minpeakdistance = round(span/2), npeaks = 10)
    }
    
    # Extraer índices de picos significativos
    indices_maximos <- if(!is.null(maximos) && nrow(maximos) > 0) maximos[,2] else numeric(0)
    indices_minimos <- if(!is.null(minimos) && nrow(minimos) > 0) minimos[,2] else numeric(0)
    
    # Combinar y ordenar todos los picos
    indices_cambio <- sort(c(indices_maximos, indices_minimos))
  }
  
  # Convertir índices a fechas de cambio
  fechas_cambio <- bdsout$Fecha[indices_cambio]
  
  # Crear dataframe de resultados
  resultados <- data.frame(
    Indice = indices_cambio,
    Fecha = fechas_cambio,
    Valor_BDS = bdsout$bdsM[indices_cambio]
  )
  
  return(resultados)
}


# ============================================================================
# CONFIGURACIÓN NECESARIA PARA BDS
# ============================================================================

#    OJO → configuración tanto para serie DIARIA como INTRADIARIA 

#Correción de outliers  
corregir_outliers <- FALSE  # Cambiar a FALSE si no quieres corregir outliers o TRUE si sí 

#Definir parametros (ir probando diferentes casos)
m <- 6        # Dimensión para el test BDS (número de valores pasados)
N0 <- 50      # Tamaño inicial de la subserie 
#Al aplicar un N0 muy bajo las primeras subseries del BDS han dado valores altamente inestables 
nstd <- 0.5   # Número de veces la desviación estándar para definir el radio
lapso <- 1    # Intervalo de crecimiento en cada iteración 

Maxm <- 5  # Número máximo de dimensiones a considerar en el test BDS - nunca superior a m (lo aplicamos para calcular bdsM)



# ============================================================================
# PASOS PREVIOS A APLICACIÓN
# ============================================================================

# CONVERTIR LA SERIE A DIARIA 

colnames(datosD3) <- c("Fecha", paste0(1:24, "h"))

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

datosD3_diario <- datosD3_long %>%
  group_by(Fecha) %>%
  summarise(PFdemanda_promedio = mean(Precio_Demanda, na.rm = TRUE), .groups = 'drop')

datosD3_diario <- datosD3_diario[order(datosD3_diario$Fecha), ]


# ESTACIONARIEDAD 

y0 <- datosD3_diario$PFdemanda_promedio

# VerificaMOS si la serie es estacionaria
adf_test <- adf.test(y0)  # Test de Dickey-Fuller aumentado
kpss_test <- kpss.test(y0)  # Test de KPSS

#Si no es estacionaria, la diferenciamos 
ndiff <- ndiffs(y0)  # Determina cuántas diferencias hacer para que sea estacionaria
y <- diff(y0, differences = ndiff)  # Diferenciar la serie

# Graficamos ambas series 

# Crear dataframe con la serie original
df_original <- data.frame(
  Fecha = datosD3_diario$Fecha,  # Asegurarse de que `Fecha` está en formato Date
  Precio = y0
)

# Crear dataframe con la serie diferenciada
df_diferenciada <- data.frame(
  Fecha = datosD3_diario$Fecha[-1],  # Se pierde el primer valor tras diferenciar
  Precio_Diferenciado = y
)

# Definimos los puntos donde queremos que aparezcan las marcas en el eje X
breaks_fechas <- seq(from = min(df_original$Fecha), to = max(df_original$Fecha), by = "3 months")  # Cada 6 meses

# Utilizo ggplot para representar ambas series y luego mostrarlas en una sola imagen 
p_original <- ggplot(df_original, aes(x = Fecha, y = Precio)) +
  geom_line(color = "deepskyblue4") +
  labs(title = "Serie Original: Precio Final de la Demanda", x = "Fecha", y = "Precio") +
  scale_x_date(breaks = breaks_fechas, labels = date_format("%d/%m/%Y")) +  # Formato personalizado
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Inclinamos las fechas para mejor visibilidad
  )

p_diferenciada <- ggplot(df_diferenciada, aes(x = Fecha, y = Precio_Diferenciado)) +
  geom_line(color = "deepskyblue4") +
  labs(title = paste("Serie Diferenciada (D=", ndiff, "): Precio Final de la Demanda"), x = "Fecha", y = "Precio Diferenciado") +
  scale_x_date(breaks = breaks_fechas, labels = date_format("%d/%m/%Y")) +  # Formato personalizado
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Inclinamos las fechas para mejor visibilidad
  )

p_original / p_diferenciada  # Usa `patchwork` para apilarlas verticalmente



# DETECTAR Y CORREGIR OUTLIERS (SI LO HEMOS INDICADO AL INICIO DEL CÓDGIO)

if (corregir_outliers) {
  cat("\n🔹 Detectando y corrigiendo outliers en la serie...\n")
  
  media_y <- mean(y, na.rm = TRUE)  # Calculamos la media de la serie
  sd_y <- sd(y, na.rm = TRUE)       # Calculamos la desviación estándar
  
  # Calculamos los valores z-score
  # Mide cuanda desviaciones estandar está un valor por encima/debajo de la media 
  z_scores <- (y - media_y) / sd_y
  
  # Definimos un umbral para detectar outliers (z-score > 3 o < -3)
  umbral <- 3
  
  # Identificamos los índices de outliers
  outliers <- which(abs(z_scores) > umbral)
  
  # Reemplazamos los outliers por la media de la serie
  if (length(outliers) > 0) {
    y[outliers] <- media_y
    cat("\n Se corrigieron", length(outliers), "outliers en la serie.\n")
  } else {
    cat("\n No se detectaron outliers en la serie.\n")
  }
}


# ============================================================================
# APLICACIÓN DEL TEST BDS Y DETECCIÓN DE CAMBIO ESTRUCTURAL
# ============================================================================

# Ejecutamos el test BDS 
bdsout <- bds_recurV3(y, N0=N0, m=m, nstd=nstd, lapso=lapso, tracebar=TRUE)

# Asignar las fechas correctas a `bdsout` - el tamaño varia respecto a la serie original`
bdsout$Fecha <- tail(datosD3_diario$Fecha, n = nrow(bdsout))

# Definimos las marcas de fecha en el eje X para cuando grafiquemos (cada 3 meses)
breaks_fechas <- seq(from = min(bdsout$Fecha), 
                     to = max(bdsout$Fecha), 
                     by = "3 months")

# Calcular `bdsM` (Resumen del test BDS)
bdsout$bdsM <- rowMeans(scale(bdsout[, c(4:(min(Maxm, m+1)))]))  

# Graficamos el test BDS
g_bds <- ggplot(bdsout, aes(x = Fecha)) +
  geom_line(aes(y = scale(m3)), color = "deepskyblue4", size = 0.6) +
  #geom_line(aes(y = scale(m4)), color = "darkgreen", size = 0.6) +
  #geom_line(aes(y = scale(m5)), color = "purple", size = 0.6) +
  #geom_line(aes(y = bdsM), color = "black", linetype = "solid", size = 1) +
  labs(title = "Evolución del Test BDS en el Precio Final de la Demanda",        #OJO si hemos quitado outliers señalarlo en el titulo ( "- outliers ajustados")
       x = "Fecha",
       y = "Valor Normalizado BDS") +
  scale_x_date(breaks = breaks_fechas, labels = date_format("%d/%m/%Y")) +  # Marcas cada 3 meses
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(g_bds)


# Método por picos (máximos y mínimos locales)
cambios_bds_picos <- detectar_cambios_bds(bdsout, 
                                          metodo = "picos", 
                                          span_porcentaje = 0.05,  # 5% de los datos
                                          umbral = 0.5)
cambios_bds_picos

# Método por gradiente (cambios abruptos)
cambios_bds_gradiente <- detectar_cambios_bds(bdsout, 
                                              metodo = "gradiente", 
                                              span_porcentaje = 0.05,  # 5% de los datos
                                              umbral = 0.8)
cambios_bds_gradiente


# ============================================================================
# VISUALIZACIÓN DE CAMBIOS ESTRUCTURALES DETECTADOS EN EL TEST BDS
# ============================================================================

# Definir fechas para el eje X
fechas_eje_x <- seq(from = min(bdsout$Fecha), 
                    to = max(bdsout$Fecha), 
                    by = "3 months")

# Crear gráfico del test BDS normalizado con detección de cambios

grafico_bds <- ggplot(bdsout, aes(x = Fecha)) +
  geom_line(aes(y = bdsM), color = "deepskyblue4", size = 1) +
  geom_vline(xintercept = cambios_bds_picos$Fecha, 
             color = "springgreen4", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cambios_bds_gradiente$Fecha, 
             color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Test BDS con Cambios Estructurales Detectados",
       x = "Fecha", y = "Valor Normalizado BDS",
       subtitle = "Verde: cambios max - min, Morado: Cambios gradiente") +
  scale_x_date(breaks = fechas_eje_x, date_labels = "%d/%m/%Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Mostrar el gráfico
print(grafico_bds)


# ============================================================================
# VISUALIZACIÓN DE CAMBIOS BDS (PICOS Y GRADIENTE) SOBRE LA SERIE ORIGINAL
# ============================================================================

grafico_bds_original <- ggplot(df_original, aes(x = Fecha, y = Precio)) +
  geom_line(color = "deepskyblue4", size = 0.8) +
  
  # Cambios detectados por BDS (Picos)
  geom_vline(xintercept = cambios_bds_picos$Fecha, 
             color = "springgreen4", 
             linetype = "dotted", size = 1) +
  
  # Cambios detectados por BDS (Gradiente)
  geom_vline(xintercept = cambios_bds_gradiente$Fecha, 
             color = "purple", 
             linetype = "dotdash", size = 1) +
  
  labs(title = "Cambios Detectados por el Test BDS sobre la Serie Original",
       subtitle = "Líneas verdes: cambios max - min, Líneas moradas: cambios gradientes",
       x = "Fecha", y = "Precio Final de la Demanda") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Mostrar el gráfico
print(grafico_bds_original)


# ============================================================================
# TESTS ADICIONALES DE CAMBIO ESTRUCTURAL: OLS-MOSUM Y OLS-CUSUM
# ============================================================================

# Crear variable de tiempo como regresora artificial
bdsout$Tiempo <- 1:nrow(bdsout)

# Definir modelo de regresión para el test
modelo <- bdsM ~ Tiempo
h_0 <- 0.15  # Tamaño de la ventana

# ----------------------------
# Test OLS-MOSUM
# ----------------------------

test_mosum <- efp(modelo, data = bdsout, type = "OLS-MOSUM", h = h_0)
proceso_mosum <- abs(na.omit(as.numeric(test_mosum$process)))

puntos_cambio_mosum <- NULL
if(length(proceso_mosum) > 0) {
  tryCatch({
    maximos <- findpeaks(proceso_mosum, minpeakdistance = max(1, round(length(proceso_mosum) * 0.05)), npeaks = 10)
    if (!is.null(maximos) && nrow(maximos) > 0) {
      indices_maximos <- maximos[,2]
      indices_maximos <- indices_maximos[indices_maximos <= nrow(bdsout)]
      puntos_cambio_mosum <- bdsout$Fecha[indices_maximos]
    }
  }, error = function(e) {
    cat("Error en MOSUM:", e$message, "\n")
  })
} else {
  cat("No hay datos válidos para analizar con OLS-MOSUM\n")
}
puntos_cambio_mosum

# ----------------------------
# Test OLS-CUSUM
# ----------------------------

test_cusum <- efp(modelo, data = bdsout, type = "OLS-CUSUM", h = h_0)
proceso_cusum <- abs(na.omit(as.numeric(test_cusum$process)))

puntos_cambio_cusum <- NULL
if(length(proceso_cusum) > 0) {
  tryCatch({
    maximos_cusum <- findpeaks(proceso_cusum, minpeakdistance = max(1, round(length(proceso_cusum) * 0.05)), npeaks = 10)
    if (!is.null(maximos_cusum) && nrow(maximos_cusum) > 0) {
      indices_maximos_cusum <- maximos_cusum[,2]
      indices_maximos_cusum <- indices_maximos_cusum[indices_maximos_cusum <= nrow(bdsout)]
      puntos_cambio_cusum <- bdsout$Fecha[indices_maximos_cusum]
    }
  }, error = function(e) {
    cat("Error en CUSUM:", e$message, "\n")
  })
} else {
  cat("No hay datos válidos para analizar con OLS-CUSUM\n")
}
puntos_cambio_cusum
# ----------------------------
# Tabla comparativa de métodos detectados (sin cambios reales)
# ----------------------------

resultados_comparados <- data.frame(
  "Método" = c("BDS (Picos)", "BDS (Gradiente)", "OLS-MOSUM", "OLS-CUSUM"),
  "Fechas_Detectadas" = c(
    paste(format(cambios_bds_picos$Fecha, "%d/%m/%Y"), collapse = ", "),
    paste(format(cambios_bds_gradiente$Fecha, "%d/%m/%Y"), collapse = ", "),
    if (!is.null(puntos_cambio_mosum)) paste(format(puntos_cambio_mosum, "%d/%m/%Y"), collapse = ", ") else "No detectados",
    if (!is.null(puntos_cambio_cusum)) paste(format(puntos_cambio_cusum, "%d/%m/%Y"), collapse = ", ") else "No detectados"
  )
)

# Mostrar tabla formateada si lo deseas
kable(resultados_comparados, caption = "Fechas detectadas por cada método") %>%
  kable_styling(full_width = FALSE, position = "center")




# ============================================================================
# VISUALIZACIÓN DE CAMBIOS ESTRUCTURALES DETECTADOS SOBRE EL TEST BDS (bdsM)
# ============================================================================

# Paleta de colores y estilos para los métodos detectados
colores_metodos <- c("BDS (Picos)" = "springgreen4", 
                     "BDS (Gradiente)" = "purple", 
                     "OLS-MOSUM" = "violetred2", 
                     "OLS-CUSUM" = "orange")

lineas_metodos <- c("BDS (Picos)" = "dotted", 
                    "BDS (Gradiente)" = "dotdash", 
                    "OLS-MOSUM" = "longdash", 
                    "OLS-CUSUM" = "twodash")

# Gráfico 1: Cambios detectados por BDS (Picos)
grafico_bds_picos <- ggplot(bdsout, aes(x = Fecha, y = bdsM)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_bds_picos$Fecha, 
             color = colores_metodos["BDS (Picos)"], 
             linetype = lineas_metodos["BDS (Picos)"], 
             size = 1) + 
  labs(title = "Cambios Detectados por BDS (Picos)",
       x = "Fecha", y = "Valor Normalizado BDS") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 2: Cambios detectados por BDS (Gradiente)
grafico_bds_gradiente <- ggplot(bdsout, aes(x = Fecha, y = bdsM)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_bds_gradiente$Fecha, 
             color = colores_metodos["BDS (Gradiente)"], 
             linetype = lineas_metodos["BDS (Gradiente)"], 
             size = 1) + 
  labs(title = "Cambios Detectados por BDS (Gradiente)",
       x = "Fecha", y = "Valor Normalizado BDS") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 3: Cambios detectados por OLS-MOSUM
grafico_mosum <- ggplot(bdsout, aes(x = Fecha, y = bdsM)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = puntos_cambio_mosum, 
             color = colores_metodos["OLS-MOSUM"], 
             linetype = lineas_metodos["OLS-MOSUM"], 
             size = 1) + 
  labs(title = "Cambios Detectados por OLS-MOSUM",
       x = "Fecha", y = "Valor Normalizado BDS") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 4: Cambios detectados por OLS-CUSUM
grafico_cusum <- ggplot(bdsout, aes(x = Fecha, y = bdsM)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = puntos_cambio_cusum, 
             color = colores_metodos["OLS-CUSUM"], 
             linetype = lineas_metodos["OLS-CUSUM"], 
             size = 1) + 
  labs(title = "Cambios Detectados por OLS-CUSUM",
       x = "Fecha", y = "Valor Normalizado BDS") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar los gráficos (puedes combinarlos con patchwork si lo deseas)
print(grafico_bds_picos)
print(grafico_bds_gradiente)
print(grafico_mosum)
print(grafico_cusum)


# ============================================================================
# VISUALIZACIÓN CONJUNTA DE CAMBIOS OLS-MOSUM Y OLS-CUSUM SOBRE SERIE ORIGINAL
# ============================================================================

grafico_mosum_cusum <- ggplot(df_original, aes(x = Fecha, y = Precio)) +
  geom_line(color = "deepskyblue4", size = 0.8) +
  
  # Cambios detectados por OLS-MOSUM
  geom_vline(xintercept = puntos_cambio_mosum, 
             color = "violetred2", 
             linetype = "longdash", size = 1) +
  
  # Cambios detectados por OLS-CUSUM
  geom_vline(xintercept = puntos_cambio_cusum, 
             color = "orange", 
             linetype = "twodash", size = 1) +
  
  labs(title = "Cambios Detectados por OLS-MOSUM y OLS-CUSUM sobre la Serie Original",
       subtitle = "Líneas violetas: MOSUM, Líneas naranjas: CUSUM",
       x = "Fecha", y = "Precio Final de la Demanda") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Mostrar el gráfico
print(grafico_mosum_cusum)






















