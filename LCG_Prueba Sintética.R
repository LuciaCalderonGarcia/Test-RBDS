# ============================================================================
# ANÁLISIS DE CAMBIOS ESTRUCTURALES EN SERIES TEMPORALES CON TEST BDS RECURSIVO 
#                           PRUEBA SINTÉTICA
# ============================================================================

# Cargar librerías necesarias
library(ggplot2)
library(forecast)
library(strucchange)
library(dplyr)
library(kableExtra)
library(tseries)      # Para test BDS
library(patchwork)    # Para combinar gráficos
library(furrr)        # Para paralelización
library(future)       # Para configurar la paralelización
library(zoo)          # Para series temporales
library(pracma)       # Para detección de picos
library(gridExtra)    # Para combinar tablas y gráficos
library(grid)         # Para manejo de gráficos avanzados
library(htmltools)    # Para manipular HTML

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
# GENERACIÓN Y ANÁLISIS DE SERIE SINTÉTICA CON CAMBIOS ESTRUCTURALES
# ============================================================================

# Definir parámetros del análisis
m <- 5           # Dimensión para el test BDS
N0 <- 50         # Tamaño inicial de la subserie
nstd <- 0.5      # Número de desviaciones estándar para el radio
lapso <- 1       # Intervalo de crecimiento para cada iteración
Maxm <- 5        # Máxima dimensión a considerar (para calcular bdsM)

# Crear serie sintética diaria
set.seed(123)  # Para reproducibilidad

# Definir fechas
fechas <- seq(from = as.Date("2020-01-01"),
              to = as.Date("2024-12-31"),
              by = "day")

# Inicializar serie
series <- numeric(length(fechas))

# Definir diferentes procesos generadores con cambios estructurales en fechas específicas
for (i in 1:length(fechas)) {
  dia <- as.numeric(difftime(fechas[i], as.Date("2020-01-01"), units = "days"))
  
  if (dia <= 180) { 
    # Enero 2020 - Junio 2020 (ARIMA(0,1,0))
    series[i] <- sum(rnorm(1, mean = 0, sd = 1))
  } else if (dia > 180 & dia <= 425) { 
    # Julio 2020 - Febrero 2021 (ARIMA(0,2,1))
    arima_021 <- arima.sim(model = list(order = c(0,2,1), ma = 0.3), n = 10)
    series[i] <- tail(arima_021, 1)
  } else if (dia > 425 & dia <= 730) { 
    # Marzo 2021 - Diciembre 2021 (ARIMA(0,3,0))
    series[i] <- sum(cumsum(cumsum(rnorm(1, mean = 0, sd = 1))))
  } else if (dia > 730 & dia <= 1095) { 
    # Enero 2022 - Diciembre 2022 (ARIMA(1,1,3))
    arima_113 <- arima.sim(model = list(order = c(1,1,3), ar = 0.5, ma = c(0.4, -0.2, 0.3)), n = 10)
    series[i] <- tail(arima_113, 1)
  } else { 
    # Enero 2023 - Diciembre 2024 (ARIMA(1,3,2))
    arima_132 <- arima.sim(model = list(order = c(1,3,2), ar = 0.6, ma = c(-0.4, 0.2)), n = 10)
    series[i] <- tail(arima_132, 1)
  }
}

# Crear dataframe con la serie sintética
df_sintetica <- data.frame(Fecha = fechas, Valor = series)

# Fechas conocidas de cambios estructurales
cambios_reales <- as.Date(c("2020-07-01", "2021-03-01", "2022-01-01", "2023-01-01"))

# ============================================================================
# ANÁLISIS DE LA SERIE SINTÉTICA
# ============================================================================

# Ejecutar test BDS recursivo
bdsout <- bds_recurV3(df_sintetica$Valor, N0 = N0, m = m, nstd = nstd, lapso = lapso, tracebar = TRUE)

# Asignar fechas correctas
bdsout$Fecha <- df_sintetica$Fecha[N0:length(df_sintetica$Valor)]

# Calcular bdsM (resumen normalizado del test BDS)
bdsout$bdsM <- rowMeans(scale(bdsout[, c(3:(min(Maxm+1, m+1)))]))

# Detectar cambios estructurales usando la función personalizada
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
# COMPARACIÓN DE RESULTADOS
# ============================================================================

# Crear tabla de cambios reales
tabla_cambios_reales <- data.frame(
  "Cambios_Reales" = format(cambios_reales, "%d/%m/%Y")
)
tabla_cambios_reales
# Crear tabla con los cambios detectados
tabla_cambios_detectados <- data.frame(
  "Método" = c(rep("Picos", nrow(cambios_bds_picos)), 
               rep("Gradiente", nrow(cambios_bds_gradiente))),
  "Fecha_Detectada" = c(format(cambios_bds_picos$Fecha, "%d/%m/%Y"),
                        format(cambios_bds_gradiente$Fecha, "%d/%m/%Y")),
  "Valor_BDS" = c(round(cambios_bds_picos$Valor_BDS, 3),
                  round(cambios_bds_gradiente$Valor_BDS, 3))
)
tabla_cambios_detectados

# ============================================================================
# VISUALIZACIÓN DE RESULTADOS 
# ============================================================================

# Definir fechas para mostrar en el eje X
fechas_eje_x <- c(min(df_sintetica$Fecha), cambios_reales, max(df_sintetica$Fecha))

# Graficar la serie temporal original
grafico_serie <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) +
  geom_line(color = "deepskyblue4", size = 0.8) +
  geom_vline(xintercept = cambios_reales, col = "skyblue", linetype = "dashed", size = 1) +
  labs(title = "Serie Temporal Sintética con Cambios Estructurales Reales",
       x = "Fecha", y = "Valor") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold",  hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(breaks = fechas_eje_x, date_labels = "%d/%m/%Y")
grafico_serie

# Graficar el test BDS con detección de cambios
grafico_bds <- ggplot(bdsout, aes(x = Fecha)) +
  geom_line(aes(y = bdsM), color = "deepskyblue4", size = 1) +
  geom_vline(xintercept = cambios_reales, col = "skyblue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cambios_bds_picos$Fecha, col = "springgreen4", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cambios_bds_gradiente$Fecha, col = "purple", linetype = "dashed", size = 1) +
  labs(title = "Test BDS con Cambios Estructurales Detectados",
       x = "Fecha", y = "Valor Normalizado BDS",
       subtitle = "Líneas azules: cambios reales, Líneas verdes: cambios max - min, Líneas moradas: cambios gradiente") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold",  hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(breaks = fechas_eje_x, date_labels = "%d/%m/%Y")
grafico_bds
# Guardar los gráficos combinados
combinados <- grafico_serie / grafico_bds

# ============================================================================
# APLICAR TAMBIÉN OTROS TESTS DE CAMBIO ESTRUCTURAL
# ============================================================================

# Test OLS-MOSUM para comparación
h_0 <- 0.15  # Tamaño de la ventana
modelo <- bdsM ~ Fecha
modelo.lm <- lm(modelo, data = bdsout, na.action = na.omit)

# Ejecutar test OLS-MOSUM
testestbr <- efp(modelo, type = "OLS-MOSUM", data = bdsout, h = h_0)
bandas <- boundary(testestbr, type = "level", level = 0.05)

# Detectar cambios estructurales con OLS-MOSUM
# Asegurarnos que rowmax es un vector numérico sin NAs
rowmax <- abs(as.numeric(testestbr$process))
rowmax <- na.omit(rowmax)

# Verificar que tenemos datos válidos antes de buscar picos
puntos_cambio_mosum <- NULL
if(length(rowmax) > 0 && all(!is.na(rowmax))) {
  tryCatch({
    maximos <- pracma::findpeaks(rowmax, minpeakdistance = max(1, round(length(rowmax) * 0.05)), npeaks = 10)
    if (!is.null(maximos) && nrow(maximos) > 0) {
      indices_maximos <- maximos[,2]
      # Asegurarse que los índices están dentro del rango válido
      indices_maximos <- indices_maximos[indices_maximos <= nrow(bdsout)]
      puntos_cambio_mosum <- bdsout$Fecha[indices_maximos]
    }
  }, error = function(e) {
    cat("Error en findpeaks para MOSUM:", e$message, "\n")
  })
} else {
  cat("No hay datos válidos para analizar con OLS-MOSUM\n")
}
puntos_cambio_mosum

# Test OLS-CUSUM para comparación
testcusum <- efp(modelo, type = "OLS-CUSUM", data = bdsout, h = h_0)
bandas_cusum <- boundary(testcusum, type = "level", level = 0.05)

# Convertir el proceso a vector y ajustar longitud
Test_Process <- as.numeric(testcusum$process)
Test_Process <- na.omit(Test_Process)

# Detectar picos (máximos locales) en el test CUSUM
puntos_cambio_cusum <- NULL
if(length(Test_Process) > 0 && all(!is.na(Test_Process))) {
  rowmax_cusum <- abs(Test_Process)
  tryCatch({
    maximos_cusum <- pracma::findpeaks(rowmax_cusum, minpeakdistance = max(1, round(length(rowmax_cusum) * 0.05)), npeaks = 10)
    if (!is.null(maximos_cusum) && nrow(maximos_cusum) > 0) {
      indices_maximos_cusum <- maximos_cusum[,2]
      # Asegurarse que los índices están dentro del rango válido
      indices_maximos_cusum <- indices_maximos_cusum[indices_maximos_cusum <= nrow(bdsout)]
      puntos_cambio_cusum <- bdsout$Fecha[indices_maximos_cusum]
    }
  }, error = function(e) {
    cat("Error en findpeaks para CUSUM:", e$message, "\n")
  })
} else {
  cat("No hay datos válidos para analizar con OLS-CUSUM\n")
}
puntos_cambio_cusum

# Crear tabla comparativa de métodos
resultados_comparados <- data.frame(
  "Método" = c("Real", "BDS (Picos)", "BDS (Gradiente)", "OLS-MOSUM", "OLS-CUSUM"),
  "Fechas_Detectadas" = c(
    paste(format(cambios_reales, "%d/%m/%Y"), collapse = ", "),
    paste(format(cambios_bds_picos$Fecha, "%d/%m/%Y"), collapse = ", "),
    paste(format(cambios_bds_gradiente$Fecha, "%d/%m/%Y"), collapse = ", "),
    if(!is.null(puntos_cambio_mosum)) paste(format(puntos_cambio_mosum, "%d/%m/%Y"), collapse = ", ") else "No detectados",
    if(!is.null(puntos_cambio_cusum)) paste(format(puntos_cambio_cusum, "%d/%m/%Y"), collapse = ", ") else "No detectados"
  )
)


# ============================================================================
# VISUALIZACIÓN DE RESULTADOS INDIVIDUALES POR MÉTODO
# ============================================================================

# Definir paleta de colores para los diferentes métodos
colores_metodos <- c("Real" = "skyblue", "BDS (Picos)" = "springgreen4", 
                     "BDS (Gradiente)" = "purple", 
                     "OLS-MOSUM" = "violetred2", "OLS-CUSUM" = "orange")

# Definir estilos de líneas para los diferentes métodos
lineas_metodos <- c("Real" = "dashed", "BDS (Picos)" = "dotted", 
                    "BDS (Gradiente)" = "dotdash", "OLS-MOSUM" = "longdash", 
                    "OLS-CUSUM" = "twodash")

# 1. Graficar la serie temporal original con los cambios reales
grafico_real <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_reales, 
             color = colores_metodos["Real"], 
             linetype = lineas_metodos["Real"], 
             size = 1) + 
  labs(title = "Serie Temporal con Cambios Estructurales Reales", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom")

# 2. Graficar la serie temporal con los cambios detectados por BDS (Picos)
grafico_bds_picos <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_bds_picos$Fecha, 
             color = colores_metodos["BDS (Picos)"], 
             linetype = lineas_metodos["BDS (Picos)"], 
             size = 1) + 
  labs(title = "Serie Temporal con Cambios Detectados por BDS (Picos)", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom")

# 3. Graficar la serie temporal con los cambios detectados por BDS (Gradiente)
grafico_bds_gradiente <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_bds_gradiente$Fecha, 
             color = colores_metodos["BDS (Gradiente)"], 
             linetype = lineas_metodos["BDS (Gradiente)"], 
             size = 1) + 
  labs(title = "Serie Temporal con Cambios Detectados por BDS (Gradiente)", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom")

# 4. Graficar la serie temporal con los cambios detectados por OLS-MOSUM
grafico_mosum <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = puntos_cambio_mosum, 
             color = colores_metodos["OLS-MOSUM"], 
             linetype = lineas_metodos["OLS-MOSUM"], 
             size = 1) + 
  labs(title = "Serie Temporal con Cambios Detectados por OLS-MOSUM", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom")
grafico_mosum

# 5. Graficar la serie temporal con los cambios detectados por OLS-CUSUM
grafico_cusum <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = puntos_cambio_cusum, 
             color = colores_metodos["OLS-CUSUM"], 
             linetype = lineas_metodos["OLS-CUSUM"], 
             size = 1) + 
  labs(title = "Serie Temporal con Cambios Detectados por OLS-CUSUM", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom")

# 6. Opcionalmente, graficar los cambios reales vs cada método por separado
# Real vs BDS Picos
grafico_real_vs_bds_picos <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_reales, 
             color = colores_metodos["Real"], 
             linetype = lineas_metodos["Real"], 
             size = 1) + 
  geom_vline(xintercept = cambios_bds_picos$Fecha, 
             color = colores_metodos["BDS (Picos)"], 
             linetype = lineas_metodos["BDS (Picos)"], 
             size = 1) + 
  labs(title = "Comparación: Cambios Reales vs BDS (Picos)", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) +
  # Añadir leyenda
  annotate("text", x = min(df_sintetica$Fecha), y = max(df_sintetica$Valor), 
           label = "— Real\n... BDS (Picos)", 
           hjust = 0, vjust = 1)

# Real vs BDS Gradiente
grafico_real_vs_bds_gradiente <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_reales, 
             color = colores_metodos["Real"], 
             linetype = lineas_metodos["Real"], 
             size = 1) + 
  geom_vline(xintercept = cambios_bds_gradiente$Fecha, 
             color = colores_metodos["BDS (Gradiente)"], 
             linetype = lineas_metodos["BDS (Gradiente)"], 
             size = 1) + 
  labs(title = "Comparación: Cambios Reales vs BDS (Gradiente)", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) +
  # Añadir leyenda
  annotate("text", x = min(df_sintetica$Fecha), y = max(df_sintetica$Valor), 
           label = "— Real\n-·- BDS (Gradiente)", 
           hjust = 0, vjust = 1)

# Real vs OLS-MOSUM
grafico_real_vs_mosum <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_reales, 
             color = colores_metodos["Real"], 
             linetype = lineas_metodos["Real"], 
             size = 1) + 
  geom_vline(xintercept = puntos_cambio_mosum, 
             color = colores_metodos["OLS-MOSUM"], 
             linetype = lineas_metodos["OLS-MOSUM"], 
             size = 1) + 
  labs(title = "Comparación: Cambios Reales vs OLS-MOSUM", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) +
  # Añadir leyenda
  annotate("text", x = min(df_sintetica$Fecha), y = max(df_sintetica$Valor), 
           label = "— Real\n-- OLS-MOSUM", 
           hjust = 0, vjust = 1)

# Real vs OLS-CUSUM
grafico_real_vs_cusum <- ggplot(df_sintetica, aes(x = Fecha, y = Valor)) + 
  geom_line(color = "deepskyblue4", size = 0.8) + 
  geom_vline(xintercept = cambios_reales, 
             color = colores_metodos["Real"], 
             linetype = lineas_metodos["Real"], 
             size = 1) + 
  geom_vline(xintercept = puntos_cambio_cusum, 
             color = colores_metodos["OLS-CUSUM"], 
             linetype = lineas_metodos["OLS-CUSUM"], 
             size = 1) + 
  labs(title = "Comparación: Cambios Reales vs OLS-CUSUM", 
       x = "Fecha", y = "Valor") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) +
  # Añadir leyenda
  annotate("text", x = min(df_sintetica$Fecha), y = max(df_sintetica$Valor), 
           label = "— Real\n-·- OLS-CUSUM", 
           hjust = 0, vjust = 1)

# Para mostrar las gráficas individuales
print(grafico_real)
print(grafico_bds_picos)
print(grafico_bds_gradiente)
print(grafico_mosum)
print(grafico_cusum)

# Para mostrar las gráficas comparativas
print(grafico_real_vs_bds_picos)
print(grafico_real_vs_bds_gradiente)
print(grafico_real_vs_mosum)
print(grafico_real_vs_cusum)


   




























































































