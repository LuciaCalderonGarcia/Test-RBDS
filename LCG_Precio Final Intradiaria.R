# ============================================================================
# ANÁLISIS DE CAMBIOS ESTRUCTURALES EN SERIES TEMPORALES INTRADIARIAS CON TEST BDS RECURSIVO 
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
# FUNCIÓN: BDS RECURSIVO (VERSIÓN OPTIMIZADA CON PARALELIZACIÓN PARA MÚLTIPLES SERIES)
# ============================================================================

bds_recur_variablesmultiples <- function(data, N0=50, m=6, nstd=0.5, lapso=1, tracebar=TRUE) {
  require(tseries)
  require(zoo)
  require(furrr)
  
  # Configurar paralelización
  no_cores <- availableCores() - 1
  plan(multisession, workers = no_cores)
  
  # Procesar cada columna de datos (cada hora)
  resultados <- lapply(colnames(data), function(col) {
    xserie <- data[[col]]
    
    cat("\nPreparando datos para", col, "...\n")
    
    # Crear lista de subseries
    subconjuntos <- future_map(
      seq(from = N0, to = length(xserie), by = lapso), 
      ~ xserie[1:.x],
      .progress = tracebar
    )
    
    cat("\nCalculando BDS recursivo para", col, "...\n")
    
    # Aplicar test BDS a cada subserie
    bdsout <- future_map(
      subconjuntos, 
      ~ tryCatch(
        bds.test(.x, m = m, eps = sd(.x) * nstd),
        error = function(e) list(statistic = rep(NA, m-1), p.value = rep(NA, m-1))
      ),
      .progress = tracebar
    )
    
    cat("\nCálculo BDS recursivo completado para", col, "!\n")
    
    # Convertir resultados a data frame
    bdsout <- as.data.frame(matrix(unlist(bdsout), nrow = length(bdsout), byrow = TRUE))
    bdsout <- bdsout[, c(1:((m-1)*2))]
    bdsout[] <- lapply(bdsout, as.numeric)
    names(bdsout) <- c(paste0("m", 2:m), paste0("p-value", 2:m))
    bdsout$indice <- seq(from = N0, to = length(xserie), by = lapso)
    bdsout$Hora <- col
    
    return(bdsout)
  })
  
  # Volver al modo secuencial
  plan(sequential)
  
  cat("\nProceso completado!\n")
  
  # Combinar todos los resultados en un único dataframe
  return(bind_rows(resultados))
}

# ============================================================================
# FUNCIÓN: DETECTAR CAMBIOS ESTRUCTURALES EN SERIE BDS (PARA DATOS INTRADIARIOS)
# ============================================================================

detectar_cambios_bds_intradiario <- function(bdsout, hora, metodo = "gradiente", 
                                             span_porcentaje = 0.10, umbral = 0.5) {
  # Filtrar datos para la hora específica
  bds_hora <- bdsout %>% filter(Hora == hora)
  
  # Verificar si hay suficientes datos
  if(nrow(bds_hora) < 10) {
    return(data.frame(Indice = integer(0), Fecha = as.Date(character(0)), Valor_BDS = numeric(0)))
  }
  
  # Asegurar que bdsM existe
  if(!"bdsM" %in% colnames(bds_hora)) {
    stop("La columna 'bdsM' no existe en el dataframe. Calcúlala primero.")
  }
  
  # Método 1: Detección por cambios en el gradiente
  if(metodo == "gradiente") {
    # Calcular diferencias (gradiente)
    bds_hora$gradiente <- c(0, diff(bds_hora$bdsM))
    
    # Detectar cambios significativos (mayor que el umbral)
    cambios_sig <- which(abs(bds_hora$gradiente) > (umbral * sd(bds_hora$gradiente, na.rm = TRUE)))
    
    # Filtrar cambios muy cercanos (tomar el más fuerte en ventanas de span)
    span <- round(nrow(bds_hora) * span_porcentaje)
    indices_cambio <- numeric(0)
    
    i <- 1
    while(i <= length(cambios_sig)) {
      ventana <- cambios_sig[which(cambios_sig >= cambios_sig[i] & 
                                     cambios_sig <= cambios_sig[i] + span)]
      
      if(length(ventana) > 0) {
        # Encontrar el cambio más significativo en la ventana
        ventana_abs <- abs(bds_hora$gradiente[ventana])
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
    span <- round(nrow(bds_hora) * span_porcentaje)
    maximos <- tryCatch({
      pracma::findpeaks(bds_hora$bdsM, minpeakdistance = span, npeaks = 10)
    }, error = function(e) NULL)
    
    minimos <- tryCatch({
      pracma::findpeaks(-bds_hora$bdsM, minpeakdistance = span, npeaks = 10)
    }, error = function(e) NULL)
    
    # Si no hay picos suficientes, reducir restricciones
    if(is.null(maximos) || nrow(maximos) < 2) {
      maximos <- tryCatch({
        pracma::findpeaks(bds_hora$bdsM, minpeakdistance = round(span/2), npeaks = 10)
      }, error = function(e) NULL)
    }
    if(is.null(minimos) || nrow(minimos) < 2) {
      minimos <- tryCatch({
        pracma::findpeaks(-bds_hora$bdsM, minpeakdistance = round(span/2), npeaks = 10)
      }, error = function(e) NULL)
    }
    
    # Extraer índices de picos significativos
    indices_maximos <- if(!is.null(maximos) && nrow(maximos) > 0) maximos[,2] else numeric(0)
    indices_minimos <- if(!is.null(minimos) && nrow(minimos) > 0) minimos[,2] else numeric(0)
    
    # Combinar y ordenar todos los picos
    indices_cambio <- sort(c(indices_maximos, indices_minimos))
  }
  
  # Crear dataframe de resultados
  if(length(indices_cambio) > 0) {
    # Asignar fechas correspondientes a los índices
    fechas <- bds_hora$Fecha[indices_cambio]
    
    resultados <- data.frame(
      Indice = indices_cambio,
      Fecha = fechas,
      Valor_BDS = bds_hora$bdsM[indices_cambio]
    )
  } else {
    # Si no se detectaron cambios
    resultados <- data.frame(
      Indice = integer(0),
      Fecha = as.Date(character(0)),
      Valor_BDS = numeric(0)
    )
  }
  
  return(resultados)
}

# ============================================================================
# FUNCIÓN: DETECTAR CAMBIOS ESTRUCTURALES CON OLS-MOSUM Y OLS-CUSUM (PARA INTRADIARIO)
# ============================================================================

detectar_cambios_ols <- function(bdsout, hora, tipo = "OLS-MOSUM", h = 0.15, 
                                 span_porcentaje = 0.05) {
  # Filtrar datos para la hora específica
  bds_hora <- bdsout %>% filter(Hora == hora)
  
  # Verificar si hay suficientes datos
  if(nrow(bds_hora) < 10) {
    return(data.frame(Fecha = as.Date(character(0)), Valor_Test = numeric(0)))
  }
  
  # Definir modelo de regresión
  modelo <- bdsM ~ indice
  
  # Calcular test OLS según el tipo especificado
  test_ols <- tryCatch({
    efp(modelo, data = bds_hora, type = tipo, h = h)
  }, error = function(e) {
    cat("\nError al calcular", tipo, "para hora", hora, ":", e$message, "\n")
    return(NULL)
  })
  
  # Si hubo error en el cálculo
  if(is.null(test_ols)) {
    return(data.frame(Fecha = as.Date(character(0)), Valor_Test = numeric(0)))
  }
  
  # Extraer proceso
  proceso_ols <- abs(na.omit(as.numeric(test_ols$process)))
  
  # Si no hay datos válidos
  if(length(proceso_ols) == 0) {
    return(data.frame(Fecha = as.Date(character(0)), Valor_Test = numeric(0)))
  }
  
  # Detectar picos en el proceso
  span <- round(length(proceso_ols) * span_porcentaje)
  maximos <- tryCatch({
    pracma::findpeaks(proceso_ols, minpeakdistance = span, npeaks = 10)
  }, error = function(e) NULL)
  
  # Si no se encontraron picos
  if(is.null(maximos) || nrow(maximos) == 0) {
    return(data.frame(Fecha = as.Date(character(0)), Valor_Test = numeric(0)))
  }
  
  # Extraer índices de los picos
  indices_maximos <- maximos[,2]
  
  # Ajustar índices a la longitud de bds_hora si es necesario
  indices_maximos <- indices_maximos[indices_maximos <= nrow(bds_hora)]
  
  # Crear dataframe de resultados
  resultados <- data.frame(
    Fecha = bds_hora$Fecha[indices_maximos],
    Valor_Test = proceso_ols[indices_maximos]
  )
  
  return(resultados)
}

# ============================================================================
# CONFIGURACIÓN NECESARIA PARA BDS
# ============================================================================

# Correción de outliers  
corregir_outliers <- FALSE  # Cambiar a FALSE si no quieres corregir outliers o TRUE si sí 

# Definir parametros
m <- 6        # Dimensión para el test BDS (número de valores pasados)
N0 <- 50      # Tamaño inicial de la subserie
nstd <- 0.5   # Número de veces la desviación estándar para definir el radio
lapso <- 1    # Intervalo de crecimiento en cada iteración 

Maxm <- 5     # Número máximo de dimensiones a considerar en el test BDS

# ============================================================================
# PREPARACIÓN Y LIMPIEZA DE DATOS INTRADIARIOS
# ============================================================================

# Limpiar nombres de columnas
colnames(datosD3) <- trimws(colnames(datosD3))
colnames(datosD3) <- c("Fecha", paste0(1:24, "h"))

# Convertir fecha a tipo Date
datosD3$Fecha <- as.Date(datosD3$Fecha)

# Crear un dataframe con los datos de cada hora (excluyendo la columna de fecha)
data_horas <- datosD3[, -1]

# ============================================================================
# VERIFICAR ESTACIONARIEDAD Y DIFERENCIAR SI ES NECESARIO
# ============================================================================

# Verificar estacionariedad para cada hora y aplicar diferenciación si es necesario
cat("\n🔹 Verificando estacionariedad para cada hora...\n")

# Determinar número óptimo de diferencias para cada columna (hora)
ndiff <- apply(data_horas, 2, function(col) ndiffs(col, alpha = 0.05))

# Aplicar diferenciación a cada columna según su ndiff
data_horas_estacionaria <- as.data.frame(mapply(function(col, d) {
  if(d > 0) {
    diff(col, differences = d)
  } else {
    col
  }
}, data_horas, ndiff))

# Ajustar nombres de columnas
names(data_horas_estacionaria) <- names(data_horas)

cat("\n🔹 Estacionariedad verificada. Diferencias aplicadas:", paste(ndiff, collapse = ", "), "\n")

# ============================================================================
# DETECTAR Y CORREGIR OUTLIERS (SI SE HABILITÓ)
# ============================================================================

if (corregir_outliers) {
  cat("\n🔹 Detectando y corrigiendo outliers en la serie diferenciada...\n")
  
  data_horas_estacionaria <- as.data.frame(lapply(data_horas_estacionaria, function(col) {
    media_col <- mean(col, na.rm = TRUE)
    sd_col <- sd(col, na.rm = TRUE)
    z_scores <- (col - media_col) / sd_col
    umbral <- 3
    outliers <- abs(z_scores) > umbral
    if(sum(outliers, na.rm = TRUE) > 0) {
      col[outliers] <- media_col
      cat("  - Corregidos", sum(outliers, na.rm = TRUE), "outliers en una columna\n")
    }
    return(col)
  }))
  
  cat("\n✅ Proceso de corrección de outliers completado.\n")
}

# ============================================================================
# APLICACIÓN DEL TEST BDS RECURSIVO A DATOS INTRADIARIOS
# ============================================================================

cat("\n🔹 Aplicando test BDS recursivo a datos intradiarios...\n")

# Ejecutar el test BDS
bdsout <- bds_recur_variablesmultiples(data_horas_estacionaria, N0=N0, m=m, nstd=nstd, lapso=lapso, tracebar=TRUE)

# Agregar fechas a los resultados
fechas_validas <- tail(datosD3$Fecha, n = nrow(bdsout) / length(unique(bdsout$Hora)))
bdsout$Fecha <- rep(fechas_validas, times = length(unique(bdsout$Hora)))

# Calcular bdsM (promedio normalizado de estadísticos BDS)
bdsout$bdsM <- rowMeans(scale(bdsout[, c(1:(min(Maxm, m-1)))]), na.rm = TRUE)

cat("\n✅ Test BDS recursivo completado para todas las horas.\n")

# ============================================================================
# VISUALIZACIÓN DEL TEST BDS POR HORA
# ============================================================================

cat("\n🔹 Generando visualizaciones del test BDS por hora...\n")

# Definir marcas de fecha para el eje X (cada 3 meses)
breaks_fechas <- seq(from = min(bdsout$Fecha), to = max(bdsout$Fecha), by = "3 months")

# Gráfico general de evolución del test BDS por hora
g_bds <- ggplot(bdsout, aes(x = Fecha, y = bdsM, group = Hora, color = Hora)) +
  geom_line(alpha = 0.7) +
  labs(title = "Evolución del Test BDS por Hora",
       x = "Fecha", y = "Valor Normalizado BDS") +
  scale_x_date(breaks = breaks_fechas, labels = date_format("%d/%m/%Y")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(g_bds)

# Agrupar horas en rangos para mejor visualización
rangos_horas <- list(
  "Madrugada (1h-6h)" = c("1h", "2h", "3h", "4h", "5h", "6h"),
  "Mañana (7h-12h)" = c("7h", "8h", "9h", "10h", "11h", "12h"),
  "Tarde (13h-18h)" = c("13h", "14h", "15h", "16h", "17h", "18h"),
  "Noche (19h-24h)" = c("19h", "20h", "21h", "22h", "23h", "24h")
)

lista_graficos <- list()

for (rango in names(rangos_horas)) {
  p_rango <- bdsout %>%
    filter(Hora %in% rangos_horas[[rango]]) %>%
    ggplot(aes(x = Fecha, y = bdsM, color = Hora)) +
    geom_line() +
    labs(title = paste(" Test BDS -", rango), x = "Fecha", y = "Valor Normalizado BDS") +
    scale_x_date(breaks = breaks_fechas, labels = date_format("%d/%m/%Y")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  lista_graficos[[rango]] <- p_rango
}

# Mostrar gráficos por rango de horas
print(grid.arrange(
  lista_graficos[["Madrugada (1h-6h)"]],
  lista_graficos[["Mañana (7h-12h)"]],
  lista_graficos[["Tarde (13h-18h)"]],
  lista_graficos[["Noche (19h-24h)"]],
  ncol = 2, nrow = 2
))

# ============================================================================
# DETECCIÓN DE CAMBIOS ESTRUCTURALES CON TEST BDS PARA CADA HORA
# ============================================================================

cat("\n🔹 Detectando cambios estructurales con test BDS para cada hora...\n")

# Almacenar resultados de cambios estructurales
cambios_por_hora_bds_picos <- list()
cambios_por_hora_bds_gradiente <- list()

for (hora in unique(bdsout$Hora)) {
  cat("\n  Analizando hora:", hora, "\n")
  
  # Método por picos
  cambios_bds_picos <- detectar_cambios_bds_intradiario(
    bdsout, hora, metodo = "picos", span_porcentaje = 0.05, umbral = 0.5
  )
  
  # Método por gradiente
  cambios_bds_gradiente <- detectar_cambios_bds_intradiario(
    bdsout, hora, metodo = "gradiente", span_porcentaje = 0.05, umbral = 0.8
  )
  
  # Almacenar resultados
  if (nrow(cambios_bds_picos) > 0) {
    cambios_por_hora_bds_picos[[hora]] <- cambios_bds_picos
    cat("    - Método picos: detectados", nrow(cambios_bds_picos), "cambios\n")
  } else {
    cat("    - Método picos: no se detectaron cambios\n")
  }
  
  if (nrow(cambios_bds_gradiente) > 0) {
    cambios_por_hora_bds_gradiente[[hora]] <- cambios_bds_gradiente
    cat("    - Método gradiente: detectados", nrow(cambios_bds_gradiente), "cambios\n")
  } else {
    cat("    - Método gradiente: no se detectaron cambios\n")
  }
}

# ============================================================================
# VISUALIZACIÓN DE CAMBIOS ESTRUCTURALES DETECTADOS EN EL TEST BDS POR HORA
# ============================================================================

cat("\n🔹 Generando visualizaciones de cambios estructurales en el test BDS...\n")

lista_graficos_bds <- list()

for (hora in unique(bdsout$Hora)) {
  # Filtrar datos para la hora especificada
  bds_hora <- bdsout %>% filter(Hora == hora)
  
  if (nrow(bds_hora) < 10) next
  
  # Obtener fechas de cambios para esta hora (si existen)
  fechas_cambio_picos <- if (hora %in% names(cambios_por_hora_bds_picos)) {
    cambios_por_hora_bds_picos[[hora]]$Fecha
  } else {
    as.Date(character(0))
  }
  
  fechas_cambio_gradiente <- if (hora %in% names(cambios_por_hora_bds_gradiente)) {
    cambios_por_hora_bds_gradiente[[hora]]$Fecha
  } else {
    as.Date(character(0))
  }
  
  # Crear gráfico para esta hora
  p <- ggplot(bds_hora, aes(x = Fecha, y = bdsM)) +
    geom_line(color = "deepskyblue4", size = 0.8) +
    geom_vline(xintercept = fechas_cambio_picos, 
               color = "springgreen4", linetype = "dotted", size = 1) +
    geom_vline(xintercept = fechas_cambio_gradiente, 
               color = "purple", linetype = "dashed", size = 1) +
    labs(title = paste("Test BDS -", hora),
         x = "Fecha", y = "Valor Normalizado BDS") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 9),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8)
    )
  
  lista_graficos_bds[[hora]] <- p
}

# Mostrar gráficos con cambios estructurales
if (length(lista_graficos_bds) > 0) {
  # Ajustar disposición según el número de gráficos
  n_col <- min(6, length(lista_graficos_bds))
  n_row <- ceiling(length(lista_graficos_bds) / n_col)
  
  print(do.call(grid.arrange, c(lista_graficos_bds, ncol = n_col, nrow = n_row)))
} else {
  cat("No hay suficientes datos para generar gráficos de test BDS.\n")
}

# ============================================================================
# TESTS ADICIONALES: OLS-MOSUM Y OLS-CUSUM PARA CADA HORA
# ============================================================================

cat("\n🔹 Aplicando tests OLS-MOSUM y OLS-CUSUM por hora...\n")

# Almacenar resultados
cambios_por_hora_mosum <- list()
cambios_por_hora_cusum <- list()

for (hora in unique(bdsout$Hora)) {
  cat("\n  Analizando hora:", hora, "\n")
  
  # Test OLS-MOSUM
  cambios_mosum <- detectar_cambios_ols(
    bdsout, hora, tipo = "OLS-MOSUM", h = 0.15, span_porcentaje = 0.05
  )
  
  # Test OLS-CUSUM
  cambios_cusum <- detectar_cambios_ols(
    bdsout, hora, tipo = "OLS-CUSUM", h = 0.15, span_porcentaje = 0.05
  )
  
  # Almacenar resultados
  if (nrow(cambios_mosum) > 0) {
    cambios_por_hora_mosum[[hora]] <- cambios_mosum
    cat("    - OLS-MOSUM: detectados", nrow(cambios_mosum), "cambios\n")
  } else {
    cat("    - OLS-MOSUM: no se detectaron cambios\n")
  }
  
  if (nrow(cambios_cusum) > 0) {
    cambios_por_hora_cusum[[hora]] <- cambios_cusum
    cat("    - OLS-CUSUM: detectados", nrow(cambios_cusum), "cambios\n")
  } else {
    cat("    - OLS-CUSUM: no se detectaron cambios\n")
  }
}


