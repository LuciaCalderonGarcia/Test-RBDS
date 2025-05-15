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
# FUNCIÓN: BDS RECURSIVO (VERSIÓN OPTIMIZADA CON PARALELIZACIÓN PARA MÚLTIPLES SERIES)
# ============================================================================

bds_recur_intradiario <- function(data, N0=50, m=6, nstd=0.5, lapso=1, tracebar=TRUE) {
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
