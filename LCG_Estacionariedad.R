# ---- LIBRERÍAS NECESARIAS ----
library(tseries)    # Para el Test de Dickey-Fuller
library(dplyr)      # Manipulación de datos
library(tidyr)      # Transformación de datos
library(readxl)     # Lectura de archivos Excel
library(knitr)      # Generar tablas
library(kableExtra) # Estilizar tablas
library(ggplot2)    # Visualización gráfica

# ---- CARGAR EL ARCHIVO EXCEL ----
ruta_archivo <- "C:/Users/lucia/OneDrive/Escritorio/Asignaturas/Cuarto/TFG/LuciaCalderon_DatosTFG.xlsx"

# ---- CARGAR SERIES TEMPORALES DIARIAS ----
datosD1 <- read_excel(ruta_archivo, sheet = "D_Pmercdiario_max, min, avg")
datosD2 <- read_excel(ruta_archivo, sheet = "D_Pcomercial max,min,avg")
datosD3 <- read_excel(ruta_archivo, sheet = "D_Pfdemanda")
datosD4 <- read_excel(ruta_archivo, sheet = "D_Energia")
datosD5 <- read_excel(ruta_archivo, sheet = "D_Tecnologia")

# ---- AJUSTAR LAS COLUMNAS DE FECHA ----
colnames(datosD3)[1] <- "Fecha"
colnames(datosD4)[1] <- "Fecha"

datosD1$Fecha <- as.Date(datosD1$Fecha, format = "%d/%m/%Y")
datosD2$Fecha <- as.Date(datosD2$Fecha, format = "%d/%m/%Y")
datosD3$Fecha <- as.Date(datosD3$Fecha, format = "%d/%m/%Y")
datosD4$Fecha <- as.Date(datosD4$Fecha, format = "%d/%m/%Y")
datosD5$Fecha <- as.Date(datosD5$Fecha, format = "%d/%m/%Y")

# ---- CONVERTIR SERIES HORARIAS AL FORMATO LARGO ----
colnames(datosD3)[-1] <- paste0(1:24, "h")
colnames(datosD4)[-1] <- paste0(1:24, "h")

datosD3_long <- datosD3 %>%
  pivot_longer(cols = -Fecha, names_to = "Hora", values_to = "Precio_Demanda") %>%
  mutate(Hora = as.numeric(gsub("h", "", Hora)))

datosD4_long <- datosD4 %>%
  pivot_longer(cols = -Fecha, names_to = "Hora", values_to = "Energia") %>%
  mutate(Hora = as.numeric(gsub("h", "", Hora)))

# ---- UNIR LAS SERIES POR FECHA Y HORA ----
datosD3_D4 <- inner_join(datosD3_long, datosD4_long, by = c("Fecha", "Hora"))

# ---- SELECCIONAR VARIABLES PARA EL TEST DE DICKEY-FULLER ----
series_a_testear <- list(
  "Precio mínimo (D1)" = datosD1$`Precio mínimo`,
  "Precio medio aritmético (D1)" = datosD1$`Precio medio aritmético`,
  "Precio máximo (D1)" = datosD1$`Precio máximo`,
  "Precio mínimo (D2)" = datosD2$`Precio mínimo`,
  "Precio medio (D2)" = datosD2$`Precio medio`,
  "Precio máximo (D2)" = datosD2$`Precio máximo`,
  "Energía Comercializadores (D2)" = datosD2$`Energía (MWh)`,
  "Precio Demanda (D3)" = datosD3_D4$Precio_Demanda,
  "Energía Demanda (D4)" = datosD3_D4$Energia,
  "Carbón (D5)" = datosD5$CARBÓN,
  "Nuclear (D5)" = datosD5$NUCLEAR,
  "Hidráulica (D5)" = datosD5$HIDRÁULICA,
  "Ciclo Combinado (D5)" = datosD5$`CICLO COMBINADO`,
  "Eólica (D5)" = datosD5$EÓLICA,
  "Solar Térmica (D5)" = datosD5$`SOLAR TÉRMICA`,
  "Solar Fotovoltaica (D5)" = datosD5$`SOLAR FOTOVOLTAICA`,
  "Cogeneración (D5)" = datosD5$`COGENERACIÓN/RESIDUOS/MINI HIDRA`,
  "Importación Inter. (D5)" = datosD5$`IMPORTACIÓN INTER.`,
  "Importación Inter. sin MIBEL (D5)" = datosD5$`IMPORTACIÓN INTER. SIN MIBEL`
)

# ---- APLICAR EL TEST DE DICKEY-FULLER Y TRANSFORMAR SERIES NO ESTACIONARIAS ----
resultados_adf <- lapply(names(series_a_testear), function(nombre) {
  serie <- na.omit(series_a_testear[[nombre]])
  
  if (length(serie) > 10) {
    test_adf <- adf.test(serie)
    p_valor <- test_adf$p.value
    estacionaria <- ifelse(p_valor < 0.05, "Sí", "No")
    
    if (estacionaria == "No") {
      serie_transformada <- diff(serie)
      test_adf_transformada <- adf.test(serie_transformada)
      p_valor_transformada <- test_adf_transformada$p.value
      estacionaria_transformada <- ifelse(p_valor_transformada < 0.05, "Sí", "No")
      return(data.frame(
        Variable = nombre,
        P_Value = round(p_valor, 4),
        Stationary = estacionaria,
        P_Value_Transformada = round(p_valor_transformada, 4),
        Stationary_Transformada = estacionaria_transformada
      ))
    } else {
      return(data.frame(
        Variable = nombre,
        P_Value = round(p_valor, 4),
        Stationary = estacionaria,
        P_Value_Transformada = NA,
        Stationary_Transformada = NA
      ))
    }
  }
})

# ---- CREAR TABLA DE RESULTADOS ----
resultados_adf_df <- do.call(rbind, resultados_adf)

# ---- MOSTRAR RESULTADOS EN TABLA FORMATEADA ----
tabla_resultados_adf <- kable(resultados_adf_df, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(resultados_adf_df), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_resultados_adf

# ---- GRAFICO DE RESULTADOS ----
resultados_adf_df %>%
  mutate(P_Value = as.numeric(P_Value)) %>%
  filter(!is.na(P_Value)) %>%
  arrange(P_Value) %>%
  ggplot(aes(x = reorder(Variable, P_Value), y = P_Value)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Test de Dickey-Fuller",
       subtitle = "Línea roja: Nivel de significancia (0.05)",
       x = "Variable", y = "P-Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12)) +
  geom_text(aes(label = round(P_Value, 3)), vjust = -0.5, size = 3.5, color = "black")












