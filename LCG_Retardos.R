# ---- LIBRERÍAS NECESARIAS ----
library(tseries)    # Para el Test de Dickey-Fuller
library(dplyr)      # Manipulación de datos
library(tidyr)      # Transformación de datos
library(readxl)     # Lectura de archivos Excel
library(knitr)      # Generar tablas
library(kableExtra) # Estilizar tablas
library(ggplot2)    # Visualización gráfica
library(vars)       # Para calcular retardos óptimos

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

# ---- TRANSFORMAR SERIES NO ESTACIONARIAS Y GUARDAR RETARDOS ----
estacionarias <- list()
retardos_optimos <- data.frame(Variable = character(), Retardo_Optimo_Dias = integer())

for (nombre in names(series_a_testear)) {
  serie <- na.omit(series_a_testear[[nombre]])
  if (length(serie) > 10) {
    test_adf <- adf.test(serie)
    if (test_adf$p.value >= 0.05) {
      serie_estacionaria <- diff(serie)
      estacionarias[[nombre]] <- serie_estacionaria
    } else {
      estacionarias[[nombre]] <- serie
    }
    # Determinar retardo óptimo
    lag_selection <- VARselect(serie, lag.max = 10, type = "const")
    retardo_optimo <- lag_selection$selection["AIC(n)"]
    retardos_optimos <- rbind(retardos_optimos, data.frame(Variable = nombre, Retardo_Optimo_Dias = retardo_optimo))
  }
}

# ---- GUARDAR SERIES Y RETARDOS EN UN ARCHIVO .RData ----
save(estacionarias, retardos_optimos, file = "Series_Estacionarias_y_Retardos.RData")

# ---- CREAR TABLAS Y GRÁFICOS ----
tabla_retardos <- kable(retardos_optimos, format = "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1:ncol(retardos_optimos), color = "black") %>%
  row_spec(0, background = "deepskyblue", color = "white")

tabla_retardos

# Mostrar gráfico de retardos
retardos_optimos %>%
  ggplot(aes(x = reorder(Variable, Retardo_Optimo), y = Retardo_Optimo)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Retardos Óptimos por Variable", x = "Variable", y = "Retardo Óptimo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
