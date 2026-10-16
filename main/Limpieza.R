library(fpp3)
library(readxl)
library(here)

# 1. Leer la hoja BAL
datos <- read_excel(
  here("DATA", "SERIES SUBSISTEMA BANCA PRIVADA 2026_08.xlsx"),
  sheet = "BAL",
  skip = 5
)
names(datos)
# 2. Necesitamos depositos a la vista entonces debemos quedarnos unicamente con la cuenta 2101
depositos <- datos |>
  dplyr::filter(CODIGO == 2101)

# 3. Pasar los meses de columnas a filas
depositos <- depositos |>
  pivot_longer(
  cols = 3:(ncol(depositos) - 3),
  names_to = "fecha",
  values_to = "deposito"
)

# 4. Convertir la fecha y crear la variable mensual
depositos <- depositos |>
  mutate(
    fecha = as.Date(as.numeric(fecha), origin = "1899-12-31"), #Interpreta estos números como fechas de Excel
    mes = yearmonth(fecha)
  ) |>
  select(mes, deposito)

# 5. Convertir a tsibble
depositos <- depositos |>
  as_tsibble(index = mes)

depositos

class(depositos)
