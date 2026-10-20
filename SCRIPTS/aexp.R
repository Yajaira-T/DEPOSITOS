# Depósitos a la vista de la banca privada del Ecuador
#
# Serie temporal mensual de los depósitos a la vista de la
# banca privada ecuatoriana, correspondientes a la cuenta 2101.
# Contiene 284 observaciones desde enero de 2003 hasta agosto de 2026.
#
# Fuente: Superintendencia de Bancos del Ecuador.

library(fpp3)
library(here)

# 1. Cargar los datos
depositos <- read.csv(here("DATA", "Data.csv"))

class(depositos)

# 2. Convertir a tsibble
depositos <- depositos |>
  mutate(mes = yearmonth(mes)) |>
  as_tsibble(index = mes)

class(depositos)

# 3. Grafico temporal
depositos |>
  autoplot(deposito) +
  labs(
    title = "Evolucion temporal de los depositos a la vista",
    x = "Tiempo",
    y = "Depositos"
  )

# 4. Grafico estacional
depositos |>
  gg_season(deposito, labels = "both") +
  labs(
    title = "Comportamiento estacional de los depositos a la vista",
    x = "Mes",
    y = "Depositos"
  )

# 5. Grafico de subseries
depositos |>
  gg_subseries(deposito) +
  labs(
    title = "Subseries mensuales de los depositos a la vista",
    x = "Año",
    y = "Depositos"
  )
