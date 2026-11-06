# DESCOMPOSICION MULTIPLICATIVA CLASICA

library(fpp3)
library(slider)
library(here)

# 1. Leer los datos
depositos <- read.csv(here("DATA", "Data.csv"))

depositos <- depositos |>
  mutate(mes = yearmonth(mes)) |>
  as_tsibble(index = mes)

depositos

# Comprobar si existen meses faltantes
has_gaps(depositos)

# PASO 1: Estimar la tendencia-ciclo

# Como la serie es mensual, se utiliza m = 12.
# Al ser un periodo par se calcula una 2x12-MA.

depositos_mult <- depositos |>
  mutate(
    `12-MA` = slider::slide_dbl(
      deposito, mean,
      .before = 6, .after = 5,
      .complete = TRUE
    ),
    `2x12-MA` = slider::slide_dbl(
      `12-MA`, mean,
      .before = 0, .after = 1,
      .complete = TRUE
    )
  )

depositos_mult

# Grafica de la serie original y la tendencia
depositos_mult |>
  autoplot(deposito, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(
    title = "Tendencia de los depositos a la vista",
    x = "Tiempo (meses)",
    y = "Depositos"
  )


# PASO 2: Eliminar la tendencia

# En la descomposicion multiplicativa se divide
# la serie original entre la tendencia.

depositos_mult <- depositos_mult |>
  mutate(
    sin_tendencia = deposito / `2x12-MA`
  )

depositos_mult

# Grafica de la serie sin tendencia
depositos_mult |>
  autoplot(sin_tendencia) +
  labs(
    title = "Serie sin tendencia - Depositos a la vista",
    x = "Tiempo (meses)",
    y = "Depositos / Tendencia"
  )


# PASO 3: Calcular la componente estacional

# Identificar el mes de cada observacion
depositos_mult <- depositos_mult |>
  mutate(
    estacion = month(mes)
  )

# Calcular el promedio para cada mes
depositos_mult <- depositos_mult |>
  group_by(estacion) |>
  mutate(
    media_estacional = mean(sin_tendencia, na.rm = TRUE)
  ) |>
  ungroup()

depositos_mult

# Obtener los 12 promedios estacionales
medias <- depositos_mult |>
  distinct(estacion, media_estacional)

medias

# Calcular el promedio de los 12 valores
ajuste <- mean(medias$media_estacional)

ajuste

# Ajustar los indices estacionales para que sumen 12
depositos_mult <- depositos_mult |>
  mutate(
    estacional = media_estacional / ajuste
  )

# Revisar los 12 indices estacionales
depositos_mult |>
  distinct(estacion, estacional)

# Comprobar que los indices estacionales sumen 12
depositos_mult |>
  distinct(estacion, estacional) |>
  summarise(suma = sum(estacional))

# Grafica de la componente estacional
depositos_mult |>
  autoplot(estacional) +
  labs(
    title = "Componente estacional - Depositos a la vista",
    x = "Tiempo (meses)"
  )


# PASO 4: Calcular la componente restante

# Se divide la serie original entre el producto
# de la tendencia y la componente estacional.

depositos_mult <- depositos_mult |>
  mutate(
    restante = deposito / (`2x12-MA` * estacional)
  )

depositos_mult

# Grafica de la componente restante
depositos_mult |>
  autoplot(restante) +
  labs(
    title = "Componente restante - Depositos a la vista",
    x = "Tiempo (meses)",
    y = "Componente restante"
  )


# AUDITORIA CON CLASSICAL_DECOMPOSITION

# 1. Descomposicion multiplicativa con classical_decomposition
# No se especifica el periodo porque la serie es mensual
# y R reconoce automaticamente el periodo de 12 meses.

descomposicion <- depositos |>
  model(
    classical_decomposition(
      deposito,
      type = "multiplicative"
    )
  ) |>
  components()

descomposicion

# Grafica de la descomposicion multiplicativa
descomposicion |>
  autoplot() +
  labs(
    x = "Tiempo (Meses)",
    title = "Descomposicion multiplicativa clasica de los depositos a la vista"
  )


# 2. Unir los resultados manuales con los de la funcion

comparacion <- depositos_mult |>
  left_join(
    descomposicion |>
      as_tibble() |>
      select(mes, trend, seasonal, random),
    by = "mes"
  )

# 3. Comparar las tres componentes

comparacion <- comparacion |>
  mutate(
    diferencia_tendencia = `2x12-MA` - trend,
    diferencia_estacional = estacional - seasonal,
    diferencia_restante = restante - random
  )

comparacion

# 4. Calcular las diferencias maximas

max(abs(comparacion$diferencia_tendencia), na.rm = TRUE)
max(abs(comparacion$diferencia_estacional), na.rm = TRUE)
max(abs(comparacion$diferencia_restante), na.rm = TRUE)

# CONCLUSION
# Las componentes de tendencia, estacionalidad y restante
# calculadas manualmente coinciden con las obtenidas mediante
# classical_decomposition, siguiendo el procedimiento del
# capitulo 3.4 de FPP3.
# Las diferencias son aproximadamente cero
