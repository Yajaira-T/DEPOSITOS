
#Aplicamos la media movil a la serie original, en meses
depositos_ma <- depositos |>
  mutate(
    `12-MA` = slider::slide_dbl(
      deposito, mean,
      .before = 6, .after = 5, .complete = TRUE
    ),
    `2x12-MA` = slider::slide_dbl(
      `12-MA`, mean,
      .before = 0, .after = 1, .complete = TRUE
    )
  )

#Serie con la tendencia marcada en una li2nea color naranja
depositos_ma |>
  autoplot(deposito, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(
    title = "Tendencia de los depositos",
    x = "Tiempo",
    y = "Depositos"
  )

#Serie sin tendencia = estacionalidad + residuo 
pt_depositos <- depositos_ma |>
  mutate(pt = deposito - `2x12-MA`)

pt_depositos |> autoplot(pt)

#Estacionalidad como promedio de la serie sin tendencia por mes del año
prom_pt <- pt_depositos |>
  group_by(month(mes)) |>
  mutate(media = mean(pt, na.rm = TRUE)) |>
  ungroup()

#Residuo
rt_depositos <- prom_pt |>
  mutate(residuo = pt - media)

rt_depositos |> autoplot(residuo)



# AUDITORIA DE LA DESCOMPOSICION ADITIVA
# TOAQUIZA YAJAIRA

# 1. Descomposicion con classical_decomposition
descomposicion <- depositos |>
  model(
    classical_decomposition(
      deposito, #R utiliza automaticamente m=12 porque el indice es mensual.
      type = "additive"
    )
  ) |>
  components()

descomposicion

# Grafica de la descomposicion aditiva clasica

descomposicion |>
  autoplot() +
  labs(
    x = "Mes",
    title = "Descomposicion aditiva clasica de los depositos a la vista"
  )

# 2. Unir los resultados manuales con los de la funcion
comparacion <- rt_depositos |>
  left_join(
    descomposicion |>
      as_tibble() |>
      select(mes, trend, seasonal, random),
    by = "mes"
  )


# 3. Comparar la tendencia
comparacion <- comparacion |>
  mutate(
    diferencia_tendencia = `2x12-MA` - trend
  )

comparacion |>
  select(mes, `2x12-MA`, trend, diferencia_tendencia)

# Calcular la diferencia maxima
max(abs(comparacion$diferencia_tendencia), na.rm = TRUE)

# La diferencia es aproximadamente cero.
# La tendencia manual coincide con classical_decomposition.


# 4. Comparar la componente estacional
comparacion <- comparacion |>
  mutate(
    diferencia_estacional = media - seasonal
  )

comparacion |>
  select(mes, media, seasonal, diferencia_estacional)

# Calcular la diferencia maxima
max(abs(comparacion$diferencia_estacional), na.rm = TRUE)

# La diferencia es 6906.648, por lo que los resultados no coinciden.


# 5. Comparar la componente restante
comparacion <- comparacion |>
  mutate(
    diferencia_restante = residuo - random
  )

comparacion |>
  select(mes, residuo, random, diferencia_restante)

# Calcular la diferencia maxima
max(abs(comparacion$diferencia_restante), na.rm = TRUE)

# La diferencia es 6906.648, por lo que los resultados no coinciden.


# 6. Comprobar la causa de las diferencias

# Obtener los 12 promedios estacionales
medias <- comparacion |>
  as_tibble() |>
  mutate(estacion = month(mes)) |>
  distinct(estacion, media)

medias

# Calcular el promedio de los 12 valores
ajuste <- mean(medias$media)

ajuste

# El promedio es -6906.648.
# Falta ajustar las medias estacionales para que sumen cero.


# 7. Comprobar los resultados aplicando el ajuste

comparacion <- comparacion |>
  mutate(
    estacional_ajustada = media - ajuste,
    residuo_ajustado = pt - estacional_ajustada
  )

# Comparar nuevamente con classical_decomposition
comparacion <- comparacion |>
  mutate(
    diferencia_estacional_ajustada = estacional_ajustada - seasonal,
    diferencia_restante_ajustada = residuo_ajustado - random
  )

# Diferencia maxima de la componente estacional ajustada
max(abs(comparacion$diferencia_estacional_ajustada), na.rm = TRUE)

# Diferencia maxima de la componente restante ajustada
max(abs(comparacion$diferencia_restante_ajustada), na.rm = TRUE)


# CONCLUSION
# La tendencia manual coincide con classical_decomposition.
# Las diferencias en las componentes estacional y
# restante se deben a que las medias estacionales no fueron
# ajustadas para que sumen cero, como indica el paso 3
# del capitulo 3.4 de FPP3 
# Al aplicar el ajuste, ambas diferencias son aproximadamente
# cero
