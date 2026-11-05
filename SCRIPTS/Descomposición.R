
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


