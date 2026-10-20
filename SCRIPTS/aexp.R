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
depositos$mes <- yearmonth(depositos$mes)

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
#*****************************************************************
#REVISADO POR: EVELIN CUYO
#GRAFICO TEMPORAL
# 1. ESTACIONARIEDAD: NO
# La serie no es estacionaria. Se observa claramente que 
# la media cambia de forma sistemática a lo largo del tiempo, 
# incrementándose de manera continua y alejándose de un comportamiento estacionario.

# 2. TENDENCIA: SI
# Se identifica una clara tendencia ascendente y sostenida 
# a lo largo de todo el periodo (2003-2026). Los depósitos 
# muestran un crecimiento de largo plazo fuerte y continuo.

# 3. ESTACIONALIDAD: NPO
# A simple vista en el gráfico general no destaca un patrón 
# estacional intraanual dominante, ya que la fuerte tendencia 
# de crecimiento absorbe las variaciones periódicas menores.

# 4. CICLOS: SI
# Se aprecian ligeras fluctuaciones de mediano plazo o cambios 
# de pendiente alrededor de ciertos años (como leves 
# desaceleraciones), lo que sugiere posibles movimientos cíclicos 
# vinculados al ciclo macroeconómico.

#GRAFICO ESTACIONAL*********************************************
# 1. ESTACIONARIEDAD: No
# La serie no es estacionaria porque la media cambia 
# sistemáticamente a lo largo del tiempo; los niveles de los depósitos 
# aumentan año con año, impidiendo que la media y la varianza permanezcan constantes.

# 2. TENDENCIA: Sí
# Se evidencia claramente en la separación y el orden vertical 
# de las líneas (los años más antiguos abajo y los más recientes arriba), 
# reflejando un crecimiento sostenido y de largo plazo en los depósitos.

# 3. ESTACIONALIDAD: Sí
# Sí existe un patrón intraanual repetitivo. Aunque la mayor 
# parte del año las líneas son planas, se observa una aceleración 
# constante y consistente hacia el cierre de cada año (meses de noviembre y diciembre).

# 4. CICLOS: No
# Este tipo de gráfico estacional está diseñado para mostrar 
# el comportamiento mes a mes de cada año frente a los demás, por lo que 
# los ciclos económicos o fluctuaciones de mediano plazo no se aíslan ni 
# se visualizan de manera directa en esta perspectiva.

#GRAFICO DE SUBSERIES********************************************
# 1. ESTACIONARIEDAD: No
# La serie no es estacionaria porque en cada panel mensual 
# la media cambia drásticamente a lo largo de los años.

# 2. TENDENCIA: Sí
# Se evidencia de manera muy marcada en cada subserie mensual, 
# donde se observa una trayectoria ascendente continua y pronunciada 
# desde los primeros años hasta el final del periodo analizado.

# 3. ESTACIONALIDAD: Sí
# Sí se detecta estacionalidad; al observar las líneas horizontales 
# azules (que representan la media de cada mes), se aprecian ligeras variaciones 
# en los niveles promedio al comparar unos meses con otros a lo largo del año.

# 4. CICLOS: No
# Este gráfico está estructurado específicamente para mostrar 
# la evolución de cada mes de forma aislada a lo largo del tiempo, por lo que 
# las fluctuaciones o ciclos económicos de mediano plazo no se visualizan directamente.
