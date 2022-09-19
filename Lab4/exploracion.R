# Librerias
library(readr)
library(tidyverse)
library(highcharter) #Para graficar
library(readr)
library(dplyr)
library(plotly)


# Importacion de la base de datos 
df <- read_delim("tabla_completa2.csv", 
                 ",", escape_double = FALSE, trim_ws = TRUE)
View(df)
names(df) <- tolower(names(df))
df$despacho <- tolower(df$despacho)
df$faltante <- tolower(df$faltante)
df$faltante <- gsub(" ","",df$faltante)
names(df)[5] <- 'estado'



# SITUACION ACTUAL

df %>%  # total de ventas
  summarise(ventas = sum(q))


df %>% # compras por cliente
  select(cliente, q) %>% 
  group_by(cliente) %>% 
  summarise(ventas = sum(q)) %>% 
  arrange(desc(ventas)) %>% 
  hchart("pie", hcaes(x = cliente, y = ventas)) %>%
  hc_title(text = "<b> Compras por cliente </b>")

df %>% # ventas por mes
  select(mes, q) %>%
  group_by(mes) %>% 
  summarise(ventas = sum(q)) %>% 
  hchart("column", hcaes(x = mes, y = ventas)) %>%
  hc_title(text = "<b> Ventas por mes </b>") %>%
  hc_subtitle(text = "<i>Las ventas por mes se mantienen constantes </i>") 


df %>% #  viajes por piloto 
  select(piloto) %>%
  group_by(piloto) %>%
  summarise(viajes = n()) %>% 
  arrange(desc(viajes)) %>% 
  hchart("column", hcaes(x = piloto, y = viajes)) %>%
  hc_title(text = "<b> Viajes por piloto </b>") %>%
  hc_subtitle(text = "<i>Pilotos con más viajes y, por lo tanto, más efectivos</i>") 

df %>% #  ingreso por piloto 
  select(piloto, q) %>%
  group_by(piloto) %>%
  summarise(ingreso = sum(q)) %>% 
  arrange(desc(ingreso)) %>% 
  hchart("column", hcaes(x = piloto, y = ingreso)) %>%
  hc_title(text = "<b> Ingreso por piloto </b>") %>%
  hc_subtitle(text = "<i>Los son muy similares</i>") 



# PROBLEMA 

df %>%
  select(estado) %>% 
  group_by(estado) %>% 
  summarise(cantidad = n()) %>% 
  hchart("pie", hcaes(x = estado, y = cantidad)) %>%
  hc_title(text = "<b> Distribución de envios </b>")


df %>% # cantidad minima y minima registrada en viajes incompletos
  select(unidad, estado, cantidad)  %>% 
  filter(estado == 'faltante') %>%
  group_by(unidad) %>%
  summarise(minimo = min(cantidad), maximo = max(cantidad))

df %>% # cantidad minima viajes completos
  select(unidad, estado, cantidad)  %>% 
  filter(estado != 'faltante') %>%
  group_by(unidad) %>%
  summarise(minimo = min(cantidad), maximo = max(cantidad))

parcial <- df %>% 
  filter(unidad == 'Camion Grande') %>%
  filter(estado == 'faltante') %>% 
  filter(cantidad > 1885) %>% 
  summarise(n = n())

total <-  df %>% 
  filter(estado == 'faltante') %>% 
  summarise(n = n())


parcial$n / total$n

# PREOCUPACIONES
df %>% 
  filter(estado == 'faltante') %>%
  select(piloto) %>%
  group_by(piloto) %>%
  summarise(viajes = n()) %>%
  hchart("bar", hcaes(x = piloto, y = viajes)) %>%
  hc_title(text = "<b> Envios incompletos por piloto </b>")

df %>% # Ventas por mes
  select(mes, q) %>% 
  group_by(mes) %>% 
  summarise(ventas = sum(q)) %>%
  hchart("column", hcaes(x = mes, y = ventas)) %>%
  hc_title(text = "<b> Ventas al mes </b>")







# Análsis exploratorio previo 
# cantidad de viajes faltantes 
df %>%
  filter(estado == 'faltante') %>% 
  summarise(faltantes = n())

# cantidad de viajes devueltos 
df %>%
  filter(estado == 'devolucion') %>% 
  summarise(devoluciones = n())


# cantidad de viajes completos, faltantes y devueltos
test <- df %>% 
  select(estado, cod_viaje) %>% 
  group_by(estado) %>% 
  summarise(viajes = n())

test
sum(test$viajes)

# cantidad de viajes por cliente
df %>% 
  select(cliente, cod_viaje) %>% 
  group_by(cliente) %>% 
  summarise(viajes = n()) %>% 
  arrange(desc(viajes))



# Cantidad de viajes por empleado 
df %>% 
  select(piloto, estado) %>% 
  filter(estado == 'faltante') %>%
  group_by(piloto) %>%
  summarise(cantida = n())

 
# Cantidad de creditos    PROBLEMAS DE LIQUIDEZ 
df %>% 
  select(credito) %>% 
  group_by(credito) %>%
  summarise(n=n())


# cantidad de viajes por centro de distribuCion 
df %>%
  select(ubicacion) %>% 
  group_by(ubicacion) %>%
  summarise(n=n())
#     Los centros de distribución están bien parejos


# Tiendas por centro de distribución
tiendas <- df %>% 
  group_by(cliente, ubicacion) %>%
  summarise(n = n())

view(tiendas)




### Seccion solo para credito





### sección solo para ver faltantes
# faltantes por tipo de carro
df %>% 
  select(unidad, estado) %>%
  group_by(unidad, estado) %>% 
  summarise(viajes = n())

df %>% 
  select(unidad) %>%
  group_by(unidad) %>% 
  summarise(viajes = n())




### Seccion para camiones 





### Seccion solo para ver ingresos por mes 
df %>% 
  select(mes) %>% 
  group_by(mes) %>% 
  summarise(viajes = n()) %>% 
  hchart("column",hcaes(x = mes, y = viajes))
  
  # las ventas de cada mes

  # ¿cuantos faltantes hay por mes? 

df %>% 
  select(mes, estado) %>%
  filter(estado == 'faltante') %>% 
  group_by(mes) %>% 
  summarise(viajes = n())
  
test <- df %>%
  summarise(cantidad = mean(cantidad), 
            q = mean(q))

test$q / test$cantidad
# el precio por la unidad es 4, en todos los 


test <- df %>%
  summarise(cantidad = mean(q / cantidad))
test  


#### TODA LA siguiente sección busca responder las preguntas del personal y los camiones



# la cantidad minima en los viajes incompletos por camion
df %>% 
  select(unidad, estado, cantidad)  %>% 
  filter(estado == 'faltante') %>%
  group_by(unidad) %>%
  summarise(minimo = min(cantidad))


df %>% # prueba de cantidad minima de camiones grandes 
  select(unidad, estado) %>%
  filter(unidad == 'Camion Grande') %>% 
  filter(estado == 'faltante') %>%
  summarise(n =n())

df %>% 
  select(unidad, cantidad) %>%
  filter(unidad == 'Camion Grande') %>% 
  filter(cantidad >= 1003) %>%
  summarise(n=n())

df %>% # prueba de cantidad minima de camiones pequeños 
  select(unidad, estado) %>%
  filter(unidad == 'Camion Pequeño') %>% 
  filter(estado == 'faltante') %>%
  summarise(n =n())

df %>% 
  select(unidad, cantidad) %>%
  filter(unidad == 'Camion Pequeño') %>% 
  filter(cantidad >= 502) %>%
  summarise(n=n())


df %>% # prueba de cantidad minima de panel 
  select(unidad, estado) %>%
  filter(unidad == 'Panel') %>% 
  filter(estado == 'faltante') %>%
  summarise(n =n())

df %>% 
  select(unidad, cantidad) %>%
  filter(unidad == 'Panel') %>% 
  filter(cantidad >= 206) %>%
  summarise(n=n())

  



### Cantidad máxima de viajes incompletos  
df %>% # cantidad minima viajes completos
  select(unidad, estado, cantidad)  %>% 
  filter(estado == 'faltante') %>%
  group_by(unidad) %>%
  summarise(minimo = min(cantidad), maximo = max(cantidad))

df %>% # cantidad minima viajes completos
  select(unidad, estado, cantidad)  %>% 
  filter(estado != 'faltante') %>%
  group_by(unidad) %>%
  summarise(minimo = min(cantidad), maximo = max(cantidad))


df %>% # cantidad maxima viajes completos
  select(unidad, estado, cantidad)  %>% 
  filter(estado == 'faltante') %>%
  group_by(unidad) %>%
  summarise(maximo = max(cantidad))

df %>% # cantidad maxima viajes completos
  select(unidad, estado, cantidad)  %>% 
  filter(estado != 'faltante') %>%
  group_by(unidad) %>%
  summarise(maximo = max(cantidad))



# ¿Cuanto necesitamos cada mes para sobrevivir?
df %>% 
  filter(credito == 30 || credito == 60) %>%
  select(mes, q) %>%
  group_by(mes) %>% 
  summarise(q_total = sum(q)) %>% 
  hchart("column",hcaes(x = mes, y = q_total))


df %>% 
  filter(credito == 30) %>%
  select(mes, q) %>%
  group_by(mes) %>% 
  summarise(q_total = sum(q)) %>% 
  hchart("column",hcaes(x = mes, y = q_total))
## Cantidad de dinero por mes de ventas al crédito

df %>% 
  filter(credito == 60) %>%
  select(mes, q) %>%
  group_by(mes) %>% 
  summarise(q_total = sum(q)) %>% 
  hchart("column",hcaes(x = mes, y = q_total))
## Cantidad de dinero por mes de ventas al mayoreo



df %>% 
  select(despacho, q) %>%
  group_by(despacho) %>% 
  summarise(promedio = mean(q)) %>% 
  hchart("column", hcaes(x = despacho, y = promedio))

  # no hay diferencia significativa entre las ventas promedios de ambos tipos





# Sección para el 80 - 20 
### Sección solo para clientes 
# cantidad y quetzales por cliente
df %>% 
  select(cliente, cantidad) %>% 
  group_by(cliente) %>% 
  summarise(ccantidad = sum(cantidad)) %>% 
  arrange(desc(ccantidad)) %>% 
  hchart("column", hcaes(x = cliente, y = ccantidad)) %>%
  hc_title(text = "<b> Cantidad de compras po cliente </b>")

df %>% 
  select(cliente, cantidad, q) %>% 
  group_by(cliente) %>% 
  summarise(ccantidad = sum(cantidad), q = sum(q)) %>% 
  arrange(desc(ccantidad)) %>% 
  hchart("column", hcaes(x = cliente, y = q)) %>%
  hc_title(text = "<b> Cantidad de ingresos por cliente </b>")

df %>% 
  filter()
  select(cliente, cantidad) %>% 
  group_by(cliente) %>% 
  summarise(ccantidad = sum(cantidad), q = sum(q)) %>% 
  arrange(desc(ccantidad)) %>% 
  hchart("column", hcaes(x = cliente, y = q)) %>%
  hc_title(text = "<b> Cantidad de ingresos por cliente </b>")



# Pilotos más efectivo
df %>% 
  select(piloto) %>%
  group_by(piloto) %>% 
  summarise(viajes = n()) %>%
  arrange(desc(viajes)) %>% 
  hchart("column", hcaes(x = piloto, y = viajes)) %>%
  hc_title(text = "<b> Pilotos con más viajes </b>")
  
  

df %>% 
  filter(estado == 'faltante') %>%
  select(piloto) %>%
  group_by(piloto) %>% 
  summarise(viajes = n()) %>%
  arrange(desc(viajes)) %>% 
  hchart("column", hcaes(x = piloto, y = viajes)) %>%
  hc_title(text = "<b> Pilotos con más faltantes </b>")

 
df %>%
  select(piloto, unidad) %>%
  group_by(piloto, unidad) %>% 
  summarise(viajes = n()) %>%
  arrange(desc(viajes))



# Para resolver el tema de inventarios
viajest <- df %>%
  summarise(totales = n()) 

viajesf <- df %>%
  filter(estado == 'faltante') %>% 
  summarise(faltantes = n())
viajesf

viajesf$faltantes / viajest$totales


# pedidos falantes por cantidad 

