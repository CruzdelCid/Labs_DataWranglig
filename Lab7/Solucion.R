library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(highcharter)
library(plotly)
library(resumeRdesc) 


# Margen operativo 2017 
data <-data 
View(data)

margen_2017 <- (data %>%
                  filter(Fecha < dmy("01-10-2017")) %>%
                  summarise(margen_op = sum(ProfitO)))$margen_op

margen_2017

# Margen operativo 2018

margen_2018 <- margen_2017 - margen_2017*0.25
margen_2018

# Comparacion
x <- c(2017, 2018) 
y <- c(margen_2017, margen_2018)

fig <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Margen Operativo', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Margen (Q)'), 
         yaxis = list(title = 'Año'), legend = list(text='<b>Disminuyo 25% en 2018</b>'))
fig

margen_2017-margen_2018



# Informacion de los postes 
data %>% 
  summarise(n=n_distinct(ID))



res <-data %>%
  filter(Fecha < dmy("01-10-2017")) %>%
  group_by(ID) %>% 
  summarise(n = n(),
            Lat = unique(Lat), 
            Long = unique(Long)) %>% 
  arrange(desc(n))

sum(res$n) * 0.8

res %>% 
  filter(n > 12) %>% 
  nrow()


quantile(res$n, probs = c(0.10, 0.30, 0.50, 0.70, 0.80, 0.90, 0.91, 0.92, 0.98), type = 7)

res %>%
  filter(n=>20) %>% 
  summarise(revisiones => )

# Cantidad de ventas al mes
data %>% 
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>%
  


  ay <- list(
    tickfont = list(color = "orange"),
    overlaying = "y",
    side = "right",
    title = "Total"
  )
plot_ly(
  data= data %>%
    select(Cod,ProfitO) %>%
    group_by(Cod) %>%
    summarise(cantidad = n()) %>% 
    arrange(desc(cantidad)),
  x=~Cod,
  y=~cantidad,
  type="bar",
  name="Viajes"
) %>%
  add_trace(
    data=data %>%
      select(Cod,ProfitO) %>%
      group_by(Cod) %>%
      summarise(ProfitO = round(mean(ProfitO),2)),
    x=~Cod,
    y=~ProfitO,
    type="scatter",
    mode="lines+markers",
    name="margen_venta",
    yaxis="y2"
  )%>%
  layout(yaxis = list(title = 'Count'), 
         yaxis2 = ay)


# Porcentaje de los servicios 

total <- data %>% 
  filter(Fecha < dmy("01-10-2017")) %>%
  nrow()

res <- data %>% 
  filter(Fecha < dmy("01-10-2017")) %>%
  group_by(Cod) %>% 
  summarise(Cantidad = n(), 
            Porcentaje = round(Cantidad/total*100,2)) %>% 
  arrange(desc(Porcentaje))

fig <- plot_ly(res, x = ~Cod, y = ~Porcentaje, type = 'bar', name = 'Porcentaje por servicio') %>% 
  layout(title = 'Porcentaje por servicio', 
         xaxis = list(title = 'Servicio'),  
         yaxis = list(title = 'Porcentaje de venta'), 
         barmode = 'group')
fig 


##### Compra de nuevos vehículos ##### 
# Cuanto aumentaron los costos
total = nrow(data)

ganancia <- data %>% 
  summarise(promedio = avg())




# Cantidad de vehiculos 

data %>% 
  group_by(Fecha, Vehiculo)%>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  group_by(Vehiculo) %>%
  summarise(Promedio_Diario = mean(n))

# cuanto carros 



# cuanto me cuestan - inversion



# cuanto gasto en mantenimiento




# Cuantos debemos alquilar




# Costo de alquiler



# Tabla de precios 
data %>%
  group_by(Cod) %>% 
  summarise(minimo = min(factura), 
            promedio = mean(factura), 
            maximo = max(factura)
            )




