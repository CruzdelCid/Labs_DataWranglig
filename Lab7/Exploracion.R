library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(highcharter)
library(plotly)
library(tidyverse)
library(sf)
library(mapview)


#####Preparacion de datos#### 
data <- read_csv("c1.csv")
View(data)

data <- data[-c(23, 24, 25, 26, 27, 28)]
str(data)

data %>% 
  filter(nchar(Pickup)>2 & nchar(Moto)>2)

data <- data %>% 
  mutate(Fecha = dmy(Fecha))


data <- data %>% 
  mutate(Camion_5 = gsub("Q","",Camion_5),
         Camion_5 = gsub("-","0",Camion_5), 
         Camion_5 = as.numeric(Camion_5), 
         Pickup = gsub("Q","",Pickup),
         Pickup = gsub("-","0",Pickup), 
         Pickup = as.numeric(Pickup),
         Moto = gsub("Q","",Moto),
         Moto = gsub("-","0",Moto), 
         Moto = as.numeric(Moto))

data <- data %>% 
  mutate(factura = gsub("Q","",factura),
         factura = gsub("-","0",factura), 
         factura = as.numeric(factura), 
         
         directoCamion_5 = gsub("Q","",directoCamion_5),
         directoCamion_5 = gsub("-","0",directoCamion_5), 
         directoCamion_5 = as.numeric(directoCamion_5), 
         
         directoPickup = gsub("Q","",directoPickup),
         directoPickup = gsub("-","0",directoPickup), 
         directoPickup = as.numeric(directoPickup), 
         
         directoMoto = gsub("Q","",directoMoto),
         directoMoto = gsub("-","0",directoMoto), 
         directoMoto = as.numeric(directoMoto), 
          
         fijoCamion_5 = gsub("Q","",fijoCamion_5),
         fijoCamion_5 = gsub("-","0",fijoCamion_5), 
         fijoCamion_5 = as.numeric(fijoCamion_5), 
         
         fijoPickup = gsub("Q","",fijoPickup),
         fijoPickup = gsub("-","0",fijoPickup), 
         fijoPickup = as.numeric(fijoPickup), 
         
         fijoMoto = gsub("Q","",fijoMoto),
         fijoMoto = gsub("-","0",fijoMoto), 
         fijoMoto = as.numeric(fijoMoto))

data <- data %>% 
  mutate(Vehiculo = case_when(Camion_5 != 0 ~ "Camion", 
                              Pickup != 0~ "Pickup", 
                              Moto != 0 ~ "Moto"))
data <- data %>% 
  mutate(Costo_total = case_when(Camion_5 != 0 ~ Camion_5, 
                           Pickup != 0~ Pickup, 
                           Moto != 0 ~ Moto))

data <- data %>% 
  mutate(Costo_directo = case_when(directoCamion_5 != 0 ~ directoCamion_5, 
                                   directoPickup != 0~ directoPickup, 
                                   directoMoto != 0 ~ directoMoto))

data <- data %>% 
  mutate(Costo_fijo = case_when(fijoCamion_5 != 0 ~ fijoCamion_5, 
                                fijoPickup != 0~ fijoPickup, 
                                fijoMoto != 0 ~ fijoMoto))
data %>% 
  select(Costo_total, Costo_fijo, Costo_directo) %>% 
  mutate(xx=Costo_fijo + Costo_directo) %>% 
  filter(Costo_total != (xx))



n <- names(data)

n[18] <- "r1"
n[19] <- "r2"
n[20] <- "r3"
n[21] <- "r4"
n[22] <- "r5"
n

names(data) <- n


data <- data %>% 
  mutate(tiempo = case_when(r1 == "x" ~ 17, 
                            r2 == "x" ~ 37, 
                            r3 == "x" ~ 60, 
                            r4 == "x" ~ 97, 
                            r5 == "x" ~ 120))
"
data <- data %>% 
  mutate(tiempo = case_when(5-30, 
                            30-45  
                            45-75
                            75-120
                            120+ 
"


#####Analisis explorativo#####

# ggplot(data, aes(x=Camion_5)) + geom_histogram(color="black", fill="white")
# ggplot(data, aes(x=Pickup)) + geom_histogram(color="black", fill="white")
# ggplot(data, aes(x=Moto)) + geom_histogram(color="black", fill="white")




# Cantidad por servicios 
data %>% 
  group_by(Cod) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  hchart("column",hcaes(x = Cod, y = n)) 

# Ganancia Operacional
data <- data %>% 
  mutate(ProfitO = factura - (Camion_5 + Pickup + Moto))

data <- data %>% 
  mutate(ProfitO1 = factura - (directoCamion_5 + directoPickup + directoMoto + 
                                fijoCamion_5 + fijoPickup + fijoMoto))


sum(data$ProfitO)
sum(data$ProfitO1)


# Ganancia total por servicio
data %>% 
  group_by(Cod) %>% 
  summarise(Profit = sum(ProfitO),
            n = n()) %>% 
  arrange(desc(Profit)) %>%
  hchart("column",hcaes(x = Cod, y = Profit)) 


# Ganancia promedio por servicio
data %>% 
  group_by(Cod) %>% 
  summarise(mean_profit = round(mean(ProfitO),2)) %>% 
  arrange(desc(mean_profit)) %>%
  hchart("column",hcaes(x = Cod, y = mean_profit)) 


#Costos por servicio a lo largo del tiempo

set.seed(20)
data %>% 
  select(Fecha, Costo_total, Cod) %>% 
  filter(Cod == "REVISION") %>% 
  sample_n(size = 50) %>% 
  arrange(Fecha) %>% 
  hchart("scatter", hcaes(x=Fecha, y= Costo_total))



data %>% 
  select(Fecha, Costo_total, Cod) %>% 
  filter(Cod == "VERIFICACION_MEDIDORES") %>% 
  ggplot(mapping = aes(x = Fecha, y = Costo_total)) +
  geom_function()





data %>% 
  select(Fecha, Costo_total, Cod) %>% 
  filter(Cod == "CAMBIO_FUSIBLE") %>% 
  arrange(Fecha)  %>%
  ggplot(aes(x=Costo_total)) + geom_histogram(color="black", fill="white")

data %>% 
  select(Fecha, Costo_total, Cod) %>% 
  filter(Cod == "VERIFICACION_INDICADORES") %>% 
  arrange(Fecha)  %>%
  ggplot(aes(x=Costo_total)) + geom_histogram(color="black", fill="white")

data %>% 
  select(Fecha, Costo_total, Cod) %>% 
  filter(Cod == "CAMBIO_CORRECTIVO") %>% 
  arrange(Fecha)  %>%
  ggplot(aes(x=Costo_total)) + geom_histogram(color="black", fill="white")




attach(mtcars)
par(mfrow=c(3,1))
hist(wt)
hist(mpg)
hist(disp)




ggp <- ggplot(res)  + 
  geom_bar(aes(x=Cod, y=course),stat="identity", fill="cyan",colour="#006000")+
  geom_line(aes(x=Cod, y=100*penroll),stat="identity",color="red",size=2)+
  labs(title= "Courses vs Students Enrolled in GeeksforGeeks",
       x="Year",y="Number of Courses Sold")+
  scale_y_continuous(sec.axis=sec_axis(~.*0.01,name="Percentage"))
ggp


# Baja de ventas - Ver cuantos servicios hicimos al mes
# Aumentos de costos - Ver nuestros proveedores de transporte
# Baja de precios - Facturas


# ¿Como podemos subir un 10% para el siguiente año? 
# Para ganar más es necesario subir los precios o bajar los costos
# ver si los costos flucturon mucho, costos más bajos y más alto
# Ver el rango de los costos por ventas
# buscar otro rango de costos de venta y mejorarlo para que en el 2019 

# Por qué cobramos menos??



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


# ventas por mes   
data %>% 
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(cantidad = n()) %>%
  hchart("bar", hcaes(x=mes,y=cantidad))


# ganancias por mes   
data %>% 
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = sum(ProfitO)) %>%
  hchart("line", hcaes(x=mes,y=ganancias)) # hacer grafica de linea 


data %>% 
  filter(Cod=="REVISION") %>%
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = sum(ProfitO)) %>%
  hchart("line", hcaes(x=mes,y=ganancias)) # hacer grafica de linea 

a <- function(nombre){
  plot_ly(data = data %>% 
    filter(Cod=="REVISION") %>%
    mutate(mes = month(Fecha)) %>% 
    group_by(mes) %>% 
    summarise(ganancias = sum(ProfitO)), 
    
    
  )
    
  
}


a <- data %>% 
  filter(Cod=="REVISION") %>%
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = sum(ProfitO)) %>% 
  plot_ly(x = ~mes,y = ~ganancias,type="scatter",mode="lines+markers") 
a

b <- data %>% 
  filter(Cod=="VERIFICACION_MEDIDORES") %>%
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = sum(ProfitO)) %>% 
  plot_ly(x = ~mes,y = ~ganancias, type="scatter",mode="lines+markers")
b

c <- data %>% 
  filter(Cod=="CAMBIO_FUSIBLE") %>%
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = sum(ProfitO)) %>% 
  plot_ly(x = ~mes,y = ~ganancias, type="scatter",mode="lines+markers")
c

d <- data %>% 
  filter(Cod=="VERIFICACION_INDICADORES") %>%
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = sum(ProfitO)) %>% 
  plot_ly(x = ~mes,y = ~ganancias, type="scatter",mode="lines+markers")
d




subplot(a,b,c,d, nrows = 2) %>% 
  layout(title = 'Side By Side Subplots')

# ganancias por mes   
data %>% 
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = sum(ProfitO)) %>%
  hchart("line", hcaes(x=mes,y=ganancias))


# ganancias promedio por mes   
data %>% 
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = mean(ProfitO)) %>%
  hchart("line", hcaes(x=mes,y=ganancias))


data %>% 
  mutate(mes = day(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(ganancias = sum(ProfitO)) %>%
  hchart("line", hcaes(x=mes,y=ganancias))


data %>% 
  group_by(Fecha) %>% 
  summarise(ganancias = sum(ProfitO)) %>%
  hchart("scatter", hcaes(x=Fecha,y=ganancias))


data %>% 
  nrow()

data %>% 
  select(ID) %>% 
  unique() %>% 
  nrow()

data %>% 
  summarise(n=n_distinct(Lat,Long))

# analisis por vehiculos
data %>% 
  group_by(Vehiculo) %>% 
  summarise(n=n())

res <- data %>%
  select(Lat,Long) %>% 
  unique() %>%
  sample_n(size = 100)


mapview(res, xcol = "Lat", ycol = "Long", crs = 4326)


data %>% 
  group_by(origen) %>% 
  summarise(n=n())


data %>% 
  filter(origen == 150224) %>% 
  mutate(mes = month(Fecha)) %>% 
  group_by(mes) %>% 
  summarise(n=n()) %>% 
  hchart("line", hcaes(x=mes,y=n))
