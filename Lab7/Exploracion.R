library(readr)
library(dplyr)

data <- read_csv("c1.csv")
View(data)

data <- data[-c(23, 24, 25, 26, 27, 28)]
str(data)

data %>% 
  filter(nchar(Pickup)>2 & nchar(Moto)>2)

data <- data %>% 
  mutate(Camion_5 = gsub("Q","",Camion_5),
         Camion_5 = gsub("-","",Camion_5), 
         Camion_5 = as.numeric(Camion_5), 
         Pickup = gsub("Q","",Pickup),
         Pickup = gsub("-","",Pickup), 
         Pickup = as.numeric(Pickup),
         Moto = gsub("Q","",Moto),
         Moto = gsub("-","",Moto), 
         Moto = as.numeric(Moto))
