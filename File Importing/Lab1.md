Laboratorio 1: Extraccion de archivos
================
Cruz del Cid
2022-08-03

## Problema 1

Ha sido contratado para trabajar en una consultoría a una embotelladora
nacional. La embotelladora se encarga de distribuir su producto a
distintos clientes, utilizando diferentes equipos de transporte y
pilotos.

``` r
read_add <- function(file_name){ 
  x = read_xlsx(file_name) 
  x$Fecha <- c(substr(file_name, star=1, stop=7))
  return(x)
}


files <- list.files(pattern = "*.xlsx")

tablas <- lapply(files, read_add) %>% bind_rows()
```

    ## New names:
    ## * `` -> `...10`

``` r
tablas <- tablas[c("COD_VIAJE", "CLIENTE", "UBICACION", 
         "CANTIDAD", "PILOTO", "Q", "CREDITO", 
         "UNIDAD", "Fecha")]


write_excel_csv2(tablas, "tablas.csv", delim = ",")

str(tablas)
```

    ## tibble [2,180 x 9] (S3: tbl_df/tbl/data.frame)
    ##  $ COD_VIAJE: num [1:2180] 1e+07 1e+07 1e+07 1e+07 1e+07 ...
    ##  $ CLIENTE  : chr [1:2180] "EL PINCHE OBELISCO / Despacho a cliente" "TAQUERIA EL CHINITO |||Faltante" "TIENDA LA BENDICION / Despacho a cliente" "TAQUERIA EL CHINITO" ...
    ##  $ UBICACION: num [1:2180] 76002 76002 76002 76002 76001 ...
    ##  $ CANTIDAD : num [1:2180] 1200 1433 1857 339 1644 ...
    ##  $ PILOTO   : chr [1:2180] "Fernando Mariano Berrio" "Hector Aragones Frutos" "Pedro Alvarez Parejo" "Angel Valdez Alegria" ...
    ##  $ Q        : num [1:2180] 300 358.2 464.2 84.8 411 ...
    ##  $ CREDITO  : num [1:2180] 30 90 60 30 30 30 90 60 30 90 ...
    ##  $ UNIDAD   : chr [1:2180] "Camion Grande" "Camion Grande" "Camion Grande" "Panel" ...
    ##  $ Fecha    : chr [1:2180] "01-2018" "01-2018" "01-2018" "01-2018" ...

## Problema 2

Utilizando la función lapply, encuentre la moda de cada vector de una
lista de por lo menos 3 vectores.

``` r
generate_vector <- function(x){ return(
  c(sample(1:10, size = 20, replace = TRUE))
  )
}

Mode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


lista <- lapply(1:3, generate_vector)
lista
```

    ## [[1]]
    ##  [1]  9  1  3  4  9  2  8  4  7  5  1  8  6  3  5  2  8 10  9  8
    ## 
    ## [[2]]
    ##  [1]  3  9  2  9  3  9  9  3  8 10  3  8  6  2  5  5  3 10  3  8
    ## 
    ## [[3]]
    ##  [1]  1  1  2  7  1  2  6  1  4  3  5  2  1  5  6  4  1  3  7 10

``` r
modas <- lapply(lista, Mode)
modas
```

    ## [[1]]
    ## [1] 8
    ## 
    ## [[2]]
    ## [1] 3
    ## 
    ## [[3]]
    ## [1] 1

## Problema 3

A. Descargue de la página web de la SAT el aechivo de Parque Vehicular
de Enero 2019. B. Leer el archivo en R. (Nota: usar read_delim() del
paquete readr)

``` r
parque_vehicular <- read_delim("INE_PARQUE_VEHICULAR_080219.txt", delim = "|")
```

    ## New names:
    ## * `` -> `...11`

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 2435294 Columns: 11
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "|"
    ## chr (8): MES, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, MODELO_VEHICULO, LINEA_...
    ## dbl (2): ANIO_ALZA, CANTIDAD
    ## lgl (1): ...11
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(parque_vehicular)
```

    ## # A tibble: 6 x 11
    ##   ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##       <dbl> <chr> <chr>               <chr>            <chr>          
    ## 1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ## 2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ## 3      2007 05    SAN MARCOS          "OCOS"           2007           
    ## 4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ## 5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ## 6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ## # ... with 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, ...11 <lgl>
