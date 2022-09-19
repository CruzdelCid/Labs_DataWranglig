dw-2022-parcial-1
================
Cruz del Cid
9/19/2022

# Examen parcial

Indicaciones generales:

-   Usted tiene el período de la clase para resolver el examen parcial.

-   La entrega del parcial, al igual que las tareas, es por medio de su
    cuenta de github, pegando el link en el portal de MiU.

-   Pueden hacer uso del material del curso e internet (stackoverflow,
    etc.). Sin embargo, si encontramos algún indicio de copia, se
    anulará el exámen para los estudiantes involucrados. Por lo tanto,
    aconsejamos no compartir las agregaciones que generen.

## Sección I: Preguntas teóricas.

-   Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
    deberá responder 5. Las 5 a responder estarán determinadas por un
    muestreo aleatorio basado en su número de carné.

-   Ingrese su número de carné en `set.seed()` y corra el chunk de R
    para determinar cuáles preguntas debe responder.

``` r
set.seed(20200394) 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 4, 5, 7, 8"

\###Mis preguntas a resolver son: 1, 4, 5, 7, 8

### Listado de preguntas teóricas

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:

    -   `str()`
    -   `df[,c("a","b")]`
    -   `names(df)[4] <- "new_name"` donde la posición 4 corresponde a
        la variable `old_name`
    -   `df[df$variable == "valor",]`

    Contraparte

    -   df %\>% str()
    -   df %\>% select(a, b)
    -   df \<- df %\> rename(old_name = ‘new_name’)
    -   df %\>% filter(varible == “valor”)

2.  Al momento de filtrar en SQL, ¿cuál keyword cumple las mismas
    funciones que el keyword `OR` para filtrar uno o más elementos una
    misma columna?

3.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

4.  ¿Cuál es la diferencia entre utilizar `==` y `=` en R?

‘==’ Se utiliza para comparar el valor de dos varibles. Da como
resultado TRUE si son iguales. ‘=’ Se utiliza para asignar un valor a
una variable

5.  ¿Cuál es la forma correcta de cargar un archivo de texto donde el
    delimitador es `:`?

La forma correcta es utilizar la función read_delim, donde como primer
parámetro le madamos el nombre de nuestro archivo txt y como segundo
parámetro delim = “:” indicando que nuestro delimitador es “:”

Ejemplo: txt_1 \<- read_delim(“example_3.txt”, delim = “:”)

6.  ¿Qué es un vector y en qué se diferencia en una lista en R?
7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes?

Lo que pasará es que se agregará un NA un Not Available, un valor
perdido. Porque el factor no soporta un valor con una categoría que no
exista. Por ejemplo, si tenemos las catogorías o levels c(“alto”,
“bajo”)y agregamos un valor “medio” se guardará NA.

8.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?
    -   El nuevo elemento
    -   `NA`

El resultado esperado sería “NA” porque el valor no es reconocido por el
factor. Lo que hacen el factor es delimitar un lista de valores posibles
que podemos agregar, si intentamos agregar uno que no esté en esa lista
guardará “NA” esa celda.

9.  En SQL, ¿para qué utilizamos el keyword `HAVING`?

10. Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?

    -   SELECT \* FROM A \_\_\_\_\_\_\_ B ON A.KEY = B.KEY WHERE
        \_\_\_\_\_\_\_\_\_\_ = \_\_\_\_\_\_\_\_\_\_

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

Cantidad de posibles exámenes:

``` r
library(gtools)

nrow(combinations(10, 5)) 
```

    ## [1] 252

## Sección II Preguntas prácticas.

-   Conteste las siguientes preguntas utilizando sus conocimientos de R.
    Adjunte el código que utilizó para llegar a sus conclusiones en un
    chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas teóricas

## A

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
df <- readRDS("parcial_anonimo.rds")

df %>%
  group_by(Cliente) %>% 
  summarise(Paises = n_distinct(Pais),
            Ingresos = sum(Venta)) %>%
  filter(Paises > 1) %>%
  arrange(desc(Ingresos)) %>%
  head(1)
```

    ## # A tibble: 1 x 3
    ##   Cliente  Paises Ingresos
    ##   <chr>     <int>    <dbl>
    ## 1 a17a7558      2   19818.

El cliente más rentable que está en más de un país es a17a7558, o mejor
conocido como “Distribuidora El Gallo”, porque es la que tienen el
ingreso mayor. La rentabilidad se tomó como la cantidad de ingresos.

## B

Supuestos:

-   Estamos asumiendo que los costos para mantener operaciones en un
    territorio es de Q1000.00

-   Tendrán perdidas cuando el ingreso no supere los costos de mentener
    la operacion.

-   Las perdidas son considerables cuando estén por arriba de Q100.00

``` r
territorios <- df %>%
  group_by(Territorio) %>% 
  summarise(Ingresos = sum(Venta)) %>%
  filter(Ingresos < 900) %>% 
  arrange(desc(Ingresos)) %>% 
  mutate(Perdidas = Ingresos - 1000)


territorios
```

    ## # A tibble: 11 x 3
    ##    Territorio Ingresos Perdidas
    ##    <chr>         <dbl>    <dbl>
    ##  1 0320288f      845.     -155.
    ##  2 aed8e579      747.     -253.
    ##  3 3e0d75d0      647.     -353.
    ##  4 4163fa3f      580.     -420.
    ##  5 456278b8      493.     -507.
    ##  6 0bfe69a0      384.     -616.
    ##  7 e034e3c8      247.     -753.
    ##  8 79428560      132      -868 
    ##  9 368301e2      121.     -879.
    ## 10 13b223c9       49.9    -950.
    ## 11 e6fd9da9       18.2    -982.

Se deben parar operaciones en los territorios listados arriba porque sus
perdidas superan los Q100. Que son pérdidas considerables como se aclara
en los supuestos.
