Lab 4 Lubridate
================

## Parte 1: Predecir un eclipse solar

En tiempo de Norte América, el eclipse total inició el 21 de agosto del
2017 a las 18:26:40.

Este mismo evento, sucederá un Saros después. Un Saros equivale a 223
Synodic Months Un Synodic Month equivale a 29 días con 12 horas, con 44
minutos y 3 segundos.

Con esta información, predecir el siguiente eclipse solar.

``` r
eclipse <- ymd_hms("2017/08/21 18:26:40")
eclipse
```

    ## [1] "2017-08-21 18:26:40 UTC"

``` r
synodic <- ddays(x=29) + dhours(x= 12) + 
  dminutes(x=44) + dseconds(x=3)
synodic
```

    ## [1] "2551443s (~4.22 weeks)"

``` r
saros <- 223 * synodic 
saros 
```

    ## [1] "568971789s (~18.03 years)"

``` r
nuevo_eclipse <- eclipse + saros
nuevo_eclipse
```

    ## [1] "2035-09-02 02:09:49 UTC"
