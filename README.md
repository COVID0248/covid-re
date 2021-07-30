# covid-re

Estimación (preliminar) del R efectivo a nivel comunal/semanal.

## Construcción

Para construir este proyecto, ejecute lo siguiente en R:

```r
renv::restore(prompt = FALSE)
targets::tar_make("r")
```

Note que la instalación del paquete `sf` solo será exitosa si Ud. cuenta con sus dependencias. Para mayor información, visite el [sitio web de la librería](https://r-spatial.github.io/sf/). Note además que la librería rstan usa muchos artefactos (i.e. tiene dependencias que no son otros paquetes de R), por lo que es difícil asegurar su instalación automática en cualquier computador (por lo menos, no sin Docker). En caso de que rstan falle, se recomienda reinstalarlo manualmente. Para mayor información, visite el [sitio web de la librería](https://mc-stan.org/users/interfaces/rstan).

Una vez construido este proyecto, puede invocar cualquier target (e.g. `r`), ejecutando

```r
targets::tar_read("r")
```

Todos los targets están en `_targets.R`, mientras que las funciones llamadas por estos están en `R/targets.R`. Los targets asociados a las 5 figuras del estudio son:

  1. `plot_r_p10`. Percentil 10 del R efectivo, según comuna y método.
  2. `plot_r_p50`. Percentil 50 del R efectivo, según comuna y método.
  3. `plot_r_p90`. Percentil 90 del R efectivo, según comuna y método.
  4. `plot_r_bp`. Boxplot del R efectivo, según método y semana epidemiológica.
  5. `plot_r_ts`. R efectivo para 4 comunas seleccionadas, según método y semana epidemiológica.

y de la misma manera, el target al Cuadro 1 es `b_gamma`. Así, por ejemplo, el usuario puede replicar la figura 1 ejecutando

```r
targets::tar_make("r_p10")
targets::tar_read("r_p10")
```

Quien desee entender las dependencias entre los objetos intermedios creados durante las reconstrucción del proyecto (digamos, para entender la lógica del proyecto), puede visualizar la red dependencias ejecutando:

```
targets::tar_visnetwork()
``` 
