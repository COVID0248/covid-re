# covid-re

Estimación (preliminar) del R efectivo a nivel comunal/semanal.

## Construcción

Para construir este proyecto, ejecute lo siguiente en R:

```r
renv::restore(prompt = FALSE)
targets::tar_make("r")
```

Note que la instalación del paquete `sf` solo será exitosa si Ud. cuenta con sus dependencias. Para mayor información, visite el [sitio web de la librería](https://r-spatial.github.io/sf/). Note además que la librería rstan usa muchos artefactos (i.e. dependencias que no son otros paquetes de R), por lo que es difícil asegurar su instalación automática en cualquier computador (por lo menos, no sin Docker). En caso de que rstan falle, se recomienda reinstalarlo manualmente. Para mayor información, visite el [sitio web de la librería](https://mc-stan.org/users/interfaces/rstan). Finalmente, es necesario que tenga el solver PARDISO propiamente instalado (la licencia académica es gratuita, pero debe ser solicitada). Para mayor información, visite el [sitio web de la librería](https://www.pardiso-project.org/)

Una vez construido este proyecto, puede invocar cualquier target (e.g. `r`), ejecutando

```r
targets::tar_read("r")
```

Todos los targets están en `_targets.R`, mientras que las funciones llamadas por estos están en `R/targets.R`. Los targets asociados a las 5 figuras del estudio son:

  1. `plot_r_p10_en`. Percentil 10 del R efectivo, según comuna y método.
  2. `plot_r_p50_en`. Percentil 50 del R efectivo, según comuna y método.
  3. `plot_r_p90_en`. Percentil 90 del R efectivo, según comuna y método.
  4. `plot_r_bp_en`. Boxplot del R efectivo, según método y semana epidemiológica.
  5. `plot_r_ts_en`. R efectivo para 4 comunas seleccionadas, según método y semana epidemiológica.

(reemplazar el sufijo `_en` por un `_es` si se desea el gráfico en español). De la misma manera, el target asociado al Cuadro 1 es `b_gamma`. Así, por ejemplo, el usuario puede replicar la figura 1 ejecutando

```r
targets::tar_make("r_p10")
targets::tar_read("r_p10")
```

Otro producto que los usuarios podrían considerar relevante es la BB.DD. con todos los r-efectivos, según comuna, semana epidemiológica y método. El target asociado a esta variable es `r`. Así, el usuario puede reconstruir esta BBDD (actualizada) ejecutando
```r
targets::tar_make("r")
targets::tar_read("r")
```

Quien desee entender las dependencias entre los objetos intermedios creados durante las reconstrucción del proyecto (digamos, para entender la lógica del proyecto), puede visualizar la red dependencias ejecutando:

```r
targets::tar_visnetwork()
``` 
