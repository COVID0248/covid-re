# covid-re

Estimación (preliminar) del efecto de las fases del plan paso a paso sobre distintas medidas del R efectivo.

## Construcción

Para construir este proyecto, ejecute lo siguiente en R:

```r
renv::restore(prompt = FALSE)
targets::tar_make("r")
```

Note que la instalación del paquete `sf` solo será exitosa si Ud. cuenta con sus dependencias. Para mayo información, visite el [sitio web de la librería](https://r-spatial.github.io/sf/). Note además que la librería rstan es una librería con muchas artefactos (dependencias que no son otros paquetes de R), por lo que es difícil asegurar su instalación automática en cualquier computador (por lo menos, no sin Docker). En caso de que rstan falle, se recomienda reinstalarlo manualmente. Para mayor información, visite el [sitio web de la librería](https://mc-stan.org/users/interfaces/rstan).

Una vez construido este proyecto, puede invocar cualquier target (e.g. `r`), ejecutando

```r
targets::tar_read("r")
```

Todos los targets están en `_targets.R`, mientras que las funciones llamadas por estos están en `R/targets.R`. Los targets asociados a las 5 figuras del estudio son:

  1. `plot_r_p10`.
  2. `plot_r_p50`.
  3. `plot_r_p90`.
  4. `plot_r_ts`.
  5. `plot_r_bp`.
  
Así, por ejemplo, el usuario puede replicar la figura 1 ejecutando

```r
targets::tar_make("r_p10")
targets::tar_read("r_p10")
```
