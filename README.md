# covid-re

Estimación del efecto de un cambio de fase en el plan paso a paso sobre distintas medidas del R efectivo.

# Construcción

Para construir este proyecto, ejecute lo siguiente en R:

```r
renv::restore(prompt = FALSE)
targets::tar_make()
```

Una vez construido este proyecto, puede invocar cualquier target (e.g. `r`), ejecutando

```r
targets::tar_read("r")
```

El listado de todos los targets está en `_targets.R`, mientras que las funciones llamadas para producirlos están en `R/targets.R`. 