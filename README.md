# covid-re

Estimación (preliminar) del efecto de las fases del plan paso a paso sobre distintas medidas del R efectivo.

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

Todos los targets están en `_targets.R`, mientras que las funciones llamadas por estos están en `R/targets.R`. 