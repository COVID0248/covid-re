# Reproduce data/df_aprox2.rds

# 1. Carga librerías auxiliares ----
library(magrittr)

# 2. Genera funciones de ayuda ----

# 2.1. is_integer64 - Verifica si el argumento es de clase integer64 ----
is_integer64 <- function(x) class(x)[1] == "integer64"

# 2.2. get_sem_epi - Retorna la se asociada a una fecha ----
get_sem_epi <- function(x = Sys.Date()) { 
  as.integer(difftime(x, "2020-02-23", units = "weeks")) + 9
}

# 3. Importa BBDD desde AWS ----

# 3.1. Establece una conexión con la nube ----
conn <-
  noctua::athena() %>%
  noctua::dbConnect(
    profile_name   = "covid-anid",
    s3_staging_dir = "s3://covid-anid/athena-results/",
    schema_name    = "struct_covid",
    region_name    = "us-east-1",
    bigint         = "integer"
  )


# Carga una BBDD de prueba
# 3.2. Importa las tablas/vistas ----
tbl <-
  purrr::set_names(c(
    "vecinos", 
    "comunas", 
    "p19_activos_dc", 
    "p29_cuarentena", 
    "vw_data_aproximacion_2"
  )) %>%
  purrr::map(~ dplyr::tbl(conn, .x) %>% dplyr::collect())
