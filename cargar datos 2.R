precios_2023 <- read_csv("datos/precios_2023.csv") %>% 
  filter(id_producto == 250)

establecimientos <- read_delim("datos/establecimiento.csv", delim = ";")

productos <- read_csv2("datos/productos.csv")

usethis::edit_r_environ(
  scope = "project"
)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)


precios_canasta <- DBI::dbGetQuery(
  con,
  "
  SELECT
    id_producto,
    id_establecimientos,
    precio,
    DATE_FORMAT(date, '%Y-%m') AS year_and_month
  FROM
    scraping_precios.fact_price
  WHERE
    id_producto IN (1,2,3,13,14,15,16,17,18,19,20,21,23,22,24,25,29,30,40,41,48,49,50,52,53,54,61,62,76,77,78,85,86,87,102,103,104,121,122,123,124,130,131,132,133,134,135,140,141,142,149,150,151,26,27,28,55,56,57,359,361,365) 
  GROUP BY id_producto, id_establecimientos, precio, DATE_FORMAT(date, '%Y-%m') AS year_and_month
  "
)

establecimientos <- read_delim("datos/establecimiento.csv", delim = ";") %>% 
  select(id.establecimientos, nombre.sucursal, barrio, cadena, long, lat, depto)

productos <- read_csv2("datos/productos.csv") %>% 
  select(id.producto, producto, marca, nombre)

precios_canasta <- left_join(precios_canasta, establecimientos, by = c("id_establecimientos" = "id.establecimientos")) %>% 
  left_join(productos, by = c("id_producto" = "id.producto"))

saveRDS(precios_canasta, "precios_canasta.RDS")
