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


df <- DBI::dbGetQuery(
  con,
  "
  SELECT
    id_producto,
    id_establecimientos,
    AVG(precio),
    EXTRACT(year from fecha) * 100 + EXTRACT(month from fecha) as year_month
  FROM
    scraping_precios.fact_price
  WHERE
    id_producto BETWEEN 225 AND 297
    OR id_producto IN (1,2,3,10,11,12,15,16,17,18,19,20,24,25,29,30,48,49,50,52,53,54,55,56,57,76,77,78,85,86,87,121,130,131,132,136,137,138,140,141,142) 
  GROUP BY id_producto, id_establecimientos, EXTRACT(year from fecha) * 100 + EXTRACT(month from fecha)
  "
)

saveRDS(df, "precios.RDS")
