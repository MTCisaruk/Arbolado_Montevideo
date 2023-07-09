library(RColorBrewer)
library(shiny)
library(leaflet)
library(tidyverse)

precios <- readRDS(here("precios_canasta.RDS"))

establecimientos <- read_delim("establecimiento.csv", delim = ";", locale = locale(encoding = "latin1")) %>% 
  select(id.establecimientos, nombre.sucursal, ccz, cadena, long, lat, depto, id.depto) %>% 
  mutate(lat=as.numeric(sub(',','.',lat,fixed=T)),
         long=as.numeric(sub(',','.',long,fixed=T)),
         # arreglamos Ta Ta que le faltaba el signo 
         long = ifelse(long > 0, -34.88675, long),
         # arreglamos San Roque del aeropuerto, que estaba mal
         long = ifelse(id.establecimientos == 679, -34.83675, long),
         lat = ifelse(id.establecimientos == 679, -56.01600, lat)) %>% 
  filter_at(c("long", "lat"), function(x) !is.na(x)) |> mutate(lng=lat,lat=long) |> select(-long)


preciosmvd <- filter(precios,depto=="Montevideo") |> select(-c(id_producto,nombre.sucursal,barrio,cadena,depto,id.depto,nombre,geometry))
anho2023 <- c("2023-01-01","2023-02-01","2023-03-01")
marcas <- levels(as.factor(preciosmvd$marca))

establecimientosmvd <- filter(establecimientos,id.establecimientos %in% preciosmvd$id_establecimientos) |> filter(!(id.establecimientos==874)) |>  # se elimina porque está en durazno o mal puesta
  select(-c(ccz,depto,id.depto))

productos2 <- levels(as.factor(preciosmvd$producto))


ui <- fluidPage(
  titlePanel("Mapa"),
  sidebarLayout(
    sidebarPanel=sidebarPanel(
      selectizeInput("eleg_producto",'Producto:',choices=productos2,options = list(maxItems = 1)),
      selectizeInput("eleg_marca",'Marca:',choices=marcas) # Cómo hago un input reactivo
    ),
    mainPanel=mainPanel(leafletOutput("mapa"))
  )
)
server <- function(input, output) {
  #  observe({
  #  marcas <- levels(as.factor(filter(preciosmvd,producto %in% input$eleg_producto)$marca))
  # updateSelectizeInput(session=getDefaultReactiveDomain(),"eleg_marca","Marca:",choices=marcas)
  # })
  preciosproducto <- reactive({
    filter(preciosmvd,
           producto %in% input$eleg_producto,
           marca %in% input$eleg_marca)
  })
  precioestablecimiento <- reactive({
    preciosproducto <- preciosproducto()
    preciosproducto |> 
      filter(year_month %in% anho2023) |> 
      group_by(id_establecimientos) |> 
      summarise(id_establecimientos,precio=mean(avg)) #SACAR ID PARA IDENTIFICAR
  })
  establecimientosproducto <- reactive({
    preciosproducto <- preciosproducto()
    precioestablecimiento <- precioestablecimiento()
    filter(establecimientosmvd,
           id.establecimientos %in% preciosproducto$id_establecimiento) |> 
      left_join(precioestablecimiento,
                by = c("id.establecimientos" = "id_establecimientos"),
                suffix=c("","")) #METER EL PRECIO EN EL ESTABLECIMIENTO
  })
  output$mapa <- renderLeaflet({
    establecimientosproducto <- establecimientosproducto()
    pal <- colorNumeric(palette = "OrRd",domain = establecimientosproducto$precio)
    leaflet(establecimientosproducto) %>% addTiles() %>%
      fitBounds(-56.270, -34.836, -56.091, -34.929)%>%
      clearShapes() %>%
      addCircles(radius=15,weight = 0.5, color = "black",fillColor = ~pal(precio), fillOpacity = 0.7, popup = ~paste("Local:",nombre.sucursal,"<br>","Precio:",precio))
  })
}
shinyApp(ui, server)