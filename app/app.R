library(readr)
library(tidyverse)
library(sf)
library(leaflet)
library(terra)
library(RColorBrewer)
library(xgboost)
library(caret)
library(tidymodels)
library(modeltime)
library(timetk)
library(shiny)
library(gt)
library(plotly)

precios <- readRDS("precios_canasta.RDS")

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

df_depto <- st_as_sf(vect("ine_depto.shp")) %>% st_set_crs(5382) %>%  st_transform(4326)
productos2 <- levels(as.factor(preciosmvd$producto))


ui <- fluidPage(
  titlePanel("Mapa"),
  sidebarLayout(
    sidebarPanel=sidebarPanel(
      selectizeInput(inputId="eleg_producto",'Producto:',choices=productos2,options = list(maxItems = 1)),
      selectInput(inputId="eleg_marca",'Marca:',choices=marcas,multiple = TRUE) # Cómo hago un input reactivo
    ),
    mainPanel=mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Establecimientos de Montevideo", leafletOutput("mapa")),
                  tabPanel("Precios por departamento",plotOutput("uruguay")),
                  tabPanel("Predicción de precio",plotlyOutput("modelo"))
      )
    )
  )
)
server <- function(input, output) {

  observeEvent(input$eleg_producto,{
    updateSelectInput(session=getDefaultReactiveDomain(),"eleg_marca",choices = levels(as.factor(filter(preciosmvd,producto %in% input$eleg_producto)$marca)))
    })
  
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
      summarise(id_establecimientos,precio=round(mean(avg), 1)) #SACAR ID PARA IDENTIFICAR
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
  
  
  
  output$uruguay <- renderPlot({
    prod_dpto <- precios %>%
      filter(producto %in% input$eleg_producto, marca %in% input$eleg_marca) %>%
      group_by(id.depto) %>%
      summarise(precio = mean(avg))

    ggplot(data=df_depto %>% left_join(prod_dpto, by = c("DEPTO" = "id.depto"))) +
      geom_sf(aes(fill=precio),color="gray20") +
      scale_fill_gradientn(colours = brewer.pal(5, "OrRd")) +
      theme_void() +
      labs(title="Departamentos") +
      theme(plot.title=element_text(hjust=1/2))
  })
  
  

  output$modelo <- renderPlotly({
    prod <- precios %>%
        filter(producto %in% input$eleg_producto & marca %in% input$eleg_marca) %>%
        group_by(year_month) %>%
        summarise(precio = mean(avg))

    if(nrow(prod)>0){

    splits <- time_series_split(prod, year_month, assess = "3 months")

    model_arima <- arima_reg() %>%
      set_engine("auto_arima") %>%
      fit(precio ~ year_month, training(splits))

     model_glmnet <- linear_reg(penalty = 0.01) %>%
       set_engine("glmnet") %>%
       fit(precio ~ month(year_month) + as.numeric(year_month), training(splits))

     model_prophet <- prophet_reg() %>%
       set_engine("prophet") %>%
       fit(precio ~ year_month, training(splits))

    model_tbl <- modeltime_table(
       model_prophet,
       model_glmnet,
      model_arima)

    calib_tbl <- model_tbl %>%
      modeltime_calibrate(testing(splits))
    future_forecast <- calib_tbl %>%
      modeltime_refit(prod) %>%
      modeltime_forecast(h = "9 months", actual_data = prod)

    future_forecast %>% plot_modeltime_forecast(
      .title = paste("Precio de", input$eleg_producto, "entre 2016 y 2023 ($)"),
      .conf_interval_alpha = 0.1)

    }else{plotly_empty()}
  })
}
shinyApp(ui, server)