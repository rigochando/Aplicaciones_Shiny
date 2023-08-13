#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(scales)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gráficos de producción"),

    #Widget
    sidebarLayout(
        sidebarPanel(
          #Seleciona el tipo de fluido
          selectInput("fluido","Fludio", c("Qo", "Qw", "Qg")),
          
          #Activar para graficar producción acumulada
          checkboxInput("AcumShow" ,"Ver producción acumulada"),
          
          #seleciona los pozos a graficar
          selectInput("pozos", "Pozos",choices = c("None"), selected = "None",  size = 10, multiple=TRUE, selectize=FALSE),
          
          #Activar para generar matriz de graficos
          checkboxInput("facetGrafico" ,"Grafico por pozo"),
          #Activar para cambiar escala del y a log
          checkboxInput("logYaxis" ,"Eje y logaritmico"),
          
          #Selecciona rl tipo de linea
          selectInput("Plot_Type","Plot Type", 
                      c("Line", "Points", "Line - Points"), selected = "Line - Points")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("plot_Datos", height = "70vh")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
    #Lectura y carga de datos desde archivo CSV
    datosProd <- read.csv("Production_Data.csv", header = TRUE)
    datosProd$Date <- as.Date(as.character(datosProd$Date), "%d/%m/%Y")
    pozos <- c(unique(as.character(datosProd$Well)))
    
    #Actualización de lista de pozos de acuerdo a los datos cargados
    updateSelectInput(getDefaultReactiveDomain(), "pozos", label = "Pozos", choices = pozos,selected = pozos[1])
    
    #Calculo de producción acumulada
    datosProd <- datosProd %>% 
      group_by(Fluid, Well) %>% 
      arrange(Date) %>%
      mutate(days = days_in_month(Date),
             cum_month = Rate*days, 
             Np = cumsum(cum_month)/1000000) %>%
      as.data.frame()
    
    
    #Grafico de producción
    output$plot_Datos <- renderPlotly({
      
      #Filtro de datos de acuero a pozos y fluido
      datosPlot <- datosProd %>%
        filter(Fluid == as.character(input$fluido)) %>%
        filter(Well == as.character(input$pozos))
      
      #Inicializacion de objeto ggplot
      p <- ggplot(datosPlot)
      
      
      #Se agregan las lineas de producción acumulada o gasto segun este marcado el checkbox
      if(input$AcumShow){
        
        if(input$Plot_Type == "Line"){
          p <- p + 
            geom_line(aes(Date, Np, color = Well)) 
        } 
        if(input$Plot_Type == "Points"){
          p <- p + 
            geom_point(aes(Date, Np, color = Well)) 
        }
        if(input$Plot_Type == "Line - Points"){
          p <- p + 
            geom_line(aes(Date, Np, color = Well)) + 
            geom_point(aes(Date, Np,  color = Well)) 
        }
        
        p <- p + labs(y ="Producción acumulada (MMSTB - MMMscf)", x = "Fecha")
        
      }else{
        if(input$Plot_Type == "Line"){
          p <- p + 
            geom_line(aes(Date, Rate, color = Well)) 
        } 
        if(input$Plot_Type == "Points"){
          p <- p + 
            geom_point(aes(Date, Rate, color = Well))
        }
        if(input$Plot_Type == "Line - Points"){
          p <- p + 
            geom_line(aes(Date, Rate, color = Well)) + 
            geom_point(aes(Date, Rate, color = Well))
        }
        
        p <- p + labs(y ="Gasto (STB/dia - Mscf/dia)", x = "Fecha")
        
        
      }
        
      
      #Cambio de escala del eje y a log
      if(input$logYaxis){
        
        p <- p + scale_y_log10()
        
      }
      
      #Generación de matriz de graficos dividio por pozo
      if(input$facetGrafico){
        p <- p + facet_wrap(~Well)
      }
      
      
      #Cambio de grafico tipo ggplot a plotly para hacerlo interactivo 
      p <- ggplotly(p)
      
      p <- layout(p) %>%
        config(modeBarButtonsToAdd = list("drawline",  "eraseshape"))
       
      p
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
