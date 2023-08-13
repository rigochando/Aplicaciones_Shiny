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
library(rhandsontable)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Índice de productividad"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(width = 3,
                   h3(" "),
                   selectInput("PI_cor","Método", c("Darcy (PPS)", "Voguel")),
                   
                   # &mu;
                   
                   conditionalPanel(condition = "input.PI_cor == 'Darcy (PPS)'",
                                    column (6,
                                            
                                            
                                            numericInput("PI_py","Presión de yacimiento (psi)", value = ""),
                                            numericInput("PI_ko","Permeabilidad (md)", value = ""),
                                            numericInput("PI_vo", "Viscosidad del aceite (cp)", value = ""),
                                            numericInput("PI_bo", "Factor de volumen de formación del aceite (bbl/STB)", value = ""), 
                                            
                                            checkboxInput("Darcy_PPS_vogel" ,"Vogel por debajo de la Pb (Presión de burbuja)"),
                                            conditionalPanel(condition = "input.Darcy_PPS_vogel == 1", 
                                                             numericInput("PI_Pb", "Presión de burbuja (psi)", value = "")
                                            )
                                            
                                            
                                    ),
                                    
                                    column (6,
                                            numericInput("PI_re", "Radio de drene (ft)", value = ""),
                                            numericInput("PI_rw", "Radio de pozo (ft)", value = ""),
                                            numericInput("PI_h", "Espesor de aceite (ft)", value = ""),
                                            numericInput("PI_S", "Daño", value = ""),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br()
                                            
                                    )#,
                   ),
                   

                   
                   conditionalPanel(condition = "input.PI_cor == 'Voguel'",
                                    column (6,
                                            
                                            
                                            numericInput("PI_py2","Presión de yacimiento (psi)", value = ""),
                                            numericInput("PI_Pb2","Presión de burbuja (psi)", value = "")
                                            
                                            
                                    ),
                                    
                                    column (6,
                                            numericInput("PI_Q", "Gasto de prueba (stb/day)", value = ""),
                                            numericInput("PI_Pwf", "Presión de fondo fluyente (psi)", value = ""),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br(),
                                            br()
                                            
                                    )#,
                   ),
                   
                   actionButton("calculatePI", "Calcular") , 
                   actionButton("LoadExPI","Cargar ejemplo") ,
                   actionButton("EquePI", "Información"),
                   
                   numericInput("PI_PI", "Indice de productividad (stb/day-psi)", value = "")
                   # column (12,
                   #         br(),
                   #         br(),
                   #         
                   #         
                   #         numericInput("PI_Qoc", "Critical flow rate (bbl/d)", value = "")
                   # )
                   
                   
      ),
      
      mainPanel(width = 9,
                
                
                column(5,
                     plotlyOutput("plot_PI", height = "70vh")
                    ),
                 
                 
                column(5,
                     rHandsontableOutput("tablePI")
                 ),
                 
                column(2,
                   numericInput("Data_points_IP", "Número de datos", value = 10),
                     
                     selectInput("Plot_Type_IP","Plot Type", 
                                 c("Line", "Points", "Line - Points"), selected = "Line - Points")
                )
 
      )
      
    )
      
    
)

# Define server 
server <- function(input, output) {

  vPI <- reactiveValues(PI = NULL, pwf = NULL)
  
  #Funciones
  
  #Darcy_PSS
  Darcy_PSS <- function(py,k,h,Bo,viso,re,rw,S, Pb = 0, data_P = 10){
    
    if(Pb == 0){
      J = (k*h)/(141.2*Bo*viso*(log(re/rw)-3/4+S))
      
      qomax <- J*py
      
      q <- seq(0,qomax, length.out = data_P)
      
      Pwf <- py -q/J
      
      Pwf_res <- data.frame(qo = q, Pwf = Pwf)
      
      Results <- list(PI = J, Pwf_res = Pwf_res)
    }else{
      
      J <- (k*h)/(141.2*Bo*viso*(log(re/rw)-3/4+S))
      #J
      
      qb <- (J)*(py-Pb)
      
      qv <- J*Pb/1.8
      
      pwf <- seq(0,py, length.out = data_P)
      
      qo <- ifelse(pwf >= Pb, J*(py-pwf), qb + qv*(1-0.2*(pwf/Pb)-0.8*(pwf/Pb)^2)) 
      
      Pwf_res <- data.frame(qo = qo, Pwf = pwf)
      
      Results <- list(PI = J, Pwf_res = Pwf_res)
      
    }
    
    return(Results)
  }
  
  
  #Funcion Vogel
  vogel <- function(py, Pb, q, pwf, data_P = 10){
    
    
    if(pwf > Pb){
      
      J <- q/(py-pwf)
      
    }else{
      
      J <- q/((py-Pb) + (Pb/1.8)*(1-0.2*(pwf/Pb)-0.8*(pwf/Pb)^2))
      
    }
    
    qb <- (J)*(py-Pb)
    
    qv <- J*Pb/1.8
    
    pwf <- seq(0,py, length.out = data_P)
    
    qo <- ifelse(pwf >= Pb, J*(py-pwf), qb + qv*(1-0.2*(pwf/Pb)-0.8*(pwf/Pb)^2)) 
    
    Pwf_res <- data.frame(qo = qo, Pwf = pwf)
    
    Results <- list(PI = J, Pwf_res = Pwf_res)
    
    return(Results)
  }
  

  #Boton calcular
  
  observeEvent(input$calculatePI, {
    
    if(input$PI_cor == "Darcy (PPS)"){
      
      py <- input$PI_py
      k <- input$PI_ko
      h <- input$PI_h
      Bo <- input$PI_bo
      viso <- input$PI_vo
      re <- input$PI_re
      rw <- input$PI_rw
      S <- input$PI_S
      #hp <- input$wC_hp
      
      if(input$Darcy_PPS_vogel == 1){
        Pb <- input$PI_Pb
        Results <-  Darcy_PSS(py,k,h,Bo,viso,re,rw,S, Pb, data_P =input$Data_points_IP)
      }else{
        Results <-  Darcy_PSS(py,k,h,Bo,viso,re,rw,S, data_P =input$Data_points_IP)
      }
      
      
      
      vPI$pwf <- Results
    }
    
    
    if(input$PI_cor == "Voguel"){
      
      py <- input$PI_py2
      Pb <- input$PI_Pb2
      q <- input$PI_Q
      pwf <- input$PI_Pwf
      
      Results <-  vogel(py,Pb, q, pwf, data_P =input$Data_points_IP)
      
      vPI$pwf <- Results
    }
    
    
    updateNumericInput(getDefaultReactiveDomain(),"PI_PI","Índice de productividad (stb/day-psi)", value = Results[[1]])
    
  })
  
  #Grafico IP
  output$plot_PI <- renderPlotly({
    
    if (!is.null(vPI$pwf)){
      data <- as.data.frame(vPI$pwf[2])
      #data2 <- vADL$dataADLdatos
      colnames(data) <- c("Qo", "Pwf")
      
      
      if(input$Plot_Type_IP == "Line") type_plot <- "lines"
      if(input$Plot_Type_IP == "Points") type_plot <- "markers"
      if(input$Plot_Type_IP == "Line - Points") type_plot <- "lines+markers"
      
      p <- plot_ly(data, x = ~Qo, y = ~Pwf, type = 'scatter', mode = type_plot, name = ~"PI") %>% 
        layout(xaxis = list(title = "Qo (STB/day)"), yaxis = list(title = "Pwf (Psi)"),
               legend = list(orientation = "h",xanchor = "center", x = 0.5, y = -0.25))
      
      
      
    }else{
      p <- ggplot() +
        xlab("Oil Rate (bbl/d)") +
        ylab("Pwf") +
        scale_x_continuous(limits = c(0, 1000)) +
        scale_y_continuous(limits = c(0, 500)) 
      p <- ggplotly(p)
    }
    
    p
    
  })
  
  
  
  #Load examples button
  observeEvent(input$LoadExPI,{
    
    if(input$PI_cor == "Darcy (PPS)"){ 
      updateCheckboxInput(getDefaultReactiveDomain(),"Darcy_PPS_vogel" ,"Vogel below Pb (bubble pressure)", value = TRUE)
      updateNumericInput(getDefaultReactiveDomain(),"PI_py","Reservoir pressure (psi)", value = 5651)
      updateNumericInput(getDefaultReactiveDomain(),"PI_ko","Permeability (md)", value = 8.2)
      updateNumericInput(getDefaultReactiveDomain(),"PI_vo", "Oil viscosity (cp)", value = 1.7)
      updateNumericInput(getDefaultReactiveDomain(),"PI_bo", "Oil fomation volume factor (bbl/STB)", value = 1.1)
      updateNumericInput(getDefaultReactiveDomain(),"PI_re", "Drainage radius (ft)", value = 2980)
      updateNumericInput(getDefaultReactiveDomain(),"PI_rw", "Wellbore radius (ft)", value = 0.328)
      updateNumericInput(getDefaultReactiveDomain(),"PI_h", "Oil column thickness (ft)", value = 53)
      updateNumericInput(getDefaultReactiveDomain(),"PI_S", "skin", value = 0)
      updateNumericInput(getDefaultReactiveDomain(),"PI_Pb", "Bubble point pressure (psi)", value = 5651)
      
    }
    
    
    if(input$PI_cor == "Voguel"){ 
      
      updateNumericInput(getDefaultReactiveDomain(),"PI_py2","Reservoir pressure (psi)", value = 5000)
      updateNumericInput(getDefaultReactiveDomain(),"PI_Pb2","Bubble point pressure (psi)", value = 3000)
      updateNumericInput(getDefaultReactiveDomain(),"PI_Q", "Test Rate (stb/day)", value = 900)
      updateNumericInput(getDefaultReactiveDomain(),"PI_Pwf", "FLowing Bottom-hole pressure (psi)", value = 2000)
      
    }
    
    
    
    
  })
  
  
  
  output$tablePI = renderRHandsontable({
    
    if (!is.null(vPI$pwf)){
      
      data <- as.data.frame(vPI$pwf[2])
      
      colnames(data) <- c("Q (stb/day)", "Pwf (psi)")
      
      rhandsontable(data, stretchH = "all")
    }
    
    
    
    
  })
  
  
  #Ayuda Indice de productividad
  Func_Info_PI <- function(failed = FALSE) {
    modalDialog(
      
      uiOutput("Info_PI"),
      size = "l"
      #includeHTML("Formulas_Rs.html")
    )
  } 
  
  observeEvent(input$EquePI,{
    
    
    showModal(Func_Info_PI())
  })
  
  output$Info_PI <- renderUI({
    
    withMathJax(includeMarkdown(file.path("formulas", "Info_PI.Rmd")))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
