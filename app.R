
library(shiny)
library(ggplot2)
library(extremeStat)
library(lmomco)
library(dplyr)
library(lubridate)

# distribuciones estadisticas posibles

distribuciones<-c("wak", "kap", "wei", "gev", "pe3", "gno", "nor", "gpa", "glo", 
                  "ray", "rice", "lap", "revgum", "gum", "gam", "exp", "ln3") 


# Define UI for application that fit an statistic distribution for a datasets
ui <- fluidPage(fileInput("upload", "busque archivo de depositos", accept = ".txt"),
                dateRangeInput("rangoobs", "Ingrese rango de fechas para ajustar distribucion"), 
                selectInput("distribucion", "Seleccione distribucion ajustada", distribuciones),
                numericInput("confianza", "Seleccione nivel de confianza", value = 99, min = 95, max = 100),
                plotOutput("hist"),
                verbatimTextOutput("ktest"),
                verbatimTextOutput("parametrosdist"),
                tableOutput("contents"))


# Define server logic required to draw a histogram
server <- function(input, output) {
  data<-reactive({
    file<-input$upload
    ext<-tools::file_ext(file$datapath)
    req(file)  
    validate(need(ext == "txt", "Please upload a txt file"))
    read.csv(file$datapath, header = TRUE, sep="\t", dec = ",")
  })
  
## Mi duda fundamentalmente es (1) si necesito usar un reactive() para c/u de las variables dinamica a continuacion, como de hecho lo hice 
### o (2) si puedo incluir todas dentro de un mismo reactive y luego invocarlos en los Render en forma de objetos de una lista

##filtra un periodo de fechas para las cuales tomara los datos para ajustar una distribucion

  x1vista <- reactive({data() %>%  filter(dmy(Fecha)> as.POSIXct(input$rangoobs[1]) & 
                                                                 dmy(Fecha)< as.POSIXct(input$rangoobs[2]))})
  
###Calcula los retornos geometricos de los valores mediante la fomula:  log neperiano (valor dia n /valor dia n-1)

  rvista<-reactive(log(x1vista()[-1,2]/x1vista()[-nrow(x1vista()),2]))
  volatilidad<-reactive(sd(rvista())) ###Calcula la volatilidad (desviacion estandar)
  
  dpv <- reactive(distLfit(rvista())) ##dpv es un vector de distribuciones ajustadas
  
  x2 <- reactive( rlmomco(length(rvista()),dpv()[["parameter"]][[input$distribucion]])) ###genero valores aleatorios de la distribcion ajustada y seleccionada
  
  binwidth = 0.1
  
  valorcritico<-reactive(quantile(rvista(),1-(input$confianza/100)))  ##calculo el valor critico (cuantil) de la distribucion simulada

  VaR<-reactive({data.frame(fecha=x1vista()[1,1], Saldo=x1vista()[1,2], volatilidad()*100,
                     valorcritico(),
                     VaRPorcentaje=(volatilidad()*valorcritico())*100, 
                     VaRBs=volatilidad()*valorcritico()*x1vista()[1,2])} )
                     
  
  output$hist <- renderPlot({
    
    ggplot(data.frame(
      x = c(rvista(), x2()),
      g = c(rep("real", length(rvista())), rep("ajustada", length(x2())))
    ), aes(x, colour = g)) +  geom_freqpoly(binwidth = binwidth, size = 1)
  })
  
  output$ktest <- renderPrint({
      ks.test(rvista(), x2()) })
  
  output$parametrosdist <- renderPrint({
    dpv()[["parameter"]][[input$distribucion]] })
  
  output$contents <- renderTable(VaR(), digits=4)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
