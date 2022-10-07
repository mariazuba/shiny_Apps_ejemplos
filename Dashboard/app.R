

library(shiny)
library(shinydashboard)
#library(semantic.dashboard) # cambia el tema o color de la página web
library(DT)  # para incluir una tabla


data1<-seq(1,10,1)
data2<-seq(15,25,1)

ui <- dashboardPage(
    dashboardHeader(title = "CBA sardina común"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Criterios",tabName = "iris",icon=icon("fish")),
                      menuSubItem("Escenarios Reclutamiento",tabName = "iris2"),
            menuItem("Rpromedio bajo 1991-2007",tabName = "cars",icon=icon("fish"))
                
      )
    ),
    dashboardBody(
        tabItems(
            
        tabItem("iris"),
 
                tabItem("iris2",    
                       box(plotOutput("correlation_plot"),width=8),
                       box(selectInput("features","Características:",
                                c("Sepal.Width",
                                  "Petal.Length",
                                  "Petal.Width")),
                           width = 4)
                ),
        
        tabItem("cars",
            fluidPage(
            h1("CBA inicial"),
            dataTableOutput("carstable"),
            h1("Primera Revisión CBA")
            ) # cierra fluidPage
       ) # cierra tabItem cars
     ) # cierra tabItem iris 2
   ) # cierra dashboardBody
 ) # cierra dashboardPage



server <- function(input, output) {
 output$correlation_plot <- renderPlot({
     plot(iris$Sepal.Length,iris[[input$features]],
          xlab = "Sepal length", ylab="Feature")
 })
 
 percentil <- rep(0.2,7)
 CBA<-c(215013,214856,214709,214560,214413,214265,214117)
 desc1<-c(212863,212707,212561,212415,212269,212122,211976)
 desc2<-c(210713,210559,210414,210269,210125,209979,209835)
 desc3<-c(208563,208410,208267,208123,207981,207837,207694)
 
 tabla1<-data.frame(percentil,CBA,desc1,desc2,desc3)
 output$carstable <- renderDataTable(tabla1)
 
}

# Run the application 
shinyApp(ui = ui, server = server)
