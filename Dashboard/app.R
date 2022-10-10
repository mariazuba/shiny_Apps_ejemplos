

library(shiny)
library(shinydashboard)
#library(semantic.dashboard) # cambia el tema o color de la página web
library(DT)  # para incluir una tabla


data1<-seq(1,10,1)
data2<-seq(15,25,1)
stock<-c("Anchovy 1","Anchovy 2","Anchovy 3")
indicadores<-c("catch","landings","rec","ssb")
strategias<-c("Str_1","Str_2","Str_3","Str_4") 
  
  
title <- tags$a(tags$img(src="LOGO_2SN_recortado.png",height='45',width ='90'))

ui <- dashboardPage(skin = "black", 
      
      dashboardHeader(title = title,titleWidth = 250),
      
      dashboardSidebar(
           sidebarMenu(
               menuItem("Home",tabName = "home",icon=icon("fish")),
      
               menuItem("About",tabName = "about",icon=icon("fish")),
            
               menuItem("Simulaciones",tabName = "iris",icon=icon("fish"),startExpanded = F,
                      menuSubItem("Stocks",tabName = "iris2"),
                      menuSubItem("Fleets",tabName = "iris3"),
                      menuSubItem("Fleets and stocks",tabName = "iris4"),
                      menuSubItem("Summary",tabName = "iris5"),
                      menuSubItem("Advice",tabName = "iris6"),selected = T)
                      ) # cierra sidebarMenu()
                      ), # cierra dashboardSidebar()
      
      dashboardBody(
        
        tabItems(
            tabItem(tabName="home",
                    fluidRow(
                      imageOutput("logo_proy")), # IMAGE TOO BIG AND BOX CANNOT COMPLETELY CONTAIN IT
            ),
            
            tabItem(tabName="about",
                        h2("Math4fish"),
                        textOutput("about_proy"),
                        h2("Caso estudio"),
                        textOutput("about_proy2")),
            
            tabItem(tabName="iris2",
                    tabBox(id="t1",width=12,height="600px",
                           tabPanel("Time series","probando texto",
                                plotOutput("correlation_plot"),width=4,
                                    sliderInput("slider",h4("Years:"),min=2000,max=2030,value=c(2000,2030)),
                                
                                    selectizeInput("features2",h4("Stock:"),
                                                  levels(as.factor(stock)),
                                                  selected=unique(stock)[1],
                                                  multiple=T,
                                                  options=list(plugins=list("remove_button","drag_drop"))),
                                
                                    selectizeInput("features3",h4("Indicators:"),
                                                  levels(as.factor(indicadores)),
                                                  selected=unique(indicadores)[1],
                                                  multiple=T,
                                                  options=list(plugins=list("remove_button","drag_drop"))),
                                
                                    selectizeInput("features4",h4("Scenarios:"),
                                                  levels(as.factor(strategias)),
                                                  selected=unique(strategias)[1],
                                                  multiple=T,
                                                  options=list(plugins=list("remove_button","drag_drop")))),
                           
                           tabPanel("Area plot","probando texto 2"),
                           tabPanel("Kobeplot","probando texto 3"),
                           tabPanel("Spider","probando texto 4"))),
           
            tabItem(tabName="iris3",
                    tabBox(id="t2",width=12,
                           tabPanel("Time series","probando texto 1"),
                           tabPanel("Area plot","probando texto 2"),
                           tabPanel("Kobeplot","probando texto 3"),
                           tabPanel("Spider","probando texto 4"))),
            
            tabItem(tabName="iris4",
                    tabBox(id="t3",width=12,
                           tabPanel("Time series","probando texto 1"),
                           tabPanel("Area plot","probando texto 2"),
                           tabPanel("Kobeplot","probando texto 3"),
                           tabPanel("Spider","probando texto 4"))),
            
            tabItem(tabName="iris5",
                    tabBox(id="t4",width=12,
                           tabPanel("Time series","First tab content"),
                           tabPanel("Area plot","Tab content 2"),
                           tabPanel("Kobeplot", "xxxx"),
                           tabPanel("Spider","XXXXs"))),
            
            tabItem(tabName="iris6",
                    tabBox(id="t5",width=12,
                           tabPanel("Time series","probando texto 1"),
                           tabPanel("Area plot","probando texto 2"),
                           tabPanel("Kobeplot","probando texto 3"),
                           tabPanel("Spider","probando texto 4")))
            
     ) # cierra tabItems
   ) # cierra dashboardBody
 ) # cierra dashboardPage


##################################################################################################
# SERVER
##################################################################################################
server <- function(input, output) {
          
         # HOME 
          output$logo_proy <- renderImage({
                  filename <- normalizePath(file.path('www/LOGO_2SN_recortado.png'))
                              list(src = filename,width="100%",
                              align = "center",style="height: 250px")},
                              deleteFile = FALSE)
          
         # ABOUT
          output$about_proy<-renderText({"Management strategy evaluation framework to find the 
            optimum harvest control rule in a co-creation process with stakeholders: Interactive
            shortcut approach with a general Ecosystem Toolbox for European anchovy. Para más información visite la página web: https://math4fish.ieo.csic.es"  })
          
          
          output$about_proy2<-renderText ({"En esta aplicación se muestran los resultados para 
            diferentes estrategias de gestión de las pesquerías xxx. El objetivo de las simulaciones es probar si .... "})
          
         # SIMULACIONES
          output$value <- renderPrint({ input$features2 })
          
          output$correlation_plot <- renderPlot({
          plot(seq(2000,2030),rep(0,length(seq(2000,2030))),ylim=c(0,6),
          xlab = "Year", ylab=indicadores[1],main=stock[1])
          })
 

 
}

# Run the application 
shinyApp(ui = ui, server = server)
