

library(shiny)
library(shinydashboard)
#library(semantic.dashboard) # cambia el tema o color de la página web
library(DT)  # para incluir una tabla
library(ggplot2)

# indicadores 
year  <- seq(2000,2030)
nyear <- length(year)

stock<-c("Anch_1","Anch_2","Anch_3")
indicadores<-c("Catch","Recl","Ssb","Ft")
strategias<-c("MO_1","MO_2","MO_3")


title <- tags$a(tags$img(src="LOGO_2SN_recortado.png",height='40',width ='95'))

# Define UI for application that draws a histogram
shinyUI(
  
dashboardPage(
  skin = "black", 
  dashboardHeader(
    title = title,titleWidth = 250
    ), # cierra dashboardHeader
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",tabName = "home",icon=icon("home")
      ), # cierra menuItem
      menuItem("About",tabName = "about",icon=icon("fish")
      ), # cierra menuItem
      menuItem("Simulaciones",tabName = "iris",icon=icon("fish"),startExpanded = F,
         menuSubItem("Modelos Operativos",tabName = "iris2"),
         menuSubItem("Estrategias de gestión",tabName = "iris5"),
         menuSubItem("Advice",tabName = "iris6")
         ,selected = T
      ) # cierra menuItem
    ) # cierra sidebarMenu()
  ), # cierra dashboardSidebar()
  dashboardBody(
    tabItems(
      #list(src = filename,width="100%",
      #            align = "center",style="height: 350px")},
      #            deleteFile = FALSE)
 ########################################################################
 # HOME
 ########################################################################
    tabItem(
      tabName="home",
      fluidRow(
       box(
        align="center",
        HTML("<center><img src='stakeholders.png' width=80% height=80%></center>"),
        HTML("</h2>"),
        h1(textOutput("home_proy")),
        HTML("</h2>"),
        HTML("</h2>"),
        HTML("<center><img src='https://math4fish.ieo.csic.es/wp-content/uploads/2022/09/Banners_Convenio-2048x357.png' width=80% height=80%></center>"),
        width=12,
        )  # cierra box
      ), # cierra fluixRow
    ), # cierra tabItem
 ########################################################################
 # ABOUT
 ########################################################################
    tabItem(
      tabName="about",
      HTML("<center><img src='https://math4fish.ieo.csic.es/wp-content/uploads/2022/08/F4.jpeg' width=80% height=80%></center>"),
      h2("Caso estudio"),
      h4(textOutput("about_proy2"))
      ), # cierra tabItem
 
 ########################################################################
 # SIMULATION
 ########################################################################
    tabItem(
      tabName="iris2",
      tabBox(id="t1",
             width=12,
             height="600px",
#-----------------
# TIME SERIES   
#-----------------
      tabPanel(h4("Time series"),
      h4("Time series plots show the evolution of indicators over time. 
      The indicator time series in each scenario is drawn with a different color.
      Confidence intervals can also be added marking the corresponding option. 
      Several indicators and stock, fleets or metiers (depending on the case) 
      can be selected simultaneously and they are plotted in different facets"),
        fluidRow(
          #--------------------
          # PANEL DE SELECCION
          #--------------------
          box( 
           sliderInput("slider",
                       h4("Years:"),
                       min=min(year),
                       max=max(year),
                       value=c(min(year),max(year))
                       ),
           selectizeInput("features2",
                          h4("Stock:"),
                          levels(as.factor(stock)),
                          selected=unique(stock)[1],
                          multiple=T,
                          options=list(plugins=list("remove_button","drag_drop"))
                          ),
           selectizeInput("features3",
                          h4("Indicators:"),
                          levels(as.factor(indicadores)),
                          selected=unique(indicadores)[1],
                          multiple=T,
                          options=list(plugins=list("remove_button","drag_drop"))
                          ),
           selectizeInput("features4",
                          h4("Scenarios:"),
                          levels(as.factor(strategias)),
                          selected=unique(strategias)[1],
                          multiple=T,
                          options=list(plugins=list("remove_button","drag_drop"))
                          ),             
            width=4,
            height="600px"
            ), #cierra box
          #--------------------
          # FIGURA
          #--------------------
          box( 
             plotOutput("timeserie",
                        height = 650),
             width=8,
             height="600px"
             ) #cierra box
           ) # cierra fluixRow
         ), # cierra tabPanel

 #-----------------
 # AREA PLOT   
 #-----------------
      tabPanel(h4("Area plot"),
        h4("Area plots show the composition, in median, of an specific indicator 
        (catch, biomass...) over time. Each coloured area corresponds with one 
        stock, fleet or metier (depending on the case). When several indicators 
        are selected simultaneously they are plotted in different facets."),
        fluidRow(
          #--------------------
          # PANEL DE SELECCION
          #--------------------
          box(
           sliderInput("slider",
                      h4("Years:"),
                      min=min(year),
                      max=max(year),
                      value=c(min(year),max(year))),
           selectizeInput("features2",
                         h4("Stock:"),
                         levels(as.factor(stock)),
                         selected=unique(stock)[1],
                         multiple=T,
                         options=list(plugins=list("remove_button","drag_drop"))
                         ),
           selectizeInput("features3",
                         h4("Indicators:"),
                         levels(as.factor(indicadores)),
                         selected=unique(indicadores)[1],
                         multiple=T,
                         options=list(plugins=list("remove_button","drag_drop"))
                         ),
           selectizeInput("features4",
                         h4("Scenarios:"),
                         levels(as.factor(strategias)),
                         selected=unique(strategias)[1],
                         multiple=T,
                         options=list(plugins=list("remove_button","drag_drop"))
                         ),
                         width=4
             ), # cierra box
          
          #--------------------
          # FIGURA
          #--------------------         
          box(
           plotOutput("areaplot"),
           width=8
            ) # cierra box
          ) # cierra fluixRow
        ), # cierra tabPanel

#-----------------
# KOBE PLOT
#-----------------
      tabPanel(h4("Kobeplot"),
        h4("Kobe plots provide the trajectory of SSB and F pairs (in median) over time.
        They divide the plot area in different quadrants defined by the ratio between SSB and F
        and their corresponding MSY values. The green quadrant represents the area where the 
        stock is sustainably exploited (SSB>Bmsy and F Bmsy and F>Fmsy), and the red the
        area where the stock is over-exploited and over-fished (SSBFmsy)."),
        
        fluidRow(
          #--------------------
          # PANEL DE SELECCION
          #--------------------
          box(
             sliderInput("slider",
                         h4("Years:"),
                         min=min(year),
                         max=max(year),
                         value=c(min(year),max(year))
                         ),
             selectizeInput("features2",
                            h4("Stock:"),
                            levels(as.factor(stock)),
                            selected=unique(stock)[1],
                            multiple=T,
                            options=list(plugins=list("remove_button","drag_drop"))
                            ),
             selectizeInput("features4",
                            h4("Scenarios:"),
                            levels(as.factor(strategias)),
                            selected=unique(strategias)[1],
                            multiple=T,
                            options=list(plugins=list("remove_button","drag_drop"))
                            ),
              width=4
              ), #cierra box
          #--------------------
          # FIGURA
          #--------------------
          box(
             plotOutput("kobe_plot"),
             width=8
             ) # cierra box
           ) # cierra fluixRow
         ), # cierra tabPanel
                                 
      tabPanel(h4("Spider"),
                                           h4("Spider plots allow to compare the (median) value of an indicator along a big set of 
                                    scenarios for a certain year. The scenarios correspond with the edges of the web. 
                                    The value is standardized comparing the value in one year with a base year (year option) 
                                    or with a base scenario (scenario option). Thus, the dashed black line correspond with 
                                    the unit circle, lines outside the circle represent values higher than in the base year 
                                    or scenario and those inside the circle represent lower values. The variables used to draw 
                                    lines and facets can be exchange, so lines can correspond with stock or fleets (depending 
                                    on the case) and facets with indicators or the other way around.")
                                   ))),
                          
                          tabItem(tabName="iris3",
                                  tabBox(id="t2",width=12,
                                         tabPanel(
                                           "Time series",
                                           h5("xxx")
                                         ),
                                         tabPanel("Area plot",
                                                  "xxx"),
                                         
                                         tabPanel("Kobeplot",
                                                  "probando texto 3"),
                                         
                                         tabPanel("Spider",
                                                  "probando texto 4"),
                                  )),
                          
                          tabItem(tabName="iris4",
                                  tabBox(id="t3",width=12,
                                         tabPanel("Time series","probando texto 1"),
                                         tabPanel("Area plot","probando texto 2"),
                                         tabPanel("Kobeplot","probando texto 3"),
                                         tabPanel("Spider","probando texto 4")))
                          
                        ) # cierra tabItems
                      ) # cierra dashboardBody
) # cierra dashboardPage
) 