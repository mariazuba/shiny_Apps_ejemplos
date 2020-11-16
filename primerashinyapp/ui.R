#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
    #################################################     
            sliderInput("bins", "Number of bins:",
                        min = 1,
                        max = 50, 
                        value = 30),  # distPlot
            actionButton("action", label="Acción"),      # ActionB
            hr(),                                        # crea un espacio bajo Action
            actionLink("alink",label="Click Me"),        # output2
            hr(),
            checkboxGroupInput("chkgrp",
                               label="Choose:",
                               choices=c(1,2,3,4)),
            hr(),
            checkboxInput("chkbx",
                          "Acepto:"),
            hr(),
            dateInput("InDate",
                      "Seleccione una fecha:",
                      value=Sys.Date(),
                      min=Sys.Date()-15,
                      max=Sys.Date()+3)
        ),
    #################################################     
     
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            textOutput("ActionB"),
            textOutput("output2"),
            textOutput("output3"),
            textOutput("output4"),
            textOutput("output5")
        )
    )
))
