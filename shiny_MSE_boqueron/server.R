
library(shiny)
library(shinydashboard)
#library(semantic.dashboard) # cambia el tema o color de la página web
library(DT)  # para incluir una tabla
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)
# generando data.frame para figuras

year  <- seq(2000,2030)
nyear <- length(year)

# stock

stock<-c("Anch_1","Anch_2","Anch_3")

Stocks <- rep(stock[1],nyear)

# scenarios

sce<-c("MO_1","MO_2","MO_3","HCR_1","HCR_2","HCR_3","HCR_4")

Sce   <- rep(sce[1],nyear)

#indicators

ind<-c("catch","rec","ssb","ft")


Catch <- sample(100:15000,nyear,replace=F)
           
Recl  <- sample(1000:50000,nyear,replace=F)

Ssb   <- sample(10000:35000,nyear,replace=F)

Ft    <- runif(nyear,0.01,3) %>% round(2)



Dat<-data.frame(year,Stocks,Sce,Catch,Recl,Ssb,Ft) %>% 
     melt(id=c("year","Stocks","Sce"))

yproy <- 2020

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    ######################################################################
    # HOME 
    ######################################################################
    
    output$home_proy<-renderText({
      
      "Management strategy evaluation framework to find the 
          optimum harvest control rule in a co-creation process with stakeholders: 
          Interactive shortcut approach with a general Ecosystem Toolbox for
          European anchovy."  
      
    })
    
    ###################################################################### 
    # ABOUT
    ######################################################################
    
    output$about_proy2<-renderText ({
      
      "En esta aplicación se muestran los resultados para 
           diferentes estrategias de gestión de las pesquerías xxx. 
           El objetivo de las simulaciones es probar si .... "
      
    })
    
    ######################################################################
    # SIMULACIONES
    ######################################################################
    
    #-------------------
    # Tab Time series
    #-------------------
    output$value <- renderPrint({ input$features2 })
    
    output$correlation_plot <- renderPlot({
      
      Dat %>% #filter(variable=="Catch") %>% 
      ggplot()+
        geom_line(aes(x=year,y=value,color=Sce))+
        geom_vline(aes(xintercept=yproy),color="grey",linetype="dashed",lwd=1)+
        ylab("")+xlab("Year")+xlim(input$slider)+
        #facet_wrap(~ variable + Stocks, ncol=2, scales="free_y")+
        facet_grid(variable ~ Stocks, scales="free")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
              title=element_text(size=16),
              text=element_text(size=16),legend.position="top")
      
      
      
    })
    
    #---------------------------------------------------------------------
    # Tab Area plot
    #---------------------------------------------------------------------
    output$value2 <- renderPrint({ input$features2 })
    
    output$correlation_plot2 <- renderPlot({
      plot(year,rep(0,nyear),ylim=c(0,6),
           xlab = "Year", ylab=indicadores[1],main=stock[1])
      
    })
    
    #---------------------------------------------------------------------
    # Tab Kobe plot
    #---------------------------------------------------------------------
    output$value3 <- renderPrint({ input$features2 })
    
    output$correlation_plot3 <- renderPlot({
      plot(seq(0,2),rep(0,length(seq(0,2))),ylim=c(0,2),
           xlab = "SSB/Bmsy", ylab="F/Fmsy",main=stock[1])
      
    })
  
  
})
