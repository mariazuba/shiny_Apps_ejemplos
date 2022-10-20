
library(shiny)
library(shinydashboard)
#library(semantic.dashboard) # cambia el tema o color de la página web
library(DT)  # para incluir una tabla
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)


#--------------------------------------------------------------------------
# generando data.frame para figuras
#--------------------------------------------------------------------------
# years

year  <- seq(2000,2030)
nyear <- length(year)

# stock

stock  <- c("Anch_1","Anch_2","Anch_3")
nstock <- length(stock)


# scenarios

sce  <- c("MO_1","MO_2","MO_3")
nsce <- length(sce)


# indicators

ind  <- c("Catch","Recl","Ssb","Ft")
nind <- length(ind)

# datos simulados 

Years  <- rep(year,nstock) %>% rep(nsce)
Stocks <- gl(nstock,nyear,length=nstock*nyear,labels=stock) %>% rep(nsce)
Sce    <- gl(nsce, nyear*nstock,length=nstock*nyear*nsce,labels=sce)


Catch  <- c(sample(100:15000,nyear,replace=F), # "Anch_1" - "MO_1"
            sample(100:1500,nyear,replace=F),   # "Anch_2" - "MO_1"
            sample(100:35000,nyear,replace=F),  # "Anch_3" - "MO_1"
            sample(100:15000*0.5,nyear,replace=F),  # "Anch_1" - "MO_2"
            sample(100:1500*0.5,nyear,replace=F),   # "Anch_2" - "MO_2"
            sample(100:35000*0.5,nyear,replace=F),  # "Anch_3" - "MO_2"
            sample(100:15000*1.5,nyear,replace=F),  # "Anch_1" - "MO_3"
            sample(100:1500*1.5,nyear,replace=F),   # "Anch_2" - "MO_3"
            sample(100:35000*1.5,nyear,replace=F)   # "Anch_3" - "MO_3"
)
Recl  <- c(sample(1000:50000,nyear,replace=F), # "Anch_1" - "MO_1"
           sample(1000:25000,nyear,replace=F), # "Anch_2" - "MO_1"
           sample(1000:80000,nyear,replace=F), # "Anch_3" - "MO_1"
           sample(1000:50000*0.5,nyear,replace=F), # "Anch_1" - "MO_2"
           sample(1000:25000*0.5,nyear,replace=F), # "Anch_2" - "MO_2"
           sample(1000:80000*0.5,nyear,replace=F), # "Anch_3" - "MO_2"
           sample(1000:50000*1.5,nyear,replace=F), # "Anch_1" - "MO_3"
           sample(1000:25000*1.5,nyear,replace=F), # "Anch_2" - "MO_3"
           sample(1000:80000*1.5,nyear,replace=F)  # "Anch_3" - "MO_3"
)
Ssb   <- c(sample(10000:35000,nyear,replace=F), # "Anch_1" - "MO_1"
           sample(10000:15000,nyear,replace=F), # "Anch_2" - "MO_1"
           sample(10000:85000,nyear,replace=F), # "Anch_3" - "MO_1"
           sample(10000:35000*0.5,nyear,replace=F), # "Anch_1" - "MO_2"
           sample(10000:15000*0.5,nyear,replace=F), # "Anch_2" - "MO_2"
           sample(10000:85000*0.5,nyear,replace=F), # "Anch_3" - "MO_2"
           sample(10000:35000*1.5,nyear,replace=F), # "Anch_1" - "MO_3"
           sample(10000:15000*1.5,nyear,replace=F), # "Anch_2" - "MO_3"
           sample(10000:85000*1.5,nyear,replace=F)  # "Anch_3" - "MO_3"
)
Ft    <- c(runif(nyear,0.01,3), # "Anch_1" - "MO_1"
           runif(nyear,0.01,5), # "Anch_2" - "MO_1"
           runif(nyear,0.01,1), # "Anch_3" - "MO_1"
           runif(nyear,0.01,3*0.5), # "Anch_1" - "MO_2"
           runif(nyear,0.01,5*0.5), # "Anch_2" - "MO_2"
           runif(nyear,0.01,1*0.5), # "Anch_3" - "MO_2"
           runif(nyear,0.01,3*1.5), # "Anch_1" - "MO_3"
           runif(nyear,0.01,5*1.5), # "Anch_2" - "MO_3"
           runif(nyear,0.01,1*1.5)  # "Anch_3" - "MO_3"
) %>% round(2)


Dat<-data.frame(Years,Stocks,Sce,Catch,Recl,Ssb,Ft) %>% 
     melt(id=c("Years","Stocks","Sce"))

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
    
    output$timeserie <- renderPlot({
      
      Dat %>% 
        filter(Stocks %in% input$features2 & 
               variable %in% input$features3 & Sce %in% input$features4)  %>% 
      ggplot(aes(x=Years,y=value,group=Sce,col=Sce))+
        geom_line()+
        geom_vline(aes(xintercept=yproy),color="grey",linetype="dashed",lwd=1)+
        ylab("")+xlab("Year")+xlim(input$slider)+
        #facet_wrap(~ Stocks + variable , ncol=3, scales="free_y")+
        facet_grid(variable ~ Stocks, scales="free")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
              title=element_text(size=16),
              text=element_text(size=16),legend.position="top")
      
      
      
    })
    
    #---------------------------------------------------------------------
    # Tab Area plot
    #---------------------------------------------------------------------
    
    
    output$areaplot <- renderPlot({
      
      Dat %>% 
        filter(Stocks %in% input$features2 & 
                 variable %in% input$features3 & Sce %in% input$features4)  %>% 
        ggplot(aes(x=Years,y=value,group=Stocks,fill=Stocks))+
        geom_area()+
        geom_vline(aes(xintercept=yproy),color="grey",linetype="dashed",lwd=1)+
        ylab("")+xlab("Year")+xlim(input$slider)+
        #facet_wrap(~ Stocks + variable , ncol=3, scales="free_y")+
        facet_grid(variable ~ Sce, scales="free")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
              title=element_text(size=16),
              text=element_text(size=16),legend.position="top")
      
      
    })
    
    #---------------------------------------------------------------------
    # Tab Kobe plot
    #---------------------------------------------------------------------
   
    
    output$kobe_plot <- renderPlot({
      
      SSB <- Dat %>% 
             filter(variable == "Ssb" & Stocks %in% input$features2 & Sce %in% input$features4) 
      
      SSB_bmsy <- SSB$value/mean(SSB$value)
      
      FT <- Dat %>% filter(variable == "Ft" & Stocks %in% input$features2 & Sce %in% input$features4) 
      FT_fmsy <-  FT$value/median(FT$value)
        
        
        date<-data.frame(Stocks,Sce,ssb=SSB_bmsy,ft=FT_fmsy)
        date %>% 
          
        ggplot(aes(x=ssb,y=ft))+
        geom_rect(aes(xmin =0, xmax = 1, ymin = 0, ymax = 1), 
                  fill = "yellow", alpha = 0.2) +
        geom_rect(aes(xmin =0, xmax = 1, ymin = 1, ymax = 3), 
                    fill = "red", alpha = 0.2) +
        geom_rect(aes(xmin =1, xmax = 3, ymin = 1, ymax = 3), 
                    fill = "yellow", alpha = 0.2) +
        geom_rect(aes(xmin =1, xmax = 3, ymin = 0, ymax = 1), 
                    fill = "green", alpha = 0.2) +
        geom_line()+
        geom_point()+
        ylab("F/Fmsy")+xlab("SSB/Bmsy")+xlim(0,3)+ylim(0,3)+
        #facet_wrap(~Stocks + Sce , ncol=3, scales="free_y")+
        facet_grid(Stocks ~ Sce, scales="free")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
              title=element_text(size=16),
              text=element_text(size=16),legend.position="top")
      
      
      
    })
  
  
})
