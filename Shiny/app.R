#----------- LIBRARIES --------------#
library(shiny)
#library(shinydashboard)
library(sf) #for map
library(ggplot2)
library(leaflet)

#----------- SET WORK DIRECTORY AND LOAD DATA --------------
#Species- used later for plot
Species<-c("Torsk","Skrubbe","Rødspætte","Sild","Brisling","Hvilling")


#----------- UI --------------#
ui <- fluidPage(

   titlePanel(""),  #titlePanel(""), #Gives extra space at top

  sidebarLayout(  # Sidebar layout with input and output definitions
    sidebarPanel(  
      fluidRow(
        column(3, #DTU LOGO
          img(src='dtu.svg', height = 50)
        )
      ),
      
       # Sidebar panel for inputs:
        h2("Vælg inputs"),
        # Input selection:
          selectInput("Areas", h4("Område"),
                      choices=list("Nordlige Kattegat", "Kattegat, øst for Læsø", "Nord for Øresund",
                                   "Øresund", "Storebælt", "Mecklenburg bugten", "Sydvest for Bornholm"),
                      selected="Nordlige Kattegat"
          ),
          
          uiOutput("Map"),
      
          fluidRow(column(6,
                          h4("Størrelsesgruppe"),
                          radioButtons("Sizes","",
                                       choices=list("Juvenile","Unge kønsmodne","Ældre kønsmodne"),
                                       selected="Juvenile"),
                          ),column(6,
                          h4("Periode"),
                          em("Vælg den periode der skal sammenlignes med perioden 2018-2022"),
                          radioButtons("periode", "",
                                       choices= list("2000-2005","2006-2011","2012-2017"),
                                       selected="2000-2005"))
         
                 )
          ),
    
    # Main panel for displaying plot outputs, Størrelsesgruppe-explanation and Background
    mainPanel( #Output: Tabset w/ plot, summary, and table
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",icon = icon("chart-column"),
                           h3(textOutput("choices")),
                           plotOutput("resultJ"),
                           textOutput("xaxis"),
                           ),
                  
                  tabPanel("Størrelsesgrupper",icon = icon("fish"),
                           includeHTML("App_content.Rhtml")
                           
                           ),
                  tabPanel("Baggrund",icon = icon("info"),
                           includeHTML("Baggrund.Rhtml"),

                           )
                  )#close tabsetPanel
              )#close mainPanel
          )#close SidebarLayout
) #close fluidPage


#----------- SERVER --------------#
server <- function(input, output) {
  
  # Header of plot:
  output$choices<-renderText({
    paste(input$Sizes,"fisk i", input$Areas, "sammenlignet med perioden",input$periode)
    })
  
  #X-axis:
  output$xaxis<-renderText({paste(
    "Ændring i procent fra", input$periode, "til 2018-2022 for", input$Sizes, " fisk i", input$Areas,
    ". Rød betyder at der har været en tilbagegang i arten og grøn, at der er sket en stigning. Grå betyder at der ikke er observeret en signifikant ændring 
    eller at den observerede ændring kan skyldes tilfældigheder og måle-usikkerhed. 
    Er der ikke vist noget resultat, har der ikke været tilstrækkeligt med data.
    Bemærk at Grå ikke nødvendigvis er et dårligt tegn, hvis forekomsten af fisk på et sundt niveau i begge perioder der sammenlignes., Omvendt kan grå også betyde at forekomsten er på et uhensigtsmæssigt lavt niveau i begge perider.")
    })

  #Map:
  #OBS: width=100% is important so that it follows the sidebarLayout size
  output$Map<-renderUI({
        if(input$Areas=="Nordlige Kattegat"){
          img(src='NordligeKattegat.png',width="100%")
        }else if (input$Areas=="Nord for Øresund"){
           img(src='NTZ.png',width="100%")
         }else if(input$Areas=="Kattegat, øst for Læsø"){
          img(src='KattegatMidt.png',width="100%")
        }else if(input$Areas=="Øresund"){
         img(src='Øresund.png',width="100%")
         }else if(input$Areas=="Storebælt"){
         img(src='Storebælt.png',width="100%")
         }else if(input$Areas=="Mecklenburg bugten"){
           img(src='Mecklenburg.png',width="100%")
         }else if(input$Areas=="Sydvest for Bornholm"){
                img(src='Østersøen.png',width="100%")
         }
      })
  
  #Result PLOT:
  output$resultJ<- renderPlot({
    #----- Nordlige Kattegat-----#
    if(input$Areas=="Nordlige Kattegat" && input$Sizes=="Juvenile") { #evt slet juvenile så alle str altid bliver vist
      newdat<- readRDS("../GAM/newdat/newdat_kattegatnordJ.rds")
    }else if(input$Areas=="Nordlige Kattegat" && input$Sizes=="Unge kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_kattegatnordM.rds")
    }else if(input$Areas=="Nordlige Kattegat" && input$Sizes=="Ældre kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_kattegatnordA.rds")
      #----- Kattegat, øst for Læsø-----#
    }else if(input$Areas=="Kattegat, øst for Læsø" && input$Sizes=="Juvenile"){
      newdat<- readRDS("../GAM/newdat/newdat_kattegatmidtJ.rds")
    }else if(input$Areas=="Kattegat, øst for Læsø" && input$Sizes=="Unge kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_kattegatmidtM.rds")
    }else if(input$Areas=="Kattegat, øst for Læsø" && input$Sizes=="Ældre kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_kattegatmidtA.rds")
      #----- Nord for Øresund-----#
    }else if(input$Areas=="Nord for Øresund" && input$Sizes=="Ældre kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_kattegatsydA.rds")
    }else if(input$Areas=="Nord for Øresund" && input$Sizes=="Unge kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_kattegatsydM.rds")
    }else if(input$Areas=="Nord for Øresund" && input$Sizes=="Juvenile"){
      newdat<- readRDS("../GAM/newdat/newdat_kattegatsydJ.rds")
      #----- Øresund-----#
    }else if(input$Areas=="Øresund" && input$Sizes=="Ældre kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_øresundA.rds")
    }else if(input$Areas=="Øresund" && input$Sizes=="Unge kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_øresundM.rds")
    }else if(input$Areas=="Øresund" && input$Sizes=="Juvenile"){
      newdat<- readRDS("../GAM/newdat/newdat_øresundJ.rds")
      #----- Storebælt-----#
    }else if(input$Areas=="Storebælt" && input$Sizes=="Juvenile"){
      newdat<- readRDS("../GAM/newdat/newdat_storebæltJ.rds")
    }else if(input$Areas=="Storebælt" && input$Sizes=="Unge kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_storebæltM.rds")
    }else if(input$Areas=="Storebælt" && input$Sizes=="Ældre kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_storebæltA.rds")
      #----- Mecklenburg bugten-----#
    }else if(input$Areas=="Mecklenburg bugten" && input$Sizes=="Juvenile"){
      newdat<- readRDS("../GAM/newdat/newdat_femerenJ.rds")
    }else if(input$Areas=="Mecklenburg bugten" && input$Sizes=="Unge kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_femerenM.rds")
    }else if(input$Areas=="Mecklenburg bugten" && input$Sizes=="Ældre kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_femerenA.rds")
      #----- Sydvest for Bornholm-----#
    }else if(input$Areas=="Sydvest for Bornholm" && input$Sizes=="Juvenile"){
      newdat<- readRDS("../GAM/newdat/newdat_østersøenJ.rds")
    }else if(input$Areas=="Sydvest for Bornholm" && input$Sizes=="Unge kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_østersøenM.rds")
    }else if(input$Areas=="Sydvest for Bornholm" && input$Sizes=="Ældre kønsmodne"){
      newdat<- readRDS("../GAM/newdat/newdat_østersøenA.rds")
    }
    
  
    ind.per1<- if(input$periode == "2000-2005"){
         as.numeric(newdat$period)==1
                                              }else if(input$periode=="2006-2011"){
                                                as.numeric(newdat$period)==2
                                              }else if(input$periode=="2012-2017"){
                                                as.numeric(newdat$period)==3} 
     ind.per2<-as.numeric(newdat$period) == 4
    
    z.score <- qnorm(0.95 + (1 - 0.95)/2)
    
    ## Select location (put in loop later)
    lows <- ups <- pcs <- sds <- pvals <- rep(NA, length(levels(newdat$loc)))
    for(i in 1:length(levels(newdat$loc))){
      ind.loc <- newdat$loc == levels(newdat$loc)[i]
      ## Ratio
      m.per1 <- newdat$cpue_pred[ind.per1 & ind.loc]
      m.per2 <- newdat$cpue_pred[ind.per2 & ind.loc]
      s.per1 <- newdat$cpue_sd[ind.per1 & ind.loc]
      s.per2 <- newdat$cpue_sd[ind.per2 & ind.loc]
      pcs[i] <- (m.per2/m.per1 - 1) * 100
      sds[i] <- abs(pcs[i]) * sqrt((s.per1/m.per1)^2 + (s.per2/m.per2)^2)
      lows[i] <- pcs[i] - z.score * sds[i]
      ups[i] <- pcs[i] + z.score * sds[i]
    }
    
    plot(1,1, ty = "n",
         xlim = c(-1e3, 1e3),
         ylim = c(0,7),
         xlab = "", ylab = "",
         yaxt = "n",
         frame.plot=FALSE#TRUE
    )
    
    abline(v = 0, col = "grey40", lty = 2, lwd = 1.5)
    abline(v = c(seq(-8e2,-1e2,1e2),
                 seq(1e2,1e3,1e2)), col = "grey40", lty = 3, lwd = 1)
    mtext("Procent ændring [%]",1,3)
    for(i in 1:length(pcs)){
      ##      if (sds[i]<1.75){
      if(pcs[i]>0 && lows[i]>0 && ups[i]>0){
        coli <- "#008835"
      }else if(pcs[i]<0 && lows[i]<0 && ups[i]<0){
        coli <- "#E83F48"
      }else{
        coli <- "grey"
      }
      suppressWarnings({
        arrows(lows[i], i,
               ups[i], i,
               code = 3, angle = 90, length = 0.1,
               col = coli,
               lwd = 2)
      })
      points(pcs[i], i,
             col = coli,
             lwd = 2,
             pch = 16, cex = 1.3)
      text(-9e2,i,
           lwd = 2,
           pch = 16, cex = 1.3, Species[i])
    }
    
  })
  
}

shinyApp(ui = ui, server = server)