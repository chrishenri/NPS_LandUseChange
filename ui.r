#--------------------------------------------------------
#Genereate concentration statistics for NPS contamination
#--------------------------------------------------------

library(shiny)
library(plotly)

# Source the switch button function
# source("./Rsource/SwitchButton.R")

#--------------------------------------------------------
#import original data (c_i(t))


shinyUI(navbarPage(#theme = "bootstrap.css",             
  "NPS_Stochastic_Assessment",
  theme = "flatly_button.css",#shinythemes::shinytheme("flatly"),
  
  #---------------------------
  #Panel#2: Background
  #---------------------------
  tabPanel(title = "Background",
           includeMarkdown("background.Rmd")
  ),
  
  #---------------------------
  #Panel#1: Plot c
  #---------------------------
  tabPanel(title = "Concentration Statistics",
           sidebarLayout(
             sidebarPanel(
                          sliderInput("poro", h4("Effective Porosity [-]"), min=0, max=1, value = 0.3),
                          br(), br(),
                          
                          selectInput("well_dpth", h4("Well depth (screen top) [m]"),
                                      choices=c(50, 100, 150),
                                      selected = 100),
                          h5("(Extraction rate fixed to 3000 m3/d)"),
                          br(), br(),
                          
                          numericInput("MCL", h4("Control Concentration [mg/L]"), value = 1),
                          br(), br(),
                          
                          uiOutput("InputsPerc"),
                          
                          uiOutput("InputsHisto"),
                          
                          br(), br(),
                          actionButton("instruPanel1", "Instructions")
                          #downloadButton("download_button", label = "Generate Input File")
                        ),
             
             mainPanel(img(src='logo_UCD.jpg', width = "150px", align = "right"),
                       br(),br(),
                       tabsetPanel(id = "tabs",
                         tabPanel("Exceed. Prob.", value = "A",
                                  fluidPage(
                           fluidRow(
                             column(width = 3,
                                    br(),
                                    plotlyOutput("plotC", width = "450px", height = "400px")
                             ),
                             column(width = 4, offset = 4,
                                    br(),
                                    h4("Quick Analysis"),
                                    hr(),
                                    
                                    textOutput("AnaPerc10"),
                                    textOutput("AnaPerc10b"),
                                    tags$head(tags$style("#AnaPerc10b{color:rgb(64, 142, 202);font-size: 16px;}"
                                              )),
                                    
                                    br(),
                                    textOutput("AnaPerc50"),
                                    textOutput("AnaPerc50b"),
                                    tags$head(tags$style("#AnaPerc50b{color:rgb(255, 198, 38);font-size: 16px;}"
                                    )),
                                    
                                    br(),
                                    textOutput("AnaPerc90"),
                                    textOutput("AnaPerc90b"),
                                    tags$head(tags$style("#AnaPerc90b{color:rgb(166, 212, 135);font-size: 16px;}"
                                    ))
                             )
                         ))),

                         tabPanel("Exceed. Prob. Matrix", value = "B",
                                  br(),
                                  plotlyOutput("plotProbC", width = "500px", height = "450px")
                         ),
                       
                         tabPanel("Histogram", value = "C",
                                br(),
                                plotlyOutput("plotHistoC", width = "650px", height = "450px")
                       )
                       
                         )
                       )
             )
           ),

  
  #---------------------------
  #Panel#1: LUC
  #---------------------------
  tabPanel(title = "Land Use Change",
           sidebarLayout(
             sidebarPanel(h4("Initial land use"),
                          h5("Proportion of the land covered by each crop"),
                          div(style="display:inline-block; width: 150px",sliderInput("almond_propIni", h6("Almond"), min=0, max=1, value = 0.24, step = 0.01)), 
                          div(style="display:inline-block; width: 150px",sliderInput("citrus_propIni", h6("Citrus"), min=0, max=1, value = 0.24, step = 0.01)),
                          div(style="display:inline-block; width: 150px",sliderInput("corn_propIni", h6("Corn"), min=0, max=1, value = 0.18, step = 0.01)), 
                          br(),
                          
                          div(style="display:inline-block; width: 150px",sliderInput("cotton_propIni", h6("Cotton"), min=0, max=1, value = 0.12, step = 0.01)),
                          div(style="display:inline-block; width: 150px",sliderInput("grain_propIni", h6("Grain"), min=0, max=1, value = 0.12, step = 0.01)),
                          div(style="display:inline-block; width: 150px",sliderInput("grape_propIni", h6("Grape"), min=0, max=1, value = 0.10, step = 0.01)),
                          br(),
                          
                          h5("Nitrate mass flux leaking into the aquifer"),
                          selectInput("mf_unit_INI", h6("Units"),
                                      choices=c("kg N/ha/y", "lb N/acre/y"),
                                      selected = "kg N/ha/y",
                                      width='50%'),
                          uiOutput("mfINI"),
                          # div(style="display:inline-block; width: 150px",numericInput("almond_cIni", h6("Almond"), value = 0.0186, step = 0.001)), 
                          # div(style="display:inline-block; width: 150px",numericInput("citrus_cIni", h6("Citrus"), value = 0.0153, step = 0.001)),
                          # div(style="display:inline-block; width: 150px",numericInput("corn_cIni", h6("Corn"), value = 0.0118, step = 0.001)), 
                          # br(),
                          # 
                          # div(style="display:inline-block; width: 150px",numericInput("cotton_cIni", h6("Cotton"), value = 0.0117, step = 0.001)), 
                          # div(style="display:inline-block; width: 150px",numericInput("grain_cIni", h6("Grain"), value = 0.0106, step = 0.001)),
                          # div(style="display:inline-block; width: 150px",numericInput("grape_cIni", h6("Grape"), value = 0.0029, step = 0.001)), 
                          # br(), br(),
                          
                          #------------------
                          h4("New land use"),
                          h5("Proportion of the land covered by each crop"),
                          div(style="display:inline-block; width: 150px",sliderInput("almond_propLUC", h6("Almond"), min=0, max=1, value = 0.24, step = 0.01)), 
                          div(style="display:inline-block; width: 150px",sliderInput("citrus_propLUC", h6("Citrus"), min=0, max=1, value = 0.24, step = 0.01)),
                          div(style="display:inline-block; width: 150px",sliderInput("corn_propLUC", h6("Corn"), min=0, max=1, value = 0.18, step = 0.01)), 
                          br(),
                          
                          div(style="display:inline-block; width: 150px",sliderInput("cotton_propLUC", h6("Cotton"), min=0, max=1, value = 0.12, step = 0.01)),
                          div(style="display:inline-block; width: 150px",sliderInput("grain_propLUC", h6("Grain"), min=0, max=1, value = 0.12, step = 0.01)),
                          div(style="display:inline-block; width: 150px",sliderInput("grape_propLUC", h6("Grape"), min=0, max=1, value = 0.10, step = 0.01)),
                          br(),
                          
                          h5("Nitrate mass flux leaking into the aquifer"),
                          selectInput("mf_unit_LUC", h6("Units"),
                                      choices=c("kg N/ha/y", "lb N/acre/y"),
                                      selected = "kg N/ha/y",
                                      width='50%'),
                          uiOutput("mfLUC"),
                          # div(style="display:inline-block; width: 150px",numericInput("almond_cLUC", h6("Almond"), min=0, value = 0.0186, step = 0.001)), 
                          # div(style="display:inline-block; width: 150px",numericInput("citrus_cLUC", h6("Citrus"), min=0, value = 0.0153, step = 0.001)),
                          # div(style="display:inline-block; width: 150px",numericInput("corn_cLUC", h6("Corn"), min=0, value = 0.0118, step = 0.001)), 
                          # br(),
                          # 
                          # div(style="display:inline-block; width: 150px",numericInput("cotton_cLUC", h6("Cotton"), min=0, value = 0.0117, step = 0.001)), 
                          # div(style="display:inline-block; width: 150px",numericInput("grain_cLUC", h6("Grain"), min=0, value = 0.0106, step = 0.001)),
                          # div(style="display:inline-block; width: 150px",numericInput("grape_cLUC", h6("Grape"), min=0, value = 0.0029, step = 0.001)), 
                          # br(), br(),
                          
                          h4("Analysis"),
                          selectInput("well_dpthLUC", h5("Well depth (screen top, meters):"),
                                      choices=c(50, 100, 150),
                                      selected = 100,
                                      width='50%'),
                          h5("(Extraction rate fixed to 3000 m3/d)"),
                          
                          #numericInput("tLUC", h5("Time of LUC (years)"), value = 68),
                          
                          div(style="display:inline-block; width: 175px",numericInput("yearINI", h5("Year Initial Land-Use"), value = 1950, step = 1.0)), 
                          div(style="display:inline-block; width: 175px",numericInput("yearLUC", h5("Year Land-Use Change"), value = 2019, step = 1.0)),
                          
                          selectInput("ana_exceed", h5("Exceedance prob. to plot:"),
                                      choices=c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 99),
                                      selected = 50,
                                      width='50%'),
                          
                          sliderInput("poroLUC", h5("Effective Porosity"), min=0, max=1, value = 0.3),
                          
                          br(),
                          actionButton("instruPanel2", "Instructions")

             ),
             
             mainPanel(img(src='logo_UCD.jpg', width = "150px", align = "right"),
                       br(),br(),
                       fluidPage(
                         fluidRow(
                           column(width = 4,
                                  br(),
                                  plotlyOutput("pie0", width = "250px", height = "250px"),
                                  br(),
                                  textOutput("crops0"),
                                  tags$head(tags$style("#crops0{color:rgb(205, 12, 24);font-size: 16px;}")),
                                  br(),
                                  textOutput("test_RCHcropsINI"),
                                  tags$head(tags$style("#test_RCHcropsINI{color:rgb(150, 150, 150);font-size: 14px;}")),
                                  textOutput("accu_RCHcropsINI"),
                                  tags$head(tags$style("#accu_RCHcropsINI{color:rgb(205, 12, 24);font-size: 14px;}")),
                                  # tags$head(tags$style(HTML("
                                  #   #pie0 {position: fixed; top: 100px;}
                                  #   #crops0 {position: fixed; margin-top: 175px;}
                                  #   #test_RCHcropsINI {position: fixed; margin-top: 175px;}
                                  #   #accu_RCHcropsINI {position: fixed; margin-top: 200px;}
                                  #   "))),
                                  br(),br()
                                  ),
                           
                           column(width = 4, offset = 1,
                                  br(),
                                  plotlyOutput("pieLUC", width = "250px", height = "250px"),
                                  br(),
                                  textOutput("cropsLUC"),
                                  tags$head(tags$style("#cropsLUC{color:rgb(205, 12, 24);font-size: 16px;}")),
                                  br(),
                                  textOutput("test_RCHcropsLUC"),
                                  tags$head(tags$style("#test_RCHcropsLUC{color:rgb(150, 150, 150);font-size: 14px;}")),
                                  textOutput("accu_RCHcropsLUC"),
                                  tags$head(tags$style("#accu_RCHcropsLUC{color:rgb(205, 12, 24);font-size: 14px;}")),
                                  # tags$head(tags$style(HTML("
                                  #   #pieLUC {position: fixed; top: 100px;}
                                  #   #cropsLUC {position: fixed; margin-top: 175px;}
                                  #   #test_RCHcropsLUC {position: fixed; margin-top: 175px;}
                                  #   #accu_RCHcropsLUC {position: fixed; margin-top: 200px;}
                                  # "))),
                                  br(),br()
                                  )
                         ),
                         
                         plotlyOutput("plotLUC", width = "750px", height = "400px") #,
                         # tags$head(tags$style(HTML("
                         #            #plotLUC {position: fixed; top: 500px;}
                         #            ")))
                         
                       )
           )
           )
  )
  
  
  #---------------------------
  #Panel#3: About
  #---------------------------
  #tabPanel(title = "About",
  #         includeMarkdown("about.Rmd")
  #)
  
  
#--------------------------  
) #navbar
) #shinyUI

