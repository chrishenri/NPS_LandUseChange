library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(RColorBrewer)

#Instructions
#----------------------------------------------------------
shinyServer( function(input,output,session){
  
  observeEvent(input$instruPanel1, {
    showModal(modalDialog(
      title = "Instructions",
      h4("Porosity"), br(), 
      "Give the effective porosity corresponding to your area;", br(), 
      "Will impact all analysis by scaling travel times", br(), 
      br(),
      hr(),
      
      h4("Concentration Percentiles Analysis"), br(), 
      "Give a control concentration such as the MCL;", br(), br(),
      "Select which plot the produce:", br(),
      "- 90% exceedance probability: 90% of wells within the region will exceed the concentration in y-axis at the time in the x-axis;", br(),
      "- 50% exceedance probability: 50% of wells within the region will exceed the concentration in y-axis at the time in the x-axis;", br(),
      "- 10% exceedance probability: 10% of wells within the region will exceed the concentration in y-axis at the time in the x-axis;", br(),
      br(),
      hr(),
      
      h4("Concentration Histogram Analysis"), br(), 
      "Give at which time you would like to get the concentration histogram",
      
      easyClose = TRUE
    ))
  })
  
  
  Prob <- reactive({
    data.frame(
      Parameter = c("Porosity","Control C"),
      Value = c(input$poro,input$MCL)
      )
  })
  
  
  output$problem <- renderTable({
    Prob()
  })
  
  
  #Panel specific Inputs
  #----------------------------------------------------------
  output$InputsPerc <- renderUI({
    numAq <- as.integer(input$naq)
    if (input$tabs=="A") {
      tagList(
      h4("Concentration Statistical Analysis"),
      h5("Plot/Analyze Concentration Exceedance Probability"),
      div(style="display:inline-block; width: 70px",checkboxInput("c10", "90%", value = FALSE, width = "200%")),
      div(style="display:inline-block; width: 70px",checkboxInput("c50", "50%", value = FALSE, width = "200%")),
      div(style="display:inline-block; width: 70px",checkboxInput("c90", "10%", value = FALSE, width = "100%"))
      )
    }
  })
  
  output$InputsHisto <- renderUI({
    numAq <- as.integer(input$naq)
    if (input$tabs=="C") {
      tagList(
        h4("Concentration Histogram Analysis"),
        sliderInput("thisto", h5("Time for concentration histogram"), min=0, max=400, value = 0, step = 1)
      )
    }
  })
  

  #Plot CBTCs stats
  #----------------------------------------------------------
  output$plotC <- renderPlotly({
    
    fperc <- paste(c("percentiles_d",input$well_dpth,"m.csv"), collapse = '')
    dataC <- read.csv(fperc, header=TRUE, sep=",", dec=".")
    P90 <- dataC$c10
    P50 <- dataC$c50
    P10 <- dataC$c90
    times <- dataC$time*input$poro/0.3
    
    dataMCL <- data.frame("MCL" = c(input$MCL,input$MCL), "timesMCL" = c(0,max(times)))
    
    a <- list(
      autotick = TRUE,
      # ticks = "outside",
      # tick0 = 0,
      # dtick = 0.25,
      # ticklen = 5,
      # tickwidth = 2,
      tickcolor = toRGB("blue")
    )
    
    
    #plot MCL
    p <- plot_ly(dataC, y = ~dataMCL$MCL, x = ~dataMCL$timesMCL, type = 'scatter', mode = 'lines', name = 'MCL', opacity = 0.4,
                 line = list(color = 'rgb(205, 12, 24)', width = 9, dash = 'dash')
                 ) %>%
      layout(
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
             xaxis = list(title = "Time [years]",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          dtick = 50.0,
                          zeroline = FALSE),
             yaxis = list(title = "Concentration [mg/L]",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
                   
    #plot c10
    if (input$c10 == "TRUE") {
      p <- add_trace(p, y = ~P90, x = ~times, name = 'P90', mode = 'lines', opacity = 0.9,
                     line = list(color = 'rgb(64, 142, 202)', width = 4, dash = 'dots')
                     )
    }
    
    #plot c50
    if (input$c50 == "TRUE") {
      p <- add_trace(p, y = ~P50, x = ~times, name = 'P50', mode = 'lines', opacity = 0.9,
                     line = list(color = 'rgb(255, 198, 38)', width = 4, dash = 'dots')
                     )
    }
    
    #plot c90
    if (input$c90 == "TRUE") {
      p <- add_trace(p, y = ~P10, x = ~times, name = 'P10', mode = 'lines', opacity = 0.9,
                     line = list(color = 'rgb(166, 212, 135)', width = 4, dash = 'dots')
                     )
    }
    p
    
  })
  
  
  output$AnaPerc10 <- renderText({
    if (input$c10 == "TRUE") {
      paste("90% of wells will exceed the control concentration of", input$MCL, "mg/L after ")
    }
  })
  output$AnaPerc10b <- renderText({
    fperc  <- paste(c("percentiles_d",input$well_dpth,"m.csv"), collapse = '')
    dataC <- read.csv(fperc, header=TRUE, sep=",", dec=".")
    times <- dataC$time*input$poro/0.3
    if (max(dataC$c10) < input$MCL & input$c10 == "TRUE") {
      paste("...","never")
    }
    else{
    if (input$c10 == "TRUE") {
      t10i <- min(which(dataC$c10 > input$MCL))
      t10 <- round(times[t10i-1])
      paste(t10, "years")
    }}
  })
  
  
  output$AnaPerc50 <- renderText({
    if (input$c50 == "TRUE") {
      paste("50% of wells will exceed the control concentration of", input$MCL, "mg/L after ")
    }
  })
  output$AnaPerc50b <- renderText({
    fperc  <- paste(c("percentiles_d",input$well_dpth,"m.csv"), collapse = '')
    dataC <- read.csv(fperc, header=TRUE, sep=",", dec=".")
    times <- dataC$time*input$poro/0.3
    if (max(dataC$c50) < input$MCL & input$c50 == "TRUE") {
      paste("...","never")
    }
    else{
    if (input$c50 == "TRUE") {
      t50i <- min(which(dataC$c50 > input$MCL))
      t50 <- round(times[t50i-1])
      paste(t50, "years")
    }}
  })
  
  
  output$AnaPerc90 <- renderText({
    if (input$c90 == "TRUE") {
      paste("10% of wells will exceed the control concentration of", input$MCL, "mg/L after ")
    }
  })
  output$AnaPerc90b <- renderText({
    fperc  <- paste(c("percentiles_d",input$well_dpth,"m.csv"), collapse = '')
    dataC <- read.csv(fperc, header=TRUE, sep=",", dec=".")
    times <- dataC$time*input$poro/0.3
    if (max(dataC$c90) < input$MCL & input$c90 == "TRUE") {
      paste("...","never")
    }
      else{
      if (input$c90 == "TRUE") {
        t90i <- min(which(dataC$c90 > input$MCL))
        t90 <- round(times[t90i-1])
        paste(t90, "years")
      }}
  })
  
  
  #Plot CBTCs stats
  #----------------------------------------------------------
  output$plotProbC <- renderPlotly({
    
    #time vector (for all depths)
    fprobCt <- paste(c("logprob_cVSt_times.csv"), collapse = '')
    probCt <- read.csv(fprobCt, header=TRUE, sep=",", dec=".")
    Times <- probCt$time*input$poro/0.3
    
    #concentration vector (depth dependent)
    fprobC <- paste(c("logprob_cVSt_d",input$well_dpth,"m.csv"), collapse = '')
    probC <- read.csv(fprobC, header=FALSE, sep=",", dec=".")
    probCmat <- as.matrix(probC)
    
    #probability matrix  (depth dependent)
    fprobCc <- paste(c("logprob_cVSt_c_d",input$well_dpth,"m.csv"), collapse = '')
    probCc <- read.csv(fprobCc, header=TRUE, sep=",", dec=".")
    
    vals <- unique(scales::rescale(c(volcano)))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Blues", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    
    
    col <- rev(terrain.colors(99))
    p2 <- plot_ly(x = Times, y = probCc$conc, z = probCmat, type = "heatmap", colors = "YlGnBu", opacity = 0.8, colorbar = list(title = "log Prob.")
                 ) %>%
      add_segments(x = 0, xend = max(Times), y = input$MCL, yend = input$MCL, line=list(color=I(rgb(205/255, 12/255, 24/255)), width = 8, dash = 'dash'), opacity = 0.6, name = 'MCL') %>%
      layout(
        xaxis = list(title = "Time [years]"),
        yaxis = list(title = "Concentration [mg/L]")
        ) 

  })
  
  
  #Plot CBTCs histogram
  #----------------------------------------------------------
  output$plotHistoC <- renderPlotly({
    fcbtcDB <- paste(c("cbtcDB_d",input$well_dpth,"m.csv"), collapse = '')
    cbtcDB <- read.csv(fcbtcDB, header=FALSE, sep=",", dec=".")
    
    times <- cbtcDB[,1]*input$poro/0.3 
    
    thisto <- input$thisto
    itAna = which(abs(times-thisto)==min(abs(times-thisto)))
    
    cbtcDB2 = t(cbtcDB)
    
    histoC <- hist(cbtcDB2[2:151,itAna], plot=FALSE)
    maxprob = max(histoC$counts/sum(histoC$counts))
    
    plot_ly(alpha = 0.6) %>%
      add_histogram(x = ~cbtcDB2[2:151,itAna], type = "histogram", histnorm = "probability", name = 'Histo. Conc.') %>%
      add_segments(x = input$MCL, xend = input$MCL, y = 0, yend = maxprob, name = 'MCL', line=list(color=I(rgb(205/255, 12/255, 24/255)), width = 8, dash = 'dash'), opacity = 0.8) %>%
      layout(
        paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
        xaxis = list(title = "Concentration [mg/L]"),
        yaxis = list(title = "Probability [-]")
      ) 
      
  })
  
  
  #LUC
  #----------------------------------------------------------
  
  output$mfINI <- renderUI({
    if (input$mf_unit_INI=="kg N/ha/y") {
      mult=3650} else {
      mult=3256.459
      }
    tagList(
      div(style="display:inline-block; width: 150px",numericInput("almond_cIni", h6("Almond"), value = round(0.0186*mult, digits = 1), step = 1)), 
      div(style="display:inline-block; width: 150px",numericInput("citrus_cIni", h6("Citrus"), value = round(0.0153*mult, digits = 1), step = 1)),
      div(style="display:inline-block; width: 150px",numericInput("corn_cIni", h6("Corn"), value = round(0.0118*mult, digits = 1), step = 1)), 
      br(),
      
      div(style="display:inline-block; width: 150px",numericInput("cotton_cIni", h6("Cotton"), value = round(0.0117*mult, digits = 1), step = 1)), 
      div(style="display:inline-block; width: 150px",numericInput("grain_cIni", h6("Grain"), value = round(0.0106*mult, digits = 1), step = 1)),
      div(style="display:inline-block; width: 150px",numericInput("grape_cIni", h6("Grape"), value = round(0.0029*mult, digits = 1), step = 1)), 
      br(), br()
    )
  })
  
  #----------------------------------------------------------
  output$mfLUC <- renderUI({
    if (input$mf_unit_LUC=="kg N/ha/y") {
      mult=3650} else {
        mult=3256.459
      }
    tagList(
      div(style="display:inline-block; width: 150px",numericInput("almond_cLUC", h6("Almond"), value = round(0.0186*mult, digits = 1), step = 1)), 
      div(style="display:inline-block; width: 150px",numericInput("citrus_cLUC", h6("Citrus"), value = round(0.0153*mult, digits = 1), step = 1)),
      div(style="display:inline-block; width: 150px",numericInput("corn_cLUC", h6("Corn"), value = round(0.0118*mult, digits = 1), step = 1)), 
      br(),
      
      div(style="display:inline-block; width: 150px",numericInput("cotton_cLUC", h6("Cotton"), value = round(0.0117*mult, digits = 1), step = 1)), 
      div(style="display:inline-block; width: 150px",numericInput("grain_cLUC", h6("Grain"), value = round(0.0106*mult, digits = 1), step = 1)),
      div(style="display:inline-block; width: 150px",numericInput("grape_cLUC", h6("Grape"), value = round(0.0029*mult, digits = 1), step = 1)), 
      br(), br()
    )
  })
  
  #----------------------------------------------------------
  output$crops0 <- renderText({
    crop_prop0 = c(input$almond_propIni, input$citrus_propIni, input$corn_propIni, input$cotton_propIni, input$grain_propIni, input$grape_propIni)
    sumCrop0 = sum(crop_prop0)
    if (sumCrop0 != 1) {
      paste("(!) Initial crops proportions sum to", sumCrop0, "... must sum to 1.")
    }
  })
  
  #----------------------------------------------------------
  output$cropsLUC <- renderText({
    crop_propLUC = c(input$almond_propLUC,input$citrus_propLUC,input$corn_propLUC,input$cotton_propLUC,input$grain_propLUC,input$grape_propLUC)
    sumCropLUC = sum(crop_propLUC)
    if (sumCropLUC != 1) {
      paste("(!) New crops proportions sum to", sumCropLUC, "... must sum to 1.")
    }
  })
  
  
  #----------------------------------------------------------
  output$pie0 <- renderPlotly({
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)', 'rgb(171,104,87)', 'rgb(128,133,133)')
    
    crop.data <- data.frame(
      crop_type = c("almond", "citrus", "corn", "cotton", "grain", "grape", "to allocate"),
      crop_prop0 = c(input$almond_propIni, input$citrus_propIni, input$corn_propIni, input$cotton_propIni, input$grain_propIni, input$grape_propIni,0),
      crop_c0 = c(input$almond_cIni, input$citrus_cIni, input$corn_cIni, input$cotton_cIni, input$grain_cIni, input$grape_cIni,0)
    )
    
    # #estimate the average recharge rate and check if the analytical solution is applicable
    # rch = matrix(
    #   c(0.0023,0.0017,0.0015,0.0017,0.0004,0.0018,
    #     0.0017,0.0014,0.0013,0.0014,0.0005,0.0014,
    #     1e-03*0.0289,1e-03*0.2310,1e-03*0.2608,1e-03*0.1974,1e-03*0.0207,1e-03*0.1544),
    #   nrow=3,              # number of soil type
    #   ncol=6,              # number of crops
    #   byrow = TRUE)
    # 
    # propK = c(0.45,0.25,0.3) #proportion of each soil type in the basin
    # propCZ = 0.3265 #proportion of the basin covered by the stochastic capture zone
    # crop_prop00 = c(0.24, 0.24, 0.18, 0.12, 0.12, 0.10) #crop proportions for the base scenario
    # 
    # rwei0 <- matrix(, nrow = 3, ncol = 6)
    # rweiINI <- matrix(, nrow = 3, ncol = 6)
    # for (icrop in 1:6){
    #   for (isoil in 1:3){
    #     rwei0[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_prop00[icrop]
    #     rweiINI[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop.data$crop_prop0[icrop]
    #   }
    # }
    # reqINI = mean(rwei0)*(1-propCZ)+mean(rweiINI)*(propCZ)
    # rfrac = reqINI/mean(rwei0)
    
    #plot chart
    sumCrop0= sum(crop.data$crop_prop0)
    crop.data$crop_prop0[7] = 1-sumCrop0
    data <- crop.data[, c('crop_type', 'crop_prop0', 'crop_c0')]
    # if (sumCrop0 == 1) {
      p <- plot_ly(data, labels = ~crop_type, values = ~crop_prop0, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(crop_c0),
                   marker = list(colors = brewer.pal(7, "Set2"),
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = FALSE) %>%
        layout(title = 'Intial Land-Use',
               # annotations = list(text = paste(c("r=", format(rfrac,digits=2)), collapse = " "),  x = 0, y = 0,showarrow=FALSE ),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    # }
    
  })
  
  #-------------
  output$test_RCHcropsINI <- renderText({
    #estimate the average recharge rate and check if the analytical solution is applicable
    rch = matrix(
      c(0.0023,0.0017,0.0015,0.0017,0.0004,0.0018,
        0.0017,0.0014,0.0013,0.0014,0.0005,0.0014,
        1e-03*0.0289,1e-03*0.2310,1e-03*0.2608,1e-03*0.1974,1e-03*0.0207,1e-03*0.1544),
      nrow=3,              # number of soil type
      ncol=6,              # number of crops
      byrow = TRUE)
    
    propK = c(0.45,0.25,0.3) #proportion of each soil type in the basin
    propCZ = 0.3265 #proportion of the basin covered by the stochastic capture zone
    crop_prop0 = c(0.24, 0.24, 0.18, 0.12, 0.12, 0.10) #crop proportions for the base scenario
    crop_propINI = c(input$almond_propIni, input$citrus_propIni, input$corn_propIni, input$cotton_propIni, input$grain_propIni, input$grape_propIni)
    sumCropINI = sum(crop_propINI)
    
    rwei0 <- matrix(, nrow = 3, ncol = 6)
    rweiINI <- matrix(, nrow = 3, ncol = 6)
    for (icrop in 1:6){
      for (isoil in 1:3){
        rwei0[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_prop0[icrop]
        rweiINI[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_propINI[icrop]
      }
    }
    reqINI = mean(rwei0)*(1-propCZ)+mean(rweiINI)*(propCZ)
    rfrac = reqINI/mean(rwei0)
    DeltaRCH = abs(1-rfrac)*100
    
    #output
    if (sumCropINI == 1) {
      paste("The average recharge has been changed by", format(DeltaRCH,digits=2),"%;")
    }
    
  })
  
  #-------------
  output$accu_RCHcropsINI <- renderText({
    #estimate the average recharge rate and check if the analytical solution is applicable
    rch = matrix(
      c(0.0023,0.0017,0.0015,0.0017,0.0004,0.0018,
        0.0017,0.0014,0.0013,0.0014,0.0005,0.0014,
        1e-03*0.0289,1e-03*0.2310,1e-03*0.2608,1e-03*0.1974,1e-03*0.0207,1e-03*0.1544),
      nrow=3,              # number of soil type
      ncol=6,              # number of crops
      byrow = TRUE)
    
    propK = c(0.45,0.25,0.3) #proportion of each soil type in the basin
    propCZ = 0.3265 #proportion of the basin covered by the stochastic capture zone
    crop_prop0 = c(0.24, 0.24, 0.18, 0.12, 0.12, 0.10) #crop proportions for the base scenario
    crop_propINI = c(input$almond_propIni, input$citrus_propIni, input$corn_propIni, input$cotton_propIni, input$grain_propIni, input$grape_propIni)
    sumCropINI = sum(crop_propINI)
    
    rwei0 <- matrix(, nrow = 3, ncol = 6)
    rweiINI <- matrix(, nrow = 3, ncol = 6)
    for (icrop in 1:6){
      for (isoil in 1:3){
        rwei0[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_prop0[icrop]
        rweiINI[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_propINI[icrop]
      }
    }
    reqINI = mean(rwei0)*(1-propCZ)+mean(rweiINI)*(propCZ)
    rfrac = reqINI/mean(rwei0)
    DeltaRCH = abs(1-rfrac)*100
    
    #output
    if (sumCropINI == 1) {
      if (DeltaRCH<7) {
        "The solution is accurate"
      } else {
        "The solution is not accurate"
      }
    }
    
  })
  
  
  #-------------
  output$test_RCHcropsLUC <- renderText({
    #estimate the average recharge rate and check if the analytical solution is applicable
    rch = matrix(
      c(0.0023,0.0017,0.0015,0.0017,0.0004,0.0018,
        0.0017,0.0014,0.0013,0.0014,0.0005,0.0014,
        1e-03*0.0289,1e-03*0.2310,1e-03*0.2608,1e-03*0.1974,1e-03*0.0207,1e-03*0.1544),
      nrow=3,              # number of soil type
      ncol=6,              # number of crops
      byrow = TRUE)
    
    propK = c(0.45,0.25,0.3) #proportion of each soil type in the basin
    propCZ = 0.3265 #proportion of the basin covered by the stochastic capture zone
    crop_prop0 = c(0.24, 0.24, 0.18, 0.12, 0.12, 0.10) #crop proportions for the base scenario
    crop_propLUC = c(input$almond_propLUC, input$citrus_propLUC, input$corn_propLUC, input$cotton_propLUC, input$grain_propLUC, input$grape_propLUC)
    sumCropLUC = sum(crop_propLUC)
    
    rwei0 <- matrix(, nrow = 3, ncol = 6)
    rweiLUC <- matrix(, nrow = 3, ncol = 6)
    for (icrop in 1:6){
      for (isoil in 1:3){
        rwei0[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_prop0[icrop]
        rweiLUC[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_propLUC[icrop]
      }
    }
    reqLUC = mean(rwei0)*(1-propCZ)+mean(rweiLUC)*(propCZ)
    rfrac = reqLUC/mean(rwei0)
    DeltaRCH = abs(1-rfrac)*100
    
    #output
    if (sumCropLUC == 1) {
      paste("The average recharge has been changed by", format(DeltaRCH,digits=2),"%;")
    }
    
  })
  
  
  #-------------
  output$accu_RCHcropsLUC <- renderText({
    #estimate the average recharge rate and check if the analytical solution is applicable
    rch = matrix(
      c(0.0023,0.0017,0.0015,0.0017,0.0004,0.0018,
        0.0017,0.0014,0.0013,0.0014,0.0005,0.0014,
        1e-03*0.0289,1e-03*0.2310,1e-03*0.2608,1e-03*0.1974,1e-03*0.0207,1e-03*0.1544),
      nrow=3,              # number of soil type
      ncol=6,              # number of crops
      byrow = TRUE)
    
    propK = c(0.45,0.25,0.3) #proportion of each soil type in the basin
    propCZ = 0.3265 #proportion of the basin covered by the stochastic capture zone
    crop_prop0 = c(0.24, 0.24, 0.18, 0.12, 0.12, 0.10) #crop proportions for the base scenario
    crop_propLUC = c(input$almond_propLUC, input$citrus_propLUC, input$corn_propLUC, input$cotton_propLUC, input$grain_propLUC, input$grape_propLUC)
    sumCropLUC = sum(crop_propLUC)
    
    rwei0 <- matrix(, nrow = 3, ncol = 6)
    rweiLUC <- matrix(, nrow = 3, ncol = 6)
    for (icrop in 1:6){
      for (isoil in 1:3){
        rwei0[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_prop0[icrop]
        rweiLUC[isoil,icrop] = rch[isoil,icrop]*propK[isoil]*crop_propLUC[icrop]
      }
    }
    reqLUC = mean(rwei0)*(1-propCZ)+mean(rweiLUC)*(propCZ)
    rfrac = reqLUC/mean(rwei0)
    DeltaRCH = abs(1-rfrac)*100
    
    #output
    if (sumCropLUC == 1) {
      if (DeltaRCH<5) {
        "The solution is accurate"
      } else {
        "The solution is not accurate"
      }
    }
    
  })
  
  #-------------
  output$pieLUC <- renderPlotly({
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)', 'rgb(171,104,87)', 'rgb(128,133,133)')
    
    crop.data <- data.frame(
      crop_type = c("almond", "citrus", "corn", "cotton", "grain", "grape", "to allocate"),
      crop_propLUC = c(input$almond_propLUC,input$citrus_propLUC,input$corn_propLUC,input$cotton_propLUC,input$grain_propLUC,input$grape_propLUC,0),
      crop_cLUC = c(input$almond_cLUC, input$citrus_cLUC, input$corn_cLUC, input$cotton_cLUC, input$grain_cLUC, input$grape_cLUC,0)
    )
    sumCropLUC= sum(crop.data$crop_propLUC)
    crop.data$crop_propLUC[7] = 1-sumCropLUC
    data <- crop.data[, c('crop_type', 'crop_propLUC', 'crop_cLUC')]
    # if (sumCropLUC == 1) {
    p <- plot_ly(data, labels = ~crop_type, values = ~crop_propLUC, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste(crop_cLUC),
                 marker = list(colors = brewer.pal(7, "Set2"),
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE) %>%
      layout(title = 'New Land-Use',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    # }
    
  })
  
  
  #-------------
  output$plotLUC <- renderPlotly({
    
    crop.data <- data.frame(
      crop_type = c("almond", "citrus", "corn", "cotton", "grain", "grape"),
      crop_prop0 = c(0.24, 0.24, 0.18, 0.12, 0.12, 0.10),
      crop_propIni = c(input$almond_propIni, input$citrus_propIni, input$corn_propIni, input$cotton_propIni, input$grain_propIni, input$grape_propIni),
      crop_propLUC = c(input$almond_propLUC, input$citrus_propLUC, input$corn_propLUC, input$cotton_propLUC, input$grain_propLUC, input$grape_propLUC)
    )
    sumCrop0 = sum(crop.data$crop_prop0)
    sumCropIni = sum(crop.data$crop_propIni)
    sumCropLUC = sum(crop.data$crop_propLUC)
    
    if (sumCropLUC==1 & sumCropIni==1) {
      
      resamp = 0 # 1: resample data to reduce computational cost, 0: keep data as it is | could be an input
      ndata = 40 # number of data if resampling | could be an input
      
      #--------------- Inputs
      #--------------- get index of analyzed percentile
      allperc <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
      ana_perc = 100-as.numeric(input$ana_exceed) #transform exceedance probability (input) to percentile
      iperc = match(ana_perc,allperc)+1
      
      #--------------- original data
      fpercC <- c("percentiles_d",input$well_dpthLUC,"m.csv")
      fperc  <- paste(fpercC, collapse = '')
      
      if (input$mf_unit_INI=="kg N/ha/y") {
        mult=3650} else {
        mult=3256.459
        }
      
      dataC0 <- read.csv(fperc, header=TRUE, sep=",", dec=".")
      mfi_avtK0 <- c(0.0186, 0.0153, 0.0118, 0.0117, 0.0106, 0.0029)
      mfi_avtKIni <- c(input$almond_cIni/mult, input$citrus_cIni/mult, input$corn_cIni/mult, input$cotton_cIni/mult, input$grain_cIni/mult, input$grape_cIni/mult)
      mfi_avtKLUC <- c(input$almond_cLUC/mult, input$citrus_cLUC/mult, input$corn_cLUC/mult, input$cotton_cLUC/mult, input$grain_cLUC/mult, input$grape_cLUC/mult)
      if (resamp == 1){
        dataC0_samplef <- approxfun(dataC0[,1],dataC0[,iperc])
        dataC0_sample <- dataC_samplef(times_sample)
      }
      if (resamp == 0){
        dataC0_sample = dataC0[,iperc]
      }
      
      #--------------- Times
      ## scale times by porosity
      dataC0[,1] = dataC0[,1]*input$poroLUC/0.3
      ## maximun time for analysis
      tmax = max(dataC0[,1])
      ## time of LUC
      #tLUC = input$tLUC
      tLUC = input$yearLUC - input$yearINI
      ## resample data to reduce vector size (if applied)
      if (resamp == 1){
        times_sample <- seq(from=0,to=tmax,by=tmax/ndata)        
      }
      if (resamp == 0){
        times_sample = dataC0[,1]
      }
      itLUC = which(abs(times_sample-tLUC)==min(abs(times_sample-tLUC))) #get index of closest values to time of LUCs
      times_sample[itLUC] = tLUC #replace this closest values by the actual time of LUCs
      
      #--------------- Multipliers
      sum0 = crossprod(crop.data[,2], mfi_avtK0[])
      ### ...for intial crop distribution
      sumIni = crossprod(crop.data[,3], mfi_avtKIni[])
      multIni = sumIni/sum0
      ### ...for new crop distribution
      sumLUC = crossprod(crop.data[,4], mfi_avtKLUC[])
      multLUC = sumLUC/sumIni
      
      #--------------- Percentiles for initial crop distribution
      ## concentration percentiles before LUC  
      dataC_Ini = dataC0*multIni
      dataC_Ini[,1] = dataC0[,1]
      
      if (resamp == 1){
        dataCIni_samplef <- approxfun(dataC_Ini[,1],dataC_Ini[,iperc])
        dataCIni_sample <- dataCIni_samplef(times_sample)
      }
      if (resamp == 0){
        dataCIni_sample = dataC_Ini[,iperc]
      }
      
      ## derivative of concentration percentile (only the % to be analyzed)
      mf0 = numeric(length(times_sample))
      mfIni = numeric(length(times_sample))
      for (it in 2:length(times_sample)){
        # initial scenario
        mf0[it-1] = (dataC0_sample[it]-dataC0_sample[it-1])#/(times_sample[it]-times_sample[it-1])
        # LUC
        mfIni[it-1] = (dataCIni_sample[it]-dataCIni_sample[it-1])#/(times_sample[it]-times_sample[it-1])
      }
      
      # Convolution
      conv0 = numeric(length(times_sample))
      convIni = numeric(length(times_sample))
      
      conv0[1:length(times_sample)] = 0
      convIni[1:length(times_sample)] = 1

      perc0 = numeric(length(times_sample))
      percIni = numeric(length(times_sample))
      percInif = numeric(length(times_sample))
      
      for (it in 2:length(perc0)){
        for (it2 in 1:(it-1)){
          perc0[it]   = perc0[it]   + (conv0[it2] * mf0[it-it2])
          percIni[it] = percIni[it] + (convIni[it2] * mfIni[it-it2])
        }
      }
      percInif = perc0 + percIni
      
      
      #--------------- Percentiles for new crop distribution
      ## concentration percentiles after LUC  
      dataC_LUC = dataC_Ini*multLUC
      dataC_LUC[,1] = dataC_Ini[,1]
      
      if (resamp == 1){
        dataC_LUC_samplef <- approxfun(dataC_LUC[,1],dataC_LUC[,iperc])
        dataC_LUC_sample <- dataC_LUC_samplef(times_sample)
      }
      if (resamp == 0){
        dataC_LUC_sample = dataC_LUC[,iperc]
      }
      
      ## derivative of concentration percentile (only the one to be analyzed)
      mfIni = numeric(length(times_sample))
      mfLUC = numeric(length(times_sample))
      for (it in 2:length(times_sample)){
        # initial scenario
        mfIni[it-1] = (dataCIni_sample[it]-dataCIni_sample[it-1])#/(times_sample[it]-times_sample[it-1])
        # LUC
        mfLUC[it-1] = (dataC_LUC_sample[it]-dataC_LUC_sample[it-1])#/(times_sample[it]-times_sample[it-1])
      }
      
      
      # Convolution
      convIni = numeric(length(times_sample))
      convLUC = numeric(length(times_sample))
      
      convIni[1:itLUC] = 1
      convIni[itLUC:length(times_sample)] = 0
      convLUC[1:itLUC] = 0
      convLUC[itLUC:length(times_sample)] = 1
      
      percIni = numeric(length(times_sample))
      percLUC = numeric(length(times_sample))
      percLUCf = numeric(length(times_sample))
      
      for (it in 2:length(perc0)){
        for (it2 in 1:(it-1)){
          percIni[it] = percIni[it] + (convIni[it2] * mfIni[it-it2])
          percLUC[it] = percLUC[it] + (convLUC[it2] * mfLUC[it-it2])
        }
      }
      percLUCf = percIni + percLUC
      
      
      #-----------------------------------------
      # Plot
      p <- plot_ly(x = input$yearINI+times_sample, y = percLUCf, type = 'scatter', mode = 'lines', name = 'Land use change (LUC)', opacity = 0.5,
                   line = list(color = 'rgb(205, 12, 24)', width = 5)
      ) %>%
        layout(title = paste(input$ana_exceed, "% probability of exceedance"),
          paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
          xaxis = list(title = "Year",
                       gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       dtick = 50.0,
                       zeroline = FALSE),
          yaxis = list(title = "Concentration [mg/L]",
                       gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE))
      
      # p <- add_trace(p, x = times_sample, y = dataC0_sample, name = 'Original', mode = 'lines', opacity = 0.4,
      #                line = list(color = 'rgb(38, 38, 38)', width = 4)
      # )
      
      p <- add_trace(p, x = input$yearINI+times_sample, y = dataCIni_sample, name = 'Business as usual', mode = 'lines', opacity = 0.4,
                     line = list(color = 'rgb(255, 198, 38)', width = 4)
      )
      
      dataTLUC <- data.frame("TLUC" = c(input$yearLUC,input$yearLUC), "cLUC" = c(0,max(max(percLUCf),max(dataCIni_sample))))
      p <- add_trace(p, x = ~dataTLUC$TLUC, y = ~dataTLUC$cLUC, name = 'Time LUC', mode = 'lines', opacity = 0.2,
                     line = list(color = 'rgb(38, 38, 38)', width = 4, dash = 'dash')       
      )
      
      p
    }
    
  })
  
  #-------------
  observeEvent(input$instruPanel2, {
    showModal(modalDialog(
      title = "Instructions",
      
      h4("Land Uses"), br(), 
      "* Give the proportion of each crop within the modeled basin;", br(), 
      "* For each crop, give the loading of nitrate mass lost to aquifer;", br(),
      "* Do so for the initial land use (first aquifer contamination) and then for the new land use (after land use change).",
      br(),
      hr(),
      
      h4("Analysis"), br(), 
      "* Chose the depth of the top of the well screens. 3 options: 50 m. (164 ft), 100 m. (328 ft.), 150 m. (492 ft.);", br(), 
      "* Give the year when contaminant first reached the groundwater (e.g., post-development case); Give the year of the land use change;", br(), 
      "* Chose which probability of concentration exceedance to be plotted;", br(),
      "  E.g., 90% exceedance probability means that 90% of wells within the region will exceed the concentration in y-axis at the time in the x-axis;", br(),
      br(),
      hr(),
      
      h4("Effective Porosity"), br(), 
      "Give the effective porosity corresponding to your area;", br(), 
      "Will impact all analysis by scaling travel times", br(), 
      br(),
      hr(),
      
      easyClose = TRUE
    ))
  })
  
})