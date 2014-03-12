library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
    
    load("data/standardCont.RData")
    
    
    variableInputY <- reactive({
        switch(input$variableY)
    })
    variableInputX <- reactive({
        switch(input$variableX)
    })
    variableInputYear <- reactive({
        switch(input$year,
               "1946" = 1946,
               "1947" = 1947,
               "1948" = 1948,
               "1949" = 1949,
               "1950" = 1950,
               "1951" = 1951,
               "1952" = 1952,
               "1953" = 1953,
               "1954" = 1954,
               "1955" = 1955,
               "1956" = 1956,
               "1957" = 1957,
               "1958" = 1958,
               "1959" = 1959,
               "1960" = 1960,
               "1961" = 1961,
               "1962" = 1962,
               "1963" = 1963,
               "1964" = 1964,
               "1965" = 1965,
               "1966" = 1966,
               "1967" = 1967,
               "1968" = 1968,
               "1969" = 1969,
               "1970" = 1970,
               "1971" = 1971,
               "1972" = 1972,
               "1973" = 1973,
               "1974" = 1974,
               "1975" = 1975,
               "1976" = 1976,
               "1977" = 1977,
               "1978" = 1978,
               "1979" = 1979,
               "1980" = 1980,
               "1981" = 1981,
               "1982" = 1982,
               "1983" = 1983,
               "1984" = 1984,
               "1985" = 1985,
               "1986" = 1986,
               "1987" = 1987,
               "1988" = 1988,
               "1989" = 1989,
               "1990" = 1990,
               "1991" = 1991,
               "1992" = 1992,
               "1993" = 1993,
               "1994" = 1994,
               "1995" = 1995,
               "1996" = 1996,
               "1997" = 1997,
               "1998" = 1998,
               "1999" = 1999,
               "2000" = 2000,
               "2001" = 2001,
               "2002" = 2002,
               "2003" = 2003,
               "2004" = 2004,
               "2005" = 2005,
               "2006" = 2006,
               "2007" = 2007,
               "2008" = 2008,
               "2009" = 2009,
               "2010" = 2010,
               "2011" = 2011,
               "2012" = 2012)
    })
    variableInputCont <- reactive({
        switch(input$continent)
    })
    
    #******************************#
    #*** Scatterplot
    
    datasetInput <- reactive({
        #varx <- datPlot$perGini
        vary <- dat[, input$variableY]
        varx <- dat[, input$variableX]
        cntry <- as.character(dat$cname)
        contName <- as.character(dat$contName)
        group1 <- as.character(dat$group1)
        year <- dat$year
        datPlotX <- data.frame(vary,varx,cntry,year,contName,group1)
        datPlot <- datPlotX[datPlotX$year == input$year,]    
        
        })
    
    ## ****** ###
    # Plot
    
    plotInput <- reactive({
        
        cbPalette <- c("#000000", "#D55E00", "#56B4E9",  "#CC79A7", "#0072B2", "#F0E442","orange","dim grey")
        datPlot <- datasetInput()
        

        ## highlight a country
#         if (input$hl_country1 == "none") {
#             highlight_dot <- scale_x_continuous()
#             highlight_name <- scale_y_continuous()
#             }
#         if (input$hl_country1 != "none") {
#             dat_highlight <- datPlot[datPlot$cntry %in% input$hl_country1,]
#             highlight_dot <- geom_point(data=dat_highlight, aes(x=varx, y=vary,group=1),
#                        color="red", size=5)
#             highlight_name <- geom_text(data=dat_highlight, aes(x=varx, y=vary,
#                                            label=cntry,group=1),
#                           size=5, color="red",vjust=1, hjust=-.2)
#         }
        # log scale
        if (input$log_scale_scatter == TRUE) {
            scale_y <- scale_y_log10()
            scale_x <- scale_x_log10()
        }
        if (input$log_scale_scatter == FALSE) {
            scale_y <- scale_y_continuous()
            scale_x <- scale_x_continuous()
        }
         # subset the continent
         if (input$show_other == TRUE) datPlot <- datPlot
         if (input$show_other == FALSE) datPlot <- datPlot[datPlot$group != "X_other",]

#         
#         
        ggplot(datPlot, aes(x=varx, y=vary, 
                            label=cntry,group=1)) +
            geom_point(data=datPlot, aes(color=group1), size=4) +
            geom_smooth(method=lm, se=TRUE, alpha=.5, 
                        linetype="dashed", size=0.5) +  
            geom_text(size=4, vjust=-0.8, hjust=0.5) +
            labs(x = input$variableX,
                 y = input$variableY) + 
            theme_minimal() +
            scale_colour_manual(values=cbPalette) +
            theme(legend.title=element_blank()) +
            theme(legend.text=element_text(size=16)) +
            theme(legend.position="top") +
            theme(axis.title = element_text(size=16)) +
            theme(axis.text = element_text(size=16)) +
            guides(color = guide_legend(nrow = 2)) + 
            labs(title=paste("Year",input$year)) +
            #highlight_dot +
            #highlight_name +
            scale_y +
            scale_x
    })
    
    
    output$plot <- renderPlot({
        print(plotInput())
    })

    
    #******************************#
    #*** Time Scatterplot
    
#     datasetInputT <- reactive({
#         #varx <- datPlot$perGini
#         vary <- dat[, input$variableY]
#         varx <- dat[, input$variableX]
#         cntry <- as.character(dat$cname)
#         year <- dat$year
#         datPlot <- data.frame(vary,varx,cntry,year)
#         datPlot <- datPlot[datPlot$cntry %in% c(input$Country1,
#                                                 input$Country2,
#                                                 input$Country3,
#                                                 input$Country4,
#                                                 input$Country5),]
#         datPlot <- datPlot[datPlot$year >= input$yearStart,]
#         datPlot <- datPlot[datPlot$year <= input$yearEnd,]
#         datPlot <- datPlot[!is.na(datPlot$varx), ]
#         datPlotT <- datPlot[!is.na(datPlot$vary), ]
#         datPlotT <- datPlotT[with(datPlotT, order(year)), ]
#     })
    
    ## ****** ###
    # Plot
    
#     plotInputT <- reactive({
#       cbPalette <- c("#000000", "#D55E00", "#56B4E9",  "#CC79A7", "#0072B2", "#F0E442","orange","dim grey")
#       datPlotT <- datasetInput()
#         
#         ggplot(datPlotT, aes(x=varx, y=vary, 
#                              group=cntry,color=group1,
#                              label=year)) +
#             geom_point(alpha=.5) + 
#             geom_path(alpha=.5)  +
#             geom_text(size=5, hjust=0.0, vjust=-0.5,alpha=.7) +
#             geom_text(data=merge(datPlotT, aggregate(year ~ cntry, datPlotT, max),
#                                  by=c("year","cntry")),
#                       aes(x=varx,y=vary,label=cntry),
#                       hjust=1,vjust=-1,size=5) + 
#             labs(x = input$variableX,
#                  y = input$variableY) + 
#             theme_minimal() +
#             scale_colour_manual(values=cbPalette) +
#             theme(legend.title=element_blank()) +
#             theme(legend.text=element_text(size=16)) +
#             theme(legend.position="top") +
#             theme(axis.title = element_text(size=16)) +
#             theme(axis.text = element_text(size=16))
#         
#     })
#     
#     
#     output$timeplot <- renderPlot({
#         print(plotInputT())
#     })
    
    ## Lineplot
datasetInputL <- reactive({
  #varx <- datPlot$perGini
  vary <- dat[, input$variableY]
  varx <- dat[, input$variableX]
  cntry <- as.character(dat$cname)
  contName <- as.character(dat$contName)
  group1 <- as.character(dat$group1)
  year <- dat$year
  datPlot <- data.frame(vary,varx,cntry,year,contName,group1)
  datPlot <- datPlot[datPlot$year >= input$yearStart,]
  datPlot <- datPlot[datPlot$year <= input$yearEnd,]
  datPlot <- datPlot[!is.na(datPlot$varx), ]
  datPlotT <- datPlot[!is.na(datPlot$vary), ]
  datPlotT <- datPlotT[with(datPlotT, order(year)), ]
})

  
    plotInputL <- reactive({
      cbPalette <- c("#000000", "#D55E00", "#56B4E9",  "#CC79A7", "#0072B2", "#F0E442","orange","dim grey")
      datPlot <- datasetInputL()
      # cre4ate comparison data  
      datComp <- datPlot[datPlot$cntry %in% c(input$Country1,
                                              input$Country2,
                                              input$Country3,
                                              input$Country4,
                                              input$Country5),]
      if (input$Country1 != "none") {
        comparison <- geom_line(data=datComp, aes(x=year, y=varx, 
                                    group=cntry), color="red")
          #geom_point(data=datComp, aes(x=year, y=varx, 
          #                             group=cntry))
          #geom_point(data=datComp, aes(x=year, y=varx, 
          #                            group=cntry), color="red") #+
          text <-  geom_text(data=merge(datComp, aggregate(year ~ cntry, datComp, max),
                                by=c("year","cntry")),
                     aes(x=year,y=varx,label=cntry),
                     hjust=1,vjust=-1,size=5, color="red")
          
          
      } else {comparison <- scale_y_continuous()
              text <- scale_y_continuous()}
      
      
      
      # subset the continent
      if (input$show_other == TRUE) datPlot <- datPlot
      if (input$show_other == FALSE) datPlot <- datPlot[datPlot$group != "X_other",]
        
        if (input$log_scale == TRUE) scale <- scale_y_log10()
        if (input$log_scale == FALSE) scale <- scale_y_continuous()
      
      
      
        ggplot(datPlot, aes(x=year, y=varx, 
                             group=cntry,color=group1,
                             label=year)) +
            geom_point(alpha=.5) + 
            geom_path(alpha=.5)  +
          #geom_text(size=5, hjust=0.0, vjust=-0.5,alpha=.7) +
            geom_text(data=merge(datPlot, aggregate(year ~ cntry, datPlot, max),
                                 by=c("year","cntry")),
                      aes(x=year,y=varx,label=cntry),
                      hjust=1,vjust=-1,size=5) + 
            labs(x = "year",
                 y = input$variableX) + 
            theme_minimal() +
            scale_colour_manual(values=cbPalette) +
            theme(legend.title=element_blank()) +
            theme(legend.text=element_text(size=16)) +
            theme(legend.position="right") +
            theme(axis.title = element_text(size=16)) +
            theme(axis.text = element_text(size=16)) +
            scale +
        comparison +
        text
        
    })
    
    output$lineplot <- renderPlot({
        print(plotInputL())
    })
    
#     ## relative line plot
# 
# datasetInputR <- reactive({
#   #varx <- datPlot$perGini
#   vary <- dat[, input$variableY]
#   varx <- dat[, input$variableX]
#   cntry <- as.character(dat$cname)
#   contName <- as.character(dat$contName)
#   group1 <- as.character(dat$group1)
#   year <- dat$year
#   datPlot <- data.frame(vary,varx,cntry,year,contName,group1)
#   datPlot <- datPlot[datPlot$year >= input$yearStart,]
#   datPlot <- datPlot[datPlot$year <= input$yearEnd,]
#   datPlot <- datPlot[!is.na(datPlot$varx), ]
#   datPlotT <- datPlot[!is.na(datPlot$vary), ]
#   datPlotT <- datPlotT[with(datPlotT, order(year)), ]
#   # into wide format for relative figures
#   datPlotT <- datPlotT[datPlotT$group1 != "X_other",]
#   library(reshape2)
#   datPlotT$year <- factor(paste("x",datPlotT$year,sep=""))
#   datPlotT.w <- dcast(data=datPlotT, cntry + group1 ~ year, value.var="varx")
#   A <- function(x) {x / datPlotT.w[,3] * 100}
#   datPlotT.w <- as.data.frame(cbind(datPlotT.w[1:2], sapply(datPlotT.w[3:ncol(datPlotT.w)], A) ))
#   datPlotTl <- melt(datPlotT.w, id.vars=c("cntry","group1"))
#   datPlotTl$year <- gsub(x=datPlotTl$variable, pattern="x",replacement="")
#   datPlotTl$year <- as.factor(datPlotTl$year)
#   datPlotTl$year <- as.numeric(levels(datPlotTl$year))[datPlotTl$year]
# })
# 
#     
#     plotInputR <- reactive({
#       datPlotTl <- datasetInputR()
#         cbPalette <- c("#000000", "#D55E00", "#56B4E9",  "#CC79A7", "#0072B2", "#F0E442","orange","dim grey")
#         
# #         datPlotR$year  <- as.character(datPlotR$year)
# #         datPlotR$group1  <- as.character(datPlotR$group1)
# #         datPlotR$cntry  <- as.character(datPlotR$cntry)
# 
#          ggplot(datPlotTl, aes(x=year, y=value)) +
#            geom_point()
#         
#         ggplot(datPlotR, aes(x=factor(year), y=value, 
#                              group=cntry,color=group1)) +
#             geom_point(alpha=.5) + 
#             geom_path(alpha=.5)  +
#             geom_text(data=merge(datPlotR, aggregate(factor(year) ~ cntry, datPlotR, max),
#                                  by=c("year","cntry")),
#                       aes(x=factor(year),y=value,label=cntry),
#                       hjust=1,vjust=-1,size=5) + 
#             #labs(x = "year",
#             #     y = input$variableX) + 
#             theme_minimal() +
#             scale_colour_manual(values=cbPalette) +
#             theme(legend.title=element_blank()) +
#             theme(legend.text=element_text(size=16)) +
#             theme(legend.position="top") +
#             theme(axis.title = element_text(size=16)) +
#             theme(axis.text = element_text(size=16))
#         
#     })
    
    output$relaplot <- renderPlot({
        #print(plotInputR())
      vary <- dat[, input$variableY]
      varx <- dat[, input$variableX]
      cntry <- as.character(dat$cname)
      contName <- as.character(dat$contName)
      group1 <- as.character(dat$group1)
      year <- dat$year
      datPlot <- data.frame(vary,varx,cntry,year,contName,group1)
      datPlot <- datPlot[datPlot$year >= input$yearStart,]
      datPlot <- datPlot[datPlot$year <= input$yearEnd,]
      datPlot <- datPlot[!is.na(datPlot$varx), ]
      datPlotT <- datPlot[!is.na(datPlot$vary), ]
      datPlotT <- datPlotT[with(datPlotT, order(year)), ]
      # into wide format for relative figures
      datPlotT <- datPlotT[datPlotT$group1 != "X_other",]
      library(reshape2)
      datPlotT$year <- factor(paste("x",datPlotT$year,sep=""))
      datPlotT.w <- dcast(data=datPlotT, cntry + group1 ~ year, value.var="varx")
      A <- function(x) {x / datPlotT.w[,3] * 100}
      datPlotT.w <- as.data.frame(cbind(datPlotT.w[1:2], sapply(datPlotT.w[3:ncol(datPlotT.w)], A) ))
      datPlotTl <- melt(datPlotT.w, id.vars=c("cntry","group1"))
      datPlotTl$year <- gsub(x=datPlotTl$variable, pattern="x",replacement="")
      datPlotTl$year <- as.factor(datPlotTl$year)
      datPlotTl$year <- as.numeric(levels(datPlotTl$year))[datPlotTl$year]
      


     if (input$Country1 != "none") {
       datCompX <- datPlot[datPlot$group1 == "X_other",]
       datComp <- datCompX[datCompX$cntry %in% c(input$Country1,
                                               input$Country2,
                                               input$Country3,
                                               input$Country4,
                                               input$Country5),]
       datComp <- datComp[!is.na(datComp$vary), ]
       datComp <- datComp[with(datComp, order(year)), ]
       # into wide format for relative figures
       library(reshape2)
       datComp$year <- factor(paste("x",datComp$year,sep=""))
       datComp.w <- dcast(data=datComp, cntry + group1 ~ year, value.var="varx")
       A <- function(x) {x / datComp.w[,3] * 100}
       datComp.w <- cbind(datComp.w[1:2], sapply(datComp.w[3:ncol(datComp.w)], A) )
       datCompl <- melt(datComp.w, id.vars=c("cntry","group1"))
       datCompl$year <- gsub(x=datCompl$variable, pattern="x",replacement="")
       datCompl$year <- as.factor(datCompl$year)
       datCompl$year <- as.numeric(levels(datCompl$year))[datCompl$year]
       
       comparisonR <- geom_line(data=datCompl, aes(x=year, y=value, 
                                                  group=cntry), color="red")
        #geom_point(data=datComp, aes(x=year, y=varx, 
        #                             group=cntry))
        #geom_point(data=datComp, aes(x=year, y=varx, 
        #                            group=cntry), color="red") #+
        textR <-  geom_text(data=merge(datCompl, aggregate(year ~ cntry, datCompl, max),
                                      by=c("year","cntry")),
                           aes(x=year,y=value,label=cntry),
                           hjust=1,vjust=-1,size=5, color="red")
      } else {comparisonR <- scale_y_continuous()
              textR <- scale_y_continuous()}
      
      
      
      
      p <- ggplot(datPlotTl, aes(x=year, y=value, 
                                        group=cntry,color=group1)) +
        geom_point(alpha=.5) + 
        geom_path(alpha=.5)  +
        geom_text(data=merge(datPlotTl, aggregate(year ~ cntry, datPlotTl, max),
                             by=c("year","cntry")),
                  aes(x=year,y=value,label=cntry),
                  hjust=1,vjust=-1,size=5) + 
        labs(x = "year",
             y = input$variableX) + 
        theme_minimal() +
        scale_colour_manual(values=cbPalette) +
        theme(legend.title=element_blank()) +
        theme(legend.text=element_text(size=16)) +
        theme(legend.position="top") +
        theme(axis.title = element_text(size=16)) +
        theme(axis.text = element_text(size=16)) +
      comparisonR +
      textR

      print(p)
    })
    
    
    
    
    #******************************#
    #*** Downloads plots
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,"_vary_",input$variableY,Sys.time(),'.png', sep='') },
        content = function(file) {
            png(file, width=800, height=800,res=72)
            print(plotInput())
            dev.off()
        })
    
    output$downloadPlotT <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,"_vary_",input$variableY,Sys.time(),'.png', sep='') },
        content = function(file) {
            png(file, width=800, height=800,res=72)
            print(plotInputT())
            dev.off()
        })

    output$downloadPlotL <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,Sys.time(),'.png', sep='') },
        content = function(file) {
            png(file, width=800, height=800,res=72)
            print(plotInputL())
            dev.off()
        })
    
    output$downloadPlotR <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,Sys.time(),'.png', sep='') },
        content = function(file) {
            png(file, width=800, height=800,res=72)
            print(plotInputR())
            dev.off()
        })
    
    output$downloadPlotMap <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,Sys.time(),'.png', sep='') },
        content = function(file) {
            png(file, width=2500, height=2000,res=72)
            print(plotInputMap())
            dev.off()
        })
    
    ## preparing data for download
    
    datasetInput2 <- reactive({
        datPlot <- datasetInput()
        ## subset the continent
        if (input$continent == "All") datPlot <- datPlot
        if (input$continent != "All") datPlot <- datPlot[datPlot$contName == input$continent,]
    })
        
    output$downloadData <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,"_vary_",input$variableY,Sys.time(),'.csv', sep='') },
        content = function(file) {
            write.csv(datasetInput2(), file)
        }
    )
    
    output$downloadDataT <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,"_vary_",input$variableY,Sys.time(),'.csv', sep='') },
        content = function(file) {
            write.csv(datasetInputT(), file)
        }
    )
    
})