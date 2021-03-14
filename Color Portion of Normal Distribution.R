
library(shiny)
library(shinydashboard)
library(shinyjs)

######################################################################################
# js code to change background color of numeric fields
######################################################################################
jsCode <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'



######################################################################################
# function to change color for TP,TN,FP,FN using Polygon
######################################################################################
fncutoffcoloring <- function(MU,SD, Lower_limit, Upper_limit,fill.color){
  xcordinate <- c(Lower_limit,seq(Lower_limit,Upper_limit,0.01),Upper_limit)
  ycordinate <- c(0,dnorm(seq(Lower_limit,Upper_limit,0.01), mean = MU, sd = SD),0)
  polygon(xcordinate, ycordinate, col=fill.color)
}

######################################################################################
# ui function
######################################################################################
ui <- dashboardPage(
  dashboardHeader(title = "Normal Distribution"),
  dashboardSidebar(),
  dashboardBody(
    shinyjs::useShinyjs(),
    useShinyjs(),
    extendShinyjs(text = jsCode,functions = c('backgroundCol')),
    
    #to hider vertical scroll bar
    tags$head(
      tags$style(
        "body {overflow-y: hidden;}"
      )
    ),

    column(
      width = 12,
      align="center",
      HTML(paste('<h3><b>',"Coloring TP, FP, TN, FN in Normal Distribution",'</b><h5>')),
      box(
        width = 6,
        height = 425,
        plotOutput(outputId = "mcutoffplot",height = 400,width = "100%")
      ),
      box(
        width = 6,
        height = 425,
        align="center",
        # background = 'black',
        HTML(paste('<h5><u><b>',"Sequence & Other Parameters",'</b></u><h5>')),
        splitLayout(
          cellWidths = c("15%", "15%","15%","15%","15%","15%"),
          textInput(inputId = 'chart1title',label ="Name" ,value = "Chart 1"),
          shinyWidgets::autonumericInput(inputId = "xMin",label = "From",value = -6,digitGroupSeparator = ",", decimalPlaces = 2),
          shinyWidgets::autonumericInput(inputId = "xMax",label = "To",value = 7,digitGroupSeparator = ",", decimalPlaces = 2),
          shinyWidgets::autonumericInput(inputId = "xLength",label = "Seq Length",value = 1000,digitGroupSeparator = ",", decimalPlaces = 0),
          shinyWidgets::autonumericInput(inputId = "xMU",label = "Mean",value = -1,digitGroupSeparator = ",", decimalPlaces = 2),
          shinyWidgets::autonumericInput(inputId = "xSD",label = "SD",value = 1.5,digitGroupSeparator = ",", decimalPlaces = 2)
        ),
        splitLayout(
          cellWidths = c("15%", "15%","15%","15%","15%","15%"),
          textInput(inputId = 'chart2title',label =NULL ,value = "Chart 2"),
          shinyWidgets::autonumericInput(inputId = "yMin",label = NULL,value = -6,digitGroupSeparator = ",", decimalPlaces = 2),
          shinyWidgets::autonumericInput(inputId = "yMax",label = NULL,value = 7,digitGroupSeparator = ",", decimalPlaces = 2),
          shinyWidgets::autonumericInput(inputId = "yLength",label = NULL,value = 1000,digitGroupSeparator = ",", decimalPlaces = 0),
          shinyWidgets::autonumericInput(inputId = "yMU",label = NULL,value = 1,digitGroupSeparator = ",", decimalPlaces = 2),
          shinyWidgets::autonumericInput(inputId = "ySD",label = NULL,value = 1.5,digitGroupSeparator = ",", decimalPlaces = 2)
        ),
        sliderInput("mCutOffLine", "Cutoff Line",min = 0, max = 100, value = 0.6,step = .01,width = "90%"),
        HTML(paste('<h5><u><b>',"x and y coordinates for Chart Labelling (have options to modify)",'</b></u><h5>')),
        splitLayout(
          cellWidths = c("18%", "18%","18%","18%","18%"),
          textInput(inputId = 'xAxistitle',label ="Axis" ,value = "x Axis"),
          numericInput(inputId = 'xTPlabel',label = "TP",value = 1.5,step = 0.01),
          numericInput(inputId = 'xFPlabel',label = "FP",value = 0.65,step = 0.01),
          numericInput(inputId = 'xTNlabel',label = "TN",value = -1.5,step = 0.01),
          numericInput(inputId = 'xFNlabel',label = "FN",value = -0.30,step = 0.01)
        ),
        splitLayout(
          cellWidths = c("18%", "18%","18%","18%","18%"),
          textInput(inputId = 'yAxistitle',label = NULL ,value = "Y Axis"),
          numericInput(inputId = 'yTPlabel',label = NULL,value = 0.2,step = 0.01),
          numericInput(inputId = 'yFPlabel',label = NULL,value = 0.04,step = 0.01),
          numericInput(inputId = 'yTNlabel',label = NULL,value = 0.2,step = 0.01),
          numericInput(inputId = 'yFNlabel',label = NULL,value = .04,step = 0.01)
        )
      )
    )
  )#dashboardBody closure
)


server <- function(input, output,session) {
  ######################################################################################
  #Code to hide right side bar on load of dashboard as well as you click on the body
  ######################################################################################
  shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))

  
  ######################################################################################
  #just to disable certain text fields on the dashbaord
  ######################################################################################
  disable(id = "xAxistitle")
  disable(id = "yAxistitle")
  disable(id = "chart1title")
  disable(id = "chart2title")
  
  ######################################################################################
  #here is the reactive variable and ObserveEvent update values for the plot
  ######################################################################################
  vcutoff <- reactiveValues(x=NULL,f.x=NULL,y=NULL,f.y=NULL)
  observeEvent({
    input$xMin
    input$xMax
    input$yMin
    input$yMax
    input$xLength
    input$yLength
    input$xMU
    input$xSD
    input$yMU
    input$ySD
    input$mCutOffLine},{
      if (input$mCutOffLine>=0){
        updateNumericInput(session,inputId = 'xTPlabel',value = round(input$mCutOffLine+input$ySD/2,2))
        updateNumericInput(session,inputId = 'xTNlabel',value = round(input$mCutOffLine-input$xSD,2))
      }
      else{
        updateNumericInput(session,inputId = 'xTPlabel',value = round(input$mCutOffLine+input$ySD,2))
        updateNumericInput(session,inputId = 'xTNlabel',value = round(input$mCutOffLine-input$xSD/2,2))
      }
      updateNumericInput(session,inputId = 'xFPlabel',value = round(input$mCutOffLine+input$ySD/4,2))
      updateNumericInput(session,inputId = 'xFNlabel',value = round(input$mCutOffLine-input$ySD/4,2))
      updateSliderInput(session,inputId = "mCutOffLine",min =-1 ,max = 2,step = 0.01)
      
      
      vcutoff$x <- seq(from= input$xMin,to= input$xMax,length.out = input$xLength)
      vcutoff$f.x <- dnorm(vcutoff$x,mean = input$xMU,sd = input$xSD)
      vcutoff$y <- seq(from=input$yMin,to=input$yMax,length.out = input$yLength)
      vcutoff$f.y <-  dnorm(vcutoff$y,mean =input$yMU,sd = input$ySD)
      
      
      js$backgroundCol("xTPlabel","#ffd3d9")
      js$backgroundCol("xFPlabel","#ffd3d9")
      js$backgroundCol("xTNlabel","#ffd3d9")
      js$backgroundCol("xFNlabel","#ffd3d9")
      
      js$backgroundCol("yTPlabel","lightblue")
      js$backgroundCol("yFPlabel","lightblue")
      js$backgroundCol("yTNlabel","lightblue")
      js$backgroundCol("yFNlabel","lightblue")
      
      ######################################################################################
      #fixing margin for the plot
      ######################################################################################
      par(mar = c(2, 2, 1, 1))
      
    })
  

  output$mcutoffplot <- renderPlot({
    plot(vcutoff$x,vcutoff$f.x,type = 'l',col='#000000',lwd=1,xlab = "x",ylab = "y")+
      lines(vcutoff$y, vcutoff$f.y, col="#000000",lwd=1)+
      #True Negative
      fncutoffcoloring(MU = input$xMU,SD = input$xSD,Lower_limit = input$xMin,Upper_limit = input$mCutOffLine,fill.color = '#cbf3f3')+
      #False Negative
      fncutoffcoloring(MU = input$yMU,SD = input$ySD,Lower_limit = input$yMin,Upper_limit = input$mCutOffLine,fill.color = "#e5a3ad")+
      #True Positive
      fncutoffcoloring(MU = input$yMU,SD = input$ySD,Lower_limit = input$mCutOffLine,Upper_limit = input$yMax,fill.color = "#ffd3d9")+
      #False Positive
      fncutoffcoloring(MU = input$xMU,SD = input$xSD,Lower_limit = input$mCutOffLine,Upper_limit = input$yMax,fill.color = "#98c0c0")+
      abline(v= input$mCutOffLine,col='red',lwd=1)+
      text(x =  input$xTPlabel, y = input$yTPlabel, label = "TP", srt = 360,col = "black")+
      text(x =  input$xTNlabel, y = input$yTNlabel, label = "TN", srt = 360,col = "black")+
      text(x =  input$xFPlabel, y = input$yFPlabel, label = "FP", srt = 360,col = "black")+
      text(x =  input$xFNlabel, y = input$yFNlabel, label = "FN", srt = 360,col = "black")+
      lines(vcutoff$x, vcutoff$f.x, col="#000000",lwd=1)+
      lines(vcutoff$y, vcutoff$f.y, col="#000000",lwd=1)
    
  })
  
  

} #server closure


shinyApp(ui,server)