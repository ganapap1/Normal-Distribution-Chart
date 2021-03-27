
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(stringr) #to remove perentage from string and for other string functions
library(kableExtra) # to show HTML Table
library(shinyalert)

######################################################################################
## js code to change background color of numeric fields
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
##Button formatting function
######################################################################################

styleButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:#4682B4;
                        border-color: #ffffff;
                        border-width:3px;
                        height:35px;
                        width:150px;
                        font-size: 13px;"
}



styletinyButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:#4682B4;
                        border-color: #ffffff;
                        border-width:2px;
                        height:25px;
                        width:40px;
                        font-size: 13px;"
}


buttonInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}




######################################################################################
## function to change color for TP,TN,FP,FN using Polygon
## Thanks to Christopher DeSante for this polygon coloring function posted in 
## https://gist.github.com/cdesante/3750663. I have modified to suit plotly
######################################################################################

fnaddpolygonplotly <- function(MU,SD, Lower, Upper,dataLength,fill.color,Fig){
  cord.x <- c(Lower,seq(from = Lower,to = Upper,length.out = dataLength),Upper)
  cord.y <- c(0,dnorm(seq(from = Lower,to = Upper,length.out = dataLength ), mean = MU, sd = SD),0)
  add_polygons(p = Fig,x = cord.x, y = cord.y, fillcolor=fill.color,
               line=list(width=1,color="black"))
}



######################################################################################
## function Percentage of overlapping regions of two normal distributions
## Thanks to the contribution in stackexchange.com
##https://stats.stackexchange.com/questions/12209/percentage-of-overlapping-regions-of-two-normal-distributions
######################################################################################
get_overlap_coef <- function(mu1, mu2, sd1, sd2){
  xs  <- seq(min(mu1 - 4*sd1, mu2 - 4*sd2),
             max(mu1 + 4*sd1, mu2 + 4*sd2),
             length.out = 1000)
  f1  <- dnorm(xs, mean=mu1, sd=sd1)
  f2  <- dnorm(xs, mean=mu2, sd=sd2)
  int <- xs[which.max(pmin(f1, f2))]
  l   <- pnorm(int, mu1, sd1, lower.tail = mu1>mu2)
  r   <- pnorm(int, mu2, sd2, lower.tail = mu1<mu2)
  l+r
}


######################################################################################
## Display as percentage with 1 decimals
######################################################################################
fnpercentZeroDigit <- function(x, digits = 0, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}



######################################################################################
## ui 
######################################################################################

ui <- dashboardPage(
  dashboardHeader(
    title = "Normal Distribution"
  ),
  
  sidebar <- dashboardSidebar(
    collapsed = TRUE
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    useShinyjs(),
    extendShinyjs(text = jsCode,functions = c('backgroundCol')),
    useShinyalert(),
    
    # #to hider vertical scroll bar
    # tags$head(
    #   tags$style(
    #     "body {overflow-y: hidden;}"
    #   )
    # ),
    align="center",
    HTML(paste('<h3>', '<p style="font-size:18px; color:#000000; font-family:Tahoma;">',"Coloring Portions of Normal Distribution & Probability of shaded area",'</p>','</b><h5>')),
    
    fluidRow(
      column(
        width = 12,
        # align="center",
        # HTML(paste('<h3>', '<p style="font-size:18px; color:#000000; font-family:Tahoma;">',"Coloring Portions of Normal Distribution & Probability of shaded area",'</p>','</b><h5>')),
        # 
        box(
          width = 6,
          height = 450,
          title = "Normal Distribution - Dual Chart",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          align ='center',
          plotlyOutput(outputId = "mcutoffplot",height = 350,width = "100%")
        ),
        
        box(
          width = 6,
          height = 450,
          title = "Sequence & Other Parameters",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          align ='center',
          # 
          # HTML(paste('<h5><u><b>',"Sequence & Other Parameters",'</b></u><h5>')),
          splitLayout(
            cellWidths = c("15%", "15%","15%","15%","15%","15%"),
            textInput(inputId = 'chart1title',label ="Name" ,value = "Chart 1"),
            shinyWidgets::autonumericInput(inputId = "xMin",label = "From",value = -6,digitGroupSeparator = ",", decimalPlaces = 2),
            shinyWidgets::autonumericInput(inputId = "xMax",label = "To",value = 7,digitGroupSeparator = ",", decimalPlaces = 2),
            shinyWidgets::autonumericInput(inputId = "Length",label = "Seq Length",value = 1000,digitGroupSeparator = ",", decimalPlaces = 0),
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
          
          HTML(paste('<br><h5><u><b>',"x and y coordinates for Chart Labelling (have options to modify)",'</b></u><h5>')),
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
          ),
          noUiSliderInput(
            inputId = "mCutOffLine", label = NULL,
            min = -1, max = 2, value = 0,
            tooltips = TRUE, step = .01,
            direction = 'ltr', 
            orientation = "horizontal", 
            width = "100%", 
            height = "20px",
            format = wNumbFormat(decimals = 2 )
          ),
          HTML(paste('<h5><b>',"Move the slider to see cutoff A and B; and their impact",'</b><h5>')),
          
        )#box closure
      )#column closure
    ), #fluidrow closure
    fluidRow(
      column(
        width = 12,
        box(
          width = 6,
          height = 475,
          title = "Confusion Matrix at both Cutoffs",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          HTML(paste('<h5>',"probability of area under each portion of the curve is shown as counts in the matrix")),
          HTML(paste('<h5><u><b>',"Cutoff A",'</b></u><h5>')),
          htmlOutput('mcutoff_A_matrix'),
          HTML(paste('<h5><u><b>',"Cutoff B",'<FONT COLOR="#E51616">', "(Current)",'</b></u><h5>')),
          htmlOutput('mcutoff_B_matrix')  
        ),
        box(
          width = 6,
          height = 475,
          title = "Impact of Cut off move from A to B",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          # br(),
          # br(),
          plotlyOutput('mshowmatrixplot', width = "100%",height = 425)
        )
        
      )# column closure
    )# fluidrow closure
  ) #dashboardBody closure
) #dashboardPage closure


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
  disable(id = "yLength")
  
  
  ######################################################################################
  #here is the reactive variable and ObserveEvent update values for the plot
  ######################################################################################
  vc <- reactiveValues(x=NULL,f.x=NULL,y=NULL,f.y=NULL,tpval=NULL,fpval=NULL,tnval=NULL,fnval=NULL,data=NULL,rr=NULL,tt=NULL,zz=NULL,mm=NULL,pp=NULL)
  observeEvent({
    input$xMin
    input$xMax
    input$yMin
    input$yMax},{
      updateNoUiSliderInput(session,inputId = "mCutOffLine",range =c(round(input$xMin/4,0),round(input$yMax/4,0)) )
    })
  
  vc$rr  <- data.frame(TP1=0,FP1=0,TN1=0,FN1=0,TP2=0,FP2=0,TN2=0,FN2=0,Cutoff1=0,Cutoff2=0)
  
  
  Cutoff_A <- c(0,0,0,0)
  Cutoff_B <- c(0,0,0,0)
  Impact_   <- c(0,0,0,0)
  Header_  <- c("PPV","NPV","Sensitivity","Specificity")
  vc$tt     <- data.frame(Header_,Cutoff_A,Cutoff_B,Impact_)
  
  
  fnupdateNumericInput <- function(xmTPfactor,  xmTNfactor,  ymTPfactor, ymTNfactor,  xmFPfactor,  xmFNfactor, ymFPfactor, ymFNfactor ){
    updateNumericInput(session,inputId = 'xTPlabel',value = round(input$mCutOffLine+input$ySD*xmTPfactor,2))
    updateNumericInput(session,inputId = 'xTNlabel',value = round(input$mCutOffLine-input$xSD*xmTNfactor,2))
    updateNumericInput(session,inputId = 'yTPlabel',value = ymTPfactor)
    updateNumericInput(session,inputId = 'yTNlabel',value = ymTNfactor)
    updateNumericInput(session,inputId = 'xFPlabel',value = round(input$mCutOffLine+input$ySD*xmFPfactor,2))
    updateNumericInput(session,inputId = 'xFNlabel',value = round(input$mCutOffLine-input$ySD*xmFNfactor,2))
    updateNumericInput(session,inputId = 'yFPlabel',value = ymFPfactor)
    updateNumericInput(session,inputId = 'yFNlabel',value = ymFNfactor)
  }
  
  
  observeEvent({
    input$xMin
    input$xMax
    input$yMin
    input$yMax
    input$Length
    input$xMU
    input$xSD
    input$yMU
    input$ySD
    input$mCutOffLine},{
      if (input$mCutOffLine>=1 || input$mCutOffLine<=-1 ){
        xmTPfactor   <- ifelse(input$mCutOffLine>=1,0.40,2.40)
        xmTNfactor   <- ifelse(input$mCutOffLine>=1,2.40,0.40)
        ymTPfactor   <- 0.11
        ymTNfactor   <- 0.11
        xmFPfactor   <- 0.25
        xmFNfactor   <- 0.25
        ymFPfactor   <- 0.01
        ymFNfactor   <- 0.01
        fnupdateNumericInput (xmTPfactor,  xmTNfactor,  ymTPfactor, ymTNfactor,  xmFPfactor,  xmFNfactor, ymFPfactor, ymFNfactor )
      }
      else {
        xmTPfactor   <- ifelse(input$mCutOffLine>=0 & input$mCutOffLine<=1,0.75,1.30)
        xmTNfactor   <- ifelse(input$mCutOffLine>=0 & input$mCutOffLine<=1,1.30,0.75)
        ymTPfactor   <- 0.15
        ymTNfactor   <- 0.15
        xmFPfactor   <- 0.25
        xmFNfactor   <- 0.25
        ymFPfactor   <- 0.04
        ymFNfactor   <- 0.04
        fnupdateNumericInput (xmTPfactor,  xmTNfactor,  ymTPfactor, ymTNfactor,  xmFPfactor,  xmFNfactor, ymFPfactor, ymFNfactor )
      }
      
      shinyWidgets::updateAutonumericInput(session,inputId = 'yLength',value = input$Length)
      
      vc$x      <- seq(from= input$xMin,to= input$xMax,length.out = input$Length)
      vc$f.x    <- dnorm(vc$x,mean = input$xMU,sd = input$xSD)
      vc$y      <- seq(from=input$yMin,to=input$yMax,length.out = input$Length)
      vc$f.y    <-  dnorm(vc$y,mean =input$yMU,sd = input$ySD)
      vc$data   <- data.frame(x=vc$x,trace_0=vc$f.x , trace_1=vc$f.y)
      
      
      vc$fnval  <-pnorm(q = input$mCutOffLine,mean = input$yMU,sd = input$ySD)
      vc$fpval  <- 1- pnorm(q = input$mCutOffLine,mean = input$xMU,sd = input$xSD)
      if (input$mCutOffLine>=0){
        vc$tnval  <- 1- get_overlap_coef(mu1=input$xMU, mu2=input$yMU, sd1=input$xSD, sd2=input$ySD)
        vc$tpval <- 1-(vc$fnval+vc$fpval)
      }
      else{
        vc$tnval  <- 1-(vc$fnval+vc$fpval)
        vc$tpval  <- 1- get_overlap_coef(mu1=input$xMU, mu2=input$yMU, sd1=input$xSD, sd2=input$ySD)
      }
      xxgtotal  <-(vc$fnval+vc$fpval+vc$tnval+vc$tpval)
      vc$fnval  <- fnpercentZeroDigit(vc$fnval/xxgtotal)
      vc$fpval  <- fnpercentZeroDigit(vc$fpval/xxgtotal)
      vc$tnval  <- fnpercentZeroDigit(vc$tnval/xxgtotal)
      vc$tpval  <- fnpercentZeroDigit(vc$tpval/xxgtotal)
      
      
      if (sum(abs(vc$rr[1:4]))==0){
        vc$rr[1,1]    <- c(as.numeric(str_sub(vc$tpval, end=-2)))
        vc$rr[1,2]    <- c(as.numeric(str_sub(vc$fpval, end=-2)))
        vc$rr[1,3]    <- c(as.numeric(str_sub(vc$tnval, end=-2)))
        vc$rr[1,4]    <- c(as.numeric(str_sub(vc$fnval, end=-2)))
        vc$rr[1,9]    <- c(as.numeric(input$mCutOffLine))
        vc$rr[1,10]    <- c(as.numeric(input$mCutOffLine))
        
      }
      
      
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
  
  t<-list(
    family = "Tahoma",
    size = 15,
    color = toRGB("black"))
  f <-list(
    family = "Tahoma",
    size = 14,
    color = "red"
  )
  
  output$mcutoffplot <- renderPlotly({
    fig <- plot_ly(vc$data, x = ~x, y = ~trace_0, name = 'trace 0', 
                   type = 'scatter', mode = 'lines',showlegend = FALSE) 
    fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines')
    fig <- fig %>% layout(
      xaxis = list(
        title = HTML(paste("Probability of","TP:", vc$tpval,";  ","FP:",vc$fpval,";  ",'<br>',"FN:",vc$fnval,";  ","TN:",vc$tnval )),
        titlefont = f,
        showticklabels = TRUE,
        exponentformat = "E",
        showgrid = FALSE,
        showline = T,
        tickformat = ".1f"
      ),
      yaxis = list(
        title = "",
        titlefont = f,
        showticklabels = TRUE,
        exponentformat = "E",
        showgrid = ifelse(input$mxygrid==TRUE,TRUE,FALSE),
        showline = T,
        tickformat = ".2f"
      )
      
    )
    
    #True Negative
    fig <- fnaddpolygonplotly(MU = input$xMU,SD = input$xSD,Lower = input$xMin,Upper = input$mCutOffLine,dataLength = input$Length,fill.color = '#cbf3f3',Fig = fig)
    #False Negative
    fig <- fnaddpolygonplotly(MU = input$yMU,SD = input$ySD,Lower = input$yMin,Upper = input$mCutOffLine,dataLength = input$Length,fill.color = "#e5a3ad",Fig = fig)
    #True Positive
    fig <- fnaddpolygonplotly(MU = input$yMU,SD = input$ySD,Lower = input$mCutOffLine,Upper = input$yMax,dataLength = input$Length,fill.color = "#ffd3d9",Fig = fig)
    #False Positive
    fig <- fnaddpolygonplotly(MU = input$xMU,SD = input$xSD,Lower = input$mCutOffLine,Upper = input$yMax,dataLength = input$Length,fill.color = "#98c0c0",Fig = fig)
    
    
    fig <-  add_lines(p = fig,x =vc$rr[1,9],y = NULL,z = NULL, color = I("black"))
    fig <-  add_text(p = fig,x =  vc$rr[1,9], y =max(vc$data$trace_0)*1.02, text = "A", textfont = t,color = I("black"))
    
    fig <-  add_lines(p = fig,x =vc$rr[1,10],y = NULL,z = NULL, color = I("red"))
    fig <-  add_text(p = fig,x =  vc$rr[1,10], y =max(vc$data$trace_0)*1.02, text = "B", textfont = t,color = I("red"))
    
    fig <-  add_text(p = fig,x =  input$xTPlabel, y = input$yTPlabel, text = "TP", textfont = t)
    fig <-  add_text(p = fig,x =  input$xTNlabel, y = input$yTNlabel, text = "TN", textfont = t)
    fig <-  add_text(p = fig,x =  input$xFPlabel, y = input$yFPlabel, text = "FP", textfont = t)
    fig <-  add_text(p = fig,x =  input$xFNlabel, y = input$yFNlabel, text = "FN", textfont = t)
    
    fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines')
    fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 1', mode = 'lines')
    
    fig
  })
  
  
  observeEvent(input$mCutOffLine,{
    if (sum(abs(vc$rr[5:8]))==0){
      vc$rr[1,5]    <- c(as.numeric(str_sub(vc$tpval, end=-2)))
      vc$rr[1,6]    <- c(as.numeric(str_sub(vc$fpval, end=-2)))
      vc$rr[1,7]    <- c(as.numeric(str_sub(vc$tnval, end=-2)))
      vc$rr[1,8]    <- c(as.numeric(str_sub(vc$fnval, end=-2)))
      vc$rr[1,9]    <- c(as.numeric(input$mCutOffLine))
      vc$rr[1,10]    <- c(as.numeric(input$mCutOffLine))
    }
    else if (sum(abs(vc$rr[5:8]))!=0){
      vc$rr[1,1]    <<- vc$rr[1,5] 
      vc$rr[1,2]    <<- vc$rr[1,6] 
      vc$rr[1,3]    <<- vc$rr[1,7] 
      vc$rr[1,4]    <<- vc$rr[1,8]
      vc$rr[1,9]    <<- vc$rr[1,10]
      
      vc$rr[1,5]    <- c(as.numeric(str_sub(vc$tpval, end=-2)))
      vc$rr[1,6]    <- c(as.numeric(str_sub(vc$fpval, end=-2)))
      vc$rr[1,7]    <- c(as.numeric(str_sub(vc$tnval, end=-2)))
      vc$rr[1,8]    <- c(as.numeric(str_sub(vc$fnval, end=-2)))
      vc$rr[1,10]    <- c(as.numeric(input$mCutOffLine))
      
    }
    
    
    Cutoff_A <- c(0,0,0,0)
    Cutoff_B <- c(0,0,0,0)
    Impact_   <- c(0,0,0,0)
    Header_  <- c("PPV","NPV","Sensitivity","Specificity")
    vc$tt     <- data.frame(Header_,Cutoff_A,Cutoff_B,Impact_)
    
    vc$tt[1,2]  <- round(vc$rr$TP1 / (vc$rr$TP1 + vc$rr$FP1),4)  # PPV1
    vc$tt[2,2]  <- round(vc$rr$TN1 / (vc$rr$FN1 + vc$rr$TN1),4)  #mNPV1
    vc$tt[3,2]  <- round(vc$rr$TP1 / (vc$rr$TP1 + vc$rr$FN1),4)  #mSensitivity1
    vc$tt[4,2]  <- round(vc$rr$TN1 / (vc$rr$FP1 + vc$rr$TN1),4)  #mSpecificity1
    
    vc$tt[1,3]  <- round(vc$rr$TP2 / (vc$rr$TP2 + vc$rr$FP2),4) # PPV2
    vc$tt[2,3]  <- round(vc$rr$TN2 / (vc$rr$FN2 + vc$rr$TN2),4) #mNPV2
    vc$tt[3,3]  <- round(vc$rr$TP2 / (vc$rr$TP2 + vc$rr$FN2),4) #mSensitivity2
    vc$tt[4,3]  <- round(vc$rr$TN2 / (vc$rr$FP2 + vc$rr$TN2),4) #mSpecificity2
    
    for (c in 1:nrow(vc$tt)){
      
      vc$tt[c,4] <- buttonInput(
        FUN = actionButton,
        len = 1,
        id = 'button_',
        label = "",
        onclick = 'Shiny.onInputChange(\"lastClick\",  this.id)',
        if(vc$tt[c,2]  == vc$tt[c,3]){
          icon = icon("equals",lib = "font-awesome")
        }
        else if(vc$tt[c,2]  >vc$tt[c,3]){
          icon = icon("arrow-alt-circle-down",lib = "font-awesome")
        }
        else{
          icon = icon("arrow-alt-circle-up",lib = "font-awesome")
        },
        style="white-space: normal; text-align:center;color: #ffffff; background-color:#4682B4;border-color: #ffffff;border-width:2px;height:40px;width:45px;font-size: 20px;",
        
        
      )
      
    }
    
    vc$zz <- vc$tt
    for (i in names(vc$zz)[c(-1,-4)]){
      vc$zz[,i] <- fnpercentZeroDigit(vc$zz[,i])
      
    }
    
    Disease <- c(0,0,0)
    No_disease <- c(0,0,0)
    Total_   <- c(0,0,0)
    Header_  <- c("Exposed","Unexposed","Total->")
    vc$mm     <- data.frame(Header_,Disease,No_disease,Total_)
    
    
    vc$mm[1,2]<<- vc$rr$TP1
    vc$mm[1,3]<<- vc$rr$FP1
    vc$mm[1,4]<<- vc$rr$TP1 + vc$rr$FP1
    vc$mm[2,2]<<- vc$rr$FN1
    vc$mm[2,3]<<- vc$rr$TN1
    vc$mm[2,4]<<- vc$rr$FN1 + vc$rr$TN1
    vc$mm[3,2]<<- vc$rr$TP1 + vc$rr$FN1
    vc$mm[3,3]<<- vc$rr$FP1 + vc$rr$TN1
    vc$mm[3,4]<<-  vc$rr$TP1 + vc$rr$FN1 + vc$rr$FP1 + vc$rr$TN1
    
    
    Disease <- c(0,0,0)
    No_disease <- c(0,0,0)
    Total_   <- c(0,0,0)
    Header_  <- c("Exposed","Unexposed","Total->")
    vc$pp     <- data.frame(Header_,Disease,No_disease,Total_)
    
    
    vc$pp[1,2]<<- vc$rr$TP2
    vc$pp[1,3]<<- vc$rr$FP2
    vc$pp[1,4]<<- vc$rr$TP2 + vc$rr$FP2
    vc$pp[2,2]<<- vc$rr$FN2
    vc$pp[2,3]<<- vc$rr$TN2
    vc$pp[2,4]<<- vc$rr$FN2 + vc$rr$TN2
    vc$pp[3,2]<<- vc$rr$TP2 + vc$rr$FN2
    vc$pp[3,3]<<- vc$rr$FP2 + vc$rr$TN2
    vc$pp[3,4]<<-  vc$rr$TP2 + vc$rr$FN2 + vc$rr$FP2 + vc$rr$TN2
    
  })
  
  
  output$mcutoff_A_matrix <- renderUI({
    
    n<- ncol(vc$mm)
    HTML( kbl(vc$mm,format = "html", escape = F,caption ="",align=c(rep('c',times=n)),digits = 0,
              table.attr = "style='width:120%;'")%>%
            kable_styling(font_size = 14, position = "center", html_font = "Cambria",fixed_thead = T) %>%
            kable_paper("striped", full_width = T) %>%
            column_spec(c(1:n), color = "white",background = 'black',width = "5em",bold = F,border_left = TRUE,border_right = TRUE)%>%
            row_spec(0, angle = 360,bold=TRUE,background = "black",color = "white")
    )
  })
  
  output$mcutoff_B_matrix <- renderUI({
    
    n<- ncol(vc$pp)
    HTML( kbl(vc$pp,format = "html", escape = F,caption ="",align=c(rep('c',times=n)),digits = 0,
              table.attr = "style='width:120%;'")%>%
            kable_styling(font_size = 14, position = "center", html_font = "Cambria",fixed_thead = T) %>%
            kable_paper("striped", full_width = T) %>%
            column_spec(c(1:n), color = "white",background = 'black',width = "5em",bold = F,border_left = TRUE,border_right = TRUE)%>%
            row_spec(0, angle = 360,bold=TRUE,background = "black",color = "white")
    )
  })
  
  
  output$mshowmatrixplot <- renderPlotly({
    Statistics <- c("PPV","NPV","Sensitivity","Specificity")
    Cutoff_A <- c(vc$tt$Cutoff_A)
    Cutoff_B <- c(vc$tt$Cutoff_B)
    
    xform <- list(categoryorder = "array",
                  categoryarray = Statistics)
    
    fig <- plot_ly(vc$tt, x = ~Statistics, y = ~Cutoff_A*100, type = 'bar', marker = list(color = '#008080F'),
                   name = 'Cutoff A',text = scales::percent(Cutoff_A,accuracy = 0.1 ), textposition = 'auto')
    
    fig <- fig %>% add_trace(y = ~Cutoff_B*100, name = 'Cutoff B',text = scales::percent(Cutoff_B,accuracy = 0.1),
                             textposition = 'auto',marker = list(color = '#FFA600'))
    fig <- fig %>% layout(yaxis = list(title = ""),xaxis = list(title = ""), barmode = 'group')
    fig <- fig %>% layout(legend = list(orientation = "h",xanchor = "center",x = 0.5))             # put legend in center of x-axis
    fig <- fig %>% layout(yaxis = list(ticksuffix = "%"))
    fig <- fig %>% layout(showlegend = TRUE, hoverlabel = list(bgcolor = "white"), bargap = 0.4)
    fig <- fig %>% layout(xaxis = xform)
    
    fig
    
    
  })
  
  
  
} #server closure


shinyApp(ui,server)