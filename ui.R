
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Inventory Replenishment Alarming System"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$style(type="text/css", "
             #loadmessage {
                 position: fixed;
                 top: 0px;
                 left: 0px;
                 width: 100%;
                 padding: 5px 0px 5px 0px;
                 text-align: center;
                 font-weight: bold;
                 font-size: 100%;
                 color: #FFFFFF;
                 background-color: #289dd7;
                 z-index: 105;
                 }
                 "),
      fileInput ("file1","上傳庫存表格-限CSV格式"),
      hr(),
      fileInput("file2","上傳銷售表格-限CSV格式"),
      hr(),
      sliderInput("conservativepara","保守係數",0.5,1,step = 0.1,value = 0.7),
      actionButton("act","Run"),
      downloadButton("downloadthis","Download"),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Processing...",id="loadmessage")
                       
      )
      ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
            tabPanel("contents",dataTableOutput("contents")),
            tabPanel("recommendations",dataTableOutput("recommendations"))         
                     )
          ) 
  )
))
