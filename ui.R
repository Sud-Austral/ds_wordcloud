

library(shiny)
library(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("rio")
library(DT)
library(wordcloud2)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  
  fluidRow(
    column(4),
    column(2, align="center",
           
           # sliderInput("anio",
           #             "",
           #             min = 2002,
           #             max = 2021,
           #             sep = "",
           #             value = 2002,
           #             # width = "900px",
           #             animate = animationOptions(interval = 1000,loop = TRUE))
           # fileInput("csvs", label = "Subir serie de tiempo en formato .csv", multiple = TRUE),
           titlePanel(h1("Wordcloud")),
    ),
    column(2, align="center",
           
           # img(height = 105, width = 350, src = "https://www.dataintelligence-group.com/Content/img/blanco-trans.png"),
           img(height = 85, width = 340, src = "https://raw.githubusercontent.com/Sud-Austral/LOGOS-DATA/main/logo_dataIntelligence_normal.png"),
    ), column(4)),
  
  # titlePanel("Mineria de datos"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    # Show a plot of the generated distribution
    mainPanel(
      
      div(style="display: none;",textInput(inputId = "parrafo",label = "parrafo")),
      fluidRow(
        column(6),
        column(6, align="center", wordcloud2Output("distPlot", height = "500px" )),
      ),
      fluidRow(
        column(4, align="center", DTOutput("repetidos")),
        column(4, align="center", DTOutput("frecuencia")), 
        column(4, align="center", plotOutput("frecuencia_plot")), 
      )
      
      
    ),
    mainPanel()
  )
))
