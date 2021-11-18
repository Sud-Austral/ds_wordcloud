

library(shiny)
library(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("rio")
library(DT)
library(wordcloud2)
library(readxl)
library(rio)

  


ui <- fluidPage(
  
  
  fluidRow(
    
    column(4),
    column(4, align="center",
            # img(height = 105, width = 350, src = "https://www.dataintelligence-group.com/Content/img/blanco-trans.png"),
            # img(height = 80, width = 340, src = "https://raw.githubusercontent.com/Sud-Austral/LOGOS-DATA/main/logo_dataIntelligence_normal.png"), 
    )),
    
    fluidRow(
      column(4),
      column(4, align="center",
            
           # h2(textOutput("titulo1"))
      
    )),
  
  sidebarLayout(
    
    
    mainPanel(
      br(),
      br(),
      br(),
      br(),
      br(),
      
      div(style="display: none;",textInput(inputId = "parrafo",label = "parrafo")),
      fluidRow(
        column(6),
        column(6, align="center", wordcloud2Output("distPlot", height = "500px")),
      ),
      
      fluidRow(
        # column(4, align="center", DTOutput("repetidos")),
        # column(4, align="center", DTOutput("frecuencia")),
        # column(4, align="center", plotOutput("frecuencia_plot")),
      )
      
      
    ),
    mainPanel()
  )
)


server <- function(input, output, session) {
  
  
  
  observeEvent(input$go, {
    toggle("div_tabla1")
  })
  
  observeEvent(input$go, {
    toggle("div_plot1")
  })
  
  
  
  fn_parrafo = reactive({
    
    if (length(input$parrafo) == 0) {
      
    } else {
      # link_1 <- import("https://raw.githubusercontent.com/VictorEnamorado/prueba_nube/main/buko2.xlsx")
      link_1 <- import("https://raw.githubusercontent.com/Sud-Austral/ds_insumos_wordcloud/main/buko2.xlsx")
      palabras <- import("https://raw.githubusercontent.com/Sud-Austral/ds_insumos_wordcloud/main/palabras.xlsx")
      palabras <- palabras$value  
      
      req(input$parrafo)
      direccion <- link_1[as.numeric(input$parrafo),2]
      options(encoding="utf-8")
      text <- read.delim2(direccion)
      
      texto <- paste0()
      for (i in 1:length(text)) {
        texto <- paste0(texto,text[i])
      } 
      texto_split = strsplit(texto, split=" ")
      texto_columnas = tibble(unlist(texto_split))
      names(texto_columnas)[1] <- "lista_palabras"
      lista <- texto_columnas$lista_palabras
      
      lista <- gsub("\\W", "", lista)
      lista <- gsub("\\d", "", lista)
      lista <- tolower(lista)
      lista <- as_tibble(lista)
      lista <- lista %>% 
        group_by(value) %>% 
        summarise(frequency = n()) 
      lista <- filter(lista, value != "")
      
      for (i in palabras) {
        lista <- filter(lista, value != i)
        
      }
      lista <- lista[with(lista, order(lista$frequency, decreasing = T )), ] # Orden directo
      lista <- lista[1:30,]
      
    #   req(input$parrafo)
    #   direccion <- link_1[as.numeric(input$parrafo),2] 
    #   options(encoding="utf-8")
    #   text <- read.delim2(direccion)
    #   
    #   texto <- paste0()
    #   for (i in 1:length(text)) {
    #     texto <- paste0(texto,text[i])
    #   }
    #   
    #   docs <- Corpus(VectorSource(texto))
    #   toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    #   docs <- tm_map(docs, toSpace, "/")
    #   docs <- tm_map(docs, toSpace, "@")
    #   docs <- tm_map(docs, toSpace, "\\|") 
    #   # docs <- tm_map(docs, content_transformer(tolower)) 
    #   docs <- tm_map(docs, removeNumbers) 
    #   docs <- tm_map(docs, removeWords, stopwords("english"))  
    #   
    #   
    #   # docs <- tm_map(docs, removeWords, c(palabras)) 
    #   
    #   
    #   docs <- tm_map(docs, removePunctuation) 
    #   docs <- tm_map(docs, stripWhitespace) 
    #   docs <- tm_map(docs, stemDocument)
    #   dtm <- TermDocumentMatrix(docs) 
    #   m <- as.matrix(dtm)
    #   v <- sort(rowSums(m),decreasing=TRUE)
    #   d <- tibble(word = names(v),freq=v)
    #   
    #   for (i in palabras) {
    #     d <- filter(d, word != i) 
    #   } 
    #   d <- d[1:30,]
    #   
    #   
    }
    
  })
  
  
  
  fn_titulo = reactive({
    
    if (length(input$parrafo) == 0) {
      
    } else { 
      # link_1 <- import("https://raw.githubusercontent.com/VictorEnamorado/prueba_nube/main/buko2.xlsx")
      link_1 <- import("https://raw.githubusercontent.com/Sud-Austral/ds_insumos_wordcloud/main/buko2.xlsx")
      req(input$parrafo) 
      titulo <- link_1[as.numeric(input$parrafo),1]
      
    }
    
  })
  
  
  output$titulo1 <- renderText({
    paste0(fn_titulo())
  })
  

  
  
  output$distPlot <- renderWordcloud2({  
    wordcloud2(data=fn_parrafo(),size = .7, shape = "rectangle",backgroundColor = '#ffffff') 
  })
  

  # output$repetidos <- renderDT({
  #   DT::datatable(fn_parrafo())
  # })
  
  #############################################################---- Tablas
  
  
 
  
  ########################
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['parrafo']])) {
      updateTextInput(session, "parrafo", value = query[['parrafo']])
    } 
    if (!is.null(query[['go']])) {
      updateTextInput(session, "go", value = query[['go']])
    } 
  })
  ########################
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
