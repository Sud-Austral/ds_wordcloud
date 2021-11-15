
library(shiny)
library(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(DT)
library(wordcloud2)
library(memoise)
library(plotly)
library(shinyjs)
##################################################################################################################

shinyServer(function(input, output, session) {
  
  
  
  observeEvent(input$go, {
    toggle("div_tabla1")
  })
  
  observeEvent(input$go, {
    toggle("div_plot1")
  })
  
  
  
  fn_parrafo = reactive({
    
    if (length(input$parrafo) == 0) {
      
    } else {       
      link_1 <- import("https://raw.githubusercontent.com/VictorEnamorado/prueba_nube/main/buko2.xlsx")
      req(input$parrafo)
      direccion <- link_1[as.numeric(input$parrafo),2] 
      options(encoding="utf-8")
      text <- read.delim2(direccion)
      
      texto <- paste0()
      for (i in 1:length(text)) {
        texto <- paste0(texto,text[i])
      }
      
      docs <- Corpus(VectorSource(texto))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|") 
      docs <- tm_map(docs, content_transformer(tolower)) 
      docs <- tm_map(docs, removeNumbers) 
      docs <- tm_map(docs, removeWords, stopwords("english"))  
      # Artículos
      docs <- tm_map(docs, removeWords, c("el","la","los","las","un","una","unos","unas","del" ))
      # Conjunciones
      docs <- tm_map(docs, removeWords, c("y", "e", "ni", "que", "pero", "mas", "aunque", "sino", "siquiera", "o", "u", "ora", "sea",
                                          "bien", "ya", "cerca","lejos", "éste","aquél", "pues", "porque", "puesto", "ya que",
                                          "como", "más","que", "igual que", "menos","que", "así","como", "Si", "tal",  "dado", "siempre",
                                          "aunque", "pesar", "si", "por más que", "lo",
                                          "así", "luego", "tan", "conque", "fin",
                                          "Cuando", "mientras", "antes", "apenas", "cuanto" ))
      # preposiciones
      docs <- tm_map(docs, removeWords, c("a","durante","según","ante","en","sin","bajo","entre","so","cabe","hacia",
                                          "sobre","con","hasta","tras","contra","mediante","versus","de","para","vía","desde","por"	)) 
      docs <- tm_map(docs, removePunctuation) 
      docs <- tm_map(docs, stripWhitespace) 
      docs <- tm_map(docs, stemDocument)
      dtm <- TermDocumentMatrix(docs) 
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      
    }
    
  })
  
  
  
  fn_titulo = reactive({
    
    if (length(input$parrafo) == 0) {
      
    } else { 
      link_1 <- import("https://raw.githubusercontent.com/VictorEnamorado/prueba_nube/main/buko2.xlsx")
      req(input$parrafo) 
      titulo <- link_1[as.numeric(input$parrafo),1]
      
    }
    
  })
  
  
  output$titulo1 <- renderText({
    paste0(fn_titulo())
  })
  
  
  
  output$distPlot <- renderWordcloud2({ 
    
    wordcloud2(data=fn_parrafo(),size = 1,  color = 'random-light',backgroundColor = '#ffffff')
    
    
  })
  
  
  
  #############################################################---- Tablas
  
 
  v <- reactiveValues(doPlot = FALSE) #<- variable reactiva
  
  observeEvent(input$go, { 
    v$doPlot <- TRUE
  })
  
  output$repetidos <- renderDT({
    if (v$doPlot == FALSE) return()
    
    isolate({
      
      
      
      # output$frecuencia_plot <- renderPlot({
      output$frecuencia_plot <- renderPlotly({
        
 
        fig <- plot_ly(
          x = fn_parrafo()[1:10,]$word,
          y = fn_parrafo()[1:10,]$freq,
          name = "Palabras",
          type = "bar"
        )
        
        fig
        
        
        
        
      })
      
      
      fn_parrafo()
    })
  })
  
  
  
  
  
  output$frecuencia <- renderDT({
    
  })
  
  
  
  
  #############################################################
  
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
  
  
  
})
