 
library(shiny)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        set.seed(1234)
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))

    })
    
    
    output$repetidos <- renderDT({
        
        tabla1 <- data.frame(findFreqTerms(dtm, lowfreq = 3))
        names(tabla1)[1] <- "Palabras_repetidas"
        tabla1
        
    })
    
    output$frecuencia <- renderDT({
        
        d
        
    })
    
    
    
    

})
