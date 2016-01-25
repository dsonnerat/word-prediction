library(shiny)
library(NLP)
library(tm)
options( java.parameters = "-Xmx4g" )
library(RWeka)
library(slam)

load('nGrams')
load('badWords')

nbWords <- sum(nGrams[[1]]$freq)

endWithSpace <- function(chars = ''){
        nbChar  <- nchar(chars)
        lastChar <- substr(chars,nbChar,nbChar)
        lastChar == ' '
}

splitWords <- function(words){
        simplify2array(strsplit(words, ' ', fixed =  TRUE))
}

concat <- function(words){
        
        con <- ''
        
        for(i in seq(along.with= words)){
                con <- paste(con, words[i], sep = ' ')
        }
        
        leng <- nchar(con)
        
        if(leng > 0){
                con <- substr(con, 2, leng)
        }
        con
}

removeFirstWord <-  function(words, len){
        if(len==1){
                ws <- NULL
        }else{
                ws <- words[2:len]
        }
        
        ws
}

wordScore <- function(word, precedingWords = NULL){
        n = length(precedingWords)
        
        if(n > 0){
                
                nGram <- nGrams[[n + 1]]
                freqNGram <- nGram$freq[nGram$grams == concat(c(precedingWords, word))]
                
                
                if(length(freqNGram) > 0 && freqNGram > 0){
                        nMinusOneGram <- nGrams[[n]]
                        freqNMinusOneGram <- nMinusOneGram$freq[nMinusOneGram$grams == concat(precedingWords)]
                        score <- freqNGram / freqNMinusOneGram
                        
                }else{
                        score <- 0.4 * wordScore(word, removeFirstWord(precedingWords, n))
                }
        }else{
                score <- nGrams[[1]]$freq[nGrams[[1]]$grams == word] / (nbWords * 100000)
        }    
        
        score
}

scorePerWord <- function(precedingWords){
        sapply(levels(nGrams[[1]]$grams), FUN = function(word){wordScore(word, precedingWords)})
}

removeBadWords <- function(x){removeWords(x, as.character(badWords[[1]]))}

transFuns <- list(removePunctuation,
                  removeNumbers,
                  content_transformer(tolower),
                  removeBadWords,
                  stripWhitespace)

predictNextWord <- function(sentence){
        coprusTyped <- Corpus(VectorSource(sentence))
        typedClean <-  tm_map(coprusTyped, FUN = tm_reduce, tmFuns = transFuns)
        
        if(length(typedClean) == 0 || nchar(typedClean[[1]]$content) == 0){
                return(rep('',3))
        }
        
        words <- splitWords(typedClean[[1]]$content)
        nbWords <- length(words)
        
        if(length(words) > 3){
                words <- words[(nbWords - 2) : nbWords]
        }
        
        scores <- scorePerWord(words)
        levels(nGrams[[1]]$grams)[order(scores, decreasing = TRUE)][1:3]
}

shinyServer(function(input, output, session) {
        
        predictedWords <- reactive({
                if(endWithSpace(input$text)){
                        words <- predictNextWord(input$text)
                }else{
                        words <- c('','','')
                }
                
                words
        })
        
        updateText <- function(wordIndex){
                updateTextInput(session, 'text', value = paste(input$text, predictedWords()[wordIndex], ' ', sep = ''))
        }
        
        observeEvent(input$word1Insertion, {
                updateText(1)
        }) 
        
        observeEvent(input$word2Insertion, {
                updateText(2)
        }) 
        
        observeEvent(input$word3Insertion, {
                updateText(3)
        }) 
        
        output$word1 <- renderText({
                predictedWords()[1]
        })
        
        output$word2 <- renderText({
                predictedWords()[2]
        })
        
        output$word3 <- renderText({
                predictedWords()[3]
        })
})
