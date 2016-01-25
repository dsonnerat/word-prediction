# Reduction des fichiers awk '{if(NR % 100 == 0) print $0}' test.txt
library(NLP)
library(tm)
options( java.parameters = "-Xmx4g" )
library(RWeka)
library(slam)

### Loading and splitting data

if(!file.exists('dataTrain') || !file.exists('dataTest') || !file.exists('dataValidation')){

        ## Reading data from 'en_US-reduit-2'
        dataDir <- DirSource(directory = 'en_US-reduit', mode = 'text', encoding = 'UTF-8')
        data <- Corpus(dataDir, readerControl = list(reader = readPlain, language = 'en_US', load = TRUE))
        
        ## Splitting data into one document per line
        singleChar <- c(data[[1]]$content, data[[2]]$content,data[[3]]$content)
        dataPerLine <- Corpus(VectorSource(singleChar))
        
        ## Splitting the sentences into training, testing, validating corpus
        # Adding tags 'train', 'test', 'validation' that respect 0.56, 0.24, 0.2 proportion respectively.
        set.seed(1)
        dataType <- sample(x = c('train', 'test', 'validation'), prob=c(0.56, 0.24, 0.2), size = length(dataPerLine), replace = TRUE)
        meta(dataPerLine, type = 'local', 'typeSet')  <- dataType
        
        # Splitting into three Corpus
        dataTypeFilter <- function(x, dataType){meta(x)$typeSet == dataType}
        dataTrain <- tm_filter(dataPerLine, FUN = dataTypeFilter, 'train')
        dataTest <- tm_filter(dataPerLine, FUN = dataTypeFilter, 'test')
        dataValidation <- tm_filter(dataPerLine, FUN = dataTypeFilter, 'validation')
        
        ## Saving data
        save(dataTrain, file='dataTrain')
        save(dataTest, file='dataTest')
        save(dataValidation, file='dataValidation')
}else{
        load('dataTrain')
        load('dataTest')
        load('dataValidation')
}

### N-Gram construction

## Cleaning train data set
# http://www.cs.cmu.edu/~biglou/resources/bad-words.txt bad-words
badwordsDir <- DirSource(directory = 'bad-words', mode = 'text', encoding = 'UTF-8')
badWords <- Corpus(badwordsDir, readerControl = list(reader = readPlain, language = 'en_US', load = TRUE))
removeBadWords <- function(x){removeWords(x, as.character(badWords[[1]]))}

save(badWords, file='badWords')

transFuns <- list(removePunctuation,
                  removeNumbers,
                  content_transformer(tolower),
                  removeBadWords,
                  stripWhitespace)

if(!file.exists('oneGram') || !file.exists('twoGram') || !file.exists('threeGram')){

        cleanDataTrain <- tm_map(dataTrain, FUN = tm_reduce, tmFuns = transFuns)
        
        ## Building N-Grams
        
        nGramBuider <- function(n, vCorp){
                freqs <- NULL
                
                tokenizer <- function(x){
                        NGramTokenizer(x, control = Weka_control(min = n, max = n))
                }
                
                tremFreqEval <- function(document){
                        freqsOneDoc <- termFreq(document, control = list(tokenize = tokenizer)); 
                        
                        if(is.null(freqs)){
                                freqs <<- freqsOneDoc
                        }else{
                                freqs <<- c(freqs, freqsOneDoc)         
                        }
                }
                
                lapply(vCorp, FUN=tremFreqEval)
                row_sums(freqs)
        }
        
        oneGram  <- nGramBuider(1, cleanDataTrain)
        save(oneGram,file='oneGram')
        
        twoGram  <- nGramBuider(2, cleanDataTrain)
        save(twoGram,file='twoGram')
        
        threeGram <- nGramBuider(3, cleanDataTrain)
        save(threeGram,file='threeGram')
        
        fourGram <- nGramBuider(4, cleanDataTrain)
        save(fourGram,file='fourGram')
}else{
        load('oneGram')
        load('twoGram')
        load('threeGram')
        load('fourGram')
}

### Prediction

## How many word needed?

splitWords <- function(words){
        simplify2array(strsplit(words, ' ', fixed =  TRUE))
}

sortedOneGram = sort(oneGram, decreasing = TRUE)
x=1:length(oneGram)
y = cumsum(sortedOneGram)/sum(oneGram)*100
plot(x, y, xlab='Number of unique words', ylab = '% of total words', type='l')

# about 80% with 4000 words 

# let's keep 80% of words
reducedOneGram <- sortedOneGram[y<=60]
# let's delete every n-gram than end with a word that is not kept.

allWordsIn <- function(sentence, words){
        
        for(ws in splitWords(sentence)){
                
                found <- FALSE
                
                for(w in words){
                        
                        if(w == ws){
                                found <- TRUE
                                break
                        }      
                }
                
                if(!found){
                        return(FALSE)       
                }
        }
                
        TRUE
}

reduceNGram <- function(nGram){
        keepNGram <- sapply(attributes(nGram)[1]$names, 
                            FUN = function(sentence){
                                    allWordsIn(sentence, attributes(reducedOneGram)[1]$names)
                            })
        nGram[keepNGram]
}

reducedTwoGram <- reduceNGram(twoGram)
reducedThreeGram <- reduceNGram(threeGram)
reducedFourGram <- reduceNGram(fourGram) 

##

nGramStruct <- function(nGram){
        data.frame(grams = attributes(nGram)[1]$names, freq = nGram)
}

nGrams <- list(oneGram = nGramStruct(reducedOneGram), 
               twoGram = nGramStruct(reducedTwoGram), 
               threeGram = nGramStruct(reducedThreeGram),
               fourGram = nGramStruct(reducedFourGram)) 

save(nGrams, file = 'nGrams')

nbWords <- sum(nGrams[[1]]$freq)

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

predictWord <- function(sentence){
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


