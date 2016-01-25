library(shiny)

shinyUI(fluidPage(
        titlePanel("Word prediction"),
        fluidRow(
                column(12,
                        helpText('Type you text in the input bellow. Word suggestions for the next word will be given after each spaces. If any predicted word correspond to your expectation, click "Insert" to have it inserted into the text input area.')
                )
        ),
        fluidRow(
                column(8,
                       tags$h3('Type your text'),
                       tags$textarea(id="text", rows=10, cols=40),
                       tags$head(tags$style(type="text/css", "#text {width: 500px}"))
                ),
                column(4,
                       tags$h3('Predicted words'),
                       textOutput("word1"),
                       actionButton('word1Insertion', 'Insert'),
                       textOutput("word2"),
                       actionButton('word2Insertion', 'Insert'),
                       textOutput("word3"),
                       actionButton('word3Insertion', 'Insert')
                )
        ),
        fluidRow(
                column(12,
                       tags$h3('About this application'),
                       helpText('This application is the resutl of the Data Science Capstone Project proposed by Johns Hopkins University and SwiftKey CO. It is part of a Data Science Specialization provided by Crousera. The whole application was build by Damien Sonnerat. Read the following documentation To know how this applications works.'),
                       tags$a(href="https://github.com/dsonnerat/word-prediction.git", "Documentation")
                )
        )
))

