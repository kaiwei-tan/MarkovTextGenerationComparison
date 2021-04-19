library(shiny)
library(dplyr)
library(textclean)
library(tidytext)
library(tidyverse)

# Set of Markov models, RNN, LSTM 
model_markov <- readRDS("models_markov_long")
lexicon <- readRDS("lexicon")

# Input -> Generic tokens
input.replace <- function(text) {
    text %>%
        tolower() %>%
        textclean::strip(char.keep='.') %>%
        str_replace_all('[0-9]+pm', 'timetoken') %>% # time token
        str_replace_all('[0-9]+am', 'timetoken') %>% # time token
        str_replace_all('[0-9]+', 'numbertoken') %>% # number token
        textclean::mgsub(c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'),
                         'daytoken') %>% # day of week token
        str_squish() %>%
        return()
}

# Generic Tokens -> Output
token.replace <- function(text) {
    text %>%
        gsub('daytoken',
             sample(c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'), 1), .) %>%
        gsub('numbertoken',
             as.character(sample(c(0,9), 1)), .) %>%
        gsub('timetoken',
             paste(as.character(sample(c(1,12), 1)), sample(c('am', 'pm'), 1), sep=''), .) %>%
        gsub('fullstop', '.', .) %>%
        return()
}

# Markov chain
# Will generate sequence up till output_length, or until it reaches a full stop / absorbing state
generate.sequence.markov <- function(input_text, model_long, n, output_length, options=FALSE) {
    
    output <- list(input=character(), output=character(), message=character())
    input_words <-
        input_text %>%
        input.replace() %>%
        strsplit(' ') %>%
        unlist()

    input_length <- length(input_words)
    selected_model <- model_long[[n]]

    if (length(input_words) < n) {
        return('(Length of input must be larger than n.)')
    } else if (length(input_words) == n) {
        seed_words <- input_words
    } else {
        seed_words <- input_words[-1:-(length(input_words)-n)]
    }
    
    for (i in 1:output_length) {
        selected_rows <-
            selected_model %>%
            filter(start_state == seed_words %>% paste(collapse=' ')) %>%
            arrange(desc(prob))
        
        if (nrow(selected_rows) == 0) {
            output$message <- 'sequence ran into unidentified state'
            next_ngrams <- c()
            break
        } else {
            if (options) {
                next_ngrams <- selected_rows$end_state %>% head(3)
            }
            selected_rows_max <- selected_rows[which(selected_rows$prob == max(selected_rows$prob)),]
            next_ngram <- selected_rows_max$end_state
        }
        
        # Detect absorbing state, break if so
        if (nrow(selected_rows_max) == 1) {
            if (selected_rows_max$start_state == selected_rows_max$end_state &
                selected_rows_max$prob[1] == 1) {
                output$message <- 'sequence ran into absorbing state'
                break
            }
        }
        
        # If multiple results with same probability, select the next n-gram at random
        if (length(next_ngram) > 0) {
            next_ngram <- sample(next_ngram, 1)
        }
        
        next_ngram_words <- strsplit(next_ngram, ' ') %>% unlist()
        
        if (length(next_ngram_words) > 1) {
            input_words <- c(input_words, next_ngram_words[-1:-(n-1)])
            seed_words <- input_words[-1:-(length(input_words)-n)] %>% paste(collapse= ' ')
        } else {
            input_words <- c(input_words, next_ngram_words)
            seed_words <- next_ngram_words
        }
    }

    output$input <- input_text
    
    if (length(input_words) > input_length) {
        output$output <- 
            paste(input_words[(1+input_length):length(input_words)], collapse=' ') %>%
            token.replace()
    } else {
        output$output <- ''
    }
    
    if (length(output$message) > 0) {
        output_final <-
            paste(output$input, ' ', output$output, ' (', output$message, ')', sep='') %>%
            gsub(' \\.', '.', .) %>%
            str_squish()
    } else {
        output_final <-
            paste(output$input, output$output, sep=' ') %>%
            gsub(' \\.', '.', .) %>%
            str_squish()
    }

    return(output_final)
}

server <- function(input, output, session) {
    
    observeEvent(input$random_input, {
        updateTextInput(session, 'text_input_1', value=sample(lexicon, 1))
    })
    
    model.comparison <- reactive({
        
        text_input_one <- input.replace(input$text_input_1)
        output_list_one <- list(model=input$model_selection_1, text=vector())
        
        if ('Markov 1-gram' %in% input$model_selection_1) {
            output_list_one$text <-
                c(output_list_one$text,
                  generate.sequence.markov(text_input_one, model_markov, 1, input$output_length))
        }
        
        if ('Markov 2-gram' %in% input$model_selection_1) {
            output_list_one$text <-
                c(output_list_one$text,
                  generate.sequence.markov(text_input_one, model_markov, 2, input$output_length))
        }
        
        if ('Markov 3-gram' %in% input$model_selection_1) {
            output_list_one$text <-
                c(output_list_one$text,
                  generate.sequence.markov(text_input_one, model_markov, 3, input$output_length))
        }
        
        if ('Markov 4-gram' %in% input$model_selection_1) {
            output_list_one$text <-
                c(output_list_one$text,
                  generate.sequence.markov(text_input_one, model_markov, 4, input$output_length))
        }
        
        if ('Markov 5-gram' %in% input$model_selection_1) {
            output_list_one$text <-
                c(output_list_one$text,
                  generate.sequence.markov(text_input_one, model_markov, 5, input$output_length))
        }
        
        if ('Markov 6-gram' %in% input$model_selection_1) {
            output_list_one$text <-
                c(output_list_one$text,
                  generate.sequence.markov(text_input_one, model_markov, 6, input$output_length))
        }
        
        data.frame(output_list_one)
        })
    
    output$comparison_text <- renderTable(model.comparison(),
                                          colnames=FALSE,
                                          bordered=TRUE,
                                          spacing='m')
}