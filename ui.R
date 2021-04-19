library(shiny)
library(shinydashboard)

header <- dashboardHeader(title='Singlish Text Generator')

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Model Comparison', tabName='comparison', icon=icon('cogs'))
    )
)

model_choices <-
    c('Markov 1-gram', 'Markov 2-gram', 'Markov 3-gram', 'Markov 4-gram', 'Markov 5-gram', 'Markov 6-gram')

body <- dashboardBody(
    tabItems(
        # Tab 1: Model Comparison
        tabItem(tabName='comparison',
                fluidRow(
                    column(width=8,
                           box(width=12,
                               height=420,
                               textInput(inputId='text_input_1',
                                         label=NULL,
                                         placeholder='Type here...'
                                         ),
                               actionButton(inputId='random_input',
                                            label='Random word as input',
                                            icon=icon('question'),
                               ),
                               p(''),
                               tableOutput('comparison_text')
                               )
                           ),
                    column(width=4,
                           box(height=420,
                               checkboxGroupInput(inputId='model_selection_1',
                                                  label='Select Models',
                                                  choices=model_choices,
                                                  selected=model_choices[1:6]
                                                  ),
                               sliderInput(inputId='output_length',
                                           label='Output Length (Words)',
                                           min=1, max=10,
                                           value=1, step=1,
                                           ticks=FALSE
                                           )
                               )
                    )
                )
        )
    )
)

dashboardPage(header, sidebar, body,
              skin='blue')