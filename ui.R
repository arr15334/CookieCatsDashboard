library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)

shinyUI(
  dashboardPage(
    dashboardHeader(title='CookieCats'),
    dashboardSidebar(
      sidebarMenu(
        menuItem('Summary', tabName = 'summary', icon = icon('table')),
        menuItem('A/B test', tabName='abtest', icon=icon('flask'))
        )
      ),
    dashboardBody(
      tabItems(
        tabItem('summary',
                fluidRow(
                  box(title='User split', plotOutput('usersplit')),
                  box(title = 'User retention',
                      switchInput(inputId='retType',
                                  onLabel='1-day', offLabel='7-day', value=T),
                      withSpinner(plotOutput('onedayret'))),
                  box(title = 'Retention rate vs games played', width=12,
                      sliderInput('maxGames', 'Maximum number of games', value=1000, min=1000, max=4000, step=500),
                      withSpinner(plotOutput('retentionGamesControl')),
                      withSpinner(plotOutput('retentionGamesTreatment'))
                      )
                )),
        tabItem('abtest',
                fluidRow(
                  box(selectInput('metric', 'Metric',
                                  choices = c('One-day Retention' = 'ONEDAY',
                                              'Seven-day Retention' = 'SEVENDAY',
                                              'Games played' = 'GAMES',
                                              'Combined' = 'ALL'
                                              ))
                      ),
                  box(title='Weights for combined OEC',
                    numericInput('oneWeight', 'One-day', value=0.33, min = 0, max=1, step=0.05),
                    numericInput('SevenWeight', 'Seven-day', value=0.33, min = 0, max=1, step=0.05),
                    numericInput('gamesPlayed', 'Games Played', value=0.34, min = 0, max=1, step=0.05)),
                  box(title='Result', width=12, status='primary',
                      textOutput('abresult'),
                      tableOutput('confInt')),
                  valueBoxOutput('upliftone'),
                  valueBoxOutput('upliftseven'),
                  valueBoxOutput('upliftgames')
                )
                )
      )
    ),
    skin='green'
  )
)
