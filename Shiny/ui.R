library(shiny)
library(leaflet)

fluidPage(
  titlePanel("Cartograms"),
  sidebarLayout(
    sidebarPanel(
      radioButtons('index', 'Index',
                   c('United States of America'='USA',
                     'China'='CHN',
                     'India'='IND',
                     'Other'='other'),
                   ','),
      conditionalPanel(
        condition = 'input.index == \'other\'',
        fileInput('file1', 'Upload CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        actionButton("getcsv", "Download Template", onclick ="window.open('https://www.dropbox.com/s/3frcu6y8ia7ysa1/countries.csv?dl=1', '_blank')")
      )
    ),
    mainPanel(
      fluidRow(
        column(6,
          leafletOutput("map", width="400", height="400")
        ),
        column(6,
               leafletOutput("map2", width="400", height="400")
        )
      )
    )
  )
  
)