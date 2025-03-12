

ui <- fluidPage(
  
  theme = shinytheme('united'),
  
  # App title ----
  titlePanel("Stiffness Hopping Test"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      tags$head(tags$style("#Summaryhead{color: red;
                             font-size: 40px;
                         }"
      )
      ),
      
      # Input: Select a file ----
      numericInput("Mass", "Mass:", 0),
      fileInput("Left1", "Choose Left 1 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Left2", "Choose Left 2 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Left3", "Choose Left 3 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Left4", "Choose Left 4 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Left5", "Choose Left 5 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Left6", "Choose Left 6 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right1", "Choose Right 1 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right2", "Choose Right 2 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right3", "Choose Right 3 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right4", "Choose Right 4 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right5", "Choose Right 5 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right6", "Choose Right 6 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      actionButton("goButton", "Go!")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Left1",
                           plotlyOutput('projectplotLeft1'),
                           tableOutput('resultstableLeft1'),
                  ),
                  
                  tabPanel("Left2",
                           plotlyOutput('projectplotLeft2'),
                           tableOutput('resultstableLeft2'),
                  ),
                  
                  tabPanel("Left3",
                           plotlyOutput('projectplotLeft3'),
                           tableOutput('resultstableLeft3'),
                  ),
                  
                  tabPanel("Left4",
                           plotlyOutput('projectplotLeft4'),
                           tableOutput('resultstableLeft4'),
                  ),
                  
                  tabPanel("Left5",
                           plotlyOutput('projectplotLeft5'),
                           tableOutput('resultstableLeft5'),
                  ),
                  
                  tabPanel("Left6",
                           plotlyOutput('projectplotLeft6'),
                           tableOutput('resultstableLeft6'),
                  ),
                  
                  tabPanel("Right1",
                           plotlyOutput('projectplotRight1'),
                           tableOutput('resultstableRight1'),
                  ),
                  
                  tabPanel("Right2",
                           plotlyOutput('projectplotRight2'),
                           tableOutput('resultstableRight2'),
                  ),
                  
                  tabPanel("Right3",
                           plotlyOutput('projectplotRight3'),
                           tableOutput('resultstableRight3'),
                  ),
                  
                  tabPanel("Right4",
                           plotlyOutput('projectplotRight4'),
                           tableOutput('resultstableRight4'),
                  ),
                  tabPanel("Right5",
                           plotlyOutput('projectplotRight5'),
                           tableOutput('resultstableRight5'),
                  ),
                  tabPanel("Right6",
                           plotlyOutput('projectplotRight6'),
                           tableOutput('resultstableRight6'),
                  ),
                  tabPanel("Summary", textOutput("Summaryhead"),
                           tableOutput('resultstableSummary')
                  )
      )
      
      
      
      # Output: Data file ----
      
    )
  )
)





# Create Shiny app ----
# shinyApp(ui, server)