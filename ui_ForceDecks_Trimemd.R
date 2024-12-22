

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
      fileInput("Left2.0", "Choose Left 2.0 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Left2.3", "Choose Left 2.3 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Left2.6", "Choose Left 2.6 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("LeftSS", "Choose Left SS CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right1.7", "Choose Right 1.7 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right2.0", "Choose Right 2.0 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right2.3", "Choose Right 2.3 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("Right2.6", "Choose Right 2.6 CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("RightSS", "Choose Right SS CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      actionButton("goButton", "Go!")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Left 1",
                           plotOutput('projectplotLeft1'),
                           tableOutput('resultstableLeft1'),
                  ),
                  
                  tabPanel("Left 2.0Hz",
                           plotOutput('projectplotLeft2.0'),
                           tableOutput('resultstableLeft2.0'),
                  ),
                  
                  tabPanel("Left 2.3Hz",
                           plotOutput('projectplotLeft2.3'),
                           tableOutput('resultstableLeft2.3'),
                  ),
                  
                  tabPanel("Left 2.6Hz",
                           plotOutput('projectplotLeft2.6'),
                           tableOutput('resultstableLeft2.6'),
                  ),
                  
                  tabPanel("Left SSHz",
                           plotOutput('projectplotLeftSS'),
                           tableOutput('resultstableLeftSS'),
                  ),
                  
                  tabPanel("Right 1.7Hz",
                           plotOutput('projectplotRight1.7'),
                           tableOutput('resultstableRight1.7'),
                  ),
                  
                  tabPanel("Right 2.0Hz",
                           plotOutput('projectplotRight2.0'),
                           tableOutput('resultstableRight2.0'),
                  ),
                  
                  tabPanel("Right 2.3Hz",
                           plotOutput('projectplotRight2.3'),
                           tableOutput('resultstableRight2.3'),
                  ),
                  
                  tabPanel("Right 2.6Hz",
                           plotOutput('projectplotRight2.6'),
                           tableOutput('resultstableRight2.6'),
                  ),
                  
                  tabPanel("Right SSHz",
                           plotOutput('projectplotRightSS'),
                           tableOutput('resultstableRightSS'),
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