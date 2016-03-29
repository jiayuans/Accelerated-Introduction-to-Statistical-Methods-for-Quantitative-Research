library(shiny)

mdearn <- read.csv("md_earn.csv",header=T)
# Define UI for application that draws a histogram
# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("College Scorecard Data"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("region", "Working Years Not Enrolled:", 
                    choices=colnames(mdearn[,2:4])),
        hr(),
        helpText("Data from College Scorecard. It shows median earnings of students from
                  Boston College, Boston University, Harvard University, 
                  Massachusetts Institute of Technology and Northeastern University,
                  working in different years not enrolled after entry.")
        ),
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("collegePlot")  
      )
      
    )
  )
)