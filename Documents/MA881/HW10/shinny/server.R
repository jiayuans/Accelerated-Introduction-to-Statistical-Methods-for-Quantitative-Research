library(shiny)

mdearn <- read.csv("/Users/jiayuan/Documents/MA881/HW10/shinny/md_earn.csv",header=T)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Fill in the spot we created for a plot
  output$collegePlot <- renderPlot({
    
    # Render a barplot
    barplot(mdearn[,input$region], 
            main=input$region,
            ylab="Median Earnings of Students Working",
            xlab="College",
            names.arg = c("BC", "BU", "Harvard","MIT", "NU"))
     })
})