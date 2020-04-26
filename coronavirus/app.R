
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)

# loading data


# ui
ui <- navbarPage(
    "In the Time of Coronavirus",
    tabPanel("Life",
             fluidPage(theme = shinytheme("journal"),
                       titlePanel("Cases of Coronavirus Over Time"),
                       mainPanel(plotOutput("line_plot")))
    ),
    tabPanel("Love",
             titlePanel("Love in the Time of Coronavirus"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Sorrow",
             titlePanel("Sorrow in the Time of Coronavirus"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("'In the Time of Coronavirus' seeks to explore the untold stories
             of coronavirus from the lenses of life, death, love, and economy.
             We are being inundated with news about the number of coronavirus
             cases and key figures that speak to our physical health, but there
             are endless stories to be told about the impact of coronavirus on 
             our daily lives and the rest of the human existence -- from the 
             deteriation of our relationships to struggling to put food on the
             table."),
             h3("About Me"),
             p("My name is Jessica, and I study Applied Math and Economics.
             You can reach me at jessicawu@college.harvard.edu."))
             )


server <- function(input, output) {
    output$line_plot <- renderPlot(
        
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
