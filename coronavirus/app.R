
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
    tabPanel("Death",
             titlePanel("Death in the Time of Coronavirus"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Love",
             titlePanel("Love in the Time of Coronavirus"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Economy",
             titlePanel("Economy in the Time of Coronavirus"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
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
