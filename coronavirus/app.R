
library(shiny)
library(shinythemes)
library(knitr)
library(scales)
library(ggthemes)
library(lubridate)
library(tidyverse)

# loading data

# Data for Life
bread <- read_csv("data-sources/bread.csv",
                  skip = 1,
                  col_types = cols(
                      Day = col_date(format = ""),
                      `bread recipe: (United States)` = col_double()
                  )) %>%
    rename(bread = `bread recipe: (United States)`) %>%
    mutate(bread = bread / 16 * 100)

hair <- read_csv("data-sources/hair.csv",
                 skip = 1,
                 col_types = cols(
                     Day = col_date(format = ""),
                     `how to cut your own hair: (United States)` = col_double()
                 )) %>%
    rename(hair = `how to cut your own hair: (United States)`) %>%
    mutate(hair = hair / 4 * 100)

dalgona <- read_csv("data-sources/dalgona.csv",
                    skip = 1,
                    col_types = cols(
                        Day = col_date(format = ""),
                        `dalgona coffee: (United States)` = col_double()
                    )) %>%
    rename(dalgona = `dalgona coffee: (United States)`) %>%
    mutate(dalgona = dalgona * 100)

boredom <- read_csv("data-sources/boredom.csv",
                    skip = 1,
                    col_types = cols(
                        Day = col_date(format = ""),
                        `boredom: (United States)` = col_double()
                    )) %>%
    rename(boredom = `boredom: (United States)`) %>%
    mutate(boredom = boredom / 5 * 100)

home_life <- boredom %>%
    left_join(dalgona, by = "Day") %>%
    left_join(hair, by = "Day") %>%
    left_join(bread, by = "Day") %>%
    tail(-44) %>%
    rename(date = Day) %>%
    pivot_longer(-date, names_to = "search", values_to = "frequency")

events <- tibble(time = c(ymd(20200314),
                          ymd(20200317),
                          ymd(20200321)),
                 text = c('Harvard College students must move out by today',
                          'Trump announces "15 days to slow the spread" plan',
                          'Governor Cuomo issues stay-at-home order for NY'))

# Data for Love

long_dist <- read_csv("data-sources/longdistance.csv",
                      skip = 1,
                      col_types = cols(
                          Day = col_date(format = ""),
                          `Long-distance relationship: (United States)` = col_double()
                      )) %>%
    rename(long_dist = `Long-distance relationship: (United States)`) %>%
    tail(-44) %>%
    mutate(long_dist = long_dist / 42 * 100)


# ui
ui <- navbarPage(
    "In the Time of Coronavirus",
    tabPanel("Life",
             fluidPage(theme = shinytheme("journal"),
                       titlePanel("Life in the Time of Coronavirus"),
                       p("On Tuesday, March 10th, 2020, Harvard College students were 
             instructed to move out of their dorms in 5 days. Here's how life
             patterns have changed since then."),
                       mainPanel(plotOutput("google_life_plot"))
    )
    ),
    tabPanel("Love",
             fluidPage(titlePanel("Love in the Time of Coronavirus"),
             p("How is the face of love changing during quarantine?"),
             mainPanel(plotOutput("long_dist_plot")))
             ),
    tabPanel("Sorrow",
             titlePanel("Sorrow in the Time of Coronavirus"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")
             ),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("'In the Time of Coronavirus' seeks to explore the untold stories
             of coronavirus from the lenses of life, death, love, and economy.
             We are being inundated with news about the number of coronavirus
             cases and key figures that speak to our physical health, but there
             are endless stories to be told about the impact of coronavirus on 
             our daily lives and the rest of the human existence -- from the 
             deterioration of our relationships to struggling to put food on the
             table."),
             h3("About Me"),
             p("My name is Jessica, and I study Applied Math and Economics.
             You can reach me at jessicawu@college.harvard.edu."))
             )


server <- function(input, output) {
    output$google_life_plot <- renderPlot(
        
    home_life %>%
        ggplot() +
        geom_line(aes(x = date, y = frequency, color = search)) + 
        geom_vline(xintercept=as.numeric(home_life$date[c(24, 25, 49)]),
                   linetype=2, colour="black") +
        theme_calc() +
        labs(title = "What are people doing while at home?",
             subtitle = "Taking a look a Google Search trends",
             y = "Percent (%) Frequency of Search \n\ Relative to Itself on March 10th",
             x = "Date") +
        scale_color_discrete(name = "Search Term", labels = c('"boredom"',
                                                              '"bread recipe"',
                                                              '"dalgona coffee"',
                                                              '"how to cut your own hair"')) +
        geom_text(data = events, mapping = aes(label = text, y = 700, x = time), 
                  angle = 90,
                  hjust = -0.20,
                  size = 3)
    )
    
    output$long_dist_plot <- renderPlot(
        ggplot(long_dist, aes(x = Day, y = long_dist)) +
            geom_line(color = "light blue") +
            labs(
                title = "Interest in Long-Distance Relationships During Social Distancing",
                subtitle = "Taking a look at Google Search Trends",
                y = "Popularity of the Topic, 'Long Distance', \n\ Relative to March 10th (%)",
                x = "Date"
            ) +
            geom_vline(xintercept=as.numeric(long_dist$Day[c(6, 7, 13)]),
                       linetype=2, colour="black") +
            geom_text(data = events, mapping = aes(label = text, y = 0, x = time), 
                      angle = 90,
                      hjust = -0.20,
                      size = 3) +
            theme_calc()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
