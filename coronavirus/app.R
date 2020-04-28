# Load packages

library(shiny)
library(shinythemes)
library(knitr)
library(scales)
library(ggthemes)
library(lubridate)
library(choroplethr)
library(choroplethrMaps)
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

divorce <- read_csv("data-sources/divorce.csv",
                    skip = 1,
                    col_types = cols(
                        Day = col_date(format = ""),
                        `Divorce: (United States)` = col_double()
                    )) %>%
    rename(divorce = `Divorce: (United States)`) %>%
    mutate(divorce = divorce / 82 * 100)

divorce_lawyer <- read_csv("data-sources/divorcelawyer.csv",
                           skip = 1,
                           col_types = cols(
                               Day = col_date(format = ""),
                               `divorce lawyer: (United States)` = col_double()
                           )) %>%
    rename(divorce_lawyer = `divorce lawyer: (United States)`) %>%
    mutate(divorce_lawyer = divorce_lawyer / 79 * 100)

legal_sep <- read_csv("data-sources/legalseparation.csv",
                      skip = 1,
                      col_types = cols(
                          Day = col_date(format = ""),
                          `Legal separation: (United States)` = col_double()
                      )) %>%
    rename(legal_sep = `Legal separation: (United States)`) %>%
    mutate(legal_sep = legal_sep / 56 * 100)

divorce_terms <- divorce %>%
    left_join(divorce_lawyer, by = "Day") %>%
    left_join(legal_sep, by = "Day") %>%
    tail(-44) %>%
    rename(date = Day) %>%
    pivot_longer(-date, names_to = "search", values_to = "frequency")

dom_violence <- read_csv("data-sources/domesticviolence.csv",
                         skip = 1,
                         col_types = cols(
                             Day = col_date(format = ""),
                             `domestic violence: (United States)` = col_double()
                         )) %>%
    rename(dom_violence = `domestic violence: (United States)`) %>%
    tail(-44) %>%
    mutate(dom_violence = dom_violence / 78 * 100)

# Data for Sorrow

covid <- read_csv("data-sources/us-counties-covid-04-26.csv",
                  col_types = cols(
                      date = col_date(format = ""),
                      county = col_character(),
                      state = col_character(),
                      fips = col_double(),
                      cases = col_double(),
                      deaths = col_double()
                  )) %>%
    filter(date == "2020-04-25")

data(df_pop_county) 
data("df_county_demographics")
df_county_demographics$value = df_county_demographics$percent_black

chloropleth_covid <- df_pop_county %>%
    left_join(covid, by = c("region" = "fips"))

chloropleth_covid$value = chloropleth_covid$cases

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
             HTML("<strong>How is the face of love changing during quarantine?</strong>"),
             mainPanel(h4("Long Distance"),
                       p("People must suddenly be finding themselves forced into 
               long-distance relationships. Do Google Search trends support
               this conclusion?"),
                       plotOutput("long_dist_plot")),
             mainPanel(h4("Divorce"),
                       p("On the other hand, people must have been suddenly forced to spend
                         essentially 24/7 with their partners. Is it possible that
                         too much time together exacerbates marital problems,
                         perhaps even bringing them to a head?"),
                       plotOutput("divorce_plot")),
             mainPanel(h4("Domestic Violence*"),
                       p("Many experts, including the", a(href= "https://www.apa.org/topics/covid-19/domestic-violence-child-abuse",
                         "American Psychological Association,"),"have suggested
                         that people are facing increased domestic violence 
                         under stay-at-home orders. I looked to Google Search
                         trends to see if this was possibly captured by web activity."),
                       plotOutput("dom_violence_plot"),
                       p("*Although under this category, domestic violence is
                         NOT love; it is abuse."))
             )),
    tabPanel("Sorrow",
             titlePanel("Sorrow in the Time of Coronavirus"),
             p("The sad reality of our times is that there is a lot of sorrow."),
             mainPanel(plotOutput("us_black_map"),
                       plotOutput("us_cases_map"),
                       plotOutput("ny_black_map"),
                       plotOutput("ny_cases_map"))
             ),
    tabPanel("About", 
             titlePanel("About"),
             h3("About the Project"),
             p("I first began thinking about focusing on COVID-19 for my final
             project back in mid-February 2020. At the time, very few people in
             the United States had any idea just how intensely and quickly this
             pandemic would come to dominate our lives, myself included. In a mere 
             matter of weeks, the world's top data scientists began focusing on
             documenting the spread of the novel coronavirus. I wondered to myself,
             'What narratives are still important but being left out of the main news
             cycle? When we look back in 50 years at the history that is being made,
             what stories will we forget to tell?'"),
             HTML('<strong> "In the Time of Coronavirus" </strong>, a play on the famous
             novel and film, "Love in the Time of Cholera," seeks to explore 
             the untold stories of coronavirus from the lenses of life, love, and sorrow.
             We are being inundated with news about the number of coronavirus
             cases and key figures that speak to our physical health, but there
             are endless stories to be told about the impact of coronavirus on 
             our daily lives and the rest of the human existence -- from the 
             deterioration of our relationships to struggling to put food on the
             table.'),
             h3("About Jessica"),
             p("I am a first-year at Harvard College studying Applied
             Math and Economics. You can reach me at jessicawu@college.harvard.edu."))
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
    
    output$divorce_plot <- renderPlot(
        divorce_terms %>%
            ggplot() +
            geom_line(aes(x = date, y = frequency, color = search)) + 
            geom_vline(xintercept=as.numeric(divorce_terms$date[c(18, 19, 37)]),
                       linetype=2, colour="black") +
            theme_calc() +
            labs(title = "How are people feeling about their marriages?",
                 subtitle = "Taking a look a Google Search trends",
                 y = "Percent (%) Frequency of Search \n\ Relative to Itself on March 10th",
                 x = "Date") +
            scale_color_discrete(name = "Searches", labels = c('Topic: Divorce',
                                                               'Term: "divorce lawyer"',
                                                               'Topic: Legal separation')) +
            geom_text(data = events, mapping = aes(label = text, y = 0, x = time), 
                      angle = 90,
                      hjust = -0.20,
                      size = 3)
    )
    
    output$dom_violence_plot <- renderPlot(
        ggplot(dom_violence, aes(x = Day, y = dom_violence)) +
            geom_line(color = "red") +
            labs(
                title = "Interest in Domestic Violence During Social Distancing",
                subtitle = "Taking a look at Google Search Trends",
                y = "Popularity of the Topic, 'Domestic Violence', \n\ Relative to March 10th (%)",
                x = "Date"
            ) +
            geom_vline(xintercept=as.numeric(long_dist$Day[c(6, 7, 13)]),
                       linetype=2, colour="black") +
            geom_text(data = events, mapping = aes(label = text, y = 50, x = time), 
                      angle = 90,
                      hjust = -0.20,
                      size = 3) +
            theme_calc()
    )
    
    output$us_black_map <- renderPlot(
        
        county_choropleth(df_county_demographics,
                          title = "Demographics of the United States in 2013\nPercent Black",
                          legend = "Percent Black")
    )
    
    output$us_cases_map <- renderPlot(
        county_choropleth(chloropleth_covid,
                          num_colors = 9,
                          title = "Number of Coronavirus Cases as of April 25th, 2020", 
                          legend = "Number of Cases")
    )
    
    output$ny_black_map <- renderPlot(
        county_choropleth(df_county_demographics,
                          title = "Demographics of New York in 2013\nPercent Black", 
                          legend = "Percent Black",
                          state_zoom = "new york")
    )
    
    output$ny_cases_map <- renderPlot(
        county_choropleth(chloropleth_covid,
                          num_colors = 9,
                          title = "Coronavirus Cases in New York as of April 25th, 2020", 
                          legend = "Number of Cases",
                          state_zoom = "new york")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
