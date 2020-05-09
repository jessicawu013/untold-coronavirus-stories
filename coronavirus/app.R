# Load packages

library(shiny)
library(shinythemes)
library(knitr)
library(scales)
library(ggthemes)
library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(readr)
library(janitor)
library(gt)
library(gganimate)
library(tidycensus)
library(broom)
library(tidyverse)
census_api_key("efa9cdd00f5d4e029a2b1ab0bece1542d3532d7f")

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

covid <- read_csv("data-sources/us-counties-covid-05-08.csv",
                  col_types = cols(
                      date = col_date(format = ""),
                      county = col_character(),
                      state = col_character(),
                      fips = col_double(),
                      cases = col_double(),
                      deaths = col_double()
                  )) %>%
    filter(date == "2020-05-07")

data(df_pop_county) 
data("df_county_demographics")
df_county_demographics$value = df_county_demographics$percent_black

chloropleth_covid <- df_pop_county %>%
    left_join(covid, by = c("region" = "fips"))

chloropleth_covid$value = chloropleth_covid$cases

medinc <- get_acs(geography = "county",
                  variables = c(medincome = "B19013_001"),
                  year = 2018) %>%
    rename(medincome = estimate) %>%
    select(GEOID, NAME, medincome)

pop <- get_decennial(geography = "county",
                     variables = "P001001",
                     year = 2010,
                     output = "wide",
                     geometry = TRUE,
                     shift_geo = TRUE) %>%
    left_join(medinc, by = c("GEOID", "NAME")) 

# percent uninsured by county
sahie <- read_csv("data-sources/sahie_2018.csv",
                  skip = 79) %>%
    filter(racecat == 0 & sexcat == 0 & iprcat == 0 & 
               geocat == 50 & agecat == 0) %>%
    select(statefips, countyfips, PCTELIG) %>%
    mutate(fips = paste(statefips, countyfips, sep = "")) %>%
    select(fips, PCTELIG)

# combined dataset w pop, median income, percent uninsured, cases, deaths

covid1 <- read_csv("data-sources/us-counties-covid-05-08.csv",
                  col_types = cols(
                      date = col_date(format = ""),
                      county = col_character(),
                      state = col_character(),
                      fips = col_character(),
                      cases = col_double(),
                      deaths = col_double()
                  )) %>%
    filter(date == "2020-05-07")

by_county <- pop %>%
    left_join(covid1, by = c("GEOID" = "fips")) %>%
    left_join(sahie, by = c("GEOID" = "fips")) %>%
    rename(pct_uninsured = PCTELIG)

cases_vs_status <- glm(cases ~ medincome + pct_uninsured, data = by_county)

# ui
ui <- navbarPage(
    "In the Time of Coronavirus",
    tabPanel("Life",
             fluidPage(theme = shinytheme("journal"),
                       titlePanel("Life in the Time of Coronavirus"),
                       p("On Tuesday, March 10th, 2020, Harvard College students were 
             instructed to move out of their dorms in 5 days. Here's how life
             patterns have changed since then."),
                       mainPanel(plotOutput("google_life_plot"),
                                )
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
             HTML("<strong> The reality is, there is a lot of sorrow in this 
                  world right now. </strong>"),
             h4("Socioeconomic Injustice"),
             p("A well-known and unsettling fact about COVID-19 in the United 
               States is that it is disproportionately affecting African Americans.
               (Check out this piece by",
               a(href = "https://www.brookings.edu/blog/fixgov/2020/04/09/why-are-blacks-dying-at-higher-rates-from-covid-19/",
                              "the Brookings Institute.)"),
               "Many suggest that the disprortionate number of African Americans with
               pre-existing health conditions and in lower-income neighborhoods is
               what drives this trend.
               I set out to see if this trend was reflected in an analysis 
               between median income, health insurance, and the number of cases
               by county."),
             splitLayout(plotOutput("us_black_map"),
             plotOutput("us_cases_map")),
             splitLayout(plotOutput("ny_black_map"),
             plotOutput("ny_cases_map")),
             HTML("Sources: American Community Survey and the New York Times"),
             gt_output("cor_table"),
                       gt_output("lm_table"),
                       h5("Why doesn't this add up?"),
                       p("Unfortunately, looking at a model for the counties in
                         the United States did not yield the answers I expected.
                         I suspect this is due to the fact that coronavirus has
                         hit metropolitan areas, where median income is often
                         high, masking neighborhoods and cities where the actual
                         average income is much lower. Some states also have so
                         little data that there is not enough to generate a 
                         significant relationship. Counties with few or no cases
                         of coronavirus are likely to skew the data. We can see 
                         that the linear relationship between the number of 
                         cases and the given variable varies widely by state. 
                         For example, in Maryland, we see that the number of 
                         coronavirus cases increases as the percent uninsured 
                         increases, which aligns with our hypothesis. However, 
                         this relationship is negatively correlated for many 
                         other states."),
             sidebarLayout(
                 sidebarPanel(
                 helpText("View the relationship between median income, percent
                 without health insurance, and number of coronavirus cases by 
                 county for a particular state."),
                 
                 selectInput("var", 
                             label = "Choose a state to display",
                             choices = c("Alabama", "Alaska", "Arizona", 
                                         "Arkansas", "California", "Colorado",
                                         "Connecticut", "Delaware", "Florida",
                                         "Georgia", "Hawaii", "Idaho", "Illinois",
                                         "Indiana", "Iowa", "Kansas", "Kentucky",
                                         "Louisiana", "Maine", "Maryland", "Massachusetts",
                                         "Michigan", "Minnesota", "Mississippi",
                                         "Missouri", "Montana", "Nebraska", 
                                         "Nevada", "New Hampshire", "New Jersey",
                                         "New Mexico", "New York", "North Carolina",
                                         "North Dakota", "Ohio", "Oklahoma", "Oregon",
                                         "Pennsylvania", "Rhode Island", "South Carolina",
                                         "South Dakota", "Tennessee", "Texas", 
                                         "Utah", "Vermont", "Virginia", "Washington",
                                         "West Virginia", "Wisconsin", "Wyoming"
                             ),
                             selected = "New York")
             ),
             mainPanel(
                 plotOutput("medinc_v_cases_by_state"),
                 plotOutput("pct_unins_v_cases_by_state")
             )
             
    )
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
             p("View the", a(href = "https://github.com/jessicawu013/untold-coronavirus-stories",
                             "Github repo.")),
             h3("About Jessica"),
             p("I am a student at Harvard College studying Applied
             Math and Economics. You can reach me at jessicawu@college.harvard.edu
             or find me on LinkedIn", a(href = "https://www.linkedin.com/in/jessica-wu-22b24a192/",
             "here.")))
             )


server <- function(input, output) {
    
    # Renders plot comparing search trends related to daily habits + boredom
    
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
    
    # Renders long distance plot 
    
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
    
    # Renders long distance plot
    
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
    
    # Renders domestic violence plot

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
    
    # Renders map showing percent black for all of U.S. by county
    
    output$us_black_map <- renderPlot(
        
        county_choropleth(df_county_demographics,
                          title = "Demographics of the United States in 2013\nPercent Black",
                          legend = "Percent Black")
    )
    
    # Renders map showing number of cases for all of U.S. by county
    
    output$us_cases_map <- renderPlot(
        county_choropleth(chloropleth_covid,
                          num_colors = 9,
                          title = "Number of Coronavirus Cases as of April 25th, 2020", 
                          legend = "Number of Cases")
    )
    
    # Renders map showing percent black by county for New York
    
    output$ny_black_map <- renderPlot(
        county_choropleth(df_county_demographics,
                          title = "Demographics of New York in 2013\nPercent Black", 
                          legend = "Percent Black",
                          state_zoom = "new york")
    )
    
    # Renders map showing number of coronavirus cases by county for New York
    
    output$ny_cases_map <- renderPlot(
        county_choropleth(chloropleth_covid,
                          num_colors = 9,
                          title = "Coronavirus Cases in New York as of May 7th, 2020", 
                          legend = "Number of Cases",
                          state_zoom = "new york")
    )
    
    # Renders correlation matrix for median income, pct uninsured, number of 
    # cases by county
    
    output$cor_table <- render_gt(
        by_county %>%
            as_tibble() %>%
            select(-geometry) %>%
            select(cases, deaths, medincome, pct_uninsured) %>%
            cor(use = "complete.obs") %>%
            gt() %>%
            tab_header(
                title = "Relationship Between Coronavirus Cases, Deaths, \n\
    Median Income, and Percent Uninsured By County",
                subtitle = "Correlation Matrix"
            ) %>%
            cols_label(
                cases = "Number of Cases",
                deaths = "Number of Deaths",
                medincome = "Median Income",
                pct_uninsured = "% without Health Insurance"
            ) %>%
            tab_source_note(
                source_note = "Sources: The New York Times, American Community Survey 2018"
            )
    )
    
    # Renders linear regression model
    
    output$lm_table <- render_gt(
        cases_vs_status %>%
            tidy(conf.int = TRUE) %>%
            select(term, estimate, conf.low, conf.high) %>%
            gt() %>%
            tab_header(
                title = "Linear Regression for Number of Coronavirus Cases 
    on Median Income and % Uninsured",
                subtitle = "by County"
            ) %>%
            fmt_number(columns = vars(estimate, conf.low, conf.high),
                       decimals = 3) %>%
            cols_label(
                term = "Variable",
                estimate = "Estimate",
                conf.low = "Lower Bound",
                conf.high = "Upper Bound")
    )
    
    # Renders reactive output plot: med income vs number of cases for the 
    # selected state
    
    output$medinc_v_cases_by_state <- renderPlot(
        by_county %>%
            filter(state == input$var) %>%
            ggplot(aes(x = medincome, y = cases)) +
            geom_point() +
            geom_smooth(method = "glm", se = FALSE) +
            labs(
                title = "Median Income vs. Number of Coronavirus Cases",
                subtitle = "By County as of May 7th, 2020",
                y = "Number of Cases",
                x = "Median Income"
            ) +
            theme_classic() +
            theme(legend.position = "none")
    )
    
    # Renders reactive output plot: pct uninsured vs number of cases for the 
    # selected state
    
    output$pct_unins_v_cases_by_state <- renderPlot(
        by_county %>%
            filter(state == input$var) %>%
            ggplot(aes(x = pct_uninsured, y = cases)) +
            geom_point() +
            geom_smooth(method = "glm", se = FALSE) +
            labs(
                title = "Percent without Health Insurance vs. Number of Coronavirus Cases",
                subtitle = "By County as of May 7th, 2020",
                y = "Number of Cases",
                x = "% without Health Insurance"
            ) +
            theme_classic() +
            theme(legend.position = "none")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
