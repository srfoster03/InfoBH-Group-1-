# InfoBH-Group-1-

library(shiny)
library(tidyverse)
data <- read_delim('COVID19_state.csv')


ui <- fluidPage(
  
  titlePanel("Data about COVID 19 Tests"),
  
  
  tabsetPanel(
    tabPanel(
      "Introduction",
      sidebarLayout(
        p(
          "For our project we are analyzing a ",
          strong("Covid-19 state data"),
          ", dataset, it was collected by ",
          em("The Covid Tracking project"),
          ",
                             from 2010 through 2020. Our aim is to examine if there is a correlation between income per capita and COVID -
                             19 tests within states. As well as the relationship between GDP per capita and deaths from COVID -
                             19. Finally we want to examine the relationship between the rate of infections (infections /
                             population estimates) and population density. We want to use this analysis to understand these relationships,
                             and reduce the spread of deadly diseases in the future.
                            "),
        p(
          img(src ="https://www.cdc.gov/dotw/covid-19/images/main_928px.jpg")
        )
      )
    ),
    
    tabPanel("Income", plotOutput("income"), textOutput("incomeText"), sidebarLayout
             (sidebarPanel
               (sliderInput(inputId = "income", label = "Select the range of years", value = c(0,74561),min = 0, max = 74561)),
               
               
               tabPanel(“GDP”,
               # Title for page about GDP/Income related to Deaths per State
                         sidebarLayout(
                           sidebarPanel(
               # Description of the page and summary
                             h5("On this page we will be exploring the effect GDP per capita and average income had on the number of deaths from COVID-19. It will answer questions regarding the number of deaths in each state, the income and GDP for those states in correspondence with the number of deaths, and how GDP or Income may have influenced more or less deaths in each state. On the Y-Axis of this chart we see each state, depending on the number chosen from the slider, and on the X-axis we see the number of deaths in each state. The legend on the graph will depend on whether GDP or Income has been selected. It will rank the highest (red) to lowest (blue) and color the states according to there Income and GDP rate."),
                             br(),
               # Interactive options, one slider and one pull down bar
                             selectInput("xvar", "Select variable for x-axis:",
                                         choices = c("GDP", "Income"), selected = "GDP"),
                             sliderInput("n_states", "Number of States to Display:",
                                         min = 5, max = 50, value = 50)
                           ),
               # Main panel for histogram
                           mainPanel(
                             plotOutput("histogram", width = "700px", height = "750px")
                           ),
                           
                           tabPanel("Infection Rate",
                                    #Relationship between population density and rate of infections:
                                    sidebarLayout(
                                      sidebarPanel(
                                        p("In this section, we can explore the relationship between", em("population
                    density"), "and the", em("infection rate"), "for every state."),
                                        p("We can also look to see if states with different", strong("ranges of income per capita"), 
                                          "have obviously different infection rates."),
                                        sliderInput("income_range",
                                                    "Income Per Capita Range:",
                                                    min = 37994,
                                                    max = 74561,
                                                    value = c(37994,46000)),
                                        p("As you can see, including the entire range of income per capita, 
                     results in a downward sloping trend line. One may think that more highly dense 
                     state populations would result in higher COVID 19 infection rates. However,
                     this trend opposes that expectation. This is probably due to some confounding variable. 
                     Perhaps more densely populated states also took more precautions to prevent infections 
                     from happening."),
                                        #Relationship between state health spending and rate of infections
                                        p("Maybe another variable, such as", em("Health Spending"), "will better explain the 
                     differing infection rates between states."),
                                        p("The scatter plot on the right, also reacting to different income ranges, shows that 
                     states with higher income per capita have higher state spending on health, and thus
                     lower rates of COVID 19 infections."),
                                        p("States with higher population densities have higher income per capita, and 
                     states with higher income per capita also have more state spending on health as well 
                     as lower infection rates of COVID 19. Because of this, we can conclude that", 
                                          em("Health Spending"), "is one of the reasons more densely populated states had
                     lower rates of infection.")
                                      ),
                                      mainPanel(plotOutput("section3plot"),
                                                textOutput("section3msg"),
                                                plotOutput("section3plot2")
                                      )
                                    )
                           ),
                           tabPanel(
                             "Conclusion",
                             p(
                               "There is a negative correlation between the amount of health spending and the infection rate.
         Which means that states that spent more on health services had lower infection rates.
         Economists and politicians can look at this and use it  learn for future epidemics.
         We believe that the data is of reasonable quality and that it gives unbiased results.
         A problem that arises is that the data only shows state data while it could vary greatly
         between the cities within the states. Some of the graphs showed correlation but it was also 
         correlated to the population of the states.
         So while these correlations may appear does not mean there is causation.
         A more thorough examination could look at regions within each city and look at test accessibility."
                             )
                           )
                           
                         )
               )
               
               server <- function(input, output) {
                 data1 <- reactive({
                   subset(data, Income>input$income[1] & Income<input$income[2])
                 })
                 output$income <- renderPlot({
                   data1() %>% 
                     ggplot(aes(Income, Tested))+geom_point()
                 }) 
                 output$incomeText <- renderPrint({
                   paste(input$income[1],"is the number", mean(data1()$Income))
                 })
                 
                 # SECTION 2: DEATHS PER STATE AND GDP/INCOME
                 # Server function for GDP and Income descending in states
                 sorted_data <- reactive({
                   if (input$xvar == "GDP") {
                     data %>% 
                       arrange(desc(GDP))
                   } else {
                     data %>% 
                       arrange(desc(Income))
                   }
                 })
                 # Histogram function for GDP and Deaths per State
                 output$histogram <- renderPlot({
                   ggplot(sorted_data() %>% head(input$n_states), aes(x = reorder(State, -!!sym(input$xvar)), y = Deaths, fill = !!sym(input$xvar))) +
                     geom_col() +
                     coord_flip() +
                     scale_fill_gradient(name = paste(input$xvar, "($)", sep = ""), low = "blue", high = "red") +
                     labs(x = "", y = "Deaths", title = paste("COVID-19 Deaths by State (Ranked by", input$xvar,")")) +
                     theme_minimal() +
                     theme(
                       axis.text.y = element_text(size = 10),
                       legend.position = c(0.85, 0.5),
                       legend.direction = "vertical",
                       legend.box.just = "right",
                       axis.text.x = element_text(size = 8, angle = 90, hjust = 1)
                     )
                 })
                 
                 #SECTION 3: INFECTION RATE
                 #plotting pop density vs infection rate:
                 filtereddata_3 <- reactive({
                   data %>% 
                     filter(Income >= input$income_range[1],
                            Income <= input$income_range[2])
                 })
                 
                 #plotting pop density vs infection rate:
                 output$section3plot <- renderPlot({
                   filtereddata_3() %>%
                     filter(State != "District of Columbia") %>% 
                     mutate(InfectionRate = Infected/Tested) %>% 
                     ggplot( aes(x= `Pop Density`, y= InfectionRate))+
                     geom_point()+
                     geom_smooth(method = lm, se = FALSE, col = "red")+
                     labs(title = "Infection Rate for Different Population Densities",
                          x = "Population Density",
                          y= "Infection Rate")
                 })
                 output$section3msg <- renderText ({
                   filtereddata_3() %>% 
                     mutate(InfectionRate = Infected/Tested) %>%
                     pull(InfectionRate) %>% 
                     mean() %>% 
                     paste("Out of this income range, the average rate of infection is",.)
                   
                 })
                 
                 #plotting health density vs infection rate:
                 output$section3plot2 <- renderPlot({
                   filtereddata_3() %>%
                     filter(State != "District of Columbia") %>% 
                     mutate(InfectionRate = Infected/Tested) %>% 
                     ggplot( aes(x= `Health Spending`, y= InfectionRate))+
                     geom_point()+
                     geom_smooth(method = lm, se = FALSE, col = "red")+
                     labs(title = "Infection Rate for Different State Spending on Health",
                          x = "Health Spending",
                          y= "Infection Rate")
                 })
                 
                 
               }
               
               # Run app
               
               shinyApp(ui = ui, server = server)
               
