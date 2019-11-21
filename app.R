# Load required packages
library("shiny")
library("dplyr")
library("ggmap")
library("plotly")
library("ggplot2")

# Load the data 
education_data <- read.csv("data/barro_lee_dataset.csv", stringsAsFactors = FALSE)

# Create a background page containing information about our problem and our 
# research questions
page_one <- tabPanel(
  "Background",
  titlePanel("Background"),
  p("Today, education remains inaccessible for millions of children and adolescents 
    around the world. According to",
    a("UNICEF,", href ="https://www.unicef.org/education"),
    '"roughly one in five school-aged children are not in school at all."',
    "This is due to various reasons such as poverty, gender, location, ethnicity, 
    etc."
  ),
  p("We hope to bring more awareness to the high levels of education inequality 
    occurring across countries, particularly developed countries vs. developing 
    countries, with our visualizations."
  ),
  br(),
  h3("Research Questions"),
  tags$ul(
    tags$li("How does the percent of primary school completion differ across the 
            countries in a region?"),
    tags$li("How has the percent of no schooling attained in the population changed
            over the years for each region?"),
    tags$li("How does average years of schooling differ across countries?")
  )
)

# Create a page containing bar graphs of primary school completion rates
page_two <- tabPanel(
  "Primary School Completion",
  titlePanel("Primary School Completion"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region",
        label = "Choose a region",
        choices = unique(education_data$region_code),
        selected = "Middle East and North Africa"
      )
    ),
    mainPanel(plotlyOutput("completion_plot"))
  )
)

# Create a page containing line graphs of no schooling attained rates
page_three <- tabPanel(
  "No Schooling Percentage",
  titlePanel("No Schooling Percentage"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "no_school_region",
                  label = "Choose a region",
                  choices = unique(education_data$region_code),
                  selected = "Middle East and North Africa"
      )
    ),
    mainPanel(plotlyOutput("no_schooling_plot"))
  )
)

# Create a page containing a world choropleth map of average years of schooling
page_four <- tabPanel(
  "Average Years of Schooling",
  titlePanel("Average Years of Schooling"),
  br(),
  plotlyOutput("world_map")
)

# Create a conclusion page
page_five <- tabPanel(
  "Conclusion",
  titlePanel("Conclusion"),
  p("These various visualizations show that, although education inequality has 
    improved, in 2010 there was a lot of youth in the world not receiving a proper
    education.")
)

# Create a page describing the technology we used
page_six <- tabPanel(
  "About the Tech",
  titlePanel("About the Tech"),
  br(),
  p("To create our visualizations for this project we used ggplot, ggmap, and plotly.
    We did not use any APIs."),
  p("For more information read our",
    a("technical report", href ="https://github.com/kaylamchea/info201-project/wiki/Technical-Report")
  )
)

# Create an about us page
page_seven <- tabPanel(
  "About Us",
  titlePanel("About Us"),
  br(),
  h4("Team Members"),
  tags$ul(
    tags$li("Kayla Chea"),
    tags$li("Kevin Chou"),
    tags$li("Sinan Zhou"),
    tags$li("Yanpei Xu")
  )
)

# Pass each page to a multi-page layout
ui <- fluidPage(
  titlePanel("Youth Education Across the World"),
  tabsetPanel(
    page_one,        
    page_two,         
    page_three,       
    page_four,  
    page_five,
    page_six,
    page_seven
  )
)

server <- function(input, output) {
  # Define a bar graph of primary school completion rates to render in the UI
  output$completion_plot <- renderPlotly({
    # Create a data frame of the passed in region's primary school completion 
    # rates in 2010
    get_specific_lpc <- function(selected_region) {
      lpc <- education_data %>%
        filter(year == 2010) %>%
        select(year, country, lpc, region_code) %>%
        group_by(country) %>%
        summarise(lpc = sum(lpc),
                  region_code = region_code) %>%
        filter(region_code == selected_region)
    }
    
    # Create a bar graph of the data frame
    bar_graph <- ggplot(get_specific_lpc(input$region), 
           aes(x = country, y = lpc)) +
      geom_col() +
      coord_flip() +
      labs(title = paste0("% of Primary School Completion in ",
                          input$region, ", 2010"),
           x = "Country", y = "% of Primary School Completion")
    
    # Make the bar graph interactive and return it
    ggplotly(bar_graph)
  })
  
  # Create a line graph for the percentage of no schooling attained by year to
  # render in the UI
  output$no_schooling_plot <- renderPlotly({
    # Create a data frame of the average percentage of no schooling attained 
    # by year for the given region  
    avg_lu_for_region <- function(region_of_interest) {
      education_data %>% 
        filter(region_code == region_of_interest) %>% 
        group_by(year) %>% 
        summarize(lu = mean(lu)) 
    }
    
    # Create a line graph of the data frame
    line_graph <- ggplot(data = avg_lu_for_region(input$no_school_region),
             mapping = aes(x = year, y = lu)) +
        geom_point() +
        geom_line() +
        labs(title = paste("% of No Schooling in", 
                           input$no_school_region,
                           "by Year"),
             x = "Year", y = "% of No Schooling") #+
        #scale_x_continuous(breaks = seq(1950, 2010, 5)) +
        #scale_y_continuous(breaks = seq(0, 100, 10))
    
    # Make the line graph interactive and return it
    ggplotly(line_graph)
  })
  
  # Create a world choropleth map of average years of schooling attained in 2010
  # to render in the UI
  output$world_map <- renderPlotly({
    # Create a data frame of the average years of schooling in 2010 for each country 
    locations <- education_data %>%
      filter(year == "2010") %>%
      select(country, yr_sch)
    
    # Plot the countries' average years of schooling on a world map
    map <- plot_geo(
      type = 'choropleth',
      locations = locations$country,
      locationmode = 'country names',
      z = locations$yr_sch,
      colorscale = 'Blues') %>%
    colorbar(title = "Average Years of Schooling") %>%
    layout(
        title = "Average Years of Schooling Attained by Country, 2010"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

