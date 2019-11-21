# Source the file
source("analysis.R")

# Load the shiny, ggplot2, ggmap, and dplyr libraries
library("shiny")
library("ggplot2")
library("dplyr")
library("ggmap")
library("ggplot2")

# Define content for the first page
page_one <- tabPanel(
  "Background & Research Questions", # label for the tab in the navbar
  titlePanel("Page 1"), # show with a displayed title
  p("Our project is ....")
)

# Define content for the second page
# Display a dropdown menu that lets the user pick one of
# the regions to show the percentage of primany school completion
# per country in that region in 2010
page_two <- tabPanel(
  "Visualization I", 
  titlePanel("The percentage of primary school completion per country 
             based on different region in 2010"), 
  sidebarLayout(
    sidebarPanel(
      helpText("Show user the percentage of primary school completion per 
               country based on different region"),
      selectInput(inputId = "region",
                  label = "Choose a region",
                  choices = unique(education_data$region_code),
                  selected = "Middle East and North Africa"
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define content for the third page
# Display a map of no schooling in 2010 in each country
page_three <- tabPanel(
  "Visualization II",
  titlePanel("A map of no schooling in each country in 2010"),
  plotlyOutput("map")
)

# Define content for the third page
# Display a dropdown menu that lets the user pick one of
# the regions to show the percentage of no schooling 
# in that region in 2010
page_four <- tabPanel(
  "Visualization III",
  titlePanel("The percentage of no schooling in different regions"),
  sidebarLayout(
    sidebarPanel(
      helpText("Show user the percentage of no schooling based 
               on different region"),
      selectInput(inputId = "no_school_region",
                  label = "Choose a region",
                  choices = unique(education_data$region_code),
                  selected = "Middle East and North Africa"
      )
    ),
    mainPanel(
      plotOutput("no_school_plot")
    )
  )
)

# Define content for the third page
page_five <- tabPanel(
  "About Us" # label for the tab in the navbar
  # ...more content would go here...
)

ui <- fluidPage(
  titlePanel("Youth Education Across the World"), 
  tabsetPanel(
  page_one,        
  page_two,         
  page_three,       
  page_four,  
  page_five         
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(get_specific_lpc(input$region), 
           aes(x = abbreviate(country), y = lpc, fill = country)
    ) +
    geom_col() +
    labs(title = paste0("The percentage of primary school completion in ",
                        input$region, " in 2010"),
           x = "Country", y = "% of primary school completion")
  })
  output$map <- renderPlotly({
    map <- plot_geo(
      type = 'choropleth',
      locations = locations$country,
      locationmode = 'country names',
      colorscale = 'Viridis',
      z = locations$lu
    )  
    return(map)
  })
  output$no_school_plot <- renderPlot({
    ggplot(data = avg_lu_for_region(input$no_school_region),
           mapping = aes(x = year, y = lu)) +
      geom_point() +
      geom_smooth() +
      labs(title = paste("The Percentage of No Schooling in", 
                         input$no_school_region),
           x = "Year", y = "% of No Schooling") +
      scale_x_continuous(breaks = seq(1950, 2010, 5)) +
      scale_y_continuous(breaks = seq(0, 100, 10))
  
  })
}

shinyApp(ui = ui, server = server)



