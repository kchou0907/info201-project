source("m.R")

# Load the shiny, ggplot2, and dplyr libraries
library("shiny")
library("ggplot2")
library("dplyr")

# Read the file

page_one <- tabPanel(
  "Background & Research Questions", # label for the tab in the navbar
  titlePanel("Page 1"), # show with a displayed title
  
  # This content uses a sidebar layout
  
 # show with a displayed title
  
  # This content uses a sidebar layout
  p("Our project is ....")
)

# Define content for the second page
page_two <- tabPanel(
  "Visualization",
  titlePanel("The percentage of primary school completion per country based on different region in 2010"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Show user the percentage of primary complete per country based on different region"),
      
      selectInput("region",
                  label = "Choose a region",
                  choices = unique(education_data$region_code),
                  selected = "Middle East and North Africa"
      )
    ),
    
    mainPanel(
      plotOutput("plot"),
      plotlyOutput("map")
    )
  )
)

# Define content for the third page
page_three <- tabPanel(
  "Conclusion" # label for the tab in the navbar
  # ...more content would go here...
)

# Define content for the third page
page_four <- tabPanel(
  "About the Tech" # label for the tab in the navbar
  # ...more content would go here...
)

# Define content for the third page
page_five <- tabPanel(
  "About Us" # label for the tab in the navbar
  # ...more content would go here...
)

ui <- fluidPage(
  titlePanel("Youth Education Across the World"), # application title
  tabsetPanel(
  page_one,         # include the first page content
  page_two,         # include the second page content
  page_three,       # include the third page content
  page_four,        # include the four page content
  page_five         # include the five page content 
  )
)


server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(get_specific_lpc(input$region), 
           aes(x = abbreviate(country), y = lpc, fill = country)
    ) +
    geom_col()
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
  
}

# Pass each page to a multi-page layout (`navbarPage`)

# Run the application 
shinyApp(ui = ui, server = server)

