page_one <- tabPanel(
  "Background & Research Questions", # label for the tab in the navbar
  titlePanel("Background & Research Questions"), # show with a displayed title
  
  # This content uses a sidebar layout
  p("Our project is ....")
)

# Define content for the second page
page_two <- tabPanel(
  "Visualization" # label for the tab in the navbar
  # ...more content would go here...
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

ui <- navbarPage(
  "Youth Education Across the World", # application title
  page_one,         # include the first page content
  page_two,         # include the second page content
  page_three,       # include the third page content
  page_four,
  page_five
)


# Pass each page to a multi-page layout (`navbarPage`)

# Run the application 
shinyApp(ui = ui, server = server)

