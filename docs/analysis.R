library("dplyr")
library("ggmap")
library("plotly")

education_data <- read.csv("data/barro_lee_dataset.csv", stringsAsFactors = FALSE)

## Percentage of no schooling in 2010 in each country. 
locations <- education_data %>%
  filter(year == "2010") %>%
  select(country, lu)

plot_geo(
  type = 'choropleth',
  locations = locations$country,
  locationmode = 'country names',
  colorscale = 'Viridis',
  z = locations$lu
)
  