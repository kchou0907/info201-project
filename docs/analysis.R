# Load ggplot2, and dplyr libraries
library("plotly")
library("ggplot2")
library("dplyr")
library("ggmap")


# Read the data
education_data <- read.csv("../data/barro_lee_dataset.csv", stringsAsFactors = FALSE)

# Plot
get_specific_lpc <- function(code) {
  lpc <- education_data %>%
    filter(year == 2010) %>%
    select(year, country, lpc, region_code) %>%
    group_by(country) %>%
    summarise(lpc = sum(lpc),
              region_code = region_code) %>%
    filter(region_code == code)
}

lpc_region <- get_specific_lpc("Middle East and North Africa")


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