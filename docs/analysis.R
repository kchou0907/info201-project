# Load ggplot2, and dplyr, plotly, ggmap libraries
library("plotly")
library("ggplot2")
library("dplyr")
library("ggmap")


# Read the file
education_data <- read.csv("../data/barro_lee_dataset.csv", stringsAsFactors = FALSE)

# create a data frame of percentage of no schooling in 2010 in each country. 
locations <- education_data %>%
  filter(year == "2010") %>%
  select(country, lu)

# create a map of no schooling in 2010 in each country
plot_geo(
  type = 'choropleth',
  locations = locations$country,
  locationmode = 'country names',
  colorscale = 'Viridis',
  z = locations$lu
)

# Create a data frame that describes the percentage of primary 
# school completion in 2010
get_specific_lpc <- function(code) {
  lpc <- education_data %>%
    filter(year == 2010) %>%
    select(year, country, lpc, region_code) %>%
    group_by(country) %>%
    summarise(lpc = sum(lpc),
              region_code = region_code) %>%
    filter(region_code == code)
}

# narrows down the data frame to just the region of interest and finds the 
# average % of children not in school in that region
# region_of_interest = the name of the region whose info you want to find
avg_lu_for_region <- function(region_of_interest) {
  education_data %>% 
    filter(region_code == region_of_interest) %>% 
    group_by(year) %>% 
    summarize(lu = mean(lu)) 
}

# makes the plot (dots + smooth line) that shows change in % of kids not in 
# school over time
# area = the data frame for the area that you are looking into
# name = name of the country/region that you are trying to make a graph for
plot_children_not_in_school <- function(area, name) {
  ggplot(data = area, mapping = aes(x = year, y = lu)) +
    geom_point() +
    geom_smooth() +
    labs(title = paste("The Percentage of No Schooling in ", name),
         x = "Year", y = "% of No Schooling") +
    scale_x_continuous(breaks = seq(1950, 2010, 5)) +
    scale_y_continuous(breaks = seq(0, 100, 10))
  
}  

# data frames for all 7 regions
mideast_and_north_africa <- avg_lu_for_region("Middle East and North Africa")
subsaharan_africa <- avg_lu_for_region("Sub-Saharan Africa")
latin_america_and_caribbean <- avg_lu_for_region("Latin America and the Caribbean")
adv_economies <- avg_lu_for_region("Advanced Economies")
south_asia <- avg_lu_for_region("South Asia")
east_asia_and_pacific <- avg_lu_for_region("East Asia and the Pacific")
europe_central_asia <- avg_lu_for_region("Europe and Central Asia")

plot_children_not_in_school(mideast_and_north_africa, "Middle East and North Africa")
plot_children_not_in_school(subsaharan_africa, "Sub-Saharan Africa")
plot_children_not_in_school(latin_america_and_caribbean, "Latin America and the Caribbean")
plot_children_not_in_school(adv_economies, "Advanced Economies")
plot_children_not_in_school(south_asia, "South Asia")
plot_children_not_in_school(east_asia_and_pacific, "East Asia and the Pacific")
plot_children_not_in_school(europe_central_asia, "Europe and Central Asia")
