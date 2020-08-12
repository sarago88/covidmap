################################################################################
###
### Data analysis in R: Week 9
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Marx Library
### Yale University
###
################################################################################

## Research question: What is the percent increase in Covid-19 cases across states
## in the US since July 1st?

# Load packages

library(tidyverse)
library(choroplethrMaps)
library(cowplot)

# Load data

data("state.map")
states_50 <- state.map
states_48 <- map_data('state')

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Manipulate the covid data frame to include a "percent increase" variable

covid_increase <- covid %>%
  mutate(date = as.Date(date)) %>%
  filter(date == max(date) | date == "2020-07-01") %>%
  select(state, date, cases) %>%
  spread(key = date, value = cases) %>%
  mutate(state = tolower(state),
         difference = `2020-07-21` - `2020-07-01`,
         percent_increase = (difference/`2020-07-01`)*100) %>%
  rename(region = state)

# I took the following steps to manipulate the data set:
# 1. Convert date to date format so that I can index max(date) to get the most recent
# day.
# 2. Filter the data set to include data from the most recent day, and also July 1st.
# 3. I selected only the columns I need. Otherwise, using spread (next step) will create
# two lines per state. I recommend running the pipe with and without this step to visualize
# why this line is important.
# 4. I used "spread" to make two separate columns for early vs. late July cases.
# 5. I can then calculate a difference and a percent increase. I also mutated state in this 
# step to be all lower case. This will make state names identical to (and joinable) with 
# the map data.
# 6. I renamed state as region in order to join it with the map data.

# In the following steps, I made a map separately for the 48 states, HI and AK.
# We can then combine them in one step.

# Join covid_increase the map data of the 48 states

map_data_48 <- states_48 %>%
  inner_join(covid_increase, by = "region")

map_data_hi <- states_50 %>%
  filter(region == "hawaii") %>%
  inner_join(covid_increase, by="region")

map_data_ak <- states_50 %>%
  filter(region == "alaska") %>%
  inner_join(covid_increase, by="region")

# Create yor map! Fill the color of each state according to its percent
# increase in cases.

# Credit for the following code goes to Reid Lewis. He followed this tutorial: 
# https://www.r-bloggers.com/inset-maps-with-ggplot2

# First plot the 48 states

plot48 <- ggplot() + 
  geom_polygon(data = map_data_48, aes(x = long, y = lat, group = group, fill = percent_increase)) + 
  coord_fixed(1.3) +
  scale_fill_gradient(low = "navy", high = "red") +
  theme_nothing() +
  theme(legend.position = "right", plot.title = element_text()) +
  labs(fill = "% increase", title = "Percent increase in July")

plotHI <- ggplot() + 
  geom_polygon(data = map_data_hi, aes(x = long, y = lat, group = group, fill = percent_increase)) + 
  coord_fixed(1.3) + 
  scale_fill_gradient(low = "navy", high = "red", 
                      limits = c(min(covid_increase$percent_increase), 
                                 max(covid_increase$percent_increase))) +
  theme_nothing()

plotAK <- ggplot() + 
  geom_polygon(data = map_data_ak, aes(x = long, y = lat, group = group, fill = percent_increase)) + 
  coord_fixed(1.3) + 
  scale_fill_gradient(low = "navy", high = "red", 
                      limits = c(min(covid_increase$percent_increase), 
                                 max(covid_increase$percent_increase))) +
  theme_nothing()

# Setting the min/max number for scale_fill_gradient is important so that the colors of HI/AK
# are put on the same scale as the full data frame. You will notice that if you omit these lines,
# those states will be colored incorrectly since the color scale includes only one value/color.

# Now we can use ggdraw to combine all three plots!

ggdraw() + 
  draw_plot(plot48) + 
  draw_plot(plotHI, x = 0, y = 0.02, width = 0.25, height = 0.25) + 
  draw_plot(plotAK, x = 0.27, y = 0.02, width = 0.25, height = 0.25)

