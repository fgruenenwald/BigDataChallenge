#*******************************************************************************
#VISUALIZING & ANALYZING FAST FOOD RESTAURANT DISTRIBUTION ACROSS THE USA
#*******************************************************************************


# Trying to plot the graph of all the restaurant with their numbers. 
# Since the list of restaurant was big this graph won't be useful, but
# you can still make use this to see that some restaurant are opened more
# than others.
plot(table(df), ylab = 'Frequency', ylim = c(0, 1000))

# creating a frequency table with years from 2014 to 2019
# and the total number of restaurants
yearOfEstablishmentTable <- table(fastFoodClean$dateEstablished)

#safe plot as pdf
pdf(paste(path.fig , 'NewFastFoodRestaurants.pdf', sep =""))

# creating barplot to visualize the above table
barplot(yearOfEstablishmentTable, xlab = 'Years',
        ylab = 'Number of New Restaurants', col = 'firebrick', las = 1, 
        ylim = c(0, 3000),
        main = 'New Fast Food Restaurants Openining From 2014-19')
dev.off()

# Filtering the main data according to the country. If the fast food restaurant 
# is not in the USA it is removed from the table 
USA <- fastFood %>% filter(country == "US")

# Creating a table with city, province, longitutde and latitude
USA <- USA %>% group_by(city, province, longitude, latitude) %>%
  dplyr::summarise(count = n()) %>%
  arrange(desc(count))

# Specifying my own color to use
myColor <- colorNumeric(palette = 'RdYlBu', domain = c(1:2300))


# Using the leaflet package and CartoDB provider to plot all the places
# restaurant in the map of USA. Magnifying the graph shows that there are
# more restaurant in the west and east side than the middle.

#you will have to safe the map manually

USA %>%
  leaflet() %>%
  addProviderTiles('CartoDB') %>% 
  addCircleMarkers(radius = 1, color = ~myColor(count))

