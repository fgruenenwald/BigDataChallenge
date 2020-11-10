#=============================================================================
#TITLE: MAIN SCRIPT - FACTORS FOR OBESITY IN THE U.S.A.
#AUTHORS: CYRUS RAJ GAUTAM, FERDINAND GRUENENWALD
#=============================================================================
#
#
# show which version of R we are using for reproducibility
R.version.string
#  we used : "R version 4.0.3 (2020-10-10)"
#
# =============================================================================
# NOTES
# • 
# • where we got the data from
# • title, authors, date of the last update
#
#
# =============================================================================
# --- global variables ---
# we indicate variable that is static and should remain the same throughout the
# project

wk.dir <- getwd() #get working directory

#load the datasets that we will use:
#Loading the data of Obesity rate and Fast Foood Restaurant

# =============================================================================


#===============================================================================
#Installing various packages to use in our program.
install.packages('maps')
install.packages('mapproj')
install.packages("biscale")
install.packages('leaflet')
install.packages('plyr')
install.packages('dplyr')
install.packages('devtools')
install.packages('Rtools')
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("hadley/lazyeval")
devtools::install_github("hadley/dplyr")

#Calling the installed packages
library(dplyr)
library(plyr)
library(leaflet)
library(biscale)
library(stringr)
library(maps)
library(mapproj)
library(ggplot2)


#=============================================================================
  # --- folder management ---
  
  # names of project folders ("figures", "data.raw","data.clean","results")
  # store names of the folders in an object
  folder.names <- c("1.data.raw","2.data.clean", "3.results",
                    "4.figures")
# and make the folders if they don't exit yet. No need to understand this now
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}

# you need to store in an object the file path to these folders so we can 
# read from them and write to them.
#the raw.data folder should already have the data sets in it
path.data.raw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
path.data.clean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
path.results <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
path.fig <- paste(wk.dir, "/", folder.names[4], "/", sep = "")
# ==============================================================================
# --- run scripts ---
#run getting.to.know.weathercan.R to load data, and make figures

source(paste(wk.dir,"/" ,"5.scripts", "/", "analysis_obesity_trends_us_fr.R",
             sep = ""))
source(paste (wk.dir, "/", "5.scripts", "/", "visualizing.obesity.trends.R", 
              sep = ""))

# ==== end =====================================================================
#===============================================================================
#===============================================================================






# Creating a table that shows how many Fast food chains are in each city
totalRestaurantEachCity <- table(fastFoodClean$city)
head(totalRestaurantEachCity)

# Creating a list of all the fast food restaurant available in our data 
allFastFoodRestName <- fastFoodClean$name

# Creating a data frame of all the fast food restaurant to count the total number
# of each retaurant
df <- data.frame(allFastFoodRestName)
df

# While looking through the data of all the restaurant name, we found 
# something interesting. Even though some restaurants in the data were the same,
# they were counted as different. It was becasuse the restaurant 
# were spelled differently.
# For eg: KFC and kfc were counted differently. Therefore, in order to
# correct these spelling mistake we use the 'which' function that takes
# the name of the wrongly spelled restaurant and equals(changes) it to
# the correct spelling. 

# Correcting the spelling error for KFC. 
df$allFastFoodRestName[which(df$allFastFoodRestName == "Kentucky Fried Chicken" |df$allFastFoodRestName == "KFC - Kentucky Fried Chicken"
             |df$allFastFoodRestName == "KFC Kentucky Fried Chicken"|df$allFastFoodRestName == "KFC AW"|df$allFastFoodRestName == "Kfc")] = "KFC"
# Correcting the spelling error for Mcdonalds
df$allFastFoodRestName[which(df$allFastFoodRestName == "McDonalds Family Restaurant"| df$allFastFoodRestName == "McDonalds Family Restaurant"
             | df$allFastFoodRestName == "McDonalds"|df$allFastFoodRestName == "Mc Donald's"|df$allFastFoodRestName == "Mcdonalds")] = "McDonald's"
# Similarly, Correcting the spelling error for other restaurants.
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mucho Gusto Mexican Kitchen")] = "Mucho Gusto"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mr Sub Sandwiches")] = "Mr Submarine"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Moes Southwest Grill")] = "Moe's Southwest Grill"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Melt Bar and Grilled")] = "Melt Bar & Grilled"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mcalister's Deli"| df$allFastFoodRestName == "McAlisters Deli")] = "McAlister's Deli"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Marys Pizza Shack")] = "Mary's Pizza Shack"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mambo Grill and Tapas")] = "Mambo Grill Tapas"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Mai Tai Restaurant")] = "Mai-Tai Restaurant"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Long John Silvers")] = "Long John Silver's"
df$allFastFoodRestName[which(df$allFastFoodRestName == "L L Hawaiian Barbecue"| df$allFastFoodRestName == "L L Hawaiian Barbeque")] = "L & L Hawaiian Barbecue"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Krystal Burgers")] = "Krystal"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Killer Burger")] = "Killer Burgers"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Jimmy Johns")] = "Jimmy John's"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Jersey Mikes Subs")] = "Jersey Mike's Subs"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Jerry's Subs and Pizza")] = "Jerry's Subs & Pizza"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Jasons Deli")] = "Jason's Deli"
df$allFastFoodRestName[which(df$allFastFoodRestName == "InNOut Burger")] = "In-N-Out Burger"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Hunan Lion Gourmet Chinese")] = "Hunan Lion Gourmet Chinese Dining"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Hot Dog On A Stick")] = "Hot Dog on a Stick"
df$allFastFoodRestName[which(df$allFastFoodRestName == "HomeTown Buffet")] = "Hometown Buffet"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Hardee's/red Burrito"| df$allFastFoodRestName == "Hardee's/Red Burrito")] = "Hardees Red Burrito"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Hardee's Restaurants")] = "Hardee's Restaurant"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Guthries")] = "Guthrie's"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Great Wall Chinese Restaurant")] = "Great Wall Of China Restaurant"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Golden Palace Restaurant")] = "Golden Palace"
df$allFastFoodRestName[which(df$allFastFoodRestName == "George's Gyros Spot 2")] = "George's Gyros Spot"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Full Moon Bar B Que")] = "Full Moon Bar-B-Que"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Frisch's Big Boy Restaurant")] = "Frisch's Big Boy"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Freddys Frozen Custard Steakburgers"| df$allFastFoodRestName == "Freddy's Frozen Custard Steakburgers")] = "Freddy's Frozen Custard & Steakburgers"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Foxs Pizza Den")] = "Fox's Pizza Den"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Five Guys Burgers Fries")] = "Five Guys Burgers & Fries"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Fireplace Restaurant Lounge")] = "Fireplace Restaurant & Lounge"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Farlows on the Water")] = "Farlow's On The Water"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Famous Daves")] = "Famous Dave's"
df$allFastFoodRestName[which(df$allFastFoodRestName == "Emidio & Sons Italian Restaurant")] = "Emidio Sons Italian Restaurant"

# Creating a table with all the corrected restaurants name.
numberOfTotalRestaurant <- table(df)
# Checking the corrected table
numberOfTotalRestaurant
# Trying to plot all the graph of all the restaurant with their numbers. 
# Since the list of restaurant was big this graph won't be useful, but
# you can still make use this to see that some restaurant are opened more
# than others.
plot(table(df))


# Cleaning the date of establishment more to get just the
# years when the restaurants were established
date1 <- fastFoodClean$dateEstablished
# Retriving just the year from our date. 
yearOfEstablishment <- gsub('-.*', '', date1)

# creating a frequency table with years from 2014 to 2019
# and the total number of restaurants
yearOfEstablishmentTable <- table(yearOfEstablishment)

# creating barplot to visualize the above table
barplot(yearOfEstablishmentTable, xlab = 'Years', ylab = 'Number of New Restaurants', col = 'firebrick', las = 1, ylim = c(0, 3000), main = 'New Fast Food Restaurants Openining From 2014-19')

plot(yearOfEstablishmentTable, type = 'l')


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
USA %>%
  leaflet() %>%
  addProviderTiles('CartoDB') %>% 
  addCircleMarkers(radius = 1, color = ~myColor(count))
#----------------------------------------------------------------------------------------------------------
