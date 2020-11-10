
#---------------Cleaning the world wide obesity data---------------------------

# Since our Obesity data has all the countries data, we filter the data
# of the USA using the filter function.
obesityUSA <- obesity %>% filter(Country == "United States of America")
# %>% passes the left hand side of the operator to the first argument of the 
# right hand side of the operator

# checking the column name of the filtered data
names(obesityUSA)

#create clean data table
obesityUSA$Obesity....
obesityChanged <- gsub(' .*', '', obesityUSA$Obesity....)
obesityUSA$Obesity.... <- obesityChanged
obesityUSA #show clean data table

# Creating a table containing Year, Obesity rate and Sex.
obesityUSA <- obesityUSA %>% group_by(Year, Obesity...., Sex)%>%
  dplyr::summarise(count = n())

# Filtering the obesity rate of male 
maleObesity <- obesityUSA %>% filter(Sex == 'Male')
# Filtering the obesity rate of Female
FemaleObesity <- obesityUSA %>% filter(Sex == 'Female')
# Filtering the obesity rate of both sexes
bothSexesObesity <- obesityUSA %>% filter(Sex == 'Both sexes')

# Removing everything after the rate of Obesity. For eg: our obesity
# rate is given like: 10.3 [20.7, 20.3], so extracting just the rate i.e.10.3
# from all sexes

# Extracting the rate of Obesity in Men
maleObesity$Obesity.... <- gsub(' .*', '', maleObesity$Obesity....)
# Extracting the rate of obesity in Female
FemaleObesity$Obesity.... <- gsub(' .*', '', FemaleObesity$Obesity....)
# Extracting the rate of obesity in bothsexes
bothSexesObesity$Obesity.... <- gsub(' .*', '', bothSexesObesity$Obesity....)

#---------------store cleaned dataset in 2.clean.data folder--------------------

write.csv(obesityUSA, 
          paste(path.data.clean,"us.obesity1975.cleaned.csv", sep = ""), 
          row.names = FALSE)



#===============================================================================
# clean data for france
obesityFrance <- obesity %>% filter(Country == 'France')
obesityFrance$Obesity....
obesityChanged <- gsub(' .*', '', obesityFrance$Obesity....)
obesityFrance$Obesity.... <- obesityChanged
obesityFrance
obesityFrance <- obesityFrance %>% group_by(Year, Obesity...., Sex)%>%
  dplyr::summarise(count = n())

#store the table for france in cleaned data folder
write.csv(obesityFrance, 
          paste(path.data.clean,"fr.obesity1975.cleaned.csv", sep = ""), 
          row.names = FALSE)


#===============================================================================
#---------------------------------For Fast Food Restaurant----------------------

# Loading the Fast Food restaurant graph
fastFood <- read.csv(file = paste(path.data.raw,'fastFoodRestaurant.csv', 
                                  sep = "" ))

# check the column of our data
names(fastFood)

# Making your own new table by deleting certain column from the main data and changing the column name
fastFoodClean <- data.frame(name = fastFood$name, dateEstablished = fastFood$dateAdded, address = fastFood$address, city = fastFood$city, country = fastFood$country)

# Checking the new table and its column name
fastFoodClean
names(fastFoodClean)

# Trying to clean the establised data information. For now the date is displayed as "2019-05-19T23:58:05Z". 
# Removing everything after T from the above example by using a single date first.

# The first date in the tabel
date1 <- fastFoodClean$dateEstablished[1]
# Removing everything after T using gsub function. In this case, gsub takes 
# three argument.first argument takes the what you want to remove, second 
# argument takes what you want to replace it with(in this case nothing) and 
# third it takes the name of the list containing the data
gsub('T.*', '', date1)


# Applying the above proceess to Remove everything after T from rest of the date.
# All the date from out data
allDate <- fastFoodClean$dateEstablished
# Applying the gsub function to all our data
fastFoodCleanDate<-gsub("T.*","",allDate)
# Checking the change
fastFoodCleanDate


# Creating another table with the improved date from above
fastFoodClean <- data.frame(name = fastFood$name, dateEstablished = fastFood$dateAdded, city = fastFood$city, country = fastFood$country, longitude = fastFood$longitude, latitude = fastFood$latitude)
fastFoodClean$dateEstablished <- gsub('-.*', '', fastFoodClean$dateEstablished )
head(fastFoodClean)

write.csv(fastFoodClean, 
          paste(path.data.clean,"fastFoodRestaurantClean.csv", sep = ""), 
          row.names = FALSE)




