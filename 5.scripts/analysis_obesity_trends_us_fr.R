#*******************************************************************************
# ANALYSIS OF OBESITY TRENDS USA

#------------load the data from the clean data folder---------------------------
#load data forthe US
obesityUSA <- read.csv(paste( path.data.clean,
                                   'us.obesity1975.cleaned.csv', sep=""))
#load data for france
obesityFrance <- read.csv(paste( path.data.clean,
                                 'fr.obesity1975.cleaned.csv', sep=""))

#-----------------seperating categories-----------------------------------------
# Filtering the obesity rate of male 
maleObesity <- obesityUSA %>% filter(Sex == 'Male')
maleObesity
maleObesityFrance <- obesityFrance %>% filter(Sex == 'Male')

# Filtering the obesity rate of Female
FemaleObesity <- obesityUSA %>% filter(Sex == 'Female')
FemaleObesityFrance <- obesityFrance %>% filter(Sex == 'Female')

# Filtering the obesity rate of both sexes
bothSexesObesity <- obesityUSA %>% filter(Sex == 'Both sexes')
bothSexesObesityFrance <- obesityFrance %>% filter(Sex == 'Both sexes')
# Removing everything after the rate of Obesity. For eg: our obesity
# rate is given like: 10.3 [20.7, 20.3], so extracting just the rate i.e.10.3
# from all sexes



# Extracting the rate of Obesity in Men
maleObesity$Obesity.... <- gsub(' .*', '', maleObesity$Obesity....)
maleObesity
maleObesityFrance$Obesity.... <- gsub(' .*', '', maleObesityFrance$Obesity....)

# Extracting the rate of obesity in Female
FemaleObesity$Obesity.... <- gsub(' .*', '', FemaleObesity$Obesity....)
FemaleObesityFrance$Obesity.... <- gsub(' .*', '',
                                        FemaleObesityFrance$Obesity....)

# Extracting the rate of obesity in bothsexes
bothSexesObesity$Obesity.... <- gsub(' .*', '', bothSexesObesity$Obesity....)
bothSexesObesityFrance$Obesity.... <- gsub(' .*', '',
                                           bothSexesObesityFrance$Obesity....)
obesityUSA



# Checking the quantile of the all group in the USA
quantile(as.numeric(maleObesity$Obesity....))
quantile(as.numeric(FemaleObesity$Obesity....))
quantile(as.numeric(bothSexesObesity$Obesity....))

# Checking the quantile of the all group in France
quantile(as.numeric(maleObesityFrance$Obesity....))
quantile(as.numeric(FemaleObesityFrance$Obesity....))
quantile(as.numeric(bothSexesObesityFrance$Obesity....))

#==============================================================================

fastFoodClean <- read.csv(paste(path.data.clean, 'fastFoodRestaurantClean.csv',
                                sep = ""))

# Creating a table that consists the number of total retaurant established that year
yearOfEstablishmentTable <- table(fastFoodClean$dateEstablished)
yearOfEstablishmentTable

# Seperating our obesity data in order to match the data of the established number of restaurant per year
obesityUSAAfter14 <- obesityUSA %>% filter(Year >= 2014)

# Using the Obesity data to filter out the Both Sexes category of Sex. This is because
# It gives us the average for the Male and Female population.
obesityUSAAfter14BothSexes <- obesityUSAAfter14 %>% filter(Sex == 'Both sexes')
obesityUSAAfter14BothSexes$Obesity....

# Using that table to create a frequency table because it helps us to filter our data
# for all the year before 2016 easily. We are only extracting the number of restaurant
# established from 2014-2016 to match it with the data from obesity data above.
frequencyTableForEstablishment <- count(yearOfEstablishmentTable)

# Filtering our data
frequencyTableForEstablishmentNew <- frequencyTableForEstablishment %>% filter(as.numeric(as.character(frequencyTableForEstablishment$x.Var1)) <= 2016)


# Using the two different data: obesity and the numbers of fast food restaurant
# established to see the correlation between them. 
cor(frequencyTableForEstablishmentNew$x.Freq, as.numeric(obesityUSAAfter14BothSexes$Obesity....) , method = "pearson")
# Correlation is 0.9993099. This points out that our two datasets have positive linear relationship.
# However, this result cannot be true. It is because we didn't have enough data to
# test the correlation coefficient between the two variable. Even if this test is 
# inconclusive, it definitely gives us hints on how our data might look if we have a
# bigger data set