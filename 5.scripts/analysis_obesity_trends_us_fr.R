#*******************************************************************************
# ANALYSIS OF OBESITY TRENDS USA
#*******************************************************************************


#------------load the data from the clean data folder---------------------------
#load data forthe US
obesityUSA <- read.csv(paste( path.data.clean,
                                   'us.obesity1975.cleaned.csv', sep=""))
#load data for france
obesityFrance <- read.csv(paste( path.data.clean,
                                 'fr.obesity1975.cleaned.csv', sep=""))
#------------------------------------------------------------------------------------

#================================================================================

#-----------------seperating categories of obesity rate---------------------------
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
#------------------------------------------------------------------------------------

#================================================================================

#-----------------------checking the quantile for the USA---------------------------

# Checking the quantile of the all group in the USA
quantile(as.numeric(maleObesity$Obesity....))
quantile(as.numeric(FemaleObesity$Obesity....))
quantile(as.numeric(bothSexesObesity$Obesity....))

# From above data we find out that the 50% of male have the obesity rate of approx
# 21% or below whereas approx 50% of the female have the obesity rate of
# 24% or below. This shows us that the female have more obesity than men in
# USA.

#------------------------------------------------------------------------------------

#-----------------------checking the quantile for France---------------------------

# Checking the quantile of the all group in France
quantile(as.numeric(maleObesityFrance$Obesity....))
quantile(as.numeric(FemaleObesityFrance$Obesity....))
quantile(as.numeric(bothSexesObesityFrance$Obesity....))

# From above data we find out that the 50% of male have the obesity rate of
# 13% or below whereas approx 50% of the female have the obesity rate of
# 15% or below. This shows us that the female have more obesity than men in
# France. However, the it also says that 100% male have the obesity rate of
# 22% or lower whereas women have approx 21% or lower. This indicates that
# over the years the obesity in the women has decreased or is comparitevely
# smaller than men in France.
#------------------------------------------------------------------------------------


#================================================================================
#
#------------measuring the correlation coefficent---------------------------

# Seperating our obesity data in order to match the data of the established 
# number of restaurant per year
obesityUSAAfter14 <- obesityUSA %>% filter(Year >= 2014)

# Using the Obesity data to filter out the Both Sexes category of Sex. This is 
# because it gives us the average for the Male and Female population.
obesityUSAAfter14BothSexes <- obesityUSAAfter14 %>% filter(Sex == 'Both sexes')
obesityUSAAfter14BothSexes$Obesity....

# Creating a table that consists the number of total retaurant established that year
yearOfEstablishmentTable <- table(yearOfEstablishment)

# Using that table to create a frequency table because it helps us to filter our data
# for all the year before 2016 easily. We are only extracting the number of restaurant
# established from 2014-2016 to match it with the data from obesity data above.
frequencyTableForEstablishment <- count(yearOfEstablishmentTable)

# Filtering our data
frequencyTableForEstablishmentNew <- frequencyTableForEstablishment %>% filter(as.numeric(as.character(frequencyTableForEstablishment$x.yearOfEstablishment)) <= 2016)


# Using the two different data: obesity and the numbers of fast food restaurant
# established to see the correlation between them. 
cor(frequencyTableForEstablishmentNew$x.Freq, as.numeric(obesityUSAAfter14BothSexes$Obesity....) , method = "pearson")
# Correlation is 0.9993099. This points out that our two datasets have positive linear relationship.
# However, this result cannot be true. It is because we didn't have enough data to
# test the correlation coefficient between the two variable. Even if this test is 
# inconclusive, it definitely gives us hints on how our data might look if we have a
# bigger data set
#------------------------------------------------------------------------------------


#================================================================================
#--------------------------two sample t test---------------------------------

# The frequency of established restaurant on 2014, 2015 and 2016 respectively
frequencyTableForEstablishmentNew$x.Freq

# The frequency of obesity rate of both sexes on 2014, 2015 and 2016
# respectively
as.numeric(obesityUSAAfter14BothSexes$Obesity....)

# Creating a table using the frequencies from above
tableForTTest <- cbind(frequencyTableForEstablishmentNew$x.Freq,
                       as.numeric(obesityUSAAfter14BothSexes$Obesity....))

# Assigning column name for the table
colnames(tableForChiSquared) <- c('ffRestaurant', 'obesityRate')

# Assigning row name for the table
rownames(tableForChiSquared) <- c('2014', '2015', '2016')
tableForChiSquared

# Performing two sample t test. 
t.test(frequencyTableForEstablishmentNew$x.Freq,
       as.numeric(obesityUSAAfter14BothSexes$Obesity....), alternative = "two.sided", 
       var.equal = FALSE)

# t = 1.7398, df = 2, p-value = 0.224
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -1949.503  4596.369
# sample estimates:
#  mean of x  mean of y 
# 1359.00000   35.56667 

# Since the p-value is 0.224, i.e. greater than 0.05 (or 5 percent), it can be concluded 
# that there is no difference between the means. This indicated that the mean of rate of obesity 
# increase is same as the means of increasing number of restaurant per year, which supports our 
# hypothesis.
