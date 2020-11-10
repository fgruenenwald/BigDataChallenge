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

