#FoodEnvironmentAtlas

#load the data------------------------------------------------------------------
#load health, food and environment related data for counties in the US
my.data <- read.csv('obesity/FoodEnvironmentAtlas/StateAndCountyData.csv',
                    strip.white = TRUE, stringsAsFactors = FALSE)

#load variable names and descriptions
variables <- read.csv('obesity/FoodEnvironmentAtlas/VariableList.csv')

#explore health data structure
str(my.data)
head(my.data)


# **************************VARIABLES*******************************************

#which varibales do we want to look at?

variables[variables$Variable_Code == 'PCH_LACCESS_POP_10_15', ]

# view allvariables that fall into fast food  subcategory
# look at all variables concerned with fast food
fa.foo.var <- variables [variables$Subcategory_Name == "Fast-food", ]
fa.foo.var
#the most interesting variables are:
#                                         Variable_Name
#81               Fast-food restaurants/1,000 pop, 2011
#82               Fast-food restaurants/1,000 pop, 2016
#83 Fast-food restaurants/1,000 pop (% change), 2011-16

#                              Category_Name Category_Code
#81 Restaurant Availability and Expenditures   RESTAURANTS
#82 Restaurant Availability and Expenditures   RESTAURANTS
#83 Restaurant Availability and Expenditures   RESTAURANTS

#Subcategory_Name    Variable_Code Geography           Units
#81        Fast-food         FFRPTH11    CNTY10 # per 1,000 pop
#82        Fast-food         FFRPTH16    CNTY10 # per 1,000 pop
#83        Fast-food PCH_FFRPTH_11_16    CNTY10        % change


# look at all variables concerned with health 
h.var <- variables [variables$Subcategory_Name == 'Health',]
h.var
# we are interested in obesity rates so we will look at:
#258 Adult obesity rate, 2012* Health and Physical Activity        HEALTH
#259 Adult obesity rate, 2017* Health and Physical Activity        HEALTH

#258           Health    PCT_OBESE_ADULTS12    CNTY10 Percent
#259           Health    PCT_OBESE_ADULTS17    CNTY10 Percent

#let's remember the varible codes sowe can refer to these variables quickly
# this is the variable code for adult obesity rates in 2012
ob12 <  'PCT_OBESE_ADULTS12' 

# this is the variable code for adult obesity rates in 2017
 'PCT_OBESE_ADULTS17' 

#check if all entries are the same for each state
States <- c(sort(unique(my.data$State))) #list states alphabetically
St.var <- c(rep(NA, length(States))) #variances of each state

#calculate the variance inobesity data for all states
for (i in 1: length(States) ){
  #add variance forthe ithstate to the variance vector
  St.var[i] <- var(ob17$Value[ ob17$State == States[i]])
}

#we see that the obesity rates were entered per state and not per county
# all have a variance of 0, which seem imopossible. DC only has one entry,
#hence the variance is NA.

#***********************************VISUALIZATION*******************************
#let's try to visualize some of the data:
#correlation of fast food restaurants/ 1000 and obesity rate in each county:


#dataframe of all obesity data in 2017
ob17 <- my.data[ my.data$Variable_Code == 'PCT_OBESE_ADULTS17', ]
#names(ob17)[5] <- "Obesity.%"

#data frame of fast food restaurant density in each county in year 2016
fast.food17 <-  my.data[my.data$Variable_Code == 'FFRPTH16', ]
#names(fast.food17)[5] <-"fast.food.restaurant.density.num.per.1000.pop"


#a not so useful correlation plot:
plot(ob17$Value, fast.food17$Value)
# this plot isnot useful,because obesity data is all the sameoncountylevel.
#we have to turnitinto state level

#so Let us calculate the mean fastfood restaurant density for each state:

States <- c(sort(unique(my.data$State))) #list states alphabetically
St.mean.ff.dens <- c(rep(NA, length(States))) #mean of each state (empty list)

#calculate the mean in obesity data for all states
for (i in 1: length(States) ){
  #add mean for the ithstate to the mean vector
  St.mean.ff.dens[i] <- mean(fast.food17$Value[fast.food17$State == States[i]])
}

#similarly , we retrieve the values for obesity rates for each state
St.mean.ob17 <- c(rep(NA, length(States)))
for (i in 1: length(States) ){
  #add mean for the ithstate to the mean vector
  St.mean.ob17[i] <- mean(ob17$Value[ob17$State == States[i]])
}
St.mean.ob17

#visualize the correlation of mean fast food restaurant density and mean obesity
#rates

plot( St.mean.ob17, St.mean.ff.dens,
     ylab = "fast food restaurant density (#/1000 pop)",
     xlab = "obesity rate of adults in percent in 2017")


#to do
#table of states and obesity
state.obesity <- data.frame( States = States,
                             Adult.obesity.rate.2017 = St.mean.ob17)

state.obesity[order(state.obesity$Adult.obesity.rate.2017),]

#--------what about the change in fast food restaurants density-----------------


#****************************confounding variables******************************
#other interesting vatiables to look at: 

#---------------Food insecurity
#Household food insecurity (%, three-year average), 2012-14*	Food Insecurity	INSECURITY	State Food Insecurity	FOODINSEC_12_14	CNTY10	Percent
#Household food insecurity (%, three-year average), 2015-17*	Food Insecurity	INSECURITY	State Food Insecurity	FOODINSEC_15_17	CNTY10	Percent
#Household food insecurity (change %),2012-14 to 2015-17*	Food Insecurity	INSECURITY	State Food Insecurity	CH_FOODINSEC_14_17	CNTY10	Percentage points
#Household very low food security (%, three-year average), 2012-14*	Food Insecurity	INSECURITY	State Food Insecurity	VLFOODSEC_12_14	CNTY10	Percent
#Household very low food security (%, three-year average), 2015-17*	Food Insecurity	INSECURITY	State Food Insecurity	VLFOODSEC_15_17	CNTY10	Percent
#Household very low food security (change %), 2012-14 to 2015-17*	Food Insecurity	INSECURITY	State Food Insecurity	CH_VLFOODSEC_14_17	CNTY10	Percentage points

#a data frame with food insecurity data averaged from 2015 to 2017
f.insec_15_17 <- my.data[my.data$Variable_Code == 'FOODINSEC_15_17',]
head(f.insec_15_17)

#-----check if all entries are the same for each state-------
States <- c(sort(unique(my.data$State))) #list states alphabetically
St.insec.var <- c(rep(NA, length(States))) #variances of each state

#calculate the variance in insecurity data for all states
for (i in 1: length(States) ){
  #add variance forthe ith state to the variance vector
  St.insec[i].var <- var(f.insec_15_17$Value[ ob17$State == States[i]])
}
# all entries are 0. Therefore, all counties have the same food insecurity in
# each state in my.data.

#so, let's make a list of the social insecuritry data in from 2017
St.insec <- c(rep(NA, length(States))) #variances of each state

#calculate the variance in insecurity data for all states
for (i in 1: length(States) ){
  St.insec[i] <- c(f.insec_15_17$Value[ ob17$State == States[i]])[1]
}

plot( St.insec, St.mean.ob17, ylab = 'Adult Obesity rates per statein 2017',
      xlab = 'Food insecurity(%)')

#--------------------------------per capita expenditure------------------------
#the variable code for per capita expenditure is:
#PC_FFRSALES07
#it ismeasurerd in dollars

St.insec <- c(rep(NA, length(States))) #variances of each state

#calculate the variance in insecurity data for all states
for (i in 1: length(States) ){
  St.insec[i] <- c(f.insec_15_17$Value[ ob17$State == States[i]])[1]
}
data.test <- data.frame(St.insec = St.insec, Obesity = St.mean.ob17)

lm(St.insec ~ Obesity , data = data.test)


plot( St.insec, St.mean.ob17, ylab = 'Adult Obesity rates per statein 2017',
      xlab = 'Food insecurity(%)')

#try calulating the correlation coefficient

cor()

#===============================================================================
#___write a function that plots all the variables against obesity_______________
#we will asuume all values are only written for states
corr.to.ob17 <- function(variable_code){
  States <- c(sort(unique(my.data$State)))
  St.variable_code <- c(rep(NA, length(States)))
  #retrieve variable data
  variable_code.data <- my.data[my.data$Variable_Code == variable_code, ]
  #calulate the mean for each state for the variable
  for (i in 1: length(States) ){
    St.variable_code[i] <- mean(c(variable_code.data$Value 
                                  [variable_code.data$State ==  States[i]]))
  }

  #make a data frame with two collumns : obesity and variable
  data <- data.frame(Obesity = St.mean.ob17,
                     St.variable_code = St.variable_code)
  
  # create a plot with linear regression model 
  plt <- plot(St.variable_code, St.mean.ob17,
              xlab = variables$Variable_Name [which(variables$Variable_Code == 
                                                      variable_code)],
              ylab = 'Adult Obesity rates per state in 2017')
  z <- lm(Obesity ~ St.variable_code, data = data)
  abline(z, col = "blue")
  
  #install.packages("ggpubr")
  #library('ggpubr')
  #plt2 <- ggscatter(data, x = "Obesity", y = "St.variable_code", 
   #         add = "reg.line", cor.coef = TRUE, cor.method = "pearson",
    #        xlab = variables$Variable_Name [which(variables$Variable_Code == 
     #                                               variable_code)],
      #      ylab = 'Adult Obesity rates per state in 2017', add = "reg.line")
  return(z)
}


#-----------------implementations-----------------------------------------------
#make a lot of graphs correlations to more interesting variables
# for variables of the category Restaurants
iteration.list <- variables$Variable_Code[variables$Category_Code ==
                                               'RESTAURANTS']

for (i in 1:length(iteration.list)){
  corr.to.ob17(iteration.list[i])
}


#---------------for socioeconimic variables of the category--------------------- 
iteration.list <- variables$Variable_Code[variables$Category_Code ==
                                            'SOCIOECONOMIC']
for (i in 1:length(iteration.list)){
  corr.to.ob17(iteration.list[i])
}

# --------------for Health variables----------------------------------
iteration.list <- variables$Variable_Code[variables$Category_Code ==
'HEALTH']

for (i in 1:length(iteration.list)){
  corr.to.ob17(iteration.list[i])
}

# --------------for Accessibility variables----------------------------------
#iteration.list <- variables$Variable_Code[variables$Category_Code ==
#                                            'ACCESS']

#for (i in 1:length(iteration.list)){
#  corr.to.ob17(iteration.list[i])
#}

   
     
     
     