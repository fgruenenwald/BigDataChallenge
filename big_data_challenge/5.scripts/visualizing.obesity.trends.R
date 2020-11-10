#*******************************************************************************
# VISUALIZING  OBESITY TRENDS IN US AND FRANCE
#*******************************************************************************

#save as spdf in figures
pdf(paste (path.fig, 'us.obesity.trends.pdf', sep = ""))
# Creating space to include three graph in one figure. The par function
# takes one argument mfrow that signifies how many rows and column are require.
# Since we require 3 rows and 1 column value for our 3 graphs, mfrow is (3, 1)
par(mfrow=c(3,1))

# Plotting the rate of Male Obesity vs Year graph
plot(maleObesity$Year, maleObesity$Obesity...., ylim = c(1, 50),
     ylab = 'Male Obesity %', xlab = NA, type = 'l' )

# Plotting the rate of Female Obesity vs Year graph
plot(FemaleObesity$Year, FemaleObesity$Obesity...., ylim = c(1, 50), 
     ylab = 'Female Obesity %', xlab = NA , type = 'l')

# Plotting the rate of obesity in both sexes vs the year graph
plot(bothSexesObesity$Year, bothSexesObesity$Obesity...., ylim = c(1, 50), 
     ylab = 'Both Sexes Obesity %', xlab = 'Year' , type = 'l')

# Creating a space for one graph only to compare the change in obesity rate
# between male and female in one graph.
par(mfrow = c(1, 1))

# Plotting male obesity vs year graph
plot(maleObesity$Year,maleObesity$Obesity....,type="l",col="red", xlab = 'Year',
     ylab = 'Obesity Rate')
#Adding the rate of Female Obesity in the graph above
lines(maleObesity$Year,FemaleObesity$Obesity....,col="green")
# Creating an index to distinguish between two graph.
legend("topleft",
       c("Male Obesity","Female Obesity"),
       fill=c("red","green"))
dev.off()


# create graohs for France

pdf(paste (path.fig, 'fr.obesity.trends.pdf', sep = ""))

par(mfrow=c(3,1))

# Plotting the rate of Male Obesity vs Year graph for France
plot(maleObesityFrance$Year, maleObesityFrance$Obesity...., ylim = c(1, 50),
     ylab = 'Male Obesity %', xlab = NA, type='l' )

# Plotting the rate of Female Obesity vs Year graph for France
plot(FemaleObesityFrance$Year, FemaleObesityFrance$Obesity...., ylim = c(1, 50),
     ylab = 'Female Obesity %', xlab = NA , type='l' )

# Plotting the rate of obesity in both sexes vs the year graph for France
plot(bothSexesObesityFrance$Year, bothSexesObesityFrance$Obesity....,
     ylim = c(1, 50), ylab = 'Both Sexes Obesity %', xlab = 'Year', type='l'  )

par(mfrow = c(1, 1))
# Plotting male obesity vs year graph
plot(maleObesityFrance$Year,maleObesityFrance$Obesity....,type="l",col="red",
     xlab = 'Year', ylab = 'Obesity Rate', ylim = c(0,50))
#Adding the rate of Female Obesity in the graph above
lines(FemaleObesityFrance$Year,FemaleObesityFrance$Obesity....,col="green")
# Creating an index to distinguish between two graph.
legend("topleft",
       c("Male Obesity","Female Obesity"),
       fill=c("red","green"))
dev.off()

