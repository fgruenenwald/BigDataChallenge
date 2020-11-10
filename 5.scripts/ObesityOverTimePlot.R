#create figure showing progression of obesity in the US overtime for both sexes

#extract data for United states of America andBoth sexes Obesity
us.ob<- as.data.frame( ob.c[ob.c$Country =='United States of America' & 
              ob.c$Sex == 'Both sexes',], strings.as.factors = TRUE )
#create a pdf graph
pdf(file = paste(path.fig, 'Obese_pop_vs_time' )) #opens the pdf device

#the plot we will draw year vs. Obesity levels
plot(us.ob$Year, us.ob$Obesity, xlab = 'Year',  
     ylab = 'Obese Population in the USA (%)', ylim = c(0, 100) )
#add error bars
us.ob.min <- as.numeric(us.ob$min.Obesity) #minimum obesity each year
us.ob.max <- as.numeric(us.ob$max.Obesity) #maximum obesity each year
arrows(x0= us.ob$Year, y0=  us.ob.min, #lower bound 
       x1= us.ob$Year, y1=  us.ob.max, #upper bound
       code=3, angle=90, length=0.1) #format arrows as error bars

dev.off() #close the pdf writing device
