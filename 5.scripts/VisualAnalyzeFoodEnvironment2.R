#load the data
my.data <- read.csv (paste (path.data.clean, 'StateAndCountyDataClean.csv', 
                            sep = ""))
str(my.data)
head(my.data)

#load variable names and descriptions
variables <- read.csv(paste(path.data.clean, 'VariableListClean.csv',sep = ""))


#===============================================================================
#----function that plots all the variables against obesity correlation__________
corr.plt <- function(var.cd.rsp, var.cd.exp){
  
  #create list of all states
  States <- c(sort(unique(my.data$State)))
  
  #---------create vector of mean response variable for each state--------------
  # create output vector list that can be filled with means of explanatory
  # variable for each state
  St.var.cd.exp <- c(rep(NA, length(States)))
  
  #---------create vector of mean explanatory variable for each state-----------
  #retrieve explanatory variable data
  var.cd.exp.data <- my.data[my.data$Variable_Code == var.cd.exp, ]
  
  #calulate the mean of that variable per county for each state 
  for (i in 1: length(States) ){
    St.var.cd.exp[i] <- mean(c(var.cd.exp.data$Value 
                               [var.cd.exp.data$State ==  States[i]]))
  }
  
  #---------create vector of mean response variable for each state--------------
  # create output vector list that can be filled with means of response variable
  # for each state
  St.var.cd.rsp <- c(rep(NA, length(States)))
  #retrieve response variable data
  var.cd.rsp.data <- my.data[my.data$Variable_Code == var.cd.rsp, ]
  #calulate the mean of that variable per county for each state
  for (i in 1: length(States) ){
    St.var.cd.rsp[i] <- mean(c(var.cd.rsp.data$Value 
                                  [var.cd.rsp.data$State ==  States[i]]))
  }
  
  #reponse variable name
  rsp.name <- variables$Variable_Name [which(variables$Variable_Code == 
                                               var.cd.rsp)]
  #reponse variable name
  exp.name <- variables$Variable_Name [which(variables$Variable_Code == 
                                              var.cd.exp)]

  #make a data frame with two collumns : explanatory and response variable
  data <- data.frame(var.cd.exp = St.var.cd.exp,
                     var.cd.rsp = St.var.cd.rsp)
  #perform a correlation test:
  correlation <- cor.test(data$var.cd.exp, data$var.cd.rsp, method = 'pearson')
  
  # -------------create a plot with linear regression model --------------------
  plt <- plot(St.var.cd.exp, St.var.cd.rsp, xlab = exp.name, ylab = rsp.name)
  
  #create the linear model
  z <- lm(St.var.cd.rsp ~ St.var.cd.exp, data = data)
  abline(z, col = "blue") #add the linear model
  mtext( paste("correlation estimate:", round(correlation$estimate, digits = 4),
               ",    p-value:", round(correlation$p.value, digits = 4)) ,
         side = 3, line = 1) #add results from correlation test in graph
  return(z)
}


#-----------------implementations-----------------------------------------------
# since we want to see which factors influence obesity rates, our response 
# variables are all variables concerned with obesity rates: 

#  X             Variable_Name         Variable_Code
#258 Adult obesity rate, 2012*    PCT_OBESE_ADULTS12
#259 Adult obesity rate, 2017*    PCT_OBESE_ADULTS17

#so, we create a list of response variable codes:
var.resp  <- c('PCT_OBESE_ADULTS12', 'PCT_OBESE_ADULTS17')

#make a lot of graphs correlations to more interesting variables
# for variables of the category Restaurants
rest.exp <- variables$Variable_Code [variables$Category_Code ==
                                            'RESTAURANTS']
#create graphs of explanatory variables toresponse variables
#looping through all combinations of response and explanatory variables
#and safe all plots in a pdf
pdf(paste(path.fig, 'ObesityRestaurantCorrelation.pdf', sep = "" ))
for(r in 1:length (var.resp)){
  for (i in 1:length(rest.exp )){
  corr.plt(var.resp[r], rest.exp[i])
  }
}
dev.off()


#---------------for socioeconimic variables of the category--------------------- 
soc.exp <- variables$Variable_Code[variables$Category_Code ==
                                            'SOCIOECONOMIC']
pdf(paste(path.fig, 'ObesitySocioeconomicCorrelation.pdf', sep = "" ))
for(r in 1:length (var.resp)){
  for (i in 1:length(soc.exp )){
    corr.plt(var.resp[r], soc.exp[i])
  }
}
dev.off()

# --------------for Health variables----------------------------------
hlt.exp <- variables$Variable_Code[variables$Category_Code ==
                                            'HEALTH']

pdf(paste(path.fig, 'ObesityHealthCorrelation.pdf', sep = "" ))
for(r in 1:length (var.resp)){
  for (i in 1:length(hlt.exp  )){
    corr.plt(var.resp[r], hlt.exp [i])
  }
}
dev.off()

#===============================================================================
# Numerical analysis
#*********************
#*
#*I want towrite my result data into a data frame so I can find the strongest 
#*correlations
#*How canwe dothat? 
#*the idea for the data frame is to have a column with explanatory variable 
#*name,exp variable code response variable name, response variable code
#*then thetest results as estimated.correlation, p-value, confidence interval.
#*
#* We will need a functiontodo that
#* this function will take in a list of response variables, and a list of 
#* explanatory variables.
#* The output will be a list data frame with variables and test values 
exp.variables <- c(rest.exp, soc.exp, hlt.exp[1:2], hlt.exp[5:length(hlt.exp)])
resp.variables <- c(var.resp)


fea.rslts <- data.frame(explanatory.variable.name = c(NA),
                        response.variable.name = c(NA),
                        explanatory.variable.code = c(NA),
                        response.variable.code = c(NA),
                        correlation.estimate = c(NA),
                        correlation.p_value = c(NA))
                        
                  

#===============================================================================
#function that performs a correlation test between two variables and adds 
#test results to a existing data frame--- __________
add.corr.tst <- function(var.cd.rsp, var.cd.exp){
  
  #create list of all states
  States <- c(sort(unique(my.data$State)))
  element <- e * r
  #---------create vector of mean response variable for each state--------------
  # create output vector list that can be filled with means of explanatory
  # variable for each state
  St.var.cd.exp <- c(rep(NA, length(States)))
  
  #---------create vector of mean explanatory variable for each state-----------
  #retrieve explanatory variable data
  var.cd.exp.data <- my.data[my.data$Variable_Code == var.cd.exp, ]
  
  #calulate the mean of that variable per county for each state 
  for (i in 1: length(States) ){
    St.var.cd.exp[i] <- mean(c(var.cd.exp.data$Value 
                               [var.cd.exp.data$State ==  States[i]]))
  }
  
  #---------create vector of mean response variable for each state--------------
  # create output vector list that can be filled with means of response variable
  # for each state
  St.var.cd.rsp <- c(rep(NA, length(States)))
  #retrieve response variable data
  var.cd.rsp.data <- my.data[my.data$Variable_Code == var.cd.rsp, ]
  #calulate the mean of that variable per county for each state
  for (i in 1: length(States) ){
    St.var.cd.rsp[i] <- mean(c(var.cd.rsp.data$Value 
                               [var.cd.rsp.data$State ==  States[i]]))
  }
  
  #reponse variable name
  rsp.name <- variables$Variable_Name [which(variables$Variable_Code == 
                                               var.cd.rsp)]
  #reponse variable name
  exp.name <- variables$Variable_Name [which(variables$Variable_Code == 
                                               var.cd.exp)][1]
  
  #make a data frame with two collumns : explanatory and response variable
  data <- data.frame(var.cd.exp = St.var.cd.exp,
                     var.cd.rsp = St.var.cd.rsp)
  #perform a correlation test:
  correlation <- cor.test(data$var.cd.exp, data$var.cd.rsp, method = 'pearson')
  
  
  fea.rslts$explanatory.variable.name[element] <- exp.name
  fea.rslts$response.variable.name[element] <-  rsp.name
  fea.rslts$explanatory.variable.code[element] <-  var.cd.exp
  fea.rslts$response.variable.code[element] <-  var.cd.rsp
  fea.rslts$correlation.estimate[element] <-  correlation$estimate
  fea.rslts$correlation.p_value[element] <-  correlation$p.value
  
  
  new.obs <- data.frame(explanatory.variable.name = exp.name,
                         response.variable.name =rsp.name,
                          explanatory.variable.code = var.cd.exp,
                          response.variable.code = var.cd.rsp,
                          correlation.estimate = correlation$estimate,
                          correlation.p_value = correlation$p.value)
  #print(new.obs)
                          
  #append the fea.rslt data frame with the test results
  return( new.obs)


}
  
#---------------implementation to write results---------------------------------

#create a vector of all variable codes we looked at int the visualization:


for (r in 1: length(resp.variables)){
  for (e in 1: length(exp.variables)){
   fea.rslts <- rbind (fea.rslts, add.corr.tst(resp.variables[r],exp.variables[e] ))
  }
}
fea.rslts <-na.omit(fea.rslts)

#find the factors with maximal correlation by showing the results ordered by 
#descending absolute correlation estimates:
fea.rslts <- fea.rslts[order(abs(fea.rslts$correlation.estimate), 
                             decreasing= TRUE),]

write.csv(fea.rslts, paste(path.results, 'ObesityCorrelations2012.csv'))
?order

