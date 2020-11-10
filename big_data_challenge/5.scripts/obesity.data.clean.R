

#obesity data cleaning
ob <- read.csv("obesity/Obesity_in_different_countries_at_different_time_cleaned.csv")
str(ob)

#see which countries we have data for
unique(obesity$Country) # we see 195 different countries. that are all countries


#change obesity entries into three columns Obesity min.Obesity and max.Obesity
#first Obesity
#us.ob.n <- us.ob['Both sexes' == us.ob$Sex, ]
Obesity <- gsub( " .*", "" , ob$Obesity....)

#create a vector min.obesity for containing all minimum estimates of obesity %
min.Obesity.t <- gsub( "-.*", "" , ob$Obesity....) #cut away the max estimate
min.Obesity <- gsub( ".*\\[", "" , min.Obesity.t )# cut everything before [
min.Obesity

#create a vector with max.obesity
#keep every character after the - except for ]
max.Obesity.t <- gsub( ".*-", "" , ob$Obesity....)#cut characters before "-" 
max.Obesity <- gsub( "\\]", "", max.Obesity.t)  #remove "]"

#replace Obesity.... column in data frame with Obesity vector
ob.c <- data.frame(X = ob$X, Country = ob$Country, Year= ob$Year, 
                   Obesity = Obesity, min.Obesity= min.Obesity,
                   max.Obesity= max.Obesity, Sex = ob$Sex )
?write.csv
write.csv()
