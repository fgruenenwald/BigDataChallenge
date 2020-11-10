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
source(paste (wk.dir, "/", "5.scripts", "/", 
              "visualizing.fastFoodRestaurants.R", 
              sep = ""))

#consider confounding variables and other factors
source(paste (wk.dir, "/", "5.scripts", "/", "VisualAnalyzeFoodEnvironment2.R",
              sep = ""))

# ==== end =====================================================================
#===============================================================================
#===============================================================================

