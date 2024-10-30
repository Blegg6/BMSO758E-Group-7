rm( list=ls() )
workdir = "C:/Users/Michel Wedel/Dropbox/Courses/BUMK742 (2018)/R-codes/Project5"
setwd(workdir)

install.packages("dplyr")
library(dplyr) #to use arrange function below

project = read.csv("coffeemakers.csv")
attach(project)
str(project)
summary(project)

# names(project)
project$experiment = 1:nrow(project) #Indicating an index for each choice experiment

###################################################################
### To make variable names consistent with other variable names ###
###################################################################
names(project)[ which( names(project) == "Philips1" ) ] = "Philips_1"
names(project)[ which( names(project) == "Krups1" ) ] = "Krups_1"

names(project)[ which( names(project) == "Philips2" ) ] = "Philips_2"
names(project)[ which( names(project) == "Krups2" ) ] = "Krups_2"

names(project)[ which( names(project) == "Philips3" ) ] = "Philips_3"
names(project)[ which( names(project) == "Krups3" ) ] = "Krups_3"

#####################
### Tidy data set ###
#####################
project = project[ , c("Respondent", "experiment", "Choice_Set", "Choice", 
                       "Philips_1", "Philips_2", "Philips_3",
                       "Krups_1", "Krups_2", "Krups_3",
                       "Capacity5_1", "Capacity5_2", "Capacity5_3",
                       "Capacity10_1", "Capacity10_2", "Capacity10_3",
                       "Price59_1", "Price59_2", "Price59_3",
                       "Price79_1", "Price79_2", "Price79_3",
                       "Filter_1", "Filter_2", "Filter_3",
                       "Grinder_1", "Grinder_2", "Grinder_3") ]

##############################################
### Names for the product feature variable ###
##############################################
vnames <- c( "Philips", "Krups",
             "Capacity5", "Capacity10",
             "Price59", "Price79",
             "Filter",
             "Grinder")

###################################################
### Transform data into a long form data format ###
###################################################
project_long = reshape( project,
                        idvar = c("Respondent", "experiment", "Choice_Set"),
                        times = c(1, 2, 3), #3 alternatives
                        timevar = "alternative", 
                        varying = matrix( colnames(project)[ 5:ncol(project) ], nrow = length(vnames), byrow = TRUE), #8 means number of features
                        v.names = vnames,
                        direction = "long" )

#Here, -1 esentially represents 0 
project_long[ (project_long == -1 ) ] = 0 

#To make a variable indicating whether alternative is chosen (True or False)
project_long$Choice = with(project_long, Choice == alternative)

project_long = arrange(project_long, Respondent, experiment) #Arrange data to look more intuitively understandable

