##CLIM 759##
##Group Project##
##Recreating Bell et al paper with updated data##
#Written by Shreya Guha

library(rstatix)
library(maditr)
library(corrplot)
library(Hmisc)
library(dplyr)
library(tidyverse)

#set working directory same as the files
setwd("C:/Users/15712/Desktop/CLIM_759")

#read file
data <- read.csv("aqs.data.monthly.1999.2020.csv")
#data compiled together by Rasel


###--------temporal analysis----------###


###---yearly----###

#create yearwise summary
year.summary <- aggregate(data[10], list(data$chemicals, data$year), mean)

#renaming columns
names(year.summary)[1] <- 'chemicals'
names(year.summary)[2] <- 'year'

#subseting to remove year 1999
year.data <- subset(year.summary, year != 1999)

#long to wide data
data_wide_year <- dcast(year.data, year ~ chemicals, value.var="mean")
cor_data_year <- subset(data_wide_year, select = -c(year))

#correlation matrix
mydata.rcorr <- rcorr(as.matrix(cor_data_year))
mydata.coeff <- mydata.rcorr$r
mydata.p <- mydata.rcorr$P #shows associated p-values

#display everything together
#define function
flat_cor_mat <- function(cor_r, cor_p){
  #This function provides a simple formatting of a correlation matrix
  #into a table with 4 columns containing :
  # Column 1 : row names (variable 1 for the correlation test)
  # Column 2 : column names (variable 2 for the correlation test)
  # Column 3 : the correlation coefficients
  # Column 4 : the p-values of the correlations
  library(tidyr)
  library(tibble)
  cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- gather(cor_r, column, cor, -1)
  cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
  cor_p <- gather(cor_p, column, p, -1)
  cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
  cor_p_matrix
}
#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff, mydata.p)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p <= 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
plus <- as.character("+")
minus <- as.character("_")
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- c("x")
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "yearly_cor_final.csv", row.names = F) 

#visualization
#corrplot(cor_data, method = "color")


##-------------seasonal summary----------##

#subseting to remove year 1999
data <- subset(data, year != 1999)

#name seasons based on months
data$month[data$month >= 3 & data$month <= 5 ] <- c("spring")
data$month[data$month >= 6 & data$month <= 8 ] <- c("summer")
data$month[data$month >= 9 & data$month <= 11 ] <- c("fall")
data$month[data$month == 1 & data$month == 2 & data$month == 12 ] <- c("winter")

#calculate seasonal summary
season.summary <- aggregate(data[10], list(data$chemicals, data$year, data$month), mean)

#renaming columns
names(season.summary)[1] <- 'chemicals'
names(season.summary)[2] <- 'year'
names(season.summary)[3] <- 'season'

#long to wide data
data_wide_season <- dcast(season.summary, year + season ~ chemicals, value.var="mean")




##---winter----##

data_wide_winter <- subset(data_wide_season, data_wide_season$season == "winter")
cor_data_winter <- subset(data_wide_winter, select = -c(year, season))

#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_winter))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p <= 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "winter_cor_final.csv", row.names = F) 




##-----spring------##

data_wide_spring <- subset(data_wide_season, data_wide_season$season == "spring")
cor_data_spring <- subset(data_wide_spring, select = -c(year, season))

#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_spring))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p < 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "spring_cor_final.csv", row.names = F) 



##----summer-----##

data_wide_summer <- subset(data_wide_season, data_wide_season$season == "summer")
cor_data_summer <- subset(data_wide_summer, select = -c(year, season))


#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_summer))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p < 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "summer_cor_final.csv", row.names = F) 




##---------fall-------##

data_wide_fall <- subset(data_wide_season, data_wide_season$season == "fall")
cor_data_fall <- subset(data_wide_fall, select = -c(year, season))


#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_fall))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p < 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "fall_cor_final.csv", row.names = F) 



###--------------------spatial analysis-------------------###


#divide the states into eastern and western US           
data$State.Name[data$State.Name == "Arizona"] <- "western"
data$State.Name[data$State.Name == "California"] <- "western"
data$State.Name[data$State.Name == "Colorado"] <- "western"
data$State.Name[data$State.Name == "Idaho"] <- "western"
data$State.Name[data$State.Name == "Montana"] <- "western"
data$State.Name[data$State.Name == "Nevada"] <- "western"
data$State.Name[data$State.Name == "New Mexico"] <- "western"
data$State.Name[data$State.Name == "Oregon"] <- "western"
data$State.Name[data$State.Name == "Utah"] <- "western"
data$State.Name[data$State.Name == "Washington"] <- "western"
data$State.Name[data$State.Name == "Wyoming"] <- "western"
data$State.Name[data$State.Name != "western"] <- "eastern"

#create yearwise summary
space.summary <- aggregate(data[10], 
                           list(data$chemicals, data$year, data$month, data$State.Name), 
                           mean)

#renaming columns
names(space.summary)[1] <- 'chemicals'
names(space.summary)[2] <- 'year'
names(space.summary)[3] <- 'season'
names(space.summary)[4] <- 'region'

#subseting to remove year 1999
space.data <- subset(space.summary, year != 1999)

#create eastern & western data sets 
eastern.data <- subset(space.data, region == "eastern")
eastern.data <- subset(eastern.data, select = -c(region))
western.data <- subset(space.data, region == "western")
western.data <- subset(western.data, select = -c(region))

#repeat similar temporal analysis as above
##----Similar Temporal Analysis----##

##--Yearly calculations--##

##Western##
#create yearwise summary
w.year.summary <- aggregate(western.data[4], 
                            list(western.data$chemicals, western.data$year), mean)

#renaming columns
names(w.year.summary)[1] <- 'chemicals'
names(w.year.summary)[2] <- 'year'

#long to wide data
data_wide_year <- dcast(w.year.summary, year ~ chemicals, value.var="mean")
cor_data_year <- subset(data_wide_year, select = -c(year))

#correlation matrix
mydata.rcorr <- rcorr(as.matrix(cor_data_year))
mydata.coeff <- mydata.rcorr$r
mydata.p <- mydata.rcorr$P #shows associated p-values

#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff, mydata.p)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p < 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x" 
cor_25$cor[cor_25$cor == "x"] <- minus

#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "yearly_cor_western_final.csv", row.names = F) 



##--Eastern--##

#create yearwise summary
e.year.summary <- aggregate(eastern.data[4], 
                            list(eastern.data$chemicals, eastern.data$year), mean)

#renaming columns
names(e.year.summary)[1] <- 'chemicals'
names(e.year.summary)[2] <- 'year'

#long to wide data
data_wide_year <- dcast(e.year.summary, year ~ chemicals, value.var="mean")
cor_data_year <- subset(data_wide_year, select = -c(year))

#correlation matrix
mydata.rcorr <- rcorr(as.matrix(cor_data_year))
mydata.coeff <- mydata.rcorr$r
mydata.p <- mydata.rcorr$P #shows associated p-values

#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff, mydata.p)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p <= 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x" 
cor_25$cor[cor_25$cor == "x"] <- minus

#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "yearly_cor_eastern_final.csv", row.names = F) 


##---------Seasonal Analysis-----------------##

##----Western-----##

#calculate seasonal summary
w.season.summary <- aggregate(western.data[4], 
                              list(western.data$chemicals, western.data$year, western.data$season), 
                              mean)

#renaming columns
names(w.season.summary)[1] <- 'chemicals'
names(w.season.summary)[2] <- 'year'
names(w.season.summary)[3] <- 'season'

#long to wide data
data_wide_season <- dcast(w.season.summary, year + season ~ chemicals, value.var="mean")



##---winter----##

data_wide_winter <- subset(data_wide_season, data_wide_season$season == "winter")
cor_data_winter <- subset(data_wide_winter, select = -c(year, season))

#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_winter))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p <= 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus

#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "winter_cor_western_final.csv", row.names = F) 


##-----spring------##

data_wide_spring <- subset(data_wide_season, data_wide_season$season == "spring")
cor_data_spring <- subset(data_wide_spring, select = -c(year, season))

#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_spring))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p <= 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "spring_cor_western_final.csv", row.names = F) 


##----summer-----##

data_wide_summer <- subset(data_wide_season, data_wide_season$season == "summer")
cor_data_summer <- subset(data_wide_summer, select = -c(year, season))


#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_summer))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p < 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "summer_cor_western_final.csv", row.names = F) 




##---------fall-------##

data_wide_fall <- subset(data_wide_season, data_wide_season$season == "fall")
cor_data_fall <- subset(data_wide_fall, select = -c(year, season))


#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_fall))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p < 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "fall_cor_western_final.csv", row.names = F) 


##-----Eastern------##

#calculate seasonal summary
e.season.summary <- aggregate(eastern.data[4], 
                              list(eastern.data$chemicals, eastern.data$year, eastern.data$season), 
                              mean)

#renaming columns
names(e.season.summary)[1] <- 'chemicals'
names(e.season.summary)[2] <- 'year'
names(e.season.summary)[3] <- 'season'

#long to wide data
data_wide_season <- dcast(e.season.summary, year + season ~ chemicals, value.var="mean")



##---winter----##

data_wide_winter <- subset(data_wide_season, data_wide_season$season == "winter")
cor_data_winter <- subset(data_wide_winter, select = -c(year, season))

#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_winter))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p <= 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus

#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "winter_cor_eastern_final.csv", row.names = F) 


##-----spring------##

data_wide_spring <- subset(data_wide_season, data_wide_season$season == "spring")
cor_data_spring <- subset(data_wide_spring, select = -c(year, season))

#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_spring))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p <= 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "spring_cor_eastern_final.csv", row.names = F) 


##----summer-----##

data_wide_summer <- subset(data_wide_season, data_wide_season$season == "summer")
cor_data_summer <- subset(data_wide_summer, select = -c(year, season))


#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_summer))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p < 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "summer_cor_eastern_final.csv", row.names = F) 




##---------fall-------##

data_wide_fall <- subset(data_wide_season, data_wide_season$season == "fall")
cor_data_fall <- subset(data_wide_fall, select = -c(year, season))


#correlation matrix
mydata.rcorr.2 <- rcorr(as.matrix(cor_data_fall))
mydata.coeff.2 <- mydata.rcorr.2$r
mydata.p.2 <- mydata.rcorr.2$P #shows associated p-values


#create correlation matrix with p-values
my_cor_matrix <- flat_cor_mat(mydata.coeff.2, mydata.p.2)

#filter out insignificant p-values, p > 0.05
cor_matrix_imp <- subset(my_cor_matrix, p < 0.05)

#keep just the correlation coefficient values
correlation_data <- subset(cor_matrix_imp, select = - p)

#keeping r values above 0.25 only
cor_25 <- subset(correlation_data, abs(cor) > 0.25)

#convert rest of the values according to sign conventions
cor_25$cor[cor_25$cor >= 0.25 & cor_25$cor <= 0.50 ] <- plus
cor_25$cor[cor_25$cor %between% c(-0.25,-0.50) ] <- "x"
cor_25$cor[cor_25$cor == "x"] <- minus


#data manipulation
data_wide_cor_25 <- dcast(cor_25, row ~ column, value.var="cor") #long to wide data
cor_data_25 <- as.data.frame(data_wide_cor_25) #convert to data frame

#replace na's generated due to filtering (less p-value and less r-value) to blanks
cor_data_25[is.na(cor_data_25)] = c(" ") 

#save all data
fwrite(cor_data_25, file = "fall_cor_eastern_final.csv", row.names = F) 

###----Truncating saved results for paper------###

#Finding the most important constituent
mass.summary <- aggregate(data[10], list(data$chemicals), mean)

#renaming columns
names(mass.summary)[1] <- 'chemicals'

#find ratio
mass.summary$ratio <- (mass.summary$mean/sum(mass.summary$mean))*100

#save data
fwrite(mass.summary, file = "mass_ratio.csv", row.names = F) 


###Truncating saved data###

##---Whole US---##

##For all constituents##

year_us <- read.csv("yearly_cor_final.csv")
year_us_a <- year_us[c(35, 46, 34, 33, 2, 47, 17), 
                     c(1, 36, 47, 35, 34, 3, 48, 18, 43, 45, 25, 13, 38, 10, 2, 14)]
fwrite(year_us_a, file = "year_us.csv")

#similarly, truncate data for all saved seasonal data files for whole US
year_us <- read.csv("winter_cor_final.csv")
year_us_a <- year_us[c(35, 46, 34, 33, 2, 47, 17), 
                     c(1, 36, 47, 35, 34, 3, 48, 18, 43, 45, 25, 13, 38, 10, 2, 14)]
fwrite(year_us_a, file = "winter_us.csv")

year_us <- read.csv("spring_cor_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 2, 46, 16), 
                     c(1, 35, 46, 34, 33, 3, 47, 17, 42, 44, 24, 12, 37, 9, 2, 13)]
fwrite(year_us_a, file = "spring_us.csv")

year_us <- read.csv("summer_cor_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 2, 46, 16), 
                     c(1, 35, 46, 34, 33, 3, 47, 17, 42, 44, 24, 12, 37, 9, 2, 13)]
fwrite(year_us_a, file = "summer_us.csv")

year_us <- read.csv("fall_cor_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 2, 46, 16), 
                     c(1, 35, 46, 34, 33, 3, 47, 17, 42, 44, 24, 13, 37, 10, 2, 14)]
fwrite(year_us_a, file = "fall_us.csv")


##For PM mass only##
year_us <- read.csv("yearly_cor_final.csv")
year_us_a <- year_us[c(35, 46, 34, 33, 2, 47, 17), 
                     c(1, 36)]
fwrite(year_us_a, file = "yearly_us_PM.csv")

#similarly, truncate data for all saved seasonal data files for whole US
year_us <- read.csv("winter_cor_final.csv")
year_us_a <- year_us[c(35, 46, 34, 33, 2, 47, 17), 
                     c(1, 36)]
fwrite(year_us_a, file = "winter_us_PM.csv")

year_us <- read.csv("spring_cor_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 1, 46, 16), 
                     c(1, 35)]
fwrite(year_us_a, file = "spring_us_PM.csv")

year_us <- read.csv("summer_cor_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 1, 46, 16), 
                     c(1, 35)]
fwrite(year_us_a, file = "summer_us_PM.csv")

year_us <- read.csv("fall_cor_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 1, 46, 16), 
                     c(1, 35)]
fwrite(year_us_a, file = "fall_us_PM.csv")


##---Regional US---##
##For PM

year_us <- read.csv("yearly_cor_western_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 2, 46, 16), 
                     c(1, 35)]
fwrite(year_us_a, file = "year_west_PM.csv")

#Repeat for other seasons
year_us <- read.csv("winter_cor_western_final.csv")
year_us_a <- year_us[c(33, 44, 32, 31, 2, 45, 15), 
                     c(1, 34)]
fwrite(year_us_a, file = "winter_west_PM.csv")

year_us <- read.csv("spring_cor_western_final.csv")
year_us_a <- year_us[c(34, 44, 33, 32, 2, 45, 16), 
                     c(1, 35)]
fwrite(year_us_a, file = "spring_west_PM.csv")

year_us <- read.csv("summer_cor_western_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 2, 46, 16), 
                     c(1, 35)]
fwrite(year_us_a, file = "summer_west_PM.csv")

year_us <- read.csv("fall_cor_western_final.csv")
year_us_a <- year_us[c(33, 44, 32, 31, 2, 45, 15), 
                     c(1, 34)]
fwrite(year_us_a, file = "fall_west_PM.csv")

##Repeat for eastern US
year_us <- read.csv("yearly_cor_eastern_final.csv")
year_us_a <- year_us[c(35, 46, 34, 33, 2, 47, 17), 
                     c(1, 36)]
fwrite(year_us_a, file = "year_east_PM.csv")

year_us <- read.csv("winter_cor_eastern_final.csv")
year_us_a <- year_us[c(35, 46, 34, 33, 2, 47, 17), 
                     c(1, 36)]
fwrite(year_us_a, file = "winter_east_PM.csv")

year_us <- read.csv("spring_cor_eastern_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 2, 45, 16), 
                     c(1, 35)]
fwrite(year_us_a, file = "spring_east_PM.csv")

year_us <- read.csv("summer_cor_eastern_final.csv")
year_us_a <- year_us[c(34, 45, 33, 32, 2, 46, 16), 
                     c(1, 35)]
fwrite(year_us_a, file = "summer_east_PM.csv")

year_us <- read.csv("fall_cor_eastern_final.csv")
year_us_a <- year_us[c(35, 46, 34, 33, 2, 47, 17), 
                     c(1, 36)]
fwrite(year_us_a, file = "fall_east_PM.csv")

