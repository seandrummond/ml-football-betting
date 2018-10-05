####################
# Pre-modelling Data Prep and Modelling
#
# Description:
# This file reads in the data prepared in 'data_prep_filename' and segregates the data by
# Bet type and by League. This creates a total of 13 (bets) x 9 (leagues) different datasets
# stored in one data structure.
#
# Recursive Feature Elimination with a Random Forest Classifier is used to select the optimum
# Features for each dataset.
####################

###### Import Libraries #####
library(dplyr)
library(randomForest)
library(caret)
library(readr)

##### Define Functions ######
main_football_file_reader <- function(x, path){
		y <- sub("data", "", x)
		z <- read_csv(paste(path, "/", y, ".csv", sep = ""), col_types = 
		  cols(
			FTHG = col_double(),
			FTHG = col_double(),
			HTHG = col_double(),
			HTAG = col_double(),
			HS = col_double(),
			AS = col_double(),
			HST = col_double(),
			AST = col_double(),
			HC = col_double(),
			AC = col_double(),
			HF = col_double(),
			AF = col_double(),
			HY= col_double(), 
			AY= col_double(), 
			HR= col_double(), 
			AR= col_double()
			)
		)
		return(z)
	}
	
football_file_reader <- function(x, path){
	#Function which reads in raw data
	#Inputs:
	#x = raw data
	#path = data path
	#Outputs:
	#x = DataFrame
	
	y <- sub("data", "", x)
	assign(x, read_csv(paste(path, "/", y, ".csv", sep = "")), envir = .GlobalEnv)	
}

bet_split <- function(full_data){
	#Function to split data set into different bets.
	#Abbreviations:
	#OU_X = Over/Under X Goals market
	#BTS = Both Teams to Score
	#FH/SH/FT = First Half/Second Half/Full Time
	#FTR/HTR = Full Time Result/Half Time Result
	#Inputs:
	#full_data = DataFrame
	#Outputs:
	#data_list = List of DataFrames
	
	
	#Derived Under/Over Columns
	full_data$OU_05 <- ifelse(full_data$FTHG + full_data$FTAG > 0.5, 1, 0)	
	full_data$OU_15 <- ifelse(full_data$FTHG + full_data$FTAG > 1.5, 1, 0)
	full_data$OU_25 <- ifelse(full_data$FTHG + full_data$FTAG > 2.5, 1, 0)
	full_data$OU_35 <- ifelse(full_data$FTHG + full_data$FTAG > 3.5, 1, 0)
	full_data$OU_45 <- ifelse(full_data$FTHG + full_data$FTAG > 4.5, 1, 0)
	full_data$OU_55 <- ifelse(full_data$FTHG + full_data$FTAG > 5.5, 1, 0)
	#BTS Columns
	full_data$BTS_FT <- ifelse(full_data$FTHG & full_data$FTAG > 0, 1, 0)
	full_data$BTS_FH <- ifelse(full_data$HTHG & full_data$HTAG > 0, 1, 0)
	full_data$BTS_SH <- ifelse((full_data$FTHG - full_data$HTHG) & (full_data$FTAG - full_data$HTAG) > 0, 1, 0)

	
	#Included Results
	FTR_data <- full_data[,c(7, 25:117, 120:129)]
	HTR_data <- full_data[,c(10, 25:117,120:129)]
	OU_05_data <- full_data[c(130, 25:117,120:129)]
	OU_15_data <- full_data[c(131, 25:117,120:129)]
	OU_25_data <- full_data[c(132, 25:117,120:129)]
	OU_35_data <- full_data[c(133, 25:117,120:129)]
	OU_45_data <- full_data[c(134, 25:117,120:129)]
	OU_55_data <- full_data[c(135, 25:117,120:129)]
	BTS_FT_data <- full_data[c(136, 25:117,120:129)]
	BTS_FH_data <- full_data[c(137, 25:117,120:129)]
	BTS_SH_data <- full_data[c(138, 25:117,120:129)]
	
	list_names <- c("FTR", "HTR", "OU_05", "OU_15", "OU_25", "OU_35", "OU_45", "OU_55", "BTS_FT", "BTS_FH", "BTS_SH")
	data_list = list(FTR_data, HTR_data, OU_05_data, OU_15_data, OU_25_data, OU_35_data, OU_45_data, OU_55_data, BTS_FT_data, BTS_FH_data, BTS_SH_data)
	names(data_list) <- list_names
	
	#Factorisation
	for (i in 1:length(data_list)){
		data_list[[i]][,1] <- as.factor(pull(data_list[[i]][,1]))
		data_list[[i]] <- data.frame(data_list[[i]])
	}
	
	return(data_list)
}	

division_split <- function(data_set){
	#Function which divides DataFrame into a list of DataFrames by League
	#Order: E0, E1, E2, E3, SP1, D1, I1, SC0, FRA
	#Inputs:
	#data_set = DataFrame
	#Outputs:
	#league_data_list = List of DataFrames

	#Subset
	E0_data <- subset(data_set, E0 == 1)
	E1_data <- subset(data_set, E1 == 1)
	E2_data <- subset(data_set, E2 == 1)
	E3_data <- subset(data_set, E3 == 1)
	SP1_data <- subset(data_set, SP1 == 1)
	D1_data <- subset(data_set, D1 == 1)
	I1_data <- subset(data_set, I1 == 1)
	SC0_data <- subset(data_set, SC0 == 1)
	FRA_data <- subset(data_set, FRA == 1)
	#Select Rows
	E0_data <- E0_data[,c(1:95)]
	E1_data <- E1_data[,c(1:95)]
	E2_data <- E2_data[,c(1:95)]
	E3_data <- E3_data[,c(1:95)]
	SP1_data <- SP1_data[,c(1:95)]
	D1_data <- D1_data[,c(1:95)]
	I1_data <- I1_data[,c(1:95)]
	SC0_data <- SC0_data[,c(1:95)]
	FRA_data <- FRA_data[,c(1:95)]
	
	league_data_list = list(E0_data, E1_data, E2_data, E3_data, SP1_data, D1_data, I1_data, SC0_data, FRA_data)
	league_names <- c("E0", "E1", "E2", "E3", "SP1", "D1", "I1", "SC0", "FRA")
	names(league_data_list) <- league_names
	
	return(league_data_list)
}

gen_str <- function(x,y){
	#Function which generates data structure to hold information for each league within each bet_split
	#Inputs:
	#x = Number of Bets
	#y = Number of Leagues
	#Outputs:
	#z = Data Structure
	k <- list()
	for (i in 1:y){
		k[[length(k)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	z <- list()
	for (i in 1:x){
		z[[length(z)+1]] <- assign(paste("y",i,sep=""),k)
	}
	return(z)
}

##### Import Data ######

#Set Working Directory
setwd("C:/Users/Sean Drummond/Data/Clean")

#League Names
test <- c("EL1","EL2","EL3_n","EL4","ESP1","GER1","ITA1","SCT1","FRA1")

#Folder Code
folder_code <- "Main_Data"

#Import Data
data_list <- test %>% lapply(main_football_file_reader, folder_code)

### Rowbind Data and Introduce League Code ###
data_set <- data_list[[1]]
for (i in 2:length(data_list)){
	data_set <- rbind(data_set, data_list[[i]])
}

n_t <- names(data_list[[1]])
for (i in 2:length(data_list)){
	n_t <- append(n_t, names(data_list[[i]]))
}

data_set$E0 <- ifelse(data_set$Div == "E0", 1,0)
data_set$E1 <- ifelse(data_set$Div == "E1", 1,0)
data_set$E2 <- ifelse(data_set$Div == "E2", 1,0)
data_set$E3 <- ifelse(data_set$Div == "E3", 1,0)
data_set$SP1 <- ifelse(data_set$Div == "SP1", 1,0)
data_set$D1 <- ifelse(data_set$Div == "D1", 1,0)
data_set$I1 <- ifelse(data_set$Div == "I1", 1,0)
data_set$SC0 <- ifelse(data_set$Div == "SC0", 1,0)
data_set$FRA <- ifelse(data_set$Div == "F1", 1, 0)

### Generate Labels for each Bet and Separate Datasets ###
#Bet Split
bs_data_set <- bet_split(data_set)
#Division Split
for (i in 1:length(bs_data_set)){
	bs_data_set[[i]] <- division_split(bs_data_set[[i]])
}

##### Modelling ######

#Define Feature Selection Method: Recursive Feature Selection with Cross Validation
control <- rfeControl(functions=rfFuncs, method="cv", number = 2)

#Generate Structures

#Results from RFE Structure
results <- gen_str(length(bs_data_set), length(bs_data_set[[1]]))
#Dataset with Features Selected
correct_data_set_list <- gen_str(length(bs_data_set), length(bs_data_set[[1]]))
#Optimum Features
chosen_vars_list <- gen_str(length(bs_data_set), length(bs_data_set[[1]]))
#Optimum Models
models_list <- gen_str(length(bs_data_set), length(bs_data_set[[1]]))


for (i in 1:length(bs_data_set)){
	for (j in 1:length(bs_data_set[[1]])){
		#Feature Select
		results[[i]][[j]] <- rfe(bs_data_set[[i]][[j]][,-1], bs_data_set[[i]][[j]][,1], sizes = c(1:94), rfeControl = control)
		correct_data_set_list[[i]][[j]] <- full_data[[i]][[j]][,1]
		correct_data_set_list[[i]][[j]] <- cbind(correct_data_set_list[[i]][[j]],full_data[[i]][[j]][,predictors(results[[i]][[j]])])
		colnames(correct_data_set_list[[i]][[j]])[1] <- names(full_data[[i]][[j]][1])
		chosen_vars_list[[i]][[j]] <- predictors(results[[i]][[j]])
		
		#Build Model
		models_list[[i]][[j]] <- train(correct_data_set_list[[i]][[j]][,-1], correct_data_set_list[[i]][[j]][,1], method = "rf")
		
		#Save Models and Chosen Variables
		folder_name <- names(full_data)[i]
		league_name <- names(full_data[[i]])[j]
		models_folder <- c("Latest_Models")
		if (j == 1){
			dir.create(file.path(models_folder, folder_name))
		}
		saveRDS(models_list[[i]][[j]], paste("Latest_Models\\",folder_name,"\\",league_name,"_model.rda",sep= ""))
		write.csv(chosen_vars_list[[i]][[j]], file = paste("Latest_Models\\",folder_name,"\\",league_name,"_vars.csv",sep=""), row.names = FALSE)
	}
}