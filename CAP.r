####################
# CAP LUT Generation
#
# Description:
# This file takes in the prepared data and built models and calculates the accuracy of each
# model when they model is above a certain level of confidence.
#
####################

###### Import Libraries #####
library(randomForest)
library(caret)
library(readr)
library("e1071")

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
	# x = nrow(index)
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

three_way_split <- function(x){
	#3 Way Cross Validation Split
	
	n_rows <- nrow(x)
	mixed <- sample(n_rows)
	third_1 <- mixed[1:round(n_rows/3)]
	third_2 <- mixed[(round(n_rows/3)+1):round(2*n_rows/3)]
	third_3 <- mixed[(round(2*n_rows/3)+1):n_rows]
	
	train_set_1 <- x[c(third_1, third_2),]
	test_set_1 <- x[third_3,]
	train_set_2 <- x[c(third_1, third_3),]
	test_set_2 <- x[third_2,]
	train_set_3 <- x[c(third_2, third_3),]
	test_set_3 <- x[third_1,]

	list_1 <- list(train_set_1, test_set_1)
	list_2 <- list(train_set_2, test_set_2)
	list_3 <- list(train_set_3, test_set_3)
	
	z <- list(list_1, list_2, list_3)
	
	return (z)
}

#Create Result Architecture
gen_str_results <- function(x,y,z){
	# x = nrow(index)
	m <- list()
	for (i in 1:z){
		m[[length(m)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	k <- list()
	for (i in 1:y){
		k[[length(k)+1]] <- assign(paste("x",i, sep = ""), m)
	}
	z <- list()
	for (i in 1:x){
		z[[length(z)+1]] <- assign(paste("y",i,sep=""),k)
	}
	return(z)
}

#Create Result Architecture
gen_str_results_2 <- function(x,y,z){
	# x = nrow(index)
	#Generate Lists with 3 Dataframes corresponding to 3 Way Cross Validation
	m <- list()
	for (i in 1:z){
		m[[length(m)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	#Generate 3 of m in a list for 3 Option Bets
	n <- list()
	for (i in 1:3){
		n[[length(n)+1]] <- assign(paste("x",i, sep = ""), m)
	}
	#Generate 2 of m in a list for 2 Option Bets
	w <- list()
	for (i in 1:2){
		w[[length(w)+1]] <- assign(paste("x",i, sep = ""), m)
	}
	#Generate 20 of m in a list for 20 Option Bets
	u <- list()
	for (i in 1:20){
		u[[length(u)+1]] <- assign(paste("x",i, sep = ""), m)
	}
	#List Together Results Bets
	r <- list(n,n,w,w,w,w,w,w,w,w,w,u,u)
	#Generate Leagues
	a <- list()
	for (i in 1:y){
		a[[length(a)+1]] <- assign(paste("y",i,sep=""),r)
	}
	#Generate Bets
	z <- list()
	for (i in 1:x){
		z[[length(z)+1]] <- assign(paste("y",i,sep=""),a)
	}
	return(z)
}

gen_str_results_3 <- function(x,y,z){
	# x = nrow(index)
	#Generate 3 dataframes in a list for 3 Option Bets
	n <- list()
	for (i in 1:3){
		n[[length(n)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	#Generate 2 dataframes in a list for 2 Option Bets
	w <- list()
	for (i in 1:2){
		w[[length(w)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	#Generate 20 of m in a list for 20 Option Bets
	u <- list()
	for (i in 1:20){
		u[[length(u)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	#Generate Lists with 3 of each type corresponding to 3 Way Cross Validation
	m <- list()
	for (i in 1:z){
		m[[length(m)+1]] <- assign(paste("x",i, sep = ""), n)
	}	
	#Generate Lists with 3 of each type corresponding to 3 Way Cross Validation
	b <- list()
	for (i in 1:z){
		b[[length(b)+1]] <- assign(paste("x",i, sep = ""), w)
	}
	#Generate Lists with 3 of each type corresponding to 3 Way Cross Validation
	d <- list()
	for (i in 1:z){
		d[[length(d)+1]] <- assign(paste("x",i, sep = ""), u)
	}
	#Generate 9 of each Type corresponding to each league
	e <- list()
	for (i in 1:y){
		e[[length(e)+1]] <- assign(paste("x",i, sep = ""), m)
	}	
	f <- list()
	for (i in 1:y){
		f[[length(f)+1]] <- assign(paste("x",i, sep = ""), b)
	}
	g <- list()
	for (i in 1:y){
		g[[length(g)+1]] <- assign(paste("x",i, sep = ""), d)
	}
	
	r <- list(e,e,f,f,f,f,f,f,f,f,f,g,g)

	return(r)
}

gen_str_results_4 <- function(x,y,z){
	# x = nrow(index)
	#Generate 3 dataframes in a list for 3 Option Bets
	n <- list()
	for (i in 1:3){
		n[[length(n)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	#Generate 2 dataframes in a list for 2 Option Bets
	w <- list()
	for (i in 1:2){
		w[[length(w)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	#Generate 20 of m in a list for 20 Option Bets
	u <- list()
	for (i in 1:20){
		u[[length(u)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	#Generate 9 of each Type corresponding to each league
	e <- list()
	for (i in 1:y){
		e[[length(e)+1]] <- assign(paste("x",i, sep = ""), n)
	}	
	f <- list()
	for (i in 1:y){
		f[[length(f)+1]] <- assign(paste("x",i, sep = ""), w)
	}
	g <- list()
	for (i in 1:y){
		g[[length(g)+1]] <- assign(paste("x",i, sep = ""), u)
	}
	
	r <- list(e,e,f,f,f,f,f,f,f,f,f,g,g)

	return(r)
}

##### Definitions #####

#Confidence Intervals
three_results_conf <- c(0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9)
two_results_conf <- c(0.533, 0.566, 0.6, 0.633, 0.666, 0.7, 0.733, 0.766, 0.8, 0.833, 0.866, 0.9)
league_names <- c("E0", "E1", "E2", "E3", "SP1", "D1", "I1", "SC0", "FRA")
bet_names <- c("FTR", "HTR", "OU_05", "OU_15", "OU_25", "OU_35", "OU_45", "OU_55", "BTS_FT", "BTS_FH", "BTS_SH")
confidence_intervals <- data.frame(three_results_conf,three_results_conf,two_results_conf,two_results_conf,two_results_conf,two_results_conf,two_results_conf,two_results_conf,two_results_conf,two_results_conf,two_results_conf)

#Results List
result_results <- c("A", "D", "H")
BTS_OU_result <- c(0,1)
result_outcomes <- list(result_results, result_results, BTS_OU_result, BTS_OU_result, BTS_OU_result, BTS_OU_result, BTS_OU_result, BTS_OU_result, BTS_OU_result,BTS_OU_result,BTS_OU_result)

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


### Import Optimum Feature Vectors ###
n_bets <- 11
n_leagues <- 9

chosen_vars <- gen_str(n_bets, n_leagues)
models <- gen_str(n_bets, n_leagues)

for (i in 1:n_bets){
	folder_name <- names(bs_data_set)[i]
	for (j in 1:n_leagues){
		league_name <- names(bs_data_set[[i]])[j]
		chosen_vars[[i]][[j]] <- read.csv(paste("Latest_Models\\",folder_name,"\\",league_name,"_vars.csv",sep=""))
	}
}


### Subset Data ###
fs_data_set <- gen_str(n_bets, n_leagues)

#Subset Dataset
for (i in 1:n_bets){
	for (j in 1:n_leagues){
		fs_data_set[[i]][[j]] <- bs_data_set[[i]][[j]][,1]
		fs_data_set[[i]][[j]] <- cbind(fs_data_set[[i]][[j]],bs_data_set[[i]][[j]][,as.character(pull(chosen_vars[[i]][[j]]))])
		colnames(fs_data_set[[i]][[j]])[1] <- names(bs_data_set[[i]][[j]][1])
	}
}

#Set seed
set.seed(10)


##### CAP Generation #####

#Split each Dataset into 3 Train and Test Sets
for (i in 1:n_bets){
	for (j in 1:n_leagues){
		fs_data_set[[i]][[j]] <- three_way_split(fs_data_set[[i]][[j]])		
	}
}

#Generate Results Structures
n_cross <- 3
CAP_results <- gen_str_results_3(n_bets, n_leagues, n_cross)
CAP_mean_results <- gen_str_results_4(n_bets, n_leagues, n_cross)

#Predictions
#Each Type of Bet
#n_bets
for (i in 1:n_bets){
	#Each League
	#n_leagues
	folder_name <- bet_names[i]
	for (j in 1:2){
		league_name <- league_names[j]
		#Each Train/Test Set
		for (r in 1:n_cross){
		#Build model per set
		model <- train(fs_data_set[[i]][[j]][[r]][[1]][,-1], fs_data_set[[i]][[j]][[r]][[1]][,1], method = "rf")
		#Make predictions on set
		prob_prediction <- predict(model, fs_data_set[[i]][[j]][[r]][[2]], type = "prob")
		class_prediction <- predict(model, fs_data_set[[i]][[j]][[r]][[2]], type = "raw")
			#For each result within the type of Bet
			for (k in 1:length(result_outcomes[[i]])){
				#For each confidence interval
				n_conf <- c()
				model_accuracy <- c()
				for (m in 1:nrow(confidence_intervals)){
					#Index of confidence resets
					conf_index <- c()
					#For each row in prob_prediction data frame
					for (n in 1:nrow(prob_prediction)){
						#For each column in prob_prediction data frame
						for (p in 1:ncol(prob_prediction)){
							#If prob prediction is greater than the confidence interval, record index
							if (prob_prediction[n,p] > confidence_intervals[m,i]){
								conf_index <- append(conf_index, n)
							}
						}
					}
					#Result index where prediction equals type of result
					result_index <- which(class_prediction == result_outcomes[[i]][k])
					#Unique values of conf_index
					conf_index <- unique(conf_index)
					#Intersection of Indexes
					intersect_index <- intersect(result_index,conf_index)
					#Number of values
					n_g = length(unique(intersect_index))
					
					n_conf[m] = n_g * 100/length(fs_data_set[[i]][[j]][[r]][[2]][,1] == result_outcomes[[i]][k])
					model_accuracy[m] <- mean(fs_data_set[[i]][[j]][[r]][[2]][intersect_index,1] == class_prediction[intersect_index])
					if (is.nan(model_accuracy[m])){
						model_accuracy[m] <- 0
					}
				}
				CAP_results[[i]][[j]][[r]][[k]] <- data.frame(confidence_intervals[,i], model_accuracy, n_conf)
			}
		}
		CAP_mean_results[[i]][[j]] <- get_mean(CAP_results[[i]][[j]])
		#Working Average Insertion
		models_folder <- c("Latest_CAP_LUT")
		if (j == 1){
			dir.create(file.path(models_folder, folder_name))
		}
		for (k in 1:length(result_outcomes[[i]])){
			write.csv(CAP_mean_results[[i]][[j]][[k]], file = paste("Latest_CAP_LUT\\",folder_name,"\\",folder_name,"_",league_name,"_",result_outcomes[[i]][k],"_CAP.csv",sep=""), row.names = FALSE)
		}
		
	}
}
