####################
# Data Preparation & Feature Extraction
#
# Description:
# This file reads in data on each match in a certain league and compiles
# two types of files. One is a dataframe which contains form variables of
# each team coming into a certain fixture. The other type is a DataFrame 
# for each team that contains form variables from head-to-head fixtures
# with each other team.
#
####################

##### Import Libraries #####

library(readr)
library(dplyr)

##### Define Functions #####

data_import_link <- function(year, league, base_split){
	#Function which produces correct path to pull data
	#Inputs:
	#year = year code
	#league = league code
	#base_split = base_link split by "/"
	#Outputs:
	#output = correct path
	output = paste(base_split[[1]][1],"//",base_split[[1]][3],"/",base_split[[1]][4],"/",year,"/",paste(league,".csv", sep = ""), sep = "")
	
	return(output)

}

main_football_file_reader <- function(x, path){
	#Function which reads in raw data
	#Inputs:
	#x = raw data
	#path = data path
	#Outputs:
	#x = DataFrame
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

matchweek_creator <- function(data, n_seasons, n_matchweeks, n_games_p_matchweek){
	#Function which creates matchweek variable
	#Inputs:
	#data = DataFrame
	#n_seasons = Number of Seasons
	#n_matchweeks = Number of Matchweeks
	#n_games_p_matchweek = Number of Games per Matchweek
	#Outputs:
	#data = DataFrame with Matchweek Variable Added
	z <- 0
	n_row <- nrow(data)
	for (i in 1:n_seasons){
		y <- 1
		for (j in 1:n_matchweeks){
				for (k in 1:n_games_p_matchweek){
					if ((k + (j-1) * n_games_p_matchweek + (i-1) * n_games_p_matchweek * n_matchweeks) > n_row){
						break
					}
					data$Matchweek[k + (j-1) * n_games_p_matchweek + (i-1) * n_games_p_matchweek * n_matchweeks] <- y
					z <- z + 1
					if (z == n_row){
						break
					}
				}
			y <- y + 1
		}
	}
	return(data)
}
	
league_position <- function(data_sample_2, n_seasons, n_games, n_teams){
	#Function which generates league position feature
	#Inputs:
	#data_sample_2 = data
	#n_seasons = Number of Seasons
	#n_games = Number of games in a season
	#n_teams = Number of teams in a league
	#Outputs:
	#data_sample_2 = data 
	
	for (k in 1:n_seasons){
		Team <- unique(data_sample_2$HomeTeam[(((k - 1) * n_games) + 1):(((k - 1) * n_games) + (n_teams * 2))])
		Points <- integer(length(Team))
		Goals_For <- integer(length(Team))
		Goals_Against <- integer(length(Team))
		Goal_Difference <- integer(length(Team))	
		League_Table <- data.frame(Team, Points, Goals_For, Goals_Against, Goal_Difference)
		
		for (i in 1:n_games){
			for (j in 1:n_teams){
				if (data_sample_2$HomeTeam[i + (k - 1) * n_games] == League_Table$Team[j]) {
				
					data_sample_2$H_Position[i + (k - 1) * n_games] <- j
					
					if (data_sample_2$FTR[i + (k - 1) * n_games] == "H") {
						League_Table$Points[j] <- League_Table$Points[j] + 3
					} else if (data_sample_2$FTR[i + (k - 1) * n_games] == "D"){
						League_Table$Points[j] <- League_Table$Points[j] + 1
					} else if (data_sample_2$FTR[i + (k - 1) * n_games] == "A"){
						League_Table$Points[j] <- League_Table$Points[j] + 0
					}
					League_Table$Goals_For[j] <- League_Table$Goals_For[j] + data_sample_2$FTHG[i + (k - 1) * n_games]
					League_Table$Goals_Against[j] <- League_Table$Goals_Against[j] + data_sample_2$FTAG[i + (k - 1) * n_games]
					League_Table$Goal_Difference[j] <- League_Table$Goals_For[j] - League_Table$Goals_Against[j]
				} else if (data_sample_2$AwayTeam[i + (k - 1) * n_games] == League_Table$Team[j]) {
				
					data_sample_2$A_Position[i + (k - 1) * n_games] <- j
					
					if (data_sample_2$FTR[i + (k - 1) * n_games] == "H") {
						League_Table$Points[j] <- League_Table$Points[j] + 0
					} else if (data_sample_2$FTR[i + (k - 1) * n_games] == "D"){
						League_Table$Points[j] <- League_Table$Points[j] + 1
					} else if (data_sample_2$FTR[i + (k - 1) * n_games] == "A"){
						League_Table$Points[j] <- League_Table$Points[j] + 3
					}
					League_Table$Goals_For[j] <- League_Table$Goals_For[j] + data_sample_2$FTAG[i + (k - 1) * n_games]
					League_Table$Goals_Against[j] <- League_Table$Goals_Against[j] + data_sample_2$FTHG[i + (k - 1) * n_games]
					League_Table$Goal_Difference[j] <- League_Table$Goals_For[j] - League_Table$Goals_Against[j]
				}
			}
			League_Table <- arrange(League_Table, desc(Points), desc(Goal_Difference), desc(Goals_For), Goals_Against, Team)
		}
	}
	return(data_sample_2)
}

form_table_generator <- function(data_sample_3, n_seasons, n_games, n_teams){
	#Function which generates form features for each game based on a weighted ranking system.
	#Inputs:
	#data_sample_3 = DataFrame
	#n_seasons = Number of Seasons
	#n_games = Number of Games
	#n_teams = Number of Teams
	#Outputs:
	#data_sample_3 = DataFrame with Form Variables


	#Variable Names for Form Table
	form_names <-c("FT_Form", "FT_Home_Form", "FT_Away_Form", "HT_Form", "HT_Home_Form",
	"HT_Away_Form", "FT_Att_Form", "FT_Att_Home_Form", "FT_Att_Away_Form", "FT_Mid_Form",
	"FT_Mid_Home_Form", "FT_Mid_Away_Form", "FT_Def_Form", "FT_Def_Home_Form", "FT_Def_Away_Form",
	"HT_Att_Form", "HT_Att_Home_Form", "HT_Att_Away_Form", "HT_Mid_Form",
	"HT_Mid_Home_Form", "HT_Mid_Away_Form", "HT_Def_Form", "HT_Def_Home_Form", "HT_Def_Away_Form",
	"Shot_Form", "Home_Shot_Form", "Away_Shot_Form", "Shot_Target_Form", 
	"Home_Shot_Target_Form", "Away_Shot_Target_Form", "Shot_Target_P_Form",
	"Home_Shot_Target_P_Form", "Away_Shot_Target_P_Form", "Corner_Form",
	"Home_Corner_Form", "Away_Corner_Form", "Foul_Form", "Home_Foul_Form",
	"Away_Foul_Form", "YC_Form", "Home_YC_Form", "Away_YC_Form", "RC_Form",
	"Home_RC_Form", "Away_RC_Form")

	#Vector of all teams in a season
	Team <- unique(data_sample_3$HomeTeam[1:60])

	#Form Table Values Initialized
	values <- matrix((1/length(Team)), nrow = length(Team), ncol = length(form_names))

	#Column Names Updated
	colnames(values) <- form_names

	#Full Form Table Initialized
	Form_Table <- data.frame(Team, values)

	#Form Table Variable order re-arranged to have Form:Home Form:Away Form
	Form_Table <- Form_Table[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,3,6,9,12,15,18,21,24,27,
	30,33,36,39,42,45,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)]

	#Data Variable Creation
	col_names <- names(Form_Table)
	col_names <- col_names[-1]

	#Creating and Re-arranging Variables
	data_form_names <- col_names[1:15]
	home_names <- col_names[16:30]
	away_names <- col_names[31:45]

	#Define function which can be used with lapply and paste characters to front of variable
	paste_word <- function(x, word){
		y <- paste(word, x, sep = "_")
		return(y)
	}

	PH_form_names <- data_form_names %>% lapply(paste_word, "PH") %>% unlist()
	PA_form_names <- data_form_names %>% lapply(paste_word, "PA") %>% unlist()
	home_names <- home_names %>% lapply(paste_word, "PH") %>% unlist()
	away_names <- away_names %>% lapply(paste_word, "PA") %>% unlist()

	total_names <- c(PH_form_names, PA_form_names, home_names, away_names)

	#Initialize Matrix
	values_2 <- matrix(0, nrow = nrow(data_sample_3), ncol = length(total_names))

	#Column Names Updated
	colnames(values_2) <- total_names

	#Data Updated
	data_sample_3 <- data.frame(data_sample_3, values_2)

	#Implement Form Updates


	#Number of Form Variables, non-repeated (Form, Home, Away)
	n_form_variables <- 15
	#Data Home Team Form Index
	d_hf_index <- 27
	#Date Home Team at Home Form Index
	d_hhf_index <- 57
	#Data Away Team Form Index
	d_af_index <- 42
	#Data Away Team at Away Form Index
	d_aaf_index <- 72

	#Define Form Update Function
	form_update <- function(x, form){
		x <- x * form
		return(x)
	}

	for (k in 1:n_seasons){
		#Vector of all teams in a season
		Team <- unique(data_sample_3$HomeTeam[(1 + (k-1)*n_games) :((2 * n_teams) + (k-1)*n_games)])

		#Form Table Values Initialized
		values <- matrix((1/length(Team)), nrow = length(Team), ncol = length(form_names))

		#Column Names Updated
		colnames(values) <- form_names

		#Full Form Table Initialized
		Form_Table <- data.frame(Team, values)

		#Form Table Variable order re-arranged to have Form:Home Form:Away Form
		Form_Table <- Form_Table[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)]
		
		for (i in 1:n_games) {
			for (j in 1:n_teams){
				if (data_sample_3$HomeTeam[i + (k - 1) * n_games] == Form_Table$Team[j]){
					#Form Update
					for (m in 1:n_form_variables){
						data_sample_3[i + (k - 1) * n_games,m + d_hf_index] <- Form_Table[j,m + 1]
					}
					#Home Form Update
					for (m in 1:n_form_variables){
						data_sample_3[i + (k - 1) * n_games,m + d_hhf_index] <- Form_Table[j,m + 1 + n_form_variables]
					}
					#Full Time Results
					if (data_sample_3$FTR[i + (k - 1) * n_games] == "H") {
						Form_Table$FT_Form[j] <- form_update(Form_Table$FT_Form[j], 1.2)
						Form_Table$FT_Home_Form[j] <- form_update(Form_Table$FT_Home_Form[j], 1.2)
					} else if (data_sample_3$FTR[i + (k - 1) * n_games] == "D"){
						Form_Table$FT_Form[j] <- form_update(Form_Table$FT_Form[j], 1.1)
						Form_Table$FT_Home_Form[j] <- form_update(Form_Table$FT_Home_Form[j], 1.1)
					} else if (data_sample_3$FTR[i + (k - 1) * n_games] == "A"){
						Form_Table$FT_Form[j] <- form_update(Form_Table$FT_Form[j], 0.8)
						Form_Table$FT_Home_Form[j] <- form_update(Form_Table$FT_Home_Form[j], 0.8)
					} 
					#Half Time Results
					if (data_sample_3$HTR[i + (k - 1) * n_games] == "H"){
						Form_Table$HT_Form[j] <- form_update(Form_Table$HT_Form[j], 1.2)
						Form_Table$HT_Home_Form[j] <- form_update(Form_Table$HT_Home_Form[j], 1.2)
					} else if (data_sample_3$HTR[i + (k - 1) * n_games] == "D"){
						Form_Table$HT_Form[j] <- form_update(Form_Table$HT_Form[j], 1.1)
						Form_Table$HT_Home_Form[j] <- form_update(Form_Table$HT_Home_Form[j], 1.1)
					} else if (data_sample_3$HTR[i + (k - 1) * n_games] == "A"){
						Form_Table$HT_Form[j] <- form_update(Form_Table$HT_Form[j], 0.8)
						Form_Table$HT_Home_Form[j] <- form_update(Form_Table$HT_Home_Form[j], 0.8)
					}
					#Values irrespective of Win or Lose
					Form_Table$Shot_Form[j] <- form_update(Form_Table$Shot_Form[j], (data_sample_3$HS[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Home_Shot_Form[j] <- form_update(Form_Table$Home_Shot_Form[j], (data_sample_3$HS[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Shot_Target_Form[j] <- form_update(Form_Table$Shot_Target_Form[j], (data_sample_3$HST[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Home_Shot_Target_Form[j] <- form_update(Form_Table$Home_Shot_Target_Form[j], (data_sample_3$HST[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Shot_Target_P_Form[j] <- form_update(Form_Table$Shot_Target_P_Form[j], (data_sample_3$HTSTP[i + (k - 1) * n_games]))
					Form_Table$Home_Shot_Target_P_Form[j] <- form_update(Form_Table$Home_Shot_Target_P_Form[j], (data_sample_3$HTSTP[i + (k - 1) * n_games]))
					Form_Table$Corner_Form[j] <- form_update(Form_Table$Corner_Form[j], (data_sample_3$HC[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Home_Corner_Form[j] <- form_update(Form_Table$Home_Corner_Form[j], (data_sample_3$HC[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Foul_Form[j] <- form_update(Form_Table$Foul_Form[j], (data_sample_3$HF[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Home_Foul_Form[j] <- form_update(Form_Table$Home_Foul_Form[j], (data_sample_3$HF[i + (k - 1) * n_games]/10 + 1))
					Form_Table$YC_Form[j] <- form_update(Form_Table$YC_Form[j], (data_sample_3$HY[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Home_YC_Form[j] <- form_update(Form_Table$Home_YC_Form[j], (data_sample_3$HY[i + (k - 1) * n_games]/10 + 1))
					Form_Table$RC_Form[j] <- form_update(Form_Table$RC_Form[j], (data_sample_3$HR[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Home_RC_Form[j] <- form_update(Form_Table$Home_RC_Form[j], (data_sample_3$HR[i + (k - 1) * n_games]/10 + 1))
					#Att/Mid/Def Form
					Form_Table$FT_Att_Form[j] <- form_update(Form_Table$FT_Att_Form[j], (data_sample_3$FTHG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$FT_Att_Home_Form[j] <- form_update(Form_Table$FT_Att_Home_Form[j], (data_sample_3$FTHG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$HT_Att_Form[j] <- form_update(Form_Table$HT_Att_Form[j], (data_sample_3$HTHG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$HT_Att_Home_Form[j] <- form_update(Form_Table$HT_Att_Home_Form[j], (data_sample_3$HTHG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$FT_Mid_Form[j] <- form_update(Form_Table$FT_Mid_Form[j], (data_sample_3$FTHG[i + (k - 1) * n_games] + 1)/(10*(data_sample_3$FTAG[i + (k - 1) * n_games] + 1)) + 1)
					Form_Table$FT_Mid_Home_Form[j] <- form_update(Form_Table$FT_Mid_Home_Form[j], (data_sample_3$FTHG[i + (k - 1) * n_games] + 1)/(10*(data_sample_3$FTAG[i + (k - 1) * n_games] + 1)) + 1)
					Form_Table$HT_Mid_Form[j] <- form_update(Form_Table$HT_Mid_Form[j], (data_sample_3$HTHG[i + (k - 1) * n_games] + 1)/(10*(data_sample_3$HTAG[i + (k - 1) * n_games] + 1)) + 1)
					Form_Table$HT_Mid_Home_Form[j] <- form_update(Form_Table$HT_Mid_Home_Form[j], (data_sample_3$HTHG[i + (k - 1) * n_games] + 1)/(10*(data_sample_3$HTAG[i + (k - 1) * n_games] + 1)) + 1)
					Form_Table$FT_Def_Form[j] <- form_update(Form_Table$FT_Def_Form[j], (data_sample_3$FTAG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$FT_Def_Home_Form[j] <- form_update(Form_Table$FT_Def_Home_Form[j], (data_sample_3$FTAG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$HT_Def_Form[j] <- form_update(Form_Table$HT_Def_Form[j], (data_sample_3$HTAG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$HT_Def_Home_Form[j] <- form_update(Form_Table$HT_Def_Home_Form[j], (data_sample_3$HTAG[i + (k - 1) * n_games]/10 + 1))
					
				} else if (data_sample_3$AwayTeam[i + (k - 1) * n_games] == Form_Table$Team[j]){
					# Record Values pre Update from Results
					for (m in 1:n_form_variables){
						data_sample_3[i + (k - 1) * n_games,m + d_af_index] <- Form_Table[j,m + 1]
					}
					for (m in 1:n_form_variables){
						data_sample_3[i + (k - 1) * n_games,m + d_aaf_index] <- Form_Table[j,m + 1 + (2 * n_form_variables)]
					}
					if (data_sample_3$FTR[i + (k - 1) * n_games] == "H") {
						Form_Table$FT_Form[j] <- form_update(Form_Table$FT_Form[j], 0.8)
						Form_Table$FT_Away_Form[j] <- form_update(Form_Table$FT_Away_Form[j], 0.8)
					} else if (data_sample_3$FTR[i + (k - 1) * n_games] == "D"){
						Form_Table$FT_Form[j] <- form_update(Form_Table$FT_Form[j], 1.1)
						Form_Table$FT_Away_Form[j] <- form_update(Form_Table$FT_Away_Form[j], 1.1)
					} else if (data_sample_3$FTR[i + (k - 1) * n_games] == "A"){
						Form_Table$FT_Form[j] <- form_update(Form_Table$FT_Form[j], 1.2)
						Form_Table$FT_Away_Form[j] <- form_update(Form_Table$FT_Away_Form[j], 1.2)
					} 
					if (data_sample_3$HTR[i + (k - 1) * n_games] == "H") {
						Form_Table$HT_Form[j] <- form_update(Form_Table$HT_Form[j], 0.8)
						Form_Table$HT_Away_Form[j] <- form_update(Form_Table$HT_Away_Form[j], 0.8)
					} else if (data_sample_3$HTR[i + (k - 1) * n_games] == "D"){
						Form_Table$HT_Form[j] <- form_update(Form_Table$HT_Form[j], 1.1)
						Form_Table$HT_Away_Form[j] <- form_update(Form_Table$HT_Away_Form[j], 1.1)
					} else if (data_sample_3$HTR[i + (k - 1) * n_games] == "A"){
						Form_Table$HT_Form[j] <- form_update(Form_Table$HT_Form[j], 1.2)
						Form_Table$HT_Away_Form[j] <- form_update(Form_Table$HT_Away_Form[j], 1.2)
					}
					#Values irrespective of Win or Lose
					Form_Table$Shot_Form[j] <- form_update(Form_Table$Shot_Form[j], (data_sample_3$AS[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Away_Shot_Form[j] <- form_update(Form_Table$Away_Shot_Form[j], (data_sample_3$AS[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Shot_Target_Form[j] <- form_update(Form_Table$Shot_Target_Form[j], (data_sample_3$AST[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Away_Shot_Target_Form[j] <- form_update(Form_Table$Away_Shot_Target_Form[j], (data_sample_3$AST[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Shot_Target_P_Form[j] <- form_update(Form_Table$Shot_Target_P_Form[j], (data_sample_3$ATSTP[i + (k - 1) * n_games]))
					Form_Table$Away_Shot_Target_P_Form[j] <- form_update(Form_Table$Away_Shot_Target_P_Form[j], (data_sample_3$ATSTP[i + (k - 1) * n_games]))
					Form_Table$Corner_Form[j] <- form_update(Form_Table$Corner_Form[j], (data_sample_3$AC[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Away_Corner_Form[j] <- form_update(Form_Table$Away_Corner_Form[j], (data_sample_3$AC[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Foul_Form[j] <- form_update(Form_Table$Foul_Form[j], (data_sample_3$AF[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Away_Foul_Form[j] <- form_update(Form_Table$Away_Foul_Form[j], (data_sample_3$AF[i + (k - 1) * n_games]/10 + 1))
					Form_Table$YC_Form[j] <- form_update(Form_Table$YC_Form[j], (data_sample_3$AY[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Away_YC_Form[j] <- form_update(Form_Table$Away_YC_Form[j], (data_sample_3$AY[i + (k - 1) * n_games]/10 + 1))
					Form_Table$RC_Form[j] <- form_update(Form_Table$RC_Form[j], (data_sample_3$AR[i + (k - 1) * n_games]/10 + 1))
					Form_Table$Away_RC_Form[j] <- form_update(Form_Table$Away_RC_Form[j], (data_sample_3$AR[i + (k - 1) * n_games]/10 + 1))
					#Att/Mid/Def Form
					Form_Table$FT_Att_Form[j] <- form_update(Form_Table$FT_Att_Form[j], (data_sample_3$FTAG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$FT_Att_Away_Form[j] <- form_update(Form_Table$FT_Att_Away_Form[j], (data_sample_3$FTAG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$HT_Att_Form[j] <- form_update(Form_Table$HT_Att_Form[j], (data_sample_3$HTAG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$HT_Att_Away_Form[j] <- form_update(Form_Table$HT_Att_Away_Form[j], (data_sample_3$HTAG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$FT_Mid_Form[j] <- form_update(Form_Table$FT_Mid_Form[j], (data_sample_3$FTAG[i + (k - 1) * n_games] + 1)/(10*(data_sample_3$FTHG[i + (k - 1) * n_games] + 1)) + 1)
					Form_Table$FT_Mid_Away_Form[j] <- form_update(Form_Table$FT_Mid_Away_Form[j], (data_sample_3$FTAG[i + (k - 1) * n_games] + 1)/(10*(data_sample_3$FTHG[i + (k - 1) * n_games] + 1)) + 1)
					Form_Table$HT_Mid_Form[j] <- form_update(Form_Table$HT_Mid_Form[j], (data_sample_3$HTAG[i + (k - 1) * n_games] + 1)/(10*(data_sample_3$HTHG[i + (k - 1) * n_games] + 1)) + 1)
					Form_Table$HT_Mid_Away_Form[j] <- form_update(Form_Table$HT_Mid_Away_Form[j], (data_sample_3$HTAG[i + (k - 1) * n_games] + 1)/(10*(data_sample_3$HTHG[i + (k - 1) * n_games] + 1)) + 1)
					Form_Table$FT_Def_Form[j] <- form_update(Form_Table$FT_Def_Form[j], (data_sample_3$FTHG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$FT_Def_Away_Form[j] <- form_update(Form_Table$FT_Def_Away_Form[j], (data_sample_3$FTHG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$HT_Def_Form[j] <- form_update(Form_Table$HT_Def_Form[j], (data_sample_3$HTHG[i + (k - 1) * n_games]/10 + 1))
					Form_Table$HT_Def_Away_Form[j] <- form_update(Form_Table$HT_Def_Away_Form[j], (data_sample_3$HTHG[i + (k - 1) * n_games]/10 + 1))
				}
			}
			total <- c(0)
			for (m in 2:45){
				total[m] <- sum(Form_Table[,m])
				Form_Table[,m] <- Form_Table[,m]/total[m]
			}
		}
	}
	
	
	
	return(data_sample_3)
	
}

H2H_table_generator <- function(data_sample_3, n_seasons, n_games, n_teams){
	#Function which generates H2H form features and generates H2H Tables for each Team
	#Inputs:
	#data_sample_3 = DataFrame
	#n_seasons = Number of Seasons
	#n_games = Number of Games
	#n_teams = Number of Teams
	#Outputs:
	#H2H_test = DataFrame with Form Variables
	#H2H_tables = List of Head-2-Head Form DataFrames

	H2H_test <- data_sample_3


	H2H_names <-c("FT_Form", "FT_Venue_Form", "HT_Form", "HT_Venue_Form",
	 "FT_Att_Form", "FT_Att_Venue_Form", "FT_Mid_Form",
	"FT_Mid_Venue_Form", "FT_Def_Form", "FT_Def_Venue_Form",
	"HT_Att_Form", "HT_Att_Venue_Form", "HT_Mid_Form",
	"HT_Mid_Venue_Form", "HT_Def_Form", "HT_Def_Venue_Form",
	"Shot_Form", "Venue_Shot_Form", "Shot_Target_Form", 
	"Venue_Shot_Target_Form", "Shot_Target_P_Form",
	"Venue_Shot_Target_P_Form", "Corner_Form",
	"Venue_Corner_Form", "Foul_Form", "Venue_Foul_Form",
	"YC_Form", "Venue_YC_Form", "RC_Form",
	"Venue_RC_Form")

	H2H_table_names <-c("Team", "FT_Form", "FT_Home_Form", "FT_Away_Form", "HT_Form", "HT_Home_Form",
	"HT_Away_Form", "FT_Att_Form", "FT_Att_Home_Form", "FT_Att_Away_Form", "FT_Mid_Form",
	"FT_Mid_Home_Form", "FT_Mid_Away_Form", "FT_Def_Form", "FT_Def_Home_Form", "FT_Def_Away_Form",
	"HT_Att_Form", "HT_Att_Home_Form", "HT_Att_Away_Form", "HT_Mid_Form",
	"HT_Mid_Home_Form", "HT_Mid_Away_Form", "HT_Def_Form", "HT_Def_Home_Form", "HT_Def_Away_Form",
	"Shot_Form", "Home_Shot_Form", "Away_Shot_Form", "Shot_Target_Form", 
	"Home_Shot_Target_Form", "Away_Shot_Target_Form", "Shot_Target_P_Form",
	"Home_Shot_Target_P_Form", "Away_Shot_Target_P_Form", "Corner_Form",
	"Home_Corner_Form", "Away_Corner_Form", "Foul_Form", "Home_Foul_Form",
	"Away_Foul_Form", "YC_Form", "Home_YC_Form", "Away_YC_Form", "RC_Form",
	"Home_RC_Form", "Away_RC_Form")

	H2H_var_names <- H2H_names %>% lapply(paste_word, "H2H") %>% unlist()

	form_names <- H2H_table_names[2:16]
	home_names <- H2H_table_names[17:31]
	away_names <- H2H_table_names[32:46]

	PH_form_names <- form_names %>% lapply(paste_word, "PH") %>% unlist()
	PA_form_names <- form_names %>% lapply(paste_word, "PA") %>% unlist()
	home_names <- home_names %>% lapply(paste_word, "PH") %>% unlist()
	away_names <- away_names %>% lapply(paste_word, "PA") %>% unlist()

	total_names <- c(H2H_table_names[1], PH_form_names, PA_form_names, home_names, away_names)

	H2H_table_names <- H2H_table_names %>% lapply(paste_word, "H2H") %>% unlist()

	H2H_table_names <- H2H_table_names[c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,3,6,9,12,15,18,21,24,27,
	30,33,36,39,42,45,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)]

	# Creating H2H Table Form
	teams_vec <- unique(H2H_test$HomeTeam)
	team_id <- c()

	for (i in 1:length(teams_vec)){
		team_id[i] <- paste("id", i, sep = "_")

	}
	init_form <- 0.5

	H2H_tables = list()

	for (i in 1:length(teams_vec)) {	
		matr <- matrix(init_form, nrow = (length(teams_vec) - 1), ncol = length(H2H_table_names) - 1)	
		fram <- data.frame(teams_vec[-i], matr)
		colnames(fram) <- H2H_table_names
		assign(team_id[i], fram)
		H2H_tables[[i]] = fram
	}

	# All Form Tables are now initialised based on team_id
	# Now create form variables in H2H_test

	H2H_var_names <- H2H_var_names[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)]

	values_3 <- matrix(0, nrow = nrow(H2H_test), ncol = length(H2H_var_names))
	colnames(values_3) <- H2H_var_names
	H2H_test <- data.frame(H2H_test, values_3)


	# Add team_id to home_team
	for (i in 1:nrow(H2H_test)){
		for (j in 1:length(teams_vec)){
			if(H2H_test$HomeTeam[i] == teams_vec[j]){
				H2H_test$Home_Team_Id[i] <- team_id[j]
			}
		}
	}

	# Add team_id to away_team
	for (i in 1:nrow(H2H_test)){
		for (j in 1:length(teams_vec)){
			if(H2H_test$AwayTeam[i] == teams_vec[j]){
				H2H_test$Away_Team_Id[i] <- team_id[j]
			}
		}
	}

	for (z in 1:n_seasons){
		for (i in 1:n_games){
			for (j in 1:length(team_id)){
				if (H2H_test$Home_Team_Id[i + (z-1) *  n_games] == team_id[j]){
					#Found Home Table Id
					f_h_t_ind <- j
					for(k in 1:nrow(H2H_tables[[j]])){
						if(H2H_tables[[j]][k,1] == H2H_test$AwayTeam[i + ((z-1) *  n_games)]){
							#Found Away in Home Table Id
							f_a_h_t_ind <- k
							#Form Update to Data
							for (r in 1:30){
								H2H_test[i + (z-1) * n_games, r + 87] <- H2H_tables[[f_h_t_ind]][f_a_h_t_ind, r + 1]
							}
							
							if(H2H_test$FTR[i + (z-1) *  n_games] == "H"){
								H2H_tables[[j]]$H2H_FT_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Form[k], 1.2)
								H2H_tables[[j]]$H2H_FT_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Home_Form[k], 1.2)
							}  else if (H2H_test$FTR[i + (z-1) *  n_games] == "D"){
								H2H_tables[[j]]$H2H_FT_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Form[k], 1)
								H2H_tables[[j]]$H2H_FT_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Home_Form[k], 1)
							}  else if (H2H_test$FTR[i + (z-1) *  n_games] == "A"){
								H2H_tables[[j]]$H2H_FT_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Form[k], 1)
								H2H_tables[[j]]$H2H_FT_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Home_Form[k], 1)
							} 
							
							if (H2H_test$HTR[i + (z-1) *  n_games] == "H") {
								H2H_tables[[j]]$H2H_HT_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Form[k], 1.2)
								H2H_tables[[j]]$H2H_HT_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Home_Form[k], 1.2)
							} else if (H2H_test$HTR[i + (z-1) *  n_games] == "D"){
								H2H_tables[[j]]$H2H_HT_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Form[k], 1)
								H2H_tables[[j]]$H2H_HT_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Home_Form[k], 1)
							} else if (H2H_test$HTR[i + (z-1) *  n_games] == "A"){
								H2H_tables[[j]]$H2H_HT_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Form[k], 1)
								H2H_tables[[j]]$H2H_HT_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Home_Form[k], 1)
							}
							
							#Values irrespective of Win or Lose
							H2H_tables[[j]]$H2H_Shot_Form[k] <- form_update(H2H_tables[[j]]$H2H_Shot_Form[k], (H2H_test$HS[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Home_Shot_Form[k] <- form_update(H2H_tables[[j]]$H2H_Home_Shot_Form[k], (H2H_test$HS[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Shot_Target_Form[k] <- form_update(H2H_tables[[j]]$H2H_Shot_Target_Form[k], (H2H_test$HST[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Home_Shot_Target_Form[k] <- form_update(H2H_tables[[j]]$H2H_Home_Shot_Target_Form[k], (H2H_test$HST[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Shot_Target_P_Form[k] <- form_update(H2H_tables[[j]]$H2H_Shot_Target_P_Form[k], (H2H_test$HTSTP[i + (z-1) *  n_games]))
							H2H_tables[[j]]$H2H_Home_Shot_Target_P_Form[k] <- form_update(H2H_tables[[j]]$H2H_Home_Shot_Target_P_Form[k], (H2H_test$HTSTP[i + (z-1) *  n_games]))
							H2H_tables[[j]]$H2H_Corner_Form[k] <- form_update(H2H_tables[[j]]$H2H_Corner_Form[k], (H2H_test$HC[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Home_Corner_Form[k] <- form_update(H2H_tables[[j]]$H2H_Home_Corner_Form[k], (H2H_test$HC[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Foul_Form[k] <- form_update(H2H_tables[[j]]$H2H_Foul_Form[k], (H2H_test$HF[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Home_Foul_Form[k] <- form_update(H2H_tables[[j]]$H2H_Home_Foul_Form[k], (H2H_test$HF[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_YC_Form[k] <- form_update(H2H_tables[[j]]$H2H_YC_Form[k], (H2H_test$HY[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Home_YC_Form[k] <- form_update(H2H_tables[[j]]$H2H_Home_YC_Form[k], (H2H_test$HY[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_RC_Form[k] <- form_update(H2H_tables[[j]]$H2H_RC_Form[k], (H2H_test$HR[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Home_RC_Form[k] <- form_update(H2H_tables[[j]]$H2H_Home_RC_Form[k], (H2H_test$HR[i + (z-1) *  n_games]/10 + 1))
							#Att/Mid/Def Form
							H2H_tables[[j]]$H2H_FT_Att_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Att_Form[k], (H2H_test$FTHG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_FT_Att_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Att_Home_Form[k], (H2H_test$FTHG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_HT_Att_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Att_Form[k], (H2H_test$HTHG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_HT_Att_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Att_Home_Form[k], (H2H_test$HTHG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_FT_Mid_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Mid_Form[k], (H2H_test$FTHG[i + (z-1) *  n_games] + 1)/(10*(H2H_test$FTAG[i + (z-1) *  n_games] + 1)) + 1)
							H2H_tables[[j]]$H2H_FT_Mid_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Mid_Home_Form[k], (H2H_test$FTHG[i + (z-1) *  n_games] + 1)/(10*(H2H_test$FTAG[i + (z-1) *  n_games] + 1)) + 1)
							H2H_tables[[j]]$H2H_HT_Mid_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Mid_Form[k], (H2H_test$HTHG[i + (z-1) *  n_games] + 1)/(10*(H2H_test$HTAG[i + (z-1) *  n_games] + 1)) + 1)
							H2H_tables[[j]]$H2H_HT_Mid_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Mid_Home_Form[k], (H2H_test$HTHG[i + (z-1) *  n_games] + 1)/(10*(H2H_test$HTAG[i + (z-1) *  n_games] + 1)) + 1)
							H2H_tables[[j]]$H2H_FT_Def_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Def_Form[k], (H2H_test$FTAG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_FT_Def_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Def_Home_Form[k], (H2H_test$FTAG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_HT_Def_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Def_Form[k], (H2H_test$HTAG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_HT_Def_Home_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Def_Home_Form[k], (H2H_test$HTAG[i + (z-1) *  n_games]/10 + 1))
						}
					}
				} else if (H2H_test$Away_Team_Id[i + (z-1) *  n_games] == team_id[j]){
					#Found Away Table Id
					f_a_t_ind <- j
					for (k in 1:nrow(H2H_tables[[j]])){
						if(H2H_tables[[j]][k,1] == H2H_test$HomeTeam[i + ((z-1) *  n_games)]){
							#Found Home in Away Table Id
							f_h_a_t_ind <- k
							if(H2H_test$FTR[i + (z-1) *  n_games] == "H"){
								H2H_tables[[j]]$H2H_FT_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Form[k], 1)
								H2H_tables[[j]]$H2H_FT_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Away_Form[k], 1)
							} else if (H2H_test$FTR[i + (z-1) *  n_games] == "D"){
								H2H_tables[[j]]$H2H_FT_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Form[k], 1)
								H2H_tables[[j]]$H2H_FT_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Away_Form[k], 1)
							}  else if (H2H_test$FTR[i + (z-1) *  n_games] == "A"){
								H2H_tables[[j]]$H2H_FT_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Form[k], 1.2)
								H2H_tables[[j]]$H2H_FT_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Away_Form[k], 1.2)
							} 
							
							if (H2H_test$HTR[i + (z-1) *  n_games] == "H"){
								H2H_tables[[j]]$H2H_HT_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Form[k], 1)
								H2H_tables[[j]]$H2H_HT_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Away_Form[k], 1)
							} else if (H2H_test$HTR[i + (z-1) *  n_games] == "D"){
								H2H_tables[[j]]$H2H_HT_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Form[k], 1)
								H2H_tables[[j]]$H2H_HT_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Away_Form[k], 1)
							} else if (H2H_test$HTR[i + (z-1) *  n_games] == "A"){
								H2H_tables[[j]]$H2H_HT_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Form[k], 1.2)
								H2H_tables[[j]]$H2H_HT_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Away_Form[k], 1.2)
							}
											
							H2H_tables[[j]]$H2H_Shot_Form[k] <- form_update(H2H_tables[[j]]$H2H_Shot_Form[k], (H2H_test$AS[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Away_Shot_Form[k] <- form_update(H2H_tables[[j]]$H2H_Away_Shot_Form[k], (H2H_test$AS[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Shot_Target_Form[k] <- form_update(H2H_tables[[j]]$H2H_Shot_Target_Form[k], (H2H_test$AST[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Away_Shot_Target_Form[k] <- form_update(H2H_tables[[j]]$H2H_Away_Shot_Target_Form[k], (H2H_test$AST[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Shot_Target_P_Form[k] <- form_update(H2H_tables[[j]]$H2H_Shot_Target_P_Form[k], (H2H_test$ATSTP[i + (z-1) *  n_games]))
							H2H_tables[[j]]$H2H_Away_Shot_Target_P_Form[k] <- form_update(H2H_tables[[j]]$H2H_Away_Shot_Target_P_Form[k], (H2H_test$ATSTP[i + (z-1) *  n_games]))
							H2H_tables[[j]]$H2H_Corner_Form[k] <- form_update(H2H_tables[[j]]$H2H_Corner_Form[k], (H2H_test$AC[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Away_Corner_Form[k] <- form_update(H2H_tables[[j]]$H2H_Away_Corner_Form[k], (H2H_test$AC[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Foul_Form[k] <- form_update(H2H_tables[[j]]$H2H_Foul_Form[k], (H2H_test$AF[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Away_Foul_Form[k] <- form_update(H2H_tables[[j]]$H2H_Away_Foul_Form[k], (H2H_test$AF[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_YC_Form[k] <- form_update(H2H_tables[[j]]$H2H_YC_Form[k], (H2H_test$AY[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Away_YC_Form[k] <- form_update(H2H_tables[[j]]$H2H_Away_YC_Form[k], (H2H_test$AY[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_RC_Form[k] <- form_update(H2H_tables[[j]]$H2H_RC_Form[k], (H2H_test$AR[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_Away_RC_Form[k] <- form_update(H2H_tables[[j]]$H2H_Away_RC_Form[k], (H2H_test$AR[i + (z-1) *  n_games]/10 + 1))
							#Att/Mid/Def Form
							H2H_tables[[j]]$H2H_FT_Att_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Att_Form[k], (H2H_test$FTAG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_FT_Att_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Att_Away_Form[k], (H2H_test$FTAG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_HT_Att_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Att_Form[k], (H2H_test$HTAG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_HT_Att_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Att_Away_Form[k], (H2H_test$HTAG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_FT_Mid_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Mid_Form[k], (H2H_test$FTAG[i + (z-1) *  n_games] + 1)/(10*(H2H_test$FTHG[i + (z-1) *  n_games] + 1)) + 1)
							H2H_tables[[j]]$H2H_FT_Mid_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Mid_Away_Form[k], (H2H_test$FTAG[i + (z-1) *  n_games] + 1)/(10*(H2H_test$FTHG[i + (z-1) *  n_games] + 1)) + 1)
							H2H_tables[[j]]$H2H_HT_Mid_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Mid_Form[k], (H2H_test$HTAG[i + (z-1) *  n_games] + 1)/(10*(H2H_test$HTHG[i + (z-1) *  n_games] + 1)) + 1)
							H2H_tables[[j]]$H2H_HT_Mid_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Mid_Away_Form[k], (H2H_test$HTAG[i + (z-1) *  n_games] + 1)/(10*(H2H_test$HTHG[i + (z-1) *  n_games] + 1)) + 1)
							H2H_tables[[j]]$H2H_FT_Def_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Def_Form[k], (H2H_test$FTHG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_FT_Def_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_FT_Def_Away_Form[k], (H2H_test$FTHG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_HT_Def_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Def_Form[k], (H2H_test$HTHG[i + (z-1) *  n_games]/10 + 1))
							H2H_tables[[j]]$H2H_HT_Def_Away_Form[k] <- form_update(H2H_tables[[j]]$H2H_HT_Def_Away_Form[k], (H2H_test$HTHG[i + (z-1) *  n_games]/10 + 1))
						}
					}
				}
			}
			#Normalisation Test
			total <- c()
			for (m in 2:46){
				if (m <= 16){
					total[m] <- sum(H2H_tables[[f_h_t_ind]][f_a_h_t_ind,m],H2H_tables[[f_a_t_ind]][f_h_a_t_ind,m])
					H2H_tables[[f_h_t_ind]][f_a_h_t_ind,m] <- H2H_tables[[f_h_t_ind]][f_a_h_t_ind,m]/total[m]
					H2H_tables[[f_a_t_ind]][f_h_a_t_ind,m] <- H2H_tables[[f_a_t_ind]][f_h_a_t_ind,m]/total[m]
				} else if (m <= 31){
					total[m] <- sum(H2H_tables[[f_h_t_ind]][f_a_h_t_ind,m],H2H_tables[[f_a_t_ind]][f_h_a_t_ind,m + 15])
					H2H_tables[[f_h_t_ind]][f_a_h_t_ind,m] <- H2H_tables[[f_h_t_ind]][f_a_h_t_ind,m]/total[m]
				} else if (m <= 46){
					#total[m] <- sum(H2H_tables[[f_h_t_ind]][f_a_h_t_ind,m-15],H2H_tables[[f_a_t_ind]][f_h_a_t_ind,m])
					H2H_tables[[f_a_t_ind]][f_h_a_t_ind,m] <- H2H_tables[[f_a_t_ind]][f_h_a_t_ind,m]/total[m-15]
				}
			}
		}
	}
	
	return_list = list(H2H_test, H2H_tables)
	
	return(return_list)
}

data_name_creator <- function(y, x){
	#Function to get data name in correct format
	#Inputs:
	#x
	#Outputs:
	#z
	z <- paste(x,y, sep = "")
	return(z)
}

##### Import Data #####

#www.football-data.co.uk League Names
#English Premier League, English Championship, English League 1, English League 2, La Liga, Bundesliga, Serie A, SPL, Ligue 1
leagues <- c("E0", "E1", "E2", "E3", "SP1", "D1", "I1", "SC0","F1")

base_link = "http://www.football-data.co.uk/mmz4281/0001/SC0.csv"
base_split = strsplit(base_link,"/")

years <- list("0001","0102","0203","0304","0405","0506","0607","0708","0809","0910","1011","1112","1213","1314","1415","1516","1617")

#####INPUT####		Choose League
x = 
league_code = leagues[[x]]
data_name <- lapply(years, data_import_link, league_code, base_split)

#Import Data
data_list <- data_name %>% lapply(main_football_file_reader, folder_code)

data_list <- data_name %>% lapply(read_csv)

#Select Relevant Columns
data_list <- data_list %>% lapply(select, Div, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)

#Row Bind Data into Single File
data_frame <- data_list[[1]]
for (i in 2:length(data_list)){
	data_frame <- rbind(data_frame, data_list[[i]])
}

##### Check for Missing Data #####
#Done for each league individually

#Check for Missing Data
any(is.na(data_frame))
unique(which(is.na(data_frame),arr.ind=TRUE)[,1])

miss_rows <- c(3311,3734)

for (k in 1:length(miss_rows)){
	for (i in 1:18){
		if (is.na(data_frame[miss_rows[k],(4+i)])){
			if ((i+4) == 8 | (i+4) == 9 | (i+4) == 10){
				data_frame[miss_rows[k],(4+i)] <- data_frame[miss_rows[k],(i+1)]
			} else {
				val_10 <- 0
				for (j in 1:10) {
					val_10 <- data_frame[c(miss_rows[k] + j), (4+i)] + val_10
				}
				data_frame[miss_rows[k],(4+i)] <- val_10/10
			}
		}
	}
}

##### Feature Creation #####

#####INPUT####		Features specific to Each League

#Details for Matchweek Feature - Specific to League
n_seasons <- 17 #Depends on how many seasons of Data are available with stats on www.football-data.co.uk
n_matchweeks <- 38 
n_teams <- 12
n_games_p_matchweek <- n_teams/2
n_games <- n_matchweeks * n_games_p_matchweek

#Implement Matchweek Creator
data_frame <- matchweek_creator(data_frame, n_seasons, n_matchweeks, n_games_p_matchweek)

#Clean Wrongly Spelled Team Names
(home_team_names <- unique(data_frame$HomeTeam) %>% sort())

#League Table Position Implementation
data_frame <- league_position(data_frame, n_seasons, n_games, n_teams)

#Data Transfer
data_frame <- data_frame

#Include Shots on Target Percentage Columns with Laplace Smoothing
data_frame <- data_frame %>% mutate(HTSTP = (HST + 1)/(HS + 1), ATSTP = (AST + 1)/(AS + 1))

#Rearrange columns to have HTSTP beside HST
data_frame <- data_frame[,c(1:14, 26:27, 15:25)]

#Generate Form Variables
data_frame = form_table_generator(data_frame, n_seasons, n_games, n_teams)

#Generate H2H Variables and H2H Tables
return_list <- H2H_table_generator(data_frame, n_seasons, n_games, n_teams)
data_frame = return_list[[1]]
H2H_tables = return_list[[2]]

#Introduce Season Number starting in 05 season
for (i in 1:n_seasons){
	for (j in 1:n_games){
		data_frame$Season[j + (i-1) * n_games] <- (i)
	}
}

#Write Files to Directory
filenames = c("EL1.csv","EL2.csv","EL3.csv","EL4.csv","ESP1.csv","FRA1.csv","GER1.csv","ITA1.csv","SCT1.csv")

output_folder = c("Main_Data")

dir.create(output_folder)

write.csv(data_frame, file = paste(output_folder,filenames[[x]],sep="//"), row.names = FALSE)

vec <- c(1:length(H2H_tables))

sheet_names <- vec %>% lapply(data_name_creator, "id_") %>% unlist()
sheet_names <- sheet_names %>% lapply(paste, ".csv", sep = "") %>% unlist()

H2H_output_folder = c("H2H_Data")

dir.create(H2H_output_folder)

dir.create(paste(H2H_output_folder,filenames[[x]], sep = "//"))

for (i in 1:length(H2H_tables)){
	write.csv(H2H_tables[[i]], file = paste(H2H_output_folder,filenames[[x]],sheet_names[i],sep="//"), row.names=FALSE)
}



