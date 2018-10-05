prediction_odd_request_8 <- function(league_code){
	####################
	# Prediction Odd Request
	#
	# Description:
	# Function which produces a list of confident bets on matches in the next 
	# matchweek for a specified league.
	#
	####################
	
	##### Define Function #####
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
		
	form_football_file_reader <- function(x, path){
		#Function which creates path to pull H2H DataFrames
		#Inputs:
		#x
		#path
		#Outputs:
		#z
		y <- sub("data", "", x)
		z <- read_csv(paste(path, "/", y, ".csv", sep = ""),col_types = 
		  cols(H2H_Team = col_character())
		  )
		return(z)
	}
	
	form_name_creator <- function(y, x){
		#Function which creates path to pull H2H DataFrames
		#Inputs:
		#x
		#y
		#Outputs:
		#z
		z <- paste(x,y, sep = "_")
		return(z)
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

	league_position_POR <- function(data_sample_2, n_seasons, n_games, n_teams){
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
			
			if (nrow(data_sample_2) < n_games){
				n_games = nrow(data_sample_2)
			}
			
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
		
		return_list = list(data_sample_2, League_Table)
		
		return(return_list)
	}

	form_table_generator_POR <- function(data_sample_3, n_seasons, n_games, n_teams){
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
		
		
		if (nrow(data_sample_3) < n_games){
				n_games = nrow(data_sample_3)
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
		
		return_list = list(data_sample_3, Form_Table, total_names)
		
		return(return_list)
		
	}

	H2H_table_updater <- function(latest_season, form_tables, teams_vec, n_seasons, n_games, n_teams){
		#Function which generates H2H form features and generates H2H Tables for each Team
		#Inputs:
		#data_sample_3 = DataFrame
		#n_seasons = Number of Seasons
		#n_games = Number of Games
		#n_teams = Number of Teams
		#Outputs:
		#H2H_test = DataFrame with Form Variables
		#H2H_tables = List of Head-2-Head Form DataFrames
		
		
		
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
		
		paste_word <- function(x, word){
			y <- paste(word, x, sep = "_")
			return(y)
		}
		
		#Define Form Update Function
		form_update <- function(x, form){
			x <- x * form
			return(x)
		}

		H2H_var_names <- H2H_names %>% lapply(paste_word, "H2H") %>% unlist()

		# Creating H2H Table Form
		#teams_vec <- unique(latest_season$HomeTeam)
		team_id <- c()
		
		for (i in 1:length(teams_vec)){
		team_id[i] <- paste("id", i, sep = "_")

		}

		H2H_var_names <- H2H_var_names[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)]
		
		values_3 <- matrix(0, nrow = nrow(latest_season), ncol = length(H2H_var_names))
		colnames(values_3) <- H2H_var_names
		latest_season <- data.frame(latest_season, values_3)
		
		lst <- sort(unique(latest_season$HomeTeam))
		
		new_team_indexes <- c()
		
		count_2 <- 1
		
		for (i in 1:length(lst)){
			count <- 0
			for (j in 1:length(teams_vec)){
				if (lst[i] == teams_vec[j]){
					count <- count + 1
				}
			}
			if (count == 0){
				new_team_indexes[[count_2]] <- i
				count_2 = count_2 + 1
			}
			
		}
		
		if (length(new_team_indexes) > 0){
			for (t in 1:length(new_team_indexes)){
				new_team_index = new_team_indexes[[t]]
		
				teams_vec[length(teams_vec) + 1] <- lst[new_team_index]
				team_id[length(teams_vec)] <- paste("id", length(teams_vec), sep = "_")
				
				init_form <- 0.5
				
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

				H2H_table_names <- H2H_table_names %>% lapply(paste_word, "H2H") %>% unlist()

				H2H_table_names <- H2H_table_names[c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,3,6,9,12,15,18,21,24,27,
				30,33,36,39,42,45,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)]
				
				matr <- matrix(init_form, nrow = (length(teams_vec) - 1), ncol = length(H2H_table_names) - 1)	
				fram <- data.frame(teams_vec[-length(teams_vec)], matr)
				colnames(fram) <- H2H_table_names
				fram[,1] = as.character(fram[,1])
				assign(team_id[length(teams_vec)], fram)
				
				form_tables[length(teams_vec)] <- list(as.tibble(fram))
				
				for (i in 1:(length(teams_vec) - 1)){
					#form_tables[i][[1]][,1] <- as.character(form_tables[i][[1]][,1])
					form_tables[i][[1]][(nrow(form_tables[i][[1]]) + 1),1] <- lst[new_team_index]
					for (j in 2:ncol(form_tables[i][[1]])){
						form_tables[i][[1]][nrow(form_tables[i][[1]]),j] <- 0.5
					}
				}
			}
		}
		
		# Add team_id to home_team
		for (i in 1:nrow(latest_season)){
			for (j in 1:length(teams_vec)){
				if(latest_season$HomeTeam[i] == teams_vec[j]){
					latest_season$Home_Team_Id[i] <- team_id[[j]]
				}
			}
		}

		# Add team_id to away_team
		for (i in 1:nrow(latest_season)){
			for (j in 1:length(teams_vec)){
				if(latest_season$AwayTeam[i] == teams_vec[j]){
					latest_season$Away_Team_Id[i] <- team_id[j]
				}
			}
		}
		
		#Form Variables
		for (i in 1:nrow(latest_season)){
			for (j in 1:length(team_id)){
				if (latest_season$Home_Team_Id[i] == team_id[j]){
					#Found Home Table Id
					f_h_t_ind <- j
					for(k in 1:nrow(form_tables[[j]])){
						if(form_tables[[j]][k,1] == latest_season$AwayTeam[i]){
							#Found Away in Home Table Id
							f_a_h_t_ind <- k
							#Form Update to Data
							for (r in 1:30){
								latest_season[i, r + 87] <- form_tables[[f_h_t_ind]][f_a_h_t_ind, r + 1]
							}
							
							if(latest_season$FTR[i] == "H"){
								form_tables[[j]]$H2H_FT_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Form[k], 1.2)
								form_tables[[j]]$H2H_FT_Home_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Home_Form[k], 1.2)
							}  else if (latest_season$FTR[i] == "D"){
								form_tables[[j]]$H2H_FT_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Form[k], 1)
								form_tables[[j]]$H2H_FT_Home_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Home_Form[k], 1)
							}  else if (latest_season$FTR[i] == "A"){
								form_tables[[j]]$H2H_FT_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Form[k], 1)
								form_tables[[j]]$H2H_FT_Home_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Home_Form[k], 1)
							} 
							
							if (latest_season$HTR[i] == "H") {
								form_tables[[j]]$H2H_HT_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Form[k], 1.2)
								form_tables[[j]]$H2H_HT_Home_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Home_Form[k], 1.2)
							} else if (latest_season$HTR[i] == "D"){
								form_tables[[j]]$H2H_HT_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Form[k], 1)
								form_tables[[j]]$H2H_HT_Home_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Home_Form[k], 1)
							} else if (latest_season$HTR[i] == "A"){
								form_tables[[j]]$H2H_HT_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Form[k], 1)
								form_tables[[j]]$H2H_HT_Home_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Home_Form[k], 1)
							}
							
							#Values irrespective of Win or Lose
							form_tables[[j]]$H2H_Shot_Form[k] <- form_update(form_tables[[j]]$H2H_Shot_Form[k], (latest_season$HS[i]/10 + 1))
							form_tables[[j]]$H2H_Home_Shot_Form[k] <- form_update(form_tables[[j]]$H2H_Home_Shot_Form[k], (latest_season$HS[i]/10 + 1))
							form_tables[[j]]$H2H_Shot_Target_Form[k] <- form_update(form_tables[[j]]$H2H_Shot_Target_Form[k], (latest_season$HST[i]/10 + 1))
							form_tables[[j]]$H2H_Home_Shot_Target_Form[k] <- form_update(form_tables[[j]]$H2H_Home_Shot_Target_Form[k], (latest_season$HST[i]/10 + 1))
							form_tables[[j]]$H2H_Shot_Target_P_Form[k] <- form_update(form_tables[[j]]$H2H_Shot_Target_P_Form[k], (latest_season$HTSTP[i]))
							form_tables[[j]]$H2H_Home_Shot_Target_P_Form[k] <- form_update(form_tables[[j]]$H2H_Home_Shot_Target_P_Form[k], (latest_season$HTSTP[i]))
							form_tables[[j]]$H2H_Corner_Form[k] <- form_update(form_tables[[j]]$H2H_Corner_Form[k], (latest_season$HC[i]/10 + 1))
							form_tables[[j]]$H2H_Home_Corner_Form[k] <- form_update(form_tables[[j]]$H2H_Home_Corner_Form[k], (latest_season$HC[i]/10 + 1))
							form_tables[[j]]$H2H_Foul_Form[k] <- form_update(form_tables[[j]]$H2H_Foul_Form[k], (latest_season$HF[i]/10 + 1))
							form_tables[[j]]$H2H_Home_Foul_Form[k] <- form_update(form_tables[[j]]$H2H_Home_Foul_Form[k], (latest_season$HF[i]/10 + 1))
							form_tables[[j]]$H2H_YC_Form[k] <- form_update(form_tables[[j]]$H2H_YC_Form[k], (latest_season$HY[i]/10 + 1))
							form_tables[[j]]$H2H_Home_YC_Form[k] <- form_update(form_tables[[j]]$H2H_Home_YC_Form[k], (latest_season$HY[i]/10 + 1))
							form_tables[[j]]$H2H_RC_Form[k] <- form_update(form_tables[[j]]$H2H_RC_Form[k], (latest_season$HR[i]/10 + 1))
							form_tables[[j]]$H2H_Home_RC_Form[k] <- form_update(form_tables[[j]]$H2H_Home_RC_Form[k], (latest_season$HR[i]/10 + 1))
							#Att/Mid/Def Form
							form_tables[[j]]$H2H_FT_Att_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Att_Form[k], (latest_season$FTHG[i]/10 + 1))
							form_tables[[j]]$H2H_FT_Att_Home_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Att_Home_Form[k], (latest_season$FTHG[i]/10 + 1))
							form_tables[[j]]$H2H_HT_Att_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Att_Form[k], (latest_season$HTHG[i]/10 + 1))
							form_tables[[j]]$H2H_HT_Att_Home_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Att_Home_Form[k], (latest_season$HTHG[i]/10 + 1))
							form_tables[[j]]$H2H_FT_Mid_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Mid_Form[k], (latest_season$FTHG[i] + 1)/(10*(latest_season$FTAG[i] + 1)) + 1)
							form_tables[[j]]$H2H_FT_Mid_Home_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Mid_Home_Form[k], (latest_season$FTHG[i] + 1)/(10*(latest_season$FTAG[i] + 1)) + 1)
							form_tables[[j]]$H2H_HT_Mid_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Mid_Form[k], (latest_season$HTHG[i] + 1)/(10*(latest_season$HTAG[i] + 1)) + 1)
							form_tables[[j]]$H2H_HT_Mid_Home_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Mid_Home_Form[k], (latest_season$HTHG[i] + 1)/(10*(latest_season$HTAG[i] + 1)) + 1)
							form_tables[[j]]$H2H_FT_Def_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Def_Form[k], (latest_season$FTAG[i]/10 + 1))
							form_tables[[j]]$H2H_FT_Def_Home_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Def_Home_Form[k], (latest_season$FTAG[i]/10 + 1))
							form_tables[[j]]$H2H_HT_Def_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Def_Form[k], (latest_season$HTAG[i]/10 + 1))
							form_tables[[j]]$H2H_HT_Def_Home_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Def_Home_Form[k], (latest_season$HTAG[i]/10 + 1))
						}
					}
				} else if (latest_season$Away_Team_Id[i] == team_id[j]){
					#Found Away Table Id
					f_a_t_ind <- j
					for (k in 1:nrow(form_tables[[j]])){
						if(form_tables[[j]][k,1] == latest_season$HomeTeam[i]){
							#Found Home in Away Table Id
							f_h_a_t_ind <- k
							if(latest_season$FTR[i] == "H"){
								form_tables[[j]]$H2H_FT_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Form[k], 1)
								form_tables[[j]]$H2H_FT_Away_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Away_Form[k], 1)
							} else if (latest_season$FTR[i] == "D"){
								form_tables[[j]]$H2H_FT_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Form[k], 1)
								form_tables[[j]]$H2H_FT_Away_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Away_Form[k], 1)
							}  else if (latest_season$FTR[i] == "A"){
								form_tables[[j]]$H2H_FT_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Form[k], 1.2)
								form_tables[[j]]$H2H_FT_Away_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Away_Form[k], 1.2)
							} 
							
							if (latest_season$HTR[i] == "H"){
								form_tables[[j]]$H2H_HT_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Form[k], 1)
								form_tables[[j]]$H2H_HT_Away_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Away_Form[k], 1)
							} else if (latest_season$HTR[i] == "D"){
								form_tables[[j]]$H2H_HT_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Form[k], 1)
								form_tables[[j]]$H2H_HT_Away_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Away_Form[k], 1)
							} else if (latest_season$HTR[i] == "A"){
								form_tables[[j]]$H2H_HT_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Form[k], 1.2)
								form_tables[[j]]$H2H_HT_Away_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Away_Form[k], 1.2)
							}
											
							form_tables[[j]]$H2H_Shot_Form[k] <- form_update(form_tables[[j]]$H2H_Shot_Form[k], (latest_season$AS[i]/10 + 1))
							form_tables[[j]]$H2H_Away_Shot_Form[k] <- form_update(form_tables[[j]]$H2H_Away_Shot_Form[k], (latest_season$AS[i]/10 + 1))
							form_tables[[j]]$H2H_Shot_Target_Form[k] <- form_update(form_tables[[j]]$H2H_Shot_Target_Form[k], (latest_season$AST[i]/10 + 1))
							form_tables[[j]]$H2H_Away_Shot_Target_Form[k] <- form_update(form_tables[[j]]$H2H_Away_Shot_Target_Form[k], (latest_season$AST[i]/10 + 1))
							form_tables[[j]]$H2H_Shot_Target_P_Form[k] <- form_update(form_tables[[j]]$H2H_Shot_Target_P_Form[k], (latest_season$ATSTP[i]))
							form_tables[[j]]$H2H_Away_Shot_Target_P_Form[k] <- form_update(form_tables[[j]]$H2H_Away_Shot_Target_P_Form[k], (latest_season$ATSTP[i]))
							form_tables[[j]]$H2H_Corner_Form[k] <- form_update(form_tables[[j]]$H2H_Corner_Form[k], (latest_season$AC[i]/10 + 1))
							form_tables[[j]]$H2H_Away_Corner_Form[k] <- form_update(form_tables[[j]]$H2H_Away_Corner_Form[k], (latest_season$AC[i]/10 + 1))
							form_tables[[j]]$H2H_Foul_Form[k] <- form_update(form_tables[[j]]$H2H_Foul_Form[k], (latest_season$AF[i]/10 + 1))
							form_tables[[j]]$H2H_Away_Foul_Form[k] <- form_update(form_tables[[j]]$H2H_Away_Foul_Form[k], (latest_season$AF[i]/10 + 1))
							form_tables[[j]]$H2H_YC_Form[k] <- form_update(form_tables[[j]]$H2H_YC_Form[k], (latest_season$AY[i]/10 + 1))
							form_tables[[j]]$H2H_Away_YC_Form[k] <- form_update(form_tables[[j]]$H2H_Away_YC_Form[k], (latest_season$AY[i]/10 + 1))
							form_tables[[j]]$H2H_RC_Form[k] <- form_update(form_tables[[j]]$H2H_RC_Form[k], (latest_season$AR[i]/10 + 1))
							form_tables[[j]]$H2H_Away_RC_Form[k] <- form_update(form_tables[[j]]$H2H_Away_RC_Form[k], (latest_season$AR[i]/10 + 1))
							#Att/Mid/Def Form
							form_tables[[j]]$H2H_FT_Att_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Att_Form[k], (latest_season$FTAG[i]/10 + 1))
							form_tables[[j]]$H2H_FT_Att_Away_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Att_Away_Form[k], (latest_season$FTAG[i]/10 + 1))
							form_tables[[j]]$H2H_HT_Att_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Att_Form[k], (latest_season$HTAG[i]/10 + 1))
							form_tables[[j]]$H2H_HT_Att_Away_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Att_Away_Form[k], (latest_season$HTAG[i]/10 + 1))
							form_tables[[j]]$H2H_FT_Mid_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Mid_Form[k], (latest_season$FTAG[i] + 1)/(10*(latest_season$FTHG[i] + 1)) + 1)
							form_tables[[j]]$H2H_FT_Mid_Away_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Mid_Away_Form[k], (latest_season$FTAG[i] + 1)/(10*(latest_season$FTHG[i] + 1)) + 1)
							form_tables[[j]]$H2H_HT_Mid_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Mid_Form[k], (latest_season$HTAG[i] + 1)/(10*(latest_season$HTHG[i] + 1)) + 1)
							form_tables[[j]]$H2H_HT_Mid_Away_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Mid_Away_Form[k], (latest_season$HTAG[i] + 1)/(10*(latest_season$HTHG[i] + 1)) + 1)
							form_tables[[j]]$H2H_FT_Def_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Def_Form[k], (latest_season$FTHG[i]/10 + 1))
							form_tables[[j]]$H2H_FT_Def_Away_Form[k] <- form_update(form_tables[[j]]$H2H_FT_Def_Away_Form[k], (latest_season$FTHG[i]/10 + 1))
							form_tables[[j]]$H2H_HT_Def_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Def_Form[k], (latest_season$HTHG[i]/10 + 1))
							form_tables[[j]]$H2H_HT_Def_Away_Form[k] <- form_update(form_tables[[j]]$H2H_HT_Def_Away_Form[k], (latest_season$HTHG[i]/10 + 1))
						}
					}
				}
			}
			#Normalisation Test
			total <- c()
			for (m in 2:46){
				if (m <= 16){
					total[m] <- sum(form_tables[[f_h_t_ind]][f_a_h_t_ind,m],form_tables[[f_a_t_ind]][f_h_a_t_ind,m])
					form_tables[[f_h_t_ind]][f_a_h_t_ind,m] <- form_tables[[f_h_t_ind]][f_a_h_t_ind,m]/total[m]
					form_tables[[f_a_t_ind]][f_h_a_t_ind,m] <- form_tables[[f_a_t_ind]][f_h_a_t_ind,m]/total[m]
				} else if (m <= 31){
					total[m] <- sum(form_tables[[f_h_t_ind]][f_a_h_t_ind,m],form_tables[[f_a_t_ind]][f_h_a_t_ind,m + 15])
					form_tables[[f_h_t_ind]][f_a_h_t_ind,m] <- form_tables[[f_h_t_ind]][f_a_h_t_ind,m]/total[m]
				} else if (m <= 46){
					#total[m] <- sum(form_tables[[f_h_t_ind]][f_a_h_t_ind,m-15],form_tables[[f_a_t_ind]][f_h_a_t_ind,m])
					form_tables[[f_a_t_ind]][f_h_a_t_ind,m] <- form_tables[[f_a_t_ind]][f_h_a_t_ind,m]/total[m-15]
				}
			}
		}
		
		return_list = list(latest_season, form_tables, H2H_var_names, team_id)
		
		return(return_list)
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
	
	gen_str_2 <- function(x){
		# x = nrow(index)
		k <- list()
		for (i in 1:x){
			k[[length(k)+1]] <- assign(paste("x",i, sep = ""), data.frame())
		}
		return(k)
	}
	
	gen_str_4 <- function(){
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
		r <- list(n,n,w,w,w,w,w,w,w)

		return(r)
	}
	
	prep_fixtures <- function(next_fixtures, n_matchweeks, League_Table, total_names, form_tables, H2H_var_names, team_id){
		
		next_fixtures$Matchweek <- (n_matchweeks + 1)	
		next_fixtures$H_Position <- c(NA)
		next_fixtures$A_Position <- c(NA)
		
		#Pull League Table Data
		for (i in 1:nrow(next_fixtures)){
			for (j in 1:n_teams){
				if (next_fixtures$HomeTeam[i] == League_Table$Team[j]) {
					next_fixtures$H_Position[i] <- j
				} else if (next_fixtures$AwayTeam[i] == League_Table$Team[j]) {
					next_fixtures$A_Position[i] <- j
				}
			}	
		}
		
		#Initialize Matrix
		values_2 <- matrix(0, nrow = nrow(next_fixtures), ncol = length(total_names))

		#Column Names Updated
		colnames(values_2) <- total_names

		#Data Updated
		next_fixtures <- data.frame(next_fixtures, values_2)
		
		#Number of Form Variables, non-repeated (Form, Home, Away)
		n_form_variables <- 15
		#Data Home Team Form Index
		d_hf_index <- 5
		#Date Home Team at Home Form Index
		d_hhf_index <- 35
		#Data Away Team Form Index
		d_af_index <- 20
		#Data Away Team at Away Form Index
		d_aaf_index <- 50

		#Pulling Form Details
		for (i in 1:nrow(next_fixtures)) {
			for (j in 1:length(Form_Table$Team)){
				if (next_fixtures$HomeTeam[i] == Form_Table$Team[j]){
					
					for (m in 1:n_form_variables){
						next_fixtures[i,m + d_hf_index] <- Form_Table[j,m + 1]
					}
					
					for (m in 1:n_form_variables){
						next_fixtures[i,m + d_hhf_index] <- Form_Table[j,m + 1 + n_form_variables]
					}
					
				} else if (next_fixtures$AwayTeam[i] == Form_Table$Team[j]){
					
					for (m in 1:n_form_variables){
						next_fixtures[i,m + d_af_index] <- Form_Table[j,m + 1]
					}                                                             
					for (m in 1:n_form_variables){
						next_fixtures[i,m + d_aaf_index] <- Form_Table[j,m + 1 + (2 * n_form_variables)]
					}
				}	
			}
		}
		
		#Prep for H2H Variables
		values_3 <- matrix(0, nrow = nrow(next_fixtures), ncol = length(H2H_var_names))
		colnames(values_3) <- H2H_var_names
		next_fixtures <- data.frame(next_fixtures, values_3)
			
		# Add team_id to home_team
		for (i in 1:nrow(next_fixtures)){
			for (j in 1:length(teams_vec)){
				if(next_fixtures$HomeTeam[i] == teams_vec[j]){
					next_fixtures$Home_Team_Id[i] <- team_id[j]
				}
			}
		}

		# Add team_id to away_team
		for (i in 1:nrow(next_fixtures)){
			for (j in 1:length(teams_vec)){
				if(next_fixtures$AwayTeam[i] == teams_vec[j]){
					next_fixtures$Away_Team_Id[i] <- team_id[j]
				}
			}
		}
		
		next_fixtures = next_fixtures[rowSums(is.na(next_fixtures)) == 0,]	
		rownames(next_fixtures) <- seq(length=nrow(next_fixtures))
			
		#Pull H2H Form 
		for (i in 1:nrow(next_fixtures)){
			for (j in 1:length(team_id)){
				if (next_fixtures$Home_Team_Id[i] == team_id[j]){
					#Found Home Table Id
					f_h_t_ind <- j
					for(k in 1:nrow(form_tables[[j]])){
						if(form_tables[[j]][k,1] == next_fixtures$AwayTeam[i]){
							#Found Away in Home Table Id
							f_a_h_t_ind <- k
							#Form Update to Data (Only take in home teams data because all normalised)
							for (r in 1:30){
								next_fixtures[i, r + 65] <- form_tables[[f_h_t_ind]][f_a_h_t_ind, r + 1]
							}
						}
					}
				}
			}
		}
		
		#Include Season Variable
		for (i in 1:nrow(next_fixtures)){
			next_fixtures$Season <- 18
		}
		
		return(next_fixtures)
	}
	
	round_d_5 <- function(x){
		#Function which rounds to the nearest 0.05
		y <- floor(x*20)/20
		return(y)
	}
	
	round_d_3 <- function(x){
		#Function which rounds values between 0.3 and 1 to the nearest 0.033 (roughly)
		y <- 0
		vals <- c(0.3,0.333,0.366,0.4,0.433,0.466,0.5, 0.533, 0.566, 0.6, 0.633, 0.666, 0.7, 0.733, 0.766, 0.8, 0.833, 0.866, 0.9, 1)
		for (i in 1:(length(vals) - 1)){
			if (x > vals[i] & x < vals[i + 1]){
				y <- vals[i]
			}
		}
		return(y)
	}
	
	#Merger Function to Pull Indices together, avoiding NA Values
	merger <- function(list1){
		vec <- pull(list1[[1]])
		for (i in 2:length(list1)){
			vec <- append(vec,pull(list1[[i]]))	
		}
		vec <- data.frame(sort(vec[which(!is.na(vec))]))
		if (nrow(vec) == 0){
			vec[1,1] <- NA
		}
		names(vec) <- c("Index_Values")
		return (vec)
	}
	
	conf_bets_generator <- function(prob_prediction, class_prediction, CAP_LUT, result_outcomes, test_MCY){
			
		#Result Index Generation - Records Index of Games with Predicted Result in Result Location in Data Structure 
		result_indexes <- gen_str_4()

		for (i in 1:length(class_prediction)){
			for (k in 1:length(result_outcomes[[i]])){
				count <- 1
				for (j in 1:length(class_prediction[[i]])){
					if (class_prediction[[i]][j] == result_outcomes[[i]][k]){
						result_indexes[[i]][[k]][count,1] <- j
						count <- count + 1
					}
					if (j == length(class_prediction[[i]]) & count == 1){
						result_indexes[[i]][[k]][count,1] <- NA
					}
				}
			}
		}
			
		#Confident Bet Selection
		prob_prediction_outcome <- gen_str_4()

		#Seperate Datasets - Pull Prob predictions this Index
		for (i in 1:length(prob_prediction)){
			for (k in 1:length(result_outcomes[[i]])){
				prob_prediction_outcome[[i]][[k]] <- prob_prediction[[i]][pull(result_indexes[[i]][[k]]),]
			}
		}

		conf_index <- gen_str_4()

		#Confident Bet Selection - Indexes which are above Designated Threshold
		for (i in 1:length(prob_prediction_outcome)){
			for (k in 1:length(prob_prediction_outcome[[i]])){
				if (is.na(prob_prediction_outcome[[i]][[k]][1,1])){
					conf_index[[i]][[k]][1,1] <- NA
				} else {
					count <- 1
					for (j in 1:nrow(prob_prediction_outcome[[i]][[k]])){
						if (max(prob_prediction_outcome[[i]][[k]][j,]) > test_MCV[i]){
							conf_index[[i]][[k]][count,1] <- as.numeric(row.names(prob_prediction_outcome[[i]][[k]]))[j]
							count <- count + 1
						}
						if (j == nrow(prob_prediction_outcome[[i]][[k]]) & count == 1){
							conf_index[[i]][[k]][count,1] <- NA
						}
					}
				}
			}
		}

			
		#Generate Full Index Structure
		full_conf_index <- gen_str_2(n_bets)	
		conf_index_size <- 0

		#Merge Data Structure and Get Total Number of Bets	
		for (i in 1:length(conf_index)){
			full_conf_index[[i]] <- merger(conf_index[[i]])
			conf_index_size <- conf_index_size + nrow(full_conf_index[[i]])
		}
				
		#Create Confident Bets Table
		bet_names <- c("FTR", "HTR", "OU_05", "OU_15", "OU_25", "OU_35", "OU_45", "OU_55", "BTS_FT")
		bf_market_names <- c("Match Odds", "Half Time", "Over/Under 0.5 Goals","Over/Under 1.5 Goals","Over/Under 2.5 Goals", "Over/Under 3.5 Goals","Over/Under 4.5 Goals","Over/Under 5.5 Goals","Both teams to Score?")

		HomeTeam <- c()
		AwayTeam <- c()
		Bet <- c()

		conf_bets <- matrix(0, nrow = conf_index_size, ncol = 8)
		conf_bets <- data.frame(conf_bets)
		names(conf_bets) <- c("HomeTeam", "AwayTeam", "Bet", "BetfairMarket", "Prediction", "BF_Pred", "Accuracy", "Returns")
				
		bet_rounder <- list(round_d_5,round_d_5,round_d_3,round_d_3,round_d_3,round_d_3,round_d_3,round_d_3,round_d_3)
			
		k <- 1
		for (i in 1:length(full_conf_index)){
			for (j in 1:nrow(full_conf_index[[i]])){
				if (is.na(full_conf_index[[i]][j,1])){
					next
				} else {
					conf_bets$HomeTeam[k] <- fixture_games$HomeTeam[pull(full_conf_index[[i]])[j]]
					conf_bets$AwayTeam[k] <- fixture_games$AwayTeam[pull(full_conf_index[[i]])[j]]
					conf_bets$League[k] <- league_code
					conf_bets$Bet[k] <- bet_names[i]
					conf_bets$BetfairMarket[k] <- bf_market_names[i]
					conf_bets$Prediction[k] <- class_prediction[[i]][pull(full_conf_index[[i]])[j]]
					
					if (i == 1 | i == 2){
						if (conf_bets$Prediction[k] == 1){
							conf_bets$BF_Pred[k] <- fixture_games$AwayTeam[pull(full_conf_index[[i]])[j]]
						} else if (conf_bets$Prediction[k] == 2){
							conf_bets$BF_Pred[k] <- c("The Draw")
						} else if (conf_bets$Prediction[k] == 3){
							conf_bets$BF_Pred[k] <- fixture_games$HomeTeam[pull(full_conf_index[[i]])[j]]
						}
					} else if (i == 3){
						if (conf_bets$Prediction[k] == 2){
							conf_bets$BF_Pred[k] <- c("Over 0.5 Goals")
						} else if (conf_bets$Prediction[k] == 1){
							conf_bets$BF_Pred[k] <- c("Under 0.5 Goals")
						}
					} else if (i == 4){
						if (conf_bets$Prediction[k] == 2){
							conf_bets$BF_Pred[k] <- c("Over 1.5 Goals")
						} else if (conf_bets$Prediction[k] == 1){
							conf_bets$BF_Pred[k] <- c("Under 1.5 Goals")
						}
					}else if (i == 5){
						if (conf_bets$Prediction[k] == 2){
							conf_bets$BF_Pred[k] <- c("Over 2.5 Goals")
						} else if (conf_bets$Prediction[k] == 1){
							conf_bets$BF_Pred[k] <- c("Under 2.5 Goals")
						}
					}else if (i == 6){
						if (conf_bets$Prediction[k] == 2){
							conf_bets$BF_Pred[k] <- c("Over 3.5 Goals")
						} else if (conf_bets$Prediction[k] == 1){
							conf_bets$BF_Pred[k] <- c("Under 3.5 Goals")
						}
					}else if (i == 7){
						if (conf_bets$Prediction[k] == 2){
							conf_bets$BF_Pred[k] <- c("Over 4.5 Goals")
						} else if (conf_bets$Prediction[k] == 1){
							conf_bets$BF_Pred[k] <- c("Under 4.5 Goals")
						}
					}else if (i == 8){
						if (conf_bets$Prediction[k] == 2){
							conf_bets$BF_Pred[k] <- c("Over 5.5 Goals")
						} else if (conf_bets$Prediction[k] == 1){
							conf_bets$BF_Pred[k] <- c("Under 5.5 Goals")
						}
					}else if (i == 9) {
						if (conf_bets$Prediction[k] == 2){
							conf_bets$BF_Pred[k] <- c("Yes")
						} else if (conf_bets$Prediction[k] == 1){
							conf_bets$BF_Pred[k] <- c("No")
						}
					}
					conf1 <- max(prob_prediction[[i]][pull(full_conf_index[[i]])[j],])
					conf_bets$Confidence[k] <- conf1
					conf1 <- bet_rounder[[i]](conf1)							
					for (m in 1:nrow(CAP_LUT[[i]][[conf_bets$Prediction[k]]])){
						if (CAP_LUT[[i]][[conf_bets$Prediction[k]]][m,1] == conf1){
							conf_bets$Accuracy[k] <- CAP_LUT[[i]][[conf_bets$Prediction[k]]][m,2]
							conf_bets$Proportion[k] <- CAP_LUT[[i]][[conf_bets$Prediction[k]]][m,3]
						}
					}
					conf_bets$Returns[k] <- 0
					k <- k + 1
				}
			}
		}
		
		return(conf_bets)
	}
	
	extract_returns <- function(conf_bets, league, bf, betfair_conversion_names, betfair_names){
	
		for (g in 1:nrow(conf_bets)){
		
			#Get Match Event Id
			for (j in 1:nrow(league)){
				if (league$HomeTeam[j] == conf_bets$HomeTeam[g]){
					event_id <- league$event_id[j]
					break
				}
			}

			#Get Markets for Particular Match
			markets <- bf$marketCatalogue(filter = marketFilter(eventTypeIds = 1, eventIds = event_id),maxResults = 100, sort = "MAXIMUM_TRADED")
			
			#Test
			if (g > 1){
				test_change <- market_id
			} else {
				test_change <- 0
			}
			
			#Get Market Id
			for (i in 1:length(markets)){
				if (markets[[i]][1]$raw$marketName == conf_bets$BetfairMarket[g]){
					market_id <- markets[[i]][1]$raw$marketId
					break
				}
			}
			
			#market[[1]][1]$raw$runners[[x]]$runnerName
			
			if (test_change == market_id){
				conf_bets$Returns[g] <- 0
			} else {
				#Get Team Description
				market <- bf$marketCatalogue(filter = marketFilter(marketIds = market_id), marketProjection = "RUNNER_DESCRIPTION")
				summary(market)
				
				if (conf_bets[g,3] == "FTR" | conf_bets[g,3] == "HTR"){
					for (a in 1:length(betfair_names)){
						if (conf_bets$BF_Pred[g] == betfair_conversion_names[a]){
							conf_bets$BF_Pred[g] <- betfair_names[a]
						}
					}	
				}
				
				bet_index <- c()
				
				for (r in 1:length(market[[1]]$raw$runners)){
					bet_index[r] <- market[[1]]$raw$runners[[r]]$runnerName
				}
				
				for (r in 1:length(bet_index)){
					if(conf_bets$BF_Pred[g] == bet_index[r]){
						market_index <- r
					}
				}
				
				select_id <- market[[1]]$raw$runners[[market_index]]$selectionId
							
				#Get Odds
				market_data <- bf$marketBook(marketIds = market_id, priceProjection = "EX_ALL_OFFERS")
				summary(market_data)
				
				odds_index <- c()
				
				for (r in 1:length(market_data[[1]]$raw$runners)){
					odds_index[r] <- market_data[[1]]$raw$runners[[r]]$selectionId
				}
				
				for (r in 1:length(odds_index)){
					if(select_id == odds_index[r]){
						returns_index <- r
					}
				}

				#Record Odds
				if (is.null(market_data[[1]][1]$raw$runners[[returns_index]]$lastPriceTraded)){
					conf_bets$Returns[g] <- 0
				} else{
					conf_bets$Returns[g] <- market_data[[1]][1]$raw$runners[[returns_index]]$lastPriceTraded
				}				
			}	
		}
		return(conf_bets)
	}
	
	##### Import Data #####
	
	#Read in Names Tables
	names_E1 <- read.csv("Names//names_E1.csv")
	names_E2 <- read.csv("Names//names_E2.csv")
	names_E3 <- read.csv("Names//names_E3.csv")
	names_SCO1 <- read.csv("Names//names_SCO1.csv")
	names_SPA1 <- read.csv("Names//names_SPA1.csv")
	names_GER1 <- read.csv("Names//names_GER1.csv")
	names_ITA1 <- read.csv("Names//names_ITA1.csv")
	names_E4 <- read.csv("Names//names_E4.csv")
	names_FRA1 <- read.csv("Names//names_FRA1.csv")
		
	#Convert Type
	names_E1 <- data.frame(lapply(names_E1, as.character), stringsAsFactors=FALSE)
	names_E2 <- data.frame(lapply(names_E2, as.character), stringsAsFactors=FALSE)
	names_E3 <- data.frame(lapply(names_E3, as.character), stringsAsFactors=FALSE)
	names_SCO1 <- data.frame(lapply(names_SCO1, as.character), stringsAsFactors=FALSE)
	names_SPA1 <- data.frame(lapply(names_SPA1, as.character), stringsAsFactors=FALSE)	
	names_GER1 <- data.frame(lapply(names_GER1, as.character), stringsAsFactors=FALSE)
	names_ITA1 <- data.frame(lapply(names_ITA1, as.character), stringsAsFactors=FALSE)
	names_E4 <- data.frame(lapply(names_E4, as.character), stringsAsFactors=FALSE)
	names_FRA1 <- data.frame(lapply(names_FRA1, as.character), stringsAsFactors=FALSE)
		
	conversion_names <- list(names_E1,names_E2,names_E3,names_E4,names_SPA1,names_GER1,names_ITA1,names_SCO1,names_FRA1)
	
	#Main LUT
	Main_LUT <- read.csv("Main_LUT.csv")	
	Main_LUT[,c(1:3,5,7:10)] <- data.frame(lapply(Main_LUT[,c(1:3,5,7:10)], as.character), stringsAsFactors=FALSE)
	
	#Pull Constants
	for (i in 1:nrow(Main_LUT)){
		if (league_code == Main_LUT[i,1]){
			main_file_folder_code <- Main_LUT[i,2]
			main_file_name <- Main_LUT[i,3]
			number_H2H_tables <- Main_LUT[i,4]
			H2H_table_folder_code <- Main_LUT[i,5]
			number_teams <- Main_LUT[i,6]
			latest_season_link <- Main_LUT[i,7]
			fixture_folder_code <- Main_LUT[i,8]
			fixture_file_name <- Main_LUT[i,9]
			betfair_league_name <- Main_LUT[i,10]
			conversion_index <- i
		}	
	}
	
	#Pull Conversion Names
	fixture_conversion_names <- conversion_names[[conversion_index]][,1]
	fixture_names <- conversion_names[[conversion_index]][,2]
	betfair_conversion_names <- conversion_names[[conversion_index]][,3]
	betfair_names <- conversion_names[[conversion_index]][,4]	
	
	#Import Data: Main File
	main_file <- main_file_name %>% lapply(main_football_file_reader, main_file_folder_code)
	
	teams_vec <- sort(unique(main_file[[1]]$HomeTeam))
		
	#Form Files Generated
	form_file <- c(1:number_H2H_tables) %>% form_name_creator("id")
	
	#Import Form Tables
	form_tables <- form_file %>% lapply(form_football_file_reader, paste("H2H_Data",H2H_table_folder_code),sep="//")
	
	#Import Latest Season
	
	#latest_season_link
	latest_season <- read.csv(latest_season_link)
	
	#Select Relevant Columns from latest_season
	latest_season <- latest_season %>% select(Div, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR)
	
	#Change First Four Variables to Characters
	latest_season[,c(1:4)] <- data.frame(lapply(latest_season[,c(1:4)], as.character), stringsAsFactors=FALSE)
	
	
	##### Data Preparation and Feature Extraction
		
	#Season Details
	n_seasons <- 1
	n_teams <- number_teams
	n_matchweeks <- ceiling(nrow(latest_season)/(n_teams/2))
	n_games_p_matchweek <- n_teams/2
	n_games <- n_matchweeks * n_games_p_matchweek
	
	#Matchweek Variable Implementation
	latest_season <- matchweek_creator(latest_season, n_seasons, n_matchweeks, n_games_p_matchweek)
	#latest_season < - final[complete.cases(latest_season), ]
	
	#League Position Feature Implementation
	return_list <- league_position_POR(latest_season, n_seasons, n_games, n_teams)
	latest_season = return_list[[1]]
	League_Table = return_list[[2]]
	
	#Include Shots on Target Percentage Columns with Laplace Smoothing
	latest_season <- latest_season %>% mutate(HTSTP = (HST + 1)/(HS + 1), ATSTP = (AST + 1)/(AS + 1))

	#Rearrange columns to have HTSTP beside HST
	latest_season <- latest_season[,c(1:14, 26:27, 15:25)]
	
	#Generate Form Variables
	return_list = form_table_generator_POR(latest_season, n_seasons, n_games, n_teams)
	latest_season = return_list[[1]]
	Form_Table = return_list[[2]]
	total_names = return_list[[3]]
	
	#Generate H2H Variables and H2H Tables
	return_list <- H2H_table_updater(latest_season, form_tables, teams_vec, n_seasons, n_games, n_teams)
	latest_season = return_list[[1]]
	H2H_tables = return_list[[2]]
	H2H_var_names = return_list[[3]]
	team_id = return_list[[4]]
	
	##### Pull Features for Next Matchweek #####
	
	#Login
	bf <- betfair(usr = "seandrum95", pwd = "headphones95", key = "kTDxYeaaIvxcjZAQ")
	
	#Get Football Competitions
	football_competitions <- bf$competitions(filter = marketFilter(eventTypeIds = 1))
	
	#Get League Id
	for (i in 1:nrow(football_competitions)){
		if (football_competitions$competition_name[i] == betfair_league_name){
			league_id <- football_competitions$competition_id[i]
			break
		}
	}
	
	#Filter for League
	league <- bf$events(filter = marketFilter(eventTypeIds = 1, competitionIds = league_id))
	
	#Clean League Data
	league <- league %>% arrange(event_openDate)
	league <- separate(league, event_name, into = c("HomeTeam", "AwayTeam"), sep = " v ")
	league <- filter(league, !is.na(AwayTeam))
	league <- filter(league, !is.na(HomeTeam))
	
	if (nrow(league) > (n_teams/2)){
		league <- league[c(1:(n_teams/2)),]
	}

	#Replace Incorrect Names
	for (i in 1:nrow(league)){
		for (j in 1:length(betfair_names)){
			if (league$HomeTeam[i] == betfair_names[j]){
				league$HomeTeam[i] <- betfair_conversion_names[j]
			}
			if (league$AwayTeam[i] == betfair_names[j]){
				league$AwayTeam[i] <- betfair_conversion_names[j]
			}
		}
	}
		
	next_fixtures <- league[,c(2,3)]
			
	#Pull Features for Next Fixtures
	next_fixtures <- prep_fixtures(next_fixtures, n_matchweeks, League_Table, total_names, form_tables, H2H_var_names, team_id)
	
	#Matchweek Games	
	fixture_games <- next_fixtures[,c(1,2)]
	#Matchweek Features
	next_fixtures <- next_fixtures[,c(3:95,98)]	

	##### Prediction #####
		
	#League and Bet Names
	league_names <- c("E0", "E1", "E2", "E3", "SP1", "D1", "I1", "SC0", "FRA")
	bet_names <- c("FTR", "HTR", "OU_05", "OU_15", "OU_25", "OU_35", "OU_45", "OU_55", "BTS_FT")
	
	#Create Structure for Storing Models
	n_bets <- length(bet_names)	
	chosen_vars <- gen_str_2(n_bets)
	models <- gen_str_2(n_bets)
	
	#Import Models and Optimum Features
	for (i in 1:n_bets){
		folder_name <- bet_names[i]
		league_name <- league_names[conversion_index]
		chosen_vars[[i]] <- read.csv(paste("Latest_Models\\",folder_name,"\\",league_name,"_vars.csv",sep=""))
		models[[i]] <- readRDS(paste("Latest_Models\\",folder_name,"\\",league_name,"_model.rda",sep=""))
		
	}
	
	#Generate Structure to Store Subsetted Data
	fixt_data <- gen_str_2(n_bets)

	#Subset Dataset
	for (i in 1:n_bets){
		fixt_data[[i]] <- next_fixtures[,as.character(pull(chosen_vars[[i]]))]
	}
	
	#Generate Structure for Predictions
	prob_prediction <- gen_str_2(n_bets)
	class_prediction <- gen_str_2(n_bets)
	
	#Make Predictions
	for (i in 1:n_bets){
		prob_prediction[[i]] <- predict(models[[i]], fixt_data[[i]], "prob")
		class_prediction[[i]] <- predict(models[[i]], fixt_data[[i]], "raw")		
	}
		
	##### CAP LUTs #####
	
	#Define Results
	result_results <- c("A", "D", "H")
	BTS_OU_result <- c(0,1)
	result_outcomes <- list(result_results, result_results, BTS_OU_result, BTS_OU_result, BTS_OU_result, BTS_OU_result, BTS_OU_result, BTS_OU_result, BTS_OU_result)
			
	#Generate CAP LUT Structure
	CAP_LUT <- gen_str_4()
	
	#Import Data
	for (i in 1:n_bets){
		for (k in 1:length(result_outcomes[[i]])){
			CAP_LUT[[i]][[k]] <- read.csv(paste("Latest_CAP_LUT\\",bet_names[i],"\\",bet_names[i],"_",league_names[conversion_index],"_",result_outcomes[[i]][k],"_CAP.csv", sep = ""))	
		}	
	}
	
	#Define Minimum Confidence Thresholds for each Outcome	
	test_MCV <- c(0.39,0.39,0.39,0.39,0.39,0.39,0.53,0.53,0.53,0.53,0.53,0.53,0.53,0.53,0.53,0.53,0.53,0.53)
	
	#Generate Confident Bets
	conf_bets <- conf_bets_generator(prob_prediction, class_prediction, CAP_LUT, result_outcomes, test_MCV)
	
	##### Extract Returns #####	
	
	conf_bets <- extract_returns(conf_bets, league, bf, betfair_conversion_names, betfair_names)
		
	return(conf_bets)
}