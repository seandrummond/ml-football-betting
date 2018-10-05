###################
# Return Optimisation Strategy
#
# Run Prediction_Odd_Request Function over Leagues to retrieve likely bets for this gameweek.
# Create new metric ConfAcc (Product of Confidence and Accuracy)
# Quantile Bets by ConfAcc
# Select Top Bets per Game
# Rank Bets
# Find Optimum Accumulator


### Import Libraries ###

#Import Libraries
library(readr)
library(dplyr)
library(randomForest)
library(caret)
library(betfaiR)
library(tidyr)
library(tibble)
library(ggplot2)
library(nloptr)

### Define Functions ###

data_struct_ct <- function(n_frames){
	#Function which Creates a List of N Lists
	#Inputs: 
	#n_frames = Number of Output Lists Desired
	#Outputs: 
	#n = List of N Lists
	
	n <- list()
	for (i in 1:n_frames){
		n[[length(n)+1]] <- assign(paste("x",i, sep = ""), data.frame())
	}
	return(n)
}

bet_quantile <- function(bets_3, quants){
	#Function which Divides Bets into Quantiles based on ConfAcc Parameter
	#Inputs:
	#bets_3 = DataFrame of Bets
	#quants = Desired Number of Quantiles
	#Outputs:
	#bets_3 = DataFrame of Bets with Quantile Columns

	data_struct_ct <- function(n_frames){
		n <- list()
		for (i in 1:n_frames){
			n[[length(n)+1]] <- assign(paste("x",i, sep = ""), data.frame())
		}
		return(n)
	}
	
	
	data_list <- data_struct_ct(9)
	
	bet_names <- c("FTR", "HTR", "OU_05", "OU_15", "OU_25", "OU_35", "OU_45", "OU_55", "BTS_FT")
	
	for (i in 1:length(bet_names)){
		data_list[[i]] <- bets_3[which(bets_3$Bet == bet_names[i]),]
	}
	
	bets_3 <- data_list[[1]] %>% mutate(quantile = ntile(ConfAcc, quants))

	for (i in 2:length(bet_names)){
		
		bets_3 <- rbind(bets_3, data_list[[i]]%>% mutate(quantile = ntile(ConfAcc, quants)))
	}
	return(bets_3)
}

exp_value <- function(returns, accuracy){
	#Function to Calculate Expected Value from Returns and Accuracy
	#Inputs:
	#Returns
	#Accuracy
	#Outputs:
	#Expected Value
	return((returns + 1) * accuracy - 1)
}

#Generate Data Structure
gen_returns_str <- function(x){
	#Function which Generates a List of x Matrices
	#Inputs:
	#x = Number of Desired Matrices
	#Outputs:
	#z = List of x Matrices

	z <- list()
	for (i in 1:x){
		z[[length(z)+1]] <- assign(paste("y",i,sep=""),matrix())
	}
	return(z)
}

gen_accum_str <- function(x){
	# x = nrow(index)
	for (i in 1:2){
		assign(paste("x",i, sep = ""), data.frame())
	}
	z <- list()
	for (i in 1:x){
		z[[length(z)+1]] <- assign(paste("y",i,sep=""),list(x1,x2))
	}
	return(z)
}

### Apply Prediction_Odd_Request to Leagues ###

#Set Working Directory
setwd()

#League Codes
leagues <- c("E1", "E2", "E3", "E4", "SPA1", "GER1", "ITA1", "SCO1","FRA1")

#Confident Bet Data Structure
conf_bets <- list()

#Apply Prediction Odd Request Function to Each League
for (i in 1:length(leagues)){
	conf_bets[[i]] <- prediction_odd_request_8(leagues[i])
}

#Rowbind all Confident Bets
all_conf_bets <- conf_bets[[1]]
for (i in 2:length(conf_bets)){
	all_conf_bets <- rbind(all_conf_bets, conf_bets[[i]])
} 

### Bet Selection ###

#Filer Out incorrect Bets
all_conf_bets <- all_conf_bets %>% filter(Returns != 0 & Accuracy != 0)

#Create ConfAcc Metric
all_conf_bets <- all_conf_bets %>% mutate(ConfAcc = Confidence * Accuracy)

#Remove poor Return Bets
bets_2 <- all_conf_bets[-(all_conf_bets$Returns < 1.1),]
bets_3 <- bets_2[- which(bets_2$Returns < 1.1),]

#Rank by ConfAcc
bets_3 <- bets_3[order(bets_3$ConfAcc, decreasing = TRUE),]

#Divide bets into 10 Quantiles
quants <- 10
bets_3 <- bet_quantile(bets_3, quants)
bets_4 <- bets_3[which(bets_3$quantile == quants),]

#Rank by ConfAcc
bets_4 <- bets_4[order(bets_4$ConfAcc, decreasing = TRUE),]

# Select Best Bet for Each Match
team_l <-length(unique(bets_4$HomeTeam))
bets_5 <- data.frame(rep(NULL, ncol(bets_4)))

for (i in 1:team_l){
	vec <- c()
	del_vec <- c()
	for (j in 1:nrow(bets_4)){
		if (bets_4$HomeTeam[1] == bets_4$HomeTeam[j]){
			vec <- append(vec,j)
		}
	}
	vec_df <- data.frame(vec)
	vec_df$result <- bets_4$ConfAcc[vec_df$vec]
	index <- which.max(vec_df$result)
	bets_5 <- rbind(bets_5, bets_4[index,])
	bets_4 <- bets_4[-vec,]
}

bets <- bets_5

#Order Bets by Accuracy
bets <- bets[order(bets$Accuracy),]

#Remove High Proportion Bets
bets <- bets[-which(bets$Proportion > 50),]

#Remove Low ConfAcc Bets
bets <- bets[-which(bets$ConfAcc < 0.5),]

### Accumulator Generation ###

#Generate Combinations based on Number of Bets
combinations <- list()
for (i in 1: nrow(bets)){
	combinations[i] <- list(combn(c(1:nrow(bets)), i))
}

#Generate Empty Data Structures to hold the Combined Returns and Combined Accuracy of Each Combination
returns <- gen_returns_str(nrow(bets))
accuracys <- gen_returns_str(nrow(bets))

#Iterate through Combinations and Calculate Returns and Accuracy
for (i in 1:(length(combinations))){
	for (j in 1:ncol(combinations[[i]])){
		ret_prod <- 1
		acc_prod <- 1
		for (k in 1:length(combinations[[i]][,j])){
			ret_prod <- ret_prod * bets$Returns[combinations[[i]][,j][k]]
			acc_prod <- acc_prod * bets$Accuracy[combinations[[i]][,j][k]]
		}
		returns[[i]][j] <- ret_prod
		accuracys[[i]][j] <- acc_prod
	}
}

#Choose Accumulator Min Accuracy Thresholds
min_accuracy <- c(0.85,0.8,0.775,0.75,0.7,0.6,0.5,0.4,0.3)
max_return <- c(rep(0, length(min_accuracy)))

#Create Index DataFrame
index <- data.frame(matrix(0,nrow = length(max_return), ncol = 2))
accum_accuracy <- c()

#Tools to Ensure each Bet is only included in accumulators a number of times
used_index <- c(NULL)
count_index <- c(rep(0,nrow(bets)))
max_uses <- 1

for (k in 1:length(max_return)){	
	for (i in 1:(length(returns))){
		for (j in 1:length(returns[[i]])){
			overlap = 0
			p_count_index <- count_index[combinations[[i]][,j]]
			for (m in 1:length(p_count_index)){
				if (p_count_index[m] >= max_uses) {
					overlap = 1
				}
			}
			if (returns[[i]][j] > max_return[k] & accuracys[[i]][j] > min_accuracy[k] & overlap != 1){
				index[k,1] <- i
				index[k,2] <- j
				max_return[k] <- returns[[i]][j]
				accum_accuracy[k] <- accuracys[[i]][j]
			}
		}
	}
	if (index[k,1] > 0){
	count_index[combinations[[index[k,1]]][,index[k,2]]] <- count_index[combinations[[index[k,1]]][,index[k,2]]] + 1
	}
}

#Print Accumulators
accumulators <- gen_accum_str(nrow(index))
accum_indexs <- c()

for (i in 1:nrow(index)){
	if (index[i,1] == 0){
		next
	} else {
	accum_indexs <- combinations[[index[i,1]]][,index[i,2]]
	accumulators[[i]][[1]] <- data.frame(max_return[i],accum_accuracy[i])
	names(accumulators[[i]][[1]]) <- c("Total Returns", "Total Accuracy")
	accumulators[[i]][[2]] <- bets[combinations[[index[i,1]]][,index[i,2]],c(1,2,4,6,8,7,9,10)]
	}
}

print(accumulators)


#Tidy Accumulators: remove Empty Ones
non_empty <- c(NULL)

#Iterature through Accumulators & record non-empty ones
for (i in 1:length(accumulators)){
	if (!is.null(accumulators[[i]][[1]][1,1])){
		non_empty <- append(non_empty, i)
	}
}

#Tidy Accumulator Structure
accumulators_t <- gen_accum_str(length(non_empty))

#Pull Non-empty Accumulators into Structure
for (i in 1:length(non_empty)){
	accumulators_t[[i]] <- accumulators[[non_empty[i]]]
}

### Combination of Accumulator Generation ###

#Generate Accumulator Combinations
accum_combinations <- list()
for (i in 1:length(accumulators_t)){
	accum_combinations[i] <- list(combn(c(1:length(accumulators_t)), i))
}

#Generate Accumulator Combination Expected Return Structure
expected_returns <- gen_returns_str(length(accumulators_t))

#Iterate Through Combinations and Calculate Expected Return
for (i in 1:(length(accum_combinations))){
	for (j in 1:ncol(accum_combinations[[i]])){
		all_bets <- data.frame(rep(NULL,ncol(bets)))
		rets <- 0
		for (k in 1:length(accum_combinations[[i]][,j])){
			rets <- rets + accumulators_t[[accum_combinations[[i]][k,j]]][[1]][1,1]
			all_bets <- rbind(all_bets, accumulators_t[[accum_combinations[[i]][k,j]]][[2]])
		}
		uniq_bets <- unique(all_bets)
		acc <- prod(uniq_bets$Accuracy)
		expected_returns[[i]][j] <- exp_value(rets, acc)
	}	
}

#Iterate through Expected Returns and Pull Maximum Expected Return
target_exp_return <- 0

for (i in 1:(length(expected_returns))){
	for (j in 1:length(expected_returns[[i]])){
		if (expected_returns[[i]][j] > target_exp_return){
			i_ind <- i
			j_ind <- j
			target_exp_return <- expected_returns[[i]][j]
		}
	}
}

#Pull Optimum Accumulator
opt_accum_index <- accum_combinations[[i_ind]][,j_ind]
opt_accum <- gen_accum_str(length(opt_accum_index))

for (i in 1:length(opt_accum_index)){
	opt_accum[[i]] <- accumulators_t[[opt_accum_index[i]]]
}

### Expected Value Calculation ###
opt_co_effs <- c()
prob_error <- c()
opt_returns <- c()

for (i in 1:length(opt_accum)){
	opt_co_effs[i] <- (opt_accum[[i]][[1]][1,1] + 1) * opt_accum[[i]][[1]][1,2] - 1
	prob_error[i] <- 1 - opt_accum[[i]][[1]][1,2]
	opt_returns[i] <- opt_accum[[i]][[1]][1,1]
}

splt <- c()

for (i in 1:length(opt_accum)){
	splt[[i]] = 1/length(opt_accum)
}

s_exp_value <- sum(opt_returns * splt)

opt_weights <- list(s_exp_value,splt)

for (i in 1:length(opt_accum)){
	opt_accum[[i]][[1]]$Weight <- opt_weights[[2]][i]
}

opt_accum_expected_returns <- c()

for (i in 1:length(opt_accum)){
	opt_accum_expected_returns[i] <- opt_accum[[i]][[1]][1,3] * ( (opt_accum[[i]][[1]][1,1] + 1) * opt_accum[[i]][[1]][1,2] - 1)
}

total_expected_returns <- sum(opt_accum_expected_returns)

#Print Optimum Accumulator
print(opt_accum)

s_rets <- opt_returns
s_weights <- opt_weights[[2]]
s_ret_weig <- s_rets * s_weights

s_combs <- expand.grid(rep(list(1:2), length(opt_accum)))

test <- c()

for (i in 1:nrow(s_combs)){
	sign_vec <- c()
	neg_weight_vec <- c()
	for (j in 1:ncol(s_combs)){
		if (s_combs[i,j] == 1){
			sign_vec <- append(sign_vec, 1)
			neg_weight_vec <- append(neg_weight_vec,0)
		} else if (s_combs[i,j] == 2){
			sign_vec <- append(sign_vec, 0)
			neg_weight_vec <- append(neg_weight_vec,1)
		}
	}
	s_combs$Return[i] <- (2/3)*sum(sign_vec * s_ret_weig ) + 1/3
}

### Print Accumulator Combination Expected Returns
print(s_combs)

s_exp_return <- c(rep(1, (nrow(s_combs))))

for (i in 1:nrow(s_combs)){
	for (j in 1:ncol(s_combs)){
		if (s_combs[i,j] == 1){
			s_exp_return[i] = s_exp_return[i] * opt_accum[[j]][[1]][[2]]
		} else if (s_combs[i,j] == 2){
			s_exp_return[i] = s_exp_return[i] * (1 - opt_accum[[j]][[1]][[2]])
		} else if (names(s_combs)[j] == "Return"){
			s_exp_return[i] = s_exp_return[i] * s_combs[i,j]
		}
 	}
}

### Print Total Expected Returns
print(sum(s_exp_return))

