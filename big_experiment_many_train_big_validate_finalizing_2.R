##############
##################
######################
######################### Modelling with GBM
######################
##################
##############

##### 

rm(list=ls());gc()

setwd('C:/R_study/fx/big_experiment/')

### loading working data
load(file = 'Data/many_train_samples.R')
load(file = 'Data/big_dat_test.R')

## load best inputs by target
top_inputs_targets <- read.table(file = 'Large_Study_3part/top_inputs_targets_laplace.csv'
	    , sep = ';'
	    , dec = ','
	   , header = T)

library(caret)
library(gbm)
library(doParallel)
library(tseries)


# trading loss function
my_metrics <- function (data,
			lev = NULL,
			model = NULL) {
	
	require(tseries)
	
	cross_val_trade_df <- as.data.frame(data$pred)
	cross_val_trade_df$observations <- data$obs
	
	if(sum(is.na(data$pred) == T) == 0){
		
		quant <- 0.9
		spread <- 0.0001
		
		gray_zone_thresholds <- c(
			quantile(as.numeric(data$pred), 0.5 - quant / 2)
			, quantile(as.numeric(data$pred), 0.5 + quant / 2))
		
		cross_val_trade_df$directions <- 0
		cross_val_trade_df$directions [cross_val_trade_df[, 1] > gray_zone_thresholds[2]] <- 1
		cross_val_trade_df$directions [cross_val_trade_df[, 1] < gray_zone_thresholds[1]] <- -1
		cross_val_trade_df$trades <- cross_val_trade_df[, 2] * cross_val_trade_df[, 3]
		cross_val_trade_trades <- subset(cross_val_trade_df, cross_val_trade_df$directions != 0)$trades
		
		if (length(cross_val_trade_trades) > 0){
		
			sum_trade <- sum(cross_val_trade_trades)
			sum_trade_spreaded <- sum(cross_val_trade_trades - spread)
			mean_trade <- mean(cross_val_trade_trades)
			mean_trade_spreaded <- mean(cross_val_trade_trades - spread)
			r_sqr <- 1 - sum((data$obs - data$pred) ^ 2) / sum((data$obs - mean(data$obs)) ^ 2)
			mae_mean <- 1 - sum(abs(data$obs - data$pred)) / sum(abs(data$obs - mean(data$obs)))
			pro_draw <- sum_trade_spreaded - maxdrawdown(cumsum(cross_val_trade_trades - spread))[[1]]
			trade_count_cv <- length(cross_val_trade_trades) / length(data$obs)
			
		} else {
			
			sum_trade <- 0
			sum_trade_spreaded <- 0
			mean_trade <- 0
			mean_trade_spreaded <- 0
			r_sqr <- 0
			mae_mean <- 0
			pro_draw <- 0
			trade_count_cv <- 0
			
		}
		
	} else {
		
		sum_trade <- 0
		sum_trade_spreaded <- 0
		mean_trade <- 0
		mean_trade_spreaded <- 0
		r_sqr <- 0
		mae_mean <- 0
		pro_draw <- 0
		trade_count_cv <- 0
		
	}
	
	out <- c(sum_trade
		 , sum_trade_spreaded
		 , mean_trade
		 , mean_trade_spreaded
		 , r_sqr
		 , mae_mean
		 , pro_draw
		 , trade_count_cv)
	
	names(out) <- c('sum_trade'
			, 'sum_trade_spreaded'
			, 'mean_trade'
			, 'mean_trade_spreaded'
			, 'r_sqr'
			, 'mae_mean'
			, 'pro_draw'
			, 'trade_count_cv')
	out
}


# tuning GBM
gbmGrid <- expand.grid(interaction.depth = seq(from = 1, to = 3, by = 1)
                       , n.trees = seq(from = 100, to = 300, by = 50)
                       , shrinkage = seq(from = 0.005, to = 0.015, by = 0.005)
                       , n.minobsinnode = seq(from = 30, to = 90, by = 30))

# define variables
inputs <- c(names(many_train_samples[[1]])[1:114]) #'symbol', 'hour', 'lag_diff_724', 'lag_diff_512', 'lag_diff_64', 'lag_range_724', 'lag_range_512', 'lag_sd_256', 'lag_mean_diff_128', 'lag_min_diff_23', 'lag_min_diff_11') #
outputs <- names(many_train_samples[[1]])[115:132]


# start training cycle

symbol_set <- as.character(unique(as.data.frame(table(many_train_samples[[1]]$symbol))[, 1]))

spreads <- as.data.frame(cbind(
				c('audusd'
				, 'eurusd'
				, 'gbpusd'
				, 'usdcad'
				, 'usdchf')
				, c(0.00018
				, 0.0001
				, 0.00014
				, 0.00013
				, 0.00012)))

spreads$V1 <- as.character(spreads$V1)
spreads$V2 <- as.numeric(as.character(spreads$V2))
optimized_metric <- 'mae_mean'
validating_arr <- data.frame()
distributions <- c('gaussian', 'laplace')
used_distribution <- 1
max_best_models <- 1
max_best_input_number <- 6
gray_zone <- 0.9
max_cv_folds <- 7
max_bag_fraction <- 0.5
gbm_model_list <- list()
counter <- 1

start <- Sys.time()

for (train_subset in 1:length(many_train_samples)){
	
	if(train_subset%%10 == 1){
		gbm_model_list <- list()
	}
	
	for (symbol in 1:length(symbol_set)){
	
		for (targets in 13:18){
			
			for (best_inp_num in seq(from = 6, to = max_best_input_number, by = 1)){
				
				for (cv_fold_num in seq(from = 7, to = max_cv_folds, by = 1)){
					
					for (bag_frac in seq(from = 0.5, to = max_bag_fraction, by = 0.1)){
			
						train_set <- many_train_samples[[train_subset]][, c(eval(inputs), eval(outputs[targets]))]
						train_set$symbol <- as.character(train_set$symbol)
						train_set <- train_set[train_set['symbol'] == symbol_set[symbol], ]
						
						rownames(train_set) <- NULL
						
						symbols <- length(unique(train_set$symbol))
						cv_folds <- cv_fold_num
						
						hold_in_indexes <- list()
						hold_out_indexes <- list()
						
						for (i in 1:cv_folds){
							hold_out_one <- integer()
							for (j in 1:symbols){
								
								hold_out_one <- append(hold_out_one, round(seq(from = nrow(train_set) / 5 * (j - 1) + nrow(train_set) / (cv_folds * symbols) * (i - 1) + 1
													       , to = nrow(train_set) / 5 * (j - 1) + nrow(train_set) / (cv_folds * symbols) * i
													       , by = 1)))
							}
							hold_in_indexes[[i]] <- as.integer(hold_out_one)
							hold_out_indexes[[i]] <- as.integer(rownames(train_set)[!rownames(train_set) %in% hold_in_indexes[[i]]])
						}
						
						train_set$symbol <- NULL
						
						#input_eval <- gbm(train_set[, ncol(train_set)] ~ .
						#    , data = train_set[, 1:(ncol(train_set) - 1)]
						#    , distribution = distributions[used_distribution]
						#    , n.trees = 100
						#    , interaction.depth = best_inp_num
						#    , n.minobsinnode = 100
						#    , bag.fraction = bag_frac
						#    , shrinkage = 0.01
						#    , verbose = T
						#    , n.cores = 4)
						
						#best_inputs <- as.character(summary(input_eval)[[1]][1:best_inp_num])
						
						best_inputs <- as.character(subset(top_inputs_targets, top_inputs_targets$target2 == as.integer(unlist(strsplit(x = outputs[targets], split = '_', fixed = T))[3]))[, 'value'])
						
						print(best_inputs)
						
						train_set <- train_set[, c(eval(best_inputs), eval(outputs[targets]))]
						
						# prepare training scheme
						gbmControl <- trainControl(method = 'cv'
									   , index = hold_in_indexes
									   #, indexOut = hold_out_indexes
									   , verboseIter = F
									   , returnData = F
									   , returnResamp = 'all'
									   , savePredictions = 'none'
									   , summaryFunction = my_metrics
									   , search = 'grid'
									   , allowParallel = T)
						
						# initiate paralleling
						cores <- 4
						cl <- makePSOCKcluster(cores)
						registerDoParallel(cl)
						
						# train the model
						best_models <- train(train_set[, ncol(train_set)] ~ .
						       , data = train_set[, 1:(ncol(train_set) - 1)]
						       , method = 'gbm'
						       , distribution = distributions[used_distribution]
						       , bag.fraction = bag_frac
						       , metric = optimized_metric
						       , maximize = T
						       , trControl = gbmControl
						       , tuneGrid = gbmGrid)
						
						# stop paralleling
						stopCluster(cl)
						
						# summarize the model
						best_models_arr <- as.data.frame(cbind(best_models[[4]][1]
										, best_models[[4]][2]
										, best_models[[4]][3]
										, best_models[[4]][4]
										, best_models[[4]][5]
										, best_models[[4]][6]
										, best_models[[4]][7]
										, best_models[[4]][8]
										, best_models[[4]][9]
										, best_models[[4]][10]
										, best_models[[4]][11]
										, best_models[[4]][12]))
						
						rm(best_models)
						gc()
						
						best_models_arr_ordered <- best_models_arr[order(best_models_arr[, eval(optimized_metric)], decreasing = T), ]
						
						# train best models
						
						for (i in 1:max_best_models){
						
							#gbm_obj <- best_models$finalModel
							
							gbm_obj <- gbm(train_set[, ncol(train_set)] ~ .
								       , data = train_set[, 1:(ncol(train_set) - 1)]
								       , distribution = distributions[used_distribution]
								       , n.trees = best_models_arr_ordered$n.trees[i]
								       , interaction.depth = best_models_arr_ordered$interaction.depth[i]
								       , n.minobsinnode = best_models_arr_ordered$n.minobsinnode[i]
								       , bag.fraction = bag_frac
								       , shrinkage = best_models_arr_ordered$shrinkage[i]
								       , verbose = T
								       , n.cores = 4)
							
							gbm_model_list[[length(gbm_model_list) + 1]] <- gbm_obj
							
							gbm_predict_train <- predict(object = gbm_obj
							         , newdata = train_set[, 1:(ncol(train_set) - 1)]
							         , n.trees = gbm_obj$n.trees)
							
							train_trade_df <- as.data.frame(gbm_predict_train)
							train_trade_df$observations <- train_set[, ncol(train_set)]
							
							for (jj in seq(from = 0.9, to = gray_zone, by = 0.1)){
							
								gray_zone_thresholds <- c(
									quantile(gbm_predict_train, 0.5 - jj / 2)
									, quantile(gbm_predict_train, 0.5 + jj / 2))
								
								train_trade_df$directions <- 0
								train_trade_df$directions [train_trade_df[, 1] > gray_zone_thresholds[2]] <- 1
								train_trade_df$directions [train_trade_df[, 1] < gray_zone_thresholds[1]] <- -1
								
								train_trade_df$trades <- train_trade_df[, 2] * train_trade_df[, 3]
								
								train_trades <- subset(train_trade_df, train_trade_df$directions != 0)$trades
								
								if (length(train_trades) > 0){
								
									sum_trade_train <- sum(train_trades)
									sum_trade_spreaded_train <- sum(train_trades - spreads[spreads[1] == symbol_set[symbol], 2])
									mean_trade_train <- mean(train_trades)
									mean_trade_spreaded_train <- mean(train_trades - spreads[spreads[1] == symbol_set[symbol], 2])
									pro_draw_train <- sum(train_trades - spreads[spreads[1] == symbol_set[symbol], 2]) - maxdrawdown(cumsum(train_trades - spreads[spreads[1] == symbol_set[symbol], 2]))[[1]]
									trade_count_train <- length(train_trades)
									r_sqr_train <- 1 - sum((train_trade_df[, 2] - train_trade_df[, 1]) ^ 2) / sum((train_trade_df[, 2] - mean(train_trade_df[, 2])) ^ 2)
									mae_mean_train <- 1 - sum(abs(train_trade_df[, 2] - train_trade_df[, 1])) / sum(abs(train_trade_df[, 2] - mean(train_trade_df[, 2])))
								
								} else {
									
									sum_trade_train <- NA
									sum_trade_spreaded_train <- NA
									mean_trade_train <- NA
									mean_trade_spreaded_train <- NA
									pro_draw_train <- NA
									trade_count_train <- NA
									r_sqr_train <- NA
									mae_mean_train <- NA
									
								}
								
								### validate best models
								
								big_dat_test$symbol <- as.character(big_dat_test$symbol)
								validate_set <- big_dat_test[big_dat_test['symbol'] == symbol_set[symbol], c(eval(best_inputs), eval(outputs[targets]))]
								
								rownames(validate_set) <- NULL
								indexes <- sample(nrow(validate_set) / 2, nrow(validate_set) / 10, replace = F)
								indexes <- sort(indexes, decreasing = F)
								validate_set <- validate_set[indexes, ]
								
								gbm_predict_validate <- predict(object = gbm_obj
								      			, newdata = validate_set[, 1:(ncol(validate_set) - 1)]
								      			, n.trees = gbm_obj$n.trees)
								
								validate_trade_df <- as.data.frame(gbm_predict_validate)
								validate_trade_df$observations <- validate_set[, ncol(validate_set)]
								
								validate_trade_df$directions <- 0
								validate_trade_df$directions [validate_trade_df[, 1] > gray_zone_thresholds[2]] <- 1
								validate_trade_df$directions [validate_trade_df[, 1] < gray_zone_thresholds[1]] <- -1
								
								validate_trade_df$trades <- validate_trade_df[, 2] * validate_trade_df[, 3]
								
								validate_trades <- subset(validate_trade_df, validate_trade_df$directions != 0)$trades
								
								if (length(validate_trades) > 0){
								
									sum_trade_validate <- sum(validate_trades)
									sum_trade_spreaded_validate <- sum(validate_trades - spreads[spreads[1] == symbol_set[symbol], 2])
									mean_trade_validate <- mean(validate_trades)
									mean_trade_spreaded_validate <- mean(validate_trades - spreads[spreads[1] == symbol_set[symbol], 2])
									pro_draw_validate <- sum(validate_trades - spreads[spreads[1] == symbol_set[symbol], 2]) - maxdrawdown(cumsum(validate_trades - spreads[spreads[1] == symbol_set[symbol], 2]))[[1]]
									trade_count_validate <- length(validate_trades)
									r_sqr_validate <- 1 - sum((validate_trade_df[, 2] - validate_trade_df[, 1]) ^ 2) / sum((validate_trade_df[, 2] - mean(validate_trade_df[, 2])) ^ 2)
									mae_mean_validate <- 1 - sum(abs(validate_trade_df[, 2] - validate_trade_df[, 1])) / sum(abs(validate_trade_df[, 2] - mean(validate_trade_df[, 2])))
									
									
								} else {
									
									sum_trade_validate <- NA
									sum_trade_spreaded_validate <- NA
									mean_trade_validate <- NA
									mean_trade_spreaded_validate <- NA
									pro_draw_validate <- NA
									trade_count_validate <- NA
									r_sqr_validate <- NA
									mae_mean_validate <- NA
									
								}
								
								### write data
								
								validating_arr[counter, 1] <- 'GBM'
								validating_arr[counter, 2] <- outputs[targets]
								validating_arr[counter, 3] <- distributions[used_distribution]
								validating_arr[counter, 4] <- optimized_metric
								validating_arr[counter, 5] <- symbol_set[symbol]
								validating_arr[counter, 6] <- paste(best_models_arr_ordered[1, 1]
								                          , best_models_arr_ordered[1, 2]
								                          , best_models_arr_ordered[1, 3]
								                          , best_models_arr_ordered[1, 4]
								                          , sep = '_|_')
								validating_arr[counter, 7] <- paste(best_inputs
										      	 , collapse = '_|_')
								validating_arr[counter, 8] <- best_inp_num
								validating_arr[counter, 9] <- cv_fold_num
								validating_arr[counter, 10] <- bag_frac
								validating_arr[counter, 11] <- jj
								
								validating_arr[counter, 12] <- sum_trade_train
								validating_arr[counter, 13] <- sum_trade_spreaded_train
								validating_arr[counter, 14] <- mean_trade_train
								validating_arr[counter, 15] <- mean_trade_spreaded_train
								validating_arr[counter, 16] <- pro_draw_train
								validating_arr[counter, 17] <- r_sqr_train
								validating_arr[counter, 18] <- mae_mean_train
								validating_arr[counter, 19] <- trade_count_train / nrow(train_set)
								
								validating_arr[counter, 20] <- best_models_arr_ordered[1, 5]
								validating_arr[counter, 21] <- best_models_arr_ordered[1, 6]
								validating_arr[counter, 22] <- best_models_arr_ordered[1, 7]
								validating_arr[counter, 23] <- best_models_arr_ordered[1, 8]
								validating_arr[counter, 24] <- best_models_arr_ordered[1, 9]
								validating_arr[counter, 25] <- best_models_arr_ordered[1, 10]
								validating_arr[counter, 26] <- best_models_arr_ordered[1, 11]
								validating_arr[counter, 27] <- best_models_arr_ordered[1, 12]
								
								validating_arr[counter, 28] <- sum_trade_validate
								validating_arr[counter, 29] <- sum_trade_spreaded_validate
								validating_arr[counter, 30] <- mean_trade_validate
								validating_arr[counter, 31] <- mean_trade_spreaded_validate
								validating_arr[counter, 32] <- pro_draw_validate
								validating_arr[counter, 33] <- r_sqr_validate
								validating_arr[counter, 34] <- mae_mean_validate
								validating_arr[counter, 35] <- trade_count_validate
								
								validating_arr[counter, 36] <- train_subset
								
								counter <- counter + 1
								
								colnames(validating_arr) <- c(
												'method'
												, 'target'
												, 'loss_function'
												, 'eval_function'
												, 'symbol'
												, 'model_params'
												, 'best_inputs'
												, 'best_inp_numb'
												, 'cv_fold_numb'
												, 'bag_frac'
												, 'gray_zone'
												
												, "trade_sum_train"
												, "trade_sum_spreaded_train"
												, "trade_mean_train"
												, "trade_mean_spreaded_train"
												, 'pro_draw_train'
												, 'r_sqr_train'
												, 'mae_mean_train'
												, "trade_count_train"
												
												, "trade_sum_cv"
												, "trade_sum_spreaded_cv"
												, "trade_mean_cv"
												, "trade_mean_spreaded_cv"
												, 'r_sqr_cv'
												, 'mae_mean_cv'
												, 'pro_draw_cv'
												, 'trade_count_cv'
												
												, "trade_sum_validate"
												, "trade_sum_spreaded_validate"
												, "trade_mean_validate"
												, "trade_mean_spreaded_validate"
												, 'pro_draw_validate'
												, 'r_sqr_validate'
												, 'mae_mean_validate'
												, "trade_count_validate"
												
												, "train_subset"
												)
								
							}
							
						}
						
						print(paste(Sys.time() - start, ' progress: ', round(counter / 9000, 3), sep = ''))
						print(tail(validating_arr, 1))
						
					}
					
				}
				
			}	
			
		}
		
		
	}
	
	write.table(validating_arr, file = 'Large_Study_5part/gbm_gauss_all_symbols.csv'
		    , sep = ';'
		    , dec = ','
		    , row.names = F)
	
	if(train_subset%%10 == 0){
		save(gbm_model_list, file = paste0('Large_Study_5part/gbm_model_list_laplace_all_symbols_', train_subset%/%10, '.R'))
		rm(gbm_model_list)
		gc()
	}
	
}

save(gbm_model_list, file = paste0('Large_Study_5part/gbm_model_list_laplace_all_symbols_', train_subset%/%10+1, '.R'))


##### 








### build trading charts and detaild stats for validation
#########

setwd('C:/R_study/fx/big_experiment/')

library(data.table)

load('Large_Study_3part/gbm_model_list_laplace_all_symbols_05.R')

all_models_gbm_list <- gbm_model_list[1:1710]

all_models_gbm_list <- c(all_models_gbm_list, gbm_model_list[1:1440])

all_models_gbm_list <- c(all_models_gbm_list, gbm_model_list[1:1710])

all_models_gbm_list <- c(all_models_gbm_list, gbm_model_list[1:1710])

all_models_gbm_list <- c(all_models_gbm_list, gbm_model_list[1:1710])

all_models_gbm_list <- c(all_models_gbm_list, gbm_model_list[1:630])

save(all_models_gbm_list, file = 'Large_Study_3part/all_models_gbm_list.R')


r1 <- read.table('Large_Study_3part/gbm_laplace_all_symbols_00.csv'
		, header = T
		, sep = ';'
		, dec = ',')
r2 <- read.table('Large_Study_3part/gbm_laplace_all_symbols_01.csv'
		 , header = T
		 , sep = ';'
		 , dec = ',')
r3 <- read.table('Large_Study_3part/gbm_laplace_all_symbols_02.csv'
		 , header = T
		 , sep = ';'
		 , dec = ',')
r4 <- read.table('Large_Study_3part/gbm_laplace_all_symbols_03.csv'
		 , header = T
		 , sep = ';'
		 , dec = ',')
r5 <- read.table('Large_Study_3part/gbm_laplace_all_symbols_04.csv'
		 , header = T
		 , sep = ';'
		 , dec = ',')
r6 <- read.table('Large_Study_3part/gbm_laplace_all_symbols_05.csv'
		 , header = T
		 , sep = ';'
		 , dec = ',')

all_results_gbm_df <- rbind(r1
			, r2
			, r3
			, r4
			, r5
			, r6)

all_results_gbm_df$models <- seq(1, 8910, 1)
all_results_gbm_df <- na.omit(all_results_gbm_df)

all_results_gbm_df$target <- as.character(all_results_gbm_df$target)
all_results_gbm_df$target2 <- as.integer(gsub("future_lag_", '', all_results_gbm_df$target))

write.table(all_results_gbm_df, file = 'Large_Study_3part/all_results_gbm_df.csv'
	   , row.names = F
	   , sep = ';'
	   , dec = ',')


#### tuned predictor set results

setwd('C:/R_study/fx/big_experiment/')

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_1.R')
tuned_all_models_gbm_list <- gbm_model_list

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_2.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_3.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_4.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_5.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_6.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_7.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_8.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_9.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)

load('Large_Study_5part/gbm_model_list_laplace_all_symbols_10.R')
tuned_all_models_gbm_list <- c(tuned_all_models_gbm_list, gbm_model_list)


save(tuned_all_models_gbm_list, file = 'Large_Study_5part/tuned_all_models_gbm_list.R')


tuned_all_results_gbm_df <- validating_arr
tuned_all_results_gbm_df$models <- seq(1, 2970, 1)
tuned_all_results_gbm_df <- na.omit(tuned_all_results_gbm_df)
tuned_all_results_gbm_df$target <- as.character(tuned_all_results_gbm_df$target)
tuned_all_results_gbm_df$target2 <- as.integer(gsub("future_lag_", '', tuned_all_results_gbm_df$target))

write.table(tuned_all_results_gbm_df, file = 'Large_Study_5part/tuned_all_results_gbm_df.csv'
	    , row.names = F
	    , sep = ';'
	    , dec = ',')

########################### Charts

setwd('C:/R_study/fx/big_experiment/')

library(data.table)
library(caret)
library(gbm)
library(doParallel)
library(tseries)
library(gridExtra)

all_results_gbm_df <- read.table('Large_Study_3part/all_results_gbm_df.csv'
	    , header = T
	    , sep = ';'
	    , dec = ',')

tuned_all_results_gbm_df <- read.table('Large_Study_5part/tuned_all_results_gbm_df.csv'
				 , header = T
				 , sep = ';'
				 , dec = ',')


working_data <- as.data.table(all_results_gbm_df)
tuned_working_data <- as.data.table(tuned_all_results_gbm_df)

#### distributions of mae improve for validate

title <- "Distributions of Mean Absolute Error Decrease by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = working_data, aes(x = as.factor(target2), y = mae_mean_validate)) + 
	facet_wrap(~ symbol) +
	theme_bw() +
	geom_boxplot(color = 'black', fill = 'grey') +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### distributions of R^2 for validate

title <- "Distributions of Squared Error Decrease by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = working_data, aes(x = as.factor(target2), y = r_sqr_validate)) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot(color = 'black', fill = 'grey') +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### distributions of Expectation for validate

title <- "Distributions of Trade Size Expectation (point) After Spread by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = working_data, aes(x = as.factor(target2), y = trade_mean_spreaded_validate)) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot(color = 'black', fill = 'grey') +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### distributions of Profit Minus Drawdown for validate

title <- "Distributions of Profit Minus Drawdown (point) After Spread by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = working_data, aes(x = as.factor(target2), y = pro_draw_validate)) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot(color = 'black', fill = 'grey') +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### comparison cv VS validate for distributions of mae improve for validate

dat <- melt(working_data, id.vars = c('symbol', 'target2', 'models'), measure.vars = c("mae_mean_cv", "mae_mean_validate"))

title <- "Comparison of Distributions of Mean Absolute Error Decrease for CV and Validate Data by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = dat, aes(x = as.factor(target2), y = value, fill = as.factor(variable))) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot() +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### comparison cv VS validate for distributions of trade expectation for validate

dat <- melt(working_data, id.vars = c('symbol', 'target2', 'models'), measure.vars = c("trade_mean_spreaded_cv", "trade_mean_spreaded_validate"))

title <- "Comparison of Distributions of Trade Size Expectation (point) After Spread for CV and Validate Data by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = dat, aes(x = as.factor(target2), y = value, fill = as.factor(variable))) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot() +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### comparison of distributions of Profit Minus Drawdown for CV vs. validate

dat <- melt(working_data, id.vars = c('symbol', 'target2', 'models'), measure.vars = c("pro_draw_cv", "pro_draw_validate"))

title <- "Comparison of Distributions of Profit - Drawdown (point) for CV and Validate Data After Spread by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = dat, aes(x = as.factor(target2), y = value, fill = as.factor(variable))) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot() +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### comparison all_train VS tuned_train for distributions of mae improve for validate

compare_dat <- rbind(working_data[target2 >= 128, c('symbol', 'target2', 'models', "mae_mean_validate"), with = F]
		     , tuned_working_data[, c('symbol', 'target2', 'models', "mae_mean_validate"), with = F])
compare_dat[, iteration:= c(rep('all_predictors', times = nrow(working_data[target2 >= 128, ])), rep('tuned_predictors', times = nrow(tuned_working_data)))]

title <- "Comparison of Distributions of Mean Absolute Error Decrease for All-Predictor and Tuned-Predictor Models by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = compare_dat, aes(x = as.factor(target2), y = mae_mean_validate, fill = iteration)) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot() +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()

rm(compare_dat)


#### comparison all_train VS tuned_train for distributions of R^2 for validate

compare_dat <- rbind(working_data[target2 >= 128, c('symbol', 'target2', 'models', "r_sqr_validate"), with = F]
		     , tuned_working_data[, c('symbol', 'target2', 'models', "r_sqr_validate"), with = F])
compare_dat[, iteration:= c(rep('all_predictors', times = nrow(working_data[target2 >= 128, ])), rep('tuned_predictors', times = nrow(tuned_working_data)))]

title <- "Comparison of Distributions of R^2 for All-Predictor and Tuned-Predictor Models by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = compare_dat, aes(x = as.factor(target2), y = r_sqr_validate, fill = iteration)) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot() +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()

rm(compare_dat)


#### comparison all_train VS tuned_train for distributions of profit minus drawdown for validate

compare_dat <- rbind(working_data[target2 >= 128, c('symbol', 'target2', 'models', "pro_draw_validate"), with = F]
		     , tuned_working_data[, c('symbol', 'target2', 'models', "pro_draw_validate"), with = F])
compare_dat[, iteration:= c(rep('all_predictors', times = nrow(working_data[target2 >= 128, ])), rep('tuned_predictors', times = nrow(tuned_working_data)))]

title <- "Comparison of Distributions of Profit Minus Max Drawdown (point) for All-Predictor and Tuned-Predictor Models by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = compare_dat, aes(x = as.factor(target2), y = pro_draw_validate, fill = iteration)) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot() +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()

rm(compare_dat)


#### comparison all_train VS tuned_train for distributions of expectation for validate

compare_dat <- rbind(working_data[target2 >= 128, c('symbol', 'target2', 'models', "trade_mean_spreaded_validate"), with = F]
		     , tuned_working_data[, c('symbol', 'target2', 'models', "trade_mean_spreaded_validate"), with = F])
compare_dat[, iteration:= c(rep('all_predictors', times = nrow(working_data[target2 >= 128, ])), rep('tuned_predictors', times = nrow(tuned_working_data)))]

title <- "Comparison of Distributions of Trade Expectation (point) for All-Predictor and Tuned-Predictor Models by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = compare_dat, aes(x = as.factor(target2), y = trade_mean_spreaded_validate, fill = iteration)) + 
	facet_wrap(~ symbol, scales = "free") +
	theme_bw() +
	geom_boxplot() +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()

rm(compare_dat)


#### comparison all_train VS tuned_train for train sets of expectation for validate

compare_dat <- rbind(working_data[target2 >= 128, c('symbol', 'target2', "train_subset", "trade_mean_spreaded_validate"), with = F]
		     , tuned_working_data[, c('symbol', 'target2', "train_subset", "trade_mean_spreaded_validate"), with = F])
compare_dat[, iteration:= c(rep('all_predictors', times = nrow(working_data[target2 >= 128, ])), rep('tuned_predictors', times = nrow(tuned_working_data)))]

compare_dat[, 
	    {
	    	title <- paste0("Comparison of Distributions of Trade Expectation (point) for All-Predictor and Tuned-Predictor Models by Train Set, Symbol, for Target ", target2, ", for Validation_1 Samples")
	    	p <- ggplot(data = .SD, aes(x = as.factor(train_subset), y = trade_mean_spreaded_validate, group = iteration, colour = iteration)) + 
			facet_wrap(~ symbol) +
			theme_bw() +
			geom_line() +
	    		scale_x_discrete(breaks = c(1,11,21,31,41,51,61,71,81,91)) +
			geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
			ggtitle(title) + 
			xlab('Train Subset (one model)') +
			theme(plot.title = element_text(lineheight =.8, size = 10, face = "bold")) +
			theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
			theme(text = element_text(size = 7))
	    	print(p)
	    	
	    	ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
	    	       , plot = last_plot()
	    	       , device = 'jpeg'
	    	       , width = 300, height = 200, units = "mm"
	    	       , dpi = 300)
	    	dev.off()
	    }

, by = target2]

rm(compare_dat)


#### Correlation of Sample Statistics cv and validate - for mae improve

dat <- melt(working_data, id.vars = c('symbol', 'target2', 'models'), measure.vars = c("mae_mean_cv", "mae_mean_validate"))

cor <- dat[, 
	   {
	   	cor(.SD[variable == "mae_mean_cv", value], .SD[variable == "mae_mean_validate", value])
	   }
	   , by = .(symbol, target2)
	   ]

title <- "Correlation of Abs. Err. Decr. Sample Statistics for CV and Validate Data by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = cor, aes(x = as.factor(target2), y = V1)) + 
	facet_wrap(~ symbol) +
	theme_bw() +
	geom_bar(stat = "identity", fill = 'blue', colour = 'blue') +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	ylab("Pearson's Correlation Coefficient") +
	xlab('Target Prediction Horizon (t0 + n Minutes)') +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(axis.title.y = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### Correlation of Sample Statistics cv and validate - for trade expectation

dat <- melt(working_data, id.vars = c('symbol', 'target2', 'models'), measure.vars = c("trade_mean_spreaded_cv", "trade_mean_spreaded_validate"))

cor <- dat[, 
	   {
	   	cor(.SD[variable == "trade_mean_spreaded_cv", value], .SD[variable == "trade_mean_spreaded_validate", value])
	   }
	   
	   , by = .(symbol, target2)
	   ]

title <- "Correlation of Trade Expectation (point) Sample Statistics for CV and Validate Data by Symbol and Target for Validation_1 Samples"

p <- ggplot(data = cor, aes(x = as.factor(target2), y = V1)) + 
	facet_wrap(~ symbol) +
	theme_bw() +
	geom_bar(stat = "identity", fill = 'blue', colour = 'blue') +
	geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
	ggtitle(title) + 
	ylab("Pearson's Correlation Coefficient") +
	theme(plot.title = element_text(lineheight =.8, size = 12, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(axis.title.y = element_text(lineheight =.8, size = 10, face = "bold")) +
	theme(text = element_text(size = 7))

print(p)

ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
       , plot = last_plot()
       , device = 'jpeg'
       , width = 300, height = 200, units = "mm"
       , dpi = 300)

dev.off()


#### Scatterplot with LM (alpha 0.01) of Trade Expectation (point)

working_data[, {
	title <- paste('Scatterplot with LM of Trade Expectation (point) Sample Statistics for CV and Validate Data by Target for ', symbol, ' for Validation_1 Samples', sep = '')
	
	p <- ggplot(data = .SD, aes(x = trade_mean_spreaded_cv, y = trade_mean_spreaded_validate)) + 
		facet_wrap(~ target2, scales = 'free') +
		theme_bw() +
		geom_point(fill = 'black', colour = 'black', size = 1, alpha = 0.5) +
		geom_smooth(method = 'lm', level = 0.99, linetype = 1, size = 0.5) +
		geom_smooth(method = 'lm', level = 0.999, linetype = 1, size = 0.5) +
		geom_smooth(method = 'lm', level = 0.9999, linetype = 1, size = 0.5) +
		geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
		geom_vline(aes(xintercept = 0), linetype = 2, colour = 'red', size = 1) +
		ggtitle(title) + 
		theme(plot.title = element_text(lineheight =.8, size = 10, face = "bold")) +
		theme(text = element_text(size = 7))
	print(p)
	ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
	       , plot = last_plot()
	       , device = 'jpeg'
	       , width = 300, height = 200, units = "mm"
	       , dpi = 300)
	
	dev.off()
}
, by = symbol]


#### Scatterplot with LM (alpha 0.01) of Profit Minus Drawdown (point)

working_data[, {
	title <- paste('Scatterplot with LM (alpha 0.01) of  Profit Minus Drawdown (point) Sample Statistics for CV and Validate Data by Target for ', symbol, ' for Validation_1 Samples', sep = '')
	
	p <- ggplot(data = .SD, aes(x = pro_draw_cv, y = pro_draw_validate)) + 
		facet_wrap(~ target2, scales = 'free') +
		theme_bw() +
		geom_point(fill = 'black', colour = 'black', size = 1, alpha = 0.5) +
		geom_smooth(method = 'lm', level = 0.99, linetype = 1, size = 0.5) +
		geom_smooth(method = 'lm', level = 0.999, linetype = 1, size = 0.5) +
		geom_smooth(method = 'lm', level = 0.9999, linetype = 1, size = 0.5) +
		geom_hline(aes(yintercept = 0), linetype = 2, colour = 'red', size = 1) +
		geom_vline(aes(xintercept = 0), linetype = 2, colour = 'red', size = 1) +
		ggtitle(title) + 
		theme(plot.title = element_text(lineheight =.8, size = 11, face = "bold")) +
		theme(text = element_text(size = 7))
	print(p)
	ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
	       , plot = last_plot()
	       , device = 'jpeg'
	       , width = 300, height = 200, units = "mm"
	       , dpi = 300)
	
	dev.off()
}
, by = symbol]


#### Best n Predictors by Order of Inclusion for Target (all symbols)

count <- 1:6
working_data[, c(paste('best_inputs_', count, sep = '')) := tstrsplit(best_inputs, '_|_', fixed = T)]
dat <- melt(working_data, id.vars = 'target2', measure.vars = c('best_inputs_1', 'best_inputs_2', 'best_inputs_3', 'best_inputs_4', 'best_inputs_5', 'best_inputs_6'))
counts <- dat[, .N, by = .(target2, variable, value)]
setorder(counts, target2, variable, -N)
top_counts <- counts[, .SD[1:10], .(target2, variable)]

top_inputs_targets <- top_counts[variable == 'best_inputs_1', ]

write.table(top_inputs_targets, file = 'Large_Study_3part/top_inputs_targets_laplace.csv'
	    , sep = ';'
	    , dec = ','
	    , row.names = F)

top_counts[, 
       {
	title <- paste('Best n Predictors by Order of Inclusion for Target ', target2, sep = '')
	
	p <- ggplot(data = .SD, aes(x = value, y = N)) + 
		facet_wrap(~ variable, scales = 'free') +
		theme_bw() +
		geom_bar(stat = "identity", fill = 'blue', colour = 'blue', alpha = 0.5) +
		ggtitle(title) + 
		ylab("Frequency of Use") +
		theme(plot.title = element_text(lineheight =.8, size = 11, face = "bold")) +
		theme(text = element_text(size = 7), 
		      axis.text.x = element_text(angle = 45, vjust = 1))
	
	print(p)
	ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
	       , plot = last_plot()
	       , device = 'jpeg'
	       , width = 300, height = 200, units = "mm"
	       , dpi = 300)
	
	dev.off()
       }
, by = target2]


#### Best n Predictors on First Place for Symbols, by Targets

count <- 1:6
working_data[, c(paste('best_inputs_', count, sep = '')) := tstrsplit(best_inputs, '_|_', fixed = T)]
dat <- melt(working_data, id.vars = c('symbol', 'target2'), measure.vars = 'best_inputs_1')
counts <- dat[, .N, by = .(symbol, target2, value)]
setorder(counts, symbol, target2, -N)
top_counts <- counts[, .SD[1:10], by = .(symbol, target2)]

top_counts[, 
	   {
	   	title <- paste('Best n Predictors on First Place for Symbols, for Target ', target2, sep = '')
	   	
	   	p <- ggplot(data = .SD, aes(x = value, y = N)) + 
	   		facet_wrap(~ symbol, scales = 'free') +
	   		theme_bw() +
	   		geom_bar(stat = "identity", fill = 'blue', colour = 'blue', alpha = 0.5) +
	   		ggtitle(title) + 
	   		ylab("Frequency of Use") +
	   		theme(plot.title = element_text(lineheight =.8, size = 11, face = "bold")) +
	   		theme(text = element_text(size = 7), 
	   		      axis.text.x = element_text(angle = 45, vjust = 1))
	   	
	   	print(p)
	   	ggsave(filename = paste('analysis/', title, '.jpeg', sep = '')
	   	       , plot = last_plot()
	   	       , device = 'jpeg'
	   	       , width = 300, height = 200, units = "mm"
	   	       , dpi = 300)
	   	
	   	dev.off()
	   }
	   , by = target2]


#########








############### Trade Sequence Modelling for Best Validation_1 Models
##################

rm(list=ls());gc()

setwd('C:/R_study/fx/big_experiment/')

library(data.table)
library(caret)
library(gbm)
library(doParallel)
library(tseries)
library(gridExtra)


load(file = 'Large_Study_3part/all_models_gbm_list.R')

all_results_gbm_df <- fread('Large_Study_3part/all_results_gbm_df.csv'
				 , header = T
				 , sep = ';'
				 , dec = ',')

# tuned results

load(file = 'Large_Study_5part/tuned_all_models_gbm_list.R')

tuned_all_results_gbm_df <- fread('Large_Study_5part/tuned_all_results_gbm_df.csv'
			    , header = T
			    , sep = ';'
			    , dec = ',')


working_data <- all_results_gbm_df

tuned_working_data <- tuned_all_results_gbm_df

# load train data

load(file = 'Data/many_train_samples.R')

### define model
model_symbol <- 'eurusd'
model_target <- 181
model_target_name <- paste0('future_lag_', model_target)
model_spread <- 0.0001
nseq <- 1000
modeled_trade_number <- round(5827000/3/2/model_target/2)


model_set_00 <- working_data[symbol == model_symbol & target2 == model_target, c(5, 20:38), with = F]
setorder(model_set_00, - trade_mean_spreaded_cv)

all_dat_train <- as.data.table(do.call(rbind, many_train_samples))

rm(many_train_samples)

all_dat_train <- all_dat_train[symbol == model_symbol, ]

models <- model_set_00[, models]

all_dat_train[, (paste('model_output_', models, sep = '')):= lapply(models, function(x) as.numeric(predict(object = all_models_gbm_list[[x]]
												, newdata = .SD[, 1:114, with = F]
												, n.trees = all_models_gbm_list[[x]]$n.trees)))]

predictions <- all_dat_train[, 133:(132+length(models)), with = F]

rm(all_dat_train)

gc()

predictions[, model_avg:= rowMeans(.SD), .SDcols = c(1:length(models))]

quantile_threshold <- 0.9

gray_zone_thresholds <- c(
	quantile(predictions[, model_avg], 0.5 - quantile_threshold / 2)
	, quantile(predictions[, model_avg], 0.5 + quantile_threshold / 2))


###### validation data

load(file = 'Data/big_dat_test.R')

big_dat_test <- as.data.table(big_dat_test)
big_dat_test <- big_dat_test[symbol == model_symbol, ]

big_dat_test[, (paste('model_output_', models, sep = '')):= lapply(models, function(x) as.numeric(predict(object = all_models_gbm_list[[x]]
													  , newdata = .SD[, 1:114, with = F]
													  , n.trees = all_models_gbm_list[[x]]$n.trees)))]

rm(all_models_gbm_list)
gc()

validate_predictions <- big_dat_test[, c(model_target_name, paste0('model_output_', models)), with = F]
rm(big_dat_test)
validate_predictions[, model_avg:= rowMeans(.SD), .SDcols = c(2:(1+length(models)))]
validate_predictions[, select:= ifelse(model_avg < gray_zone_thresholds[1] | model_avg > gray_zone_thresholds[2], 1, 0)]
validate_predictions[, trades:= get(model_target_name) * sign(model_avg)]
validate_trades <- validate_predictions[select == 1, trades]
random_trades <- validate_predictions[, trades]


############ build sequence of trades

# indexes
validate_indexes <- list()
for (i in 1:nseq){
	n <- sample(round(length(validate_trades) / 2), modeled_trade_number, replace = FALSE)
	validate_indexes[[i]] <- sort(n, decreasing = F)
}

# sequences
validate_sequence <- list()
for (i in 1:nseq){
	validate_sequence[[i]] <- cumsum(validate_trades[validate_indexes[[i]]] - model_spread)
}

validate_sequence_dt <- as.data.table(do.call(c, validate_sequence))
validate_sequence_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
validate_sequence_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]

### up
validate_indexes_randoms <- list()
for (i in 1:nseq){
	n <- sample(round(length(random_trades) / 2), modeled_trade_number, replace = FALSE)
	validate_indexes_randoms[[i]] <- sort(n, decreasing = F)
}

validate_sequence_up <- list()
for (i in 1:nseq){
	validate_sequence_up[[i]] <- cumsum(random_trades[validate_indexes_randoms[[i]]] - model_spread)
}

validate_sequence_up_dt <- as.data.table(do.call(c, validate_sequence_up))
validate_sequence_up_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
validate_sequence_up_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]

### down
validate_sequence_down <- list()
for (i in 1:nseq){
	validate_sequence_down[[i]] <- cumsum(-random_trades[validate_indexes_randoms[[i]]] - model_spread)
}

validate_sequence_down_dt <- as.data.table(do.call(c, validate_sequence_down))
validate_sequence_down_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
validate_sequence_down_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]

### rand
validate_sequence_rand <- list()
for (i in 1:nseq){
	validate_sequence_rand[[i]] <- cumsum(random_trades[validate_indexes_randoms[[i]]] * ifelse(runif(1, -1, 1) > 0, 1, -1) - model_spread)
}

validate_sequence_rand_dt <- as.data.table(do.call(c, validate_sequence_rand))
validate_sequence_rand_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
validate_sequence_rand_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]

all_sequences <- rbind(validate_sequence_dt
		       , validate_sequence_up_dt
		       , validate_sequence_down_dt
		       , validate_sequence_rand_dt)

all_sequences[, model_type:= rep(c('model', 'buy-only', 'sell-only', 'random'), each = modeled_trade_number * nseq)]

median_seq <- all_sequences[, median(V1), by = .(model_type, step)]
low_quant_seq <- all_sequences[, quantile(V1, 0.01), by = .(model_type, step)]
upp_quant_seq <- all_sequences[, quantile(V1, 0.99), by = .(model_type, step)]

trade_sequence_summary <- cbind(median_seq
				, low_quant_seq[, V1]
				, upp_quant_seq[, V1])

colnames(trade_sequence_summary) <- c("model_type", "step", 'median', "low_quantile", "upp_quantile")

last_distr <- all_sequences[, V1[.N], by = .(model_type, sample)]
last_distr[, sample:= rep(c('model', 'buy-only', 'sell-only', 'random'), each = nseq)]

rf_distr <- all_sequences[, V1[.N] / maxdrawdown(V1)[[1]], by = .(model_type, sample)]
rf_distr[, sample:= rep(c('model', 'buy-only', 'sell-only', 'random'), each = nseq)]



############ plot sequences

p1 <- ggplot(data = trade_sequence_summary, aes(x = step, y = median, color = model_type)) +
	geom_ribbon(aes(ymin = low_quantile, ymax = upp_quantile, fill = model_type, alpha = 0.1)) +
	geom_line(size = 2) +
	scale_y_continuous(limits = c(-2, 2)) +
	ggtitle('Simulated Cumulative Trading Outcomes (Median, 01-Quantile, and 99-Quantile)') +
	ylab("Cumulative Points") +
	xlab("Trade Sequence") +
	theme(plot.title = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(axis.title.y = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(text = element_text(size = 12))
	      
p2 <- ggplot(data = last_distr, aes(x = V1, group = as.factor(sample), fill = as.factor(sample), color = as.factor(sample))) +
	geom_density(alpha = 0.3) + 
	coord_flip() +
	scale_x_continuous(limits = c(-2, 2)) +
	ggtitle('Cumulative Profit (Point) Density') +
	ylab("Density") +
	xlab("Cumulative Points") +
	theme(plot.title = element_text(lineheight =.8, size = 16, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(axis.title.y = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(text = element_text(size = 12))
	
p3 <- ggplot(data = rf_distr, aes(x = V1, fill = sample, color = sample)) +
	geom_density(alpha = 0.3) +
	facet_wrap(~ sample, scales = 'free_y', ncol = 1) +
	ggtitle('Recovery Factor Density') +
	ylab("Density") +
	xlab("Recovery Factor") +
	geom_vline(xintercept = median(rf_distr[sample == 'model', V1]), size = 1, linetype = 2, colour = 'red') +
	annotate('text', x = 1, y = 0.1, label = paste0("Model's Median RF = ", round(median(rf_distr[sample == 'model', V1]), 2))) +
	theme(plot.title = element_text(lineheight =.8, size = 16, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(axis.title.y = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(text = element_text(size = 12))

title <- paste0('Manifold Representation of Trade Sequence Simulation with All-Model Ensemble, for ', model_symbol, ', and target ', model_target_name)

jpeg(filename = paste('analysis/', title, '.jpeg', sep = '')
     , width = 2000, height = 800, units = "px")
	
grid.arrange(p1, p2, p3, ncol = 3)

dev.off()

############## optimize ensemble

ensembles_rf_median <- numeric()

for (ii in 1:length(models)){
	
	predictions[, ensemble_tune:= rowMeans(.SD), .SDcols = c(1:ii)]

	gray_zone_thresholds <- c(
		quantile(predictions[, ensemble_tune], 0.5 - quantile_threshold / 2)
		, quantile(predictions[, ensemble_tune], 0.5 + quantile_threshold / 2))
	
	validate_predictions[, ensemble_tune:= rowMeans(.SD), .SDcols = c(2:(ii+1))]
	validate_predictions[, select:= ifelse(ensemble_tune < gray_zone_thresholds[1] | ensemble_tune > gray_zone_thresholds[2], 1, 0)]
	validate_predictions[, trades:= get(model_target_name) * sign(ensemble_tune)]
	validate_trades <- validate_predictions[select == 1, trades]
	
	# indexes
	validate_indexes <- list()
	for (i in 1:nseq){
		n <- sample(round(length(validate_trades) / 2), modeled_trade_number, replace = FALSE)
		validate_indexes[[i]] <- sort(n, decreasing = F)
	}
	
	# sequences
	validate_sequence <- list()
	for (i in 1:nseq){
		validate_sequence[[i]] <- cumsum(validate_trades[validate_indexes[[i]]] - model_spread)
	}
	
	validate_sequence_dt <- as.data.table(do.call(c, validate_sequence))
	validate_sequence_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
	validate_sequence_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]
	
	rf_distr <- validate_sequence_dt[, V1[.N] / maxdrawdown(V1)[[1]], by = .(sample)]
	
	ensembles_rf_median[ii] <- median(rf_distr[, V1])
	
}

title = paste0('Median Recovery Factor on Tuned Ensemble for 1000 Simulated Trade Sequences for ', model_symbol, ', and target ', model_target_name)

jpeg(filename = paste('analysis/', title, '.jpeg', sep = '')
       , width = 1200, height = 800, units = "px")

plot(ensembles_rf_median, type = 's', main = title, xlab = "Number of Models Added in Descending Order Based on Cross-Validation Trade Expectation Value", ylab = "Median RF")

dev.off()


###################### find best emsemble
#################

model_number <- which.max(ensembles_rf_median)

predictions[, ensemble_tune:= rowMeans(.SD), .SDcols = c(1:model_number)]

gray_zone_thresholds <- c(
	quantile(predictions[, ensemble_tune], 0.5 - quantile_threshold / 2)
	, quantile(predictions[, ensemble_tune], 0.5 + quantile_threshold / 2))

validate_predictions[, ensemble_tune:= rowMeans(.SD), .SDcols = c(2:(model_number + 1))]
validate_predictions[, select:= ifelse(ensemble_tune < gray_zone_thresholds[1] | ensemble_tune > gray_zone_thresholds[2], 1, 0)]
validate_predictions[, trades:= get(model_target_name) * sign(ensemble_tune)]
validate_trades <- validate_predictions[select == 1, trades]
random_trades <- validate_predictions[, trades]

############ build sequence of trades

# indexes
validate_indexes <- list()
for (i in 1:nseq){
	n <- sample(round(length(validate_trades) / 2), modeled_trade_number, replace = FALSE)
	validate_indexes[[i]] <- sort(n, decreasing = F)
}

# sequences
validate_sequence <- list()
for (i in 1:nseq){
	validate_sequence[[i]] <- cumsum(validate_trades[validate_indexes[[i]]] - model_spread)
}

validate_sequence_dt <- as.data.table(do.call(c, validate_sequence))
validate_sequence_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
validate_sequence_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]

### up
validate_indexes_randoms <- list()
for (i in 1:nseq){
	n <- sample(round(length(random_trades) / 2), modeled_trade_number, replace = FALSE)
	validate_indexes_randoms[[i]] <- sort(n, decreasing = F)
}

validate_sequence_up <- list()
for (i in 1:nseq){
	validate_sequence_up[[i]] <- cumsum(random_trades[validate_indexes_randoms[[i]]] - model_spread)
}

validate_sequence_up_dt <- as.data.table(do.call(c, validate_sequence_up))
validate_sequence_up_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
validate_sequence_up_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]

### down
validate_sequence_down <- list()
for (i in 1:nseq){
	validate_sequence_down[[i]] <- cumsum(-random_trades[validate_indexes_randoms[[i]]] - model_spread)
}

validate_sequence_down_dt <- as.data.table(do.call(c, validate_sequence_down))
validate_sequence_down_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
validate_sequence_down_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]

### rand
validate_sequence_rand <- list()
for (i in 1:nseq){
	validate_sequence_rand[[i]] <- cumsum(random_trades[validate_indexes_randoms[[i]]] * ifelse(runif(1, -1, 1) > 0, 1, -1) - model_spread)
}

validate_sequence_rand_dt <- as.data.table(do.call(c, validate_sequence_rand))
validate_sequence_rand_dt[, step:= rep(1:modeled_trade_number, times = nseq)]
validate_sequence_rand_dt[, sample:= rep(1:nseq, each = modeled_trade_number)]

all_sequences <- rbind(validate_sequence_dt
		       , validate_sequence_up_dt
		       , validate_sequence_down_dt
		       , validate_sequence_rand_dt)

all_sequences[, model_type:= rep(c('model', 'buy-only', 'sell-only', 'random'), each = modeled_trade_number * nseq)]

median_seq <- all_sequences[, median(V1), by = .(model_type, step)]
low_quant_seq <- all_sequences[, quantile(V1, 0.01), by = .(model_type, step)]
upp_quant_seq <- all_sequences[, quantile(V1, 0.99), by = .(model_type, step)]

trade_sequence_summary <- cbind(median_seq
				, low_quant_seq[, V1]
				, upp_quant_seq[, V1])

colnames(trade_sequence_summary) <- c("model_type", "step", 'median', "low_quantile", "upp_quantile")

last_distr <- all_sequences[, V1[.N], by = .(model_type, sample)]
last_distr[, sample:= rep(c('model', 'buy-only', 'sell-only', 'random'), each = nseq)]

rf_distr <- all_sequences[, V1[.N] / maxdrawdown(V1)[[1]], by = .(model_type, sample)]
rf_distr[, sample:= rep(c('model', 'buy-only', 'sell-only', 'random'), each = nseq)]



############ plot sequences

p1 <- ggplot(data = trade_sequence_summary, aes(x = step, y = median, color = model_type)) +
	geom_ribbon(aes(ymin = low_quantile, ymax = upp_quantile, fill = model_type, alpha = 0.1)) +
	geom_line(size = 2) +
	scale_y_continuous(limits = c(-2, 2)) +
	ggtitle('Simulated Cumulative Trading Outcomes (Median, 01-Quantile, and 99-Quantile)') +
	ylab("Cumulative Points") +
	xlab("Trade Sequence") +
	theme(plot.title = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(axis.title.y = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(text = element_text(size = 12))

p2 <- ggplot(data = last_distr, aes(x = V1, group = as.factor(sample), fill = as.factor(sample), color = as.factor(sample))) +
	geom_density(alpha = 0.3) + 
	coord_flip() +
	scale_x_continuous(limits = c(-2, 2)) +
	ggtitle('Cumulative Profit (Point) Density') +
	ylab("Density") +
	xlab("Cumulative Points") +
	theme(plot.title = element_text(lineheight =.8, size = 16, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(axis.title.y = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(text = element_text(size = 12))

p3 <- ggplot(data = rf_distr, aes(x = V1, fill = sample, color = sample)) +
	geom_density(alpha = 0.3) +
	facet_wrap(~ sample, scales = 'free_y', ncol = 1) +
	ggtitle('Recovery Factor Density') +
	ylab("Density") +
	xlab("Recovery Factor") +
	geom_vline(xintercept = median(rf_distr[sample == 'model', V1]), size = 1, linetype = 2, colour = 'red') +
	annotate('text', x = 1, y = 0.1, label = paste0("Model's Median RF = ", round(median(rf_distr[sample == 'model', V1]), 2))) +
	theme(plot.title = element_text(lineheight =.8, size = 16, face = "bold")) +
	theme(axis.title.x = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(axis.title.y = element_text(lineheight =.8, size = 14, face = "bold")) +
	theme(text = element_text(size = 12))

title <- paste0('Manifold Representation of Trade Sequence Simulation with Tuned Model Ensemble, for ', model_symbol, ', and target ', model_target_name)

jpeg(filename = paste('analysis/', title, '.jpeg', sep = '')
     , width = 2000, height = 800, units = "px")

plot(grid.arrange(p1, p2, p3, ncol = 3))

dev.off()

