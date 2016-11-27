
setwd('C:/R_study/fx/big_experiment/Data/')
options(scipen=999)


### getting data
{
dat_eurusd <- read.table('EURUSD1.csv', header = T, sep = ',', dec = '.')
dat_audusd <- read.table('AUDUSD1.csv', header = T, sep = ',', dec = '.')
dat_gbpusd <- read.table('GBPUSD1.csv', header = T, sep = ',', dec = '.')
dat_usdcad <- read.table('USDCAD1.csv', header = T, sep = ',', dec = '.')
dat_usdchf <- read.table('USDCHF1.csv', header = T, sep = ',', dec = '.')
}

### selecting train
{
dat_train <- as.data.frame(c(head(dat_audusd, nrow(dat_audusd) / 3 * 2)[, 3]
		     , head(dat_eurusd, nrow(dat_eurusd) / 3 * 2)[, 3]
		     , head(dat_gbpusd, nrow(dat_gbpusd) / 3 * 2)[, 3]
		     , head(dat_usdcad, nrow(dat_usdcad) / 3 * 2)[, 3]
		     , head(dat_usdchf, nrow(dat_usdchf) / 3 * 2)[, 3]))
dat_train$date <- c(as.Date(head(dat_audusd, nrow(dat_audusd) / 3 * 2)[, 1], format = "%Y.%m.%d")
		    , as.Date(head(dat_eurusd, nrow(dat_eurusd) / 3 * 2)[, 1], format = "%Y.%m.%d")
		    , as.Date(head(dat_gbpusd, nrow(dat_gbpusd) / 3 * 2)[, 1], format = "%Y.%m.%d")
		    , as.Date(head(dat_usdcad, nrow(dat_usdcad) / 3 * 2)[, 1], format = "%Y.%m.%d")
		    , as.Date(head(dat_usdchf, nrow(dat_usdchf) / 3 * 2)[, 1], format = "%Y.%m.%d"))
dat_train$time <- c(as.POSIXct(as.character(head(dat_audusd, nrow(dat_audusd) / 3 * 2)[, 2]), format = '%H:%M')
		    , as.POSIXct(as.character(head(dat_eurusd, nrow(dat_eurusd) / 3 * 2)[, 2]), format = '%H:%M')
		    , as.POSIXct(as.character(head(dat_gbpusd, nrow(dat_gbpusd) / 3 * 2)[, 2]), format = '%H:%M')
		    , as.POSIXct(as.character(head(dat_usdcad, nrow(dat_usdcad) / 3 * 2)[, 2]), format = '%H:%M')
		    , as.POSIXct(as.character(head(dat_usdchf, nrow(dat_usdchf) / 3 * 2)[, 2]), format = '%H:%M'))
dat_train$symbol <- as.factor(c(rep('audusd', times = nrow(dat_audusd) / 3 * 2)
		    , rep('eurusd', times = nrow(dat_eurusd) / 3 * 2)
		    , rep('gbpusd', times = nrow(dat_gbpusd) / 3 * 2)
		    , rep('usdcad', times = nrow(dat_usdcad) / 3 * 2)
		    , rep('usdchf', times = nrow(dat_usdchf) / 3 * 2)))
colnames(dat_train) <- c(
			'open'
			, 'date'
			, 'time'
			, 'symbol'
			)
}


### selecting test
{
dat_test <- as.data.frame(c(tail(dat_audusd, nrow(dat_audusd) / 3 )[, 3]
			     , tail(dat_eurusd, nrow(dat_eurusd) / 3 )[, 3]
			     , tail(dat_gbpusd, nrow(dat_gbpusd) / 3 )[, 3]
			     , tail(dat_usdcad, nrow(dat_usdcad) / 3 )[, 3]
			     , tail(dat_usdchf, nrow(dat_usdchf) / 3 )[, 3]))
dat_test$date <- c(as.Date(tail(dat_audusd, nrow(dat_audusd) / 3 )[, 1], format = "%Y.%m.%d")
		    , as.Date(tail(dat_eurusd, nrow(dat_eurusd) / 3 )[, 1], format = "%Y.%m.%d")
		    , as.Date(tail(dat_gbpusd, nrow(dat_gbpusd) / 3 )[, 1], format = "%Y.%m.%d")
		    , as.Date(tail(dat_usdcad, nrow(dat_usdcad) / 3 )[, 1], format = "%Y.%m.%d")
		    , as.Date(tail(dat_usdchf, nrow(dat_usdchf) / 3 )[, 1], format = "%Y.%m.%d"))
dat_test$time <- c(as.POSIXct(as.character(tail(dat_audusd, nrow(dat_audusd) / 3 )[, 2]), format = '%H:%M')
		    , as.POSIXct(as.character(tail(dat_eurusd, nrow(dat_eurusd) / 3 )[, 2]), format = '%H:%M')
		    , as.POSIXct(as.character(tail(dat_gbpusd, nrow(dat_gbpusd) / 3 )[, 2]), format = '%H:%M')
		    , as.POSIXct(as.character(tail(dat_usdcad, nrow(dat_usdcad) / 3 )[, 2]), format = '%H:%M')
		    , as.POSIXct(as.character(tail(dat_usdchf, nrow(dat_usdchf) / 3 )[, 2]), format = '%H:%M'))
dat_test$symbol <- as.factor(c(rep('audusd', times = nrow(dat_audusd) - head(nrow(dat_audusd) / 3 * 2))
				, rep('eurusd', times = nrow(dat_eurusd) - head(nrow(dat_eurusd) / 3 * 2) + 1)
				, rep('gbpusd', times = nrow(dat_gbpusd) - head(nrow(dat_gbpusd) / 3 * 2) + 1)
				, rep('usdcad', times = nrow(dat_usdcad) - head(nrow(dat_usdcad) / 3 * 2) + 1)
				, rep('usdchf', times = nrow(dat_usdchf) - head(nrow(dat_usdchf) / 3 * 2) + 1)))
colnames(dat_test) <- c(
			'open'
			, 'date'
			, 'time'
			, 'symbol'
			)
}

### saving samples
{
save(dat_train, file = 'dat_train.R')
save(dat_test, file = 'dat_test.R')
}

### loading working data
{
dat_train1 <- read.csv('dat_train.csv'
			, sep = ','
			, dec = '.'
			, colClasses = 'numeric')

dat_test1 <- read.csv('dat_test.csv'
		      , sep = ','
		      , dec = '.'
		      , colClasses = 'numeric')

load('dat_train.R')
load('dat_test.R')

}

################################## feature engineering
# train set
dat_train_working <- dat_train

max_lag_power <- 9.5

# difference
{
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_diff', lag, sep = '_')
	
	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), "x"] - 
		dat_train_working[(2 ^ max_lag_power + 1 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag), 'x']   # p - p
	
#	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
#		log(dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), "x"]) - 
#		log(dat_train_working[(2 ^ max_lag_power + 1 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag), 'x'])   # log(p) - log(p)
#	
#	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
#		dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), 'x'] / 
#		lag(dat_train_working[(2 ^ max_lag_power + 1 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag), 'x']) - 1   # p / p - 1
	
}

write.table(dat_train_working[, 2:19], file = 'dat_train_differences.csv', sep = ',', dec = '.', row.names = F)
rm(dat_train_working)
gc()
}

require(zoo)
# differences from mean
{
dat_train_working <- dat_train

start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_mean_diff', lag, sep = '_')
	
	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), 'x'] -
		rollapply(dat_train_working$x, lag, mean)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag + 1)]

	
}
Sys.time() - start
write.table(dat_train_working[, 2:19], file = 'dat_train_diffmean.csv', sep = ',', dec = '.', row.names = F)
rm(dat_train_working)
gc()
}

# differences from max
{
dat_train_working <- dat_train
start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_max_diff', lag, sep = '_')
	
	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), 'x'] -
		rollapply(dat_train_working$x, lag, max)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_train_working[, 2:19], file = 'dat_train_diffmax.csv', sep = ',', dec = '.', row.names = F)
rm(dat_train_working)
gc()
}

# differences from min
{
dat_train_working <- dat_train
start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_min_diff', lag, sep = '_')
	
	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), 'x'] -
		rollapply(dat_train_working$x, lag, min)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_train_working[, 2:19], file = 'dat_train_diffmin.csv', sep = ',', dec = '.', row.names = F)
rm(dat_train_working)
gc()
}

# rolling sd
{
dat_train_working <- dat_train
start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_sd', lag, sep = '_')
	
	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		rollapply(dat_train_working$x, lag, sd)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_train_working[, 2:19], file = 'dat_train_sd.csv', sep = ',', dec = '.', row.names = F)
rm(dat_train_working)
gc()
}

# rolling range
{
dat_train_working <- dat_train
start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_range', lag, sep = '_')
	
	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		rollapply(dat_train_working$x, lag, max)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag + 1)] -
		rollapply(dat_train_working$x, lag, min)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_train_working[, 2:19], file = 'dat_train_range.csv', sep = ',', dec = '.', row.names = F)
rm(dat_train_working)
gc()
}

# additional features
{
dat_train_working <- dat_train

dat_train_working$month <- as.numeric(format(dat_train_working$date, "%m"))
dat_train_working$day <- as.numeric(format(dat_train_working$date, "%d"))
dat_train_working$week_day <- as.POSIXlt(dat_train_working$date)$wday
dat_train_working$hour <- as.POSIXlt(dat_train_working$time)[[3]]
dat_train_working$minute <- as.POSIXlt(dat_train_working$time)[[2]]

write.table(dat_train_working[, 4:9], file = 'dat_train_additional.csv', sep = ',', dec = '.', row.names = F)
rm(dat_train_working)
gc()
}


# output = future difference
{
dat_train_working <- dat_train

for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('future_lag', lag, sep = '_')
	
	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_train_working[(2 ^ max_lag_power + 1 + lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power + lag), 'x'] - 
		dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), "x"]
		   # p - p
	
	#	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
	#		log(dat_train_working[(2 ^ max_lag_power + 1 + lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power + lag), 'x']) - 
	#               log(dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), "x"])    # log(p) - log(p)
	#		   
	#	
	#	dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
	#		dat_train_working[(2 ^ max_lag_power + 1 + lag) : (dim(dat_train_working)[1] - 2 ^ max_lag_power + lag), 'x'] / 
	#               dat_train_working[(2 ^ max_lag_power + 1) : (dim(dat_train_working)[1] - 2 ^ max_lag_power), 'x'] - 1   # p / p - 1
	#		
	
}

write.table(dat_train_working[, 2:19], file = 'dat_train_future_lag.csv', sep = ',', dec = '.', row.names = F)
rm(dat_train_working)
gc()
}

### create final training sample
{

set.seed(10)

start <- round(runif(1, min = 2 ^ max_lag_power, max = 1440))
nrows <- numeric()
nrows[1] <- start
counter <- 2
while (start <= (nrow(dat_train) - (2 ^ max_lag_power) * 2)){
	
	start <- start + round(2 ^ max_lag_power + runif(1, min = -50, max = 50))
	nrows[counter] <- start
	counter <- counter + 1
	
}

# save indexes
save(nrows, file = 'dat_train_sample_indexes.R')

}

######################### unite inputs and outputs
{
dat_train_diff <- read.table('dat_train_differences.csv'
				, header = T
				, sep = ','
				, dec = '.'
				, colClasses = rep('numeric', 18)
				, nrows = 19000000)

dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% nrows)
rm(dat_train_diff)
gc()

dat_train_diff <- read.csv('dat_train_diffmean.csv'
			   , header = T
			   , sep = ','
			   , dec = '.'
			   , colClasses = rep('numeric', 18)
			   , nrows = 19000000)

dat_train_final <- cbind(dat_train_final, subset(dat_train_diff, rownames(dat_train_diff) %in% nrows))
rm(dat_train_diff)
gc()

dat_train_diff <- read.csv('dat_train_diffmax.csv'
			   , header = T
			   , sep = ','
			   , dec = '.'
			   , colClasses = rep('numeric', 18)
			   , nrows = 19000000)

dat_train_final <- cbind(dat_train_final, subset(dat_train_diff, rownames(dat_train_diff) %in% nrows))
rm(dat_train_diff)
gc()


dat_train_diff <- read.csv('dat_train_diffmin.csv'
			   , header = T
			   , sep = ','
			   , dec = '.'
			   , colClasses = rep('numeric', 18)
			   , nrows = 19000000)

dat_train_final <- cbind(dat_train_final, subset(dat_train_diff, rownames(dat_train_diff) %in% nrows))
rm(dat_train_diff)
gc()


dat_train_diff <- read.csv('dat_train_sd.csv'
			   , header = T
			   , sep = ','
			   , dec = '.'
			   , colClasses = rep('numeric', 18)
			   , nrows = 19000000)

dat_train_final <- cbind(dat_train_final, subset(dat_train_diff, rownames(dat_train_diff) %in% nrows))
rm(dat_train_diff)
gc()


dat_train_diff <- read.csv('dat_train_range.csv'
			   , header = T
			   , sep = ','
			   , dec = '.'
			   , colClasses = rep('numeric', 18)
			   , nrows = 19000000)

dat_train_final <- cbind(dat_train_final, subset(dat_train_diff, rownames(dat_train_diff) %in% nrows))
rm(dat_train_diff)
gc()


dat_train_diff <- read.csv('dat_train_additional.csv'
			   , header = T
			   , sep = ','
			   , dec = '.'
			   , colClasses = c('factor', rep('integer', 5))
			   , nrows = 19000000)

dat_train_final <- cbind(dat_train_final, subset(dat_train_diff, rownames(dat_train_diff) %in% nrows))
rm(dat_train_diff)
gc()


dat_train_diff <- read.csv('dat_train_future_lag.csv'
			   , header = T
			   , sep = ','
			   , dec = '.'
			   , colClasses = rep('numeric', 18)
			   , nrows = 19000000)

dat_train_final <- cbind(dat_train_final, subset(dat_train_diff, rownames(dat_train_diff) %in% nrows))
rm(dat_train_diff)
gc()
}

#observe train set
{
str(dat_train_final)
summary(dat_train_final)
plot(dat_train_final$lag_diff_724, type = 'l')
plot(dat_train_final$future_lag_724, type = 'l')
nrow(subset(dat_train_final, dat_train_final$lag_diff_724 > 0.05 | dat_train_final$lag_diff_724 < -0.05))
}

# delete history overlap artefacts
{
dat_train_final <- subset(dat_train_final, dat_train_final$lag_diff_724 < 0.05 & dat_train_final$lag_diff_724 > -0.05)
dat_train_final <- subset(dat_train_final, dat_train_final$future_lag_724 < 0.05 & dat_train_final$future_lag_724 > -0.05)
}

# save final training sample
{
write.table(dat_train_final, file = 'dat_train_final.csv', sep = ',', dec = '.', row.names = F)

rm(dat_train)
gc()
}



########################
###########################
############################# Creating many train samples
###########################
########################

### generating sample indexes
{
	dat_train <- read.csv('dat_train.csv'
			     , sep = ','
			     , dec = '.'
			     , colClasses = 'numeric')
	
	max_lag_power <- 9.5
	
	# create list of index vectors
	many_train_sample_indexes <- list()
	
	for (i in 1:99){
		
		start <- round(runif(1, min = 2 ^ max_lag_power, max = 1440))
		nrows <- integer()
		nrows[1] <- start
		counter <- 2
		
		while (start <= (nrow(dat_train) - (2 ^ max_lag_power) * 2)){
			
			start <- start + round(2 ^ max_lag_power + runif(1, min = 0, max = 50))
			nrows[counter] <- start
			counter <- counter + 1
			
		}
		
		many_train_sample_indexes[[i]] <- nrows
		
	}
	
	plot(many_train_sample_indexes[[2]] / 1440 - floor(many_train_sample_indexes[[2]] / 1440), type = 'l', col = 'blue')
	lines(many_train_sample_indexes[[17]] / 1440 - floor(many_train_sample_indexes[[17]] / 1440), type = 'l', col = 'red')
	cor(many_train_sample_indexes[[2]] / 1440 - floor(many_train_sample_indexes[[2]] / 1440), many_train_sample_indexes[[17]] / 1440 - floor(many_train_sample_indexes[[17]] / 1440))
	
	save(many_train_sample_indexes, file = 'many_train_sample_indexes.R')
}

######################### unite inputs and outputs for many train samples
{

# load list of indexes
load(file = 'many_train_sample_indexes.R')

# create list of samples
many_train_samples <- list()

# create sample names
train_sample_names <- character()
for (i in 1:99){
	train_sample_names[i] <- paste('train_set_', i, sep = '')
}

# start loading differences
dat_train_diff <- read.table('dat_train_differences.csv'
			    , header = T
			    , sep = ','
			    , dec = '.'
			    , colClasses = rep('numeric', 18)
			    , nrows = 19000000)

for (i in 1:99){
	
	dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% many_train_sample_indexes[[i]])
	many_train_samples[[i]] <- dat_train_final
	
}

rm(dat_train_final)
rm(dat_train_diff)
gc()

#save(many_train_samples, file = 'many_train_samples.R')


# start loading diff means
dat_train_diff <- read.csv('dat_train_diffmean.csv'
			  , header = T
			  , sep = ','
			  , dec = '.'
			  , colClasses = rep('numeric', 18)
			  , nrows = 19000000)

for (i in 1:99){
	
	dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% many_train_sample_indexes[[i]])
	many_train_samples[[i]] <- cbind(many_train_samples[[i]], dat_train_final)
	
}

rm(dat_train_final)
rm(dat_train_diff)
gc()

#save(many_train_samples, file = 'many_train_samples.R')


# start loading diff maxs
dat_train_diff <- read.csv('dat_train_diffmax.csv'
			  , header = T
			  , sep = ','
			  , dec = '.'
			  , colClasses = rep('numeric', 18)
			  , nrows = 19000000)

for (i in 1:99){
	
	dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% many_train_sample_indexes[[i]])
	many_train_samples[[i]] <- cbind(many_train_samples[[i]], dat_train_final)
	
}

rm(dat_train_final)
rm(dat_train_diff)
gc()

#save(many_train_samples, file = 'many_train_samples.R')


# start loading diff mins
dat_train_diff <- read.csv('dat_train_diffmin.csv'
			  , header = T
			  , sep = ','
			  , dec = '.'
			  , colClasses = rep('numeric', 18)
			  , nrows = 19000000)

for (i in 1:99){
	
	dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% many_train_sample_indexes[[i]])
	many_train_samples[[i]] <- cbind(many_train_samples[[i]], dat_train_final)
	
}

rm(dat_train_final)
rm(dat_train_diff)
gc()

#save(many_train_samples, file = 'many_train_samples.R')


# start loading diff sds
dat_train_diff <- read.csv('dat_train_sd.csv'
			  , header = T
			  , sep = ','
			  , dec = '.'
			  , colClasses = rep('numeric', 18)
			  , nrows = 19000000)

for (i in 1:99){
	
	dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% many_train_sample_indexes[[i]])
	many_train_samples[[i]] <- cbind(many_train_samples[[i]], dat_train_final)
	
}

rm(dat_train_final)
rm(dat_train_diff)
gc()

#save(many_train_samples, file = 'many_train_samples.R')


# start loading diff ranges
dat_train_diff <- read.csv('dat_train_range.csv'
			  , header = T
			  , sep = ','
			  , dec = '.'
			  , colClasses = rep('numeric', 18)
			  , nrows = 19000000)

for (i in 1:99){
	
	dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% many_train_sample_indexes[[i]])
	many_train_samples[[i]] <- cbind(many_train_samples[[i]], dat_train_final)
	
}

rm(dat_train_final)
rm(dat_train_diff)
gc()

#save(many_train_samples, file = 'many_train_samples.R')


# start loading additional features
dat_train_diff <- read.csv('dat_train_additional.csv'
			  , header = T
			  , sep = ','
			  , dec = '.'
			  , colClasses = c('factor', rep('integer', 5))
			  , nrows = 19000000)

for (i in 1:99){
	
	dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% many_train_sample_indexes[[i]])
	many_train_samples[[i]] <- cbind(many_train_samples[[i]], dat_train_final)
	
}

rm(dat_train_final)
rm(dat_train_diff)
gc()

#save(many_train_samples, file = 'many_train_samples.R')


# start loading future lags
dat_train_diff <- read.csv('dat_train_future_lag.csv'
			  , header = T
			  , sep = ','
			  , dec = '.'
			  , colClasses = rep('numeric', 18)
			  , nrows = 19000000)

for (i in 1:99){
	
	dat_train_final <- subset(dat_train_diff, rownames(dat_train_diff) %in% many_train_sample_indexes[[i]])
	many_train_samples[[i]] <- cbind(many_train_samples[[i]], dat_train_final)
	
}

rm(dat_train_final)
rm(dat_train_diff)
gc()

save(many_train_samples, file = 'many_train_samples.R')

}


# delete history overlap artefacts
{

for (i in 1:99){
	
	many_train_samples[[i]] <- subset(many_train_samples[[i]], many_train_samples[[i]]$lag_diff_724 < 0.05 & many_train_samples[[i]]$lag_diff_724 > -0.05)
	many_train_samples[[i]] <- subset(many_train_samples[[i]], many_train_samples[[i]]$future_lag_724 < 0.05 & many_train_samples[[i]]$future_lag_724 > -0.05)
	
}

}

# save final training set
{

save(many_train_samples, file = 'many_train_samples.R')
gc()

}







####################################
##############################
######################## test set
##############################
####################################
dat_test_working <- dat_test

max_lag_power <- 9.5

# difference
{
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_diff', lag, sep = '_')
	
	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), "x"] - 
		dat_test_working[(2 ^ max_lag_power + 1 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag), 'x']   # p - p
	
	#	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
	#		log(dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), "x"]) - 
	#		log(dat_test_working[(2 ^ max_lag_power + 1 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag), 'x'])   # log(p) - log(p)
	#	
	#	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
	#		dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), 'x'] / 
	#		lag(dat_test_working[(2 ^ max_lag_power + 1 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag), 'x']) - 1   # p / p - 1
	
}

write.table(dat_test_working[, 2:19], file = 'dat_test_differences.csv', sep = ',', dec = '.', row.names = F)
rm(dat_test_working)
gc()
}

require(zoo)
# differences from mean
{
dat_test_working <- dat_test

start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_mean_diff', lag, sep = '_')
	
	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), 'x'] -
		rollapply(dat_test_working$x, lag, mean)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_test_working[, 2:19], file = 'dat_test_diffmean.csv', sep = ',', dec = '.', row.names = F)
rm(dat_test_working)
gc()
}

# differences from max
{
dat_test_working <- dat_test
start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_max_diff', lag, sep = '_')
	
	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), 'x'] -
		rollapply(dat_test_working$x, lag, max)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_test_working[, 2:19], file = 'dat_test_diffmax.csv', sep = ',', dec = '.', row.names = F)
rm(dat_test_working)
gc()
}

# differences from min
{
dat_test_working <- dat_test
start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_min_diff', lag, sep = '_')
	
	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), 'x'] -
		rollapply(dat_test_working$x, lag, min)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_test_working[, 2:19], file = 'dat_test_diffmin.csv', sep = ',', dec = '.', row.names = F)
rm(dat_test_working)
gc()
}

# rolling sd
{
dat_test_working <- dat_test
start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_sd', lag, sep = '_')
	
	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		rollapply(dat_test_working$x, lag, sd)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_test_working[, 2:19], file = 'dat_test_sd.csv', sep = ',', dec = '.', row.names = F)
rm(dat_test_working)
gc()
}

# rolling range
{
dat_test_working <- dat_test
start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('lag_range', lag, sep = '_')
	
	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		rollapply(dat_test_working$x, lag, max)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag + 1)] -
		rollapply(dat_test_working$x, lag, min)[(2 ^ max_lag_power + 2 - lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power - lag + 1)]
	
	
}
Sys.time() - start
write.table(dat_test_working[, 2:19], file = 'dat_test_range.csv', sep = ',', dec = '.', row.names = F)
rm(dat_test_working)
gc()
}

# additional features
{
dat_test_working <- dat_test

dat_test_working$month <- as.numeric(format(dat_test_working$date, "%m"))
dat_test_working$day <- as.numeric(format(dat_test_working$date, "%d"))
dat_test_working$week_day <- as.POSIXlt(dat_test_working$date)$wday
dat_test_working$hour <- as.POSIXlt(dat_test_working$time)[[3]]
dat_test_working$minute <- as.POSIXlt(dat_test_working$time)[[2]]

write.table(dat_test_working[, 4:9], file = 'dat_test_additional.csv', sep = ',', dec = '.', row.names = F)
rm(dat_test_working)
gc()
}

# output = future difference
{
dat_test_working <- dat_test

start <- Sys.time()
for (lags in seq(from = 1, to = max_lag_power, by = 0.5)){
	
	lag <- round(2 ^ lags)
	newcolname_lag <- paste('future_lag', lag, sep = '_')
	
	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
		dat_test_working[(2 ^ max_lag_power + 1 + lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power + lag), 'x'] - 
		dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), "x"]
	# p - p
	
	#	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
	#		log(dat_test_working[(2 ^ max_lag_power + 1 + lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power + lag), 'x']) - 
	#               log(dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), "x"])    # log(p) - log(p)
	#		   
	#	
	#	dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), eval(newcolname_lag)] <- 
	#		dat_test_working[(2 ^ max_lag_power + 1 + lag) : (dim(dat_test_working)[1] - 2 ^ max_lag_power + lag), 'x'] / 
	#               dat_test_working[(2 ^ max_lag_power + 1) : (dim(dat_test_working)[1] - 2 ^ max_lag_power), 'x'] - 1   # p / p - 1
	#		
	
}
Sys.time() - start

write.table(dat_test_working[, 2:19], file = 'dat_test_future_lag.csv', sep = ',', dec = '.', row.names = F)
rm(dat_test_working)
gc()
}





######## create final testing sample
{
	set.seed(10)
	big_test_indexes <- sample(9460610, 2000000, replace = F)
	big_test_indexes <- big_test_indexes[big_test_indexes > 724 & big_test_indexes < 9459886]
	big_test_indexes <- sort(big_test_indexes, decreasing = F)
	
	save(big_test_indexes, file = 'big_test_indexes.R')
}

######################### unite inputs and outputs
{
	dat_test_diff <- read.table('dat_test_differences.csv'
				    , header = T
				    , sep = ','
				    , dec = '.'
				    , colClasses = rep('numeric', 18)
				    , nrows = 19000000)
	
	big_dat_test <- subset(dat_test_diff, rownames(dat_test_diff) %in% big_test_indexes)
	rm(dat_test_diff)
	gc()
	
	dat_test_diff <- read.csv('dat_test_diffmean.csv'
				  , header = T
				  , sep = ','
				  , dec = '.'
				  , colClasses = rep('numeric', 18)
				  , nrows = 19000000)
	
	big_dat_test <- cbind(big_dat_test, subset(dat_test_diff, rownames(dat_test_diff) %in% big_test_indexes))
	rm(dat_test_diff)
	gc()
	
	dat_test_diff <- read.csv('dat_test_diffmax.csv'
				  , header = T
				  , sep = ','
				  , dec = '.'
				  , colClasses = rep('numeric', 18)
				  , nrows = 19000000)
	
	big_dat_test <- cbind(big_dat_test, subset(dat_test_diff, rownames(dat_test_diff) %in% big_test_indexes))
	rm(dat_test_diff)
	gc()
	
	
	dat_test_diff <- read.csv('dat_test_diffmin.csv'
				  , header = T
				  , sep = ','
				  , dec = '.'
				  , colClasses = rep('numeric', 18)
				  , nrows = 19000000)
	
	big_dat_test <- cbind(big_dat_test, subset(dat_test_diff, rownames(dat_test_diff) %in% big_test_indexes))
	rm(dat_test_diff)
	gc()
	
	
	dat_test_diff <- read.csv('dat_test_sd.csv'
				  , header = T
				  , sep = ','
				  , dec = '.'
				  , colClasses = rep('numeric', 18)
				  , nrows = 19000000)
	
	big_dat_test <- cbind(big_dat_test, subset(dat_test_diff, rownames(dat_test_diff) %in% big_test_indexes))
	rm(dat_test_diff)
	gc()
	
	
	dat_test_diff <- read.csv('dat_test_range.csv'
				  , header = T
				  , sep = ','
				  , dec = '.'
				  , colClasses = rep('numeric', 18)
				  , nrows = 19000000)
	
	big_dat_test <- cbind(big_dat_test, subset(dat_test_diff, rownames(dat_test_diff) %in% big_test_indexes))
	rm(dat_test_diff)
	gc()
	
	
	dat_test_diff <- read.csv('dat_test_additional.csv'
				  , header = T
				  , sep = ','
				  , dec = '.'
				  , colClasses = c('factor', rep('integer', 5))
				  , nrows = 19000000)
	
	big_dat_test <- cbind(big_dat_test, subset(dat_test_diff, rownames(dat_test_diff) %in% big_test_indexes))
	rm(dat_test_diff)
	gc()
	
	
	dat_test_diff <- read.csv('dat_test_future_lag.csv'
				  , header = T
				  , sep = ','
				  , dec = '.'
				  , colClasses = rep('numeric', 18)
				  , nrows = 19000000)
	
	big_dat_test <- cbind(big_dat_test, subset(dat_test_diff, rownames(dat_test_diff) %in% big_test_indexes))
	rm(dat_test_diff)
	gc()
	
	
	# delete history overlap artefacts
	
	big_dat_test <- subset(big_dat_test, big_dat_test$lag_diff_724 < 0.05 & big_dat_test$lag_diff_724 > -0.05)
	big_dat_test <- subset(big_dat_test, big_dat_test$future_lag_724 < 0.05 & big_dat_test$future_lag_724 > -0.05)
	
	
	# save final testing set
	
	save(big_dat_test, file = 'big_dat_test.R')
	
	rm(dat_test)
	gc()

}





