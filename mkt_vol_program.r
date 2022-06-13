# This file includes some R codes that are used when conducting the SOA sponsored research
# project "Market Volatility Risk Management in an Era of Extreme Events"
# The code is for eductional purpose only. It is provided 'as is' without warranty of any kind, 
# either express or implied, warranties of fitness for a purpose, or the warranty of non-infringement. 
# Although the authors try their best to test the tool, they make no warranty that
# (1) it will meet your requirements;
# (2) it will be secure or error-free;
# (3) the results that may be obtained from the use of the code will be effective, accurate or reliable;
# (4) any errors in the tool will be corrected.

# The CIA, the project oversight group and the author assume no responsibility for errors or omissions 
# in the code or related documentation. In no event shall the sponsor and the authors be liable to you 
# or any third parties for any damages of any kind arising out of or in connection with the use of the code. 

# You may contact Kailan Shang at klshang81@gmail.com for any questions about the code.

# R is a open-source statistical software and can be downloaded at www.r-project.org
# The codes have been tested using R4.0.4

############################################################################
# Market Volatility Analysis of SPX
############################################################################

# Set working directory
setwd("C:/dsge/market_vol")

# Load historical data from an EXCEL file

# install.packages("readxl")
library(readxl)

daily_data <- read_excel("data.xlsx", sheet="daily_final", col_types = c("date", rep("numeric", 20)))

n_business_days_yearly <- 252
n_business_days_monthly <- round(252/12)

# Figure 1

par(mfrow=c(2,1))

plot(daily_data$Date,daily_data$SPX_rtn, type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return", xlab="Date", ylab="SPX daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

moving_filter <- function(x, w, FUN, cl) {
  w = as.integer(w)
  if (w < 1) {
    stop("The length of the window must be no less than 1")
  }
  output <- x
  for (i in 1:length(x)) {
     # plus 1 because the index is inclusive with the upper_bound 'i'
    lower_bound <- i - w + 1
    if (lower_bound < 1) {
      output[i] <- NA_real_
    } else {
	  if(missing(cl)){
		output[i] <- FUN(x[lower_bound:i])
	  } else {
		output[i] <- FUN(x[lower_bound:i], cl)	  
	  }
    }
  }
  output
}

# compute daily return volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(daily_data$SPX_rtn,w,sd)
plot(daily_data$Date,monthly_vol_spx , type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return Volatility (Monthly Window)", xlab="Date", ylab="SPX daily return volatility")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


# descriptive statistics, Table 1
library(moments)

create_ds <- function(dataset, cl){
	NA_counts <- sapply(dataset, function(x) sum(is.na(x)))
	record_counts <- nrow(dataset) - NA_counts
	min <- sapply(dataset, min, na.rm=TRUE)
	max <- sapply(dataset, max, na.rm=TRUE)
	mean <- sapply(dataset, mean, na.rm=TRUE)
	std_dev <- sapply(dataset, sd, na.rm=TRUE)
	skewness <- sapply(dataset, skewness, na.rm=TRUE)
	kurtosis <- sapply(dataset, kurtosis, na.rm=TRUE)
	var_left <- sapply(dataset, function(x) quantile(x, cl, na.rm=TRUE))
	cte_left <- sapply(dataset, function(x) mean(x[x<=quantile(x, cl, na.rm=TRUE)], na.rm=TRUE))
	var_right <- sapply(dataset, function(x) quantile(x, 1-cl, na.rm=TRUE))
	cte_right <- sapply(dataset, function(x) mean(x[x>quantile(x, 1-cl, na.rm=TRUE)], na.rm=TRUE))

	data.frame(record_counts, min, max, mean, std_dev, skewness, kurtosis, var_left, cte_left, var_right, cte_right)
}

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
# daily_new <- daily_data[23103:23355,]
daily_fc_2008 <- daily_data[20062:20458,]
daily_great_depression <- daily_data[397:1306,]
daily_black_monday <- daily_data[14975:15291,]

perc <- 0.01

daily_all_ds <- create_ds(daily_data[, !names(daily_data) %in% c("Date")], perc)
write.csv(daily_all_ds,"daily_all_ds.csv")
daily_new_ds <- create_ds(daily_new[, !names(daily_new) %in% c("Date")], perc)
write.csv(daily_new_ds,"daily_new_ds_v2.csv")
daily_fc_2008_ds <- create_ds(daily_fc_2008[, !names(daily_fc_2008) %in% c("Date")], perc)
write.csv(daily_fc_2008_ds,"daily_fc_2008_ds.csv")
daily_great_depression_ds <- create_ds(daily_great_depression[, !names(daily_great_depression) %in% c("Date")], perc)
write.csv(daily_great_depression_ds,"daily_great_depression_ds.csv")
daily_black_monday_ds <- create_ds(daily_black_monday[, !names(daily_black_monday) %in% c("Date")], perc)
write.csv(daily_black_monday_ds,"daily_black_monday_ds.csv")


# Implied Volatility, Figure 2
iv_data <- daily_data[as.Date(daily_data$Date)>as.Date("1990-1-1"),]
plot(iv_data$Date,unlist(iv_data[, "VIXCLS"]), type = "p", col="blue", pch=20, cex=0.25, main="SPX Implied Volatility", xlab="Date", ylab="VIXCLS")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}

# VVIX, Figure 3
iv_data <- daily_data[as.Date(daily_data$Date)>as.Date("2006-3-1"),]
plot(iv_data$Date,unlist(iv_data[, "VVIX"]), type = "p", col="blue", pch=20, cex=0.25, main="SPX Volatility of Volatility", xlab="Date", ylab="VVIX")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 3, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}

# Realized Vol of (Implied) Volatility, Figure 4
par(mfrow=c(2,1))

# compute vol of volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(daily_data$SPX_rtn,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5)
monthly_vol_of_vol_spx <- moving_filter(monthly_vol_spx,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5)
plot(daily_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="SPX Realized Volatility of Volatility", xlab="Date", ylab="Vol of Monthly SPX Vol")

for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute vol of implied volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_of_vol_spx <- moving_filter(daily_data$VIXCLS,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5) #annualized
plot(daily_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="SPX Realized Volatility of Implied Volatility", xlab="Date", ylab="Vol of VIXCLS")

for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)









#NASDAQ Analysis

nasdaq_data <- daily_data[as.Date(daily_data$Date)>as.Date("1971-2-1"),]
# Figure A.1

par(mfrow=c(2,1))

plot(nasdaq_data$Date,nasdaq_data$NASDAQCOM_rtn, type = "p", col="blue", pch=20, cex=0.25, main="NASDAQ Daily Return", xlab="Date", ylab="NASDAQ daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(nasdaq_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(nasdaq_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- nasdaq_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- nasdaq_data$Date[idx]
		} else {
			next
		}
	}
}
rect(nasdaq_data$Date[14975-10764], -1, nasdaq_data$Date[15291-10764], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute daily return volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(nasdaq_data$NASDAQCOM_rtn,w,sd)
plot(nasdaq_data$Date,monthly_vol_spx , type = "p", col="blue", pch=20, cex=0.25, main="NASDAQ Daily Return Volatility (Monthly Window)", xlab="Date", ylab="NASDAQ daily return volatility")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(nasdaq_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(nasdaq_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- nasdaq_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- nasdaq_data$Date[idx]
		} else {
			next
		}
	}
}
rect(nasdaq_data$Date[14975-10764], -1, nasdaq_data$Date[15291-10764], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# Implied Volatility, Figure 2
iv_data <- daily_data[as.Date(daily_data$Date)>as.Date("2001-2-1"),]
plot(iv_data$Date,unlist(iv_data[, "VXNCLS"]), type = "p", col="blue", pch=20, cex=0.25, main="NASDAQ Implied Volatility", xlab="Date", ylab="VXNCLS")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}

# Realized Vol of (Implied) Volatility, Figure 4
par(mfrow=c(2,1))

# compute vol of volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(nasdaq_data$NASDAQCOM_rtn,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5)
monthly_vol_of_vol_spx <- moving_filter(monthly_vol_spx,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5)
plot(nasdaq_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="NASDAQ Realized Volatility of Volatility", xlab="Date", ylab="Vol of Monthly NASDAQ Vol")

for (idx in c(1:nrow(nasdaq_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(nasdaq_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- nasdaq_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- nasdaq_data$Date[idx]
		} else {
			next
		}
	}
}
rect(nasdaq_data$Date[14975-10764], -1, nasdaq_data$Date[15291-10764], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute vol of implied volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_of_vol_spx <- moving_filter(nasdaq_data$VXNCLS,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5) #annualized
plot(nasdaq_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="NASDAQCOM Realized Volatility of Implied Volatility", xlab="Date", ylab="Vol of VXNCLS")

for (idx in c(1:nrow(nasdaq_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(nasdaq_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- nasdaq_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- nasdaq_data$Date[idx]
		} else {
			next
		}
	}
}
rect(nasdaq_data$Date[14975-10764], -1, nasdaq_data$Date[15291-10764], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)




#RUSSELL2000 Analysis

# Figure A.1
russell_data <- daily_data[as.Date(daily_data$Date)>as.Date("1987-9-10"),]

par(mfrow=c(2,1))

plot(russell_data$Date,russell_data$RUT_rtn, type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL Daily Return", xlab="Date", ylab="RUSSELL daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(russell_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(russell_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- russell_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- russell_data$Date[idx]
		} else {
			next
		}
	}
}
rect(russell_data$Date[14975-14961], -1, russell_data$Date[15291-14961], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute daily return volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(russell_data$RUT_rtn,w,sd)
plot(russell_data$Date,monthly_vol_spx , type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL Daily Return Volatility (Monthly Window)", xlab="Date", ylab="RUSSELL daily return volatility")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(russell_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(russell_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- russell_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- russell_data$Date[idx]
		} else {
			next
		}
	}
}
rect(russell_data$Date[14975-14961], -1, russell_data$Date[15291-14961], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# Implied Volatility, Figure 2
iv_data <- daily_data[as.Date(daily_data$Date)>as.Date("2004-1-1"),]
plot(iv_data$Date,unlist(iv_data[, "RVXCLS"]), type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL 2000 Implied Volatility", xlab="Date", ylab="RVXCLS")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}

# Realized Vol of (Implied) Volatility, Figure 4
par(mfrow=c(2,1))

# compute vol of volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(russell_data$RUT_rtn,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5)
monthly_vol_of_vol_spx <- moving_filter(monthly_vol_spx,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5)
plot(russell_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL Realized Volatility of Volatility", xlab="Date", ylab="Vol of Monthly RUSSELL Vol")

for (idx in c(1:nrow(russell_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(russell_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- russell_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- russell_data$Date[idx]
		} else {
			next
		}
	}
}
rect(russell_data$Date[14975-14961], -1, russell_data$Date[15291-14961], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute vol of implied volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_of_vol_spx <- moving_filter(russell_data$RVXCLS,w,sd)*((n_business_days_yearly/n_business_days_monthly)**0.5) #annualized
plot(russell_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL Realized Volatility of Implied Volatility", xlab="Date", ylab="Vol of RVXCLS")

for (idx in c(1:nrow(russell_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(russell_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- russell_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- russell_data$Date[idx]
		} else {
			next
		}
	}
}
rect(russell_data$Date[14975-14961], -1, russell_data$Date[15291-14961], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)





#monthly data
monthly_data <- read_excel("data.xlsx", sheet="monthly_final", col_types = c(rep("numeric", 2), "date","text", rep("numeric", 19)), skip=0)

monthly_new <- monthly_data[monthly_data$Year>=2020,]
monthly_extreme <- monthly_data[monthly_data$Contraction==1,]
monthly_fc_2008 <- monthly_data[960:978,]
monthly_great_depression <- monthly_data[20:63,]
monthly_black_monday <- monthly_data[718:732,]

perc <- 0.01

monthly_all_ds <- create_ds(monthly_data[, !names(monthly_data) %in% c("Date","yyyy-mm")], perc)
write.csv(monthly_all_ds,"monthly_all_ds.csv")
monthly_new_ds <- create_ds(monthly_new[, !names(monthly_new) %in% c("Date","yyyy-mm")], perc)
write.csv(monthly_new_ds,"monthly_new_ds_v2.csv")
monthly_fc_2008_ds <- create_ds(monthly_fc_2008[, !names(monthly_fc_2008) %in% c("Date","yyyy-mm")], perc)
write.csv(monthly_fc_2008_ds,"monthly_fc_2008_ds.csv")
monthly_great_depression_ds <- create_ds(monthly_great_depression[, !names(monthly_great_depression) %in% c("Date","yyyy-mm")], perc)
write.csv(monthly_great_depression_ds,"monthly_great_depression_ds.csv")
monthly_black_monday_ds <- create_ds(monthly_black_monday[, !names(monthly_black_monday) %in% c("Date","yyyy-mm")], perc)
write.csv(monthly_black_monday_ds,"monthly_black_monday_ds.csv")

# Figure 1

par(mfrow=c(2,1))

plot(monthly_data$Date,monthly_data$SPX_rtn, type = "p", col="blue", pch=20, cex=0.25, main="SPX Monthly Return", xlab="Date", ylab="SPX monthly return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(monthly_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(monthly_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- monthly_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- monthly_data$Date[idx]
		} else {
			next
		}
	}
}
rect(monthly_data$Date[718], -1, monthly_data$Date[732], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute daily return volatility using 21-day window
w <- 12
monthly_vol_spx <- moving_filter(monthly_data$SPX_rtn,w,sd)
plot(monthly_data$Date,monthly_vol_spx , type = "p", col="blue", pch=20, cex=0.25, main="SPX Monthly Return Volatility (Monthly Window)", xlab="Date", ylab="SPX monthly return volatility")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(monthly_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(monthly_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- monthly_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- monthly_data$Date[idx]
		} else {
			next
		}
	}
}
rect(monthly_data$Date[718], -1, monthly_data$Date[732], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


# Implied Volatility, Figure 2
iv_data <- monthly_data[as.Date(monthly_data$Date)>as.Date("1990-1-1"),]
plot(iv_data$Date,unlist(iv_data[, "VIXCLS"]), type = "p", col="blue", pch=20, cex=0.25, main="SPX Implied Volatility", xlab="Date", ylab="VIXCLS")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}


# Realized Vol of (Implied) Volatility, Figure 4
par(mfrow=c(2,1))

# compute vol of volatility using 21-day window
w <- 12
monthly_vol_spx <- moving_filter(monthly_data$SPX_rtn,w,sd)*((12)**0.5)
monthly_vol_of_vol_spx <- moving_filter(monthly_vol_spx,w,sd)*((12)**0.5)
plot(monthly_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="SPX Realized Volatility of Volatility", xlab="Date", ylab="Vol of Monthly SPX Vol")

for (idx in c(1:nrow(monthly_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(monthly_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- monthly_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- monthly_data$Date[idx]
		} else {
			next
		}
	}
}
rect(monthly_data$Date[718], -1, monthly_data$Date[732], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute vol of implied volatility using 21-day window
w <- 12
monthly_vol_of_vol_spx <- moving_filter(monthly_data$VIXCLS,w,sd)*((12)**0.5) #annualized
plot(monthly_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="SPX Realized Volatility of Implied Volatility", xlab="Date", ylab="Vol of VIXCLS")

for (idx in c(1:nrow(monthly_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(monthly_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- monthly_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- monthly_data$Date[idx]
		} else {
			next
		}
	}
}
rect(monthly_data$Date[718], -1, monthly_data$Date[732], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)



#Volatility clustering

#ACF
lag_max = 20

par(mfrow=c(3,2))

acf_all <- acf(daily_data$SPX_rtn[2:nrow(daily_data)], lag.max = lag_max, plot=FALSE)
acf_new <- acf(daily_new$SPX_rtn, lag.max = lag_max, plot=FALSE)
acf_fc_2008 <- acf(daily_fc_2008$SPX_rtn, lag.max = lag_max, plot=FALSE)
acf_black_monday <- acf(daily_black_monday$SPX_rtn, lag.max = lag_max, plot=FALSE)
acf_great_depression <- acf(daily_great_depression$SPX_rtn, lag.max = lag_max, plot=FALSE)

plot(acf_all, main="ACF: S&P 500 Daily Return (Jan 1928 - Apr 2022)", ylim=c(-0.4,1))
plot(acf_new, main="ACF: S&P 500 Daily Return (Post 2020)", ylim=c(-0.4,1))
plot(acf_fc_2008, main="ACF: S&P 500 Daily Return (Dec 2007 - Jun 2009)", ylim=c(-0.4,1))
plot(acf_black_monday, main="ACF: S&P 500 Daily Return (Oct 1987 - Dec 1988)", ylim=c(-0.4,1))
plot(acf_great_depression, main="ACF: S&P 500 Daily Return (Aug 1929 - Mar 1933)", ylim=c(-0.4,1))

#ARMA + GARCH
library(fGarch)

p <- 1
q <- 1
g_p <-1
g_q <-1

formu <- as.formula(paste0("~ arma(",p,",",q,") + garch(",g_p,",",g_q,")"))

# Fitting the data to ARMA(1,1) and GARCH(1,1) models
gf_all <- garchFit(formu, data = daily_data$SPX_rtn[2:nrow(daily_data)], cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_all
gf_new <- garchFit(formu, data = daily_new$SPX_rtn, cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_new
gf_fc_2008 <- garchFit(formu, data = daily_fc_2008$SPX_rtn, cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_fc_2008
gf_great_depression <- garchFit(formu, data = daily_great_depression$SPX_rtn, cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_great_depression
gf_black_monday <- garchFit(formu, data = daily_black_monday$SPX_rtn, cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_black_monday


# Draw return and conditional volatility
par(mfrow=c(2,1))

plot(daily_data$Date[2:length(daily_data$Date)],daily_data$SPX_rtn[2:length(daily_data$Date)], type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return", xlab="Date", ylab="SPX daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:(length(daily_data$Date)-1))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx+1]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx+1]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

plot(daily_data$Date[2:length(daily_data$Date)],gf_all@sigma.t, type = "p", col="blue", pch=20, cex=0.25, main="SPX Conditional Volatility", xlab="Date", ylab="SPX conditional volatility")
for (idx in c(1:(length(gf_all@sigma.t)-1))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx+1]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, 0, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx+1]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


# compare conditional vol using different study periods
plot(daily_data$Date[2:length(daily_data$Date)],gf_all@sigma.t, type = "p", col="blue", pch=20, cex=0.25, main="SPX conditional volatility (All Periods)", xlab="Date", ylab="SPX conditional volatility")
for (idx in c(1:(length(gf_all@sigma.t)-1))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx+1]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, 0, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx+1]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

sigma_t <- rep(NA, length(gf_all@sigma.t))

sigma_t[20061:20457] <- gf_fc_2008@sigma.t
sigma_t[396:1305] <- gf_great_depression@sigma.t
sigma_t[14974:15290] <- gf_black_monday@sigma.t
sigma_t[(length(sigma_t)-length(gf_new@sigma.t)+1):length(sigma_t)] <- gf_new@sigma.t

plot(daily_data$Date[2:length(daily_data$Date)],sigma_t, type = "p", col="blue", pch=20, cex=0.25, main="SPX conditional volatility (Extreme Periods)", xlab="Date", ylab="SPX conditional volatility")
for (idx in c(1:(length(gf_all@sigma.t)-1))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx+1]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, 0, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx+1]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


# QQplot comparing Gaussian and SGED
res <- gf_new@residuals/gf_new@sigma.t
# number of data
n <- length(res)
n<-500000
set.seed(123)
avg <- mean(res)
std <- sd(res)
# simulated Gaussian distribution
NormalDistribution<-rnorm(n,0,1)
par(mfrow=c(1,2))
# Q-Q plot
qqplot(res,NormalDistribution,main="Q-Q plot (Empirical v.s. Normal Distribution)",xlab = "Historical Experience Quantiles", ylab = "Normal Distribution Quantiles",xlim=range(-7.5,7.5),ylim=range(-7.5,7.5))
abline(0,1)

# simulated SGED
SGEDistribution<-rsged(n,0,1,1.519,0.6652)
# Q-Q plot
qqplot(res,SGEDistribution,main="Q-Q plot (Empirical v.s. SGED)",xlab = "Historical Experience Quantiles", ylab = "Skewed Generalized Error Distribution Quantiles",xlim=range(-7.5,7.5),ylim=range(-7.5,7.5))
abline(0,1)


# VaR estimation using simulation from March 2020 for 1 year (252 trading days)
start_idx <- 40 #40, 62, 73, 93

actual_rtn <- daily_new$SPX_rtn[start_idx:(start_idx+251)]

percentile_5th <- rep(0,252)
percentile_95th <- rep(0,252)
percentile_0_5th <- rep(0,252)
percentile_99_5th <- rep(0,252)

initial_rtn <- daily_new$SPX_rtn[start_idx-1]
initial_cond_vol <- gf_new@sigma.t[start_idx-1]
initial_residual <- gf_new@residuals[start_idx-1]
c <- 2.363e-04
phi1 <- 7.049e-01
theta1 <- -8.286e-01
omega <- 4.089e-06
alpha1 <- 1.860e-01
beta1 <-  8.014e-01
lambda  <-  6.652e-01
p <-  1.519e+00

n_sim <- 100000

simulated_rtn <- matrix(0,n_sim, 252)
rtn_period <- rep(0,n_sim)
rtn_cum <- rep(1,n_sim)
vol_period <- rep(0,n_sim)
res_period <- rep(0,n_sim)

set.seed(123)
for (idx in c(1:252)){
	if (idx == 1){
		res_period <- rsged(n_sim,0,1,p,lambda)
		vol_period <- (omega+alpha1*initial_residual^2+beta1*initial_cond_vol^2)^0.5
		rtn_period <- c+phi1*initial_rtn+theta1*initial_residual+res_period*vol_period
		rtn_cum <- rtn_cum*(1+rtn_period)
		percentile_5th[idx] <- quantile(rtn_period, 0.05)
		percentile_95th[idx] <- quantile(rtn_period, 0.95)
		percentile_0_5th[idx] <- quantile(rtn_period, 0.005)
		percentile_99_5th[idx] <- quantile(rtn_period, 0.995)
	}else{
		rtn_period <- c+phi1*rtn_period+theta1*res_period*vol_period
		vol_period <- (omega+alpha1*(res_period*vol_period)^2+beta1*vol_period^2)^0.5
		res_period <- rsged(n_sim,0,1,p,lambda)
		rtn_period <- rtn_period + res_period*vol_period
		rtn_cum <- rtn_cum*(1+rtn_period)
		percentile_5th[idx] <- quantile(rtn_period, 0.05)
		percentile_95th[idx] <- quantile(rtn_period, 0.95)
		percentile_0_5th[idx] <- quantile(rtn_period, 0.005)
		percentile_99_5th[idx] <- quantile(rtn_period, 0.995)
	}
}

rtn_cum = rtn_cum - 1
annual_VaR_conditional <- quantile(rtn_cum, c(0.005, 0.05, 0.95, 0.995))
annual_VaR_unconditional <- qnorm(c(0.005, 0.05, 0.95, 0.995),0.0006092149*252,0.01602575*252^0.5)

plot(daily_new$Date[start_idx:(start_idx+252-1)],actual_rtn, type = "p", col="blue", pch=20, cex=0.25, main="Conditional VaR Estimation (Mar 2020 - Feb 2021)", xlab="Date", ylab="SPX return", ylim=c(-0.15,0.15))
lines(daily_new$Date[start_idx:(start_idx+252-1)],percentile_5th, type="l", lty=2, col="green", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],percentile_95th, type="l", lty=2, col="green", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],percentile_0_5th, type="l", lty=3, col="black", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],percentile_99_5th, type="l", lty=3, col="black", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],rep(0.01602575*qnorm(0.005,0,1),252), type="l", lty=4, col="brown", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],rep(0.01602575*qnorm(0.995,0,1),252), type="l", lty=4, col="brown", cex=0.25)
legend(daily_new$Date[start_idx+180], 0.15, legend=c("actual return", "5/95th percentile conditional", "0.5/99.5th percentile conditional", "0.5/99.5th percentile Unconditional"), col=c("blue", "green", "black","brown"), pch = c(20,26,26,26), lty=c(0,2,3,4), cex=0.8)



# Jump Diffusion Calibration
install.packages("DiffusionRjgqd")
library(DiffusionRjgqd)


daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
daily_fc_2008 <- daily_data[20062:20458,]
daily_great_depression <- daily_data[397:1306,]
daily_black_monday <- daily_data[14975:15291,]

#post-2020 period
daily_jump_data <- daily_new

for (i in c(1:10)){
	JGQD.remove()
	G1 <- function(t){theta[1]}
	Q1 <- function(t){theta[2]*theta[2]}
	Jmu  <- function(t){theta[4]}
	Jsig <- function(t){theta[5]}
	Lam0 <- function(t){theta[3]}
	priors <- function(theta)
	{
	 dgamma(theta[3],0.001,0.001)*dgamma(1/theta[5]^2,0.001,0.001)
	}

	# Define some starting parameters and run the MCMC:
	updates <- 200000
	burns   <- 10000
	theta   <- c(0.1,0.2,10,0,0.1)
	sds     <- c(0.1,0.05,0.2,0.01,0.01)/4
	model_1 <- JGQD.mcmc(daily_jump_data$SPX/5000.0,seq(0,length(daily_jump_data$SPX)-1)/250,mesh=20,theta=theta,sds=sds,Jdist="Normal",Jtype="Mult",
                    updates=updates,burns=burns,print.output=TRUE)
	print(i)
	gc()
	if (is.na(model_1$model.info$DIC)==FALSE){break}
}

model_estimate <- JGQD.estimates(model_1,thin=100,burns, corrmat=TRUE, acf.plot=FALSE)

i <- "new"
for (name in names(model_1)){
	if (name != "dec.matrix"){
		write.csv(model_1[name], paste0("./jdm/",i, "_", name, ".csv"))
	}
}

for (name in names(model_estimate)){
	write.csv(model_estimate[name], paste0("./jdm/",i, "_estimate_", name, ".csv"))
}


#2008 financial crisis
daily_jump_data <- daily_fc_2008

for (i in c(1:10)){
	JGQD.remove()
	G1 <- function(t){theta[1]}
	Q1 <- function(t){theta[2]*theta[2]}
	Jmu  <- function(t){theta[4]}
	Jsig <- function(t){theta[5]}
	Lam0 <- function(t){theta[3]}
	priors <- function(theta)
	{
	 dgamma(theta[3],0.001,0.001)*dgamma(1/theta[5]^2,0.001,0.001)
	}

	# Define some starting parameters and run the MCMC:
	updates <- 200000
	burns   <- 10000
	theta   <- c(-0.1,0.2,10,0,0.1)
	sds     <- c(0.1,0.05,0.2,0.01,0.01)/4
	model_1 <- JGQD.mcmc(daily_jump_data$SPX/2000.0,seq(0,length(daily_jump_data$SPX)-1)/250,mesh=20,theta=theta,sds=sds,Jdist="Normal",Jtype="Mult",
                    updates=updates,burns=burns,print.output=TRUE)
	print(i)
	gc()
	if (is.na(model_1$model.info$DIC)==FALSE){break}
}

model_estimate <- JGQD.estimates(model_1,thin=100,burns, corrmat=TRUE, acf.plot=FALSE)

i <- "fc"
for (name in names(model_1)){
	if (name != "dec.matrix"){
		write.csv(model_1[name], paste0("./jdm/",i, "_", name, ".csv"))
	}
}

for (name in names(model_estimate)){
	write.csv(model_estimate[name], paste0("./jdm/",i, "_estimate_", name, ".csv"))
}

#1987 Black Monday
daily_jump_data <- daily_black_monday

for (i in c(1:10)){
	JGQD.remove()
	G1 <- function(t){theta[1]}
	Q1 <- function(t){theta[2]*theta[2]}
	Jmu  <- function(t){theta[4]}
	Jsig <- function(t){theta[5]}
	Lam0 <- function(t){theta[3]}
	priors <- function(theta)
	{
	 dgamma(theta[3],0.001,0.001)*dgamma(1/theta[5]^2,0.001,0.001)
	}

	# Define some starting parameters and run the MCMC:
	updates <- 200000
	burns   <- 10000
	theta   <- c(0.05,0.2,10,0,0.1)
	sds     <- c(0.1,0.05,0.2,0.01,0.01)/4
	model_1 <- JGQD.mcmc(daily_jump_data$SPX/500.0,seq(0,length(daily_jump_data$SPX)-1)/250,mesh=20,theta=theta,sds=sds,Jdist="Normal",Jtype="Mult",
                    updates=updates,burns=burns,print.output=TRUE)
	print(i)
	gc()
	if (is.na(model_1$model.info$DIC)==FALSE){break}
}

model_estimate <- JGQD.estimates(model_1,thin=100,burns, corrmat=TRUE, acf.plot=FALSE)

i <- "bm"
for (name in names(model_1)){
	if (name != "dec.matrix"){
		write.csv(model_1[name], paste0("./jdm/",i, "_", name, ".csv"))
	}
}

for (name in names(model_estimate)){
	write.csv(model_estimate[name], paste0("./jdm/",i, "_estimate_", name, ".csv"))
}

#1929-1933 Great Depression
daily_jump_data <- daily_great_depression

for (i in c(1:10)){
	JGQD.remove()
	G1 <- function(t){theta[1]}
	Q1 <- function(t){theta[2]*theta[2]}
	Jmu  <- function(t){theta[4]}
	Jsig <- function(t){theta[5]}
	Lam0 <- function(t){theta[3]}
	priors <- function(theta)
	{
	 dgamma(theta[3],0.001,0.001)*dgamma(1/theta[5]^2,0.001,0.001)
	}

	# Define some starting parameters and run the MCMC:
	updates <- 200000
	burns   <- 10000
	theta   <- c(-0.35,0.4,10,0,0.1)
	sds     <- c(0.1,0.05,0.2,0.01,0.01)/4
	model_1 <- JGQD.mcmc(daily_jump_data$SPX/50.0,seq(0,length(daily_jump_data$SPX)-1)/250,mesh=20,theta=theta,sds=sds,Jdist="Normal",Jtype="Mult",
                    updates=updates,burns=burns,print.output=TRUE)
	print(i)
	gc()
	if (is.na(model_1$model.info$DIC)==FALSE){break}
}

model_estimate <- JGQD.estimates(model_1,thin=100,burns, corrmat=TRUE, acf.plot=FALSE)

i <- "gd"
for (name in names(model_1)){
	if (name != "dec.matrix"){
		write.csv(model_1[name], paste0("./jdm/",i, "_", name, ".csv"))
	}
}

for (name in names(model_estimate)){
	write.csv(model_estimate[name], paste0("./jdm/",i, "_estimate_", name, ".csv"))
}



#plot jump probability

jump_probs <- rep(NA,nrow(daily_data))
jump_probs[398:1306] <- read.csv("./jdm/gd_decode.prob.csv")$decode.prob
jump_probs[14976:15291] <- read.csv("./jdm/bm_decode.prob.csv")$decode.prob
jump_probs[20063:20458] <- read.csv("./jdm/fc_decode.prob.csv")$decode.prob
jump_probs[23104:length(jump_probs)] <- read.csv("./jdm/new_decode.prob.csv")$decode.prob

par(mfrow=c(2,1))

plot(daily_data$Date,daily_data$SPX_rtn, type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return", xlab="Date", ylab="SPX daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

plot(daily_data$Date,jump_probs, type = "p", col="blue", pch=20, cex=0.25, main="SPX Jump Probability", xlab="Date", ylab="Jump Probability")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

#calculate the percentage of jump probability greater than the threshold
threshold <- 0.5
prob_gd <- sum(read.csv("./jdm/gd_decode.prob.csv")$decode.prob>=threshold)/length(read.csv("./jdm/gd_decode.prob.csv")$decode.prob)
prob_bm <- sum(read.csv("./jdm/bm_decode.prob.csv")$decode.prob>=threshold)/length(read.csv("./jdm/bm_decode.prob.csv")$decode.prob)
prob_fc <- sum(read.csv("./jdm/fc_decode.prob.csv")$decode.prob>=threshold)/length(read.csv("./jdm/fc_decode.prob.csv")$decode.prob)
prob_new <- sum(read.csv("./jdm/new_decode.prob.csv")$decode.prob>=threshold)/length(read.csv("./jdm/new_decode.prob.csv")$decode.prob)


# Correlation Matrix

selected_vars <- c("SPX_rtn", "VIXCLS",	"T10YIE", "DFF", "DGS1", "DGS10", "BAA10Y")

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
daily_fc_2008 <- daily_data[20062:20458,]
daily_great_depression <- daily_data[397:1306,]
daily_black_monday <- daily_data[14975:15291,]

# Correlation Matrices

# Daily Data
cm_all <- cor(daily_data[,(names(daily_data) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_all, 2)

cm_extreme <- cor(daily_data[daily_data$Contraction == 1,(names(daily_data) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_extreme, 2)

cm_new <- cor(daily_new[,(names(daily_new) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_new, 2)

cm_fc_2008 <- cor(daily_fc_2008[,(names(daily_fc_2008) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_fc_2008, 2)

cm_black_monday <- cor(daily_black_monday[,(names(daily_black_monday) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_black_monday, 2)

# install.packages("reshape2")
library(reshape2)
library(ggplot2)
 
melted_corr_mat <- melt(round(cm_all,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Jul 1954 - Apr 2022)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)

melted_corr_mat <- melt(round(cm_extreme,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Recession Periods)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)

melted_corr_mat <- melt(round(cm_new,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Jan 2020 - Apr 2022)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)

melted_corr_mat <- melt(round(cm_fc_2008,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Dec 2007 – Jun 2009)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)



# Monthly data

monthly_data <- read_excel("data.xlsx", sheet="monthly_final", col_types = c(rep("numeric", 2), "date","text", rep("numeric", 19)), skip=0)

monthly_new <- monthly_data[monthly_data$Year>=2020,]
monthly_extreme <- monthly_data[monthly_data$Contraction==1,]
monthly_fc_2008 <- monthly_data[960:978,]
monthly_great_depression <- monthly_data[20:63,]
monthly_black_monday <- monthly_data[718:732,]

cm_all <- cor(monthly_data[,(names(monthly_data) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_all, 2)

cm_extreme <- cor(monthly_data[monthly_data$Contraction == 1,(names(monthly_data) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_extreme, 2)

cm_new <- cor(monthly_new[,(names(monthly_new) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_new, 2)

cm_fc_2008 <- cor(monthly_fc_2008[,(names(monthly_fc_2008) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_fc_2008, 2)

cm_black_monday <- cor(monthly_black_monday[,(names(monthly_black_monday) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_black_monday, 2)

# install.packages("reshape2")
library(reshape2)
library(ggplot2)
 
melted_corr_mat <- melt(round(cm_all,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Jul 1954 - Apr 2022)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)

melted_corr_mat <- melt(round(cm_extreme,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Recession Periods)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)

melted_corr_mat <- melt(round(cm_new,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Jan 2020 - Apr 2022)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)

melted_corr_mat <- melt(round(cm_fc_2008,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Dec 2007 – Jun 2009)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)


# Copula Simulation
# install.packages("copula")
library(copula)

n<-2000
gaussian_cop <- normalCopula(0.85)
sim_cop<- rCopula(n, gaussian_cop)
plot(sim_cop, main="Copule de Gauss", xlab="x", ylab="y", cex = 1)

t_cop <- tCopula(0.85, df=5)
sim_cop<- rCopula(n, t_cop)
plot(sim_cop, main="Copule t", xlab="x", ylab="y", cex = 1)

gumbel_cop <- gumbelCopula(3)
sim_cop<- rCopula(n, gumbel_cop)
plot(sim_cop, main="Copule de Gumbel", xlab="x", ylab="y", cex = 1)

clayton_cop <- claytonCopula(4)
sim_cop<- rCopula(n, clayton_cop)
plot(sim_cop, main="Copule de Clayton", xlab="x", ylab="y", cex = 1)

frank_cop <- frankCopula(9.5)
sim_cop<- rCopula(n, frank_cop)
plot(sim_cop, main="Copule de Frank", xlab="x", ylab="y", cex = 1)

ind_cop<-indepCopula(dim = 2)
sim_cop<- rCopula(n, ind_cop)
plot(sim_cop, main="Copule indépendante", xlab="x", ylab="y", cex = 1)

# Copula Fitting
# Choose five variables and retain complete records
selected_vars <- c("SPX_rtn", "VIXCLS",	"T10YIE", "DFF", "DGS1", "DGS10", "BAA10Y")

selected_data <- daily_data[,selected_vars]

complete_data <- selected_data[complete.cases(selected_data), ]

u <- pobs(complete_data[,!(names(complete_data) %in% c('Date', 'Contraction'))],lower.tail = FALSE)

## Maximum pseudo-likelihood method using Gaussian Copula
gaussian_cop_mpl <- fitCopula(normalCopula(dim=7, dispstr = "un"), u, method="mpl")
write.csv(coef(gaussian_cop_mpl),"gaussian_cop_estimate.csv")
write.csv(confint(gaussian_cop_mpl),"gaussian_cop_ci_estimate.csv")

## Maximum pseudo-likelihood method using t Copula
t_cop_mpl <- fitCopula(tCopula(dim=7, dispstr = "un"), u, method="mpl")
write.csv(coef(t_cop_mpl),"t_cop_estimate.csv")
write.csv(confint(t_cop_mpl),"t_cop_ci_estimate.csv")

## Maximum pseudo-likelihood method using Gumbel Copula
gumbel_cop_mpl <- fitCopula(gumbelCopula(dim=7), u, method="mpl")
coef(gumbel_cop_mpl)
confint(gumbel_cop_mpl)

## Maximum pseudo-likelihood method using Calyton Copula
clayton_cop_mpl <- fitCopula(claytonCopula(dim=7), u, method="mpl")
coef(clayton_cop_mpl)
confint(clayton_cop_mpl)

## Maximum pseudo-likelihood method using Frank Copula
frank_cop_mpl <- fitCopula(frankCopula(dim=7), u, method="ml")
coef(frank_cop_mpl)
confint(frank_cop_mpl)

## Compare copulas between implied volatility and credit spread

par(mfrow=c(2,3))
n<-3000
set.seed(123)
gaussian_cop <- normalCopula(0.619115656)
sim_cop<- rCopula(n, gaussian_cop)
plot(sim_cop, main="Gaussian Copula", xlab="x", ylab="y", cex = 0.5)

t_cop <- tCopula(0.6423154, df=12)
sim_cop<- rCopula(n, t_cop)
plot(sim_cop, main="t Copula", xlab="x", ylab="y", cex = 0.5)

gumbel_cop <- gumbelCopula(1.037)
sim_cop<- rCopula(n, gumbel_cop)
plot(sim_cop, main="Gumbel Copula", xlab="x", ylab="y", cex = 0.5)

clayton_cop <- claytonCopula(0.125)
sim_cop<- rCopula(n, clayton_cop)
plot(sim_cop, main="Clayton Copula", xlab="x", ylab="y", cex = 0.5)

frank_cop <- frankCopula(0.35)
sim_cop<- rCopula(n, frank_cop)
plot(sim_cop, main="Frank Copula", xlab="x", ylab="y", cex = 0.5)

sim_cop<- u[,c('VIXCLS','BAA10Y')]
plot(sim_cop, main="Empirical", xlab="x", ylab="y", cex = 0.5)

############################################################################
# Structured Model
###########################################################################

#Structured Model VAR
#install.packages("vars")
library(vars)

selected_vars <- c("SPX_rtn", "VIXCLS",	"T10YIE", "DFF", "DGS1", "DGS10", "BAA10Y")


#All complete data
selected_data <- daily_data[,selected_vars]
Traindata <- selected_data[complete.cases(selected_data), ]

p <- 5

var1 <- VAR(Traindata, p = p, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges
serial.test(var1, lags.pt=10, type="PT.asymptotic")

nr <- length(selected_vars)*p + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(selected_vars))
colnames(varoutput) <- selected_vars

for (i in selected_vars) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)

A<-tvaroutput[,1:length(selected_vars)]
if (p>1){
	for (i in c(2:p)){
		A <- A+tvaroutput[,(length(selected_vars)*(i-1)+1):(length(selected_vars)*i)]
	}

}
B<-tvaroutput[,length(selected_vars)*p+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans.csv")
write.csv(sapply(Traindata, mean, na.rm=TRUE),"historicalmeans.csv")

chol <- chol(summary(var1)$corres) #Cholesky decomposition of residuals
write.csv(chol,"chol.csv", row.names=FALSE)

#Pre 2020
daily_data_exclude <- daily_data[as.Date(daily_data$Date) < as.Date("2020-1-1"),]
selected_data <- daily_data_exclude[,selected_vars]
Traindata <- selected_data[complete.cases(selected_data), ]

p <- 5

var1 <- VAR(Traindata, p = p, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges
serial.test(var1, lags.pt=10, type="PT.asymptotic")

nr <- length(selected_vars)*p + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(selected_vars))
colnames(varoutput) <- selected_vars

for (i in selected_vars) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput_ex.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres_ex.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres_ex.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)

A<-tvaroutput[,1:length(selected_vars)]
if (p>1){
	for (i in c(2:p)){
		A <- A+tvaroutput[,(length(selected_vars)*(i-1)+1):(length(selected_vars)*i)]
	}

}
B<-tvaroutput[,length(selected_vars)*p+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans_ex.csv")
write.csv(sapply(Traindata, mean, na.rm=TRUE),"historicalmeans.csv")

chol <- chol(summary(var1)$corres) #Cholesky decomposition of residuals
write.csv(chol,"chol_ex.csv", row.names=FALSE)


#post-2020 data
selected_data <- daily_new[,selected_vars]
Traindata <- selected_data[complete.cases(selected_data), ]

p <- 10

var1 <- VAR(Traindata, p = p, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges
serial.test(var1, lags.pt=10, type="PT.asymptotic")

nr <- length(selected_vars)*p + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(selected_vars))
colnames(varoutput) <- selected_vars

for (i in selected_vars) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput_new.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres_new.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres_new.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)

A<-tvaroutput[,1:length(selected_vars)]
if (p>1){
	for (i in c(2:p)){
		A <- A+tvaroutput[,(length(selected_vars)*(i-1)+1):(length(selected_vars)*i)]
	}

}
B<-tvaroutput[,length(selected_vars)*p+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans_new.csv")
write.csv(sapply(Traindata, mean, na.rm=TRUE),"historicalmeans_new.csv")

chol <- chol(summary(var1)$corres) #Cholesky decomposition of residuals
write.csv(chol,"chol_new.csv", row.names=FALSE)

#2008 financial crisis data
selected_data <- daily_fc_2008[,selected_vars]
Traindata <- selected_data[complete.cases(selected_data), ]

p <- 5

var1 <- VAR(Traindata, p = p, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges
serial.test(var1, lags.pt=10, type="PT.asymptotic")

nr <- length(selected_vars)*p + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(selected_vars))
colnames(varoutput) <- selected_vars

for (i in selected_vars) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput_fc_2008.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres_fc_2008.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres_fc_2008.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)

A<-tvaroutput[,1:length(selected_vars)]
if (p>1){
	for (i in c(2:p)){
		A <- A+tvaroutput[,(length(selected_vars)*(i-1)+1):(length(selected_vars)*i)]
	}

}
B<-tvaroutput[,length(selected_vars)*p+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans_fc_2008.csv")
write.csv(sapply(Traindata, mean, na.rm=TRUE),"historicalmeans_fc_2008.csv")

chol <- chol(summary(var1)$corres) #Cholesky decomposition of residuals
write.csv(chol,"chol_fc_2008.csv", row.names=FALSE)


#STL (Loess)

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
daily_fc_2008 <- daily_data[20062:20458,]
daily_great_depression <- daily_data[397:1306,]
daily_black_monday <- daily_data[14975:15291,]

daily_new <- daily_data[397:1306,]
ts_new <-ts(daily_new$SPX/daily_new$SPX[1], start = 0, frequency = 250)
stl_result<-stl(ts_new, s.window = "per")

par(mfrow=c(3,1))
plot(stl_result$time.series[,1], main="STL: Great Depression (Aug. 1929 ~ Mar. 1933)", ylab ="Seasonality")
plot(stl_result$time.series[,2], ylab = "Trend")
plot(stl_result$time.series[,3], ylab = "Residual")

daily_new <- daily_data[14975:15475,]
ts_new <-ts(daily_new$SPX/daily_new$SPX[1], start = 0, frequency = 250)
stl_result<-stl(ts_new, s.window = "per")

par(mfrow=c(3,1))
plot(stl_result$time.series[,1], main="STL: Balck Monday (Oct. 1987 ~ Sept. 1989)", ylab ="Seasonality")
plot(stl_result$time.series[,2], ylab = "Trend")
plot(stl_result$time.series[,3], ylab = "Residual")

daily_new <- daily_data[20062:20562,]
ts_new <-ts(daily_new$SPX/daily_new$SPX[1], start = 0, frequency = 250)
stl_result<-stl(ts_new, s.window = "per")

par(mfrow=c(3,1))
plot(stl_result$time.series[,1], main="STL: Financial Crisis (Dec. 2007 ~ Nov. 2009)", ylab ="Seasonality")
plot(stl_result$time.series[,2], ylab = "Trend")
plot(stl_result$time.series[,3], ylab = "Residual")

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
ts_new <-ts(daily_new$SPX/daily_new$SPX[1], start = 0, frequency = 250)
stl_result<-stl(ts_new, s.window = "per")

par(mfrow=c(3,1))
plot(stl_result$time.series[,1], main="STL: COVID Pandemic (Jan. 2020 ~ Apr. 2022)", ylab ="Seasonality")
plot(stl_result$time.series[,2], ylab = "Trend")
plot(stl_result$time.series[,3], ylab = "Residual")