# library(ggplot2)   # produce good looking graphs (qplot)
library(quantmod)  # allows to easily import data directly from downloading financial data from the internet, directly
# from some open sources, including Yahoo Finance, Google Finance, and the Federal
# Reserve Economic Data (FRED) of Federal Reserve Bank of St. Louis.
library(xts)
library(readr)
library(latex2exp) # to wtie latex formulas in graphs!
#library(gridExtra) # multiple plots in one graph
library(summarytools)
library(qwraps2)
library(normtest)
library(nortest)
library(moments)
library(xtable)
library(sm)
library(astsa)
library(portes)
#library(xlsx)
# library(timeSeries)
library(forecast)
# library(forecast)
library(portes)

NVDA <- getSymbols("NVDA",from="1999-11-29", to="2020-12-31", auto.assign=FALSE)

# daily prices
Pt.d  <- NVDA$NVDA.Adjusted ; names(Pt.d)  <- "Pt.d" # Prices
pt.d  <- log(Pt.d)       ; names(Pt.d)  <- "pt.d" # log -prices

# find end of month/week/year dates
last_day_of_month <- endpoints(pt.d, on = "months") 
last_day_of_week  <- endpoints(pt.d, on = "weeks")
last_day_of_year  <- endpoints(pt.d, on = "years")

# Compute weekly (w), monthly(m), and annual(y) log prices
pt.w <- pt.d[last_day_of_week] ; names(pt.w)   <- "pt.w.all" 
pt.m <- pt.d[last_day_of_month]; names(pt.m)   <- "pt.m.all"
pt.y <- pt.d[last_day_of_year] ; names(pt.y)   <- "pt.y.all"

# compute log returns # for entire history of S&P 500
rt.d  <- diff(pt.d)     ; names(rt.d)  <- "rt.d"
rt.w  <- diff(pt.w)     ; names(rt.w)  <- "rt.w"
rt.m  <- diff(pt.m)     ; names(rt.m)  <- "rt.m"
rt.y  <- diff(pt.y)     ; names(rt.y)  <- "rt.y"

# convert prices int dataframes to produce nice plots
Pt.d.df <- cbind(index(Pt.d), data.frame(Pt.d)); names(Pt.d.df)[1] <- "date";
pt.d.df <- cbind(index(pt.d), data.frame(pt.d)); names(pt.d.df)[1] <- "date";
pt.w.df <- cbind(index(pt.w), data.frame(pt.w)); names(pt.w.df)[1] <- "date";
rt.d.df <- cbind(index(rt.d), data.frame(rt.d)); names(rt.d.df)[1] <- "date";
rt.w.df <- cbind(index(rt.w), data.frame(rt.w)); names(rt.w.df)[1] <- "date";
rt.m.df <- cbind(index(rt.m), data.frame(rt.m)); names(rt.m.df)[1] <- "date";
rt.y.df <- cbind(index(rt.y), data.frame(rt.y)); names(rt.y.df)[1] <- "date";

# personalized table of summary statistics
# creates a 'list' with the data of SP500 sampled at different frequencies
# a list is needed and not a matrix / dataframe as the vectors have different lengths!!!
X <-list("Daily" = rt.d.df[-1,2],
         "Weekly" = rt.w.df[-1,2],
         "Montly" = rt.m.df[-1,2],
         "Annual" = rt.y.df[-1,2]);

#  Create function (named 'multi.fun' which computes the statics that we want on the inpit 'x')
###############################################
multi.fun <- function(x) {
  c(Mean = mean(x)*100, 
    St.Deviation = sd(x)*100,
    Diameter.C.I.Mean =  qnorm(0.975)*sqrt(var(x)/length(x))*100,
    Skewness=moments::skewness(x),
    Kurtosis=moments::kurtosis(x),
    Excess.Kurtosis=moments::kurtosis(x)-3,
    Min    = min(x)*100, 
    Quant  = quantile(x, probs = 0.05)*100,
    Quant  = quantile(x, probs = 0.25)*100,
    Median = quantile(x, probs = 0.50)*100,
    Quant  = quantile(x, probs = 0.75)*100,
    Quant  = quantile(x, probs = 0.95)*100,
    Max    = max(x)*100,
    Jarque.Bera.stat = jarque.bera.test(x)$statistic,
    Jarque.Bera.pvalue.X100 = jarque.bera.test(x)$p.value*100,
    Lillie.test.stat = lillie.test(x)$statistic,
    Lillie.test.pvalue.X100 = lillie.test(x)$p.value*100,
    N.obs = length(x) 
  )}

# GENRETAES TABLE 1 in slide 91
a <- sapply(X, multi.fun) # apply function to all elements of list X, 
# PRINT TABLE 1 ON R console
print(a)
# and return results in a nice and tidy table
round(a, digits = 5) # show nicer-looking table

#Stylized fact 1

#dailylogprice
lwd2plot  <- c(1    , 1     , 1    )
lty2plot  <- c(1    , 1     , 1    )
plot(x = pt.d.df[,1], y = pt.d.df[,2], type = 'l', col="dodgerblue1"   , lty = lty2plot[1], lwd = lwd2plot[1],
     xlab=""  , ylab=TeX('log-price: $p_t$'), main="Daily 'Adjusted closing' of log prices",   xaxt ="none")     # do not diaply x and y-axes labels/ticks) 
# X-Axis display
seq_sel <- endpoints(pt.d.df$date, on = 'years'); date_seq = pt.d.df$date[seq_sel]; date_lab = format(date_seq,"%b-%y")
axis(1, at = date_seq, label = date_lab, las = 1, cex.axis=1.0)
abline(0,0, lty = 2)

#Daily log price

# scatterplot of log prices: p_t vs. p_{t-1}
plot(Lag(pt.d.df[,2]), pt.d.df[,2], col = "dodgerblue", lwd = 1, cex = 2, xlab=TeX('LAGGED daily log-price ($p_{t-1}$)')  , ylab=TeX('daily log-price ($p_{t}$)'))
abline(0, 1, lty = 1, lwd = 2, col="violetred1")

#ACF
acf(pt.d, main="Autocorrelations of the daily Prices")


#Stylized fact 2
plot(x = rt.d.df[,1], y = rt.d.df[,2], type = 'l', col="springgreen2"   , lty = lty2plot[1], lwd = lwd2plot[1],
     xlab=""  , ylab=TeX('log-price: $p_t$'), main="Daily 'Adjusted closing' of log returns",   xaxt ="none")     # do not diaply x and y-axes labels/ticks) 
# X-Axis display
seq_sel <- endpoints(rt.d.df$date, on = 'years'); date_seq = rt.d.df$date[seq_sel]; date_lab = format(date_seq,"%b-%y")
axis(1, at = date_seq, label = date_lab, las = 1, cex.axis=1.0)
abline(0,0, lty = 2)

#Histograms and QQ-plots
par(mfrow=c(2,2))

# daily returns 
hist_OUT <- hist(rt.d.df[,2], freq = FALSE, breaks = 50, col="skyblue1",  xlab="", main=TeX('Daily log-return'), )
norm_y <-  dnorm(hist_OUT$mids, mean=mean(rt.d.df[,2], na.rm=TRUE), sd=sd(rt.d.df[,2], na.rm=TRUE));
lines(x=hist_OUT$mids, y=norm_y,col="violetred1", lwd=1)

# monthly returns 
hist_OUT <- hist(rt.m.df[,2], freq = FALSE, breaks = 50, col="skyblue1",  xlab="", main=TeX('Monthly log-return'), )
norm_y <-  dnorm(hist_OUT$mids, mean=mean(rt.m.df[,2], na.rm=TRUE), sd=sd(rt.m.df[,2], na.rm=TRUE));
lines(x=hist_OUT$mids, y=norm_y,col="violetred1", lwd=1)

# QQ-plot vs quantiles of normal distribution
qqnorm(rt.d.df[,2], pch = 1, frame = FALSE, xlab="Normal Quantiles")
qqline(rt.d.df[,2], col = "skyblue1", lwd = 2)

qqnorm(rt.m.df[,2], pch = 1, frame = FALSE, xlab="Normal Quantiles")
qqline(rt.m.df[,2], col = "skyblue1", lwd = 2)

# ACF of returns: Autocorrelation function
par(mfrow=c(1,3))
lag.max.acf = 40;  lim.y.axes = c(-0.08,0.08)
# daily returns 
Acf(rt.d.df[-1,2], main=TeX('Daily returns : $r_t$'), lag.max = lag.max.acf, xlab = "Lag in days", ylim=lim.y.axes, xlim = c(1,40))
# weekly returns 
Acf(rt.w.df[-1,2], main=TeX('Weekly returns : $r_t^w$'), lag.max = lag.max.acf, xlab = "Lag in weeks", ylim=lim.y.axes, xlim = c(1,40))
 # monthly returns 
Acf(rt.m.df[-1,2], main=TeX('Monthly returns : $r_t^m$'), lag.max = lag.max.acf, xlab = "Lag in years", ylim=lim.y.axes, xlim = c(1,40))


#dailylogprice&squared MIN
par(mfrow=c(1,2))
lwd2plot  <- c(1    , 1     , 1    )
lty2plot  <- c(1    , 1     , 1    )
plot(x = rt.d.df[,1], y = rt.d.df[,2], type = 'l', col="dodgerblue1"   , lty = lty2plot[1], lwd = lwd2plot[1],
     xlab=""  , ylab=TeX('log-price: $p_t$'), main="Daily 'Adjusted closing' of log prices",   xaxt ="none")     # do not diaply x and y-axes labels/ticks) 
# X-Axis display
seq_sel <- endpoints(rt.d.df$date, on = 'years'); date_seq = rt.d.df$date[seq_sel]; date_lab = format(date_seq,"%b-%y")
axis(1, at = date_seq, label = date_lab, las = 1, cex.axis=1.0)
abline(0,0, lty = 2)


plot(x = rt.d.df[,1], y = (rt.d.df[,2]^2), type = 'l', col="springgreen2"   , lty = lty2plot[1], lwd = lwd2plot[1],
     xlab=""  , ylab=TeX('squared log-price: $p_t$'), main="Daily 'Adjusted closing' of squared log prices",   xaxt ="none")     # do not diaply x and y-axes labels/ticks) 
# X-Axis display
seq_sel <- endpoints(rt.d.df$date, on = 'years'); date_seq = rt.d.df$date[seq_sel]; date_lab = format(date_seq,"%b-%y")
axis(1, at = date_seq, label = date_lab, las = 1, cex.axis=1.0)
abline(0,0, lty = 2)


# Daily log-returns and squared returns on a recent subsample PROF
par(mfrow=c(1,2))

plot(x = rt.d.df[,1], y = rt.d.df[,2], type = 'l', col="dodgerblue1"   , lty = lty2plot[1], lwd = lwd2plot[1],
     xlab=""  , ylab=TeX('$r_t$'),    main=TeX('Daily log-return'),   xaxt ="none")     # do not diaply x and y-axes labels/ticks) 
# X-Axis display
seq_sel <- endpoints(rt.d.df$date, on = 'years'); date_seq = rt.d.df$date[seq_sel]; date_lab = format(date_seq,"%y")
axis(1, at = date_seq, label = date_lab, las = 1, cex.axis=1.0); abline(0,0, lty = 2) # add zero line

plot(x = rt.d.df[,1], y = (rt.d.df[,2])^2, type = 'l', col="springgreen2"   , lty = lty2plot[1], lwd = lwd2plot[1],
     xlab=""  , ylab=TeX('$r_t^2$'),    main=TeX('Daily squared log-return'),   xaxt ="none")     # do not diaply x and y-axes labels/ticks) 
# X-Axis display
seq_sel <- endpoints(rt.d.df$date, on = 'years'); date_seq = rt.d.df$date[seq_sel]; date_lab = format(date_seq,"%y")
axis(1, at = date_seq, label = date_lab, las = 1, cex.axis=1.0); abline(0,0, lty = 2) # add zero line


#ACF of squared returns
par(mfrow=c(1,3))
lag.max.acf = 80;  lim.y.axes = c(-0.10,0.30)
data2plot = (rt.d.df[,2])^2 ; # daily squared returns 
Acf(data2plot, main=TeX('Daily squared returns : $r_t^2$'), lag.max = lag.max.acf, xlab = "lag in days", ylim=lim.y.axes)
data2plot = (rt.w.df[,2])^2; # weekly squared returns 
Acf(data2plot, main=TeX('Weekly squared returns : $r_t^{w 2}$'), lag.max = lag.max.acf, xlab = "lag in weeks", ylim=lim.y.axes)
data2plot = (rt.m.df[,2])^2; # monthly squared returns 
Acf(data2plot, main=TeX('Monthly squared returns : $r_t^{m 2}$'), lag.max = lag.max.acf, xlab = "lag in years", ylim=lim.y.axes)

# ACF of absolute returns
lag.max.acf = 80;  lim.y.axes = c(-0.10,0.30)
data2plot = abs(rt.d.df[,2]) ; # daily abs returns 
Acf(data2plot, main=TeX('Daily absolute returns : $|r_t|$'), lag.max = lag.max.acf, xlab = "lag in days", ylim=lim.y.axes)
data2plot = abs(rt.w.df[,2]); # weekly abs returns 
Acf(data2plot, main=TeX('Weekly absolute returns : $|r_t^w|$'), lag.max = lag.max.acf, xlab = "lag in weeks", ylim=lim.y.axes)
data2plot = abs(rt.m.df[,2]); # monthly abs returns 
Acf(data2plot, main=TeX('Monthly absolute returns : $|r_t$^m|'), lag.max = lag.max.acf, xlab = "lag in years", ylim=lim.y.axes)

ret   = (rt.d.df[-1,2])   ; # daily returns 
ret2  = (rt.d.df[-1,2])^2 ; # daily SQUARED returns 

# Cross correaltion plot in the slides
par(mfrow=c(1,1))
ss_dates3 <- "19880101/20181231"
rt.d.subsamp3 <- rt.d.all[ss_dates3];
ret  <- as.numeric(rt.d.subsamp3); ret2 <- ret^2
ccf(ret, ret2, lag.max = 10, type = "correlation",  plot = TRUE, 
    main=TeX('Cross-coorrelation between daily $r_{t+j}$ and $r_t^2$ = corr($r_{t+j}$, $r_{t}^2$)'), 
    xlab = TeX('lag $j$ in days'), ylab=TeX('Cross-correlation'))


