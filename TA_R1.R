library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

#Charting
getSymbols("AMZN",from = "2008-08-01",to = "2018-08-20")
AMZN_log_returns <- AMZN%>%Ad()%>%dailyReturn(type = "log")

AMZN%>%Ad()%>%chartSeries()
AMZN%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2018')

#Log Return
getSymbols("FB",from="2008-08-01",to="2018-08-20")
FB_log_returns <- FB%>%Ad()%>%dailyReturn(type = "log")

getSymbols("TSLA",from="2008-08-01",to="2018-08-20")
TSLA_log_returns <- TSLA%>%dailyReturn(type = "log")

getSymbols("AAPL", from="2008-08-01",to = "2018-08-20")
AAPL_log_returns <- AAPL%>%dailyReturn(type = "log")

getSymbols("IBM",from="2008-08-01", to = "2018-08-20")
IBM_log_returns <- dailyReturn(IBM,type = "log")

# Mean Log Return
AMZN_mean_log <- mean(AMZN_log_returns)
FB_mean_log <- mean(FB_log_returns)
TSLA_mean_log <- mean(TSLA_log_returns)
AAPL_mean_log <- mean(AAPL_log_returns)
IBM_mean_log <- mean(IBM_log_returns)

#Rounding
mean_log <- c(AMZN_mean_log,FB_mean_log,TSLA_mean_log,AAPL_mean_log,IBM_mean_log)
mean_log <- round(mean_log,4)
mean_log

# Standard Deviation
AMZN_sd <- round(sd(AMZN_log_returns),4)
FB_sd <- round(sd(FB_log_returns),4)
TSLA_sd <- round(sd(TSLA_log_returns),4)
AAPL_sd <- round(sd(AAPL_log_returns),4)
IBM_sd <- round(sd(IBM_log_returns),4)
sd_log <- c(AMZN_sd,FB_sd,TSLA_sd,AAPL_sd,IBM_sd)

#Create Data Frame
df1 <- data.frame(rbind(c("AMZN",AMZN_mean_log,AMZN_sd),c("FB",FB_mean_log,FB_sd),c("TSLA",TSLA_mean_log,TSLA_sd),
      c("AAPL",AAPL_mean_log,AAPL_sd),c("IBM",IBM_mean_log,IBM_sd)),stringsAsFactors=FALSE)
df1 <- data.frame(mean_log,sd_log)
rownames(df1) <- c("AMZN","FB","TSLA","AAPL","IBM")
colnames(df1) <- c("Mean_Log_return","Sd_Log_return")
df1



chartSeries(AMZN$AMZN.Close, TA = c(addBBands(),addBBands(draw = "p"),addMACD(),addVo()), subset = "2018")

chartSeries(FB$FB.Close, TA = c(addBBands(),addBBands(draw = "p"),addMACD(),addVo()), subset = "2018")

chartSeries(TSLA$TSLA.Close, TA = c(addBBands(),addBBands(draw = "p"),addMACD(),addVo()), subset = "2018")

chartSeries(AAPL$AAPL.Close, TA = c(addBBands(),addBBands(draw = "p"),addMACD(),addVo()), subset = "2018")

chartSeries(IBM$IBM.Close, TA = c(addBBands(),addBBands(draw = "p"),addMACD(),addVo()), subset = "2018")
