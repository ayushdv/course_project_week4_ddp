library(shiny)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(RColorBrewer)
library(tseries)
library(lubridate)
library(Quandl)
Quandl.api_key("zrcB2Ejv9UmvhPCUsy2_")
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

shinyServer(function(input, output){
    output$symbol <- renderText(input$name)
    
    output$chart1 <- renderPlot({
        
        symbol <- input$name
        ta <- input$tech_ind
        start.date <- as.Date.yearmon(input$year[1])
        end.date <- as.Date.yearmon(input$year[2])
        
        if(symbol == "Microsoft"){
            msft <- getSymbols("MSFT", auto.assign = F)
            MICROSOFT <- msft[paste(start.date,end.date,sep="::")]
            if(ta == "Bollinger Bands"){chartSeries(MICROSOFT, type = "line", TA = c(addBBands(n=20,sd=2)), theme = chartTheme('white'))}
            if(ta == "Relative Strength Index"){chartSeries(MICROSOFT, type = "line", TA = c(addRSI()), theme = chartTheme('white'))}
            if(ta == "Exponential Moving Averages"){chartSeries(MICROSOFT, type = "line", TA = c(addEMA(n=30)), theme = chartTheme('white'))}
            if(ta == "Moving Averages Convergence Divergence"){chartSeries(MICROSOFT, type = "line", TA = c(addMACD()), theme = chartTheme('white'))}
        }
        
        if(symbol == "IBM"){
            ibm <- getSymbols("IBM", auto.assign = F)
            IBM <- ibm[paste(start.date,end.date,sep="::")]
            if(ta == "Bollinger Bands"){chartSeries(IBM, type = "line", TA = c(addBBands(n=20,sd=2)), theme = chartTheme('white'))}
            if(ta == "Relative Strength Index"){chartSeries(IBM, type = "line", TA = c(addRSI()), theme = chartTheme('white'))}
            if(ta == "Exponential Moving Averages"){chartSeries(IBM, type = "line", TA = c(addEMA(n=30)), theme = chartTheme('white'))}
            if(ta == "Moving Averages Convergence Divergence"){chartSeries(IBM, type = "line", TA = c(addMACD()), theme = chartTheme('white'))}
        }
        
        if(symbol == "Netflix"){
            nflx <- getSymbols("NFLX", auto.assign = F)
            NETFLIX <- nflx[paste(start.date,end.date,sep="::")]
            if(ta == "Bollinger Bands"){chartSeries(NETFLIX, type = "line", TA = c(addBBands(n=20,sd=2)), theme = chartTheme('white'))}
            if(ta == "Relative Strength Index"){chartSeries(NETFLIX, type = "line", TA = c(addRSI()), theme = chartTheme('white'))}
            if(ta == "Exponential Moving Averages"){chartSeries(NETFLIX, type = "line", TA = c(addEMA(n=30)), theme = chartTheme('white'))}
            if(ta == "Moving Averages Convergence Divergence"){chartSeries(NETFLIX, type = "line", TA = c(addMACD()), theme = chartTheme('white'))}
        }
        
        if(symbol == "Apple Inc."){
            aapl <- getSymbols("AAPL", auto.assign = F)
            APPLE <- aapl[paste(start.date,end.date,sep="::")]
            if(ta == "Bollinger Bands"){chartSeries(APPLE, type = "line", TA = c(addBBands(n=20,sd=2)), theme = chartTheme('white'))}
            if(ta == "Relative Strength Index"){chartSeries(APPLE, type = "line", TA = c(addRSI()), theme = chartTheme('white'))}
            if(ta == "Exponential Moving Averages"){chartSeries(APPLE, type = "line", TA = c(addEMA(n=30)), theme = chartTheme('white'))}
            if(ta == "Moving Averages Convergence Divergence"){chartSeries(APPLE, type = "line", TA = c(addMACD()), theme = chartTheme('white'))}
        }
    })
    
    output$chart2 <- renderPlot({
        
        symbol <- input$name
        ta <- input$tech_ind
        start.date <- as.Date.yearmon(input$year[1])
        end.date <- as.Date.yearmon(input$year[2])
        
        if(symbol == "Microsoft"){
            msft <- getSymbols("MSFT", auto.assign = F)
            MICROSOFT <- msft[paste(start.date,end.date,sep="::")]
            MICROSOFT_DailyReturns <- dailyReturn(Ad(MICROSOFT))
            charts.PerformanceSummary(MICROSOFT_DailyReturns, main = "Microsoft: Cumulative and Daily Returns")
        }
        
        if(symbol == "IBM"){
            ibm <- getSymbols("IBM", auto.assign = F)
            IBM <- ibm[paste(start.date,end.date,sep="::")]
            IBM_DailyReturns <- dailyReturn(Ad(IBM))
            charts.PerformanceSummary(IBM_DailyReturns, main = "IBM: Cumulative and Daily Returns")
        }
        
        if(symbol == "Netflix"){
            nflx <- getSymbols("NFLX", auto.assign = F)
            NETFLIX <- nflx[paste(start.date,end.date,sep="::")]
            NETFLIX_DailyReturns <- dailyReturn(Ad(NETFLIX))
            charts.PerformanceSummary(NETFLIX_DailyReturns, main = "Netflix:  Cumulative and Daily Returns")
        }
        
        if(symbol == "Apple Inc."){
            aapl <- getSymbols("AAPL", auto.assign = F)
            APPLE <- aapl[paste(start.date,end.date,sep="::")]
            APPLE_DailyReturns <- dailyReturn(Ad(APPLE))
            charts.PerformanceSummary(APPLE_DailyReturns, main = "Apple: Cumulative and Daily Returns")
        }
    })
    
    output$chart3 <- renderPlot({
        
        symbol <- input$name
        ts <- input$trade_strat
        start.date <- as.Date.yearmon(input$year[1])
        end.date <- as.Date.yearmon(input$year[2])
        
        if(symbol == "Microsoft"){
            if(ts == "Simple Buy Filter"){
                msft <- getSymbols("MSFT", auto.assign = F)
                MICROSOFT <- msft[paste(start.date,end.date,sep="::")]
                price <- Cl(MICROSOFT)
                r <- price/Lag(price) - 1
                delta <- 0.01
                signal <- c(0)
                
                for (i in 2: length(price)){
                    if(r[i] > delta){
                        signal[i] <- 1
                    } else
                        signal[i] <- 0
                }
                
                signal<-reclass(signal,price)
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(MICROSOFT)*trade
                head(ret)
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Simple Buy Filter')
            }
            if(ts == "Simple Buy and Sell Filter"){
                msft <- getSymbols("MSFT", auto.assign = F)
                MICROSOFT <- msft[paste(start.date,end.date,sep="::")]
                price <- Cl(MICROSOFT)
                r <- price/Lag(price) - 1
                delta <- 0.01
                signal <- c(0)
                
                for (i in 2: length(price)){
                    if(r[i] > delta){
                        signal[i] <- 1
                    } 
                    if(r[i] < -delta){
                        signal[i] <- -1
                    } else
                        signal[i] <- 0
                }
                
                signal<-reclass(signal,price)
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(MICROSOFT)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Simple Buy and Sell Filter')
            }
            if(ts == "Relative Strength Index Buy Filter"){
                msft <- getSymbols("MSFT", auto.assign = F)
                MICROSOFT <- msft[paste(start.date,end.date,sep="::")]
                day <- 14
                price <- Cl(MICROSOFT)
                
                signal <- c(NULL)
                
                rsi <- RSI(price, day)
                
                signal[1:(day + 1)] <- 0
                
                for(i in (day +1):length(price)){
                    if(rsi[i] < 30){signal[i]<- 1}
                    else{signal[i]<- 0}
                }
                
                signal <- reclass(signal, Cl(MICROSOFT))
                trade <- Lag(signal, 1)
                ret <- dailyReturn(MICROSOFT)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Relative Strength Index Buy Filter')
            }
            if(ts == "Relative Strength Index Buy and Sell Filter"){
                msft <- getSymbols("MSFT", auto.assign = F)
                MICROSOFT <- msft[paste(start.date,end.date,sep="::")]
                day <- 14
                price <- Cl(MICROSOFT)
                
                signal <- c(NULL)
                
                rsi <- RSI(price, day)
                
                signal[1:(day + 1)] <- 0
                
                for(i in (day +1):length(price)){
                    if(rsi[i] < 30){signal[i]<- 1}
                    else{if(rsi[i]>70){signal[i]<- -1}
                        else{signal[i]<- 0}}
                }
                
                signal <- reclass(signal, Cl(MICROSOFT))
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(MICROSOFT)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Relative Strength Index Buy and Sell Filter')
            }
        }
        if(symbol == "IBM"){
            if(ts == "Simple Buy Filter"){
                ibm <- getSymbols("IBM", auto.assign = F)
                IBM <- ibm[paste(start.date,end.date,sep="::")]
                price <- Cl(IBM)
                r <- price/Lag(price) - 1
                delta <- 0.01
                signal <- c(0)
                
                for (i in 2: length(price)){
                    if(r[i] > delta){
                        signal[i] <- 1
                    } else
                        signal[i] <- 0
                }
                
                signal<-reclass(signal,price)
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(IBM)*trade
                head(ret)
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Simple Buy Filter')
            }
            if(ts == "Simple Buy and Sell Filter"){
                ibm <- getSymbols("IBM", auto.assign = F)
                IBM <- ibm[paste(start.date,end.date,sep="::")]
                price <- Cl(IBM)
                r <- price/Lag(price) - 1
                delta <- 0.01
                signal <- c(0)
                
                for (i in 2: length(price)){
                    if(r[i] > delta){
                        signal[i] <- 1
                    } 
                    if(r[i] < -delta){
                        signal[i] <- -1
                    } else
                        signal[i] <- 0
                }
                
                signal<-reclass(signal,price)
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(IBM)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Simple Buy and Sell Filter')
            }
            if(ts == "Relative Strength Index Buy Filter"){
                ibm <- getSymbols("IBM", auto.assign = F)
                IBM <- ibm[paste(start.date,end.date,sep="::")]
                day <- 14
                price <- Cl(IBM)
                
                signal <- c(NULL)
                
                rsi <- RSI(price, day)
                
                signal[1:(day + 1)] <- 0
                
                for(i in (day +1):length(price)){
                    if(rsi[i] < 30){signal[i]<- 1}
                    else{signal[i]<- 0}
                }
                
                signal <- reclass(signal, Cl(IBM))
                trade <- Lag(signal, 1)
                ret <- dailyReturn(IBM)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Relative Strength Index Buy Filter')
            }
            if(ts == "Relative Strength Index Buy and Sell Filter"){
                ibm <- getSymbols("IBM", auto.assign = F)
                IBM <- ibm[paste(start.date,end.date,sep="::")]
                day <- 14
                price <- Cl(IBM)
                
                signal <- c(NULL)
                
                rsi <- RSI(price, day)
                
                signal[1:(day + 1)] <- 0
                
                for(i in (day +1):length(price)){
                    if(rsi[i] < 30){signal[i]<- 1}
                    else{if(rsi[i]>70){signal[i]<- -1}
                        else{signal[i]<- 0}}
                }
                
                signal <- reclass(signal, Cl(IBM))
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(IBM)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Relative Strength Index Buy and Sell Filter')
            }
        }
        if(symbol == "Netflix"){
            if(ts == "Simple Buy Filter"){
                nflx <- getSymbols("NFLX", auto.assign = F)
                NETFLIX <- nflx[paste(start.date,end.date,sep="::")]
                price <- Cl(NETFLIX)
                r <- price/Lag(price) - 1
                delta <- 0.01
                signal <- c(0)
                
                for (i in 2: length(price)){
                    if(r[i] > delta){
                        signal[i] <- 1
                    } else
                        signal[i] <- 0
                }
                
                signal<-reclass(signal,price)
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(NETFLIX)*trade
                head(ret)
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Simple Buy Filter')
            }
            if(ts == "Simple Buy and Sell Filter"){
                nflx <- getSymbols("NFLX", auto.assign = F)
                NETFLIX <- nflx[paste(start.date,end.date,sep="::")]
                price <- Cl(NETFLIX)
                r <- price/Lag(price) - 1
                delta <- 0.01
                signal <- c(0)
                
                for (i in 2: length(price)){
                    if(r[i] > delta){
                        signal[i] <- 1
                    } 
                    if(r[i] < -delta){
                        signal[i] <- -1
                    } else
                        signal[i] <- 0
                }
                
                signal<-reclass(signal,price)
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(NETFLIX)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Simple Buy and Sell Filter')
            }
            if(ts == "Relative Strength Index Buy Filter"){
                nflx <- getSymbols("NFLX", auto.assign = F)
                NETFLIX <- nflx[paste(start.date,end.date,sep="::")]
                day <- 14
                price <- Cl(NETFLIX)
                
                signal <- c(NULL)
                
                rsi <- RSI(price, day)
                
                signal[1:(day + 1)] <- 0
                
                for(i in (day +1):length(price)){
                    if(rsi[i] < 30){signal[i]<- 1}
                    else{signal[i]<- 0}
                }
                
                signal <- reclass(signal, Cl(NETFLIX))
                trade <- Lag(signal, 1)
                ret <- dailyReturn(NETFLIX)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Relative Strength Index Buy Filter')
            }
            if(ts == "Relative Strength Index Buy and Sell Filter"){
                nflx <- getSymbols("NFLX", auto.assign = F)
                NETFLIX <- nflx[paste(start.date,end.date,sep="::")]
                day <- 14
                price <- Cl(NETFLIX)
                
                signal <- c(NULL)
                
                rsi <- RSI(price, day)
                
                signal[1:(day + 1)] <- 0
                
                for(i in (day +1):length(price)){
                    if(rsi[i] < 30){signal[i]<- 1}
                    else{if(rsi[i]>70){signal[i]<- -1}
                        else{signal[i]<- 0}}
                }
                
                signal <- reclass(signal, Cl(NETFLIX))
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(NETFLIX)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Relative Strength Index Buy and Sell Filter')
            }
        }
        if(symbol == "Apple Inc."){
            if(ts == "Simple Buy Filter"){
                aapl <- getSymbols("AAPL", auto.assign = F)
                APPLE <- aapl[paste(start.date,end.date,sep="::")]
                price <- Cl(APPLE)
                r <- price/Lag(price) - 1
                delta <- 0.01
                signal <- c(0)
                
                for (i in 2: length(price)){
                    if(r[i] > delta){
                        signal[i] <- 1
                    } else
                        signal[i] <- 0
                }
                
                signal<-reclass(signal,price)
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(APPLE)*trade
                head(ret)
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Simple Buy Filter')
            }
            if(ts == "Simple Buy and Sell Filter"){
                aapl <- getSymbols("AAPL", auto.assign = F)
                APPLE <- aapl[paste(start.date,end.date,sep="::")]
                price <- Cl(APPLE)
                r <- price/Lag(price) - 1
                delta <- 0.01
                signal <- c(0)
                
                for (i in 2: length(price)){
                    if(r[i] > delta){
                        signal[i] <- 1
                    } 
                    if(r[i] < -delta){
                        signal[i] <- -1
                    } else
                        signal[i] <- 0
                }
                
                signal<-reclass(signal,price)
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(APPLE)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Simple Buy and Sell Filter')
            }
            if(ts == "Relative Strength Index Buy Filter"){
                aapl <- getSymbols("AAPL", auto.assign = F)
                APPLE <- aapl[paste(start.date,end.date,sep="::")]
                day <- 14
                price <- Cl(APPLE)
                
                signal <- c(NULL)
                
                rsi <- RSI(price, day)
                
                signal[1:(day + 1)] <- 0
                
                for(i in (day +1):length(price)){
                    if(rsi[i] < 30){signal[i]<- 1}
                    else{signal[i]<- 0}
                }
                
                signal <- reclass(signal, Cl(APPLE))
                trade <- Lag(signal, 1)
                ret <- dailyReturn(APPLE)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Relative Strength Index Buy Filter')
            }
            if(ts == "Relative Strength Index Buy and Sell Filter"){
                aapl <- getSymbols("AAPL", auto.assign = F)
                APPLE <- aapl[paste(start.date,end.date,sep="::")]
                day <- 14
                price <- Cl(APPLE)
                
                signal <- c(NULL)
                
                rsi <- RSI(price, day)
                
                signal[1:(day + 1)] <- 0
                
                for(i in (day +1):length(price)){
                    if(rsi[i] < 30){signal[i]<- 1}
                    else{if(rsi[i]>70){signal[i]<- -1}
                        else{signal[i]<- 0}}
                }
                
                signal <- reclass(signal, Cl(APPLE))
                trade <- Lag(signal, 1)
                
                ret <- dailyReturn(APPLE)*trade
                names(ret) <- 'filter'
                charts.PerformanceSummary(ret, main = 'Relative Strength Index Buy and Sell Filter')
            }
        }
    })
})