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

shinyUI(fluidPage(
    titlePanel("Tabs!"),
    sidebarLayout(
        sidebarPanel(
            selectInput("name", "Select Company:", c("Microsoft", "IBM", "Netflix", "Apple Inc.")),
            sliderInput("year", "Select Time Range:", min = 2008, max = 2020, value = c(2012, 2016), sep = "", pre = "Year "),
            selectInput("tech_ind", "Select Technical Indicators:", c("Bollinger Bands", "Relative Strength Index", 
                                                                             "Exponential Moving Averages", 
                                                                             "Moving Averages Convergence Divergence")),
            selectInput("trade_strat", "Select Trading Strategy:", c("Simple Buy Filter", "Simple Buy and Sell Filter", 
                                                                             "Relative Strength Index Buy Filter", 
                                                                             "Relative Strength Index Buy and Sell Filter")),
            submitButton("Chart!")
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Documentation", br(), strong("Select Company: "), "Select a company from the given list of 4 to chart.",
                                 br(), strong("Select Time Range: "), "Choose for which years [from 2008 to 2020] should the charts be plotted",
                                 br(), strong("Select Technical Indicators: "), "Choose which Technical Indicators should be added to the plot", 
                                 br(), strong("Select Trading Strategy: "), "Select which Trading Strategy should be used", 
                                 br(), "The charts are in the 'Charts' tab, while the trading strategies are in the 'Trading Strategies' tab"),
                        tabPanel("Charts", br(), "The selected company is: ", strong(textOutput("symbol")), 
                                 br(), plotOutput("chart1"), br(), plotOutput("chart2")),
                        tabPanel("Trading Strategies", br(), plotOutput("chart3"))
            )
        )
    )
))