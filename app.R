#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny, plotly, TSstudio, tsbox, forecast, dynlm, corrplot, readxl, zoo)
dt <- read_excel("dt_usa7.xlsx")
dt$...1 <- NULL

dt$inf_defl_ld <- c(NA,diff(log(dt$inf_defl))*100)
dt$rgdp_ld <- c(NA,diff(log(dt$rgdp))*100)
dt$ngdpld <- c(NA,diff(log(dt$ngdp))*100)
dt$prod_roh1_ld <- c(NA,diff(log(dt$prod_roh1))*100)
dt$wages_ld <- c(NA,diff(log(dt$wages))*100)
dt$m2_ld <- c(NA,diff(log(dt$m2))*100)
dt$inf_cpi_nofe_ld <- c(NA,diff(log(dt$inf_cpi_nofe))*100)
dt$prod_tfp_ld <- c(NA,diff(log(dt$prod_tfp))*100)
dt$unemplev_ld <- c(NA,diff(log(dt$unemplev))*100)
dt$interest_rate_ld <- c(NA,diff(log(dt$interest_rate))*100)
dt$m0_ld <- c(NA,diff(log(dt$m0))*100)
dt$vel_m_ld <- c(NA,diff(log(dt$vel_m))*100)

inf_defl <- ts(dt$inf_defl, start=1968, end=2019)
rgdp <- ts(dt$rgdp, start=1968, end=2019)
ngdp <- ts(dt$ngdp, start=1968, end=2019)
wages <- ts(dt$wages, start=1968, end=2019)
m2 <- ts(dt$m2, start=1968, end=2019)
unemprate_civ <- ts(dt$unemprate_civ[-1], start=1969, end=2019)
prod_roh1 <- ts(dt$prod_roh1, start=1968, end=2019)
prod_roh2 <-ts(dt$prod_roh2[-1], start=1969, end=2019)
inf_cpi_nofe <- ts(dt$inf_cpi_nofe, start=1968, end=2019)
inf_cpi_stick <- ts(dt$inf_cpi_stick[-1], start=1969, end=2019)
prod_tfp <- ts(dt$prod_tfp, start=1968, end=2019)
unemplev <- ts(dt$unemplev, start=1968, end=2019)
interest_rate <- ts(dt$interest_rate, start=1968, end=2019)
m0 <- ts(dt$interest_rate, start=1968, end=2019)


date<-ts(dt$date[2:52], start=1969, end=2019)
inf_defl_ld <- ts(dt$inf_defl_ld[-1], start=1969, end=2019)
rgdp_ld <- ts(dt$rgdp_ld[-1], start=1969, end=2019)
ngdpld <- ts(dt$ngdpld[-1], start=1969, end=2019)
prod_roh1_ld <- ts(dt$prod_roh1_ld[-1], start=1969, end=2019)
wages_ld <- ts(dt$wages_ld[-1], start=1969, end=2019)
m2_ld <- ts(dt$m2_ld[-1], start=1969, end=2019)
inf_cpi_nofe_ld <- ts(dt$inf_cpi_nofe_ld[-1], start=1969, end=2019)
prod_tfp_ld <- ts(dt$prod_tfp_ld[-1], start=1969, end=2019)
unemplev_ld <- ts(dt$unemplev_ld[-1], start=1969, end=2019)
Empty<-ts(dt$Empty[-1],start=1969,end=2019)
interest_rate_ld <- ts(dt$interest_rate_ld[-1], start=1969, end=2019)
m0_ld <- ts(dt$m0_ld[-1], start=1969, end=2019)
ma_m2_ld <- rollmean(dt$m2_ld , 2, align = "right")
ma_m2_ld <- ts(ma_m2_ld, start=1969, end=2019)
ma_prod_roh1_ld <- rollmean(dt$prod_roh1_ld , 2, align = "right")
ma_prod_roh1_ld <- ts(ma_prod_roh1_ld, start=1969, end=2019)
ma_prod_roh2 <- rollmean(dt$prod_roh2 , 2, align = "right")
ma_prod_roh2 <- ts(ma_prod_roh2, start=1969, end=2019)
ma_prod_tfp_ld <- rollmean(dt$prod_tfp_ld , 2, align = "right")
ma_prod_tfp_ld <- ts(ma_prod_tfp_ld, start=1969, end=2019)
vel_m_ld <- ts(dt$vel_m_ld[-1], start=1969, end=2019)

dtts<-ts_c(date,inf_defl_ld,rgdp_ld,ngdpld,prod_roh1_ld,wages_ld,m2_ld,inf_cpi_nofe_ld,inf_cpi_stick,prod_tfp_ld,unemplev_ld,unemprate_civ,prod_roh2,Empty,interest_rate,m0_ld,ma_m2_ld,ma_prod_roh1_ld,ma_prod_roh2,ma_prod_tfp_ld,vel_m_ld)


#User Interface--------------------------------------------------------------------------------
ui <- navbarPage("Masked inflation",
                 
                 tabPanel("Pre-analysis of USA data",
                          
                          sidebarLayout(
                            sidebarPanel(width=4,
                                         column(10,
                                                helpText(div(h3(strong("Time series selection")), style="color:black"),div("Choose the time series to be plotted"), style="color:black"),
                                                helpText(h4("1) Selection of the ",strong("first time series")," and of its", strong("lag")), style="color:black"),
                                                selectInput("varselect1", label = h3(""),
                                                            choices = c("Inflation rate (GDP deflator)", "Inflation rate (CPI NOFE)","Inflation rate (CPI STICK)","Productivity growth (prod_roh1)","2Y backward MA of prod_roh1 growth","Productivity growth (prod_roh2)","2Y backward MA of prod_roh2 growth","Productivity growth (prod_tfp)","2Y backward MA of prod_tfp growth","M2 aggregate growth","2Y backward MA of M2 growth","M0 aggregate growth","interest rate","Wages growth","Real output growth","Nominal output growth","Unemployment rate (unemprate_civ)","Unemployment rate growth (unemplev_ld)","Velocity of money"),
                                                            selected = "2Y backward MA of M2 growth"),
                                                sliderInput("lagselection1", h3(""),
                                                            min = 0, max = 5, value = 0, step = 1,round = TRUE),
                                                helpText(h4("2) Selection of the ",strong("second time series")," and of its", strong("lag")), style="color:black"),
                                                selectInput("varselect2", label = h3(""),
                                                            choices = c("Inflation rate (GDP deflator)", "Inflation rate (CPI NOFE)","Inflation rate (CPI STICK)","Productivity growth (prod_roh1)","2Y backward MA of prod_roh1 growth","Productivity growth (prod_roh2)","2Y backward MA of prod_roh2 growth","Productivity growth (prod_tfp)","2Y backward MA of prod_tfp growth","M2 aggregate growth","2Y backward MA of M2 growth","M0 aggregate growth","interest rate","Wages growth","Real output growth","Nominal output growth","Unemployment rate (unemprate_civ)","Unemployment rate growth (unemplev_ld)","Velocity of money"),
                                                            selected = "Inflation rate (GDP deflator)"),
                                                sliderInput("lagselection2", h3(""),
                                                            min = 0, max = 5, value = 0, step = 1,round = TRUE),
                                                helpText(h4("3) Selection of the ",strong("third time series")," and of its", strong("lag")), style="color:black"),
                                                selectInput("varselect3", label = h3(""),
                                                            choices = c("Inflation rate (GDP deflator)", "Inflation rate (CPI NOFE)","Inflation rate (CPI STICK)","Productivity growth (prod_roh1)","2Y backward MA of prod_roh1 growth","Productivity growth (prod_roh2)","2Y backward MA of prod_roh2 growth","Productivity growth (prod_tfp)","2Y backward MA of prod_tfp growth","M2 aggregate growth","2Y backward MA of M2 growth","M0 aggregate growth","interest rate","Wages growth","Real output growth","Nominal output growth","Unemployment rate (unemprate_civ)","Unemployment rate growth (unemplev_ld)","Velocity of money","Empty"),
                                                            selected = "Empty"),
                                                sliderInput("lagselection3", h3(""),
                                                            min = 0, max = 5, value = 0, step = 1,round = TRUE),
                                                helpText(div(h3(strong("Time period selection")), style="color:black"),div("Choose the sample period you want.",br(),"The default range cover the whole available sample period. Only the 1969 is not selected to allow the crosscorrelation function to work also with MA series."), style="color:black"),
                                                sliderInput("periodslider1", h3(""),
                                                            min = 1969, max = 2019, value = c(1970,2019), step = 1,round = TRUE))
                                         
                            ),
                            
                            
                            mainPanel(width=8,
                                      htmlOutput("timeseriedescri"),
                                      br(),
                                      plotlyOutput("scatterplot"),
                                      br(),
                                      htmlOutput("corrtitle"),
                                      plotOutput("correlations"),
                                      htmlOutput("corr_est"))
                          )),
                 
                 tabPanel("Kiley replication",
                          
                          sidebarLayout(
                            sidebarPanel(width=4,
                                         column(10,
                                                helpText(div(h3(strong("Time series selection")), style="color:black"),div("Choose the inflation rate and productivity growth time series.",br(),"There are three possible series for each.",br(),"It will change the serie in all the equations in which it appears."), style="color:black"),
                                                helpText(h4(strong("Inflation rate time series")), style="color:black"),
                                                selectInput("varselectINF", label = h3(""),
                                                            choices = c("Inflation rate (GDP deflator)", "Inflation rate (CPI NOFE)","Inflation rate (CPI STICK)"),
                                                            selected = "Inflation rate (GDP deflator)"),
                                                helpText(h4(strong("Productivity growth time series")), style="color:black"),
                                                selectInput("varselectPROD", label = h3(""),
                                                            choices = c("2Y backward MA of prod_roh1 growth","2Y backward MA of prod_roh2 growth","2Y backward MA of prod_tfp growth"),
                                                            selected = "2Y backward MA of prod_roh1 growth"),
                                                helpText(div(h3(strong("Time period")), style="color:black"),div("Choose the sample period you want.",br(),"The default range cover part of the sample period that Michael T. Kiley used (Observations from 1950 to 1969 were not available for some important series) in this analysis"), style="color:black"),
                                                sliderInput("periodslider3", h3(""),
                                                            min = 1969, max = 2019, value = c(1970,2000), step = 1,round = TRUE),
                                                helpText(div(h3(strong("M2 Lag selection")), style="color:black"),div("Choose the lag for the M2 aggregate growth in equation 3.",br(), "Kiley used the lag 0, but lag 2 seems to be the right one according to the pre-analysis section."), style="color:black"),
                                                sliderInput("lagselectionM2", h3(""),
                                                            min = 0, max = 5, value = 2, step = 1,round = TRUE))
                                         
                            ),
                            
                            
                            mainPanel(width=8,
                                      htmlOutput("kiley_eq_est1"),
                                      br(),
                                      htmlOutput("kiley_eq_est2"),
                                      br(),
                                      htmlOutput("kiley_eq_est3"),
                                      br(),
                                      htmlOutput("kiley_eq_est4"),
                                      br(),
                                      htmlOutput("kiley_eq_est5"),
                                      br(),
                                      htmlOutput("kiley_eq_est6"),
                                      br(),
                                      htmlOutput("kiley_eq_est7")
                            )
                          )),
                 
                 tabPanel("Regression framework",
                          
                          sidebarLayout(
                            sidebarPanel(width=4,
                                         column(10,
                                                helpText(div(h3(strong("Time series selection")), style="color:black"),div("Choose the time series to be plotted"), style="color:black"),
                                                helpText(h4("1) Selection of the ",strong("first time series")," and of its", strong("lag")), style="color:black"),
                                                selectInput("varselect2_1", label = h3(""),
                                                            choices = c("Inflation rate (GDP deflator)", "Inflation rate (CPI NOFE)","Inflation rate (CPI STICK)","Productivity growth (prod_roh1)","2Y backward MA of prod_roh1 growth","Productivity growth (prod_roh2)","2Y backward MA of prod_roh2 growth","Productivity growth (prod_tfp)","2Y backward MA of prod_tfp growth","M2 aggregate growth","2Y backward MA of M2 growth","M0 aggregate growth","interest rate","Wages growth","Real output growth","Nominal output growth","Unemployment rate (unemprate_civ)","Unemployment rate growth (unemplev_ld)","Velocity of money"),
                                                            selected = "Inflation rate (GDP deflator)"),
                                                sliderInput("lagselection2_1", h3(""),
                                                            min = 0, max = 5, value = 0, step = 1,round = TRUE),
                                                helpText(h4("2) Selection of the ",strong("second time series")," and of its", strong("lag")), style="color:black"),
                                                selectInput("varselect2_2", label = h3(""),
                                                            choices = c("Inflation rate (GDP deflator)", "Inflation rate (CPI NOFE)","Inflation rate (CPI STICK)","Productivity growth (prod_roh1)","2Y backward MA of prod_roh1 growth","Productivity growth (prod_roh2)","2Y backward MA of prod_roh2 growth","Productivity growth (prod_tfp)","2Y backward MA of prod_tfp growth","M2 aggregate growth","2Y backward MA of M2 growth","M0 aggregate growth","interest rate","Wages growth","Real output growth","Nominal output growth","Unemployment rate (unemprate_civ)","Unemployment rate growth (unemplev_ld)","Velocity of money"),
                                                            selected = "2Y backward MA of M2 growth"),
                                                sliderInput("lagselection2_2", h3(""),
                                                            min = 0, max = 5, value = 2, step = 1,round = TRUE),
                                                helpText(h4("3) Selection of the ",strong("third time series")," and of its", strong("lag")), style="color:black"),
                                                selectInput("varselect2_3", label = h3(""),
                                                            choices = c("Inflation rate (GDP deflator)", "Inflation rate (CPI NOFE)","Inflation rate (CPI STICK)","Productivity growth (prod_roh1)","2Y backward MA of prod_roh1 growth","Productivity growth (prod_roh2)","2Y backward MA of prod_roh2 growth","Productivity growth (prod_tfp)","2Y backward MA of prod_tfp growth","M2 aggregate growth","2Y backward MA of M2 growth","M0 aggregate growth","interest rate","Wages growth","Real output growth","Nominal output growth","Unemployment rate (unemprate_civ)","Unemployment rate growth (unemplev_ld)","Velocity of money"),
                                                            selected = "2Y backward MA of prod_roh1 growth"),
                                                sliderInput("lagselection2_3", h3(""),
                                                            min = 0, max = 5, value = 0, step = 1,round = TRUE),
                                                helpText(h4("3) Selection of the ",strong("fourth time serie")," and of its", strong("lag")), style="color:black"),
                                                selectInput("varselect2_4", label = h3(""),
                                                            choices = c("Inflation rate (GDP deflator)", "Inflation rate (CPI NOFE)","Inflation rate (CPI STICK)","Productivity growth (prod_roh1)","2Y backward MA of prod_roh1 growth","Productivity growth (prod_roh2)","2Y backward MA of prod_roh2 growth","Productivity growth (prod_tfp)","2Y backward MA of prod_tfp growth","M2 aggregate growth","2Y backward MA of M2 growth","M0 aggregate growth","interest rate","Wages growth","Real output growth","Nominal output growth","Unemployment rate (unemprate_civ)","Unemployment rate growth (unemplev_ld)","Velocity of money"),
                                                            selected = "Inflation rate (GDP deflator)"),
                                                sliderInput("lagselection2_4", h3(""),
                                                            min = 0, max = 5, value = 1, step = 1,round = TRUE),
                                                helpText(div(h3(strong("Time period selection")), style="color:black"),div("Choose the sample period you want.",br(),"The default range cover the time period of interest for masked inflation analysis."), style="color:black"),
                                                sliderInput("periodslider2", h3(""),
                                                            min = 1969, max = 2019, value = c(1988,2006), step = 1,round = TRUE))
                                         
                            ),
                            
                            mainPanel(width=8,
                                      htmlOutput("timeseriedescri2"),
                                      br(),
                                      plotlyOutput("scatterplot2"),
                                      br(),
                                      htmlOutput("eq_est1"),
                                      br(),
                                      htmlOutput("eq_est2"),
                                      br(),
                                      htmlOutput("eq_est3")
                            )
                          ))
                 
)

#Server----------------------------------------------------------------------------------------
server <- function(input, output) {
  
  output$timeseriedescri <- renderUI({
    descriptionserie1 <- switch (input$varselect1,
                                 "Empty"="No series has been selected",
                                 "Inflation rate (GDP deflator)" = "The Inflation rate (GDP deflator) series is the price index for business output (yearly measured) in log-difference multiplied by 100. It measure the prices (GDP deflator) percentage growth by year.",
                                 "Inflation rate (CPI NOFE)" = "The Inflation rate (CPI NOFE) series is the Consumer Price Index for All Urban Consumers growth: It includes all Items Less Food & Energy. This measurement, known as 'Core CPI' is widely used by economists because food and energy have very volatile prices. The Inflation rate (CPI NOFE) is the Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                 "Inflation rate (CPI STICK)" = "The Inflation rate (CPI STICK) series is The Sticky Price Consumer Price Index growth. The index is calculated from a subset of goods and services included in the CPI that change price relatively infrequently.The Inflation rate (CPI STICK) is the Sticky Price Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                 "Productivity growth (prod_roh2)" = "Productivity growth (prod_roh2) series is the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                 "2Y backward MA of prod_roh2 growth" = "The 2Y backward MA of prod_roh2 growth series is the two years backward moving average of the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                 "Productivity growth (prod_roh1)" = "The Productivity growth (prod_roh1) series is the Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                 "2Y backward MA of prod_roh1 growth" = "The 2Y backward MA of prod_roh1 growth series is the two years backward moving average of Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                 "Productivity growth (prod_tfp)" = "The Productivity growth (prod_tfp) series is the Total Factor Productivity at Constant National Prices (yearly measured) for United States (Index 2017=1).",
                                 "2Y backward MA of prod_tfp growth" = "The 2Y backward MA of prod_tfp growth series is the two years backward moving average of he Total Factor Productivity at Constant National Prices (yearly measured) for United States",
                                 "M2 aggregate growth" = "The M2 aggregate growth series is the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                 "2Y backward MA of M2 growth" = "The 2Y backward MA of M2 growth series is the two years backward moving average of the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                 "M0 aggregate growth" = "The M0 aggregate growth series is the Monetary base of Federal Reserve Bank (yearly measured) in log-difference multiplied by 100.",
                                 "Velocity of money" = "The Velocity of money series is the number of times one dollar is spent to buy goods and services per unit of time",
                                 "interest rate"= "The interest rate series is the Discount Rate for United States (yearly measured)",
                                 "Wages growth" = "The Wages growth series is the growth of the Wages growth (yearly measured) computed by taking the log-difference multiplied by 100.",
                                 "Real output growth" = "The real output growth series is the real output (yearly measured) for the business sector in log-difference multiplied by 100",
                                 "Nominal output growth" = "The nominal output growth series is the nominal output (yearly measured) for the business sector in log-difference multiplied by 100",
                                 "Unemployment rate (unemprate_civ)" = "The Unemployment rate (unemprate_civ) series is the civilian unemployment rate (yearly measured)",
                                 "Unemployment rate growth (unemplev_ld)" = "The Unemployment rate growth (unemplev_ld) series is the unemployment rate (yearly measured) in log-difference multiplied by 100.")
    descriptionserie2 <- switch (input$varselect2,
                                 "Empty"="No series has been selected",
                                 "Inflation rate (GDP deflator)" = "The Inflation rate (GDP deflator) series is the price index for business output (yearly measured) in log-difference multiplied by 100. It measure the prices (GDP deflator) percentage growth by year.",
                                 "Inflation rate (CPI NOFE)" = "The Inflation rate (CPI NOFE) series is the Consumer Price Index for All Urban Consumers growth: It includes all Items Less Food & Energy. This measurement, known as 'Core CPI' is widely used by economists because food and energy have very volatile prices. The Inflation rate (CPI NOFE) is the Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                 "Inflation rate (CPI STICK)" = "The Inflation rate (CPI STICK) series is The Sticky Price Consumer Price Index growth. The index is calculated from a subset of goods and services included in the CPI that change price relatively infrequently.The Inflation rate (CPI STICK) is the Sticky Price Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                 "Productivity growth (prod_roh2)" = "Productivity growth (prod_roh2) series is the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                 "2Y backward MA of prod_roh2 growth" = "The 2Y backward MA of prod_roh2 growth series is the two years backward moving average of the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                 "Productivity growth (prod_roh1)" = "The Productivity growth (prod_roh1) series is the Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                 "2Y backward MA of prod_roh1 growth" = "The 2Y backward MA of prod_roh1 growth series is the two years backward moving average of Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                 "Productivity growth (prod_tfp)" = "The Productivity growth (prod_tfp) series is the Total Factor Productivity at Constant National Prices (yearly measured) for United States (Index 2017=1).",
                                 "2Y backward MA of prod_tfp growth" = "The 2Y backward MA of prod_tfp growth series is the two years backward moving average of he Total Factor Productivity at Constant National Prices (yearly measured) for United States",
                                 "M2 aggregate growth" = "The M2 aggregate growth series is the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                 "2Y backward MA of M2 growth" = "The 2Y backward MA of M2 growth series is the two years backward moving average of the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                 "M0 aggregate growth" = "The M0 aggregate growth series is the Monetary base of Federal Reserve Bank (yearly measured) in log-difference multiplied by 100.",
                                 "Velocity of money" = "The Velocity of money series is the number of times one dollar is spent to buy goods and services per unit of time",
                                 "interest rate"= "The interest rate series is the Discount Rate for United States (yearly measured)",
                                 "Wages growth" = "The Wages growth series is the growth of the Wages growth (yearly measured) computed by taking the log-difference multiplied by 100.",
                                 "Real output growth" = "The real output growth series is the real output (yearly measured) for the business sector in log-difference multiplied by 100",
                                 "Nominal output growth" = "The nominal output growth series is the nominal output (yearly measured) for the business sector in log-difference multiplied by 100",
                                 "Unemployment rate (unemprate_civ)" = "The Unemployment rate (unemprate_civ) series is the civilian unemployment rate (yearly measured)",
                                 "Unemployment rate growth (unemplev_ld)" = "The Unemployment rate growth (unemplev_ld) series is the unemployment rate (yearly measured) in log-difference multiplied by 100.")
    descriptionserie3 <- switch (input$varselect3,
                                 "Empty"="No series has been selected",
                                 "Inflation rate (GDP deflator)" = "The Inflation rate (GDP deflator) series is the price index for business output (yearly measured) in log-difference multiplied by 100. It measure the prices (GDP deflator) percentage growth by year.",
                                 "Inflation rate (CPI NOFE)" = "The Inflation rate (CPI NOFE) series is the Consumer Price Index for All Urban Consumers growth: It includes all Items Less Food & Energy. This measurement, known as 'Core CPI' is widely used by economists because food and energy have very volatile prices. The Inflation rate (CPI NOFE) is the Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                 "Inflation rate (CPI STICK)" = "The Inflation rate (CPI STICK) series is The Sticky Price Consumer Price Index growth. The index is calculated from a subset of goods and services included in the CPI that change price relatively infrequently.The Inflation rate (CPI STICK) is the Sticky Price Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                 "Productivity growth (prod_roh2)" = "Productivity growth (prod_roh2) series is the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                 "2Y backward MA of prod_roh2 growth" = "The 2Y backward MA of prod_roh2 growth series is the two years backward moving average of the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                 "Productivity growth (prod_roh1)" = "The Productivity growth (prod_roh1) series is the Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                 "2Y backward MA of prod_roh1 growth" = "The 2Y backward MA of prod_roh1 growth series is the two years backward moving average of Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                 "Productivity growth (prod_tfp)" = "The Productivity growth (prod_tfp) series is the Total Factor Productivity at Constant National Prices (yearly measured) for United States (Index 2017=1).",
                                 "2Y backward MA of prod_tfp growth" = "The 2Y backward MA of prod_tfp growth series is the two years backward moving average of he Total Factor Productivity at Constant National Prices (yearly measured) for United States",
                                 "M2 aggregate growth" = "The M2 aggregate growth series is the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                 "2Y backward MA of M2 growth" = "The 2Y backward MA of M2 growth series is the two years backward moving average of the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                 "M0 aggregate growth" = "The M0 aggregate growth series is the Monetary base of Federal Reserve Bank (yearly measured) in log-difference multiplied by 100.",
                                 "Velocity of money" = "The Velocity of money series is the number of times one dollar is spent to buy goods and services per unit of time",
                                 "interest rate"= "The interest rate series is the Discount Rate for United States (yearly measured)",
                                 "Wages growth" = "The Wages growth series is the growth of the Wages growth (yearly measured) computed by taking the log-difference multiplied by 100.",
                                 "Real output growth" = "The real output growth series is the real output (yearly measured) for the business sector in log-difference multiplied by 100",
                                 "Nominal output growth" = "The nominal output growth series is the nominal output (yearly measured) for the business sector in log-difference multiplied by 100",
                                 "Unemployment rate (unemprate_civ)" = "The Unemployment rate (unemprate_civ) series is the civilian unemployment rate (yearly measured)",
                                 "Unemployment rate growth (unemplev_ld)" = "The Unemployment rate growth (unemplev_ld) series is the unemployment rate (yearly measured) in log-difference multiplied by 100.")
    msg1<-paste(strong("First time series description"))
    msg2<-paste(strong("Second time series description"))
    msg3<-paste(strong("Third time series description"))
    HTML(paste(msg1,descriptionserie1,msg2,descriptionserie2,msg3,descriptionserie3, sep = "<br/>"))
    
  })
  
  
  output$scatterplot <- renderPlotly({
    z <- window(dtts, start = input$periodslider1[1], end = input$periodslider1[2])
    time_serie_name1 <- switch (input$varselect1,
                                "Empty"=z[,14],
                                "Inflation rate (GDP deflator)" = z[,2],
                                "Inflation rate (CPI NOFE)" = z[,8],
                                "Inflation rate (CPI STICK)" = z[,9],
                                "Productivity growth (prod_roh2)" = z[,13],
                                "2Y backward MA of prod_roh2 growth"= z[,19],
                                "Productivity growth (prod_roh1)" = z[,5],
                                "2Y backward MA of prod_roh1 growth"= z[,18],
                                "Productivity growth (prod_tfp)" = z[,10],
                                "2Y backward MA of prod_tfp growth"= z[,20],
                                "M2 aggregate growth" = z[,7],
                                "2Y backward MA of M2 growth" = z[,17],
                                "M0 aggregate growth" = z[,16],
                                "Velocity of money"= z[,21],
                                "interest rate"= z[,15],
                                "Wages growth" = z[,6],
                                "Real output growth" = z[,3],
                                "Nominal output growth" = z[,4],
                                "Unemployment rate (unemprate_civ)" = z[,12],
                                "Unemployment rate growth (unemplev_ld)" = z[,11])
    time_serie_name2 <- switch (input$varselect2,
                                "Empty"=z[,14],
                                "Inflation rate (GDP deflator)" = z[,2],
                                "Inflation rate (CPI NOFE)" = z[,8],
                                "Inflation rate (CPI STICK)" = z[,9],
                                "Productivity growth (prod_roh2)" = z[,13],
                                "2Y backward MA of prod_roh2 growth"= z[,19],
                                "Productivity growth (prod_roh1)" = z[,5],
                                "2Y backward MA of prod_roh1 growth"= z[,18],
                                "Productivity growth (prod_tfp)" = z[,10],
                                "2Y backward MA of prod_tfp growth"= z[,20],
                                "M2 aggregate growth" = z[,7],
                                "2Y backward MA of M2 growth" = z[,17],
                                "M0 aggregate growth" = z[,16],
                                "Velocity of money"= z[,21],
                                "interest rate"= z[,15],
                                "Wages growth" = z[,6],
                                "Real output growth" = z[,3],
                                "Nominal output growth" = z[,4],
                                "Unemployment rate (unemprate_civ)" = z[,12],
                                "Unemployment rate growth (unemplev_ld)" = z[,11])
    time_serie_name3 <- switch (input$varselect3,
                                "Empty"=z[,14],
                                "Inflation rate (GDP deflator)" = z[,2],
                                "Inflation rate (CPI NOFE)" = z[,8],
                                "Inflation rate (CPI STICK)" = z[,9],
                                "Productivity growth (prod_roh2)" = z[,13],
                                "2Y backward MA of prod_roh2 growth"= z[,19],
                                "Productivity growth (prod_roh1)" = z[,5],
                                "2Y backward MA of prod_roh1 growth"= z[,18],
                                "Productivity growth (prod_tfp)" = z[,10],
                                "2Y backward MA of prod_tfp growth"= z[,20],
                                "M2 aggregate growth" = z[,7],
                                "2Y backward MA of M2 growth" = z[,17],
                                "M0 aggregate growth" = z[,16],
                                "Velocity of money"= z[,21],
                                "interest rate"= z[,15],
                                "Wages growth" = z[,6],
                                "Real output growth" = z[,3],
                                "Nominal output growth" = z[,4],
                                "Unemployment rate (unemprate_civ)" = z[,12],
                                "Unemployment rate growth (unemplev_ld)" = z[,11])
    First_timeserie <- lag(time_serie_name1, -(input$lagselection1))
    Second_timeserie <- lag(time_serie_name2, -(input$lagselection2))
    Third_timeserie <- lag(time_serie_name3, -(input$lagselection3))
    TSstudio::ts_plot(ts_c(First_timeserie,Second_timeserie,Third_timeserie),Xtitle="date",title="Time series plot")
  })
  
  output$corrtitle <- renderUI({
    msg1<-paste(strong("Cross correlation plot"))
    msg2<-paste("It tells you the correlation between the first and the second time series at every lag of the first one")
    HTML(paste(msg1,msg2, sep = "<br/>"))
    
  })
  
  output$correlations <- renderPlot({
    z <- window(dtts, start = input$periodslider1[1], end = input$periodslider1[2])
    First_timeserie <- switch (input$varselect1,
                               "Empty"=z[,14],
                               "Inflation rate (GDP deflator)" = z[,2],
                               "Inflation rate (CPI NOFE)" = z[,8],
                               "Inflation rate (CPI STICK)" = z[,9],
                               "Productivity growth (prod_roh2)" = z[,13],
                               "2Y backward MA of prod_roh2 growth"= z[,19],
                               "Productivity growth (prod_roh1)" = z[,5],
                               "2Y backward MA of prod_roh1 growth"= z[,18],
                               "Productivity growth (prod_tfp)" = z[,10],
                               "2Y backward MA of prod_tfp growth"= z[,20],
                               "M2 aggregate growth" = z[,7],
                               "2Y backward MA of M2 growth" = z[,17],
                               "M0 aggregate growth" = z[,16],
                               "Velocity of money"= z[,21],
                               "interest rate"= z[,15],
                               "Wages growth" = z[,6],
                               "Real output growth" = z[,3],
                               "Nominal output growth" = z[,4],
                               "Unemployment rate (unemprate_civ)" = z[,12],
                               "Unemployment rate growth (unemplev_ld)" = z[,11])
    Second_timeserie <- switch (input$varselect2,
                                "Empty"=z[,14],
                                "Inflation rate (GDP deflator)" = z[,2],
                                "Inflation rate (CPI NOFE)" = z[,8],
                                "Inflation rate (CPI STICK)" = z[,9],
                                "Productivity growth (prod_roh2)" = z[,13],
                                "2Y backward MA of prod_roh2 growth"= z[,19],
                                "Productivity growth (prod_roh1)" = z[,5],
                                "2Y backward MA of prod_roh1 growth"= z[,18],
                                "Productivity growth (prod_tfp)" = z[,10],
                                "2Y backward MA of prod_tfp growth"= z[,20],
                                "M2 aggregate growth" = z[,7],
                                "2Y backward MA of M2 growth" = z[,17],
                                "M0 aggregate growth" = z[,16],
                                "Velocity of money"= z[,21],
                                "interest rate"= z[,15],
                                "Wages growth" = z[,6],
                                "Real output growth" = z[,3],
                                "Nominal output growth" = z[,4],
                                "Unemployment rate (unemprate_civ)" = z[,12],
                                "Unemployment rate growth (unemplev_ld)" = z[,11])
    corr<-ccf( First_timeserie,Second_timeserie,  lag.max = 5,type = "correlation")
    corr
    
  })
  output$corr_est <- renderUI({
    z <- window(dtts, start = input$periodslider1[1], end = input$periodslider1[2])
    time_serie_name1 <- switch (input$varselect1,
                                "Empty"=z[,14],
                                "Inflation rate (GDP deflator)" = z[,2],
                                "Inflation rate (CPI NOFE)" = z[,8],
                                "Inflation rate (CPI STICK)" = z[,9],
                                "Productivity growth (prod_roh2)" = z[,13],
                                "2Y backward MA of prod_roh2 growth"= z[,19],
                                "Productivity growth (prod_roh1)" = z[,5],
                                "2Y backward MA of prod_roh1 growth"= z[,18],
                                "Productivity growth (prod_tfp)" = z[,10],
                                "2Y backward MA of prod_tfp growth"= z[,20],
                                "M2 aggregate growth" = z[,7],
                                "2Y backward MA of M2 growth" = z[,17],
                                "M0 aggregate growth" = z[,16],
                                "Velocity of money"= z[,21],
                                "interest rate"= z[,15],
                                "Wages growth" = z[,6],
                                "Real output growth" = z[,3],
                                "Nominal output growth" = z[,4],
                                "Unemployment rate (unemprate_civ)" = z[,12],
                                "Unemployment rate growth (unemplev_ld)" = z[,11])
    time_serie_name2 <- switch (input$varselect2,
                                "Empty"=z[,14],
                                "Inflation rate (GDP deflator)" = z[,2],
                                "Inflation rate (CPI NOFE)" = z[,8],
                                "Inflation rate (CPI STICK)" = z[,9],
                                "Productivity growth (prod_roh2)" = z[,13],
                                "2Y backward MA of prod_roh2 growth"= z[,19],
                                "Productivity growth (prod_roh1)" = z[,5],
                                "2Y backward MA of prod_roh1 growth"= z[,18],
                                "Productivity growth (prod_tfp)" = z[,10],
                                "2Y backward MA of prod_tfp growth"= z[,20],
                                "M2 aggregate growth" = z[,7],
                                "2Y backward MA of M2 growth" = z[,17],
                                "M0 aggregate growth" = z[,16],
                                "Velocity of money"= z[,21],
                                "interest rate"= z[,15],
                                "Wages growth" = z[,6],
                                "Real output growth" = z[,3],
                                "Nominal output growth" = z[,4],
                                "Unemployment rate (unemprate_civ)" = z[,12],
                                "Unemployment rate growth (unemplev_ld)" = z[,11])
    time_serie_name3 <- switch (input$varselect3,
                                "Empty"=z[,14],
                                "Inflation rate (GDP deflator)" = z[,2],
                                "Inflation rate (CPI NOFE)" = z[,8],
                                "Inflation rate (CPI STICK)" = z[,9],
                                "Productivity growth (prod_roh2)" = z[,13],
                                "2Y backward MA of prod_roh2 growth"= z[,19],
                                "Productivity growth (prod_roh1)" = z[,5],
                                "2Y backward MA of prod_roh1 growth"= z[,18],
                                "Productivity growth (prod_tfp)" = z[,10],
                                "2Y backward MA of prod_tfp growth"= z[,20],
                                "M2 aggregate growth" = z[,7],
                                "2Y backward MA of M2 growth" = z[,17],
                                "M0 aggregate growth" = z[,16],
                                "Velocity of money"= z[,21],
                                "interest rate"= z[,15],
                                "Wages growth" = z[,6],
                                "Real output growth" = z[,3],
                                "Nominal output growth" = z[,4],
                                "Unemployment rate (unemprate_civ)" = z[,12],
                                "Unemployment rate growth (unemplev_ld)" = z[,11])
    corr<-ccf(time_serie_name1, time_serie_name2, lag.max = 5)
    p<-data.frame(corr$acf,corr$lag)
    msg0 <- paste(strong("Four previous lags correlation values"))
    msg1 <- paste("The correlation between ",input$varselect1," and ",input$varselect2, "at lag ", p$corr.lag[2]," is ",round(p$corr.acf[2],digit=2))
    msg2 <- paste("The correlation between ",input$varselect1," and ",input$varselect2, "at lag ", p$corr.lag[3]," is ",round(p$corr.acf[3],digit=2))
    msg3 <- paste("The correlation between ",input$varselect1," and ",input$varselect2, "at lag ", p$corr.lag[4]," is ",round(p$corr.acf[4],digit=2))
    msg4 <- paste("The correlation between ",input$varselect1," and ",input$varselect2, "at lag ", p$corr.lag[5]," is ",round(p$corr.acf[5],digit=2))
    msg5 <- paste("The correlation between ",input$varselect1," and ",input$varselect2, "at lag ", p$corr.lag[6]," is ",round(p$corr.acf[6],digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, msg5, sep = "<br/>"))
    
  })
  
  
  
  output$kiley_eq_est1 <- renderUI({
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    productivity <- switch (input$varselectPROD,
                            "2Y backward MA of prod_roh2 growth"= z[,19],
                            "2Y backward MA of prod_roh1 growth"= z[,18],
                            "2Y backward MA of prod_tfp growth"= z[,20])
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    inflation <- switch (input$varselectINF,
                         "Inflation rate (GDP deflator)" = z[,2],
                         "Inflation rate (CPI NOFE)" = z[,8],
                         "Inflation rate (CPI STICK)" = z[,9])
    case1.1a <- tslm(inflation~productivity)
    case1.1asum<-summary(case1.1a)
    
    intercept<-case1.1asum$coefficients[1]
    coeff1<-case1.1asum$coefficients[2]
    intse<-case1.1asum$coefficients[1,2]
    coeff1se<-case1.1asum$coefficients[2,2]
    intpv<-case1.1asum$coefficients[1,4]
    coeff1pv<-case1.1asum$coefficients[2,4]
    rsquared<-case1.1asum$r.squared
    adjrsquared<-case1.1asum$adj.r.squared
    msg0 <- paste("1) Regression of ",strong(input$varselectINF)," against a two-year (backward) moving avarage of ",strong(input$varselectPROD))
    msg1 <- paste("____Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("____The ",input$varselectPROD, " 2 years MA coefficient: ",strong(round(coeff1,digit=2)),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",strong(round(coeff1pv,digit=4))," )")
    msg3 <- paste("____R Squared: ", strong(round(rsquared,digit=2)))
    msg4 <- paste("____Adjusted R Squared: ", round(adjrsquared,digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, sep = "<br/>"))
    
    
    
  })
  
  output$kiley_eq_est2 <- renderUI({
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    inflation <- switch (input$varselectINF,
                         "Inflation rate (GDP deflator)" = z[,2],
                         "Inflation rate (CPI NOFE)" = z[,8],
                         "Inflation rate (CPI STICK)" = z[,9])
    un <- z[,12]
    case1.2a <- tslm(inflation~un)
    case1.2asum <- summary(case1.2a)
    
    intercept<-case1.2asum$coefficients[1]
    coeff1<-case1.2asum$coefficients[2]
    intse<-case1.2asum$coefficients[1,2]
    coeff1se<-case1.2asum$coefficients[2,2]
    intpv<-case1.2asum$coefficients[1,4]
    coeff1pv<-case1.2asum$coefficients[2,4]
    rsquared<-case1.2asum$r.squared
    adjrsquared<-case1.2asum$adj.r.squared
    msg0 <- paste("2) Regression of ",strong(input$varselectINF)," against the ",strong("Unemployment rate (unemprate_civ)"))
    msg1 <- paste("_____Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("_____The Unemployment rate (unemprate_civ) coefficient: ",strong(round(coeff1,digit=2)),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",strong(round(coeff1pv,digit=4))," )")
    msg3 <- paste("_____R Squared: ", strong(round(rsquared,digit=2)))
    msg4 <- paste("_____Adjusted R Squared: ", round(adjrsquared,digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, sep = "<br/>"))
    
    
    
  })
  
  output$kiley_eq_est3 <- renderUI({
    z <- window(dtts, start = input$periodslider3[1]-input$lagselectionM2, end = input$periodslider3[2]-input$lagselectionM2)
    ma_m2_ld <- ts(z[,17], start=input$periodslider3[1], end=input$periodslider3[2])
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    inflation <- switch (input$varselectINF,
                         "Inflation rate (GDP deflator)" = z[,2],
                         "Inflation rate (CPI NOFE)" = z[,8],
                         "Inflation rate (CPI STICK)" = z[,9])
    case1.3a <- tslm(inflation~ma_m2_ld)
    case1.3asum<-summary(case1.3a)
    
    intercept<-case1.3asum$coefficients[1]
    coeff1<-case1.3asum$coefficients[2]
    intse<-case1.3asum$coefficients[1,2]
    coeff1se<-case1.3asum$coefficients[2,2]
    intpv<-case1.3asum$coefficients[1,4]
    coeff1pv<-case1.3asum$coefficients[2,4]
    rsquared<-case1.3asum$r.squared
    adjrsquared<-case1.3asum$adj.r.squared
    msg0 <- paste("3) Regression of ",strong(input$varselectINF)," against a two-year (backward) moving avarage of the ",strong("M2 aggregate growth")," at lag ",strong(input$lagselectionM2))
    msg1 <- paste("_____Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("_____The M2 aggregate growth 2 years MA (at lag",input$lagselectionM2,") coefficient: ",strong(round(coeff1,digit=2)),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",strong(round(coeff1pv,digit=4))," )")
    msg3 <- paste("_____R Squared: ", strong(round(rsquared,digit=2)))
    msg4 <- paste("_____Adjusted R Squared: ", round(adjrsquared,digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, sep = "<br/>"))
    
    
    
  })
  
  output$kiley_eq_est4 <- renderUI({
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    productivity <- switch (input$varselectPROD,
                            "2Y backward MA of prod_roh2 growth"= z[,19],
                            "2Y backward MA of prod_roh1 growth"= z[,18],
                            "2Y backward MA of prod_tfp growth"= z[,20])
    inflation <- switch (input$varselectINF,
                         "Inflation rate (GDP deflator)" = dtts[,2],
                         "Inflation rate (CPI NOFE)" = dtts[,8],
                         "Inflation rate (CPI STICK)" = dtts[,9])
    inf <- window(inflation, start = input$periodslider3[1], end = input$periodslider3[2])
    lag_inf <- window(inflation, start = input$periodslider3[1]-1, end = input$periodslider3[2]-1)
    lag_inf <- ts(lag_inf, start=input$periodslider3[1], end=input$periodslider3[2])
    unemp<-window(dtts[,12],start=input$periodslider3[1],end=input$periodslider3[2])
    case2.3a <- tslm(inf~productivity +lag_inf+unemp)
    case2.3asum<-summary(case2.3a)
    
    
    intercept<-case2.3asum$coefficients[1]
    coeff1<-case2.3asum$coefficients[2]
    coeff2<-case2.3asum$coefficients[3]
    coeff3<-case2.3asum$coefficients[4]
    intse<-case2.3asum$coefficients[1,2]
    coeff1se<-case2.3asum$coefficients[2,2]
    coeff2se<-case2.3asum$coefficients[3,2]
    coeff3se<-case2.3asum$coefficients[4,2]
    intpv<-case2.3asum$coefficients[1,4]
    coeff1pv<-case2.3asum$coefficients[2,4]
    coeff2pv<-case2.3asum$coefficients[3,4]
    coeff3pv<-case2.3asum$coefficients[4,4]
    rsquared<-case2.3asum$r.squared
    adjrsquared<-case2.3asum$adj.r.squared
    msg0 <- paste("4) Regression of ",strong(input$varselectINF)," against a two-year (backward) moving avarage of ",strong(input$varselectPROD),", the ",strong(input$varselectINF)," at lag ",strong("1"), " and the ",strong("Unemployment rate (unemprate_civ)"))
    msg1 <- paste("_____Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("_____The ",input$varselectPROD," 2 years MA coefficient: ",strong(round(coeff1,digit=2)),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",strong(round(coeff1pv,digit=4))," )")
    msg3 <- paste("_____The ",input$varselectINF," (at lag 1): ",strong(round(coeff2,digit=2)),"(S.E: ",round(coeff2se,digit=2),"pvalue: ",strong(round(coeff2pv,digit=4))," )")
    msg4 <- paste("_____The Unemployment rate (unemprate_civ): ",strong(round(coeff3,digit=2)),"(S.E: ",round(coeff3se,digit=2),"pvalue: ",strong(round(coeff3pv,digit=4))," )")
    msg5 <- paste("_____R Squared: ", round(rsquared,digit=2))
    msg6 <- paste("_____Adjusted R Squared: ", strong(round(adjrsquared,digit=2)))
    HTML(paste(msg0,msg1, msg2, msg3, msg4,msg5,msg6, sep = "<br/>"))
    
    
    
  })
  
  output$kiley_eq_est5 <- renderUI({
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    productivity <- switch (input$varselectPROD,
                            "2Y backward MA of prod_roh2 growth"= z[,19],
                            "2Y backward MA of prod_roh1 growth"= z[,18],
                            "2Y backward MA of prod_tfp growth"= z[,20])
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    wag <- tslm(z[,6]~productivity)
    wagsum<-summary(wag)
    
    intercept<-wagsum$coefficients[1]
    coeff1<-wagsum$coefficients[2]
    intse<-wagsum$coefficients[1,2]
    coeff1se<-wagsum$coefficients[2,2]
    intpv<-wagsum$coefficients[1,4]
    coeff1pv<-wagsum$coefficients[2,4]
    rsquared<-wagsum$r.squared
    adjrsquared<-wagsum$adj.r.squared
    msg0 <- paste("5) Regression of ",strong("Wages growth")," against a two-year (backward) moving avarage of ",strong(input$varselectPROD))
    msg1 <- paste("_____Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("_____The ",input$varselectPROD," 2 years MA coefficient: ",strong(round(coeff1,digit=2)),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",strong(round(coeff1pv,digit=4))," )")
    msg3 <- paste("_____R Squared: ", strong(round(rsquared,digit=2)))
    msg4 <- paste("_____Adjusted R Squared: ", round(adjrsquared,digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, sep = "<br/>"))
    
    
    
  })
  output$kiley_eq_est6 <- renderUI({
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    productivity <- switch (input$varselectPROD,
                            "2Y backward MA of prod_roh2 growth"= z[,19],
                            "2Y backward MA of prod_roh1 growth"= z[,18],
                            "2Y backward MA of prod_tfp growth"= z[,20])
    z <- window(dtts, start = input$periodslider3[1], end = input$periodslider3[2])
    inc <- tslm(z[,4]~productivity)
    incsum<-summary(inc)
    
    intercept<-incsum$coefficients[1]
    coeff1<-incsum$coefficients[2]
    intse<-incsum$coefficients[1,2]
    coeff1se<-incsum$coefficients[2,2]
    intpv<-incsum$coefficients[1,4]
    coeff1pv<-incsum$coefficients[2,4]
    rsquared<-incsum$r.squared
    adjrsquared<-incsum$adj.r.squared
    msg0 <- paste("6) Regression of ",strong("Nominal income growth")," against a two-year (backward) moving avarage of ",strong(input$varselectPROD))
    msg1 <- paste("_____Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("_____The ",input$varselectPROD," coefficient: ",strong(round(coeff1,digit=2)),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",strong(round(coeff1pv,digit=4))," )")
    msg3 <- paste("_____R Squared: ", strong(round(rsquared,digit=2)))
    msg4 <- paste("_____Adjusted R Squared: ", round(adjrsquared,digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, sep = "<br/>"))
    
    
    
  })
  
  output$kiley_eq_est7 <- renderUI({
    inflation <- switch (input$varselectINF,
                         "Inflation rate (GDP deflator)" = dtts[,2],
                         "Inflation rate (CPI NOFE)" = dtts[,8],
                         "Inflation rate (CPI STICK)" = dtts[,9])
    inf <- window(inflation, start = input$periodslider3[1]+1, end = input$periodslider3[2])
    lag_inf <- window(inflation, start = input$periodslider3[1]+1-1, end = input$periodslider3[2]-1)
    lag_inf <- ts(lag_inf, start=input$periodslider3[1]+1, end=input$periodslider3[2])
    unemp<-window(dtts[,12],start=input$periodslider3[1]+1,end=input$periodslider3[2])
    tpc <- tslm(inf~lag_inf+unemp)
    tpcsum<-summary(tpc)
    
    
    intercept<-tpcsum$coefficients[1]
    coeff1<-tpcsum$coefficients[2]
    coeff2<-tpcsum$coefficients[3]
    intse<-tpcsum$coefficients[1,2]
    coeff1se<-tpcsum$coefficients[2,2]
    coeff2se<-tpcsum$coefficients[3,2]
    intpv<-tpcsum$coefficients[1,4]
    coeff1pv<-tpcsum$coefficients[2,4]
    coeff2pv<-tpcsum$coefficients[3,4]
    rsquared<-tpcsum$r.squared
    adjrsquared<-tpcsum$adj.r.squared
    msg0 <- paste("7) Regression of ",strong(input$varselectINF)," against the ",strong(input$varselectINF)," at lag ",strong("1"), " and the ",strong("Unemployment rate (unemprate_civ)"))
    msg1 <- paste("_____Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg3 <- paste("_____The Inflation rate (GDP deflator at lag 1): ",strong(round(coeff1,digit=2)),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",strong(round(coeff1pv,digit=4))," )")
    msg4 <- paste("_____The Unemployment rate (unemprate_civ): ",strong(round(coeff2,digit=2)),"(S.E: ",round(coeff2se,digit=2),"pvalue: ",strong(round(coeff2pv,digit=4))," )")
    msg5 <- paste("_____R Squared: ", round(rsquared,digit=2))
    msg6 <- paste("_____Adjusted R Squared: ", strong(round(adjrsquared,digit=2)))
    HTML(paste(msg0, msg1, msg3, msg4,msg5,msg6, sep = "<br/>"))
    
    
    
  })
  
  
  
  
  
  
  
  
  
  output$timeseriedescri2 <- renderUI({
    descriptionserie2_1 <- switch (input$varselect2_1,
                                   "Empty"="No series has been selected",
                                   "Inflation rate (GDP deflator)" = "The Inflation rate (GDP deflator) series is the price index for business output (yearly measured) in log-difference multiplied by 100. It measure the prices (GDP deflator) percentage growth by year.",
                                   "Inflation rate (CPI NOFE)" = "The Inflation rate (CPI NOFE) series is the Consumer Price Index for All Urban Consumers growth: It includes all Items Less Food & Energy. This measurement, known as 'Core CPI' is widely used by economists because food and energy have very volatile prices. The Inflation rate (CPI NOFE) is the Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                   "Inflation rate (CPI STICK)" = "The Inflation rate (CPI STICK) series is The Sticky Price Consumer Price Index growth. The index is calculated from a subset of goods and services included in the CPI that change price relatively infrequently.The Inflation rate (CPI STICK) is the Sticky Price Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                   "Productivity growth (prod_roh2)" = "Productivity growth (prod_roh2) series is the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                   "2Y backward MA of prod_roh2 growth" = "The 2Y backward MA of prod_roh2 growth series is the two years backward moving average of the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                   "Productivity growth (prod_roh1)" = "The Productivity growth (prod_roh1) series is the Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                   "2Y backward MA of prod_roh1 growth" = "The 2Y backward MA of prod_roh1 growth series is the two years backward moving average of Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                   "Productivity growth (prod_tfp)" = "The Productivity growth (prod_tfp) series is the Total Factor Productivity at Constant National Prices (yearly measured) for United States (Index 2017=1).",
                                   "2Y backward MA of prod_tfp growth" = "The 2Y backward MA of prod_tfp growth series is the two years backward moving average of he Total Factor Productivity at Constant National Prices (yearly measured) for United States",
                                   "M2 aggregate growth" = "The M2 aggregate growth series is the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                   "2Y backward MA of M2 growth" = "The 2Y backward MA of M2 growth series is the two years backward moving average of the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                   "M0 aggregate growth" = "The M0 aggregate growth series is the Monetary base of Federal Reserve Bank (yearly measured) in log-difference multiplied by 100.",
                                   "Velocity of money" = "The Velocity of money series is the number of times one dollar is spent to buy goods and services per unit of time",
                                   "interest rate"= "The interest rate series is the Discount Rate for United States (yearly measured)",
                                   "Wages growth" = "The Wages growth series is the growth of the Wages growth (yearly measured) computed by taking the log-difference multiplied by 100.",
                                   "Real output growth" = "The real output growth series is the real output (yearly measured) for the business sector in log-difference multiplied by 100",
                                   "Nominal output growth" = "The nominal output growth series is the nominal output (yearly measured) for the business sector in log-difference multiplied by 100",
                                   "Unemployment rate (unemprate_civ)" = "The Unemployment rate (unemprate_civ) series is the civilian unemployment rate (yearly measured)",
                                   "Unemployment rate growth (unemplev_ld)" = "The Unemployment rate growth (unemplev_ld) series is the unemployment rate (yearly measured) in log-difference multiplied by 100.")
    descriptionserie2_2 <- switch (input$varselect2_2,
                                   "Empty"="No series has been selected",
                                   "Inflation rate (GDP deflator)" = "The Inflation rate (GDP deflator) series is the price index for business output (yearly measured) in log-difference multiplied by 100. It measure the prices (GDP deflator) percentage growth by year.",
                                   "Inflation rate (CPI NOFE)" = "The Inflation rate (CPI NOFE) series is the Consumer Price Index for All Urban Consumers growth: It includes all Items Less Food & Energy. This measurement, known as 'Core CPI' is widely used by economists because food and energy have very volatile prices. The Inflation rate (CPI NOFE) is the Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                   "Inflation rate (CPI STICK)" = "The Inflation rate (CPI STICK) series is The Sticky Price Consumer Price Index growth. The index is calculated from a subset of goods and services included in the CPI that change price relatively infrequently.The Inflation rate (CPI STICK) is the Sticky Price Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                   "Productivity growth (prod_roh2)" = "Productivity growth (prod_roh2) series is the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                   "2Y backward MA of prod_roh2 growth" = "The 2Y backward MA of prod_roh2 growth series is the two years backward moving average of the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                   "Productivity growth (prod_roh1)" = "The Productivity growth (prod_roh1) series is the Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                   "2Y backward MA of prod_roh1 growth" = "The 2Y backward MA of prod_roh1 growth series is the two years backward moving average of Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                   "Productivity growth (prod_tfp)" = "The Productivity growth (prod_tfp) series is the Total Factor Productivity at Constant National Prices (yearly measured) for United States (Index 2017=1).",
                                   "2Y backward MA of prod_tfp growth" = "The 2Y backward MA of prod_tfp growth series is the two years backward moving average of he Total Factor Productivity at Constant National Prices (yearly measured) for United States",
                                   "M2 aggregate growth" = "The M2 aggregate growth series is the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                   "2Y backward MA of M2 growth" = "The 2Y backward MA of M2 growth series is the two years backward moving average of the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                   "M0 aggregate growth" = "The M0 aggregate growth series is the Monetary base of Federal Reserve Bank (yearly measured) in log-difference multiplied by 100.",
                                   "Velocity of money" = "The Velocity of money series is the number of times one dollar is spent to buy goods and services per unit of time",
                                   "interest rate"= "The interest rate series is the Discount Rate for United States (yearly measured)",
                                   "Wages growth" = "The Wages growth series is the growth of the Wages growth (yearly measured) computed by taking the log-difference multiplied by 100.",
                                   "Real output growth" = "The real output growth series is the real output (yearly measured) for the business sector in log-difference multiplied by 100",
                                   "Nominal output growth" = "The nominal output growth series is the nominal output (yearly measured) for the business sector in log-difference multiplied by 100",
                                   "Unemployment rate (unemprate_civ)" = "The Unemployment rate (unemprate_civ) series is the civilian unemployment rate (yearly measured)",
                                   "Unemployment rate growth (unemplev_ld)" = "The Unemployment rate growth (unemplev_ld) series is the unemployment rate (yearly measured) in log-difference multiplied by 100.")
    descriptionserie2_3 <- switch (input$varselect2_3,
                                   "Empty"="No series has been selected",
                                   "Inflation rate (GDP deflator)" = "The Inflation rate (GDP deflator) series is the price index for business output (yearly measured) in log-difference multiplied by 100. It measure the prices (GDP deflator) percentage growth by year.",
                                   "Inflation rate (CPI NOFE)" = "The Inflation rate (CPI NOFE) series is the Consumer Price Index for All Urban Consumers growth: It includes all Items Less Food & Energy. This measurement, known as 'Core CPI' is widely used by economists because food and energy have very volatile prices. The Inflation rate (CPI NOFE) is the Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                   "Inflation rate (CPI STICK)" = "The Inflation rate (CPI STICK) series is The Sticky Price Consumer Price Index growth. The index is calculated from a subset of goods and services included in the CPI that change price relatively infrequently.The Inflation rate (CPI STICK) is the Sticky Price Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                   "Productivity growth (prod_roh2)" = "Productivity growth (prod_roh2) series is the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                   "2Y backward MA of prod_roh2 growth" = "The 2Y backward MA of prod_roh2 growth series is the two years backward moving average of the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                   "Productivity growth (prod_roh1)" = "The Productivity growth (prod_roh1) series is the Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                   "2Y backward MA of prod_roh1 growth" = "The 2Y backward MA of prod_roh1 growth series is the two years backward moving average of Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                   "Productivity growth (prod_tfp)" = "The Productivity growth (prod_tfp) series is the Total Factor Productivity at Constant National Prices (yearly measured) for United States (Index 2017=1).",
                                   "2Y backward MA of prod_tfp growth" = "The 2Y backward MA of prod_tfp growth series is the two years backward moving average of he Total Factor Productivity at Constant National Prices (yearly measured) for United States",
                                   "M2 aggregate growth" = "The M2 aggregate growth series is the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                   "2Y backward MA of M2 growth" = "The 2Y backward MA of M2 growth series is the two years backward moving average of the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                   "M0 aggregate growth" = "The M0 aggregate growth series is the Monetary base of Federal Reserve Bank (yearly measured) in log-difference multiplied by 100.",
                                   "Velocity of money" = "The Velocity of money series is the number of times one dollar is spent to buy goods and services per unit of time",
                                   "interest rate"= "The interest rate series is the Discount Rate for United States (yearly measured)",
                                   "Wages growth" = "The Wages growth series is the growth of the Wages growth (yearly measured) computed by taking the log-difference multiplied by 100.",
                                   "Real output growth" = "The real output growth series is the real output (yearly measured) for the business sector in log-difference multiplied by 100",
                                   "Nominal output growth" = "The nominal output growth series is the nominal output (yearly measured) for the business sector in log-difference multiplied by 100",
                                   "Unemployment rate (unemprate_civ)" = "The Unemployment rate (unemprate_civ) series is the civilian unemployment rate (yearly measured)",
                                   "Unemployment rate growth (unemplev_ld)" = "The Unemployment rate growth (unemplev_ld) series is the unemployment rate (yearly measured) in log-difference multiplied by 100.")
    descriptionserie2_4 <- switch (input$varselect2_4,
                                   "Empty"="No series has been selected",
                                   "Inflation rate (GDP deflator)" = "The Inflation rate (GDP deflator) series is the price index for business output (yearly measured) in log-difference multiplied by 100. It measure the prices (GDP deflator) percentage growth by year.",
                                   "Inflation rate (CPI NOFE)" = "The Inflation rate (CPI NOFE) series is the Consumer Price Index for All Urban Consumers growth: It includes all Items Less Food & Energy. This measurement, known as 'Core CPI' is widely used by economists because food and energy have very volatile prices. The Inflation rate (CPI NOFE) is the Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                   "Inflation rate (CPI STICK)" = "The Inflation rate (CPI STICK) series is The Sticky Price Consumer Price Index growth. The index is calculated from a subset of goods and services included in the CPI that change price relatively infrequently.The Inflation rate (CPI STICK) is the Sticky Price Consumer Price Index (yearly measured) in log-difference multiplied by 100.",
                                   "Productivity growth (prod_roh2)" = "Productivity growth (prod_roh2) series is the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                   "2Y backward MA of prod_roh2 growth" = "The 2Y backward MA of prod_roh2 growth series is the two years backward moving average of the percentage change at annual rate of the Real output per hour of all persons for the business sector",
                                   "Productivity growth (prod_roh1)" = "The Productivity growth (prod_roh1) series is the Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                   "2Y backward MA of prod_roh1 growth" = "The 2Y backward MA of prod_roh1 growth series is the two years backward moving average of Real output per hour of all persons for the business sector (yearly measured) in log-difference multiplied by 100.",
                                   "Productivity growth (prod_tfp)" = "The Productivity growth (prod_tfp) series is the Total Factor Productivity at Constant National Prices (yearly measured) for United States (Index 2017=1).",
                                   "2Y backward MA of prod_tfp growth" = "The 2Y backward MA of prod_tfp growth series is the two years backward moving average of he Total Factor Productivity at Constant National Prices (yearly measured) for United States",
                                   "M2 aggregate growth" = "The M2 aggregate growth series is the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                   "2Y backward MA of M2 growth" = "The 2Y backward MA of M2 growth series is the two years backward moving average of the aggregated money stock (yearly measured) in log-difference multiplied by 100.",
                                   "M0 aggregate growth" = "The M0 aggregate growth series is the Monetary base of Federal Reserve Bank (yearly measured) in log-difference multiplied by 100.",
                                   "Velocity of money" = "The Velocity of money series is the number of times one dollar is spent to buy goods and services per unit of time",
                                   "interest rate"= "The interest rate series is the Discount Rate for United States (yearly measured)",
                                   "Wages growth" = "The Wages growth series is the growth of the Wages growth (yearly measured) computed by taking the log-difference multiplied by 100.",
                                   "Real output growth" = "The real output growth series is the real output (yearly measured) for the business sector in log-difference multiplied by 100",
                                   "Nominal output growth" = "The nominal output growth series is the nominal output (yearly measured) for the business sector in log-difference multiplied by 100",
                                   "Unemployment rate (unemprate_civ)" = "The Unemployment rate (unemprate_civ) series is the civilian unemployment rate (yearly measured)",
                                   "Unemployment rate growth (unemplev_ld)" = "The Unemployment rate growth (unemplev_ld) series is the unemployment rate (yearly measured) in log-difference multiplied by 100.")
    msg1<-paste(strong("First time series description"))
    msg2<-paste(strong("Second time series description"))
    msg3<-paste(strong("Third time series description"))
    msg4<-paste(strong("Fourth time series description"))
    HTML(paste(msg1,descriptionserie2_1,msg2,descriptionserie2_2,msg3,descriptionserie2_3,msg4,descriptionserie2_4,  sep = "<br/>"))
    
  })
  
  output$scatterplot2 <- renderPlotly({
    z <- window(dtts, start = input$periodslider2[1], end = input$periodslider2[2])
    time_serie_name2_1 <- switch (input$varselect2_1,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    time_serie_name2_2 <- switch (input$varselect2_2,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    time_serie_name2_3 <- switch (input$varselect2_3,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    First_timeserie <- lag(time_serie_name2_1, -(input$lagselection2_1))
    Second_timeserie <- lag(time_serie_name2_2, -(input$lagselection2_2))
    Third_timeserie <- lag(time_serie_name2_3, -(input$lagselection2_3))
    TSstudio::ts_plot(ts_c(First_timeserie,Second_timeserie,Third_timeserie),Xtitle="date",title="Time series plot")
  })
  output$eq_est2 <- renderUI({
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_1), end = (input$periodslider2[2]-input$lagselection2_1))
    time_serie_name2_1 <- switch (input$varselect2_1,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_2), end = (input$periodslider2[2]-input$lagselection2_2))
    time_serie_name2_2 <- switch (input$varselect2_2,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_3), end = (input$periodslider2[2]-input$lagselection2_3))
    time_serie_name2_3 <- switch (input$varselect2_3,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    tsn1 <- ts(time_serie_name2_1, start=input$periodslider2[1], end=input$periodslider2[2])
    tsn2 <- ts(time_serie_name2_2, start=input$periodslider2[1], end=input$periodslider2[2])
    tsn3 <- ts(time_serie_name2_3, start=input$periodslider2[1], end=input$periodslider2[2])
    
    est_eq <- tslm(tsn1~tsn2+tsn3)
    est_eqsum<-summary(est_eq)
    intercept<-est_eqsum$coefficients[1]
    coeff1<-est_eqsum$coefficients[2]
    coeff2<-est_eqsum$coefficients[3]
    intse<-est_eqsum$coefficients[1,2]
    coeff1se<-est_eqsum$coefficients[2,2]
    coeff2se<-est_eqsum$coefficients[3,2]
    intpv<-est_eqsum$coefficients[1,4]
    coeff1pv<-est_eqsum$coefficients[2,4]
    coeff2pv<-est_eqsum$coefficients[3,4]
    rsquared<-est_eqsum$r.squared
    adjrsquared<-est_eqsum$adj.r.squared
    msg0 <- paste(strong("Regression Output 1"))
    msg1 <- paste("______1) Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("______2) ",input$varselect2_2," coefficient: ",round(coeff1,digit=2),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",round(coeff1pv,digit=4)," )")
    msg3 <- paste("______3) ", input$varselect2_3," coefficient: ",round(coeff2,digit=2),"(S.E: ",round(coeff2se,digit=2),"pvalue: ",round(coeff2pv,digit=4)," )")
    msg4 <- paste("______4) R Squared: ", round(rsquared,digit=2))
    msg5 <- paste("______5) Adjusted R Squared: ", round(adjrsquared,digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, msg5, sep = "<br/>"))
    
    
    
  })
  output$eq_est1 <- renderUI({
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_1), end = (input$periodslider2[2]-input$lagselection2_1))
    time_serie_name2_1 <- switch (input$varselect2_1,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_2), end = (input$periodslider2[2]-input$lagselection2_2))
    time_serie_name2_2 <- switch (input$varselect2_2,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    
    tsn1 <- ts(time_serie_name2_1, start=input$periodslider2[1], end=input$periodslider2[2])
    tsn2 <- ts(time_serie_name2_2, start=input$periodslider2[1], end=input$periodslider2[2])
    
    est_eq <- tslm(tsn1~tsn2)
    est_eqsum<-summary(est_eq)
    intercept<-est_eqsum$coefficients[1]
    coeff1<-est_eqsum$coefficients[2]
    intse<-est_eqsum$coefficients[1,2]
    coeff1se<-est_eqsum$coefficients[2,2]
    intpv<-est_eqsum$coefficients[1,4]
    coeff1pv<-est_eqsum$coefficients[2,4]
    rsquared<-est_eqsum$r.squared
    adjrsquared<-est_eqsum$adj.r.squared
    msg0 <- paste(strong("Regression Output 2"))
    msg1 <- paste("______1) Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("______2) ",input$varselect2_2," coefficient: ",round(coeff1,digit=2),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",round(coeff1pv,digit=4)," )")
    msg3 <- paste("______3) R Squared: ", round(rsquared,digit=2))
    msg4 <- paste("______4) Adjusted R Squared: ", round(adjrsquared,digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, sep = "<br/>"))
    
    
    
  })
  
  output$eq_est3 <- renderUI({
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_1), end = (input$periodslider2[2]-input$lagselection2_1))
    time_serie_name2_1 <- switch (input$varselect2_1,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_2), end = (input$periodslider2[2]-input$lagselection2_2))
    time_serie_name2_2 <- switch (input$varselect2_2,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_3), end = (input$periodslider2[2]-input$lagselection2_3))
    time_serie_name2_3 <- switch (input$varselect2_3,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    z <- window(dtts, start = (input$periodslider2[1]-input$lagselection2_4), end = (input$periodslider2[2]-input$lagselection2_4))
    time_serie_name2_4 <- switch (input$varselect2_4,
                                  "Empty"=z[,14],
                                  "Inflation rate (GDP deflator)" = z[,2],
                                  "Inflation rate (CPI NOFE)" = z[,8],
                                  "Inflation rate (CPI STICK)" = z[,9],
                                  "Productivity growth (prod_roh2)" = z[,13],
                                  "2Y backward MA of prod_roh2 growth"= z[,19],
                                  "Productivity growth (prod_roh1)" = z[,5],
                                  "2Y backward MA of prod_roh1 growth"= z[,18],
                                  "Productivity growth (prod_tfp)" = z[,10],
                                  "2Y backward MA of prod_tfp growth"= z[,20],
                                  "M2 aggregate growth" = z[,7],
                                  "2Y backward MA of M2 growth" = z[,17],
                                  "M0 aggregate growth" = z[,16],
                                  "Velocity of money"= z[,21],
                                  "interest rate"= z[,15],
                                  "Wages growth" = z[,6],
                                  "Real output growth" = z[,3],
                                  "Nominal output growth" = z[,4],
                                  "Unemployment rate (unemprate_civ)" = z[,12],
                                  "Unemployment rate growth (unemplev_ld)" = z[,11])
    tsn1 <- ts(time_serie_name2_1, start=input$periodslider2[1], end=input$periodslider2[2])
    tsn2 <- ts(time_serie_name2_2, start=input$periodslider2[1], end=input$periodslider2[2])
    tsn3 <- ts(time_serie_name2_3, start=input$periodslider2[1], end=input$periodslider2[2])
    tsn4 <- ts(time_serie_name2_4, start=input$periodslider2[1], end=input$periodslider2[2])
    
    est_eq <- tslm(tsn1~tsn2+tsn3+tsn4)
    est_eqsum<-summary(est_eq)
    intercept<-est_eqsum$coefficients[1]
    coeff1<-est_eqsum$coefficients[2]
    coeff2<-est_eqsum$coefficients[3]
    coeff3<-est_eqsum$coefficients[4]
    intse<-est_eqsum$coefficients[1,2]
    coeff1se<-est_eqsum$coefficients[2,2]
    coeff2se<-est_eqsum$coefficients[3,2]
    coeff3se<-est_eqsum$coefficients[4,2]
    intpv<-est_eqsum$coefficients[1,4]
    coeff1pv<-est_eqsum$coefficients[2,4]
    coeff2pv<-est_eqsum$coefficients[3,4]
    coeff3pv<-est_eqsum$coefficients[4,4]
    rsquared<-est_eqsum$r.squared
    adjrsquared<-est_eqsum$adj.r.squared
    msg0 <- paste(strong("Regression Output 3"))
    msg1 <- paste("______1) Intercept coefficient: ", round(intercept,digit=2),"(S.E: ",round(intse,digit=2),"pvalue: ",round(intpv,digit=4)," )")
    msg2 <- paste("______2) ",input$varselect2_2," coefficient: ",round(coeff1,digit=2),"(S.E: ",round(coeff1se,digit=2),"pvalue: ",round(coeff1pv,digit=4)," )")
    msg3 <- paste("______3) ", input$varselect2_3," coefficient: ",round(coeff2,digit=2),"(S.E: ",round(coeff2se,digit=2),"pvalue: ",round(coeff2pv,digit=4)," )")
    msg4 <- paste("______4) ", input$varselect2_4," coefficient: ",round(coeff3,digit=2),"(S.E: ",round(coeff3se,digit=2),"pvalue: ",round(coeff3pv,digit=4)," )")
    msg5 <- paste("______5) R Squared: ", round(rsquared,digit=2))
    msg6 <- paste("______6) Adjusted R Squared: ", round(adjrsquared,digit=2))
    HTML(paste(msg0,msg1, msg2, msg3, msg4, msg5, msg6, sep = "<br/>"))
    
    
    
  })
  
  
}
# Run the application------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
