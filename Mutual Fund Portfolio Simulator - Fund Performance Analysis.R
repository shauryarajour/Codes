###########################################################################################################################
################################### CAPSTONE PROJECT - GROUP 9 ############################################################
############### SUBMITTED BY :- SHAURYA RAJOUR/SHITAL GUPTA/SHASHANK SRIVASTAVA/SANJEEV SHARMA ############################
############################### MUTUAL FUND PORTFOLIO SIMULATOR ###########################################################
############################### DATA ANALYSIS THROUGH R PROGRAMMING #######################################################
###########################################################################################################################
######################################## Start of the Analysis ############################################################
#                                                                                                                         #
#   Note:                                                                                                                 #
#   #### starts a new section, ### a new sub-section, ## a new sub-section topic, #- a new comment and # Random Headings  #
#      The starting symbol will match with a respective ending symbol to mark the ending of the topic.                    #
#      Each section will mention the packages to be used in the start for quick reference.                                #
#                                                                                                                         #
########################################## Start Of The Code ##############################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                            MUTUAL FUND PERFORMANCE ANALYSIS                              #
#                                                                                          #
#    We will work on three use cases in this code, given below are details for the same:   #
#                                                                                          #
#  USE CASE 2: DOMINANTS: Identifying the top performing mutual fund schemes in the data   #
#                         based on their historical and predicted performance.             #
#  USE CASE 3: PREDICTOR: Predicting the return for mutual fund schemes for the time       #
#                         horizons 1W,1M,3M,6M,1Y,3Y,5Y.                                   #
#  USE CASE 4: ALLOCATOR: This use case will be performed manually on the dashboard by     # 
#                         assigning the top performing mutual fund schemes based on the    #
#                         analysis done in the last two use cases and investor profile     #
#                         scheme allocation defined earlier.                               #
#                                                                                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#------------------------------------------------------------------------------------------#
####              Step 1 :- Setting the Current Working Directory                       ####
#------------------------------------------------------------------------------------------#
getwd()
#-This should be as per your own local system R directory-#
setwd("C:/Users/shital/Documents/R/RProgramming") 
dir()
#------------------------------------------------------------------------------------------#
####          Step 2 :- Reading and viewing the data from both the dataset              ####
#------------------------------------------------------------------------------------------#

###READING THE DATASET AND STORING DATA INTO TWO SEPERATE DATA FRAMES FOR ANALYSIS###

MFPerfDataO <- read.csv(file.choose()) #-Data Source - Mutual Fund Performance Data.csv-#
#-Seperate datasets are created to ensure we maintain our original data source as is-#

MFPerfData <- MFPerfDataO[,-c(3,4,6,9:12,21:23,25,27,42:84)]
#-Comprises of Mutual Fund Performance Attributes, this will be used for modelling phase-#

MFBaseData <- MFPerfDataO[,c(1,3,4,6,9:12,21:23,25,27,42:84)]
#-Comprises of Mutual Fund Informational Attributes & Performance attributes that we will-#
#-not use during bi-variate analysis & modelling phase because we have either have better-# 
#-representations of the variables present in it in the performance dataset or they are  -#
#-specific to scheme categories or type and hence are not relevant since our model works -#
#-on a generalized approach for analysis of all mutual fund schemes.This dataset will be -#
#-used only to study dominant categories and performance variations during the EDA phase.-#
#-Fund Name was kept common for both the datasets.                                       -#

###VIEWING THE DATASETS TO CHECK IF THEY WERE CORRECTLY CREATED###

View(MFPerfData)
View(MFBaseData)
#-Data has been cleaned already so no additional action required with respect to filtering-# 
#-rows/columns in the data.                                                               -#

#------------------------------------------------------------------------------------------#
####          Step 3 :- Creating the additional variables for analysis                 #####
#------------------------------------------------------------------------------------------#
library(anytime)

#-CHANGES FOR MF BASE DATASET-#

###CREATING THE VARIABLE CALCULATING MUTUAL FUNDS ACTIVE DURATION SINCE LAUNCH###
MFBaseData$Launch <- anydate(MFBaseData$Launch)
MFBaseData$SchemeActiveDuration.InYrs <- as.numeric(round(((Sys.Date()- MFBaseData$Launch)/365),1))

#-CHANGES FOR MF PERF DATASET-#

###CREATING THE RISK RATING FOR THE MUTUAL FUNDS BASED ON THE RISK RATIOS GIVEN###

#-We are not using the risk ratios since a lot of our new schemes do not have values   -#
#-assigned for them, these ratios are only available on the source site if the mutual  -#
#-fund scheme is older than three years.Also we intend to use all the schemes for      -#
#-analysis and assigning a risk rating to the schemes will help us further in          -#
#-generalizing our analysis.                                                           -#

#-We will be analyzing each risk ratio based on their ideal values checked for &       -#
#-verified through our market research and accordingly score them per their variation  -#
#-from the ideal value to finally put together one final score that will help us in    -#
#-deciding the scheme risk rating.                                                     -#

##ASSIGNING THE RISK RATING VARIABLE A DEFAULT VALUE OF 0##
MFPerfData$MFSchemeRiskRating <- 0 

##ADDING SCORES TO RISK RATING VARIABLE FOR EACH MUTUAL FUND BASED ON THEIR VALUES OF RISK RATIOS##

#1. Scoring for STANDARD DEVIATION - Lower the Standard Deviation Better the Score#
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Standard.Deviation) & MFPerfData$Standard.Deviation >= 0.00 & MFPerfData$Standard.Deviation <= 10.00,4,ifelse(is.na(MFPerfData$Standard.Deviation),1,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Standard.Deviation) & MFPerfData$Standard.Deviation >10.00 & MFPerfData$Standard.Deviation <= 20.00,3,ifelse(is.na(MFPerfData$Standard.Deviation),1,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Standard.Deviation) & MFPerfData$Standard.Deviation >20.00 & MFPerfData$Standard.Deviation <= 30.00,2,ifelse(is.na(MFPerfData$Standard.Deviation),1,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Standard.Deviation) & MFPerfData$Standard.Deviation >30.00,1,ifelse(is.na(MFPerfData$Standard.Deviation),1,MFPerfData$MFSchemeRiskRating))
#2. Scoring for SHARPE RATIO - Higher the Sharpe Ratio Better the Score#
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Sharpe.Ratio) & MFPerfData$Sharpe.Ratio > 3.00,MFPerfData$MFSchemeRiskRating+4,ifelse(is.na(MFPerfData$Sharpe.Ratio),MFPerfData$MFSchemeRiskRating+1,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Sharpe.Ratio) & MFPerfData$Sharpe.Ratio > 2.00 & MFPerfData$Sharpe.Ratio <= 3.00 ,MFPerfData$MFSchemeRiskRating+3,ifelse(is.na(MFPerfData$Sharpe.Ratio),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Sharpe.Ratio) & MFPerfData$Sharpe.Ratio >= 1.00 & MFPerfData$Sharpe.Ratio <= 2.00 ,MFPerfData$MFSchemeRiskRating+2,ifelse(is.na(MFPerfData$Sharpe.Ratio),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Sharpe.Ratio) & MFPerfData$Sharpe.Ratio < 1.00 ,MFPerfData$MFSchemeRiskRating+1,ifelse(is.na(MFPerfData$Sharpe.Ratio),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
#3. Scoring for SORTINO RATIO - Higher the Sortino Ratio Better the Score#
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Sortino.Ratio) & MFPerfData$Sortino.Ratio > 8.00,MFPerfData$MFSchemeRiskRating+4,ifelse(is.na(MFPerfData$Sortino.Ratio),MFPerfData$MFSchemeRiskRating+1,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Sortino.Ratio) & MFPerfData$Sortino.Ratio > 4.00 & MFPerfData$Sortino.Ratio <= 8.00 ,MFPerfData$MFSchemeRiskRating+3,ifelse(is.na(MFPerfData$Sortino.Ratio),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Sortino.Ratio) & MFPerfData$Sortino.Ratio >= 2.00 & MFPerfData$Sortino.Ratio <= 4.00 ,MFPerfData$MFSchemeRiskRating+2,ifelse(is.na(MFPerfData$Sortino.Ratio),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Sortino.Ratio) & MFPerfData$Sortino.Ratio < 2.00 ,MFPerfData$MFSchemeRiskRating+1,ifelse(is.na(MFPerfData$Sortino.Ratio),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
#4. Scoring for BETA - Value of 1 is ideal other than that Score would be low#
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Beta) & MFPerfData$Beta == 1.00,MFPerfData$MFSchemeRiskRating+4,ifelse(is.na(MFPerfData$Beta),MFPerfData$MFSchemeRiskRating+1,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Beta) & MFPerfData$Beta > 0.00 & MFPerfData$Beta < 1.00 ,MFPerfData$MFSchemeRiskRating+3,ifelse(is.na(MFPerfData$Beta),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Beta) & MFPerfData$Beta >= -1.00 & MFPerfData$Beta <= 0.00 ,MFPerfData$MFSchemeRiskRating+3,ifelse(is.na(MFPerfData$Beta),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse((MFPerfData$Beta < -1.00 | MFPerfData$Beta > 1.00) & !is.na(MFPerfData$Beta),MFPerfData$MFSchemeRiskRating+1,ifelse(is.na(MFPerfData$Beta),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
#5. Scoring for ALPHA - Higher the Alpha Better the Score#
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Alpha) & MFPerfData$Alpha > 8.00,MFPerfData$MFSchemeRiskRating+4,ifelse(is.na(MFPerfData$Alpha),MFPerfData$MFSchemeRiskRating+1,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Alpha) & MFPerfData$Alpha > 2.00 & MFPerfData$Alpha <= 8.00 ,MFPerfData$MFSchemeRiskRating+3,ifelse(is.na(MFPerfData$Alpha),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Alpha) & MFPerfData$Alpha >= 0.00 & MFPerfData$Alpha <= 2.00 ,MFPerfData$MFSchemeRiskRating+2,ifelse(is.na(MFPerfData$Alpha),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$Alpha) & MFPerfData$Alpha < 0 ,MFPerfData$MFSchemeRiskRating+1,ifelse(is.na(MFPerfData$Alpha),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
#5. Scoring for R-SQUARED - Higher the R-Squared Better the Score#
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$R.Squared ) & MFPerfData$R.Squared > 0.80,MFPerfData$MFSchemeRiskRating+4,ifelse(is.na(MFPerfData$R.Squared),MFPerfData$MFSchemeRiskRating+1,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$R.Squared) & MFPerfData$R.Squared >= 0.41 & MFPerfData$R.Squared <= 0.80 ,MFPerfData$MFSchemeRiskRating+3,ifelse(is.na(MFPerfData$R.Squared),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$R.Squared) & MFPerfData$R.Squared >= 0.20 & MFPerfData$R.Squared <= 0.40 ,MFPerfData$MFSchemeRiskRating+2,ifelse(is.na(MFPerfData$R.Squared),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))
MFPerfData$MFSchemeRiskRating <- ifelse(!is.na(MFPerfData$R.Squared) & MFPerfData$R.Squared < 0.20 ,MFPerfData$MFSchemeRiskRating+1,ifelse(is.na(MFPerfData$R.Squared),MFPerfData$MFSchemeRiskRating,MFPerfData$MFSchemeRiskRating))


##CHECKING IF THE MUTUAL FUND RISK RATING WAS GENERATED CORRECTLY##
str(MFPerfData$MFSchemeRiskRating) #-Structure Check-#
summary(MFPerfData$MFSchemeRiskRating) #-Summary Check-#

##ASSIGNING RISK ASSESSMENT CATEGORIES BASED ON SCORES CALCULATED FOR EACH FUND##
MFPerfData$MF.Risk.Rating <- "A" #Assigning random value to the risk rating variable#
MFPerfData$MF.Risk.Rating <- as.factor(MFPerfData$MF.Risk.Rating) #Conversion to factor since we will be having risk categories assigned#
MFPerfData$MF.Risk.Rating <- ifelse(MFPerfData$MFSchemeRiskRating<8,"High",MFPerfData$MF.Risk.Rating)
MFPerfData$MF.Risk.Rating <- ifelse(MFPerfData$MFSchemeRiskRating>=8 & MFPerfData$MFSchemeRiskRating<=11 ,"Medium-High",MFPerfData$MF.Risk.Rating)
MFPerfData$MF.Risk.Rating <- ifelse(MFPerfData$MFSchemeRiskRating>=12 & MFPerfData$MFSchemeRiskRating<=15 ,"Medium",MFPerfData$MF.Risk.Rating)
MFPerfData$MF.Risk.Rating <- ifelse(MFPerfData$MFSchemeRiskRating>=16 & MFPerfData$MFSchemeRiskRating<=19 ,"Low-Medium",MFPerfData$MF.Risk.Rating)
MFPerfData$MF.Risk.Rating <- ifelse(MFPerfData$MFSchemeRiskRating>=20 & MFPerfData$MFSchemeRiskRating<=24 ,"Low",MFPerfData$MF.Risk.Rating)
MFPerfData$MF.Risk.Rating <- as.factor(MFPerfData$MF.Risk.Rating)

#-We will be removing the variables not required in the last step for this section-#

###CREATING THE FUND TO BENCHMARK EVALUATION VARIABLE FOR THE MUTUAL FUNDS BASED ON###
###                   THE RETURN PERFORMANCE DATA GIVEN                            ###

##ASSIGNING DEFAULT VALUE OF 0 TO THE EVALUATION VARIABLES CREATED BASED ON DIFFERENT TIME HORIZONS##
MFPerfData$FP.BP.Comp.1Wk <- 0
MFPerfData$FP.BP.Comp.1Mth <- 0
MFPerfData$FP.BP.Comp.3Mth <- 0
MFPerfData$FP.BP.Comp.6Mth <- 0
MFPerfData$FP.BP.Comp.1Yr <- 0
MFPerfData$FP.BP.Comp.3Yr <- 0
MFPerfData$FP.BP.Comp.5Yr <- 0
#-The concept that we will be using is if the Fund % return is greater than Benchmark -#
#-% return then a value of 1 will be assigned depicting good performance else the     -#
#-value will be 0 depicting bad performance.                                          -#
#-For new funds which does not have values for all time horizons,0 will marked        -#
#-wherever no value is given so that we can consider them bad performers just for that-# 
#-specific time horizon.                                                              -#

##COMPARING FUND RETURN PERFORMANCE WITH BENCHMARK PERFORMANCE AND ACCORDINGLY ASSIGN ##
##                        VALUES TO PERFORMANCE VARIABLE                              ##

MFPerfData$FP.BP.Comp.1Wk <- ifelse(!is.na(MFPerfData$X1.Wk.Ret....) & MFPerfData$X1.Wk.Ret.... >= MFPerfData$Ben.1.Wk.Ret....,1,ifelse(is.na(MFPerfData$X1.Wk.Ret....),0,0))
MFPerfData$FP.BP.Comp.1Mth <- ifelse(!is.na(MFPerfData$X1.Mth.Ret....) & MFPerfData$X1.Mth.Ret.... >= MFPerfData$Ben.1.Mth.Ret....,1,ifelse(is.na(MFPerfData$X1.Mth.Ret....),0,0))
MFPerfData$FP.BP.Comp.3Mth <- ifelse(!is.na(MFPerfData$X3.Mth.Ret....) & MFPerfData$X3.Mth.Ret.... >= MFPerfData$Ben.3.Mth.Ret....,1,ifelse(is.na(MFPerfData$X3.Mth.Ret....),0,0))
MFPerfData$FP.BP.Comp.6Mth <- ifelse(!is.na(MFPerfData$X6.Mth.Ret....) & MFPerfData$X6.Mth.Ret.... >= MFPerfData$Ben.6.Mth.Ret....,1,ifelse(is.na(MFPerfData$X6.Mth.Ret....),0,0))
MFPerfData$FP.BP.Comp.1Yr <- ifelse(!is.na(MFPerfData$X1.Yr.Ret....) & MFPerfData$X1.Yr.Ret.... >= MFPerfData$Ben.1.Yr.Ret....,1,ifelse(is.na(MFPerfData$X1.Yr.Ret....),0,0))
MFPerfData$FP.BP.Comp.3Yr <- ifelse(!is.na(MFPerfData$X3.Yr.Ret....) & MFPerfData$X3.Yr.Ret.... >= MFPerfData$Ben.3.Yr.Ret....,1,ifelse(is.na(MFPerfData$X3.Yr.Ret....),0,0))
MFPerfData$FP.BP.Comp.5Yr <- ifelse(!is.na(MFPerfData$X5.Yr.Ret....) & MFPerfData$X5.Yr.Ret.... >= MFPerfData$Ben.5.Yr.Ret....,1,ifelse(is.na(MFPerfData$X5.Yr.Ret....),0,0))

##REMOVING THE VARIABLES NOT REQUIRED FOR THE NEXT PART OF THE ANALYSIS##
MFPD <- MFPerfData[,-c(1,2,7:12,30)]
View(MFPD)

#-We have removed Fund Name and Fund Category from MFPERFDATA at the moment since our -# 
#-focus here is more on fund performance.We can add them later on once our analysis   -# 
#-is complete just to understand which fund is performing best and which is not.      -#
#-Risk Ratios and buffer variable used for risk rating were also removed since we     -#
#-already have a risk rating variable that assesses all of these ratios for the funds.-#

#-No variable reductions was done for MFBASEDATA since we are only using it for-#
#-generating insights, we will not use it during modelling phase.              -#

#------------------------------------------------------------------------------------------#
####                     Step 4 :- Exploratory Data Analysis                           #####
#------------------------------------------------------------------------------------------#
library(DataExplorer)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(caret)
library(descr)
library(psych)
library(explore)
library(corrplot)
library(pastecs)
#=========================================================================================#
###                                UNIVARIATE ANALYSIS                                  ###
#=========================================================================================#

#-Step 4.1:- Data Structure Analysis-#

##CHECKING THE DIMENSIONS FOR THE DATA##
dim(MFPD)
dim(MFBaseData)

##READING THE FIRST FIVE LINES FOR THE DATA##
head(MFPD,5)
head(MFBaseData,5)

##CHECKING THE DATA STRUCTURE FOR THE DATA##

str(MFPD)
str(MFBaseData)

#Fixing the data type for Average Maturity and Yield to Maturity Variable#

#-Renaming the Not Applicable Category to 999 for better representation-#
levels(MFBaseData$Average.Maturity..Yrs.)[levels(MFBaseData$Average.Maturity..Yrs.)=="Not Applicable"] <- "999"
levels(MFBaseData$Yield.to.Maturity....)[levels(MFBaseData$Yield.to.Maturity....)=="Not Applicable"] <- "999"
#Converting the variables to numerical values#
MFBaseData$Average.Maturity..Yrs. <- as.numeric(as.character(MFBaseData$Average.Maturity..Yrs.))
MFBaseData$Yield.to.Maturity.... <- as.numeric(as.character(MFBaseData$Yield.to.Maturity....))
MFBaseData$Average.Maturity..Yrs. <- round(MFBaseData$Average.Maturity..Yrs.,2)
MFBaseData$Yield.to.Maturity.... <- round(MFBaseData$Yield.to.Maturity....,2)
str(MFBaseData) #-Structure Recheck-#

#-All the variables are now with correct data type hence no further modifications-#
#-are needed.                                                                    -#

##FIRST LEVEL EDA ANALYSIS FOR THE DATA##
plot_intro(MFPD)
plot_intro(MFBaseData)

#-Step 4.2:- Data Summary Statistics-#

##BASIC SUMMARY FOR THE DATA##
summary(MFPD)
summary(MFBaseData)

##FOR CONTINUOUS DATA - DESCRIPTIVE STATISTICS##
#-Shows measures like skewness,kurtosis and range for the variables-#
psych::describe(MFPD)
psych::describe(MFBaseData)
options(scipen = 100)
options(digits = 2)
#-Shows measures like mean around confidence interval,standard error and covariance-#
#-for the variables.                                                               -#
stat.desc(MFPD, basic = F)
stat.desc(MFBaseData, basic = F)
#-Shows uniqueness of data and if there is zero variance in the variables-#
nearZeroVar(MFPD, saveMetrics = TRUE)
nearZeroVar(MFBaseData, saveMetrics = TRUE)

##FOR CATEGORICAL DATA - FREQUENCY AND PROPORTIONS##
#MF BASE DATASET#
#-Fund.Name will not be analyzed since it is unique for each observation-#
table(MFBaseData$Category)
table(as.factor(MFBaseData$Launch))
table(MFBaseData$Performance.Benchmark)
table(MFBaseData$Bond.Fund.Style)
table(MFBaseData$Average.Credit.Quality)
table(MFBaseData$Fund.Manager..Tenure.)
prop.table(table(MFBaseData$Category))
prop.table(table(as.factor(MFBaseData$Launch)))
prop.table(table(MFBaseData$Performance.Benchmark))
prop.table(table(MFBaseData$Bond.Fund.Style))
prop.table(table(MFBaseData$Average.Credit.Quality))
prop.table(table(MFBaseData$Fund.Manager..Tenure.))
#MF PERF DATASET#
table(MFPD$Fund.Investment.Style)
table(MFPD$Market.Cap)
table(MFPD$MF.Risk.Rating)
prop.table(table(MFPD$Fund.Investment.Style))
prop.table(table(MFPD$Market.Cap))
prop.table(table(MFPD$MF.Risk.Rating))

#-Step 4.3:- General Analysis on Data-#

##ANALYZING DATASETS FOR DOMINANT VARIABLE CATEGORIES/TOP PERFORMING SCHEMES/SCHEME##
##              PERFORMANCE VARIATIONS FOR DIFFERENT TIME PERIODS                  ##

#-Fund.Name will not be used for analysis since it is unique for each observation-#

#-GENERAL DATA ANALYSIS PER EACH MUTUAL FUND PERFORMANCE/INFORMATIONAL ATTRIBUTE-#

#Dominant Fund Category in the data#
as.data.frame(table(MFPerfData$Fund.Category))
#-Data is dominated by Equity & Debt funds-#

#Dominant Fund Category Types in the data#
head(arrange(count(data.frame(MFBaseData$Category), MFBaseData$Category), desc((count(data.frame(MFBaseData$Category), MFBaseData$Category))$n)),10)
#-It was observed that the most common categories for funds were Multicap, Large Cap-# 
#-type under equity category and ELSS and Liquid type under Debt Category           -#

#Dominant Fund Launch Years in the data#
SchemeYr <- as.data.frame(table(data.frame(as.numeric(format(MFBaseData$Launch,'%Y')))))
arrange(SchemeYr,desc(SchemeYr$Freq))
#-Majority of the funds belong to the years 2019 followed by 2018 & 2020-#

#Dominant Fund Investment Style in the data#
as.data.frame(table(MFPerfData$Fund.Investment.Style))
#-Majority of the funds had a Growth or Blend investment Style-#

#Dominant Fund Performance Benchmarks in the data#
head(arrange(count(data.frame(MFBaseData$Performance.Benchmark ), MFBaseData$Performance.Benchmark), desc((count(data.frame(MFBaseData$Performance.Benchmark), MFBaseData$Performance.Benchmark))$n)),10)
#-Majority of the funds were evaluated against CCIL T Bill Liquidity Weight & S&P BSE-#
#-500 TRI benchmarks                                                                   -#

#Dominant Fund Market Cap in the data#
as.data.frame(table(MFPerfData$Market.Cap))
#-Majority of the funds belonged to the Mega & Small Cap categories-#

#Dominant Bond Fund Style and Fund Average Credit Quality in the data#
arrange(count(data.frame(MFBaseData$Bond.Fund.Style), MFBaseData$Bond.Fund.Style), desc((count(data.frame(MFBaseData$Bond.Fund.Style), MFBaseData$Bond.Fund.Style))$n))
arrange(count(data.frame(MFBaseData$Average.Credit.Quality), MFBaseData$Average.Credit.Quality), desc((count(data.frame(MFBaseData$Average.Credit.Quality), MFBaseData$Average.Credit.Quality))$n))
#-Since these attributes are specific to funds with fixed income composition hence-#
#-ignoring not applicable category majority of the funds under this group had a   -#
#-high bond fund style and AAA or GOI/Cash average credit  quality.                 -#

#Dominant Fund managers Groups by count for the data#
head(arrange(count(data.frame(MFBaseData$Fund.Manager..Tenure.), MFBaseData$Fund.Manager..Tenure.), desc((count(data.frame(MFBaseData$Fund.Manager..Tenure.), MFBaseData$Fund.Manager..Tenure.))$n)),10)

#-Through Excel we were able to identify the fund managers managing the maximum number of funds -#
#-Dwijendra Srivastava,Ratish Varier,Rohit Seksaria,Sankaran Naren,Gautam Kaul and Kayzad Eghlim-#
#-managed the maximum number of funds. Post performance analysis we can look out for funds with -#
#-these fund managers provided they are performing well in the market.                          -#

#Top 10 Highest/Lowest/Average/Median Values for Fund Performance Attributes observed in the data#

PerfAttribute <- c("Turnover","Net Assets (Cr)","Standard Deviation","Sharpe Ratio","Sortino Ratio",
                   "Beta","Alpha","R-Squared","Latest NAV","Minimum Investment","Exit Load",
                   "1Wk Ret (%)","1Mth Ret (%)","3Mth Ret (%)","6Mth Ret (%)","1Yr Ret (%)",
                   "3Yr Ret (%)","5Yr Ret (%)","Ben 1Wk Ret (%)","Ben 1Mth Ret (%)",
                   "Ben 3Mth Ret (%)","Ben 6Mth Ret (%)","Ben 1Yr Ret (%)","Ben 3Yr Ret (%)",
                   "Ben 5Yr Ret (%)","Average Maturity(Yrs)","Yield to Maturity(%)",
                   "Previous NAV","52-Week High NAV","52-Week Low NAV","Expense Ratio(%)",
                   "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020",
                   "Q1-Mar 2018","Q2-Jun 2018","Q3-Sep 2018","Q4-Dec 2018","Q5-Mar 2019",
                   "Q6-Jun 2019","Q7-Sep 2019","Q8-Dec 2019","Q9-Mar 2020","Q10-Jun 2020",
                   "Q11-Sep 2020","Q12-Dec 2020","Jan 2020","Feb 2020","Mar 2020","Apr 2020",
                   "May 2020","Jun 2020","Jul 2020","Aug 2020","Sep 2020","Oct 2020","Nov 2020",
                   "Dec 2020","6-Nov","13-Nov","20-Nov","27-Nov","4-Dec","11-Dec","18-Dec","25-Dec","1-Jan")

PerfAnalysis <- data.frame(PerfAttribute,"HighestValue","LowestValue", "AverageValue", "MedianValue")
PerfAnalysis$X.HighestValue. <- 0
PerfAnalysis$X.LowestValue. <- 0
PerfAnalysis$X.AverageValue. <- 0
PerfAnalysis$X.MedianValue. <- 0
PerfAttData <- data.frame(MFPerfData[,c(5:29)],MFBaseData[,c(7:12,14:56)])
options(scipen = 999)
for(i in 1:74)
{
  
  PerfAnalysis[i,2] <- rbind(max(PerfAttData[,i], na.rm = TRUE))
  PerfAnalysis[i,3] <- rbind(min(PerfAttData[,i], na.rm = TRUE))
  PerfAnalysis[i,4] <- rbind((mean(PerfAttData[,i], na.rm = TRUE)))
  PerfAnalysis[i,5] <- rbind(median(PerfAttData[,i], na.rm = TRUE))
}
#-Fixing the highest values for Averate Maturity and & Yield to Maturity-#
PerfAnalysis$X.HighestValue.[26] <- 23.63
PerfAnalysis$X.HighestValue.[27] <- 13.70
#-Viewing the updated data frame for analysis-#
View(PerfAnalysis)
#-This data frame created above gives us an idea on how each performance oriented attribute-#
#-is variating amongst all funds.You can understand which funds are performing best in the -#
#-next section but this gives a general overview for all the funds.                        -#

#Performance Benchmarks with Highest & Lowest Returns Per Time Period#

BencData <- data.frame(MFBaseData$Performance.Benchmark, MFPerfData[,c(23:29)])

#Benchmarks with Highest Returns for different time periods#

BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.1.Wk.Ret....) %>% arrange(desc(Ben.1.Wk.Ret....)) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.1.Mth.Ret....) %>% arrange(desc(Ben.1.Mth.Ret....)) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.3.Mth.Ret....) %>% arrange(desc(Ben.3.Mth.Ret....)) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.6.Mth.Ret....) %>% arrange(desc(Ben.6.Mth.Ret....)) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.1.Yr.Ret....) %>% arrange(desc(Ben.1.Yr.Ret....)) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.3.Yr.Ret....) %>% arrange(desc(Ben.3.Yr.Ret....)) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.5.Yr.Ret....) %>% arrange(desc(Ben.5.Yr.Ret....)) %>% head(10)
#-Highest returns were observed for Nifty 500 Value 50 TRI in monthly/weekly returns,NIFTY IT TRI & S&P BSE Energy TRI in yearly returns-# 

#Benchmarks with Lowest Returns for different time periods#

BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.1.Wk.Ret....) %>% arrange(Ben.1.Wk.Ret....) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.1.Mth.Ret....) %>% arrange(Ben.1.Mth.Ret....) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.3.Mth.Ret....) %>% arrange(Ben.3.Mth.Ret....) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.6.Mth.Ret....) %>% arrange(Ben.6.Mth.Ret....) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.1.Yr.Ret....) %>% arrange(Ben.1.Yr.Ret....) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.3.Yr.Ret....) %>% arrange(Ben.3.Yr.Ret....) %>% head(10)
BencData %>% distinct(MFBaseData.Performance.Benchmark,.keep_all=TRUE) %>% select(MFBaseData.Performance.Benchmark,Ben.5.Yr.Ret....) %>% arrange(Ben.5.Yr.Ret....) %>% head(10)

#-Lowest returns were observed for Domestic Price of Gold in monthly returns, Nifty IT TRI for weekly return, S&P BSE PSU TRI in yearly returns-#

##TOP 10 PERFORMING FUNDS PER EACH MUTUAL FUND PERFORMANCE ATTRIBUTE##

PerfData <- data.frame(MFPerfData$Fund.Name,PerfAttData[,-c(19:25)])
PerfDataHigh <- PerfData[,-c(4,11,12,20,25)]
PerfDataLow <- PerfData[,c(1,4,11,12,20,25)]

#Top 10 Oldest Mutual Funds in the data#
head(arrange(MFBaseData[,c(1,3,57)], desc(MFBaseData$SchemeActiveDuration)),10)

#Top 10 Mutual Funds Per Each Performance Attribute - Higher Values Preferred#

#-This group will have Turnover, NetAssets, Risk Statistics,NAV Statistics,Aggregate    -#
#-Returns(7 time horizons),Yield To Maturity,yearly(2011-2020), quarterly(12 quarters), -#
#-monthly(12 months)and weekly (9 weeks) returns.                                       -#

for(i in 2:63)
{
  print("----------------------------------------------------------------------------------")
  print(head(arrange(PerfDataHigh[,c(1,i)], desc(PerfDataHigh[,i])),10))
  print("----------------------------------------------------------------------------------")
}

#Ignoring 999 value for yield to maturity and updated corrected results#
PerfDataHigh[,c(1,17)] %>% filter(Yield.to.Maturity....<999) %>% arrange(desc((Yield.to.Maturity....))) %>% head(10)

#Top 10 Mutual Funds Per Each Performance Attribute - Lower Values Preferred#

#-This group contains Standard Deviation, Minimum Investment, Exit Load,Average Maturity & -#
#-Expense Ratio.                                                                           -#

for(i in 2:6)
{
  print("----------------------------------------------------------------------------------")
  print(head(PerfDataLow[order(PerfDataLow[,i]),][,c(1,i)],10))
  print("----------------------------------------------------------------------------------")
}

#-Step 4.4:- Null Values, Outliers, Duplicate Values and Negative Values Analysis-#

##NULL VALUE ANALYSIS##

#-No changes will be done to the MF Base dataset since we will be not using it during  -#
#-the modelling phase, it is only used for generating insights for all schemes overall.-#

any(is.na(MFPD))
colSums(is.na(MFPD))


#-Only Fund % Return variables have null values, although imputation should not be-#
#-done since this is performance but just for our ease of analysis and also since -#
#-we are using individual time horizons for modelling we will be imputing 0 as    -#
#-return value.0 would signify no performance for the time horizon so even if a   -#
#-value is predicted for the same they will still be very very low and hence will -#
#-not show up in the top performing mutual fund schemes.Also once the data is     -#
#-refreshed again these values with time will slowly get filled in with the right -#
#-performance values so this should not be an issue.                              -#

#-Replacing NA with value 0 for the return variables-#
MFPD$X3.Mth.Ret.... <- replace_na(MFPD$X3.Mth.Ret....,0)
MFPD$X6.Mth.Ret.... <- replace_na(MFPD$X6.Mth.Ret....,0)
MFPD$X1.Yr.Ret.... <- replace_na(MFPD$X1.Yr.Ret....,0)
MFPD$X3.Yr.Ret.... <- replace_na(MFPD$X3.Yr.Ret....,0)
MFPD$X5.Yr.Ret.... <- replace_na(MFPD$X5.Yr.Ret....,0)
colSums(is.na(MFPD)) #-Verified here that the NA values were correctly replaced-#

##NEGATIVE AND DUPLICATE VALUE ANALYSIS##
colSums(MFBaseData<0, na.rm = TRUE)
colSums(MFPD<0, na.rm = TRUE)
#-This is again for analysis only since the negative records depict the mutual    -#
#-fund's performance hence we will not be treating them, they will be kept as is. -#
#-Duplicate Values will also has certain significance in our data hence no        -#
#-treatment required from that perspective as well.                               -#

##OUTLIER TREATMENT ALSO WONT BE DONE FOR OUR DATA AS THE HIGH VALUES RELATE TO THE##
##PERFORMANCE OF THE MUTUAL FUNDS AND HENCE IMPUTATION WILL NOT BE LOGICALLY       ##
##CORRECT HERE                                                                     ##

#-Step 4.5:- Data Visualization - Univariate Analysis-#

##CONTINUOUS DATA - HISTOGRAMS, BOXPLOTS AND KERNEL DENSITY PLOTS##

#HISTOGRAMS#
MFPD[,-c(1,2,22)] %>% gather() %>% ggplot(aes(value,na.rm=TRUE)) + facet_wrap(~ key, scales = "free") + geom_histogram(fill = "blue")
MFBaseData[,c(7:12)] %>%  gather() %>% ggplot(aes(value,na.rm=TRUE)) + facet_wrap(~ key, scales = "free") + geom_histogram(fill = "blue")
MFBaseData[,c(14:23)] %>%  gather() %>% ggplot(aes(value,na.rm=TRUE)) + facet_wrap(~ key, scales = "free") + geom_histogram(fill = "blue")
MFBaseData[,c(24:35)] %>%  gather() %>% ggplot(aes(value,na.rm=TRUE)) + facet_wrap(~ key, scales = "free") + geom_histogram(fill = "blue")
MFBaseData[,c(36:47)] %>%  gather() %>% ggplot(aes(value,na.rm=TRUE)) + facet_wrap(~ key, scales = "free") + geom_histogram(fill = "blue")
MFBaseData[,c(48:56)] %>%  gather() %>% ggplot(aes(value,na.rm=TRUE)) + facet_wrap(~ key, scales = "free") + geom_histogram(fill = "blue")
#-This shows the data distribution for numerical variables and how they are        -#
#-variating from a normal distribution                                             -#

#BOXPLOTS#
MFPD[,-c(1,2,22)] %>% gather() %>% ggplot(aes(x="", y=value, na.rm = TRUE)) + facet_wrap(~ key, scales = "free") + geom_boxplot()
MFBaseData[,c(7:12)] %>% gather() %>% ggplot(aes(x="", y=value, na.rm = TRUE)) + facet_wrap(~ key, scales = "free") + geom_boxplot()
MFBaseData[,c(14:23)] %>% gather() %>% ggplot(aes(x="", y=value, na.rm = TRUE)) + facet_wrap(~ key, scales = "free") + geom_boxplot()
MFBaseData[,c(24:35)] %>% gather() %>% ggplot(aes(x="", y=value, na.rm = TRUE)) + facet_wrap(~ key, scales = "free") + geom_boxplot()
MFBaseData[,c(36:47)] %>% gather() %>% ggplot(aes(x="", y=value, na.rm = TRUE)) + facet_wrap(~ key, scales = "free") + geom_boxplot()
MFBaseData[,c(48:56)] %>% gather() %>% ggplot(aes(x="", y=value, na.rm = TRUE)) + facet_wrap(~ key, scales = "free") + geom_boxplot()
#-This can give us an idea on the variables with outliers but as said these values -#
#-are required for analysis hence we will not treat them.                          -#

#KERNEL DENSITY PLOTS#
MFPD[,-c(1,2,22)] %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density(fill = "light blue") + theme(strip.text.x = element_text(face = "bold"))
MFBaseData[,c(7:12)] %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density(fill = "light blue") + theme(strip.text.x = element_text(face = "bold"))
MFBaseData[,c(14:23)] %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density(fill = "light blue") + theme(strip.text.x = element_text(face = "bold"))
MFBaseData[,c(24:35)] %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density(fill = "light blue") + theme(strip.text.x = element_text(face = "bold"))
MFBaseData[,c(36:47)] %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density(fill = "light blue") + theme(strip.text.x = element_text(face = "bold"))
MFBaseData[,c(48:56)] %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density(fill = "light blue") + theme(strip.text.x = element_text(face = "bold"))
#-This can give us an idea on how much skewness is present in each numerical variable-#

##CATEGORICAL DATA - BAR CHARTS REPRESENTATION##

#BAR CHARTS#
MFPD[,c(1,2,22)] %>% gather() %>% ggplot(aes(value)) + 
    facet_wrap(~ key, scales = "free") +
    geom_bar(fill = "light blue") + 
    geom_text(stat = "count", aes(label = ..count..),vjust = 1.6) + 
    labs(y = "Frequency", x="Categories") + 
    theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10),strip.text.x = element_text(face="bold"))

MFBaseData[,c(5,6)] %>% gather() %>% ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") +
  geom_bar(fill = "light blue") + 
  geom_text(stat = "count", aes(label = ..count..),vjust = 1.6) + 
  labs(y = "Frequency", x="Categories") + 
  theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10),strip.text.x = element_text(face="bold"))

#Fund Category Type#
ggplot(MFBaseData,aes(MFBaseData$Category)) + geom_bar(fill = "light blue") + 
geom_text(stat = "count", aes(label = ..count..),vjust = 1.6) + coord_flip()+
labs(y = "Frequency", x="Categories") + 
theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))

#Performance Benchmarks#
ggplot(MFBaseData,aes(MFBaseData$Performance.Benchmark)) + geom_bar(fill = "light blue") + 
  geom_text(stat = "count", aes(label = ..count..),vjust = 1.6) + coord_flip()+
  labs(y = "Frequency", x="Categories") + 
  theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))

#-We are not assessing fund managers here as there are too many groups in it visual-#
#-representation for the same is not feasible                                      -#

#-THE NEXT SECTION OF ANALYSIS WILL BE DONE ONLY ON PERFORMANCE DATASET (MFPERFDATA)    -#
#-WITH OUR RELEVANT VARIABLES THAT WE WILL BE MOVING AHEAD WITH FOR THE MODELLING PHASE.-#

#=========================================================================================#
###                                BIVARIATE ANALYSIS                                   ###
#=========================================================================================#

#-We have seven time horizons available to us based on which we will focus our bi-variate-#
#-analysis for variables. The time horizons will be used individually as dependent       -#
#-variables for analysis with respect to other variables. One Way Anova for categorical  -#
#-data while Pearson Correlation for continuous data will be used for analysis.For data  -#
#-visualization we will be using Scatter Plot, Density Plot and Box plot for analysis    -#

#-Step 4.6:- Correlation Analysis-#

##ONE WAY ANOVA TEST FOR CATEGORICAL DATA - SIGNIFICANCE ANALYSIS##

#-For 1 Wk Ret-#
summary(aov(X1.Wk.Ret....~Fund.Investment.Style + Market.Cap + MF.Risk.Rating , data = MFPD))
#-For 1 Mth Ret-#
summary(aov(X1.Mth.Ret....~Fund.Investment.Style + Market.Cap + MF.Risk.Rating , data = MFPD))
#-For 3 Mth Ret-#
summary(aov(X3.Mth.Ret....~Fund.Investment.Style + Market.Cap + MF.Risk.Rating , data = MFPD))
#-For 6 Mth Ret-#
summary(aov(X6.Mth.Ret....~Fund.Investment.Style + Market.Cap + MF.Risk.Rating , data = MFPD))
#-For 1 Yr Ret-#
summary(aov(X1.Yr.Ret....~Fund.Investment.Style + Market.Cap + MF.Risk.Rating , data = MFPD))
#-For 3 Yr Ret-#
summary(aov(X3.Yr.Ret....~Fund.Investment.Style + Market.Cap + MF.Risk.Rating , data = MFPD))
#-For 5 Yr Ret-#
summary(aov(X5.Yr.Ret....~Fund.Investment.Style + Market.Cap + MF.Risk.Rating , data = MFPD))

#-As seen above from the one way anova test we can deduce that all three variables are -#
#-significant and hence are to be kept for the next part of the analysis.              -#

##PEARSON CORRELATION ANALYSIS FOR CONTINUOUS NUMERICAL DATA##

#-Calculating the correlation coefficient for the mutual fund data and plotting the    -#
#-correlation chart for analysis.                                                      -#

#-For 1 Wk Ret-#
cor(MFPD[,c(3:8,15,23)])
corrplot(cor(MFPD[,c(3:8,15,23)]) , method = "number", type = "lower", tl.cex = 1.0)
#-For 1 Mth Ret-#
cor(MFPD[,c(3:7,9,16,24)])
corrplot(cor(MFPD[,c(3:7,9,16,24)]) , method = "number", type = "lower", tl.cex = 1.0)
#-For 3 Mth Ret-#
cor(MFPD[,c(3:7,10,17,25)])
corrplot(cor(MFPD[,c(3:7,10,17,25)]) , method = "number", type = "lower", tl.cex = 1.0)
#-For 6 Mth Ret-#
cor(MFPD[,c(3:7,11,18,26)])
corrplot(cor(MFPD[,c(3:7,11,18,26)]) , method = "number", type = "lower", tl.cex = 1.0)
#-For 1 Yr Ret-#
cor(MFPD[,c(3:7,12,19,27)])
corrplot(cor(MFPD[,c(3:7,12,19,27)]), method = "number", type = "lower", tl.cex = 1.0)
#-For 3 Yr Ret-#
cor(MFPD[,c(3:7,13,20,28)])
corrplot(cor(MFPD[,c(3:7,13,20,28)]), method = "number", type = "lower", tl.cex = 1.0)
#-For 5 Yr Ret-#
cor(MFPD[,c(3:7,14,21,29)])
corrplot(cor(MFPD[,c(3:7,14,21,29)]), method = "number", type = "lower", tl.cex = 1.0)

#-Based on our analysis above we see a high correlation between Benchmark and Fund Returns -#
#-for mutual funds in certain time horizons thereby indicating 3Mth and 6Mth Returns for   -#
#-funds to be matching the benchmark performances thus inferring a good performance overall-#
#-for those time horizons. We will be using Random Forest models without Benchmark         -#
#-performance variables as we already have a performance representation variable for the   -#
#-same in the data so the collinearity issue will be sorted out with it.                   -#

#-Step 4.7:- Data Visualization - Bivariate Analysis-#

##KERNEL DENSITY PLOTS WITH RESPECT TO OUR CATEGORICAL DATA##

#FOR FUND INVESTMENT STYLE#

#-For 1 Wk Ret-#
ggplot(MFPD, aes(x=X1.Wk.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_density() + labs(title = "1Wk Return Data Distribution by Fund Investment Style")
#-For 1 Mth Ret-#
ggplot(MFPD, aes(x=X1.Mth.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_density() + labs(title = "1Mth Return Data Distribution by Fund Investment Style")
#-For 3 Mth Ret-#
ggplot(MFPD, aes(x=X3.Mth.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_density() + labs(title = "3Mth Return Data Distribution by Fund Investment Style")
#-For 6 Mth Ret-#
ggplot(MFPD, aes(x=X6.Mth.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_density() + labs(title = "6Mth Return Data Distribution by Fund Investment Style")
#-For 1 Yr Ret-#
ggplot(MFPD, aes(x=X1.Yr.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_density() + labs(title = "1Yr Return Data Distribution by Fund Investment Style")
#-For 3 Yr Ret-#
ggplot(MFPD, aes(x=X3.Yr.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_density() + labs(title = "3Yr Return Data Distribution by Fund Investment Style")
#-For 5 Yr Ret-#
ggplot(MFPD, aes(x=X5.Yr.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_density() + labs(title = "5Yr Return Data Distribution by Fund Investment Style")

#FOR MARKET CAP#

#-For 1 Wk Ret-#
ggplot(MFPD, aes(x=X1.Wk.Ret....,fill = MFPD$Market.Cap)) + geom_density() + labs(title = "1Wk Return Data Distribution by Market Cap")
#-For 1 Mth Ret-#
ggplot(MFPD, aes(x=X1.Mth.Ret....,fill = MFPD$Market.Cap)) + geom_density() + labs(title = "1Mth Return Data Distribution by Market Cap")
#-For 3 Mth Ret-#
ggplot(MFPD, aes(x=X3.Mth.Ret....,fill = MFPD$Market.Cap)) + geom_density() + labs(title = "3Mth Return Data Distribution by Market Cap")
#-For 6 Mth Ret-#
ggplot(MFPD, aes(x=X6.Mth.Ret....,fill = MFPD$Market.Cap)) + geom_density() + labs(title = "6Mth Return Data Distribution by Market Cap")
#-For 1 Yr Ret-#
ggplot(MFPD, aes(x=X1.Yr.Ret....,fill = MFPD$Market.Cap)) + geom_density() + labs(title = "1Yr Return Data Distribution by Market Cap")
#-For 3 Yr Ret-#
ggplot(MFPD, aes(x=X3.Yr.Ret....,fill = MFPD$Market.Cap)) + geom_density() + labs(title = "3Yr Return Data Distribution by Market Cap")
#-For 5 Yr Ret-#
ggplot(MFPD, aes(x=X5.Yr.Ret....,fill = MFPD$Market.Cap)) + geom_density() + labs(title = "5Yr Return Data Distribution by Market Cap")

#FOR RISK RATING#

#-For 1 Wk Ret-#
ggplot(MFPD, aes(x=X1.Wk.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_density() + labs(title = "1Wk Return Data Distribution by Scheme Risk Rating")
#-For 1 Mth Ret-#
ggplot(MFPD, aes(x=X1.Mth.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_density() + labs(title = "1Mth Return Data Distribution by Scheme Risk Rating")
#-For 3 Mth Ret-#
ggplot(MFPD, aes(x=X3.Mth.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_density() + labs(title = "3Mth Return Data Distribution by Scheme Risk Rating")
#-For 6 Mth Ret-#
ggplot(MFPD, aes(x=X6.Mth.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_density() + labs(title = "6Mth Return Data Distribution by Scheme Risk Rating")
#-For 1 Yr Ret-#
ggplot(MFPD, aes(x=X1.Yr.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_density() + labs(title = "1Yr Return Data Distribution by Scheme Risk Rating")
#-For 3 Yr Ret-#
ggplot(MFPD, aes(x=X3.Yr.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_density() + labs(title = "3Yr Return Data Distribution by Scheme Risk Rating")
#-For 5 Yr Ret-#
ggplot(MFPD, aes(x=X5.Yr.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_density() + labs(title = "5Yr Return Data Distribution by Scheme Risk Rating")

##BOX PLOTS WITH RESPECT TO OUR CATEGORICAL DATA##

#FOR FUND INVESTMENT STYLE#

#-For 1 Wk Ret-#
ggplot(MFPD, aes(x="", y=X1.Wk.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_boxplot() + labs(title = "1Wk Return Data Variation by Fund Investment Style")
#-For 1 Mth Ret-#
ggplot(MFPD, aes(x="", y=X1.Mth.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_boxplot() + labs(title = "1Mth Return Data Variation by Fund Investment Style")
#-For 3 Mth Ret-#
ggplot(MFPD, aes(x="", y=X3.Mth.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_boxplot() + labs(title = "3Mth Return Data Variation by Fund Investment Style")
#-For 6 Mth Ret-#
ggplot(MFPD, aes(x="", y=X6.Mth.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_boxplot() + labs(title = "6Mth Return Data Variation by Fund Investment Style")
#-For 1 Yr Ret-#
ggplot(MFPD, aes(x="", y=X1.Yr.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_boxplot() + labs(title = "1Yr Return Data Variation by Fund Investment Style")
#-For 3 Yr Ret-#
ggplot(MFPD, aes(x="", y=X3.Yr.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_boxplot() + labs(title = "3Yr Return Data Variation by Fund Investment Style")
#-For 5 Yr Ret-#
ggplot(MFPD, aes(x="", y=X5.Yr.Ret....,fill = MFPD$Fund.Investment.Style)) + geom_boxplot() + labs(title = "5Yr Return Data Variation by Fund Investment Style")

#FOR MARKET CAP#

#-For 1 Wk Ret-#
ggplot(MFPD, aes(x="", y=X1.Wk.Ret....,fill = MFPD$Market.Cap)) + geom_boxplot() + labs(title = "1Wk Return Data Variation by Market Cap")
#-For 1 Mth Ret-#
ggplot(MFPD, aes(x="", y=X1.Mth.Ret....,fill = MFPD$Market.Cap)) + geom_boxplot() + labs(title = "1Mth Return Data Variation by Market Cap")
#-For 3 Mth Ret-#
ggplot(MFPD, aes(x="", y=X3.Mth.Ret....,fill = MFPD$Market.Cap)) + geom_boxplot() + labs(title = "3Mth Return Data Variation by Market Cap")
#-For 6 Mth Ret-#
ggplot(MFPD, aes(x="", y=X6.Mth.Ret....,fill = MFPD$Market.Cap)) + geom_boxplot() + labs(title = "6Mth Return Data Variation by Market Cap")
#-For 1 Yr Ret-#
ggplot(MFPD, aes(x="", y=X1.Yr.Ret....,fill = MFPD$Market.Cap)) + geom_boxplot() + labs(title = "1Yr Return Data Variation by Market Cap")
#-For 3 Yr Ret-#
ggplot(MFPD, aes(x="", y=X3.Yr.Ret....,fill = MFPD$Market.Cap)) + geom_boxplot() + labs(title = "3Yr Return Data Variation by Market Cap")
#-For 5 Yr Ret-#
ggplot(MFPD, aes(x="", y=X5.Yr.Ret....,fill = MFPD$Market.Cap)) + geom_boxplot() + labs(title = "5Yr Return Data Variation by Market Cap")

#FOR RISK RATING#

#-For 1 Wk Ret-#
ggplot(MFPD, aes(x="", y=X1.Wk.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_boxplot() + labs(title = "1Wk Return Data Variation by Scheme Risk Rating")
#-For 1 Mth Ret-#
ggplot(MFPD, aes(x="", y=X1.Mth.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_boxplot() + labs(title = "1Mth Return Data Variation by Scheme Risk Rating")
#-For 3 Mth Ret-#
ggplot(MFPD, aes(x="", y=X3.Mth.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_boxplot() + labs(title = "3Mth Return Data Variation by Scheme Risk Rating")
#-For 6 Mth Ret-#
ggplot(MFPD, aes(x="", y=X6.Mth.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_boxplot() + labs(title = "6Mth Return Data Variation by Scheme Risk Rating")
#-For 1 Yr Ret-#
ggplot(MFPD, aes(x="", y=X1.Yr.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_boxplot() + labs(title = "1Yr Return Data Variation by Scheme Risk Rating")
#-For 3 Yr Ret-#
ggplot(MFPD, aes(x="", y=X3.Yr.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_boxplot() + labs(title = "3Yr Return Data Variation by Scheme Risk Rating")
#-For 5 Yr Ret-#
ggplot(MFPD, aes(x="", y=X5.Yr.Ret....,fill = MFPD$MF.Risk.Rating)) + geom_boxplot() + labs(title = "5Yr Return Data Variation by Scheme Risk Rating")

#-Data distribution for 1Yr, 3Yr and 5Yr returns had more visible patterns as compared to-#
#-others for both boxplot and density plots.                                             -#

##SCATTER PLOT FOR CONTINUOUS NUMERICAL DATA##

#-For 1 Wk Ret-#
OWR <- gather(MFPD[,-c(1,2,22)], variable, value, -X1.Wk.Ret....)
ggplot(OWR) +
geom_jitter(aes(value,X1.Wk.Ret...., colour=variable)) + 
geom_smooth(aes(value,X1.Wk.Ret...., colour=variable), method=lm) +
facet_wrap(~variable, scales="free_x") +
labs(title="Relation Of 1Wk Return With Other Features")
#-For 1 Mth Ret-#
OMR <- gather(MFPD[,-c(1,2,22)], variable, value, -X1.Mth.Ret....)
ggplot(OMR) +
  geom_jitter(aes(value,X1.Mth.Ret...., colour=variable)) + 
  geom_smooth(aes(value,X1.Mth.Ret...., colour=variable), method=lm) +
  facet_wrap(~variable, scales="free_x") +
  labs(title="Relation Of 1Mth Return With Other Features")
#-For 3 Mth Ret-#
TMR <- gather(MFPD[,-c(1,2,22)], variable, value, -X3.Mth.Ret....)
ggplot(TMR) +
  geom_jitter(aes(value,X3.Mth.Ret...., colour=variable)) + 
  geom_smooth(aes(value,X3.Mth.Ret...., colour=variable), method=lm) +
  facet_wrap(~variable, scales="free_x") +
  labs(title="Relation Of 3Mth Return With Other Features")
#-For 6 Mth Ret-#
SMR <- gather(MFPD[,-c(1,2,22)], variable, value, -X6.Mth.Ret....)
ggplot(SMR) +
  geom_jitter(aes(value,X6.Mth.Ret...., colour=variable)) + 
  geom_smooth(aes(value,X6.Mth.Ret...., colour=variable), method=lm) +
  facet_wrap(~variable, scales="free_x") +
  labs(title="Relation Of 6Mth Return With Other Features")
#-For 1 Yr Ret-#
OYR <- gather(MFPD[,-c(1,2,22)], variable, value, -X1.Yr.Ret....)
ggplot(OYR) +
  geom_jitter(aes(value,X1.Yr.Ret...., colour=variable)) + 
  geom_smooth(aes(value,X1.Yr.Ret...., colour=variable), method=lm) +
  facet_wrap(~variable, scales="free_x") +
  labs(title="Relation Of 1Yr Return With Other Features")
#-For 3 Yr Ret-#
TYR <- gather(MFPD[,-c(1,2,22)], variable, value, -X3.Yr.Ret....)
ggplot(TYR) +
  geom_jitter(aes(value,X3.Yr.Ret...., colour=variable)) + 
  geom_smooth(aes(value,X3.Yr.Ret...., colour=variable), method=lm) +
  facet_wrap(~variable, scales="free_x") +
  labs(title="Relation Of 3Yr Return With Other Features")
#-For 5 Yr Ret-#
FYR <- gather(MFPD[,-c(1,2,22)], variable, value, -X5.Yr.Ret....)
ggplot(FYR) +
  geom_jitter(aes(value,X5.Yr.Ret...., colour=variable)) + 
  geom_smooth(aes(value,X5.Yr.Ret...., colour=variable), method=lm) +
  facet_wrap(~variable, scales="free_x") +
  labs(title="Relation Of 5Yr Return With Other Features")

#-This gives us a rough idea on how all our numerical independent variables variate-#
#-with the returns variables,it was observed some of the return variables show a   -#
#-linear relationship with our time horizon variable in question.                  -#

#------------------------------------------------------------------------------------------#
####                        Step 5 :- Data Partitioning                                #####
#------------------------------------------------------------------------------------------#

#-Since the attributes in the data are not specific to any fund categories or      -#
#-investment style types hence we are good to divide the data without considering  -#
#-these diversifications in mind. We will be dividing the data into Train and Test -#
#-data with a ratio of 70:30 where the sample will be picked randomly from the data-#
#-source.                                                                          -#

###CREATING THE TRAIN AND TEST DATASETS BASED ON 70:30 RATIO###

#-Randomizing the sample so that all categories of funds can be picked in both -#
#-Train & Test datasets                                                        -#

set.seed(100) 
DataSample <- sample(2, nrow(MFPD), prob = c(0.7,0.3),replace=T)
TrainO <-  MFPD[DataSample==1,]
TestO <-  MFPD[DataSample==2,]

#-Backing up original train and test datasets for reference-# 
Train <- TrainO
Test <- TestO

#------------------------------------------------------------------------------------------#
####                       Step 6 :- Data Modelling                                    #####
#------------------------------------------------------------------------------------------#

#-We will be building three seperate models for each time horizon given for the  -#
#-funds the techniques that we will be using here is Random Forest,Support Vector-#
#-Regression and Gradient Boosting. Based on them we will be training our train  -#
#-data and predicting returns for test data.                                     -#

library(mlbench)
library(randomForest)
library(caret)
library(writexl)
library(MLmetrics)
library(gbm)
library(rminer)

###CREATING TRAIN DATA PER TIME HORIZON###

Train1Wk <-  Train[,-c(9:21,24:29)]
Train1Mth <- Train[,-c(8,10:21,23,25:29)]
Train3Mth <- Train[,-c(8:9,11:21,23:24,26:29)]
Train6Mth <- Train[,-c(8:10,12:21,23:25,27:29)]
Train1Yr <-  Train[,-c(8:11,13:21,23:26,28:29)]
Train3Yr <-  Train[,-c(8:12,14:21,23:27,29)]
Train5Yr <-  Train[,-c(8:13,15:21,23:28)]

#-Benchmark Performances were removed from data from here since we already have a  -#
#-variable assessing the performance for the same with respect to fund.(FP.BP.Comp)-#

###CREATING THE PREDICTION MODELS FOR THE MUTUAL FUNDS FOR ALL RETURN TIME HORIZONS###

##RANDOM FOREST MODELLING##

#TRAINING RANDOM FOREST MODELS WITH 10-FOLD CROSS VALIDATION TO FIND STARTING MTRY #
#               AND NTREE VALUES FOR THE FINAL MODEL                               #

#-MTRY VALUES-#

KFOLDS <- trainControl(method="cv", number=10)
TUNEGRID <- expand.grid(.mtry = c(1:10))
mtry1Wk <- train(X1.Wk.Ret....~., data=Train1Wk, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS)
mtry1Wk #mtry for 1Wk can be betweem 1-4#
plot(mtry1Wk)
mtry1Mth <- train(X1.Mth.Ret....~., data=Train1Mth, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS)
mtry1Mth #mtry for 1Mth came out to be 1-4#
plot(mtry1Mth)
mtry3Mth <- train(X3.Mth.Ret....~., data=Train3Mth, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS)
mtry3Mth #mtry for 3Mth came out to be 8-10#
plot(mtry3Mth)
mtry6Mth <- train(X6.Mth.Ret....~., data=Train6Mth, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS)
mtry6Mth #mtry for 6Mth came out to be 6-8#
plot(mtry6Mth) 
mtry1Yr <- train(X1.Yr.Ret....~., data=Train1Yr, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS)
mtry1Yr #mtry for 1Yr came out to be 1-4#
plot(mtry1Yr)
mtry3Yr <- train(X3.Yr.Ret....~., data=Train3Yr, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS)
mtry3Yr #mtry for 3Yr came out to be 2-6#
plot(mtry3Yr)
mtry5Yr <- train(X5.Yr.Ret....~., data=Train5Yr, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS)
mtry5Yr #mtry for 5Yr came out to be 2-6#
plot(mtry5Yr)

#-NTREE VALUES-#

#-Initializing list variables to store the results-#
List1Wk <- list()
List1Mth <- list()
List3Mth <- list()
List6Mth <- list()
List1Yr <- list()
List3Yr <- list()
List5Yr <- list()
#-Creating train models with different RF values-#
for (ntree in c(100,200,500,1000)){
  NTreeModel <- train(X1.Wk.Ret....~., data=Train1Wk, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS,ntree = ntree)
  key <- toString(ntree)
  List1Wk[[key]] <- NTreeModel
}
for (ntree in c(100,200,500,1000)){
  NTreeModel <- train(X1.Mth.Ret....~., data=Train1Mth, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS,ntree = ntree)
  key <- toString(ntree)
  List1Mth[[key]] <- NTreeModel
}
for (ntree in c(100,200,500,1000)){
  NTreeModel <- train(X3.Mth.Ret....~., data=Train3Mth, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS,ntree = ntree)
  key <- toString(ntree)
  List3Mth[[key]] <- NTreeModel
}
for (ntree in c(100,200,500,1000)){
  NTreeModel <- train(X6.Mth.Ret....~., data=Train6Mth, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS,ntree = ntree)
  key <- toString(ntree)
  List6Mth[[key]] <- NTreeModel
}
for (ntree in c(100,200,500,1000)){
  NTreeModel <- train(X1.Yr.Ret....~., data=Train1Yr, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS,ntree = ntree)
  key <- toString(ntree)
  List1Yr[[key]] <- NTreeModel
}
for (ntree in c(100,200,500,1000)){
  NTreeModel <- train(X3.Yr.Ret....~., data=Train3Yr, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS,ntree = ntree)
  key <- toString(ntree)
  List3Yr[[key]] <- NTreeModel
}
for (ntree in c(100,200,500,1000)){
  NTreeModel <- train(X5.Yr.Ret....~., data=Train5Yr, method="rf", tuneGrid = TUNEGRID, trControl=KFOLDS,ntree = ntree)
  key <- toString(ntree)
  List5Yr[[key]] <- NTreeModel
}

#COMPARING RESULTS FOR EACH NTREE EVALUATION PER TIME HORIZON#

#-Resampling Results for all lists-#
Res1Wk <- resamples(List1Wk)
Res1Mth <- resamples(List1Mth)
Res3Mth <- resamples(List3Mth)
Res6Mth <- resamples(List6Mth)
Res1Yr <- resamples(List1Yr)
Res3Yr <- resamples(List3Yr)
Res5Yr <- resamples(List5Yr)

#-Printing the summarized results for all lists-#
#-We will be checking the RMSE/MAE mean values here for analysis, lower values shows-#
#-better fit.                                                                       -#

summary(Res1Wk) #n tree value b/w 100-200#
summary(Res1Mth)#n tree value b/w 100-200#
summary(Res3Mth)#n tree value b/w 500-1000#
summary(Res6Mth)#n tree value b/w 500-1000#
summary(Res1Yr) #n tree value b/w 200-500#
summary(Res3Yr) #n tree value b/w 500-1000#
summary(Res5Yr) #n tree value b/w 200-500#

#-The values of mtry and ntree can change everytime you run the above code since the-#
#-train and test data are created from data observations picked randomly, analysis  -#
#-done above just gives you an idea as to what values you can use to start          -#
#-optimizing your model with.The values we will use in the model were obtained      -#
#-based on the most ideal value evaluation for which was based on performance (RMSE)-#
#-observed through multiple iterations done for the code.                           -#

#CREATING THE FINALIZED RANDOM FOREST MODELS FOR THE TIME HORIZONS WITH OPTIMIZED NTREE & MTREE VALUES#
RF1Wk <- randomForest((X1.Wk.Ret....)~.,data=Train1Wk, mtry=1, ntree=100)
RF1Mth <- randomForest((X1.Mth.Ret....)~.,data=Train1Mth, mtry=3, ntree=200)
RF3Mth <- randomForest((X3.Mth.Ret....)~.,data=Train3Mth, mtry=9, ntree=1000)
RF6Mth <- randomForest((X6.Mth.Ret....)~.,data=Train6Mth, mtry=6, ntree=1000)
RF1Yr <- randomForest((X1.Yr.Ret....)~.,data=Train1Yr, mtry=4, ntree=500)
RF3Yr <- randomForest((X3.Yr.Ret....)~.,data=Train3Yr, mtry=6, ntree=1000)
RF5Yr <- randomForest((X5.Yr.Ret....)~.,data=Train5Yr, mtry=6, ntree=500)

##SUPPORT VECTOR REGRESSION MODELLING##

#CREATING THE SUPPORT VECTOR MACHINE-REGRESSION MODELS FOR THE TIME HORIZONS#
SVR1Wk <- fit((X1.Wk.Ret....)~.,data=Train1Wk,model = "svm")
SVR1Mth <- fit((X1.Mth.Ret....)~.,data=Train1Mth,model = "svm")
SVR3Mth <- fit((X3.Mth.Ret....)~.,data=Train3Mth,model = "svm")
SVR6Mth <- fit((X6.Mth.Ret....)~.,data=Train6Mth,model = "svm")
SVR1Yr <- fit((X1.Yr.Ret....)~.,data=Train1Yr,model = "svm")
SVR3Yr <- fit((X3.Yr.Ret....)~.,data=Train3Yr,model = "svm")
SVR5Yr <- fit((X5.Yr.Ret....)~.,data=Train5Yr,model = "svm")

##GRADIENT BOOSTING MODELLING##

#CREATING THE GRADIENT BOOSTING MODELS FOR THE TIME HORIZONS#
GB1Wk <- gbm((X1.Wk.Ret....)~.,data=Train1Wk)
GB1Mth <- gbm((X1.Mth.Ret....)~.,data=Train1Mth)
GB3Mth <- gbm((X3.Mth.Ret....)~.,data=Train3Mth)
GB6Mth <- gbm((X6.Mth.Ret....)~.,data=Train6Mth)
GB1Yr <- gbm((X1.Yr.Ret....)~.,data=Train1Yr)
GB3Yr <- gbm((X3.Yr.Ret....)~.,data=Train3Yr)
GB5Yr <- gbm((X5.Yr.Ret....)~.,data=Train5Yr)

##SUMMARIZING THE PREDICTION MODELS FOR ANALYSIS##

RF1Wk
RF1Mth
RF3Mth
RF6Mth
RF1Yr
RF3Yr
RF5Yr
SVR1Wk
SVR1Mth
SVR3Mth
SVR6Mth
SVR1Yr
SVR3Yr
SVR5Yr
GB1Wk
GB1Mth
GB3Mth
GB6Mth
GB1Yr
GB3Yr
GB5Yr

##ASSESSING THE VARIABLE IMPORTANCE PER EACH TIME HORIZON FROM THE MODELS##

#-Random Forest - Variable Importance Analysis with Plots-#
varImp(RF1Wk)
varImpPlot(RF1Wk,n.var=5)
varImp(RF1Mth)
varImpPlot(RF1Mth,n.var=5)
varImp(RF3Mth)
varImpPlot(RF3Mth,n.var=5)
varImp(RF6Mth)
varImpPlot(RF6Mth,n.var=5)
varImp(RF1Yr)
varImpPlot(RF1Yr,n.var=5)
varImp(RF3Yr)
varImpPlot(RF3Yr,n.var=5)
varImp(RF5Yr)
varImpPlot(RF5Yr,n.var=5)

#-Support Vector Regression - Variable Importance Analysis with Plots-#

#Creating the Variable Importance Variables#
SVRvarImps1Wk = as.data.frame(cbind(colnames(Train1Wk)[1:10], (Importance(SVR1Wk, data=Train1Wk))$imp[1:10]))
SVRvarImps1Mth = as.data.frame(cbind(colnames(Train1Mth)[1:10], (Importance(SVR1Mth, data=Train1Mth))$imp[1:10]))
SVRvarImps3Mth = as.data.frame(cbind(colnames(Train3Mth)[1:10], (Importance(SVR3Mth, data=Train3Mth))$imp[1:10]))
SVRvarImps6Mth = as.data.frame(cbind(colnames(Train6Mth)[1:10], (Importance(SVR6Mth, data=Train6Mth))$imp[1:10]))
SVRvarImps1Yr = as.data.frame(cbind(colnames(Train1Yr)[1:10], (Importance(SVR1Yr, data=Train1Yr))$imp[1:10]))
SVRvarImps3Yr = as.data.frame(cbind(colnames(Train3Yr)[1:10], (Importance(SVR3Yr, data=Train3Yr))$imp[1:10]))
SVRvarImps5Yr = as.data.frame(cbind(colnames(Train5Yr)[1:10], (Importance(SVR5Yr, data=Train5Yr))$imp[1:10]))

#Changing the Column Names for the variable importance variables#
colnames(SVRvarImps1Wk) = paste(c("Variable", "Importance"))
colnames(SVRvarImps1Mth) = paste(c("Variable", "Importance"))
colnames(SVRvarImps3Mth) = paste(c("Variable", "Importance"))
colnames(SVRvarImps6Mth) = paste(c("Variable", "Importance"))
colnames(SVRvarImps1Yr) = paste(c("Variable", "Importance"))
colnames(SVRvarImps3Yr) = paste(c("Variable", "Importance"))
colnames(SVRvarImps5Yr) = paste(c("Variable", "Importance"))

#Plotting the variable importance variables for analysis#
SVRvarImps1Wk %>% arrange(Importance) %>% mutate(Variable=factor(Variable, levels=Variable))%>%ggplot( aes(x=Variable, y=as.numeric(as.character((Importance))))) +
                  geom_segment( aes(xend=Variable, yend=0)) + geom_point( size=4, color="orange") + coord_flip()+ ylab("Variable Importance") + xlab("")

SVRvarImps1Mth %>% arrange(Importance) %>% mutate(Variable=factor(Variable, levels=Variable))%>%ggplot( aes(x=Variable, y=as.numeric(as.character((Importance))))) +
  geom_segment( aes(xend=Variable, yend=0)) + geom_point( size=4, color="orange") + coord_flip()+ ylab("Variable Importance") + xlab("")

SVRvarImps3Mth %>% arrange(Importance) %>% mutate(Variable=factor(Variable, levels=Variable))%>%ggplot( aes(x=Variable, y=as.numeric(as.character((Importance))))) +
  geom_segment( aes(xend=Variable, yend=0)) + geom_point( size=4, color="orange") + coord_flip()+ ylab("Variable Importance") + xlab("")

SVRvarImps6Mth %>% arrange(Importance) %>% mutate(Variable=factor(Variable, levels=Variable))%>%ggplot( aes(x=Variable, y=as.numeric(as.character((Importance))))) +
  geom_segment( aes(xend=Variable, yend=0)) + geom_point( size=4, color="orange") + coord_flip()+ ylab("Variable Importance") + xlab("")

SVRvarImps1Yr %>% arrange(Importance) %>% mutate(Variable=factor(Variable, levels=Variable))%>%ggplot( aes(x=Variable, y=as.numeric(as.character((Importance))))) +
  geom_segment( aes(xend=Variable, yend=0)) + geom_point( size=4, color="orange") + coord_flip()+ ylab("Variable Importance") + xlab("")

SVRvarImps3Yr %>% arrange(Importance) %>% mutate(Variable=factor(Variable, levels=Variable))%>%ggplot( aes(x=Variable, y=as.numeric(as.character((Importance))))) +
  geom_segment( aes(xend=Variable, yend=0)) + geom_point( size=4, color="orange") + coord_flip()+ ylab("Variable Importance") + xlab("")

SVRvarImps5Yr %>% arrange(Importance) %>% mutate(Variable=factor(Variable, levels=Variable))%>%ggplot( aes(x=Variable, y=as.numeric(as.character((Importance))))) +
  geom_segment( aes(xend=Variable, yend=0)) + geom_point( size=4, color="orange") + coord_flip()+ ylab("Variable Importance") + xlab("")

#-Gradient Boosting - Variable Importance Analysis with Plots-#
par(mar = c(5, 9, 1, 1))
summary(GB1Wk, cBars = 5,method = relative.influence,las=1)
summary(GB1Mth, cBars = 5,method = relative.influence,las=1)
summary(GB3Mth, cBars = 5,method = relative.influence,las=1)
summary(GB6Mth, cBars = 5,method = relative.influence,las=1)
summary(GB1Yr, cBars = 5,method = relative.influence,las=1)
summary(GB3Yr, cBars = 5,method = relative.influence,las=1)
summary(GB5Yr, cBars = 5,method = relative.influence,las=1)
summary(SVR5Yr, cBars = 5,method = relative.influence,las=1)


#-As seen above we can see that variables FP.BP.Comp, Latest NAV, Market Cap, MF Risk-#
#-Rating & Turnover shows the highest importance amongst all hence we will be using  -#
#-them the same for performance analysis. We have taken the most repeating first five-# 
#-highly significant variables amongst all time horizons.                            -#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# OBJECTIVE 1: DOMINANTS: Identifying the top performing mutual fund schemes in the data   # 
#                         based on their performance data.                                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#-Here we will be analyzing the top 10 mutual fund schemes based on the significant -#
#-variables we interpreted in the previous part of our analysis to identify the top -#
#-performers. Scoring will be based on values between 0 & 1 where 1 will mark the   -#
#-highest score while 0 will be lowest and ideal performance is decided on market   -#
#-research for the variables that are being analyzed.                               -#

##ASSIGNING A DEFAULT VALUE TO THE PERFORMANCE SCORE VARIABLE##

MFPD$FinalScore <- 0

##ASSIGNING SCORES TO THE MUTUAL FUNDS BASED ON SIGNIFICANT VARIABLES IDEAL PERFORMANCE COMPARISON##

#FP.BP.Comp ANALYSIS FOR MUTUAL FUNDS#
MFPD$FinalScore <- ifelse(MFPD$FP.BP.Comp.1Wk==1,1,0)
MFPD$FinalScore <- ifelse(MFPD$FP.BP.Comp.1Mth==1,MFPD$FinalScore+1,MFPD$FinalScore)
MFPD$FinalScore <- ifelse(MFPD$FP.BP.Comp.3Mth==1,MFPD$FinalScore+1,MFPD$FinalScore)
MFPD$FinalScore <- ifelse(MFPD$FP.BP.Comp.6Mth==1,MFPD$FinalScore+1,MFPD$FinalScore)
MFPD$FinalScore <- ifelse(MFPD$FP.BP.Comp.1Yr==1,MFPD$FinalScore+1,MFPD$FinalScore)
MFPD$FinalScore <- ifelse(MFPD$FP.BP.Comp.3Yr==1,MFPD$FinalScore+1,MFPD$FinalScore)
MFPD$FinalScore <- ifelse(MFPD$FP.BP.Comp.5Yr==1,MFPD$FinalScore+1,MFPD$FinalScore)
#LATEST NAV ANALYSIS FOR MUTUAL FUNDS#
MFPD$FinalScore <- ifelse(MFPD$Latest.NAV<50,MFPD$FinalScore+1,MFPD$FinalScore)
#MARKET CAP ANALYSIS FOR MUTUAL FUNDS#
MFPD$FinalScore <- ifelse(MFPD$Market.Cap=="Mega Cap"|MFPD$Market.Cap=="Large Cap",MFPD$FinalScore+1,MFPD$FinalScore)
#RISK RATING ANALYSIS FOR MUTUAL FUNDS#
MFPD$FinalScore <- ifelse(MFPD$MF.Risk.Rating=="Low"|MFPD$MF.Risk.Rating=="Low-Medium",MFPD$FinalScore+1,MFPD$FinalScore)
#TURNOVER ANALYSIS FOR MUTUAL FUNDS#
MFPD$FinalScore <- ifelse(MFPD$Fund.Investment.Style == "Growth" & MFPD$Turnover > 50 & MFPD$FP.BP.Comp.1Yr ==1,MFPD$FinalScore+1,MFPD$FinalScore)
MFPD$FinalScore <- ifelse(MFPD$Fund.Investment.Style == "Blend" & MFPD$Turnover > 50 & MFPD$FP.BP.Comp.1Yr==1,MFPD$FinalScore+1,MFPD$FinalScore)
MFPD$FinalScore <- ifelse(MFPD$Fund.Investment.Style == "Value" & MFPD$Turnover < 20 & MFPD$FP.BP.Comp.1Yr==1,MFPD$FinalScore+1,MFPD$FinalScore)

#-Now that our scoring has been done we will finally be evaluating the funds based on three aspects: final score computed above, time horizon returns and scheme categories-#

##CREATING A PERFORMANCE SHEET FOR FINAL ANALYSIS##

MFPerformanceDataO <- data.frame(MFPerfData$Fund.Name, MFPerfData$Fund.Category,MFPD$MF.Risk.Rating,MFPD$X1.Wk.Ret....,MFPD$X1.Mth.Ret....,MFPD$X3.Mth.Ret....,MFPD$X6.Mth.Ret....,MFPD$X1.Yr.Ret....,MFPD$X3.Yr.Ret....,MFPD$X5.Yr.Ret...., MFPD$FinalScore)
colnames(MFPerformanceDataO) <- c("Scheme Name", "Category","Risk Rating","1Wk % Ret","1Mth % Ret", "3Mth % Ret", "6Mth % Ret", "1Yr % Ret","3Yr % Ret","5Yr % Ret", "OverallPerfScore")
MFPerformanceData <- MFPerformanceDataO

##SORTING THE MUTUAL FUNDS BASED ON PERFORMANCE ANALYSIS DONE ON HISTORICAL DATA##

#SORTING DONE BASED ON FINAL SCORE - TOP 20 MUTUAL FUND SCHEMES#
#-Reordering the mutual funds in descending order of performance score-#
MFPerformanceData <- arrange(MFPerformanceData, desc(MFPerformanceData$OverallPerfScore))
head(MFPerformanceData,20)

#SORTING DONE BASED ON FUND CATEGORIES - TOP 10 MUTUAL FUND SCHEMES#
#FOR EQUITY#
head(filter(MFPerformanceData, Category=="Equity"),10)
#FOR DEBT#
head(filter(MFPerformanceData, Category=="Debt"),10)
#FOR OTHERS#
head(filter(MFPerformanceData, Category=="Others"),10)

#SORTING DONE PER RETURNS FOR EACH TIME HORIZON AND FUND CATEGORY - TOP 20 MUTUAL FUND SCHEMES#
#FOR HIGHEST 1 WEEK RETURNS#
head(arrange(MFPerformanceData, desc(MFPerformanceData$`1Wk % Ret`)),20)
MFPerformanceData %>% filter(Category =="Equity") %>% arrange(desc(`1Wk % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Debt") %>% arrange(desc(`1Wk % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Others") %>% arrange(desc(`1Wk % Ret`)) %>% head(20)
#FOR HIGHEST 1 MONTH RETURNS#
head(arrange(MFPerformanceData, desc(MFPerformanceData$`1Mth % Ret`)),20)
MFPerformanceData %>% filter(Category =="Equity") %>% arrange(desc(`1Mth % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Debt") %>% arrange(desc(`1Mth % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Others") %>% arrange(desc(`1Mth % Ret`)) %>% head(20)
#FOR HIGHEST 3 MONTHS RETURNS#
head(arrange(MFPerformanceData, desc(MFPerformanceData$`3Mth % Ret`)),20)
MFPerformanceData %>% filter(Category =="Equity") %>% arrange(desc(`3Mth % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Debt") %>% arrange(desc(`3Mth % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Others") %>% arrange(desc(`3Mth % Ret`)) %>% head(20)
#FOR HIGHEST 6 MONTHS RETURNS#
head(arrange(MFPerformanceData, desc(MFPerformanceData$`6Mth % Ret`)),20)
MFPerformanceData %>% filter(Category =="Equity") %>% arrange(desc(`6Mth % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Debt") %>% arrange(desc(`6Mth % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Others") %>% arrange(desc(`6Mth % Ret`)) %>% head(20)
#FOR HIGHEST 1 YEAR RETURNS#
head(arrange(MFPerformanceData, desc(MFPerformanceData$`1Yr % Ret`)),20)
MFPerformanceData %>% filter(Category =="Equity") %>% arrange(desc(`1Yr % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Debt") %>% arrange(desc(`1Yr % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Others") %>% arrange(desc(`1Yr % Ret`)) %>% head(20)
#FOR HIGHEST 3 YEARS RETURNS#
head(arrange(MFPerformanceData, desc(MFPerformanceData$`3Yr % Ret`)),20)
MFPerformanceData %>% filter(Category =="Equity") %>% arrange(desc(`3Yr % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Debt") %>% arrange(desc(`3Yr % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Others") %>% arrange(desc(`3Yr % Ret`)) %>% head(20)
#FOR HIGHEST 5 YEARS RETURNS#
head(arrange(MFPerformanceData, desc(MFPerformanceData$`5Yr % Ret`)),20)
MFPerformanceData %>% filter(Category =="Equity") %>% arrange(desc(`5Yr % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Debt") %>% arrange(desc(`5Yr % Ret`)) %>% head(20)
MFPerformanceData %>% filter(Category =="Others") %>% arrange(desc(`5Yr % Ret`)) %>% head(20)

#-NOW YOU CAN ACCORDINGLY COMPARE THE MUTUAL FUND SCHEMES WITH THE HIGHEST RETURNS,          -#
#-CATEGORY AND PERFORMANCE SCORE AND PRESENT IT TO THE INVESTOR FOR ANALYSIS. YOU            -#
#-WILL ALSO BE ABLE TO COMPARE THE PREDICTED RETURNS FOR THE MUTUAL FUND SCHEMES IN          -#
#-THE TABLEAU DASHBOARD FOR FURTHER ANALYSIS, WILL PROVIDE THE SAME IN PREDICTION SECTION TOO-#

#------------------------------------------------------------------------------------------#
####                 Step 7 :- Data Model Performance Evaluation                       #####
#------------------------------------------------------------------------------------------#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# OBJECTIVE 2: PREDICTORS: Predicting the returns for mutual fund schemes for the time     # 
#                          horizons 1W,1M,3M,6M,1Y,3Y,5Y.                                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#-Here we will be predicting the return performance for mutual fund schemes in test -#
#-and train data based on the models created earlier and accordingly compare them to-#
#-understand which amongst them performs the best with respect to predictions.      -#

##CREATING THE TEST DATA FOR PREDICTIONS##

Test1Wk <- Test[,-c(9:21,24:29)]
Test1Mth <- Test[,-c(8,10:21,23,25:29)]
Test3Mth <- Test[,-c(8:9,11:21,23:24,26:29)]
Test6Mth <- Test[,-c(8:10,12:21,23:25,27:29)]
Test1Yr <- Test[,-c(8:11,13:21,23:26,28:29)]
Test3Yr <- Test[,-c(8:12,14:21,23:27,29)]
Test5Yr <- Test[,-c(8:13,15:21,23:28)]

##CREATING A FINAL PERFORMANCE SHEET WITH FORMER RETURN VALUES & PREDICTED VALUES AND OTHER PERFORMANCE VARIABLES##

TrIndex <- as.integer(as.character(unlist(data.frame(rownames(Train)))))
TeIndex <- as.integer(as.character(unlist(data.frame(rownames(Test)))))
FinalIndex<- append(TrIndex, TeIndex)
FinalIndex <- as.data.frame(unlist(FinalIndex))
Index <- as.numeric(unlist(FinalIndex))
PredPerfData <- NULL
for(i in Index){
  
  PredPerfData <- rbind(PredPerfData,(MFPerformanceDataO[i,]))
}

##PREDICTING THE TEST & TRAIN DATA FOR MODEL PERFORMANCE ANALYSIS##

#INITIALIZING THE TRAIN AND TEST PREDICTION DATA FRAMES FOR THE MODELS#

TrPrediction <- as.data.frame(matrix(ncol=21,nrow = 705))
colnames(TrPrediction) <- c("RFPred1Wk","RFPred1Mth","RFPred3Mth", "RFPred6Mth","RFPred1Yr","RFPred3Yr","RFPred5Yr","SVRPred1Wk","SVRPred1Mth","SVRPred3Mth", "SVRPred6Mth","SVRPred1Yr","SVRPred3Yr","SVRPred5Yr","GBPred1Wk","GBPred1Mth","GBPred3Mth", "GBPred6Mth","GBPred1Yr","GBPred3Yr","GBPred5Yr")
TePrediction <- as.data.frame(matrix(ncol=21,nrow = 330))
colnames(TePrediction) <- c("RFPred1Wk","RFPred1Mth","RFPred3Mth", "RFPred6Mth","RFPred1Yr","RFPred3Yr","RFPred5Yr","SVRPred1Wk","SVRPred1Mth","SVRPred3Mth", "SVRPred6Mth","SVRPred1Yr","SVRPred3Yr","SVRPred5Yr","GBPred1Wk","GBPred1Mth","GBPred3Mth", "GBPred6Mth","GBPred1Yr","GBPred3Yr","GBPred5Yr")

#PREDICTING RETURNS FOR RANDOM FOREST MODEL#

#FOR TRAIN DATA#
TrPrediction$RFPred1Wk <- predict(RF1Wk,newdata = Train1Wk[,-8])
TrPrediction$RFPred1Mth <- predict(RF1Mth,newdata = Train1Mth[,-8])
TrPrediction$RFPred3Mth <- predict(RF3Mth,newdata = Train3Mth[,-8])
TrPrediction$RFPred6Mth <- predict(RF6Mth,newdata = Train6Mth[,-8])
TrPrediction$RFPred1Yr <- predict(RF1Yr,newdata = Train1Yr[,-8])
TrPrediction$RFPred3Yr <- predict(RF3Yr,newdata = Train3Yr[,-8])
TrPrediction$RFPred5Yr <- predict(RF5Yr,newdata = Train5Yr[,-8])
#FOR TEST DATA#
TePrediction$RFPred1Wk <- predict(RF1Wk,newdata = Test1Wk[,-8])
TePrediction$RFPred1Mth <- predict(RF1Mth,newdata = Test1Mth[,-8])
TePrediction$RFPred3Mth <- predict(RF3Mth,newdata = Test3Mth[,-8])
TePrediction$RFPred6Mth <- predict(RF6Mth,newdata = Test6Mth[,-8])
TePrediction$RFPred1Yr <- predict(RF1Yr,newdata = Test1Yr[,-8])
TePrediction$RFPred3Yr <- predict(RF3Yr,newdata = Test3Yr[,-8])
TePrediction$RFPred5Yr <- predict(RF5Yr,newdata = Test5Yr[,-8])

#PREDICTING RETURNS FOR SUPPORT VECTOR MACHINE - REGRESSION MODEL#

#FOR TRAIN DATA#
TrPrediction$SVRPred1Wk <- predict(SVR1Wk,newdata = Train1Wk[,-8])
TrPrediction$SVRPred1Mth <- predict(SVR1Mth,newdata = Train1Mth[,-8])
TrPrediction$SVRPred3Mth <- predict(SVR3Mth,newdata = Train3Mth[,-8])
TrPrediction$SVRPred6Mth <- predict(SVR6Mth,newdata = Train6Mth[,-8])
TrPrediction$SVRPred1Yr <- predict(SVR1Yr,newdata = Train1Yr[,-8])
TrPrediction$SVRPred3Yr <- predict(SVR3Yr,newdata = Train3Yr[,-8])
TrPrediction$SVRPred5Yr <- predict(SVR5Yr,newdata = Train5Yr[,-8])
#FOR TEST DATA#
TePrediction$SVRPred1Wk <- predict(SVR1Wk,newdata = Test1Wk[,-8])
TePrediction$SVRPred1Mth <- predict(SVR1Mth,newdata = Test1Mth[,-8])
TePrediction$SVRPred3Mth <- predict(SVR3Mth,newdata = Test3Mth[,-8])
TePrediction$SVRPred6Mth <- predict(SVR6Mth,newdata = Test6Mth[,-8])
TePrediction$SVRPred1Yr <- predict(SVR1Yr,newdata = Test1Yr[,-8])
TePrediction$SVRPred3Yr <- predict(SVR3Yr,newdata = Test3Yr[,-8])
TePrediction$SVRPred5Yr <- predict(SVR5Yr,newdata = Test5Yr[,-8])

#PREDICTING RETURNS FOR GRADIENT BOOSTING MODEL#

#FOR TRAIN DATA#
TrPrediction$GBPred1Wk <- predict(GB1Wk,newdata = Train1Wk[,-8])
TrPrediction$GBPred1Mth <- predict(GB1Mth,newdata = Train1Mth[,-8])
TrPrediction$GBPred3Mth <- predict(GB3Mth,newdata = Train3Mth[,-8])
TrPrediction$GBPred6Mth <- predict(GB6Mth,newdata = Train6Mth[,-8])
TrPrediction$GBPred1Yr <- predict(GB1Yr,newdata = Train1Yr[,-8])
TrPrediction$GBPred3Yr <- predict(GB3Yr,newdata = Train3Yr[,-8])
TrPrediction$GBPred5Yr <- predict(GB5Yr,newdata = Train5Yr[,-8])
#FOR TEST DATA#
TePrediction$GBPred1Wk <- predict(GB1Wk,newdata = Test1Wk[,-8])
TePrediction$GBPred1Mth <- predict(GB1Mth,newdata = Test1Mth[,-8])
TePrediction$GBPred3Mth <- predict(GB3Mth,newdata = Test3Mth[,-8])
TePrediction$GBPred6Mth <- predict(GB6Mth,newdata = Test6Mth[,-8])
TePrediction$GBPred1Yr <- predict(GB1Yr,newdata = Test1Yr[,-8])
TePrediction$GBPred3Yr <- predict(GB3Yr,newdata = Test3Yr[,-8])
TePrediction$GBPred5Yr <- predict(GB5Yr,newdata = Test5Yr[,-8])

#ROUNDING THE RETURNS PREDICTED TO TWO DECIMAL POINTS#

TrPrediction <- round(TrPrediction,2)
TePrediction <- round(TePrediction,2)

##MODEL PERFORMANCE EVALUATION FROM THE PREDICTIONS MADE BY THE MODELS CREATED##

#SCATTER PLOT TO UNDERSTAND ACTUAL VERSUS PREDICTED RETURNS VARIATION#

#-Red is the predicted value while black is the original return value-#

#TRAIN DATA#

#RANDOM FOREST MODEL#
plot(Train1Wk$X1.Wk.Ret...., TrPrediction$RFPred1Wk , col = c("black","red"), pch=16)
plot(Train1Mth$X1.Mth.Ret...., TrPrediction$RFPred1Mth , col = c("black","red"), pch=16)
plot(Train3Mth$X3.Mth.Ret...., TrPrediction$RFPred3Mth , col = c("black","red"), pch=16)
plot(Train6Mth$X6.Mth.Ret...., TrPrediction$RFPred6Mth , col = c("black","red"), pch=16)
plot(Train1Yr$X1.Yr.Ret...., TrPrediction$RFPred1Yr , col = c("black","red"), pch=16)
plot(Train3Yr$X3.Yr.Ret...., TrPrediction$RFPred3Yr , col = c("black","red"), pch=16)
plot(Train5Yr$X5.Yr.Ret...., TrPrediction$RFPred5Yr , col = c("black","red"), pch=16)
#SUPPORT VECTOR MACHINE - REGRESSION MODEL#
plot(Train1Wk$X1.Wk.Ret...., TrPrediction$SVRPred1Wk , col = c("black","red"), pch=16)
plot(Train1Mth$X1.Mth.Ret...., TrPrediction$SVRPred1Mth , col = c("black","red"), pch=16)
plot(Train3Mth$X3.Mth.Ret...., TrPrediction$SVRPred3Mth , col = c("black","red"), pch=16)
plot(Train6Mth$X6.Mth.Ret...., TrPrediction$SVRPred6Mth , col = c("black","red"), pch=16)
plot(Train1Yr$X1.Yr.Ret...., TrPrediction$SVRPred1Yr , col = c("black","red"), pch=16)
plot(Train3Yr$X3.Yr.Ret...., TrPrediction$SVRPred3Yr , col = c("black","red"), pch=16)
plot(Train5Yr$X5.Yr.Ret...., TrPrediction$SVRPred5Yr , col = c("black","red"), pch=16)
#GRADIENT BOOSTING MODEL#
plot(Train1Wk$X1.Wk.Ret...., TrPrediction$GBPred1Wk , col = c("black","red"), pch=16)
plot(Train1Mth$X1.Mth.Ret...., TrPrediction$GBPred1Mth , col = c("black","red"), pch=16)
plot(Train3Mth$X3.Mth.Ret...., TrPrediction$GBPred3Mth , col = c("black","red"), pch=16)
plot(Train6Mth$X6.Mth.Ret...., TrPrediction$GBPred6Mth , col = c("black","red"), pch=16)
plot(Train1Yr$X1.Yr.Ret...., TrPrediction$GBPred1Yr , col = c("black","red"), pch=16)
plot(Train3Yr$X3.Yr.Ret...., TrPrediction$GBPred3Yr , col = c("black","red"), pch=16)
plot(Train5Yr$X5.Yr.Ret...., TrPrediction$GBPred5Yr , col = c("black","red"), pch=16)

#TEST DATA#

#RANDOM FOREST MODEL#
plot(Test1Wk$X1.Wk.Ret...., TePrediction$RFPred1Wk , col = c("black","red"), pch=16)
plot(Test1Mth$X1.Mth.Ret...., TePrediction$RFPred1Mth , col = c("black","red"), pch=16)
plot(Test3Mth$X3.Mth.Ret...., TePrediction$RFPred3Mth , col = c("black","red"), pch=16)
plot(Test6Mth$X6.Mth.Ret...., TePrediction$RFPred6Mth , col = c("black","red"), pch=16)
plot(Test1Yr$X1.Yr.Ret...., TePrediction$RFPred1Yr , col = c("black","red"), pch=16)
plot(Test3Yr$X3.Yr.Ret...., TePrediction$RFPred3Yr , col = c("black","red"), pch=16)
plot(Test5Yr$X5.Yr.Ret...., TePrediction$RFPred5Yr , col = c("black","red"), pch=16)
#SUPPORT VECTOR MACHINE - REGRESSION MODEL#
plot(Test1Wk$X1.Wk.Ret...., TePrediction$SVRPred1Wk , col = c("black","red"), pch=16)
plot(Test1Mth$X1.Mth.Ret...., TePrediction$SVRPred1Mth , col = c("black","red"), pch=16)
plot(Test3Mth$X3.Mth.Ret...., TePrediction$SVRPred3Mth , col = c("black","red"), pch=16)
plot(Test6Mth$X6.Mth.Ret...., TePrediction$SVRPred6Mth , col = c("black","red"), pch=16)
plot(Test1Yr$X1.Yr.Ret...., TePrediction$SVRPred1Yr , col = c("black","red"), pch=16)
plot(Test3Yr$X3.Yr.Ret...., TePrediction$SVRPred3Yr , col = c("black","red"), pch=16)
plot(Test5Yr$X5.Yr.Ret...., TePrediction$SVRPred5Yr , col = c("black","red"), pch=16)
#GRADIENT BOOSTING MODEL#
plot(Test1Wk$X1.Wk.Ret...., TePrediction$GBPred1Wk , col = c("black","red"), pch=16)
plot(Test1Mth$X1.Mth.Ret...., TePrediction$GBPred1Mth , col = c("black","red"), pch=16)
plot(Test3Mth$X3.Mth.Ret...., TePrediction$GBPred3Mth , col = c("black","red"), pch=16)
plot(Test6Mth$X6.Mth.Ret...., TePrediction$GBPred6Mth , col = c("black","red"), pch=16)
plot(Test1Yr$X1.Yr.Ret...., TePrediction$GBPred1Yr , col = c("black","red"), pch=16)
plot(Test3Yr$X3.Yr.Ret...., TePrediction$GBPred3Yr , col = c("black","red"), pch=16)
plot(Test5Yr$X5.Yr.Ret...., TePrediction$GBPred5Yr , col = c("black","red"), pch=16)

#-As observed above the Random Forest model predictions shows a more linear relationship-#
#-and accuracy to the original data as compared to others.                              -#

#RMSE/MAE/R-SQUARED/MSE VALUE ANALYSIS FOR PREDICTION EVALUATION#

#-Lower values indicate better results-#

#For 1 Week RF Model#

#TRAIN DATA#
#RF#
MLmetrics::RMSE(TrPrediction$RFPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::MAE(TrPrediction$RFPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::R2_Score(TrPrediction$RFPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::MSE(TrPrediction$RFPred1Wk,Train1Wk$X1.Wk.Ret....)
#SVR#
MLmetrics::RMSE(TrPrediction$SVRPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::MAE(TrPrediction$SVRPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::R2_Score(TrPrediction$SVRPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::MSE(TrPrediction$SVRPred1Wk,Train1Wk$X1.Wk.Ret....)
#GB#
MLmetrics::RMSE(TrPrediction$GBPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::MAE(TrPrediction$GBPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::R2_Score(TrPrediction$GBPred1Wk,Train1Wk$X1.Wk.Ret....)
MLmetrics::MSE(TrPrediction$GBPred1Wk,Train1Wk$X1.Wk.Ret....)

#TEST DATA#
#RF#
MLmetrics::RMSE(TePrediction$RFPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::MAE(TePrediction$RFPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::R2_Score(TePrediction$RFPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::MSE(TePrediction$RFPred1Wk,Test1Wk$X1.Wk.Ret....)
#SVR#
MLmetrics::RMSE(TePrediction$SVRPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::MAE(TePrediction$SVRPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::R2_Score(TePrediction$SVRPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::MSE(TePrediction$SVRPred1Wk,Test1Wk$X1.Wk.Ret....)
#GB#
MLmetrics::RMSE(TePrediction$GBPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::MAE(TePrediction$GBPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::R2_Score(TePrediction$GBPred1Wk,Test1Wk$X1.Wk.Ret....)
MLmetrics::MSE(TePrediction$GBPred1Wk,Test1Wk$X1.Wk.Ret....)

#For 1 Month RF Model#

#TRAIN DATA#
#RF#
MLmetrics::RMSE(TrPrediction$RFPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::MAE(TrPrediction$RFPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$RFPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::MSE(TrPrediction$RFPred1Mth,Train1Mth$X1.Mth.Ret....)
#SVR#
MLmetrics::RMSE(TrPrediction$SVRPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::MAE(TrPrediction$SVRPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$SVRPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::MSE(TrPrediction$SVRPred1Mth,Train1Mth$X1.Mth.Ret....)
#GB#
MLmetrics::RMSE(TrPrediction$GBPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::MAE(TrPrediction$GBPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$GBPred1Mth,Train1Mth$X1.Mth.Ret....)
MLmetrics::MSE(TrPrediction$GBPred1Mth,Train1Mth$X1.Mth.Ret....)

#TEST DATA#
#RF#
MLmetrics::RMSE(TePrediction$RFPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::MAE(TePrediction$RFPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$RFPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::MSE(TePrediction$RFPred1Mth,Test1Mth$X1.Mth.Ret....)
#SVR#
MLmetrics::RMSE(TePrediction$SVRPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::MAE(TePrediction$SVRPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$SVRPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::MSE(TePrediction$SVRPred1Mth,Test1Mth$X1.Mth.Ret....)
#GB#
MLmetrics::RMSE(TePrediction$GBPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::MAE(TePrediction$GBPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$GBPred1Mth,Test1Mth$X1.Mth.Ret....)
MLmetrics::MSE(TePrediction$GBPred1Mth,Test1Mth$X1.Mth.Ret....)

#For 3 Month RF Model#

#TRAIN DATA#
#RF#
MLmetrics::RMSE(TrPrediction$RFPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::MAE(TrPrediction$RFPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$RFPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::MSE(TrPrediction$RFPred3Mth,Train3Mth$X3.Mth.Ret....)
#SVR#
MLmetrics::RMSE(TrPrediction$SVRPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::MAE(TrPrediction$SVRPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$SVRPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::MSE(TrPrediction$SVRPred3Mth,Train3Mth$X3.Mth.Ret....)
#GB#
MLmetrics::RMSE(TrPrediction$GBPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::MAE(TrPrediction$GBPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$GBPred3Mth,Train3Mth$X3.Mth.Ret....)
MLmetrics::MSE(TrPrediction$GBPred3Mth,Train3Mth$X3.Mth.Ret....)

#TEST DATA#
#RF#
MLmetrics::RMSE(TePrediction$RFPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::MAE(TePrediction$RFPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$RFPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::MSE(TePrediction$RFPred3Mth,Test3Mth$X3.Mth.Ret....)
#SVR#
MLmetrics::RMSE(TePrediction$SVRPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::MAE(TePrediction$SVRPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$SVRPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::MSE(TePrediction$SVRPred3Mth,Test3Mth$X3.Mth.Ret....)
#GB#
MLmetrics::RMSE(TePrediction$GBPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::MAE(TePrediction$GBPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$GBPred3Mth,Test3Mth$X3.Mth.Ret....)
MLmetrics::MSE(TePrediction$GBPred3Mth,Test3Mth$X3.Mth.Ret....)

#For 6 Month RF Model#

#TRAIN DATA#
#RF#
MLmetrics::RMSE(TrPrediction$RFPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::MAE(TrPrediction$RFPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$RFPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::MSE(TrPrediction$RFPred6Mth,Train6Mth$X6.Mth.Ret....)
#SVR#
MLmetrics::RMSE(TrPrediction$SVRPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::MAE(TrPrediction$SVRPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$SVRPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::MSE(TrPrediction$SVRPred6Mth,Train6Mth$X6.Mth.Ret....)
#GB#
MLmetrics::RMSE(TrPrediction$GBPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::MAE(TrPrediction$GBPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::R2_Score(TrPrediction$GBPred6Mth,Train6Mth$X6.Mth.Ret....)
MLmetrics::MSE(TrPrediction$GBPred6Mth,Train6Mth$X6.Mth.Ret....)

#TEST DATA#
#RF#
MLmetrics::RMSE(TePrediction$RFPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::MAE(TePrediction$RFPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$RFPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::MSE(TePrediction$RFPred6Mth,Test6Mth$X6.Mth.Ret....)
#SVR#
MLmetrics::RMSE(TePrediction$SVRPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::MAE(TePrediction$SVRPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$SVRPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::MSE(TePrediction$SVRPred6Mth,Test6Mth$X6.Mth.Ret....)
#GB#
MLmetrics::RMSE(TePrediction$GBPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::MAE(TePrediction$GBPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::R2_Score(TePrediction$GBPred6Mth,Test6Mth$X6.Mth.Ret....)
MLmetrics::MSE(TePrediction$GBPred6Mth,Test6Mth$X6.Mth.Ret....)

#For 1 Year RF Model#

#TRAIN DATA#
#RF#
MLmetrics::RMSE(TrPrediction$RFPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::MAE(TrPrediction$RFPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$RFPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::MSE(TrPrediction$RFPred1Yr,Train1Yr$X1.Yr.Ret....)
#SVR#
MLmetrics::RMSE(TrPrediction$SVRPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::MAE(TrPrediction$SVRPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$SVRPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::MSE(TrPrediction$SVRPred1Yr,Train1Yr$X1.Yr.Ret....)
#GB#
MLmetrics::RMSE(TrPrediction$GBPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::MAE(TrPrediction$GBPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$GBPred1Yr,Train1Yr$X1.Yr.Ret....)
MLmetrics::MSE(TrPrediction$GBPred1Yr,Train1Yr$X1.Yr.Ret....)

#TEST DATA#
#RF#
MLmetrics::RMSE(TePrediction$RFPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::MAE(TePrediction$RFPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$RFPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::MSE(TePrediction$RFPred1Yr,Test1Yr$X1.Yr.Ret....)
#SVR#
MLmetrics::RMSE(TePrediction$SVRPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::MAE(TePrediction$SVRPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$SVRPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::MSE(TePrediction$SVRPred1Yr,Test1Yr$X1.Yr.Ret....)
#GB#
MLmetrics::RMSE(TePrediction$GBPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::MAE(TePrediction$GBPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$GBPred1Yr,Test1Yr$X1.Yr.Ret....)
MLmetrics::MSE(TePrediction$GBPred1Yr,Test1Yr$X1.Yr.Ret....)

#For 3 Year RF Model#

#TRAIN DATA#
#RF#
MLmetrics::RMSE(TrPrediction$RFPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::MAE(TrPrediction$RFPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$RFPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::MSE(TrPrediction$RFPred3Yr,Train3Yr$X3.Yr.Ret....)
#SVR#
MLmetrics::RMSE(TrPrediction$SVRPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::MAE(TrPrediction$SVRPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$SVRPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::MSE(TrPrediction$SVRPred3Yr,Train3Yr$X3.Yr.Ret....)
#GB#
MLmetrics::RMSE(TrPrediction$GBPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::MAE(TrPrediction$GBPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$GBPred3Yr,Train3Yr$X3.Yr.Ret....)
MLmetrics::MSE(TrPrediction$GBPred3Yr,Train3Yr$X3.Yr.Ret....)

#TEST DATA#
#RF#
MLmetrics::RMSE(TePrediction$RFPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::MAE(TePrediction$RFPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$RFPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::MSE(TePrediction$RFPred3Yr,Test3Yr$X3.Yr.Ret....)
#SVR#
MLmetrics::RMSE(TePrediction$SVRPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::MAE(TePrediction$SVRPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$SVRPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::MSE(TePrediction$SVRPred3Yr,Test3Yr$X3.Yr.Ret....)
#GB#
MLmetrics::RMSE(TePrediction$GBPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::MAE(TePrediction$GBPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$GBPred3Yr,Test3Yr$X3.Yr.Ret....)
MLmetrics::MSE(TePrediction$GBPred3Yr,Test3Yr$X3.Yr.Ret....)

#For 5 Year RF Model#

#TRAIN DATA#
#RF#
MLmetrics::RMSE(TrPrediction$RFPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::MAE(TrPrediction$RFPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$RFPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::MSE(TrPrediction$RFPred5Yr,Train5Yr$X5.Yr.Ret....)
#SVR#
MLmetrics::RMSE(TrPrediction$SVRPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::MAE(TrPrediction$SVRPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$SVRPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::MSE(TrPrediction$SVRPred5Yr,Train5Yr$X5.Yr.Ret....)
#GB#
MLmetrics::RMSE(TrPrediction$GBPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::MAE(TrPrediction$GBPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::R2_Score(TrPrediction$GBPred5Yr,Train5Yr$X5.Yr.Ret....)
MLmetrics::MSE(TrPrediction$GBPred5Yr,Train5Yr$X5.Yr.Ret....)

#TEST DATA#
#RF#
MLmetrics::RMSE(TePrediction$RFPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::MAE(TePrediction$RFPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$RFPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::MSE(TePrediction$RFPred5Yr,Test5Yr$X5.Yr.Ret....)
#SVR#
MLmetrics::RMSE(TePrediction$SVRPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::MAE(TePrediction$SVRPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$SVRPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::MSE(TePrediction$SVRPred5Yr,Test5Yr$X5.Yr.Ret....)
#GB#
MLmetrics::RMSE(TePrediction$GBPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::MAE(TePrediction$GBPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::R2_Score(TePrediction$GBPred5Yr,Test5Yr$X5.Yr.Ret....)
MLmetrics::MSE(TePrediction$GBPred5Yr,Test5Yr$X5.Yr.Ret....)

#-As seen above the RMSE and MAE values are best for Random Forest Model   -#
#-hence we will be using the same for final performance prediction analysis-#

##MERGING THE PREDICTIONS FOR TRAIN AND TEST DATASET TOGETHER FOR SELECTED MODEL##

PredMerge <- as.data.frame(matrix(ncol=7,nrow = 1035))
colnames(PredMerge) <- c("Pred1Wk","Pred1Mth","Pred3Mth", "Pred6Mth","Pred1Yr","Pred3Yr","Pred5Yr")
for(i in 1:705)
{
  PredMerge$Pred1Wk[i] <- TrPrediction$RFPred1Wk[i]
  PredMerge$Pred1Mth[i] <- TrPrediction$RFPred1Mth[i]
  PredMerge$Pred3Mth[i] <- TrPrediction$RFPred3Mth[i]
  PredMerge$Pred6Mth[i] <- TrPrediction$RFPred6Mth[i]
  PredMerge$Pred1Yr[i] <- TrPrediction$RFPred1Yr[i]
  PredMerge$Pred3Yr[i] <- TrPrediction$RFPred3Yr[i]
  PredMerge$Pred5Yr[i] <- TrPrediction$RFPred5Yr[i]
}

for(i in 1:330)
{
  PredMerge$Pred1Wk[705+i] <- TePrediction$RFPred1Wk[i]
  PredMerge$Pred1Mth[705+i] <- TePrediction$RFPred1Mth[i]
  PredMerge$Pred3Mth[705+i] <- TePrediction$RFPred3Mth[i]
  PredMerge$Pred6Mth[705+i] <- TePrediction$RFPred6Mth[i]
  PredMerge$Pred1Yr[705+i] <- TePrediction$RFPred1Yr[i]
  PredMerge$Pred3Yr[705+i] <- TePrediction$RFPred3Yr[i]
  PredMerge$Pred5Yr[705+i] <- TePrediction$RFPred5Yr[i]
}

##ADDING THE MERGED DATA TO OUR FINAL PERFORMANCE SHEET AND EXPORTING IT TO CSV FILE##

PredPerfData <- cbind(PredPerfData,PredMerge$Pred1Wk)
PredPerfData <- cbind(PredPerfData,PredMerge$Pred1Mth)
PredPerfData <- cbind(PredPerfData,PredMerge$Pred3Mth)
PredPerfData <- cbind(PredPerfData,PredMerge$Pred6Mth)
PredPerfData <- cbind(PredPerfData,PredMerge$Pred1Yr)
PredPerfData <- cbind(PredPerfData,PredMerge$Pred3Yr)
PredPerfData <- cbind(PredPerfData,PredMerge$Pred5Yr)
colnames(PredPerfData) <- c("Scheme Name","Category","Risk Rating","Orig1Wk","Orig1Mth","Orig3Mth","Orig6Mth","Orig1Yr","Orig3Yr","Orig5Yr","OverallPerfScore","Pred1Wk","Pred1Mth", "Pred3Mth","Pred6Mth","Pred1Yr","Pred3Yr","Pred5Yr")

##PERFORMANCE ANALYSIS WITH RESPECT TO PREDICTED RETURNS FOR THE MUTUAL FUND SCHEMES##

#-As we have already shown the sorting by overall performance score hence we will not be -#
#-showing that here.                                                                     -#

#SORTING DONE PER PREDICTED RETURNS FOR EACH TIME HORIZON AND FUND CATEGORY - TOP 20 MUTUAL FUND SCHEMES#
#FOR HIGHEST 1 WEEK RETURNS#
head(arrange(PredPerfData[,-c(4:10,13:18)], desc(PredPerfData$Pred1Wk)),20)
PredPerfData[,-c(4:10,13:18)] %>% filter(Category =="Equity") %>% arrange(desc(Pred1Wk)) %>% head(20)
PredPerfData[,-c(4:10,13:18)] %>% filter(Category =="Debt") %>% arrange(desc(Pred1Wk)) %>% head(20)
PredPerfData[,-c(4:10,13:18)] %>% filter(Category =="Others") %>% arrange(desc(Pred1Wk)) %>% head(20)
#FOR HIGHEST 1 MONTH RETURNS#
head(arrange(PredPerfData[,-c(4:10,12,14:18)], desc(PredPerfData$Pred1Mth)),20)
PredPerfData[,-c(4:10,12,14:18)] %>% filter(Category =="Equity") %>% arrange(desc(Pred1Mth)) %>% head(20)
PredPerfData[,-c(4:10,12,14:18)] %>% filter(Category =="Debt") %>% arrange(desc(Pred1Mth)) %>% head(20)
PredPerfData[,-c(4:10,12,14:18)] %>% filter(Category =="Others") %>% arrange(desc(Pred1Mth)) %>% head(20)
#FOR HIGHEST 3 MONTHS RETURNS#
head(arrange(PredPerfData[,-c(4:10,12:13,15:18)], desc(PredPerfData$Pred3Mth)),20)
PredPerfData[,-c(4:10,12:13,15:18)] %>% filter(Category =="Equity") %>% arrange(desc(Pred3Mth)) %>% head(20)
PredPerfData[,-c(4:10,12:13,15:18)] %>% filter(Category =="Debt") %>% arrange(desc(Pred3Mth)) %>% head(20)
PredPerfData[,-c(4:10,12:13,15:18)] %>% filter(Category =="Others") %>% arrange(desc(Pred3Mth)) %>% head(20)
#FOR HIGHEST 6 MONTHS RETURNS#
head(arrange(PredPerfData[,-c(4:10,12:14,16:18)], desc(PredPerfData$Pred6Mth)),20)
PredPerfData[,-c(4:10,12:14,16:18)] %>% filter(Category =="Equity") %>% arrange(desc(Pred6Mth)) %>% head(20)
PredPerfData[,-c(4:10,12:14,16:18)] %>% filter(Category =="Debt") %>% arrange(desc(Pred6Mth)) %>% head(20)
PredPerfData[,-c(4:10,12:14,16:18)] %>% filter(Category =="Others") %>% arrange(desc(Pred6Mth)) %>% head(20)
#FOR HIGHEST 1 YEAR RETURNS#
head(arrange(PredPerfData[,-c(4:10,12:15,17:18)], desc(PredPerfData$Pred1Yr)),20)
PredPerfData[,-c(4:10,12:15,17:18)] %>% filter(Category =="Equity") %>% arrange(desc(Pred1Yr)) %>% head(20)
PredPerfData[,-c(4:10,12:15,17:18)] %>% filter(Category =="Debt") %>% arrange(desc(Pred1Yr)) %>% head(20)
PredPerfData[,-c(4:10,12:15,17:18)] %>% filter(Category =="Others") %>% arrange(desc(Pred1Yr)) %>% head(20)
#FOR HIGHEST 3 YEARS RETURNS#
head(arrange(PredPerfData[,-c(4:10,12:16,18)], desc(PredPerfData$Pred3Yr)),20)
PredPerfData[,-c(4:10,12:16,18)] %>% filter(Category =="Equity") %>% arrange(desc(Pred3Yr)) %>% head(20)
PredPerfData[,-c(4:10,12:16,18)] %>% filter(Category =="Debt") %>% arrange(desc(Pred3Yr)) %>% head(20)
PredPerfData[,-c(4:10,12:16,18)] %>% filter(Category =="Others") %>% arrange(desc(Pred3Yr)) %>% head(20)
#FOR HIGHEST 5 YEARS RETURNS#
head(arrange(PredPerfData[,-c(4:10,12:17)], desc(PredPerfData$Pred5Yr)),20)
PredPerfData[,-c(4:10,12:17)] %>% filter(Category =="Equity") %>% arrange(desc(Pred5Yr)) %>% head(20)
PredPerfData[,-c(4:10,12:17)] %>% filter(Category =="Debt") %>% arrange(desc(Pred5Yr)) %>% head(20)
PredPerfData[,-c(4:10,12:17)] %>% filter(Category =="Others") %>% arrange(desc(Pred5Yr)) %>% head(20)
writexl::write_xlsx(PredPerfData,"./Final_Prediction_MFPerformanceData.xlsx")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# OBJECTIVE 3: ALLOCATOR: Create a mutual fund portfolio for the investor based on the     # 
#                          time horizon and top performers.                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                          #
# THIS PART OF THE ANALYSIS WILL BE DONE MOSTLY IN THE TABLEAU DASHBOARD.HERE WE WILL BE   #
# ANALYZING THE TOP PERFORMERS BASED ON THE PERFORMANCE EVALUATION SHEET CREATED ABOVE FOR #
# THE MUTUAL FUND SCHEMES AND FROM THIS THE INVESTOR PORTFOLIO WILL BE CREATED.            #
#                                                                                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

RiskProfile <- c("Aggressive","Growth","Balanced","Conservative","Risk-Averse")
EquitySchemes <- c(4,3,2,2,1)
DebtSchemes <- c(1,2,2,3,4)
OtherSchemes <- c(1,1,2,1,1)
InvEqAlloc <- c(80,60,40,15,5)
InvDebtAlloc <- c(10,30,50,80,90)
InvOthersAlloc <- c(10,10,10,5,5)

#-The portfolio will be based on investor preferences be it time horizon, investment % -#
#-allocation or other preferences, the visual dashboard can help the investors compare -# 
#-between mutual funds for a DIY functionality and accordingly select the relevant     -#
#-funds.We will be suggesting the portfolio here for the investors based on the overall-#
#-performance of the mutual funds only but this can change provided the investors have -#
#-other expectations in mind.The ideal investment % allocation will be shown below and -#
#-the ideal number of schemes will be picked based on our bucket allocations decided   -#
#-and hard coded for the project through market research.                              -# 

###CREATING THE PORTFOLIOS FOR DIFFERENT RISK PROFILES###

for(i in 1:5)
{
print(paste("The ideal portfolio for investment profile -",RiskProfile[i],"is given below:"))
print(paste("Investment % Allocation: Equity-",InvEqAlloc[i]," Debt-",InvDebtAlloc[i], " Others-",InvOthersAlloc[i]))
cat("\n")
print(paste("Total Number of Equity Schemes:",EquitySchemes[i]))
print(PredPerfData[,-c(4:10)] %>% filter(Category =="Equity") %>% arrange(desc(OverallPerfScore)) %>% head(EquitySchemes[i]))
cat("\n")
print(paste("Total Number of Debt Schemes:",DebtSchemes[i]))
print(PredPerfData[,-c(4:10)] %>% filter(Category =="Debt") %>% arrange(desc(OverallPerfScore)) %>% head(DebtSchemes[i]))
cat("\n")
print(paste("Total Number of Other Schemes:",OtherSchemes[i]))
print(PredPerfData[,-c(4:10)] %>% filter(Category =="Others") %>% arrange(desc(OverallPerfScore)) %>% head(OtherSchemes[i]))
cat("\n\n\n\n")
}
#------------------------------------------------------------------------------------------#
####                           Step 8 :- End of Study                                  #####
#------------------------------------------------------------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# CONCLUSION: FROM OUR STUDY HERE, WE CAN NOW SUCCESSFULLY IDENTIFY THE TOP PERFORMING     #
#             FUNDS IN OUR DATA BASED ON FUND CATEGORY,OVERALL PERFORMANCE SCORE,          # 
#             HISTORICAL AND PREDICTED RETURNS. % RETURNS FOR EACH MUTUAL FUND SCHEME      #
#             FOR EACH TIME HORIZON WERE PREDICTED & ASSESSED AND BASED ON THE TOP         #
#             PERFORMERS THE FINAL PORTFOLIO WAS CREATED WITH IDEAL INVESTMENT AMOUNT %    #
#             ALLOCATION & TOTAL SCHEMES PER FUND CATEGORY BEING DISPLAYED ACCORDING TO    #
#             THE DIFFERENT RISK PROFILES CREATED  FOR THE INVESTORS.                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
########################################### End Of The Code ###############################################################
#   Note:                                                                                                                 #
#   #### starts a new section, ### a new sub-section, ## a new sub-section topic, #- a new comment and # Random Headings  #
#      The starting symbol will match with a respective ending symbol to mark the ending of the topic.                    #
#      Each section will mention the packages to be used in the start for quick reference.                                #
#                                                                                                                         #
######################################## End of the Analysis ##############################################################