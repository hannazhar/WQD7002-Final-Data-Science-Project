#--------------------------------    PROFILE INFORMATION   ----------------------------
# 
#       WQD7002 Science Data Research Project - Session 2018-2019 (Sem I and II)
#
#       Submission By : Mohd Azhar Shahbudin (WQD170058)
#
#       Instructor    : Assoc. Dr Salimah Mokhtar
#
#       Dateline      : TBD
#
#
#-------------------------------  DATASETS DECLARATION    ----------------------------
# Extracting data set to be used for the R PRogramming activities and streamline those 
# required field for the to be the ultimate dataset for the exercise.
#
# Packages installation:

#----------------------    PACKAGES FOR DATA MANIPULATION    -------------------------
# 1. install.packages("excel.link")
# 2. install.packages("dplyr")

#--------------------------    PACKAGES FOR BASIC PLOTTING    ------------------------
# 3. install.packages("plotly")
# 4. install.packages("ggplot2")

#----------------------    PACKAGES FOR ANIMATED PLOTTING    -------------------------
# 5. install.packages("gganimate")
# 6. install.packages("gifski")
# 7. install.packages("png")
#
#-------------------------------  INITIALIZED LIBRARIES   ----------------------------
cat("\014\n\n")
cat("Initializing all relevant R libraries......................................\n")

library(excel.link)
library(dplyr)
library(plotly)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(stringr)

#--------------------------  SETTING ALL IMPORTANT DIRECTORIES   ---------------------
cat("Setting all directories paths..............................................\n\n")

DirDataConv <- "F:/WQD7002-WQD170058-Project Data Science-Final/Datasource/Conversion"
DirDataPrep <- "F:/WQD7002-WQD170058-Project Data Science-Final/Datasource/PreFinal"
DirProgram  <- "F:/WQD7002-WQD170058-Project Data Science-Final/R-Programs"
DirVisual   <- "F:/WQD7002-WQD170058-Project Data Science-Final/Visualization"

#-----------------------  READING CONVERTED INPUT DATASET FILES   --------------------
cat("Reading all datasets source................................................\n")

#------ Setting the Conversion Dataset directory   ----------------------------------------

setwd(DirDataConv)

dsRate      = xl.read.file('01_MyLabourForceRate.xls')
dsGender    = xl.read.file('02_MyLabourGender.xls')
dsRural     = xl.read.file('03_MyLabourStrataRural.xls')
dsUrban     = xl.read.file('03_MyLabourStrataUrban.xls')
dsAge       = xl.read.file('04_MyLabourAgeSegment.xls')
dsEthnic    = xl.read.file('05_MyLabourEthnicWorkforce.xls')
dsEdu       = xl.read.file('06_MyLabourEduWorkforce.xls')
dsCert      = xl.read.file('07_MyLabourCertEduWorkforce.xls')
dsMarital   = xl.read.file('08_MyLabourMaritalWorkforce.xls')

#--------------------------     DATA PREPROCESSING PROCESS  ---------------------------
# Filling up the missing value in the dataset in which some of the records may have  
# empty values or NA as partof the data cleaning exercise either we could apply the 
# mean:mode:average:delete situation to filling up those empty slots (if applied).
# -------------------------------------------------------------------------------------
cat("Start Preprocessing the datasets...........................................\n\n")

#------ Setting the source Program directory   ----------------------------------------
setwd(DirProgram)
source("DPDataPreProcessing.R")

#------ Processing the Data Pre Processing of those datasets   ------------------------
DataPreProcessing("dsRate")
DataPreProcessing("dsGender")
DataPreProcessing("dsRural")
DataPreProcessing("dsUrban")
DataPreProcessing("dsAge")
DataPreProcessing("dsEthnic")
DataPreProcessing("dsEdu")
DataPreProcessing("dsCert")
DataPreProcessing("dsMarital")

#--------------------------------   MERGING DATASET   --------------------------------
# We can merge some of the dataset to be more representable structure as some of 
# the details was kept in other separate dataset thus it also allows us to minimize 
# the number of final dataset to be dealing with on the next visualizatio prosesses.
#-------------------------------------------------------------------------------------
cat("Start combining or merging datasets........................................\n")

cat("Combining Labour (Rate, Gender and Ethnic) dataset.........................\n")
dsLabourDetail     <- full_join(full_join(dsRate, dsGender, by=c("ConState","Year")), dsEthnic, by=c("ConState","Year"))

cat("Combining Labour (Rural and Urban) dataset.................................\n")
dsLabourArea       <- full_join(dsRural, dsUrban, by=c("ConState","Year"))

cat("Combining Labour (Education and Certification) dataset.....................\n")
dsLabourEducation  <- full_join(dsEdu,   dsCert,  by=c("ConState","Year"))

cat("Combining Labour (Marital and Age) dataset.................................\n\n")
dsLabourMaritalAge <- full_join(dsMarital, dsAge, by=c("ConState","Year"))

#--------     ELIMINATION OF UNUSED OR REDUNDANT VARIABLES     -----------------------
#  Some fo the rate variable was not relevant upon joining/combination of few dataset 
#  into consolidate dataset prior further usage in the project programming. The value
#  of those rate need to be recalculated as it shall derive from the new combined
#  variables.
#-------------------------------------------------------------------------------------
cat("Removing Labour dataset variables that not relevant........................\n")

dsLabourDetail  <- dsLabourDetail %>% 
  select(-RateLabEmp, -RateLabUnEmp, -RateLabEmpM, -RateLabUnEmpM, -RateLabEmpF, -RateLabUnEmpF)

dsLabourArea    <- dsLabourArea %>% 
  select(-LabForceRateR, -LabForceUempRateR, -LabForceRateU, -LabForceUempRateU)


#-----------------------        DATA CLEANING PROCESS       --------------------------
# Filling up the missing value in the dataset in which some of the records may have  
# empty values or NA as partof the data cleaning exercise either we could apply the 
# mean:mode:average:delete situation to filling up those empty slots (if applied).
# As during the merging dataset, some dataset for example in Rural was not having any
# data for W.P Kuala Lumpur and W.P Putrajaya, thus this dataset when merge with Urban
# dataset definitely having no relevant values. Compare to W.P Labuan, it was consider 
# having both Rural and Urban information (as Labuan was not similar to W.P Kuala 
# Lumpur and W.P Putrajaya).
# -------------------------------------------------------------------------------------
cat("Conducting Data Cleaning upon merging dataset..............................\n")

#------ Setting the source Program directory   ----------------------------------------
setwd(DirProgram)
source("DPDataCleaning.R")

#------ Processing the Data Cleaning of those datatsets   -----------------------------
DataCleaning("dsLabourDetail")
DataCleaning("dsLabourArea")
DataCleaning("dsLabourEducation")
DataCleaning("dsLabourMaritalAge")

#----------------   SEGREGATE DATA OBSERVATION WITH VALUE (COUNTRY)   ----------------
# All of the dataset inlcudes states and country Malaysia inthe observation, thus we
# need to extract out (exlude) "Malaysia" rows/observation from the dataset. 
#
# Objective : To avoid duplicates values of entry calculations of total number of 
#             Labour Force in entire datasets.
#-------------------------------------------------------------------------------------
cat("Segregating based on country as Malaysia dataset...........................\n")

#------ Extracting for country level (Malaysia) observation records   ----------------
dsLabourDetailMy      <- dsLabourDetail     %>% filter(ConState =="Malaysia")
dsLabourAreaMy        <- dsLabourArea       %>% filter(ConState =="Malaysia")
dsLabourEducationMy   <- dsLabourEducation  %>% filter(ConState =="Malaysia")
dsLabourMaritalAgeMy  <- dsLabourMaritalAge %>% filter(ConState =="Malaysia")

#-----------------   SEGREGATE DATA OBSERVATION WITH VALUE (STATE)   ------------------
# All of the dataset inl4cudes states and country Malaysia inthe observation, thus we
# need to extract out (exlude) "Malaysia" rows/observation from the dataset. 
#
# Objective : To avoid duplicates values of entry calculations of total number of 
#             Labour Force in entire datasets.
#-------------------------------------------------------------------------------------
cat("Segregating based on States dataset........................................\n\n")

#------ Extracting for state level (States) observation records   --------------------
dsLabourDetailState     <- dsLabourDetail     %>% filter(ConState !="Malaysia")
dsLabourAreaState       <- dsLabourArea       %>% filter(ConState !="Malaysia")
dsLabourEducationState  <- dsLabourEducation  %>% filter(ConState !="Malaysia")
dsLabourMaritalAgeState <- dsLabourMaritalAge %>% filter(ConState !="Malaysia")

#---------------------   MULTIPLE BAR PLOTTING (LABOUR DETAIL)  ----------------------
# Based on dataset selection, we provide multiple types of Plotting:
#
#    - To show overall Labour Force Performance and change of frequency demand from 
#      year 1982 until 2017. Using dataset dsLabourDetail. Plotting Labour Force 
#      data from Labour Details (State & Malaysia).
#
#    - To show usgae distribution in Rural and Urban categories. Using dataset 
#      dsLabourArea. Plotting Labour Force data from Labour Area (Rural & Urban).
#
#    - To show the utilization of Ethnic based labour. Using dataset dsLabourDetail
#      Plotting Labour Force data from Labour Area (Ethnic).
#
#-------------------------------------------------------------------------------------
cat("\014")
sequence_set <- 1
while(sequence_set > 0) 
{
  cat("\014")
  cat("\n\n")
  cat("----------------------------------------------------------------------\n")
  cat("                         M A I N   M E N U                            \n")
  cat("                  Labour Force Dataset Selection                      \n")
  cat("----------------------------------------------------------------------\n\n")   
  cat("    1. State & Malaysia Performance (Distribution):                   \n\n")
  cat("       - Performance Time Series Data from 1982 till 2017             \n\n")
  cat("    2. Area Penetration:                                              \n\n")
  cat("       - Covering the entire State & Malaysia according to Rural      \n")
  cat("         and Urban area of distribution.                              \n\n")
  cat("    3. Ethnic Service:                                                \n\n")
  cat("       - Distribution by Malaysian(Bumi, Chinese, Indian & Others)    \n")
  cat("         and Foreign labours                                          \n\n")
  cat("    4. Interactive Visualization                                      \n")
  cat("                                                                      \n\n")
  cat("    5. Machine Learning Algorithm                                     \n")
  cat("          e.g Naive Bayes, K-Nearest Neighbour                        \n\n")
  cat("    6. Exit                                                           \n\n")
  optvalue <- readline(prompt="       Please enter your number: ")
  optvalue <- as.numeric(optvalue)
  par(mfrow = c(1,1))
  
  if (optvalue==1)
  { 
    #-------------------------   BAR PLOTTING (LABOUR DETAIL)  ---------------------------
    # Based on State selection, we do Bar plotting to show the frequency changes from 
    # year 1982 until 2017. Dataset used was dsLabourDetail.
    #-------------------------------------------------------------------------------------
    setwd(DirProgram)
    par(mfrow = c(1,1))
    source("BPLabourDetail.R")
    datafile <- dsLabourDetail %>% 
      select(ConState, Year, TotInsLabour, TotOutLabour, 
             TotInsLabourM, TotOutLabourM, TotInsLabourF, TotOutLabourF)
    BarPlotDetail(datafile)
    
    }
  if (optvalue==2)
  { 
    #-------------------   BAR PLOTTING (LABOUR AREA - RURAL/URBAN)  ---------------------
    # Based on State selection, we do Bar plotting to show the frequency changes from 
    # year 1982 until 2017. Dataset used was dsLabourArea.
    #-------------------------------------------------------------------------------------
    setwd(DirProgram)
    par(mfrow = c(1,1))
    source("BPLabourArea.R")
    datafile <- dsLabourArea %>% 
      select(ConState, Year, AllLabourInsR, AllLabourOutR, AllLabourInsU, AllLabourOutU)
    BarPlotArea(datafile)
    
    }
  if (optvalue==3)
  { 
    #---------------------   BAR PLOTTING (LABOUR AREA - ETHNIC)  ------------------------
    # Based on State selection, we do Bar plotting to show the frequency changes from 
    # year 1982 until 2017. Dataset used was dsLabourDetail.
    #-------------------------------------------------------------------------------------
    setwd(DirProgram)
    par(mfrow = c(1,1))
    source("BPLabourEthnic.R")
    datafile <- dsLabourDetail %>%
      select(ConState, Year, SubTotWorkForce, SubTotForeignWorkForce,
             BumiWorkForce, ChineseWorkForce, IndianWorkForce, OtherWorkForce)
    BarPlotEthnic(datafile)
    
   }
  if (optvalue==4)
  { 
    #------------------------   INTERACTIVE VISUALIZATION PLOT  --------------------------
    # Based on State selection, we do Interactive plotting to show the frequency changes 
    # from year 1982 until 2017 with selection of State interactively using Plot_ly()
    # Dataset used was dsLabourDetailState.
    #-------------------------------------------------------------------------------------
    setwd(DirProgram)
    par(mfrow = c(1,1))
    source("IPInteractivePlot.R")
    datafile <- dsLabourDetailState
    datafile <- datafile %>% mutate(TotMaleLabour=TotInsLabourM + TotOutLabourM)
    datafile <- datafile %>% mutate(TotFemaleLabour=TotInsLabourF + TotOutLabourF)
    VisualPlotly(datafile)
    
    }
  if (optvalue==5)
  { 
    #------------------------   MACHINE LEARNING ALGORITHM   -----------------------------
    # Based on the given dataset, after the restructure of those variables into few 
    # pre-final dataset, we applied Machine Learning algorith to make some prediction of
    # accuracy using for example Naive Bayes, KNN K-Nearest Neighbour with Confusion Matrix .....
    #-------------------------------------------------------------------------------------
    setwd(DirProgram)
    par(mfrow = c(1,1))
    source("MLMachineLearning.R")
    datafileML <- dsLabourDetailState
    MLAlgoTypes(datafileML)
    
    #-------------------------------------------------------------------------------------
    # Combine and updated the result after the revised Rate have been calculated
    #-------------------------------------------------------------------------------------
    dsLabourDetailState <- right_join(dsLabourDetailState, datafileML, by=c("ConState","Year"))
    
    }
  if (optvalue >= 6) 
  {  
    sequence_set <- 0
    paste("Main Menu: Bar Plotting has been completed")
  }
  par(mfrow = c(1,1))
  setwd(DirProgram)
}

#-----------------------   SAVING THE CLEANING DATASET (ALL)  ------------------------
#----------------   Save Pre-Final Dataset (From Excel*.xls to *.csv)  ---------------
# The source dataset contains full data as Country and State level inside each
# of the source dataset. 
#-------------------------------------------------------------------------------------

#------ Setting the Saving Location of the datatset   --------------------------------
setwd(DirDataPrep)

#------ Saving (Write Command) to save those datatset   ------------------------------
cat("Copying all dataset .........................................................\n")

write.csv(dsLabourDetail,    file="AllLabourDetail.csv",    row.names = FALSE)
write.csv(dsLabourArea,      file="AllLabourArea.csv",      row.names = FALSE)
write.csv(dsLabourEducation, file="AllLabourEducation.csv", row.names = FALSE)
write.csv(dsLabourMaritalAge,file="AllLabourMaritalAge.csv",row.names = FALSE)

#----------------------   SAVING THE CLEANING DATASET (COUNTRY) ----------------------
#----------------   Save Pre-Final Dataset (From Excel*.xls to *.csv)  ---------------
# The source dataset contains both data as Country level to State level inside each
# of the source dataset. We have extract those and spliting its into 2 set of dataset
# group as Malaysia and States category.
#
# Malaysia Dataset: Comprise all the Main Total for each year compilation generated
#                   from every States level.
#-------------------------------------------------------------------------------------

#------ Setting the Saving Location of the datatset   --------------------------------
setwd(DirDataPrep)
cat("Saving dataset (after processing) according to Malaysia as subcategory.......\n")


#------ Saving (Write Command) to save those datatset   ------------------------------
write.csv(dsLabourDetailMy,    file="MyLabourDetail.csv",    row.names = FALSE)
write.csv(dsLabourAreaMy,      file="MyLabourArea.csv",      row.names = FALSE)
write.csv(dsLabourEducationMy, file="MyLabourEducation.csv", row.names = FALSE)
write.csv(dsLabourMaritalAgeMy,file="MyLabourMaritalAge.csv",row.names = FALSE)

#-----------------------   SAVING THE CLEANING DATASET (STATE)  ----------------------
#----------------   Save Pre-Final Dataset (From Excel*.xls to *.csv)  ---------------
# The source dataset contains both data as Country level to State level inside each
# of the source dataset. We have extract those and spliting its into 2 set of dataset
# group as Malaysia and States category.
#
# State Dataset: Comprise all the detail total for each year compilation generated
#                from individual States level.
#-------------------------------------------------------------------------------------

#------ Setting the Saving Location of the datatset   --------------------------------
setwd(DirDataPrep)
cat("Saving dataset (after processing) according to State as subcategory..........\n\n")

#------ Saving (Write Command) to save those datatset   ------------------------------
write.csv(dsLabourDetailState,    file="StateLabourDetail.csv",    row.names = FALSE)
write.csv(dsLabourAreaState,      file="StateLabourArea.csv",      row.names = FALSE)
write.csv(dsLabourEducationState, file="StateLabourEducation.csv", row.names = FALSE)
write.csv(dsLabourMaritalAgeState,file="StateLabourMaritalAge.csv",row.names = FALSE)

dev.off()
#----------------------------------------------------------------------------------
#                    END OF SCIENCE DATA RESEARCH PROJECT
#----------------------------------------------------------------------------------


