MLAlgoTypes <- function(datafileML)
{
  cat("\014")
  #
  # Preparation of the dataset for Machine Learning algorithm
  #
  dataord <- datafileML %>% select(ConState,Year,TotInsLabour, TotInsLabourM, TotInsLabourF) %>% 
    arrange(ConState,Year) %>% 
    mutate(ChgAllRate=0, ChgAllLabour=0, ChgMRate=0, ChgMLabour=0, ChgFRate=0, ChgFLabour=0)

  print(head(dataord[c(1:2,6:11)], 10))
  print(tail(dataord[c(1:2,6:11)], 10))
  readline("The result of dataset before prediction value calculation.....<press return when completed>")

  num <- nrow(dataord)
  num <- num-1

  for (i in 1:num)
  {
    if (i<=num)
    { 
      if (dataord$ConState[i] == dataord$ConState[i+1])
      {
        #
        #------------------------    CALCULATE NUMBER AND RATE    -----------------------------
        # Calculate the different (increase/decrease) of Labour Force on each year. Thus, we could 
        # know the total number of incremental of Labur by Gender (M/F) in overall for each year. 
        # Our target was to fin dth eanswer if the Female categories was having more percentage 
        # increase compare to Male categories. This to concur that Female Labour have contribute 
        # as one of economic factors influencer to Malaysia economy as teh counrty moving towards
        # Developed Country vision.
        #
        #------------------------   CALCULATE INCREMENTAL NUMBER   -----------------------------
        # Find the calculated number of incremental value of Labour Force intake on each year.
        #
        dataord$ChgAllLabour[i+1] <- as.numeric(dataord$TotInsLabour[i+1])  - as.numeric(dataord$TotInsLabour[i])
        dataord$ChgMLabour[i+1]   <- as.numeric(dataord$TotInsLabourM[i+1]) - as.numeric(dataord$TotInsLabourM[i])
        dataord$ChgFLabour[i+1]   <- as.numeric(dataord$TotInsLabourF[i+1]) - as.numeric(dataord$TotInsLabourF[i])
        #
        #------------------------     CALCULATE RATE PERCENTAGE    -----------------------------
        # Find the calculated percentage rate (rebuidling back those rate values) value of 
        # Labour Force intake on each year.
        #
        dataord$ChgAllRate[i+1] <- as.numeric((dataord$ChgAllLabour[i+1]/dataord$TotInsLabour[i])*100)
        dataord$ChgMRate[i+1]   <- as.numeric((dataord$ChgMLabour[i+1]/dataord$TotInsLabourM[i])*100)
        dataord$ChgFRate[i+1]   <- as.numeric((dataord$ChgMLabour[i+1]/dataord$TotInsLabourF[i])*100)
      }
    }
  }
  datafileML <<- dataord
  
  print(head(dataord[c(1:2,6:11)], 10))
  print(tail(dataord[c(1:2,6:11)], 10))
  readline("The result of dataset after prediction value calculation.....<press return when completed>")
  
  sequence_set <- 1
  while(sequence_set > 0) 
  {
    cat("\014")
    cat("\n\n")
    cat("----------------------------------------------------------------------\n\n")
    cat("                           Machine Learning                           \n")
    cat("                   Algorithms Implementation Series                   \n")
    cat("                   (Time Series Data from 1982-2017)                  \n\n")
    cat("----------------------------------------------------------------------\n\n")   
    cat("            1. Change by Gender                                       \n")
    cat("               1.1 Male & Female - Change (#) Number                  \n")
    cat("               1.2 Male & Female - Change (%) Rate                    \n\n")
    cat("            2. Change by Number (#) and Rate (%)                      \n")
    cat("               2.1 Male   - Comparison of Number (#) and Rate (%)     \n")
    cat("               2.2 Female - Comparison of Number (#) and Rate (%)     \n\n")
    cat("            3. Machine Learning Algorithm                             \n")
    cat("               3.1 Naive Bayes                                        \n")
    cat("               3.2 K-Nearest Neighbour                                \n")
    cat("               3.3 Random Forest                                      \n\n")
    cat("            4. Exit                                                   \n")
    cat("\n")
    vstate <- readline(prompt="       Please enter your number: \n")
    vstate <- as.numeric(vstate)
    
    if (vstate >= 4) 
    {
      sequence_set <- 0
      return(paste("Machine Learning Algorithm has been completed \n"))
    }
    else if (vstate == 1.1)
    {
      
      #----------------------------------------------------------------------------------
      #------------------------      NAIVE BAYES PREDICTION      ------------------------
      #----------------------------------------------------------------------------------
      # Prediction module using the standard Naive Bayes. A list of tables, one for 
      # each predictor variable.
      #
      # Packages     : install.packages('e1071')
      # Library use  : library(e1071)
      # Classifier   : classifier()
      # Prediction   : predict()
      #
      # Plotting the # number change in Labour Force for both Male and Female to shown 
      # the Time series change over period from 1982-2017.
      #
      #----------------------------------------------------------------------------------
      
      sdf <- dataord
      
      p1<- plot_ly(sdf,x=sdf$Year) %>%
        layout(title="1.1 Labour Change in # numbers ('000) for Male and Female Statistic", 
               xaxis=list(title = 'Year'), yaxis = list(range = c(-150,350))) %>%
        add_lines(x=~Year, y=sdf$ChgMLabour, marker=list(color='blue'), name = "Male - NUmber in ('000)")

      p2 <- plot_ly(sdf,x=sdf$Year) %>%
        layout(title="1.1 Labour Change in # numbers ('000) for Male and Female Statistic", 
               xaxis=list(title = 'Year'), yaxis = list(range = c(-150,350))) %>%
        add_lines(x=~Year, y=sdf$ChgFLabour, marker=list(color='green'), name = "Female - Number in ('000)")
      
      p3 <- subplot(p1, p2, titleX = TRUE, widths = c(0.5, 0.5))
      
      print(p3)
      
      #--------------------------------------------------------------------------------
      #  Saving the plot in html format for later use. Save in the Visual directory
      #--------------------------------------------------------------------------------
      
      setwd(DirVisual)
      htmlwidgets::saveWidget(as.widget(p3), file = "1.1-LabourGender-InNumber-Plotly.html")
      setwd(DirProgram)

      #--------------------------------------------------------------------------------
      #  Close the html file, Change back to Program directory
      #--------------------------------------------------------------------------------
      
    }
    else if (vstate == 1.2)
    {
      #
      # Plotting the rate (%) change in Labour Force for both Male and Female to shown the Time series
      # change over period from 1982-2017.
      #
      sdf <- dataord
      p1<- plot_ly(sdf,x=sdf$Year) %>%
        layout(title="1.2 Labour Change in % Rate for Male and Female Statistic", 
               xaxis=list(title = 'Year'), yaxis = list(range = c(-150,350))) %>%
        add_lines(x=~Year, y=sdf$ChgMRate, marker=list(color='blue'), name = "Male - Rate (%)")
      
      p2 <- plot_ly(sdf,x=sdf$Year) %>%
        layout(title="1.2 Labour Change in % Rate for Male and Female Statistic", 
               xaxis=list(title = 'Year'), yaxis = list(range = c(-150,350))) %>%
        add_lines(x=~Year, y=sdf$ChgFRate, marker=list(color='green'), name = "Female - Rate (%)")
      
      p3 <- subplot(p1, p2, titleX = TRUE, widths = c(0.5, 0.5)) 

      print(p3)

      #--------------------------------------------------------------------------------
      #  Saving the plot in html format for later use. Save in the Visual directory
      #--------------------------------------------------------------------------------
      
      setwd(DirVisual)
      htmlwidgets::saveWidget(as.widget(p3), file = "1.2-LabourGender-InRate-Plotly.html")
      setwd(DirProgram)
      
      #--------------------------------------------------------------------------------
      #  Close the html file, Change back to Program directory
      #--------------------------------------------------------------------------------
      
    }
    else if (vstate == 2.1)
    {
      #
      # Plotting the rate (%) change in Labour Force for both Male and Female to shown the Time series
      # change over period from 1982-2017.
      #
      sdf <- dataord
      p1<- plot_ly(sdf,x=sdf$Year) %>%
        layout(title="2.1 Male - Change of Labour Number (#) and Rate (%) Statistic", 
               xaxis=list(title = 'Year'), yaxis = list(range = c(-150,350))) %>%
        add_lines(x=~Year, y=sdf$ChgMLabour, marker=list(color='blue'), name = "Male - Number (#'000)")
      
      p2 <- plot_ly(sdf,x=sdf$Year) %>%
        layout(title="2.1 Male - Change of Labour Number (#) and Rate (%) Statistic", 
               xaxis=list(title = 'Year'), yaxis = list(range = c(-150,350))) %>%
        add_lines(x=~Year, y=sdf$ChgMRate, marker=list(color='green'), name = "Male - Rate (%)")
      
      p3 <- subplot(p1, p2, titleX = TRUE, widths = c(0.5, 0.5))
     
      print(p3)
      
      #--------------------------------------------------------------------------------
      #  Saving the plot in html format for later use. Save in the Visual directory
      #--------------------------------------------------------------------------------
      
      setwd(DirVisual)
      htmlwidgets::saveWidget(as.widget(p3), file = "2.1-LabourMale-InNumberRate-Plotly.html")
      setwd(DirProgram)
      
      #--------------------------------------------------------------------------------
      #  Close the html file, Change back to Program directory
      #--------------------------------------------------------------------------------
      
    }
    else if (vstate == 2.2)
    {
      #
      # Plotting the rate (%) change in Labour Force for Female to shown the Time series
      # change over period from 1982-2017.
      #
      sdf <- dataord
      p1<- plot_ly(sdf,x=sdf$Year) %>%
        layout(title="2.2 Female - Change of Labour Number (#) and Rate (%) Statistic", 
               xaxis=list(title = 'Year'), yaxis = list(range = c(-150,350))) %>%
        add_lines(x=~Year, y=sdf$ChgFLabour, marker=list(color='blue'), name = "Female - Number (#'000)")
      
      p2 <- plot_ly(sdf,x=sdf$Year) %>%
        layout(title="2.2 Female - Change of Labour Number (#) and Rate (%) Statistic", 
               xaxis=list(title = 'Year'), yaxis = list(range = c(-150,350))) %>%
        add_lines(x=~Year, y=sdf$ChgFRate, marker=list(color='green'), name = "Female - Rate (%)")
      
      p3 <- subplot(p1, p2, titleX = TRUE, widths = c(0.5, 0.5))

      print(p3)
      
      #--------------------------------------------------------------------------------
      #  Saving the plot in html format for later use. Save in the Visual directory
      #--------------------------------------------------------------------------------
      
      setwd(DirVisual)
      htmlwidgets::saveWidget(as.widget(p3), file = "2.2-LabourFemale-InNumberRate-Plotly.html")
      setwd(DirProgram)
      
      #--------------------------------------------------------------------------------
      #  Close the html file, Change back to Program directory
      #--------------------------------------------------------------------------------
      
    }
    else if (vstate == 3.1)
    {
      #
      # Algorithm Naive Bayes.
      #
      #
      # Annual Accepttable rate (AARate) of labour intake. This value was to define certain
      # rate increase in order to control and balance jobs opportunities within industry
      # level.
      #
      par(mfrow = c(1,1))
      
      library(caTools)
      library(e1071)
      library(caret)
      library(ElemStatLearn)

      setwd(DirProgram)
      source("MLAlgorithms.R")
    
      sdf <- dataord
      minAllLab <- min(as.numeric(sdf$ChgAllRate))
      maxAllLab <- max(as.numeric(sdf$ChgAllRate))
      
      cat("---------------------------------------------------------------------------\n\n")
      cat("                        N A I V E    B A Y E S                             \n\n")
      cat("Starting processing Overall Labour categories..............................\n\n")
      cat("The minimum value of Overall Labour changes (%) was: ",minAllLab,"\n")
      cat("The maximum value of Overall Labour changes (%) was: ",maxAllLab)

      AARate <- readline("Please enter Acceptable Annual rate (x.xx)% range: ")

      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate,  ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))

      # Encoding the target algortihm for All Labour
      
      algos <- "NaiveBayes"
      types <- "AllLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabour, ChgAllLabour, IndAllLab)
      mlsdf$IndAllLab = factor(mlsdf$IndAllLab, levels = c(0, 1))
      
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("All Labour: Completed process prediction category..........................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      
      # Encoding the target algortihm for Male Labour
      
      sdf <- dataord
      minMLab <- min(as.numeric(sdf$ChgMRate))
      maxMLab <- max(as.numeric(sdf$ChgMRate))
      
      cat("Starting processing Male Labour categories.................................\n\n")
      cat("The minimum value of Male Labour changes (%) was: ",minMLab,"\n")
      cat("The maximum value of Male Labour changes (%) was: ",maxMLab)
      
      AARate <- readline("Please enter Acceptable Annual rate (x.xx)% range: ")
      
      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate,  ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))

      # Encoding the target algortihm for Male Labour
      
      algos <- "NaiveBayes"
      types <- "MLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabourM, ChgMLabour, IndMLab)
      mlsdf$IndMLab = factor(mlsdf$IndMLab, levels = c(0, 1))
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("Male Labour: Completed process prediction category.........................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      
      # Encoding the target algortihm for Female Labour
      
      sdf <- dataord
      minFLab <- min(as.numeric(sdf$ChgFRate))
      maxFLab <- max(as.numeric(sdf$ChgFRate))
      
      cat("Starting processing Female Labour categories...............................\n\n")
      cat("The minimum value of Female Labour changes (%) was: ",minFLab,"\n")
      cat("The maximum value of Female Labour changes (%) was: ",maxFLab)
      
      AARate <- readline("Please enter Acceptable Annual rate (x.xx)% range: ")
      
      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate,  ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))

      # Encoding the target algortihm for Female Labour
      
      algos <- "NaiveBayes"
      types <- "FLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabourF, ChgFLabour, IndFLab)
      mlsdf$IndFLab = factor(mlsdf$IndFLab, levels = c(0, 1))
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("Female Labour: Completed process prediction category.......................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      
      cat("Note:\n")
      cat("We can retry these options again by changing the Acceptable Annual Rate(%) value.\n\n")
      readline("Complete Plotting for Naive Bayes.............<Press return to continue> : \n")
    }
    else if (vstate == 3.2)
    {
  
      #----------------------------------------------------------------------------------
      #--------------------------    K-NEAREST NEIGHBORS ALGORITHM   --------------------
      #----------------------------------------------------------------------------------
      # Fitting K-NN to the Training set and Predicting the Test set results
      # each predictor variable.
      #
      # Packages     : install.packages('class')
      # Library use  : library(class)
      # 
      # Annual Accepttable rate (AARate) of labour intake. This value was to define 
      # certain rate increase in order to control and balance jobs opportunities within
      # industry level.
      #----------------------------------------------------------------------------------

      par(mfrow = c(1,1))
      
      library(caTools)
      library(e1071)
      library(class)
      library(caret)
      library(ElemStatLearn)
      
      setwd(DirProgram)
      source("MLAlgorithms.R")
      
      sdf <- dataord
      minAllLab <- min(as.numeric(sdf$ChgAllRate))
      maxAllLab <- max(as.numeric(sdf$ChgAllRate))
      
      cat("---------------------------------------------------------------------------\n\n")
      cat("                  K - N E A R E S T   N E I G H B O U R                    \n\n")
      cat("Starting processing Overall Labour categories..............................\n\n")
      cat("The minimum value of Overall Labour changes (%) was: ",minAllLab,"\n")
      cat("The maximum value of Overall Labour changes (%) was: ",maxAllLab)
      
      AARate <- readline("Please enter Acceptable Annual rate (x.xx)% range: ")
      
      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate,  ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))
      
      # Encoding the target algortihm for All Labour
      
      algos <- "KNNeighbour"
      types <- "AllLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabour, ChgAllLabour, IndAllLab)
      mlsdf$IndAllLab = factor(mlsdf$IndAllLab, levels = c(0, 1))
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("All Labour: Completed process prediction category..........................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      
      # Encoding the target algortihm for Female Labour
      
      sdf <- dataord
      minMLab <- min(as.numeric(sdf$ChgMRate))
      maxMLab <- max(as.numeric(sdf$ChgMRate))
      
      cat("Starting processing Female Labour categories...............................\n\n")
      cat("The minimum value of Male Labour changes (%) was: ",minMLab,"\n")
      cat("The maximum value of Male Labour changes (%) was: ",maxMLab)
      
      AARate <- readline("Please enter Accpetable Annual rate (x.xx)% range: ")
      
      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate,  ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))
      
      # Encoding the target algortihm for Female Labour
      
      algos <- "KNNeighbour"
      types <- "MLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabourM, ChgMLabour, IndMLab)
      mlsdf$IndMLab = factor(mlsdf$IndMLab, levels = c(0, 1))
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("Male Labour: Completed process prediction category.........................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      
      # Encoding the target algortihm for Female Labour
      
      sdf <- dataord
      minFLab <- min(as.numeric(sdf$ChgFRate))
      maxFLab <- max(as.numeric(sdf$ChgFRate))
      
      cat("Starting processing Female Labour categories...............................\n\n")
      cat("The minimum value of Female Labour changes (%) was: ",minFLab,"\n")
      cat("The maximum value of Female Labour changes (%) was: ",maxFLab)
      
      AARate <- readline("Please enter Acceptable Annual rate (x.xx)% range: ")
      
      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate,  ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))
      
      # Encoding the target algortihm for Female Labour
      
      algos <- "KNNeighbour"
      types <- "FLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabourF, ChgFLabour, IndFLab)
      mlsdf$IndFLab = factor(mlsdf$IndFLab, levels = c(0, 1))
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("Female Labour: Completed process prediction category.......................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      cat("Note:\n")
      cat("We can retry these options again by changing the Acceptable Annual Rate(%) value.\n\n")
      readline("Complete Plotting for K Nearest Neighbour.....<Press return to continue> : \n")

    }
    else if (vstate == 3.3)
    {
      
      #----------------------------------------------------------------------------------
      #----------------------------      RANDOM FOREST ALGORITHM     --------------------
      #----------------------------------------------------------------------------------
      # Fitting K-Mean Clustering to the Training set and Predicting the Test set results
      # each predictor variable.
      #
      # Annual Accepttable rate (AARate) of labour intake. This value was to define 
      # certain rate increase in order to control and balance jobs opportunities within
      # industry level.
      #
      # An algorithm used to find homogeneous subgroups in a data population. KMmeans 
      # comes in base R with need the data and number of centers or groups to be runs. 
      # Its start by randomly assigning points to groups and you can find local minimums
      # so running it multiple times shall find the global min. and we run kmeans many 
      # times to estimate the number od subgroups when it is not known a priori.
      #----------------------------------------------------------------------------------
      
      par(mfrow = c(1,1))
      
      library(caTools)
      library(e1071)
      library(class)
      library(caret)
      library(ElemStatLearn)
      library(randomForest)
      
      setwd(DirProgram)
      source("MLAlgorithms.R")
      
      sdf <- dataord
      minAllLab <- min(as.numeric(sdf$ChgAllRate))
      maxAllLab <- max(as.numeric(sdf$ChgAllRate))
      
      cat("---------------------------------------------------------------------------\n\n")
      cat("                   R A N D O M   F O R E S T                               \n\n")
      cat("Starting processing Overall Labour categories..............................\n\n")
      cat("The minimum value of Overall Labour changes (%) was: ",minAllLab,"\n")
      cat("The maximum value of Overall Labour changes (%) was: ",maxAllLab)
      
      AARate <- readline("Please enter Acceptable Annual rate (x.xx)% range: ")
      
      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate, ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))
      
      # Encoding the target algortihm for All Labour
      
      algos <- "RandomForest"
      types <- "AllLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabour, ChgAllLabour, IndAllLab)
      mlsdf$IndAllLab = factor(mlsdf$IndAllLab, levels = c(0, 1))
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("All Labour: Completed process prediction category..........................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      
      # Encoding the target algortihm for Male Labour
      
      sdf <- dataord
      minMLab <- min(as.numeric(sdf$ChgMRate))
      maxMLab <- max(as.numeric(sdf$ChgMRate))
      
      cat("Starting processing Male Labour categories.................................\n\n")
      cat("The minimum value of Male Labour changes (%) was: ",minMLab,"\n")
      cat("The maximum value of Male Labour changes (%) was: ",maxMLab)
      
      AARate <- readline("Please enter Acceptable Annual rate (x.xx)% range: ")
      
      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate, ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))
      
      # Encoding the target algortihm for Female Labour
      
      algos <- "RandomForest"
      types <- "MLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabourM, ChgMLabour, IndMLab)
      mlsdf$IndMLab = factor(mlsdf$IndMLab, levels = c(0, 1))
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("Male Labour: Completed process prediction category.........................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      
      # Encoding the target algortihm for Female Labour
      
      sdf <- dataord
      minFLab <- min(as.numeric(sdf$ChgFRate))
      maxFLab <- max(as.numeric(sdf$ChgFRate))
      
      cat("Starting processing Female Labour categories...............................\n\n")
      cat("The minimum value of Female Labour changes (%) was: ",minFLab,"\n")
      cat("The maximum value of Female Labour changes (%) was: ",maxFLab)
      
      AARate <- readline("Please enter Acceptable Annual rate (x.xx)% range: ")
      
      sdf <- sdf %>%
        select(ConState, Year, TotInsLabour, TotInsLabourM, TotInsLabourF,
               ChgAllRate, ChgAllLabour, ChgMRate, ChgMLabour, ChgFRate, ChgFLabour) %>%
        mutate(IndAllLab=(ifelse(ChgAllRate > as.integer(AARate),1,0)),
               IndMLab  =(ifelse(ChgMRate > as.integer(AARate),1,0)),
               IndFLab  =(ifelse(ChgFRate > as.integer(AARate),1,0)))
      
      # Encoding the target algortihm for Female Labour
      
      algos <- "RandomForest"
      types <- "FLabour"
      rateAA <- AARate
      
      mlsdf <- sdf %>% select(TotInsLabourF, ChgFLabour, IndFLab)
      mlsdf$IndFLab = factor(mlsdf$IndFLab, levels = c(0, 1))
      
      MLAlgorithmTypes(mlsdf,algos,types,rateAA)
      
      cat("Female Labour: Completed process prediction category.......................\n\n")
      cat("---------------------------------------------------------------------------\n\n")
      
      
      cat("Note:\n")
      cat("We can retry these options again by changing the Acceptable Annual Rate(%) value.\n\n")
      readline("Complete Plotting for Random Forest...........<Press return to continue> : \n")

    }
  }
  return(datafileML <<- dataord)
}
