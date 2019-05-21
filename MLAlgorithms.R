MLAlgorithmTypes <- function(mlsdf,algos,types,rateAA)
{
  setwd(DirVisual)
  
  if (algos == "NaiveBayes")
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

    if (types == "AllLabour")
    {
      
      #---------------------------------------------------------------------------
      # Open a pdf file
      #---------------------------------------------------------------------------
      pdf("NaiveBayes-AllLabour-Plot.pdf") 
      
      par(mfrow = c(2,1))
      
      AARate <- rateAA
      set.seed(128)
      split        = sample.split(mlsdf$IndAllLab, SplitRatio = 0.80)
      training_set = subset(mlsdf, split == TRUE)
      test_set     = subset(mlsdf, split == FALSE)

      training_set[-3] = scale(training_set[-3])
      test_set[-3]     = scale(test_set[-3])

      training_NB   <- training_set
      test_NB       <- test_set

      classifier_NB  = naiveBayes(x = training_NB[-3], y = training_NB$IndAllLab)
      print(classifier_NB)

      pred_NB   = predict(classifier_NB, newdata = test_NB[-3])
      cm_NB     = table(test_NB[, 3], pred_NB)
      
      results_NB <- confusionMatrix(data=cm_NB, reference=pred_NB)
      print(results_NB)

      cat("All Labour: Result of Classifier, Predictor and Confusion Matrix Accuracy..\n")

      #---------------------------------------------------------------------------
      # Visualising the Naive Bayes Training set results
      #---------------------------------------------------------------------------
            set = training_NB

      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabour', 'ChgAllLabour')

      y_grid = predict(classifier_NB, newdata = grid_set)
      
      plot(set[, -3],
           main = 'All Labour: On Naive Bayes Algorithm (Training)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of All Labour Number (#)',
           ylab = 'Change of All Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

      cat("Progressing plotting contour points on Training sample.....................\n")
 
      #---------------------------------------------------------------------------     
      # Visualising the Naive Bayes Testing set results
      #---------------------------------------------------------------------------
      set = test_NB

      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabour', 'ChgAllLabour')

      y_grid = predict(classifier_NB, newdata = grid_set)
      
      plot(set[, -3],
          main = 'All Labour: On Naive Bayes Algorithm (Testing)',
          sub = paste("Acceptable Annual Rate (%) of ",AARate),
          xlab = 'Change of All Labour Number (#)',
          ylab = 'Change of All Labour Rate (%)',
          xlim = range(X1),
          ylim = range(X2))

      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Testing sample......................\n")
    }
    if (types == "MLabour")
    {
      #---------------------------------------------------------------------------
      # Open a pdf file
      #---------------------------------------------------------------------------
      pdf("NaiveBayes-MaleLabour-Plot.pdf") 
      
      par(mfrow = c(2,1))
      
      AARate <- rateAA
      set.seed(128)
      split        = sample.split(mlsdf$IndMLab, SplitRatio = 0.80)
      training_set = subset(mlsdf, split == TRUE)
      test_set     = subset(mlsdf, split == FALSE)
      
      training_set[-3] = scale(training_set[-3])
      test_set[-3]     = scale(test_set[-3])
      
      training_NB   <- training_set
      test_NB       <- test_set
      
      classifier_NB  = naiveBayes(x = training_NB[-3], y = training_NB$IndMLab)
      print(classifier_NB)
      
      pred_NB   = predict(classifier_NB, newdata = test_NB[-3])
      cm_NB     = table(test_NB[, 3], pred_NB)
      
      results_NB <- confusionMatrix(data=cm_NB, reference=pred_NB)
      print(results_NB)
      
      cat("Male Labour: Result of Classifier, Predictor and Confusion Matrix Accuracy.\n")

      #---------------------------------------------------------------------------
      # Visualising the Naive Bayes Training set results
      #---------------------------------------------------------------------------     
      set = training_NB
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourM', 'ChgMLabour')
      
      y_grid = predict(classifier_NB, newdata = grid_set)
      
      plot(set[, -3],
           main = 'Male Labour: On Naive Bayes Algorithm (Training)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of Male Labour Number (#)',
           ylab = 'Change of Male Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Training sample.....................\n")

      #---------------------------------------------------------------------------      
      # Visualising the Naive Bayes Testing set results
      #---------------------------------------------------------------------------
      set = test_NB
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourM', 'ChgMLabour')
      
      y_grid = predict(classifier_NB, newdata = grid_set)
      
      plot(set[, -3],
           main = 'Male Labour: On Naive Bayes Algorithm (Testing)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of Male Labour Number (#)',
           ylab = 'Change of Male Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Testing sample......................\n")
    }
    if (types == "FLabour")
    {
      
      #---------------------------------------------------------------------------
      # Open a pdf file
      #---------------------------------------------------------------------------
      pdf("NaiveBayes-FemaleLabour-Plot.pdf") 
      
      par(mfrow = c(2,1))
      
      AARate <- rateAA
      set.seed(128)
      split        = sample.split(mlsdf$IndFLab, SplitRatio = 0.80)
      training_set = subset(mlsdf, split == TRUE)
      test_set     = subset(mlsdf, split == FALSE)
      
      training_set[-3] = scale(training_set[-3])
      test_set[-3]     = scale(test_set[-3])
      
      training_NB   <- training_set
      test_NB       <- test_set
      
      classifier_NB  = naiveBayes(x = training_NB[-3], y = training_NB$IndFLab)
      print(classifier_NB)
      
      pred_NB   = predict(classifier_NB, newdata = test_NB[-3])
      cm_NB     = table(test_NB[, 3], pred_NB)
      
      results_NB <- confusionMatrix(data=cm_NB, reference=pred_NB)
      print(results_NB)
      
      cat("Female Labour:Result of Classifier,Predictor and Confusion Matrix Accuracy.\n")
      
      #---------------------------------------------------------------------------
      # Visualising the Naive Bayes Training set results
      #---------------------------------------------------------------------------
      set = training_NB
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourF', 'ChgFLabour')
      
      y_grid = predict(classifier_NB, newdata = grid_set)
      
      plot(set[, -3],
           main = 'Female Labour: On Naive Bayes Algorithm (Training)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of Female Labour Number (#)',
           ylab = 'Change of Female Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

      cat("Progressing plotting contour points on Training sample.....................\n")
      
      #---------------------------------------------------------------------------
      # Visualising the Naive Bayes Testing set results
      #---------------------------------------------------------------------------
      set = test_NB
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourF', 'ChgFLabour')
      
      y_grid = predict(classifier_NB, newdata = grid_set)
      
      plot(set[, -3],
           main = 'Female Labour: On Naive Bayes Algorithm (Testing)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of Female Labour Number (#)',
           ylab = 'Change of Female Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Testing sample......................\n")
    }
  }
  if (algos == "KNNeighbour")
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
  
    if (types == "AllLabour")
    {
      
      #---------------------------------------------------------------------------
      # Open a pdf file
      #---------------------------------------------------------------------------
      pdf("KNearestNeighbour-AllLabour-Plot.pdf") 
      
      par(mfrow = c(2,1))
      
      AARate <- rateAA
      set.seed(128)
      split        = sample.split(mlsdf$IndAllLab, SplitRatio = 0.80)
      training_set = subset(mlsdf, split == TRUE)
      test_set     = subset(mlsdf, split == FALSE)
    
      training_set[-3] = scale(training_set[-3])
      test_set[-3]     = scale(test_set[-3])
    
      training_KNN   <- training_set
      test_KNN       <- test_set
    
      pred_KNN = knn(train = training_KNN[, -3], test  = test_KNN[, -3],
                   cl = training_KNN[, 3], k = 9, prob = TRUE)
      pred_KNN
    
      cm_KNN     = table(test_KNN[, 3], pred_KNN)
      cm_KNN
    
      results_KNN <- confusionMatrix(data=cm_KNN, reference=pred_KNN)
      print(results_KNN)
    
      cat("All Labour: Result of Predictor and Confusion Matrix Accuracy..............\n")
    
      #---------------------------------------------------------------------------
      # Visualising the KN Neighbour Training set results
      #---------------------------------------------------------------------------
      set = training_KNN
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabour', 'ChgAllLabour')
    
      y_grid = knn(train=training_KNN[, -3], test=grid_set, cl=training_KNN[, 3], k=5)
      
      plot(set[, -3],
         main = 'All Labour: On K-Nearest Neighbour Algorithm (Training)',
         sub = paste("Acceptable Annual Rate (%) of ",AARate),
         xlab = 'Change of All Labour Number (#)',
         ylab = 'Change of All Labour Rate (%)',
         xlim = range(X1),
         ylim = range(X2))
    
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

      cat("Progressing plotting contour points on Training sample.....................\n")
      
      #---------------------------------------------------------------------------
      # Visualising the KN Neighbour Testing set results
      #---------------------------------------------------------------------------
      set = test_KNN
    
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabour', 'ChgAllLabour')
    
      y_grid = knn(train=training_KNN[, -3], test=grid_set, cl=training_KNN[, 3], k=5)
    
      plot(set[, -3],
         main = 'All Labour: On K-Nearest Neighbour Algorithm (Testing)',
         sub = paste("Acceptable Annual Rate (%) of ",AARate),
         xlab = 'Change of All Labour Number (#)',
         ylab = 'Change of All Labour Rate (%)',
         xlim = range(X1),
         ylim = range(X2))
    
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

      cat("Progressing plotting contour points on Testing sample......................\n")
      cat("---------------------------------------------------------------------------\n\n\n")
    }
    if (types == "MLabour")
    {
      #---------------------------------------------------------------------------
      # Open a pdf file
      #---------------------------------------------------------------------------
      pdf("KNearestNeighbour-MaleLabour-Plot.pdf") 
      
      par(mfrow = c(2,1))
      
      AARate <- rateAA
      set.seed(128)
      split        = sample.split(mlsdf$IndMLab, SplitRatio = 0.80)
      training_set = subset(mlsdf, split == TRUE)
      test_set     = subset(mlsdf, split == FALSE)
      
      training_set[-3] = scale(training_set[-3])
      test_set[-3]     = scale(test_set[-3])
      
      training_KNN   <- training_set
      test_KNN       <- test_set
      
      pred_KNN = knn(train = training_KNN[, -3], test  = test_KNN[, -3],
                     cl = training_KNN[, 3], k = 9, prob = TRUE)
      pred_KNN
      
      cm_KNN     = table(test_KNN[, 3], pred_KNN)
      cm_KNN
      
      results_KNN <- confusionMatrix(data=cm_KNN, reference=pred_KNN)
      print(results_KNN)
      
      cat("Male Labour:Result of Predictor and Confusion Matrix Accuracy..............\n")
      
      #---------------------------------------------------------------------------
      # Visualising the KN Neighbour Training set results
      #---------------------------------------------------------------------------
      set = training_KNN
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourM', 'ChgMLabour')
      
      y_grid = knn(train=training_KNN[, -3], test=grid_set, cl=training_KNN[, 3], k=5)
      
      plot(set[, -3],
           main = 'Male Labour: On K-Nearest Neighbour Algorithm (Training)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of Male Labour Number (#)',
           ylab = 'Change of Male Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
 
      cat("Progressing plotting contour points on Training sample.....................\n")
      
      #---------------------------------------------------------------------------
      # Visualising the KN Neighbour Testing set results
      #---------------------------------------------------------------------------
      set = test_KNN
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourM', 'ChgMLabour')
      
      y_grid = knn(train=training_KNN[, -3], test=grid_set, cl=training_KNN[, 3], k=5)
      
      plot(set[, -3],
           main = 'Male Labour: On K-Nearest Neighbour Algorithm (Testing)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of Male Labour Number (#)',
           ylab = 'Change of Male Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Testing sample......................\n")
      cat("---------------------------------------------------------------------------\n\n\n")
    }
    if (types == "FLabour")
    {
      #---------------------------------------------------------------------------
      # Open a pdf file
      #---------------------------------------------------------------------------
      pdf("KNearestNeighbour-FemaleLabour-Plot.pdf") 
      
      par(mfrow = c(2,1))
      
      AARate <- rateAA
      set.seed(128)
      split        = sample.split(mlsdf$IndFLab, SplitRatio = 0.80)
      training_set = subset(mlsdf, split == TRUE)
      test_set     = subset(mlsdf, split == FALSE)
      
      training_set[-3] = scale(training_set[-3])
      test_set[-3]     = scale(test_set[-3])
      
      training_KNN   <- training_set
      test_KNN       <- test_set
      
      pred_KNN = knn(train = training_KNN[, -3], test  = test_KNN[, -3],
                     cl = training_KNN[, 3], k = 9, prob = TRUE)
      pred_KNN
      
      cm_KNN     = table(test_KNN[, 3], pred_KNN)
      cm_KNN
      
      results_KNN <- confusionMatrix(data=cm_KNN, reference=pred_KNN)
      print(results_KNN)
      
      cat("Female Labour:Result of Predictor and Confusion Matrix Accuracy............\n")
      
      #---------------------------------------------------------------------------
      # Visualising the KN Neighbour Training set results
      #---------------------------------------------------------------------------
      set = training_KNN
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourF', 'ChgFLabour')
      
      y_grid = knn(train=training_KNN[, -3], test=grid_set, cl=training_KNN[, 3], k=5)
      
      plot(set[, -3],
           main = 'Female Labour: On K-Nearest Neighbour Algorithm (Training)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of Female Labour Number (#)',
           ylab = 'Change of Female Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

      cat("Progressing plotting contour points on Training sample.....................\n")
      
      #---------------------------------------------------------------------------
      # Visualising the KN Neighbour Testing set results
      #---------------------------------------------------------------------------
      set = test_KNN
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourF', 'ChgFLabour')
      
      y_grid = knn(train=training_KNN[, -3], test=grid_set, cl=training_KNN[, 3], k=5)
      
      plot(set[, -3],
           main = 'Female Labour: On K-Nearest Neighbour Algorithm (Testing)',
           sub = paste("Acceptable Annual Rate (%) of ",AARate),
           xlab = 'Change of Female Labour Number (#)',
           ylab = 'Change of Female Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Testing sample......................\n")
      cat("---------------------------------------------------------------------------\n\n\n")
    }
  }
  if (algos == "RandomForest")
  {
   #----------------------------------------------------------------------------------
   #--------------------------    RANDOM FOREST ALGORITHM   --------------------------
   #----------------------------------------------------------------------------------
   # Fitting Random Forest Classification to the Training set and Predicting the Test 
   # set results each predictor variable.
   #   Packages     : install.packages('randomForest')
   #   Library use  : library(randomForest)
   #
   # 2. Set.seed that we used was based on 128 compare to others usual number of 123 due 
   #    mainly to get the different set of random number to be used for the generation
   #    of splitting the datatset.
   #------------------------------------------------------------------------------------

   if (types == "AllLabour")
    {
     #---------------------------------------------------------------------------
     # Open a pdf file
     #---------------------------------------------------------------------------
      pdf("RandonForest-AllLabour-Plot.pdf") 
        
      par(mfrow = c(2,1))
        
      AARate <- rateAA
      set.seed(128)
      split        = sample.split(mlsdf$IndAllLab, SplitRatio = 0.80)
      training_set = subset(mlsdf, split == TRUE)
      test_set     = subset(mlsdf, split == FALSE)
       
      training_set[-3] = scale(training_set[-3])
      test_set[-3]     = scale(test_set[-3])
       
      training_RF   <- training_set
      test_RF       <- test_set
      
      classifier_RF = randomForest(x=training_RF[-3],y=training_RF$IndAllLab,ntree=500)
      
      cat("All Labour: Result of Classifier Predictor.................................\n")

      print(classifier_RF)
        
      pred_RF = predict(classifier_RF, newdata = test_RF[-3])
      pred_RF
        
      cm_RF     = table(test_RF[, 3], pred_RF)
      cm_RF
        
      results_RF <- confusionMatrix(data=cm_RF, reference=pred_RF)
      print(results_RF)
        
      cat("All Labour: Result of Predictor and Confusion Matrix Accuracy..............\n")
        
      #---------------------------------------------------------------------------
      # Visualising the Random Forest Training set results
      #---------------------------------------------------------------------------
      set = training_RF
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabour', 'ChgAllLabour')
        
      y_grid = predict(classifier_RF, grid_set)
        
      plot(set[, -3],
         main = 'All Labour: On Random Forest Algorithm (Training)',
         xlab = 'Change of All Labour Number (#)',
         ylab = 'Change of All Labour Rate (%)',
         xlim = range(X1),
         ylim = range(X2))
        
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

      cat("Progressing plotting contour points on Training sample.....................\n")
      
      #---------------------------------------------------------------------------
      # Visualising the Random Forest Testing set results
      #---------------------------------------------------------------------------
      set = test_RF
        
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabour', 'ChgAllLabour')
        
      y_grid = predict(classifier_RF, grid_set)
      
      plot(set[, -3],
           main = 'All Labour: On On Random Forest Algorithm (Testing)',
           xlab = 'Change of All Labour Number (#)',
           ylab = 'Change of All Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
        
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Testing sample......................\n")
      cat("---------------------------------------------------------------------------\n\n\n")
      }
    if (types == "MLabour")
    {
      #---------------------------------------------------------------------------
      # Open a pdf file
      #---------------------------------------------------------------------------
      pdf("RandonForest-MaleLabour-Plot.pdf") 
        
      par(mfrow = c(2,1))
    
      AARate <- rateAA
      set.seed(128)
      split        = sample.split(mlsdf$IndMLab, SplitRatio = 0.80)
      training_set = subset(mlsdf, split == TRUE)
      test_set     = subset(mlsdf, split == FALSE)
        
      training_set[-3] = scale(training_set[-3])
      test_set[-3]     = scale(test_set[-3])
        
      training_RF   <- training_set
      test_RF       <- test_set
        
      classifier_RF = randomForest(x=training_RF[-3],y=training_RF$IndMLab,ntree=500)
      
      cat("All Labour: Result of Classifier Predictor.................................\n")

      print(classifier_RF)
      
      pred_RF = predict(classifier_RF, newdata=test_RF[-3])
      pred_RF
      
      cm_RF   = table(test_RF[, 3], pred_RF)
      cm_RF
      
      results_RF <- confusionMatrix(data=cm_RF, reference=pred_RF)
      print(results_RF)
        
      cat("Male Labour: Result of Predictor and Confusion Matrix Accuracy.............\n")
        
      #---------------------------------------------------------------------------
      # Visualising the Random Forest Training set results
      #---------------------------------------------------------------------------
      set = training_RF
      
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourM', 'ChgMLabour')
      
      y_grid = predict(classifier_RF, grid_set)
      
      plot(set[, -3],
           main = 'Male Labour: On Random Forest Algorithm (Training)',
           xlab = 'Change of Male Labour Number (#)',
           ylab = 'Change of Male Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Training sample.....................\n")
      
      #---------------------------------------------------------------------------
      # Visualising the Random Forest Testing set results
      #---------------------------------------------------------------------------
      set = test_RF
        
      X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
      X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
      grid_set = expand.grid(X1, X2)
      colnames(grid_set) = c('TotInsLabourM', 'ChgMLabour')
        
      y_grid = predict(classifier_RF, grid_set)
        
      plot(set[, -3],
           main = 'Male Labour: On Random Forest Algorithm (Testing)',
           xlab = 'Change of Male Labour Number (#)',
           ylab = 'Change of Male Labour Rate (%)',
           xlim = range(X1),
           ylim = range(X2))
      
      contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
      points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
      points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
      
      cat("Progressing plotting contour points on Testing sample......................\n")
      cat("---------------------------------------------------------------------------\n\n\n")
    }
  if (types == "FLabour")
    {
     #---------------------------------------------------------------------------
     # Open a pdf file
     #---------------------------------------------------------------------------
     pdf("RandonForest-FemaleLabour-Plot.pdf") 
        
     par(mfrow = c(2,1))
        
     AARate <- rateAA
     set.seed(128)
     split        = sample.split(mlsdf$IndFLab, SplitRatio = 0.80)
     training_set = subset(mlsdf, split == TRUE)
     test_set     = subset(mlsdf, split == FALSE)
        
     training_set[-3] = scale(training_set[-3])
     test_set[-3]     = scale(test_set[-3])
     
     training_RF   <- training_set
     test_RF       <- test_set
      
     classifier_RF = randomForest(x=training_RF[-3],y=training_RF$IndFLab,ntree=500)
     
     cat("All Labour: Result of Classifier Predictor.................................\n")

     print(classifier_RF)
        
     pred_RF = predict(classifier_RF, newdata = test_RF[-3])
     pred_RF
        
     cm_RF     = table(test_RF[, 3], pred_RF)
     cm_RF
        
     results_RF <- confusionMatrix(data=cm_RF, reference=pred_RF)
     print(results_RF)
        
     cat("All Labour: Result of Predictor and Confusion Matrix Accuracy..............\n")
        
     #---------------------------------------------------------------------------
     # Visualising the Random Forest Training set results
     #---------------------------------------------------------------------------
     set = training_RF
        
     X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
     X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
     grid_set = expand.grid(X1, X2)
     colnames(grid_set) = c('TotInsLabourF', 'ChgFLabour')
        
     y_grid = predict(classifier_RF, grid_set)
        
     plot(set[, -3],
          main = 'Female Labour: On Random Forest Algorithm (Training)',
          xlab = 'Change of Female Labour Number (#)',
          ylab = 'Change of Female Labour Rate (%)',
          xlim = range(X1),
          ylim = range(X2))
        
     contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
     points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
     points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
        
     cat("Progressing plotting contour points on Training sample.....................\n")
     
     #---------------------------------------------------------------------------
     # Visualising the Random Forest Testing set results
     #---------------------------------------------------------------------------
     set = test_RF
        
     X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
     X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
     grid_set = expand.grid(X1, X2)
     colnames(grid_set) = c('TotInsLabourF', 'ChgFLabour')
        
     y_grid = predict(classifier_RF, grid_set)
       
     plot(set[, -3],
          main = 'Female Labour: On Random Forest Algorithm (Testing)',
          xlab = 'Change of Female Labour Number (#)',
          ylab = 'Change of Female Labour Rate (%)',
          xlim = range(X1),
          ylim = range(X2))
        
     contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
     points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
     points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
        
     cat("Progressing plotting contour points on Testing sample......................\n")
     cat("---------------------------------------------------------------------------\n\n\n")
    }
  }
  #---------------------------------------------------------------------------
  # Close the pdf file and set back to Program directory
  #---------------------------------------------------------------------------
  dev.off()
  setwd(DirProgram)
  par(mfrow = c(2,1))
}

  