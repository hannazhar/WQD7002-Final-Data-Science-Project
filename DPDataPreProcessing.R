DataPreProcessing <- function(types) 
{
  #-------------------------------------------------------------------------------------
  #                               dsRate Dataset
  #-------------------------------------------------------------------------------------
  
  if (types=="dsRate")
  {
    dsRate$Year              = ifelse(is.na(dsRate$Year), ave(dsRate$Year,
                             FUN=function(x) mean(x), na.rm=TRUE), dsRate$Year)
    
    dsRate$TotInsLabour      = ifelse(is.na(dsRate$TotInsLabour), ave(dsRate$TotInsLabour,
                             FUN=function(x) mean(x), na.rm=TRUE), dsRate$TotInsLabour)
    
    dsRate$InsLabEmp         = ifelse(is.na(dsRate$InsLabEmp), ave(dsRate$InsLabEmp,
                             FUN=function(x) mean(x), na.rm=TRUE), dsRate$InsLabEmp)
  
    dsRate$InsLabUnemp       = ifelse(is.na(dsRate$InsLabUnemp), ave(dsRate$InsLabUnempp,
                             FUN=function(x) mean(x), na.rm=TRUE), dsRate$InsLabUnemp)
  
    dsRate$TotOutLabour      = ifelse(is.na(dsRate$TotOutLabour), ave(dsRate$TotOutLabour,
                             FUN=function(x) mean(x), na.rm=TRUE), dsRate$TotOutLabour)
  
    dsRate$RateLabEmp        = ifelse(is.na(dsRate$RateLabEmp), ave(dsRate$RateLabEmp,
                             FUN=function(x) mean(x), na.rm=TRUE), dsRate$RateLabEmp)
  
    dsRate$RateLabUnEmp      = ifelse(is.na(dsRate$RateLabUnEmp), ave(dsRate$RateLabUnEmp,
                             FUN=function(x) mean(x), na.rm=TRUE), dsRate$RateLabUnEmp)
   
    return(paste("Data Cleaning Proses for Dataset (dsRate) has been completed"))
  }
  
  #-------------------------------------------------------------------------------------
  #                               dsGender Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsGender")
  {
    dsGender$Year            = ifelse(is.na(dsGender$Year), ave(dsGender$Year,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$Year)

    dsGender$TotInsLabourM   = ifelse(is.na(dsGender$TotInsLabourM), ave(dsGender$TotInsLabourM,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$TotInsLabourM)

    dsGender$InsLabEmpM      = ifelse(is.na(dsGender$InsLabEmpM), ave(dsGender$InsLabEmpM,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$InsLabEmpM)

    dsGender$InsLabUnempM    = ifelse(is.na(dsGender$InsLabUnempM), ave(dsGender$InsLabUnempM,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$InsLabUnempM)

    dsGender$TotOutLabourM   = ifelse(is.na(dsGender$TotOutLabourM), ave(dsGender$TotOutLabourM,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$TotOutLabourM)

    dsGender$RateLabEmpM     = ifelse(is.na(dsGender$RateLabEmpM), ave(dsGender$RateLabEmpM,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$RateLabEmpM)

    dsGender$RateLabUnEmpM   = ifelse(is.na(dsGender$RateLabUnEmpM), ave(dsGender$RateLabUnEmpM,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$RateLabUnEmpM)

    dsGender$TotInsLabourF   = ifelse(is.na(dsGender$TotInsLabourF), ave(dsGender$TotInsLabourF,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$TotInsLabourF)

    dsGender$InsLabEmpF      = ifelse(is.na(dsGender$InsLabEmpF), ave(dsGender$InsLabEmpFr,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$InsLabEmpF)

    dsGender$InsLabUnempF    = ifelse(is.na(dsGender$InsLabUnempF), ave(dsGender$InsLabUnempF,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$InsLabUnempF)

    dsGender$TotOutLabourF   = ifelse(is.na(dsGender$TotOutLabourF), ave(dsGender$TotOutLabourF,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$TotOutLabourF)

    dsGender$RateLabEmpF     = ifelse(is.na(dsGender$RateLabEmpF), ave(dsGender$RateLabEmpF,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$RateLabEmpF)

    dsGender$RateLabUnEmpF   = ifelse(is.na(dsGender$RateLabUnEmpF), ave(dsGender$RateLabUnEmpF,
                               FUN=function(x) mean(x), na.rm=TRUE), dsGender$RateLabUnEmpF)
  
    return(paste("Data Cleaning Proses for Dataset (dsGender) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsRural Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsRural")
  {
    dsRural$Year              = ifelse(is.na(dsRural$Year), ave(dsRural$Year,
                                FUN=function(x) mean(x), na.rm=TRUE), dsRural$Year)
    
    dsRural$AllLabourInsR     = ifelse(is.na(dsRural$AllLabourInsR), ave(dsRural$AllLabourInsR,
                                FUN=function(x) mean(x), na.rm=TRUE), dsRural$AllLabourInsR)
    
    dsRural$InsEmpR           = ifelse(is.na(dsRural$InsEmpR), ave(dsRural$InsEmpR,
                                FUN=function(x) mean(x), na.rm=TRUE), dsRural$InsEmpR)
    
    dsRural$InsUnempR         = ifelse(is.na(dsRural$InsUnempR), ave(dsRural$InsUnempR,
                                FUN=function(x) mean(x), na.rm=TRUE), dsRural$InsUnempR)
    
    dsRural$AllLabourOutR     = ifelse(is.na(dsRural$AllLabourOutR), ave(dsRural$AllLabourOutR,
                                FUN=function(x) mean(x), na.rm=TRUE), dsRural$AllLabourOutR)
    
    dsRural$LabForceRateR     = ifelse(is.na(dsRural$LabForceRateR), ave(dsRural$LabForceRateR,
                                FUN=function(x) mean(x), na.rm=TRUE), dsRural$LabForceRateR)
    
    dsRural$LabForceUempRateR = ifelse(is.na(dsRural$LabForceUempRateR), ave(dsRural$LabForceUempRateR,
                                FUN=function(x) mean(x), na.rm=TRUE), dsRural$LabForceUempRateR)
    
    
    return(paste("Data Cleaning Proses for Dataset (dsRural) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsUrban Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsUrban")
  {
    dsUrban$Year              = ifelse(is.na(dsUrban$Year), ave(dsUrban$Year,
                                FUN=function(x) mean(x), na.rm=TRUE), dsUrban$Year)
    
    dsUrban$AllLabourInsU     = ifelse(is.na(dsUrban$AllLabourInsU), ave(dsUrban$AllLabourInsU,
                                FUN=function(x) mean(x), na.rm=TRUE), dsUrban$AllLabourInsU)
    
    dsUrban$InsEmpU           = ifelse(is.na(dsUrban$InsEmpU), ave(dsUrban$InsEmpU,
                                FUN=function(x) mean(x), na.rm=TRUE), dsUrban$InsEmpU)
    
    dsUrban$InsUnempU         = ifelse(is.na(dsUrban$InsUnempU), ave(dsUrban$InsUnempU,
                                FUN=function(x) mean(x), na.rm=TRUE), dsUrban$InsUnempU)
    
    dsUrban$AllLabourOutU     = ifelse(is.na(dsUrban$AllLabourOutU), ave(dsUrban$AllLabourOutU,
                                FUN=function(x) mean(x), na.rm=TRUE), dsUrban$AllLabourOutU)
    
    dsUrban$LabForceRateU     = ifelse(is.na(dsUrban$LabForceRateU), ave(dsUrban$LabForceRateU,
                                FUN=function(x) mean(x), na.rm=TRUE), dsUrban$LabForceRateU)
    
    dsUrban$LabForceUempRateU = ifelse(is.na(dsUrban$LabForceUempRateU), ave(dsUrban$LabForceUempRateU,
                                FUN=function(x) mean(x), na.rm=TRUE), dsUrban$LabForceUempRateU)
  
    return(paste("Data Cleaning Proses for Dataset (dsUrban) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsAge Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsAge")
  {
    dsAge$Year              = ifelse(is.na(dsAge$Year), ave(dsAge$Year,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Year)
    
    dsAge$AllTotalAge       = ifelse(is.na(dsAge$AllTotalAge), ave(dsAge$AllTotalAge,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$AllTotalAge)
    
    dsAge$Age15to19         = ifelse(is.na(dsAge$Age15to19), ave(dsAge$Age15to19,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age15to19)
    
    dsAge$Age20to24         = ifelse(is.na(dsAge$Age20to24), ave(dsAge$Age20to24,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age20to24)
    
    dsAge$Age25to29         = ifelse(is.na(dsAge$Age25to29), ave(dsAge$Age25to29,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age25to29)
    
    dsAge$Age30to34         = ifelse(is.na(dsAge$Age30to34), ave(dsAge$Age30to34,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age30to34)
    
    dsAge$Age35to39         = ifelse(is.na(dsAge$Age35to39), ave(dsAge$Age35to39,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age35to39)
    
    dsAge$Age40to44         = ifelse(is.na(dsAge$Age40to44), ave(dsAge$Age40to44,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age40to44)
    
    dsAge$Age45_49          = ifelse(is.na(dsAge$Age45to49), ave(dsAge$Age45to49,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age45to49)
    
    dsAge$Age50to54         = ifelse(is.na(dsAge$Age50to54), ave(dsAge$Age50to54,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age50to54)
    
    dsAge$Age55to59         = ifelse(is.na(dsAge$Age55to59), ave(dsAge$Age55to59,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age55to59)
    
    dsAge$Age60to64         = ifelse(is.na(dsAge$Age60to64), ave(dsAge$Age60to64,
                              FUN=function(x) mean(x), na.rm=TRUE), dsAge$Age60to64)
    
    return(paste("Data Cleaning Proses for Dataset (dsAge) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsEthnic Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsEthnic")
  {
    dsEthnic$Year                   = ifelse(is.na(dsEthnic$Year), ave(dsEthnic$Year,
                                      FUN=function(x) mean(x), na.rm=TRUE), dsEthnic$Year)
    
    dsEthnic$AllTotalWorkForce      = ifelse(is.na(dsEthnic$AllTotalWorkForce), ave(dsEthnic$AllTotalWorkForce,
                                      FUN=function(x) mean(x), na.rm=TRUE), dsEthnic$AllTotalWorkForce)
    
    dsEthnic$SubTotWorkForce        = ifelse(is.na(dsEthnic$SubTotWorkForce), ave(dsEthnic$SubTotWorkForce,
                                      FUN=function(x) mean(x), na.rm=TRUE), dsEthnic$SubTotWorkForce)
    
    dsEthnic$BumiWorkForce          = ifelse(is.na(dsEthnic$BumiWorkForce), ave(dsEthnic$BumiWorkForce,
                                      FUN=function(x) mean(x), na.rm=TRUE), dsEthnic$BumiWorkForce)
    
    dsEthnic$ChineseWorkForce       = ifelse(is.na(dsEthnic$ChineseWorkForce), ave(dsEthnic$ChineseWorkForce,
                                      FUN=function(x) mean(x), na.rm=TRUE), dsEthnic$ChineseWorkForce)
    
    dsEthnic$IndianWorkForce        = ifelse(is.na(dsEthnic$IndianWorkForce), ave(dsEthnic$IndianWorkForce,
                                      FUN=function(x) mean(x), na.rm=TRUE), dsEthnic$IndianWorkForce)
    
    dsEthnic$OtherWorkForce         = ifelse(is.na(dsEthnic$OtherWorkForce), ave(dsEthnic$OtherWorkForce,
                                      FUN=function(x) mean(x), na.rm=TRUE), dsEthnic$OtherWorkForce)
    
    dsEthnic$SubTotForeignWorkForce = ifelse(is.na(dsEthnic$SubTotForeignWorkForce), ave(dsEthnic$SubTotForeignWorkForce,
                                      FUN=function(x) mean(x), na.rm=TRUE), dsEthnic$SubTotForeignWorkForce)
    
    return(paste("Data Cleaning Proses for Dataset (dsEthnic) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsEdu Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsEdu")
  {
    dsEdu$Year                   = ifelse(is.na(dsEdu$Year), ave(dsEdu$Year,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsEdu$Year)
    
    dsEdu$AllTotalEdu            = ifelse(is.na(dsEdu$AllTotalEdu), ave(dsEdu$AllTotalEdu,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsEdu$AllTotalEdu)
    
    dsEdu$NonEduc                = ifelse(is.na(dsEdu$NonEduc), ave(dsEdu$NonEduc,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsEdu$NonEduc)
    
    dsEdu$Primary                = ifelse(is.na(dsEdu$Primary), ave(dsEdu$Primary,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsEdu$Primary)
    
    dsEdu$Secondary              = ifelse(is.na(dsEdu$Secondary), ave(dsEdu$Secondary,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsEdu$Secondary)
    
    dsEdu$Tertiary               = ifelse(is.na(dsEdu$Tertiary), ave(dsEdu$Tertiary,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsEdu$Tertiary)
  
    return(paste("Data Cleaning Proses for Dataset (dsEdu) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsCert Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsCert")
  {
    dsCert$Year                  = ifelse(is.na(dsCert$Year), ave(dsCert$Year,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$Year)
    
    dsCert$AllTotalCert          = ifelse(is.na(dsCert$AllTotalCert), ave(dsCert$AllTotalCert,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$AllTotalCert)
    
    dsCert$UPSRAEquiv            = ifelse(is.na(dsCert$UPSRAEquiv), ave(dsCert$UPSRAEquiv,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$UPSRAEquiv)
  
    dsCert$PMRSRPLCEEquiv        = ifelse(is.na(dsCert$PMRSRPLCEEquiv), ave(dsCert$PMRSRPLCEEquiv,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$PMRSRPLCEEquiv)
    
    dsCert$SPMEquiv              = ifelse(is.na(dsCert$SPMEquiv), ave(dsCert$SPMEquiv,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$SPMEquiv)
    
    dsCert$STPMEquiv             = ifelse(is.na(dsCert$STPMEquiv), ave(dsCert$STPMEquiv,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$STPMEquiv)
    
    dsCert$Certificate           = ifelse(is.na(dsCert$Certificate), ave(dsCert$Certificate,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$Certificate)
    
    dsCert$Diploma               = ifelse(is.na(dsCert$Diploma), ave(dsCert$Diploma,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$Diploma)
    
    dsCert$Degree                = ifelse(is.na(dsCert$Degree), ave(dsCert$Degree,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$Degree)
    
    dsCert$ReligCert             = ifelse(is.na(dsCert$ReligCert), ave(dsCert$ReligCert,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$ReligCert)
    
    dsCert$NoCert                = ifelse(is.na(dsCert$NoCert), ave(dsCert$NoCert,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$NoCert)
    
    dsCert$NoRelevant            = ifelse(is.na(dsCert$NoRelevant), ave(dsCert$NoRelevant,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsCert$NoRelevant)
    
    return(paste("Data Cleaning Proses for Dataset (dsCert) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsMarital Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsMarital")
  {
    dsMarital$AllTotalMarital    = ifelse(is.na(dsMarital$AllTotalMarital), ave(dsMarital$AllTotalMarital,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsMarital$AllTotalMarital)
    
    dsMarital$Year               = ifelse(is.na(dsMarital$Year), ave(dsMarital$Year,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsMarital$Year)
    
    dsMarital$NeverMarried       = ifelse(is.na(dsMarital$NeverMarried), ave(dsMarital$NeverMarried,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsMarital$NeverMarried)
    
    dsMarital$Married            = ifelse(is.na(dsMarital$Married), ave(dsMarital$Married,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsMarital$Married)
    
    dsMarital$Widow              = ifelse(is.na(dsMarital$Widow), ave(dsMarital$Widow,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsMarital$Widow)
    
    dsMarital$DivorcePermSeparate= ifelse(is.na(dsMarital$DivorcePermSeparate), ave(dsMarital$DivorcePermSeparate,
                                   FUN=function(x) mean(x), na.rm=TRUE), dsMarital$DivorcePermSeparate)
    
    return(paste("Data Cleaning Proses for Dataset (dsMarital) has been completed"))
  }
  else
  {
    return(paste("NO Data Cleaning Proses Executed"))
  }
}

