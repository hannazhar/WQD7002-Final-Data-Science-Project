DataCleaning <- function(types) 
{
  #-------------------------------------------------------------------------------------
  #                               dsLabourDetail Dataset
  #-------------------------------------------------------------------------------------
  if (types=="dsLabourDetail")
  {
    i <- 0
    n <- nrow(dsLabourDetail)
    for (i in 1:n)
    {
      dsLabourDetail$TotInsLabour[i]            = ifelse(is.na(dsLabourDetail$TotInsLabour[i]),          dsLabourDetail$TotInsLabour[i]<-0,           dsLabourDetail$TotInsLabour[i])
      dsLabourDetail$InsLabEmp[i]               = ifelse(is.na(dsLabourDetail$InsLabEmp[i]),             dsLabourDetail$InsLabEmp[i]<-0,              dsLabourDetail$InsLabEmp[i])
      dsLabourDetail$InsLabUnemp[i]             = ifelse(is.na(dsLabourDetail$InsLabUnemp[i]),           dsLabourDetail$InsLabUnemp[i]<-0,            dsLabourDetail$InsLabUnemp[i])
      dsLabourDetail$TotOutLabour[i]            = ifelse(is.na(dsLabourDetail$TotOutLabour[i]),          dsLabourDetail$TotOutLabour[i]<-0,           dsLabourDetail$TotOutLabour[i])
      dsLabourDetail$TotInsLabourM[i]           = ifelse(is.na(dsLabourDetail$TotInsLabourM[i]),         dsLabourDetail$TotInsLabourM[i]<-0,          dsLabourDetail$TotInsLabourM[i])
      dsLabourDetail$InsLabEmpM[i]              = ifelse(is.na(dsLabourDetail$InsLabEmpM[i]),            dsLabourDetail$InsLabEmpM[i]<-0,             dsLabourDetail$InsLabEmpM[i])
      dsLabourDetail$InsLabUnempM[i]            = ifelse(is.na(dsLabourDetail$InsLabUnempM[i]),          dsLabourDetail$InsLabUnempM[i]<-0,           dsLabourDetail$InsLabUnempM[i])
      dsLabourDetail$TotOutLabourM[i]           = ifelse(is.na(dsLabourDetail$TotOutLabourM[i]),         dsLabourDetail$TotOutLabourM[i]<-0,          dsLabourDetail$TotOutLabourM[i])
      dsLabourDetail$TotInsLabourF[i]           = ifelse(is.na(dsLabourDetail$TotInsLabourF[i]),         dsLabourDetail$TotInsLabourF[i]<-0,          dsLabourDetail$TotInsLabourF[i])
      dsLabourDetail$InsLabEmpF[i]              = ifelse(is.na(dsLabourDetail$InsLabEmpF[i]),            dsLabourDetail$InsLabEmpF[i]<-0,             dsLabourDetail$InsLabEmpF[i])
      dsLabourDetail$InsLabUnempF[i]            = ifelse(is.na(dsLabourDetail$InsLabUnempF[i]),          dsLabourDetail$InsLabUnempF[i]<-0,           dsLabourDetail$InsLabUnempF[i])
      dsLabourDetail$TotOutLabourF[i]           = ifelse(is.na(dsLabourDetail$TotOutLabourF[i]),         dsLabourDetail$TotOutLabourF[i]<-0,          dsLabourDetail$TotOutLabourF[i])
      dsLabourDetail$AllTotalWorkForce[i]       = ifelse(is.na(dsLabourDetail$AllTotalWorkForce[i]),     dsLabourDetail$AllTotalWorkForce[i]<-0,      dsLabourDetail$AllTotalWorkForce[i])
      dsLabourDetail$SubTotWorkForce[i]         = ifelse(is.na(dsLabourDetail$SubTotWorkForce[i]),       dsLabourDetail$SubTotWorkForce[i]<-0,        dsLabourDetail$SubTotWorkForce[i])
      dsLabourDetail$BumiWorkForce[i]           = ifelse(is.na(dsLabourDetail$BumiWorkForce[i]),         dsLabourDetail$BumiWorkForce[i]<-0,          dsLabourDetail$BumiWorkForce[i])
      dsLabourDetail$ChineseWorkForce[i]        = ifelse(is.na(dsLabourDetail$ChineseWorkForce[i]),      dsLabourDetail$ChineseWorkForce[i]<-0,       dsLabourDetail$ChineseWorkForce[i])
      dsLabourDetail$IndianWorkForce[i]         = ifelse(is.na(dsLabourDetail$IndianWorkForce[i]),       dsLabourDetail$IndianWorkForce[i]<-0,        dsLabourDetail$IndianWorkForce[i])
      dsLabourDetail$OtherWorkForce[i]          = ifelse(is.na(dsLabourDetail$OtherWorkForce[i]),        dsLabourDetail$OtherWorkForce[i]<-0,         dsLabourDetail$OtherWorkForce[i])
      dsLabourDetail$SubTotForeignWorkForce[i]  = ifelse(is.na(dsLabourDetail$SubTotForeignWorkForce[i]),dsLabourDetail$SubTotForeignWorkForce[i]<-0, dsLabourDetail$SubTotForeignWorkForce[i])
    }
    dsLabourDetail <<- dsLabourDetail
    return(paste("Data Cleaning Proses for Dataset (dsLabourDetail) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsLabourArea Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsLabourArea")
  {
    j <- 0
    m <- nrow(dsLabourArea)
    for (j in 1:m)
    {
      dsLabourArea$AllLabourInsR[j]   = ifelse(is.na(dsLabourArea$AllLabourInsR[j]),  dsLabourArea$AllLabourInsR[j]<-0, dsLabourArea$AllLabourInsR[j])
      dsLabourArea$InsEmpR[j]         = ifelse(is.na(dsLabourArea$InsEmpR[j]),        dsLabourArea$InsEmpR[j]<-0,       dsLabourArea$InsEmpR[j])
      dsLabourArea$InsUnempR[j]       = ifelse(is.na(dsLabourArea$InsUnempR[j]),      dsLabourArea$InsUnempR[j]<-0,     dsLabourArea$InsUnempR[j])
      dsLabourArea$AllLabourOutR[j]   = ifelse(is.na(dsLabourArea$AllLabourOutR[j]),  dsLabourArea$AllLabourOutR[j]<-0, dsLabourArea$AllLabourOutR[j])
      dsLabourArea$AllLabourInsU[j]   = ifelse(is.na(dsLabourArea$AllLabourInsU[j]),  dsLabourArea$AllLabourInsU[j]<-0, dsLabourArea$AllLabourInsU[j])
      dsLabourArea$InsEmpU[j]         = ifelse(is.na(dsLabourArea$InsEmpU[j]),        dsLabourArea$InsEmpU[j]<-0,       dsLabourArea$InsEmpU[j])
      dsLabourArea$InsUnempU[j]       = ifelse(is.na(dsLabourArea$InsUnempU[j]),      dsLabourArea$InsUnempU[j]<-0,     dsLabourArea$InsUnempU[j])
      dsLabourArea$AllLabourOutU[j]   = ifelse(is.na(dsLabourArea$AllLabourOutU[j]),  dsLabourArea$AllLabourOutU[j]<-0, dsLabourArea$AllLabourOutU[j])
    }
    dsLabourArea <<- dsLabourArea
    return(paste("Data Cleaning Proses for Dataset (dsLabourArea) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsLabourEducation Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsLabourEducation")
  {
    k <- 0
    o <- nrow(dsLabourEducation)
    for (k in 1:o)
    {
      dsLabourEducation$AllTotalEdu[k]    = ifelse(is.na(dsLabourEducation$AllTotalEdu[k]),    dsLabourEducation$AllTotalEdu[k]<-0,    dsLabourEducation$AllTotalEdu[k])
      dsLabourEducation$NonEduc[k]        = ifelse(is.na(dsLabourEducation$NonEduc[k]),        dsLabourEducation$NonEduc[k]<-0,        dsLabourEducation$NonEduc[k])
      dsLabourEducation$Primary[k]        = ifelse(is.na(dsLabourEducation$Primary[k]),        dsLabourEducation$Primary[k]<-0,        dsLabourEducation$Primary[k])
      dsLabourEducation$Secondary[k]      = ifelse(is.na(dsLabourEducation$Secondary[k]),      dsLabourEducation$Secondary[k]<-0,      dsLabourEducation$Secondary[k])
      dsLabourEducation$Tertiary[k]       = ifelse(is.na(dsLabourEducation$Tertiary[k]),       dsLabourEducation$Tertiary[k]<-0,       dsLabourEducation$Tertiary[k])
      dsLabourEducation$AllTotalCert[k]   = ifelse(is.na(dsLabourEducation$AllTotalCert[k]),   dsLabourEducation$AllTotalCert[k]<-0,   dsLabourEducation$AllTotalCert[k])
      dsLabourEducation$UPSRAEquiv[k]     = ifelse(is.na(dsLabourEducation$UPSRAEquiv[k]),     dsLabourEducation$UPSRAEquiv[k]<-0,     dsLabourEducation$UPSRAEquiv[k])
      dsLabourEducation$PMRSRPLCEEquiv[k] = ifelse(is.na(dsLabourEducation$PMRSRPLCEEquiv[k]), dsLabourEducation$PMRSRPLCEEquiv[k]<-0, dsLabourEducation$PMRSRPLCEEquiv[k])
      dsLabourEducation$SPMEquiv[k]       = ifelse(is.na(dsLabourEducation$SPMEquiv[k]),       dsLabourEducation$SPMEquiv[k]<-0,       dsLabourEducation$SPMEquiv[k])
      dsLabourEducation$STPMEquiv[k]      = ifelse(is.na(dsLabourEducation$STPMEquiv[k]),      dsLabourEducation$STPMEquiv[k]<-0,      dsLabourEducation$STPMEquiv[k])
      dsLabourEducation$Certificate[k]    = ifelse(is.na(dsLabourEducation$Certificate[k]),    dsLabourEducation$Certificate[k]<-0,    dsLabourEducation$Certificate[k])
      dsLabourEducation$Diploma[k]        = ifelse(is.na(dsLabourEducation$Diploma[k]),        dsLabourEducation$Diploma[k]<-0,        dsLabourEducation$Diploma[k])
      dsLabourEducation$Degree[k]         = ifelse(is.na(dsLabourEducation$Degree[k]),         dsLabourEducation$Degree[k]<-0,         dsLabourEducation$Degree[k])
      dsLabourEducation$ReligCert[k]      = ifelse(is.na(dsLabourEducation$ReligCert[k]),      dsLabourEducation$ReligCert[k]<-0,      dsLabourEducation$ReligCert[k])
      dsLabourEducation$NoCert[k]         = ifelse(is.na(dsLabourEducation$NoCert[k]),         dsLabourEducation$NoCert[k]<-0,         dsLabourEducation$NoCert[k])
      dsLabourEducation$NoRelevant[k]     = ifelse(is.na(dsLabourEducation$NoRelevant[k]),     dsLabourEducation$NoRelevant[k]<-0,     dsLabourEducation$NoRelevant[k])
    }
    dsLabourEducation <<- dsLabourEducation 
    return(paste("Data Cleaning Proses for Dataset (dsLabourEducation) has been completed"))
  }
  #-------------------------------------------------------------------------------------
  #                               dsLabourMaritalAge Dataset
  #-------------------------------------------------------------------------------------
  else if (types=="dsLabourMaritalAge")
  {
    l <- 0
    p <- nrow(dsLabourMaritalAge)
    for (l in 1:p)
    {
      dsLabourMaritalAge$AllTotalMarital[l]     = ifelse(is.na(dsLabourMaritalAge$AllTotalMarital[l]),     dsLabourMaritalAge$AllTotalMarital[l]<-0,     dsLabourMaritalAge$AllTotalMarital[l])
      dsLabourMaritalAge$NeverMarried[l]        = ifelse(is.na(dsLabourMaritalAge$NeverMarried[l]),        dsLabourMaritalAge$NeverMarried[l]<-0,        dsLabourMaritalAge$NeverMarried[l])
      dsLabourMaritalAge$Married[l]             = ifelse(is.na(dsLabourMaritalAge$Married[l]),             dsLabourMaritalAge$Married[l]<-0,             dsLabourMaritalAge$Married[l])
      dsLabourMaritalAge$Widow[l]               = ifelse(is.na(dsLabourMaritalAge$Widow[l]),               dsLabourMaritalAge$Widow[l]<-0,               dsLabourMaritalAge$Widow[l])
      dsLabourMaritalAge$DivorcePermSeparate[l] = ifelse(is.na(dsLabourMaritalAge$DivorcePermSeparate[l]), dsLabourMaritalAge$DivorcePermSeparate[l]<-0, dsLabourMaritalAge$DivorcePermSeparate[l])
      dsLabourMaritalAge$AllTotalAge[l]         = ifelse(is.na(dsLabourMaritalAge$AllTotalAge[l]),         dsLabourMaritalAge$AllTotalAge[l]<-0,         dsLabourMaritalAge$AllTotalAge[l])
      dsLabourMaritalAge$Age15to19[l]           = ifelse(is.na(dsLabourMaritalAge$Age15to19[l]),           dsLabourMaritalAge$Age15to19[l]<-0,           dsLabourMaritalAge$Age15to19[l])
      dsLabourMaritalAge$Age20to24[l]           = ifelse(is.na(dsLabourMaritalAge$Age20to24[l]),           dsLabourMaritalAge$Age20to24[l]<-0,           dsLabourMaritalAge$Age20to24[l])
      dsLabourMaritalAge$Age25to29[l]           = ifelse(is.na(dsLabourMaritalAge$Age25to29[l]),           dsLabourMaritalAge$Age25to29[l]<-0,           dsLabourMaritalAge$Age25to29[l])
      dsLabourMaritalAge$Age30to34[l]           = ifelse(is.na(dsLabourMaritalAge$Age30to34[l]),           dsLabourMaritalAge$Age30to34[l]<-0,           dsLabourMaritalAge$Age30to34[l])
      dsLabourMaritalAge$Age35to39[l]           = ifelse(is.na(dsLabourMaritalAge$Age35to39[l]),           dsLabourMaritalAge$Age35to39[l]<-0,           dsLabourMaritalAge$Age35to39[l])
      dsLabourMaritalAge$Age40to44[l]           = ifelse(is.na(dsLabourMaritalAge$Age40to44[l]),           dsLabourMaritalAge$Age40to44[l]<-0,           dsLabourMaritalAge$Age40to44[l])
      dsLabourMaritalAge$Age45to49[l]           = ifelse(is.na(dsLabourMaritalAge$Age45to49[l]),           dsLabourMaritalAge$Age45to49[l]<-0,           dsLabourMaritalAge$Age45to49[l])
      dsLabourMaritalAge$Age50to54[l]           = ifelse(is.na(dsLabourMaritalAge$Age50to54[l]),           dsLabourMaritalAge$Age50to54[l]<-0,           dsLabourMaritalAge$Age50to54[l])
      dsLabourMaritalAge$Age55to59[l]           = ifelse(is.na(dsLabourMaritalAge$Age55to59[l]),           dsLabourMaritalAge$Age55to59[l]<-0,           dsLabourMaritalAge$Age55to59[l])
      dsLabourMaritalAge$Age60to64[l]           = ifelse(is.na(dsLabourMaritalAge$Age60to64[l]),           dsLabourMaritalAge$Age60to64[l]<-0,           dsLabourMaritalAge$Age60to64[l])
    }
    dsLabourMaritalAge <<- dsLabourMaritalAge 
    return(paste("Data Cleaning Proses for Dataset (dsLabourMaritalAge) has been completed"))
  }
  else
  {
    return(paste("NO Dataset Cleaning Proses Executed"))
  }
}

