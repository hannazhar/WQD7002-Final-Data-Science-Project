BarPlotDetail <- function(datafile)
{
  setwd(DirVisual)
  par(mfrow = c(2,1))
  sequence_set <- 1
  while(sequence_set > 0) 
  {
    cat("\014")
    cat("\n\n")
    cat("----------------------------------------------------------------------\n\n")
    cat("           State & Malaysia Performance (Distribution)                \n")
    cat("                 Labour Force Details Selection                       \n")
    cat("               (Time Series Data from 1982-2017)                      \n\n")
    cat("----------------------------------------------------------------------\n\n")   
    cat("            1. Johor             2. Kedah        \n")
    cat("            3. Kelantan          4. Melaka       \n")
    cat("            5. N. Sembilan       6. P. Pinang    \n")
    cat("            7. Pahang            8. Perak        \n")
    cat("            9. Perlis           10. Sabah        \n")
    cat("           11. Sarawak          12. Selangor     \n")
    cat("           13. Terengganu       14. Kuala Lumpur \n")
    cat("           15. Labuan           16. Putrajaya    \n\n")
    cat("           17. Malaysia (Overall)                \n")
    cat("           18. Exit                              \n\n")
    vstate <- readline(prompt="       Please enter your number: ")
    vstate <- as.numeric(vstate)
    
    if (vstate==1)  { state <- "Johor"}
    if (vstate==2)  { state <- "Kedah"}
    if (vstate==3)  { state <- "Kelantan"}
    if (vstate==4)  { state <- "Melaka"}
    if (vstate==5)  { state <- "N. Sembilan"}
    if (vstate==6)  { state <- "P. Pinang"}
    if (vstate==7)  { state <- "Pahang"}
    if (vstate==8)  { state <- "Perak"}
    if (vstate==9)  { state <- "Perlis"}
    if (vstate==10) { state <- "Sabah"}
    if (vstate==11) { state <- "Sarawak"}
    if (vstate==12) { state <- "Selangor"}  
    if (vstate==13) { state <- "Terengganu"}
    if (vstate==14) { state <- "W.P Kuala Lumpur"}
    if (vstate==15) { state <- "W.P Labuan"}
    if (vstate==16) { state <- "W.P Putrajaya"}
    if (vstate==17) { state <- "Malaysia"}

    if (vstate > 17) 
    {  
      sequence_set <- 0
      return(paste("Bar Plotting has been completed"))
    } 
    else if (vstate < 17)
    {
      
      #--------------------------------------------------------------------------------
      #     Gathering the limit for Y axes plotting based on state variable values
      #--------------------------------------------------------------------------------
      Hvalue <- datafile %>% filter(ConState==state)
      MaxA <- max(as.numeric(Hvalue$TotInsLabour))
      MaxB <- max(as.numeric(Hvalue$TotOutLabour))
      MaxC <- max(as.numeric(Hvalue$TotInsLabourM))
      MaxD <- max(as.numeric(Hvalue$TotOutLabourM))
      MaxE <- max(as.numeric(Hvalue$TotInsLabourF))
      MaxF <- max(as.numeric(Hvalue$TotOutLabourF))
      InsMax <- max(MaxA, MaxB, MaxC, MaxD, MaxE, MaxF)
      #--------------------------------------------------------------------------------
      # 1.   Comparison variable of dsLabourDetail: State Overall Labour Force Statistic
      #--------------------------------------------------------------------------------
      slb    <- paste(state,": Local Overall Labour Statistic from 1982-2017 ('000)")
      sdf    <- datafile %>% filter(datafile$ConState==state)
      sbp    <- barplot(sdf$TotInsLabour, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax)))
      text(sbp, sdf$TotInsLabour, paste(sdf$TotInsLabour,sep=""),pos=3,cex=0.7)
      
      slb    <- paste(state,": Foreign Overall Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$TotOutLabour, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax)))
      text(sbp, sdf$TotOutLabour, paste(sdf$TotOutLabour,sep=""),pos=3,cex=0.7)
      
      filetext   <- paste("PlotLabourDetailOverall_",state,".jpg")
      fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
      fileoutput <- str_replace_all(fileoutput,"-", "")
      dev.copy(jpeg,filename=fileoutput);
      dev.off()
      
      #--------------------------------------------------------------------------------
      # 2.   Comparison variable of dsLabourDetail: State Labour Force by Male category
      #--------------------------------------------------------------------------------
      slb    <- paste(state,": Local (Male) Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$TotInsLabourM, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax)))
      text(sbp, sdf$TotInsLabourM, paste(sdf$TotInsLabourM,sep=""),pos=3,cex=0.7)
      
      slb    <- paste(state,": Foreign (Male) Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$TotOutLabourM, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax)))
      text(sbp, sdf$TotOutLabourM, paste(sdf$TotOutLabourM,sep=""),pos=3,cex=0.7)
      
      filetext   <- paste("PlotLabourDetailMale_",state,".jpg")
      fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
      fileoutput <- str_replace_all(fileoutput,"-", "")
      dev.copy(jpeg,filename=fileoutput);
      dev.off()
      
      #--------------------------------------------------------------------------------
      # 3.   Comparison variable of dsLabourDetail: State Labour Force by Female category
      #--------------------------------------------------------------------------------
      slb    <- paste(state,": Local (Female) Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$TotInsLabourF, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax)))
      text(sbp, sdf$TotInsLabourF, paste(sdf$TotInsLabourF,sep=""),pos=3,cex=0.7)
      
      slb    <- paste(state,": Foreign (Female) Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$TotOutLabourF, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax)))
      text(sbp, sdf$TotOutLabourF, paste(sdf$TotOutLabourF,sep=""),pos=3,cex=0.7)

      filetext   <- paste("PlotLabourDetailFemale_",state,".jpg")
      fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
      fileoutput <- str_replace_all(fileoutput,"-", "")
      dev.copy(jpeg,filename=fileoutput);
      dev.off()
      
    }
    else if (vstate == 17)
    {
      #--------------------------------------------------------------------------------
      #     Gathering the limit for Y axes plotting based on country variable values
      #--------------------------------------------------------------------------------
      Hvalue <- datafile %>% filter(ConState==state)
      MaxA <- max(as.numeric(Hvalue$TotInsLabour))
      MaxB <- max(as.numeric(Hvalue$TotOutLabour))
      MaxC <- max(as.numeric(Hvalue$TotInsLabourM))
      MaxD <- max(as.numeric(Hvalue$TotOutLabourM))
      MaxE <- max(as.numeric(Hvalue$TotInsLabourF))
      MaxF <- max(as.numeric(Hvalue$TotOutLabourF))
      InsMax <- max(MaxA, MaxB, MaxC, MaxD, MaxE, MaxF)
      #--------------------------------------------------------------------------------
      # 1. Comparison variable of dsLabourDetail: Country Overall Labour Force Statistic
      #--------------------------------------------------------------------------------
      clb    <- paste(state,": Local Labour Overall Statistic from 1982-2017 ('000)")
      cdf    <- datafile %>% filter(datafile$ConState==state)
      cbp    <- barplot(cdf$TotInsLabour, border=F, names.arg=cdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=clb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
      text(cbp, cdf$TotInsLabour, paste(cdf$TotInsLabour,sep=""),pos=3,cex=0.7)
      
      clb    <- paste(state,": Foreign Labour Overall Statistic from 1982-2017 ('000)")
      cbp    <- barplot(cdf$TotOutLabour, border=F, names.arg=cdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=clb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
      text(cbp, cdf$TotOutLabour, paste(cdf$TotOutLabour,sep=""),pos=3,cex=0.7)
      
      filetext   <- paste("PlotLabourDetailOverall_",state,".jpg")
      fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
      fileoutput <- str_replace_all(fileoutput,"-", "")
      dev.copy(jpeg,filename=fileoutput);
      dev.off()
      
      #--------------------------------------------------------------------------------
      # 2. Comparison variable of dsLabourDetail: Country Labour Force by Male category
      #--------------------------------------------------------------------------------
      clb    <- paste(state,": Local (Male) Labour Statistic from 1982-2017 ('000)")
      cbp    <- barplot(cdf$TotInsLabourM, border=F, names.arg=cdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=clb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
      text(cbp, cdf$TotInsLabourM, paste(cdf$TotInsLabourM,sep=""),pos=3,cex=0.7)
      
      clb    <- paste(state,": Foreign (Male) Labour Statistic from 1982-2017 ('000)")
      cbp    <- barplot(cdf$TotOutLabourM, border=F, names.arg=cdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=clb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
      text(cbp, cdf$TotOutLabourM, paste(cdf$TotOutLabourM,sep=""),pos=3,cex=0.7)
      
      filetext   <- paste("PlotLabourDetailMale_",state,".jpg")
      fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
      fileoutput <- str_replace_all(fileoutput,"-", "")
      dev.copy(jpeg,filename=fileoutput);
      dev.off()
      
      #--------------------------------------------------------------------------------
      # 3. Comparison variable of dsLabourDetail: Country Labour Force by Female category
      #--------------------------------------------------------------------------------
      clb    <- paste(state,": Local (Female) Labour Statistic from 1982-2017 ('000)")
      cbp    <- barplot(cdf$TotInsLabourF, border=F, names.arg=cdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=clb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
      text(cbp, cdf$TotInsLabourF, paste(cdf$TotInsLabourF,sep=""),pos=3,cex=0.7)
      
      clb    <- paste(state,": Foreign (Female) Labour Statistic from 1982-2017 ('000)")
      cbp    <- barplot(cdf$TotOutLabourF, border=F, names.arg=cdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=clb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
      text(cbp, cdf$TotOutLabourF, paste(cdf$TotOutLabourF,sep=""),pos=3,cex=0.7)
      
      filetext   <- paste("PlotLabourDetailFemale_",state,".jpg")
      fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
      fileoutput <- str_replace_all(fileoutput,"-", "")
      dev.copy(jpeg,filename=fileoutput);
      dev.off()
      
    }
  }
  dev.off()
}

