BarPlotEthnic <- function(datafile)
{
  setwd(DirVisual)
  par(mfrow = c(2,1))
  sequence_set <- 1
  while(sequence_set > 0) 
  {
   cat("\014")
   cat("\n\n")
   cat("----------------------------------------------------------------------\n\n")
   cat("                        Ethnic Service                                \n")
   cat("                  Labour Force Ethnic Selection                       \n")
   cat("                (Time Series Data from 1982-2017)                     \n\n")
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
     MaxA <- max(as.numeric(Hvalue$SubTotWorkForce))
     MaxB <- max(as.numeric(Hvalue$SubTotForeignWorkForce))
     
     MaxC <- max(as.numeric(Hvalue$BumiWorkForce))
     MaxD <- max(as.numeric(Hvalue$ChineseWorkForce))
     MaxE <- max(as.numeric(Hvalue$IndianWorkForce))
     MaxF <- max(as.numeric(Hvalue$OtherWorkForce))
     
     InsMaxOvall <- max(MaxA, MaxB)
     InsMaxAll <- max(MaxC, MaxD, MaxE, MaxF)
     
     #--------------------------------------------------------------------------------
     # 1.   Comparison variable of dsLabourDetail: State Overall Labour Force Statistic
     #--------------------------------------------------------------------------------
     slb    <- paste(state,": Overall Local Labour Statistic from 1982-2017 ('000)")
     sdf    <- datafile %>% filter(datafile$ConState==state)     
     sbp    <- barplot(sdf$SubTotWorkForce, border=F, names.arg=sdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=slb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMaxOvall+(InsMaxOvall*0.25))))
                text(sbp, sdf$SubTotWorkForce, paste(sdf$SubTotWorkForce,sep=""),pos=3,cex=0.7)
                
      slb    <- paste(state,": Overall Foreign Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$SubTotForeignWorkForce, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMaxOvall+(InsMaxOvall*0.25))))
                text(sbp, sdf$SubTotForeignWorkForce, paste(sdf$SubTotForeignWorkForce,sep=""),pos=3,cex=0.7)
                
                filetext   <- paste("PlotLabourEthnicOverall_",state,".jpg")
                fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
                fileoutput <- str_replace_all(fileoutput,"-", "")
                dev.copy(jpeg,filename=fileoutput);
                dev.off()
                
      #--------------------------------------------------------------------------------
      # 2. Comparison variable of dsLabourDetail: Country Workforce Ethnic Categories
      #--------------------------------------------------------------------------------
      slb    <- paste(state,": Malaysian (Bumi) Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$BumiWorkForce, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMaxAll+(InsMaxAll*0.25))))
                text(sbp, sdf$BumiWorkForce, paste(sdf$BumiWorkForce,sep=""),pos=3,cex=0.7)
                
                
      slb    <- paste(state,": Malaysian (Chinese) Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$ChineseWorkForce, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMaxAll+(InsMaxAll*0.25))))
                text(sbp, sdf$ChineseWorkForce, paste(sdf$ChineseWorkForce,sep=""),pos=3,cex=0.7)
                
                filetext   <- paste("PlotLabourEthnicBumiChinese_",state,".jpg")
                fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
                fileoutput <- str_replace_all(fileoutput,"-", "")
                dev.copy(jpeg,filename=fileoutput);
                dev.off()
                
      slb    <- paste(state,": Malaysian (Indian) Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$IndianWorkForce, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMaxAll+(InsMaxAll*0.25))))
                text(sbp, sdf$IndianWorkForce, paste(sdf$IndianWorkForce,sep=""),pos=3,cex=0.7)
                
      slb    <- paste(state,": Malaysian (Others) Labour Statistic from 1982-2017 ('000)")
      sbp    <- barplot(sdf$OtherWorkForce, border=F, names.arg=sdf$Year,las=2,
                        col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                        main=slb,
                        xlab = "Years (Time Series)",
                        ylab = "Number of Labour Force",
                        ylim = c(0,as.numeric(InsMaxAll+(InsMaxAll*0.25))))
                text(sbp, sdf$OtherWorkForce, paste(sdf$OtherWorkForce,sep=""),pos=3,cex=0.7)
                
                filetext   <- paste("PlotLabourEthnicIndianOthers_",state,".jpg")
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
     MaxA <- max(as.numeric(Hvalue$SubTotWorkForce))
     MaxB <- max(as.numeric(Hvalue$SubTotForeignWorkForce))
     
     MaxC <- max(as.numeric(Hvalue$BumiWorkForce))
     MaxD <- max(as.numeric(Hvalue$ChineseWorkForce))
     MaxE <- max(as.numeric(Hvalue$IndianWorkForce))
     MaxF <- max(as.numeric(Hvalue$OtherWorkForce))
     
     InsMaxOvall <- max(MaxA, MaxB)
     InsMaxAll <- max(MaxC, MaxD, MaxE, MaxF)
     
     #--------------------------------------------------------------------------------
     # 1. Comparison variable of dsLabourDetail: Country Workforce Ethnic Categories
     #--------------------------------------------------------------------------------
     clb    <- paste(state,": Overall Local Labour Statistic from 1982-2017 ('000)")
     cdf    <- datafile %>% filter(datafile$ConState==state)
     cbp    <- barplot(cdf$SubTotWorkForce, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMaxOvall+(InsMaxOvall*0.25))))
               text(cbp, cdf$SubTotWorkForce, paste(cdf$SubTotWorkForce,sep=""),pos=3,cex=0.7)
               
     clb    <- paste(state,": Overall Foreign Labour Statistic from 1982-2017 ('000)")
     cbp    <- barplot(cdf$SubTotForeignWorkForce, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMaxOvall+(InsMaxOvall*0.25))))
               text(cbp, cdf$SubTotForeignWorkForce, paste(cdf$SubTotForeignWorkForce,sep=""),pos=3,cex=0.7)
               
               filetext   <- paste("PlotLabourEthnicOverall_",state,".jpg")
               fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
               fileoutput <- str_replace_all(fileoutput,"-", "")
               dev.copy(jpeg,filename=fileoutput);
               dev.off()
               
     #--------------------------------------------------------------------------------
     # 2. Comparison variable of dsLabourDetail: Country Workforce Ethnic Categories
     #--------------------------------------------------------------------------------
     clb    <- paste(state,": Malaysian (Bumi) Labour Statistic from 1982-2017 ('000)")
     cbp    <- barplot(cdf$BumiWorkForce, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMaxAll+(InsMaxAll*0.25))))
               text(cbp, cdf$BumiWorkForce, paste(cdf$BumiWorkForce,sep=""),pos=3,cex=0.7)
               
     clb    <- paste(state,": Malaysian (Chinese) Labour Statistic from 1982-2017 ('000)")
     cbp    <- barplot(cdf$ChineseWorkForce, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMaxAll+(InsMaxAll*0.25))))
               text(cbp, cdf$ChineseWorkForce, paste(cdf$ChineseWorkForce,sep=""),pos=3,cex=0.7)
               
               filetext   <- paste("PlotLabourEthnicBumiChinese_",state,".jpg")
               fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
               fileoutput <- str_replace_all(fileoutput,"-", "")
               dev.copy(jpeg,filename=fileoutput);
               dev.off()
               
     clb    <- paste(state,": Malaysian (Indian) Labour Statistic from 1982-2017 ('000)")
     cbp    <- barplot(cdf$IndianWorkForce, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMaxAll+(InsMaxAll*0.25))))
               text(cbp, cdf$IndianWorkForce, paste(cdf$IndianWorkForce,sep=""),pos=3,cex=0.7)
               
     clb    <- paste(state,": Malaysian (Others) Labour Statistic from 1982-2017 ('000)")
     cbp    <- barplot(cdf$OtherWorkForce, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMaxAll+(InsMaxAll*0.25))))
               text(cbp, cdf$OtherWorkForce, paste(cdf$OtherWorkForce,sep=""),pos=3,cex=0.7)
               
               filetext   <- paste("PlotLabourEthnicIndianOthers_",state,".jpg")
               fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
               fileoutput <- str_replace_all(fileoutput,"-", "")
               dev.copy(jpeg,filename=fileoutput);
               dev.off()

    }
  }
  dev.off()
}

