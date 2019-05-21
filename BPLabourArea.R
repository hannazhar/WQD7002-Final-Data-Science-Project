BarPlotArea <- function(datafile)
{
  setwd(DirVisual)
  par(mfrow = c(2,1))
  sequence_set <- 1
  while(sequence_set > 0) 
  {
    cat("\014")
    cat("\n\n")
    cat("----------------------------------------------------------------------\n\n")
    cat("                         Area Penetration                             \n")
    cat("                  Labour Force Areas Selection                        \n")
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
     MaxA <- max(as.numeric(Hvalue$AllLabourInsR))
     MaxB <- max(as.numeric(Hvalue$AllLabourOutR))
     MaxC <- max(as.numeric(Hvalue$AllLabourInsU))
     MaxD <- max(as.numeric(Hvalue$AllLabourOutU))
     InsMax <- max(MaxA, MaxB, MaxC, MaxD)
     #----------------------------------------------------------------------------------
     #                            S T A T E   L E V E L
     # 1.   Comparison variable of dsLabourArea (Rural): AllLabourInsR and AllLabourOutR
     #----------------------------------------------------------------------------------
     slb    <- paste(state,": Local Labour at Rural Area Statistic from 1982-2017 ('000)")
     sdf    <- datafile %>% filter(datafile$ConState==state)
     sbp    <- barplot(sdf$AllLabourInsR, border=F, names.arg=sdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=slb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
                text(sbp, sdf$AllLabourInsR, paste(sdf$AllLabourInsR,sep=""),pos=3,cex=0.7)
                
     slb    <- paste(state,": Foreign Labour at Rural Area Statistic from 1982-2017 ('000)")
     sdf    <- datafile %>% filter(datafile$ConState==state)
     sbp    <- barplot(sdf$AllLabourOutR, border=F, names.arg=sdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=slb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
                text(sbp, sdf$AllLabourOutR, paste(sdf$AllLabourOutR,sep=""),pos=3,cex=0.7)
     
                filetext   <- paste("PlotLabourAreaRural_",state,".jpg")
                fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
                fileoutput <- str_replace_all(fileoutput,"-", "")
                dev.copy(jpeg,filename=fileoutput);
                dev.off()
                
     #----------------------------------------------------------------------------------
     #                            S T A T E   L E V E L
     # 2.   Comparison variable of dsLabourArea (Urban): AllLabourInsU and AllLabourOutU
     #----------------------------------------------------------------------------------
     slb    <- paste(state,": Local Labour at Urban Area Statistic from 1982-2017 ('000)")
     sdf    <- datafile %>% filter(datafile$ConState==state)
     sbp    <- barplot(sdf$AllLabourInsU, border=F, names.arg=sdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=slb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
                text(sbp, sdf$AllLabourInsU, paste(sdf$AllLabourInsU,sep=""),pos=3,cex=0.7)
                
     slb    <- paste(state,": Foreign Labour at Urban Area Statistic from 1982-2017 ('000)")
     sdf    <- datafile %>% filter(datafile$ConState==state)
     sbp    <- barplot(sdf$AllLabourOutU, border=F, names.arg=sdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=slb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
                text(sbp, sdf$AllLabourOutU, paste(sdf$AllLabourOutU,sep=""),pos=3,cex=0.7)
                
                filetext   <- paste("PlotLabourAreaUrban_",state,".jpg")
                fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
                fileoutput <- str_replace_all(fileoutput,"-", "")
                dev.copy(jpeg,filename=fileoutput);
                dev.off()
    }
   else if (vstate == 17)
    {
     #--------------------------------------------------------------------------------
     #     Gathering the limit for Y axes plotting based on state variable values
     #--------------------------------------------------------------------------------
     Hvalue <- datafile %>% filter(ConState==state)
     MaxA <- max(as.numeric(Hvalue$AllLabourInsR))
     MaxB <- max(as.numeric(Hvalue$AllLabourOutR))
     MaxC <- max(as.numeric(Hvalue$AllLabourInsU))
     MaxD <- max(as.numeric(Hvalue$AllLabourOutU))
     InsMax <- max(MaxA, MaxB, MaxC, MaxD)
     #----------------------------------------------------------------------------------
     #                            C O U N T R Y   L E V E L
     # 1.   Comparison variable of dsLabourArea (Rural): AllLabourInsR and AllLabourOutR
     #----------------------------------------------------------------------------------
     clb    <- paste(state,": Local Labour at Rural Area Statistic from 1982-2017 ('000)")
     cdf    <- datafile %>% filter(datafile$ConState==state)
     cbp    <- barplot(cdf$AllLabourInsR, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
               text(cbp, cdf$AllLabourInsR, paste(cdf$AllLabourInsR,sep=""),pos=3,cex=0.7)
               
     clb    <- paste(state,": Foreign Labour at Rural Area Statistic from 1982-2017 ('000)")
     cdf    <- datafile %>% filter(datafile$ConState==state)
     cbp    <- barplot(cdf$AllLabourOutR, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
               text(cbp, cdf$AllLabourOutR, paste(cdf$AllLabourOutR,sep=""),pos=3,cex=0.7)
               
               filetext   <- paste("PlotLabourAreaRural_",state,".jpg")
               fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
               fileoutput <- str_replace_all(fileoutput,"-", "")
               dev.copy(jpeg,filename=fileoutput);
               dev.off()
               
     #----------------------------------------------------------------------------------
     #                            C O U N T R Y   L E V E L
     # 2.   Comparison variable of dsLabourArea (Urban): AllLabourInsU and AllLabourOutU
     #----------------------------------------------------------------------------------
     clb    <- paste(state,": Local Labour at Urban Area Statistic from 1982-2017 ('000)")
     cdf    <- datafile %>% filter(datafile$ConState==state)
     cbp    <- barplot(cdf$AllLabourInsU, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
               text(cbp, cdf$AllLabourInsU, paste(cdf$AllLabourInsU,sep=""),pos=3,cex=0.7)
               
     clb    <- paste(state,": Foreign Labour at Urban Area Statistic from 1982-2017 ('000)")
     cdf    <- datafile %>% filter(datafile$ConState==state)
     cbp    <- barplot(cdf$AllLabourOutU, border=F, names.arg=cdf$Year,las=2,
                       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)),
                       main=clb,
                       xlab = "Years (Time Series)",
                       ylab = "Number of Labour Force",
                       ylim = c(0,as.numeric(InsMax+(InsMax*0.25))))
               text(cbp, cdf$AllLabourOutU, paste(cdf$AllLabourOutU,sep=""),pos=3,cex=0.7)
               
               filetext   <- paste("PlotLabourAreaUrban_",state,".jpg")
               fileoutput <- str_replace_all(str_trim(filetext, side="left")," ", "-")
               fileoutput <- str_replace_all(fileoutput,"-", "")
               dev.copy(jpeg,filename=fileoutput);
               dev.off()
    }
  }
  dev.off()
}

