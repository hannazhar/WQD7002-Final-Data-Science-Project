VisualPlotly <- function(datafile)
{
  setwd(DirVisual)
  df <- datafile
  sequence_set <- 1
  while(sequence_set > 0) 
  {
    cat("\014")
    cat("\n\n")
    cat("----------------------------------------------------------------------\n\n")
    cat("                       Interactive Visualization                      \n")
    cat("                         Labour Force Selection                       \n")
    cat("                   (Time Series Data from 1982-2017)                  \n\n")
    cat("----------------------------------------------------------------------\n\n")   
    cat("            1. Labour Overall by State                                \n")
    cat("            2. Animate State Overall (Time Series)                    \n")
    cat("            3. Scatter Gender (Local/Foreign)                         \n\n")
    cat("            4. Exit                                                   \n")
    cat("\n")
    vstate <- readline(prompt="       Please enter your number: \n")
    vstate <- as.numeric(vstate)
    
    if (vstate > 3) 
    {
      sequence_set <- 0
      return(paste("Interactive Plotting has been completed \n"))
    }
    else if (vstate == 1)
    {
      sd       <- highlight_key(df, ~ConState,"Select State Within Malaysia: (dsLabourDetailState)")
      base     <- plot_ly(sd, color = I("blue"),height = 600) %>% group_by(ConState)
      
      p1 <- base %>% summarise(miss=sum(as.numeric(TotInsLabour))) %>%
        filter(miss > 0) %>% 
        add_markers(x=~miss, y=~forcats::fct_reorder(ConState, miss), 
                    hoverinfo = "x+y") %>%  layout(barmode = "overlay",
                    xaxis = list(title = "Number of Labour Force Distribution"),
                    yaxis = list(title = "State of Malaysia"))
      
      p2 <- base %>% add_lines(x=~Year, y=~TotInsLabour, alpha = 0.3) %>%
        layout(xaxis = list(title = "Years"), 
               yaxis = list(title = "State of Malaysia"))
      
      p3 <- subplot(p1, p2, titleX = TRUE, widths = c(0.4, 0.6)) %>% hide_legend() %>%
        highlight(off="plotly_doubleclick", dynamic=TRUE, selectize=TRUE,
                  color=toRGB(RColorBrewer::brewer.pal(3, "Dark2"), 0.5))
      print(p3)
      
      #--------------------------------------------------------------------------------
      #  Saving the plot in html format for later use. Save in the Visual directory
      #--------------------------------------------------------------------------------
      
      setwd(DirVisual)
      htmlwidgets::saveWidget(as.widget(p3), file="PlotInteractiveOverall.html")
      setwd(DirProgram)
      
      #--------------------------------------------------------------------------------
      #  Close the html file, Change back to Program directory
      #--------------------------------------------------------------------------------
      
    }
    else if (vstate == 2)
    {
      cat("Regenerating the values for animate plotting ..... progressing\n")
      
      p1 <- ggplot(df, aes(df$TotInsLabour, df$ConState)) +
              geom_point(size=3, color="blue") +
              scale_colour_manual() +
              labs(title = 'Dataset: dsLabourDetailState | Year: {frame_time}',
              subtitle = "Labour Force (Time Series Data) by State movement from 1982-2017",
              colour = "df$ConState",
              x="Total Labour Force in ('000)", y="State of Malaysia") +
              transition_time(as.integer(df$Year)) +
              ease_aes('cubic-in-out')
      
      print(p1)
      
      #--------------------------------------------------------------------------------
      #  Saving the plot in html format for later use. Save in the Visual directory
      #--------------------------------------------------------------------------------
      
      setwd(DirVisual)
      ggsave("PlotInteractiveAnimate.jpg")
      setwd(DirProgram)
      
      #--------------------------------------------------------------------------------
      #  Close the html file, Change back to Program directory
      #--------------------------------------------------------------------------------
      
    }
    else if (vstate == 3)
    {
      cat("Regenerating the overall plotting ............. progressing\n")

      par(mfrow = c(2,2))
      sdf <- df
      
      maxMale <- max(as.integer(df$TotMaleLabour))
      maxFema <- max(as.integer(df$TotFemaleLabour))
      maxLabour <- max(as.integer(maxMale,maxFema))
      
      p1 <- plot_ly(sdf, x=sdf$Year, name=("Total Labour Male (Local and Foreign)")) %>%
        add_bars(y=sdf$TotMaleLabour)

      p2 <- plot_ly(sdf,x=sdf$Year, name=("Total All Labour Male Statistic")) %>%
        add_bars(x=~Year, y=sdf$TotInsLabourM, marker=list(color='red'), name='Male (Local)') %>%
        add_bars(x=~Year, y=sdf$TotOutLabourM, marker=list(color='blue'), name = 'Male (Foreign)')

      p3 <- plot_ly(sdf,x=sdf$Year, name=("Total All Labour Female Statistic")) %>%
        add_trace(y=sdf$TotInsLabourF, name="Female (Local)") %>%
        add_trace(y=sdf$TotOutLabourF, name="Female (Foreign)")

      p4 <- plot_ly(sdf, x=sdf$Year, name=("Total Labour Female (Local and Foreign)")) %>%
        add_trace(y=sdf$TotFemaleLabour) 
      
      p5 <- print(subplot(p2, p1, p3, p4, nrows=2, heights=c(0.5, 0.5), margin=0.03)) %>%
        layout(title = 'Statistic of Labour Gender (Local and Foreign) from 1982-2017',
               margin=list(l=50, r=20, t=70, b=70),
               plot_bgcolor='rgb(248, 248, 255)')
      
      #--------------------------------------------------------------------------------
      #  Saving the plot in html format for later use. Save in the Visual directory
      #--------------------------------------------------------------------------------
      
      setwd(DirVisual)
      htmlwidgets::saveWidget(as.widget(p5), file="PlotInteractiveScatter.html")
      setwd(DirProgram)
      
      #--------------------------------------------------------------------------------
      #  Close the html file, Change back to Program directory
      #--------------------------------------------------------------------------------
      
      
    }
  }
}
