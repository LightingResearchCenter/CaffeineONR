
library(ggsignif)
library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)


##This script plots EEG data fro each subjects

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

plot_allSubjects <- function(plotData, outcomeMeasure){
  
  outcomeMeasureTitles <- c("GNG-rt")

  plotData$group_var <- paste(plotData$Subject_id, plotData$condition, sep = "_" )
  

  if(outcomeMeasure == "GNG-rt"){
    GNG_rt <- subset(plotData, display == "go" & response_time >= .1)
    #GNG_rt_summarized1 <- aggregate(response_time ~ Subject_id * condition * test_period, data = GNG_rt, FUN= mean)
    perform_summarized1 <- summarySE( GNG_rt, measurevar = "response_time", groupvars = c("condition", "Subject_id", "test_period", "group_var", "condition2"))
    colnames(perform_summarized1)[7] <- "value"
  }
  
  if(outcomeMeasure == "OB-rt"){
    plotData$response_time <- as.numeric(plotData$response_time)
    OB_rt <- subset(plotData, accuracy == 1 & response_time >= .1)
    #GNG_rt_summarized1 <- aggregate(response_time ~ Subject_id * condition * test_period, data = GNG_rt, FUN= mean)
    perform_summarized1 <- summarySE( OB_rt, measurevar = "response_time", groupvars = c("condition", "Subject_id", "test_period", "group_var", "condition2"))
    colnames(perform_summarized1)[7] <- "value"
  }
  
  if(outcomeMeasure == "MOB-rt"){
    plotData$response_time <- as.numeric(plotData$response_time)
    OB_rt <- subset(plotData, accuracy == 1 & response_time >= .1)
    #GNG_rt_summarized1 <- aggregate(response_time ~ Subject_id * condition * test_period, data = GNG_rt, FUN= mean)
    perform_summarized1 <- summarySE( OB_rt, measurevar = "response_time", groupvars = c("condition", "Subject_id", "test_period", "group_var", "condition2"))
    colnames(perform_summarized1)[7] <- "value"
  }
  
  yUpper <- max(perform_summarized1$value) + (max(perform_summarized1$value) * .10)
  meanDuration <- round(mean(  perform_summarized1$value), digits = 2) 
  medianDuration <- round(median(  perform_summarized1$value), digits = 2) 
  sd2_Duration_upper <- sd(  perform_summarized1$value)*2 + meanDuration
  sd2_Duration_lower <- meanDuration - sd(  perform_summarized1$value)*2 
  
  
  perform_summarized1$value <- round(perform_summarized1$value, digits = 2) 
  
  figSpecsList <- c(yUpper, meanDuration, medianDuration, sd2_Duration_upper, sd2_Duration_lower)
  
  
  

  fontSize <- 3
  
  titleText <- simpleCap(outcomeMeasure)
  
  subPlot1 <- ggplot(perform_summarized1, aes_string(x="test_period", y="value", group = "group_var", fill = "condition")) +
    #geom_line(colour =  "black")+
    #geom_point(aes(size = .5))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=value-se, ymax=value+se, colour = "black"),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    labs(y = outcomeMeasure)+
    coord_cartesian(ylim=c(0,figSpecsList[1]))+
    #geom_text(aes_string(label = "value", vjust=2)) +
    geom_segment(aes(x = 0,xend=.2,y=figSpecsList[2],yend=figSpecsList[2]), color = "red", linetype = "dashed",  size = .4) + 
    annotate("text", x = .3, y = figSpecsList[2]+(figSpecsList[2]*.07), label = paste("mu : ", figSpecsList[2], sep = ""), parse = TRUE, color = "red", size = fontSize) +
    
    geom_segment(aes(x = 4.2,xend=4.4,y=figSpecsList[3],yend=figSpecsList[3]), color = "red", linetype = "dashed", size = .4) + 
    annotate("text", x = 3.7, y=figSpecsList[3]+(figSpecsList[3]*.07), label = paste("M : ", figSpecsList[3], sep = ""), parse = TRUE, color = "red", size = fontSize) +
    
    facet_wrap(~ Subject_id,  ncol=3, scales = "free")+
    theme(axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5))+
    
   # theme(axis.title.x=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5))+
    ggtitle(titleText)+

    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          plot.title = element_text(size = 30), strip.text = element_text(size=11))
  
  
  if(figSpecsList[5] > 0){
    subPlot1 <- subPlot1 + geom_hline(aes(yintercept = figSpecsList[5]), colour = "red", size = .4, linetype = "dashed") + 
      annotate("text", x = 2.5, y=figSpecsList[5]+(yUpper*.03), label = "2 SD from mean", colour = "red", size = fontSize) 
  }
  
  subPlot1 <- subPlot1 + geom_hline(aes(yintercept = figSpecsList[4]), colour = "red", size =  .4, linetype="dashed") + 
  annotate("text", x = 2.5, y=figSpecsList[4]+(yUpper*.03), label = "2 SD from mean", colour = "red", size = fontSize) 
  
 # subPlot1 <- subPlot1 +   scale_colour_manual(values=c("deepskyblue2", "dodgerblue2",  "gray80", "gray90", "red1", "red4" )) 
  subPlot1 <- subPlot1 +   scale_fill_manual(values=c("deepskyblue2", "dodgerblue2",  "gray80", "gray90", "red1", "red4" )) 
  

  
  return(subPlot1)
  
  
}

plot_allSubjectsBoxPlot <- function(plotData, outcomeMeasure){
  
  outcomeMeasureTitles <- c("GNG-rt")
  
  plotData$group_var <- paste(plotData$Subject_id, plotData$condition, sep = "_" )
  
  
  if(outcomeMeasure == "GNG-rt"){
    GNG_rt <- subset(plotData, display == "go" & response_time >= .1)
    #GNG_rt_summarized1 <- aggregate(response_time ~ Subject_id * condition * test_period, data = GNG_rt, FUN= mean)
    perform_summarized1 <- summarySE( GNG_rt, measurevar = "response_time", groupvars = c("condition", "Subject_id", "test_period", "group_var", "condition2"))
    colnames(perform_summarized1)[7] <- "value"
  }
  
  if(outcomeMeasure == "OB-rt"){
    plotData$response_time <- as.numeric(plotData$response_time)
    OB_rt <- subset(plotData, accuracy == 1 & response_time >= .1)
    #GNG_rt_summarized1 <- aggregate(response_time ~ Subject_id * condition * test_period, data = GNG_rt, FUN= mean)
    perform_summarized1 <- summarySE( OB_rt, measurevar = "response_time", groupvars = c("condition", "Subject_id", "test_period", "group_var", "condition2"))
    colnames(perform_summarized1)[7] <- "value"
  }
  if(outcomeMeasure == "MOB-rt"){
    plotData$response_time <- as.numeric(plotData$response_time)
    OB_rt <- subset(plotData, accuracy == 1 & response_time >= .1)
    #GNG_rt_summarized1 <- aggregate(response_time ~ Subject_id * condition * test_period, data = GNG_rt, FUN= mean)
    perform_summarized1 <- summarySE( OB_rt, measurevar = "response_time", groupvars = c("condition", "Subject_id", "test_period", "group_var", "condition2"))
    colnames(perform_summarized1)[7] <- "value"
  }
  
  yUpper <- max(perform_summarized1$value) + (max(perform_summarized1$value) * .10)
  meanDuration <- round(mean(  perform_summarized1$value), digits = 2) 
  medianDuration <- round(median(  perform_summarized1$value), digits = 2) 
  sd2_Duration_upper <- sd(  perform_summarized1$value)*2 + meanDuration
  sd2_Duration_lower <- meanDuration - sd(  perform_summarized1$value)*2 
  
  
  perform_summarized1$value <- round(perform_summarized1$value, digits = 2) 
  
  figSpecsList <- c(yUpper, meanDuration, medianDuration, sd2_Duration_upper, sd2_Duration_lower)
  
  
  
  
  fontSize <- 3
  
  titleText <- simpleCap(outcomeMeasure)
  perform_summarized1$condition <- as.factor(perform_summarized1$condition)
  perform_summarized1$test_period <- as.factor(perform_summarized1$test_period)
  
  subPlot1 <- ggplot(perform_summarized1, aes_string(x="test_period", y="value", fill = "condition")) +
    geom_boxplot()+
    #geom_line(colour =  "black")+
    #geom_point(aes(size = .5))+
    #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    #geom_errorbar(aes(ymin=value-se, ymax=value+se, colour = "black"),
    #              width=.2,                    # Width of the error bars
    #              position=position_dodge(.9)) +
    labs(y = outcomeMeasure)+
    #coord_cartesian(ylim=c(0,figSpecsList[1]))+
    #geom_text(aes_string(label = "value", vjust=2)) +
    #geom_segment(aes(x = 0,xend=.2,y=figSpecsList[2],yend=figSpecsList[2]), color = "red", linetype = "dashed",  size = .4) + 
    #annotate("text", x = .3, y = figSpecsList[2]+(figSpecsList[2]*.07), label = paste("mu : ", figSpecsList[2], sep = ""), parse = TRUE, color = "red", size = fontSize) +
    
    #geom_segment(aes(x = 4.2,xend=4.4,y=figSpecsList[3],yend=figSpecsList[3]), color = "red", linetype = "dashed", size = .4) + 
    #annotate("text", x = 3.7, y=figSpecsList[3]+(figSpecsList[3]*.07), label = paste("M : ", figSpecsList[3], sep = ""), parse = TRUE, color = "red", size = fontSize) +
    
    #facet_wrap(~ condition, scales = "free")+
    theme(axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5))+
    
    # theme(axis.title.x=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5))+
    ggtitle(titleText)+
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          plot.title = element_text(size = 30), strip.text = element_text(size=11))
  
  
  #if(figSpecsList[5] > 0){
    #subPlot1 <- subPlot1 + geom_hline(aes(yintercept = figSpecsList[5]), colour = "red", size = .4, linetype = "dashed") + 
     # annotate("text", x = 2.5, y=figSpecsList[5]+(yUpper*.03), label = "2 SD from mean", colour = "red", size = fontSize) 
  #}
  
  #subPlot1 <- subPlot1 + geom_hline(aes(yintercept = figSpecsList[4]), colour = "red", size =  .4, linetype="dashed") + 
    #annotate("text", x = 2.5, y=figSpecsList[4]+(yUpper*.03), label = "2 SD from mean", colour = "red", size = fontSize) 
  
  # subPlot1 <- subPlot1 +   scale_colour_manual(values=c("deepskyblue2", "dodgerblue2",  "gray80", "gray90", "red1", "red4" )) 
  subPlot1 <- subPlot1 +   scale_fill_manual(values=c("deepskyblue2", "dodgerblue2",  "gray80", "gray90", "red1", "red4" )) 
  
  
  
  return(subPlot1)
  
  
}

nsubsReturn <- function(plotData, color){
  
  return(length(unique(plotData[plotData$color == color,]$subject)))
}




if(FALSE){
  GNG_caffiene <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/GNG_caffieneISIaddedRemovedISI.csv")
  OB_caffiene <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/OB_caffieneISIaddedRemovedISI.csv")
  TB_caffiene<- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/TB_caffieneISIaddedRemovedISI.csv")
  MOB_caffiene <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/MOB_caffieneISIaddedRemovedISI.csv")
  
  
  plot1 <- plot_allSubjects(GNG_caffiene, 'GNG-rt' )
  ggsave("//root/projects/Caffeine_ONR_Study/plots/GNG_rt.pdf", dpi = 800, width = 17, height = 17, units = "in")
  
  plot_allSubjects(OB_caffiene, 'OB-rt' )
  ggsave("//root/projects/Caffeine_ONR_Study/plots/OB_rt.pdf", dpi = 800, width = 17, height = 17, units = "in")
  
  plot_allSubjects(MOB_caffiene, 'MOB-rt' )
  ggsave("//root/projects/Caffeine_ONR_Study/plots/MOB_rt.pdf", dpi = 800, width = 17, height = 17, units = "in")
  
  
  plot_allSubjectsBoxPlot(GNG_caffiene, 'GNG-rt' )
  ggsave("//root/projects/Caffeine_ONR_Study/plots/GNG_rt_box.pdf", dpi = 800, width = 8, height = 5, units = "in")
  ggsave("//root/projects/Caffeine_ONR_Study/plots/GNG_rt_box.png", dpi = 800, width = 8, height = 3.5, units = "in")
  
  plot_allSubjectsBoxPlot(OB_caffiene, 'OB-rt' )
  ggsave("//root/projects/Caffeine_ONR_Study/plots/OB_rt_box.pdf", dpi = 800, width = 8, height = 5, units = "in")
  ggsave("//root/projects/Caffeine_ONR_Study/plots/OB_rt_box.png", dpi = 800, width = 8, height = 3.5, units = "in")
  
  plot_allSubjectsBoxPlot(MOB_caffiene, 'MOB-rt' )
  ggsave("//root/projects/Caffeine_ONR_Study/plots/MOB_rt_box.pdf", dpi = 800, width = 8, height = 5, units = "in")
  ggsave("//root/projects/Caffeine_ONR_Study/plots/MOB_rt_box.png", dpi = 800, width = 8, height = 3.5, units = "in")
  
  

  thetaRed_plot <- plot_allSubjects(GNG_caffiene, 'theta', "red", FALSE )
  
  
}


if(FALSE){
  
  thetaRed_plot <- plot_allSubjects(UpdatedEEG_4_30_18, 'theta', "red", FALSE )
  alphacyan_plot1 <- plot_allSubjects(UpdatedEEG_4_30_18, 'alpha', "cyan", FALSE )
  alphacyan_plot2 <- plot_allSubjects(UpdatedEEG_4_30_18, 'alpha', "cyan", TRUE )
  
  
  alphaRed_d_plot <- plot_allSubjects(daytime_eeg, 'alpha', "red", FALSE )

  alphaRed_d_plot <- plot_allSubjects(daytime_eeg, 'theta', "blue", FALSE )
  alphaRed_d_plot <- plot_allSubjects(daytime_eeg, 'atheta', "blue", FALSE )
  alphaRed_d_plot <- plot_allSubjects(daytime_eeg, 'alpha', "blue", FALSE )
  
  
  ggsave("//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/ThetaRed.pdf", dpi = 800, width = 17, height = 22, units = "in")
  
  
}

createNames <- function(colorList){
  
  
  powerBins <- c("theta", "atheta", "alpha", "beta")
  newList <- c()
  for(i in 1:length(colorList)){
    for(j in 1:length(powerBins)){
      #print(paste(colorList[i], powerBins[j], sep = "_"))
      newList <- c(newList, paste(colorList[i], powerBins[j], sep = "_"))
    }
  }
  return(newList)
}


if(FALSE){

  
  colorList <- c("blue", "cyan", "green", "amber", "red")
  colorList <- c( "cyan", "amber")
  
  
  
  for(i in 1:length(figure_list)){
    curr <- strsplit(figure_list[i], "_")
    print(curr[[1]][1])
    print(curr[[1]][2])
    currPlot <- plot_allSubjects(filtered_eeg, curr[[1]][2], curr[[1]][1], TRUE)
    nSubs <- nsubsReturn(filtered_eeg[filtered_eeg$color == "cyan" | filtered_eeg$color == "amber", ], curr[[1]][1])
    
    dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/"
    pdfFileName <- paste(figure_list[i], "_filterd.png")
    ggsave(paste(dir, pdfFileName, sep = ""), dpi = 250, width = 9.5, height = (ceiling(nSubs/3))*3, units = "in")
    
  }
  
}


saveGraph <- function(dayNight, colorList, PlotData ){
  
  figure_list <- createNames(colorList)
  
  for(i in 1:length(figure_list)){
    curr <- strsplit(figure_list[i], "_")
    print(curr[[1]][1])
    print(curr[[1]][2])
    currPlot <- plot_allSubjects(PlotData, curr[[1]][2], curr[[1]][1], FALSE)
    nSubs <- nsubsReturn(PlotData[PlotData$color == curr[[1]][1], ], curr[[1]][1])
    
    if(tolower(dayNight) == "day"){
      dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/DAY/"
      pdfFileName <- paste(figure_list[i], "_day.pdf")
    }
    if(tolower(dayNight) == "night"){
      dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/NIGHT/"
      pdfFileName <- paste(figure_list[i], "_night.pdf")
    }
    
    ggsave(paste(dir, pdfFileName, sep = ""), dpi = 250, width = 14.5, height = (ceiling(nSubs/3))*3, units = "in")
    
  }
}

colorList <- c("blue", "cyan", "green", "amber", "red")
colorList <- c( "cyan", "amber")
colorList <- c( "red", "blue")

if(FALSE){
  colorList <- c( "red", "blue")
  saveGraph("Day", colorList, daytime_eeg)
  
  
  colorList <- c("blue", "cyan", "green", "amber", "red")
  saveGraph("night", colorList, UpdatedEEG_4_30_18)
}
