#source("C:/Users/roohac/Documents/GitHub/CaffeineONR/runAnalysis.R")
library(ggsignif)
library(ggplot2)
library(nlme)
library(lsmeans)
library(ggplot2)
library(Rmisc)
library(MuMIn)
library(ReporteRs)
library(magrittr)
load("//root/projects/Caffeine_ONR_Study/performanceTestData/tables/outputList.RData")

add_plotSigs <- function(means, comparisons, x_str, y_str, include_fill){
  
  if(x_str == "Light"){
    
    means$lsmeans.light <- factor(means$lsmeans.light, levels = c("Red" , "Blue", "Dim" ))
    
  }
  
  
  if(include_fill ){
    gg <- ggplot(means, aes_string(x = colnames(means)[1], y = colnames(means)[2], fill = colnames(means)[1]))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none") +
      #coord_cartesian(ylim=c(.5,2))+
      labs(x=x_str , y = y_str) 
    
    if(colnames(means)[1] == "lsmeans.light"){
      
      #means$means <- factor(means$means, levels = c("Red Light" , "Blue Light", "Dim light" ))
      
      gg <- gg +  scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) 
      
    }
    if(colnames(means)[1] == "lsmeans.condition2"){
      gg <- gg +  scale_fill_manual(values=c( "gray100",  "gray40")) 
      
    }
  }else{
    gg <- ggplot(means, aes_string(x = colnames(means)[1], y = colnames(means)[2]))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none") +
      #coord_cartesian(ylim=c(.5,2))+
      labs(x=x_str , y = y_str) 
    
  }
  
  
  
  
  sig_start <- max(means$lsmeans.lsmean) 
  sig_inter <- max(means$lsmeans.SE) + (.5*max(means$lsmeans.SE)) 
  
  sig_nifComp <- comparisons[comparisons$contrasts.p.value < .05,]
  if(length(sig_nifComp$contrasts.contrast) > 0){
    for(i in 1:length(sig_nifComp$contrasts.contrast)){
      comparison_group <- strsplit(as.character(sig_nifComp$contrasts.contrast[i]), " - ")[[1]]
      
      gg <- gg + geom_signif(comparisons = list(c(comparison_group[1], comparison_group[2])), annotations=substr(as.character(sprintf("%.3f", round(sig_nifComp$contrasts.p.value[i], digits = 3))), 2, 5), y_position = sig_start + (i*sig_inter))
      
    }
  }
  
  
  return(gg)
}  



add_plotSigs2 <- function(means, comparisons, x_str, y_str){
  
  
  gg <- ggplot(means, aes_string(x = colnames(means)[1], y = colnames(means)[3]))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                  width=.2,                    
                  position=position_dodge(.9))+
    #scale_fill_manual(values=c("gray100", "gray80", "gray60", "gray40")) +
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
    theme(legend.position="none") +
    #coord_cartesian(ylim=c(.5,2))+
    labs(x=x_str , y = y_str) 
  
  
  sig_start <- max(means$lsmeans.lsmean) 
  sig_inter <- max(means$lsmeans.SE) + (.5*max(means$lsmeans.SE)) 
  
  sig_nifComp <- comparisons[comparisons$contrasts.p.value < .05,]
  if(length(sig_nifComp$contrasts.contrast) > 0){
    for(i in 1:length(sig_nifComp$contrasts.contrast)){
      comparison_group <- strsplit(as.character(sig_nifComp$contrasts.contrast[i]), " - ")[[1]]
      
      gg <- gg + geom_signif(comparisons = list(c(comparison_group[1], comparison_group[2])), annotations=substr(as.character(sprintf("%.3f", round(sig_nifComp$contrasts.p.value[i], digits = 3))), 2, 5), y_position = sig_start + (i*sig_inter))
      
    }
  }
  
  
  return(gg)
}  

colNum <- function(dataFrame){
  return(length(colnames(dataFrame)))
}

addLsmeans <- function(doc0, lsmeansComparisons, name, numCols){
  
  lsmeansComparisons$contrasts.contrast <- as.factor(lsmeansComparisons$contrasts.contrast)
  print(name)
  print(numCols)
  #lsmeansComparisons$contrasts.contrast <- revalue(lsmeansComparisons$contrasts.contrast, c("Active - Placebo" = "CSE - CSN"))
  if(numCols == 6){
    colnames(lsmeansComparisons)[1] <- "Compared groups"
    
    
    colnames(lsmeansComparisons)[4] <- "df"
    colnames(lsmeansComparisons)[5] <- "t"
    colnames(lsmeansComparisons)[6] <- "p"
    lsmeansComparisons$contrasts.estimate <- NULL
    lsmeansComparisons$contrasts.SE <- NULL
  }

  if(numCols == 7){
    colnames(lsmeansComparisons)[1] <- "Compared groups"
    colnames(lsmeansComparisons)[2] <- "Within groups"
    
    
    colnames(lsmeansComparisons)[5] <- "df"
    colnames(lsmeansComparisons)[6] <- "t"
    colnames(lsmeansComparisons)[7] <- "p"
    lsmeansComparisons$contrasts.estimate <- NULL
    lsmeansComparisons$contrasts.SE <- NULL
  }
  
  
  lsmeansComparisons$p <-  ifelse(lsmeansComparisons$p == 1, "1", 
                                  ifelse(!is.na(lsmeansComparisons$p), substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5), lsmeansComparisons$p))
  
  
  lsmeansComparisons$t <-  ifelse(!is.na(lsmeansComparisons$t), as.character(round(lsmeansComparisons$t, digits = 3)), lsmeansComparisons$t)
  
  
  
  
  
  doc0 = addParagraph( doc0,name, stylename = "Normal" )
  
  
  sigFTable2 = vanilla.table( data = lsmeansComparisons )
  if(numCols == 7){
    sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
  }
  if(numCols == 6){
    sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 4] = chprop( baseCellProp, background.color = "green")
    
  }  
  doc0 = addFlexTable(doc0, sigFTable2)
  
}

baseCellProp = cellProperties( padding = 0 )
doc = docx()


####"GNG hit rate"
doc <- addTitle( doc, outputList[[1]][[1]][[1]], level = 1 )

outputList[[1]][[1]][[1]]
outputList[[1]][[2]]

  name <- outputList[[1]][[2]][[1]][[1]]
  df <- outputList[[1]][[2]][[1]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "Mean hit rate", TRUE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
  name <- outputList[[1]][[2]][[2]][[1]]
  df <- outputList[[1]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "Mean GNG hit rate", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  




#"GNG false_positve"
doc <- addTitle( doc, outputList[[2]][[1]][[1]], level = 1 )
  
outputList[[2]][[1]][[1]]
outputList[[2]][[2]]

 name <- outputList[[2]][[2]][[1]][[1]]
 df <- outputList[[2]][[2]][[1]][[2]]
 
 
 lsmeans <- data.frame(summary(df)[1])
 lsmeansComparisons <- data.frame(summary(df)[2])
   addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
 
 gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "Mean false positve rate", TRUE)
 doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
 
 
 name <- outputList[[2]][[2]][[2]][[1]]
 df <- outputList[[2]][[2]][[2]][[2]]
 
 lsmeans <- data.frame(summary(df)[1])
 lsmeansComparisons <- data.frame(summary(df)[2])
   addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
 
 gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "Mean GNG false positve rate", FALSE)
 doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
 
 
 if(FALSE){
   name <- outputList[[2]][[2]][[2]][[1]]
   df <- outputList[[2]][[2]][[2]][[2]]
   
   lsmeans <- data.frame(summary(df)[1])
   lsmeansComparisons <- data.frame(summary(df)[2])
     addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
   
   
   gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "Mean GNG false positve rate", FALSE)
   doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
   
   
 }
 
 
#GNG response time
doc <- addTitle( doc, outputList[[3]][[1]][[1]], level = 1 )
 
outputList[[3]][[1]][[1]]
outputList[[3]][[2]]


  name <- outputList[[3]][[2]][[1]][[1]]
  df <- outputList[[3]][[2]][[1]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "Mean GNG response time (s)", TRUE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  

  name <- outputList[[3]][[2]][[2]][[1]]
  df <- outputList[[3]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Light", "Mean GNG response time (s)", TRUE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  



#outputList[[4]][[1]][[1]]
#outputList[[4]][[2]]
doc <- addTitle( doc, outputList[[5]][[1]][[1]], level = 1 )
outputList[[5]][[1]][[1]]
outputList[[5]][[2]]

  name <- outputList[[5]][[2]][[1]]
  df <- outputList[[5]][[2]][[2]]
  

  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "GNG bottom 10% response time (s)", TRUE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
doc <- addTitle( doc, outputList[[6]][[1]][[1]], level = 1 )
outputList[[6]][[1]][[1]]
outputList[[6]][[2]]
  


  name <- outputList[[6]][[2]][[1]][[1]]
  df <- outputList[[6]][[2]][[1]][[2]]
  
  

  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "GNG top 10% response time (s)", TRUE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  

  name <- outputList[[6]][[2]][[2]][[1]]
  df <- outputList[[6]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "GNG top 10% response time (s)", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  



outputList[[7]][[1]][[1]]

#outputList[[7]][[2]]

outputList[[8]][[1]][[1]]
#outputList[[8]][[2]]






outputList[[9]][[1]][[1]]
doc <- addTitle( doc, outputList[[9]][[1]][[1]], level = 1 )
outputList[[9]][[2]]

if(FALSE){
  name <- outputList[[9]][[2]][[1]]
  df <- outputList[[9]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "1-back mean accuracy", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
}
 
  

outputList[[10]][[1]][[1]]
doc <- addTitle( doc, outputList[[10]][[1]][[1]], level = 1 )

outputList[[10]][[2]]


outputList[[11]][[1]][[1]]
doc <- addTitle( doc, outputList[[11]][[1]][[1]], level = 1 )

outputList[[11]][[2]]
if(FALSE){
  name <- outputList[[11]][[2]][[1]]
  df <- outputList[[11]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "1-back mean accuracy", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
}




outputList[[12]][[1]][[1]]
doc <- addTitle( doc, outputList[[12]][[1]][[1]], level = 1 )

outputList[[12]][[2]]


  name <- outputList[[12]][[2]][[1]]
  df <- outputList[[12]][[2]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "1-back respone time",  FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  

  if(FALSE){
    name <- outputList[[12]][[2]][[1]][[2]][[1]]
    df <- outputList[[12]][[2]][[1]][[2]][[2]]
    
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
      addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
    
    gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "1-back respone time", FALSE)
    doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    
    
    name <- outputList[[12]][[2]][[2]][[1]]
    df <- outputList[[12]][[2]][[2]][[2]]
    
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
      addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
    
    gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "1-back respone time", FALSE)
    doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    
  }
 






outputList[[13]][[1]][[1]]
#outputList[[13]][[2]]





outputList[[14]][[1]][[1]]
doc <- addTitle( doc, outputList[[14]][[1]][[1]], level = 1 )
outputList[[14]][[2]]

  
  name <- outputList[[14]][[2]][[1]]
  df <- outputList[[14]][[2]][[2]]

  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))

  
  #add_plotSigs2(lsmeans, lsmeansComparisons, "Light:Test period", "2-back mean accuracy")
  ########******* ###********** fix

  gg0 <- ggplot(lsmeans, aes(x = lsmeans.test_period, y = lsmeans.lsmean, fill = lsmeans.light))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                  width=.2,                    
                  position=position_dodge(.9))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
    theme(legend.position="none") +
    scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) +
    labs(x="Period" , y = "2-back mean accuracy") 
    #geom_signif(comparisons = list(c("Caffe:Blue", "Place:Blue")), annotations=".017" )
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
outputList[[15]][[1]][[1]]
doc <- addTitle( doc, outputList[[15]][[1]][[1]], level = 1 )
outputList[[15]][[2]]

if(FALSE){
  name <- outputList[[15]][[2]][[1]]
  df <- outputList[[15]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "2-back correct matches", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
}
  
  

outputList[[16]][[1]][[1]]
doc <- addTitle( doc, outputList[[16]][[1]][[1]], level = 1 )
outputList[[16]][[2]]
  if(FALSE){
    name <- outputList[[16]][[2]][[1]][[1]]
    df <- outputList[[16]][[2]][[1]][[2]]
    
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
      addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
    
    gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "2-back correct no-matches", FALSE)
    doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
  }
  
  
  name <- outputList[[16]][[2]][[1]]
  df <- outputList[[16]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  

  gg0 <- ggplot(lsmeans, aes(x = lsmeans.condition2, y = lsmeans.lsmean, fill = lsmeans.light))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                  width=.2,                    
                  position=position_dodge(.9))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
    theme(legend.position="none") +
    scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) +
    labs(x="Caffeine" , y = "2-back mean correct no-matches") +
    geom_signif(comparisons = list(c("Caffe", "Place")), annotations=".074" )
  
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
outputList[[17]][[1]][[1]]
doc <- addTitle( doc, outputList[[17]][[1]][[1]], level = 1 )
outputList[[17]][[2]]



  name <- outputList[[17]][[2]][[1]][[1]]
  df <- outputList[[17]][[2]][[1]][[2]]
  
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "2-back response time",  TRUE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
  
  name <- outputList[[17]][[2]][[2]][[1]]
  df <- outputList[[17]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "2-back response time", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  




outputList[[18]][[1]][[1]]
#outputList[[18]][[2]]


outputList[[19]][[1]][[1]]
doc <- addTitle( doc, outputList[[19]][[1]][[1]], level = 1 )
outputList[[19]][[2]]
  if(FALSE){
    name <- outputList[[19]][[2]][[1]]
    df <- outputList[[19]][[2]][[2]]
    
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
      addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
    
    gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "MOB 1-back mean accuracy", FALSE)
    doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    
  }
 


outputList[[20]][[1]][[1]]
doc <- addTitle( doc, outputList[[20]][[1]][[1]], level = 1 )

outputList[[20]][[2]]

if(FALSE){
  name <- outputList[[20]][[2]][[1]]
  df <- outputList[[20]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "MOB 1-back mean correct matches", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
}



outputList[[21]][[1]][[1]]
doc <- addTitle( doc, outputList[[21]][[1]][[1]], level = 1 )

outputList[[21]][[2]]

if(FALSE){
  name <- outputList[[21]][[2]][[1]]
  df <- outputList[[21]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "MOB 1-back mean correct no-matches", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
  
  
}
 
outputList[[22]][[1]][[1]]
doc <- addTitle( doc, outputList[[22]][[1]][[1]], level = 1 )

outputList[[22]][[2]]

  name <- outputList[[22]][[2]][[1]][[1]]
  df <- outputList[[22]][[2]][[1]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "MOB 1-back response time",  TRUE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
  name <- outputList[[22]][[2]][[2]][[1]]
  df <- outputList[[22]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
    addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
  
  gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "MOB 1-back response time", FALSE)
  doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
  
  
  if(FALSE){
    name <- outputList[[2]][[2]][[2]][[1]]
    df <- outputList[[2]][[2]][[2]][[2]]
    
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
      addLsmeans(doc,lsmeansComparisons, name, colNum(lsmeansComparisons))
    
    gg0 <- add_plotSigs(lsmeans, lsmeansComparisons, "Session", "MOB 1-back response time", FALSE)
    doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    
    
  }
  


#outputList[[23]][[1]][[1]]
#length(outputList[[23]][[2]])


# write the doc
dir <- "//root/projects/Caffeine_ONR_Study/output-MarkdownFiles/results-output/"
filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "Caffeine-PostHoc.docx")







writeDoc( doc, file = filename )

  

