library(readxl)
library(reshape2)
library(data.table)
library(ggsignif)
Caffeine_Biomarkers <- read_excel("//root/projects/Caffeine_ONR_Study/bio-markers/Caffeine_Biomarkers_long_format2.xlsx")


KSS_data<- read_excel("//root/projects/Caffeine_ONR_Study/KSS/KSS_data_long_format.xlsx")

if(FALSE){
  # 1. Normalize data
  # 2. split condition function into two 
  # 3. melt the Caffeine_Biomarkers
  # 4. split the varibale facrots into tow
  Caffeine_Biomarkers$Cortisol_n2 <- Caffeine_Biomarkers$Cortisol_2/Caffeine_Biomarkers$Cortisol_1
  Caffeine_Biomarkers$Cortisol_n3 <- Caffeine_Biomarkers$Cortisol_3/Caffeine_Biomarkers$Cortisol_1
  Caffeine_Biomarkers$Cortisol_n4 <- Caffeine_Biomarkers$Cortisol_4/Caffeine_Biomarkers$Cortisol_1
  Caffeine_Biomarkers$Cortisol_n5 <- Caffeine_Biomarkers$Cortisol_5/Caffeine_Biomarkers$Cortisol_1
  Caffeine_Biomarkers$Cortisol_n6 <- Caffeine_Biomarkers$Cortisol_6/Caffeine_Biomarkers$Cortisol_1
  Caffeine_Biomarkers$Cortisol_n7 <- Caffeine_Biomarkers$Cortisol_7/Caffeine_Biomarkers$Cortisol_1
  
  Caffeine_Biomarkers$Melatonin_n2 <- Caffeine_Biomarkers$Melatonin_2/Caffeine_Biomarkers$Melatonin_1
  Caffeine_Biomarkers$Melatonin_n3 <- Caffeine_Biomarkers$Melatonin_3/Caffeine_Biomarkers$Melatonin_1
  Caffeine_Biomarkers$Melatonin_n4 <- Caffeine_Biomarkers$Melatonin_4/Caffeine_Biomarkers$Melatonin_1
  Caffeine_Biomarkers$Melatonin_n5 <- Caffeine_Biomarkers$Melatonin_5/Caffeine_Biomarkers$Melatonin_1
  Caffeine_Biomarkers$Melatonin_n6 <- Caffeine_Biomarkers$Melatonin_6/Caffeine_Biomarkers$Melatonin_1
  Caffeine_Biomarkers$Melatonin_n7 <- Caffeine_Biomarkers$Melatonin_7/Caffeine_Biomarkers$Melatonin_1
  
  KSS_data$KSS_n2 <- KSS_data$KSS_2/KSS_data$KSS_1
  KSS_data$KSS_n3 <- KSS_data$KSS_3/KSS_data$KSS_1
  KSS_data$KSS_n4 <- KSS_data$KSS_4/KSS_data$KSS_1
  KSS_data$KSS_n5 <- KSS_data$KSS_5/KSS_data$KSS_1
  KSS_data$KSS_n6 <- KSS_data$KSS_6/KSS_data$KSS_1
  KSS_data$KSS_n7 <- KSS_data$KSS_7/KSS_data$KSS_1
  
  KSS_data$KSS_1 <- NULL
  KSS_data$KSS_2 <- NULL
  KSS_data$KSS_3 <- NULL
  KSS_data$KSS_4 <- NULL
  KSS_data$KSS_5 <- NULL
  KSS_data$KSS_6 <- NULL
  KSS_data$KSS_7 <- NULL
  
  
  
  KSS_data$condition2 <- substr(KSS_data$Condition, 5, 9 )
  KSS_data$condition2 <- ifelse(KSS_data$condition2 == "/Caff", "Caffe",  ifelse(KSS_data$condition2 == "/Plac", "Place", KSS_data$condition2 ))
  KSS_data$light <- substr(KSS_data$Condition, 1, 4 )
  
  KSS_data$light <- gsub( "/", "", KSS_data$light)
  KSS_data$Subject <- as.vector(as.list(KSS_data$Subject))
  KSS_data$Week <- as.vector(as.list(KSS_data$Week))
  KSS_data$Condition <- as.vector(as.list(KSS_data$Condition))
  KSS_data$condition2 <- as.vector(as.list(KSS_data$condition2))
  KSS_data$light <- as.vector(as.list(KSS_data$light))
  
  setDT(KSS_data)
  
  KSS_data2 <- melt(KSS_data, id=c("Subject","Week", "Condition", "condition2", "light"))
  
  
  library(stringr)
  KSS_data3 <- cbind( KSS_data2, str_split_fixed(KSS_data2$variable, "_", 2))
  
  KSS_data3$variable <- NULL
  
  colnames(KSS_data3)[7] <- "Variable"
  colnames(KSS_data3)[8] <- "sample"
  
  KSS_data3$dataType <- "norm"
  
  KSS_data4 <- KSS_data3
  KSS_data4$Subject <-unlist(KSS_data4$Subject)
  KSS_data4$Week <-unlist(KSS_data4$Week)
  KSS_data4$Condition <-unlist(KSS_data4$Condition)
  KSS_data4$condition2 <-unlist(KSS_data4$condition2)
  KSS_data4$light <-unlist(KSS_data4$light)
  
  KSS_data4 <- data.frame(KSS_data4)
  
  ###Set each to correct factors
  
  KSS_data4$Subject <- as.factor(KSS_data4$Subject)
  KSS_data4$Week <- as.factor(KSS_data4$Week)
  KSS_data4$Condition <- as.factor(KSS_data4$Condition)
  KSS_data4$condition2 <- as.factor(KSS_data4$condition2)
  KSS_data4$light <- as.factor(KSS_data4$light)
  KSS_data4$sample <- as.factor(KSS_data4$sample)
  
  Caffeine_Biomarkers$condition2 <- substr(Caffeine_Biomarkers$Condition, 5, 9 )
  Caffeine_Biomarkers$condition2 <- ifelse(Caffeine_Biomarkers$condition2 == "/Caff", "Caffe",  ifelse(Caffeine_Biomarkers$condition2 == "/Plac", "Place", Caffeine_Biomarkers$condition2 ))
  Caffeine_Biomarkers$light <- substr(Caffeine_Biomarkers$Condition, 1, 4 )
  
  Caffeine_Biomarkers$light <- gsub( "/", "", Caffeine_Biomarkers$light)
  
  Caffeine_Biomarkers$Subject <- as.vector(as.list(Caffeine_Biomarkers$Subject))
  Caffeine_Biomarkers$Week <- as.vector(as.list(Caffeine_Biomarkers$Week))
  Caffeine_Biomarkers$Condition <- as.vector(as.list(Caffeine_Biomarkers$Condition))
  Caffeine_Biomarkers$condition2 <- as.vector(as.list(Caffeine_Biomarkers$condition2))
  Caffeine_Biomarkers$light <- as.vector(as.list(Caffeine_Biomarkers$light))
  
  setDT(Caffeine_Biomarkers)
  
  
  Caffeine_Biomarkers2 <- melt(Caffeine_Biomarkers, id=c("Subject","Week", "Condition", "condition2", "light"))
  
  library(stringr)
  Caffeine_Biomarkers3 <- cbind( Caffeine_Biomarkers2, str_split_fixed(Caffeine_Biomarkers2$variable, "_", 2))
  
  Caffeine_Biomarkers3$variable <- NULL
  
  colnames(Caffeine_Biomarkers3)[7] <- "Variable"
  colnames(Caffeine_Biomarkers3)[8] <- "sample"
  
  Caffeine_Biomarkers3$dataType <- ifelse(substr(Caffeine_Biomarkers3$sample, 1, 1) == "n", "norm", "raw")
  
  Caffeine_Biomarkers4 <- Caffeine_Biomarkers3
  Caffeine_Biomarkers4$Subject <-unlist(Caffeine_Biomarkers4$Subject)
  Caffeine_Biomarkers4$Week <-unlist(Caffeine_Biomarkers4$Week)
  Caffeine_Biomarkers4$Condition <-unlist(Caffeine_Biomarkers4$Condition)
  Caffeine_Biomarkers4$condition2 <-unlist(Caffeine_Biomarkers4$condition2)
  Caffeine_Biomarkers4$light <-unlist(Caffeine_Biomarkers4$light)
  
  Caffeine_Biomarkers4 <- data.frame(Caffeine_Biomarkers4)
  
  ###Set each to correct factors
  
  Caffeine_Biomarkers4$Subject <- as.factor(Caffeine_Biomarkers4$Subject)
  Caffeine_Biomarkers4$Week <- as.factor(Caffeine_Biomarkers4$Week)
  Caffeine_Biomarkers4$Condition <- as.factor(Caffeine_Biomarkers4$Condition)
  Caffeine_Biomarkers4$condition2 <- as.factor(Caffeine_Biomarkers4$condition2)
  Caffeine_Biomarkers4$light <- as.factor(Caffeine_Biomarkers4$light)
  Caffeine_Biomarkers4$sample <- as.factor(Caffeine_Biomarkers4$sample)
  
  
  ###creat 4 data sets raw or norm * cortisol or melontonin
  
  
  melatonin_raw <- subset(Caffeine_Biomarkers4, Variable == "Melatonin" & dataType == "raw" & !is.na(Subject))
  
  melatonin_norm <- subset(Caffeine_Biomarkers4, Variable == "Melatonin" & dataType == "norm" & !is.na(Subject))
  
  cort_raw <- subset(Caffeine_Biomarkers4, Variable == "Cortisol" & dataType == "raw" & !is.na(Subject))
  
  cort_norm <- subset(Caffeine_Biomarkers4, Variable == "Cortisol" & dataType == "norm" & !is.na(Subject))
  
  KSS_norm <- subset(KSS_data4, Variable == "KSS" & dataType == "norm" & !is.na(Subject))
  
  
  ###Mixed models, post hoc and graphs
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  library(officer)
  library(magrittr)
  
  KSS_norm$uni3 <- paste(KSS_norm$Subject, KSS_norm$Condition, sep = "_")
  
  #uni3 != "s714_Blue Light/Caffeine" & uni3 != "s714_Blue Light/Placebo" & Subject_id != "s711" & uni3 != "s712_Red Light/Placebo"
  #KSS_norm[KSS_norm$Subject != 708 & KSS_norm$uni3 != "712_Red/Placebo" & KSS_norm$uni3 != "714_Blue/Caffeine" & KSS_norm$uni3 != "714_Blue/Placebo",]
  
  KSS_norm2 <- KSS_norm[KSS_norm$Subject != 712,]
  
  kss_model <- lme(value ~ condition2 * light * sample , random = ~1|Subject/condition2/light/sample,
                   data=KSS_norm2)
  
  anova(kss_model)
  #emmeans::emmeans(kss_model, pairwise~ light:condition2, adjust="bonferroni")
  emmeans::emmeans(kss_model, pairwise~ sample,lmer.df = "satterthwaite"
                   , adjust="bonferroni")
  
  emmeans::emmeans(kss_model, pairwise~ sample|light, adjust="bonferroni")
  ###t-tests:
  #1
  kss_data_tTest <- aggregate(value ~ Subject * light, data = KSS_norm[KSS_norm$Subject != 712,], FUN = mean)
  
  t.test(value ~ light, data = kss_data_tTest[kss_data_tTest$light != "Blue",], var.equal = TRUE)
  t.test(value ~ light, data = kss_data_tTest[kss_data_tTest$light != "Red",], var.equal = TRUE)
  t.test(value ~ light, data = kss_data_tTest[kss_data_tTest$light != "Dim",], var.equal = TRUE)
  
  #2
  KSS_norm$light_sample <- paste(KSS_norm$light, KSS_norm$sample, sep = "_")
  kss_data_tTest2 <- aggregate(value ~ Subject * light_sample * light * sample, data = KSS_norm[KSS_norm$Subject != 708,], FUN = mean)
  
  table <- performTtests0(kss_data_tTest2, "na", "bonferroni")
  sampleList <- unique(kss_data_tTest2$sample)
  for(i in sampleList){
    print(i)
    currTestData <- subset(kss_data_tTest2, sample == i)
    print("Red vs. Dim:")
    t1 <- t.test(value ~ light, data = currTestData[currTestData$light != "Blue",], paired = TRUE)
    print(t1)
    print("Blue vs. Dim:")
    t2 <- t.test(value ~ light, data = currTestData[currTestData$light != "Red",], paired = TRUE)
    print(t2)
    print("Red vs. Blue")
    t3 <- t.test(value ~ light, data = currTestData[currTestData$light != "Dim",], paired = TRUE)
    print(t3)
    print("-----------------------------------------------")
    
  }
  
  
  ###unpaired test 
  
  #2
  KSS_norm$light_sample <- paste(KSS_norm$light, KSS_norm$sample, sep = "_")
  kss_data_tTest2 <- aggregate(value ~ Subject * sample * light * condition2, data = KSS_norm, FUN = mean)
  
  table <- performTtests0(kss_data_tTest2, "na", "bonferroni")
  
  
  
  
  
  summarized_data_kssInteraction <- summarySE(measurevar = "value", groupvars = c("light", "sample"), data = kss_data_tTest2)
  
  ggplot(summarized_data_kssInteraction, aes(x = sample, y = value, colour = light, group = light))+
    geom_point()+
    geom_line()+
    
    geom_errorbar(aes(ymin=value - se, ymax=value + se),
                  width=.2                    
                  ,position=position_dodge(.1))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    labs(x="", y ="KSS") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
    scale_colour_manual (values=c( "deepskyblue4", "gray80", "red4" )) 
  
  
  summarized_data_kssInteraction2 <- summarySE(measurevar = "value", groupvars = c("light", "sample"), data = kss_data_tTest2)
  
  ggplot(summarized_data_kssInteraction2, aes(x = sample, y = value, colour = light, group = light))+
    geom_point()+
    geom_line()+
    
    geom_errorbar(aes(ymin=value - se, ymax=value + se),
                  width=.2                    
                  ,position=position_dodge(.1))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    labs(x="", y ="KSS") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
    scale_colour_manual (values=c( "deepskyblue4", "gray80", "red4" )) 
  
  melatonin_raw_model <- lme(value ~ condition2 * light * sample , random = ~1|Subject/condition2/light/sample,
                             data=melatonin_raw)
  
  anova(melatonin_raw_model)
  
  
  
  melatonin_norm_model <- lme(value ~ condition2 * light * sample , random = ~1|Subject/condition2/light/sample,
                              data=melatonin_norm)
  
  melatonin_norm_modelpvals <- anova(melatonin_norm_model)
  
  
  
  
  
  
  cortisol_norm_model <- lme(value ~ condition2 * light * sample , random = ~1|Subject/condition2/light/sample,
                             data=cort_norm)
  
  anova(cortisol_norm_model)
  
  
  
  cortisol_raw_model <- lme(value ~ condition2 * light * sample , random = ~1|Subject/condition2/light/sample,
                            data=cort_raw)
  
  anova(cortisol_raw_model)
  
  
  
  
  KSS_norm2_sample <- aggregate(value ~ Subject * sample, data = KSS_norm2, FUN = mean)
  summarized_KSS_norm2_sample <- summarySE(measurevar = "value", groupvars = c("sample"), data = KSS_norm2_sample)
  
  ggplot(summarized_KSS_norm2_sample, aes(x = sample, y = value))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=value - se, ymax=value + se),
                  width=.2                    
                  ,position=position_dodge(.1))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    labs(x="", y ="KSS") 
  
  
  cort_norm_sample <- aggregate(value ~ Subject * sample, data = cort_norm, FUN = mean)
  summarized_cort_norm_sample <- summarySE(measurevar = "value", groupvars = c("sample"), data = cort_norm_sample)
  
  ggplot(summarized_cort_norm_sample, aes(x = sample, y = value))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=value - se, ymax=value + se),
                  width=.2                    
                  ,position=position_dodge(.1))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    labs(x="", y ="cort") 
}

outputWordAnalysis <- function(currTitle, mixedmodel, modelData, doc){
  #textProp <- textProperties()
  
  
  library(MuMIn)
  #library(ReporteRs)
  library(magrittr)
 # baseCellProp = cellProperties( padding = 0 )
  #doc = addTitle( doc, currTitle, level = 1 )
  doc <- doc %>%
    body_add_par(currTitle, style = "heading 1") 
  
  #make word table
  currDF <-  setDT(anova(mixedmodel), keep.rownames = TRUE)[]
  currDF <- currDF[2:length(currDF$p),]
  
  sigList <- currDF[currDF$p < .05,]$rn
  
  colnames(currDF)[1] <- paste( modelData$Variable[1],modelData$dataType[1], "- dependent measures", sep = " ")
  
  colnames(currDF)[2] <- "df"
  colnames(currDF)[3] <- "Error"
  colnames(currDF)[4] <- "F"
  colnames(currDF)[5] <- "p"
  
  
  #currDF$p <-  ifelse(!is.na(currDF$p),ifelse(round(currDF$p, digits = 3) == 1, "1",  substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5)) )
  
  
  currDF$p <- ifelse(round(currDF$p, digits = 3) == 1, "1", ifelse(round(currDF$p, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5) ) )  
  
  currDF$F <-  ifelse(!is.na(currDF$F), as.character(round(currDF$F, digits = 3)), currDF$F)
  
  currDF$Error <- ifelse(is.na(currDF$Error), "", currDF$Error)
  currDF$df <- ifelse(is.na(currDF$df), "", currDF$df)
  
  
  #MyFTable = vanilla.table( data = currDF )
  
  
  #MyFTable[as.numeric(currDF$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
  #MyFTable[as.numeric(substr(currDF$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
  
  doc <- doc %>%
    body_add_table(currDF, style = "table_template")
  #doc = addFlexTable(doc, MyFTable)
  
  if('condition2:light:sample' %in% sigList){
    df <- lsmeans(mixedmodel, pairwise~ condition2:light:sample, adjust="bonferroni")
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
    lsmeansComparisons2 <- lsmeansComparisons
  }
  
  if('condition2' %in% sigList){
    df <- lsmeans(mixedmodel, pairwise~ condition2, adjust="bonferroni")
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
    lsmeansComparisons2 <- lsmeansComparisons
    lsmeansComparisons$contrasts.contrast <- as.factor(lsmeansComparisons$contrasts.contrast)
    
    #lsmeansComparisons$contrasts.contrast <- revalue(lsmeansComparisons$contrasts.contrast, c("Active - Placebo" = "CSE - CSN"))
    
    colnames(lsmeansComparisons)[1] <- "Compared groups"
    colnames(lsmeansComparisons)[5] <- "t"
    colnames(lsmeansComparisons)[6] <- "p"      
    colnames(lsmeansComparisons)[4] <- "df"
    lsmeansComparisons$contrasts.estimate <- NULL
    lsmeansComparisons$contrasts.SE <- NULL

    lsmeansComparisons$p <-  ifelse(lsmeansComparisons$p == 1, "1", 
                               ifelse(!is.na(lsmeansComparisons$p), substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5), lsmeansComparisons$p))
    
    
    lsmeansComparisons$t <-  ifelse(!is.na(lsmeansComparisons$t), as.character(round(lsmeansComparisons$t, digits = 3)), lsmeansComparisons$t)
    
    
    

    
   #doc = addParagraph( doc,'condition2', stylename = "Normal" )
    doc <- doc  %>% 
      body_add_par(value = 'condition2', style = "centered") 
    
    #sigFTable2 = vanilla.table( data = lsmeansComparisons )
    #sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 4] = chprop( baseCellProp, background.color = "green")
    #doc = addFlexTable(doc, sigFTable2)
    
    #gg0 <- add_plotSigs(lsmeans, lsmeansComparisons2, "condition2", paste( modelData$Variable[1],modelData$dataType[1], sep = " "), TRUE)
    
    #doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    
    
    
    #Add t-tests to document
    newData <- aggregate(value ~ Subject + condition2, data = modelData, FUN = mean)
    performTtests(newData, doc, "Caffeine", "none")
  }
  
  if('light' %in% sigList){
    df <- lsmeans(mixedmodel, pairwise~ light, adjust="bonferroni")
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
    lsmeansComparisons2 <- lsmeansComparisons
    lsmeansComparisons$contrasts.contrast <- as.factor(lsmeansComparisons$contrasts.contrast)
    
    #lsmeansComparisons$contrasts.contrast <- revalue(lsmeansComparisons$contrasts.contrast, c("Active - Placebo" = "CSE - CSN"))
    
    colnames(lsmeansComparisons)[1] <- "Compared groups"
    
    colnames(lsmeansComparisons)[5] <- "t"
    colnames(lsmeansComparisons)[6] <- "p"      
    colnames(lsmeansComparisons)[4] <- "df"
    lsmeansComparisons$contrasts.estimate <- NULL
    lsmeansComparisons$contrasts.SE <- NULL

    lsmeansComparisons$p <-  ifelse(lsmeansComparisons$p == 1, "1", 
                                    ifelse(!is.na(lsmeansComparisons$p), substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5), lsmeansComparisons$p))
    
    
    lsmeansComparisons$t <-  ifelse(!is.na(lsmeansComparisons$t), as.character(round(lsmeansComparisons$t, digits = 3)), lsmeansComparisons$t)
    
    
    
    
    
    #doc = addParagraph( doc,'light', stylename = "Normal" )
    doc <- doc %>% 
      body_add_par(value = 'light', style = "centered") 
    
    #sigFTable2 = vanilla.table( data = lsmeansComparisons )
    #sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 4] = chprop( baseCellProp, background.color = "green")
    #doc = addFlexTable(doc, sigFTable2)
    
    #gg0 <- add_plotSigs(lsmeans, lsmeansComparisons2, "light", paste( modelData$Variable[1],modelData$dataType[1], sep = " "), TRUE)
    
    #doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    
    #Add t-tests to document
    newData <- aggregate(value ~ Subject + light, data = modelData, FUN = mean)
    performTtests(newData, doc, "light", "bonferroni")
    
  }
  
  
  if('sample' %in% sigList){
    if(FALSE){
      subject_sample <- aggregate(value ~ sample * Subject, data = modelData, FUN = mean)
      summary_test <- summarySE(data = subject_sample, measurevar = c("value"), groupvars = "sample")
      pairwise.t.test(subject_sample$value , subject_sample$sample, p.adjust.method = "bonferroni", paired = TRUE)
    }
    
    df <- lsmeans(mixedmodel, pairwise~ sample, adjust="bonferroni")
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
    lsmeansComparisons2 <- lsmeansComparisons
    lsmeansComparisons$contrasts.contrast <- as.factor(lsmeansComparisons$contrasts.contrast)
    
    #lsmeansComparisons$contrasts.contrast <- revalue(lsmeansComparisons$contrasts.contrast, c("Active - Placebo" = "CSE - CSN"))
    
    colnames(lsmeansComparisons)[1] <- "Compared groups"
    
    colnames(lsmeansComparisons)[5] <- "t"
    colnames(lsmeansComparisons)[6] <- "p"     
    colnames(lsmeansComparisons)[4] <- "df"
    lsmeansComparisons$contrasts.estimate <- NULL
    lsmeansComparisons$contrasts.SE <- NULL

    lsmeansComparisons$p <-  ifelse(lsmeansComparisons$p == 1, "1", 
                                    ifelse(!is.na(lsmeansComparisons$p), substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5), lsmeansComparisons$p))
    
    
    lsmeansComparisons$t <-  ifelse(!is.na(lsmeansComparisons$t), as.character(round(lsmeansComparisons$t, digits = 3)), lsmeansComparisons$t)
    
    
    
    
    
    #doc = addParagraph( doc,'sample', stylename = "Normal" )
    doc <- doc  %>% 
      body_add_par(value = 'sample', style = "centered") 
    
    #sigFTable2 = vanilla.table( data = lsmeansComparisons )
   # sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 4] = chprop( baseCellProp, background.color = "green")
    #doc = addFlexTable(doc, sigFTable2)
    
    #gg0 <- add_plotSigs(lsmeans, lsmeansComparisons2, "sample", paste( modelData$Variable[1],modelData$dataType[1], sep = " "), FALSE)
    
    #doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    
    #Add t-tests to document
    newData <- aggregate(value ~ Subject + sample, data = modelData, FUN = mean)
    performTtests(newData, doc, "sample", "bonferroni")
    
  }
  
  if('Week' %in% sigList){
    df <- lsmeans(mixedmodel, pairwise~ Week, adjust="bonferroni")
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
    lsmeansComparisons2 <- lsmeansComparisons
    lsmeansComparisons$contrasts.contrast <- as.factor(lsmeansComparisons$contrasts.contrast)
    
    #lsmeansComparisons$contrasts.contrast <- revalue(lsmeansComparisons$contrasts.contrast, c("Active - Placebo" = "CSE - CSN"))
    
    colnames(lsmeansComparisons)[1] <- "Compared groups"
    
    colnames(lsmeansComparisons)[5] <- "t"
    colnames(lsmeansComparisons)[6] <- "p"      
    colnames(lsmeansComparisons)[4] <- "df"
    lsmeansComparisons$contrasts.estimate <- NULL
    lsmeansComparisons$contrasts.SE <- NULL

    lsmeansComparisons$p <-  ifelse(lsmeansComparisons$p == 1, "1", 
                                    ifelse(!is.na(lsmeansComparisons$p), substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5), lsmeansComparisons$p))
    
    
    lsmeansComparisons$t <-  ifelse(!is.na(lsmeansComparisons$t), as.character(round(lsmeansComparisons$t, digits = 3)), lsmeansComparisons$t)
    
    
    
    
    
    #doc = addParagraph( doc,'Week', stylename = "Normal" )
    doc <- doc  %>% 
      body_add_par(value = 'Week', style = "centered") 
    
   # sigFTable2 = vanilla.table( data = lsmeansComparisons )
    #sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 4] = chprop( baseCellProp, background.color = "green")
    #doc = addFlexTable(doc, sigFTable2)
    
    #gg0 <- add_plotSigs(lsmeans, lsmeansComparisons2, "Week", paste( modelData$Variable[1],modelData$dataType[1], sep = " "), FALSE)
    
    #doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    
    #Add t-tests to document
    newData <- aggregate(value ~ Subject + Week, data = modelData, FUN = mean)
    performTtests(newData, doc, "Week", "bonferroni")
    
  }
  
  if('light:sample' %in% sigList){
    df <- lsmeans(mixedmodel, pairwise~ light:sample, adjust="bonferroni")
    lsmeans <- data.frame(summary(df)[1])
    lsmeansComparisons <- data.frame(summary(df)[2])
    lsmeansComparisons2 <- lsmeansComparisons
    lsmeansComparisons$contrasts.contrast <- as.factor(lsmeansComparisons$contrasts.contrast)
    
    #lsmeansComparisons$contrasts.contrast <- revalue(lsmeansComparisons$contrasts.contrast, c("Active - Placebo" = "CSE - CSN"))
    
    colnames(lsmeansComparisons)[1] <- "Compared groups"
    

    lsmeansComparisons$contrasts.estimate <- NULL
    lsmeansComparisons$contrasts.SE <- NULL
    colnames(lsmeansComparisons)[2] <- "df"
    colnames(lsmeansComparisons)[3] <- "t"
    colnames(lsmeansComparisons)[4] <- "p"
    
    lsmeansComparisons$p <-  ifelse(lsmeansComparisons$p == 1, "1", 
                                    ifelse(!is.na(lsmeansComparisons$p), substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5), lsmeansComparisons$p))
    
    
    lsmeansComparisons$t <-  ifelse(!is.na(lsmeansComparisons$t), as.character(round(lsmeansComparisons$t, digits = 3)), lsmeansComparisons$t)
    
    
    
    
    
    #doc = addParagraph( doc,'light X sample', stylename = "Normal" )
    doc <- doc  %>% 
      body_add_par(value = 'Liht X Sample', style = "centered") 
    
    #sigFTable2 = vanilla.table( data = lsmeansComparisons )
    #sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 4] = chprop( baseCellProp, background.color = "green")
    #doc = addFlexTable(doc, sigFTable2)
    
    #gg0 <- add_plotSigs(lsmeans, lsmeansComparisons2, "light:sample", paste( modelData$Variable[1],modelData$dataType[1], sep = " "), FALSE)
    #doc <- addPlot(doc = doc, fun = print, x = gg0, vector.graphic = TRUE, width = 4, height = 3)
    
    gg1 <- add_plotSigs(lsmeans, lsmeansComparisons2, "light:sample_line", paste( modelData$Variable[1],modelData$dataType[1], sep = " "), FALSE)
    #doc <- addPlot(doc = doc, fun = print, x = gg1, vector.graphic = TRUE, width = 4, height = 3)
    
    
    #Add t-tests to document
    modelData$condition3 <- paste(modelData$light,",", modelData$sample, sep= "")
    newData <- aggregate(value ~ Subject + condition3, data = modelData, FUN = mean)
    performTtests(newData, doc, "light:sample", "bonferroni")
    
    
  }

  
 }


performTtests <- function(Data, doc0, x_str, Correction){
  
  removeDim <- TRUE
  removeRedBluecompare <- TRUE
  
 # Correction <- "none"
  
  ComparedGroups <- c()
  df <- c()
  t <- c()
  p <- c()
  
  colnames(Data)[1] <- "Subject"
  colnames(Data)[2] <- "condition"
  if(x_str == "light"){
    
    Data$condition <- factor(Data$condition, levels = c("Red" , "Blue", "Dim" ))
    removeRedBluecompare <- FALSE
    
  }
  #get comparisons
  testModel<- lme(value ~ condition, random = ~1|Subject,
                  data=Data)
  
  df5 <- lsmeans(testModel, pairwise~ condition, adjust=Correction, data = Data)
  lsmeansComparisons3 <- data.frame(summary(df5)[2])
  comparistonList <- strsplit(as.character(lsmeansComparisons3$contrasts.contrast), " - ")
  ###
  
  for(i in 1:length(comparistonList)){
    orignialData <- Data[Data$condition == comparistonList[[i]][1] | Data$condition == comparistonList[[i]][2] ,]
    
    subList001 <- intersect(Data[Data$condition == comparistonList[[i]][1],]$Subject , Data[Data$condition == comparistonList[[i]][2],]$Subject)
    cleanedData <- orignialData[orignialData$Subject %in% subList001, ]
    
    curr_Ttest <- t.test(value ~ condition, data = cleanedData, paired =  TRUE)
    ComparedGroups <- c(ComparedGroups, paste(comparistonList[[i]][1], comparistonList[[i]][2], sep = " - "  ))
    df <- c(df, as.numeric(curr_Ttest$parameter))
    t <- c(t, as.numeric(curr_Ttest$statistic))
    p <- c(p, curr_Ttest$p.value)
    
  }
  t_test_table <- data.frame(ComparedGroups, df, t, p )
  t_test_table$p <- as.numeric(as.character(t_test_table$p))
  

  
  if(removeDim){
    
    t_test_table$number<- str_count(t_test_table$ComparedGroups, "Dim")
    
    t_test_table <- subset(t_test_table, number < 2)
    t_test_table$number <- NULL
  }
  
  
  if(removeRedBluecompare){
    
    t_test_table$Rednumber<- str_count(t_test_table$ComparedGroups, "Red")
    t_test_table$Bluenumber<- str_count(t_test_table$ComparedGroups, "Blue")
    t_test_table$RedBluecompare <- ifelse(t_test_table$Rednumber == 1 & t_test_table$Bluenumber == 1, 1, 0)
    t_test_table <- subset(t_test_table, RedBluecompare == 0)
    t_test_table$Rednumber <- NULL
    t_test_table$Bluenumber <- NULL
    t_test_table$RedBluecompare <- NULL
    
  }
  
  
  
  ###p-value-correction 
  if(Correction != "None" | Correction != "none"){
    t_test_table$p <- p.adjust(t_test_table$p, method = Correction, n = length(t_test_table$p) )
    tableStatement <- paste("P value adjustment: ",Correction, " method for ", length(t_test_table$p), " tests", sep =""  )
  }else{
    tableStatement <- "No P-value adjustment applied"
    
  }
  t_test_table2 <- t_test_table
  
  t_test_table$t <- as.numeric(as.character(t_test_table$t))
  t_test_table$p <- ifelse(round(t_test_table$p, digits = 4) == 1, "1", ifelse(round(t_test_table$p, digits = 4) < .05, paste(substr(as.character(sprintf("%.4f", round(t_test_table$p, digits = 4))), 2, 6), "*", sep = " ") ,substr(as.character(sprintf("%.4f", round(t_test_table$p, digits = 4))), 2, 6) ) )
  
  t_test_table$t <-  ifelse(!is.na(t_test_table$t), as.character(round(t_test_table$t, digits = 3)), t_test_table$t)
  
  
  
  
  #textProp <- textProperties()
  
 # sigFTable2 = vanilla.table( data = t_test_table )
  #sigFTable2[as.numeric(substr(t_test_table$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
  doc0 <- doc0 %>% 
    body_add_par("comparison method: t.test", style = "heading 2") %>% 
    body_add_par("", style = "Normal")%>% 
    body_add_par("Table 1.", style = "table title")  %>% 
    body_add_table(value = t_test_table, style = "table_template" )   %>%
    body_add_par("tableStatement", style = "Normal")
    

  
  ##Summarize data and graph
  
  summarized_data <- summarySE(measurevar = "value", groupvars = "condition", data = Data)
  
  gg <- ggplot(summarized_data, aes(x = condition, y = value))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.2,                    
                  position=position_dodge(.9))+
    labs(x=x_str) +
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
    theme(legend.position="none")
  
  sig_start <- max(summarized_data$value) 
  sig_inter <- max(summarized_data$se) + (.5*max(summarized_data$se)) 
  
  sig_nifComp <- t_test_table2[t_test_table2$p < .05,]
  if(length(sig_nifComp$ComparedGroups) > 0){
    for(i in 1:length(sig_nifComp$ComparedGroups)){
      comparison_group <- strsplit(as.character(sig_nifComp$ComparedGroups[i]), " - ")[[1]]
      
      gg <- gg + geom_signif(comparisons = list(c(comparison_group[1], comparison_group[2])), annotations=substr(as.character(sprintf("%.3f", round(sig_nifComp$p[i], digits = 3))), 2, 5), y_position = sig_start + (i*sig_inter))
      
    }
  }
  
  if(x_str == "light"){
    
    gg <- gg +  scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) 
    
  }
  if(x_str == "Caffeine"){
    gg <- gg +  scale_fill_manual(values=c( "gray100",  "gray40")) 
    
  }
  #doc0 = addParagraph( doc0, "Athethmetic means", stylename = "BulletList" )

  doc0 <- doc0 %>% body_add_par("Figure 1.", style = "graphic title")  %>% 
    body_add_gg(value = gg, width = 4, height = 3, style = "Normal" )
}

performTtests0 <- function(Data, x_str, Correction){
  
  removeDim <- FALSE
  removeRedBluecompare <- FALSE
  
  # Correction <- "none"
  
  ComparedGroups <- c()
  df <- c()
  t <- c()
  p <- c()
  
  colnames(Data)[1] <- "Subject"
  colnames(Data)[2] <- "condition"
  if(x_str == "light"){
    
    Data$condition <- factor(Data$condition, levels = c("Red" , "Blue", "Dim" ))
    removeRedBluecompare <- FALSE
    
  }
  #get comparisons
  testModel<- lme(value ~ condition, random = ~1|Subject,
                  data=Data)
  
  df5 <- lsmeans(testModel, pairwise~ condition, adjust=Correction, data = Data)
  lsmeansComparisons3 <- data.frame(summary(df5)[2])
  comparistonList <- strsplit(as.character(lsmeansComparisons3$contrasts.contrast), " - ")
  ###
  
  for(i in 1:length(comparistonList)){
    orignialData <- Data[Data$condition == comparistonList[[i]][1] | Data$condition == comparistonList[[i]][2] ,]
    
    subList001 <- intersect(Data[Data$condition == comparistonList[[i]][1],]$Subject , Data[Data$condition == comparistonList[[i]][2],]$Subject)
    cleanedData <- orignialData[orignialData$Subject %in% subList001, ]
    
    curr_Ttest <- t.test(value ~ condition, data = cleanedData, paired =  TRUE)
    curr_Ttest2 <- t.test(value ~ condition, data = orignialData, var.equal = TRUE)
    
    ComparedGroups <- c(ComparedGroups, paste(comparistonList[[i]][1], comparistonList[[i]][2], sep = " - "  ))
    df <- c(df, as.numeric(curr_Ttest2$parameter))
    t <- c(t, as.numeric(curr_Ttest2$statistic))
    p <- c(p, curr_Ttest2$p.value)
    
  }
  t_test_table <- data.frame(ComparedGroups, df, t, p )
  t_test_table$p <- as.numeric(as.character(t_test_table$p))
  
  
  
  if(removeDim){
    
    t_test_table$number<- str_count(t_test_table$ComparedGroups, "Dim")
    
    t_test_table <- subset(t_test_table, number < 2)
    t_test_table$number <- NULL
  }
  
  
  if(removeRedBluecompare){
    
    t_test_table$Rednumber<- str_count(t_test_table$ComparedGroups, "Red")
    t_test_table$Bluenumber<- str_count(t_test_table$ComparedGroups, "Blue")
    t_test_table$RedBluecompare <- ifelse(t_test_table$Rednumber == 1 & t_test_table$Bluenumber == 1, 1, 0)
    t_test_table <- subset(t_test_table, RedBluecompare == 0)
    t_test_table$Rednumber <- NULL
    t_test_table$Bluenumber <- NULL
    t_test_table$RedBluecompare <- NULL
    
  }
  
  
  
  ###p-value-correction 
  if(Correction != "None" | Correction != "none"){
    t_test_table$p <- p.adjust(t_test_table$p, method = Correction, n = length(t_test_table$p) )
    tableStatement <- paste("P value adjustment: ",Correction, " method for ", length(t_test_table$p), " tests", sep =""  )
  }else{
    tableStatement <- "No P-value adjustment applied"
    
  }
  t_test_table2 <- t_test_table
  
  t_test_table$t <- as.numeric(as.character(t_test_table$t))
  t_test_table$p <- ifelse(round(t_test_table$p, digits = 4) == 1, "1", ifelse(round(t_test_table$p, digits = 4) < .05, paste(substr(as.character(sprintf("%.3f", round(t_test_table$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(t_test_table$p, digits = 3))), 2, 5) ) )
  
  t_test_table$t <-  ifelse(!is.na(t_test_table$t), as.character(round(t_test_table$t, digits = 3)), t_test_table$t)
  
  return(t_test_table)
}

add_plotSigs <- function(means, comparisons, x_str, y_str, include_fill){
  
  if(x_str == "light"){
    means$lsmeans.light<- factor(means$lsmeans.light, levels = c( "Red", "Blue", "Dim"))
  }
  
  if(x_str ==  "light:sample"){
    means$lsmeans.light<- factor(means$lsmeans.light, levels = c( "Red", "Blue", "Dim"))
    
    means$var <- paste(means$lsmeans.light, means$lsmeans.sample, sep = "_")
    means$lsmeans.sample <- NULL
    
    gg <- ggplot(means, aes_string(x = colnames(means)[7], y = colnames(means)[2], fill = colnames(means)[1]))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +

      geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                    width=.2                    
                    ,position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      labs(x="", y =y_str) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none") +
      scale_fill_manual (values=c("red4", "deepskyblue4",  "gray80" )) 
 
    
    if(y_str == "Melatonin norm"){
      gg <- gg + scale_x_discrete(labels=c("Blue_n2" = "2", "Blue_n3" = "3", "Blue_n4" = "4", "Blue_n5" = "5", "Blue_n6" = "6", "Blue_n7" = "7", 
                                  "Red_n2" = "2", "Red_n3" = "3", "Red_n4" = "4", "Red_n5" = "5", "Red_n6" = "6", "Red_n7" = "7",
                                  "Dim_n2" = "2", "Dim_n3" = "3", "Dim_n4" = "4", "Dim_n5" = "5", "Dim_n6" = "6", "Dim_n7" = "7"))
      
    }
    if(y_str == "Melatonin raw"){
      gg <- gg + scale_x_discrete(labels=c("Blue_1" = "1", "Blue_2" = "2", "Blue_3" = "3", "Blue_4" = "4", "Blue_5" = "5", "Blue_6" = "6", "Blue_7" = "7", 
                                  "Red_1" = "1", "Red_2" = "2", "Red_3" = "3", "Red_4" = "4", "Red_5" = "5", "Red_6" = "6", "Red_7" = "7",
                                  "Dim_1" = "1", "Dim_2" = "2", "Dim_3" = "3", "Dim_4" = "4", "Dim_5" = "5", "Dim_6" = "6", "Dim_7" = "7"))
      
    }
  
    
     }      
  if(x_str ==  "light:sample_line"){
    means$lsmeans.light<- factor(means$lsmeans.light, levels = c( "Red", "Blue", "Dim"))
    

    
    gg <- ggplot(means, aes_string(x = colnames(means)[2], y = colnames(means)[3], group = colnames(means)[1], colour = colnames(means)[1]))+
      #geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_point()+
      geom_line()+
      geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                    width=.2)+ #                    
      #,position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      labs(x="", y =y_str) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none") +
      scale_colour_manual (values=c("red4", "deepskyblue4",  "gray80" ))
    
    return(gg)
    
  }      
  
  if(x_str == 'Condition percentage normalized'){
    
    means$lsmeans.light3 <- ifelse(means$lsmeans.light3 == "Active", "CSE", "CSN")
    
    means$lsmeans.light3 <- factor(means$lsmeans.light3, levels = c("CSE", "CSN"))
    comparisons$contrasts.contrast <- ifelse(comparisons$contrasts.contrast == "Active - Placebo", "CSE - CSN", comparisons$contrasts.contrast)
  }
  
  if(include_fill ){
    
    gg <- ggplot(means, aes_string(x = colnames(means)[1], y = colnames(means)[2], fill = colnames(means)[1]))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      labs(x=ifelse(x_str == "CS_levels_1", "", gsub("_", " ", x_str)) , y = gsub("_", " ", y_str)) +
      geom_text(aes(label = round(lsmeans.lsmean, digits = 3), vjust=2)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none")
    
    
    
    
    if(colnames(means)[1] == "lsmeans.condition2"){
      gg <- gg + scale_fill_manual(values=c("white", "grey42"))  
    }
    
    if(colnames(means)[1] == "lsmeans.light"){
      
      #means$means <- factor(means$means, levels = c("Red Light" , "Blue Light", "Dim light" ))
      gg <- gg +  scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) 
      
    }
    if(colnames(means)[1] == "lsmeans.light2"){
      gg <- gg +  scale_fill_manual(values=c( "deepskyblue2",  "orangered1")) 
    }
    
    if(colnames(means)[1] == "lsmeans.light3"){
      gg <- gg +  scale_fill_manual(values=c( "deepskyblue2",  "orangered1")) 
    }
  }else if( x_str !=  "light:sample" & x_str !=  "light:sample_line" & !(include_fill)) {
    gg <- ggplot(means, aes_string(x = colnames(means)[1], y = colnames(means)[2]))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      labs(x=x_str , y = y_str) +
      geom_text(aes(label = round(lsmeans.lsmean, digits = 3), vjust=2)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
      theme(legend.position="none")
    
  }
  
  sig_start <- max(means$lsmeans.lsmean) 
  sig_inter <- max(means$lsmeans.SE) + (.5*max(means$lsmeans.SE)) 
  
  sig_nifComp <- comparisons[comparisons$contrasts.p.value < .06,]
  if(length(sig_nifComp$contrasts.contrast) > 0){
    for(i in 1:length(sig_nifComp$contrasts.contrast)){
      comparison_group <- strsplit(as.character(sig_nifComp$contrasts.contrast[i]), " - ")[[1]]
      
      numLimDF <- data.frame(table(sig_nifComp$contrasts.light))
      
      if(x_str ==  "light:sample"){
        comparison_group[1] <- paste(sig_nifComp$contrasts.light[i], comparison_group[1], sep = "_")
        comparison_group[2] <- paste(sig_nifComp$contrasts.light[i], comparison_group[2], sep = "_")
        
        gg <- gg + geom_signif(comparisons = list(c(comparison_group[1], comparison_group[2])), 
                               annotations=substr(as.character(sprintf("%.3f", round(sig_nifComp$contrasts.p.value[i], digits = 3))), 2, 5), 
                               y_position = sig_start + ((i%%(numLimDF[numLimDF$Var1 == sig_nifComp$contrasts.light[i],]$Freq[1]+ 1))*sig_inter))
        
      }else{
        gg <- gg + geom_signif(comparisons = list(c(comparison_group[1], comparison_group[2])), annotations=substr(as.character(sprintf("%.3f", round(sig_nifComp$contrasts.p.value[i], digits = 3))), 2, 5), y_position = sig_start + (i*sig_inter))
        
      }
      
    }
  }
  
  
  return(gg)
}  




if(FALSE){
  
  my_doc <- read_docx() 
  
  #outputWordAnalysis("Melatonin", melatonin_raw_model, melatonin_raw, doc)
  outputWordAnalysis("Melatonin", melatonin_norm_model, melatonin_norm, my_doc)
  #outputWordAnalysis(cortisol_raw_model, cort_raw)
  outputWordAnalysis("Cortisol", cortisol_norm_model, cort_norm, my_doc)
  outputWordAnalysis("KSS" ,kss_model, KSS_norm2, my_doc)
  
  # write the doc
  dir <- "//root/projects/Caffeine_ONR_Study/output-MarkdownFiles/results-output/"
  filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"),"-CaffeineBio-markers-KSS-Results.docx")

  print(my_doc, target = filename)
  #writeDoc( doc, file = filename )
}



if(FALSE){
  

  
  
  subdf <- data.frame(table(Caffeine_Biomarkers$Condition, Caffeine_Biomarkers$Week))
  subdf$Var1 <- factor(subdf$Var1, levels = c("Red/Placebo" , "Red/Caffeine", "Blue/Placebo", "Blue/Caffeine", "Dim/Placebo", "Dim/Caffeine"  ))
  subdf$Var2 <- paste("Week", subdf$Var2 , sep = " ")
  ggplot(subdf,aes(x=Var2,y=Freq,fill=Var1)) + geom_bar(stat="identity",position = "identity", alpha=.3)+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90")) +
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(subdf,aes(x=Var2,y=Freq,fill=Var1)) + geom_bar(stat="identity",position = "dodge")+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray30", "gray60"))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    labs(x = '', y = "Number of subjects")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  ggplot(subdf,aes(x=Var1,y=Freq,fill=Var1)) + geom_bar(stat="identity",position = "dodge")+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray30", "gray60"))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    facet_grid(.~ Var2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.ticks.x=element_blank(), axis.text.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  
}

