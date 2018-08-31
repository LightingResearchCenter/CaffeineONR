

###ONR Caffeine study performed in 2017. Adding the factor of ISI to the data.frame and including it's related factors:
#1. Previous ISI
#2. Next ISI
#3. fast_response_time < 200 ms
#4. false_start_after_miss
#5. fast_rt_after_miss
#6. 


library(readr)
GNG_caffeine <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-05-21_142835_GNG_addedTimeCropped.csv")

OB_caffeine <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-05-21_142835_OB_addedTimeCropped.csv")
TB_caffeine <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-05-21_142835_TB_addedTimeCropped.csv")
MOB_caffeine <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-05-21_142835_MOB_addedTimeCropped.csv")



addISIfactors <- function(PerformData, task){
  
  PerformData$block <- substr(PerformData$filename, 1, 1)
  PerformData$sub_cond_block <- paste(PerformData$Subject_id, PerformData$condition, PerformData$test_period, PerformData$block, sep = "_")
  
  
  PerformData$next_time <- NA
  
  
  PerformData$time2 <- FALSE
  PerformData$previous_isi <- NA
  PerformData$next_isi <- NA
  
  
  
  blockList <- unique(PerformData$sub_cond_block)
  
  for(i in 1:length(blockList)){
    
    
    print(blockList[i])
    
    blockLength <- length(PerformData[PerformData$sub_cond_block == blockList[i],]$date)
    
    PerformData[PerformData$sub_cond_block == blockList[i],]$next_time[1:(blockLength - 1)] <-
      PerformData[PerformData$sub_cond_block == blockList[i],]$time[2:blockLength]
    
    
  }
  
  PerformData$next_isi <- PerformData$next_time - PerformData$time
  
  for(i in 1:length(blockList)){
    
    
    print(blockList[i])
    
    blockLength <- length(PerformData[PerformData$sub_cond_block == blockList[i],]$date)
    
    PerformData[PerformData$sub_cond_block == blockList[i],]$previous_isi[2:(blockLength)] <-
      PerformData[PerformData$sub_cond_block == blockList[i],]$next_isi[1:blockLength]
    
    
  }
  
  
  PerformData$previous_isi <- as.numeric(PerformData$previous_isi)
  PerformData$next_isi <- as.numeric(PerformData$next_isi)
  PerformData$response_time <- as.numeric(PerformData$response_time)
  
  PerformData$fast_response_time <- ifelse(PerformData$response_time < .2, 1, 0)
  
  
  

  
  
  ###adding misses and fast rt after misses
  
  PerformData$false_start_after_miss <- 0
  PerformData$fast_rt_after_miss <- 0
  
  
  
  if(task == "GNG"){
    PerformData$false_start <- ifelse(PerformData$response_time < .1, 1, 0)
    PerformData$false_start <- ifelse(is.na(PerformData$false_start), 0, PerformData$false_start)
    
    PerformData$lapse <- ifelse(as.numeric(PerformData$response_time) > .5 & PerformData$display == "go" , 1, 0)
    PerformData$lapse <- ifelse(is.na(PerformData$response_time), 0, PerformData$lapse)
    
    PerformData$lapse400 <- ifelse(as.numeric(PerformData$response_time) > .4 & PerformData$display == "go", 1, 0)
    PerformData$lapse400 <- ifelse(is.na(PerformData$response_time), 0, PerformData$lapse400)
    
    PerformData$false_positive <- ifelse((as.numeric(PerformData$response_time) > 0 & PerformData$display == "no_go") & PerformData$display == "no_go" , 1, 0)
    PerformData$false_positive <- ifelse(is.na(PerformData$false_positive), 0, PerformData$false_positive)
    
    PerformData$miss <- ifelse((is.na(PerformData$response_time)) & PerformData$display == "go", 1, 0)
    
    
  }
  if(task == "N-back"){
    PerformData$false_start <- ifelse(PerformData$response_time < .1, 1, 0)
    PerformData$false_start <- ifelse(is.na(PerformData$false_start), 0, PerformData$false_start)
    PerformData$miss <- ifelse(is.na(PerformData$response_time) & PerformData$trial_num != 1, 1, 0)
  }
  PerformData$previous_miss <- NA
  
  for(i in 1:length(blockList)){
    
    
    print(blockList[i])
    
    
    blockLength <- length(PerformData[PerformData$sub_cond_block == blockList[i],]$date)
    
    PerformData[PerformData$sub_cond_block == blockList[i],]$previous_miss[2:(blockLength)] <-
      PerformData[PerformData$sub_cond_block == blockList[i],]$miss[1:blockLength]
    
  }
  
  PerformData$false_start_after_miss <- ifelse(PerformData$previous_miss == 1 & PerformData$false_start == 1, 1, 0)
  PerformData$fast_rt_after_miss <- ifelse(PerformData$previous_miss ==1 & PerformData$fast_response_time == 1, 1, 0)
  
  if(task == "GNG"){
  PerformData$false_positive <- ifelse(PerformData$display == "no_go" & !is.na(PerformData$response_time), 1, 0)
  }
  if(task == "N-back"){
    PerformData$false_positive <- ifelse(PerformData$accuracy == 0, 1, 0)
  }
  return(PerformData)
}


plotScenerios <- function(PlotData){
  
  previous_isi_data <- subset(PlotData, !is.na(previous_isi) )
  next_isi_data <- subset(PlotData, !is.na(next_isi) )
  
  ########################
  
  ###Specific scenerios
  library(ggplot2)
  
  ## Relationship: previous ISI * false positive
  previous_isi_data$isi_rounded <- as.numeric(substr(as.character(previous_isi_data$previous_isi) , 1, 3))
  prev_isi_falsePos <- data.frame(table(previous_isi_data$isi_rounded , previous_isi_data$false_positive))
  prev_isi_falsePosTRUE <- subset(prev_isi_falsePos, Var2 == 1)
  gg <- ggplot(prev_isi_falsePosTRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Previous ISI", y = "Number of false positives")
  
  print(gg)
  
  ## Relationship: previous ISI * false start
  prev_isi_falseStart <-data.frame(table(previous_isi_data$isi_rounded , previous_isi_data$false_start))
  prev_isi_falseStartTRUE <- subset(prev_isi_falseStart, Var2 == 1)
  gg <- ggplot(prev_isi_falseStartTRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Previous ISI", y = "Number of false starts")
  
  
  print(gg)
  
  ## Relationship: previous ISI * false start after miss
  falseStart_afterMiss <-data.frame(table(previous_isi_data$isi_rounded, previous_isi_data$false_start_after_miss))
  falseStartaftermissTRUE <- subset(falseStart_afterMiss, Var2 == 1)
  gg <- ggplot(falseStartaftermissTRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Previous ISI", y = "Number of false starts after miss")
  
  
  print(gg)
  
  ## Relationship: previous ISI * fast RT ( < 200ms) after miss
  fastRt_afterMiss <- data.frame(table(previous_isi_data$isi_rounded, previous_isi_data$fast_rt_after_miss))
  fastRt_afterMissTRUE <- subset(fastRt_afterMiss, Var2 == 1)
  gg <- ggplot(fastRt_afterMissTRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Previous ISI", y = "Number of fast RT ( < 200ms) after miss")
  
  
  print(gg)
  
  ## Relationship: Next ISI * miss
  next_isi_data$isi_rounded <- as.numeric(substr(as.character(next_isi_data$next_isi) , 1, 3))
  miss_afterISI <- data.frame(table(next_isi_data$isi_rounded , next_isi_data$miss))
  miss_afterISI_TRUE <- subset(miss_afterISI, Var2 == 1)
  gg <- ggplot(miss_afterISI_TRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Next ISI", y = "Misses")
  print(gg)
  
  gg <- ggplot(PlotData, aes(x=next_isi, y=response_time)) +
    geom_point()+
    labs(x="Next ISI", y = "RT")
  
  
  print(gg)
}


GNG_caffeine <- addISIfactors(GNG_caffeine, "GNG")
OB_caffeine <- addISIfactors(OB_caffeine, "N-back")
TB_caffeine <- addISIfactors(TB_caffeine, "N-back")
MOB_caffeine <- addISIfactors(MOB_caffeine, "N-back")


GNG_isi_removed <- subset(GNG_caffeine, Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711" & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
OB_isi_removed <- subset(OB_caffeine, Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
TB_isi_removed <- subset(TB_caffeine, Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
MOB_isi_removed <- subset(MOB_caffeine, Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")


write.csv(GNG_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/GNG_caffieneISIadded.csv", row.names = FALSE)
write.csv(OB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/OB_caffieneISIadded.csv", row.names = FALSE)
write.csv(TB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/TB_caffieneISIadded.csv", row.names = FALSE)
write.csv(MOB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/MOB_caffieneISIadded.csv", row.names = FALSE)




GNG_isi_removed <- subset(GNG_caffeine, next_isi > 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711" & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
OB_isi_removed <- subset(OB_caffeine, next_isi > 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
TB_isi_removed <- subset(TB_caffeine, next_isi > 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
MOB_isi_removed <- subset(MOB_caffeine, next_isi > 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")


write.csv(GNG_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/GNG_caffieneISIaddedRemovedISI.csv", row.names = FALSE)
write.csv(OB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/OB_caffieneISIaddedRemovedISI.csv", row.names = FALSE)
write.csv(TB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/TB_caffieneISIaddedRemovedISI.csv", row.names = FALSE)
write.csv(MOB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/MOB_caffieneISIaddedRemovedISI.csv", row.names = FALSE)



GNG_caffieneISIadded <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/GNG_caffieneISIadded.csv")
OB_caffieneISIadded <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/OB_caffieneISIadded.csv")
TB_caffieneISIadded <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/TB_caffieneISIadded.csv")
MOB_caffieneISIadded <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/MOB_caffieneISIadded.csv")

plotScenerios(GNG_caffieneISIadded)
plotScenerios(OB_caffieneISIadded)
plotScenerios(TB_caffieneISIadded)
plotScenerios(MOB_caffieneISIadded)

