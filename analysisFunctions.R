


Nback_output_mixed_models_caffeine_study_errors <- function(OB, TB, MOB, GNG, post_hoc){
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  
  ctrl <- lmeControl(opt='optim');
  options(warn=-1)
  

  
  ###GNG
  
  GNG$Subject_id <- as.factor(GNG$Subject_id )
  GNG$condition <- as.factor(GNG$condition )
  GNG$condition2 <- as.factor(GNG$condition2 )
  GNG$light <- as.factor(GNG$light )
  GNG$test_period <- as.factor(GNG$test_period )
  GNG$response_time <- as.numeric(GNG$response_time )
  GNG$session <- as.factor(GNG$session )
  
  go_only <- subset(GNG, display == "go")
  no_only <- subset(GNG, display == "no_go")
  gng_rt <- subset(GNG, display == "go" & response_time >= .1 & time2 > 0)
  gng_rt$log_rt <- log(gng_rt$response_time)
  
  go_only$hit <- ifelse(go_only$response_detail == "CORRECT MATCH", 1, 0)
  no_only$fp <- ifelse(no_only$response_detail == "FALSE-POSITIVE", 1, 0)

  hit_rate <- aggregate(hit ~ Subject_id + condition2 + light + test_period + session + condition, data = go_only, FUN= mean)
  fp_rate <- aggregate(fp ~ Subject_id + condition2 + light + test_period + session + condition, data = no_only, FUN= mean)
  
  
  bottem10 <- tb10(gng_rt, "bottom")
  top10 <- tb10(gng_rt, "top")
  bottem10log <- tb10Log(gng_rt, "bottom")
  top10log <- tb10Log(gng_rt, "top")
  
  
  hitrate_model <- lme(hit ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=hit_rate)
  sigList_output1 <- pval_postHoc_OutPut("GNG hit rate", hitrate_model, hit_rate, post_hoc)
  
  
  fp_rate_model <- lme(fp ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=fp_rate)
  sigList_output2 <- pval_postHoc_OutPut("GNG false_positve", fp_rate_model, fp_rate, post_hoc)
  
  
  gng_rt_model<- lme(response_time ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=gng_rt)
  sigList_output3 <- pval_postHoc_OutPut("GNG response time", gng_rt_model, gng_rt, post_hoc)

  
  gng_logrt_model<- lme(log_rt ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                     data=gng_rt)
  sigList_output4 <- pval_postHoc_OutPut("GNG log(response time)", gng_logrt_model, gng_rt, post_hoc)
  
  
  gng_bot10rt_model<- lme(response_time ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                     data=bottem10)
  sigList_output4.1 <- pval_postHoc_OutPut("GNG bottom 10% response time", gng_bot10rt_model, bottem10, post_hoc)
  
  
  gng_top10rt_model<- lme(response_time ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                          data=top10)
  sigList_output4.2 <- pval_postHoc_OutPut("GNG top 10% response time", gng_top10rt_model, top10, post_hoc)
  
  
  gng_Logbot10rt_model<- lme(log_rt ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                          data=bottem10log)
  sigList_output4.3 <- pval_postHoc_OutPut("GNG bottom 10% log(response time)", gng_Logbot10rt_model, bottem10log, post_hoc)
  
  
  gng_Logtop10rt_model<- lme(log_rt ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                          data=top10log)
  sigList_output4.4 <- pval_postHoc_OutPut("GNG top 10% log(response time)", gng_Logtop10rt_model, top10log, post_hoc)
  
  ###OB
  OB$Subject_id <- as.factor(OB$Subject_id)
  OB$condition <- as.factor(OB$condition)
  OB$condition2 <- as.factor(OB$condition2)
  OB$light <- as.factor(OB$light)
  OB$test_period <- as.factor(OB$test_period)
  OB$session <- as.factor(OB$session)
  
  OB$light <- as.factor(OB$light)
  OB$light <-  factor(OB$light, levels = c("Red","Blue", "Dim"))
  OB$accuracy <- as.numeric(OB$accuracy)
  OB$accuracy <- ifelse(is.na(OB$accuracy), 0, OB$accuracy)
  OB <- subset(OB, trial_num != 1)
  OB$correct_Match <- ifelse(OB$accuracy == 1 & OB$response == "MATCH", 1, 0)
  OB$correct_noMatch <- ifelse(OB$accuracy == 1 & OB$response == "NO-MATCH", 1, 0)
  
  accuracy_data00 <- aggregate(accuracy ~ Subject_id + condition2 + light + test_period + condition + session, data = OB, FUN= mean)
  accuracy_data01 <- aggregate(correct_Match ~ Subject_id + condition2 + light + test_period + condition + session, data = OB, FUN= sum)
  accuracy_data02 <- aggregate(correct_noMatch ~ Subject_id + condition2 + light + test_period + condition + session, data = OB, FUN= sum)
  
  
  OB2 <- subset(OB, accuracy == 1 & response_time >= .1 & !is.na(response_time))
  OB2$response_time <- as.numeric(OB2$response_time)
  OB2$log_rt <- log(OB2$response_time)
  
  
  
  acc_model1 <- lme(accuracy ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data00)
  sigList_output5 <- pval_postHoc_OutPut("1-back accuracy", acc_model1, accuracy_data00, post_hoc)
  
  
  acc_model2 <- lme(correct_Match ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data01)
  sigList_output6 <- pval_postHoc_OutPut("1-back correct matches", acc_model2, accuracy_data01, post_hoc)
  
  
  acc_model3 <- lme(correct_noMatch ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data02)
  sigList_output7 <- pval_postHoc_OutPut("1-back correct no-match", acc_model3, accuracy_data02, post_hoc)
  

  OBrt_model1 <- lme(response_time ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                   data=OB2)
  sigList_output8 <- pval_postHoc_OutPut("1-back respone time", OBrt_model1, OB2, post_hoc)
  
  
  OBrt_model2 <- lme(log_rt ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                   data=OB2)
  sigList_output9 <- pval_postHoc_OutPut("1-back log(respone time)", OBrt_model2, OB2, post_hoc)
  
  

  
  #TB
  
  ###Set factors to correct 
  TB$Subject_id <- as.factor(TB$Subject_id)
  TB$condition <- as.factor(TB$condition)
  TB$condition2 <- as.factor(TB$condition2)
  TB$light <- as.factor(TB$light)
  TB$test_period <- as.factor(TB$test_period)
  TB$session <- as.factor(TB$session)
  
  TB$light <- as.factor(TB$light)
  TB$light <-  factor(TB$light, levels = c("Red","Blue", "Dim"))
  TB$accuracy <- as.numeric(TB$accuracy)
  TB$accuracy <- ifelse(is.na(TB$accuracy), 0, TB$accuracy)
  TB <- subset(TB, trial_num > 2)
  TB$correct_Match <- ifelse(TB$accuracy == 1 & TB$response == "MATCH", 1, 0)
  TB$correct_noMatch <- ifelse(TB$accuracy == 1 & TB$response == "NO-MATCH", 1, 0)
  
  accuracy_data03 <- aggregate(accuracy ~ Subject_id + condition2 + light + test_period + condition + session, data = TB, FUN= mean)
  accuracy_data04 <- aggregate(correct_Match ~ Subject_id + condition2 + light + test_period + condition + session, data = TB, FUN= sum)
  accuracy_data05 <- aggregate(correct_noMatch ~ Subject_id + condition2 + light + test_period + condition + session, data = TB, FUN= sum)
  
  
  TB2 <- subset(TB, accuracy == 1 & response_time >= .1)
  TB2$response_time <- as.numeric(TB2$response_time)
  TB2$log_rt <- log(TB2$response_time)
  
  
  
  acc_model4 <- lme(accuracy ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data03)
  sigList_output10 <- pval_postHoc_OutPut("2-back accuracy", acc_model4, accuracy_data03, post_hoc)
  

  acc_model5 <- lme(correct_Match ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data04)
  sigList_output11 <- pval_postHoc_OutPut("2-back correct matches", acc_model5, accuracy_data04, post_hoc)
  
  
  acc_model6 <- lme(correct_noMatch ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data05)
  sigList_output12 <- pval_postHoc_OutPut("2-back correct no-matches", acc_model6, accuracy_data05, post_hoc)
  
 
  TBrt_model1 <- lme(response_time ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                   data=TB2)
  sigList_output13 <- pval_postHoc_OutPut("2-back response time", TBrt_model1, TB2, post_hoc)
  
  
  TBrt_model2 <- lme(log_rt ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                   data=TB2)
  sigList_output14 <- pval_postHoc_OutPut("2-back log(response time)", TBrt_model2, TB2, post_hoc)
  
  
  ###MOB
  MOB$Subject_id <- as.factor(MOB$Subject_id)
  MOB$condition <- as.factor(MOB$condition)
  MOB$condition2 <- as.factor(MOB$condition2)
  MOB$light <- as.factor(MOB$light)
  MOB$test_period <- as.factor(MOB$test_period)
  MOB$session <- as.factor(MOB$session)
  
  MOB$light <- as.factor(MOB$light)
  MOB$light <-  factor(MOB$light, levels = c("Red","Blue", "Dim"))
  MOB$accuracy <- as.numeric(MOB$accuracy)
  MOB$accuracy <- ifelse(is.na(MOB$accuracy), 0, MOB$accuracy)
  MOB <- subset(MOB, trial_num != 1)
  MOB$correct_Match <- ifelse(MOB$accuracy == 1 & MOB$response == "MATCH", 1, 0)
  MOB$correct_noMatch <- ifelse(MOB$accuracy == 1 & MOB$response == "NO-MATCH", 1, 0)
  
  accuracy_data06 <- aggregate(accuracy ~ Subject_id + condition2 + light + test_period + condition + session, data = MOB, FUN= mean)
  accuracy_data07 <- aggregate(correct_Match ~ Subject_id + condition2 + light + test_period + condition + session, data = MOB, FUN= sum)
  accuracy_data08 <- aggregate(correct_noMatch ~ Subject_id + condition2 + light + test_period + condition + session, data = MOB, FUN= sum)
  
  
  MOB2 <- subset(MOB, accuracy == 1 & response_time >= .1)
  MOB2$response_time <- as.numeric(MOB2$response_time)
  MOB2$log_rt <- log(MOB2$response_time)
  

  
  acc_model7 <- lme(accuracy ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data06, control=ctrl)
  sigList_output15 <- pval_postHoc_OutPut("MOB 1-back accuracy", acc_model7, accuracy_data06, post_hoc)
  
  
  acc_model8 <- lme(correct_Match ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data07)
  sigList_output16 <- pval_postHoc_OutPut("MOB 1-back correct matches", acc_model8, accuracy_data07, post_hoc)
  
  
  acc_model9 <- lme(correct_noMatch ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                    data=accuracy_data08)
  sigList_output17 <- pval_postHoc_OutPut("MOB 1-back correct no-matches", acc_model9, accuracy_data08, post_hoc)
  
  
  MOBrt_model1 <- lme(response_time ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                   data=MOB2)
  sigList_output18 <- pval_postHoc_OutPut("MOB 1-back response time", MOBrt_model1, MOB2, post_hoc)
  
  
  MOBrt_model2 <- lme(log_rt ~ condition2 * light * test_period + session, random = ~1|Subject_id/condition/test_period,
                   data=MOB2)
  sigList_output19 <- pval_postHoc_OutPut("MOB 1-back log(response time)", MOBrt_model2, MOB2, post_hoc)
  
  options(warn=0)
  
  
  
  output_list1 <- list(sigList_output1, sigList_output2, sigList_output3,  sigList_output4, sigList_output4.1, sigList_output4.2, sigList_output4.3, sigList_output4.4,  sigList_output5, sigList_output6, sigList_output7, sigList_output8, sigList_output9, sigList_output10,
                       sigList_output11, sigList_output12, sigList_output13, sigList_output14, sigList_output15, sigList_output16, sigList_output17, sigList_output18, sigList_output19)
  
  return(output_list1)
}

tb10Log <- function(gngRt, side){
  

  subjects <- unique(gngRt$Subject_id)
  
  first <- TRUE
  tb <- data.frame()
  
  for(i in 1:length(subjects)){
    
    c0 = data.frame()
    c0 <- subset(gngRt, Subject_id == subjects[i])
    #print("     ")
    #print("New data Length:")
    #print(length(c0$time))
    if(side == "top"){
      #print("   Top")
      c1 <- head(c0[order(c0$log_rt,decreasing=T),],.10*nrow(c0))
      #print("Condition:")
      #print(conditions[i], max.levels = 0)
      #print("Count:")
      #print(length(c1$time))
    }else if(side == "bottom"){
      #print("   Bottem")
      c1 <- head(c0[order(c0$log_rt,decreasing=F),],.10*nrow(c0))
      #print("Condition:")
      #print(conditions[i], max.levels = 0)
      #print("Count:")
      #print(length(c1$time))
    }else{
      return(0)
    }
    if(first){
      tb <- c1
      first <- FALSE
    }else{
      tb <- rbind(tb, c1)
    }
  }
  
  return(tb)
}


tb10 <- function(gngRt, side){
  
  subjects <- unique(gngRt$Subject_id)
  
  first <- TRUE
  tb <- data.frame()
  
  for(i in 1:length(subjects)){
    
    c0 = data.frame()
    c0 <- subset(gngRt, Subject_id == subjects[i])
    #print("     ")
    #print("New data Length:")
    #print(length(c0$time))
    if(side == "top"){
      #print("   Top")
      c1 <- head(c0[order(c0$response_time,decreasing=T),],.10*nrow(c0))
      #print("Condition:")
      #print(conditions[i], max.levels = 0)
      #print("Count:")
      #print(length(c1$time))
    }else if(side == "bottom"){
      #print("   Bottem")
      c1 <- head(c0[order(c0$response_time,decreasing=F),],.10*nrow(c0))
      #print("Condition:")
      #print(conditions[i], max.levels = 0)
      #print("Count:")
      #print(length(c1$time))
    }else{
      return(0)
    }
    if(first){
      tb <- c1
      first <- FALSE
    }else{
      tb <- rbind(tb, c1)
    }
  }
  
  return(tb)
}


pval_postHoc_OutPut <- function(outcomemeasureTitle, nlme_model, modelData, post_hoc){
  
  model_r2 <- r.squaredGLMM(nlme_model)
  
  modelPvals <- data.frame(anova(nlme_model))
  sigList <- rownames(modelPvals[modelPvals$p.value < .05,])
  sigList_output <- list(outcomemeasureTitle,model_r2, modelPvals)
  if(post_hoc){
    post_hoc_list <- list()
    
    if(length(sigList) <= 1){
      sigList_output2 <- list(paste("No significance found in ", outcomemeasureTitle, ". Therefore no post-hoc tests."))
      post_hoc_list <- sigList_output2
      
    }
    

    if('condition2' %in% sigList){
      sigList_output2 <- list('condition2', lsmeans(nlme_model, pairwise~ condition2, adjust="tukey", data = modelData))    
      
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('light' %in% sigList){
      sigList_output2 <- list('light', lsmeans(nlme_model, pairwise~ light, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('test_period' %in% sigList){
      sigList_output2 <- list('test_period', lsmeans(nlme_model, pairwise~ test_period, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('session' %in% sigList){
      sigList_output2 <- list('session', lsmeans(nlme_model, pairwise~ session, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('condition2:light' %in% sigList){
      sigList_output2 <- list('condition2:light', lsmeans(nlme_model, pairwise~ condition2|light, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('condition2:test_period' %in% sigList){
      sigList_output2 <- list('condition2:test_period', lsmeans(nlme_model, pairwise~ condition2|test_period, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('light:test_period' %in% sigList){
      sigList_output2 <- list('light:test_period', lsmeans(nlme_model, pairwise~ light|test_period, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('condition2:light:test_period' %in% sigList){
      sigList_output2 <- list('condition2:light:test_period', lsmeans(nlme_model, pairwise~ condition2|light|test_period, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    sigList_output <- list(sigList_output, post_hoc_list)
    return(sigList_output)  }else{
      return(sigList_output) 
    
  }
  
  
}
  
