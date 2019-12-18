


Nback_output_mixed_models_caffeine_study_errors <- function(Save, OB, TB, MOB, GNG, post_hoc, doc, Correction){
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  library(ReporteRs)
  library(magrittr)
  library(ggsignif)
  library(stringr)
  doc = docx()
  
  ctrl <- lmeControl(opt='optim');
  options(warn=-1)
  

  
  if(FALSE){
    OB <- subset(OB, session != 1)
    TB <- subset(TB, session != 1)
    MOB <- subset(MOB, session != 1)
    GNG <- subset(GNG, session != 1)
    
  }
  OB$session_testPeriod <- paste(OB$session, OB$test_period, sep = "_")
  TB$session_testPeriod <- paste(TB$session, TB$test_period, sep = "_")
  MOB$session_testPeriod <- paste(MOB$session, MOB$test_period, sep = "_")
  GNG$session_testPeriod <- paste(GNG$session, GNG$test_period, sep = "_")
  
  if(TRUE){
    OB <- subset(OB, session_testPeriod != "1_1")
    TB <- subset(TB, session_testPeriod != "1_1")
    MOB <- subset(MOB, session_testPeriod != "1_1")
    GNG <- subset(GNG, session_testPeriod != "1_1")
    
  }
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
  gng_rt <- subset(GNG, display == "go" & response_time >= .1)
  gng_rt$log_rt <- log(gng_rt$response_time)
  
  go_only$hit <- ifelse(go_only$response_detail == "CORRECT MATCH", 1, 0)
  no_only$fp <- ifelse(no_only$response_detail == "FALSE-POSITIVE", 1, 0)

  hit_rate <- aggregate(hit ~ Subject_id + condition2 + light + test_period  + condition, data = go_only, FUN= mean)
  fp_rate <- aggregate(fp ~ Subject_id + condition2 + light + test_period  + condition, data = no_only, FUN= mean)
  
  
  bottem10 <- tb10(gng_rt, "bottom")
  top10 <- tb10(gng_rt, "top")
  bottem10log <- tb10Log(gng_rt, "bottom")
  top10log <- tb10Log(gng_rt, "top")
  
  
 
    hitrate_model <- lme(hit ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                         data=hit_rate)
    
    if(FALSE){
      
      currDF <-  setDT(currDF0, keep.rownames = TRUE)[]
      colnames(currDF)[1] <- "Dependent measures"
      colnames(currDF)[2] <- "df"
      colnames(currDF)[3] <- "Error"
      colnames(currDF)[4] <- "F"
      colnames(currDF)[5] <- "p"
      currDF <- currDF[2:length(currDF$p),]
      currDF$p <-  ifelse(!is.na(currDF$p),ifelse(round(currDF$p, digits = 3) == 1, "1",  substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5)) )
      currDF$F <-  as.character(round(currDF$F, digits = 3))
      
      doc = addParagraph( doc, currTitle, stylename = "DocDefaults" )
      MyFTable = vanilla.table( data = currDF )
      MyFTable[as.numeric(currDF$p) < .05, 5] = chprop( baseCellProp, background.color = "green") 
      doc = addFlexTable(doc, MyFTable)
      
    }
    
    
    sigList_output1 <- pval_postHoc_OutPut("GNG hit rate", hitrate_model, hit_rate, post_hoc, doc, "hit")
    
    
    fp_rate_model <- lme(fp ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                         data=fp_rate)
    sigList_output2 <- pval_postHoc_OutPut("GNG false_positve", fp_rate_model, fp_rate, post_hoc, doc, "fp")
    
    
    gng_rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                       data=gng_rt)
    sigList_output3 <- pval_postHoc_OutPut("GNG response time", gng_rt_model, gng_rt, post_hoc, doc, "response_time")
    
    
    gng_logrt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                          data=gng_rt)
    sigList_output4 <- pval_postHoc_OutPut("GNG log(response time)", gng_logrt_model, gng_rt, post_hoc, doc, "log_rt")
    
    
    gng_bot10rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                            data=bottem10)
    sigList_output5 <- pval_postHoc_OutPut("GNG bottom 10% response time", gng_bot10rt_model, bottem10, post_hoc, doc, "response_time")
    
    
    gng_top10rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                            data=top10)
    sigList_output6 <- pval_postHoc_OutPut("GNG top 10% response time", gng_top10rt_model, top10, post_hoc, doc, "response_time")
    
    
    gng_Logbot10rt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                               data=bottem10log)
    sigList_output7 <- pval_postHoc_OutPut("GNG bottom 10% log(response time)", gng_Logbot10rt_model, bottem10log, post_hoc, doc, "log_rt")
    
    
    gng_Logtop10rt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                               data=top10log)
    sigList_output8 <- pval_postHoc_OutPut("GNG top 10% log(response time)", gng_Logtop10rt_model, top10log, post_hoc, doc, "log_rt")
    
  
  
  
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
  
  accuracy_data00 <- aggregate(accuracy ~ Subject_id + condition2 + light + test_period + condition , data = OB, FUN= mean)
  accuracy_data01 <- aggregate(correct_Match ~ Subject_id + condition2 + light + test_period + condition , data = OB, FUN= sum)
  accuracy_data02 <- aggregate(correct_noMatch ~ Subject_id + condition2 + light + test_period + condition , data = OB, FUN= sum)
  
  
  OB2 <- subset(OB, accuracy == 1 & response_time >= .1 & !is.na(response_time))
  OB2$response_time <- as.numeric(OB2$response_time)
  OB2$log_rt <- log(OB2$response_time)
  
  
  bottem10_OB <- tb10(OB2, "bottom")
  top10_OB <- tb10(OB2, "top")
  bottem10log_OB <- tb10Log(OB2, "bottom")
  top10log_OB <- tb10Log(OB2, "top")
  

    acc_model1 <- lme(accuracy ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data00)
    sigList_output9 <- pval_postHoc_OutPut("1-back accuracy", acc_model1, accuracy_data00, post_hoc, doc, "accuracy")
    
    
    acc_model2 <- lme(correct_Match ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data01)
    sigList_output10 <- pval_postHoc_OutPut("1-back correct matches", acc_model2, accuracy_data01, post_hoc, doc, "correct_Match")
    
    
    acc_model3 <- lme(correct_noMatch ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data02)
    sigList_output11 <- pval_postHoc_OutPut("1-back correct no-match", acc_model3, accuracy_data02, post_hoc, doc, "correct_noMatch")
    
    
    OBrt_model1 <- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                       data=OB2)
    sigList_output12 <- pval_postHoc_OutPut("1-back respone time", OBrt_model1, OB2, post_hoc, doc, "response_time")
    
    
    OBrt_model2 <- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                       data=OB2)
    sigList_output13 <- pval_postHoc_OutPut("1-back log(respone time)", OBrt_model2, OB2, post_hoc, doc, "log_rt")
    
  
 

    OB_bot10rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                           data=bottem10_OB)
    sigList_output14 <- pval_postHoc_OutPut("1-back bottom 10% response time", OB_bot10rt_model, bottem10_OB, post_hoc, doc, "response_time")
    
    
    OB_top10rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                           data=top10_OB)
    sigList_output15 <- pval_postHoc_OutPut("1-back top 10% response time", OB_top10rt_model, top10_OB, post_hoc, doc, "response_time")
    
    
    OB_Logbot10rt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                              data=bottem10log_OB)
    sigList_output16 <- pval_postHoc_OutPut("1-back bottom 10% log(response time)", OB_Logbot10rt_model, bottem10log_OB, post_hoc, doc, "log_rt")
    
    
    OB_Logtop10rt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                              data=top10log_OB)
    sigList_output17 <- pval_postHoc_OutPut("1-back top 10% log(response time)", OB_Logtop10rt_model, top10log_OB, post_hoc, doc, "log_rt")
    
    
  
 
  
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
  
  accuracy_data03 <- aggregate(accuracy ~ Subject_id + condition2 + light + test_period + condition , data = TB, FUN= mean)
  accuracy_data04 <- aggregate(correct_Match ~ Subject_id + condition2 + light + test_period + condition , data = TB, FUN= sum)
  accuracy_data05 <- aggregate(correct_noMatch ~ Subject_id + condition2 + light + test_period + condition , data = TB, FUN= sum)
  
  
  TB2 <- subset(TB, accuracy == 1 & response_time >= .1)
  TB2$response_time <- as.numeric(TB2$response_time)
  TB2$log_rt <- log(TB2$response_time)
  
  bottem10_TB <- tb10(TB2, "bottom")
  top10_TB <- tb10(TB2, "top")
  bottem10log_TB <- tb10Log(TB2, "bottom")
  top10log_TB <- tb10Log(TB2, "top")
  

    acc_model4 <- lme(accuracy ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data03)
    sigList_output18 <- pval_postHoc_OutPut("2-back accuracy", acc_model4, accuracy_data03, post_hoc, doc, "accuracy")
    
    
    acc_model5 <- lme(correct_Match ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data04)
    sigList_output19 <- pval_postHoc_OutPut("2-back correct matches", acc_model5, accuracy_data04, post_hoc, doc, "correct_Match")
    
    
    acc_model6 <- lme(correct_noMatch ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data05)
    sigList_output20 <- pval_postHoc_OutPut("2-back correct no-matches", acc_model6, accuracy_data05, post_hoc, doc, "correct_noMatch")
    
    
    TBrt_model1 <- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                       data=TB2)
    sigList_output21 <- pval_postHoc_OutPut("2-back response time", TBrt_model1, TB2, post_hoc, doc, "response_time")
    
    
    TBrt_model2 <- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                       data=TB2)
    sigList_output22 <- pval_postHoc_OutPut("2-back log(response time)", TBrt_model2, TB2, post_hoc, doc, "log_rt")
    
  

    TB_bot10rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                           data=bottem10_TB)
    sigList_output23 <- pval_postHoc_OutPut("2-back bottom 10% response time", TB_bot10rt_model, bottem10_TB, post_hoc, doc, "response_time")
    
    
    TB_top10rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                           data=top10_TB)
    sigList_output24 <- pval_postHoc_OutPut("2-back top 10% response time", TB_top10rt_model, top10_TB, post_hoc, doc, "response_time")
    
    
    TB_Logbot10rt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                              data=bottem10log_TB)
    sigList_output25 <- pval_postHoc_OutPut("2-back bottom 10% log(response time)", TB_Logbot10rt_model, bottem10log_TB, post_hoc, doc, "log_rt")
    
    
    TB_Logtop10rt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                              data=top10log_TB)
    sigList_output26 <- pval_postHoc_OutPut("2-back top 10% log(response time)", TB_Logtop10rt_model, top10log_TB, post_hoc, doc, "log_rt")
    
    
    

  
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
  
  accuracy_data06 <- aggregate(accuracy ~ Subject_id + condition2 + light + test_period + condition , data = MOB, FUN= mean)
  accuracy_data07 <- aggregate(correct_Match ~ Subject_id + condition2 + light + test_period + condition , data = MOB, FUN= sum)
  accuracy_data08 <- aggregate(correct_noMatch ~ Subject_id + condition2 + light + test_period + condition , data = MOB, FUN= sum)
  
  
  MOB2 <- subset(MOB, accuracy == 1 & response_time >= .1)
  MOB2$response_time <- as.numeric(MOB2$response_time)
  MOB2$log_rt <- log(MOB2$response_time)
  
  bottem10_MOB <- tb10(MOB2, "bottom")
  top10_MOB <- tb10(MOB2, "top")
  bottem10log_MOB <- tb10Log(MOB2, "bottom")
  top10log_MOB <- tb10Log(MOB2, "top")
  

    acc_model7 <- lme(accuracy ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data06, control=ctrl)
    sigList_output27 <- pval_postHoc_OutPut("MOB 1-back accuracy", acc_model7, accuracy_data06, post_hoc, doc, "accuracy")
    
    
    acc_model8 <- lme(correct_Match ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data07)
    sigList_output28 <- pval_postHoc_OutPut("MOB 1-back correct matches", acc_model8, accuracy_data07, post_hoc, doc, "correct_Match")
    
    
    acc_model9 <- lme(correct_noMatch ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                      data=accuracy_data08)
    sigList_output29 <- pval_postHoc_OutPut("MOB 1-back correct no-matches", acc_model9, accuracy_data08, post_hoc, doc, "correct_noMatch")
    
    
    MOBrt_model1 <- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                        data=MOB2)
    sigList_output30 <- pval_postHoc_OutPut("MOB 1-back response time", MOBrt_model1, MOB2, post_hoc, doc, "response_time")
    
    
    MOBrt_model2 <- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                        data=MOB2)
    sigList_output31 <- pval_postHoc_OutPut("MOB 1-back log(response time)", MOBrt_model2, MOB2, post_hoc, doc, "log_rt")
    


  

    MOB_bot10rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                            data=bottem10_MOB)
    sigList_output32 <- pval_postHoc_OutPut("MOB 1-back bottom 10% response time", MOB_bot10rt_model, bottem10_MOB, post_hoc, doc, "response_time")
    
    
    MOB_top10rt_model<- lme(response_time ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                            data=top10_MOB)
    sigList_output33 <- pval_postHoc_OutPut("MOB 1-back top 10% response time", MOB_top10rt_model, top10_MOB, post_hoc, doc, "response_time")
    
    
    MOB_Logbot10rt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                               data=bottem10log_MOB)
    sigList_output34 <- pval_postHoc_OutPut("MOB 1-back bottom 10% log(response time)", MOB_Logbot10rt_model, bottem10log_MOB, post_hoc, doc, "log_rt")
    
    
    MOB_Logtop10rt_model<- lme(log_rt ~ condition2 * light * test_period , random = ~1|Subject_id/condition2/light/test_period,
                               data=top10log_MOB)
    sigList_output35 <- pval_postHoc_OutPut("MOB 1-back top 10% log(response time)", MOB_Logtop10rt_model, top10log_MOB, post_hoc, doc, "log_rt")
    
    
    
  
  
  
  
  options(warn=0)
  
  
  
  
  if(Save){
    # write the doc
    dir <- "//root/projects/Caffeine_ONR_Study/output-MarkdownFiles/results-output/"
    
    n <- readline(prompt="Analysis Specification: ")
    
    n2 <- gsub(" ", "_", n)
    filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "Caffeine-PostHoc", n2,  ".docx")
    
    
    writeDoc( doc, file = filename )
  }


  
  output_list1 <- list(sigList_output1, sigList_output2, sigList_output3,  sigList_output4, sigList_output5, sigList_output6, sigList_output7, sigList_output8, sigList_output9, sigList_output10,
                          sigList_output11, sigList_output12, sigList_output13, sigList_output14, sigList_output15, sigList_output16, sigList_output17, sigList_output18, sigList_output19, sigList_output20,
                       sigList_output21, sigList_output22, sigList_output23, sigList_output24, sigList_output25, sigList_output26, sigList_output27, sigList_output28, sigList_output29, sigList_output30, 
                       sigList_output31, sigList_output32, sigList_output33, sigList_output34, sigList_output35)
     
  
  return(output_list1)
}
tb10Log <- function(gngRt, side){
  
  
  gngRt$uniID01 <- paste(gngRt$Subject_id, gngRt$condition, gngRt$test_period, sep = "_")
  uniID01_list <- unique(gngRt$uniID01)
  
  first <- TRUE
  tb <- data.frame()
  
  for(i in 1:length(uniID01_list)){
    
    c0 = data.frame()
    c0 <- subset(gngRt, uniID01 == uniID01_list[i])
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
  
  gngRt$uniID01 <- paste(gngRt$Subject_id, gngRt$condition, gngRt$test_period, sep = "_")
  uniID01_list <- unique(gngRt$uniID01)
  
  first <- TRUE
  tb <- data.frame()
  
  for(i in 1:length(uniID01_list)){
    
    c0 = data.frame()
    c0 <- subset(gngRt, uniID01 == uniID01_list[i])
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
  #print(name)
  #print(numCols)
  lsmeansComparisons$number<- str_count(lsmeansComparisons$contrasts.contrast, "Dim")
  
  lsmeansComparisons <- subset(lsmeansComparisons, number < 2)
  lsmeansComparisons$number <- NULL
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
  
  
  #lsmeansComparisons$p <-  ifelse(lsmeansComparisons$p == 1, "1", ifelse(!is.na(lsmeansComparisons$p), substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5), lsmeansComparisons$p))
  
  lsmeansComparisons$p <- ifelse(round(lsmeansComparisons$p, digits = 3) == 1, "1", ifelse(round(lsmeansComparisons$p, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(lsmeansComparisons$p, digits = 3))), 2, 5) ) )
  
  lsmeansComparisons$t <-  ifelse(!is.na(lsmeansComparisons$t), as.character(round(lsmeansComparisons$t, digits = 3)), lsmeansComparisons$t)
  
  
  
  
  
  textProp <- textProperties()
  
  sigFTable2 = vanilla.table( data = lsmeansComparisons )
  sigFTable2[as.numeric(substr(lsmeansComparisons$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
  
  if(FALSE){
    if(numCols == 7){
      sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
      
      
    }
    if(numCols == 6){
      sigFTable2[as.numeric(lsmeansComparisons$p) < .05, 4] = chprop( baseCellProp, background.color = "green")
      
    }  
  }
  doc = addParagraph( doc0, name, stylename = "Normal" )
  doc = addParagraph( doc0, "comparison method: lsmeans", stylename = "BulletList" )
  
  doc0 = addFlexTable(doc0, sigFTable2)
  
}

performTtests <- function(Data, doc0, x_str, outcomemeasureTitle, plannedCompare){
  
  if(missing(plannedCompare)) {
    plannedCompare <- FALSE
  }
  #performTtests(newData, doc, "Caffeine:Light")
  
  if(x_str == "Period"){
    correction <- TRUE
  }else{
    correction <- FALSE
    
  }
  
  ComparedGroups <- c()
  df <- c()
  t <- c()
  p <- c()
  
  colnames(Data)[1] <- "Subject_id"
  colnames(Data)[2] <- "condition"
  if(x_str == "Light"){
    
    Data$condition <- factor(Data$condition, levels = c("Red" , "Blue", "Dim" ))
    
  }
  #get comparisons
  testModel<- lme(value ~ condition, random = ~1|Subject_id,
                  data=Data)
  
  df5 <- lsmeans(testModel, pairwise~ condition, adjust=Correction, data = Data)
  lsmeansComparisons3 <- data.frame(summary(df5)[2])
  comparistonList <- strsplit(as.character(lsmeansComparisons3$contrasts.contrast), " - ")
  ###
  if(plannedCompare){
    comparistonList <- comparistonList[-c(3,4,5,6,8,9, 12, 13)] 
    comparistonList <- comparistonList[c(7,1,4, 6,3,5,2)] 
  }
  for(i in 1:length(comparistonList)){
    orignialData <- Data[Data$condition == comparistonList[[i]][1] | Data$condition == comparistonList[[i]][2] ,]
    
    subList001 <- intersect(Data[Data$condition == comparistonList[[i]][1],]$Subject_id , Data[Data$condition == comparistonList[[i]][2],]$Subject_id)
    cleanedData <- orignialData[orignialData$Subject_id %in% subList001, ]
    
    curr_Ttest <- t.test(value ~ condition, data = cleanedData, paired =  TRUE)
    ComparedGroups <- c(ComparedGroups, paste(comparistonList[[i]][1], comparistonList[[i]][2], sep = " - "  ))
    df <- c(df, as.numeric(curr_Ttest$parameter))
    t <- c(t, as.numeric(curr_Ttest$statistic))
    p <- c(p, curr_Ttest$p.value)
    
  }
  t_test_table <- data.frame(ComparedGroups, df, t, p )
  t_test_table$p <- as.numeric(as.character(t_test_table$p))
  ###add p-value-correction here
  ###
  if(correction){
    t_test_table$p <- p.adjust(as.character(t_test_table$p), method = "bonferroni", 3)
    
  }
  t_test_table2 <- t_test_table
  
  t_test_table$t <- as.numeric(as.character(t_test_table$t))
  t_test_table$p <- ifelse(round(t_test_table$p, digits = 3) == 1, "1", ifelse(round(t_test_table$p, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(t_test_table$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(t_test_table$p, digits = 3))), 2, 5) ) )
  
  t_test_table$t <-  ifelse(!is.na(t_test_table$t), as.character(round(t_test_table$t, digits = 3)), t_test_table$t)
  
  removeDim <- TRUE
  if(removeDim){
    
    t_test_table$number<- str_count(t_test_table$ComparedGroups, "Dim")
    
    t_test_table <- subset(t_test_table, number < 2)
    t_test_table$number <- NULL
  }
  
  
  textProp <- textProperties()
  
  sigFTable2 = vanilla.table( data = t_test_table )
  sigFTable2[as.numeric(substr(t_test_table$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
  

  doc = addParagraph( doc0, "comparison method: t.test", stylename = "BulletList" )
  
  doc0 = addFlexTable(doc0, sigFTable2)
  
  
  ##Summarize data and graph
  
  summarized_data <- summarySE(measurevar = "value", groupvars = "condition", data = Data)
  summarized_data <- summarySE(measurevar = "value", groupvars = "condition", data = Data)
  filename001 <- "C:/Users/roohac/Desktop/CaffeineTemp/sigTABLES/"
  
  filename002 <- paste(outcomemeasureTitle,"--", x_str, ".csv", sep = "")
  filename002 <- gsub(":", "X", filename002)
  filename002 <- gsub(" ", "_", filename002)
  
  filename003 <- paste(filename001, filename002, sep = "")
  
  if(plannedCompare){
    write.csv(summarized_data[c(5,6,1,2,3,4),],filename003, row.names = FALSE)
    
  }else{
    write.csv(summarized_data, filename003, row.names = FALSE)
    
  }
  
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
  
  if(x_str == "Light"){
    
    gg <- gg +  scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) 
    
  }
  if(x_str == "Caffeine"){
    gg <- gg +  scale_fill_manual(values=c( "gray100",  "gray40")) 
    
  }
  #doc0 = addParagraph( doc0, "Athethmetic means", stylename = "BulletList" )
  doc0 <- addPlot(doc = doc0, fun = print, x = gg, vector.graphic = TRUE, width = 4, height = 3)
  
  
}

pval_postHoc_OutPut <- function(outcomemeasureTitle, nlme_model, modelData, post_hoc, doc, colName01){
   print(outcomemeasureTitle)
   print(colName01)
   
   if(outcomemeasureTitle == "2-back correct no-matches"){
     print("here")
   }
   
   if(outcomemeasureTitle == "GNG response time" & colName01 == "response_time"){
     x10 <- 10
     y20 <- 20
   }
   #if(colName01 == "log_rt"){
    # return("Skip")
   #}
   modelData <- as.data.frame(modelData)
  colNum001 <- which( colnames(modelData)== colName01 )
  
  modelData$value <- as.vector(modelData[,colNum001])
  tTestON <- TRUE
  
  
  baseCellProp = cellProperties( padding = 0 )
  
  
  ####"GNG hit rate"
  doc <- addTitle( doc, outcomemeasureTitle, level = 1 )
  
  
  
  model_r2 <- r.squaredGLMM(nlme_model)
  
  modelPvals <- data.frame(anova(nlme_model))
  sigList <- rownames(modelPvals[modelPvals$p.value < .05,])
  sigList_output <- list(outcomemeasureTitle,model_r2, modelPvals)
  
  plannedComparisions1 <- TRUE
  
  if(plannedComparisions1){
    sigList <- c('condition2:light')
    plannedCompare <- TRUE
  }else{
    plannedCompare <- FALSE
    
  }
  
  if(post_hoc){
    post_hoc_list <- list()
    
    if(length(sigList) <= 1){
      sigList_output2 <- list(paste("No significance found in ", outcomemeasureTitle, ". Therefore no post-hoc tests."))
      post_hoc_list <- sigList_output2
      
    }
    

    if('condition2' %in% sigList){
      sigList_output2 <- list('condition2', lsmeans(nlme_model, pairwise~ condition2, adjust=Correction, data = modelData))    
      
      

      
      #Add t-tests to document
      newData <- aggregate(value ~ Subject_id + condition2, data = modelData, FUN = mean)
      performTtests(newData, doc, "Caffeine", outcomemeasureTitle)
      
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('light' %in% sigList){
      sigList_output2 <- list('light', lsmeans(nlme_model, pairwise~ light, adjust=Correction, data = modelData))   
      
      

      
      #Add t-tests to document
      newData <- aggregate(value ~ Subject_id + light, data = modelData, FUN = mean)
      performTtests(newData, doc, "Light", outcomemeasureTitle)
      
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('test_period' %in% sigList){
      sigList_output2 <- list('test_period', lsmeans(nlme_model, pairwise~ test_period, adjust=Correction, data = modelData))  
      
      

      
      #Add t-tests to document
      newData <- aggregate(value ~ Subject_id + test_period, data = modelData, FUN = mean)
      performTtests(newData, doc, "Period", outcomemeasureTitle)
      
      
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('session' %in% sigList){
      sigList_output2 <- list('session', lsmeans(nlme_model, pairwise~ session, adjust=Correction, data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('condition2:light' %in% sigList){
      sigList_output2 <- list('condition2:light', lsmeans(nlme_model, pairwise~ condition2:light, adjust=Correction, data = modelData))    
      
     
      
      
      #Add t-tests to document
      modelData$condition3 <- paste(modelData$light,",", modelData$condition2, sep= "")
      newData <- aggregate(value ~ Subject_id + condition3, data = modelData, FUN = mean)
      performTtests(newData, doc, "Caffeine:Light", outcomemeasureTitle, plannedCompare)
      
      
      
        
        
      
      
      
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('condition2:test_period' %in% sigList){
 
      sigList_output2 <- list('condition2:test_period', lsmeans(nlme_model, pairwise~ condition2:test_period, adjust=Correction, data = modelData))    
      

      
      modelData$condition2_test_period <- paste(modelData$condition2,",", modelData$test_period, sep= "")
      newData <- aggregate(value ~ Subject_id + condition2_test_period, data = modelData, FUN = mean)
      performTtests(newData, doc, "Caffeine:Period", outcomemeasureTitle)
      
      
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('light:test_period' %in% sigList){
      sigList_output2 <- list('light:test_period', lsmeans(nlme_model, pairwise~ light:test_period, adjust=Correction, data = modelData))    
     

      
      modelData$light_test_period <- paste(modelData$light,",", modelData$test_period, sep= "")
      newData <- aggregate(value ~ Subject_id + light_test_period, data = modelData, FUN = mean)
      performTtests(newData, doc, "light:test_period", outcomemeasureTitle)
      
      
       if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('condition2:light:test_period' %in% sigList){
      sigList_output2 <- list('condition2:light:test_period', lsmeans(nlme_model, pairwise~ condition2:light:test_period, adjust=Correction, data = modelData))    
      
      #df <- lsmeans(nlme_model, pairwise~ condition2:light:test_period, adjust=Correction, data = modelData)
      
      #lsmeans <- data.frame(summary(df)[1])
      #lsmeansComparisons <- data.frame(summary(df)[2])
      #addLsmeans(doc,lsmeansComparisons, "condition2:light:test_period", colNum(lsmeansComparisons))
      
      
      modelData$condition_test_period <- paste(modelData$condition,",", modelData$test_period, sep= "")
      newData <- aggregate(value ~ Subject_id + condition_test_period, data = modelData, FUN = mean)
      performTtests(newData, doc, "condition2:light:test_period", outcomemeasureTitle)
      
      
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
  
