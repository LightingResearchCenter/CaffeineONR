library(readr)
#source("C:/Users/roohac/Documents/GitHub/CaffeineONR/analysisFunctions.R")
#source("C:/Users/roohac/Documents/GitHub/CaffeineONR/analysisFunctions2.R")
source('~/GitHub/CaffeineONR/testOutputWord.R', echo=TRUE)

#source('~/GitHub/CaffeineONR/tTestFunctions.R', echo=TRUE)
source('~/GitHub/CaffeineONR/completeAnalysisFunctions10_8-18.R', echo=TRUE)
if(FALSE){
  
  
  
  ###Data used for analysis
  GNG_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_GNG_addedTimeCropped.csv")
  OB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_OB_addedTimeCropped.csv")
  TB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_TB_addedTimeCropped.csv")
  MOB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_MOB_addedTimeCropped.csv")
  
  
  
  ###All data without 706
  GNG_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-05-21_142835_GNG_addedTimeCropped.csv")
  OB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-05-21_142835_OB_addedTimeCropped.csv")
  TB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-05-21_142835_TB_addedTimeCropped.csv")
  MOB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-05-21_142835_MOB_addedTimeCropped.csv")
  
  
  GNG_isi_removed <- subset(GNG_addedTimeCroppedRemovedISI, next_isi > 1.5 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711" & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
  OB_isi_removed <- subset(OB_addedTimeCroppedRemovedISI, next_isi > 1.5 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
  TB_isi_removed <- subset(TB_addedTimeCroppedRemovedISI, next_isi > 1.5 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
  MOB_isi_removed <- subset(MOB_addedTimeCroppedRemovedISI, next_isi > 1.5 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine")
  
}


GNG_addedTimeCroppedAddedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/GNG_caffieneISIadded.csv")
OB_addedTimeCroppedAddedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/OB_caffieneISIadded.csv")
TB_addedTimeCroppedAddedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/TB_caffieneISIadded.csv")
MOB_addedTimeCroppedAddedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/MOB_caffieneISIadded.csv")


###***reported analysis
GNG_addedTimeCroppedRemovedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/GNG_caffieneISIaddedRemovedISI.csv")
OB_addedTimeCroppedRemovedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/OB_caffieneISIaddedRemovedISI.csv")
TB_addedTimeCroppedRemovedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/TB_caffieneISIaddedRemovedISI.csv")
MOB_addedTimeCroppedRemovedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/MOB_caffieneISIaddedRemovedISI.csv")

#subjects removed 702, 708, 711



##Removing 709's outlier conditions

GNG_isi_removed_Outlier709Removed <- subset(GNG_addedTimeCroppedRemovedISI, next_isi > 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711" & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine"  & uni3 != "s709_Red Light/Placebo" & uni3 != "s709_Red Light/Caffeine")
OB_isi_removed_Outlier709Removed  <- subset(OB_addedTimeCroppedRemovedISI, next_isi > 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" & uni3 != "s709_Red Light/Placebo" & uni3 != "s709_Red Light/Caffeine")
TB_isi_removed_Outlier709Removed  <- subset(TB_addedTimeCroppedRemovedISI, next_isi > 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" & uni3 != "s709_Red Light/Placebo" & uni3 != "s709_Red Light/Caffeine")
MOB_isi_removed_Outlier709Removed  <- subset(MOB_addedTimeCroppedRemovedISI, next_isi > 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" & uni3 != "s709_Red Light/Placebo" & uni3 != "s709_Red Light/Caffeine")




###Removing special cases

####*****Submitted for analysis 10/2/18
GNG_isi_removed_Cropped2 <- subset(GNG_addedTimeCroppedAddedISI, false_start_after_miss != 1 & fast_rt_after_miss != 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711" & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine"  )
OB_isi_removed_Cropped2   <- subset(OB_addedTimeCroppedAddedISI, false_start_after_miss != 1 & fast_rt_after_miss != 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" )
TB_isi_removed_Cropped2   <- subset(TB_addedTimeCroppedAddedISI, false_start_after_miss != 1 & fast_rt_after_miss != 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" )
MOB_isi_removed_Cropped2   <- subset(MOB_addedTimeCroppedAddedISI, false_start_after_miss != 1 & fast_rt_after_miss != 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" )

##And Removing 709's outlier conditions
GNG_isi_removed_Cropped2 <- subset(GNG_addedTimeCroppedAddedISI, false_start_after_miss != 1 & fast_rt_after_miss != 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711" & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine"  & uni3 != "s709_Red Light/Placebo" & uni3 != "s709_Red Light/Caffeine")
OB_isi_removed_Cropped2   <- subset(OB_addedTimeCroppedAddedISI, false_start_after_miss != 1 & fast_rt_after_miss != 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" & uni3 != "s709_Red Light/Placebo" & uni3 != "s709_Red Light/Caffeine")
TB_isi_removed_Cropped2   <- subset(TB_addedTimeCroppedAddedISI, false_start_after_miss != 1 & fast_rt_after_miss != 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" & uni3 != "s709_Red Light/Placebo" & uni3 != "s709_Red Light/Caffeine")
MOB_isi_removed_Cropped2   <- subset(MOB_addedTimeCroppedAddedISI, false_start_after_miss != 1 & fast_rt_after_miss != 1 & Subject_id != "s702" & Subject_id != "s708" & Subject_id != "s711"  & uni3 != "s712_Red Light/Placebo" & uni3 != "s714_Blue Light/Caffeine" & uni3 != "s709_Red Light/Placebo" & uni3 != "s709_Red Light/Caffeine")



if(FALSE){
  write.csv(GNG_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/GNG_caffieneISIaddedCropped.csv", row.names = FALSE)
  write.csv(OB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/OB_caffieneISIaddedCropped.csv", row.names = FALSE)
  write.csv(TB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/TB_caffieneISIaddedCropped.csv", row.names = FALSE)
  write.csv(MOB_isi_removed, "//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/MOB_caffieneISIaddedCropped.csv", row.names = FALSE)
  
  
}


options(warn=-1)
Correction <- "bonferroni"
Correction <- "none"
Correction <- "tukey"
OB_isi_removed_Outlier709Removed
outputList <- Nback_output_mixed_models_caffeine_study_errors(TRUE, OB_isi_removed, TB_isi_removed, MOB_isi_removed, GNG_isi_removed, TRUE, Correction)

outputList <- Nback_output_mixed_models_caffeine_study_errors(TRUE, OB_isi_removed_Cropped2, TB_isi_removed_Cropped2, MOB_isi_removed_Cropped2, GNG_isi_removed_Cropped2, TRUE, Correction)
outputList <- Nback_output_mixed_models_caffeine_study_errors(TRUE, OB_isi_removed_Outlier709Removed, TB_isi_removed_Outlier709Removed, MOB_isi_removed_Outlier709Removed, GNG_isi_removed_Outlier709Removed, TRUE, Correction)

outputList <- Nback_output_mixed_models_caffeine_study_errors(TRUE, OB_addedTimeCropped, TB_addedTimeCropped, MOB_addedTimeCropped, GNG_addedTimeCropped, TRUE, Correction)



####Reported analysis
outputList <- Nback_output_mixed_models_caffeine_study_errors(TRUE, OB_addedTimeCroppedRemovedISI, TB_addedTimeCroppedRemovedISI, MOB_addedTimeCroppedRemovedISI, GNG_addedTimeCroppedRemovedISI, TRUE, Correction)


#wordTableGenerator(outputList, FALSE)
#load("//root/projects/Caffeine_ONR_Study/performanceTestData/tables/outputList2.RData")

wordTableGenerator(outputList, TRUE)

if(FALSE){
  doc = docx()
  
}

#wordTableGeneratorTwoTables(outputList, outputList2)