library(readr)

source('~/GitHub/CaffeineONR/testOutputWord.R', echo=TRUE)

source('~/GitHub/CaffeineONR/completeAnalysisFunctions10_8-18.R', echo=TRUE)



###***reported analysis
GNG_addedTimeCroppedRemovedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/GNG_caffieneISIaddedRemovedISI.csv")
OB_addedTimeCroppedRemovedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/OB_caffieneISIaddedRemovedISI.csv")
TB_addedTimeCroppedRemovedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/TB_caffieneISIaddedRemovedISI.csv")
MOB_addedTimeCroppedRemovedISI <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/ISI_addedData/MOB_caffieneISIaddedRemovedISI.csv")


options(warn=-1)
Correction <- "bonferroni"
Correction <- "none"
Correction <- "tukey"

####Reported analysis
outputList <- Nback_output_mixed_models_caffeine_study_errors(TRUE, OB_addedTimeCroppedRemovedISI, TB_addedTimeCroppedRemovedISI, MOB_addedTimeCroppedRemovedISI, GNG_addedTimeCroppedRemovedISI, TRUE, Correction)


#wordTableGenerator(outputList, FALSE)
#load("//root/projects/Caffeine_ONR_Study/performanceTestData/tables/outputList2.RData")

wordTableGenerator(outputList, TRUE)

if(FALSE){
  doc = docx()
  
}

#wordTableGeneratorTwoTables(outputList, outputList2)