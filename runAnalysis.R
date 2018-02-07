library(readr)
source("C:/Users/roohac/Documents/GitHub/CaffeineONR/analysisFunctions.R")

GNG_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_GNG_addedTimeCropped.csv")
OB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_OB_addedTimeCropped.csv")
TB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_TB_addedTimeCropped.csv")
MOB_addedTimeCropped <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_MOB_addedTimeCropped.csv")


options(warn=-1)

outputList <- Nback_output_mixed_models_caffeine_study_errors(OB_addedTimeCropped, TB_addedTimeCropped, MOB_addedTimeCropped, GNG_addedTimeCropped, TRUE)


#wordTableGenerator(outputList, TRUE)
