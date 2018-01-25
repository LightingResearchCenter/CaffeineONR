
library(plyr)
library('scales')
library(assertthat)

# Read data, build 3 data frames for each test of all data 

readData <- function( test) {
  options(digits=12)
  df = data.frame()
  
  library(readr)
  curr.dir = paste("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/complete_data0") # paste("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/session", as.character(session), sep = '')  
  library(readxl)
  ref_sheet <- read_excel("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/subConditions.xlsx")
  
  ref_sheet$date_start <-  as.Date(ref_sheet$date, format='%m/%d/%Y')
  ref_sheet$date_end <-  as.Date(ref_sheet$date, format='%m/%d/%Y') +1
  



  if(length(list.files(curr.dir)) == 0){
    next
  }else{
    subjects0 <- list.files(curr.dir)
    for(n in 1:length(subjects0)){
      curr.subject.dir <- paste(curr.dir, subjects0[n], sep = "/")
      #curr.test <- paste(curr.subject, "GNG", sep = "/")
      GNG_dir <- paste(curr.subject.dir, "GNG", sep = "/")
      OB_dir <- paste(curr.subject.dir, "OB", sep = "/")
      TB_dir <- paste(curr.subject.dir, "TB", sep = "/")
      MOB_dir <- paste(curr.subject.dir, "MOB", sep = "/")
      
      GNG.file.names <- list.files(GNG_dir)
      OB.file.names <- list.files(OB_dir)
 
      TB.file.names <- list.files(TB_dir)
      MOB.file.names <- list.files(MOB_dir)
      
      print(subjects0[n]) 
      subid2 <- substr(subjects0[n], 2, 4)
      
      
      
      GNG.files <- GNG.file.names[grep('GNG', GNG.file.names)]
      OB.files <- OB.file.names[grep('OB', OB.file.names)]
      TB.files <- TB.file.names[grep('TwoBack', TB.file.names )]
      MOB.files <- MOB.file.names[grep('MOB', MOB.file.names)]

            #if(file.info(GNG.files[i]$size) > 117)
        
        
      if(test == "GNG"){
        for(i in 1:length(GNG.files)){
          curr.block <- GNG.files[i]
          
          
          print(curr.block)
          print(file.info(paste(GNG_dir,  curr.block, sep = "/"))$size)
          if (file.info(paste(GNG_dir,  curr.block, sep = "/"))$size < 2000){
            next
          }
          
          file1 <- read.delim( paste(GNG_dir,  curr.block, sep = "/"))
          
          print(file1$Subject_id[1])
          
          
          file1$Subject_id <- subjects0[n]
          file1$date <- as.Date(file1$date, format='%Y-%m-%d')
          
          file1$condition <- ref_sheet[ref_sheet$Subject_id == as.numeric(subid2) & ref_sheet$date_start <= file1$date[1] & ref_sheet$date_end >= file1$date[1],]$condition
          
          
          file1$uni1 <- paste(file1$Subject_id[1], file1$date[1], file1$block[1], "GNG", sep="-")
          file1$uni2 <- paste(file1$Subject_id[1], file1$date[1], "GNG", sep="-")
          file1$filename <- GNG.files[i]
          
          df <- rbind(df, file1)
          
          
          
        }
      } 
      
      if(test == "MOB"){
        for(i in 1:length(MOB.files)){
          curr.block <- MOB.files[i]
          
          
          print(curr.block)
          
          if (file.info(paste(MOB_dir,  curr.block, sep = "/"))$size < 2000){
            next
          }
          
          file1 <- read.delim( paste(MOB_dir,  curr.block, sep = "/"))
          
          print(file1$Subject_id[1])
          file1$Subject_id <- subjects0[n]
          
          file1$date <- as.Date(file1$date, format='%Y-%m-%d')
          
          file1$condition <- ref_sheet[ref_sheet$Subject_id == as.numeric(subid2) & ref_sheet$date_start <= file1$date[1] & ref_sheet$date_end >= file1$date[1],]$condition
          
          
          
          
          
          file1$uni1 <- paste(file1$Subject_id[1], file1$date[1], file1$block[1], "GNG", sep="-")
          file1$uni2 <- paste(file1$Subject_id[1], file1$date[1], "GNG", sep="-")
          file1$filename <- MOB.files[i]
          
          df <- rbind(df, file1)
          
          
          
        }
      } 
      
      if(test == "OB"){
        for(i in 1:length(OB.files)){
          curr.block <- OB.files[i]
          
          
          print(curr.block)
          
          if (file.info(paste(OB_dir,  curr.block, sep = "/"))$size < 2000){
            next
          }
          
          file1 <- read.delim( paste(OB_dir,  curr.block, sep = "/"))
          
          print(file1$Subject_id[1])
          file1$Subject_id <- subjects0[n]
          
          file1$date <- as.Date(file1$date, format='%Y-%m-%d')
          
          file1$condition <- ref_sheet[ref_sheet$Subject_id == as.numeric(subid2) & ref_sheet$date_start <= file1$date[1] & ref_sheet$date_end >= file1$date[1],]$condition
          
          
          
          
          file1$uni1 <- paste(file1$Subject_id[1], file1$date[1], file1$block[1], "OB", sep="-")
          file1$uni2 <- paste(file1$Subject_id[1], file1$date[1], "OB", sep="-")
          file1$filename <- OB.files[i]
          
          df <- rbind(df, file1)
          
          
          
        }
      } 
      
      if(test == "TB"){
        for(i in 1:length(TB.files)){
          curr.block <- TB.files[i]
          
          
          print(curr.block)
          
          if (file.info(paste(TB_dir,  curr.block, sep = "/"))$size < 2000){
            next
          }
          
          file1 <- read.delim( paste(TB_dir,  curr.block, sep = "/"))
          
          print(file1$Subject_id[1])
          file1$Subject_id <- subjects0[n]
          
          file1$date <- as.Date(file1$date, format='%Y-%m-%d')
          
          file1$condition <- ref_sheet[ref_sheet$Subject_id == as.numeric(subid2) & ref_sheet$date_start <= file1$date[1] & ref_sheet$date_end >= file1$date[1],]$condition
          file1$filename <- TB.files[i]
          
          
          
          
          
          file1$uni1 <- paste(file1$Subject_id[1], file1$date[1], file1$block[1], "TB", sep="-")
          file1$uni2 <- paste(file1$Subject_id[1], file1$date[1], "TB", sep="-")
          
          df <- rbind(df, file1)
          
          
          
        }
      } 
      
      
    }
  }
  
  return(df)}

 

  OB <- readData("OB")
  TB <- readData("TB")
  MOB <- readData("MOB")
  GNG <- readData("GNG")
  
  OB_filename <- paste0("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData/",format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "OB.csv")
  TB_filename <- paste0("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData/",format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "TB.csv")
  MOB_filename <- paste0("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData/", format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "MOB.csv")
  GNG_filename <- paste0("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData/", format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "GNG.csv")
  
  write.csv(OB, OB_filename, row.names = FALSE)
  write.csv(TB, TB_filename, row.names = FALSE)
  write.csv(MOB, MOB_filename, row.names = FALSE)
  write.csv(GNG, GNG_filename, row.names = FALSE)
  
