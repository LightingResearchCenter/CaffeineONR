library(readr)

addTime <- function(data){
  
  data$uni3 <- paste(data$Subject_id, data$condition, sep = "_")
  
  data$time0 <-  paste(data$date, data$hourmin, sep = "_")
  
  data$time_stamp <-  as.POSIXct(data$time0,format="%Y-%m-%d_%H-%M-%S")
  
  data$time2 <- -10000
  uni_list <- unique(data$uni3)
  
  uni_list2 <- c()
  start_of_testing_period <-c()
  initial_test <-c()
  time_stamp_current_test <-c()
  diff_output_list1 <-c()
  
  length_uni_list <- length(uni_list)
  for(i in 1:length(uni_list)){
    #print("___________________________________________________")
    #print(uni_list[i])
    initial_timestamp <- data[data$uni3 == uni_list[i],]$time_stamp[1]
    #print("First timestamp:")
    #print(initial_timestamp)
    
    date0001 <- substr(initial_timestamp, 1, 10)
    start_timestamp <- paste(date0001, "23-00-00", sep = "_")
    start_timestamp2 <- as.POSIXct(start_timestamp, format="%Y-%m-%d_%H-%M-%S")
    
    
    for(j in 1:length(data[data$uni3 == uni_list[i],]$response_time)){
      
      new_length <- length(data[data$uni3 == uni_list[i],]$response_time)
      #print(j)
      print(sprintf("uni_list[i] = %s, %i of  %i", uni_list[i], i, length_uni_list))
      print(sprintf("Trial, %i of  %i", j, new_length))
      
      #print(data[data$uni3 == uni_list[i],]$time_stamp[j])
      #print(as.numeric(difftime(data[data$uni3 == uni_list[i],]$time_stamp[j],start_timestamp2,  units = "mins" ) ))
      data[data$uni3 == uni_list[i],]$time2[j] <- as.numeric(difftime(data[data$uni3 == uni_list[i],]$time_stamp[j],start_timestamp2,  units = "mins" ) )
      #print("Data:")
      #print(data[data$uni3 == uni_list[i],]$time2[j])
      
      uni_list2 <- c(uni_list2, uni_list[i])
      start_of_testing_period <-c(start_of_testing_period, strftime(start_timestamp2,'%Y-%m-%d %H:%M') )
      initial_test <-c(initial_test, strftime(initial_timestamp,'%Y-%m-%d %H:%M'))
      time_stamp_current_test <-c(time_stamp_current_test, strftime(data[data$uni3 == uni_list[i],]$time_stamp[j],'%Y-%m-%d %H:%M'))
      diff_output_list1 <-c(diff_output_list1, data[data$uni3 == uni_list[i],]$time2[j])
      #print(start_timestamp2)
      
      
      
      
    }
    
  }
  output_data <- data.frame(uni_list2, start_of_testing_period, initial_test, time_stamp_current_test, diff_output_list1)
  return(data)
}

addTimeMain <- function( ) {
  dir <- "//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData"
  
  fileList <- list.files(dir)[grep(".csv", list.files(dir))]
  
  
  GNG_dataLoc <- paste(dir,tail(fileList[grep("_GNG", fileList)], n=1),  sep = "/")
  OB_dataLoc <- paste(dir,tail(fileList[grep("_OB", fileList)], n=1),  sep = "/")
  TB_dataLoc <- paste(dir,tail(fileList[grep("_TB", fileList)], n=1),  sep = "/")
  MOB_dataLoc <- paste(dir,tail(fileList[grep("_MOB", fileList)], n=1),  sep = "/")
  
  #format(Sys.time(), "%Y-%m-%d_%H%M%S_")
  #z <- strptime(x, "%H:%M")
  
  GNG <- read_csv(GNG_dataLoc)
  OB <- read_csv(OB_dataLoc)
  TB <- read_csv(TB_dataLoc)
  MOB <- read_csv(MOB_dataLoc)
  
  GNG2 <- addTime(GNG)
  OB2 <- addTime(OB)
  TB2 <- addTime(TB)
  MOB2<- addTime(MOB)
  
  OB_filename <- paste0("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData/",format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "OB_addedTime.csv")
  TB_filename <- paste0("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData/",format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "TB_addedTime.csv")
  MOB_filename <- paste0("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData/", format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "MOB_addedTime.csv")
  GNG_filename <- paste0("//root/projects/Caffeine_ONR_Study/performance_tests/performance_test_data/convertedData/", format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "GNG_addedTime.csv")
  
  write.csv(OB2, OB_filename, row.names = FALSE)
  write.csv(TB2, TB_filename, row.names = FALSE)
  write.csv(MOB2, MOB_filename, row.names = FALSE)
  write.csv(GNG2, GNG_filename, row.names = FALSE)
  }
addTimeMain()