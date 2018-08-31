library(ReporteRs)
library(magrittr)
library(data.table)


wordTableGenerator <- function(outputList, n){
  
  baseCellProp = cellProperties( padding = 0 )
  options("ReporteRs-fontsize"=8)
  
  library(ReporteRs)
  library(magrittr)
  doc = docx()

  doc = addParagraph( doc, "A FlexTable example", stylename = "TitleDoc" )
  # add a section title
  #doc = addTitle( doc, "How to turn a data.frame into a Word table", level = 1 )
  final_df <- data.frame()
  
  for(i in 1:length(outputList)){

      
      currTitle <- outputList[[i]][[1]][[1]]
      
      currDF0 <- outputList[[i]][[1]][[3]]
      
      currDF <-  setDT(currDF0, keep.rownames = TRUE)[]
      
      colnames(currDF)[1] <- "Dependent measures"
      colnames(currDF)[2] <- "df"
      colnames(currDF)[3] <- "Error"
      colnames(currDF)[4] <- "F"
      colnames(currDF)[5] <- "p"
      currDF <- currDF[2:length(currDF$p),]
      
      currDF$p <- ifelse(round(currDF$p, digits = 3) == 1, "1", ifelse(round(currDF$p, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5) ) )
                          
      
      currDF$F <-  as.character(round(currDF$F, digits = 3))
      
      currDF$outcomeMeasure <- ""
      
      #doc = addParagraph( doc, currTitle, stylename = "DocDefaults" )
      
      header1 <- setNames(data.frame(matrix(ncol = 6, nrow = 1)), colnames(currDF))
      header1$outcomeMeasure[1] <- currTitle
      header1$df <- ""
      header1$Error <- ""
      currdf20 <- rbind(header1, currDF)
      currDF3 <- data.frame(currdf20)
      final_df <- rbind(final_df, currDF3)
    
    # write the doc
  
    

  }
  final_df2 <- final_df
  final_df <- final_df[c("outcomeMeasure", "Dependent.measures", "df", "Error", "F", "p")]
  
  final_df$Dependent.measures <- ifelse(final_df$Dependent.measures == "condition2", "Condition", 
                        ifelse(final_df$Dependent.measures == "light", "Light",  
                               ifelse(final_df$Dependent.measures == "test_period", "Period",
                                      ifelse(final_df$Dependent.measures == "condition2:light", "Condition:Light",
                                             ifelse(final_df$Dependent.measures == "condition2:test_period", "Condition:Period", 
                                                    ifelse(final_df$Dependent.measures == "light:test_period", "Light:Period", 
                                                           ifelse(final_df$Dependent.measures == "condition2:light:test_period", "Condition:Light:Period",  final_df$Dependent.measures)))))))
  
  
  final_df$outcomeMeasure <- gsub("_", " ",  final_df$outcomeMeasure)
  colnames(final_df)[1] <- "Outcome measures"
  colnames(final_df)[2] <- "Dependent measures"
  
  MyFTable = vanilla.table( data = final_df )
  
  textProp <- textProperties()
  
  MyFTable[as.numeric(substr(final_df$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
  
  doc = addFlexTable(doc, MyFTable)
  
  n <- readline(prompt="Analysis Specification: ")
  
  n2 <- gsub(" ", "_", n)

  
  dir <- "//root/projects/Caffeine_ONR_Study/output-MarkdownFiles/results-output/"
  filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "CaffeineResults-MainModel-", n2, ".docx")
  
  
  writeDoc( doc, file = filename )
  

}


wordTableGeneratorTwoTables <- function(outputList, outputList2, n){
  options("ReporteRs-fontsize"=8)
  
  baseCellProp = cellProperties( padding = 0 )
  library(tibble)
  
  library(ReporteRs)
  library(magrittr)
  doc = docx()
  
  doc = addParagraph( doc, "A FlexTable example", stylename = "TitleDoc" )
  # add a section title
  #doc = addTitle( doc, "How to turn a data.frame into a Word table", level = 1 )
  final_df <- data.frame()
  
  for(i in 1:length(outputList)){
    
    currTitle <- outputList[[i]][[1]][[1]]
    
    currDF0 <- outputList[[i]][[1]][[3]]
    
    #currDF <-  setDT(currDF0, keep.rownames = TRUE)[]
    currDF <-  data.frame(currDF0)
    
    currDF <- rownames_to_column(currDF, "Dependent measures")
    
    colnames(currDF)[2] <- "df"
    colnames(currDF)[3] <- "Error"
    colnames(currDF)[4] <- "F"
    colnames(currDF)[5] <- "p"
    currDF <- currDF[2:length(currDF$p),]
    
    currDF$p <- ifelse(round(currDF$p, digits = 3) == 1, "1", ifelse(round(currDF$p, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5) ) )
    
    
    currDF$F <-  as.character(round(currDF$F, digits = 3))
    
    currDF$outcomeMeasure <- ""
    
    #doc = addParagraph( doc, currTitle, stylename = "DocDefaults" )
    
    header1 <- setNames(data.frame(matrix(ncol = 6, nrow = 1)), colnames(currDF))
    header1$outcomeMeasure[1] <- currTitle
    header1$df <- ""
    header1$Error <- ""
    currdf20 <- rbind(header1, currDF)
    currDF3 <- data.frame(currdf20)
    final_df <- rbind(final_df, currDF3)
    
    # write the doc
    
    
    
  }
  #final_df2 <- final_df
  final_df <- final_df[c("outcomeMeasure", "Dependent.measures", "df", "Error", "F", "p")]
  
  final_df$Dependent.measures <- ifelse(final_df$Dependent.measures == "condition2", "Condition", 
                                        ifelse(final_df$Dependent.measures == "light", "Light",  
                                               ifelse(final_df$Dependent.measures == "test_period", "Period",
                                                      ifelse(final_df$Dependent.measures == "condition2:light", "Condition:Light",
                                                             ifelse(final_df$Dependent.measures == "condition2:test_period", "Condition:Period", 
                                                                    ifelse(final_df$Dependent.measures == "light:test_period", "Light:Period", 
                                                                           ifelse(final_df$Dependent.measures == "condition2:light:test_period", "Condition:Light:Period",  final_df$Dependent.measures)))))))
  
  
  final_df$outcomeMeasure <- gsub("_", " ",  final_df$outcomeMeasure)
  colnames(final_df)[1] <- "Outcome measures"
  colnames(final_df)[2] <- "Dependent measures"
  
  
  final_df$"" <- ""
  
  ####
  
  final_df2 <- data.frame()
  
  for(i in 1:length(outputList2)){
    
    
    currTitle <- outputList2[[i]][[1]][[1]]
    
    currDF0 <- outputList2[[i]][[1]][[3]]
    
    currDF <-  data.frame(currDF0)
    
    currDF <- rownames_to_column(currDF, "Dependent measures")
    
    colnames(currDF)[2] <- "df"
    colnames(currDF)[3] <- "Error"
    colnames(currDF)[4] <- "F"
    colnames(currDF)[5] <- "p"
    currDF <- currDF[2:length(currDF$p),]
    
    currDF$p <- ifelse(round(currDF$p, digits = 3) == 1, "1", ifelse(round(currDF$p, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5) ) )
    
    
    currDF$F <-  as.character(round(currDF$F, digits = 3))
    
    
    #doc = addParagraph( doc, currTitle, stylename = "DocDefaults" )
    
    header1 <- setNames(data.frame(matrix(ncol = 5, nrow = 1)), colnames(currDF))
    header1$df <- ""
    header1$Error <- ""
    currdf20 <- rbind(header1, currDF)
    currDF3 <- data.frame(currdf20)
    final_df2 <- rbind(final_df2, currDF3)
    
    # write the doc
    
    
    
  }
  final_df22 <- final_df2
  final_df2$Dependent.measures <- NULL
  
  final_df2 <- final_df2[c("df", "Error", "F", "p")]
  

  
  final_df4 <- cbind(final_df, final_df2)
  ####
  MyFTable = vanilla.table( data = final_df4 )
  
  textProp <- textProperties()
  
  MyFTable[as.numeric(substr(final_df4$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
  
  doc = addFlexTable(doc, MyFTable)
  
  n <- readline(prompt="Analysis Specification: ")
  
  n2 <- gsub(" ", "_", n)
  
  
  
  
  
  
  dir <- "//root/projects/Caffeine_ONR_Study/output-MarkdownFiles/results-output/"
  filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "CaffeineResults-MainModelComparison-", n2, ".docx")
  
  
  writeDoc( doc, file = filename )
  
  
}

