library(ReporteRs)
library(magrittr)




if(FALSE){
  
  ###transform summary to data.frame
  
  ###Function to import a model
  df1 <- data.frame(anova(hitrate_model))
  ###take df1 into a table and output into word
  df2 <- summary(lsmeans(hitrate_model, pairwise~ session, adjust="tukey", data = hit_rate))
  df2 <- lsmeans(hitrate_model, pairwise~ session, adjust="tukey", data = hit_rate)
  
  ###The fitst data frame will be used for graphing
  df3<- data.frame(df[1])
  ###this data frame will be used for finding significane differences
  df4<- data.frame(df[2])
  
  outputList[[1]][[1]][[1]]
  outputList[[1]][[1]][[2]]
  outputList[[1]][[1]][[3]]
  
  outputList[[1]][[2]][[2]][[1]]
  
  wordTableGenerator(outputList, TRUE)
  
  for(i in 1:length(outputList)){
    print(outputList[[i]][[1]][[1]])
    print(length(outputList[[i]][[2]]))
    #print(length(outputList[[i]][[2]]))
  }
}



wordTableGenerator <- function(outputList, post_hoc){
  
  baseCellProp = cellProperties( padding = 0 )
  
  library(ReporteRs)
  library(magrittr)
  doc = docx()

  doc = addParagraph( doc, "A FlexTable example", stylename = "TitleDoc" )
  # add a section title
  doc = addTitle( doc, "How to turn a data.frame into a Word table", level = 1 )
  final_df <- data.frame()
  
  for(i in 1:length(outputList)){
    if(!(post_hoc)){
      currTitle <- outputList[[i]][[1]]
      
      currDF <- outputList[[i]][[3]]
      
      
      colnames(currDF)[1] <- "df"
      colnames(currDF)[2] <- "Error"
      colnames(currDF)[3] <- "F"
      colnames(currDF)[4] <- "p"
      currDF <- currDF[2:length(currDF$p),]
      
      currDF$p <-  ifelse(!is.na(currDF$p),ifelse(round(currDF$p, digits = 3) == 1, "1",  substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5)) )
      
      currDF$F <-  as.character(round(currDF$F, digits = 3))
      
      doc = addParagraph( doc, currTitle, stylename = "DocDefaults" )
      
      MyFTable = vanilla.table( data = currDF )
      
      
      MyFTable[as.numeric(currDF$p) < .05, 4] = chprop( baseCellProp, background.color = "green")  
      
      doc = addFlexTable(doc, MyFTable)
      

    }else{
      
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
      
      #doc = addParagraph( doc, currTitle, stylename = "DocDefaults" )
      
     
      currDF$outcomeMeasure <- ""
      currDF$outcomeMeasure[1] <- currTitle
      
      currDF3 <- data.frame(currDF)
      final_df <- rbind(final_df, currDF3)
    }
    # write the doc
  
    

  }
  final_df2 <- final_df
  final_df <- final_df[c("outcomeMeasure", "Dependent.measures", "df", "Error", "F", "p")]
  
  colnames(final_df)[1] <- "Outcome measures"
  colnames(final_df)[2] <- "Dependent measures"
  
  MyFTable = vanilla.table( data = final_df )
  
  
  MyFTable[as.numeric(final_df$p) < .05, 6] = chprop( baseCellProp, background.color = "green") 
  
  doc = addFlexTable(doc, MyFTable)
  dir <- "//root/projects/Caffeine_ONR_Study/output-MarkdownFiles/results-output/"
  filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "CaffeineResults-withPostHoc.docx")
  
  
  writeDoc( doc, file = filename )
  
  # open the Word doc
  browseURL(filename)
}


