library(rvest)
library(tidyverse)
library(stringr)

cases <- list.files("data/pg")

check.numeric <- function(N){
  !length(grep("[^[:digit:]]", as.character(N)))
}

for (i in 1:length(cases)) {
  
  case_url <- paste0("data/pg/",cases[i])
  
  first_char <-check.numeric(substring(cases[i], 1,1))
  
  array1 <- read_html(case_url) %>%
    html_nodes(".FirstColumnPrompt") %>%
    html_text()
  
  array2 <- read_html(case_url) %>%
    html_nodes(".Value") %>%
    html_text()
  
  #BLAHBLAHBLAH
  array_length <- 1:length(array2)
  
  if (first_char==F) {
    for (z in array_length) {
      
      if (array1[z]=="Party Type:" & !is.na(array1[z])) {
        array1 <- append(array1, "Party No.:", after=z)
        
        
      }
      if (array1[z]=="City:" & !is.na(array1[z])) {
        array1 <- append(array1, "State:", after=z)
        
        
      }
      if (array1[z]=="State:" & !is.na(array1[z])) {
        array1 <- append(array1, "Zip Code:", after=z)
        
        
      }
      if (array1[z]=="Event Date:" & !is.na(array1[z])) {
        array1 <- append(array1, "Start Time:", after=z)
        
        
      }
      if (array1[z]=="Result:" & !is.na(array1[z])) {
        array1 <- append(array1, "Result Date:", after=z)
        
        
      }
    }
  } else {
    
    for (z in array_length) {
      
      if (z!=1) {
        if (array1[z]=="Case Number:" & !is.na(array1[z])) {
          array1 <- append(array1, "Claim Type:", after=z)
          
          
        }
      if (array1[z]=="District/Location Codes:" & !is.na(array1[z])) {
        array1 <- append(array1, "Filing Date:", after=z)
        
        
      }
      if (array1[z]=="Filing Date:" & array1[z-1]=="District/Location Codes:" & z!=1  & !is.na(array1[z])) {
        array1 <- append(array1, "Case Status:", after=z)
        
        
      }
      if (array1[z]=="Complaint No:"  & !is.na(array1[z])) {
        array1 <- append(array1, "Plaintiff:", after=z)
        
        
      }     
        if (array1[z]=="Plaintiff:" & !is.na(array1[z])) {
        array1 <- append(array1, "Defendant:", after=z)
        
        
      }
      if (array1[z]=="Status Date:" & !is.na(array1[z])) {
        array1 <- append(array1, "Filing Date:", after=z)
        
        
      }
        if (array1[z]=="Date:" & array1[z-1]=="Last Activity Date:" & !is.na(array1[z])) {
          array1 <- append(array1, "Time:", after=z)
          
          
        }
        if (array1[z]=="Time:" & array1[z-1]=="Date:" & !is.na(array1[z])) {
          array1 <- append(array1, "Room:", after=z)
          
          
        }        
        if (array1[z]=="Est. Duration:" & !is.na(array1[z])) {
          array1 <- append(array1, "Type:", after=z)
          
          
        }
        if (array1[z]=="Filing Date:" & array1[z-1]=="Status Date:" & !is.na(array1[z])) {
        array1 <- append(array1, "Amount:", after=z)
        
        
      }
      if (array1[z]=="Amount:" & array1[z-1]=="Filing Date:" & !is.na(array1[z])) {
        array1 <- append(array1, "Last Activity Date:", after=z)
        
        
      }
      if (array1[z]=="Judgment Type:" & !is.na(array1[z])) {
        array1 <- append(array1, "Judgment Date:", after=z)
        
        
      }
      if (array1[z]=="Judgment Amount:" & !is.na(array1[z])) {
        array1 <- append(array1, "Judgment Interest:", after=z)
        
        
      }
      if (array1[z]=="Judgment Interest:" & !is.na(array1[z])) {
        array1 <- append(array1, "Costs:", after=z)
        
        
      }
      if (array1[z]=="Costs:" & !is.na(array1[z])) {
        array1 <- append(array1, "Other Amounts:", after=z)
        
        
      }
      # ARGH ATTORNEY FEES
      if (array1[z]=="Attorney Fees:" & !is.na(array1[z])) {
        array1 <- append(array1, "Jointly and Severally:", after=z)
        
        
      }
      #  if (array1[z]=="Attorney Fees:" & !is.na(array1[z])) {
      #    array1 <- append(array1, "Post Interest Legal Rate:", after=z)
          
          
      #  }
      #  if (array1[z]=="Post Interest Legal Rate:" & !is.na(array1[z])) {
      #    array1 <- append(array1, "Jointly and Severally:", after=z)
          
          
       # }
      if (array1[z]=="Jointly and Severally:" & !is.na(array1[z])) {
        array1 <- append(array1, "in Favor of Defendant:", after=z)
        
        
      }
      if (array1[z]=="Possession Of Property Claimed valued At:" & !is.na(array1[z])) {
        array1 <- append(array1, "Is Awarded to The:", after=z)
        
        
      }
      if (array1[z]=="Is Awarded to The:" & array1[z-1]=="Possession Of Property Claimed valued At:" & !is.na(array1[z])) {
          array1 <- append(array1, "Together With Damages:", after=z)
          
          
      }
      if (array1[z]=="Value Of Property Sued For:" & !is.na(array1[z])) {
          array1 <- append(array1, "Plus Damages Of:", after=z)
          
          
        }
      if (array1[z]=="Plus Damages Of:" & !is.na(array1[z])) {
        array1 <- append(array1, "Is Awarded To The:", after=z)
        
        
      }
      if (array1[z]=="Is Awarded To The:" & array1[z-1]=="Plus Damages Of:" & !is.na(array1[z])) {
        array1 <- append(array1, "Dismissed With Prejudice:", after=z)
        
        
      }
      if (array1[z]=="Recorded Lien Date:" & !is.na(array1[z])) {
        array1 <- append(array1, "Judgment renewed Date:", after=z)
        
        
      }
      if (array1[z]=="Renewed Lien Date:" & !is.na(array1[z])) {
        array1 <- append(array1, "Satisfaction Date:", after=z)
        
        
      }
      if (array1[z]=="If Person is Attorney:" & !is.na(array1[z])) {
        array1 <- append(array1, "Attorney's Firm:", after=z)
        
        
      }
        if (array1[z]=="City:" & !is.na(array1[z])) {
          array1 <- append(array1, "State:", after=z)
          
          
        }
      if (array1[z]=="State:" & !is.na(array1[z])) {
        array1 <- append(array1, "Zip Code:", after=z)
        
        
      }
      if (array1[z]=="Event Date:" & !is.na(array1[z])) {
        array1 <- append(array1, "Start Time:", after=z)
        
        
      }
      if (array1[z]=="Result:" & !is.na(array1[z])) {
        array1 <- append(array1, "Result Date:", after=z)
        
        
      }
      if (str_trim(array1[z])=="Type:" & array1[z-1]!="Defendant:"  & array1[z-1]!="Est. Duration:" & !is.na(array1[z])) {
        array1 <- append(array1, "Complaint No.:", after=z)
        
        
      }
      if (array1[z]=="Date:" & array1[z-1]=="Complaint No.:" & !is.na(array1[z])) {
        array1 <- append(array1, "Comment:", after=z)
        
        
      }
      #  if (array1[z]=="If Person is Attorney:") {
      #    array1 <- append(array1, "Attorney Code:", after=z)
      #  }
      #  if (array1[z]=="Attorney Code:") {
      #    array1 <- append(array1, "Attorney's Firm:", after=z)
      #  }
    }
    }
  }
  
  
  array2 <- read_html(case_url) %>%
    html_nodes(".Value") %>%
    html_text()
  
  if (length(array1)!=length(array2)) {
    for (z in array_length) {
      if (z!=1) {
        if (array1[z]=="Attorney Fees:" & !is.na(array1[z])) {
            array1 <- append(array1, "Post Interest Legal Rate:", after=z)
          }
        }
    }
  }
  
  #case_num <- gsub(".*caseId=", "", case_url)
  case_num <- gsub(".*\\/", "", case_url)
  #case_num <- gsub("\\&.*", "", case_num)
  case_num <- gsub(".html", "", case_num)
  
  if (length(array1)!=length(array2)) {
    for (z in 1:length(array1)) {
      if (!is.na(array1[z]) & array1[z]=="City:" & nchar(array2[z])==2)
          array1 <- array1[-z]
    }
  }
  
  if (length(array1)!=length(array2)) {
    write.csv(array1, paste0(case_num, "_array1.csv"))
    write.csv(array2, paste0(case_num, "_array2.csv" ))
    
  } else {
  
  
  new_array <- data.frame(Type=array1, Details=array2)
  new_array$Type <- as.character(new_array$Type)
  new_array$Details <- as.character(new_array$Details)
  
  new_array$row <- row.names(new_array)
  
  
  #case_num <- filter(new_array, Type=="Case Number:")
  #case_num <- as.character(case_num$Details)
  
  new_array$case_num <- case_num
  
  new_array <- select(new_array, case_num, row, Type, Details)
  
  new_array$case <- case_num
  
  #Sys.sleep(2)
  print(i)
  }
}

write.csv(full_array, "full_array.csv")

#write.csv(summary_array, "summary.csv")

full_df <- read_csv("full_array.csv")
# summarizer

cases <- unique(full_df$case_num)



for (x in 1:length(cases)) {
 
  sub_df <- filter(full_df, case_num==cases[x])
  case_num <- cases[x]
  if (exists("description")) {
    rm(description)
  }
  
  for (i in 1:nrow(sub_df)) {
    if (!is.na(sub_df$Type[i])) {
      if (sub_df$Type[i]=="Case Description:") {
        description <- sub_df$Details[i]
      } else if (sub_df$Type[i]=="Plaintiff:") {
        plaintiff <- sub_df$Details[i]
      } else if(sub_df$Type[i]=="Defendant:") {
        defendant <- sub_df$Details[i]
        description <- paste(plaintiff, "vs.", defendant)
      }
    }
  }
  if (!exists("description")) {
    description <- ""
  }
  
  summ <- data.frame(case_num, description)
  
  if (x==1) {
    summary <- summ
  } else {
    summary <- rbind(summary, summ)
  }
  
}

write.csv(summary_array, "summary.csv")
