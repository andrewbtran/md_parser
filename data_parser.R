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

for (i in 1:nrow(full_df)) {
  
  for (x in 1:nrow(new_array)) {
    if (new_array$Type[x]=="Case Description:") {
      description<- new_array$Details[x]
    } else if (new_array$Type[x]=="Title:") {
      case_type<- new_array$Details[x]
    } else if (new_array$Type[x]=="Plaintiff:") {
      plain <- new_array$Details[x]
    } else if (new_array$Type[x]=="Defendant:") {
      def <- new_array$Details[x]
      description <- paste(plain, "vs.", def)
      #} else if (new_array$Type[x]=="Case Type:") {
      #case_type<- new_array$Details[x]
    } else if (new_array$Type[x]=="Filing Date:") {
      filing_date<- new_array$Details[x]
    } else if (new_array$Type[x]=="Case Status:") {
      case_status<- new_array$Details[x]
      #} else if (new_array$Type[x]=="Party Type:" & grepl("Hamilton", new_array$Details[x+2])) {
      #  party_type<- new_array$Details[x]
      #  party_name <- new_array$Details[x+2]
      #} else if (new_array$Type[x]=="Party Type:" & grepl("Princeton", new_array$Details[x+2])) {
      #  party_type<- new_array$Details[x]
      #  party_name <- new_array$Details[x+2]
      #} else if (new_array$Type[x]=="Result:") {
      #  result <- gsub("CaseDisp: ", "", new_array$Details[x])
    }
  }
    
  if ("CaseType:" %in% new_array$Type) {
    type <- filter(new_array, grepl("CaseType:", Type))
    type <- type$Details
  } else if ("Claim Type:" %in% new_array$Type) {
    type <- filter(new_array, grepl("Claim Type:", Type))
    type <- type$Details
  } else if ("Case Type:" %in% new_array$Type) {
    type <- filter(new_array, grepl("Case Type:", Type))
    type <- type$Details
  }
  #sum_array <- data.frame(case_num, description, case_type, filing_date, case_status, party_type, party_name, result, type)
  sum_array <- data.frame(case_num, description, filing_date, case_status, type)
  
  if (i==1) {
    full_array <- new_array
    summary_array <- sum_array
  } else {
    full_array <- rbind(full_array, new_array)
    summary_array <- rbind(summary_array, sum_array)
  }

}

write.csv(summary_array, "summary.csv")
