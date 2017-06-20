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
  
  #BLAHBLAHBLAH
  
  if (first_char==F) {
    for (z in 1:length(array1)) {
      
      if (array1[z]=="Party Type:") {
        array1 <- append(array1, "Party No.:", after=z)
      }
      if (array1[z]=="City:") {
        array1 <- append(array1, "State:", after=z)
      }
      if (array1[z]=="State:") {
        array1 <- append(array1, "Zip Code:", after=z)
      }
      if (array1[z]=="Event Date:") {
        array1 <- append(array1, "Start Time:", after=z)
      }
      if (array1[z]=="Result:") {
        array1 <- append(array1, "Result Date:", after=z)
      }
    }
  } else {
    for (z in 1:length(array1)) {
      
      if (z!=1) {
        if (array1[z]=="Case Number:") {
          array1 <- append(array1, "Claim Type:", after=z)
        }
      if (array1[z]=="District/Location Codes:") {
        array1 <- append(array1, "Filing Date:", after=z)
      }
      if (array1[z]=="Filing Date:" & array1[z-1]=="District/Location Codes:" & z!=1) {
        array1 <- append(array1, "Case Status:", after=z)
      }
      if (array1[z]=="Complaint No:") {
        array1 <- append(array1, "Plaintiff:", after=z)
      }     
        if (array1[z]=="Plaintiff:") {
        array1 <- append(array1, "Defendant:", after=z)
      }
      if (array1[z]=="Status Date:") {
        array1 <- append(array1, "Filing Date:", after=z)
      }
      if (array1[z]=="Filing Date:" & array1[z-1]=="Status Date:") {
        array1 <- append(array1, "Amount:", after=z)
      }
      if (array1[z]=="Amount:" & array1[z-1]=="Filing Date:") {
        array1 <- append(array1, "Last Activity Date:", after=z)
      }
      if (array1[z]=="Judgment Type:") {
        array1 <- append(array1, "Judgment Date:", after=z)
      }
      if (array1[z]=="Judgment Amount:") {
        array1 <- append(array1, "Judgment Interest:", after=z)
      }
      if (array1[z]=="Judgment Interest:") {
        array1 <- append(array1, "Costs:", after=z)
      }
      if (array1[z]=="Costs:") {
        array1 <- append(array1, "Other Amounts:", after=z)
      }
      if (array1[z]=="Attorney Fees:") {
        array1 <- append(array1, "Jointly and Severally:", after=z)
      }
      if (array1[z]=="Jointly and Severally:") {
        array1 <- append(array1, "in Favor of Defendant:", after=z)
      }
      if (array1[z]=="Possession Of Property Claimed valued At:") {
        array1 <- append(array1, "Is Awarded to The:", after=z)
      }
      if (array1[z]=="Is Awarded to The:" & array1[z-1]=="Possession Of Property Claimed valued At:") {
          array1 <- append(array1, "Together With Damages:", after=z)
      }
      if (array1[z]=="Value Of Property Sued For:") {
          array1 <- append(array1, "Plus Damages Of:", after=z)
        }
      if (array1[z]=="Plus Damages Of:") {
        array1 <- append(array1, "Is Awarded To The:", after=z)
      }
      if (array1[z]=="Is Awarded To The:" & array1[z-1]=="Plus Damages Of:") {
        array1 <- append(array1, "Dismissed With Prejudice:", after=z)
      }
      if (array1[z]=="Recorded Lien Date:") {
        array1 <- append(array1, "Judgment renewed Date:", after=z)
      }
      if (array1[z]=="Renewed Lien Date:") {
        array1 <- append(array1, "Satisfaction Date:", after=z)
      }
      if (array1[z]=="If Person is Attorney:") {
        array1 <- append(array1, "Attorney's Firm:", after=z)
      }
        if (array1[z]=="City:") {
          array1 <- append(array1, "State:", after=z)
        }
      if (array1[z]=="State:") {
        array1 <- append(array1, "Zip Code:", after=z)
      }
      if (array1[z]=="Event Date:") {
        array1 <- append(array1, "Start Time:", after=z)
      }
      if (array1[z]=="Result:") {
        array1 <- append(array1, "Result Date:", after=z)
      }
      if (str_trim(array1[z])=="Type:" & array1[z-1]!="Defendant:") {
        array1 <- append(array1, "Complaint No.:", after=z)
      }
      if (array1[z]=="Date:" & array1[z-1]=="Complaint No.:") {
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
  
  new_array <- data.frame(Type=array1, Details=array2)
  new_array$Type <- as.character(new_array$Type)
  new_array$Details <- as.character(new_array$Details)
  
  new_array$row <- row.names(new_array)
  
  case_num <- filter(new_array, Type=="Case Number:")
  case_num <- as.character(case_num$Details)
  
  new_array$case_num <- case_num
  
  new_array <- select(new_array, case_num, row, Type, Details)
  case_num <- gsub(".*caseId=", "", case_url)
  case_num <- gsub("\\&.*", "", case_num)
  
  new_array$case <- case_num
  
  for (x in 1:nrow(new_array)) {
    if (new_array$Type[x]=="Case Description:") {
      description<- new_array$Details[x]
    } else if (new_array$Type[x]=="Case Type:") {
      case_type<- new_array$Details[x]
    } else if (new_array$Type[x]=="Filing Date:") {
      filing_date<- new_array$Details[x]
    } else if (new_array$Type[x]=="Case Status:") {
      case_status<- new_array$Details[x]
    } else if (new_array$Type[x]=="Party Type:" & grepl("Hamilton", new_array$Details[x+2])) {
      party_type<- new_array$Details[x]
      party_name <- new_array$Details[x+2]
    } else if (new_array$Type[x]=="Party Type:" & grepl("Princeton", new_array$Details[x+2])) {
      party_type<- new_array$Details[x]
      party_name <- new_array$Details[x+2]
    } else if (new_array$Type[x]=="Result:") {
      result <- gsub("CaseDisp: ", "", new_array$Details[x])
    }
  }
  
  type <- filter(new_array, grepl("CaseType:", Details))
  type <- gsub("CaseType: ", "", result)
  
  sum_array <- data.frame(case_num, description, case_type, filing_date, case_status, party_type, party_name, result, type)
  
  if (i==1) {
    full_array <- new_array
    summary_array <- sum_array
  } else {
    full_array <- rbind(full_array, new_array)
    summary_array <- rbind(summary_array, sum_array)
  }
  Sys.sleep(2)
}

write.csv(full_array, "full_array.csv")
write.csv(summary_array, "summary.csv")
