library(rvest)
library(readr)
library(dplyr)
library(curl)
library(httr)

cases <- read_csv("data/cases.csv")

i <- 1

session <- html_session("http://casesearch.courts.state.md.us/casesearch/")
m1 <- html_form(read_html("http://casesearch.courts.state.md.us/casesearch/"))[[1]]
m2 <- set_values(m1, disclaimer="Y")
session <- submit_form(session, m2)

for (i in 1:length(cases)) {

    case_url <- cases$URL[i]
   
    session %>%
      download.file(case_url, destfile="scrapedpage.html", quiet=T)

    ##
    
      
    session <- html_session("http://casesearch.courts.state.md.us/casesearch/")
    
    m1 <- html_form(read_html("http://casesearch.courts.state.md.us/casesearch/"))[[1]] %>%
     set_values(disclaimer="Y")
    
    session1 <- submit_form(session, m1) 
    
    form2 <- session1 %>%
      read_html("http://casesearch.courts.state.md.us/casesearch/") 
    
    m2 <- html_form(form2)[[4]] %>%
      set_values(caseId="CAL14-11806")
    
    session2 <- submit_form(session1, m2)
    whoknows <- session2 %>%
      read_html("http://casesearch.courts.state.md.us/casesearch/inquiryDetail.jis?caseId=CAL1411806&loc=65&detailLoc=PGV") %>%
      html_nodes(".FirstColumnPrompt") %>%
      html_text()
    
    test <- session2 %>%
      read_html("data/test.html") %>%
      html_nodes(".FirstColumnPrompt") %>%
      html_text()
    
    
    #THIS IS TRICKY, I DONT GET IT
      read_html(case_url) %>%
      html_form()
      
    session1 <- session1[[4]]
    
    
    m2 <- set_values(session1, caseId="CAL14-11806") 
    
    session2 <- submit_form(session, m2) %>%
      read_html(case_url) %>%
      html_form()
    
    
    session %>%
      read_html(case_url) %>%
      html_nodes("span") %>%
      html_text()
    
    #BLAHBLAHBLAH
    
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
    
    array2 <- session %>%
      read_html(case_url) %>%
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
