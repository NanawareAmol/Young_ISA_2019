  #  old code



# https://stackoverflow.com/questions/56118999/issue-scraping-page-with-load-more-button-with-rvest
library(RSelenium)

rd <- rsDriver(browser = "chrome", port = 4444L)  # Download binaries, start driver
my_session <- rd$client # Create client object
my_session$open()  # Open session

# Navigate to page.
# my_session$navigate("https://ie.linkedin.com/jobs/search?keywords=Data%20Scientist&location=Dublin%2C%20Ireland&trk=guest_job_search_jobs-search-bar_search-submit&redirect=false&position=1&pageNum=0")
# my_session$navigate("https://ie.linkedin.com/jobs/search?keywords=Statistics&location=Dublin%2C%20Ireland&trk=guest_job_search_jobs-search-bar_search-submit&redirect=false&position=1&pageNum=0")
my_session$navigate(
  paste0("https://ie.linkedin.com/jobs/search?keywords=", 
         Statistician, 
         "&location=Dublin%2C%20Ireland&trk=guest_job_search_jobs-search-bar_search-submit&redirect=false&position=1&pageNum=0"))

# Find the load more jobs button and assign, then send click event
for (i in 1:6) {
  # Check if button still exists 
  load_btn <- 
    tryCatch(
      my_session$findElement(using = "css selector", ".see-more-jobs"),
      error = function(e) break
    )
  load_btn$clickElement()  # Click button
  Sys.sleep(runif(1, 3, 5)) # Random wait between 3 and 5 seconds
}

html_data <- my_session$getPageSource()[[1]]  # Get HTML
writeLines(html_data, "2019-09-23_LI_Statistics_Dublin.txt")  # Save Data

my_session$close() # Close session




for (jobtitle in c("Statistician", "DataScientist", "Statistics")) {
  linkedIn_raw <- 
    paste0("2019-09-23_LI_", jobtitle, "_Dublin.txt") %>%
    readLines() %>%  # Read in data
    xml2::read_html()  # Read as HTML
  
  lapply(1:300, function(x){  # Parse Initial HTML
    linkedIn_raw %>% 
      rvest::html_nodes(xpath = paste0('/html/body/main/div/section/ul/li[', x,']'))
  }) %>%
    lapply(function(x){  # Parse sub elements of HTML
      c(x %>% rvest::html_nodes(xpath = 'a') %>% 
          rvest::html_text() %>% ifelse(test = length(.) > 0, ., NA),
        x %>%  rvest::html_nodes(xpath = 'div[1]/h4/a') %>%
          rvest::html_text() %>% ifelse(test = length(.) > 0, ., NA),
        x %>% rvest::html_nodes(xpath = 'div[1]/div') %>% 
          rvest::html_text( ) %>% ifelse(test = length(.) > 0, ., NA),
        jobtitle)
    }) %>%
    do.call(what = rbind, .) -> linkedIn[[jobtitle]] # Combine data
}

job_data <- do.call(rbind, linkedIn) %>% as.data.frame(stringsAsFactors  = F) %>%
  select(Title = V1, Comapny = V2, Text = V3, SearchTerm = V4) %>%
  filter(!is.na(Title))








#### Older again


# linkedIn_raw <- xml2::read_html("https://ie.linkedin.com/jobs/data-scientist-jobs?position=1&pageNum=0")
linkedIn_raw <- xml2::read_html("linkedin2019-09-23.htm")

linkedIn_raw %>% 
  rvest::html_nodes(xpath = '/html/body/main/div/section/div[1]') %>%
  rvest::html_text()

linkedIn_raw %>% 
  rvest::html_nodes(xpath = paste0('/html/body/main/div/section/ul/li[', i,']')) %>%
  rvest::html_text()

linkedIn_raw %>% rvest::html_attrs()

linkedIn <- NULL
for (i in 1:75) {
  temp_job_data <- 
    linkedIn_raw %>% 
    rvest::html_nodes(xpath = paste0('/html/body/main/div/section/ul/li[', i,']'))
  
  linkedIn$title[i] <- temp_job_data %>%
    rvest::html_nodes(xpath = 'a') %>% 
    rvest::html_text() %>% ifelse(test = length(.) > 0, ., NA)
  
  linkedIn$company[i] <- temp_job_data %>% 
    rvest::html_nodes(xpath = 'div[1]/h4/a') %>%
    rvest::html_text() %>% ifelse(test = length(.) > 0, ., NA)
  
  linkedIn$text[i] <- temp_job_data %>% 
    rvest::html_nodes(xpath = 'div[1]/div') %>% 
    rvest::html_text( ) %>% ifelse(test = length(.) > 0, ., NA)
}




linkedIn <- list()

for (jobtitle in c("Statistician", "DataScientist", "Statistics")) {
  
  linkedIn_raw <- 
    paste0("2019-09-23_LI_", jobtitle, "_Dublin.txt") %>%
    readLines() %>%  # Read Data
    xml2::read_html()  # Parse HTML
  
  linkedIn_temp <- data.frame(title = NA, company = NA, text = NA, searchterm = jobtitle, stringsAsFactors = F)
  
  for (i in 1:300) {
    linkedIn_temp[i, ] <- c(NA, NA, NA, jobtitle)
    
    temp_job_data <- 
      linkedIn_raw %>% 
      rvest::html_nodes(xpath = paste0('/html/body/main/div/section/ul/li[', i,']'))
    
    linkedIn_temp$title[i] <- temp_job_data %>%
      rvest::html_nodes(xpath = 'a') %>% 
      rvest::html_text() %>% ifelse(test = length(.) > 0, ., NA)
    
    linkedIn_temp$company[i] <- temp_job_data %>% 
      rvest::html_nodes(xpath = 'div[1]/h4/a') %>%
      rvest::html_text() %>% ifelse(test = length(.) > 0, ., NA)
    
    linkedIn_temp$text[i] <- temp_job_data %>% 
      rvest::html_nodes(xpath = 'div[1]/div') %>% 
      rvest::html_text( ) %>% ifelse(test = length(.) > 0, ., NA)
    
    if (is.na(linkedIn_temp$title[i]) && is.na(linkedIn_temp$company[i]) && is.na(linkedIn_temp$text[i])) {
      linkedIn_temp <- linkedIn_temp[1:(i - 1),]
      break
    }
  }
  linkedIn[[jobtitle]] <- linkedIn_temp
}

job_data <- do.call(rbind, linkedIn)

