#Load libraries.

library(stringr)    #For string operations
library(rvest)      #For screen scrapper
library(tokenizers) #
library(tidyverse)  #For Tidyverse
library(RCurl)      #For File Operations
library(dplyr)      #For Manipulating the data frames
library(DT)         #For Data table package
library(curl)

#The end here means we want to look for the query as a job title, not just a keyword.

indeedUrlBuilder <- function(jobtitle, cityname, statecode){
startUrl <- "https://www.indeed.com/resumes?q="
jobtitle <- gsub(" ","+",jobtitle)
middle0Url <- "&l="
cityname <- gsub(" ","+",cityname)
middle1Url <- "%2C+"
endUrl <- "&searchFields=jt"
searchUrl <- paste0(startUrl,jobtitle,middle0Url,cityname,middle1Url,statecode,endUrl)
return(searchUrl)
}

#Next, parse the HTML file to get resume links.

get_resume_links <- function(url){
searchPage <- read_html(curl(url,handle = curl::new_handle("useragent" = "Mozilla/5.0")))
links <- html_attr(html_nodes(searchPage, "a"), "href")
links <- grep('^/r/',links,value=TRUE)
resume_ids <- unlist(lapply(strsplit(links,"\\?"),"[[",1))
resume_ids <- unlist(lapply(strsplit(resume_ids,"\\/r\\/"),"[[",2))
resume_links <- paste0("https://resumes.indeed.com/resume/",resume_ids)
return(resume_links)
}

#Now, let's run through the first 1,000 search results for "data scientist" in "New York, NY" and return all the resume links.
#Immediately save to an Rdata file so we don't have to run this again (trying to avoid getting blocked).
#May also want to just take first 100 results if we don't want to query too many times.

#first_1000_NY_data_science_resume_links <- rep(NA,times=1000)
#first_1000_NY_data_science_resume_links[1:50] <- get_resume_links(indeedUrlBuilder("data scientist","New York","NY"))

#for(i in seq(from=50,to=950,by=50))
#{
#first_1000_NY_data_science_resume_links[(i + 1):(i + 50)] <- get_resume_links(paste0(indeedUrlBuilder("data scientist","New York","NY"),"&start=",i))
#}

#save(first_1000_NY_data_science_resume_links,file="first_1000_NY_data_science_resume_links.Rdata")

first_100_NY_data_science_resume_links <- rep(NA,times=100)
first_100_NY_data_science_resume_links[1:50] <- get_resume_links(indeedUrlBuilder("data scientist","New York","NY"))
first_100_NY_data_science_resume_links[51:99] <- get_resume_links(paste0(indeedUrlBuilder("data scientist","New York","NY"),"&start=50")) #Getting a weird result where this returns 49 links instead of 50. Not sure why. 

save(first_100_NY_data_science_resume_links,file="first_100_NY_data_science_resume_links.Rdata")

#Next for each link, extract skills.
#Going to just use Raj's list of skills for now, but can do this for whatever dictionary we'd like.
#For each skill in order, return a 1 if the skill is found in the resume, 0 if not.

skills <- c("data engineering", "hadoop", "python", "sql", "hive", "spark", "kafka", "database", "big data", "statistic", "model", "math", "physics", "engineering", "finance", "quantitative", "data", "matlab", " r ", "probability", "stochastic", "calculus", "design", "phd", "masters", "bachelors", "development", "scala", "oracle", "aws", "amazon", "google", "engine", "predict", "linear", "regression", "logistical", "seaborn", "ggplot", "shiny", "tensorflow", "nlp", "neuro", "language", "sas", "spss", "scipy", "numpy", "scikit", "dataset", "machine learning", "deep learning", "svm", "analytics", "clustering", "decision tree", "visualization", "math", "algorithms", "bayesian")

extract_skills <- function(link){
	resume_link_read_in <- read_html(curl(link,handle = curl::new_handle("useragent" = "Mozilla/5.0")))
	extracted_skills <- unlist(str_extract_all(tolower(resume_link_read_in),skills))
	return(ifelse(skills %in% extracted_skills,1,0))
}


