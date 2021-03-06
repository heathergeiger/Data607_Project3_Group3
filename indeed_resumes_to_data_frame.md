
Extracting Indeed.com resume text using API call plus web scraping
==================================================================

Heather Geiger and Raj Kumar
----------------------------------------------

Load libraries.
---------------

``` r
library(stringr)    #For string operations
library(rvest)      #For screen scrapper
library(tidyverse)  #For Tidyverse
library(RCurl)      #For File Operations
library(dplyr)      #For Manipulating the data frames
library(DT)         #For Data table package
library(curl)

library(RJSONIO) #For processing JSON format data
```

Step 1 - Get search queries to paste into a browser.
----------------------------------------------------

We write a function to search for a given job title in a particular city and state.

``` r
indeedUrlBuilder <- function(jobtitle, cityname, statecode){
startUrl <- "https://resumes.indeed.com/search?l="
jobtitle <- gsub(" ","%20",jobtitle)
middle0Url <- "%2C%20"
cityname <- gsub(" ","%20",cityname)
middle1Url <- "&q="
endUrl <- "&searchFields=jt"
searchUrl <- paste0(startUrl,cityname,middle0Url,statecode,middle1Url,jobtitle,endUrl)
return(searchUrl)
}
```

In this example, we will run for data scientist in San Francisco, CA.

We also ran an identical set of functions for New York, NY, except taking the first 1,000 resumes instead for that city.

We will combine results for the two cities in the analysis section.

``` r
url_to_search <- indeedUrlBuilder("data scientist","San Francisco","CA")
url_to_search
```

    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt"

Pasting this URL into a browser, we find that there are 679 resumes for data scientist in San Francisco, CA.

Let's take the first 650.

``` r
for(i in seq(from=50,to=600,by=50))
{
print(paste0(url_to_search,"&start=",i))
}
```

    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=50"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=100"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=150"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=200"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=250"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=300"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=350"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=400"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=450"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=500"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=550"
    ## [1] "https://resumes.indeed.com/search?l=San%20Francisco%2C%20CA&q=data%20scientist&searchFields=jt&start=600"

Step 2 - Paste search queries into a browser and use point-and-click to get commands to download JSON files using the API.
--------------------------------------------------------------------------------------------------------------------------

For each of these URLs, we use a browser plus point-and-click to get the curl command to download the JSON file for the page using the API.

The API is not easily accessible through 100% command line methods, as presumably Indeed.com wants you to go to their website rather than scraping it.

With 50 results per page, we can get quite a few resumes just clicking on a few pages, so this is not a huge problem.

As we obtain each curl command, paste it into a file called curl\_commands\_first\_650\_san\_francisco\_resumes.txt.

Here are the steps to get each curl command using point-and-click.

1.  Open developer tools. In Google Chrome, you get this by going to "More Tools", then "Developer Tools", in the top right menu.
2.  Go to tab "Network".
3.  Reload page. All network calls the page is making will now show.
4.  Go to the one with type "fetch" whose name starts with "search".
5.  Right click, click copy, then click "Copy as cURL".

Step 3 - Run curl commands.
---------------------------

We run each curl command, outputting the result to a JSON file.

``` r
dir.create("data_scientist_san_francisco_CA_search_results_json_files")

curl_commands <- readLines("curl_commands_first_650_san_francisco_resumes.txt")

for(i in 1:length(curl_commands))
{
command <- curl_commands[i]
system(paste0(command," > data_scientist_san_francisco_CA_search_results_json_files/resumes_pg",i,".json"))
}
```

Step 4 - Process the search result JSON files.
----------------------------------------------

For each JSON file, read into R and use this to get the links to resumes.

The information we need to link to resumes will be stored in "accountKey" field.

``` r
resume_ids <- rep(NA,times=length(curl_commands)*50)

for(i in 1:length(curl_commands))
{
json_search_results <- fromJSON(paste0("data_scientist_san_francisco_CA_search_results_json_files/resumes_pg",i,".json"))
j = i - 1
resume_ids[(j*50 + 1):(j*50 + 50)] <- unlist(lapply(json_search_results$results,"[[","accountKey"))
}
```

Step 5 - Download JSON files for resumes.
-----------------------------------------

For a given value in resume\_ids, we can get the corresponding resume link by pasting to "<https://resumes.indeed.com/resume/>".

Then, read in the file. Take the line in the HTML file that is being run through the JSON.parse command.

The JSON file will be bounded by curly brackets, then "\\x22" at the beginning. It will end with "\\x22", then curly brackets.

There will also be other "\\x22" occurences in between, so take the first and last one.

Convert "\\x22" to quote, and it should now be in a format where fromJSON will interpret it as a JSON.

Each resume's JSON content will be a single string. We can add these to a character vector.

``` r
resume_jsons_for_all_resumes <- rep(NA,times=length(resume_ids))

for(i in 1:length(resume_ids))
{
resume_link <- paste0("https://resumes.indeed.com/resume/",resume_ids[i])
resume_read_as_text <- readLines(resume_link)
resume_read_as_text <- grep('JSON.parse',resume_read_as_text,value=TRUE)
x22_locations <- str_locate_all(resume_read_as_text,pattern='\\\\x22')[[1]]
resume_json <- substr(resume_read_as_text,x22_locations[1,1] - 1,x22_locations[nrow(x22_locations),2] + 1)
resume_json <- str_replace_all(resume_json,pattern='\\\\x22',replace='"')
resume_jsons_for_all_resumes[i] <- resume_json
}
```

Immediately save resume\_jsons\_for\_all\_resumes to an Rdata file.

Similar to what we did for the curl commands, we want to immediately save the result whenever we access the website so we can minimize the resources we use.

``` r
save(resume_jsons_for_all_resumes,file="first_650_san_francisco_resume_json_files_concatenated.Rdata")
```

Step 6 - Process resume JSON files.
-----------------------------------

We create a series of lists to store each resume's job titles, job descriptions, and lists of skills.

We also create a vector to store each resume's summary section.

Use isValidJSON for check if malformed. Check number of job titles vs. descriptions to make sure all jobs have descriptions.

If both of these checks pass, add to the lists.

``` r
valid_json <- rep(NA,times=length(resume_ids))
descriptions_for_every_job <- rep(NA,times=length(resume_ids))

job_titles <- vector("list",length=length(resume_ids))
job_descriptions <- job_titles
lists_of_skills <- job_titles
executive_summaries <- rep(NA,times=length(resume_ids))

for(i in 1:length(resume_ids))
{
validity_check <- isValidJSON(resume_jsons_for_all_resumes[i],asText = TRUE)
if(validity_check == FALSE){valid_json[i] <- FALSE;next}

resume_json <- fromJSON(resume_jsons_for_all_resumes[i],asText = TRUE)

job_titles_this_resume <- unlist(lapply(resume_json$resumeModel$workExperience,"[[","title"))
job_descriptions_this_resume <- unlist(lapply(resume_json$resumeModel$workExperience,"[[","description"))

descriptions_for_every_job_this_resume <- length(job_titles_this_resume) == length(job_descriptions_this_resume)
if(descriptions_for_every_job_this_resume == FALSE){descriptions_for_every_job[i] <- FALSE;next}

job_titles[[i]] <- job_titles_this_resume
job_descriptions[[i]] <- job_descriptions_this_resume

lists_of_skills[[i]] <- unlist(lapply(resume_json$resumeModel$skills,"[[","skill"))
executive_summaries[i] <- unlist(resume_json$resumeModel$summary)

valid_json[i] <- TRUE
descriptions_for_every_job[i] <- TRUE
}

job_titles <- job_titles[valid_json == TRUE & descriptions_for_every_job == TRUE]
job_descriptions <- job_descriptions[valid_json == TRUE & descriptions_for_every_job == TRUE]
lists_of_skills <- lists_of_skills[valid_json == TRUE & descriptions_for_every_job == TRUE]
executive_summaries <- executive_summaries[valid_json == TRUE & descriptions_for_every_job == TRUE]
```

Step 7 - Compile processed JSON information into data frames.
-------------------------------------------------------------

Use job\_titles and job\_descriptions list to make one big data frame.

In a given resume, there will often be multiple job title/description combinations. So, we repeat each resume index by the number of job titles there are.

For skills, there may also frequently be multiple skills per resume. So we follow a similar strategy there as well.

``` r
job_titles_and_descriptions_across_resumes <- data.frame(Resume.num = rep(which(valid_json == TRUE & descriptions_for_every_job == TRUE),
                                                        times=unlist(lapply(job_titles,function(x)length(x)))),
                                                        Job.title = unlist(job_titles),
                                                        Job.description = unlist(job_descriptions),
                                                        stringsAsFactors=FALSE)
skills_per_resume <- data.frame(Resume.num = rep(which(valid_json == TRUE & descriptions_for_every_job == TRUE),
                                                times=unlist(lapply(lists_of_skills,function(x)length(x)))),
                                                Skill = unlist(lists_of_skills),
                                                stringsAsFactors=FALSE)
```

Let's clean up by removing some of the objects we no longer need.

``` r
rm(list=setdiff(ls(),c("job_titles_and_descriptions_across_resumes","skills_per_resume","executive_summaries","valid_json","descriptions_for_every_job")))
```

Create one data frame with information from all sub-categories - job title, description, skills, and executive summary.

``` r
city <- "San Francisco"

job_titles_and_descriptions_across_resumes <- gather(job_titles_and_descriptions_across_resumes,Resume.section,Text,-Resume.num)                
job_titles_and_descriptions_across_resumes <- data.frame(City = city,job_titles_and_descriptions_across_resumes,stringsAsFactors=FALSE)
skills_per_resume <- data.frame(City = city,Resume.num = skills_per_resume$Resume.num,
                                Resume.section = "Skills",
                                Text = skills_per_resume$Skill,stringsAsFactors=FALSE)
executive_summaries <- data.frame(City = city,
                                Resume.num = which(valid_json == TRUE & descriptions_for_every_job == TRUE),
                                Resume.section = "Executive.summary",
                                Text = executive_summaries,stringsAsFactors=FALSE)
san_francisco_resumes <- rbind(job_titles_and_descriptions_across_resumes,skills_per_resume,executive_summaries)
san_francisco_resumes <- san_francisco_resumes %>% arrange(Resume.num)
```

Finally, assuming we have done the same for New York and have that information in a data frame called new\_york\_resumes, combine the two data frames.

``` r
resumes_across_cities <- rbind(new_york_resumes,san_francisco_resumes)
rm(new_york_resumes);rm(san_francisco_resumes)

resumes_across_cities <- resumes_across_cities %>% arrange(Resume.num,City)
```

Replace special encoding "\\u002F" with a "/".

``` r
resumes_across_cities$Text <- str_replace_all(resumes_across_cities$Text,pattern='\\\\u002F',replace='/')
```

Now we could stop here, if we wanted to look separately at different resume categories like executive summary vs. job descriptions.

However I think for this purpose, having one big block of text per resume would be best.

So, let's combine all Text fields for each unique combination of City and Resume.num, regardless of Resume.section.

``` r
resumes_across_cities <- resumes_across_cities %>% unite("Unique.resume.ID",c("City","Resume.num"),sep="/",remove=FALSE)

pasted_text_per_resume <- aggregate(Text ~ Unique.resume.ID,resumes_across_cities,function(x)paste0(x,collapse=" "))

city_per_resume <- resumes_across_cities %>% select(c("Unique.resume.ID","City"))
city_per_resume <- city_per_resume[!duplicated(city_per_resume$Unique.resume.ID),]

resumes_across_cities <- merge(city_per_resume,pasted_text_per_resume,by="Unique.resume.ID")
```

Reduce data frame to the minimal we will need for analysis, then save results.

``` r
resumes_across_cities <- resumes_across_cities %>% select(c("City","Text"))
resumes_across_cities$City <- as.vector(resumes_across_cities$City)
resumes_across_cities$Text <- as.vector(resumes_across_cities$Text)

save(resumes_across_cities,file="resumes_across_cities_one_text_field_per_resume.Rdata")
```
