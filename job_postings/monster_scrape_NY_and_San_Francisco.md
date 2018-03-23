
Set URLs.
---------

Set URLs based on the URLs for an actual search result in my browser. Did it this way because this way can search for job title of "data scientist" (not just keyword search). Can also search for a reasonable radius around the city.

``` r
new_york_url <- "https://www.monster.com/jobs/search/New-York+New-York-City+Data-Scientist_125?where=New-York__2c-NY&rad=20-miles"

san_francisco_url <- "https://www.monster.com/jobs/search/California+San-Francisco+Data-Scientist_125?where=San-Francisco__2c-CA&rad=20-miles"
```

Load libraries.
---------------

``` r
library(stringr)    #For string operations
library(rvest)      #For screen scrapper
```

    ## Loading required package: xml2

    ## Warning: package 'xml2' was built under R version 3.4.3

``` r
library(tokenizers) #
library(tidyverse)  #For Tidyverse
```

    ## ── Attaching packages ─────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ readr   1.1.1
    ## ✔ tibble  1.4.2     ✔ purrr   0.2.4
    ## ✔ tidyr   0.7.2     ✔ dplyr   0.7.4
    ## ✔ ggplot2 2.2.1     ✔ forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.4.3

    ## Warning: package 'forcats' was built under R version 3.4.3

    ## ── Conflicts ────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter()         masks stats::filter()
    ## ✖ readr::guess_encoding() masks rvest::guess_encoding()
    ## ✖ dplyr::lag()            masks stats::lag()
    ## ✖ purrr::pluck()          masks rvest::pluck()

``` r
library(RCurl)      #For File Operations
```

    ## Warning: package 'RCurl' was built under R version 3.4.3

    ## Loading required package: bitops

    ## 
    ## Attaching package: 'RCurl'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete

``` r
library(dplyr)      #For Manipulating the data frames
library(DT)         #For Data table package
```

    ## Warning: package 'DT' was built under R version 3.4.3

``` r
library(curl)
```

    ## Warning: package 'curl' was built under R version 3.4.3

    ## 
    ## Attaching package: 'curl'

    ## The following object is masked from 'package:readr':
    ## 
    ##     parse_date

Set city to New York or San Francisco, then make output directory and pg. 1 URL object.
---------------------------------------------------------------------------------------

``` r
city2search <- "NewYork"
state2search <- "NY"
data_store_path <- paste0("job_postings_",city2search,"_",state2search)

#dir.create(data_store_path)

searchPage_url <- new_york_url
```

Run this part only once to avoid getting banned.

If you run subsequent times, load in from Rdata file.

Base URL gives first 25 results, then run pasting "&page=2", "&page=3", etc. to get all results.

Let's take the first 500 results per city, so the first 20 pages.

I checked and both New York and San Francisco have over 500 jobs in the search results.

``` r
searchPage <- read_html(searchPage_url)

searchAllJobUrls <- unlist(str_extract_all(searchPage,'(job-openings\\.monster\\.com\\/)\\w.[^\\"]+'))
searchAllJobUrls <- paste("https://",searchAllJobUrls,sep = "")

searchAllJobUrls <- searchAllJobUrls[1:25]

for(page in 2:20)
{
searchPage <- read_html(paste0(searchPage_url,"&page=",page))
searchAllJobUrls_this_page <- unlist(str_extract_all(searchPage,'(job-openings\\.monster\\.com\\/)\\w.[^\\"]+'))
searchAllJobUrls_this_page <- paste("https://",searchAllJobUrls_this_page,sep = "")
searchAllJobUrls <- c(searchAllJobUrls,searchAllJobUrls_this_page[1:25])
}

save(searchAllJobUrls,file=paste0(data_store_path,"/searchAllJobUrls.Rdata"))
length(unique(tolower(searchAllJobUrls)))
```

    ## [1] 457

If rerunning this script after already scraping the search results, set above to eval=FALSE and the below to eval=TRUE.

``` r
load(paste0(data_store_path,"/searchAllJobUrls.Rdata"))
```

To make sure everything looks correct, show URLs 1, 26, and 51 so we can compare to the links we get by looking in a browser at search pages 1, 2, and 3.

``` r
searchAllJobUrls[c(1,26,51)]
```

    ## [1] "https://job-openings.monster.com/data-scientist-new-york-ny-us-analytic-recruiting-inc/11/194005273"                                
    ## [2] "https://job-openings.monster.com/ovp-of-customer-analytics-bi-manhattan-ny-us-gs-global-services-inc/11/193949751"                  
    ## [3] "https://job-openings.monster.com/data-scientist-computer-vision-ml-python-c-opencv-pcl-new-york-city-ny-us-cybercoders/11/194023269"

So, these match what we see by looking in browser results.

However, initially we found somewhat concerningly that the number of unique URLs is less than 500.

Looking manually through a few pages, it appears sometimes the same job will be listed under two different headlines (eg a "Data Scientist" job at Open Systems Technologies was listed as "Data Scientist" on pg2 and "Machine Learning Data Scientist" on pg3).

I think it should be fine to just run unique on searchAllJobUrls, and then proceed as normal.

``` r
searchAllJobUrls <- unique(searchAllJobUrls)
length(searchAllJobUrls)
```

    ## [1] 457

Now, read from each URL in searchAllJobUrls and save text in job description.

``` r
job_sum_text <- vector(mode = "character", length = length(searchAllJobUrls))

for(i in 1:length(searchAllJobUrls))
{
h <- read_html(searchAllJobUrls[i])
forecasthtml <- html_nodes(h,"#JobDescription")
#Adding a check to ensure that there actually is a node with "JobDescription", as one of the URLs did not have this node and it broke the for loop.
if(length(forecasthtml) == 1)
{
        job_sum_text[i] <- html_text(forecasthtml)
}
if(length(forecasthtml) != 1)
{
        job_sum_text[i] <- "" #Add an empty string to job_sum_text for now for these. May want to delete these later on.
}
}

save(job_sum_text,file=paste0(data_store_path,"/job_description_text.Rdata"))
```

If rerunning this script after already scraping the job pages, set above to eval=FALSE and below to eval=TRUE.

``` r
load(paste0(data_store_path,"/job_description_text.Rdata"))
```

``` r
length(job_sum_text)
```

    ## [1] 457

``` r
class(job_sum_text)
```

    ## [1] "character"

``` r
job_sum_text[1:3]
```

    ## [1] "\r\n\r\nMajor retail store company seeks Data Scientist to develop customer analytics and contact optimization tools.   Responsibilities:·         Develop methods for demand forecasting, business outcome simulation, determining customer preferences, and marketing investment optimization·         Work with cross-functional teams to develop statistical best practices regarding experimental design and data analysis·         Create presentations and visualizations  Requirements·         M.S. or Ph.D. in statistics, computer science, engineering, or related field. ·         Experience with R, Python, SPARK, relational databases and SQL·         Experience working with big data and tools such as Hadoop, Map/Reduce, Hive, etc.·         Familiarity with visualization tools such as Tableau·         Ability to communicate complex analyses clearly Keywords: Retail, data science, customer analytics, database marketing, Hadoop, big data, retail Refer to Job #22779 – and send MS Word attached resume to Richard Exelbert, richard@analyticrecruiting.comHelp              "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    ## [2] "\r\n\r\nData ScientistFull-Time/Permanent ($120,000 - $140,000 or more based on experience)NYC (Penn Plaza)Are you comfortable with Python and a lover of all things data? Do you thrive in smaller startup environments where you can wear a few different hats and directly interact with company decision-makers? If so, this opportunity is for you! Here are the details:Reporting to the VP of Product, the Data Scientist will build machine learning tools for external clients as well as internal teams.Day to day responsibilities may include:* Analyzing requirements and formulating an appropriate technical solution that meets requirements* Working with large data sets in the 100\"s of GBs and processing the data using Spark* Exercising data mining and predictive analytics techniques* Python machine learning development (Pandas, SKLearn, NumPy, Matplotlib)* Developing predictive regression modelsWhat we are looking for:BS or MS degree in Computer Science, Math, Statistics or other technical fieldAt least 2 years of data science experienceA solid programming foundation (Python preferred, but will consider languages like Scala and Ruby as well)Strong predictive analytics experienceSpark experience would be a BIG plus (SparkSQL, Caching, Checkpointing, Dataframes, RDDs, etc.)Experience working with large data setsWho you will work for:This data analytics company helps their customers collect, structure, and understand their data.What we can offer you:Schedule flexibility -- don\"t worry about that midday appointment or that activity with your child! Want to come in a bit early and be home before dinner? No problem!Medical and dental coverageStock optionsCommuter benefit            "                                                                                                                                                                                                                         
    ## [3] "\r\n\r\n I am currently working with a totally unique investment management Start-Up based in Mid-Town Manhattan that uses Machine Learning not only to dictate value but also to warrant authenticity of various high-end assets within a Multi-Billion Dollar market.The revolutionary key players within the investment management industry is looking for a Lead Data Scientist/Chief Data Scientist or Data Science Manager to join them and work closely but independently of the CTO and CEO as the business leader of all things Big Data. You will be expected to build, grow, technically mentor a fresh team whilst being able to roll up your sleeves and work on some machine learning algorithms yourself. Due to the unique nature of the work you will be working with both structured data (numerical, categorical) along with some unstructured data (images, videos).The skill set required includes:• Programming Language – Python – (pandas, numpy, scikit-learn, TensorFlow)• Leadership experience – Leading Data Science teams, Recruitment, Technical Mentorship, Managing• A Degree within Computer Science, Data Science or similar subject• Commercial experience as a Data Scientist (Not just academic/research)• You are a US Citizen or Green cardholder• Ability to work within a Start Up Environment – Fast Paced, Many different job roles, no job to big or smallThe Following experience would be advantageous:• Deep Learning techniques – Neural Network,• Computer Vision/Image Processing – Open CV• Natural Language Processing• PhD within a relevant subjectThis client is able to get the interview process started from Monday 12th March and could get the right fit started as soon as possible.If that Sounds like something you would be interested in hearing more about then please do feel free to get in touch with Lloyd using the details below.Principle Data Scientist, Lead Data Scientist, Chief Data Scientist            "

``` r
length(unique(job_sum_text))
```

    ## [1] 444

When running a similar script for Columbus, OH, we found at least one job without a valid JobDescription node, so we put an empty string in text field.

We also found an instance of the same job clearly listed under two different URLs.

Let's check if this happens here, and remove such instances if so.

Then, save Rdata again.

``` r
searchAllJobUrls <- searchAllJobUrls[job_sum_text != "" & duplicated(job_sum_text) == FALSE]
job_sum_text <- job_sum_text[job_sum_text != "" & duplicated(job_sum_text) == FALSE]

length(searchAllJobUrls)
```

    ## [1] 444

``` r
length(job_sum_text)
```

    ## [1] 444

``` r
save(list=c("searchAllJobUrls","job_sum_text"),
file=paste0(data_store_path,"/searchAllJobUrls_and_job_sum_text_objects_after_remove_empty_and_duplicate_postings.Rdata"))
```
