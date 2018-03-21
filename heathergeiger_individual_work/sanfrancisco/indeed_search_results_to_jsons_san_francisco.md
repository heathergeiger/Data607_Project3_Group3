
Repeat Indeed.com Resumes to JSON for San Francisco
===================================================

Heather Geiger - March 21, 2018
===============================

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

``` r
library(RJSONIO)
```

Step 1 - Get search queries to paste into a browser.
----------------------------------------------------

We write a function to search for a given job title in a particular city and state.

Edit: Actually looks like the initial URL builder is now giving results for all keyword matches to data scientist, not just those with job title of data scientist. It was not doing this yesterday. I modify the code to give the correct results today.

``` r
indeedUrlBuilder <- function(jobtitle, cityname, statecode){
#startUrl <- "https://www.indeed.com/resumes?search=l"
startUrl <- "https://resumes.indeed.com/search?l="
jobtitle <- gsub(" ","%20",jobtitle)
middle0Url <- "%2C%20"
cityname <- gsub(" ","%20",cityname)
middle1Url <- "&q="
endUrl <- "&searchFields=jt"
#searchUrl <- paste0(startUrl,jobtitle,middle0Url,cityname,middle1Url,statecode,endUrl)
searchUrl <- paste0(startUrl,cityname,middle0Url,statecode,middle1Url,jobtitle,endUrl)
return(searchUrl)
}
```

Run for data scientist in San Francisco, CA.

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

Same method as before. This time we save the curl commands in curl\_commands\_first\_650\_san\_francisco\_resumes.txt.

Step 3 - Run curl commands.
---------------------------

``` r
dir.create("data_scientist_san_francisco_CA_search_results_json_files")

curl_commands <- readLines("curl_commands_first_650_san_francisco_resumes.txt")

for(i in 1:length(curl_commands))
{
command <- curl_commands[i]
print(command)
system(paste0(command," > data_scientist_san_francisco_CA_search_results_json_files/resumes_pg",i,".json"))
}
```

Step 4 - Process the search result JSON files.
----------------------------------------------

For each JSON file, read into R and use this to get the links to resumes.

``` r
resume_ids <- rep(NA,times=length(curl_commands)*50)

for(i in 1:length(curl_commands))
{
json_search_results <- fromJSON(paste0("data_scientist_san_francisco_CA_search_results_json_files/resumes_pg",i,".json"))
j = i - 1
resume_ids[(j*50 + 1):(j*50 + 50)] <- unlist(lapply(json_search_results$results,"[[","accountKey"))
}

head(resume_ids)
```

    ## [1] "54dfd144626446a3" "aab604a5f3f0e329" "b6e9cdfd38050582"
    ## [4] "ccca8b7b348e29db" "a89881990d020759" "d3b7cdaafea60f6f"

``` r
tail(resume_ids)
```

    ## [1] "662076f3ff064556" "4b5fcb118397a8b8" "32a239b3cecb51e5"
    ## [4] "59b715b4f83afd00" "50c326a09cd9ff5c" "16da712d9ff10710"

Step 5 - Download JSON files for resumes.
-----------------------------------------

For a given value in resume\_ids, we can get the corresponding resume link by pasting to "<https://resumes.indeed.com/resume/>".

Then, read in the file. Take the line in the HTML file that is being run through the JSON.parse command.

The JSON file will be bounded by curly brackets, then "\\x22" at the beginning. It will end with "\\x22", then curly brackets.

There will also be other "\\x22" occurences in between, so take the first and last one.

Convert "\\x22" to quote, and it should now be in a format where fromJSON will interpret it as a JSON.

Last time, we output each individual JSON to a file.

However, since each JSON we output was one line, it might be better to have each JSON as a string in a character vector.

Then when we are done, output all the resumes as one big file.

This way we can also read in the JSONs from the vector, rather than having to read in from the files.

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
if(i %% 50 == 0){print(paste0("Resume ",i," has been processed"))}
}

write.table(resume_jsons_for_all_resumes,
file="first_650_san_francisco_resume_json_files_concatenated.txt",
row.names=FALSE,col.names=FALSE,quote=FALSE)

save(resume_jsons_for_all_resumes,file="first_650_san_francisco_resume_json_files_concatenated.Rdata")
```

Step 6 - Process resume JSON files.
-----------------------------------

We create a series of lists to store each resume's job titles, job descriptions, and lists of skills.

We also create a vector to store each resume's summary section.

Last time, we found that some resumes had malformed JSONs, or were missing descriptions for some jobs.

Last time figured this out manually. Here we will try to automate these checks.

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

if(i %% 50 == 0){print(paste0("Resume ",i," has been processed"))}
}
```

    ## [1] "Resume 50 has been processed"
    ## [1] "Resume 100 has been processed"
    ## [1] "Resume 150 has been processed"
    ## [1] "Resume 200 has been processed"
    ## [1] "Resume 250 has been processed"
    ## [1] "Resume 350 has been processed"
    ## [1] "Resume 400 has been processed"
    ## [1] "Resume 450 has been processed"
    ## [1] "Resume 500 has been processed"
    ## [1] "Resume 550 has been processed"
    ## [1] "Resume 600 has been processed"
    ## [1] "Resume 650 has been processed"

``` r
job_titles <- job_titles[valid_json == TRUE & descriptions_for_every_job == TRUE]
job_descriptions <- job_descriptions[valid_json == TRUE & descriptions_for_every_job == TRUE]
lists_of_skills <- lists_of_skills[valid_json == TRUE & descriptions_for_every_job == TRUE]
executive_summaries <- executive_summaries[valid_json == TRUE & descriptions_for_every_job == TRUE]

length(job_titles)
```

    ## [1] 571

``` r
which(valid_json == FALSE)
```

    ##  [1]  22  23  30  42  44  55  58  64  65  75 118 152 161 165 170 171 201
    ## [18] 207 218 242 253 276 279 281 290 291 295 299 300 301 302 315 321 326
    ## [35] 329 335 355 357 360 365 367 372 381 406 412 415 417 419 421 453 461
    ## [52] 463 484 486 491 494 505 508 551 556 559 579 582 596 598 599 610 611
    ## [69] 615 617 623 627 629 632 635 640 641 643 644

``` r
which(descriptions_for_every_job == FALSE)
```

    ## integer(0)

Looks like the number of resumes with malformed JSONs is substantially higher for this time than when we searched for New York, NY.

Nonetheless, 571 resumes is still quite a good sample size.

Let's proceed with making the data frames.

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

Save our work as we go along as good practice.

``` r
save.image("resumes_processed_san_francisco.Rdata")
```

Display first and last rows of the data frames to get a sense of what they look like.

``` r
head(job_titles_and_descriptions_across_resumes)
```

    ##   Resume.num      Job.title
    ## 1          1 Data Scientist
    ## 2          1 Data Scientist
    ## 3          2 Data Scientist
    ## 4          3 Data Scientist
    ## 5          4 Data Scientist
    ## 6          5 DATA SCIENTIST
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Job.description
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Developed a deep similarity network using TensorFlow based on VGG and ResNet to solve in-class\\nsimilarity problem from two groups of real world images, tuned the model to reach an f1 score close to 0.85 by using image augmentation and up-sampling.\\n• Built parallel crawlers with proxy network to efficiently crawl 98% of all public listings (about 100\\nmillion pages) from several short-term rental platforms and uploaded data into AWS RDS\\n• Built a a price calculator to recommend the best rental price for Pillow's customers\\n• Use LDA to automatic generate a large set of labeled images from their descriptions\\n• Perform sentiment analysis for review data and rank the reviews
    ## 2 • Built automatic decision making system for retail stores(Verizon) to make management decisions with location based sensor data, increased the sales of accessories by 30%\\n• Machine learning: Developed Location based Model to predict user behavior over 500,000 minutes of raw\\nsensor data and 400 days of sales data with ENN, achieving AUC 0.72\\n• Feature engineering: Performed detailed feature engineering for data sources from customer's position,\\npath and interactions with sales representatives to address significant factors for transitions.\\nCourses & Projects\\nSelf-Driving Car Engineer Nano Degree - Udacity\\nLearned and deployed code to control a real self-driving car in a test track\\nSkills: Tensorflow, Keras, Yolo, ROS, UKF, Particle Filter, PID, behavior planning, FCNN\\nData Analyst Nano Degree - Udacity\\nData wrangle Open street Maps Data use MongoDB, visualization with D3, A\\u002FB test\\nSkills: Python, R, MongoDB, Machine Learning, A\\u002FB Test, HTML, CSS, D3\\n\\n• Database Management                Created online post forum 'Quill And Inkpot' with MySQL\\n• Operating System                   Implemented Priority Scheduling in Nachos using Java\\n• Distributed System                 Implemented a secure distributed ATM systems with Java\\n• Computer Vision                    Compared image compression algorithm using structural similarity\\n\\nHonors and Awards\\nOct. 2009          Champion, RoboGame Competition, USTC (rank 1)\\nSep. 2007          First Prize, Chinese Physics Olympiad (provincial level), China (top 0.1%))
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             includes   applying   several   ML\\u002FStatistical   algorithms   to   real-world   problems:\\nNeural   Network, Random   Forest, Gradient   Boosted   Trees, Clustering, Linear   and   logistic\\nregression, Ensemble   methods   such   as   Bagging   and   Boosting, Naïve   Bayes   and\\nDimensionality   reduction.\\n\\nwith   Python(Numpy, Pandas, Scikit-learn, matplotlib)\\n● Strong   experience   building   end-to-end   machine   learning   platform   using   java   and   big   data\\ntechnologies(HDFS, Spark, Spark-ml, PySpark, Java, Zeppelin, Scala).\\n● Hands-on   experience   working   with   distributed   clusters   on   AWS.\\n● Good   experience   on   variety   of   databases(Oracle, MS-SQL, Postgres-SQL)
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   - Applied deep learning, computer vision \\u002F image processing on image datasets.\\n- Performed data analysis and extracted insights from data.
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Took full-ownership of model development using predictive analytics and machine learning in developing a\\ndata-driven approach to sales forecasting, and churn modeling.\\nCollaborated cross-group with sales team, communicated via Powerpoint Presentations, and weekly Webex to coordinate deliverables in a timely manner, and created and demonstrated a business analytics dashboard.

``` r
tail(job_titles_and_descriptions_across_resumes)
```

    ##      Resume.num                                              Job.title
    ## 2833        650                         Lecturer (Assistant Professor)
    ## 2834        650            Project Scientist\\u002FPostdoctoral Fellow
    ## 2835        650                                    Postdoctoral fellow
    ## 2836        650                                    Postdoctoral fellow
    ## 2837        650                                       Graduate Student
    ## 2838        650 Head Staff Research Associate\\u002FResearch Assistant
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Job.description
    ## 2833 • Supervisor of laboratory facilities with 2-5 undergraduates per year, and 2 Master's students and 3 Ph.D. students overall, including directing and designing the research\\n• Organized and analyzed 30+ independent datasets (ANOVA, Regression, Frequency Analyses)\\n• Programmed visual stimuli, model fitting, and specialized data analyses software for 5 separate research projects (including 2 Ph.D. students and 2 collaborations)\\n• Generated several Bayesian models of attention with Monte Carlo simulations\\n• Ran experiments using eye-tracking and psychophysics\\n• Prepared a patient database for Huntington's Disease (EURO-HD) for subsequent multiple linear regression
    ## 2834                                                                                                                                                                                                                                                                                                                                                  • Generated several Bayesian models of attention with Monte Carlo simulations\\n• Programmed and ran experiments using visual displays, eye-tracking and psychophysics\\n• Generated code for image processing of several hundred images\\n• Co-supervision of undergraduate students (5-8 per year) and graduate students\\n• ANOVA\\u002FRegression
    ## 2835                                                                                                                                                                                                                                                                                                                                                                                                                                                                 • Generated a Bayesian model of attention with Monte Carlo simulations\\n• Generated code for image processing of medical images (mammograms)\\n• Programmed and ran experiments using visual displays, eye-tracking and psychophysics
    ## 2836                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   • Developed code for running experiments, data processing\\n• Ran experiments using visual displays, eye-tracking, and psychophysics
    ## 2837                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          • Developed code for running experiments, data processing\\n• Developed code to calculate color coordinates of surface colors
    ## 2838                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  • Manager of laboratory facilities, including programming and statistical analyses\\n• Developed code for reading in and cleaning lung volume and EKG data, including graphical input

``` r
head(skills_per_resume)
```

    ##   Resume.num                   Skill
    ## 1          2    ANALYSIS OF VARIANCE
    ## 2          2                   Anova
    ## 3          2 APACHE HADOOP MAPREDUCE
    ## 4          2       Association Rules
    ## 5          2                Bayesian
    ## 6          4                  Python

``` r
tail(skills_per_resume)
```

    ##      Resume.num
    ## 2548        647
    ## 2549        649
    ## 2550        649
    ## 2551        649
    ## 2552        649
    ## 2553        649
    ##                                                                                                                                                                      Skill
    ## 2548 Programming: R, Python, MATLAB, IDL, SQL; Tools: NumPy, SciPy, scikit-learn, pandas, matplotlib, Flask, IPython, LaTeX, ArcGIS, ENVI, ERDAS; OS: Windows, Linux, UNIX
    ## 2549                                                                                                                                                        MICROSOFT WORD
    ## 2550                                                                                                                                                               OUTLOOK
    ## 2551                                                                                                                                                            POWERPOINT
    ## 2552                                                                                                                                                                  WORD
    ## 2553                                                                                                                                                                 EXCEL
