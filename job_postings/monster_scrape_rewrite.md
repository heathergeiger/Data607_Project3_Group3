
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

Building search URLs
--------------------

``` r
skill2search <- "Data Science"
city2search <- "Columbus"
state2search <- "OH"
data_store_path <- paste0("job_postings_",city2search,"_",state2search)

dir.create(data_store_path)
```

    ## Warning in dir.create(data_store_path): 'job_postings_Columbus_OH' already
    ## exists

``` r
monsterUrlBuilder <- function(skillname, cityname, statecode){
    startUrl <- "https://www.monster.com/jobs/search/?q="
    skillname <- gsub(" ","-",skillname)
    middle0Url <- "&where="
    cityname <- gsub(" ","-",cityname)
    middle1Url <- "__2C-"
    middle2Url <- "&intcid=skr_navigation_nhpso_searchHeader"

    searchUrl <- paste(startUrl,skillname,middle0Url,cityname,middle1Url,statecode,middle2Url, sep="")
    searchUrl
}

searchPage_url <- monsterUrlBuilder(skill2search, city2search, state2search)
```

Run this part only once to avoid getting banned.

If you run subsequent times, load in from Rdata file.

Edit: Actually found that the URL created by monsterUrlBuilder only gives first 25 results.

Beyond that, need to add "&page=2", "&page=3", etc.

Here there are 80 results. So take 25 each from first 3 pages, and 5 from the 4th page.

``` r
searchPage <- read_html(searchPage_url)

searchAllJobUrls <- unlist(str_extract_all(searchPage,'(job-openings\\.monster\\.com\\/)\\w.[^\\"]+'))
searchAllJobUrls <- paste("https://",searchAllJobUrls,sep = "")

searchAllJobUrls <- searchAllJobUrls[1:25]

page=2
searchPage <- read_html(paste0(searchPage_url,"&page=",page))
searchAllJobUrls_this_page <- unlist(str_extract_all(searchPage,'(job-openings\\.monster\\.com\\/)\\w.[^\\"]+'))
searchAllJobUrls_this_page <- paste("https://",searchAllJobUrls_this_page,sep = "")
searchAllJobUrls <- c(searchAllJobUrls,searchAllJobUrls_this_page[1:25])

page=3
searchPage <- read_html(paste0(searchPage_url,"&page=",page))
searchAllJobUrls_this_page <- unlist(str_extract_all(searchPage,'(job-openings\\.monster\\.com\\/)\\w.[^\\"]+'))
searchAllJobUrls_this_page <- paste("https://",searchAllJobUrls_this_page,sep = "")
searchAllJobUrls <- c(searchAllJobUrls,searchAllJobUrls_this_page[1:25])

page=4
searchPage <- read_html(paste0(searchPage_url,"&page=",page))
searchAllJobUrls_this_page <- unlist(str_extract_all(searchPage,'(job-openings\\.monster\\.com\\/)\\w.[^\\"]+'))
searchAllJobUrls_this_page <- paste("https://",searchAllJobUrls_this_page,sep = "")
searchAllJobUrls <- c(searchAllJobUrls,searchAllJobUrls_this_page[1:5])

save(searchAllJobUrls,file=paste0(data_store_path,"/searchAllJobUrls.Rdata"))
length(unique(tolower(searchAllJobUrls)))
```

Now, read from each URL in searchAllJobUrls and save text in job description.

If running for a city with more jobs, may only want to take first say 100 or 1,000, again to avoid getting banned.

Also again, please run this part only once and load in from Rdata file subsequent times to avoid getting banned.

``` r
job_sum_text <- vector(mode = "character", length = length(searchAllJobUrls))

for(i in 1:length(searchAllJobUrls))
{
h <- read_html(searchAllJobUrls[i])
#Write raw HTML out to a file just in case as well.
write_html(h,file=paste0(data_store_path,"/job_index_",i,".html"))
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

``` r
length(job_sum_text)
```

    ## [1] 80

``` r
class(job_sum_text)
```

    ## [1] "character"

``` r
job_sum_text[1:3]
```

    ## [1] "\r\n\r\nThis position is the Architecture leader for a business unit or an operational area within the company and is a visible leader both internal and external to Magellan. This role will be included in evaluating potential mergers and acquisitions and help drive Magellan to become a global leader in supporting technology to deliver better outcomes to our members. Must be anexpert in architecture methods and technologies across multiple platforms. Shown ability to shape and drive strategic vision.  Excel at inspiring, motivating and creating highly productive teams. Expert at attracting, hiring and developing a strong bench of talent. Possesses a unique combination of highly evolved technical skills as well as the ability to envision a strategy and be creative in order to realize ambitious goals on behalf of the business. Inspire the organization to create world-class solutions.  Must be passionate about supporting the business and company vision. Ability to communicate a clear point of view from abusiness perspective in strategic discussions with executive leadership.Essential Functions:- Influence changes and system enhancements to business processes, policies, and infrastructure to deliver the most effective IT services.- Manage the selection, evaluation, contracting and integration of externally available hardware, software, and processes to deliver business solutions.- Direct and manage, through analysis, the planning, design, development, testing, installation and maintenance of support systems for both internal/external clients.- Select and build strong teams through formal training, diverse assignments, coaching, mentoring, and other team development techniques, along with regular individual and group meetings.- Develop tactical and strategic plans to satisfy technical business needs and new business proposals.- Manage and develop project cost estimates, benefits, justification and assessment of potential project risks.- Manage projects, staff, customer expectations and business priorities to achieve customer and business unit satisfaction.- Manage, plan and track all budgets and expenses.- Oversee vendor relationships and projects.- Direct and manage business process re-engineering associated with existing, developing and implementation of systems.- Develops and maintains strong, cooperative relationships with other IT services groups. Shares and establishes bestpractices experiences and opportunities throughout the organization.- Apply Agile development practices in delivery of transformational programs as required.Hands-on experience in Big Data Components/Frameworks such as Hadoop, Spark, Storm, HBase, HDFS, Pig, Hive, Scala, Kafka, PyScripts, Unix Shell scriptsExperience with cloud service providers - AWS, Azure, GCPExperience in architecture and implementation of large and highly complex big data projectsExperience of Hadoop and related technologies (Cloudera, Hortonworks, etc.)Experience with data integration and streaming tools used for Hadoop (Spark, Kafka, etc.)Experience in Metadata management, data lineage, data governance, especially as related to Big DataExperience with cloud platforms (AWS/Azure), including readiness, provisioning, security, and governanceExperience or understanding of Data Science and related technologies (Python, R, SAS, etc.)Experience or understanding of Artificial Intelligence (AI), Machine Learning (ML), and Applied StatisticsHistory of working successfully with cross-functional engineering teamsAdditional Details:Provide thought leadership for Big Data, Data Lake, and Data GovernanceDeep understanding of infrastructure as code and cloud service providers\n\nGeneral Job Information\n \nTitleSr Director, Big Data\nGrade32\nJob FamilyIT Architecture\nCountryUnited States of America\nFLSA StatusUnited States of America (Exempt)\nRecruiting Start Date2/19/2018\nDate Requisition Created2/19/2018\n\nMinimum Qualifications\n\nEducationBachelors: Computer and Information Science (Required), Masters\nLicense and Certifications - RequiredTOGAF - The Open Group Architecture Framework - Enterprise\nLicense and Certifications - Preferred\n\n\nOther Job Requirements\n\nResponsibilitiesExperience with Managed Healthcare with an emphasis on Medicaid or Medicare., Knowledgeable in the use and implementation of SDLC methodologies.   Demonstrated ability to work and lead staff in a team-oriented or matrixed environment.  Extensive experience in working as a liaison between technical and non-technical groups.\nCompetencies\n\nLanguage(s)\n\n\n\nMagellan Health Services is proud to be an Equal Opportunity Employer and a Tobacco-free workplace. EOE/M/F/Vet/Disabled            "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    ## [2] "\r\n\r\n\nCardiology Sales Specialist - Columbus, OH VSA84054-1805562\n\n\nJoin us on our exciting journey!\nIQVIA™ is The Human Data Science Company™, focused on using data and science to help healthcare clients find better solutions for their patients. Formed through the merger of IMS Health and Quintiles, IQVIA offers a broad range of solutions that harness advances in healthcare information, technology, analytics and human ingenuity to drive healthcare forward.\n\n\n\nDescription As the only global provider of commercial solutions, IQVIA understands what it takes to deliver nationally and internationally. Our teams help biopharma get their medicines to the people who need them. We help customers gain insight and access to their markets and ultimately demonstrate their product’s value to payers, physicians and patients. A significant part of our business is acting as the biopharma’s sales force to physicians or providing nurses to educate patients or prescribers. With the right experience, you can help deliver medical breakthroughs in the real world.\n \nWe are excited to announce that at this time we are looking for a Cardiology Sales Specialist to join our team of over 10,000 global field representatives in several regions making over 20 million product details annually for our pharmaceutical and biotech clients.   In this role, you will be supporting Janssen Pharmaceuticals, Inc., a member of the Pharmaceutical Companies of Johnson& Johnson, fully dedicated to serving the needs of health care providers and their patients.\n \nCardiology Sales Specialist\n \nThe primary objective of the sale representative is to meet established sales goals by delivering real value to our customers through differentiated products and services. The sales representative will be supported in this initiative with tools and promotional resources designed to have local impact. The successful representative will demonstrate the ability to target and manage their territory strategically while operating within an assigned budget. They will also need to be a highly engaged, positive team player and show a high degree of customer focus. \n \nIQVIA offers a friendly, progressive work atmosphere and a comprehensive benefits package including medical, dental, life insurance and vision coverage, tuition assistance, bonus plan and 401(k).\n \nWe look forward to the prospect of working with you!        \n\nEEO Minorities/Females/Protected Veterans/Disabled\n\n\n \n\n \n\n\n\nQualifications Qualifications/Experience\n4 year Bachelor’s degree from an accredited University required\n3 or more years of direct selling experience to healthcare professionals in the pharmaceutical, biotech, device or healthcare industry, or large account management experience is required\nMinimum 2-3 years Specialty Pharmaceutical Sales experience is required\n1 or more years’ experience selling cardiology products to Cardiologists is preferred\nStrong scientific/clinical expertise required\nProven track record of success in a high science, competitive selling environment\nDocumented sales results depicting significant success, including examples of national awards for performance, company awards, or participation in management development program are preferred\nStrong verbal, interpersonal and listening skills\nAbility to travel overnight as required\nResidence within the current geography is required\n \nCompetencies\nAbility to handle ambiguity while driving results\nDemonstrated performance and results\nDemonstrated customer and marketplace expertise\nAbility to effectively discuss risk/benefits with targeted healthcare providers\nAbility to effectively build successful territory business plans to ensure successful product launches.\nProfessional credibility; demonstrated success in persuasion, influence and negotiation skills\nBusiness Insight; demonstrated ability to learn and apply highly technical/scientific knowledge\nProficient with the selling process\nInitiative & execution-oriented\nCollaboration within the organization and across multiple teams\nCustomer Focused with ability to identify, develop and leverage relationships\nTeam player\n \n \n \n \n \n \n \n \n \n \n \n \n \n\n\n Did You Know? We know that meaningful results require not only the right approach but also the right people. Regardless of your role, we invite you to reimagine healthcare with us. You will have the opportunity to play an important part in helping our clients drive healthcare forward and ultimately improve human health outcomes.\nWhatever your career goals, we are here to ensure you get there!\n\nWe invite you to join IQVIA™. \n\n\nPrimary Location: USA-Ohio-Columbus\n\n\n\n\nOrganization: USA03 - US Commercialization\n\n\nJob: Pharmaceutical/Medical Sales\n\n\n\n\n\n\n            "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
    ## [3] "\r\n\r\nJob Overview/SummaryAs an FBI Special Agent, you will bring your skills, talents and integrity to a dedicated team working at the highest level to get ahead of threats, protect the American people and uphold the Constitution of the United States.A “routine” day for a Special Agent is anything but routine. It may entail interviewing a subject for an ongoing investigation in the morning, testifying in court before lunch, planning an operation in the afternoon and speaking at a community event in the evening. Our agents seek out cyber thieves, infiltrate organized crime rings and oversee terrorism investigations, often training local, state, and foreign counterparts on the latest technologies in intelligence-gathering and data analysis. All applicants must pass the physical fitness requirements, including medical and hearing standards and all phases of the FBI Physical Fitness Test (PFT). Selectees must be physically fit to complete training at Quantico and maintain a high level of physical fitness throughout their careers. Special Agents are responsible for enforcing more than 300 federal statutes, as well as conducting sensitive national security investigations. You’ll work to develop relationships within and across communities in support of our mission: To protect the American people and uphold the Constitution of the United States. In an organization made up of careers like no other, being a Special Agent can be a lifelong career of uncommon days and amazing experiences — as part of a mission-focused family that is One FBI.DUTIESYou are a problem solver with great analytical skills and leadership experience; you know how to get things done both in a demanding team environment and independently. Your ability to adapt to changing situations and clearly communicate across management, peer groups, external communities and stakeholders contributes to your ability to keep our nation safe. You’re naturally curious and always driven to find a solution. You notice minute details, yet remain strategic. You objectively evaluate information and make sound judgments. Honesty and integrity are more than just words to you — they are the foundation of who you are. You take pride in making a difference in the communities you serve, and bringing your background and unique skills to initiatives that impact our most important stakeholders: the American people.In this role, you’ll be expected to use both established and innovative approaches to tackle some of the toughest challenges of our age. This opportunity should not be taken lightly. The FBI Special Agent position requires significant commitment and dedication from you and your family, but you’ll go home every night knowing that you are making a difference. Being an FBI Special Agent is an experience you\"ll never forget.Though individuals of all backgrounds are highly encouraged to apply, we’re specifically seeking the following skills:• SCIENCE, TECHNOLOGY, ENGINEERING AND MATH (STEM): Accounting, forensic science, computer technology, cyber security, electronics, economics, finance, biometrics, encryption, data science, information technology, mathematics, chemistry, computer forensics, biology, and more. When people say STEM, you perk up because you know they are talking about you.• FOREIGN LANGUAGES: You may have been raised speaking a language other than English, taught it for a few years or even participated in intensive learning before going overseas to use it. You know that your language skills can make a difference: Arabic, Yoruba, Mandarin, Tagalog, Russian, Spanish, Farsi, Punjabi, Pashtu, Urdu and more.• LAW: You are an experienced lawyer and seek out new and creative ways to push for change that matters. You use your background in corporate, intellectual property (IP), family, immigration, environmental, criminal, or commercial law to do good work, work that asks you to be part of something larger than yourself.• LAW ENFORCEMENT AND MILITARY: You work in law enforcement or in the military and have been making your way up the ranks for the past few years. You raise challenging questions that demand practical answers. Detective, SWAT, helicopter/jet/rescue pilot, K-9 handler, bomb tech, operational paramedic or otherwise. When you think about career growth, you know that the FBI is the next step for you.• EDUCATION/EDUCATORS: Whether you’re a college or university professor (tenured, adjunct or otherwise), a faculty member, or an elementary, middle, or high school teacher, you have a gift for relating with individuals of all ages and backgrounds. Your teaching skills make you methodical and analytical, and you can consolidate comprehensive information into strategic and analytical lesson plans. Your skills will easily translate to the Special Agent career, where you’ll enhance your team’s understanding of threats, vulnerabilities and gaps by investigating matters and building relationships with communities and individuals from all walks of life. KEY REQUIREMENTSMinimum Qualifications • Must be at least age 23 and not yet 37 at the time of your appointment. (Age waivers are available.)• Must be able to obtain a Top Secret Clearance.• Must possess a valid driver\"s license.• Must meet the FBI’s physical fitness requirements. • Must commit to serving as a Special Agent for a minimum of three (3) years.                                                                                                                                                                     Work Experience: All Special Agent applicants must also have at least three (3) years of full-time professional work experience unless they qualify for a waiver. Work experience showing progressive growth, leadership and responsibility is preferred. Certain specified experiences and/or abilities waive the three-year full-time work experience requirement. These include applicants with degrees in Computer Science, Information Technology, a Juris Doctor (J.D.) degree and Certified Public Accountants (CPAs). Applicants with masters’ and/or doctorate degrees require two (2) years of full-time work experience. NOTE: Meeting the requirements for a waiver does not guarantee that you will be deemed \"Most Competitive.\"Physical Requirements: Applicants must pass FBI Special Agent Physical Requirements, including medical and hearing standards. Applicants must also pass the Physical Fitness Test (PFT) and be physically fit to complete a Basic Field Training Course at Quantico and maintain a high level of physical fitness throughout their careers.All Special Agent applicants must also have at least three (3) years of full-time professional work experience unless they qualify for a waiver. Work experience showing progressive growth, leadership and responsibility is preferred. Certain specified experiences and/or abilities waive the three-year full-time work experience requirement. These include applicants with degrees in Computer Science, Information Technology, a Juris Doctorate (J.D.) degree and Certified Public Accountants (CPAs). Applicants with masters’ and/or doctorate degrees require two (2) years of full-time work experience. NOTE: Meeting the requirements for a waiver does not guarantee that you will be deemed \"Most Competitive.\"Applicants must pass FBI Special Agent Physical Requirements, including medical and hearing standards. Applicants must also pass the Physical Fitness Test (PFT) and be physically fit to complete a Basic Field Training respond to life-threatening situations in the field.For more information about the Special Agent Selection System, physical fitness requirements and work experience waivers, please visit https://fbijobs.gov/career-paths/special-agents.QUALIFICATIONS AND EVALUATIONSSpecial Agent candidates must go through an extensive testing and interview process. Learn more about this on FBIjobs.gov.AUTOMATIC DISQUALIFIERSApplicants convicted of a felony; who use illegal drugs in Applicants convicted of a felony, who use illegal drugs in violation of the FBI Employment Drug Policy, who are in default of a student loan insured by the U.S. Government, who fail an FBI urinalysis drug test or who fail to register with the Selective Service System (males only) are not eligible for employment with the FBI.Applicants should consider that the average processing time to complete all stages of the selection process may take approximately one (1) year. The FBI may disqualify applicants at any time during the process, should it be determined that they will reach age 37 before appointment to the Basic Field Training Course.SECURITYMust be able to obtain a Top Secret Clearance.HOW TO APPLYWhen submitting applications, please follow the Federal Resume Template available at www.fbijobs.gov [https://www.fbijobs.gov/working-at-FBI/how-to-apply] under the “Documents for Application” tab. This page also gives many details about the application process.      Learn more about the job posting on apply.fbijobs.gov [https://apply.fbijobs.gov/psc/ps/EMPLOYEE/HRMS/c/HRS_HRAM_FL.HRS_CG_SEARCH_FL.GBL?Page=HRS_APP_JBPST_FL&Action=U&FOCUS=Applicant&SiteId=1&JobOpeningId=13972&PostingSeq=1].EEOThe FBI is proud to be an Equal Opportunity Employer; our employees come from diverse backgrounds but share a common mission that makes us One FBI — and all qualified applicants will receive consideration. Except where otherwise provided by law, selection will be made without regard to, and with no discrimination based on color, race, religion, national origin, political affiliation, marital status, parental status, physical or mental disability, genetic information, age, sex, sexual orientation, membership or non-membership in an employee organization, personal favoritism or any other non-merit factors."

``` r
length(unique(job_sum_text))
```

    ## [1] 79

We have one less unique text value than the total values. Which one is duplicated?

``` r
searchAllJobUrls[c(match(job_sum_text[which(duplicated(job_sum_text) == TRUE)],job_sum_text),which(duplicated(job_sum_text) == TRUE))]
```

    ## [1] "https://job-openings.monster.com/commercial-banking-–data-science-analytics-insights-170114830-columbus-oh-us-jp-morgan/31/af494e17-1c49-4271-bf32-98609a230ddc"  
    ## [2] "https://job-openings.monster.com/commercial-banking-–data-science-analytics-insights-170114830_1-columbus-oh-us-jp-morgan/31/0c55cd71-f62c-4b24-81ba-0d5055dcea6b"

Clearly the same job is listed twice under two different URLs.

Let's remove the duplicate, and also remove the job with no description.

``` r
searchAllJobUrls <- searchAllJobUrls[job_sum_text != "" & duplicated(job_sum_text) == FALSE]
job_sum_text <- job_sum_text[job_sum_text != "" & duplicated(job_sum_text) == FALSE]

length(searchAllJobUrls)
```

    ## [1] 78

``` r
length(job_sum_text)
```

    ## [1] 78

``` r
save(list=c("searchAllJobUrls","job_sum_text"),
file=paste0(data_store_path,"/searchAllJobUrls_and_job_sum_text_objects.Rdata"))
```
