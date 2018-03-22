
Extracting soft skills and comparing resume frequency between cities
====================================================================

Heather Geiger
--------------

Load libraries and previous Rdata file.
---------------------------------------

``` r
library(stringr)    #For string operations
library(rvest)      #For screen scrapper
library(tidyverse)  #For Tidyverse
library(RCurl)      #For File Operations
library(dplyr)      #For Manipulating the data frames
library(DT)         #For Data table package
library(curl)

library(RJSONIO) #For processing JSON format data

library(ggplot2) #For plotting
```

``` r
load("resumes_across_cities_one_text_field_per_resume.Rdata")
```

Using regular expressions to count resumes containing each skill
----------------------------------------------------------------

Set up a dictionary of keywords per skill.

``` r
keywords <- data.frame(Skill = c("Collaboration",
                "Communication",
                "Creativity",
                "Customer service",
                "Decision making",
                "Leadership",
                "Presentation",
                "Problem solving"),
            Synonyms = c("collaboration,collaborative,collaborate,collaborated,team player,teamwork",
                "communication,communicative,communicate,communicated",
                "creativity,creative",
                "customer service",
                "decision making,made decisions,making decisions,decision maker",
                "leadership,led,leader,leading",
                "presentation,presenting,presented",
                "problem solving,solving problems,solved problems"),
            stringsAsFactors=FALSE)

keyword_list <- vector("list",length=nrow(keywords))

for(i in 1:nrow(keywords))
{
keyword_list[[i]] <- unlist(strsplit(keywords$Synonyms[i],",")[[1]])
}

keyword_list
```

    ## [[1]]
    ## [1] "collaboration" "collaborative" "collaborate"   "collaborated" 
    ## [5] "team player"   "teamwork"     
    ## 
    ## [[2]]
    ## [1] "communication" "communicative" "communicate"   "communicated" 
    ## 
    ## [[3]]
    ## [1] "creativity" "creative"  
    ## 
    ## [[4]]
    ## [1] "customer service"
    ## 
    ## [[5]]
    ## [1] "decision making"  "made decisions"   "making decisions"
    ## [4] "decision maker"  
    ## 
    ## [[6]]
    ## [1] "leadership" "led"        "leader"     "leading"   
    ## 
    ## [[7]]
    ## [1] "presentation" "presenting"   "presented"   
    ## 
    ## [[8]]
    ## [1] "problem solving"  "solving problems" "solved problems"

Write a function to give a pattern for a keyword if it has a word boundary, comma, or space on each side.

Then if there are multiple keywords, paste these together with a pipe.

Finally, run this function for every item in keyword\_list.

``` r
#Couldn't figure out how to get a regex for a space, comma, or word boundary. However did get one that can get either a space or comma.
space_or_comma <- "[[:space:],]"
word_boundary <- "\\b"

pattern_for_one_keyword <- function(keyword){
        regexes <- paste0(space_or_comma,keyword,space_or_comma)
        regexes <- c(regexes,paste0(word_boundary,keyword,word_boundary))
        regexes <- c(regexes,paste0(word_boundary,keyword,space_or_comma))
        regexes <- c(regexes,paste0(space_or_comma,keyword,word_boundary))
        return(paste0(regexes,collapse="|"))
}

pattern_for_multiple_keywords <- function(keyword_vector){
        if(length(keyword_vector) == 1){return(pattern_for_one_keyword(keyword_vector))}
        if(length(keyword_vector) > 1){
                individual_regexes <- c()
                for(i in 1:length(keyword_vector))
                {
                        individual_regexes <- c(individual_regexes,pattern_for_one_keyword(keyword_vector[i]))
                }
        return(paste0(individual_regexes,collapse="|")) 
        }
}

keyword_regexes <- unlist(lapply(keyword_list,function(x)pattern_for_multiple_keywords(x)))
```

We can now use keyword\_regexes along with str\_detect to give a TRUE/FALSE value for whether the text contains the pattern.

``` r
num_resumes_per_skill <- c()

for(i in 1:length(keyword_regexes))
{
skill <- keyword_regexes[i]
skill_in_text <- str_detect(tolower(resumes_across_cities$Text),skill)
resumes_across_cities_incl_this_skill <- resumes_across_cities[skill_in_text,]
num_resumes_this_skill <- data.frame(resumes_across_cities_incl_this_skill %>% group_by(City) %>% summarize(Num.resumes = n()),stringsAsFactors=FALSE)
num_resumes_this_skill <- data.frame(Skill = keywords$Skill[i],num_resumes_this_skill,stringsAsFactors=FALSE)
if(i == 1){num_resumes_per_skill <- num_resumes_this_skill}
if(i > 1){num_resumes_per_skill <- rbind(num_resumes_per_skill,num_resumes_this_skill)}
}

num_resumes_per_skill
```

    ##               Skill          City Num.resumes
    ## 1     Collaboration      New York         283
    ## 2     Collaboration San Francisco         122
    ## 3     Communication      New York         238
    ## 4     Communication San Francisco         114
    ## 5        Creativity      New York          47
    ## 6        Creativity San Francisco          18
    ## 7  Customer service      New York          31
    ## 8  Customer service San Francisco          24
    ## 9   Decision making      New York          70
    ## 10  Decision making San Francisco          27
    ## 11       Leadership      New York         342
    ## 12       Leadership San Francisco         212
    ## 13     Presentation      New York         263
    ## 14     Presentation San Francisco         129
    ## 15  Problem solving      New York          34
    ## 16  Problem solving San Francisco          20

Finally, add a column Percent.resumes which expresses the percentage of resumes from the city that contain the skill.

``` r
resumes_per_city <- data.frame(table(resumes_across_cities$City))
resumes_per_city[,1] <- as.vector(resumes_per_city[,1])

colnames(resumes_per_city) <- c("City","Total.resumes.this.city")

num_resumes_per_skill <- merge(num_resumes_per_skill,resumes_per_city,"City") %>% mutate(Percent.of.resumes.this.city = round(Num.resumes*100/Total.resumes.this.city,digits=2))

ggplot(num_resumes_per_skill,aes(x=Skill,y=Percent.of.resumes.this.city,fill=City)) + 
geom_bar(stat="identity",position="dodge") + 
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
ylab("Percent of resumes in city with keywords")
```

![](indeed_resume_data_frame_skill_grep_and_analysis_soft_skills_files/figure-markdown_github/unnamed-chunk-7-1.png)

Based on the plot, it appears that New York has more resumes containing keywords for skills of Collaboration, Communication, and Presentation.

San Francisco resumes seem to contain more keywords for Leadership.

Let's run a chi-squared test for significance for each of these skills.

``` r
matrix_for_chi_squared_all_skills <- num_resumes_per_skill %>%
                mutate(Num.resumes.without.this.skill = Total.resumes.this.city - Num.resumes) %>%
                select(c("Skill","City","Num.resumes","Num.resumes.without.this.skill"))

colnames(matrix_for_chi_squared_all_skills) <- c("Skill","City","Has","Does.not.have")

for(skill in c("Collaboration","Communication","Presentation","Leadership"))
{
table_for_chi_squared <- matrix_for_chi_squared_all_skills %>% filter(Skill == skill) %>% select(c("Has","Does.not.have"))
rownames(table_for_chi_squared) <- c("New York","San Francisco")
print(skill)
print(table_for_chi_squared)
print(chisq.test(table_for_chi_squared))
}
```

    ## [1] "Collaboration"
    ##               Has Does.not.have
    ## New York      283           703
    ## San Francisco 122           449
    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table_for_chi_squared
    ## X-squared = 9.7333, df = 1, p-value = 0.00181
    ## 
    ## [1] "Communication"
    ##               Has Does.not.have
    ## New York      238           748
    ## San Francisco 114           457
    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table_for_chi_squared
    ## X-squared = 3.3643, df = 1, p-value = 0.06663
    ## 
    ## [1] "Presentation"
    ##               Has Does.not.have
    ## New York      263           723
    ## San Francisco 129           442
    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table_for_chi_squared
    ## X-squared = 2.9846, df = 1, p-value = 0.08406
    ## 
    ## [1] "Leadership"
    ##               Has Does.not.have
    ## New York      342           644
    ## San Francisco 212           359
    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table_for_chi_squared
    ## X-squared = 0.83742, df = 1, p-value = 0.3601

Most of the differences we find are not significant.

However, we do find that keywords associated with skill Collaboration appear in New York data scientist resumes significantly more often than in San Francisco resumes.
