
Downloading Indeed.com Resumes Matching a Search as JSON files
==============================================================

Heather Geiger - March 20, 2018
===============================

Load libraries.
---------------

Load a bunch of libraries.

I'm actually not sure which of these I actually use, as some of this code is from Raj. Need to check that.

Currently I load all of the libraries Raj suggests, plus RJSONIO to process JSON files.

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

``` r
indeedUrlBuilder <- function(jobtitle, cityname, statecode){
startUrl <- "https://www.indeed.com/resumes?q="
jobtitle <- gsub(" ","+",jobtitle)
middle0Url <- "&l="
cityname <- gsub(" ","+",cityname)
middle1Url <- "%2C+"
endUrl <- "&searchField=jt"
searchUrl <- paste0(startUrl,jobtitle,middle0Url,cityname,middle1Url,statecode,endUrl)
return(searchUrl)
}
```

Run for data scientist in New York, NY.

``` r
url_to_search <- indeedUrlBuilder("data scientist","New York","NY")
url_to_search
```

    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt"

Paste this together with iterations of 50 from 50 to 950. This gives pages 2-20 of the search results, so in total we can look at 1,000 resumes.

``` r
for(i in seq(from=50,to=950,by=50))
{
print(paste0(url_to_search,"&start=",i))
}
```

    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=50"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=100"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=150"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=200"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=250"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=300"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=350"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=400"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=450"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=500"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=550"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=600"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=650"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=700"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=750"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=800"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=850"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=900"
    ## [1] "https://www.indeed.com/resumes?q=data+scientist&l=New+York%2C+NY&searchField=jt&start=950"

Step 2 - Paste search queries into a browser and use point-and-click to get commands to download JSON files using the API.
--------------------------------------------------------------------------------------------------------------------------

For each of these URLs, we use a browser plus point-and-click to get the curl command to download the JSON file for the page using the API.

While we do this, keep a text file called "curl\_commands\_first\_1000\_resumes.txt" open, where we will paste each command after copying.

I am eventually going to add screenshots here, but for now will just write text since can't get screenshots to work in Rmarkdown at the moment.

1.  Open developer tools. In Google Chrome, you get this by going to "More Tools", then "Developer Tools", in the top right menu.
2.  Go to tab "Network".
3.  Reload page. All network calls the page is making will now show.
4.  Go to the one with type "fetch" whose name starts with "search".
5.  Right click, click copy, then click "Copy as cURL".

Step 3 - Run curl commands.
---------------------------

We run all 20 curl commands as system commands now.

Output the results of each to their own JSON file.

``` r
dir.create("data_scientist_new_york_NY_search_results_json_files")

curl_commands <- readLines("curl_commands_first_1000_resumes.txt")

for(i in 1:length(curl_commands))
{
command <- curl_commands[i]
print(command)
system(paste0(command," > data_scientist_new_york_NY_search_results_json_files/resumes_pg",i,".json"))
}
```

    ##  [1] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; _gat=1; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beff0fc91f440a23c4dfabec463f55e06aa0c118b817879120d0aa4aa6a8c01f3fa968beb2026782035acd675db837caa1ad' -H 'Connection: keep-alive' --compressed"            
    ##  [2] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=50' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=50' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beff0fc91f440a23c4dfabec463f55e06aa0c118b817879120d0aa4aa6a8c01f3fa968beb2026782035acd675db837caa1ad' -H 'Connection: keep-alive' --compressed"  
    ##  [3] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=100' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=100' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beff0fc91f440a23c4dfabec463f55e06aa0c118b817879120d0aa4aa6a8c01f3fa968beb2026782035acd675db837caa1ad' -H 'Connection: keep-alive' --compressed"
    ##  [4] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=150' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=150' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beff0fc91f440a23c4dfabec463f55e06aa0c118b817879120d0aa4aa6a8c01f3fa968beb2026782035acd675db837caa1ad' -H 'Connection: keep-alive' --compressed"
    ##  [5] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=200' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=200' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ##  [6] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=250' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=250' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ##  [7] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=300' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=300' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ##  [8] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=350' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=350' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ##  [9] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=400' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=400' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [10] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=450' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=450' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [11] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=500' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=500' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [12] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=550' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=550' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [13] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=600' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=600' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [14] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=650' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=650' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [15] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=700' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=700' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [16] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=750' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=750' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [17] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=800' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=800' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [18] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=850' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=850' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [19] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=900' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=900' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"
    ## [20] "curl 'https://resumes.indeed.com/rpc/search?l=New%20York%2C%20NY&q=data%20scientist&searchFields=jt&start=950' -H 'Accept-Encoding: gzip, deflate, sdch, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36' -H 'Accept: */*' -H 'Referer: https://resumes.indeed.com/search?q=data+scientist&l=New+York%2C+NY&searchFields=jt&start=950' -H 'Cookie: CTK=1c90et2345tcledq; LC=\"co=US&hl=en\"; __ssid=5186686d-f0e2-4b0e-919d-6efce4d0fcb1; RF=\"TFTzyBUJoNr6YttPP3kyivpZ6-9J49o-Uk3iY6QNQqKE2fh7FyVgtUHkGuRKMXQcNnOrxC0eR_Q=\"; FD=\"ID=7cd222f603d55503:IL=1521512289904:SG=e23a8bf57bd6a5025a01bca0a489fc18\"; IRF=\"1qRi-3v0F_uf-yOkOwHemem4k5kxH8LlcrNj-SFeDD8=\"; optimizelyEndUserId=oeu1521507801503r0.5931852557320019; _ga=GA1.2.2037147864.1521507798; _gid=GA1.2.1524599205.1521507798; SURF=fYlpRwYKr2SyxvSqk0AV8Ag76alVpqzt; SHOE=\"CKrSuqBYGS6chNWlXVMVWOMHcmvhZPYQxG8nwXBZirhJeJ3EvM8owT_vAG02KSJvdCOqg2sWVQCCpFdu7nGs4I-TxvCwk1HG6MAaLjepuSQyTVwtEsUkdcbMg3YmARwhivwLUHrYmjVI\"; SOCK=\"geB5CuqO5oNe0EiW79VF1PbpYME=\"; CSRF=YV8u3sHCJPkhA56gW8g24kjGbIZIZt5E; INDEED_CSRF_TOKEN=QjrAofdkeF353rPqbx5elA1u95dfzGEy; TS01d65e80=0160a2beffa996205d067a9799f62f512c1a1a296c165bf8f955e93c39880e68a2b950f538dddcbd75b7dddf6902bf9d7d49577bb7' -H 'Connection: keep-alive' --compressed"

Step 4 - Process the search result JSON files.
----------------------------------------------

For each JSON file, read into R and use this to get the links to resumes.

``` r
resume_ids <- rep(NA,times=1000)

for(i in 1:length(curl_commands))
{
json_search_results <- fromJSON(paste0("data_scientist_new_york_NY_search_results_json_files/resumes_pg",i,".json"))
j = i - 1
resume_ids[(j*50 + 1):(j*50 + 50)] <- unlist(lapply(json_search_results$results,"[[","accountKey"))
}

head(resume_ids)
```

    ## [1] "7c8ce5eaed3759bb" "dd02fe91c666cdb1" "65a643093bccde60"
    ## [4] "fc44172915acc502" "edf0c151e73baebf" "e924720ddb3306b0"

``` r
tail(resume_ids)
```

    ## [1] "41c652349b72661b" "8a6b58e824ed557e" "ae5847b866a0047b"
    ## [4] "b145a1ac8f772a53" "926c0ac0f216d4e9" "d03e049f9a1e9c6a"

Step 5 - Download JSON files for resumes.
-----------------------------------------

For a given value in resume\_ids, we can get the corresponding resume link by pasting to "<https://resumes.indeed.com/resume/>".

Then, read in the file. Take the line in the HTML file that is being run through the JSON.parse command.

The JSON file will be bounded by curly brackets, then "\\x22" at the beginning. It will end with "\\x22", then curly brackets.

There will also be other "\\x22" occurences in between, so take the first and last one.

Convert "\\x22" to quote, and it should now be in a format where fromJSON will interpret it as a JSON.

Write out each JSON to a file.

``` r
dir.create("data_scientist_new_york_NY_resume_json_files")

for(i in 1:length(resume_ids))
{
resume_link <- paste0("https://resumes.indeed.com/resume/",resume_ids[i])
resume_read_as_text <- readLines(resume_link)
resume_read_as_text <- grep('JSON.parse',resume_read_as_text,value=TRUE)
x22_locations <- str_locate_all(resume_read_as_text,pattern='\\\\x22')[[1]]
resume_json <- substr(resume_read_as_text,x22_locations[1,1] - 1,x22_locations[nrow(x22_locations),2] + 1)
resume_json <- str_replace_all(resume_json,pattern='\\\\x22',replace='"')
write.table(resume_json,file=paste0("data_scientist_new_york_NY_resume_json_files/resume_result_",i,".json"),
row.names=FALSE,col.names=FALSE,quote=FALSE)
if(i %% 50 == 0){print(paste0("Resume ",i," has been processed"))}
}
```

    ## [1] "Resume 50 has been processed"
    ## [1] "Resume 100 has been processed"
    ## [1] "Resume 150 has been processed"
    ## [1] "Resume 200 has been processed"
    ## [1] "Resume 250 has been processed"
    ## [1] "Resume 300 has been processed"
    ## [1] "Resume 350 has been processed"
    ## [1] "Resume 400 has been processed"
    ## [1] "Resume 450 has been processed"
    ## [1] "Resume 500 has been processed"
    ## [1] "Resume 550 has been processed"
    ## [1] "Resume 600 has been processed"
    ## [1] "Resume 650 has been processed"
    ## [1] "Resume 700 has been processed"
    ## [1] "Resume 750 has been processed"
    ## [1] "Resume 800 has been processed"
    ## [1] "Resume 850 has been processed"
    ## [1] "Resume 900 has been processed"
    ## [1] "Resume 950 has been processed"
    ## [1] "Resume 1000 has been processed"

Step 6 - Process resume JSON files.
-----------------------------------

We create a series of lists to store each resume's job titles, job descriptions, and lists of skills.

We also create a vector to store each resume's summary section.

Only thing is we run this for slightly less than the full 1,000 resumes.

After some debugging, it appears that 6/1000 resumes (indices 185, 200, 763, 786, 795, and 815) are malformed JSONs.

Another 8/1000 are missing descriptions for some jobs (indices 66,111,213,290,294,505,627,837).

Going to skip these, and then all the others should be fine.

I think an error rate of 14/1000 here (1.4%) is pretty reasonable, and we will still have plenty of data to work with.

``` r
job_titles <- vector("list",length=1000)
job_descriptions <- job_titles
lists_of_skills <- job_titles
executive_summaries <- rep(NA,times=1000)

for(i in setdiff(1:1000,c(185,200,763,786,795,815,66,111,213,290,294,505,627,837)))
{
resume_json <- fromJSON(paste0("data_scientist_new_york_NY_resume_json_files/resume_result_",i,".json"))
job_titles[[i]] <- unlist(lapply(resume_json$resumeModel$workExperience,"[[","title"))
job_descriptions[[i]] <- unlist(lapply(resume_json$resumeModel$workExperience,"[[","description"))
lists_of_skills[[i]] <- unlist(lapply(resume_json$resumeModel$skills,"[[","skill"))
executive_summaries[i] <- unlist(resume_json$resumeModel$summary)
if(i %% 50 == 0){print(paste0("Resume ",i," has been processed"))}
}
```

    ## [1] "Resume 50 has been processed"
    ## [1] "Resume 100 has been processed"
    ## [1] "Resume 150 has been processed"
    ## [1] "Resume 250 has been processed"
    ## [1] "Resume 300 has been processed"
    ## [1] "Resume 350 has been processed"
    ## [1] "Resume 400 has been processed"
    ## [1] "Resume 450 has been processed"
    ## [1] "Resume 500 has been processed"
    ## [1] "Resume 550 has been processed"
    ## [1] "Resume 600 has been processed"
    ## [1] "Resume 650 has been processed"
    ## [1] "Resume 700 has been processed"
    ## [1] "Resume 750 has been processed"
    ## [1] "Resume 800 has been processed"
    ## [1] "Resume 850 has been processed"
    ## [1] "Resume 900 has been processed"
    ## [1] "Resume 950 has been processed"
    ## [1] "Resume 1000 has been processed"

``` r
job_titles <- job_titles[setdiff(1:1000,c(185,200,763,786,795,815,66,111,213,290,294,505,627,837))]
job_descriptions <- job_descriptions[setdiff(1:1000,c(185,200,763,786,795,815,66,111,213,290,294,505,627,837))]
lists_of_skills <- lists_of_skills[setdiff(1:1000,c(185,200,763,786,795,815,66,111,213,290,294,505,627,837))]

executive_summaries <- executive_summaries[setdiff(1:1000,c(185,200,763,786,795,815,66,111,213,290,294,505,627,837))]
```

Use job\_titles and job\_descriptions list to make one big data frame.

In a given resume, there will often be multiple job title/description combinations. So, we repeat each resume index by the number of job titles there are.

For skills, there may also frequently be multiple skills per resume. So we follow a similar strategy there as well.

``` r
job_titles_and_descriptions_across_resumes <- data.frame(Resume.num = rep(setdiff(1:1000,c(185,200,763,786,795,815,66,111,213,290,294,505,627,837)),
                            times=unlist(lapply(job_titles,function(x)length(x)))),
                        Job.title = unlist(job_titles),
                        Job.description = unlist(job_descriptions),
                        stringsAsFactors=FALSE)
skills_per_resume <- data.frame(Resume.num = rep(setdiff(1:1000,c(185,200,763,786,795,815,66,111,213,290,294,505,627,837)),
                times=unlist(lapply(lists_of_skills,function(x)length(x)))),
                Skill = unlist(lists_of_skills),
                stringsAsFactors=FALSE)
```

Save our work as we go along as good practice.

``` r
save.image("resumes_processed.Rdata")
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
    ## 6          5 Data Scientist
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Job.description
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              This Project AML Transaction Monitoring. Compliance programs depend on accurate and timely information. AML compliance centers on sifting through thousands of transactions and matching them against risk profiles. The result of that process is a focused examination of transactions and identification of suspicious.\\n• Performed data extraction and analysis to develop business process mining model using BupaR.\\n• Built forecast models using Prophet that improved planning and productivity by 25%.\\n• Created Anomaly detection model, and auto answering chatbot to improve task management which was highly recognized by top management.\\n• Developed SLA prediction models to help teams enhance work quality well before deadline.\\n• Improved old models of SPSS into R framework by execution speed and accuracy by 20%.\\n• Developed sophisticated data models to support automated reporting and analytics.\\n• Analyzed & processed complex data sets using advanced query, visualization and analytics tools.\\n• Collaborated with teams to develop and support internal data platform, ongoing analyses of client behavior and business outcomes, deployment of models on R server.\\n• Performed data integrity checks, data cleaning, exploratory analysis and feature engineer\\n• Developed personalized products recommendation with Machine Learning algorithms including collaborative filtering and Gradient Boosting Tree to meet the needs of existing customers and acquire new customers.\\n• Coordinated the execution of A\\u002FB tests to measure the effectiveness of personalized recommendation system.\\n• Recommended and evaluated marketing approaches based on quality analytics of customer consuming behavior.\\n• Implemented Dynamic time wrapping for time series classification.\\nEnvironment: R, Oracle 12c, Tableau.
    ## 2 Project 1: Mining and Textile Data Analysis\\nThe team of Data analysts focused on providing analytics insights and decision support tools for executives for accurate demand planning and task allocation.\\n• Identified, measured and recommended improvement strategies for KPIs across all business areas.\\n• Assisted in defining, implementing, and utilizing business metrics calculations and methodologies.\\n• Managed a team of Research, Development and Analysis (RDA) professionals for 1 year.\\n• Assisted in demand planning by delivering the accurate forecasts and allocation plans for tickets.\\n• Provided analytical support to underwriting and pricing by preparing and analyzing data to be used in auctorial calculations.\\n• Designed dashboards with Tableau and Meteor JS provided complex reports including summaries, charts, and graphs to interpret findings to team and stakeholders.\\n• Identified process improvements that significantly reduce workloads or improve quality.\\n• Worked for BI & Analytics team to conduct A\\u002FB testing, data extraction and exploratory analysis.\\n• Generated dashboards and presented the analysis to researchers explaining insights on the data.\\n• Improved probability predictions model for client to check the prices of raw material the next year.\\nEnvironment: Excel 2010, R, MS SQL Server 200.\\n\\nProject 2: Insurance and Mining\\nData Analyst\\nThe team of developers and consultant worked across multiple projects to implement OLAP and OLTP systems, data modeling, system migration from old data warehouse and system maintenance.\\n• Built data pipelines from multiple data sources by performing necessary ETL tasks.\\n• Performed Exploratory Data Analysis using R, Apache Spark and text analysis, tri-idf analysis.\\n• Worked on Data Cleaning, features scaling, features engineering.\\n• Handle natural language processing to extract features from text data.\\n• Visualized bigrams networks to investigate individual importance.\\n• Built a forecasting model to predict future sales for anti-diabetes vaccines in global market.\\n• Built multiple time-series models like ARIMA, ARIMAX (Dynamic Regression), TBATS, ETS.\\n• Evaluated model's performance on multiple test metrics such as MAPE, MAE & MASE.\\n• Developed a shiny app to highlight Bayesian analysis and performed visualizations with ggplot2.\\nEnvironment: Oracle, SQL.
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               λ\\tResponsible for data science related data processing and data analysis with Python\\nλ\\tConstructed data Pipeline with python \\nλ\\tDeveloped solutions that utilizes AWS EC2, RDS, S3 and other AWS services \\nλ\\tCooperated with technology team determining the full domain of the MVP\\nλ\\tCreated and implement relevant data model for the App and integrated the MVP into the App and any backend domains; \\nλ\\tInsure REST-base API including all CRUD operations integrate with the App and other service domains;
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         • Modeling, clustering and doing segmentation of comScore\\u002FIRI\\u002FNeustar DMP data and Annalect’s Agile Data Platform(ADP) using Python and AWS;\\n• Implementing a standard digital attribution in PostgreSQL in order to target high value audience;\\n• Building several dynamic maps and web app using Lay’s domestic data in D3.js and Vue.js;\\n• Helping BBDO marketing science team understands usage patterns – why and how customers buy and use\\nour clients(VISA, Lays, Hilton etc)’ products using ComScore URL\\u002FYoutube and Neustar DSDKdatabase
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A former web developer pivoted to data science with 1.5+ years of experience in Machine and Deep Learning. Able to leverage a heavy dose of machine learning models on large sets of structured, semi-structured and unstructured data for classifications, regressions, predictive analysis, visualizations and a healthy sense of data exploration with storytelling.

``` r
tail(job_titles_and_descriptions_across_resumes)
```

    ##      Resume.num                                     Job.title
    ## 4893       1000                 VP, ALGORITHMS & DATA SCIENCE
    ## 4894       1000           DIRECTOR, ALGORITHMS & DATA SCIENCE
    ## 4895       1000                      Program Committee Member
    ## 4896       1000                    SENIOR INVENTIVE SCIENTIST
    ## 4897       1000                                        Intern
    ## 4898       1000 RESEARCH INTERN - Audio and context awareness
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Job.description
    ## 4893                                           In charge of growing and managing the Algorithms & Data Science team. Hands-on lead developer of new algorithms and supporting existing ones powering the product intelligence. Introducing new\\nfeatures, and spearheading the company's Analytics Portal project. Led the design of the front-end and back-end architecture of the analytics infrastructure. Deeply involved with the Product and\\nBusiness Dev teams to shape the company business direction and very often setting or redesigning the Product roadmap. Reporting to CTO.\\n\\nEmiliano Miluzzo                         Last revised 4\\u002F25\\u002F2017\\nPROFESSIONAL EXPERIENCE
    ## 4894 Lead developer, responsible manager, and ground-up recruiter of the Algorithms & Data Science\\nteam, a group of 4 engineers in charge of the design and development of the ML algorithms and software at the core of the Apio's (now Sfara) product. In this role, through heavy hands-on approach and leadership skills, created all the algorithms of the Apio's platform, from hard braking, rapid\\nacceleration, distracted driving, and crash detection to name a few, by exploiting contextual\\nawareness inference using mobile devices' onboard sensors. Applied cutting edge R&D methodology and software development techniques. Part of the company executive team, reporting to CTO.
    ## 4895                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        including MobiSys, MobiCASE, SenSys, UbiComp, EWSN, PerCom, Mobiquitous, SECON.\\n\\nGuest Editor for IEEE Communications Magazine and International Journal of Distributed Sensor networks.
    ## 4896                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Applied research focused on mobile and ubiquitous computing, context-awareness with support of machine learning techniques, storage and cloud computing. Contributing to the company's product\\nportfolio.
    ## 4897                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Applying machine learning algorithm to support smartphone services,
    ## 4898                                                                                                                                                                                                                                                                                                                                                                                                            Columbia University\\nRESEARCHER  -  Wireless Sensor Networks\\nUniversity of Rome, La Sapienza\\nRESEARCHER  -  802.11 Medium Access Control\\nSiemens Mobile Communications\\nSOFTWARE ENGINEER\\u002FRESERCHER  -  WiFi and Cellular network integration\\n\\nSELECT APPLIED PROJECTS

``` r
head(skills_per_resume)
```

    ##   Resume.num         Skill
    ## 1          1   Forecasting
    ## 2          1            QA
    ## 3          1  Data science
    ## 4          1 Data analysis
    ## 5          1       Tableau
    ## 6          1     Analytics

``` r
tail(skills_per_resume)
```

    ##      Resume.num
    ## 4109        995
    ## 4110        999
    ## 4111        999
    ## 4112        999
    ## 4113        999
    ## 4114        999
    ##                                                                                                      Skill
    ## 4109 Pattern Recognition, Machine learning, FEM, FDTD, Monte Carlo, simulated annealing, gradient descent.
    ## 4110                                                                                  MICROSOFT SHAREPOINT
    ## 4111                                                                                            SHAREPOINT
    ## 4112                                                                                                   Git
    ## 4113                                                                                                 LINUX
    ## 4114                                                                                            Subversion

Final step - clean up after make the data frames from the JSON files.
---------------------------------------------------------------------

Just from looking at the head and tail, we can see we will need to do some clean-up here.

For one, we need to change the special encoding "\\u002F" to a "/".

We also see that sometimes the skills field was formatted so that each skill was an item in a vector, while sometimes it is free text, with a one-item free text character vector that might actually describe many skills.

Let's look at the first few skills with a comma in them.

``` r
skills_with_comma <- skills_per_resume[grep(',',skills_per_resume[,2]),"Skill"]
head(skills_with_comma)
```

    ## [1] "EXTRACT, TRANSFORM, AND LOAD"                                                                                                                              
    ## [2] "EXTRACT, TRANSFORM, AND LOAD"                                                                                                                              
    ## [3] "EXTRACT, TRANSFORM, AND LOAD"                                                                                                                              
    ## [4] "R, Python, C++, Matlab, Scala, Java"                                                                                                                       
    ## [5] "Python, SQL (Postgres, MySQL), Javascript, Java, R. Distributed computing, data analysis, data mining, prediction algorithms, natural language processing."
    ## [6] "R, Excel, PowerPoint, Tableau, SQL"

``` r
skills_with_comma <- skills_with_comma[skills_with_comma != "EXTRACT, TRANSFORM, AND LOAD"]
head(skills_with_comma,n=20)
```

    ##  [1] "R, Python, C++, Matlab, Scala, Java"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
    ##  [2] "Python, SQL (Postgres, MySQL), Javascript, Java, R. Distributed computing, data analysis, data mining, prediction algorithms, natural language processing."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
    ##  [3] "R, Excel, PowerPoint, Tableau, SQL"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
    ##  [4] "SAS, SPSS, R, MATLAB, SQL, Spark\\u002FHadoop and Tableau"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
    ##  [5] "Python(scikit-learn, numpy, scipy, pandas, Plotly)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
    ##  [6] "Automation, Google API, SSP API, Algorithm, Data Visualization, Data Mining, Data Modeling, Simulation, Python, Advanced Excel, SAS, R, SQL, VBA, Macro, Arena, Big Data, Google Analytics, Optimization, JSON, Quantitative Analytics, DoubleClick for Publisher, Predictive Analytics, JIRA, git, RDBMS, Matlab, Regressions"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
    ##  [7] "Excel, VBA, Access, MS Azure SQL, MySQL, Stata, R, C++, Qlik Sense, Nprinting, Bloomberg (completed the Bloomberg Essentials Online Training Program), Argus Valuation DCF, CU*Base Gold (Accounting Software), Matlab, HelioScope (Solar Design Software), Sharepoint and basic knowledge in Python, HTML & PHP"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    ##  [8] "Python, R, IDEA, SPLUNK, Hadoop, Hortonworks, Hive, Tableau, MS Excel, Linux, Regex, Big data, semi-structured data, HTML5, CSS, Bootstrap, Balsamiq, Javascript, Jquery, Google maps API, Leaflet,  Quantum GIS, PHP, MySQL, NET, C#, MVC architecture, XSLT, SDMX, XML, SVG, Json, HTML, CSS, JavaScript, Jquery, D3, Google chart tools, highcharts, Oracle, Apache."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    ##  [9] "Statistics, Regression, Bayesian Inference, Multivariate Analysis, Sampling, Stochastic Processes, Classification, Clustering, Decision Trees, Neural Networks"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
    ## [10] "R, Python, SQL, Bash, Unix, Git, Hadoop, Hive, Vertica"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    ## [11] "Large Datasets (millions of observations, terabytes in volume), Data Cleaning, Data Mining, Unstructured and Semi-Structured Datasets (natural language, JSON, HTML\\u002FXML)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
    ## [12] "Python (Numpy, Pandas)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    ## [13] "Data Analysis, Database"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    ## [14] "Excel, Word, Microsoft Office"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ## [15] "Business Intelligence, Advance SAS, SQL, Project Management, People Manager, Marketing Analytic, Data Science, Modeling, Risk Management"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
    ## [16] "Clinical Research; Hematology Oncology; Database management; Legal research; EKG, Phlebotomy, Vital Signs."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
    ## [17] "R (dplyr, tidyr, ggplot2, plotly, leaflet, glm, caret, randomforest, shiny, shinydashboard), Python (numpy, scipy, pandas, scrapy, selenium, beautifulsoup, matplotlib, seaborn, scikit-learn, spacy, gensim, NLTK), SQL"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
    ## [18] "•\\tSupervised and unsupervised algorithms including classification (Naive Bayes, Gaussian, Kmeans Clustering, Expectation maximization, Decision Tree, SVM, K-NN, etc) and Regression (Logistic, linear, stepwise, etc). Normalization of data, Accuracy measurement  and tuning predictive models, Regularization algorithms (Ridge Regression, Least Absolute Shrinkage and Selection Operator (LASSO), Elastic Net), Dimensionality reduction algorithms (Principal Component Analysis (PCA), Partial Least Squares Regression, Linear Discriminant Analysis (LDA)) •\\tDeep learning algorithms (Deep Boltzmann Machine (DBM), Deep Belief Networks (DBN), etc) •\\tFraud detection and fraud management(pattern recognition, time series analysis of time-dependent data, probability distribution, neural netwroks (fuzzy, baysian, etc), artificial intelligence, machine learning)"
    ## [19] "•\\tStatistical software: SAS programming, SPSS, JMP, GraphPad •\\tProgramming languages: .NET, C#, ASP.NET, XML, PHP, Perl, Python, R •\\tRelational database programming: MS SQL, Microsoft Access, MY SQL •\\tNon-relational DB (NoSQL): MongoDB, PyMongo , HBase, Neo4J •\\tSystem analysis tools: together, Rational Rose, UML •\\tHadoop: HDFS, Cloudera, spark, pig, hive (HQL), impala, oozie, MapReduce, Solr, ETL •\\tData mining, Machine learning: caret, OpenNLP, skleran, tm, qdap, NLTK, BeautifulSoup, Spark MLlib, panda •\\tData visualization: ggplot2, Tableau, Looker •\\tDocumentation, Version control: Knitr, Gitlab, GitHub"                                                                                                                                                                                                                                       
    ## [20] "-\\tStatistics for Neuroscientist Course, McGill, Canada -\\tMultivariate statistical methods, non-parametric methods, categorical analysis, survival analysis -\\tMixed model analysis, Longitudinal data analysis, GLM -\\tApplication of statistical methodology in SAS programming and advanced statistical modeling techniques"

Looks like we can't use the presence or absence of a comma to determine which is which without context.

A Google reveals that "EXTRACT, TRANSFORM, AND LOAD" is actually one skill (<https://en.wikipedia.org/wiki/Extract,_transform,_load>).

If we are just run simple string processing for keywords on this column, I suppose it does not matter too much. But something to keep in mind.
