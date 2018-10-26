tidyverse presentation
================

Introduction to tidyverse packages and useful functions

Load packages
-------------

``` r
##Load packages

library(tidyverse)
library(seriation)
library(RCurl)
library(magrittr)
```

load data
---------

``` r
##Psych24 dataset from the seriation package
data("Psych24")

Psych24 <- as.data.frame(Psych24)

##fake employee dataset from github
x <- getURL("https://raw.githubusercontent.com/gbufton/SIOP_2018_Master_Tutorial/master/SIOP%20Data%20Wrangling%20Master%20Tutorial%20Data%20Set%20REGEX.csv")

EmpData <- read.csv(text = x, header = TRUE)
```

dplyr
-----

### names of data

``` r
names(EmpData)
```

    ##  [1] "Number"                                                                                                           
    ##  [2] "First.Name"                                                                                                       
    ##  [3] "Last.Name"                                                                                                        
    ##  [4] "Email.Address"                                                                                                    
    ##  [5] "Department"                                                                                                       
    ##  [6] "Division"                                                                                                         
    ##  [7] "Completed.On"                                                                                                     
    ##  [8] "Respondent.IP"                                                                                                    
    ##  [9] "Variable.With.Spaces"                                                                                             
    ## [10] "Variable.With.Colons"                                                                                             
    ## [11] "Variable.U.0097.With.U.0097.EmDash"                                                                               
    ## [12] "Variable.with.Quotes"                                                                                             
    ## [13] "Employee.Commitment.and.Satisfaction.ECS_Q1"                                                                      
    ## [14] "Employee.Commitment.and.Satisfaction.ECS_Q2"                                                                      
    ## [15] "Employee.Commitment.and.Satisfaction.ECS_Q3"                                                                      
    ## [16] "Employee.Commitment.and.Satisfaction.ECS_Q4"                                                                      
    ## [17] "Employee.Commitment.and.Satisfaction.ECS_Q5"                                                                      
    ## [18] "Employee.Commitment.and.Satisfaction.ECS_Q6"                                                                      
    ## [19] "Workplace.Environment.and.Culture.WEC_Q1"                                                                         
    ## [20] "Workplace.Environment.and.Culture.WEC_Q2"                                                                         
    ## [21] "Workplace.Environment.and.Culture.WEC_Q3"                                                                         
    ## [22] "Workplace.Environment.and.Culture.WEC_Q4"                                                                         
    ## [23] "Workplace.Environment.and.Culture.WEC_Q5"                                                                         
    ## [24] "Compensation...Benefits.CB_Q1"                                                                                    
    ## [25] "Compensation...Benefits.CB_Q2"                                                                                    
    ## [26] "Compensation...Benefits.CB_Q3"                                                                                    
    ## [27] "Compensation...Benefits.CB_Q4"                                                                                    
    ## [28] "Performance.Management.PM_Q1"                                                                                     
    ## [29] "Performance.Management.PM_Q2"                                                                                     
    ## [30] "Performance.Management.PM_Q3"                                                                                     
    ## [31] "Performance.Management.PM_Q4"                                                                                     
    ## [32] "Performance.Management.PM_Q5"                                                                                     
    ## [33] "Performance.Management.PM_Q6"                                                                                     
    ## [34] "Performance.Management.PM_Q7"                                                                                     
    ## [35] "Career.Development.and.Training.CDT_Q1"                                                                           
    ## [36] "Career.Development.and.Training.CDT_Q2"                                                                           
    ## [37] "Career.Development.and.Training.CDT_Q3"                                                                           
    ## [38] "Career.Development.and.Training.CDT_Q4"                                                                           
    ## [39] "Career.Development.and.Training.CDT_Q5"                                                                           
    ## [40] "Career.Development.and.Training.CDT_Q6"                                                                           
    ## [41] "Communications.COMM_Q1"                                                                                           
    ## [42] "Communications.COMM_Q2"                                                                                           
    ## [43] "Communications.COMM_Q3"                                                                                           
    ## [44] "Communications.COMM_Q4"                                                                                           
    ## [45] "Team.Effectiveness.TE_Q1"                                                                                         
    ## [46] "Team.Effectiveness.TE_Q2"                                                                                         
    ## [47] "Team.Effectiveness.TE_Q3"                                                                                         
    ## [48] "Team.Effectiveness.TE_Q4"                                                                                         
    ## [49] "Team.Effectiveness.TE_Q5"                                                                                         
    ## [50] "Job.Stress.JS_Q1"                                                                                                 
    ## [51] "Job.Stress.JS_Q2"                                                                                                 
    ## [52] "Job.Stress.JS_Q3"                                                                                                 
    ## [53] "Company.Image.CI_Q1"                                                                                              
    ## [54] "Company.Image.CI_Q2"                                                                                              
    ## [55] "Company.Image.CI_Q3"                                                                                              
    ## [56] "Company.Image.CI_Q4"                                                                                              
    ## [57] "Strategy.STRAT_Q1"                                                                                                
    ## [58] "Strategy.STRAT_Q2"                                                                                                
    ## [59] "Strategy.STRAT_Q3"                                                                                                
    ## [60] "On.a.scale.of.zero.to.ten..how.likely.is.it.that.you.would.recommend.Company.to.friends.as.a.great.place.to.work."
    ## [61] "What.2.3.things.do.you.value.most.about.working.at.Company."                                                      
    ## [62] "What.2.3.things.should.Company.begin.to.do."                                                                      
    ## [63] "What.2.3.things.should.Company.stop.doing."                                                                       
    ## [64] "Please.provide.suggestions.for.ongoing.improvement.to.the.performance.feedback.process."

### clean up data names

``` r
###extract all string data before *last* period
# names(EmpData) <- gsub(x = names(EmpData), 
#                     pattern = ".*\\.",
#                     replacement = "")

names(EmpData)[9:59] <- gsub(x = names(EmpData)[9:59] , 
                    pattern = ".*\\.", 
                    replacement = "")
```

### multiple operations within dplyr family

``` r
# magrittr pipe - don't need to run this
# EmpData %<>% ## '%<>%' is the same thing as 'EmpData <- EmpData %>%'
#   select(Number,ECS_Q1:WEC_Q5) %>%
#   filter(Number < 10000100)


EmpTest <- EmpData %>%
  select(Number,ECS_Q1:WEC_Q5) %>%
  filter(Number < 10000100) %>%
  mutate(new_col = "Round 1",
         new_col2 = case_when(Number < 10000050 ~ "A", 
                              Number > 10000050 & Number <= 10000075 ~ "B", 
                              TRUE ~ "C")) %>%
  arrange(desc(Number)) 
  # group_by(new_col2) %>%
  # tally() -> EmpTest2
```

### look at frequencies of observations

``` r
ftable(EmpTest$new_col2)
```

    ##   A  B  C
    ##          
    ##  50 25 25

### ways to examine data quickly

``` r
names(Psych24)
```

    ##  [1] "Visual.perception"        "Cubes"                   
    ##  [3] "Paper.form.board"         "Flags"                   
    ##  [5] "General.information"      "Paragraph.comprehension" 
    ##  [7] "Sentence.completion"      "Word.classification"     
    ##  [9] "Word.meaning"             "Addition"                
    ## [11] "Code"                     "Counting.dots"           
    ## [13] "Straight.curved.capitals" "Word.recognition"        
    ## [15] "Number.recognition"       "Figure.recognition"      
    ## [17] "Object.number"            "Number.figure"           
    ## [19] "Figure.word"              "Deduction"               
    ## [21] "Numerical.puzzles"        "Problem.reasoning"       
    ## [23] "Series.completion"        "Arithmetic.problems"

``` r
str(Psych24)
```

    ## 'data.frame':    24 obs. of  24 variables:
    ##  $ Visual.perception       : num  1 0.318 0.403 0.468 0.321 0.335 0.304 0.332 0.326 0.116 ...
    ##  $ Cubes                   : num  0.318 1 0.317 0.23 0.285 0.234 0.157 0.157 0.195 0.057 ...
    ##  $ Paper.form.board        : num  0.403 0.317 1 0.305 0.247 0.268 0.223 0.382 0.184 -0.075 ...
    ##  $ Flags                   : num  0.468 0.23 0.305 1 0.227 0.327 0.335 0.391 0.325 0.099 ...
    ##  $ General.information     : num  0.321 0.285 0.247 0.227 1 0.622 0.656 0.578 0.723 0.311 ...
    ##  $ Paragraph.comprehension : num  0.335 0.234 0.268 0.327 0.622 1 0.722 0.527 0.714 0.203 ...
    ##  $ Sentence.completion     : num  0.304 0.157 0.223 0.335 0.656 0.722 1 0.619 0.685 0.246 ...
    ##  $ Word.classification     : num  0.332 0.157 0.382 0.391 0.578 0.527 0.619 1 0.532 0.285 ...
    ##  $ Word.meaning            : num  0.326 0.195 0.184 0.325 0.723 0.714 0.685 0.532 1 0.17 ...
    ##  $ Addition                : num  0.116 0.057 -0.075 0.099 0.311 0.203 0.246 0.285 0.17 1 ...
    ##  $ Code                    : num  0.308 0.15 0.091 0.11 0.344 0.353 0.232 0.3 0.28 0.484 ...
    ##  $ Counting.dots           : num  0.314 0.145 0.14 0.16 0.215 0.095 0.181 0.271 0.113 0.585 ...
    ##  $ Straight.curved.capitals: num  0.489 0.239 0.321 0.327 0.344 0.309 0.345 0.395 0.28 0.408 ...
    ##  $ Word.recognition        : num  0.125 0.103 0.177 0.066 0.28 0.292 0.236 0.252 0.26 0.172 ...
    ##  $ Number.recognition      : num  0.238 0.131 0.065 0.127 0.229 0.251 0.172 0.175 0.248 0.154 ...
    ##  $ Figure.recognition      : num  0.414 0.272 0.263 0.322 0.187 0.291 0.18 0.296 0.242 0.124 ...
    ##  $ Object.number           : num  0.176 0.005 0.177 0.187 0.208 0.273 0.228 0.255 0.274 0.289 ...
    ##  $ Number.figure           : num  0.368 0.255 0.211 0.251 0.263 0.167 0.159 0.25 0.208 0.317 ...
    ##  $ Figure.word             : num  0.27 0.112 0.312 0.137 0.19 0.251 0.226 0.274 0.274 0.19 ...
    ##  $ Deduction               : num  0.365 0.292 0.297 0.339 0.398 0.435 0.451 0.427 0.446 0.173 ...
    ##  $ Numerical.puzzles       : num  0.369 0.306 0.165 0.349 0.318 0.263 0.314 0.362 0.266 0.405 ...
    ##  $ Problem.reasoning       : num  0.413 0.232 0.25 0.38 0.441 0.386 0.396 0.357 0.483 0.16 ...
    ##  $ Series.completion       : num  0.474 0.348 0.383 0.335 0.435 0.431 0.405 0.501 0.504 0.262 ...
    ##  $ Arithmetic.problems     : num  0.282 0.211 0.203 0.248 0.42 0.433 0.437 0.388 0.424 0.531 ...

``` r
summary(Psych24)
```

    ##  Visual.perception     Cubes        Paper.form.board      Flags       
    ##  Min.   :0.1160    Min.   :0.0050   Min.   :-0.0750   Min.   :0.0660  
    ##  1st Qu.:0.2985    1st Qu.:0.1487   1st Qu.: 0.1770   1st Qu.:0.1802  
    ##  Median :0.3290    Median :0.2310   Median : 0.2485   Median :0.3135  
    ##  Mean   :0.3553    Mean   :0.2396   Mean   : 0.2629   Mean   :0.2935  
    ##  3rd Qu.:0.4055    3rd Qu.:0.2868   3rd Qu.: 0.3132   3rd Qu.:0.3360  
    ##  Max.   :1.0000    Max.   :1.0000   Max.   : 1.0000   Max.   :1.0000  
    ##  General.information Paragraph.comprehension Sentence.completion
    ##  Min.   :0.1870      Min.   :0.0950          Min.   :0.1570     
    ##  1st Qu.:0.2425      1st Qu.:0.2600          1st Qu.:0.2253     
    ##  Median :0.3195      Median :0.3180          Median :0.3090     
    ##  Mean   :0.3851      Mean   :0.3826          Mean   :0.3712     
    ##  3rd Qu.:0.4365      3rd Qu.:0.4335          3rd Qu.:0.4405     
    ##  Max.   :1.0000      Max.   :1.0000          Max.   :1.0000     
    ##  Word.classification  Word.meaning       Addition            Code       
    ##  Min.   :0.1570      Min.   :0.1130   Min.   :-0.0750   Min.   :0.0910  
    ##  1st Qu.:0.2732      1st Qu.:0.2465   1st Qu.: 0.1585   1st Qu.:0.2482  
    ##  Median :0.3595      Median :0.2800   Median : 0.2245   Median :0.3110  
    ##  Mean   :0.3877      Mean   :0.3815   Mean   : 0.2777   Mean   :0.3370  
    ##  3rd Qu.:0.4455      3rd Qu.:0.4883   3rd Qu.: 0.3390   3rd Qu.:0.3713  
    ##  Max.   :1.0000      Max.   :1.0000   Max.   : 1.0000   Max.   :1.0000  
    ##  Counting.dots    Straight.curved.capitals Word.recognition
    ##  Min.   :0.0950   Min.   :0.1390           Min.   :0.0660  
    ##  1st Qu.:0.1437   1st Qu.:0.2750           1st Qu.:0.1815  
    ##  Median :0.2305   Median :0.3250           Median :0.2425  
    ##  Mean   :0.2865   Mean   :0.3577           Mean   :0.2685  
    ##  3rd Qu.:0.3513   3rd Qu.:0.3982           3rd Qu.:0.3025  
    ##  Max.   :1.0000   Max.   :1.0000           Max.   :1.0000  
    ##  Number.recognition Figure.recognition Object.number    Number.figure   
    ##  Min.   :0.0650     Min.   :0.1190     Min.   :0.0050   Min.   :0.1590  
    ##  1st Qu.:0.1703     1st Qu.:0.2610     1st Qu.:0.2045   1st Qu.:0.2507  
    ##  Median :0.2350     Median :0.2935     Median :0.2735   Median :0.3170  
    ##  Mean   :0.2533     Mean   :0.3170     Mean   :0.2920   Mean   :0.3253  
    ##  3rd Qu.:0.2600     3rd Qu.:0.3450     3rd Qu.:0.3245   3rd Qu.:0.3518  
    ##  Max.   :1.0000     Max.   :1.0000     Max.   :1.0000   Max.   :1.0000  
    ##   Figure.word       Deduction      Numerical.puzzles Problem.reasoning
    ##  Min.   :0.1100   Min.   :0.1670   Min.   :0.1650    Min.   :0.1600   
    ##  1st Qu.:0.1915   1st Qu.:0.2695   1st Qu.:0.2960    1st Qu.:0.2672   
    ##  Median :0.2665   Median :0.3520   Median :0.3520    Median :0.3495   
    ##  Mean   :0.2814   Mean   :0.3648   Mean   :0.3586    Mean   :0.3622   
    ##  3rd Qu.:0.3150   3rd Qu.:0.4290   3rd Qu.:0.4005    3rd Qu.:0.4002   
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000    Max.   :1.0000   
    ##  Series.completion Arithmetic.problems
    ##  Min.   :0.2420    Min.   :0.1650     
    ##  1st Qu.:0.2990    1st Qu.:0.2985     
    ##  Median :0.3825    Median :0.3815     
    ##  Mean   :0.4032    Mean   :0.3842     
    ##  3rd Qu.:0.4567    3rd Qu.:0.4263     
    ##  Max.   :1.0000    Max.   :1.0000

### more operations within dplyr family

``` r
Psych24_test <- Psych24 %>%
  mutate(Gender = case_when(row_number() <= 12 ~ "F", 
                            TRUE ~ "M")) %>%
  group_by(Gender) %>%
  summarise(mean_flags = mean(Flags, na.rm = TRUE)) 
  # summarise_at(vars(Cubes:Code), mean, na.rm = TRUE)
  # summarise_all(mean, na.rm = TRUE)
```

### transform wide data into tall data

``` r
EmpTest_melt <- EmpTest %>%
  gather(key = 'variable', value = 'value', -Number, -new_col, -new_col2)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

### look at unique observations within column

``` r
unique(EmpTest_melt$value)
```

    ## [1] "Strongly Agree"    "Somewhat Agree"    "Agree"            
    ## [4] "Disagree"          "Somewhat Disagree" "Strongly Disagree"

### recode 'value' to numeric values

``` r
EmpTest_melt %<>%
  mutate(value = case_when(value == "Strongly Disagree" ~ 1, 
                           value == "Disagree" ~ 2, 
                           value == "Somewhat Disagree" ~ 3,
                           value == "Somewhat Agree" ~ 4, 
                           value == "Agree" ~ 5, 
                           value == "Strongly Agree" ~ 6, 
                           TRUE ~ NA_real_))
```

### look at unique observation within column to make sure recode worked

``` r
unique(EmpTest_melt$value)
```

    ## [1] 6 4 5 2 3 1

### spread dataframe back from tall to wide

``` r
EmpTest_cast <- EmpTest_melt %>%
  spread(key = variable, value = value)
```

### putting it all together in one code chunk

``` r
EmpTest2 <- EmpTest %>%
  gather(key = 'variable', value = 'value', -Number, -new_col, -new_col2) %>%
  mutate(value = case_when(value == "Strongly Disagree" ~ 1, 
                           value == "Disagree" ~ 2, 
                           value == "Somewhat Disagree" ~ 3,
                           value == "Somewhat Agree" ~ 4, 
                           value == "Agree" ~ 5, 
                           value == "Strongly Agree" ~ 6, 
                           TRUE ~ NA_real_)) %>%
      spread(key = variable, value = value)
```

stringr
-------

### trim white space, function 'trim\_ws' works from base r just as well

``` r
y <- c("abcd ", " abcde ")

str_trim(y, "right")
```

    ## [1] "abcd"   " abcde"

``` r
str_trim(y, "both")
```

    ## [1] "abcd"  "abcde"

### string wrapping

``` r
jabberwocky <- str_c(
  "`Twas brillig, and the slithy toves ",
  "did gyre and gimble in the wabe: ",
  "All mimsy were the borogoves, ",
  "and the mome raths outgrabe. "
)

cat(jabberwocky)
```

    ## `Twas brillig, and the slithy toves did gyre and gimble in the wabe: All mimsy were the borogoves, and the mome raths outgrabe.

``` r
cat(str_wrap(jabberwocky, width = 40))
```

    ## `Twas brillig, and the slithy toves did
    ## gyre and gimble in the wabe: All mimsy
    ## were the borogoves, and the mome raths
    ## outgrabe.

### Change case of text

``` r
x <- "I like horses."
str_to_upper(x)
```

    ## [1] "I LIKE HORSES."

``` r
str_to_title(x)
```

    ## [1] "I Like Horses."

``` r
str_to_lower(x)
```

    ## [1] "i like horses."

``` r
str_to_lower(x, "tr")
```

    ## [1] "i like horses."

### Regex examples

``` r
##Replacing spaces with underscores
names(EmpData) <- gsub(x = names(EmpData),
                    pattern = " ",
                    replacement = "_")

##Replace ":" with "_" in variable names
names(EmpData) <- gsub(x = names(EmpData),
                    pattern = "\\:",
                    replacement = "_")
```

### Other pattern matching

``` r
strings <- c(
  "apple", 
  "219 733 8965", 
  "329-293-8753", 
  "Work: 579-499-7527; Home: 543.355.3679"
)
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
```

#### Which strings contain phone numbers?

``` r
str_detect(strings, phone)
```

    ## [1] FALSE  TRUE  TRUE  TRUE

``` r
str_subset(strings, phone)
```

    ## [1] "219 733 8965"                          
    ## [2] "329-293-8753"                          
    ## [3] "Work: 579-499-7527; Home: 543.355.3679"

#### How many phone numbers in each string?

``` r
str_count(strings, phone)
```

    ## [1] 0 1 1 2

#### What are the phone numbers?

``` r
str_extract(strings, phone)
```

    ## [1] NA             "219 733 8965" "329-293-8753" "579-499-7527"

``` r
str_extract_all(strings, phone)
```

    ## [[1]]
    ## character(0)
    ## 
    ## [[2]]
    ## [1] "219 733 8965"
    ## 
    ## [[3]]
    ## [1] "329-293-8753"
    ## 
    ## [[4]]
    ## [1] "579-499-7527" "543.355.3679"

#### Pull out the three components of the match

``` r
str_match(strings, phone)
```

    ##      [,1]           [,2]  [,3]  [,4]  
    ## [1,] NA             NA    NA    NA    
    ## [2,] "219 733 8965" "219" "733" "8965"
    ## [3,] "329-293-8753" "329" "293" "8753"
    ## [4,] "579-499-7527" "579" "499" "7527"

``` r
str_replace(strings, phone, "XXX-XXX-XXXX")
```

    ## [1] "apple"                                 
    ## [2] "XXX-XXX-XXXX"                          
    ## [3] "XXX-XXX-XXXX"                          
    ## [4] "Work: XXX-XXX-XXXX; Home: 543.355.3679"

``` r
str_replace_all(strings, phone, "XXX-XXX-XXXX")
```

    ## [1] "apple"                                 
    ## [2] "XXX-XXX-XXXX"                          
    ## [3] "XXX-XXX-XXXX"                          
    ## [4] "Work: XXX-XXX-XXXX; Home: XXX-XXX-XXXX"

dplyr, forcats, & ggplot2
-------------------------

### full join two datasets

``` r
EmpData %<>%
  select(Number, Division)

Emp_Merge <- full_join(EmpData,EmpTest2,by = "Number")
```

### the first way I thought about doing it (brute force)

``` r
Emp_Merge_Sum <- Emp_Merge %>%
  select(Division, ECS_Q1:ECS_Q6) %>%
  group_by(Division) %>%
    mutate(mean = round(mean(c(ECS_Q1,ECS_Q2, ECS_Q3, ECS_Q4, ECS_Q5, ECS_Q6), na.rm = TRUE),2)) %>%
  distinct(Division, .keep_all = TRUE) %>%
  select(Division, mean)
```

### a more elegant way

``` r
Emp_Merge_Sum <- Emp_Merge %>%
  select(Division, ECS_Q1:ECS_Q6) %>%
  gather(variable, value, -Division) %>%
  group_by(Division) %>%
  summarize(mean = round(mean(value, na.rm = TRUE),2))
```

### Visualize

#### Create a pretty theme

``` r
windowsFonts(Calibri=windowsFont("Calibri"))

Hor_Bar_LightBlue_Theme <-theme(
text = element_text(family = "Calibri"),
axis.title.x = element_blank(),
axis.title.y = element_blank(),
axis.line = element_blank(),
#panel.border = element_blank(),
#panel.grid=element_blank(),
axis.ticks = element_blank(),
plot.title=element_text(size=20, face="bold",hjust=0.6),
plot.subtitle=element_text(size=15,face=c("bold","italic"),hjust=0.6),
axis.text.x=element_text(size=15),#element_blank(),
axis.text.y=element_text(size=20),
legend.position = "none",
panel.spacing=unit(2,"cm"),
    panel.background = element_rect(fill = "white",colour = NA), # or element_blank()
    panel.grid.minor = element_line(color = "gray90", size = 0.20),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.20),
    panel.grid.major.y = element_blank()#removes horizontal lines
    # plot.background = element_rect(fill = "transparent",colour = NA)
)
```

#### Horizontal bar chart

``` r
Emp_Merge_Sum_Plot <- ggplot(Emp_Merge_Sum, aes(Division, y=mean, fill=Division)) +
  geom_bar(stat='identity') + #Light Blue
  geom_text(aes(x=Division, y=mean, label=sprintf("%0.2f", round(mean, digits = 2))),hjust = -0.1, color="#4D4D4D", size = 6, fontface = "bold") + 
  Hor_Bar_LightBlue_Theme + 
  coord_flip(ylim = c(1,6)) + scale_y_continuous(breaks=seq(1,6,1), position = "right") #Add this to make 1,2,3,4,5,6 appear on axis 
```

#### print chart

``` r
print(Emp_Merge_Sum_Plot)
```

![](tidyverse_presentation_files/figure-markdown_github/unnamed-chunk-25-1.png)
