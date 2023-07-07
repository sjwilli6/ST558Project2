Project 2
================
Spencer Williams & Stephen Macropoulos
2023-07-09

- <a href="#introduction" id="toc-introduction">Introduction</a>
- <a href="#purpose-and-methods" id="toc-purpose-and-methods">Purpose and
  Methods</a>
- <a href="#reading-in-data" id="toc-reading-in-data">Reading in Data</a>
  - <a href="#splitting-the-data" id="toc-splitting-the-data">Splitting the
    Data</a>
- <a href="#summarizations" id="toc-summarizations">Summarizations</a>
- <a href="#modeling" id="toc-modeling">Modeling</a>
- <a href="#section" id="toc-section"></a>
- <a href="#section-1" id="toc-section-1"></a>
- <a href="#section-2" id="toc-section-2"></a>

# Introduction

The (Online News
Popularity)\[<https://archive.ics.uci.edu/dataset/332/online+news+popularity>\]
is a data set with a heterogeneous set of features about articles
published by (Mashable)\[www.mashable.com\]. Multivariate data was
gathered on sixty-one variables over a two year span. Our end result is
to predict the number of shares in social networks. Below are some of
the variables we will be looking at to help our prediction.

`Shares` - Number of shares (target) `n_tokens_title` - Number of words
in the title `n_unique_tokens` - Rate of unique words in the content
`num_imgs` - Number of images `num_videos` - Number of videos
`num_keywords` - Number of keywords in the metadata
`data_channel_is *` - There are six binary variables that will only be
attributed to one variable. Theses include lifestyle, entertainment,
business, social media, tech, and world. `rate_positive_words` - Rate of
positive words among non-neutral `rate_negative_words` - Rate of
negative words among non-neutral

# Purpose and Methods

Our end goal is to be able to predict the number of shares based on
having data from the eight variables listed above. We are going to split
the data set into two sets: training (70%) and test (30%). (Linear
Regression Models)\[<https://en.wikipedia.org/wiki/Linear_regression>\]
and (Ensemble Tree-Based
Models)\[<https://towardsdatascience.com/decision-trees-understanding-the-basis-of-ensemble-methods-e075d5bfa704>\]
will be utilized to help us predict the total number of shares. *Random
Forest Models* and *Boosted Tree Models* will be chosen using
cross-validation.

# Reading in Data

The read.csv() file name will change depending on who is importing the
Online News Popularity data. We have dropped any unneccessary variables
that will not be used to help us in our predictions.

``` r
# Will need to change this depending on who is working!
newsPop <- read.csv("/Users/monicabeingolea/Documents/ST558/OnlineNewsPopularity/OnlineNewsPopularity.csv")
# Only selecting the columns of interest
newsPop <- newsPop[ , c(3,5,10,11,13,14:19,49,50,61)]
# Check for missing values
sum(is.na(newsPop))
```

    ## [1] 0

We want to subset the data to work based on the different data channel
of interest. Creating a new variable called `data_channel` will allow
this to work successfully. This way, we can turn our focus to a singular
column as opposed to having six binary variables. We will use the
`mutate` function in the *tidyverse* package. Replacing NA’s in the
`data_channel` variable and setting it as a factor is very important in
order to help us predict the total shares.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# Create new variable data_channel
newsPop <- newsPop %>% mutate(data_channel = case_when(data_channel_is_bus == 1 ~ "Business", data_channel_is_entertainment == 1 ~ "Entertainment", data_channel_is_lifestyle == 1 ~ "Lifestyle", data_channel_is_socmed == 1 ~ "SocialMedia", data_channel_is_tech == 1 ~ "Tech", data_channel_is_world == 1 ~ "World"))
# Replace any missing values with "Miscellaneous"
newsPop$data_channel <- replace_na(newsPop$data_channel, "Miscellaneous")
# Make data_channel a factor variable
newsPop$data_channel <- as.factor(newsPop$data_channel)
```

Since we have added a new `data_channel` variable with the appropriate
variables, the data_channel_is\* variables can be removed from our data
set.

``` r
# Remove data_channel_is*
newsPop <- newsPop[ , -c(6:11)]
```

### Splitting the Data

``` r
# Set seed
set.seed(5432)
# split data into test and training sets
sub <- sample(1:nrow(newsPop), 0.7 * nrow(newsPop))
newsPopTrain <- newsPop[sub, ]
newsPopTest <- newsPop[-sub, ]
```

# Summarizations

We wanted to see the summary statistics of each variable that we are
using to predict the number of shares. The statistics will include the
minimum, maximum, mean, median, and quartiles.

``` r
summary(newsPopTrain)
```

    ##  n_tokens_title n_unique_tokens     num_imgs         num_videos    
    ##  Min.   : 2.0   Min.   :0.0000   Min.   :  0.000   Min.   : 0.000  
    ##  1st Qu.: 9.0   1st Qu.:0.4708   1st Qu.:  1.000   1st Qu.: 0.000  
    ##  Median :10.0   Median :0.5389   Median :  1.000   Median : 0.000  
    ##  Mean   :10.4   Mean   :0.5302   Mean   :  4.557   Mean   : 1.244  
    ##  3rd Qu.:12.0   3rd Qu.:0.6082   3rd Qu.:  4.000   3rd Qu.: 1.000  
    ##  Max.   :23.0   Max.   :1.0000   Max.   :128.000   Max.   :75.000  
    ##                                                                    
    ##   num_keywords    rate_positive_words rate_negative_words     shares      
    ##  Min.   : 1.000   Min.   :0.0000      Min.   :0.0000      Min.   :     4  
    ##  1st Qu.: 6.000   1st Qu.:0.6000      1st Qu.:0.1848      1st Qu.:   947  
    ##  Median : 7.000   Median :0.7143      Median :0.2778      Median :  1400  
    ##  Mean   : 7.237   Mean   :0.6828      Mean   :0.2870      Mean   :  3425  
    ##  3rd Qu.: 9.000   3rd Qu.:0.8000      3rd Qu.:0.3824      3rd Qu.:  2800  
    ##  Max.   :10.000   Max.   :1.0000      Max.   :1.0000      Max.   :843300  
    ##                                                                           
    ##         data_channel 
    ##  Business     :4381  
    ##  Entertainment:4949  
    ##  Lifestyle    :1433  
    ##  Miscellaneous:4273  
    ##  SocialMedia  :1634  
    ##  Tech         :5231  
    ##  World        :5849

Another thing that we wanted to look at was the number of shares for
each data channel. One way to look at this is using a number summary to
compare the means.

``` r
# Number summary
tapply(newsPopTrain$shares, newsPopTrain$data_channel, summary)
```

    ## $Business
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      22     955    1400    3069    2500  690400 
    ## 
    ## $Entertainment
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      49     838    1200    2990    2100  210300 
    ## 
    ## $Lifestyle
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      78    1100    1700    3794    3300  208300 
    ## 
    ## $Miscellaneous
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       4    1100    1900    6116    4600  843300 
    ## 
    ## $SocialMedia
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       5    1325    2100    3727    3800  122800 
    ## 
    ## $Tech
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      36    1100    1700    2988    3000   96100 
    ## 
    ## $World
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41     822    1100    2310    1800  284700

Based on the summary from the training data set, the Miscellaneous
channel actually had the highest mean at 6,116 shares, but this is most
likely due to the outlier with a total of 843,300 shares. Out of the
other six shares listed, Lifestyle and Social Media are the highest with
total shares in the 3,700’s. The World data channel has the lowest total
share count at 2,310. Below is a barplot and box and whisker plot to
help show these results in a graphical form.

``` r
# Creating base for graph
g <- ggplot(newsPopTrain, aes(x = data_channel, y = shares))
# Adding bars to the graph
g + stat_summary(fun = "mean", geom = "bar", color = "blue", fill = "blue") +
  # Creating labels and titles for graph
  labs(x = "Data Channel", y = "Shares", title = "Shares per Data Channel")
```

![](ST558Project2_files/figure-gfm/plot1-1.png)<!-- -->

``` r
g <- ggplot(newsPopTrain, aes(x = data_channel, y = shares))
g + geom_boxplot(color = "green") +
  ylim(0, 10000) +
  labs(x = "Data Channel", y = "Shares", title = "Shares per Data Channel")
```

    ## Warning: Removed 1520 rows containing non-finite values (`stat_boxplot()`).

![](ST558Project2_files/figure-gfm/plot2-1.png)<!-- -->

We are curious to see if the variables that we have selected have any
correlation. In order to check this, a correlation plot has been
created.

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
newsPopTrain1 <- newsPopTrain[ , -c(9)]
newsPopTrainCorr <- cor(newsPopTrain1)
corrplot(newsPopTrainCorr, type="upper", method="number", tl.pos="lt", number.cex=0.5)
corrplot(newsPopTrainCorr, type="lower", add=TRUE, tl.pos="n", number.cex=0.5)
```

![](ST558Project2_files/figure-gfm/corr-1.png)<!-- -->

Based on the correlation plots, none of the variables seem to be highly
correlated (near 1). This is good news in our case to predict the number
of shares.

# Modeling

The first linear regression model will consist of the predictive
variables that we have chosen. After looking at the significance level
of each variable, our second linear regression model will be selected.

``` r
model1 <- lm(shares ~ ., data = newsPopTrain) 
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ ., data = newsPopTrain)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -8144  -2189  -1477   -412 836939 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               1787.873    662.102   2.700 0.006932 ** 
    ## n_tokens_title             100.359     35.322   2.841 0.004497 ** 
    ## n_unique_tokens             97.017    817.220   0.119 0.905502    
    ## num_imgs                    34.543      9.932   3.478 0.000506 ***
    ## num_videos                  29.955     18.758   1.597 0.110293    
    ## num_keywords                68.251     40.009   1.706 0.088036 .  
    ## rate_positive_words       -289.139    670.582  -0.431 0.666344    
    ## rate_negative_words       -460.933    729.734  -0.632 0.527624    
    ## data_channelEntertainment -390.079    263.920  -1.478 0.139414    
    ## data_channelLifestyle      572.444    378.988   1.510 0.130939    
    ## data_channelMiscellaneous 2690.380    288.958   9.311  < 2e-16 ***
    ## data_channelSocialMedia    619.146    355.940   1.739 0.081963 .  
    ## data_channelTech          -239.925    255.971  -0.937 0.348604    
    ## data_channelWorld         -866.662    251.906  -3.440 0.000582 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12210 on 27736 degrees of freedom
    ## Multiple R-squared:  0.0108, Adjusted R-squared:  0.01033 
    ## F-statistic: 23.28 on 13 and 27736 DF,  p-value: < 2.2e-16

The model seems to be significant at predicting the number of shares
with an F-statistic of 23.28, corresponding to a small p-value. Out of
all the variables used for prediction, only 5 variables are significant.
Therefore, our next linear regression model will focus on these
significant variables.

# 

# 

# 

We are going to analyze the (random forest
model)\[<https://towardsdatascience.com/understanding-random-forest-58381e0602d2>\].
This model allows a user to combine multiple trees from bootstrap
samples. In most cases, the bagged trees predictions are more correlated
which will result in a smaller reduction in variance from aggregation.

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
newsPopFit_rf <- randomForest(shares ~ ., data = newsPopTrain, mtry = ncol(newsPopTrain)/3, ntree=200, importance=TRUE)
newsPopPred_rf <- predict(newsPopFit_rf, newdata = newsPopTest)
summary(newsPopPred_rf)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##    742.9   2028.3   2863.5   3620.8   4269.1 108866.8

``` r
newsPopPred_model1 <- predict(model1, newdata = newsPopTest)
summary(newsPopPred_model1)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1375    2606    3019    3432    3670   72549

``` r
RMSE_rf <- sqrt(mean((newsPopPred_rf-newsPopTest$shares)^2))
RMSE_rf
```

    ## [1] 10152.29

``` r
model1_RMSE <- sqrt(mean((newsPopPred_model1-newsPopTest$shares)^2))
model1_RMSE
```

    ## [1] 9903.451
