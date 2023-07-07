Project 2
================
Spencer Williams & Stephen Macropoulos
2023-07-09

- <a href="#introduction" id="toc-introduction">Introduction</a>
- <a href="#purpose-and-methods" id="toc-purpose-and-methods">Purpose and
  Methods</a>
- <a href="#reading-in-data" id="toc-reading-in-data">Reading in Data</a>

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
# Replace any missing values
newsPop$data_channel <- replace_na(newsPop$data_channel, "Other")
# Make data_channel a factor variable
newsPop$data_channel <- as.factor(newsPop$data_channel)
head(newsPop)
```

    ##   n_tokens_title n_unique_tokens num_imgs num_videos num_keywords
    ## 1             12       0.6635945        1          0            5
    ## 2              9       0.6047431        1          0            4
    ## 3              9       0.5751295        1          0            6
    ## 4              9       0.5037879        1          0            7
    ## 5             13       0.4156456       20          0            7
    ## 6             10       0.5598886        0          0            9
    ##   data_channel_is_lifestyle data_channel_is_entertainment data_channel_is_bus
    ## 1                         0                             1                   0
    ## 2                         0                             0                   1
    ## 3                         0                             0                   1
    ## 4                         0                             1                   0
    ## 5                         0                             0                   0
    ## 6                         0                             0                   0
    ##   data_channel_is_socmed data_channel_is_tech data_channel_is_world
    ## 1                      0                    0                     0
    ## 2                      0                    0                     0
    ## 3                      0                    0                     0
    ## 4                      0                    0                     0
    ## 5                      0                    1                     0
    ## 6                      0                    1                     0
    ##   rate_positive_words rate_negative_words shares  data_channel
    ## 1           0.7692308           0.2307692    593 Entertainment
    ## 2           0.7333333           0.2666667    711      Business
    ## 3           0.8571429           0.1428571   1500      Business
    ## 4           0.6666667           0.3333333   1200 Entertainment
    ## 5           0.8602151           0.1397849    505          Tech
    ## 6           0.5238095           0.4761905    855          Tech

Since we have added a new `data_channel` variable with the appropriate
variables, the data_channel_is\* variables can be removed from our data
set.

``` r
newsPop <- newsPop[ , -c(6:11)]
head(newsPop)
```

    ##   n_tokens_title n_unique_tokens num_imgs num_videos num_keywords
    ## 1             12       0.6635945        1          0            5
    ## 2              9       0.6047431        1          0            4
    ## 3              9       0.5751295        1          0            6
    ## 4              9       0.5037879        1          0            7
    ## 5             13       0.4156456       20          0            7
    ## 6             10       0.5598886        0          0            9
    ##   rate_positive_words rate_negative_words shares  data_channel
    ## 1           0.7692308           0.2307692    593 Entertainment
    ## 2           0.7333333           0.2666667    711      Business
    ## 3           0.8571429           0.1428571   1500      Business
    ## 4           0.6666667           0.3333333   1200 Entertainment
    ## 5           0.8602151           0.1397849    505          Tech
    ## 6           0.5238095           0.4761905    855          Tech
