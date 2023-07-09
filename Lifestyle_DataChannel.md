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

# Introduction

The (Online News
Popularity)\[<https://archive.ics.uci.edu/dataset/332/online+news+popularity>\]
is a data set with a heterogeneous set of features about articles
published by (Mashable)\[www.mashable.com\]. Multivariate data was
gathered on sixty-one variables over a two year span. Our end goal is to
predict the number of shares in social networks. Below are some of the
variables we will be looking at to help our prediction.

`Shares` - Number of shares (target)  
`n_tokens_title` - Number of words in the title  
`n_unique_tokens` - Rate of unique words in the content  
`num_imgs` - Number of images  
`num_videos` - Number of videos  
`num_keywords` - Number of keywords in the metadata  
`data_channel_is *` - There are six binary variables which will be
combined into one column. Theses include lifestyle, entertainment,
business, social media, tech, and world  
`rate_positive_words` - Rate of positive words among non-neutral  
`rate_negative_words` - Rate of negative words among non-neutral

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

The `read.csv()` filename will change depending on who is importing the
Online News Popularity data. We have dropped any unnecessary variables
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
# Create new variable data_channel
newsPop <- newsPop %>% mutate(data_channel = case_when(data_channel_is_bus == 1 ~ "Business", data_channel_is_entertainment == 1 ~ "Entertainment", data_channel_is_lifestyle == 1 ~ "Lifestyle", data_channel_is_socmed == 1 ~ "SocialMedia", data_channel_is_tech == 1 ~ "Tech", data_channel_is_world == 1 ~ "World"))
# Replace any missing values with "Miscellaneous"
newsPop$data_channel <- replace_na(newsPop$data_channel, "Miscellaneous")
# Make data_channel a factor variable
newsPop$data_channel <- as.factor(newsPop$data_channel)
```

Since we have added a new `data_channel` variable with the appropriate
variables, the data_channel_is\_\* variables can be removed from our
data set. We will also subset the data to include only observations with
the data channel we want. The possible choices are Entertainment,
SocialMedia, Tech, Business, Miscellaneous, World, and Lifestyle.

``` r
datachannel <- "Lifestyle"

# Remove data_channel_is*
newsPop <- newsPop[, -c(6:11)]
newsPop1 <- newsPop[newsPop$data_channel==datachannel, ]

newsPop1 <- newsPop1[,-9]
```

## Splitting the Data

``` r
# Set seed
set.seed(5432)
# split data into test and training sets
sub <- sample(1:nrow(newsPop1), 0.7 * nrow(newsPop1))

# for full data set
newsPopTrain <- newsPop[sub,]
newsPopTest <- newsPop[-sub, ]

# for Entertainment data channel
newsPop1Train <- newsPop1[sub, ]
newsPop1Test <- newsPop1[-sub, ]
```

# Summarizations

We wanted to see the summary statistics of each variable that we are
using to predict the number of shares. The statistics will include the
minimum, maximum, mean, median, and quartiles.

``` r
# Summary
summary(newsPop1Train)
```

    ##  n_tokens_title   n_unique_tokens     num_imgs        num_videos     
    ##  Min.   : 5.000   Min.   :0.0000   Min.   :  0.00   Min.   : 0.0000  
    ##  1st Qu.: 8.000   1st Qu.:0.4644   1st Qu.:  1.00   1st Qu.: 0.0000  
    ##  Median :10.000   Median :0.5207   Median :  1.00   Median : 0.0000  
    ##  Mean   : 9.788   Mean   :0.5235   Mean   :  4.98   Mean   : 0.5044  
    ##  3rd Qu.:11.000   3rd Qu.:0.5915   3rd Qu.:  8.00   3rd Qu.: 0.0000  
    ##  Max.   :18.000   Max.   :0.8382   Max.   :111.00   Max.   :50.0000  
    ##   num_keywords    rate_positive_words rate_negative_words     shares      
    ##  Min.   : 3.000   Min.   :0.0000      Min.   :0.0000      Min.   :    28  
    ##  1st Qu.: 7.000   1st Qu.:0.6627      1st Qu.:0.1818      1st Qu.:  1100  
    ##  Median : 8.000   Median :0.7391      Median :0.2545      Median :  1700  
    ##  Mean   : 8.246   Mean   :0.7208      Mean   :0.2662      Mean   :  3771  
    ##  3rd Qu.:10.000   3rd Qu.:0.8125      3rd Qu.:0.3333      3rd Qu.:  3200  
    ##  Max.   :10.000   Max.   :1.0000      Max.   :1.0000      Max.   :208300

It looks like the `n_unique_tokens`, `num_imgs`, `num_videos`, and
`shares` variables are quite right-skewed in our training set.

Another thing that we wanted to look at was the number of shares for
each data channel. One way to look at this is using a number summary to
compare the means.

``` r
# Number summary
tapply(newsPopTrain$shares, newsPopTrain$data_channel, summary)
```

    ## $Business
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     318     817    1200    1855    2025   16700 
    ## 
    ## $Entertainment
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     294     765    1200    2297    2050   39400 
    ## 
    ## $Lifestyle
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     343    1100    1600    2606    2600   18100 
    ## 
    ## $Miscellaneous
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     341    1100    1700    2904    2925   30400 
    ## 
    ## $SocialMedia
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     348    1375    2450    3956    4400   51900 
    ## 
    ## $Tech
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     217    1100    1700    2626    3000   17100 
    ## 
    ## $World
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   300.0   736.5  1250.0  1932.0  2200.0 25200.0

Based on the summary from the training data set, the Miscellaneous
channel actually had the highest mean at 4,188 shares, but this is most
likely due to the outlier with a total of 112,500 shares. Out of the
other six shares listed, Business and Social Media are the highest with
total shares in the 3,600’s. The World data channel has the lowest total
share count at 2,311. Below is a barplot and box and whisker plot to
help show these results in a graphical form.

Now let’s take a look at some contingency tables. First, we group the
shares values by thousands in a new column called `sharesgroups` and add
it to the data set.

``` r
sharesgroups <- numeric()

for (i in 1:length(newsPop1Train$shares)) {
  sharesgroups[i] <- floor(newsPop1Train$shares[i]/1000)
}

#head(sharesgroups,100)

newsPop1Train <- cbind(newsPop1Train,sharesgroups)
```

Now let’s see the contingency table for `sharesgroups`.

``` r
table(newsPop1Train$sharesgroups)
```

    ## 
    ##   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19 
    ## 303 542 212 107  61  48  39  28  14  12   8  14  13   8   4   6   9   2   5   2 
    ##  20  21  22  23  24  25  26  29  32  33  34  35  36  41  43  45  54  56  73 139 
    ##   3   3   1   3   2   1   1   2   2   1   1   1   1   2   1   1   1   1   1   1 
    ## 196 208 
    ##   1   1

We see that most of the observations had less than 20,000 shares. There
are larger jumps in thousands of shares once we get to around 70,000.

Now let’s take a look at a contingency table for the data channels.

``` r
table(newsPopTrain$data_channel)
```

    ## 
    ##      Business Entertainment     Lifestyle Miscellaneous   SocialMedia 
    ##           248           215           133           220           128 
    ##          Tech         World 
    ##           311           214

We see that the Business, Entertainment, Tech, and World data channels
had more observations overall than the Lifestyle and Social Media data
channels.

Now let’s look at the shares totals for each of the data channels in a
bar plot.

``` r
# Creating base for graph
g <- ggplot(newsPopTrain, aes(x = data_channel, y = shares))
# Adding bars to the graph
g + stat_summary(fun = "mean", geom = "bar", color = "blue", fill = "blue") +
  # Creating labels and titles for graph
  labs(x = "Data Channel", y = "Shares", title = "Shares per Data Channel")
```

![](ST558Project2_files/figure-gfm/plot1-1.png)<!-- -->

It appears that the total number of shares for each data channel are
similar except for the Miscellaneous category. The Business and Social
Media data channels had the most total shares while the World data
channel had the least.

Now let’s see the boxplots to better understand the variability in those
share totals.

``` r
# Creating base for graph
g <- ggplot(newsPopTrain, aes(x = data_channel, y = shares))
# Adding boxplot to the graph
g + geom_boxplot(color = "green") +
  # Setting y-axis limit and labels
  ylim(0, 10000) +
  labs(x = "Data Channel", y = "Shares", title = "Shares per Data Channel")
```

![](ST558Project2_files/figure-gfm/plot2-1.png)<!-- -->

It looks like the Social Media data channel had the highest median
shares while the Entertainment and World data channels had the smallest
median shares.

We are curious to see if the variables that we have selected have any
correlation between them. In order to check this, a correlation plot has
been created.

``` r
# Load library
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
# Remove non-numeric variable
newsPopTrain_ <- newsPopTrain[ , -9]
# Find the correlation and plot the graph
newsPopTrainCorr <- cor(newsPopTrain_)
corrplot(newsPopTrainCorr, type="upper", method="number", tl.pos="lt", number.cex=0.5)
corrplot(newsPopTrainCorr, type="lower", add=TRUE, tl.pos="n", number.cex=0.5)
```

![](ST558Project2_files/figure-gfm/corr-1.png)<!-- -->

Based on the correlation plots, only one of the variable-pairs seems to
be highly correlated (near 1 in magnitude). The strongest negative
correlation is -0.85 between `rate_positive_words` and
`rate_negative_words`. This is good news in our case to predict the
number of shares.

Let’s also check out some scatterplots for the Entertainment training
set. First, we look at `shares` vs `n_tokens_title`.

``` r
g <- ggplot(newsPop1Train, aes(x = n_tokens_title, y = shares))
g + labs(title = "Shares vs Words in Title") +
  geom_point(alpha = 0.6, size = 2, position = "jitter") 
```

![](ST558Project2_files/figure-gfm/scatter1-1.png)<!-- -->

We can inspect the trend of shares as a function of the number of words
in the title. If the points show an upward trend, then articles with
more words in the title tend to be shared more often. If we see a
negative trend then articles with more words tend to be shared less
often.

Now let’s look at the same plot but for the `n_unique_tokens` variable.
We remove the extreme outlier first.

``` r
newsPop1Train <- newsPop1Train[-915,]

g <- ggplot(newsPop1Train, aes(x = n_unique_tokens, y = shares))
g + labs(title = "Shares vs Unique Words") +
  geom_point(alpha = 0.6, size = 2, position = "jitter") 
```

![](ST558Project2_files/figure-gfm/scatter2-1.png)<!-- -->

We can inspect the trend of shares as a function of the number of unique
words in the content. If the points show an upward trend, then articles
with more unique words in the title tend to be shared more often. If we
see a negative trend then articles with more unique words tend to be
shared less often.

Now let’s look at the scatter plot of `shares` vs `num_imgs`.

``` r
g <- ggplot(newsPop1Train, aes(x = num_imgs, y = shares))
g + labs(title = "Shares vs Images") +
  geom_point(alpha = 0.6, size = 2, position = "jitter")
```

![](ST558Project2_files/figure-gfm/scatter3-1.png)<!-- -->

We can inspect the trend of shares as a function of the number of
images. If the points show an upward trend, then articles with more
images tend to be shared more often. If there is a negative trend, then
articles with more images tend to be shared less often.

Now let’s look at the scatter plot of `shares` vs `num_videos`.

``` r
g <- ggplot(newsPop1Train, aes(x = num_videos, y = shares))
g + labs(title = "Shares vs Videos") +
  geom_point(alpha = 0.6, size = 2, position = "jitter")
```

![](ST558Project2_files/figure-gfm/scatter4-1.png)<!-- -->

This plot looks very similar to the Shares vs Images plot!

Now let’s look at the scatter plot of `shares` vs `num_keywords`.

``` r
g <- ggplot(newsPop1Train, aes(x = num_keywords, y = shares))
g + labs(title = "Shares vs Keywords") +
  geom_point(alpha = 0.6, size = 2, position = "jitter")
```

![](ST558Project2_files/figure-gfm/scatter5-1.png)<!-- -->

We can inspect the trend of shares as a function of the number of
keywords. If the points show an upward trend, then articles with more
keywords tend to be shared more often. If we see a negative trend then
articles with more keywords tend to be shared less often.

Now let’s look at the scatter plot of `shares` vs
\`rate_positive_words\`\`.

``` r
g <- ggplot(newsPop1Train, aes(x = rate_positive_words, y = shares))
g + labs(title = "Shares vs Positive Word Rate") +
  geom_point(alpha = 0.6, size = 2, position = "jitter")
```

![](ST558Project2_files/figure-gfm/scatter6-1.png)<!-- -->

We can inspect the trend of shares as a function of the positive word
rate. If the points show an upward trend, then articles with more
positive words tend to be shared more often. If we see a negative trend
then articles with more positive words tend to be shared less often.

And finally let’s look at the scatter plot of `shares` vs
\`rate_negative_words\`\`.

``` r
g <- ggplot(newsPop1Train, aes(x = rate_negative_words, y = shares))
g + labs(title = "Shares vs Negative Word Rate") +
  geom_point(alpha = 0.6, size = 2, position = "jitter")
```

![](ST558Project2_files/figure-gfm/scatter7-1.png)<!-- -->

This plot looks like the mirror image of the shares vs positive word
rate plot!

# Modeling

We will use linear regression to investigate which variables best
predict the number of shares.

Linear regression is a statistical modeling procedure which optimally
estimates the slope parameters (via least squares) for each explanatory
variable in a pre-specified linear equation of the slopes. The
assumption of error normality is often made in order to calculate
confidence intervals and prediction intervals for the average and future
responses respectively. The reliability of this technique depends on the
accuracy of the chosen linear equation of the slope parameters. A
misspecified model equation can severely mislead inference and result in
very poor predictive power. Hence, the analyst often tries many
different model equations with the most sensible explanatory variables
for the given response until the parameter estimates are statistically
significant and the information criteria are relatively optimized.

The first linear regression model will consist of all the predictive
variables that we have chosen (omitting the `sharesgroups` variable) in
linear form. After looking at the significance level of each variable,
our second linear regression model will be selected. This has been
selected based on the results for every data channel combined, so we
will analyze each one using the same two models.

``` r
newsPop1Train <- newsPop1Train[,-9]

# Create a linear regression
model1 <- lm(shares ~ ., data = newsPop1Train) 
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ ., data = newsPop1Train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -27418  -2562  -1803   -319 204107 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4824.55    3062.71   1.575   0.1154    
    ## n_tokens_title        -88.34     135.35  -0.653   0.5141    
    ## n_unique_tokens     -1569.72    2931.93  -0.535   0.5925    
    ## num_imgs               54.68      32.34   1.691   0.0911 .  
    ## num_videos            510.20     122.70   4.158  3.4e-05 ***
    ## num_keywords          137.31     155.53   0.883   0.3775    
    ## rate_positive_words -1589.12    2753.35  -0.577   0.5639    
    ## rate_negative_words   434.37    3131.82   0.139   0.8897    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9747 on 1460 degrees of freedom
    ## Multiple R-squared:  0.01656,    Adjusted R-squared:  0.01185 
    ## F-statistic: 3.512 on 7 and 1460 DF,  p-value: 0.00096

We can use the above output to gauge the strength of this model. If the
overall p-value at the bottom is small then we can use the asterisks to
see which are the most useful predictors in this model.

``` r
# Create a linear regression

model2 <- lm(shares ~ poly(n_tokens_title,2) + poly(n_unique_tokens,2) +
               poly(num_imgs,2) + poly(num_videos,2) + poly(num_keywords,2) +
               poly(rate_positive_words,2) + poly(rate_negative_words,2), 
               data = newsPop1Train)
 
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ poly(n_tokens_title, 2) + poly(n_unique_tokens, 
    ##     2) + poly(num_imgs, 2) + poly(num_videos, 2) + poly(num_keywords, 
    ##     2) + poly(rate_positive_words, 2) + poly(rate_negative_words, 
    ##     2), data = newsPop1Train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -13938  -2598  -1671    -69 203964 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3771.1      253.8  14.859  < 2e-16 ***
    ## poly(n_tokens_title, 2)1       -5975.4     9781.5  -0.611    0.541    
    ## poly(n_tokens_title, 2)2      -11318.7     9772.5  -1.158    0.247    
    ## poly(n_unique_tokens, 2)1      -9580.0    20456.8  -0.468    0.640    
    ## poly(n_unique_tokens, 2)2      10231.9    25853.0   0.396    0.692    
    ## poly(num_imgs, 2)1             18158.8    10968.1   1.656    0.098 .  
    ## poly(num_imgs, 2)2            -10855.4    10953.9  -0.991    0.322    
    ## poly(num_videos, 2)1           41413.3     9777.3   4.236 2.42e-05 ***
    ## poly(num_videos, 2)2          -30289.1     9785.9  -3.095    0.002 ** 
    ## poly(num_keywords, 2)1          4987.7    10108.0   0.493    0.622    
    ## poly(num_keywords, 2)2         -8727.5     9800.6  -0.891    0.373    
    ## poly(rate_positive_words, 2)1 -11115.9    42709.6  -0.260    0.795    
    ## poly(rate_positive_words, 2)2 -18700.0    17604.8  -1.062    0.288    
    ## poly(rate_negative_words, 2)1  -7954.2    40673.8  -0.196    0.845    
    ## poly(rate_negative_words, 2)2       NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9724 on 1454 degrees of freedom
    ## Multiple R-squared:  0.0252, Adjusted R-squared:  0.01649 
    ## F-statistic: 2.892 on 13 and 1454 DF,  p-value: 0.0003779

We can use the above output to gauge the strength of this model. If the
overall p-value at the bottom is small then we can use the asterisks to
see which are the most useful predictors in this model.

We are going to analyze the (random forest
model)\[<https://towardsdatascience.com/understanding-random-forest-58381e0602d2>\].
This model allows a user to combine multiple trees from bootstrap
samples. In most cases, the bagged trees predictions are more correlated
which will result in a smaller reduction in variance from aggregation.
The random forest model uses a random subset of the predictors for each
bootstrap tree fit.

``` r
# Load library
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
# Produce random forest model
newsPopFit_rf <- randomForest(shares ~ ., data = newsPop1Train, 
                              mtry = ncol(newsPop1Train)/3, 
                              ntree=200, importance=TRUE)
```

We will also use a boosted tree model to predict the number of shares.

Boosted trees are a general approach that can be applied to trees. The
trees are grown sequentially, each subsequent tree is grown on a
modified version of the original data, and predictions are updated as
the trees are grown. Cross validation is also used to select the
shrinkage and depth parameters.

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
n.trees <- c(25,50,100,150,200)
interaction.depth <- 1:4
shrinkage <- 0.1
n.minobsinnode <- 10
X <- expand.grid(n.trees = n.trees, interaction.depth = interaction.depth,
            shrinkage = shrinkage, n.minobsinnode = n.minobsinnode)

newsPopFit_boost <- train(shares ~ ., data = newsPop1Train,
               method = "gbm",
               trControl = trainControl(method = "cv", number = 5),
               tuneGrid = X)
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 115933597.2616             nan     0.1000 77822.8799
    ##      2 115902955.9311             nan     0.1000 -54373.5364
    ##      3 115360790.0664             nan     0.1000 68851.9633
    ##      4 115267860.2902             nan     0.1000 49431.5843
    ##      5 114865869.6336             nan     0.1000 -178679.2768
    ##      6 114589204.5243             nan     0.1000 -198016.0874
    ##      7 114502828.3393             nan     0.1000 -52538.4533
    ##      8 114312725.1399             nan     0.1000 -234254.9562
    ##      9 114385852.7274             nan     0.1000 -313767.8253
    ##     10 114307792.9630             nan     0.1000 -26983.5206
    ##     20 113785008.6003             nan     0.1000 -142306.0358
    ##     40 113280756.5322             nan     0.1000 -416376.6270
    ##     60 113088098.9789             nan     0.1000 -431270.8165
    ##     80 113039806.2188             nan     0.1000 -633182.2726
    ##    100 112804047.3011             nan     0.1000 -304039.8820
    ##    120 112548689.4341             nan     0.1000 -321450.2864
    ##    140 112380253.4052             nan     0.1000 -274898.8519
    ##    160 112241090.7148             nan     0.1000 -529751.7917
    ##    180 112280674.4010             nan     0.1000 -411319.3569
    ##    200 112258842.9692             nan     0.1000 -257788.7771
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 115943243.5891             nan     0.1000 -13107.7115
    ##      2 115361286.9979             nan     0.1000 -61509.2704
    ##      3 114997504.1966             nan     0.1000 -47647.4493
    ##      4 114552818.2073             nan     0.1000 5465.4125
    ##      5 114155933.6504             nan     0.1000 -112189.6194
    ##      6 113880999.7396             nan     0.1000 -211825.9885
    ##      7 113932658.4902             nan     0.1000 -435099.9944
    ##      8 113706371.1434             nan     0.1000 -76302.5915
    ##      9 112960292.3042             nan     0.1000 15990.8204
    ##     10 112747610.0092             nan     0.1000 -543809.7765
    ##     20 112437798.7557             nan     0.1000 -304253.6562
    ##     40 111215736.9898             nan     0.1000 -486779.5221
    ##     60 110659857.0038             nan     0.1000 -503451.4318
    ##     80 109821758.8325             nan     0.1000 -320957.3056
    ##    100 108328994.8199             nan     0.1000 -487533.1729
    ##    120 107371558.7049             nan     0.1000 -513746.2654
    ##    140 106967376.6419             nan     0.1000 -65352.2887
    ##    160 106076582.2082             nan     0.1000 -181252.9974
    ##    180 105484020.1354             nan     0.1000 -301274.3905
    ##    200 104649945.7002             nan     0.1000 -573075.2833
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 115172227.4624             nan     0.1000 -178926.6712
    ##      2 114506357.1218             nan     0.1000 93504.8446
    ##      3 113191571.9401             nan     0.1000 -127260.7694
    ##      4 112339101.0554             nan     0.1000 -369540.4332
    ##      5 112124260.9580             nan     0.1000 -60039.0347
    ##      6 111627159.6463             nan     0.1000 -179987.5376
    ##      7 110941934.1325             nan     0.1000 -526907.5569
    ##      8 110695838.9002             nan     0.1000 -474451.4166
    ##      9 110288669.3196             nan     0.1000 -147428.7261
    ##     10 110126640.9842             nan     0.1000 -446978.9915
    ##     20 107771051.1739             nan     0.1000 -218098.6111
    ##     40 104810821.2916             nan     0.1000 -334915.1958
    ##     60 102161126.5807             nan     0.1000 -442105.4823
    ##     80 99793282.9490             nan     0.1000 -547790.0512
    ##    100 97989324.2129             nan     0.1000 -768302.1103
    ##    120 96607870.5782             nan     0.1000 -146514.9549
    ##    140 93920885.6932             nan     0.1000 -164912.8356
    ##    160 92330786.7951             nan     0.1000 -239543.2022
    ##    180 91286111.6693             nan     0.1000 -555527.9553
    ##    200 90335274.9736             nan     0.1000 -696294.9233
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 115800535.1205             nan     0.1000 -59290.3611
    ##      2 115502200.1663             nan     0.1000 82892.6792
    ##      3 114603684.8688             nan     0.1000 -3687.9430
    ##      4 113670603.0480             nan     0.1000 -187828.9389
    ##      5 112744099.0056             nan     0.1000 -430719.3390
    ##      6 112365222.8542             nan     0.1000 -154204.8259
    ##      7 112004771.0027             nan     0.1000 -87895.7603
    ##      8 111464063.5253             nan     0.1000 -542409.1235
    ##      9 111221803.4571             nan     0.1000 -645235.4079
    ##     10 110743094.0270             nan     0.1000 -614679.5366
    ##     20 107076710.6941             nan     0.1000 -460076.1112
    ##     40 102658269.7120             nan     0.1000 -369856.2093
    ##     60 98885962.4570             nan     0.1000 -1228822.6349
    ##     80 95987398.9541             nan     0.1000 -544396.8311
    ##    100 93613988.1821             nan     0.1000 -585640.1276
    ##    120 91509934.2576             nan     0.1000 -480743.3424
    ##    140 89701736.5696             nan     0.1000 -406554.5425
    ##    160 86939585.2070             nan     0.1000 -355022.6632
    ##    180 84743095.4390             nan     0.1000 -522152.4492
    ##    200 83030846.5244             nan     0.1000 -136397.5368
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 82075029.6238             nan     0.1000 77846.0211
    ##      2 81970418.8434             nan     0.1000 -26918.1901
    ##      3 81901630.3775             nan     0.1000 53889.8449
    ##      4 81779540.7256             nan     0.1000 -48978.8839
    ##      5 81712013.2483             nan     0.1000 -26696.3855
    ##      6 81647594.4248             nan     0.1000 -25037.6556
    ##      7 81579670.4853             nan     0.1000 -59661.7663
    ##      8 81515516.6109             nan     0.1000 -43738.3376
    ##      9 81449233.1395             nan     0.1000 -40500.8686
    ##     10 81398677.6974             nan     0.1000 -36038.1669
    ##     20 81006342.2498             nan     0.1000 -52585.3229
    ##     40 80548706.8503             nan     0.1000 -87620.3037
    ##     60 80220563.4589             nan     0.1000 -56996.3891
    ##     80 79786610.3910             nan     0.1000 -57623.2584
    ##    100 79508595.4387             nan     0.1000 -30378.5225
    ##    120 79243183.1192             nan     0.1000 -59670.1101
    ##    140 78952393.5388             nan     0.1000 -76740.9971
    ##    160 78778407.8291             nan     0.1000 -57031.8380
    ##    180 78540554.6952             nan     0.1000 -31197.2776
    ##    200 78320881.4544             nan     0.1000 -119052.8428
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 81599944.3906             nan     0.1000 -134476.9913
    ##      2 81178209.6077             nan     0.1000 -34053.4185
    ##      3 81118951.5220             nan     0.1000 -39299.5572
    ##      4 80675361.0532             nan     0.1000 -336159.7243
    ##      5 79980514.5677             nan     0.1000 -232229.4337
    ##      6 79660225.4176             nan     0.1000 -167821.8875
    ##      7 79376140.2120             nan     0.1000 -100614.9525
    ##      8 78791402.3839             nan     0.1000 -181562.3695
    ##      9 78689126.2824             nan     0.1000 37285.4679
    ##     10 78487585.4909             nan     0.1000 -209816.6667
    ##     20 76909022.4981             nan     0.1000 -113262.4069
    ##     40 74226468.8064             nan     0.1000 -126286.9474
    ##     60 72647747.6875             nan     0.1000 -308320.6029
    ##     80 71000195.0509             nan     0.1000 -23224.8602
    ##    100 69320734.3368             nan     0.1000 -228974.8499
    ##    120 68199478.6242             nan     0.1000 -91755.2597
    ##    140 67074966.8639             nan     0.1000 -114003.4188
    ##    160 65793250.7081             nan     0.1000 -72014.3348
    ##    180 64938524.2252             nan     0.1000 -118058.8387
    ##    200 64367568.7161             nan     0.1000 -64918.9273
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 82050866.5855             nan     0.1000 65128.3085
    ##      2 81403683.1312             nan     0.1000 -38617.3931
    ##      3 80840913.9230             nan     0.1000 -96913.9300
    ##      4 80186242.6862             nan     0.1000 -155450.2663
    ##      5 79740096.9343             nan     0.1000 -392745.8629
    ##      6 79081788.8694             nan     0.1000 -251332.1934
    ##      7 78684469.5570             nan     0.1000 86481.3737
    ##      8 78430197.7832             nan     0.1000 -184140.1087
    ##      9 78003105.1099             nan     0.1000 -175883.4787
    ##     10 77825951.9583             nan     0.1000 -73946.6574
    ##     20 75093441.1350             nan     0.1000 -662802.3192
    ##     40 71266090.4183             nan     0.1000 -127297.3460
    ##     60 67496298.4318             nan     0.1000 -223001.8201
    ##     80 64684681.3025             nan     0.1000 -113919.0903
    ##    100 62786617.4057             nan     0.1000 -273942.3035
    ##    120 60442828.5679             nan     0.1000 -75760.9848
    ##    140 58766243.2277             nan     0.1000 -46510.5698
    ##    160 56787418.8697             nan     0.1000 -83529.7277
    ##    180 55186261.5989             nan     0.1000 -118314.2574
    ##    200 53315349.0494             nan     0.1000 -175956.4973
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 81366776.6656             nan     0.1000 -303239.5389
    ##      2 81175277.2350             nan     0.1000 -106738.3559
    ##      3 80520170.9379             nan     0.1000 -7178.6292
    ##      4 80390610.3735             nan     0.1000 -156300.1709
    ##      5 79512459.8045             nan     0.1000 -287789.2979
    ##      6 79317915.3274             nan     0.1000 -118389.2785
    ##      7 78875033.6822             nan     0.1000 -92122.8752
    ##      8 78403392.6236             nan     0.1000 -186635.7140
    ##      9 78258529.6249             nan     0.1000 -63788.9403
    ##     10 78126480.1345             nan     0.1000 -98603.7130
    ##     20 76532185.4187             nan     0.1000 -28863.7353
    ##     40 70502748.5635             nan     0.1000 -258036.8374
    ##     60 66378075.2743             nan     0.1000 -243094.9971
    ##     80 63156452.1218             nan     0.1000 -178499.2270
    ##    100 59577804.7570             nan     0.1000 -388585.7533
    ##    120 57125995.1948             nan     0.1000 -126058.6849
    ##    140 54921932.7287             nan     0.1000 -191783.8942
    ##    160 52564691.3336             nan     0.1000 -91350.7209
    ##    180 50800537.5151             nan     0.1000 -171076.4960
    ##    200 49541062.9075             nan     0.1000 -209712.8347
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 61649790.3231             nan     0.1000 68583.2718
    ##      2 61094754.5596             nan     0.1000 -96236.6464
    ##      3 60997925.6549             nan     0.1000 16548.3495
    ##      4 60977645.6822             nan     0.1000 -76800.0848
    ##      5 60961002.0764             nan     0.1000 -56077.4772
    ##      6 60549182.4362             nan     0.1000 -79942.9783
    ##      7 60270594.3296             nan     0.1000 -130534.4540
    ##      8 60211604.0445             nan     0.1000 36567.9181
    ##      9 60318197.0642             nan     0.1000 -431200.6822
    ##     10 60060807.7264             nan     0.1000 -180133.5211
    ##     20 59546056.2355             nan     0.1000 -428410.0908
    ##     40 59133872.7053             nan     0.1000 -379794.8739
    ##     60 59020927.0277             nan     0.1000 -1356.5364
    ##     80 59101004.8563             nan     0.1000 -341867.1556
    ##    100 58858419.7759             nan     0.1000 -267939.3499
    ##    120 58823572.2898             nan     0.1000 -295068.1570
    ##    140 58830233.2567             nan     0.1000 -378608.4932
    ##    160 58714206.1993             nan     0.1000 -316946.1076
    ##    180 58578555.6980             nan     0.1000 -60431.1693
    ##    200 58568432.7964             nan     0.1000 -538605.2986
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 61498021.5077             nan     0.1000 133341.1366
    ##      2 60946320.9437             nan     0.1000 -87944.4895
    ##      3 60321799.0416             nan     0.1000 -239183.7246
    ##      4 60239920.6257             nan     0.1000 11113.4842
    ##      5 60135041.8721             nan     0.1000 17607.4680
    ##      6 59760346.1664             nan     0.1000 -286191.3688
    ##      7 59827114.1131             nan     0.1000 -339273.7674
    ##      8 59773274.2621             nan     0.1000 -22837.0605
    ##      9 59493058.2885             nan     0.1000 -380333.5014
    ##     10 59432751.4971             nan     0.1000 -52537.7781
    ##     20 58876805.7960             nan     0.1000 -231273.9266
    ##     40 58319518.1011             nan     0.1000 -527879.8707
    ##     60 58069560.5114             nan     0.1000 -245351.0516
    ##     80 58038840.4587             nan     0.1000 -606714.4054
    ##    100 57595213.2892             nan     0.1000 -384680.3130
    ##    120 56945683.5384             nan     0.1000 -161399.5498
    ##    140 56697023.0156             nan     0.1000 -145201.7124
    ##    160 56161111.2048             nan     0.1000 -486823.1605
    ##    180 55835195.7543             nan     0.1000 -903870.4170
    ##    200 55776207.5795             nan     0.1000 -266550.9422
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 61306500.1268             nan     0.1000 186852.8892
    ##      2 60685151.4334             nan     0.1000 5528.5104
    ##      3 60270177.1497             nan     0.1000 -125521.8616
    ##      4 59865071.0837             nan     0.1000 -125814.4138
    ##      5 59360862.9884             nan     0.1000 -635597.2032
    ##      6 59209583.8008             nan     0.1000 19862.0907
    ##      7 59213227.6916             nan     0.1000 -340428.4510
    ##      8 59256738.8630             nan     0.1000 -287167.4141
    ##      9 59232901.4991             nan     0.1000 -507757.3634
    ##     10 59004713.0429             nan     0.1000 -402625.3014
    ##     20 58415508.2472             nan     0.1000 -300007.6740
    ##     40 57402935.7904             nan     0.1000 -395618.6894
    ##     60 56667545.4893             nan     0.1000 -438269.6206
    ##     80 56050835.0730             nan     0.1000 -708613.3329
    ##    100 55312915.9772             nan     0.1000 -284479.0434
    ##    120 53545417.0736             nan     0.1000 -301065.8316
    ##    140 52483115.9192             nan     0.1000 -264079.7292
    ##    160 51130759.7906             nan     0.1000 -408736.1216
    ##    180 50777181.3233             nan     0.1000 -258097.3582
    ##    200 48632811.4245             nan     0.1000 86908.1918
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 61689734.8900             nan     0.1000 342119.8848
    ##      2 61356108.2698             nan     0.1000 357540.6824
    ##      3 60695284.5147             nan     0.1000 -34440.5891
    ##      4 60022240.1354             nan     0.1000 -270604.6522
    ##      5 59543350.3083             nan     0.1000 -268117.1444
    ##      6 59331460.1101             nan     0.1000 -70143.2074
    ##      7 59072382.5187             nan     0.1000 -169521.9371
    ##      8 58689137.4430             nan     0.1000 -258269.4468
    ##      9 58553980.9757             nan     0.1000 -215368.4327
    ##     10 58498605.8460             nan     0.1000 -229239.7081
    ##     20 57589732.9497             nan     0.1000 -359218.2376
    ##     40 56133164.9454             nan     0.1000 -253740.7481
    ##     60 54255638.7735             nan     0.1000 51985.2048
    ##     80 53540576.1326             nan     0.1000 -137260.3321
    ##    100 51970794.5304             nan     0.1000 -276714.3660
    ##    120 51298947.7461             nan     0.1000 -359483.1423
    ##    140 50685921.9543             nan     0.1000 -425340.1514
    ##    160 50606400.6813             nan     0.1000 -562646.9666
    ##    180 48987987.5646             nan     0.1000 -463191.5769
    ##    200 47477375.4252             nan     0.1000 -241035.6221
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109668770.6441             nan     0.1000 63924.0772
    ##      2 109604097.9344             nan     0.1000 -43777.3006
    ##      3 108912929.1875             nan     0.1000 223670.8748
    ##      4 108772206.6398             nan     0.1000 39407.7120
    ##      5 108710405.3327             nan     0.1000 28865.8326
    ##      6 108602738.5119             nan     0.1000 6743.2588
    ##      7 108085695.0813             nan     0.1000 -99251.6245
    ##      8 108021366.9350             nan     0.1000 -31277.0095
    ##      9 107544808.8602             nan     0.1000 -279003.5127
    ##     10 107490567.7916             nan     0.1000 -85875.5240
    ##     20 106737929.9377             nan     0.1000 -282172.9487
    ##     40 106574457.7122             nan     0.1000 -392472.9961
    ##     60 106327269.6298             nan     0.1000 -245211.9419
    ##     80 106176757.2352             nan     0.1000 -134377.7822
    ##    100 106106331.6176             nan     0.1000 -170291.7054
    ##    120 106067285.1565             nan     0.1000 -654289.4464
    ##    140 106273848.0207             nan     0.1000 -361681.5364
    ##    160 105899278.2817             nan     0.1000 -62008.6566
    ##    180 105753595.1295             nan     0.1000 -646020.1589
    ##    200 105650407.3345             nan     0.1000 -576533.6248
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109078326.2979             nan     0.1000 307101.4977
    ##      2 108589733.2060             nan     0.1000 -17029.6895
    ##      3 107817148.7058             nan     0.1000 -187922.9572
    ##      4 107325966.3208             nan     0.1000 -203961.9646
    ##      5 106980638.6037             nan     0.1000 -416020.5185
    ##      6 106988794.5927             nan     0.1000 -455500.5359
    ##      7 107032505.9784             nan     0.1000 -459064.2323
    ##      8 106723840.7398             nan     0.1000 -487913.5074
    ##      9 106463945.3050             nan     0.1000 -347010.2930
    ##     10 106125960.8661             nan     0.1000 3664.2992
    ##     20 105575676.2380             nan     0.1000 -598502.5176
    ##     40 104904113.2224             nan     0.1000 -392769.8528
    ##     60 104431511.6698             nan     0.1000 -104264.0425
    ##     80 103161493.5179             nan     0.1000 -237035.2609
    ##    100 102069037.4757             nan     0.1000 -407929.4386
    ##    120 100283905.9624             nan     0.1000 -107264.6382
    ##    140 99112241.4332             nan     0.1000 -215372.2950
    ##    160 98707875.6472             nan     0.1000 -247835.3135
    ##    180 98193217.6054             nan     0.1000 -277091.7850
    ##    200 97525893.8007             nan     0.1000 -501587.8579
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109355424.4669             nan     0.1000 -32056.2715
    ##      2 108964509.5690             nan     0.1000 262242.8473
    ##      3 108280432.1028             nan     0.1000 -217167.2581
    ##      4 108102294.4930             nan     0.1000 117113.2431
    ##      5 106961773.6365             nan     0.1000 61276.2066
    ##      6 106300882.6053             nan     0.1000 -84048.1061
    ##      7 105733064.6956             nan     0.1000 -96717.7808
    ##      8 105107498.9601             nan     0.1000 -173274.9151
    ##      9 105039811.3888             nan     0.1000 -58368.8394
    ##     10 104595807.1018             nan     0.1000 -119871.8273
    ##     20 102134568.7521             nan     0.1000 -527547.2685
    ##     40 98074407.7457             nan     0.1000 -204236.0560
    ##     60 95982562.1704             nan     0.1000 -733096.2809
    ##     80 94384151.9331             nan     0.1000 -346548.7774
    ##    100 92039728.5608             nan     0.1000 -465742.3622
    ##    120 90197833.3844             nan     0.1000 -241324.7266
    ##    140 88686474.0117             nan     0.1000 -363770.2912
    ##    160 87246802.9253             nan     0.1000 -340765.3678
    ##    180 85939343.4030             nan     0.1000 -376071.4771
    ##    200 83692767.0893             nan     0.1000 -240936.9821
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109583159.2931             nan     0.1000 -132497.7386
    ##      2 108155551.3868             nan     0.1000 103096.4739
    ##      3 107255285.9154             nan     0.1000 -193950.5849
    ##      4 106803441.3818             nan     0.1000 -522384.9632
    ##      5 106289433.1297             nan     0.1000 -65708.1885
    ##      6 105651557.2529             nan     0.1000 -35962.6077
    ##      7 104804162.3382             nan     0.1000 -106614.4081
    ##      8 104621559.7653             nan     0.1000 -539555.3963
    ##      9 104334332.8169             nan     0.1000 -118444.1481
    ##     10 103711366.9502             nan     0.1000 -207119.2236
    ##     20 99808210.2297             nan     0.1000 -226134.7864
    ##     40 95665700.0833             nan     0.1000 -724343.0158
    ##     60 91429318.1643             nan     0.1000 -318708.2705
    ##     80 88353595.0532             nan     0.1000 -650538.4799
    ##    100 84867390.2673             nan     0.1000 -445199.4745
    ##    120 83018124.0640             nan     0.1000 -625194.3334
    ##    140 79796387.4684             nan     0.1000 -533842.2063
    ##    160 77754642.1588             nan     0.1000 -482327.8673
    ##    180 73728099.8201             nan     0.1000 -547587.0742
    ##    200 72064845.8746             nan     0.1000 -286407.4927
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109914367.7599             nan     0.1000 73118.8522
    ##      2 109781351.9233             nan     0.1000 -57984.8308
    ##      3 109744125.8881             nan     0.1000 -20950.4067
    ##      4 109175264.7347             nan     0.1000 -93712.4044
    ##      5 108576274.0773             nan     0.1000 -400693.7994
    ##      6 108281288.3469             nan     0.1000 -256688.7508
    ##      7 108111823.7943             nan     0.1000 -146396.0781
    ##      8 108189000.3484             nan     0.1000 -273653.8637
    ##      9 107942682.2471             nan     0.1000 -225006.6690
    ##     10 107840808.9147             nan     0.1000 36627.0539
    ##     20 107341160.9457             nan     0.1000 -470724.3312
    ##     40 106961099.5327             nan     0.1000 -327936.5790
    ##     60 106576059.7899             nan     0.1000 -299295.6981
    ##     80 106265251.3932             nan     0.1000 -291393.8511
    ##    100 106178509.1479             nan     0.1000 -186362.9471
    ##    120 106234241.2167             nan     0.1000 -361149.8324
    ##    140 105903352.9760             nan     0.1000 -543697.5084
    ##    160 105693906.6813             nan     0.1000 -162283.0708
    ##    180 105563581.5632             nan     0.1000 -295385.3538
    ##    200 105453739.6189             nan     0.1000 -602451.0727
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109320147.0011             nan     0.1000 23995.8129
    ##      2 109065898.7319             nan     0.1000 -73320.9124
    ##      3 108613127.0236             nan     0.1000 -167242.9102
    ##      4 107919456.4647             nan     0.1000 -228384.8389
    ##      5 107413817.6869             nan     0.1000 -147275.4487
    ##      6 106627702.6110             nan     0.1000 23479.3032
    ##      7 106084031.1579             nan     0.1000 -216812.1649
    ##      8 106121650.3512             nan     0.1000 -258497.1139
    ##      9 105784370.0257             nan     0.1000 -175511.8393
    ##     10 105537302.0311             nan     0.1000 -209420.3857
    ##     20 104098529.1328             nan     0.1000 -369356.2211
    ##     40 102114832.4262             nan     0.1000 -147712.1380
    ##     60 100459876.7824             nan     0.1000 -486737.4338
    ##     80 99135810.2347             nan     0.1000 -151701.3269
    ##    100 98931369.2057             nan     0.1000 -453094.1600
    ##    120 97854575.6929             nan     0.1000 -261639.3249
    ##    140 96789294.3436             nan     0.1000 32073.0611
    ##    160 95383966.4600             nan     0.1000 -286036.5587
    ##    180 93940813.0574             nan     0.1000 -236031.4464
    ##    200 92936086.3220             nan     0.1000 -160262.5372
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109126992.8474             nan     0.1000 261829.3687
    ##      2 108646981.8249             nan     0.1000 91096.0096
    ##      3 107661337.3835             nan     0.1000 82795.7986
    ##      4 107597257.9333             nan     0.1000 -132317.9971
    ##      5 106926647.9311             nan     0.1000 -48000.6290
    ##      6 106315031.9264             nan     0.1000 -156998.6055
    ##      7 105412957.0061             nan     0.1000 -350348.0711
    ##      8 105414294.8037             nan     0.1000 -228531.0094
    ##      9 105328458.1620             nan     0.1000 -299992.6576
    ##     10 104513364.5870             nan     0.1000 -498655.3865
    ##     20 100861809.7960             nan     0.1000 -482237.6358
    ##     40 97523864.8126             nan     0.1000 -537028.4003
    ##     60 93570492.6751             nan     0.1000 40206.6680
    ##     80 91469458.3248             nan     0.1000 -456137.5283
    ##    100 89662504.2567             nan     0.1000 -781862.0016
    ##    120 88320462.3839             nan     0.1000 -537055.8592
    ##    140 86975938.9387             nan     0.1000 -466764.4212
    ##    160 84678899.2694             nan     0.1000 -185215.6451
    ##    180 82773795.7711             nan     0.1000 -414246.1881
    ##    200 81648129.0891             nan     0.1000 -563804.3625
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109828358.2167             nan     0.1000 14967.4634
    ##      2 109119931.4121             nan     0.1000 -167201.0681
    ##      3 108363697.6267             nan     0.1000 79826.9338
    ##      4 108069114.0096             nan     0.1000 119013.3205
    ##      5 107798927.8400             nan     0.1000 20188.3268
    ##      6 107465669.1508             nan     0.1000 -66833.5641
    ##      7 106407760.5049             nan     0.1000 -227701.7778
    ##      8 105464904.4967             nan     0.1000 -24985.4183
    ##      9 104391071.5846             nan     0.1000 -179497.8501
    ##     10 104155115.8929             nan     0.1000 -307623.1451
    ##     20 100211911.6275             nan     0.1000 -284347.5075
    ##     40 96133753.1185             nan     0.1000 -649615.7206
    ##     60 92708181.6945             nan     0.1000 -370294.5380
    ##     80 88092157.4186             nan     0.1000 -667869.5527
    ##    100 85270043.6670             nan     0.1000 -755060.9290
    ##    120 81475775.5245             nan     0.1000 -16370.7885
    ##    140 79503482.4698             nan     0.1000 -475529.8801
    ##    160 76887390.2749             nan     0.1000 -371911.0843
    ##    180 74805887.7078             nan     0.1000 -202220.0010
    ##    200 73426757.3543             nan     0.1000 -418397.6840
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 96010047.8827             nan     0.1000 -3536.7816
    ##      2 95605170.0742             nan     0.1000 -77383.5429
    ##      3 95163310.5025             nan     0.1000 -29787.6102
    ##      4 94922271.1042             nan     0.1000 -287926.6683
    ##      5 94818998.5828             nan     0.1000 54684.8534
    ##      6 94761789.5326             nan     0.1000 -36361.5900
    ##      7 94659803.7942             nan     0.1000 -37418.9765
    ##      8 94718243.5217             nan     0.1000 -191111.6113
    ##      9 94496365.0460             nan     0.1000 -207784.9422
    ##     10 94427622.6111             nan     0.1000 -17937.3030
    ##     20 93989567.2018             nan     0.1000 -37841.1130
    ##     25 93869163.7294             nan     0.1000 -325737.4502

Now we get the RMSE from each model and find the best one. The model
with the highest predictive power will have the smallest RMSE.

``` r
newsPop1Pred_model1 <- predict(model1, newdata = newsPop1Test)
newsPop1Pred_model2 <- predict(model2, newdata = newsPop1Test)
```

    ## Warning in predict.lm(model2, newdata = newsPop1Test): prediction from
    ## rank-deficient fit; attr(*, "non-estim") has doubtful cases

``` r
newsPop1Pred_rf <- predict(newsPopFit_rf, newdata = newsPop1Test)
newsPop1Pred_boost <- predict(newsPopFit_boost, newdata = newsPop1Test)

# Find RMSE
model1_RMSE <- round(sqrt(mean((newsPop1Pred_model1-newsPop1Test$shares)^2)),2)
model2_RMSE <- round(sqrt(mean((newsPop1Pred_model2-newsPop1Test$shares)^2)),2)
RMSE_rf <- round(sqrt(mean((newsPop1Pred_rf-newsPop1Test$shares)^2)),2)
RMSE_boost <- round(sqrt(mean((newsPop1Pred_boost-newsPop1Test$shares)^2)),2)

RMSE_mods <- c(model1_RMSE, "Full Linear Regression Model", 
               model2_RMSE, "Curvi-Linear Regression Model", 
               RMSE_rf, "Random Forest Model", 
               RMSE_boost, "Boosted Tree Model")

RMSE_mods
```

    ## [1] "6298.59"                       "Full Linear Regression Model" 
    ## [3] "6311.21"                       "Curvi-Linear Regression Model"
    ## [5] "6742.54"                       "Random Forest Model"          
    ## [7] "6279.6"                        "Boosted Tree Model"

``` r
paste("The", RMSE_mods[which(RMSE_mods==min(RMSE_mods))+1], "had the smallest RMSE of", RMSE_mods[which(RMSE_mods==min(RMSE_mods))])
```

    ## [1] "The Boosted Tree Model had the smallest RMSE of 6279.6"
