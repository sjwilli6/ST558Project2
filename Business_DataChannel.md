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
datachannel <- "Business"

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

    ##  n_tokens_title  n_unique_tokens     num_imgs        num_videos     
    ##  Min.   : 3.00   Min.   :0.0000   Min.   : 0.000   Min.   : 0.0000  
    ##  1st Qu.: 9.00   1st Qu.:0.4769   1st Qu.: 1.000   1st Qu.: 0.0000  
    ##  Median :10.00   Median :0.5476   Median : 1.000   Median : 0.0000  
    ##  Mean   :10.26   Mean   :0.5442   Mean   : 1.801   Mean   : 0.6749  
    ##  3rd Qu.:12.00   3rd Qu.:0.6098   3rd Qu.: 1.000   3rd Qu.: 0.0000  
    ##  Max.   :19.00   Max.   :0.8732   Max.   :38.000   Max.   :75.0000  
    ##   num_keywords    rate_positive_words rate_negative_words     shares      
    ##  Min.   : 2.000   Min.   :0.0000      Min.   :0.0000      Min.   :     1  
    ##  1st Qu.: 5.000   1st Qu.:0.6667      1st Qu.:0.1667      1st Qu.:   957  
    ##  Median : 6.000   Median :0.7500      Median :0.2500      Median :  1400  
    ##  Mean   : 6.511   Mean   :0.7364      Mean   :0.2595      Mean   :  2890  
    ##  3rd Qu.: 8.000   3rd Qu.:0.8333      3rd Qu.:0.3333      3rd Qu.:  2500  
    ##  Max.   :10.000   Max.   :1.0000      Max.   :1.0000      Max.   :310800

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
    ##     318     969    1500    3170    2300  690400 
    ## 
    ## $Entertainment
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     294     956    1400    2878    2400   67500 
    ## 
    ## $Lifestyle
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     343    1200    1800    3197    3100   36200 
    ## 
    ## $Miscellaneous
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       4    1100    1800    4898    3400  617900 
    ## 
    ## $SocialMedia
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     428    1500    2300    4008    4400   57600 
    ## 
    ## $Tech
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     217    1200    1900    3358    3200   71800 
    ## 
    ## $World
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   326.0   924.2  1400.0  2265.1  2300.0 27700.0

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
    ##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 1218 1642  624  301  162  107   82   42   42   23   22   10   10   10    6    4 
    ##   16   17   18   19   20   21   22   23   24   25   26   27   28   30   31   32 
    ##    8    6    5    3    3    3    2    2    3    1    5    3    2    1    2    1 
    ##   33   35   40   41   42   44   45   47   48   49   57   78   80   92   94   98 
    ##    1    1    1    1    2    1    1    2    1    1    1    1    1    1    1    1 
    ##  102  106  110  158  298  306  310 
    ##    1    1    1    1    1    1    1

We see that most of the observations had less than 20,000 shares. There
are larger jumps in thousands of shares once we get to around 70,000.

Now let’s take a look at a contingency table for the data channels.

``` r
table(newsPopTrain$data_channel)
```

    ## 
    ##      Business Entertainment     Lifestyle Miscellaneous   SocialMedia 
    ##           784           633           362           715           366 
    ##          Tech         World 
    ##           964           556

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
    ##  -9636  -1810  -1223   -210 307633 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           431.53    2521.15   0.171  0.86410    
    ## n_tokens_title         79.07      69.03   1.145  0.25208    
    ## n_unique_tokens       -11.15    1626.76  -0.007  0.99453    
    ## num_imgs              218.02      45.16   4.828 1.43e-06 ***
    ## num_videos            110.86      41.72   2.657  0.00791 ** 
    ## num_keywords          164.06      77.07   2.129  0.03333 *  
    ## rate_positive_words  -286.78    2498.07  -0.115  0.90861    
    ## rate_negative_words  1263.34    2613.33   0.483  0.62882    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9879 on 4371 degrees of freedom
    ## Multiple R-squared:  0.009548,   Adjusted R-squared:  0.007961 
    ## F-statistic: 6.019 on 7 and 4371 DF,  p-value: 5.3e-07

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
    ##  -8257  -1758  -1084    -91 307403 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       2890        149  19.392  < 2e-16 ***
    ## poly(n_tokens_title, 2)1          8477       9904   0.856  0.39207    
    ## poly(n_tokens_title, 2)2         12287       9902   1.241  0.21470    
    ## poly(n_unique_tokens, 2)1       -11005      12193  -0.903  0.36681    
    ## poly(n_unique_tokens, 2)2        50916      16506   3.085  0.00205 ** 
    ## poly(num_imgs, 2)1               48645      10258   4.742 2.18e-06 ***
    ## poly(num_imgs, 2)2              -13525      10028  -1.349  0.17751    
    ## poly(num_videos, 2)1             26856       9929   2.705  0.00686 ** 
    ## poly(num_videos, 2)2            -30409       9906  -3.070  0.00215 ** 
    ## poly(num_keywords, 2)1           17814      10146   1.756  0.07919 .  
    ## poly(num_keywords, 2)2           -3890       9894  -0.393  0.69424    
    ## poly(rate_positive_words, 2)1    72424      43788   1.654  0.09820 .  
    ## poly(rate_positive_words, 2)2   -14619      13426  -1.089  0.27627    
    ## poly(rate_negative_words, 2)1    81436      43921   1.854  0.06379 .  
    ## poly(rate_negative_words, 2)2       NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9860 on 4365 degrees of freedom
    ## Multiple R-squared:  0.01452,    Adjusted R-squared:  0.01159 
    ## F-statistic: 4.948 on 13 and 4365 DF,  p-value: 1.031e-08

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
    ##      1 88233554.2531             nan     0.1000 12722.2112
    ##      2 88076228.2395             nan     0.1000 24049.4091
    ##      3 87967264.3342             nan     0.1000 -10932.8332
    ##      4 87836557.3884             nan     0.1000 -70675.6988
    ##      5 87672968.5386             nan     0.1000 102006.7928
    ##      6 87501227.8292             nan     0.1000 156148.7445
    ##      7 87459953.6695             nan     0.1000 17122.2671
    ##      8 87369188.5363             nan     0.1000 -53910.1577
    ##      9 87220682.7651             nan     0.1000 105219.3486
    ##     10 87187028.4137             nan     0.1000 25210.3459
    ##     20 86665099.9289             nan     0.1000 -50281.8346
    ##     40 86041010.2947             nan     0.1000 -89343.6609
    ##     60 85700098.7659             nan     0.1000 -33436.8067
    ##     80 85318322.4250             nan     0.1000 -81410.0390
    ##    100 85114194.7386             nan     0.1000 -115004.1464
    ##    120 84916877.9479             nan     0.1000 -127059.1315
    ##    140 84828640.2621             nan     0.1000 -155871.8362
    ##    160 84575762.8191             nan     0.1000 -57962.0101
    ##    180 84420757.6192             nan     0.1000 -124510.2195
    ##    200 84196322.9754             nan     0.1000 -64705.8045
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 87858947.9106             nan     0.1000 110537.2712
    ##      2 87678343.1450             nan     0.1000 194242.2745
    ##      3 87152913.3061             nan     0.1000 73698.3630
    ##      4 87007683.4420             nan     0.1000 83671.0546
    ##      5 86793491.5469             nan     0.1000 77677.3940
    ##      6 86255761.8900             nan     0.1000 -32279.4080
    ##      7 85854618.4261             nan     0.1000 -74729.5608
    ##      8 85628039.4188             nan     0.1000 -10432.4681
    ##      9 85446280.7974             nan     0.1000 30171.4027
    ##     10 85374019.1952             nan     0.1000 -5845.9355
    ##     20 82858523.0366             nan     0.1000 -183710.7177
    ##     40 80495001.3019             nan     0.1000 -254119.8986
    ##     60 78838081.7751             nan     0.1000 -118642.3962
    ##     80 77583905.5192             nan     0.1000 -376462.9825
    ##    100 76259668.6421             nan     0.1000 -84293.1240
    ##    120 75620720.8435             nan     0.1000 -375810.7269
    ##    140 74698907.4307             nan     0.1000 -251341.0726
    ##    160 73269007.7691             nan     0.1000 -46125.3128
    ##    180 72420973.7529             nan     0.1000 -181453.4583
    ##    200 71524707.1881             nan     0.1000 -257114.9074
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 87752448.8437             nan     0.1000 74918.0724
    ##      2 87123796.3780             nan     0.1000 155967.1266
    ##      3 86640970.1891             nan     0.1000 -82701.7415
    ##      4 86144605.0800             nan     0.1000 -44115.9381
    ##      5 84617245.2621             nan     0.1000 -35347.6255
    ##      6 83942939.8812             nan     0.1000 20922.2751
    ##      7 83382071.9763             nan     0.1000 -104550.1100
    ##      8 83032613.0498             nan     0.1000 -59188.6573
    ##      9 81660345.5983             nan     0.1000 -566249.6277
    ##     10 81408445.3069             nan     0.1000 -23585.7956
    ##     20 78711932.8313             nan     0.1000 -486354.9295
    ##     40 75984662.7057             nan     0.1000 -204231.5785
    ##     60 73991050.2199             nan     0.1000 -258156.6598
    ##     80 72254531.4655             nan     0.1000 -286605.9069
    ##    100 70622056.4233             nan     0.1000 -200753.9780
    ##    120 69033535.1373             nan     0.1000 -211287.2029
    ##    140 67733558.5888             nan     0.1000 -248246.9313
    ##    160 65579686.0544             nan     0.1000 -252530.0832
    ##    180 64866013.5621             nan     0.1000 -373069.5273
    ##    200 64008924.3125             nan     0.1000 -135803.2771
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 87678134.5776             nan     0.1000 117660.5372
    ##      2 87041945.6582             nan     0.1000 112734.5818
    ##      3 84962926.3112             nan     0.1000 -57526.7813
    ##      4 84368791.3606             nan     0.1000 -65962.2752
    ##      5 83856990.0975             nan     0.1000 -121445.8021
    ##      6 82376281.1744             nan     0.1000 -753786.7316
    ##      7 81733347.0840             nan     0.1000 -152805.1431
    ##      8 81500306.3309             nan     0.1000 -40427.8577
    ##      9 80496464.7168             nan     0.1000 176176.9704
    ##     10 80140503.4996             nan     0.1000 -47141.6016
    ##     20 76998211.2234             nan     0.1000 -102625.4319
    ##     40 73541914.3658             nan     0.1000 -97410.1492
    ##     60 69627034.6658             nan     0.1000 -351129.4273
    ##     80 68411068.1218             nan     0.1000 -514541.1427
    ##    100 66552510.4418             nan     0.1000 -250262.8673
    ##    120 64698976.8805             nan     0.1000 -216430.9913
    ##    140 62598102.2209             nan     0.1000 31099.6846
    ##    160 60374505.4765             nan     0.1000 -38899.5460
    ##    180 58777871.7120             nan     0.1000 -245118.8712
    ##    200 57045568.3104             nan     0.1000 -171318.1529
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 115579932.0762             nan     0.1000 249961.8037
    ##      2 115303162.8543             nan     0.1000 -52876.0050
    ##      3 114845461.0081             nan     0.1000 12302.2994
    ##      4 114611537.4195             nan     0.1000 158745.2021
    ##      5 114288419.6350             nan     0.1000 51192.2157
    ##      6 114169242.4768             nan     0.1000 74467.6934
    ##      7 113857448.3490             nan     0.1000 -38155.8897
    ##      8 113693026.7740             nan     0.1000  817.1984
    ##      9 113572365.7230             nan     0.1000 84491.1495
    ##     10 113461845.5599             nan     0.1000 -58870.9244
    ##     20 112628925.2531             nan     0.1000 -155827.1574
    ##     40 111744935.2323             nan     0.1000 44661.1416
    ##     60 111295188.5205             nan     0.1000 -261528.3607
    ##     80 110961026.0931             nan     0.1000 -338292.0611
    ##    100 110599558.9756             nan     0.1000 -288647.5755
    ##    120 109978315.5499             nan     0.1000 -200696.7577
    ##    140 109578253.6522             nan     0.1000 -263417.5277
    ##    160 109366248.8264             nan     0.1000 -130189.6203
    ##    180 109262229.2443             nan     0.1000 -237082.3501
    ##    200 108940299.9401             nan     0.1000 -68174.0721
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114952379.8934             nan     0.1000 48193.9741
    ##      2 114157438.4345             nan     0.1000 73738.8085
    ##      3 113587700.8815             nan     0.1000 -52614.4338
    ##      4 113153684.9600             nan     0.1000 228531.5002
    ##      5 112683949.5442             nan     0.1000 -141216.0409
    ##      6 112324250.3853             nan     0.1000 72733.2188
    ##      7 111364789.5820             nan     0.1000 -143928.2958
    ##      8 110182040.8352             nan     0.1000 -618959.4924
    ##      9 108945806.3325             nan     0.1000 -510656.3810
    ##     10 108617989.8071             nan     0.1000 -82986.9062
    ##     20 105549937.8620             nan     0.1000 -334638.1536
    ##     40 102728486.7707             nan     0.1000 -42818.0594
    ##     60 101176892.1652             nan     0.1000 -358213.2713
    ##     80 99034398.1072             nan     0.1000 -460807.5065
    ##    100 97162896.5420             nan     0.1000 -54536.4259
    ##    120 96553507.4336             nan     0.1000 -221333.2065
    ##    140 95064711.9539             nan     0.1000 -108870.4217
    ##    160 94059390.6138             nan     0.1000 -316785.4373
    ##    180 93274766.7957             nan     0.1000 -402394.4521
    ##    200 92472583.7482             nan     0.1000 -434850.2905
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 115492856.6482             nan     0.1000 -28267.1973
    ##      2 113601860.8232             nan     0.1000 140806.7255
    ##      3 112094089.5292             nan     0.1000 -426840.7770
    ##      4 110975919.3468             nan     0.1000 811573.4769
    ##      5 110091639.8738             nan     0.1000 -146626.3558
    ##      6 109025626.3620             nan     0.1000 -100033.6467
    ##      7 108031004.4106             nan     0.1000 -94375.8783
    ##      8 107134418.4067             nan     0.1000 -490212.1582
    ##      9 107297062.6953             nan     0.1000 -641775.4251
    ##     10 106977312.7715             nan     0.1000 -165867.8952
    ##     20 104924385.8748             nan     0.1000 -195684.2777
    ##     40 99595362.1834             nan     0.1000 -1171534.3520
    ##     60 98352616.4859             nan     0.1000 -254305.8965
    ##     80 95944007.0669             nan     0.1000 -568135.3290
    ##    100 91795194.6901             nan     0.1000 -656638.0909
    ##    120 90445082.4876             nan     0.1000 -209095.8957
    ##    140 87580781.7139             nan     0.1000 -305592.0741
    ##    160 85438769.0109             nan     0.1000 -457254.3490
    ##    180 84278679.1663             nan     0.1000 -906303.6752
    ##    200 82851699.2663             nan     0.1000 -174708.6175
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114593401.7943             nan     0.1000 -15413.0389
    ##      2 113944076.6821             nan     0.1000 -6545.3849
    ##      3 111980405.1093             nan     0.1000 -229057.6641
    ##      4 110007651.2210             nan     0.1000 -133562.7366
    ##      5 109773291.6372             nan     0.1000 2535.4884
    ##      6 109393912.1623             nan     0.1000 -166949.6489
    ##      7 109281474.6726             nan     0.1000 -141426.4749
    ##      8 108661534.0709             nan     0.1000 -170926.9596
    ##      9 108374719.7127             nan     0.1000 -130389.1990
    ##     10 107974830.1235             nan     0.1000 -59087.2378
    ##     20 101673603.3906             nan     0.1000 -891708.4905
    ##     40 96203556.6656             nan     0.1000 -954439.2331
    ##     60 93387056.2568             nan     0.1000 -595432.2743
    ##     80 89537990.9278             nan     0.1000 -241021.2527
    ##    100 87338979.4191             nan     0.1000 -656705.6529
    ##    120 85204414.1454             nan     0.1000 -157919.8215
    ##    140 83579911.3850             nan     0.1000 -88298.9478
    ##    160 81896657.5456             nan     0.1000 -479503.3272
    ##    180 79695500.3909             nan     0.1000 -244785.2598
    ##    200 78547422.3692             nan     0.1000 -534626.0325
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 112347842.6042             nan     0.1000 79584.8844
    ##      2 112175637.1993             nan     0.1000 170165.7309
    ##      3 111567248.1231             nan     0.1000 -99469.8026
    ##      4 111333306.4544             nan     0.1000 -139490.8658
    ##      5 111242520.4355             nan     0.1000 13000.0167
    ##      6 111119313.0252             nan     0.1000 -106212.6125
    ##      7 110986056.5830             nan     0.1000 101156.0963
    ##      8 110945875.9592             nan     0.1000 -8756.7610
    ##      9 110631392.8314             nan     0.1000 -168955.9785
    ##     10 110515512.0610             nan     0.1000 50594.4021
    ##     20 109850793.8683             nan     0.1000 -108831.5085
    ##     40 109224130.4132             nan     0.1000 -357696.1483
    ##     60 108740107.9236             nan     0.1000 -219446.4905
    ##     80 108324621.3778             nan     0.1000 -108130.9182
    ##    100 108101648.8770             nan     0.1000 -294264.1464
    ##    120 107497540.5974             nan     0.1000 -125616.6305
    ##    140 107139676.4530             nan     0.1000 -217822.8417
    ##    160 106962930.5802             nan     0.1000 -183567.0716
    ##    180 106812755.1186             nan     0.1000 -250726.1039
    ##    200 106686497.9340             nan     0.1000 -147104.7509
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 111776062.1739             nan     0.1000 23620.6376
    ##      2 111299741.0474             nan     0.1000 -11152.5809
    ##      3 109796574.2511             nan     0.1000 -317713.3020
    ##      4 108410298.6140             nan     0.1000 -184729.3886
    ##      5 107946751.7505             nan     0.1000 -42643.5376
    ##      6 107384782.0990             nan     0.1000 262499.1338
    ##      7 107272193.7498             nan     0.1000  -88.9595
    ##      8 106234439.9902             nan     0.1000 -387913.4382
    ##      9 106046954.5467             nan     0.1000 -57167.4307
    ##     10 105710567.1202             nan     0.1000 -4663.7883
    ##     20 102633093.0259             nan     0.1000 -876620.6223
    ##     40 100646714.5861             nan     0.1000 -48595.3859
    ##     60 98643054.8032             nan     0.1000 -100377.5165
    ##     80 98435084.1766             nan     0.1000 -277827.3725
    ##    100 97260051.1030             nan     0.1000 -152527.4107
    ##    120 96064867.2648             nan     0.1000 -380208.5037
    ##    140 94314946.1195             nan     0.1000 -639282.7577
    ##    160 93289514.7651             nan     0.1000 -455542.6468
    ##    180 92340007.4230             nan     0.1000 -221358.7809
    ##    200 90932961.6568             nan     0.1000 -751909.2771
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 111900198.7115             nan     0.1000 -18443.9292
    ##      2 110983052.9042             nan     0.1000 256499.9226
    ##      3 109131659.6667             nan     0.1000 -196330.9224
    ##      4 107469215.3013             nan     0.1000 -354107.8660
    ##      5 106910344.1759             nan     0.1000 -6051.5689
    ##      6 106459213.8423             nan     0.1000 -103280.3632
    ##      7 106117409.1454             nan     0.1000 -147770.6473
    ##      8 104915724.5641             nan     0.1000 -568812.5672
    ##      9 104930190.8490             nan     0.1000 -347791.7946
    ##     10 104854266.3669             nan     0.1000 -188165.8512
    ##     20 101360118.9674             nan     0.1000 -854230.9479
    ##     40 99113227.1304             nan     0.1000 -715610.8495
    ##     60 97470568.6282             nan     0.1000 -380096.4401
    ##     80 94964677.2058             nan     0.1000 -276036.0438
    ##    100 93723186.7376             nan     0.1000 -752402.7266
    ##    120 92321945.7948             nan     0.1000 -1439732.9322
    ##    140 90295374.8168             nan     0.1000 -362700.3387
    ##    160 87860927.2127             nan     0.1000 -293042.3367
    ##    180 86779871.2889             nan     0.1000 -476440.3066
    ##    200 86573107.1400             nan     0.1000 -364041.6142
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 111416774.8302             nan     0.1000 48954.1616
    ##      2 110527180.5494             nan     0.1000 -45568.4158
    ##      3 108962080.6924             nan     0.1000 -326838.4975
    ##      4 107967306.5018             nan     0.1000 143314.8935
    ##      5 107579911.5786             nan     0.1000 -138389.3996
    ##      6 106970995.9215             nan     0.1000 -7476.9276
    ##      7 105704684.8519             nan     0.1000 -512021.7797
    ##      8 104287556.0032             nan     0.1000 -432156.2373
    ##      9 104260885.5807             nan     0.1000 -259838.0478
    ##     10 103861332.7033             nan     0.1000 -268903.2245
    ##     20 100297846.9604             nan     0.1000 -935498.6546
    ##     40 96005027.3290             nan     0.1000 -242706.5099
    ##     60 93909172.7433             nan     0.1000 -587939.0418
    ##     80 89843523.1094             nan     0.1000 -621152.5244
    ##    100 87595453.8737             nan     0.1000 -194238.0740
    ##    120 84290411.0454             nan     0.1000 -268031.9521
    ##    140 83148601.0133             nan     0.1000 -245669.1111
    ##    160 81516817.7494             nan     0.1000 -73257.7362
    ##    180 80135632.3042             nan     0.1000 -538695.9996
    ##    200 78964994.9065             nan     0.1000 -769930.5177
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 117002660.5579             nan     0.1000 -48470.1111
    ##      2 116792473.0758             nan     0.1000 60042.6098
    ##      3 116616815.5149             nan     0.1000 -53696.2816
    ##      4 116362789.5012             nan     0.1000 165089.6374
    ##      5 116135872.4959             nan     0.1000 233937.9121
    ##      6 116003160.2760             nan     0.1000 62736.6688
    ##      7 115905855.6520             nan     0.1000 4655.3063
    ##      8 115830654.1654             nan     0.1000 16462.6082
    ##      9 115409817.0010             nan     0.1000 -62576.2099
    ##     10 115351389.3220             nan     0.1000 -78022.6139
    ##     20 114640852.1941             nan     0.1000 -234386.3679
    ##     40 113994896.4386             nan     0.1000 -223730.4737
    ##     60 113277066.6045             nan     0.1000 -25063.9270
    ##     80 112855410.7636             nan     0.1000 -48130.0348
    ##    100 112139654.1056             nan     0.1000 -310060.5379
    ##    120 111978683.9308             nan     0.1000 -429518.1008
    ##    140 111716211.9648             nan     0.1000 -169355.8406
    ##    160 111516366.0543             nan     0.1000 -480534.7231
    ##    180 111377499.9497             nan     0.1000 -238633.3151
    ##    200 111256255.5249             nan     0.1000 -178153.7131
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 116555980.8671             nan     0.1000 -11361.8667
    ##      2 115994104.5402             nan     0.1000 -103235.0286
    ##      3 114443627.9967             nan     0.1000 -195965.2249
    ##      4 113780110.8855             nan     0.1000 -50295.6719
    ##      5 113608244.2846             nan     0.1000 -212315.5131
    ##      6 113472284.1661             nan     0.1000 -362567.5589
    ##      7 113184493.1021             nan     0.1000 -40235.9141
    ##      8 112272392.7969             nan     0.1000 -861610.3486
    ##      9 112099623.8498             nan     0.1000 -111094.4932
    ##     10 111552108.9576             nan     0.1000 -239198.9490
    ##     20 108362523.1845             nan     0.1000 -717516.8136
    ##     40 105527229.0220             nan     0.1000 -68853.8057
    ##     60 104883337.3657             nan     0.1000 -461561.4568
    ##     80 103528559.6482             nan     0.1000 -519504.3775
    ##    100 101552144.6417             nan     0.1000 -304003.2604
    ##    120 100414731.5829             nan     0.1000 -296693.0660
    ##    140 100318625.7108             nan     0.1000 -205908.1378
    ##    160 98837645.9041             nan     0.1000 -31561.9314
    ##    180 97864143.0113             nan     0.1000 -594558.1662
    ##    200 96881721.7700             nan     0.1000 -587658.3742
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 116516339.7462             nan     0.1000 52768.4441
    ##      2 114957647.1563             nan     0.1000 8898.3159
    ##      3 114337134.4191             nan     0.1000 132825.1712
    ##      4 113929440.2857             nan     0.1000 -67892.7667
    ##      5 112919340.6905             nan     0.1000 -147052.4068
    ##      6 112605104.6101             nan     0.1000 -56837.8533
    ##      7 111499596.1495             nan     0.1000 -823948.0289
    ##      8 109854397.5842             nan     0.1000 -253546.9808
    ##      9 109876092.4752             nan     0.1000 -238617.4951
    ##     10 109370964.0297             nan     0.1000 -85053.1962
    ##     20 105060497.9140             nan     0.1000 -828097.6373
    ##     40 101631115.2995             nan     0.1000 -818631.3248
    ##     60 99294221.7539             nan     0.1000 -431751.8812
    ##     80 96536435.1858             nan     0.1000 -718192.0793
    ##    100 95030260.4824             nan     0.1000 -657749.9658
    ##    120 92708254.8928             nan     0.1000 -358504.8131
    ##    140 91793411.7168             nan     0.1000 -268792.9379
    ##    160 89461259.0372             nan     0.1000 -550120.6979
    ##    180 87902756.0813             nan     0.1000 -246173.2526
    ##    200 86963717.7927             nan     0.1000 -114821.4634
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114929147.3957             nan     0.1000 107976.9785
    ##      2 114687106.3266             nan     0.1000 -37646.4415
    ##      3 114046490.8863             nan     0.1000 134484.0816
    ##      4 113434757.4205             nan     0.1000 -19766.4670
    ##      5 111664324.2798             nan     0.1000 -139076.7395
    ##      6 111368654.8725             nan     0.1000 98878.7051
    ##      7 110035338.6323             nan     0.1000 -334599.9457
    ##      8 108966396.2598             nan     0.1000 -312747.1990
    ##      9 107980149.8622             nan     0.1000 -261215.2331
    ##     10 107697629.3688             nan     0.1000 -114865.3334
    ##     20 102507527.8141             nan     0.1000 -860879.2157
    ##     40 98896965.7871             nan     0.1000 -600134.4816
    ##     60 95744524.1669             nan     0.1000 -173134.2247
    ##     80 93564099.4376             nan     0.1000 -351502.8188
    ##    100 92288656.2728             nan     0.1000 -661212.3255
    ##    120 89659106.1417             nan     0.1000 -929761.1828
    ##    140 88093129.0297             nan     0.1000 -608596.2288
    ##    160 85453305.3917             nan     0.1000 -754086.4137
    ##    180 82952496.9863             nan     0.1000 -809453.0491
    ##    200 80551126.1285             nan     0.1000 -999485.0132
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 57563391.5810             nan     0.1000 -5116.9701
    ##      2 57347228.9417             nan     0.1000 -87021.4353
    ##      3 57184337.1946             nan     0.1000 -128573.4168
    ##      4 57094850.0644             nan     0.1000 -122301.5667
    ##      5 57002105.0726             nan     0.1000 -96801.7630
    ##      6 56927961.9302             nan     0.1000 20056.3864
    ##      7 56886305.2665             nan     0.1000 -160980.9266
    ##      8 56872728.9475             nan     0.1000 -202913.1631
    ##      9 56880173.6685             nan     0.1000 -114128.6969
    ##     10 56785239.2096             nan     0.1000 -19850.5205
    ##     20 56496713.7113             nan     0.1000 -192468.5971
    ##     40 55882159.6834             nan     0.1000 -231170.9848
    ##     60 55580986.7184             nan     0.1000 82596.6621
    ##     80 55454721.2923             nan     0.1000 -172510.7519
    ##    100 55112282.6594             nan     0.1000 -190133.1272
    ##    120 55054214.4301             nan     0.1000 1478.2825
    ##    140 55047942.0238             nan     0.1000 -160176.2804
    ##    160 54918938.9289             nan     0.1000 -178460.6262
    ##    180 54805524.6598             nan     0.1000 -135076.5224
    ##    200 54671240.3816             nan     0.1000 -194013.8637
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 57036862.9240             nan     0.1000 -115508.8783
    ##      2 56826084.8274             nan     0.1000 136657.1114
    ##      3 56701461.3256             nan     0.1000 86956.5563
    ##      4 56453616.2140             nan     0.1000 33806.4694
    ##      5 56193219.4106             nan     0.1000 -30026.4338
    ##      6 56074085.7501             nan     0.1000 37822.8572
    ##      7 55877358.7418             nan     0.1000 -3122.3410
    ##      8 55703643.8254             nan     0.1000 57941.6485
    ##      9 55609193.5368             nan     0.1000 25779.0948
    ##     10 55519072.8295             nan     0.1000 10128.0201
    ##     20 54528609.8743             nan     0.1000 -173113.4875
    ##     40 53563173.6873             nan     0.1000 -204945.7332
    ##     60 52436536.2084             nan     0.1000 -323001.4456
    ##     80 51501270.9177             nan     0.1000 -93175.7148
    ##    100 50790869.8121             nan     0.1000 -163809.5478
    ##    120 50500903.6088             nan     0.1000 -298080.8507
    ##    140 49906234.3138             nan     0.1000 -9521.6333
    ##    160 49511123.9791             nan     0.1000 -301202.4044
    ##    180 49021032.2637             nan     0.1000 -30147.1062
    ##    200 48441548.2416             nan     0.1000 -273692.6908
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 57393943.0358             nan     0.1000 45761.6296
    ##      2 56899296.0284             nan     0.1000 -81569.7634
    ##      3 56748609.5352             nan     0.1000 87347.1600
    ##      4 56373486.9180             nan     0.1000 -120393.7695
    ##      5 55900498.0312             nan     0.1000 123472.8723
    ##      6 55395047.9887             nan     0.1000 -100049.0730
    ##      7 55314823.6899             nan     0.1000 -85673.8119
    ##      8 55194676.2821             nan     0.1000 20486.9526
    ##      9 54943016.3618             nan     0.1000 -76945.3134
    ##     10 54812849.1194             nan     0.1000 -105860.5082
    ##     20 53372801.8990             nan     0.1000 -110062.7657
    ##     40 52004859.3405             nan     0.1000 -177974.4683
    ##     60 50766549.7530             nan     0.1000 -214090.2626
    ##     80 50403274.6719             nan     0.1000 -178148.9974
    ##    100 49534188.1775             nan     0.1000 -115647.9780
    ##    120 49420036.8005             nan     0.1000 -124624.0649
    ##    140 48737801.0347             nan     0.1000 -97249.6993
    ##    160 47628454.3880             nan     0.1000 -165046.6882
    ##    180 46992004.5863             nan     0.1000 -141918.4826
    ##    200 46769241.6046             nan     0.1000 -97612.7237
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 56969894.2745             nan     0.1000 91997.5539
    ##      2 56719913.3574             nan     0.1000 68369.1190
    ##      3 56101961.2801             nan     0.1000 -35691.0459
    ##      4 55714114.3931             nan     0.1000 -81205.9254
    ##      5 55536166.2227             nan     0.1000 -39845.4247
    ##      6 55245069.0493             nan     0.1000 -78603.9447
    ##      7 54708886.3555             nan     0.1000 14584.2816
    ##      8 54179877.0397             nan     0.1000 -14522.7637
    ##      9 53882442.2323             nan     0.1000 -128518.9308
    ##     10 53736761.9680             nan     0.1000 -151185.3964
    ##     20 51827839.3510             nan     0.1000 110086.4083
    ##     40 49956814.8681             nan     0.1000 -141513.3082
    ##     60 48320627.4983             nan     0.1000 -172622.6449
    ##     80 47593025.8531             nan     0.1000 -131500.5954
    ##    100 46955264.0779             nan     0.1000 -121433.3437
    ##    120 46230042.4567             nan     0.1000 -328954.1674
    ##    140 45682192.3906             nan     0.1000 -153691.5978
    ##    160 44516626.8393             nan     0.1000 -10432.3915
    ##    180 43791418.5691             nan     0.1000 -141875.1953
    ##    200 43525435.0555             nan     0.1000 -238752.7077
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 98114217.9460             nan     0.1000 163181.7680
    ##      2 97955235.8039             nan     0.1000 127540.0062
    ##      3 97494397.0799             nan     0.1000 -35851.2990
    ##      4 97397979.0544             nan     0.1000 -41096.2594
    ##      5 97284346.1754             nan     0.1000 58447.8560
    ##      6 97037642.7628             nan     0.1000 -91769.5073
    ##      7 96957970.2244             nan     0.1000 16070.0210
    ##      8 96727165.1997             nan     0.1000 -269243.9338
    ##      9 96595211.6077             nan     0.1000 32952.3671
    ##     10 96617395.9836             nan     0.1000 -128652.7314
    ##     20 96138036.0065             nan     0.1000 -115958.1191
    ##     25 96128352.1269             nan     0.1000 -131336.1545

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

    ## [1] "22913.75"                      "Full Linear Regression Model" 
    ## [3] "22871.87"                      "Curvi-Linear Regression Model"
    ## [5] "23008.14"                      "Random Forest Model"          
    ## [7] "22867.62"                      "Boosted Tree Model"

``` r
paste("The", RMSE_mods[which(RMSE_mods==min(RMSE_mods))+1], "had the smallest RMSE of", RMSE_mods[which(RMSE_mods==min(RMSE_mods))])
```

    ## [1] "The Boosted Tree Model had the smallest RMSE of 22867.62"
