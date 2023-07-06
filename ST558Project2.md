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
will be utilized to help us predict the total number of shares.

# Reading in Data
