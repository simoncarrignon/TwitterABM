% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twitter_ABM.R
\name{twitter_ABM}
\alias{twitter_ABM}
\title{Agent-based model}
\usage{
twitter_ABM(
  N = 1000,
  overall_activity,
  mu = 0.3,
  cont_bias = 0,
  dem_bias = 0,
  freq_bias = 1,
  age_dep = 1,
  obs_follower_counts,
  obs_activity_levels,
  obs_init_tweets,
  sum_stats_TF = TRUE,
  diversity_TF = FALSE
)
}
\arguments{
\item{N}{Overall population size.}

\item{overall_activity}{A vector of the total of number of tweets and retweets in each timestep. The length of this vector is used to determine the number of timesteps.}

\item{mu}{Probability of writing an original tweet as opposed to retweeting.}

\item{cont_bias}{Variation in the salience of the attractiveness of content (only positive values, where 0 is neutrality).}

\item{dem_bias}{Variation in the salience of the follower count (only positive values, where 0 is neutrality).}

\item{freq_bias}{Level of frequency bias (only positive values, where < 1 is novelty and > is conformity).}

\item{age_dep}{Rate of decay in age-dependent selection.}

\item{obs_follower_counts}{A vector of the observed follower counts, randomly drawn for the simulated users.}

\item{obs_activity_levels}{A vector of the observed activity levels, randomly drawn for the simulated users.}

\item{obs_init_tweets}{A vector of the observed retweet frequencies from the first timestep, randomly drawn to initialize the tweet data table.}

\item{sum_stats_TF}{Whether you want to simplify the raw data to the following summary statistics: (1) the proportion of tweets that only appear once, (2) the proportion of the most common tweet, (3) the Hill number when q = 1 (which emphasizes more rare tweets), and (4) the Hill number when q = 2 (which emphasizes more common tweets) (TRUE/FALSE).}

\item{diversity_TF}{Whether you want to return the Simpson's diversity index from each timepoint (TRUE/FALSE).}
}
\value{
Returns one object or a list of two objects, depending on the values for sum_stats_TF and diversity_TF.
}
\description{
An agent-based model (ABM) of cultural transmission on Twitter that incorporates content bias, frequency bias, demonstrator bias, and the level of age-dependent selection.
}
