old <- Sys.time() 
#load packages
library(data.table)
library(parallel)
library(doParallel)
library(foreach)
library(devtools)
load_all(".")

#library(TwitterABM)

args=commandArgs(trailingOnly = TRUE) 
expename=args[1]
nit=args[2]
niter=args[3]
ncores=args[4]
fi=0
mainfold=file.path("data/abm_output" ,expename)
fold=mainfold
while(file.exists(fold)){
    fold=paste0(mainfold,fi)
    fi=fi+1
    fold=paste0(mainfold,fi)
}
mainfold=fold


print(paste0("Simulations results will be stored in: ",fold))
dir.create(fold)

#load data and set parameters
N <- 341676
mu <- 0.4454465
load("data/overall_activity.RData")
load("data/obs_follower_counts.RData")
load("data/obs_activity_levels.RData")
load("data/obs_init_tweets.RData")

#reduce the skew of obs_follower_counts by removing anything above 100000
obs_follower_counts <- obs_follower_counts[-which(obs_follower_counts >= 100000)]

#register 50 cores for parallelization
cl <- makeForkCluster(ncores)
registerDoParallel(cl)

#for each of npar iterations
foreach(i = 1:nit) %dopar% {
  #set random seed
  set.seed(i)

  #set number of iterations to 2000
  iter <- niter

  #sample from priors
  priors <- data.table(cont_bias = runif(iter, min = 0, max = 8), dem_bias = runif(iter, min = 0, max = 8), freq_bias = runif(iter, min = 0, max = 8), age_dep = runif(iter, min = 0, max = 8))

  #run simulations and save output
  sum_stats <- lapply(1:iter, function(x){twitter_ABM(N = N, mu = mu, overall_activity = overall_activity, cont_bias = priors$cont_bias[x], dem_bias = priors$dem_bias[x], freq_bias = priors$freq_bias[x], age_dep = priors$age_dep[x], obs_follower_counts = obs_follower_counts, obs_activity_levels = obs_activity_levels, obs_init_tweets = obs_init_tweets)})
  sum_stats <- data.table(do.call(rbind, sum_stats))
  colnames(sum_stats) <- c("prop_rare", "prop_common", "hill_1", "hill_2")

  #structure and save the output
  simulations <- list(priors = priors, sum_stats = sum_stats)
  save(simulations, file = file.path(mainfold,paste0( "simulations_", i, ".RData")))
}
new <- Sys.time() - old # calculate difference
print(new) # print in nice format
