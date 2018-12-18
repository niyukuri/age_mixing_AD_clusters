# Wrapper function which source all others and the main computatinal complete.master.epic.metric.class.phylo.features.cov script 
# which is the master model

# Define directory

# work.dir <- "/home/niyukuri/Desktop/mastermodeltest" # on laptop


work.dir <- "/home/dniyukuri/lustre/CHPCtestParallel" # on CHPC


setwd(paste0(work.dir))

#work.dir <- "/user/data/gent/vsc400/vsc40070/phylo/" # on cluster


pacman::p_load(snow, parallel, RSimpactCyan, RSimpactHelper, ape, Rsamtools)


wrapper.master.epic.metrics <- function(inputvector = inputvector){
  
  
  work.dir <- "/home/dniyukuri/lustre/CHPCtestParallel" # on CHPC

  # work.dir <- "/home/niyukuri/Desktop/mastermodeltest" # on laptop
  
  
  setwd(paste0(work.dir))
  
  library(RSimpactCyan)
  library(RSimpactHelper)
  library(Rcpp)
  library(ape)
  library(expoTree)
  library(data.table)
  library(readr)
  library(phangorn)
  library(lme4)
  library(nlme)
  library(dplyr)
  library(adephylo)
  library(treedater)
  library(geiger)
  library(picante)
  library(igraph)
  library(phyloTop)
  library(phytools)
  library(Rsamtools)
  library(robustbase)
  library(intergraph)
  library(lubridate)
  library(tidyr)
   
    source("/home/dniyukuri/lustre/CHPCtestParallel/master.epic.metrics.R")
  source("/home/dniyukuri/lustre/CHPCtestParallel/complete.master.epic.metric.R")
  
  
  
results.f <- tryCatch(complete.master.epic.metric(inputvector = inputvector),
                      error=function(e) return(rep(NA, 39)))


return(results.f)


}



# reps <- 168



inputvector <- c(-0.52, -0.05, 2, 10, 5, 0.25, -0.3, -0.1,
                 -1, -90, 0.5, 0.05, -0.14, 5, 7, 12, -1.7) 

epi.stats <- wrapper.master.epic.metrics(inputvector = inputvector)

write.csv(epi.stats, file = "Results.test.Parallel.csv")


# inputmatrix <- matrix(rep(inputvector, reps), byrow = TRUE, nrow = reps)


# epi.stats <- simpact.parallel(model = wrapper.master.epic.metrics,
#                                 actual.input.matrix = inputmatrix,
#                                 seed_count = 111,
#                                 n_cluster = 56)

# write.csv(epi.stats, file = "Results.test.Parallel.csv")


