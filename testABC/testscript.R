# install.packages("SimInf")

library(SimInf)

library(devtools)

# install_github("wdelva/MABC")

library(MABC)


helper.sir <- function(design){ # one row of the design.df dataframe is c(id, b, g)
  library(SimInf)
  u0 <- data.frame(S = 99000,
                   I = 1000,
                   R = 0)
  tspan <- c(0, 50, 75)
  feature.vect <- rep(NA, 2)
  set.seed(design[1])
  model <- SIR(u0 = u0,
               tspan = tspan,
               beta = design[2],
               gamma = design[3])
  result <- run(model, threads = 1)
  feature.vect[1] <- result@U[2, 2]
  feature.vect[2] <- result@U[2, 3]
  return(feature.vect)
}

# Testing the simple single-node approach
model.parallel.run(model = helper.sir,
                   actual.input.matrix = matrix(c(0.2, 0.02),
                                                ncol = 2,
                                                nrow = 64,
                                                byrow =  TRUE),
                   seed_count = 0,
                   n_cores = 2)

# Testing the snow multi-node approach
model.snow.run(model = helper.sir,
               actual.input.matrix = matrix(c(0.2, 0.02),
                                            ncol = 2,
                                            nrow = 64,
                                            byrow =  TRUE),
               seed_count = 0,
               n_cores = 3)

# Testing the Rmpi multi-node approach
# Commented out because it hangs...
# model.mpi.run(model = helper.sir,
#                    actual.input.matrix = matrix(c(0.2, 0.02),
#                                                 ncol = 2,
#                                                 nrow = 64,
#                                                 byrow =  TRUE),
#                    seed_count = 0,
#                    n_cores = 3)


# Testing the ultimate use case: fitting a model to data (target features)

target.features.1a <- c(59290, 37292)

b.ll <- 0.01
b.ul <- 0.5
g.ll <- 0.001
g.ul <- 0.1

mice.impute.norm <- mice::mice.impute.norm
mice.impute.rf <- mice::mice.impute.rf

MaC.out <- MaC.weighted(targets.empirical = target.features.1a,
                        RMSD.tol.max = 2,
                        min.givetomice = 20,
                        n.experiments = 100,
                        lls = c(b.ll, g.ll),
                        uls = c(b.ul, g.ul),
                        model = helper.sir,
                        strict.positive.params = 0,
                        probability.params = 0,
                        inside_prior = TRUE,
                        method = "norm",
                        predictorMatrix = "complete",
                        maxit = 10,
                        maxwaves = 240,
                        n_cores = 24,
                        multinode = TRUE)

save(MaC.out, file = "MaC.out.RData")

# Only for local testing
# plot(MaC.out$selected.experiments[[length(MaC.out$selected.experiments)]][ , 1:2])

