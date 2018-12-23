

# Setting working directory

setwd("~/age_mixing_AD_clusters/")




# Clean working encironment

rm(list=ls())


# Loading libraries -------------------------------------------------------



library(dplyr)
library(ggplot2)



# Functions for GoF meausurement ------------------------------------------


# Function for Average Root Mean Squared Error
ARMSE <- function(v1=v1, v2=v2) {
  d <- data.frame(v1,v2)
  r <- na.omit(d)
  y1 <- r[,1]
  y2 <- r[,2]
  error.na <- (y1-y2)/y2
  armse <- sqrt(mean(error.na^2)) 
  return(armse)}



# Function for Root Mean Squared Error
RMSE <- function(error) {
  error <- as.numeric(na.omit(error))
  rmse <- sqrt(mean(error^2)) 
  return(rmse)}

# Function for Mean Absolute Error
MAE <- function(error) { 
  error <- as.numeric(na.omit(error))
  mae <- mean(abs(error)) 
  return(mae)
}


# Function for Mean Realative Error
MRE <- function(error) { 
  error <- as.numeric(na.omit(error))
  mae <- mean(error) 
  return(mae)
}



# Function to summarised outputs ------------------------------------------


# library(gmodels) # no more use of ci()

quant.med <- function(input){
  
  input <- na.omit(input)
  
  quantirles.v <- quantile(input, probs = seq(0, 1, 0.25))
  
  quantirles.25.50.75 <- as.numeric(quantirles.v)[2:4]
  
  return(quantirles.25.50.75)
  
}




# Reading simulation ouput ------------------------------------------------


# dr = read.csv("Results.mcarmar.large.AD.csv")

dr1 = read.csv("/home/david/age_mixing_AD_clusters/Results.mcarmar.large.AD_280_111.csv")
dr2 = read.csv("/home/david/age_mixing_AD_clusters/Results.mcarmar.large.AD_280_777.csv")

dr1.names <- names(dr1)

# dr = rbind(dr1, dr2)

# dr = na.omit(dr)

dr1.1 <- as.matrix(dr1)

dr <- dr1

# Population level epidemic metrics ---------------------------------------



# Population level epidemic stats and age mix characteristics

dr.epi.rels.agemix <- dr %>% 
  select(starts_with("R.")) 

# prevalence

dr.prev <- dr.epi.rels.agemix %>% 
  select(starts_with("R.prev.")) 


# incidence

dr.incid <- dr.epi.rels.agemix %>% 
  select(starts_with("R.inc."))


# Population level age mix characteristics

dr.agemixstat <- dr.epi.rels.agemix %>% 
  select("R.AAD.male", "R.SDAD.male",  "R.slope.male",  "R.WSD.male", "R.BSD.male" , "R.intercept.male" )



# True output at 100% coverage --------------------------------------------------


dr.cov.100 <- dr %>% 
  select(starts_with("cov.100")) 


# MCAR simulation output --------------------------------------------------




# MAR

d.MAR <- dr %>%
  select(contains("MAR"))


d.MCAR.cov.35 <- d.MAR %>%
  select(contains("cov.MAR.c.35.")) 
d.MCAR.cov.40 <- d.MAR %>%
  select(contains("cov.MAR.c.40.")) 
d.MCAR.cov.45 <- d.MAR %>%
  select(contains("cov.MAR.c.45.")) 
d.MCAR.cov.50 <- d.MAR %>%
  select(contains("cov.MAR.c.50.")) 
d.MCAR.cov.55 <- d.MAR %>%
  select(contains("cov.MAR.c.55.")) 
d.MCAR.cov.60 <- d.MAR %>%
  select(contains("cov.MAR.c.60.")) 
d.MCAR.cov.65 <- d.MAR %>%
  select(contains("cov.MAR.c.65.")) 
d.MCAR.cov.70 <- d.MAR %>%
  select(contains("cov.MAR.c.70.")) 
d.MCAR.cov.75 <- d.MAR %>%
  select(contains("cov.MAR.c.75.")) 
d.MCAR.cov.80 <- d.MAR %>%
  select(contains("cov.MAR.c.80.")) 
d.MCAR.cov.85 <- d.MAR %>%
  select(contains("cov.MAR.c.85.")) 
d.MCAR.cov.90 <- d.MAR %>%
  select(contains("cov.MAR.c.90.")) 
d.MCAR.cov.95 <- d.MAR %>%
  select(contains("cov.MAR.c.95.")) 
# Age mixing statistics in transmission networks --------------------------



# True age mix in different age mix scenarios

dr.trans.agemix.MCAR.cov.35 <- d.MCAR.cov.35 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.40 <- d.MCAR.cov.40 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.45 <- d.MCAR.cov.45 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.50 <- d.MCAR.cov.50 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.55 <- d.MCAR.cov.55 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.60 <- d.MCAR.cov.60 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.65 <- d.MCAR.cov.65 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.70 <- d.MCAR.cov.70 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.75 <- d.MCAR.cov.75 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.80 <- d.MCAR.cov.80 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.85 <- d.MCAR.cov.85 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.90 <- d.MCAR.cov.90 %>% 
  select(contains(".T."))
dr.trans.agemix.MCAR.cov.95 <- d.MCAR.cov.95 %>% 
  select(contains(".T."))




# Statistics of transmission clusters -------------------------------------




# Cluster sizes

dr.cl.size.MCAR.cov.35 <- d.MCAR.cov.35 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.40 <- d.MCAR.cov.40 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.45 <- d.MCAR.cov.45 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.50 <- d.MCAR.cov.50 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.55 <- d.MCAR.cov.55 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.60 <- d.MCAR.cov.60 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.65 <- d.MCAR.cov.65 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.70 <- d.MCAR.cov.70 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.75 <- d.MCAR.cov.75 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.80 <- d.MCAR.cov.80 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.85 <- d.MCAR.cov.85 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.90 <- d.MCAR.cov.90 %>% 
  select(contains("cl.size"))
dr.cl.size.MCAR.cov.95 <- d.MCAR.cov.95 %>% 
  select(contains("cl.size"))





# Age mixing meausrements inferred in transmission clusters -----------------




# .cl.prop.men
# .cl.prop.women
# .cl.true.prop.men
# .cl.true.prop.women
# .tree.trans.true.prop.men
# .tree.trans.true.prop.women




# cov 35

d.MCAR.cov.35.cl.prop.men <-  d.MCAR.cov.35 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.35.cl.prop.women <-  d.MCAR.cov.35 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.35.cl.true.prop.men <-  d.MCAR.cov.35 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.35.cl.true.prop.women <-  d.MCAR.cov.35 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.35.tree.trans.true.prop.men <-  d.MCAR.cov.35 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.35.tree.trans.true.prop.women <-  d.MCAR.cov.35 %>%
  select(contains(".tree.trans.true.prop.women")) 

# cov 40

d.MCAR.cov.40.cl.prop.men <-  d.MCAR.cov.40 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.40.cl.prop.women <-  d.MCAR.cov.40 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.40.cl.true.prop.men <-  d.MCAR.cov.40 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.40.cl.true.prop.women <-  d.MCAR.cov.40 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.40.tree.trans.true.prop.men <-  d.MCAR.cov.40 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.40.tree.trans.true.prop.women <-  d.MCAR.cov.40 %>%
  select(contains(".tree.trans.true.prop.women")) 

# cov 45

d.MCAR.cov.45.cl.prop.men <-  d.MCAR.cov.45 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.45.cl.prop.women <-  d.MCAR.cov.45 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.45.cl.true.prop.men <-  d.MCAR.cov.45 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.45.cl.true.prop.women <-  d.MCAR.cov.45 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.45.tree.trans.true.prop.men <-  d.MCAR.cov.45 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.45.tree.trans.true.prop.women <-  d.MCAR.cov.45 %>%
  select(contains(".tree.trans.true.prop.women")) 


# cov 50

d.MCAR.cov.50.cl.prop.men <-  d.MCAR.cov.50 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.50.cl.prop.women <-  d.MCAR.cov.50 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.50.cl.true.prop.men <-  d.MCAR.cov.50 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.50.cl.true.prop.women <-  d.MCAR.cov.50 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.50.tree.trans.true.prop.men <-  d.MCAR.cov.50 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.50.tree.trans.true.prop.women <-  d.MCAR.cov.50 %>%
  select(contains(".tree.trans.true.prop.women")) 

# cov 55

d.MCAR.cov.55.cl.prop.men <-  d.MCAR.cov.55 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.55.cl.prop.women <-  d.MCAR.cov.55 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.55.cl.true.prop.men <-  d.MCAR.cov.55 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.55.cl.true.prop.women <-  d.MCAR.cov.55 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.55.tree.trans.true.prop.men <-  d.MCAR.cov.55 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.55.tree.trans.true.prop.women <-  d.MCAR.cov.55 %>%
  select(contains(".tree.trans.true.prop.women")) 

# cov 60

d.MCAR.cov.60.cl.prop.men <-  d.MCAR.cov.60 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.60.cl.prop.women <-  d.MCAR.cov.60 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.60.cl.true.prop.men <-  d.MCAR.cov.60 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.60.cl.true.prop.women <-  d.MCAR.cov.60 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.60.tree.trans.true.prop.men <-  d.MCAR.cov.60 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.60.tree.trans.true.prop.women <-  d.MCAR.cov.60 %>%
  select(contains(".tree.trans.true.prop.women")) 

# 65

d.MCAR.cov.65.cl.prop.men <-  d.MCAR.cov.65 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.65.cl.prop.women <-  d.MCAR.cov.65 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.65.cl.true.prop.men <-  d.MCAR.cov.65 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.65.cl.true.prop.women <-  d.MCAR.cov.65 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.65.tree.trans.true.prop.men <-  d.MCAR.cov.65 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.65.tree.trans.true.prop.women <-  d.MCAR.cov.65 %>%
  select(contains(".tree.trans.true.prop.women")) 

# 70

d.MCAR.cov.70.cl.prop.men <-  d.MCAR.cov.70 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.70.cl.prop.women <-  d.MCAR.cov.70 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.70.cl.true.prop.men <-  d.MCAR.cov.70 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.70.cl.true.prop.women <-  d.MCAR.cov.70 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.70.tree.trans.true.prop.men <-  d.MCAR.cov.70 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.70.tree.trans.true.prop.women <-  d.MCAR.cov.70 %>%
  select(contains(".tree.trans.true.prop.women")) 

# 75

d.MCAR.cov.75.cl.prop.men <-  d.MCAR.cov.75 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.75.cl.prop.women <-  d.MCAR.cov.75 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.75.cl.true.prop.men <-  d.MCAR.cov.75 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.75.cl.true.prop.women <-  d.MCAR.cov.75 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.75.tree.trans.true.prop.men <-  d.MCAR.cov.75 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.75.tree.trans.true.prop.women <-  d.MCAR.cov.75 %>%
  select(contains(".tree.trans.true.prop.women")) 


# cov 80

d.MCAR.cov.80.cl.prop.men <-  d.MCAR.cov.80 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.80.cl.prop.women <-  d.MCAR.cov.80 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.80.cl.true.prop.men <-  d.MCAR.cov.80 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.80.cl.true.prop.women <-  d.MCAR.cov.80 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.80.tree.trans.true.prop.men <-  d.MCAR.cov.80 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.80.tree.trans.true.prop.women <-  d.MCAR.cov.80 %>%
  select(contains(".tree.trans.true.prop.women")) 

# cov 85

d.MCAR.cov.85.cl.prop.men <-  d.MCAR.cov.85 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.85.cl.prop.women <-  d.MCAR.cov.85 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.85.cl.true.prop.men <-  d.MCAR.cov.85 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.85.cl.true.prop.women <-  d.MCAR.cov.85 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.85.tree.trans.true.prop.men <-  d.MCAR.cov.85 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.85.tree.trans.true.prop.women <-  d.MCAR.cov.85 %>%
  select(contains(".tree.trans.true.prop.women")) 

# cov 90

d.MCAR.cov.90.cl.prop.men <-  d.MCAR.cov.90 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.90.cl.prop.women <-  d.MCAR.cov.90 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.90.cl.true.prop.men <-  d.MCAR.cov.90 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.90.cl.true.prop.women <-  d.MCAR.cov.90 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.90.tree.trans.true.prop.men <-  d.MCAR.cov.90 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.90.tree.trans.true.prop.women <-  d.MCAR.cov.90 %>%
  select(contains(".tree.trans.true.prop.women")) 


# cov 95


d.MCAR.cov.95.cl.prop.men <-  d.MCAR.cov.95 %>%
  select(contains(".cl.prop.men")) # proportion of pairings inferred from transmission clusters

d.MCAR.cov.95.cl.prop.women <-  d.MCAR.cov.95 %>%
  select(contains(".cl.prop.women")) 

d.MCAR.cov.95.cl.true.prop.men <-  d.MCAR.cov.95 %>%
  select(contains(".cl.true.prop.men"))  # true proportion of pairings in transmission clusters

d.MCAR.cov.95.cl.true.prop.women <-  d.MCAR.cov.95 %>%
  select(contains(".cl.true.prop.women")) 

d.MCAR.cov.95.tree.trans.true.prop.men <-  d.MCAR.cov.95 %>%
  select(contains(".tree.trans.true.prop.men"))  # true proportion of pairings in the full phylogenetic tree

d.MCAR.cov.95.tree.trans.true.prop.women <-  d.MCAR.cov.95 %>%
  select(contains(".tree.trans.true.prop.women")) 




# cov 100 and true


d.MCAR.cov.100.prop.men <-  dr.cov.100 %>%
  select(contains("true.prop.men")) # true proportion of pairings inferred 

d.MCAR.cov.100.prop.women <-  dr.cov.100 %>%
  select(contains("true.prop.women")) 



# Tables of aggregated number of pairings --------------------------------------------


M.15.25.F.15.25.cov.100 <- quant.med(dr.cov.100$cov.100.M.15.25.F.15.25)
M.25.40.F.15.25.cov.100 <- quant.med(dr.cov.100$cov.100.M.25.40.F.15.25)
M.40.50.F.15.25.cov.100 <- quant.med(dr.cov.100$cov.100.M.40.50.F.15.25)

M.15.25.F.25.40.cov.100 <- quant.med(dr.cov.100$cov.100.M.15.25.F.25.40)
M.25.40.F.25.40.cov.100 <- quant.med(dr.cov.100$cov.100.M.25.40.F.25.40)
M.40.50.F.25.40.cov.100 <- quant.med(dr.cov.100$cov.100.M.40.50.F.25.40)

M.15.25.F.40.50.cov.100 <- quant.med(dr.cov.100$cov.100.M.15.25.F.40.50)
M.25.40.F.40.50.cov.100 <- quant.med(dr.cov.100$cov.100.M.25.40.F.40.50)
M.40.50.F.40.50.cov.100 <- quant.med(dr.cov.100$cov.100.M.40.50.F.40.50)



cov.100.age.groups.table <- matrix(c((M.15.25.F.15.25.cov.100[2]), (M.15.25.F.25.40.cov.100[2]), (M.15.25.F.40.50.cov.100[2]),
                                     (M.25.40.F.15.25.cov.100[2]), (M.25.40.F.25.40.cov.100[2]), (M.25.40.F.40.50.cov.100[2]),
                                     (M.40.50.F.15.25.cov.100[2]), (M.40.50.F.25.40.cov.100[2]), (M.40.50.F.40.50.cov.100[2])),
                                   ncol = 3,
                                   byrow = TRUE)

colnames(cov.100.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(cov.100.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")



# CI.cov.100.age.groups.table <- matrix(c(paste(M.15.25.F.15.25.cov.100[2], "[", M.15.25.F.15.25.cov.100[1], "-", M.15.25.F.15.25.cov.100[3], "]"), 
#                                         paste(M.15.25.F.25.40.cov.100[2], "[", M.15.25.F.25.40.cov.100[1], "-", M.15.25.F.25.40.cov.100[3], "]"), 
#                                         paste(M.15.25.F.40.50.cov.100[2], "[", M.15.25.F.40.50.cov.100[1], "-", M.15.25.F.25.40.cov.100[3], "]"),
#                                         paste(M.25.40.F.15.25.cov.100[2], "[", M.25.40.F.15.25.cov.100[1], "-", M.25.40.F.15.25.cov.100[3], "]"), 
#                                         paste(M.25.40.F.25.40.cov.100[2], "[", M.25.40.F.25.40.cov.100[1], "-", M.25.40.F.25.40.cov.100[3], "]"), 
#                                         paste(M.25.40.F.40.50.cov.100[2], "[", M.25.40.F.40.50.cov.100[1], "-", M.25.40.F.40.50.cov.100[3], "]"),
#                                         paste(M.40.50.F.15.25.cov.100[2], "[", M.40.50.F.15.25.cov.100[1], "-", M.40.50.F.15.25.cov.100[3], "]"), 
#                                         paste(M.40.50.F.25.40.cov.100[2], "[", M.40.50.F.25.40.cov.100[1], "-", M.40.50.F.25.40.cov.100[3], "]"), 
#                                         paste(M.40.50.F.40.50.cov.100[2], "[", M.40.50.F.40.50.cov.100[1], "-", M.40.50.F.40.50.cov.100[3], "]")),
#                                       ncol = 3,
#                                       byrow = TRUE)
# 
# colnames(CI.cov.100.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
# rownames(CI.cov.100.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")


# Cov 35

M.15.25.F.15.25.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.35 <- quant.med(d.MCAR.cov.35$cov.MCAR.35.cl.M.40.50.F.40.50)



MCAR.cov.35.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.35[2]), (M.15.25.F.25.40.MCAR.cov.35[2]), (M.15.25.F.40.50.MCAR.cov.35[2]),
                                            (M.25.40.F.15.25.MCAR.cov.35[2]), (M.25.40.F.25.40.MCAR.cov.35[2]), (M.25.40.F.40.50.MCAR.cov.35[2]),
                                            (M.40.50.F.15.25.MCAR.cov.35[2]), (M.40.50.F.25.40.MCAR.cov.35[2]), (M.40.50.F.40.50.MCAR.cov.35[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.35.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.35.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")



# Cov 40

M.15.25.F.15.25.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.40 <- quant.med(d.MCAR.cov.40$cov.MCAR.40.cl.M.40.50.F.40.50)



MCAR.cov.40.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.40[2]), (M.15.25.F.25.40.MCAR.cov.40[2]), (M.15.25.F.40.50.MCAR.cov.40[2]),
                                            (M.25.40.F.15.25.MCAR.cov.40[2]), (M.25.40.F.25.40.MCAR.cov.40[2]), (M.25.40.F.40.50.MCAR.cov.40[2]),
                                            (M.40.50.F.15.25.MCAR.cov.40[2]), (M.40.50.F.25.40.MCAR.cov.40[2]), (M.40.50.F.40.50.MCAR.cov.40[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.40.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.40.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")



# Cov 45

M.15.25.F.15.25.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.45 <- quant.med(d.MCAR.cov.45$cov.MCAR.45.cl.M.40.50.F.40.50)



MCAR.cov.45.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.45[2]), (M.15.25.F.25.40.MCAR.cov.45[2]), (M.15.25.F.40.50.MCAR.cov.45[2]),
                                            (M.25.40.F.15.25.MCAR.cov.45[2]), (M.25.40.F.25.40.MCAR.cov.45[2]), (M.25.40.F.40.50.MCAR.cov.45[2]),
                                            (M.40.50.F.15.25.MCAR.cov.45[2]), (M.40.50.F.25.40.MCAR.cov.45[2]), (M.40.50.F.40.50.MCAR.cov.45[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.45.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.45.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")

# Cov 50

M.15.25.F.15.25.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.50 <- quant.med(d.MCAR.cov.50$cov.MCAR.50.cl.M.40.50.F.40.50)



MCAR.cov.50.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.50[2]), (M.15.25.F.25.40.MCAR.cov.50[2]), (M.15.25.F.40.50.MCAR.cov.50[2]),
                                            (M.25.40.F.15.25.MCAR.cov.50[2]), (M.25.40.F.25.40.MCAR.cov.50[2]), (M.25.40.F.40.50.MCAR.cov.50[2]),
                                            (M.40.50.F.15.25.MCAR.cov.50[2]), (M.40.50.F.25.40.MCAR.cov.50[2]), (M.40.50.F.40.50.MCAR.cov.50[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.50.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.50.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")


# Cov 55

M.15.25.F.15.25.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.55 <- quant.med(d.MCAR.cov.55$cov.MCAR.55.cl.M.40.50.F.40.50)



MCAR.cov.55.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.55[2]), (M.15.25.F.25.40.MCAR.cov.55[2]), (M.15.25.F.40.50.MCAR.cov.55[2]),
                                            (M.25.40.F.15.25.MCAR.cov.55[2]), (M.25.40.F.25.40.MCAR.cov.55[2]), (M.25.40.F.40.50.MCAR.cov.55[2]),
                                            (M.40.50.F.15.25.MCAR.cov.55[2]), (M.40.50.F.25.40.MCAR.cov.55[2]), (M.40.50.F.40.50.MCAR.cov.55[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.55.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.55.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")

# Cov 60

M.15.25.F.15.25.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.60 <- quant.med(d.MCAR.cov.60$cov.MCAR.60.cl.M.40.50.F.40.50)



MCAR.cov.60.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.60[2]), (M.15.25.F.25.40.MCAR.cov.60[2]), (M.15.25.F.40.50.MCAR.cov.60[2]),
                                            (M.25.40.F.15.25.MCAR.cov.60[2]), (M.25.40.F.25.40.MCAR.cov.60[2]), (M.25.40.F.40.50.MCAR.cov.60[2]),
                                            (M.40.50.F.15.25.MCAR.cov.60[2]), (M.40.50.F.25.40.MCAR.cov.60[2]), (M.40.50.F.40.50.MCAR.cov.60[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.60.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.60.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")


# Cov 65

M.15.25.F.15.25.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.65 <- quant.med(d.MCAR.cov.65$cov.MCAR.65.cl.M.40.50.F.40.50)



MCAR.cov.65.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.65[2]), (M.15.25.F.25.40.MCAR.cov.65[2]), (M.15.25.F.40.50.MCAR.cov.65[2]),
                                            (M.25.40.F.15.25.MCAR.cov.65[2]), (M.25.40.F.25.40.MCAR.cov.65[2]), (M.25.40.F.40.50.MCAR.cov.65[2]),
                                            (M.40.50.F.15.25.MCAR.cov.65[2]), (M.40.50.F.25.40.MCAR.cov.65[2]), (M.40.50.F.40.50.MCAR.cov.65[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.65.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.65.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")

# Cov 70

M.15.25.F.15.25.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.70 <- quant.med(d.MCAR.cov.70$cov.MCAR.70.cl.M.40.50.F.40.50)



MCAR.cov.70.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.70[2]), (M.15.25.F.25.40.MCAR.cov.70[2]), (M.15.25.F.40.50.MCAR.cov.70[2]),
                                            (M.25.40.F.15.25.MCAR.cov.70[2]), (M.25.40.F.25.40.MCAR.cov.70[2]), (M.25.40.F.40.50.MCAR.cov.70[2]),
                                            (M.40.50.F.15.25.MCAR.cov.70[2]), (M.40.50.F.25.40.MCAR.cov.70[2]), (M.40.50.F.40.50.MCAR.cov.70[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.70.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.70.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")

# Cov 75

M.15.25.F.15.25.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.75 <- quant.med(d.MCAR.cov.75$cov.MCAR.75.cl.M.40.50.F.40.50)



MCAR.cov.75.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.75[2]), (M.15.25.F.25.40.MCAR.cov.75[2]), (M.15.25.F.40.50.MCAR.cov.75[2]),
                                            (M.25.40.F.15.25.MCAR.cov.75[2]), (M.25.40.F.25.40.MCAR.cov.75[2]), (M.25.40.F.40.50.MCAR.cov.75[2]),
                                            (M.40.50.F.15.25.MCAR.cov.75[2]), (M.40.50.F.25.40.MCAR.cov.75[2]), (M.40.50.F.40.50.MCAR.cov.75[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.75.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.75.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")


# Cov 80

M.15.25.F.15.25.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.80 <- quant.med(d.MCAR.cov.80$cov.MCAR.80.cl.M.40.50.F.40.50)



MCAR.cov.80.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.80[2]), (M.15.25.F.25.40.MCAR.cov.80[2]), (M.15.25.F.40.50.MCAR.cov.80[2]),
                                            (M.25.40.F.15.25.MCAR.cov.80[2]), (M.25.40.F.25.40.MCAR.cov.80[2]), (M.25.40.F.40.50.MCAR.cov.80[2]),
                                            (M.40.50.F.15.25.MCAR.cov.80[2]), (M.40.50.F.25.40.MCAR.cov.80[2]), (M.40.50.F.40.50.MCAR.cov.80[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.80.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.80.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")


# Cov 85

M.15.25.F.15.25.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.85 <- quant.med(d.MCAR.cov.85$cov.MCAR.85.cl.M.40.50.F.40.50)



MCAR.cov.85.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.85[2]), (M.15.25.F.25.40.MCAR.cov.85[2]), (M.15.25.F.40.50.MCAR.cov.85[2]),
                                            (M.25.40.F.15.25.MCAR.cov.85[2]), (M.25.40.F.25.40.MCAR.cov.85[2]), (M.25.40.F.40.50.MCAR.cov.85[2]),
                                            (M.40.50.F.15.25.MCAR.cov.85[2]), (M.40.50.F.25.40.MCAR.cov.85[2]), (M.40.50.F.40.50.MCAR.cov.85[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.85.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.85.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")

# Cov 90

M.15.25.F.15.25.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.90 <- quant.med(d.MCAR.cov.90$cov.MCAR.90.cl.M.40.50.F.40.50)



MCAR.cov.90.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.90[2]), (M.15.25.F.25.40.MCAR.cov.90[2]), (M.15.25.F.40.50.MCAR.cov.90[2]),
                                            (M.25.40.F.15.25.MCAR.cov.90[2]), (M.25.40.F.25.40.MCAR.cov.90[2]), (M.25.40.F.40.50.MCAR.cov.90[2]),
                                            (M.40.50.F.15.25.MCAR.cov.90[2]), (M.40.50.F.25.40.MCAR.cov.90[2]), (M.40.50.F.40.50.MCAR.cov.90[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.90.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.90.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")

# Cov 95

M.15.25.F.15.25.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.15.25.F.15.25)
M.25.40.F.15.25.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.25.40.F.15.25)
M.40.50.F.15.25.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.40.50.F.15.25)

M.15.25.F.25.40.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.15.25.F.25.40)
M.25.40.F.25.40.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.25.40.F.25.40)
M.40.50.F.25.40.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.40.50.F.25.40)

M.15.25.F.40.50.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.15.25.F.40.50)
M.25.40.F.40.50.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.25.40.F.40.50)
M.40.50.F.40.50.MCAR.cov.95 <- quant.med(d.MCAR.cov.95$cov.MCAR.95.cl.M.40.50.F.40.50)



MCAR.cov.95.cl.age.groups.table <- matrix(c((M.15.25.F.15.25.MCAR.cov.95[2]), (M.15.25.F.25.40.MCAR.cov.95[2]), (M.15.25.F.40.50.MCAR.cov.95[2]),
                                            (M.25.40.F.15.25.MCAR.cov.95[2]), (M.25.40.F.25.40.MCAR.cov.95[2]), (M.25.40.F.40.50.MCAR.cov.95[2]),
                                            (M.40.50.F.15.25.MCAR.cov.95[2]), (M.40.50.F.25.40.MCAR.cov.95[2]), (M.40.50.F.40.50.MCAR.cov.95[2])),
                                          ncol = 3,
                                          byrow = TRUE)

colnames(MCAR.cov.95.cl.age.groups.table) <- c("Female.15.25", "Female.25.40", "Female.40.50")
rownames(MCAR.cov.95.cl.age.groups.table) <- c("Male.15.25", "Male.25.40", "Male.40.50")








# proportion of pairings inferred from transmission clusters


# Output analysis ---------------------------------------------------------

# Proportions of pairing of men/women across different age groups in three cases

# (i) transmission network built from transmission clusters
# (ii) true proportions in true transmission network of these individuals in transmission clusters
# (iii) true proportions in true transmission network of individuals in full phylogeny

# .cl.prop.men
# .cl.prop.women
# .cl.true.prop.men
# .cl.true.prop.women
# .tree.trans.true.prop.men
# .tree.trans.true.prop.women



# True proprotions at 100 coverage ------------------------------


# Vectors


vector.MCAR.true.cov.100.prop.men15.25.F.15.25 <- d.MCAR.cov.100.prop.men[,1]
vector.MCAR.true.cov.100.prop.women15.25.M.15.25 <- d.MCAR.cov.100.prop.women[,1]

vector.MCAR.true.cov.100.prop.men25.40.F.15.25 <- d.MCAR.cov.100.prop.men[,2]
vector.MCAR.true.cov.100.prop.women25.40.M.15.25 <- d.MCAR.cov.100.prop.women[,2]

vector.MCAR.true.cov.100.prop.men40.50.F.15.25 <- d.MCAR.cov.100.prop.men[,3]
vector.MCAR.true.cov.100.prop.women40.50.M.15.25 <- d.MCAR.cov.100.prop.women[,3]

vector.MCAR.true.cov.100.prop.men15.25.F.25.40 <- d.MCAR.cov.100.prop.men[,4]
vector.MCAR.true.cov.100.prop.women15.25.M.25.40 <- d.MCAR.cov.100.prop.women[,4]

vector.MCAR.true.cov.100.prop.men25.40.F.25.40 <- d.MCAR.cov.100.prop.men[,5]
vector.MCAR.true.cov.100.prop.women25.40.M.25.40 <- d.MCAR.cov.100.prop.women[,5]

vector.MCAR.true.cov.100.prop.men40.50.F.25.40 <- d.MCAR.cov.100.prop.men[,6]
vector.MCAR.true.cov.100.prop.women40.50.M.25.40 <- d.MCAR.cov.100.prop.women[,6]


vector.MCAR.true.cov.100.prop.men15.25.F.40.50 <- d.MCAR.cov.100.prop.men[,7]
vector.MCAR.true.cov.100.prop.women15.25.M.40.50 <- d.MCAR.cov.100.prop.women[,7]

vector.MCAR.true.cov.100.prop.men25.40.F.40.50 <- d.MCAR.cov.100.prop.men[,8]
vector.MCAR.true.cov.100.prop.women25.40.M.40.50 <- d.MCAR.cov.100.prop.women[,8]

vector.MCAR.true.cov.100.prop.men40.50.F.40.50 <- d.MCAR.cov.100.prop.men[,9]
vector.MCAR.true.cov.100.prop.women40.50.M.40.50 <- d.MCAR.cov.100.prop.women[,9]


# Summarrised

d.MCAR.true.cov.100.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.100.prop.men[,1])
d.MCAR.true.cov.100.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.100.prop.women[,1])

d.MCAR.true.cov.100.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.100.prop.men[,2])
d.MCAR.true.cov.100.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.100.prop.women[,2])

d.MCAR.true.cov.100.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.100.prop.men[,3])
d.MCAR.true.cov.100.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.100.prop.women[,3])

d.MCAR.true.cov.100.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.100.prop.men[,4])
d.MCAR.true.cov.100.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.100.prop.women[,4])

d.MCAR.true.cov.100.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.100.prop.men[,5])
d.MCAR.true.cov.100.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.100.prop.women[,5])

d.MCAR.true.cov.100.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.100.prop.men[,6])
d.MCAR.true.cov.100.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.100.prop.women[,6])


d.MCAR.true.cov.100.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.100.prop.men[,7])
d.MCAR.true.cov.100.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.100.prop.women[,7])

d.MCAR.true.cov.100.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.100.prop.men[,8])
d.MCAR.true.cov.100.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.100.prop.women[,8])

d.MCAR.true.cov.100.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.100.prop.men[,9])
d.MCAR.true.cov.100.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.100.prop.women[,9])




# I. Inference made in transmission clusters ------------------------------



# Cov 35


# Vector


vector.MCAR.cov.35.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.35.cl.prop.men[,1]
vector.MCAR.cov.35.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.35.cl.prop.women[,1]

vector.MCAR.cov.35.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.35.cl.prop.men[,2]
vector.MCAR.cov.35.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.35.cl.prop.women[,2]

vector.MCAR.cov.35.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.35.cl.prop.men[,3]
vector.MCAR.cov.35.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.35.cl.prop.women[,3]

vector.MCAR.cov.35.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.35.cl.prop.men[,4]
vector.MCAR.cov.35.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.35.cl.prop.women[,4]

vector.MCAR.cov.35.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.35.cl.prop.men[,5]
vector.MCAR.cov.35.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.35.cl.prop.women[,5]

vector.MCAR.cov.35.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.35.cl.prop.men[,6]
vector.MCAR.cov.35.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.35.cl.prop.women[,6]


vector.MCAR.cov.35.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.35.cl.prop.men[,7]
vector.MCAR.cov.35.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.35.cl.prop.women[,7]

vector.MCAR.cov.35.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.35.cl.prop.men[,8]
vector.MCAR.cov.35.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.35.cl.prop.women[,8]

vector.MCAR.cov.35.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.35.cl.prop.men[,9]
vector.MCAR.cov.35.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.35.cl.prop.women[,9]



# Summarised

d.MCAR.cov.35.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.35.cl.prop.men[,1])
d.MCAR.cov.35.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.35.cl.prop.women[,1])

d.MCAR.cov.35.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.35.cl.prop.men[,2])
d.MCAR.cov.35.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.35.cl.prop.women[,2])

d.MCAR.cov.35.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.35.cl.prop.men[,3])
d.MCAR.cov.35.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.35.cl.prop.women[,3])

d.MCAR.cov.35.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.35.cl.prop.men[,4])
d.MCAR.cov.35.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.35.cl.prop.women[,4])

d.MCAR.cov.35.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.35.cl.prop.men[,5])
d.MCAR.cov.35.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.35.cl.prop.women[,5])

d.MCAR.cov.35.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.35.cl.prop.men[,6])
d.MCAR.cov.35.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.35.cl.prop.women[,6])


d.MCAR.cov.35.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.35.cl.prop.men[,7])
d.MCAR.cov.35.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.35.cl.prop.women[,7])

d.MCAR.cov.35.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.35.cl.prop.men[,8])
d.MCAR.cov.35.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.35.cl.prop.women[,8])

d.MCAR.cov.35.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.35.cl.prop.men[,9])
d.MCAR.cov.35.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.35.cl.prop.women[,9])


# Cov 40


# Vector


vector.MCAR.cov.40.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.40.cl.prop.men[,1]
vector.MCAR.cov.40.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.40.cl.prop.women[,1]

vector.MCAR.cov.40.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.40.cl.prop.men[,2]
vector.MCAR.cov.40.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.40.cl.prop.women[,2]

vector.MCAR.cov.40.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.40.cl.prop.men[,3]
vector.MCAR.cov.40.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.40.cl.prop.women[,3]

vector.MCAR.cov.40.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.40.cl.prop.men[,4]
vector.MCAR.cov.40.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.40.cl.prop.women[,4]

vector.MCAR.cov.40.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.40.cl.prop.men[,5]
vector.MCAR.cov.40.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.40.cl.prop.women[,5]

vector.MCAR.cov.40.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.40.cl.prop.men[,6]
vector.MCAR.cov.40.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.40.cl.prop.women[,6]


vector.MCAR.cov.40.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.40.cl.prop.men[,7]
vector.MCAR.cov.40.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.40.cl.prop.women[,7]

vector.MCAR.cov.40.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.40.cl.prop.men[,8]
vector.MCAR.cov.40.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.40.cl.prop.women[,8]

vector.MCAR.cov.40.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.40.cl.prop.men[,9]
vector.MCAR.cov.40.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.40.cl.prop.women[,9]



# Summarised

d.MCAR.cov.40.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.40.cl.prop.men[,1])
d.MCAR.cov.40.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.40.cl.prop.women[,1])

d.MCAR.cov.40.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.40.cl.prop.men[,2])
d.MCAR.cov.40.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.40.cl.prop.women[,2])

d.MCAR.cov.40.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.40.cl.prop.men[,3])
d.MCAR.cov.40.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.40.cl.prop.women[,3])

d.MCAR.cov.40.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.40.cl.prop.men[,4])
d.MCAR.cov.40.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.40.cl.prop.women[,4])

d.MCAR.cov.40.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.40.cl.prop.men[,5])
d.MCAR.cov.40.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.40.cl.prop.women[,5])

d.MCAR.cov.40.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.40.cl.prop.men[,6])
d.MCAR.cov.40.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.40.cl.prop.women[,6])


d.MCAR.cov.40.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.40.cl.prop.men[,7])
d.MCAR.cov.40.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.40.cl.prop.women[,7])

d.MCAR.cov.40.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.40.cl.prop.men[,8])
d.MCAR.cov.40.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.40.cl.prop.women[,8])

d.MCAR.cov.40.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.40.cl.prop.men[,9])
d.MCAR.cov.40.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.40.cl.prop.women[,9])


# Cov 45


# Vector


vector.MCAR.cov.45.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.45.cl.prop.men[,1]
vector.MCAR.cov.45.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.45.cl.prop.women[,1]

vector.MCAR.cov.45.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.45.cl.prop.men[,2]
vector.MCAR.cov.45.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.45.cl.prop.women[,2]

vector.MCAR.cov.45.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.45.cl.prop.men[,3]
vector.MCAR.cov.45.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.45.cl.prop.women[,3]

vector.MCAR.cov.45.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.45.cl.prop.men[,4]
vector.MCAR.cov.45.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.45.cl.prop.women[,4]

vector.MCAR.cov.45.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.45.cl.prop.men[,5]
vector.MCAR.cov.45.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.45.cl.prop.women[,5]

vector.MCAR.cov.45.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.45.cl.prop.men[,6]
vector.MCAR.cov.45.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.45.cl.prop.women[,6]


vector.MCAR.cov.45.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.45.cl.prop.men[,7]
vector.MCAR.cov.45.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.45.cl.prop.women[,7]

vector.MCAR.cov.45.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.45.cl.prop.men[,8]
vector.MCAR.cov.45.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.45.cl.prop.women[,8]

vector.MCAR.cov.45.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.45.cl.prop.men[,9]
vector.MCAR.cov.45.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.45.cl.prop.women[,9]



# Summarised

d.MCAR.cov.45.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.45.cl.prop.men[,1])
d.MCAR.cov.45.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.45.cl.prop.women[,1])

d.MCAR.cov.45.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.45.cl.prop.men[,2])
d.MCAR.cov.45.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.45.cl.prop.women[,2])

d.MCAR.cov.45.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.45.cl.prop.men[,3])
d.MCAR.cov.45.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.45.cl.prop.women[,3])

d.MCAR.cov.45.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.45.cl.prop.men[,4])
d.MCAR.cov.45.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.45.cl.prop.women[,4])

d.MCAR.cov.45.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.45.cl.prop.men[,5])
d.MCAR.cov.45.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.45.cl.prop.women[,5])

d.MCAR.cov.45.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.45.cl.prop.men[,6])
d.MCAR.cov.45.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.45.cl.prop.women[,6])


d.MCAR.cov.45.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.45.cl.prop.men[,7])
d.MCAR.cov.45.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.45.cl.prop.women[,7])

d.MCAR.cov.45.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.45.cl.prop.men[,8])
d.MCAR.cov.45.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.45.cl.prop.women[,8])

d.MCAR.cov.45.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.45.cl.prop.men[,9])
d.MCAR.cov.45.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.45.cl.prop.women[,9])




# Cov 50


# Vector


vector.MCAR.cov.50.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.50.cl.prop.men[,1]
vector.MCAR.cov.50.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.50.cl.prop.women[,1]

vector.MCAR.cov.50.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.50.cl.prop.men[,2]
vector.MCAR.cov.50.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.50.cl.prop.women[,2]

vector.MCAR.cov.50.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.50.cl.prop.men[,3]
vector.MCAR.cov.50.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.50.cl.prop.women[,3]

vector.MCAR.cov.50.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.50.cl.prop.men[,4]
vector.MCAR.cov.50.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.50.cl.prop.women[,4]

vector.MCAR.cov.50.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.50.cl.prop.men[,5]
vector.MCAR.cov.50.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.50.cl.prop.women[,5]

vector.MCAR.cov.50.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.50.cl.prop.men[,6]
vector.MCAR.cov.50.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.50.cl.prop.women[,6]


vector.MCAR.cov.50.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.50.cl.prop.men[,7]
vector.MCAR.cov.50.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.50.cl.prop.women[,7]

vector.MCAR.cov.50.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.50.cl.prop.men[,8]
vector.MCAR.cov.50.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.50.cl.prop.women[,8]

vector.MCAR.cov.50.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.50.cl.prop.men[,9]
vector.MCAR.cov.50.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.50.cl.prop.women[,9]


# Summarised

d.MCAR.cov.50.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.50.cl.prop.men[,1])
d.MCAR.cov.50.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.50.cl.prop.women[,1])

d.MCAR.cov.50.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.50.cl.prop.men[,2])
d.MCAR.cov.50.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.50.cl.prop.women[,2])

d.MCAR.cov.50.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.50.cl.prop.men[,3])
d.MCAR.cov.50.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.50.cl.prop.women[,3])

d.MCAR.cov.50.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.50.cl.prop.men[,4])
d.MCAR.cov.50.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.50.cl.prop.women[,4])

d.MCAR.cov.50.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.50.cl.prop.men[,5])
d.MCAR.cov.50.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.50.cl.prop.women[,5])

d.MCAR.cov.50.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.50.cl.prop.men[,6])
d.MCAR.cov.50.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.50.cl.prop.women[,6])


d.MCAR.cov.50.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.50.cl.prop.men[,7])
d.MCAR.cov.50.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.50.cl.prop.women[,7])

d.MCAR.cov.50.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.50.cl.prop.men[,8])
d.MCAR.cov.50.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.50.cl.prop.women[,8])

d.MCAR.cov.50.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.50.cl.prop.men[,9])
d.MCAR.cov.50.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.50.cl.prop.women[,9])



# Cov 55


# Vector


vector.MCAR.cov.55.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.55.cl.prop.men[,1]
vector.MCAR.cov.55.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.55.cl.prop.women[,1]

vector.MCAR.cov.55.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.55.cl.prop.men[,2]
vector.MCAR.cov.55.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.55.cl.prop.women[,2]

vector.MCAR.cov.55.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.55.cl.prop.men[,3]
vector.MCAR.cov.55.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.55.cl.prop.women[,3]

vector.MCAR.cov.55.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.55.cl.prop.men[,4]
vector.MCAR.cov.55.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.55.cl.prop.women[,4]

vector.MCAR.cov.55.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.55.cl.prop.men[,5]
vector.MCAR.cov.55.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.55.cl.prop.women[,5]

vector.MCAR.cov.55.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.55.cl.prop.men[,6]
vector.MCAR.cov.55.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.55.cl.prop.women[,6]


vector.MCAR.cov.55.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.55.cl.prop.men[,7]
vector.MCAR.cov.55.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.55.cl.prop.women[,7]

vector.MCAR.cov.55.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.55.cl.prop.men[,8]
vector.MCAR.cov.55.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.55.cl.prop.women[,8]

vector.MCAR.cov.55.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.55.cl.prop.men[,9]
vector.MCAR.cov.55.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.55.cl.prop.women[,9]


# Summarised

d.MCAR.cov.55.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.55.cl.prop.men[,1])
d.MCAR.cov.55.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.55.cl.prop.women[,1])

d.MCAR.cov.55.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.55.cl.prop.men[,2])
d.MCAR.cov.55.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.55.cl.prop.women[,2])

d.MCAR.cov.55.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.55.cl.prop.men[,3])
d.MCAR.cov.55.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.55.cl.prop.women[,3])

d.MCAR.cov.55.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.55.cl.prop.men[,4])
d.MCAR.cov.55.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.55.cl.prop.women[,4])

d.MCAR.cov.55.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.55.cl.prop.men[,5])
d.MCAR.cov.55.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.55.cl.prop.women[,5])

d.MCAR.cov.55.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.55.cl.prop.men[,6])
d.MCAR.cov.55.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.55.cl.prop.women[,6])


d.MCAR.cov.55.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.55.cl.prop.men[,7])
d.MCAR.cov.55.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.55.cl.prop.women[,7])

d.MCAR.cov.55.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.55.cl.prop.men[,8])
d.MCAR.cov.55.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.55.cl.prop.women[,8])

d.MCAR.cov.55.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.55.cl.prop.men[,9])
d.MCAR.cov.55.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.55.cl.prop.women[,9])



# Cov 60


# Vector


vector.MCAR.cov.60.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.60.cl.prop.men[,1]
vector.MCAR.cov.60.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.60.cl.prop.women[,1]

vector.MCAR.cov.60.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.60.cl.prop.men[,2]
vector.MCAR.cov.60.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.60.cl.prop.women[,2]

vector.MCAR.cov.60.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.60.cl.prop.men[,3]
vector.MCAR.cov.60.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.60.cl.prop.women[,3]

vector.MCAR.cov.60.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.60.cl.prop.men[,4]
vector.MCAR.cov.60.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.60.cl.prop.women[,4]

vector.MCAR.cov.60.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.60.cl.prop.men[,5]
vector.MCAR.cov.60.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.60.cl.prop.women[,5]

vector.MCAR.cov.60.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.60.cl.prop.men[,6]
vector.MCAR.cov.60.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.60.cl.prop.women[,6]


vector.MCAR.cov.60.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.60.cl.prop.men[,7]
vector.MCAR.cov.60.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.60.cl.prop.women[,7]

vector.MCAR.cov.60.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.60.cl.prop.men[,8]
vector.MCAR.cov.60.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.60.cl.prop.women[,8]

vector.MCAR.cov.60.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.60.cl.prop.men[,9]
vector.MCAR.cov.60.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.60.cl.prop.women[,9]


# Summarised

d.MCAR.cov.60.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.60.cl.prop.men[,1])
d.MCAR.cov.60.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.60.cl.prop.women[,1])

d.MCAR.cov.60.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.60.cl.prop.men[,2])
d.MCAR.cov.60.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.60.cl.prop.women[,2])

d.MCAR.cov.60.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.60.cl.prop.men[,3])
d.MCAR.cov.60.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.60.cl.prop.women[,3])

d.MCAR.cov.60.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.60.cl.prop.men[,4])
d.MCAR.cov.60.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.60.cl.prop.women[,4])

d.MCAR.cov.60.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.60.cl.prop.men[,5])
d.MCAR.cov.60.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.60.cl.prop.women[,5])

d.MCAR.cov.60.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.60.cl.prop.men[,6])
d.MCAR.cov.60.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.60.cl.prop.women[,6])


d.MCAR.cov.60.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.60.cl.prop.men[,7])
d.MCAR.cov.60.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.60.cl.prop.women[,7])

d.MCAR.cov.60.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.60.cl.prop.men[,8])
d.MCAR.cov.60.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.60.cl.prop.women[,8])

d.MCAR.cov.60.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.60.cl.prop.men[,9])
d.MCAR.cov.60.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.60.cl.prop.women[,9])



# Cov 65


# Vector


vector.MCAR.cov.65.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.65.cl.prop.men[,1]
vector.MCAR.cov.65.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.65.cl.prop.women[,1]

vector.MCAR.cov.65.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.65.cl.prop.men[,2]
vector.MCAR.cov.65.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.65.cl.prop.women[,2]

vector.MCAR.cov.65.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.65.cl.prop.men[,3]
vector.MCAR.cov.65.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.65.cl.prop.women[,3]

vector.MCAR.cov.65.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.65.cl.prop.men[,4]
vector.MCAR.cov.65.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.65.cl.prop.women[,4]

vector.MCAR.cov.65.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.65.cl.prop.men[,5]
vector.MCAR.cov.65.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.65.cl.prop.women[,5]

vector.MCAR.cov.65.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.65.cl.prop.men[,6]
vector.MCAR.cov.65.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.65.cl.prop.women[,6]


vector.MCAR.cov.65.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.65.cl.prop.men[,7]
vector.MCAR.cov.65.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.65.cl.prop.women[,7]

vector.MCAR.cov.65.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.65.cl.prop.men[,8]
vector.MCAR.cov.65.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.65.cl.prop.women[,8]

vector.MCAR.cov.65.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.65.cl.prop.men[,9]
vector.MCAR.cov.65.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.65.cl.prop.women[,9]



# Summarised

d.MCAR.cov.65.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.65.cl.prop.men[,1])
d.MCAR.cov.65.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.65.cl.prop.women[,1])

d.MCAR.cov.65.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.65.cl.prop.men[,2])
d.MCAR.cov.65.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.65.cl.prop.women[,2])

d.MCAR.cov.65.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.65.cl.prop.men[,3])
d.MCAR.cov.65.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.65.cl.prop.women[,3])

d.MCAR.cov.65.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.65.cl.prop.men[,4])
d.MCAR.cov.65.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.65.cl.prop.women[,4])

d.MCAR.cov.65.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.65.cl.prop.men[,5])
d.MCAR.cov.65.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.65.cl.prop.women[,5])

d.MCAR.cov.65.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.65.cl.prop.men[,6])
d.MCAR.cov.65.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.65.cl.prop.women[,6])


d.MCAR.cov.65.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.65.cl.prop.men[,7])
d.MCAR.cov.65.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.65.cl.prop.women[,7])

d.MCAR.cov.65.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.65.cl.prop.men[,8])
d.MCAR.cov.65.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.65.cl.prop.women[,8])

d.MCAR.cov.65.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.65.cl.prop.men[,9])
d.MCAR.cov.65.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.65.cl.prop.women[,9])


# Cov 70


# Vector


vector.MCAR.cov.70.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.70.cl.prop.men[,1]
vector.MCAR.cov.70.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.70.cl.prop.women[,1]

vector.MCAR.cov.70.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.70.cl.prop.men[,2]
vector.MCAR.cov.70.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.70.cl.prop.women[,2]

vector.MCAR.cov.70.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.70.cl.prop.men[,3]
vector.MCAR.cov.70.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.70.cl.prop.women[,3]

vector.MCAR.cov.70.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.70.cl.prop.men[,4]
vector.MCAR.cov.70.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.70.cl.prop.women[,4]

vector.MCAR.cov.70.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.70.cl.prop.men[,5]
vector.MCAR.cov.70.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.70.cl.prop.women[,5]

vector.MCAR.cov.70.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.70.cl.prop.men[,6]
vector.MCAR.cov.70.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.70.cl.prop.women[,6]


vector.MCAR.cov.70.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.70.cl.prop.men[,7]
vector.MCAR.cov.70.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.70.cl.prop.women[,7]

vector.MCAR.cov.70.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.70.cl.prop.men[,8]
vector.MCAR.cov.70.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.70.cl.prop.women[,8]

vector.MCAR.cov.70.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.70.cl.prop.men[,9]
vector.MCAR.cov.70.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.70.cl.prop.women[,9]



# Summarised

d.MCAR.cov.70.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.70.cl.prop.men[,1])
d.MCAR.cov.70.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.70.cl.prop.women[,1])

d.MCAR.cov.70.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.70.cl.prop.men[,2])
d.MCAR.cov.70.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.70.cl.prop.women[,2])

d.MCAR.cov.70.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.70.cl.prop.men[,3])
d.MCAR.cov.70.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.70.cl.prop.women[,3])

d.MCAR.cov.70.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.70.cl.prop.men[,4])
d.MCAR.cov.70.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.70.cl.prop.women[,4])

d.MCAR.cov.70.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.70.cl.prop.men[,5])
d.MCAR.cov.70.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.70.cl.prop.women[,5])

d.MCAR.cov.70.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.70.cl.prop.men[,6])
d.MCAR.cov.70.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.70.cl.prop.women[,6])


d.MCAR.cov.70.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.70.cl.prop.men[,7])
d.MCAR.cov.70.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.70.cl.prop.women[,7])

d.MCAR.cov.70.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.70.cl.prop.men[,8])
d.MCAR.cov.70.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.70.cl.prop.women[,8])

d.MCAR.cov.70.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.70.cl.prop.men[,9])
d.MCAR.cov.70.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.70.cl.prop.women[,9])



# Cov 75


# Vector


vector.MCAR.cov.75.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.75.cl.prop.men[,1]
vector.MCAR.cov.75.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.75.cl.prop.women[,1]

vector.MCAR.cov.75.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.75.cl.prop.men[,2]
vector.MCAR.cov.75.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.75.cl.prop.women[,2]

vector.MCAR.cov.75.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.75.cl.prop.men[,3]
vector.MCAR.cov.75.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.75.cl.prop.women[,3]

vector.MCAR.cov.75.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.75.cl.prop.men[,4]
vector.MCAR.cov.75.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.75.cl.prop.women[,4]

vector.MCAR.cov.75.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.75.cl.prop.men[,5]
vector.MCAR.cov.75.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.75.cl.prop.women[,5]

vector.MCAR.cov.75.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.75.cl.prop.men[,6]
vector.MCAR.cov.75.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.75.cl.prop.women[,6]


vector.MCAR.cov.75.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.75.cl.prop.men[,7]
vector.MCAR.cov.75.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.75.cl.prop.women[,7]

vector.MCAR.cov.75.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.75.cl.prop.men[,8]
vector.MCAR.cov.75.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.75.cl.prop.women[,8]

vector.MCAR.cov.75.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.75.cl.prop.men[,9]
vector.MCAR.cov.75.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.75.cl.prop.women[,9]



# Summarised

d.MCAR.cov.75.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.75.cl.prop.men[,1])
d.MCAR.cov.75.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.75.cl.prop.women[,1])

d.MCAR.cov.75.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.75.cl.prop.men[,2])
d.MCAR.cov.75.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.75.cl.prop.women[,2])

d.MCAR.cov.75.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.75.cl.prop.men[,3])
d.MCAR.cov.75.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.75.cl.prop.women[,3])

d.MCAR.cov.75.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.75.cl.prop.men[,4])
d.MCAR.cov.75.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.75.cl.prop.women[,4])

d.MCAR.cov.75.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.75.cl.prop.men[,5])
d.MCAR.cov.75.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.75.cl.prop.women[,5])

d.MCAR.cov.75.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.75.cl.prop.men[,6])
d.MCAR.cov.75.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.75.cl.prop.women[,6])


d.MCAR.cov.75.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.75.cl.prop.men[,7])
d.MCAR.cov.75.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.75.cl.prop.women[,7])

d.MCAR.cov.75.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.75.cl.prop.men[,8])
d.MCAR.cov.75.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.75.cl.prop.women[,8])

d.MCAR.cov.75.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.75.cl.prop.men[,9])
d.MCAR.cov.75.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.75.cl.prop.women[,9])



# Cov 80


# Vector


vector.MCAR.cov.80.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.80.cl.prop.men[,1]
vector.MCAR.cov.80.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.80.cl.prop.women[,1]

vector.MCAR.cov.80.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.80.cl.prop.men[,2]
vector.MCAR.cov.80.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.80.cl.prop.women[,2]

vector.MCAR.cov.80.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.80.cl.prop.men[,3]
vector.MCAR.cov.80.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.80.cl.prop.women[,3]

vector.MCAR.cov.80.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.80.cl.prop.men[,4]
vector.MCAR.cov.80.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.80.cl.prop.women[,4]

vector.MCAR.cov.80.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.80.cl.prop.men[,5]
vector.MCAR.cov.80.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.80.cl.prop.women[,5]

vector.MCAR.cov.80.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.80.cl.prop.men[,6]
vector.MCAR.cov.80.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.80.cl.prop.women[,6]


vector.MCAR.cov.80.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.80.cl.prop.men[,7]
vector.MCAR.cov.80.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.80.cl.prop.women[,7]

vector.MCAR.cov.80.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.80.cl.prop.men[,8]
vector.MCAR.cov.80.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.80.cl.prop.women[,8]

vector.MCAR.cov.80.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.80.cl.prop.men[,9]
vector.MCAR.cov.80.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.80.cl.prop.women[,9]



# Summarised

d.MCAR.cov.80.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.80.cl.prop.men[,1])
d.MCAR.cov.80.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.80.cl.prop.women[,1])

d.MCAR.cov.80.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.80.cl.prop.men[,2])
d.MCAR.cov.80.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.80.cl.prop.women[,2])

d.MCAR.cov.80.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.80.cl.prop.men[,3])
d.MCAR.cov.80.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.80.cl.prop.women[,3])

d.MCAR.cov.80.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.80.cl.prop.men[,4])
d.MCAR.cov.80.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.80.cl.prop.women[,4])

d.MCAR.cov.80.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.80.cl.prop.men[,5])
d.MCAR.cov.80.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.80.cl.prop.women[,5])

d.MCAR.cov.80.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.80.cl.prop.men[,6])
d.MCAR.cov.80.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.80.cl.prop.women[,6])


d.MCAR.cov.80.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.80.cl.prop.men[,7])
d.MCAR.cov.80.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.80.cl.prop.women[,7])

d.MCAR.cov.80.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.80.cl.prop.men[,8])
d.MCAR.cov.80.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.80.cl.prop.women[,8])

d.MCAR.cov.80.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.80.cl.prop.men[,9])
d.MCAR.cov.80.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.80.cl.prop.women[,9])



# Cov 85


# Vector


vector.MCAR.cov.85.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.85.cl.prop.men[,1]
vector.MCAR.cov.85.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.85.cl.prop.women[,1]

vector.MCAR.cov.85.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.85.cl.prop.men[,2]
vector.MCAR.cov.85.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.85.cl.prop.women[,2]

vector.MCAR.cov.85.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.85.cl.prop.men[,3]
vector.MCAR.cov.85.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.85.cl.prop.women[,3]

vector.MCAR.cov.85.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.85.cl.prop.men[,4]
vector.MCAR.cov.85.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.85.cl.prop.women[,4]

vector.MCAR.cov.85.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.85.cl.prop.men[,5]
vector.MCAR.cov.85.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.85.cl.prop.women[,5]

vector.MCAR.cov.85.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.85.cl.prop.men[,6]
vector.MCAR.cov.85.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.85.cl.prop.women[,6]


vector.MCAR.cov.85.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.85.cl.prop.men[,7]
vector.MCAR.cov.85.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.85.cl.prop.women[,7]

vector.MCAR.cov.85.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.85.cl.prop.men[,8]
vector.MCAR.cov.85.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.85.cl.prop.women[,8]

vector.MCAR.cov.85.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.85.cl.prop.men[,9]
vector.MCAR.cov.85.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.85.cl.prop.women[,9]



# Summarised

d.MCAR.cov.85.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.85.cl.prop.men[,1])
d.MCAR.cov.85.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.85.cl.prop.women[,1])

d.MCAR.cov.85.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.85.cl.prop.men[,2])
d.MCAR.cov.85.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.85.cl.prop.women[,2])

d.MCAR.cov.85.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.85.cl.prop.men[,3])
d.MCAR.cov.85.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.85.cl.prop.women[,3])

d.MCAR.cov.85.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.85.cl.prop.men[,4])
d.MCAR.cov.85.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.85.cl.prop.women[,4])

d.MCAR.cov.85.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.85.cl.prop.men[,5])
d.MCAR.cov.85.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.85.cl.prop.women[,5])

d.MCAR.cov.85.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.85.cl.prop.men[,6])
d.MCAR.cov.85.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.85.cl.prop.women[,6])


d.MCAR.cov.85.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.85.cl.prop.men[,7])
d.MCAR.cov.85.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.85.cl.prop.women[,7])

d.MCAR.cov.85.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.85.cl.prop.men[,8])
d.MCAR.cov.85.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.85.cl.prop.women[,8])

d.MCAR.cov.85.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.85.cl.prop.men[,9])
d.MCAR.cov.85.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.85.cl.prop.women[,9])



# Cov 90


# Vector


vector.MCAR.cov.90.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.90.cl.prop.men[,1]
vector.MCAR.cov.90.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.90.cl.prop.women[,1]

vector.MCAR.cov.90.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.90.cl.prop.men[,2]
vector.MCAR.cov.90.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.90.cl.prop.women[,2]

vector.MCAR.cov.90.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.90.cl.prop.men[,3]
vector.MCAR.cov.90.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.90.cl.prop.women[,3]

vector.MCAR.cov.90.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.90.cl.prop.men[,4]
vector.MCAR.cov.90.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.90.cl.prop.women[,4]

vector.MCAR.cov.90.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.90.cl.prop.men[,5]
vector.MCAR.cov.90.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.90.cl.prop.women[,5]

vector.MCAR.cov.90.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.90.cl.prop.men[,6]
vector.MCAR.cov.90.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.90.cl.prop.women[,6]


vector.MCAR.cov.90.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.90.cl.prop.men[,7]
vector.MCAR.cov.90.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.90.cl.prop.women[,7]

vector.MCAR.cov.90.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.90.cl.prop.men[,8]
vector.MCAR.cov.90.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.90.cl.prop.women[,8]

vector.MCAR.cov.90.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.90.cl.prop.men[,9]
vector.MCAR.cov.90.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.90.cl.prop.women[,9]



# Summarised

d.MCAR.cov.90.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.90.cl.prop.men[,1])
d.MCAR.cov.90.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.90.cl.prop.women[,1])

d.MCAR.cov.90.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.90.cl.prop.men[,2])
d.MCAR.cov.90.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.90.cl.prop.women[,2])

d.MCAR.cov.90.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.90.cl.prop.men[,3])
d.MCAR.cov.90.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.90.cl.prop.women[,3])

d.MCAR.cov.90.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.90.cl.prop.men[,4])
d.MCAR.cov.90.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.90.cl.prop.women[,4])

d.MCAR.cov.90.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.90.cl.prop.men[,5])
d.MCAR.cov.90.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.90.cl.prop.women[,5])

d.MCAR.cov.90.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.90.cl.prop.men[,6])
d.MCAR.cov.90.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.90.cl.prop.women[,6])


d.MCAR.cov.90.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.90.cl.prop.men[,7])
d.MCAR.cov.90.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.90.cl.prop.women[,7])

d.MCAR.cov.90.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.90.cl.prop.men[,8])
d.MCAR.cov.90.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.90.cl.prop.women[,8])

d.MCAR.cov.90.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.90.cl.prop.men[,9])
d.MCAR.cov.90.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.90.cl.prop.women[,9])


# Cov 95


# Vector


vector.MCAR.cov.95.cl.prop.men15.25.F.15.25 <- d.MCAR.cov.95.cl.prop.men[,1]
vector.MCAR.cov.95.cl.prop.women15.25.M.15.25 <- d.MCAR.cov.95.cl.prop.women[,1]

vector.MCAR.cov.95.cl.prop.men25.40.F.15.25 <- d.MCAR.cov.95.cl.prop.men[,2]
vector.MCAR.cov.95.cl.prop.women25.40.M.15.25 <- d.MCAR.cov.95.cl.prop.women[,2]

vector.MCAR.cov.95.cl.prop.men40.50.F.15.25 <- d.MCAR.cov.95.cl.prop.men[,3]
vector.MCAR.cov.95.cl.prop.women40.50.M.15.25 <- d.MCAR.cov.95.cl.prop.women[,3]

vector.MCAR.cov.95.cl.prop.men15.25.F.25.40 <- d.MCAR.cov.95.cl.prop.men[,4]
vector.MCAR.cov.95.cl.prop.women15.25.M.25.40 <- d.MCAR.cov.95.cl.prop.women[,4]

vector.MCAR.cov.95.cl.prop.men25.40.F.25.40 <- d.MCAR.cov.95.cl.prop.men[,5]
vector.MCAR.cov.95.cl.prop.women25.40.M.25.40 <- d.MCAR.cov.95.cl.prop.women[,5]

vector.MCAR.cov.95.cl.prop.men40.50.F.25.40 <- d.MCAR.cov.95.cl.prop.men[,6]
vector.MCAR.cov.95.cl.prop.women40.50.M.25.40 <- d.MCAR.cov.95.cl.prop.women[,6]


vector.MCAR.cov.95.cl.prop.men15.25.F.40.50 <- d.MCAR.cov.95.cl.prop.men[,7]
vector.MCAR.cov.95.cl.prop.women15.25.M.40.50 <- d.MCAR.cov.95.cl.prop.women[,7]

vector.MCAR.cov.95.cl.prop.men25.40.F.40.50 <- d.MCAR.cov.95.cl.prop.men[,8]
vector.MCAR.cov.95.cl.prop.women25.40.M.40.50 <- d.MCAR.cov.95.cl.prop.women[,8]

vector.MCAR.cov.95.cl.prop.men40.50.F.40.50 <- d.MCAR.cov.95.cl.prop.men[,9]
vector.MCAR.cov.95.cl.prop.women40.50.M.40.50 <- d.MCAR.cov.95.cl.prop.women[,9]


# Summarised

d.MCAR.cov.95.cl.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.95.cl.prop.men[,1])
d.MCAR.cov.95.cl.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.95.cl.prop.women[,1])

d.MCAR.cov.95.cl.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.95.cl.prop.men[,2])

d.MCAR.cov.95.cl.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.95.cl.prop.women[,2])

d.MCAR.cov.95.cl.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.95.cl.prop.men[,3])
d.MCAR.cov.95.cl.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.95.cl.prop.women[,3])

d.MCAR.cov.95.cl.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.95.cl.prop.men[,4])
d.MCAR.cov.95.cl.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.95.cl.prop.women[,4])

d.MCAR.cov.95.cl.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.95.cl.prop.men[,5])
d.MCAR.cov.95.cl.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.95.cl.prop.women[,5])

d.MCAR.cov.95.cl.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.95.cl.prop.men[,6])
d.MCAR.cov.95.cl.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.95.cl.prop.women[,6])


d.MCAR.cov.95.cl.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.95.cl.prop.men[,7])
d.MCAR.cov.95.cl.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.95.cl.prop.women[,7])

d.MCAR.cov.95.cl.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.95.cl.prop.men[,8])
d.MCAR.cov.95.cl.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.95.cl.prop.women[,8])

d.MCAR.cov.95.cl.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.95.cl.prop.men[,9])
d.MCAR.cov.95.cl.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.95.cl.prop.women[,9])



# Visualising I in different MCAR coverages -------------------------------



# Proportions from transmission clusters ---------------------------------

# AAAAAAAAAA 15.25 - 40.50 and 15.25

# Men: men15.25.F.15.25

# d.MCAR.true.cov.100.prop.men15.25.F.15.25

cl.prop.men15.25.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men15.25.F.15.25[2], d.MCAR.cov.40.cl.prop.men15.25.F.15.25[2],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.15.25[2], d.MCAR.cov.50.cl.prop.men15.25.F.15.25[2],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.15.25[2], d.MCAR.cov.60.cl.prop.men15.25.F.15.25[2],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.15.25[2], d.MCAR.cov.70.cl.prop.men15.25.F.15.25[2],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.15.25[2], d.MCAR.cov.80.cl.prop.men15.25.F.15.25[2],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.15.25[2], d.MCAR.cov.90.cl.prop.men15.25.F.15.25[2],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.15.25[2], d.MCAR.true.cov.100.prop.men15.25.F.15.25[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men15.25.F.15.25[1], d.MCAR.cov.40.cl.prop.men15.25.F.15.25[1],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.15.25[1], d.MCAR.cov.50.cl.prop.men15.25.F.15.25[1],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.15.25[1], d.MCAR.cov.60.cl.prop.men15.25.F.15.25[1],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.15.25[1], d.MCAR.cov.70.cl.prop.men15.25.F.15.25[1],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.15.25[1], d.MCAR.cov.80.cl.prop.men15.25.F.15.25[1],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.15.25[1], d.MCAR.cov.90.cl.prop.men15.25.F.15.25[1],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.15.25[1], d.MCAR.true.cov.100.prop.men15.25.F.15.25[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men15.25.F.15.25[3], d.MCAR.cov.40.cl.prop.men15.25.F.15.25[3],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.15.25[3], d.MCAR.cov.50.cl.prop.men15.25.F.15.25[3],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.15.25[3], d.MCAR.cov.60.cl.prop.men15.25.F.15.25[3],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.15.25[3], d.MCAR.cov.70.cl.prop.men15.25.F.15.25[3],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.15.25[3], d.MCAR.cov.80.cl.prop.men15.25.F.15.25[3],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.15.25[3], d.MCAR.cov.90.cl.prop.men15.25.F.15.25[3],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.15.25[3], d.MCAR.true.cov.100.prop.men15.25.F.15.25[3]))


plot.cl.prop.men15.25.F.15.25 <- ggplot(cl.prop.men15.25.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 15 - 25 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.prop.men15.25.F.15.25.png",
       plot = plot.cl.prop.men15.25.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Women: women15.25.M.15.25

cl.prop.women15.25.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women15.25.M.15.25[2], d.MCAR.cov.40.cl.prop.women15.25.M.15.25[2],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.15.25[2], d.MCAR.cov.50.cl.prop.women15.25.M.15.25[2],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.15.25[2], d.MCAR.cov.60.cl.prop.women15.25.M.15.25[2],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.15.25[2], d.MCAR.cov.70.cl.prop.women15.25.M.15.25[2],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.15.25[2], d.MCAR.cov.80.cl.prop.women15.25.M.15.25[2],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.15.25[2], d.MCAR.cov.90.cl.prop.women15.25.M.15.25[2],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.15.25[2], d.MCAR.true.cov.100.prop.women15.25.M.15.25[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women15.25.M.15.25[1], d.MCAR.cov.40.cl.prop.women15.25.M.15.25[1],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.15.25[1], d.MCAR.cov.50.cl.prop.women15.25.M.15.25[1],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.15.25[1], d.MCAR.cov.60.cl.prop.women15.25.M.15.25[1],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.15.25[1], d.MCAR.cov.70.cl.prop.women15.25.M.15.25[1],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.15.25[1], d.MCAR.cov.80.cl.prop.women15.25.M.15.25[1],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.15.25[1], d.MCAR.cov.90.cl.prop.women15.25.M.15.25[1],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.15.25[1], d.MCAR.true.cov.100.prop.women15.25.M.15.25[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women15.25.M.15.25[3], d.MCAR.cov.40.cl.prop.women15.25.M.15.25[3],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.15.25[3], d.MCAR.cov.50.cl.prop.women15.25.M.15.25[3],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.15.25[3], d.MCAR.cov.60.cl.prop.women15.25.M.15.25[3],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.15.25[3], d.MCAR.cov.70.cl.prop.women15.25.M.15.25[3],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.15.25[3], d.MCAR.cov.80.cl.prop.women15.25.M.15.25[3],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.15.25[3], d.MCAR.cov.90.cl.prop.women15.25.M.15.25[3],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.15.25[3], d.MCAR.true.cov.100.prop.women15.25.M.15.25[3]))



plot.cl.prop.women15.25.M.15.25 <- ggplot(cl.prop.women15.25.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men 15 - 25 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.women15.25.M.15.25.png",
       plot = plot.cl.prop.women15.25.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Men: men25.40.F.15.25 

cl.prop.men25.40.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men25.40.F.15.25[2], d.MCAR.cov.40.cl.prop.men25.40.F.15.25[2],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.15.25[2], d.MCAR.cov.50.cl.prop.men25.40.F.15.25[2],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.15.25[2], d.MCAR.cov.60.cl.prop.men25.40.F.15.25[2],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.15.25[2], d.MCAR.cov.70.cl.prop.men25.40.F.15.25[2],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.15.25[2], d.MCAR.cov.80.cl.prop.men25.40.F.15.25[2],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.15.25[2], d.MCAR.cov.90.cl.prop.men25.40.F.15.25[2],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.15.25[2], d.MCAR.true.cov.100.prop.men25.40.F.15.25[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men25.40.F.15.25[1], d.MCAR.cov.40.cl.prop.men25.40.F.15.25[1],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.15.25[1], d.MCAR.cov.50.cl.prop.men25.40.F.15.25[1],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.15.25[1], d.MCAR.cov.60.cl.prop.men25.40.F.15.25[1],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.15.25[1], d.MCAR.cov.70.cl.prop.men25.40.F.15.25[1],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.15.25[1], d.MCAR.cov.80.cl.prop.men25.40.F.15.25[1],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.15.25[1], d.MCAR.cov.90.cl.prop.men25.40.F.15.25[1],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.15.25[1], d.MCAR.true.cov.100.prop.men25.40.F.15.25[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men25.40.F.15.25[3], d.MCAR.cov.40.cl.prop.men25.40.F.15.25[3],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.15.25[3], d.MCAR.cov.50.cl.prop.men25.40.F.15.25[3],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.15.25[3], d.MCAR.cov.60.cl.prop.men25.40.F.15.25[3],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.15.25[3], d.MCAR.cov.70.cl.prop.men25.40.F.15.25[3],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.15.25[3], d.MCAR.cov.80.cl.prop.men25.40.F.15.25[3],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.15.25[3], d.MCAR.cov.90.cl.prop.men25.40.F.15.25[3],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.15.25[3], d.MCAR.true.cov.100.prop.men25.40.F.15.25[3]))


plot.cl.prop.men25.40.F.15.25 <- ggplot(cl.prop.men25.40.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.men25.40.F.15.25.png",
       plot = plot.cl.prop.men25.40.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Women: women25.40.M.15.25 

cl.prop.women25.40.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women25.40.M.15.25[2], d.MCAR.cov.40.cl.prop.women25.40.M.15.25[2],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.15.25[2], d.MCAR.cov.50.cl.prop.women25.40.M.15.25[2],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.15.25[2], d.MCAR.cov.60.cl.prop.women25.40.M.15.25[2],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.15.25[2], d.MCAR.cov.70.cl.prop.women25.40.M.15.25[2],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.15.25[2], d.MCAR.cov.80.cl.prop.women25.40.M.15.25[2],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.15.25[2], d.MCAR.cov.90.cl.prop.women25.40.M.15.25[2],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.15.25[2], d.MCAR.true.cov.100.prop.women25.40.M.15.25[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women25.40.M.15.25[1], d.MCAR.cov.40.cl.prop.women25.40.M.15.25[1],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.15.25[1], d.MCAR.cov.50.cl.prop.women25.40.M.15.25[1],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.15.25[1], d.MCAR.cov.60.cl.prop.women25.40.M.15.25[1],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.15.25[1], d.MCAR.cov.70.cl.prop.women25.40.M.15.25[1],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.15.25[1], d.MCAR.cov.80.cl.prop.women25.40.M.15.25[1],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.15.25[1], d.MCAR.cov.90.cl.prop.women25.40.M.15.25[1],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.15.25[1], d.MCAR.true.cov.100.prop.women25.40.M.15.25[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women25.40.M.15.25[3], d.MCAR.cov.40.cl.prop.women25.40.M.15.25[3],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.15.25[3], d.MCAR.cov.50.cl.prop.women25.40.M.15.25[3],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.15.25[3], d.MCAR.cov.60.cl.prop.women25.40.M.15.25[3],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.15.25[3], d.MCAR.cov.70.cl.prop.women25.40.M.15.25[3],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.15.25[3], d.MCAR.cov.80.cl.prop.women25.40.M.15.25[3],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.15.25[3], d.MCAR.cov.90.cl.prop.women25.40.M.15.25[3],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.15.25[3], d.MCAR.true.cov.100.prop.women25.40.M.15.25[3]))


plot.cl.prop.women25.40.M.15.25 <- ggplot(cl.prop.women25.40.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.prop.women25.40.M.15.25.png",
       plot = plot.cl.prop.women25.40.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Men: men40.50.F.15.25 

cl.prop.men40.50.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men40.50.F.15.25[2], d.MCAR.cov.40.cl.prop.men40.50.F.15.25[2],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.15.25[2], d.MCAR.cov.50.cl.prop.men40.50.F.15.25[2],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.15.25[2], d.MCAR.cov.60.cl.prop.men40.50.F.15.25[2],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.15.25[2], d.MCAR.cov.70.cl.prop.men40.50.F.15.25[2],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.15.25[2], d.MCAR.cov.80.cl.prop.men40.50.F.15.25[2],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.15.25[2], d.MCAR.cov.90.cl.prop.men40.50.F.15.25[2],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.15.25[2], d.MCAR.true.cov.100.prop.men40.50.F.15.25[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men40.50.F.15.25[1], d.MCAR.cov.40.cl.prop.men40.50.F.15.25[1],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.15.25[1], d.MCAR.cov.50.cl.prop.men40.50.F.15.25[1],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.15.25[1], d.MCAR.cov.60.cl.prop.men40.50.F.15.25[1],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.15.25[1], d.MCAR.cov.70.cl.prop.men40.50.F.15.25[1],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.15.25[1], d.MCAR.cov.80.cl.prop.men40.50.F.15.25[1],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.15.25[1], d.MCAR.cov.90.cl.prop.men40.50.F.15.25[1],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.15.25[1], d.MCAR.true.cov.100.prop.men40.50.F.15.25[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men40.50.F.15.25[3], d.MCAR.cov.40.cl.prop.men40.50.F.15.25[3],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.15.25[3], d.MCAR.cov.50.cl.prop.men40.50.F.15.25[3],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.15.25[3], d.MCAR.cov.60.cl.prop.men40.50.F.15.25[3],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.15.25[3], d.MCAR.cov.70.cl.prop.men40.50.F.15.25[3],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.15.25[3], d.MCAR.cov.80.cl.prop.men40.50.F.15.25[3],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.15.25[3], d.MCAR.cov.90.cl.prop.men40.50.F.15.25[3],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.15.25[3], d.MCAR.true.cov.100.prop.men40.50.F.15.25[3]))


plot.cl.prop.men40.50.F.15.25 <- ggplot(cl.prop.men40.50.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.men40.50.F.15.25.png",
       plot = plot.cl.prop.men40.50.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women40.50.M.15.25 

cl.prop.women40.50.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women40.50.M.15.25[2], d.MCAR.cov.40.cl.prop.women40.50.M.15.25[2],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.15.25[2], d.MCAR.cov.50.cl.prop.women40.50.M.15.25[2],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.15.25[2], d.MCAR.cov.60.cl.prop.women40.50.M.15.25[2],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.15.25[2], d.MCAR.cov.70.cl.prop.women40.50.M.15.25[2],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.15.25[2], d.MCAR.cov.80.cl.prop.women40.50.M.15.25[2],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.15.25[2], d.MCAR.cov.90.cl.prop.women40.50.M.15.25[2],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.15.25[2], d.MCAR.true.cov.100.prop.women40.50.M.15.25[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women40.50.M.15.25[1], d.MCAR.cov.40.cl.prop.women40.50.M.15.25[1],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.15.25[1], d.MCAR.cov.50.cl.prop.women40.50.M.15.25[1],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.15.25[1], d.MCAR.cov.60.cl.prop.women40.50.M.15.25[1],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.15.25[1], d.MCAR.cov.70.cl.prop.women40.50.M.15.25[1],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.15.25[1], d.MCAR.cov.80.cl.prop.women40.50.M.15.25[1],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.15.25[1], d.MCAR.cov.90.cl.prop.women40.50.M.15.25[1],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.15.25[1], d.MCAR.true.cov.100.prop.women40.50.M.15.25[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women40.50.M.15.25[3], d.MCAR.cov.40.cl.prop.women40.50.M.15.25[3],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.15.25[3], d.MCAR.cov.50.cl.prop.women40.50.M.15.25[3],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.15.25[3], d.MCAR.cov.60.cl.prop.women40.50.M.15.25[3],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.15.25[3], d.MCAR.cov.70.cl.prop.women40.50.M.15.25[3],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.15.25[3], d.MCAR.cov.80.cl.prop.women40.50.M.15.25[3],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.15.25[3], d.MCAR.cov.90.cl.prop.women40.50.M.15.25[3],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.15.25[3], d.MCAR.true.cov.100.prop.women40.50.M.15.25[3]))


plot.cl.prop.women40.50.M.15.25 <- ggplot(cl.prop.women40.50.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.women40.50.M.15.25.png",
       plot = plot.cl.prop.women40.50.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



## BBBBBBBBBBBB 15.25 - 40.50 and 25.40


# Men: men15.25.F.25.40

# d.MCAR.true.cov.100.prop.men15.25.F.25.40

cl.prop.men15.25.F.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men15.25.F.25.40[2], d.MCAR.cov.40.cl.prop.men15.25.F.25.40[2],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.25.40[2], d.MCAR.cov.50.cl.prop.men15.25.F.25.40[2],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.25.40[2], d.MCAR.cov.60.cl.prop.men15.25.F.25.40[2],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.25.40[2], d.MCAR.cov.70.cl.prop.men15.25.F.25.40[2],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.25.40[2], d.MCAR.cov.80.cl.prop.men15.25.F.25.40[2],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.25.40[2], d.MCAR.cov.90.cl.prop.men15.25.F.25.40[2],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.25.40[2], d.MCAR.true.cov.100.prop.men15.25.F.25.40[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men15.25.F.25.40[1], d.MCAR.cov.40.cl.prop.men15.25.F.25.40[1],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.25.40[1], d.MCAR.cov.50.cl.prop.men15.25.F.25.40[1],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.25.40[1], d.MCAR.cov.60.cl.prop.men15.25.F.25.40[1],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.25.40[1], d.MCAR.cov.70.cl.prop.men15.25.F.25.40[1],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.25.40[1], d.MCAR.cov.80.cl.prop.men15.25.F.25.40[1],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.25.40[1], d.MCAR.cov.90.cl.prop.men15.25.F.25.40[1],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.25.40[1], d.MCAR.true.cov.100.prop.men15.25.F.25.40[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men15.25.F.25.40[3], d.MCAR.cov.40.cl.prop.men15.25.F.25.40[3],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.25.40[3], d.MCAR.cov.50.cl.prop.men15.25.F.25.40[3],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.25.40[3], d.MCAR.cov.60.cl.prop.men15.25.F.25.40[3],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.25.40[3], d.MCAR.cov.70.cl.prop.men15.25.F.25.40[3],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.25.40[3], d.MCAR.cov.80.cl.prop.men15.25.F.25.40[3],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.25.40[3], d.MCAR.cov.90.cl.prop.men15.25.F.25.40[3],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.25.40[3], d.MCAR.true.cov.100.prop.men15.25.F.25.40[3]))


plot.cl.prop.men15.25.F.25.40 <- ggplot(cl.prop.men15.25.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 25 - 40 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.prop.men15.25.F.25.40.png",
       plot = plot.cl.prop.men15.25.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Women: women15.25.M.25.40

cl.prop.women15.25.M.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women15.25.M.25.40[2], d.MCAR.cov.40.cl.prop.women15.25.M.25.40[2],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.25.40[2], d.MCAR.cov.50.cl.prop.women15.25.M.25.40[2],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.25.40[2], d.MCAR.cov.60.cl.prop.women15.25.M.25.40[2],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.25.40[2], d.MCAR.cov.70.cl.prop.women15.25.M.25.40[2],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.25.40[2], d.MCAR.cov.80.cl.prop.women15.25.M.25.40[2],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.25.40[2], d.MCAR.cov.90.cl.prop.women15.25.M.25.40[2],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.25.40[2], d.MCAR.true.cov.100.prop.women15.25.M.25.40[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women15.25.M.25.40[1], d.MCAR.cov.40.cl.prop.women15.25.M.25.40[1],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.25.40[1], d.MCAR.cov.50.cl.prop.women15.25.M.25.40[1],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.25.40[1], d.MCAR.cov.60.cl.prop.women15.25.M.25.40[1],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.25.40[1], d.MCAR.cov.70.cl.prop.women15.25.M.25.40[1],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.25.40[1], d.MCAR.cov.80.cl.prop.women15.25.M.25.40[1],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.25.40[1], d.MCAR.cov.90.cl.prop.women15.25.M.25.40[1],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.25.40[1], d.MCAR.true.cov.100.prop.women15.25.M.25.40[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women15.25.M.25.40[3], d.MCAR.cov.40.cl.prop.women15.25.M.25.40[3],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.25.40[3], d.MCAR.cov.50.cl.prop.women15.25.M.25.40[3],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.25.40[3], d.MCAR.cov.60.cl.prop.women15.25.M.25.40[3],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.25.40[3], d.MCAR.cov.70.cl.prop.women15.25.M.25.40[3],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.25.40[3], d.MCAR.cov.80.cl.prop.women15.25.M.25.40[3],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.25.40[3], d.MCAR.cov.90.cl.prop.women15.25.M.25.40[3],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.25.40[3], d.MCAR.true.cov.100.prop.women15.25.M.25.40[3]))



plot.cl.prop.women15.25.M.25.40 <- ggplot(cl.prop.women15.25.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men 25 - 40 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.prop.women15.25.M.25.40.png",
       plot = plot.cl.prop.women15.25.M.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Men: men25.40.F.25.40 

cl.prop.men25.40.F.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men25.40.F.25.40[2], d.MCAR.cov.40.cl.prop.men25.40.F.25.40[2],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.25.40[2], d.MCAR.cov.50.cl.prop.men25.40.F.25.40[2],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.25.40[2], d.MCAR.cov.60.cl.prop.men25.40.F.25.40[2],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.25.40[2], d.MCAR.cov.70.cl.prop.men25.40.F.25.40[2],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.25.40[2], d.MCAR.cov.80.cl.prop.men25.40.F.25.40[2],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.25.40[2], d.MCAR.cov.90.cl.prop.men25.40.F.25.40[2],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.25.40[2], d.MCAR.true.cov.100.prop.men25.40.F.25.40[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men25.40.F.25.40[1], d.MCAR.cov.40.cl.prop.men25.40.F.25.40[1],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.25.40[1], d.MCAR.cov.50.cl.prop.men25.40.F.25.40[1],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.25.40[1], d.MCAR.cov.60.cl.prop.men25.40.F.25.40[1],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.25.40[1], d.MCAR.cov.70.cl.prop.men25.40.F.25.40[1],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.25.40[1], d.MCAR.cov.80.cl.prop.men25.40.F.25.40[1],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.25.40[1], d.MCAR.cov.90.cl.prop.men25.40.F.25.40[1],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.25.40[1], d.MCAR.true.cov.100.prop.men25.40.F.25.40[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men25.40.F.25.40[3], d.MCAR.cov.40.cl.prop.men25.40.F.25.40[3],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.25.40[3], d.MCAR.cov.50.cl.prop.men25.40.F.25.40[3],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.25.40[3], d.MCAR.cov.60.cl.prop.men25.40.F.25.40[3],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.25.40[3], d.MCAR.cov.70.cl.prop.men25.40.F.25.40[3],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.25.40[3], d.MCAR.cov.80.cl.prop.men25.40.F.25.40[3],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.25.40[3], d.MCAR.cov.90.cl.prop.men25.40.F.25.40[3],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.25.40[3], d.MCAR.true.cov.100.prop.men25.40.F.25.40[3]))


plot.cl.prop.men25.40.F.25.40 <- ggplot(cl.prop.men25.40.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.men25.40.F.25.40.png",
       plot = plot.cl.prop.men25.40.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women25.40.M.25.40 

cl.prop.women25.40.M.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women25.40.M.25.40[2], d.MCAR.cov.40.cl.prop.women25.40.M.25.40[2],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.25.40[2], d.MCAR.cov.50.cl.prop.women25.40.M.25.40[2],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.25.40[2], d.MCAR.cov.60.cl.prop.women25.40.M.25.40[2],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.25.40[2], d.MCAR.cov.70.cl.prop.women25.40.M.25.40[2],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.25.40[2], d.MCAR.cov.80.cl.prop.women25.40.M.25.40[2],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.25.40[2], d.MCAR.cov.90.cl.prop.women25.40.M.25.40[2],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.25.40[2], d.MCAR.true.cov.100.prop.women25.40.M.25.40[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women25.40.M.25.40[1], d.MCAR.cov.40.cl.prop.women25.40.M.25.40[1],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.25.40[1], d.MCAR.cov.50.cl.prop.women25.40.M.25.40[1],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.25.40[1], d.MCAR.cov.60.cl.prop.women25.40.M.25.40[1],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.25.40[1], d.MCAR.cov.70.cl.prop.women25.40.M.25.40[1],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.25.40[1], d.MCAR.cov.80.cl.prop.women25.40.M.25.40[1],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.25.40[1], d.MCAR.cov.90.cl.prop.women25.40.M.25.40[1],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.25.40[1], d.MCAR.true.cov.100.prop.women25.40.M.25.40[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women25.40.M.25.40[3], d.MCAR.cov.40.cl.prop.women25.40.M.25.40[3],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.25.40[3], d.MCAR.cov.50.cl.prop.women25.40.M.25.40[3],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.25.40[3], d.MCAR.cov.60.cl.prop.women25.40.M.25.40[3],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.25.40[3], d.MCAR.cov.70.cl.prop.women25.40.M.25.40[3],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.25.40[3], d.MCAR.cov.80.cl.prop.women25.40.M.25.40[3],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.25.40[3], d.MCAR.cov.90.cl.prop.women25.40.M.25.40[3],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.25.40[3], d.MCAR.true.cov.100.prop.women25.40.M.25.40[3]))


plot.cl.prop.women25.40.M.25.40 <- ggplot(cl.prop.women25.40.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.women25.40.M.25.40.png",
       plot = plot.cl.prop.women25.40.M.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men40.50.F.25.40 

cl.prop.men40.50.F.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men40.50.F.25.40[2], d.MCAR.cov.40.cl.prop.men40.50.F.25.40[2],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.25.40[2], d.MCAR.cov.50.cl.prop.men40.50.F.25.40[2],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.25.40[2], d.MCAR.cov.60.cl.prop.men40.50.F.25.40[2],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.25.40[2], d.MCAR.cov.70.cl.prop.men40.50.F.25.40[2],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.25.40[2], d.MCAR.cov.80.cl.prop.men40.50.F.25.40[2],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.25.40[2], d.MCAR.cov.90.cl.prop.men40.50.F.25.40[2],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.25.40[2], d.MCAR.true.cov.100.prop.men40.50.F.25.40[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men40.50.F.25.40[1], d.MCAR.cov.40.cl.prop.men40.50.F.25.40[1],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.25.40[1], d.MCAR.cov.50.cl.prop.men40.50.F.25.40[1],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.25.40[1], d.MCAR.cov.60.cl.prop.men40.50.F.25.40[1],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.25.40[1], d.MCAR.cov.70.cl.prop.men40.50.F.25.40[1],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.25.40[1], d.MCAR.cov.80.cl.prop.men40.50.F.25.40[1],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.25.40[1], d.MCAR.cov.90.cl.prop.men40.50.F.25.40[1],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.25.40[1], d.MCAR.true.cov.100.prop.men40.50.F.25.40[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men40.50.F.25.40[3], d.MCAR.cov.40.cl.prop.men40.50.F.25.40[3],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.25.40[3], d.MCAR.cov.50.cl.prop.men40.50.F.25.40[3],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.25.40[3], d.MCAR.cov.60.cl.prop.men40.50.F.25.40[3],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.25.40[3], d.MCAR.cov.70.cl.prop.men40.50.F.25.40[3],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.25.40[3], d.MCAR.cov.80.cl.prop.men40.50.F.25.40[3],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.25.40[3], d.MCAR.cov.90.cl.prop.men40.50.F.25.40[3],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.25.40[3], d.MCAR.true.cov.100.prop.men40.50.F.25.40[3]))


plot.cl.prop.men40.50.F.25.40 <- ggplot(cl.prop.men40.50.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.men40.50.F.25.40.png",
       plot = plot.cl.prop.men40.50.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women40.50.M.25.40 

cl.prop.women40.50.M.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women40.50.M.25.40[2], d.MCAR.cov.40.cl.prop.women40.50.M.25.40[2],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.25.40[2], d.MCAR.cov.50.cl.prop.women40.50.M.25.40[2],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.25.40[2], d.MCAR.cov.60.cl.prop.women40.50.M.25.40[2],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.25.40[2], d.MCAR.cov.70.cl.prop.women40.50.M.25.40[2],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.25.40[2], d.MCAR.cov.80.cl.prop.women40.50.M.25.40[2],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.25.40[2], d.MCAR.cov.90.cl.prop.women40.50.M.25.40[2],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.25.40[2], d.MCAR.true.cov.100.prop.women40.50.M.25.40[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women40.50.M.25.40[1], d.MCAR.cov.40.cl.prop.women40.50.M.25.40[1],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.25.40[1], d.MCAR.cov.50.cl.prop.women40.50.M.25.40[1],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.25.40[1], d.MCAR.cov.60.cl.prop.women40.50.M.25.40[1],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.25.40[1], d.MCAR.cov.70.cl.prop.women40.50.M.25.40[1],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.25.40[1], d.MCAR.cov.80.cl.prop.women40.50.M.25.40[1],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.25.40[1], d.MCAR.cov.90.cl.prop.women40.50.M.25.40[1],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.25.40[1], d.MCAR.true.cov.100.prop.women40.50.M.25.40[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women40.50.M.25.40[3], d.MCAR.cov.40.cl.prop.women40.50.M.25.40[3],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.25.40[3], d.MCAR.cov.50.cl.prop.women40.50.M.25.40[3],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.25.40[3], d.MCAR.cov.60.cl.prop.women40.50.M.25.40[3],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.25.40[3], d.MCAR.cov.70.cl.prop.women40.50.M.25.40[3],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.25.40[3], d.MCAR.cov.80.cl.prop.women40.50.M.25.40[3],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.25.40[3], d.MCAR.cov.90.cl.prop.women40.50.M.25.40[3],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.25.40[3], d.MCAR.true.cov.100.prop.women40.50.M.25.40[3]))


plot.cl.prop.women40.50.M.25.40 <- ggplot(cl.prop.women40.50.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.women40.50.M.25.40.png",
       plot = plot.cl.prop.men40.50.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


## CCCCCCCCCC 15.25 - 40 - 50 and 40.50



# Men: men15.25.F.40.50

# d.MCAR.true.cov.100.prop.men15.25.F.40.50

cl.prop.men15.25.F.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men15.25.F.40.50[2], d.MCAR.cov.40.cl.prop.men15.25.F.40.50[2],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.40.50[2], d.MCAR.cov.50.cl.prop.men15.25.F.40.50[2],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.40.50[2], d.MCAR.cov.60.cl.prop.men15.25.F.40.50[2],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.40.50[2], d.MCAR.cov.70.cl.prop.men15.25.F.40.50[2],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.40.50[2], d.MCAR.cov.80.cl.prop.men15.25.F.40.50[2],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.40.50[2], d.MCAR.cov.90.cl.prop.men15.25.F.40.50[2],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.40.50[2], d.MCAR.true.cov.100.prop.men15.25.F.40.50[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men15.25.F.40.50[1], d.MCAR.cov.40.cl.prop.men15.25.F.40.50[1],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.40.50[1], d.MCAR.cov.50.cl.prop.men15.25.F.40.50[1],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.40.50[1], d.MCAR.cov.60.cl.prop.men15.25.F.40.50[1],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.40.50[1], d.MCAR.cov.70.cl.prop.men15.25.F.40.50[1],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.40.50[1], d.MCAR.cov.80.cl.prop.men15.25.F.40.50[1],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.40.50[1], d.MCAR.cov.90.cl.prop.men15.25.F.40.50[1],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.40.50[1], d.MCAR.true.cov.100.prop.men15.25.F.40.50[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men15.25.F.40.50[3], d.MCAR.cov.40.cl.prop.men15.25.F.40.50[3],
                                             d.MCAR.cov.45.cl.prop.men15.25.F.40.50[3], d.MCAR.cov.50.cl.prop.men15.25.F.40.50[3],
                                             d.MCAR.cov.55.cl.prop.men15.25.F.40.50[3], d.MCAR.cov.60.cl.prop.men15.25.F.40.50[3],
                                             d.MCAR.cov.65.cl.prop.men15.25.F.40.50[3], d.MCAR.cov.70.cl.prop.men15.25.F.40.50[3],
                                             d.MCAR.cov.75.cl.prop.men15.25.F.40.50[3], d.MCAR.cov.80.cl.prop.men15.25.F.40.50[3],
                                             d.MCAR.cov.85.cl.prop.men15.25.F.40.50[3], d.MCAR.cov.90.cl.prop.men15.25.F.40.50[3],
                                             d.MCAR.cov.95.cl.prop.men15.25.F.40.50[3], d.MCAR.true.cov.100.prop.men15.25.F.40.50[3]))


plot.cl.prop.men15.25.F.40.50 <- ggplot(cl.prop.men15.25.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 40 - 50 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.men15.25.F.40.50.png",
       plot = plot.cl.prop.men15.25.F.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: women15.25.M.40.50

cl.prop.women15.25.M.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women15.25.M.40.50[2], d.MCAR.cov.40.cl.prop.women15.25.M.40.50[2],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.40.50[2], d.MCAR.cov.50.cl.prop.women15.25.M.40.50[2],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.40.50[2], d.MCAR.cov.60.cl.prop.women15.25.M.40.50[2],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.40.50[2], d.MCAR.cov.70.cl.prop.women15.25.M.40.50[2],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.40.50[2], d.MCAR.cov.80.cl.prop.women15.25.M.40.50[2],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.40.50[2], d.MCAR.cov.90.cl.prop.women15.25.M.40.50[2],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.40.50[2], d.MCAR.true.cov.100.prop.women15.25.M.40.50[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women15.25.M.40.50[1], d.MCAR.cov.40.cl.prop.women15.25.M.40.50[1],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.40.50[1], d.MCAR.cov.50.cl.prop.women15.25.M.40.50[1],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.40.50[1], d.MCAR.cov.60.cl.prop.women15.25.M.40.50[1],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.40.50[1], d.MCAR.cov.70.cl.prop.women15.25.M.40.50[1],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.40.50[1], d.MCAR.cov.80.cl.prop.women15.25.M.40.50[1],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.40.50[1], d.MCAR.cov.90.cl.prop.women15.25.M.40.50[1],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.40.50[1], d.MCAR.true.cov.100.prop.women15.25.M.40.50[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women15.25.M.40.50[3], d.MCAR.cov.40.cl.prop.women15.25.M.40.50[3],
                                               d.MCAR.cov.45.cl.prop.women15.25.M.40.50[3], d.MCAR.cov.50.cl.prop.women15.25.M.40.50[3],
                                               d.MCAR.cov.55.cl.prop.women15.25.M.40.50[3], d.MCAR.cov.60.cl.prop.women15.25.M.40.50[3],
                                               d.MCAR.cov.65.cl.prop.women15.25.M.40.50[3], d.MCAR.cov.70.cl.prop.women15.25.M.40.50[3],
                                               d.MCAR.cov.75.cl.prop.women15.25.M.40.50[3], d.MCAR.cov.80.cl.prop.women15.25.M.40.50[3],
                                               d.MCAR.cov.85.cl.prop.women15.25.M.40.50[3], d.MCAR.cov.90.cl.prop.women15.25.M.40.50[3],
                                               d.MCAR.cov.95.cl.prop.women15.25.M.40.50[3], d.MCAR.true.cov.100.prop.women15.25.M.40.50[3]))



plot.cl.prop.women15.25.M.40.50 <- ggplot(cl.prop.women15.25.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men 40 - 50 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.women15.25.M.40.50.png",
       plot = plot.cl.prop.women15.25.M.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men25.40.F.40.50 

cl.prop.men25.40.F.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men25.40.F.40.50[2], d.MCAR.cov.40.cl.prop.men25.40.F.40.50[2],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.40.50[2], d.MCAR.cov.50.cl.prop.men25.40.F.40.50[2],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.40.50[2], d.MCAR.cov.60.cl.prop.men25.40.F.40.50[2],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.40.50[2], d.MCAR.cov.70.cl.prop.men25.40.F.40.50[2],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.40.50[2], d.MCAR.cov.80.cl.prop.men25.40.F.40.50[2],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.40.50[2], d.MCAR.cov.90.cl.prop.men25.40.F.40.50[2],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.40.50[2], d.MCAR.true.cov.100.prop.men25.40.F.40.50[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men25.40.F.40.50[1], d.MCAR.cov.40.cl.prop.men25.40.F.40.50[1],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.40.50[1], d.MCAR.cov.50.cl.prop.men25.40.F.40.50[1],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.40.50[1], d.MCAR.cov.60.cl.prop.men25.40.F.40.50[1],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.40.50[1], d.MCAR.cov.70.cl.prop.men25.40.F.40.50[1],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.40.50[1], d.MCAR.cov.80.cl.prop.men25.40.F.40.50[1],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.40.50[1], d.MCAR.cov.90.cl.prop.men25.40.F.40.50[1],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.40.50[1], d.MCAR.true.cov.100.prop.men25.40.F.40.50[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men25.40.F.40.50[3], d.MCAR.cov.40.cl.prop.men25.40.F.40.50[3],
                                             d.MCAR.cov.45.cl.prop.men25.40.F.40.50[3], d.MCAR.cov.50.cl.prop.men25.40.F.40.50[3],
                                             d.MCAR.cov.55.cl.prop.men25.40.F.40.50[3], d.MCAR.cov.60.cl.prop.men25.40.F.40.50[3],
                                             d.MCAR.cov.65.cl.prop.men25.40.F.40.50[3], d.MCAR.cov.70.cl.prop.men25.40.F.40.50[3],
                                             d.MCAR.cov.75.cl.prop.men25.40.F.40.50[3], d.MCAR.cov.80.cl.prop.men25.40.F.40.50[3],
                                             d.MCAR.cov.85.cl.prop.men25.40.F.40.50[3], d.MCAR.cov.90.cl.prop.men25.40.F.40.50[3],
                                             d.MCAR.cov.95.cl.prop.men25.40.F.40.50[3], d.MCAR.true.cov.100.prop.men25.40.F.40.50[3]))


plot.cl.prop.men25.40.F.40.50 <- ggplot(cl.prop.men25.40.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.men25.40.F.40.50.png",
       plot = plot.cl.prop.men25.40.F.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: women25.40.M.40.50 

cl.prop.women25.40.M.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women25.40.M.40.50[2], d.MCAR.cov.40.cl.prop.women25.40.M.40.50[2],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.40.50[2], d.MCAR.cov.50.cl.prop.women25.40.M.40.50[2],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.40.50[2], d.MCAR.cov.60.cl.prop.women25.40.M.40.50[2],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.40.50[2], d.MCAR.cov.70.cl.prop.women25.40.M.40.50[2],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.40.50[2], d.MCAR.cov.80.cl.prop.women25.40.M.40.50[2],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.40.50[2], d.MCAR.cov.90.cl.prop.women25.40.M.40.50[2],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.40.50[2], d.MCAR.true.cov.100.prop.women25.40.M.40.50[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women25.40.M.40.50[1], d.MCAR.cov.40.cl.prop.women25.40.M.40.50[1],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.40.50[1], d.MCAR.cov.50.cl.prop.women25.40.M.40.50[1],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.40.50[1], d.MCAR.cov.60.cl.prop.women25.40.M.40.50[1],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.40.50[1], d.MCAR.cov.70.cl.prop.women25.40.M.40.50[1],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.40.50[1], d.MCAR.cov.80.cl.prop.women25.40.M.40.50[1],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.40.50[1], d.MCAR.cov.90.cl.prop.women25.40.M.40.50[1],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.40.50[1], d.MCAR.true.cov.100.prop.women25.40.M.40.50[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women25.40.M.40.50[3], d.MCAR.cov.40.cl.prop.women25.40.M.40.50[3],
                                               d.MCAR.cov.45.cl.prop.women25.40.M.40.50[3], d.MCAR.cov.50.cl.prop.women25.40.M.40.50[3],
                                               d.MCAR.cov.55.cl.prop.women25.40.M.40.50[3], d.MCAR.cov.60.cl.prop.women25.40.M.40.50[3],
                                               d.MCAR.cov.65.cl.prop.women25.40.M.40.50[3], d.MCAR.cov.70.cl.prop.women25.40.M.40.50[3],
                                               d.MCAR.cov.75.cl.prop.women25.40.M.40.50[3], d.MCAR.cov.80.cl.prop.women25.40.M.40.50[3],
                                               d.MCAR.cov.85.cl.prop.women25.40.M.40.50[3], d.MCAR.cov.90.cl.prop.women25.40.M.40.50[3],
                                               d.MCAR.cov.95.cl.prop.women25.40.M.40.50[3], d.MCAR.true.cov.100.prop.women25.40.M.40.50[3]))


plot.cl.prop.women25.40.M.40.50 <- ggplot(cl.prop.women25.40.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.prop.women25.40.M.40.50.png",
       plot = plot.cl.prop.women25.40.M.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men40.50.F.40.50 

cl.prop.men40.50.F.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(d.MCAR.cov.35.cl.prop.men40.50.F.40.50[2], d.MCAR.cov.40.cl.prop.men40.50.F.40.50[2],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.40.50[2], d.MCAR.cov.50.cl.prop.men40.50.F.40.50[2],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.40.50[2], d.MCAR.cov.60.cl.prop.men40.50.F.40.50[2],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.40.50[2], d.MCAR.cov.70.cl.prop.men40.50.F.40.50[2],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.40.50[2], d.MCAR.cov.80.cl.prop.men40.50.F.40.50[2],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.40.50[2], d.MCAR.cov.90.cl.prop.men40.50.F.40.50[2],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.40.50[2], d.MCAR.true.cov.100.prop.men40.50.F.40.50[2]),
                                       
                                       L = c(d.MCAR.cov.35.cl.prop.men40.50.F.40.50[1], d.MCAR.cov.40.cl.prop.men40.50.F.40.50[1],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.40.50[1], d.MCAR.cov.50.cl.prop.men40.50.F.40.50[1],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.40.50[1], d.MCAR.cov.60.cl.prop.men40.50.F.40.50[1],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.40.50[1], d.MCAR.cov.70.cl.prop.men40.50.F.40.50[1],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.40.50[1], d.MCAR.cov.80.cl.prop.men40.50.F.40.50[1],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.40.50[1], d.MCAR.cov.90.cl.prop.men40.50.F.40.50[1],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.40.50[1], d.MCAR.true.cov.100.prop.men40.50.F.40.50[1]),
                                       
                                       U = c(d.MCAR.cov.35.cl.prop.men40.50.F.40.50[3], d.MCAR.cov.40.cl.prop.men40.50.F.40.50[3],
                                             d.MCAR.cov.45.cl.prop.men40.50.F.40.50[3], d.MCAR.cov.50.cl.prop.men40.50.F.40.50[3],
                                             d.MCAR.cov.55.cl.prop.men40.50.F.40.50[3], d.MCAR.cov.60.cl.prop.men40.50.F.40.50[3],
                                             d.MCAR.cov.65.cl.prop.men40.50.F.40.50[3], d.MCAR.cov.70.cl.prop.men40.50.F.40.50[3],
                                             d.MCAR.cov.75.cl.prop.men40.50.F.40.50[3], d.MCAR.cov.80.cl.prop.men40.50.F.40.50[3],
                                             d.MCAR.cov.85.cl.prop.men40.50.F.40.50[3], d.MCAR.cov.90.cl.prop.men40.50.F.40.50[3],
                                             d.MCAR.cov.95.cl.prop.men40.50.F.40.50[3], d.MCAR.true.cov.100.prop.men40.50.F.40.50[3]))


plot.cl.prop.men40.50.F.40.50 <- ggplot(cl.prop.men40.50.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.prop.men40.50.F.40.50.png",
       plot = plot.cl.prop.men40.50.F.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women40.50.M.40.50 

cl.prop.women40.50.M.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(d.MCAR.cov.35.cl.prop.women40.50.M.40.50[2], d.MCAR.cov.40.cl.prop.women40.50.M.40.50[2],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.40.50[2], d.MCAR.cov.50.cl.prop.women40.50.M.40.50[2],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.40.50[2], d.MCAR.cov.60.cl.prop.women40.50.M.40.50[2],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.40.50[2], d.MCAR.cov.70.cl.prop.women40.50.M.40.50[2],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.40.50[2], d.MCAR.cov.80.cl.prop.women40.50.M.40.50[2],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.40.50[2], d.MCAR.cov.90.cl.prop.women40.50.M.40.50[2],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.40.50[2], d.MCAR.true.cov.100.prop.women40.50.M.40.50[2]),
                                         
                                         L = c(d.MCAR.cov.35.cl.prop.women40.50.M.40.50[1], d.MCAR.cov.40.cl.prop.women40.50.M.40.50[1],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.40.50[1], d.MCAR.cov.50.cl.prop.women40.50.M.40.50[1],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.40.50[1], d.MCAR.cov.60.cl.prop.women40.50.M.40.50[1],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.40.50[1], d.MCAR.cov.70.cl.prop.women40.50.M.40.50[1],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.40.50[1], d.MCAR.cov.80.cl.prop.women40.50.M.40.50[1],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.40.50[1], d.MCAR.cov.90.cl.prop.women40.50.M.40.50[1],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.40.50[1], d.MCAR.true.cov.100.prop.women40.50.M.40.50[1]),
                                         
                                         U = c(d.MCAR.cov.35.cl.prop.women40.50.M.40.50[3], d.MCAR.cov.40.cl.prop.women40.50.M.40.50[3],
                                               d.MCAR.cov.45.cl.prop.women40.50.M.40.50[3], d.MCAR.cov.50.cl.prop.women40.50.M.40.50[3],
                                               d.MCAR.cov.55.cl.prop.women40.50.M.40.50[3], d.MCAR.cov.60.cl.prop.women40.50.M.40.50[3],
                                               d.MCAR.cov.65.cl.prop.women40.50.M.40.50[3], d.MCAR.cov.70.cl.prop.women40.50.M.40.50[3],
                                               d.MCAR.cov.75.cl.prop.women40.50.M.40.50[3], d.MCAR.cov.80.cl.prop.women40.50.M.40.50[3],
                                               d.MCAR.cov.85.cl.prop.women40.50.M.40.50[3], d.MCAR.cov.90.cl.prop.women40.50.M.40.50[3],
                                               d.MCAR.cov.95.cl.prop.women40.50.M.40.50[3], d.MCAR.true.cov.100.prop.women40.50.M.40.50[3]))


plot.cl.prop.women40.50.M.40.50 <- ggplot(cl.prop.women40.50.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



ggsave(filename = "cl.prop.women40.50.M.40.50.png",
       plot = plot.cl.prop.women40.50.M.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# II. True values in transmission clusters --------------------------------




# Cov 35

d.MCAR.cov.35.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,1])
d.MCAR.cov.35.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,1])

d.MCAR.cov.35.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,2])
d.MCAR.cov.35.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,2])

d.MCAR.cov.35.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,3])
d.MCAR.cov.35.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,3])

d.MCAR.cov.35.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,4])
d.MCAR.cov.35.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,4])

d.MCAR.cov.35.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,5])
d.MCAR.cov.35.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,5])

d.MCAR.cov.35.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,6])
d.MCAR.cov.35.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,6])


d.MCAR.cov.35.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,7])
d.MCAR.cov.35.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,7])

d.MCAR.cov.35.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,8])
d.MCAR.cov.35.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,8])

d.MCAR.cov.35.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.35.cl.true.prop.men[,9])
d.MCAR.cov.35.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.35.cl.true.prop.women[,9])


# Cov 40
d.MCAR.cov.40.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,1])
d.MCAR.cov.40.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,1])

d.MCAR.cov.40.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,2])
d.MCAR.cov.40.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,2])

d.MCAR.cov.40.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,3])
d.MCAR.cov.40.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,3])

d.MCAR.cov.40.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,4])
d.MCAR.cov.40.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,4])

d.MCAR.cov.40.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,5])
d.MCAR.cov.40.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,5])

d.MCAR.cov.40.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,6])
d.MCAR.cov.40.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,6])


d.MCAR.cov.40.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,7])
d.MCAR.cov.40.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,7])

d.MCAR.cov.40.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,8])
d.MCAR.cov.40.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,8])

d.MCAR.cov.40.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.40.cl.true.prop.men[,9])
d.MCAR.cov.40.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.40.cl.true.prop.women[,9])


# Cov 45

d.MCAR.cov.45.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,1])
d.MCAR.cov.45.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,1])

d.MCAR.cov.45.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,2])
d.MCAR.cov.45.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,2])

d.MCAR.cov.45.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,3])
d.MCAR.cov.45.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,3])

d.MCAR.cov.45.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,4])
d.MCAR.cov.45.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,4])

d.MCAR.cov.45.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,5])
d.MCAR.cov.45.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,5])

d.MCAR.cov.45.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,6])
d.MCAR.cov.45.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,6])


d.MCAR.cov.45.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,7])
d.MCAR.cov.45.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,7])

d.MCAR.cov.45.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,8])
d.MCAR.cov.45.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,8])

d.MCAR.cov.45.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.45.cl.true.prop.men[,9])
d.MCAR.cov.45.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.45.cl.true.prop.women[,9])


# Cov 50

d.MCAR.cov.50.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,1])
d.MCAR.cov.50.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,1])

d.MCAR.cov.50.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,2])
d.MCAR.cov.50.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,2])

d.MCAR.cov.50.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,3])
d.MCAR.cov.50.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,3])

d.MCAR.cov.50.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,4])
d.MCAR.cov.50.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,4])

d.MCAR.cov.50.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,5])
d.MCAR.cov.50.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,5])

d.MCAR.cov.50.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,6])
d.MCAR.cov.50.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,6])


d.MCAR.cov.50.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,7])
d.MCAR.cov.50.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,7])

d.MCAR.cov.50.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,8])
d.MCAR.cov.50.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,8])

d.MCAR.cov.50.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.50.cl.true.prop.men[,9])
d.MCAR.cov.50.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.50.cl.true.prop.women[,9])



# Cov 55

d.MCAR.cov.55.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,1])
d.MCAR.cov.55.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,1])

d.MCAR.cov.55.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,2])
d.MCAR.cov.55.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,2])

d.MCAR.cov.55.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,3])
d.MCAR.cov.55.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,3])

d.MCAR.cov.55.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,4])
d.MCAR.cov.55.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,4])

d.MCAR.cov.55.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,5])
d.MCAR.cov.55.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,5])

d.MCAR.cov.55.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,6])
d.MCAR.cov.55.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,6])


d.MCAR.cov.55.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,7])
d.MCAR.cov.55.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,7])

d.MCAR.cov.55.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,8])
d.MCAR.cov.55.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,8])

d.MCAR.cov.55.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.55.cl.true.prop.men[,9])
d.MCAR.cov.55.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.55.cl.true.prop.women[,9])



# Cov 60

d.MCAR.cov.60.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,1])
d.MCAR.cov.60.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,1])

d.MCAR.cov.60.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,2])
d.MCAR.cov.60.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,2])

d.MCAR.cov.60.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,3])
d.MCAR.cov.60.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,3])

d.MCAR.cov.60.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,4])
d.MCAR.cov.60.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,4])

d.MCAR.cov.60.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,5])
d.MCAR.cov.60.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,5])

d.MCAR.cov.60.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,6])
d.MCAR.cov.60.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,6])


d.MCAR.cov.60.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,7])
d.MCAR.cov.60.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,7])

d.MCAR.cov.60.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,8])
d.MCAR.cov.60.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,8])

d.MCAR.cov.60.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.60.cl.true.prop.men[,9])
d.MCAR.cov.60.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.60.cl.true.prop.women[,9])



# Cov 65

d.MCAR.cov.65.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,1])
d.MCAR.cov.65.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,1])

d.MCAR.cov.65.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,2])
d.MCAR.cov.65.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,2])

d.MCAR.cov.65.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,3])
d.MCAR.cov.65.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,3])

d.MCAR.cov.65.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,4])
d.MCAR.cov.65.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,4])

d.MCAR.cov.65.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,5])
d.MCAR.cov.65.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,5])

d.MCAR.cov.65.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,6])
d.MCAR.cov.65.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,6])


d.MCAR.cov.65.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,7])
d.MCAR.cov.65.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,7])

d.MCAR.cov.65.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,8])
d.MCAR.cov.65.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,8])

d.MCAR.cov.65.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.65.cl.true.prop.men[,9])
d.MCAR.cov.65.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.65.cl.true.prop.women[,9])


# Cov 70

d.MCAR.cov.70.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,1])
d.MCAR.cov.70.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,1])

d.MCAR.cov.70.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,2])
d.MCAR.cov.70.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,2])

d.MCAR.cov.70.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,3])
d.MCAR.cov.70.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,3])

d.MCAR.cov.70.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,4])
d.MCAR.cov.70.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,4])

d.MCAR.cov.70.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,5])
d.MCAR.cov.70.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,5])

d.MCAR.cov.70.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,6])
d.MCAR.cov.70.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,6])


d.MCAR.cov.70.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,7])
d.MCAR.cov.70.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,7])

d.MCAR.cov.70.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,8])
d.MCAR.cov.70.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,8])

d.MCAR.cov.70.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.70.cl.true.prop.men[,9])
d.MCAR.cov.70.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.70.cl.true.prop.women[,9])



# Cov 75

d.MCAR.cov.75.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,1])
d.MCAR.cov.75.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,1])

d.MCAR.cov.75.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,2])
d.MCAR.cov.75.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,2])

d.MCAR.cov.75.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,3])
d.MCAR.cov.75.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,3])

d.MCAR.cov.75.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,4])
d.MCAR.cov.75.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,4])

d.MCAR.cov.75.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,5])
d.MCAR.cov.75.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,5])

d.MCAR.cov.75.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,6])
d.MCAR.cov.75.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,6])


d.MCAR.cov.75.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,7])
d.MCAR.cov.75.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,7])

d.MCAR.cov.75.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,8])
d.MCAR.cov.75.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,8])

d.MCAR.cov.75.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.75.cl.true.prop.men[,9])
d.MCAR.cov.75.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.75.cl.true.prop.women[,9])



# Cov 80

d.MCAR.cov.80.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,1])
d.MCAR.cov.80.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,1])

d.MCAR.cov.80.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,2])
d.MCAR.cov.80.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,2])

d.MCAR.cov.80.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,3])
d.MCAR.cov.80.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,3])

d.MCAR.cov.80.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,4])
d.MCAR.cov.80.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,4])

d.MCAR.cov.80.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,5])
d.MCAR.cov.80.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,5])

d.MCAR.cov.80.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,6])
d.MCAR.cov.80.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,6])


d.MCAR.cov.80.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,7])
d.MCAR.cov.80.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,7])

d.MCAR.cov.80.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,8])
d.MCAR.cov.80.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,8])

d.MCAR.cov.80.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.80.cl.true.prop.men[,9])
d.MCAR.cov.80.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.80.cl.true.prop.women[,9])



# Cov 85

d.MCAR.cov.85.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,1])
d.MCAR.cov.85.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,1])

d.MCAR.cov.85.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,2])
d.MCAR.cov.85.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,2])

d.MCAR.cov.85.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,3])
d.MCAR.cov.85.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,3])

d.MCAR.cov.85.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,4])
d.MCAR.cov.85.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,4])

d.MCAR.cov.85.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,5])
d.MCAR.cov.85.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,5])

d.MCAR.cov.85.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,6])
d.MCAR.cov.85.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,6])


d.MCAR.cov.85.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,7])
d.MCAR.cov.85.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,7])

d.MCAR.cov.85.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,8])
d.MCAR.cov.85.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,8])

d.MCAR.cov.85.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.85.cl.true.prop.men[,9])
d.MCAR.cov.85.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.85.cl.true.prop.women[,9])



# Cov 90


d.MCAR.cov.90.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,1])
d.MCAR.cov.90.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,1])

d.MCAR.cov.90.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,2])
d.MCAR.cov.90.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,2])

d.MCAR.cov.90.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,3])
d.MCAR.cov.90.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,3])

d.MCAR.cov.90.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,4])
d.MCAR.cov.90.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,4])

d.MCAR.cov.90.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,5])
d.MCAR.cov.90.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,5])

d.MCAR.cov.90.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,6])
d.MCAR.cov.90.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,6])


d.MCAR.cov.90.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,7])
d.MCAR.cov.90.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,7])

d.MCAR.cov.90.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,8])
d.MCAR.cov.90.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,8])

d.MCAR.cov.90.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.90.cl.true.prop.men[,9])
d.MCAR.cov.90.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.90.cl.true.prop.women[,9])


# Cov 95

d.MCAR.cov.95.cl.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,1])
d.MCAR.cov.95.cl.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,1])


d.MCAR.cov.95.cl.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,2])
d.MCAR.cov.95.cl.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,2])

d.MCAR.cov.95.cl.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,3])
d.MCAR.cov.95.cl.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,3])

d.MCAR.cov.95.cl.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,4])
d.MCAR.cov.95.cl.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,4])

d.MCAR.cov.95.cl.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,5])
d.MCAR.cov.95.cl.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,5])

d.MCAR.cov.95.cl.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,6])
d.MCAR.cov.95.cl.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,6])


d.MCAR.cov.95.cl.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,7])
d.MCAR.cov.95.cl.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,7])

d.MCAR.cov.95.cl.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,8])
d.MCAR.cov.95.cl.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,8])

d.MCAR.cov.95.cl.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.95.cl.true.prop.men[,9])
d.MCAR.cov.95.cl.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.95.cl.true.prop.women[,9])






# Visualising II in different MCAR coverages -------------------------------


## AAAAAAAAAa 15.25 - 40.50 and 15.25

# Men: men15.25.F.15.25

cl.true.prop.men15.25.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.15.25[2], d.MCAR.cov.40.cl.true.prop.men15.25.F.15.25[2],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.15.25[2], d.MCAR.cov.50.cl.true.prop.men15.25.F.15.25[2],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.15.25[2], d.MCAR.cov.60.cl.true.prop.men15.25.F.15.25[2],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.15.25[2], d.MCAR.cov.70.cl.true.prop.men15.25.F.15.25[2],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.15.25[2], d.MCAR.cov.80.cl.true.prop.men15.25.F.15.25[2],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.15.25[2], d.MCAR.cov.90.cl.true.prop.men15.25.F.15.25[2],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.15.25[2], d.MCAR.true.cov.100.prop.men15.25.F.15.25[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.15.25[1], d.MCAR.cov.40.cl.true.prop.men15.25.F.15.25[1],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.15.25[1], d.MCAR.cov.50.cl.true.prop.men15.25.F.15.25[1],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.15.25[1], d.MCAR.cov.60.cl.true.prop.men15.25.F.15.25[1],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.15.25[1], d.MCAR.cov.70.cl.true.prop.men15.25.F.15.25[1],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.15.25[1], d.MCAR.cov.80.cl.true.prop.men15.25.F.15.25[1],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.15.25[1], d.MCAR.cov.90.cl.true.prop.men15.25.F.15.25[1],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.15.25[1], d.MCAR.true.cov.100.prop.men15.25.F.15.25[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.15.25[3], d.MCAR.cov.40.cl.true.prop.men15.25.F.15.25[3],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.15.25[3], d.MCAR.cov.50.cl.true.prop.men15.25.F.15.25[3],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.15.25[3], d.MCAR.cov.60.cl.true.prop.men15.25.F.15.25[3],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.15.25[3], d.MCAR.cov.70.cl.true.prop.men15.25.F.15.25[3],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.15.25[3], d.MCAR.cov.80.cl.true.prop.men15.25.F.15.25[3],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.15.25[3], d.MCAR.cov.90.cl.true.prop.men15.25.F.15.25[3],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.15.25[3], d.MCAR.true.cov.100.prop.men15.25.F.15.25[3]))


plot.cl.true.prop.men15.25.F.15.25 <- ggplot(cl.true.prop.men15.25.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 15 - 25 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.true.prop.men15.25.F.15.25.png",
       plot = plot.cl.true.prop.men15.25.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Women: women15.25.M.15.25

cl.true.prop.women15.25.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.15.25[2], d.MCAR.cov.40.cl.true.prop.women15.25.M.15.25[2],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.15.25[2], d.MCAR.cov.50.cl.true.prop.women15.25.M.15.25[2],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.15.25[2], d.MCAR.cov.60.cl.true.prop.women15.25.M.15.25[2],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.15.25[2], d.MCAR.cov.70.cl.true.prop.women15.25.M.15.25[2],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.15.25[2], d.MCAR.cov.80.cl.true.prop.women15.25.M.15.25[2],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.15.25[2], d.MCAR.cov.90.cl.true.prop.women15.25.M.15.25[2],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.15.25[2], d.MCAR.true.cov.100.prop.women15.25.M.15.25[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.15.25[1], d.MCAR.cov.40.cl.true.prop.women15.25.M.15.25[1],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.15.25[1], d.MCAR.cov.50.cl.true.prop.women15.25.M.15.25[1],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.15.25[1], d.MCAR.cov.60.cl.true.prop.women15.25.M.15.25[1],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.15.25[1], d.MCAR.cov.70.cl.true.prop.women15.25.M.15.25[1],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.15.25[1], d.MCAR.cov.80.cl.true.prop.women15.25.M.15.25[1],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.15.25[1], d.MCAR.cov.90.cl.true.prop.women15.25.M.15.25[1],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.15.25[1], d.MCAR.true.cov.100.prop.women15.25.M.15.25[1]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.15.25[3], d.MCAR.cov.40.cl.true.prop.women15.25.M.15.25[3],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.15.25[3], d.MCAR.cov.50.cl.true.prop.women15.25.M.15.25[3],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.15.25[3], d.MCAR.cov.60.cl.true.prop.women15.25.M.15.25[3],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.15.25[3], d.MCAR.cov.70.cl.true.prop.women15.25.M.15.25[3],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.15.25[3], d.MCAR.cov.80.cl.true.prop.women15.25.M.15.25[3],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.15.25[3], d.MCAR.cov.90.cl.true.prop.women15.25.M.15.25[3],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.15.25[3], d.MCAR.true.cov.100.prop.women15.25.M.15.25[3]))



plot.cl.true.prop.women15.25.M.15.25 <- ggplot(cl.true.prop.women15.25.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in 15 - 25 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.true.prop.women15.25.M.15.25.png",
       plot = plot.cl.true.prop.women15.25.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Men: men25.40.F.15.25 

cl.true.prop.men25.40.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.15.25[2], d.MCAR.cov.40.cl.true.prop.men25.40.F.15.25[2],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.15.25[2], d.MCAR.cov.50.cl.true.prop.men25.40.F.15.25[2],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.15.25[2], d.MCAR.cov.60.cl.true.prop.men25.40.F.15.25[2],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.15.25[2], d.MCAR.cov.70.cl.true.prop.men25.40.F.15.25[2],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.15.25[2], d.MCAR.cov.80.cl.true.prop.men25.40.F.15.25[2],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.15.25[2], d.MCAR.cov.90.cl.true.prop.men25.40.F.15.25[2],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.15.25[2], d.MCAR.true.cov.100.prop.men25.40.F.15.25[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.15.25[1], d.MCAR.cov.40.cl.true.prop.men25.40.F.15.25[1],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.15.25[1], d.MCAR.cov.50.cl.true.prop.men25.40.F.15.25[1],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.15.25[1], d.MCAR.cov.60.cl.true.prop.men25.40.F.15.25[1],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.15.25[1], d.MCAR.cov.70.cl.true.prop.men25.40.F.15.25[1],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.15.25[1], d.MCAR.cov.80.cl.true.prop.men25.40.F.15.25[1],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.15.25[1], d.MCAR.cov.90.cl.true.prop.men25.40.F.15.25[1],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.15.25[1], d.MCAR.true.cov.100.prop.men25.40.F.15.25[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.15.25[3], d.MCAR.cov.40.cl.true.prop.men25.40.F.15.25[3],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.15.25[3], d.MCAR.cov.50.cl.true.prop.men25.40.F.15.25[3],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.15.25[3], d.MCAR.cov.60.cl.true.prop.men25.40.F.15.25[3],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.15.25[3], d.MCAR.cov.70.cl.true.prop.men25.40.F.15.25[3],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.15.25[3], d.MCAR.cov.80.cl.true.prop.men25.40.F.15.25[3],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.15.25[3], d.MCAR.cov.90.cl.true.prop.men25.40.F.15.25[3],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.15.25[3], d.MCAR.true.cov.100.prop.men25.40.F.15.25[3]))


plot.cl.true.prop.men25.40.F.15.25 <- ggplot(cl.true.prop.men25.40.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.true.prop.men25.40.F.15.25.png",
       plot = plot.cl.true.prop.men25.40.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Women: women25.40.M.15.25 

cl.true.prop.women25.40.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.15.25[2], d.MCAR.cov.40.cl.true.prop.women25.40.M.15.25[2],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.15.25[2], d.MCAR.cov.50.cl.true.prop.women25.40.M.15.25[2],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.15.25[2], d.MCAR.cov.60.cl.true.prop.women25.40.M.15.25[2],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.15.25[2], d.MCAR.cov.70.cl.true.prop.women25.40.M.15.25[2],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.15.25[2], d.MCAR.cov.80.cl.true.prop.women25.40.M.15.25[2],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.15.25[2], d.MCAR.cov.90.cl.true.prop.women25.40.M.15.25[2],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.15.25[2], d.MCAR.true.cov.100.prop.women25.40.M.15.25[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.15.25[1], d.MCAR.cov.40.cl.true.prop.women25.40.M.15.25[1],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.15.25[1], d.MCAR.cov.50.cl.true.prop.women25.40.M.15.25[1],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.15.25[1], d.MCAR.cov.60.cl.true.prop.women25.40.M.15.25[1],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.15.25[1], d.MCAR.cov.70.cl.true.prop.women25.40.M.15.25[1],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.15.25[1], d.MCAR.cov.80.cl.true.prop.women25.40.M.15.25[1],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.15.25[1], d.MCAR.cov.90.cl.true.prop.women25.40.M.15.25[1],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.15.25[1], d.MCAR.true.cov.100.prop.women25.40.M.15.25[2]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.15.25[3], d.MCAR.cov.40.cl.true.prop.women25.40.M.15.25[3],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.15.25[3], d.MCAR.cov.50.cl.true.prop.women25.40.M.15.25[3],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.15.25[3], d.MCAR.cov.60.cl.true.prop.women25.40.M.15.25[3],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.15.25[3], d.MCAR.cov.70.cl.true.prop.women25.40.M.15.25[3],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.15.25[3], d.MCAR.cov.80.cl.true.prop.women25.40.M.15.25[3],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.15.25[3], d.MCAR.cov.90.cl.true.prop.women25.40.M.15.25[3],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.15.25[3], d.MCAR.true.cov.100.prop.women25.40.M.15.25[3]))


plot.cl.true.prop.women25.40.M.15.25 <- ggplot(cl.true.prop.women25.40.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.true.prop.women25.40.M.15.25.png",
       plot = plot.cl.true.prop.women25.40.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men40.50.F.15.25 

cl.true.prop.men40.50.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.15.25[2], d.MCAR.cov.40.cl.true.prop.men40.50.F.15.25[2],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.15.25[2], d.MCAR.cov.50.cl.true.prop.men40.50.F.15.25[2],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.15.25[2], d.MCAR.cov.60.cl.true.prop.men40.50.F.15.25[2],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.15.25[2], d.MCAR.cov.70.cl.true.prop.men40.50.F.15.25[2],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.15.25[2], d.MCAR.cov.80.cl.true.prop.men40.50.F.15.25[2],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.15.25[2], d.MCAR.cov.90.cl.true.prop.men40.50.F.15.25[2],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.15.25[2], d.MCAR.true.cov.100.prop.men40.50.F.15.25[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.15.25[1], d.MCAR.cov.40.cl.true.prop.men40.50.F.15.25[1],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.15.25[1], d.MCAR.cov.50.cl.true.prop.men40.50.F.15.25[1],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.15.25[1], d.MCAR.cov.60.cl.true.prop.men40.50.F.15.25[1],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.15.25[1], d.MCAR.cov.70.cl.true.prop.men40.50.F.15.25[1],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.15.25[1], d.MCAR.cov.80.cl.true.prop.men40.50.F.15.25[1],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.15.25[1], d.MCAR.cov.90.cl.true.prop.men40.50.F.15.25[1],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.15.25[1], d.MCAR.true.cov.100.prop.men40.50.F.15.25[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.15.25[3], d.MCAR.cov.40.cl.true.prop.men40.50.F.15.25[3],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.15.25[3], d.MCAR.cov.50.cl.true.prop.men40.50.F.15.25[3],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.15.25[3], d.MCAR.cov.60.cl.true.prop.men40.50.F.15.25[3],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.15.25[3], d.MCAR.cov.70.cl.true.prop.men40.50.F.15.25[3],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.15.25[3], d.MCAR.cov.80.cl.true.prop.men40.50.F.15.25[3],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.15.25[3], d.MCAR.cov.90.cl.true.prop.men40.50.F.15.25[3],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.15.25[3], d.MCAR.true.cov.100.prop.men40.50.F.15.25[3]))


plot.cl.true.prop.men40.50.F.15.25 <- ggplot(cl.true.prop.men40.50.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.men40.50.F.15.25.png",
       plot = plot.cl.true.prop.men40.50.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: women40.50.M.15.25 

cl.true.prop.women40.50.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.15.25[2], d.MCAR.cov.40.cl.true.prop.women40.50.M.15.25[2],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.15.25[2], d.MCAR.cov.50.cl.true.prop.women40.50.M.15.25[2],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.15.25[2], d.MCAR.cov.60.cl.true.prop.women40.50.M.15.25[2],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.15.25[2], d.MCAR.cov.70.cl.true.prop.women40.50.M.15.25[2],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.15.25[2], d.MCAR.cov.80.cl.true.prop.women40.50.M.15.25[2],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.15.25[2], d.MCAR.cov.90.cl.true.prop.women40.50.M.15.25[2],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.15.25[2], d.MCAR.true.cov.100.prop.women40.50.M.15.25[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.15.25[1], d.MCAR.cov.40.cl.true.prop.women40.50.M.15.25[1],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.15.25[1], d.MCAR.cov.50.cl.true.prop.women40.50.M.15.25[1],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.15.25[1], d.MCAR.cov.60.cl.true.prop.women40.50.M.15.25[1],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.15.25[1], d.MCAR.cov.70.cl.true.prop.women40.50.M.15.25[1],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.15.25[1], d.MCAR.cov.80.cl.true.prop.women40.50.M.15.25[1],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.15.25[1], d.MCAR.cov.90.cl.true.prop.women40.50.M.15.25[1],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.15.25[1], d.MCAR.true.cov.100.prop.women40.50.M.15.25[1]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.15.25[3], d.MCAR.cov.40.cl.true.prop.women40.50.M.15.25[3],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.15.25[3], d.MCAR.cov.50.cl.true.prop.women40.50.M.15.25[3],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.15.25[3], d.MCAR.cov.60.cl.true.prop.women40.50.M.15.25[3],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.15.25[3], d.MCAR.cov.70.cl.true.prop.women40.50.M.15.25[3],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.15.25[3], d.MCAR.cov.80.cl.true.prop.women40.50.M.15.25[3],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.15.25[3], d.MCAR.cov.90.cl.true.prop.women40.50.M.15.25[3],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.15.25[3], d.MCAR.true.cov.100.prop.women40.50.M.15.25[3]))


plot.cl.true.prop.women40.50.M.15.25 <- ggplot(cl.true.prop.women40.50.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



ggsave(filename = "plot.cl.true.prop.women40.50.M.15.25.png",
       plot = plot.cl.true.prop.women40.50.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# BBBBBBBBBBBb 15.25 - 40.50 and 25.40


# Men: men15.25.F.25.40

cl.true.prop.men15.25.F.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.25.40[2], d.MCAR.cov.40.cl.true.prop.men15.25.F.25.40[2],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.25.40[2], d.MCAR.cov.50.cl.true.prop.men15.25.F.25.40[2],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.25.40[2], d.MCAR.cov.60.cl.true.prop.men15.25.F.25.40[2],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.25.40[2], d.MCAR.cov.70.cl.true.prop.men15.25.F.25.40[2],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.25.40[2], d.MCAR.cov.80.cl.true.prop.men15.25.F.25.40[2],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.25.40[2], d.MCAR.cov.90.cl.true.prop.men15.25.F.25.40[2],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.25.40[2], d.MCAR.true.cov.100.prop.men15.25.F.25.40[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.25.40[1], d.MCAR.cov.40.cl.true.prop.men15.25.F.25.40[1],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.25.40[1], d.MCAR.cov.50.cl.true.prop.men15.25.F.25.40[1],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.25.40[1], d.MCAR.cov.60.cl.true.prop.men15.25.F.25.40[1],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.25.40[1], d.MCAR.cov.70.cl.true.prop.men15.25.F.25.40[1],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.25.40[1], d.MCAR.cov.80.cl.true.prop.men15.25.F.25.40[1],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.25.40[1], d.MCAR.cov.90.cl.true.prop.men15.25.F.25.40[1],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.25.40[1], d.MCAR.true.cov.100.prop.men15.25.F.25.40[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.25.40[3], d.MCAR.cov.40.cl.true.prop.men15.25.F.25.40[3],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.25.40[3], d.MCAR.cov.50.cl.true.prop.men15.25.F.25.40[3],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.25.40[3], d.MCAR.cov.60.cl.true.prop.men15.25.F.25.40[3],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.25.40[3], d.MCAR.cov.70.cl.true.prop.men15.25.F.25.40[3],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.25.40[3], d.MCAR.cov.80.cl.true.prop.men15.25.F.25.40[3],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.25.40[3], d.MCAR.cov.90.cl.true.prop.men15.25.F.25.40[3],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.25.40[3], d.MCAR.true.cov.100.prop.men15.25.F.25.40[3]))


plot.cl.true.prop.men15.25.F.25.40 <- ggplot(cl.true.prop.men15.25.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 25 - 40 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.true.prop.men15.25.F.25.40.png",
       plot = plot.cl.true.prop.men15.25.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women15.25.M.25.40

cl.true.prop.women15.25.M.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.25.40[2], d.MCAR.cov.40.cl.true.prop.women15.25.M.25.40[2],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.25.40[2], d.MCAR.cov.50.cl.true.prop.women15.25.M.25.40[2],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.25.40[2], d.MCAR.cov.60.cl.true.prop.women15.25.M.25.40[2],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.25.40[2], d.MCAR.cov.70.cl.true.prop.women15.25.M.25.40[2],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.25.40[2], d.MCAR.cov.80.cl.true.prop.women15.25.M.25.40[2],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.25.40[2], d.MCAR.cov.90.cl.true.prop.women15.25.M.25.40[2],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.25.40[2], d.MCAR.true.cov.100.prop.women15.25.M.25.40[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.25.40[1], d.MCAR.cov.40.cl.true.prop.women15.25.M.25.40[1],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.25.40[1], d.MCAR.cov.50.cl.true.prop.women15.25.M.25.40[1],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.25.40[1], d.MCAR.cov.60.cl.true.prop.women15.25.M.25.40[1],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.25.40[1], d.MCAR.cov.70.cl.true.prop.women15.25.M.25.40[1],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.25.40[1], d.MCAR.cov.80.cl.true.prop.women15.25.M.25.40[1],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.25.40[1], d.MCAR.cov.90.cl.true.prop.women15.25.M.25.40[1],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.25.40[1], d.MCAR.true.cov.100.prop.women15.25.M.25.40[1]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.25.40[3], d.MCAR.cov.40.cl.true.prop.women15.25.M.25.40[3],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.25.40[3], d.MCAR.cov.50.cl.true.prop.women15.25.M.25.40[3],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.25.40[3], d.MCAR.cov.60.cl.true.prop.women15.25.M.25.40[3],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.25.40[3], d.MCAR.cov.70.cl.true.prop.women15.25.M.25.40[3],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.25.40[3], d.MCAR.cov.80.cl.true.prop.women15.25.M.25.40[3],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.25.40[3], d.MCAR.cov.90.cl.true.prop.women15.25.M.25.40[3],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.25.40[3], d.MCAR.true.cov.100.prop.women15.25.M.25.40[3]))



plot.cl.true.prop.women15.25.M.25.40 <- ggplot(cl.true.prop.women15.25.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in 25 - 40 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "cl.true.prop.women15.25.M.25.40.png",
       plot = cl.true.prop.women15.25.M.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Men: men25.40.F.25.40 

cl.true.prop.men25.40.F.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.25.40[2], d.MCAR.cov.40.cl.true.prop.men25.40.F.25.40[2],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.25.40[2], d.MCAR.cov.50.cl.true.prop.men25.40.F.25.40[2],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.25.40[2], d.MCAR.cov.60.cl.true.prop.men25.40.F.25.40[2],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.25.40[2], d.MCAR.cov.70.cl.true.prop.men25.40.F.25.40[2],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.25.40[2], d.MCAR.cov.80.cl.true.prop.men25.40.F.25.40[2],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.25.40[2], d.MCAR.cov.90.cl.true.prop.men25.40.F.25.40[2],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.25.40[2], d.MCAR.true.cov.100.prop.men25.40.F.25.40[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.25.40[1], d.MCAR.cov.40.cl.true.prop.men25.40.F.25.40[1],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.25.40[1], d.MCAR.cov.50.cl.true.prop.men25.40.F.25.40[1],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.25.40[1], d.MCAR.cov.60.cl.true.prop.men25.40.F.25.40[1],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.25.40[1], d.MCAR.cov.70.cl.true.prop.men25.40.F.25.40[1],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.25.40[1], d.MCAR.cov.80.cl.true.prop.men25.40.F.25.40[1],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.25.40[1], d.MCAR.cov.90.cl.true.prop.men25.40.F.25.40[1],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.25.40[1], d.MCAR.true.cov.100.prop.men25.40.F.25.40[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.25.40[3], d.MCAR.cov.40.cl.true.prop.men25.40.F.25.40[3],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.25.40[3], d.MCAR.cov.50.cl.true.prop.men25.40.F.25.40[3],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.25.40[3], d.MCAR.cov.60.cl.true.prop.men25.40.F.25.40[3],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.25.40[3], d.MCAR.cov.70.cl.true.prop.men25.40.F.25.40[3],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.25.40[3], d.MCAR.cov.80.cl.true.prop.men25.40.F.25.40[3],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.25.40[3], d.MCAR.cov.90.cl.true.prop.men25.40.F.25.40[3],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.25.40[3], d.MCAR.true.cov.100.prop.men25.40.F.25.40[3]))


plot.cl.true.prop.men25.40.F.25.40 <- ggplot(cl.true.prop.men25.40.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.men25.40.F.25.40.png",
       plot = plot.cl.true.prop.men25.40.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: women25.40.M.25.40 

cl.true.prop.women25.40.M.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.25.40[2], d.MCAR.cov.40.cl.true.prop.women25.40.M.25.40[2],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.25.40[2], d.MCAR.cov.50.cl.true.prop.women25.40.M.25.40[2],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.25.40[2], d.MCAR.cov.60.cl.true.prop.women25.40.M.25.40[2],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.25.40[2], d.MCAR.cov.70.cl.true.prop.women25.40.M.25.40[2],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.25.40[2], d.MCAR.cov.80.cl.true.prop.women25.40.M.25.40[2],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.25.40[2], d.MCAR.cov.90.cl.true.prop.women25.40.M.25.40[2],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.25.40[2], d.MCAR.true.cov.100.prop.women25.40.M.25.40[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.25.40[1], d.MCAR.cov.40.cl.true.prop.women25.40.M.25.40[1],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.25.40[1], d.MCAR.cov.50.cl.true.prop.women25.40.M.25.40[1],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.25.40[1], d.MCAR.cov.60.cl.true.prop.women25.40.M.25.40[1],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.25.40[1], d.MCAR.cov.70.cl.true.prop.women25.40.M.25.40[1],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.25.40[1], d.MCAR.cov.80.cl.true.prop.women25.40.M.25.40[1],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.25.40[1], d.MCAR.cov.90.cl.true.prop.women25.40.M.25.40[1],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.25.40[1], d.MCAR.true.cov.100.prop.women25.40.M.25.40[2]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.25.40[3], d.MCAR.cov.40.cl.true.prop.women25.40.M.25.40[3],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.25.40[3], d.MCAR.cov.50.cl.true.prop.women25.40.M.25.40[3],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.25.40[3], d.MCAR.cov.60.cl.true.prop.women25.40.M.25.40[3],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.25.40[3], d.MCAR.cov.70.cl.true.prop.women25.40.M.25.40[3],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.25.40[3], d.MCAR.cov.80.cl.true.prop.women25.40.M.25.40[3],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.25.40[3], d.MCAR.cov.90.cl.true.prop.women25.40.M.25.40[3],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.25.40[3], d.MCAR.true.cov.100.prop.women25.40.M.25.40[3]))


plot.cl.true.prop.women25.40.M.25.40 <- ggplot(cl.true.prop.women25.40.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.women25.40.M.25.40.png",
       plot = plot.cl.true.prop.women25.40.M.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men40.50.F.25.40 

cl.true.prop.men40.50.F.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.25.40[2], d.MCAR.cov.40.cl.true.prop.men40.50.F.25.40[2],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.25.40[2], d.MCAR.cov.50.cl.true.prop.men40.50.F.25.40[2],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.25.40[2], d.MCAR.cov.60.cl.true.prop.men40.50.F.25.40[2],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.25.40[2], d.MCAR.cov.70.cl.true.prop.men40.50.F.25.40[2],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.25.40[2], d.MCAR.cov.80.cl.true.prop.men40.50.F.25.40[2],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.25.40[2], d.MCAR.cov.90.cl.true.prop.men40.50.F.25.40[2],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.25.40[2], d.MCAR.true.cov.100.prop.men40.50.F.25.40[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.25.40[1], d.MCAR.cov.40.cl.true.prop.men40.50.F.25.40[1],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.25.40[1], d.MCAR.cov.50.cl.true.prop.men40.50.F.25.40[1],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.25.40[1], d.MCAR.cov.60.cl.true.prop.men40.50.F.25.40[1],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.25.40[1], d.MCAR.cov.70.cl.true.prop.men40.50.F.25.40[1],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.25.40[1], d.MCAR.cov.80.cl.true.prop.men40.50.F.25.40[1],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.25.40[1], d.MCAR.cov.90.cl.true.prop.men40.50.F.25.40[1],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.25.40[1], d.MCAR.true.cov.100.prop.men40.50.F.25.40[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.25.40[3], d.MCAR.cov.40.cl.true.prop.men40.50.F.25.40[3],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.25.40[3], d.MCAR.cov.50.cl.true.prop.men40.50.F.25.40[3],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.25.40[3], d.MCAR.cov.60.cl.true.prop.men40.50.F.25.40[3],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.25.40[3], d.MCAR.cov.70.cl.true.prop.men40.50.F.25.40[3],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.25.40[3], d.MCAR.cov.80.cl.true.prop.men40.50.F.25.40[3],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.25.40[3], d.MCAR.cov.90.cl.true.prop.men40.50.F.25.40[3],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.25.40[3], d.MCAR.true.cov.100.prop.men40.50.F.25.40[3]))


plot.cl.true.prop.men40.50.F.25.40 <- ggplot(cl.true.prop.men40.50.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.men40.50.F.25.40.png",
       plot = plot.cl.true.prop.men40.50.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women40.50.M.25.40 

cl.true.prop.women40.50.M.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.25.40[2], d.MCAR.cov.40.cl.true.prop.women40.50.M.25.40[2],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.25.40[2], d.MCAR.cov.50.cl.true.prop.women40.50.M.25.40[2],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.25.40[2], d.MCAR.cov.60.cl.true.prop.women40.50.M.25.40[2],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.25.40[2], d.MCAR.cov.70.cl.true.prop.women40.50.M.25.40[2],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.25.40[2], d.MCAR.cov.80.cl.true.prop.women40.50.M.25.40[2],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.25.40[2], d.MCAR.cov.90.cl.true.prop.women40.50.M.25.40[2],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.25.40[2], d.MCAR.true.cov.100.prop.women40.50.M.25.40[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.25.40[1], d.MCAR.cov.40.cl.true.prop.women40.50.M.25.40[1],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.25.40[1], d.MCAR.cov.50.cl.true.prop.women40.50.M.25.40[1],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.25.40[1], d.MCAR.cov.60.cl.true.prop.women40.50.M.25.40[1],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.25.40[1], d.MCAR.cov.70.cl.true.prop.women40.50.M.25.40[1],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.25.40[1], d.MCAR.cov.80.cl.true.prop.women40.50.M.25.40[1],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.25.40[1], d.MCAR.cov.90.cl.true.prop.women40.50.M.25.40[1],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.25.40[1], d.MCAR.true.cov.100.prop.women40.50.M.25.40[1]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.25.40[3], d.MCAR.cov.40.cl.true.prop.women40.50.M.25.40[3],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.25.40[3], d.MCAR.cov.50.cl.true.prop.women40.50.M.25.40[3],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.25.40[3], d.MCAR.cov.60.cl.true.prop.women40.50.M.25.40[3],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.25.40[3], d.MCAR.cov.70.cl.true.prop.women40.50.M.25.40[3],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.25.40[3], d.MCAR.cov.80.cl.true.prop.women40.50.M.25.40[3],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.25.40[3], d.MCAR.cov.90.cl.true.prop.women40.50.M.25.40[3],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.25.40[3], d.MCAR.true.cov.100.prop.women40.50.M.25.40[3]))


plot.cl.true.prop.women40.50.M.25.40 <- ggplot(cl.true.prop.women40.50.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.women40.50.M.25.40.png",
       plot = plot.cl.true.prop.women40.50.M.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


## CCCCCCCCCCCC 15.25 - 40.50 and 40.50


# Men: men15.25.F.40.50

cl.true.prop.men15.25.F.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.40.50[2], d.MCAR.cov.40.cl.true.prop.men15.25.F.40.50[2],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.40.50[2], d.MCAR.cov.50.cl.true.prop.men15.25.F.40.50[2],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.40.50[2], d.MCAR.cov.60.cl.true.prop.men15.25.F.40.50[2],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.40.50[2], d.MCAR.cov.70.cl.true.prop.men15.25.F.40.50[2],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.40.50[2], d.MCAR.cov.80.cl.true.prop.men15.25.F.40.50[2],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.40.50[2], d.MCAR.cov.90.cl.true.prop.men15.25.F.40.50[2],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.40.50[2], d.MCAR.true.cov.100.prop.men15.25.F.40.50[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.40.50[1], d.MCAR.cov.40.cl.true.prop.men15.25.F.40.50[1],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.40.50[1], d.MCAR.cov.50.cl.true.prop.men15.25.F.40.50[1],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.40.50[1], d.MCAR.cov.60.cl.true.prop.men15.25.F.40.50[1],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.40.50[1], d.MCAR.cov.70.cl.true.prop.men15.25.F.40.50[1],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.40.50[1], d.MCAR.cov.80.cl.true.prop.men15.25.F.40.50[1],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.40.50[1], d.MCAR.cov.90.cl.true.prop.men15.25.F.40.50[1],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.40.50[1], d.MCAR.true.cov.100.prop.men15.25.F.40.50[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men15.25.F.40.50[3], d.MCAR.cov.40.cl.true.prop.men15.25.F.40.50[3],
                                                  d.MCAR.cov.45.cl.true.prop.men15.25.F.40.50[3], d.MCAR.cov.50.cl.true.prop.men15.25.F.40.50[3],
                                                  d.MCAR.cov.55.cl.true.prop.men15.25.F.40.50[3], d.MCAR.cov.60.cl.true.prop.men15.25.F.40.50[3],
                                                  d.MCAR.cov.65.cl.true.prop.men15.25.F.40.50[3], d.MCAR.cov.70.cl.true.prop.men15.25.F.40.50[3],
                                                  d.MCAR.cov.75.cl.true.prop.men15.25.F.40.50[3], d.MCAR.cov.80.cl.true.prop.men15.25.F.40.50[3],
                                                  d.MCAR.cov.85.cl.true.prop.men15.25.F.40.50[3], d.MCAR.cov.90.cl.true.prop.men15.25.F.40.50[3],
                                                  d.MCAR.cov.95.cl.true.prop.men15.25.F.40.50[3], d.MCAR.true.cov.100.prop.men15.25.F.40.50[3]))


plot.cl.true.prop.men15.25.F.40.50 <- ggplot(cl.true.prop.men15.25.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 25 - 40 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.men15.25.F.40.50.png",
       plot = plot.cl.true.prop.men15.25.F.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: women15.25.M.40.50

cl.true.prop.women15.25.M.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.40.50[2], d.MCAR.cov.40.cl.true.prop.women15.25.M.40.50[2],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.40.50[2], d.MCAR.cov.50.cl.true.prop.women15.25.M.40.50[2],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.40.50[2], d.MCAR.cov.60.cl.true.prop.women15.25.M.40.50[2],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.40.50[2], d.MCAR.cov.70.cl.true.prop.women15.25.M.40.50[2],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.40.50[2], d.MCAR.cov.80.cl.true.prop.women15.25.M.40.50[2],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.40.50[2], d.MCAR.cov.90.cl.true.prop.women15.25.M.40.50[2],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.40.50[2], d.MCAR.true.cov.100.prop.women15.25.M.40.50[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.40.50[1], d.MCAR.cov.40.cl.true.prop.women15.25.M.40.50[1],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.40.50[1], d.MCAR.cov.50.cl.true.prop.women15.25.M.40.50[1],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.40.50[1], d.MCAR.cov.60.cl.true.prop.women15.25.M.40.50[1],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.40.50[1], d.MCAR.cov.70.cl.true.prop.women15.25.M.40.50[1],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.40.50[1], d.MCAR.cov.80.cl.true.prop.women15.25.M.40.50[1],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.40.50[1], d.MCAR.cov.90.cl.true.prop.women15.25.M.40.50[1],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.40.50[1], d.MCAR.true.cov.100.prop.women15.25.M.40.50[1]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women15.25.M.40.50[3], d.MCAR.cov.40.cl.true.prop.women15.25.M.40.50[3],
                                                    d.MCAR.cov.45.cl.true.prop.women15.25.M.40.50[3], d.MCAR.cov.50.cl.true.prop.women15.25.M.40.50[3],
                                                    d.MCAR.cov.55.cl.true.prop.women15.25.M.40.50[3], d.MCAR.cov.60.cl.true.prop.women15.25.M.40.50[3],
                                                    d.MCAR.cov.65.cl.true.prop.women15.25.M.40.50[3], d.MCAR.cov.70.cl.true.prop.women15.25.M.40.50[3],
                                                    d.MCAR.cov.75.cl.true.prop.women15.25.M.40.50[3], d.MCAR.cov.80.cl.true.prop.women15.25.M.40.50[3],
                                                    d.MCAR.cov.85.cl.true.prop.women15.25.M.40.50[3], d.MCAR.cov.90.cl.true.prop.women15.25.M.40.50[3],
                                                    d.MCAR.cov.95.cl.true.prop.women15.25.M.40.50[3], d.MCAR.true.cov.100.prop.women15.25.M.40.50[3]))



plot.cl.true.prop.women15.25.M.40.50 <- ggplot(cl.true.prop.women15.25.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in 25 - 40 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.women15.25.M.40.50.png",
       plot = plot.cl.true.prop.women15.25.M.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men25.40.F.40.50 

cl.true.prop.men25.40.F.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.40.50[2], d.MCAR.cov.40.cl.true.prop.men25.40.F.40.50[2],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.40.50[2], d.MCAR.cov.50.cl.true.prop.men25.40.F.40.50[2],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.40.50[2], d.MCAR.cov.60.cl.true.prop.men25.40.F.40.50[2],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.40.50[2], d.MCAR.cov.70.cl.true.prop.men25.40.F.40.50[2],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.40.50[2], d.MCAR.cov.80.cl.true.prop.men25.40.F.40.50[2],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.40.50[2], d.MCAR.cov.90.cl.true.prop.men25.40.F.40.50[2],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.40.50[2], d.MCAR.true.cov.100.prop.men25.40.F.40.50[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.40.50[1], d.MCAR.cov.40.cl.true.prop.men25.40.F.40.50[1],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.40.50[1], d.MCAR.cov.50.cl.true.prop.men25.40.F.40.50[1],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.40.50[1], d.MCAR.cov.60.cl.true.prop.men25.40.F.40.50[1],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.40.50[1], d.MCAR.cov.70.cl.true.prop.men25.40.F.40.50[1],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.40.50[1], d.MCAR.cov.80.cl.true.prop.men25.40.F.40.50[1],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.40.50[1], d.MCAR.cov.90.cl.true.prop.men25.40.F.40.50[1],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.40.50[1], d.MCAR.true.cov.100.prop.men25.40.F.40.50[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men25.40.F.40.50[3], d.MCAR.cov.40.cl.true.prop.men25.40.F.40.50[3],
                                                  d.MCAR.cov.45.cl.true.prop.men25.40.F.40.50[3], d.MCAR.cov.50.cl.true.prop.men25.40.F.40.50[3],
                                                  d.MCAR.cov.55.cl.true.prop.men25.40.F.40.50[3], d.MCAR.cov.60.cl.true.prop.men25.40.F.40.50[3],
                                                  d.MCAR.cov.65.cl.true.prop.men25.40.F.40.50[3], d.MCAR.cov.70.cl.true.prop.men25.40.F.40.50[3],
                                                  d.MCAR.cov.75.cl.true.prop.men25.40.F.40.50[3], d.MCAR.cov.80.cl.true.prop.men25.40.F.40.50[3],
                                                  d.MCAR.cov.85.cl.true.prop.men25.40.F.40.50[3], d.MCAR.cov.90.cl.true.prop.men25.40.F.40.50[3],
                                                  d.MCAR.cov.95.cl.true.prop.men25.40.F.40.50[3], d.MCAR.true.cov.100.prop.men25.40.F.40.50[3]))


plot.cl.true.prop.men25.40.F.40.50 <- ggplot(cl.true.prop.men25.40.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.men25.40.F.40.50.png",
       plot = plot.cl.true.prop.men25.40.F.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women25.40.M.40.50 

cl.true.prop.women25.40.M.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.40.50[2], d.MCAR.cov.40.cl.true.prop.women25.40.M.40.50[2],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.40.50[2], d.MCAR.cov.50.cl.true.prop.women25.40.M.40.50[2],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.40.50[2], d.MCAR.cov.60.cl.true.prop.women25.40.M.40.50[2],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.40.50[2], d.MCAR.cov.70.cl.true.prop.women25.40.M.40.50[2],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.40.50[2], d.MCAR.cov.80.cl.true.prop.women25.40.M.40.50[2],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.40.50[2], d.MCAR.cov.90.cl.true.prop.women25.40.M.40.50[2],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.40.50[2], d.MCAR.true.cov.100.prop.women25.40.M.40.50[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.40.50[1], d.MCAR.cov.40.cl.true.prop.women25.40.M.40.50[1],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.40.50[1], d.MCAR.cov.50.cl.true.prop.women25.40.M.40.50[1],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.40.50[1], d.MCAR.cov.60.cl.true.prop.women25.40.M.40.50[1],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.40.50[1], d.MCAR.cov.70.cl.true.prop.women25.40.M.40.50[1],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.40.50[1], d.MCAR.cov.80.cl.true.prop.women25.40.M.40.50[1],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.40.50[1], d.MCAR.cov.90.cl.true.prop.women25.40.M.40.50[1],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.40.50[1], d.MCAR.true.cov.100.prop.women25.40.M.40.50[2]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women25.40.M.40.50[3], d.MCAR.cov.40.cl.true.prop.women25.40.M.40.50[3],
                                                    d.MCAR.cov.45.cl.true.prop.women25.40.M.40.50[3], d.MCAR.cov.50.cl.true.prop.women25.40.M.40.50[3],
                                                    d.MCAR.cov.55.cl.true.prop.women25.40.M.40.50[3], d.MCAR.cov.60.cl.true.prop.women25.40.M.40.50[3],
                                                    d.MCAR.cov.65.cl.true.prop.women25.40.M.40.50[3], d.MCAR.cov.70.cl.true.prop.women25.40.M.40.50[3],
                                                    d.MCAR.cov.75.cl.true.prop.women25.40.M.40.50[3], d.MCAR.cov.80.cl.true.prop.women25.40.M.40.50[3],
                                                    d.MCAR.cov.85.cl.true.prop.women25.40.M.40.50[3], d.MCAR.cov.90.cl.true.prop.women25.40.M.40.50[3],
                                                    d.MCAR.cov.95.cl.true.prop.women25.40.M.40.50[3], d.MCAR.true.cov.100.prop.women25.40.M.40.50[3]))


plot.cl.true.prop.women25.40.M.40.50 <- ggplot(cl.true.prop.women25.40.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.women25.40.M.40.50.png",
       plot = plot.cl.true.prop.women25.40.M.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men40.50.F.40.50 

cl.true.prop.men40.50.F.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.40.50[2], d.MCAR.cov.40.cl.true.prop.men40.50.F.40.50[2],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.40.50[2], d.MCAR.cov.50.cl.true.prop.men40.50.F.40.50[2],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.40.50[2], d.MCAR.cov.60.cl.true.prop.men40.50.F.40.50[2],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.40.50[2], d.MCAR.cov.70.cl.true.prop.men40.50.F.40.50[2],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.40.50[2], d.MCAR.cov.80.cl.true.prop.men40.50.F.40.50[2],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.40.50[2], d.MCAR.cov.90.cl.true.prop.men40.50.F.40.50[2],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.40.50[2], d.MCAR.true.cov.100.prop.men40.50.F.40.50[2]),
                                            
                                            L = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.40.50[1], d.MCAR.cov.40.cl.true.prop.men40.50.F.40.50[1],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.40.50[1], d.MCAR.cov.50.cl.true.prop.men40.50.F.40.50[1],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.40.50[1], d.MCAR.cov.60.cl.true.prop.men40.50.F.40.50[1],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.40.50[1], d.MCAR.cov.70.cl.true.prop.men40.50.F.40.50[1],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.40.50[1], d.MCAR.cov.80.cl.true.prop.men40.50.F.40.50[1],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.40.50[1], d.MCAR.cov.90.cl.true.prop.men40.50.F.40.50[1],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.40.50[1], d.MCAR.true.cov.100.prop.men40.50.F.40.50[1]),
                                            
                                            U = c(d.MCAR.cov.35.cl.true.prop.men40.50.F.40.50[3], d.MCAR.cov.40.cl.true.prop.men40.50.F.40.50[3],
                                                  d.MCAR.cov.45.cl.true.prop.men40.50.F.40.50[3], d.MCAR.cov.50.cl.true.prop.men40.50.F.40.50[3],
                                                  d.MCAR.cov.55.cl.true.prop.men40.50.F.40.50[3], d.MCAR.cov.60.cl.true.prop.men40.50.F.40.50[3],
                                                  d.MCAR.cov.65.cl.true.prop.men40.50.F.40.50[3], d.MCAR.cov.70.cl.true.prop.men40.50.F.40.50[3],
                                                  d.MCAR.cov.75.cl.true.prop.men40.50.F.40.50[3], d.MCAR.cov.80.cl.true.prop.men40.50.F.40.50[3],
                                                  d.MCAR.cov.85.cl.true.prop.men40.50.F.40.50[3], d.MCAR.cov.90.cl.true.prop.men40.50.F.40.50[3],
                                                  d.MCAR.cov.95.cl.true.prop.men40.50.F.40.50[3], d.MCAR.true.cov.100.prop.men40.50.F.40.50[3]))


plot.cl.true.prop.men40.50.F.40.50 <- ggplot(cl.true.prop.men40.50.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.cl.true.prop.men40.50.F.40.50.png",
       plot = plot.cl.true.prop.men40.50.F.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women40.50.M.40.50 

cl.true.prop.women40.50.M.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.40.50[2], d.MCAR.cov.40.cl.true.prop.women40.50.M.40.50[2],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.40.50[2], d.MCAR.cov.50.cl.true.prop.women40.50.M.40.50[2],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.40.50[2], d.MCAR.cov.60.cl.true.prop.women40.50.M.40.50[2],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.40.50[2], d.MCAR.cov.70.cl.true.prop.women40.50.M.40.50[2],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.40.50[2], d.MCAR.cov.80.cl.true.prop.women40.50.M.40.50[2],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.40.50[2], d.MCAR.cov.90.cl.true.prop.women40.50.M.40.50[2],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.40.50[2], d.MCAR.true.cov.100.prop.women40.50.M.40.50[2]),
                                              
                                              L = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.40.50[1], d.MCAR.cov.40.cl.true.prop.women40.50.M.40.50[1],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.40.50[1], d.MCAR.cov.50.cl.true.prop.women40.50.M.40.50[1],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.40.50[1], d.MCAR.cov.60.cl.true.prop.women40.50.M.40.50[1],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.40.50[1], d.MCAR.cov.70.cl.true.prop.women40.50.M.40.50[1],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.40.50[1], d.MCAR.cov.80.cl.true.prop.women40.50.M.40.50[1],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.40.50[1], d.MCAR.cov.90.cl.true.prop.women40.50.M.40.50[1],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.40.50[1], d.MCAR.true.cov.100.prop.women40.50.M.40.50[1]),
                                              
                                              U = c(d.MCAR.cov.35.cl.true.prop.women40.50.M.40.50[3], d.MCAR.cov.40.cl.true.prop.women40.50.M.40.50[3],
                                                    d.MCAR.cov.45.cl.true.prop.women40.50.M.40.50[3], d.MCAR.cov.50.cl.true.prop.women40.50.M.40.50[3],
                                                    d.MCAR.cov.55.cl.true.prop.women40.50.M.40.50[3], d.MCAR.cov.60.cl.true.prop.women40.50.M.40.50[3],
                                                    d.MCAR.cov.65.cl.true.prop.women40.50.M.40.50[3], d.MCAR.cov.70.cl.true.prop.women40.50.M.40.50[3],
                                                    d.MCAR.cov.75.cl.true.prop.women40.50.M.40.50[3], d.MCAR.cov.80.cl.true.prop.women40.50.M.40.50[3],
                                                    d.MCAR.cov.85.cl.true.prop.women40.50.M.40.50[3], d.MCAR.cov.90.cl.true.prop.women40.50.M.40.50[3],
                                                    d.MCAR.cov.95.cl.true.prop.women40.50.M.40.50[3], d.MCAR.true.cov.100.prop.women40.50.M.40.50[3]))


plot.cl.true.prop.women40.50.M.40.50 <- ggplot(cl.true.prop.women40.50.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.cl.true.prop.women40.50.M.40.50.png",
       plot = plot.cl.true.prop.women40.50.M.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# III. True values from full phylogeny  --------------------------------




# Cov 35 

d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,1])
d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,1])

d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,2])
d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,2])

d.MCAR.cov.35.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,3])
d.MCAR.cov.35.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,3])

d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,4])
d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,4])

d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,5])
d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,5])

d.MCAR.cov.35.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,6])
d.MCAR.cov.35.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,6])


d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,7])
d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,7])

d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,8])
d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,8])

d.MCAR.cov.35.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.men[,9])
d.MCAR.cov.35.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.35.tree.trans.true.prop.women[,9])


# Cov 40
d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,1])
d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,1])

d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,2])
d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,2])

d.MCAR.cov.40.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,3])
d.MCAR.cov.40.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,3])

d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,4])
d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,4])

d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,5])
d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,5])

d.MCAR.cov.40.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,6])
d.MCAR.cov.40.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,6])


d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,7])
d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,7])

d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,8])
d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,8])

d.MCAR.cov.40.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.men[,9])
d.MCAR.cov.40.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.40.tree.trans.true.prop.women[,9])


# Cov 45

d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,1])
d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,1])

d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,2])
d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,2])

d.MCAR.cov.45.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,3])
d.MCAR.cov.45.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,3])

d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,4])
d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,4])

d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,5])
d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,5])

d.MCAR.cov.45.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,6])
d.MCAR.cov.45.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,6])


d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,7])
d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,7])

d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,8])
d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,8])

d.MCAR.cov.45.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.men[,9])
d.MCAR.cov.45.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.45.tree.trans.true.prop.women[,9])


# Cov 50

d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,1])
d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,1])

d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,2])
d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,2])

d.MCAR.cov.50.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,3])
d.MCAR.cov.50.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,3])

d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,4])
d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,4])

d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,5])
d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,5])

d.MCAR.cov.50.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,6])
d.MCAR.cov.50.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,6])


d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,7])
d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,7])

d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,8])
d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,8])

d.MCAR.cov.50.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.men[,9])
d.MCAR.cov.50.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.50.tree.trans.true.prop.women[,9])



# Cov 55

d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,1])
d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,1])

d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,2])
d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,2])

d.MCAR.cov.55.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,3])
d.MCAR.cov.55.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,3])

d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,4])
d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,4])

d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,5])
d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,5])

d.MCAR.cov.55.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,6])
d.MCAR.cov.55.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,6])


d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,7])
d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,7])

d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,8])
d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,8])

d.MCAR.cov.55.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.men[,9])
d.MCAR.cov.55.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.55.tree.trans.true.prop.women[,9])



# Cov 60

d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,1])
d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,1])

d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,2])
d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,2])

d.MCAR.cov.60.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,3])
d.MCAR.cov.60.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,3])

d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,4])
d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,4])

d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,5])
d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,5])

d.MCAR.cov.60.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,6])
d.MCAR.cov.60.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,6])


d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,7])
d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,7])

d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,8])
d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,8])

d.MCAR.cov.60.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.men[,9])
d.MCAR.cov.60.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.60.tree.trans.true.prop.women[,9])



# Cov 65

d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,1])
d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,1])

d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,2])
d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,2])

d.MCAR.cov.65.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,3])
d.MCAR.cov.65.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,3])

d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,4])
d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,4])

d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,5])
d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,5])

d.MCAR.cov.65.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,6])
d.MCAR.cov.65.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,6])


d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,7])
d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,7])

d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,8])
d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,8])

d.MCAR.cov.65.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.men[,9])
d.MCAR.cov.65.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.65.tree.trans.true.prop.women[,9])


# Cov 70

d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,1])
d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,1])

d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,2])
d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,2])

d.MCAR.cov.70.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,3])
d.MCAR.cov.70.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,3])

d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,4])
d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,4])

d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,5])
d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,5])

d.MCAR.cov.70.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,6])
d.MCAR.cov.70.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,6])


d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,7])
d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,7])

d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,8])
d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,8])

d.MCAR.cov.70.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.men[,9])
d.MCAR.cov.70.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.70.tree.trans.true.prop.women[,9])



# Cov 75

d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,1])
d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,1])

d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,2])
d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,2])

d.MCAR.cov.75.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,3])
d.MCAR.cov.75.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,3])

d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,4])
d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,4])

d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,5])
d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,5])

d.MCAR.cov.75.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,6])
d.MCAR.cov.75.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,6])


d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,7])
d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,7])

d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,8])
d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,8])

d.MCAR.cov.75.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.men[,9])
d.MCAR.cov.75.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.75.tree.trans.true.prop.women[,9])



# Cov 80

d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,1])
d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,1])

d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,2])
d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,2])

d.MCAR.cov.80.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,3])
d.MCAR.cov.80.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,3])

d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,4])
d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,4])

d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,5])
d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,5])

d.MCAR.cov.80.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,6])
d.MCAR.cov.80.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,6])


d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,7])
d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,7])

d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,8])
d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,8])

d.MCAR.cov.80.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.men[,9])
d.MCAR.cov.80.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.80.tree.trans.true.prop.women[,9])



# Cov 85

d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,1])
d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,1])

d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,2])
d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,2])

d.MCAR.cov.85.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,3])
d.MCAR.cov.85.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,3])

d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,4])
d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,4])

d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,5])
d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,5])

d.MCAR.cov.85.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,6])
d.MCAR.cov.85.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,6])


d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,7])
d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,7])

d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,8])
d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,8])

d.MCAR.cov.85.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.men[,9])
d.MCAR.cov.85.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.85.tree.trans.true.prop.women[,9])



# Cov 90


d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,1])
d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,1])

d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,2])
d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,2])

d.MCAR.cov.90.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,3])
d.MCAR.cov.90.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,3])

d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,4])
d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,4])

d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,5])
d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,5])

d.MCAR.cov.90.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,6])
d.MCAR.cov.90.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,6])


d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,7])
d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,7])

d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,8])
d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,8])

d.MCAR.cov.90.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.men[,9])
d.MCAR.cov.90.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.90.tree.trans.true.prop.women[,9])


# Cov 95

d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.15.25 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,1])
d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.15.25 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,1])

d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.15.25 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,2])
d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.15.25 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,2])

d.MCAR.cov.95.tree.trans.true.prop.men40.50.F.15.25 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,3])
d.MCAR.cov.95.tree.trans.true.prop.women40.50.M.15.25 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,3])

d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.25.40 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,4])
d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.25.40 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,4])

d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.25.40 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,5])
d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.25.40 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,5])

d.MCAR.cov.95.tree.trans.true.prop.men40.50.F.25.40 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,6])
d.MCAR.cov.95.tree.trans.true.prop.women40.50.M.25.40 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,6])


d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.40.50 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,7])
d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.40.50 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,7])

d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.40.50 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,8])
d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.40.50 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,8])

d.MCAR.cov.95.tree.trans.true.prop.men40.50.F.40.50 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.men[,9])
d.MCAR.cov.95.tree.trans.true.prop.women40.50.M.40.50 <- quant.med(d.MCAR.cov.95.tree.trans.true.prop.women[,9])






# Visualising III in different MCAR coverages -------------------------------


############## AAAAAAAA : 15.25 - 40.50 and 15.25

# Men: men15.25.F.15.25

tree.trans.true.prop.men15.25.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                    
                                                    F = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.15.25[2], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.15.25[2],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.15.25[2], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.15.25[2],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.15.25[2], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.15.25[2],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.15.25[2], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.15.25[2],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.15.25[2], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.15.25[2],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.15.25[2], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.15.25[2],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.15.25[2], d.MCAR.true.cov.100.prop.men15.25.F.15.25[2]),
                                                    
                                                    L = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.15.25[1], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.15.25[1],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.15.25[1], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.15.25[1],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.15.25[1], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.15.25[1],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.15.25[1], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.15.25[1],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.15.25[1], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.15.25[1],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.15.25[1], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.15.25[1],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.15.25[1], d.MCAR.true.cov.100.prop.men15.25.F.15.25[1]),
                                                    
                                                    U = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.15.25[3], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.15.25[3],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.15.25[3], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.15.25[3],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.15.25[3], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.15.25[3],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.15.25[3], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.15.25[3],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.15.25[3], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.15.25[3],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.15.25[3], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.15.25[3],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.15.25[3], d.MCAR.true.cov.100.prop.men15.25.F.15.25[3]))


plot.tree.trans.true.prop.men15.25.F.15.25 <- ggplot(tree.trans.true.prop.men15.25.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings between of men in 15 - 25 paired with women in 15 - 25 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.men15.25.F.15.25.png",
       plot = plot.tree.trans.true.prop.men15.25.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women15.25.M.15.25

tree.trans.true.prop.women15.25.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                      
                                                      F = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.15.25[2], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.15.25[2],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.15.25[2], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.15.25[2],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.15.25[2], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.15.25[2],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.15.25[2], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.15.25[2],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.15.25[2], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.15.25[2],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.15.25[2], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.15.25[2],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.15.25[2], d.MCAR.true.cov.100.prop.women15.25.M.15.25[2]),
                                                      
                                                      L = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.15.25[1], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.15.25[1],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.15.25[1], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.15.25[1],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.15.25[1], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.15.25[1],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.15.25[1], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.15.25[1],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.15.25[1], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.15.25[1],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.15.25[1], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.15.25[1],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.15.25[1], d.MCAR.true.cov.100.prop.women15.25.M.15.25[1]),
                                                      
                                                      U = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.15.25[3], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.15.25[3],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.15.25[3], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.15.25[3],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.15.25[3], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.15.25[3],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.15.25[3], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.15.25[3],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.15.25[3], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.15.25[3],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.15.25[3], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.15.25[3],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.15.25[3], d.MCAR.true.cov.100.prop.women15.25.M.15.25[3]))



plot.tree.trans.true.prop.women15.25.M.15.25 <- ggplot(tree.trans.true.prop.women15.25.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in 15 - 25 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.women15.25.M.15.25.png",
       plot = plot.tree.trans.true.prop.women15.25.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men25.40.F.15.25 

tree.trans.true.prop.men25.40.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                    
                                                    F = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.15.25[2], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.15.25[2],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.15.25[2], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.15.25[2],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.15.25[2], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.15.25[2],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.15.25[2], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.15.25[2],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.15.25[2], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.15.25[2],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.15.25[2], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.15.25[2],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.15.25[2], d.MCAR.true.cov.100.prop.men25.40.F.15.25[2]),
                                                    
                                                    L = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.15.25[1], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.15.25[1],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.15.25[1], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.15.25[1],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.15.25[1], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.15.25[1],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.15.25[1], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.15.25[1],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.15.25[1], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.15.25[1],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.15.25[1], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.15.25[1],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.15.25[1], d.MCAR.true.cov.100.prop.men25.40.F.15.25[2]),
                                                    
                                                    U = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.15.25[3], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.15.25[3],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.15.25[3], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.15.25[3],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.15.25[3], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.15.25[3],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.15.25[3], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.15.25[3],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.15.25[3], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.15.25[3],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.15.25[3], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.15.25[3],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.15.25[3], d.MCAR.true.cov.100.prop.men25.40.F.15.25[3]))


plot.tree.trans.true.prop.men25.40.F.15.25 <- ggplot(tree.trans.true.prop.men25.40.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.men25.40.F.15.25.png",
       plot = plot.tree.trans.true.prop.men25.40.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women25.40.M.15.25 

tree.trans.true.prop.women25.40.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                      
                                                      F = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.15.25[2], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.15.25[2],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.15.25[2], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.15.25[2],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.15.25[2], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.15.25[2],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.15.25[2], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.15.25[2],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.15.25[2], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.15.25[2],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.15.25[2], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.15.25[2],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.15.25[2], d.MCAR.true.cov.100.prop.women25.40.M.15.25[2]),
                                                      
                                                      L = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.15.25[1], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.15.25[1],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.15.25[1], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.15.25[1],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.15.25[1], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.15.25[1],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.15.25[1], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.15.25[1],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.15.25[1], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.15.25[1],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.15.25[1], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.15.25[1],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.15.25[1], d.MCAR.true.cov.100.prop.women25.40.M.15.25[1]),
                                                      
                                                      U = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.15.25[3], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.15.25[3],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.15.25[3], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.15.25[3],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.15.25[3], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.15.25[3],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.15.25[3], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.15.25[3],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.15.25[3], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.15.25[3],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.15.25[3], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.15.25[3],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.15.25[3], d.MCAR.true.cov.100.prop.women25.40.M.15.25[3]))


plot.tree.trans.true.prop.women25.40.M.15.25 <- ggplot(tree.trans.true.prop.women25.40.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.women25.40.M.15.25.png",
       plot = plot.tree.trans.true.prop.women25.40.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: men40.50.F.15.25 

tree.trans.true.prop.men40.50.F.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                    
                                                    F = c(d.MCAR.cov.35.tree.trans.true.prop.men40.50.F.15.25[2], d.MCAR.cov.40.tree.trans.true.prop.men40.50.F.15.25[2],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men40.50.F.15.25[2], d.MCAR.cov.50.tree.trans.true.prop.men40.50.F.15.25[2],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men40.50.F.15.25[2], d.MCAR.cov.60.tree.trans.true.prop.men40.50.F.15.25[2],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men40.50.F.15.25[2], d.MCAR.cov.70.tree.trans.true.prop.men40.50.F.15.25[2],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men40.50.F.15.25[2], d.MCAR.cov.80.tree.trans.true.prop.men40.50.F.15.25[2],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men40.50.F.15.25[2], d.MCAR.cov.90.tree.trans.true.prop.men40.50.F.15.25[2],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men40.50.F.15.25[2], d.MCAR.true.cov.100.prop.men40.50.F.15.25[2]),
                                                    
                                                    L = c(d.MCAR.cov.35.tree.trans.true.prop.men40.50.F.15.25[1], d.MCAR.cov.40.tree.trans.true.prop.men40.50.F.15.25[1],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men40.50.F.15.25[1], d.MCAR.cov.50.tree.trans.true.prop.men40.50.F.15.25[1],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men40.50.F.15.25[1], d.MCAR.cov.60.tree.trans.true.prop.men40.50.F.15.25[1],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men40.50.F.15.25[1], d.MCAR.cov.70.tree.trans.true.prop.men40.50.F.15.25[1],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men40.50.F.15.25[1], d.MCAR.cov.80.tree.trans.true.prop.men40.50.F.15.25[1],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men40.50.F.15.25[1], d.MCAR.cov.90.tree.trans.true.prop.men40.50.F.15.25[1],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men40.50.F.15.25[1], d.MCAR.true.cov.100.prop.men40.50.F.15.25[1]),
                                                    
                                                    U = c(d.MCAR.cov.35.tree.trans.true.prop.men40.50.F.15.25[3], d.MCAR.cov.40.tree.trans.true.prop.men40.50.F.15.25[3],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men40.50.F.15.25[3], d.MCAR.cov.50.tree.trans.true.prop.men40.50.F.15.25[3],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men40.50.F.15.25[3], d.MCAR.cov.60.tree.trans.true.prop.men40.50.F.15.25[3],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men40.50.F.15.25[3], d.MCAR.cov.70.tree.trans.true.prop.men40.50.F.15.25[3],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men40.50.F.15.25[3], d.MCAR.cov.80.tree.trans.true.prop.men40.50.F.15.25[3],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men40.50.F.15.25[3], d.MCAR.cov.90.tree.trans.true.prop.men40.50.F.15.25[3],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men40.50.F.15.25[3], d.MCAR.true.cov.100.prop.men40.50.F.15.25[3]))


plot.tree.trans.true.prop.men40.50.F.15.25 <- ggplot(tree.trans.true.prop.men40.50.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.men40.50.F.15.25.png",
       plot = plot.tree.trans.true.prop.men40.50.F.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women40.50.M.15.25 

tree.trans.true.prop.women40.50.M.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                      
                                                      F = c(d.MCAR.cov.35.tree.trans.true.prop.women40.50.M.15.25[2], d.MCAR.cov.40.tree.trans.true.prop.women40.50.M.15.25[2],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women40.50.M.15.25[2], d.MCAR.cov.50.tree.trans.true.prop.women40.50.M.15.25[2],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women40.50.M.15.25[2], d.MCAR.cov.60.tree.trans.true.prop.women40.50.M.15.25[2],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women40.50.M.15.25[2], d.MCAR.cov.70.tree.trans.true.prop.women40.50.M.15.25[2],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women40.50.M.15.25[2], d.MCAR.cov.80.tree.trans.true.prop.women40.50.M.15.25[2],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women40.50.M.15.25[2], d.MCAR.cov.90.tree.trans.true.prop.women40.50.M.15.25[2],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women40.50.M.15.25[2], d.MCAR.true.cov.100.prop.women40.50.M.15.25[2]),
                                                      
                                                      L = c(d.MCAR.cov.35.tree.trans.true.prop.women40.50.M.15.25[1], d.MCAR.cov.40.tree.trans.true.prop.women40.50.M.15.25[1],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women40.50.M.15.25[1], d.MCAR.cov.50.tree.trans.true.prop.women40.50.M.15.25[1],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women40.50.M.15.25[1], d.MCAR.cov.60.tree.trans.true.prop.women40.50.M.15.25[1],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women40.50.M.15.25[1], d.MCAR.cov.70.tree.trans.true.prop.women40.50.M.15.25[1],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women40.50.M.15.25[1], d.MCAR.cov.80.tree.trans.true.prop.women40.50.M.15.25[1],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women40.50.M.15.25[1], d.MCAR.cov.90.tree.trans.true.prop.women40.50.M.15.25[1],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women40.50.M.15.25[1], d.MCAR.true.cov.100.prop.women40.50.M.15.25[1]),
                                                      
                                                      U = c(d.MCAR.cov.35.tree.trans.true.prop.women40.50.M.15.25[3], d.MCAR.cov.40.tree.trans.true.prop.women40.50.M.15.25[3],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women40.50.M.15.25[3], d.MCAR.cov.50.tree.trans.true.prop.women40.50.M.15.25[3],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women40.50.M.15.25[3], d.MCAR.cov.60.tree.trans.true.prop.women40.50.M.15.25[3],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women40.50.M.15.25[3], d.MCAR.cov.70.tree.trans.true.prop.women40.50.M.15.25[3],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women40.50.M.15.25[3], d.MCAR.cov.80.tree.trans.true.prop.women40.50.M.15.25[3],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women40.50.M.15.25[3], d.MCAR.cov.90.tree.trans.true.prop.women40.50.M.15.25[3],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women40.50.M.15.25[3], d.MCAR.true.cov.100.prop.women40.50.M.15.25[3]))


plot.tree.trans.true.prop.women40.50.M.15.25 <- ggplot(tree.trans.true.prop.women40.50.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.women40.50.M.15.25.png",
       plot = plot.tree.trans.true.prop.women40.50.M.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



############## BBBBBBBBBB : 15.25 - 40.50 and 25.40



# Men: men15.25.F.25.40

tree.trans.true.prop.men15.25.F.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                    
                                                    F = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.25.40[2], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.25.40[2],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.25.40[2], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.25.40[2],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.25.40[2], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.25.40[2],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.25.40[2], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.25.40[2],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.25.40[2], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.25.40[2],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.25.40[2], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.25.40[2],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.25.40[2], d.MCAR.true.cov.100.prop.men15.25.F.25.40[2]),
                                                    
                                                    L = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.25.40[1], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.25.40[1],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.25.40[1], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.25.40[1],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.25.40[1], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.25.40[1],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.25.40[1], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.25.40[1],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.25.40[1], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.25.40[1],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.25.40[1], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.25.40[1],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.25.40[1], d.MCAR.true.cov.100.prop.men15.25.F.25.40[1]),
                                                    
                                                    U = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.25.40[3], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.25.40[3],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.25.40[3], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.25.40[3],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.25.40[3], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.25.40[3],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.25.40[3], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.25.40[3],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.25.40[3], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.25.40[3],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.25.40[3], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.25.40[3],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.25.40[3], d.MCAR.true.cov.100.prop.men15.25.F.25.40[3]))


plot.tree.trans.true.prop.men15.25.F.25.40 <- ggplot(tree.trans.true.prop.men15.25.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings between of men in 15 - 25 paired with women in 25 - 40 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.men15.25.F.25.40.png",
       plot = plot.tree.trans.true.prop.men15.25.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women15.25.M.25.40

tree.trans.true.prop.women15.25.M.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                      
                                                      F = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.25.40[2], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.25.40[2],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.25.40[2], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.25.40[2],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.25.40[2], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.25.40[2],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.25.40[2], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.25.40[2],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.25.40[2], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.25.40[2],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.25.40[2], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.25.40[2],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.25.40[2], d.MCAR.true.cov.100.prop.women15.25.M.25.40[2]),
                                                      
                                                      L = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.25.40[1], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.25.40[1],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.25.40[1], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.25.40[1],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.25.40[1], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.25.40[1],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.25.40[1], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.25.40[1],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.25.40[1], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.25.40[1],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.25.40[1], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.25.40[1],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.25.40[1], d.MCAR.true.cov.100.prop.women15.25.M.25.40[1]),
                                                      
                                                      U = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.25.40[3], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.25.40[3],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.25.40[3], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.25.40[3],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.25.40[3], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.25.40[3],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.25.40[3], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.25.40[3],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.25.40[3], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.25.40[3],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.25.40[3], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.25.40[3],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.25.40[3], d.MCAR.true.cov.100.prop.women15.25.M.25.40[3]))



plot.tree.trans.true.prop.women15.25.M.25.40 <- ggplot(tree.trans.true.prop.women15.25.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in  25- 40 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.tree.trans.true.prop.women15.25.M.25.40.png",
       plot = plot.tree.trans.true.prop.women15.25.M.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")





# Men: men25.40.F.25.40 

tree.trans.true.prop.men25.40.F.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                    
                                                    F = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.25.40[2], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.25.40[2],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.25.40[2], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.25.40[2],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.25.40[2], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.25.40[2],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.25.40[2], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.25.40[2],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.25.40[2], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.25.40[2],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.25.40[2], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.25.40[2],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.25.40[2], d.MCAR.true.cov.100.prop.men25.40.F.25.40[2]),
                                                    
                                                    L = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.25.40[1], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.25.40[1],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.25.40[1], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.25.40[1],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.25.40[1], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.25.40[1],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.25.40[1], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.25.40[1],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.25.40[1], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.25.40[1],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.25.40[1], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.25.40[1],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.25.40[1], d.MCAR.true.cov.100.prop.men25.40.F.25.40[2]),
                                                    
                                                    U = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.25.40[3], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.25.40[3],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.25.40[3], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.25.40[3],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.25.40[3], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.25.40[3],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.25.40[3], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.25.40[3],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.25.40[3], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.25.40[3],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.25.40[3], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.25.40[3],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.25.40[3], d.MCAR.true.cov.100.prop.men25.40.F.25.40[3]))


plot.tree.trans.true.prop.men25.40.F.25.40 <- ggplot(tree.trans.true.prop.men25.40.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


ggsave(filename = "plot.tree.trans.true.prop.men25.40.F.25.40.png",
       plot = plot.tree.trans.true.prop.men25.40.F.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# Women: women25.40.M.25.40 

tree.trans.true.prop.women25.40.M.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                      
                                                      F = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.25.40[2], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.25.40[2],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.25.40[2], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.25.40[2],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.25.40[2], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.25.40[2],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.25.40[2], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.25.40[2],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.25.40[2], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.25.40[2],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.25.40[2], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.25.40[2],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.25.40[2], d.MCAR.true.cov.100.prop.women25.40.M.25.40[2]),
                                                      
                                                      L = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.25.40[1], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.25.40[1],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.25.40[1], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.25.40[1],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.25.40[1], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.25.40[1],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.25.40[1], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.25.40[1],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.25.40[1], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.25.40[1],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.25.40[1], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.25.40[1],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.25.40[1], d.MCAR.true.cov.100.prop.women25.40.M.25.40[1]),
                                                      
                                                      U = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.25.40[3], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.25.40[3],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.25.40[3], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.25.40[3],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.25.40[3], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.25.40[3],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.25.40[3], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.25.40[3],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.25.40[3], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.25.40[3],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.25.40[3], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.25.40[3],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.25.40[3], d.MCAR.true.cov.100.prop.women25.40.M.25.40[3]))


plot.tree.trans.true.prop.women25.40.M.25.40 <- ggplot(tree.trans.true.prop.women25.40.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.women25.40.M.25.40.png",
       plot = plot.tree.trans.true.prop.women25.40.M.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



############## CCCCCCC : 15.25 - 40.50 and 40.50



# Men: men15.25.F.40.50

tree.trans.true.prop.men15.25.F.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                    
                                                    F = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.40.50[2], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.40.50[2],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.40.50[2], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.40.50[2],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.40.50[2], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.40.50[2],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.40.50[2], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.40.50[2],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.40.50[2], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.40.50[2],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.40.50[2], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.40.50[2],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.40.50[2], d.MCAR.true.cov.100.prop.men15.25.F.40.50[2]),
                                                    
                                                    L = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.40.50[1], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.40.50[1],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.40.50[1], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.40.50[1],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.40.50[1], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.40.50[1],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.40.50[1], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.40.50[1],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.40.50[1], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.40.50[1],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.40.50[1], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.40.50[1],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.40.50[1], d.MCAR.true.cov.100.prop.men15.25.F.40.50[1]),
                                                    
                                                    U = c(d.MCAR.cov.35.tree.trans.true.prop.men15.25.F.40.50[3], d.MCAR.cov.40.tree.trans.true.prop.men15.25.F.40.50[3],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men15.25.F.40.50[3], d.MCAR.cov.50.tree.trans.true.prop.men15.25.F.40.50[3],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men15.25.F.40.50[3], d.MCAR.cov.60.tree.trans.true.prop.men15.25.F.40.50[3],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men15.25.F.40.50[3], d.MCAR.cov.70.tree.trans.true.prop.men15.25.F.40.50[3],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men15.25.F.40.50[3], d.MCAR.cov.80.tree.trans.true.prop.men15.25.F.40.50[3],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men15.25.F.40.50[3], d.MCAR.cov.90.tree.trans.true.prop.men15.25.F.40.50[3],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men15.25.F.40.50[3], d.MCAR.true.cov.100.prop.men15.25.F.40.50[3]))


plot.tree.trans.true.prop.men15.25.F.40.50 <- ggplot(tree.trans.true.prop.men15.25.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings between of men in 15 - 25 paired with women in  40 - 50 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.men15.25.F.40.50.png",
       plot = plot.tree.trans.true.prop.men15.25.F.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: women15.25.M.40.50

tree.trans.true.prop.women15.25.M.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                      
                                                      F = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.40.50[2], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.40.50[2],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.40.50[2], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.40.50[2],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.40.50[2], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.40.50[2],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.40.50[2], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.40.50[2],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.40.50[2], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.40.50[2],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.40.50[2], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.40.50[2],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.40.50[2], d.MCAR.true.cov.100.prop.women15.25.M.40.50[2]),
                                                      
                                                      L = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.40.50[1], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.40.50[1],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.40.50[1], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.40.50[1],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.40.50[1], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.40.50[1],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.40.50[1], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.40.50[1],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.40.50[1], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.40.50[1],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.40.50[1], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.40.50[1],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.40.50[1], d.MCAR.true.cov.100.prop.women15.25.M.40.50[1]),
                                                      
                                                      U = c(d.MCAR.cov.35.tree.trans.true.prop.women15.25.M.40.50[3], d.MCAR.cov.40.tree.trans.true.prop.women15.25.M.40.50[3],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women15.25.M.40.50[3], d.MCAR.cov.50.tree.trans.true.prop.women15.25.M.40.50[3],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women15.25.M.40.50[3], d.MCAR.cov.60.tree.trans.true.prop.women15.25.M.40.50[3],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women15.25.M.40.50[3], d.MCAR.cov.70.tree.trans.true.prop.women15.25.M.40.50[3],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women15.25.M.40.50[3], d.MCAR.cov.80.tree.trans.true.prop.women15.25.M.40.50[3],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women15.25.M.40.50[3], d.MCAR.cov.90.tree.trans.true.prop.women15.25.M.40.50[3],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women15.25.M.40.50[3], d.MCAR.true.cov.100.prop.women15.25.M.40.50[3]))



plot.tree.trans.true.prop.women15.25.M.40.50 <- ggplot(tree.trans.true.prop.women15.25.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in  40 - 50 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.women15.25.M.40.50.png",
       plot = plot.tree.trans.true.prop.women15.25.M.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# Men: men25.40.F.40.50 

tree.trans.true.prop.men25.40.F.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                    
                                                    F = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.40.50[2], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.40.50[2],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.40.50[2], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.40.50[2],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.40.50[2], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.40.50[2],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.40.50[2], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.40.50[2],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.40.50[2], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.40.50[2],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.40.50[2], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.40.50[2],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.40.50[2], d.MCAR.true.cov.100.prop.men25.40.F.40.50[2]),
                                                    
                                                    L = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.40.50[1], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.40.50[1],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.40.50[1], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.40.50[1],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.40.50[1], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.40.50[1],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.40.50[1], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.40.50[1],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.40.50[1], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.40.50[1],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.40.50[1], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.40.50[1],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.40.50[1], d.MCAR.true.cov.100.prop.men25.40.F.40.50[2]),
                                                    
                                                    U = c(d.MCAR.cov.35.tree.trans.true.prop.men25.40.F.40.50[3], d.MCAR.cov.40.tree.trans.true.prop.men25.40.F.40.50[3],
                                                          d.MCAR.cov.45.tree.trans.true.prop.men25.40.F.40.50[3], d.MCAR.cov.50.tree.trans.true.prop.men25.40.F.40.50[3],
                                                          d.MCAR.cov.55.tree.trans.true.prop.men25.40.F.40.50[3], d.MCAR.cov.60.tree.trans.true.prop.men25.40.F.40.50[3],
                                                          d.MCAR.cov.65.tree.trans.true.prop.men25.40.F.40.50[3], d.MCAR.cov.70.tree.trans.true.prop.men25.40.F.40.50[3],
                                                          d.MCAR.cov.75.tree.trans.true.prop.men25.40.F.40.50[3], d.MCAR.cov.80.tree.trans.true.prop.men25.40.F.40.50[3],
                                                          d.MCAR.cov.85.tree.trans.true.prop.men25.40.F.40.50[3], d.MCAR.cov.90.tree.trans.true.prop.men25.40.F.40.50[3],
                                                          d.MCAR.cov.95.tree.trans.true.prop.men25.40.F.40.50[3], d.MCAR.true.cov.100.prop.men25.40.F.40.50[3]))


plot.tree.trans.true.prop.men25.40.F.40.50 <- ggplot(tree.trans.true.prop.men25.40.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in  40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.men25.40.F.40.50.png",
       plot = plot.tree.trans.true.prop.men25.40.F.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: women25.40.M.40.50 

tree.trans.true.prop.women25.40.M.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                                      
                                                      F = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.40.50[2], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.40.50[2],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.40.50[2], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.40.50[2],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.40.50[2], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.40.50[2],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.40.50[2], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.40.50[2],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.40.50[2], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.40.50[2],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.40.50[2], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.40.50[2],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.40.50[2], d.MCAR.true.cov.100.prop.women25.40.M.40.50[2]),
                                                      
                                                      L = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.40.50[1], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.40.50[1],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.40.50[1], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.40.50[1],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.40.50[1], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.40.50[1],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.40.50[1], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.40.50[1],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.40.50[1], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.40.50[1],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.40.50[1], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.40.50[1],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.40.50[1], d.MCAR.true.cov.100.prop.women25.40.M.40.50[1]),
                                                      
                                                      U = c(d.MCAR.cov.35.tree.trans.true.prop.women25.40.M.40.50[3], d.MCAR.cov.40.tree.trans.true.prop.women25.40.M.40.50[3],
                                                            d.MCAR.cov.45.tree.trans.true.prop.women25.40.M.40.50[3], d.MCAR.cov.50.tree.trans.true.prop.women25.40.M.40.50[3],
                                                            d.MCAR.cov.55.tree.trans.true.prop.women25.40.M.40.50[3], d.MCAR.cov.60.tree.trans.true.prop.women25.40.M.40.50[3],
                                                            d.MCAR.cov.65.tree.trans.true.prop.women25.40.M.40.50[3], d.MCAR.cov.70.tree.trans.true.prop.women25.40.M.40.50[3],
                                                            d.MCAR.cov.75.tree.trans.true.prop.women25.40.M.40.50[3], d.MCAR.cov.80.tree.trans.true.prop.women25.40.M.40.50[3],
                                                            d.MCAR.cov.85.tree.trans.true.prop.women25.40.M.40.50[3], d.MCAR.cov.90.tree.trans.true.prop.women25.40.M.40.50[3],
                                                            d.MCAR.cov.95.tree.trans.true.prop.women25.40.M.40.50[3], d.MCAR.true.cov.100.prop.women25.40.M.40.50[3]))


plot.tree.trans.true.prop.women25.40.M.40.50 <- ggplot(tree.trans.true.prop.women25.40.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in  40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")

ggsave(filename = "plot.tree.trans.true.prop.women25.40.M.40.50.png",
       plot = plot.tree.trans.true.prop.women25.40.M.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Statistics of age difference  of individuals in pairings ----------------


AD.MCAR.cov.35 <- d.MCAR.cov.35 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.40 <- d.MCAR.cov.40 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.45 <- d.MCAR.cov.45 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.50 <- d.MCAR.cov.50 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.55 <- d.MCAR.cov.55 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.60 <- d.MCAR.cov.60 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.65 <- d.MCAR.cov.65 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.70 <- d.MCAR.cov.70 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.75 <- d.MCAR.cov.75 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.80 <- d.MCAR.cov.80 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.85 <- d.MCAR.cov.85 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.90 <- d.MCAR.cov.90 %>%
  select(contains(".AD.")) 
AD.MCAR.cov.95 <- d.MCAR.cov.95 %>%
  select(contains(".AD.")) 


# Statistics of age difference  of individuals's true pairings at 100% coverage ----------------


AD.true.cov.100 <- dr.cov.100 %>%
  select(contains(".AD.")) 



## Vector

# Mean

vector.mean.AD.num.women.true.cov.100.15.25 <- AD.true.cov.100[,1]
vector.mean.AD.num.men.true.cov.100.15.25 <- AD.true.cov.100[,2]

vector.mean.AD.num.women.true.cov.100.25.40 <- AD.true.cov.100[,3]
vector.mean.AD.num.men.true.cov.100.25.40 <- AD.true.cov.100[,4]

vector.mean.AD.num.women.true.cov.100.40.50 <- AD.true.cov.100[,5]
vector.mean.AD.num.men.true.cov.100.40.50 <- AD.true.cov.100[,6]

# Median

vector.med.AD.num.women.true.cov.100.15.25 <- AD.true.cov.100[,7]
vector.med.AD.num.men.true.cov.100.15.25 <- AD.true.cov.100[,8]

vector.med.AD.num.women.true.cov.100.25.40 <- AD.true.cov.100[,9]
vector.med.AD.num.men.true.cov.100.25.40 <- AD.true.cov.100[,10]

vector.med.AD.num.women.true.cov.100.40.50 <- AD.true.cov.100[,11]
vector.med.AD.num.men.true.cov.100.40.50 <- AD.true.cov.100[,12]

# Standard deviation

vector.sd.AD.num.women.true.cov.100.15.25 <- AD.true.cov.100[,13]
vector.sd.AD.num.men.true.cov.100.15.25 <- AD.true.cov.100[,14]

vector.sd.AD.num.women.true.cov.100.25.40 <- AD.true.cov.100[,15]
vector.sd.AD.num.men.true.cov.100.25.40 <- AD.true.cov.100[,16]

vector.sd.AD.num.women.true.cov.100.40.50 <- AD.true.cov.100[,17]
vector.sd.AD.num.men.true.cov.100.40.50 <- AD.true.cov.100[,18]



# Summarised

# Mean

mean.AD.num.women.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,1])
mean.AD.num.men.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,2])

mean.AD.num.women.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,3])
mean.AD.num.men.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,4])

mean.AD.num.women.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,5])
mean.AD.num.men.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,6])

# Median

med.AD.num.women.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,7])
med.AD.num.men.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,8])

med.AD.num.women.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,9])
med.AD.num.men.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,10])

med.AD.num.women.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,11])
med.AD.num.men.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,12])

# Standard deviation

sd.AD.num.women.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,13])
sd.AD.num.men.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,14])

sd.AD.num.women.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,15])
sd.AD.num.men.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,16])

sd.AD.num.women.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,17])
sd.AD.num.men.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,18])


# Statistics of age difference  of individuals's in pairings  ----------------

# From transmission clusters

# Cov 35

## Vector

# Mean

vector.mean.MCAR.cov.35.AD.women.cl.15.25 <- AD.MCAR.cov.35[,1]
vector.mean.MCAR.cov.35.AD.men.cl.15.25 <- AD.MCAR.cov.35[,2]

vector.mean.MCAR.cov.35.AD.women.cl.25.40 <- AD.MCAR.cov.35[,3]
vector.mean.MCAR.cov.35.AD.men.cl.25.40 <- AD.MCAR.cov.35[,4]

vector.mean.MCAR.cov.35.AD.women.cl.40.50 <- AD.MCAR.cov.35[,5]
vector.mean.MCAR.cov.35.AD.men.cl.40.50 <- AD.MCAR.cov.35[,6]

# Median

vector.med.MCAR.cov.35.AD.women.cl.15.25 <- AD.MCAR.cov.35[,7]
vector.med.MCAR.cov.35.AD.men.cl.15.25 <- AD.MCAR.cov.35[,8]

vector.med.MCAR.cov.35.AD.women.cl.25.40 <- AD.MCAR.cov.35[,9]
vector.med.MCAR.cov.35.AD.men.cl.25.40 <- AD.MCAR.cov.35[,10]

vector.med.MCAR.cov.35.AD.women.cl.40.50 <- AD.MCAR.cov.35[,11]
vector.med.MCAR.cov.35.AD.men.cl.40.50 <- AD.MCAR.cov.35[,12]

# Standard deviation

vector.sd.MCAR.cov.35.AD.women.cl.15.25 <- AD.MCAR.cov.35[,13]
vector.sd.MCAR.cov.35.AD.men.cl.15.25 <- AD.MCAR.cov.35[,14]

vector.sd.MCAR.cov.35.AD.women.cl.25.40 <- AD.MCAR.cov.35[,15]
vector.sd.MCAR.cov.35.AD.men.cl.25.40 <- AD.MCAR.cov.35[,16]

vector.sd.MCAR.cov.35.AD.women.cl.40.50 <- AD.MCAR.cov.35[,17]
vector.sd.MCAR.cov.35.AD.men.cl.40.50 <- AD.MCAR.cov.35[,18]


## Summarised

# Mean

mean.MCAR.cov.35.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.35[,1])
mean.MCAR.cov.35.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.35[,2])

mean.MCAR.cov.35.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.35[,3])
mean.MCAR.cov.35.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.35[,4])

mean.MCAR.cov.35.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.35[,5])
mean.MCAR.cov.35.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.35[,6])

# Median

med.MCAR.cov.35.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.35[,7])
med.MCAR.cov.35.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.35[,8])

med.MCAR.cov.35.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.35[,9])
med.MCAR.cov.35.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.35[,10])

med.MCAR.cov.35.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.35[,11])
med.MCAR.cov.35.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.35[,12])

# Standard deviation

sd.MCAR.cov.35.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.35[,13])
sd.MCAR.cov.35.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.35[,14])

sd.MCAR.cov.35.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.35[,15])
sd.MCAR.cov.35.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.35[,16])

sd.MCAR.cov.35.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.35[,17])
sd.MCAR.cov.35.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.35[,18])


# Cov 40


## Vector

# Mean

vector.mean.MCAR.cov.40.AD.women.cl.15.25 <- AD.MCAR.cov.40[,1]
vector.mean.MCAR.cov.40.AD.men.cl.15.25 <- AD.MCAR.cov.40[,2]

vector.mean.MCAR.cov.40.AD.women.cl.25.40 <- AD.MCAR.cov.40[,3]
vector.mean.MCAR.cov.40.AD.men.cl.25.40 <- AD.MCAR.cov.40[,4]

vector.mean.MCAR.cov.40.AD.women.cl.40.50 <- AD.MCAR.cov.40[,5]
vector.mean.MCAR.cov.40.AD.men.cl.40.50 <- AD.MCAR.cov.40[,6]

# Median

vector.med.MCAR.cov.40.AD.women.cl.15.25 <- AD.MCAR.cov.40[,7]
vector.med.MCAR.cov.40.AD.men.cl.15.25 <- AD.MCAR.cov.40[,8]

vector.med.MCAR.cov.40.AD.women.cl.25.40 <- AD.MCAR.cov.40[,9]
vector.med.MCAR.cov.40.AD.men.cl.25.40 <- AD.MCAR.cov.40[,10]

vector.med.MCAR.cov.40.AD.women.cl.40.50 <- AD.MCAR.cov.40[,11]
vector.med.MCAR.cov.40.AD.men.cl.40.50 <- AD.MCAR.cov.40[,12]

# Standard deviation

vector.sd.MCAR.cov.40.AD.women.cl.15.25 <- AD.MCAR.cov.40[,13]
vector.sd.MCAR.cov.40.AD.men.cl.15.25 <- AD.MCAR.cov.40[,14]

vector.sd.MCAR.cov.40.AD.women.cl.25.40 <- AD.MCAR.cov.40[,15]
vector.sd.MCAR.cov.40.AD.men.cl.25.40 <- AD.MCAR.cov.40[,16]

vector.sd.MCAR.cov.40.AD.women.cl.40.50 <- AD.MCAR.cov.40[,17]
vector.sd.MCAR.cov.40.AD.men.cl.40.50 <- AD.MCAR.cov.40[,18]



## Summarised


# Mean

mean.MCAR.cov.40.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.40[,1])
mean.MCAR.cov.40.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.40[,2])

mean.MCAR.cov.40.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.40[,3])
mean.MCAR.cov.40.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.40[,4])

mean.MCAR.cov.40.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.40[,5])
mean.MCAR.cov.40.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.40[,6])

# Median

med.MCAR.cov.40.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.40[,7])
med.MCAR.cov.40.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.40[,8])

med.MCAR.cov.40.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.40[,9])
med.MCAR.cov.40.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.40[,10])

med.MCAR.cov.40.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.40[,11])
med.MCAR.cov.40.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.40[,12])

# Standard deviation

sd.MCAR.cov.40.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.40[,13])
sd.MCAR.cov.40.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.40[,14])

sd.MCAR.cov.40.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.40[,15])
sd.MCAR.cov.40.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.40[,16])

sd.MCAR.cov.40.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.40[,17])
sd.MCAR.cov.40.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.40[,18])



# Cov 45



# Vector


# Mean

vector.mean.MCAR.cov.45.AD.women.cl.15.25 <- AD.MCAR.cov.45[,1]
vector.mean.MCAR.cov.45.AD.men.cl.15.25 <- AD.MCAR.cov.45[,2]

vector.mean.MCAR.cov.45.AD.women.cl.25.40 <- AD.MCAR.cov.45[,3]
vector.mean.MCAR.cov.45.AD.men.cl.25.40 <- AD.MCAR.cov.45[,4]

vector.mean.MCAR.cov.45.AD.women.cl.40.50 <- AD.MCAR.cov.45[,5]
vector.mean.MCAR.cov.45.AD.men.cl.40.50 <- AD.MCAR.cov.45[,6]

# Median

vector.med.MCAR.cov.45.AD.women.cl.15.25 <- AD.MCAR.cov.45[,7]
vector.med.MCAR.cov.45.AD.men.cl.15.25 <- AD.MCAR.cov.45[,8]

vector.med.MCAR.cov.45.AD.women.cl.25.40 <- AD.MCAR.cov.45[,9]
vector.med.MCAR.cov.45.AD.men.cl.25.40 <- AD.MCAR.cov.45[,10]

vector.med.MCAR.cov.45.AD.women.cl.40.50 <- AD.MCAR.cov.45[,11]
vector.med.MCAR.cov.45.AD.men.cl.40.50 <- AD.MCAR.cov.45[,12]

# Standard deviation

vector.sd.MCAR.cov.45.AD.women.cl.15.25 <- AD.MCAR.cov.45[,13]
vector.sd.MCAR.cov.45.AD.men.cl.15.25 <- AD.MCAR.cov.45[,14]

vector.sd.MCAR.cov.45.AD.women.cl.25.40 <- AD.MCAR.cov.45[,15]
vector.sd.MCAR.cov.45.AD.men.cl.25.40 <- AD.MCAR.cov.45[,16]

vector.sd.MCAR.cov.45.AD.women.cl.40.50 <- AD.MCAR.cov.45[,17]
vector.sd.MCAR.cov.45.AD.men.cl.40.50 <- AD.MCAR.cov.45[,18]



## Summarised


# Mean

mean.MCAR.cov.45.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.45[,1])
mean.MCAR.cov.45.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.45[,2])

mean.MCAR.cov.45.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.45[,3])
mean.MCAR.cov.45.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.45[,4])

mean.MCAR.cov.45.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.45[,5])
mean.MCAR.cov.45.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.45[,6])

# Median

med.MCAR.cov.45.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.45[,7])
med.MCAR.cov.45.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.45[,8])

med.MCAR.cov.45.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.45[,9])
med.MCAR.cov.45.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.45[,10])

med.MCAR.cov.45.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.45[,11])
med.MCAR.cov.45.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.45[,12])

# Standard deviation

sd.MCAR.cov.45.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.45[,13])
sd.MCAR.cov.45.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.45[,14])

sd.MCAR.cov.45.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.45[,15])
sd.MCAR.cov.45.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.45[,16])

sd.MCAR.cov.45.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.45[,17])
sd.MCAR.cov.45.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.45[,18])


# Cov 50


## Vector


# Mean

vector.mean.MCAR.cov.50.AD.women.cl.15.25 <- AD.MCAR.cov.50[,1]
vector.mean.MCAR.cov.50.AD.men.cl.15.25 <- AD.MCAR.cov.50[,2]

vector.mean.MCAR.cov.50.AD.women.cl.25.40 <- AD.MCAR.cov.50[,3]
vector.mean.MCAR.cov.50.AD.men.cl.25.40 <- AD.MCAR.cov.50[,4]

vector.mean.MCAR.cov.50.AD.women.cl.40.50 <- AD.MCAR.cov.50[,5]
vector.mean.MCAR.cov.50.AD.men.cl.40.50 <- AD.MCAR.cov.50[,6]

# Median

vector.med.MCAR.cov.50.AD.women.cl.15.25 <- AD.MCAR.cov.50[,7]
vector.med.MCAR.cov.50.AD.men.cl.15.25 <- AD.MCAR.cov.50[,8]

vector.med.MCAR.cov.50.AD.women.cl.25.40 <- AD.MCAR.cov.50[,9]
vector.med.MCAR.cov.50.AD.men.cl.25.40 <- AD.MCAR.cov.50[,10]

vector.med.MCAR.cov.50.AD.women.cl.40.50 <- AD.MCAR.cov.50[,11]
vector.med.MCAR.cov.50.AD.men.cl.40.50 <- AD.MCAR.cov.50[,12]

# Standard deviation

vector.sd.MCAR.cov.50.AD.women.cl.15.25 <- AD.MCAR.cov.50[,13]
vector.sd.MCAR.cov.50.AD.men.cl.15.25 <- AD.MCAR.cov.50[,14]

vector.sd.MCAR.cov.50.AD.women.cl.25.40 <- AD.MCAR.cov.50[,15]
vector.sd.MCAR.cov.50.AD.men.cl.25.40 <- AD.MCAR.cov.50[,16]

vector.sd.MCAR.cov.50.AD.women.cl.40.50 <- AD.MCAR.cov.50[,17]
vector.sd.MCAR.cov.50.AD.men.cl.40.50 <- AD.MCAR.cov.50[,18]



## Summarised


# Mean

mean.MCAR.cov.50.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.50[,1])
mean.MCAR.cov.50.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.50[,2])

mean.MCAR.cov.50.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.50[,3])
mean.MCAR.cov.50.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.50[,4])

mean.MCAR.cov.50.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.50[,5])
mean.MCAR.cov.50.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.50[,6])

# Median

med.MCAR.cov.50.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.50[,7])
med.MCAR.cov.50.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.50[,8])

med.MCAR.cov.50.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.50[,9])
med.MCAR.cov.50.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.50[,10])

med.MCAR.cov.50.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.50[,11])
med.MCAR.cov.50.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.50[,12])

# Standard deviation

sd.MCAR.cov.50.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.50[,13])
sd.MCAR.cov.50.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.50[,14])

sd.MCAR.cov.50.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.50[,15])
sd.MCAR.cov.50.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.50[,16])

sd.MCAR.cov.50.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.50[,17])
sd.MCAR.cov.50.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.50[,18])



# Cov 55



## Vector


# Mean

vector.mean.MCAR.cov.55.AD.women.cl.15.25 <- AD.MCAR.cov.55[,1]
vector.mean.MCAR.cov.55.AD.men.cl.15.25 <- AD.MCAR.cov.55[,2]

vector.mean.MCAR.cov.55.AD.women.cl.25.40 <- AD.MCAR.cov.55[,3]
vector.mean.MCAR.cov.55.AD.men.cl.25.40 <- AD.MCAR.cov.55[,4]

vector.mean.MCAR.cov.55.AD.women.cl.40.50 <- AD.MCAR.cov.55[,5]
vector.mean.MCAR.cov.55.AD.men.cl.40.50 <- AD.MCAR.cov.55[,6]

# Median

vector.med.MCAR.cov.55.AD.women.cl.15.25 <- AD.MCAR.cov.55[,7]
vector.med.MCAR.cov.55.AD.men.cl.15.25 <- AD.MCAR.cov.55[,8]

vector.med.MCAR.cov.55.AD.women.cl.25.40 <- AD.MCAR.cov.55[,9]
vector.med.MCAR.cov.55.AD.men.cl.25.40 <- AD.MCAR.cov.55[,10]

vector.med.MCAR.cov.55.AD.women.cl.40.50 <- AD.MCAR.cov.55[,11]
vector.med.MCAR.cov.55.AD.men.cl.40.50 <- AD.MCAR.cov.55[,12]

# Standard deviation

vector.sd.MCAR.cov.55.AD.women.cl.15.25 <- AD.MCAR.cov.55[,13]
vector.sd.MCAR.cov.55.AD.men.cl.15.25 <- AD.MCAR.cov.55[,14]

vector.sd.MCAR.cov.55.AD.women.cl.25.40 <- AD.MCAR.cov.55[,15]
vector.sd.MCAR.cov.55.AD.men.cl.25.40 <- AD.MCAR.cov.55[,16]

vector.sd.MCAR.cov.55.AD.women.cl.40.50 <- AD.MCAR.cov.55[,17]
vector.sd.MCAR.cov.55.AD.men.cl.40.50 <- AD.MCAR.cov.55[,18]



## Summarised


# Mean

mean.MCAR.cov.55.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.55[,1])
mean.MCAR.cov.55.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.55[,2])

mean.MCAR.cov.55.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.55[,3])
mean.MCAR.cov.55.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.55[,4])

mean.MCAR.cov.55.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.55[,5])
mean.MCAR.cov.55.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.55[,6])

# Median

med.MCAR.cov.55.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.55[,7])
med.MCAR.cov.55.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.55[,8])

med.MCAR.cov.55.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.55[,9])
med.MCAR.cov.55.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.55[,10])

med.MCAR.cov.55.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.55[,11])
med.MCAR.cov.55.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.55[,12])

# Standard deviation

sd.MCAR.cov.55.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.55[,13])
sd.MCAR.cov.55.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.55[,14])

sd.MCAR.cov.55.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.55[,15])
sd.MCAR.cov.55.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.55[,16])

sd.MCAR.cov.55.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.55[,17])
sd.MCAR.cov.55.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.55[,18])



# Cov 60


## Vector

# Mean

vector.mean.MCAR.cov.60.AD.women.cl.15.25 <- AD.MCAR.cov.60[,1]
vector.mean.MCAR.cov.60.AD.men.cl.15.25 <- AD.MCAR.cov.60[,2]

vector.mean.MCAR.cov.60.AD.women.cl.25.40 <- AD.MCAR.cov.60[,3]
vector.mean.MCAR.cov.60.AD.men.cl.25.40 <- AD.MCAR.cov.60[,4]

vector.mean.MCAR.cov.60.AD.women.cl.40.50 <- AD.MCAR.cov.60[,5]
vector.mean.MCAR.cov.60.AD.men.cl.40.50 <- AD.MCAR.cov.60[,6]

# Median

vector.med.MCAR.cov.60.AD.women.cl.15.25 <- AD.MCAR.cov.60[,7]
vector.med.MCAR.cov.60.AD.men.cl.15.25 <- AD.MCAR.cov.60[,8]

vector.med.MCAR.cov.60.AD.women.cl.25.40 <- AD.MCAR.cov.60[,9]
vector.med.MCAR.cov.60.AD.men.cl.25.40 <- AD.MCAR.cov.60[,10]

vector.med.MCAR.cov.60.AD.women.cl.40.50 <- AD.MCAR.cov.60[,11]
vector.med.MCAR.cov.60.AD.men.cl.40.50 <- AD.MCAR.cov.60[,12]

# Standard deviation

vector.sd.MCAR.cov.60.AD.women.cl.15.25 <- AD.MCAR.cov.60[,13]
vector.sd.MCAR.cov.60.AD.men.cl.15.25 <- AD.MCAR.cov.60[,14]

vector.sd.MCAR.cov.60.AD.women.cl.25.40 <- AD.MCAR.cov.60[,15]
vector.sd.MCAR.cov.60.AD.men.cl.25.40 <- AD.MCAR.cov.60[,16]

vector.sd.MCAR.cov.60.AD.women.cl.40.50 <- AD.MCAR.cov.60[,17]
vector.sd.MCAR.cov.60.AD.men.cl.40.50 <- AD.MCAR.cov.60[,18]



## Summarised


# Mean

mean.MCAR.cov.60.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.60[,1])
mean.MCAR.cov.60.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.60[,2])

mean.MCAR.cov.60.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.60[,3])
mean.MCAR.cov.60.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.60[,4])

mean.MCAR.cov.60.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.60[,5])
mean.MCAR.cov.60.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.60[,6])

# Median

med.MCAR.cov.60.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.60[,7])
med.MCAR.cov.60.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.60[,8])

med.MCAR.cov.60.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.60[,9])
med.MCAR.cov.60.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.60[,10])

med.MCAR.cov.60.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.60[,11])
med.MCAR.cov.60.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.60[,12])

# Standard deviation

sd.MCAR.cov.60.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.60[,13])
sd.MCAR.cov.60.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.60[,14])

sd.MCAR.cov.60.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.60[,15])
sd.MCAR.cov.60.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.60[,16])

sd.MCAR.cov.60.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.60[,17])
sd.MCAR.cov.60.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.60[,18])



# Cov 65



## Vector

# Mean

vector.mean.MCAR.cov.65.AD.women.cl.15.25 <- AD.MCAR.cov.65[,1]
vector.mean.MCAR.cov.65.AD.men.cl.15.25 <- AD.MCAR.cov.65[,2]

vector.mean.MCAR.cov.65.AD.women.cl.25.40 <- AD.MCAR.cov.65[,3]
vector.mean.MCAR.cov.65.AD.men.cl.25.40 <- AD.MCAR.cov.65[,4]

vector.mean.MCAR.cov.65.AD.women.cl.40.50 <- AD.MCAR.cov.65[,5]
vector.mean.MCAR.cov.65.AD.men.cl.40.50 <- AD.MCAR.cov.65[,6]

# Median

vector.med.MCAR.cov.65.AD.women.cl.15.25 <- AD.MCAR.cov.65[,7]
vector.med.MCAR.cov.65.AD.men.cl.15.25 <- AD.MCAR.cov.65[,8]

vector.med.MCAR.cov.65.AD.women.cl.25.40 <- AD.MCAR.cov.65[,9]
vector.med.MCAR.cov.65.AD.men.cl.25.40 <- AD.MCAR.cov.65[,10]

vector.med.MCAR.cov.65.AD.women.cl.40.50 <- AD.MCAR.cov.65[,11]
vector.med.MCAR.cov.65.AD.men.cl.40.50 <- AD.MCAR.cov.65[,12]

# Standard deviation

vector.sd.MCAR.cov.65.AD.women.cl.15.25 <- AD.MCAR.cov.65[,13]
vector.sd.MCAR.cov.65.AD.men.cl.15.25 <- AD.MCAR.cov.65[,14]

vector.sd.MCAR.cov.65.AD.women.cl.25.40 <- AD.MCAR.cov.65[,15]
vector.sd.MCAR.cov.65.AD.men.cl.25.40 <- AD.MCAR.cov.65[,16]

vector.sd.MCAR.cov.65.AD.women.cl.40.50 <- AD.MCAR.cov.65[,17]
vector.sd.MCAR.cov.65.AD.men.cl.40.50 <- AD.MCAR.cov.65[,18]



## Summarised


# Mean

mean.MCAR.cov.65.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.65[,1])
mean.MCAR.cov.65.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.65[,2])

mean.MCAR.cov.65.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.65[,3])
mean.MCAR.cov.65.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.65[,4])

mean.MCAR.cov.65.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.65[,5])
mean.MCAR.cov.65.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.65[,6])

# Median

med.MCAR.cov.65.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.65[,7])
med.MCAR.cov.65.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.65[,8])

med.MCAR.cov.65.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.65[,9])
med.MCAR.cov.65.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.65[,10])

med.MCAR.cov.65.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.65[,11])
med.MCAR.cov.65.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.65[,12])

# Standard deviation

sd.MCAR.cov.65.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.65[,13])
sd.MCAR.cov.65.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.65[,14])

sd.MCAR.cov.65.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.65[,15])
sd.MCAR.cov.65.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.65[,16])

sd.MCAR.cov.65.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.65[,17])
sd.MCAR.cov.65.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.65[,18])



# Cov 70



## Vector


# Mean

vector.mean.MCAR.cov.70.AD.women.cl.15.25 <- AD.MCAR.cov.70[,1]
vector.mean.MCAR.cov.70.AD.men.cl.15.25 <- AD.MCAR.cov.70[,2]

vector.mean.MCAR.cov.70.AD.women.cl.25.40 <- AD.MCAR.cov.70[,3]
vector.mean.MCAR.cov.70.AD.men.cl.25.40 <- AD.MCAR.cov.70[,4]

vector.mean.MCAR.cov.70.AD.women.cl.40.50 <- AD.MCAR.cov.70[,5]
vector.mean.MCAR.cov.70.AD.men.cl.40.50 <- AD.MCAR.cov.70[,6]

# Median

vector.med.MCAR.cov.70.AD.women.cl.15.25 <- AD.MCAR.cov.70[,7]
vector.med.MCAR.cov.70.AD.men.cl.15.25 <- AD.MCAR.cov.70[,8]

vector.med.MCAR.cov.70.AD.women.cl.25.40 <- AD.MCAR.cov.70[,9]
vector.med.MCAR.cov.70.AD.men.cl.25.40 <- AD.MCAR.cov.70[,10]

vector.med.MCAR.cov.70.AD.women.cl.40.50 <- AD.MCAR.cov.70[,11]
vector.med.MCAR.cov.70.AD.men.cl.40.50 <- AD.MCAR.cov.70[,12]

# Standard deviation

vector.sd.MCAR.cov.70.AD.women.cl.15.25 <- AD.MCAR.cov.70[,13]
vector.sd.MCAR.cov.70.AD.men.cl.15.25 <- AD.MCAR.cov.70[,14]

vector.sd.MCAR.cov.70.AD.women.cl.25.40 <- AD.MCAR.cov.70[,15]
vector.sd.MCAR.cov.70.AD.men.cl.25.40 <- AD.MCAR.cov.70[,16]

vector.sd.MCAR.cov.70.AD.women.cl.40.50 <- AD.MCAR.cov.70[,17]
vector.sd.MCAR.cov.70.AD.men.cl.40.50 <- AD.MCAR.cov.70[,18]




## Summarised


# Mean

mean.MCAR.cov.70.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.70[,1])
mean.MCAR.cov.70.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.70[,2])

mean.MCAR.cov.70.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.70[,3])
mean.MCAR.cov.70.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.70[,4])

mean.MCAR.cov.70.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.70[,5])
mean.MCAR.cov.70.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.70[,6])

# Median

med.MCAR.cov.70.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.70[,7])
med.MCAR.cov.70.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.70[,8])

med.MCAR.cov.70.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.70[,9])
med.MCAR.cov.70.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.70[,10])

med.MCAR.cov.70.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.70[,11])
med.MCAR.cov.70.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.70[,12])

# Standard deviation

sd.MCAR.cov.70.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.70[,13])
sd.MCAR.cov.70.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.70[,14])

sd.MCAR.cov.70.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.70[,15])
sd.MCAR.cov.70.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.70[,16])

sd.MCAR.cov.70.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.70[,17])
sd.MCAR.cov.70.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.70[,18])



# Cov 75


## Vector


# Mean

vector.mean.MCAR.cov.75.AD.women.cl.15.25 <- AD.MCAR.cov.75[,1]
vector.mean.MCAR.cov.75.AD.men.cl.15.25 <- AD.MCAR.cov.75[,2]

vector.mean.MCAR.cov.75.AD.women.cl.25.40 <- AD.MCAR.cov.75[,3]
vector.mean.MCAR.cov.75.AD.men.cl.25.40 <- AD.MCAR.cov.75[,4]

vector.mean.MCAR.cov.75.AD.women.cl.40.50 <- AD.MCAR.cov.75[,5]
vector.mean.MCAR.cov.75.AD.men.cl.40.50 <- AD.MCAR.cov.75[,6]

# Median

vector.med.MCAR.cov.75.AD.women.cl.15.25 <- AD.MCAR.cov.75[,7]
vector.med.MCAR.cov.75.AD.men.cl.15.25 <- AD.MCAR.cov.75[,8]

vector.med.MCAR.cov.75.AD.women.cl.25.40 <- AD.MCAR.cov.75[,9]
vector.med.MCAR.cov.75.AD.men.cl.25.40 <- AD.MCAR.cov.75[,10]

vector.med.MCAR.cov.75.AD.women.cl.40.50 <- AD.MCAR.cov.75[,11]
vector.med.MCAR.cov.75.AD.men.cl.40.50 <- AD.MCAR.cov.75[,12]

# Standard deviation

vector.sd.MCAR.cov.75.AD.women.cl.15.25 <- AD.MCAR.cov.75[,13]
vector.sd.MCAR.cov.75.AD.men.cl.15.25 <- AD.MCAR.cov.75[,14]

vector.sd.MCAR.cov.75.AD.women.cl.25.40 <- AD.MCAR.cov.75[,15]
vector.sd.MCAR.cov.75.AD.men.cl.25.40 <- AD.MCAR.cov.75[,16]

vector.sd.MCAR.cov.75.AD.women.cl.40.50 <- AD.MCAR.cov.75[,17]
vector.sd.MCAR.cov.75.AD.men.cl.40.50 <- AD.MCAR.cov.75[,18]


## Summarised


# Mean

mean.MCAR.cov.75.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.75[,1])
mean.MCAR.cov.75.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.75[,2])

mean.MCAR.cov.75.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.75[,3])
mean.MCAR.cov.75.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.75[,4])

mean.MCAR.cov.75.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.75[,5])
mean.MCAR.cov.75.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.75[,6])

# Median

med.MCAR.cov.75.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.75[,7])
med.MCAR.cov.75.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.75[,8])

med.MCAR.cov.75.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.75[,9])
med.MCAR.cov.75.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.75[,10])

med.MCAR.cov.75.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.75[,11])
med.MCAR.cov.75.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.75[,12])

# Standard deviation

sd.MCAR.cov.75.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.75[,13])
sd.MCAR.cov.75.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.75[,14])

sd.MCAR.cov.75.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.75[,15])
sd.MCAR.cov.75.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.75[,16])

sd.MCAR.cov.75.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.75[,17])
sd.MCAR.cov.75.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.75[,18])



# Cov 80


## Vector

# Mean

vector.mean.MCAR.cov.80.AD.women.cl.15.25 <- AD.MCAR.cov.80[,1]
vector.mean.MCAR.cov.80.AD.men.cl.15.25 <- AD.MCAR.cov.80[,2]

vector.mean.MCAR.cov.80.AD.women.cl.25.40 <- AD.MCAR.cov.80[,3]
vector.mean.MCAR.cov.80.AD.men.cl.25.40 <- AD.MCAR.cov.80[,4]

vector.mean.MCAR.cov.80.AD.women.cl.40.50 <- AD.MCAR.cov.80[,5]
vector.mean.MCAR.cov.80.AD.men.cl.40.50 <- AD.MCAR.cov.80[,6]

# Median

vector.med.MCAR.cov.80.AD.women.cl.15.25 <- AD.MCAR.cov.80[,7]
vector.med.MCAR.cov.80.AD.men.cl.15.25 <- AD.MCAR.cov.80[,8]

vector.med.MCAR.cov.80.AD.women.cl.25.40 <- AD.MCAR.cov.80[,9]
vector.med.MCAR.cov.80.AD.men.cl.25.40 <- AD.MCAR.cov.80[,10]

vector.med.MCAR.cov.80.AD.women.cl.40.50 <- AD.MCAR.cov.80[,11]
vector.med.MCAR.cov.80.AD.men.cl.40.50 <- AD.MCAR.cov.80[,12]

# Standard deviation

vector.sd.MCAR.cov.80.AD.women.cl.15.25 <- AD.MCAR.cov.80[,13]
vector.sd.MCAR.cov.80.AD.men.cl.15.25 <- AD.MCAR.cov.80[,14]

vector.sd.MCAR.cov.80.AD.women.cl.25.40 <- AD.MCAR.cov.80[,15]
vector.sd.MCAR.cov.80.AD.men.cl.25.40 <- AD.MCAR.cov.80[,16]

vector.sd.MCAR.cov.80.AD.women.cl.40.50 <- AD.MCAR.cov.80[,17]
vector.sd.MCAR.cov.80.AD.men.cl.40.50 <- AD.MCAR.cov.80[,18]


## Summarised


# Mean

mean.MCAR.cov.80.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.80[,1])
mean.MCAR.cov.80.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.80[,2])

mean.MCAR.cov.80.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.80[,3])
mean.MCAR.cov.80.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.80[,4])

mean.MCAR.cov.80.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.80[,5])
mean.MCAR.cov.80.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.80[,6])

# Median

med.MCAR.cov.80.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.80[,7])
med.MCAR.cov.80.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.80[,8])

med.MCAR.cov.80.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.80[,9])
med.MCAR.cov.80.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.80[,10])

med.MCAR.cov.80.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.80[,11])
med.MCAR.cov.80.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.80[,12])

# Standard deviation

sd.MCAR.cov.80.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.80[,13])
sd.MCAR.cov.80.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.80[,14])

sd.MCAR.cov.80.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.80[,15])
sd.MCAR.cov.80.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.80[,16])

sd.MCAR.cov.80.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.80[,17])
sd.MCAR.cov.80.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.80[,18])



# Cov 85


## Vector

# Mean

vector.mean.MCAR.cov.85.AD.women.cl.15.25 <- AD.MCAR.cov.85[,1]
vector.mean.MCAR.cov.85.AD.men.cl.15.25 <- AD.MCAR.cov.85[,2]

vector.mean.MCAR.cov.85.AD.women.cl.25.40 <- AD.MCAR.cov.85[,3]
vector.mean.MCAR.cov.85.AD.men.cl.25.40 <- AD.MCAR.cov.85[,4]

vector.mean.MCAR.cov.85.AD.women.cl.40.50 <- AD.MCAR.cov.85[,5]
vector.mean.MCAR.cov.85.AD.men.cl.40.50 <- AD.MCAR.cov.85[,6]

# Median

vector.med.MCAR.cov.85.AD.women.cl.15.25 <- AD.MCAR.cov.85[,7]
vector.med.MCAR.cov.85.AD.men.cl.15.25 <- AD.MCAR.cov.85[,8]

vector.med.MCAR.cov.85.AD.women.cl.25.40 <- AD.MCAR.cov.85[,9]
vector.med.MCAR.cov.85.AD.men.cl.25.40 <- AD.MCAR.cov.85[,10]

vector.med.MCAR.cov.85.AD.women.cl.40.50 <- AD.MCAR.cov.85[,11]
vector.med.MCAR.cov.85.AD.men.cl.40.50 <- AD.MCAR.cov.85[,12]

# Standard deviation

vector.sd.MCAR.cov.85.AD.women.cl.15.25 <- AD.MCAR.cov.85[,13]
vector.sd.MCAR.cov.85.AD.men.cl.15.25 <- AD.MCAR.cov.85[,14]

vector.sd.MCAR.cov.85.AD.women.cl.25.40 <- AD.MCAR.cov.85[,15]
vector.sd.MCAR.cov.85.AD.men.cl.25.40 <- AD.MCAR.cov.85[,16]

vector.sd.MCAR.cov.85.AD.women.cl.40.50 <- AD.MCAR.cov.85[,17]
vector.sd.MCAR.cov.85.AD.men.cl.40.50 <- AD.MCAR.cov.85[,18]


## Summarised


# Mean

mean.MCAR.cov.85.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.85[,1])
mean.MCAR.cov.85.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.85[,2])

mean.MCAR.cov.85.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.85[,3])
mean.MCAR.cov.85.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.85[,4])

mean.MCAR.cov.85.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.85[,5])
mean.MCAR.cov.85.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.85[,6])

# Median

med.MCAR.cov.85.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.85[,7])
med.MCAR.cov.85.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.85[,8])

med.MCAR.cov.85.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.85[,9])
med.MCAR.cov.85.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.85[,10])

med.MCAR.cov.85.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.85[,11])
med.MCAR.cov.85.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.85[,12])

# Standard deviation

sd.MCAR.cov.85.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.85[,13])
sd.MCAR.cov.85.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.85[,14])

sd.MCAR.cov.85.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.85[,15])
sd.MCAR.cov.85.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.85[,16])

sd.MCAR.cov.85.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.85[,17])
sd.MCAR.cov.85.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.85[,18])



# Cov 90


## Vector

# Mean

vector.mean.MCAR.cov.90.AD.women.cl.15.25 <- AD.MCAR.cov.90[,1]
vector.mean.MCAR.cov.90.AD.men.cl.15.25 <- AD.MCAR.cov.90[,2]

vector.mean.MCAR.cov.90.AD.women.cl.25.40 <- AD.MCAR.cov.90[,3]
vector.mean.MCAR.cov.90.AD.men.cl.25.40 <- AD.MCAR.cov.90[,4]

vector.mean.MCAR.cov.90.AD.women.cl.40.50 <- AD.MCAR.cov.90[,5]
vector.mean.MCAR.cov.90.AD.men.cl.40.50 <- AD.MCAR.cov.90[,6]

# Median

vector.med.MCAR.cov.90.AD.women.cl.15.25 <- AD.MCAR.cov.90[,7]
vector.med.MCAR.cov.90.AD.men.cl.15.25 <- AD.MCAR.cov.90[,8]

vector.med.MCAR.cov.90.AD.women.cl.25.40 <- AD.MCAR.cov.90[,9]
vector.med.MCAR.cov.90.AD.men.cl.25.40 <- AD.MCAR.cov.90[,10]

vector.med.MCAR.cov.90.AD.women.cl.40.50 <- AD.MCAR.cov.90[,11]
vector.med.MCAR.cov.90.AD.men.cl.40.50 <- AD.MCAR.cov.90[,12]

# Standard deviation

vector.sd.MCAR.cov.90.AD.women.cl.15.25 <- AD.MCAR.cov.90[,13]
vector.sd.MCAR.cov.90.AD.men.cl.15.25 <- AD.MCAR.cov.90[,14]

vector.sd.MCAR.cov.90.AD.women.cl.25.40 <- AD.MCAR.cov.90[,15]
vector.sd.MCAR.cov.90.AD.men.cl.25.40 <- AD.MCAR.cov.90[,16]

vector.sd.MCAR.cov.90.AD.women.cl.40.50 <- AD.MCAR.cov.90[,17]
vector.sd.MCAR.cov.90.AD.men.cl.40.50 <- AD.MCAR.cov.90[,18]



## Summarised


# Mean

mean.MCAR.cov.90.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.90[,1])
mean.MCAR.cov.90.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.90[,2])

mean.MCAR.cov.90.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.90[,3])
mean.MCAR.cov.90.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.90[,4])

mean.MCAR.cov.90.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.90[,5])
mean.MCAR.cov.90.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.90[,6])

# Median

med.MCAR.cov.90.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.90[,7])
med.MCAR.cov.90.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.90[,8])

med.MCAR.cov.90.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.90[,9])
med.MCAR.cov.90.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.90[,10])

med.MCAR.cov.90.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.90[,11])
med.MCAR.cov.90.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.90[,12])

# Standard deviation

sd.MCAR.cov.90.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.90[,13])
sd.MCAR.cov.90.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.90[,14])

sd.MCAR.cov.90.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.90[,15])
sd.MCAR.cov.90.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.90[,16])

sd.MCAR.cov.90.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.90[,17])
sd.MCAR.cov.90.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.90[,18])




# Cov 95


## Vector

# Mean

vector.mean.MCAR.cov.95.AD.women.cl.15.25 <- AD.MCAR.cov.95[,1]
vector.mean.MCAR.cov.95.AD.men.cl.15.25 <- AD.MCAR.cov.95[,2]

vector.mean.MCAR.cov.95.AD.women.cl.25.40 <- AD.MCAR.cov.95[,3]
vector.mean.MCAR.cov.95.AD.men.cl.25.40 <- AD.MCAR.cov.95[,4]

vector.mean.MCAR.cov.95.AD.women.cl.40.50 <- AD.MCAR.cov.95[,5]
vector.mean.MCAR.cov.95.AD.men.cl.40.50 <- AD.MCAR.cov.95[,6]

# Median

vector.med.MCAR.cov.95.AD.women.cl.15.25 <- AD.MCAR.cov.95[,7]
vector.med.MCAR.cov.95.AD.men.cl.15.25 <- AD.MCAR.cov.95[,8]

vector.med.MCAR.cov.95.AD.women.cl.25.40 <- AD.MCAR.cov.95[,9]
vector.med.MCAR.cov.95.AD.men.cl.25.40 <- AD.MCAR.cov.95[,10]

vector.med.MCAR.cov.95.AD.women.cl.40.50 <- AD.MCAR.cov.95[,11]
vector.med.MCAR.cov.95.AD.men.cl.40.50 <- AD.MCAR.cov.95[,12]

# Standard deviation

vector.sd.MCAR.cov.95.AD.women.cl.15.25 <- AD.MCAR.cov.95[,13]
vector.sd.MCAR.cov.95.AD.men.cl.15.25 <- AD.MCAR.cov.95[,14]

vector.sd.MCAR.cov.95.AD.women.cl.25.40 <- AD.MCAR.cov.95[,15]
vector.sd.MCAR.cov.95.AD.men.cl.25.40 <- AD.MCAR.cov.95[,16]

vector.sd.MCAR.cov.95.AD.women.cl.40.50 <- AD.MCAR.cov.95[,17]
vector.sd.MCAR.cov.95.AD.men.cl.40.50 <- AD.MCAR.cov.95[,18]


## Summarised

# Mean

mean.MCAR.cov.95.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.95[,1])
mean.MCAR.cov.95.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.95[,2])

mean.MCAR.cov.95.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.95[,3])
mean.MCAR.cov.95.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.95[,4])

mean.MCAR.cov.95.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.95[,5])
mean.MCAR.cov.95.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.95[,6])

# Median

med.MCAR.cov.95.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.95[,7])
med.MCAR.cov.95.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.95[,8])

med.MCAR.cov.95.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.95[,9])
med.MCAR.cov.95.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.95[,10])

med.MCAR.cov.95.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.95[,11])
med.MCAR.cov.95.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.95[,12])

# Standard deviation

sd.MCAR.cov.95.AD.women.cl.15.25 <- quant.med(AD.MCAR.cov.95[,13])
sd.MCAR.cov.95.AD.men.cl.15.25 <- quant.med(AD.MCAR.cov.95[,14])

sd.MCAR.cov.95.AD.women.cl.25.40 <- quant.med(AD.MCAR.cov.95[,15])
sd.MCAR.cov.95.AD.men.cl.25.40 <- quant.med(AD.MCAR.cov.95[,16])

sd.MCAR.cov.95.AD.women.cl.40.50 <- quant.med(AD.MCAR.cov.95[,17])
sd.MCAR.cov.95.AD.men.cl.40.50 <- quant.med(AD.MCAR.cov.95[,18])



# Visualization of AD statistics inferred from transmission cluste --------




# Visualise age difference statistics -------------------------------------


# Mean

mean.AD.num.women.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,1])
mean.AD.num.men.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,2])

mean.AD.num.women.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,3])
mean.AD.num.men.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,4])

mean.AD.num.women.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,5])
mean.AD.num.men.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,6])

# Median

med.AD.num.women.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,7])
med.AD.num.men.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,8])

med.AD.num.women.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,9])
med.AD.num.men.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,10])

med.AD.num.women.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,11])
med.AD.num.men.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,12])

# Standard deviation

sd.AD.num.women.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,13])
sd.AD.num.men.true.cov.100.15.25 <- quant.med(AD.true.cov.100[,14])

sd.AD.num.women.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,15])
sd.AD.num.men.true.cov.100.25.40 <- quant.med(AD.true.cov.100[,16])

sd.AD.num.women.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,17])
sd.AD.num.men.true.cov.100.40.50 <- quant.med(AD.true.cov.100[,18])



## Mean



# Women 15.25

mean.MCAR.women.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(mean.MCAR.cov.35.AD.women.cl.15.25[2], mean.MCAR.cov.40.AD.women.cl.15.25[2],
                                             mean.MCAR.cov.45.AD.women.cl.15.25[2], mean.MCAR.cov.50.AD.women.cl.15.25[2],
                                             mean.MCAR.cov.55.AD.women.cl.15.25[2], mean.MCAR.cov.60.AD.women.cl.15.25[2],
                                             mean.MCAR.cov.65.AD.women.cl.15.25[2], mean.MCAR.cov.70.AD.women.cl.15.25[2],
                                             mean.MCAR.cov.75.AD.women.cl.15.25[2], mean.MCAR.cov.80.AD.women.cl.15.25[2],
                                             mean.MCAR.cov.85.AD.women.cl.15.25[2], mean.MCAR.cov.90.AD.women.cl.15.25[2],
                                             mean.MCAR.cov.95.AD.women.cl.15.25[2], mean.AD.num.women.true.cov.100.15.25[2]),
                                       
                                       L = c(mean.MCAR.cov.35.AD.women.cl.15.25[1], mean.MCAR.cov.40.AD.women.cl.15.25[1],
                                             mean.MCAR.cov.45.AD.women.cl.15.25[1], mean.MCAR.cov.50.AD.women.cl.15.25[1],
                                             mean.MCAR.cov.55.AD.women.cl.15.25[1], mean.MCAR.cov.60.AD.women.cl.15.25[1],
                                             mean.MCAR.cov.65.AD.women.cl.15.25[1], mean.MCAR.cov.70.AD.women.cl.15.25[1],
                                             mean.MCAR.cov.75.AD.women.cl.15.25[1], mean.MCAR.cov.80.AD.women.cl.15.25[1],
                                             mean.MCAR.cov.85.AD.women.cl.15.25[1], mean.MCAR.cov.90.AD.women.cl.15.25[1],
                                             mean.MCAR.cov.95.AD.women.cl.15.25[1], mean.AD.num.women.true.cov.100.15.25[1]),
                                       
                                       U = c(mean.MCAR.cov.35.AD.women.cl.15.25[3], mean.MCAR.cov.40.AD.women.cl.15.25[3],
                                             mean.MCAR.cov.45.AD.women.cl.15.25[3], mean.MCAR.cov.50.AD.women.cl.15.25[3],
                                             mean.MCAR.cov.55.AD.women.cl.15.25[3], mean.MCAR.cov.60.AD.women.cl.15.25[3],
                                             mean.MCAR.cov.65.AD.women.cl.15.25[3], mean.MCAR.cov.70.AD.women.cl.15.25[3],
                                             mean.MCAR.cov.75.AD.women.cl.15.25[3], mean.MCAR.cov.80.AD.women.cl.15.25[3],
                                             mean.MCAR.cov.85.AD.women.cl.15.25[3], mean.MCAR.cov.90.AD.women.cl.15.25[3],
                                             mean.MCAR.cov.95.AD.women.cl.15.25[3], mean.AD.num.women.true.cov.100.15.25[3]))


plot.mean.MCAR.women.cl.15.25 <- ggplot(mean.MCAR.women.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.women.cl.15.25.png",
       plot = plot.mean.MCAR.women.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

mean.MCAR.men.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                     
                                     F = c(mean.MCAR.cov.35.AD.men.cl.15.25[2], mean.MCAR.cov.40.AD.men.cl.15.25[2],
                                           mean.MCAR.cov.45.AD.men.cl.15.25[2], mean.MCAR.cov.50.AD.men.cl.15.25[2],
                                           mean.MCAR.cov.55.AD.men.cl.15.25[2], mean.MCAR.cov.60.AD.men.cl.15.25[2],
                                           mean.MCAR.cov.65.AD.men.cl.15.25[2], mean.MCAR.cov.70.AD.men.cl.15.25[2],
                                           mean.MCAR.cov.75.AD.men.cl.15.25[2], mean.MCAR.cov.80.AD.men.cl.15.25[2],
                                           mean.MCAR.cov.85.AD.men.cl.15.25[2], mean.MCAR.cov.90.AD.men.cl.15.25[2],
                                           mean.MCAR.cov.95.AD.men.cl.15.25[2], mean.AD.num.men.true.cov.100.15.25[2]),
                                     
                                     L = c(mean.MCAR.cov.35.AD.men.cl.15.25[1], mean.MCAR.cov.40.AD.men.cl.15.25[1],
                                           mean.MCAR.cov.45.AD.men.cl.15.25[1], mean.MCAR.cov.50.AD.men.cl.15.25[1],
                                           mean.MCAR.cov.55.AD.men.cl.15.25[1], mean.MCAR.cov.60.AD.men.cl.15.25[1],
                                           mean.MCAR.cov.65.AD.men.cl.15.25[1], mean.MCAR.cov.70.AD.men.cl.15.25[1],
                                           mean.MCAR.cov.75.AD.men.cl.15.25[1], mean.MCAR.cov.80.AD.men.cl.15.25[1],
                                           mean.MCAR.cov.85.AD.men.cl.15.25[1], mean.MCAR.cov.90.AD.men.cl.15.25[1],
                                           mean.MCAR.cov.95.AD.men.cl.15.25[1], mean.AD.num.men.true.cov.100.15.25[1]),
                                     
                                     U = c(mean.MCAR.cov.35.AD.men.cl.15.25[3], mean.MCAR.cov.40.AD.men.cl.15.25[3],
                                           mean.MCAR.cov.45.AD.men.cl.15.25[3], mean.MCAR.cov.50.AD.men.cl.15.25[3],
                                           mean.MCAR.cov.55.AD.men.cl.15.25[3], mean.MCAR.cov.60.AD.men.cl.15.25[3],
                                           mean.MCAR.cov.65.AD.men.cl.15.25[3], mean.MCAR.cov.70.AD.men.cl.15.25[3],
                                           mean.MCAR.cov.75.AD.men.cl.15.25[3], mean.MCAR.cov.80.AD.men.cl.15.25[3],
                                           mean.MCAR.cov.85.AD.men.cl.15.25[3], mean.MCAR.cov.90.AD.men.cl.15.25[3],
                                           mean.MCAR.cov.95.AD.men.cl.15.25[3], mean.AD.num.men.true.cov.100.15.25[3]))


plot.mean.MCAR.men.cl.15.25 <- ggplot(mean.MCAR.men.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.men.cl.15.25.png",
       plot = plot.mean.MCAR.men.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

mean.MCAR.women.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(mean.MCAR.cov.35.AD.women.cl.25.40[2], mean.MCAR.cov.40.AD.women.cl.25.40[2],
                                             mean.MCAR.cov.45.AD.women.cl.25.40[2], mean.MCAR.cov.50.AD.women.cl.25.40[2],
                                             mean.MCAR.cov.55.AD.women.cl.25.40[2], mean.MCAR.cov.60.AD.women.cl.25.40[2],
                                             mean.MCAR.cov.65.AD.women.cl.25.40[2], mean.MCAR.cov.70.AD.women.cl.25.40[2],
                                             mean.MCAR.cov.75.AD.women.cl.25.40[2], mean.MCAR.cov.80.AD.women.cl.25.40[2],
                                             mean.MCAR.cov.85.AD.women.cl.25.40[2], mean.MCAR.cov.90.AD.women.cl.25.40[2],
                                             mean.MCAR.cov.95.AD.women.cl.25.40[2], mean.AD.num.women.true.cov.100.25.40[2]),
                                       
                                       L = c(mean.MCAR.cov.35.AD.women.cl.25.40[1], mean.MCAR.cov.40.AD.women.cl.25.40[1],
                                             mean.MCAR.cov.45.AD.women.cl.25.40[1], mean.MCAR.cov.50.AD.women.cl.25.40[1],
                                             mean.MCAR.cov.55.AD.women.cl.25.40[1], mean.MCAR.cov.60.AD.women.cl.25.40[1],
                                             mean.MCAR.cov.65.AD.women.cl.25.40[1], mean.MCAR.cov.70.AD.women.cl.25.40[1],
                                             mean.MCAR.cov.75.AD.women.cl.25.40[1], mean.MCAR.cov.80.AD.women.cl.25.40[1],
                                             mean.MCAR.cov.85.AD.women.cl.25.40[1], mean.MCAR.cov.90.AD.women.cl.25.40[1],
                                             mean.MCAR.cov.95.AD.women.cl.25.40[1], mean.AD.num.women.true.cov.100.25.40[1]),
                                       
                                       U = c(mean.MCAR.cov.35.AD.women.cl.25.40[3], mean.MCAR.cov.40.AD.women.cl.25.40[3],
                                             mean.MCAR.cov.45.AD.women.cl.25.40[3], mean.MCAR.cov.50.AD.women.cl.25.40[3],
                                             mean.MCAR.cov.55.AD.women.cl.25.40[3], mean.MCAR.cov.60.AD.women.cl.25.40[3],
                                             mean.MCAR.cov.65.AD.women.cl.25.40[3], mean.MCAR.cov.70.AD.women.cl.25.40[3],
                                             mean.MCAR.cov.75.AD.women.cl.25.40[3], mean.MCAR.cov.80.AD.women.cl.25.40[3],
                                             mean.MCAR.cov.85.AD.women.cl.25.40[3], mean.MCAR.cov.90.AD.women.cl.25.40[3],
                                             mean.MCAR.cov.95.AD.women.cl.25.40[3], mean.AD.num.women.true.cov.100.25.40[3]))


plot.mean.MCAR.women.cl.25.40 <- ggplot(mean.MCAR.women.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.women.cl.25.40.png",
       plot = plot.mean.MCAR.women.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Men: 25.40

mean.MCAR.men.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                     
                                     F = c(mean.MCAR.cov.35.AD.men.cl.25.40[2], mean.MCAR.cov.40.AD.men.cl.25.40[2],
                                           mean.MCAR.cov.45.AD.men.cl.25.40[2], mean.MCAR.cov.50.AD.men.cl.25.40[2],
                                           mean.MCAR.cov.55.AD.men.cl.25.40[2], mean.MCAR.cov.60.AD.men.cl.25.40[2],
                                           mean.MCAR.cov.65.AD.men.cl.25.40[2], mean.MCAR.cov.70.AD.men.cl.25.40[2],
                                           mean.MCAR.cov.75.AD.men.cl.25.40[2], mean.MCAR.cov.80.AD.men.cl.25.40[2],
                                           mean.MCAR.cov.85.AD.men.cl.25.40[2], mean.MCAR.cov.90.AD.men.cl.25.40[2],
                                           mean.MCAR.cov.95.AD.men.cl.25.40[2], mean.AD.num.men.true.cov.100.25.40[2]),
                                     
                                     L = c(mean.MCAR.cov.35.AD.men.cl.25.40[1], mean.MCAR.cov.40.AD.men.cl.25.40[1],
                                           mean.MCAR.cov.45.AD.men.cl.25.40[1], mean.MCAR.cov.50.AD.men.cl.25.40[1],
                                           mean.MCAR.cov.55.AD.men.cl.25.40[1], mean.MCAR.cov.60.AD.men.cl.25.40[1],
                                           mean.MCAR.cov.65.AD.men.cl.25.40[1], mean.MCAR.cov.70.AD.men.cl.25.40[1],
                                           mean.MCAR.cov.75.AD.men.cl.25.40[1], mean.MCAR.cov.80.AD.men.cl.25.40[1],
                                           mean.MCAR.cov.85.AD.men.cl.25.40[1], mean.MCAR.cov.90.AD.men.cl.25.40[1],
                                           mean.MCAR.cov.95.AD.men.cl.25.40[1], mean.AD.num.men.true.cov.100.25.40[1]),
                                     
                                     U = c(mean.MCAR.cov.35.AD.men.cl.25.40[3], mean.MCAR.cov.40.AD.men.cl.25.40[3],
                                           mean.MCAR.cov.45.AD.men.cl.25.40[3], mean.MCAR.cov.50.AD.men.cl.25.40[3],
                                           mean.MCAR.cov.55.AD.men.cl.25.40[3], mean.MCAR.cov.60.AD.men.cl.25.40[3],
                                           mean.MCAR.cov.65.AD.men.cl.25.40[3], mean.MCAR.cov.70.AD.men.cl.25.40[3],
                                           mean.MCAR.cov.75.AD.men.cl.25.40[3], mean.MCAR.cov.80.AD.men.cl.25.40[3],
                                           mean.MCAR.cov.85.AD.men.cl.25.40[3], mean.MCAR.cov.90.AD.men.cl.25.40[3],
                                           mean.MCAR.cov.95.AD.men.cl.25.40[3], mean.AD.num.men.true.cov.100.25.40[3]))


plot.mean.MCAR.men.cl.25.40 <- ggplot(mean.MCAR.men.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.mean.MCAR.men.cl.25.40.png",
       plot = plot.mean.MCAR.men.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 40.50

mean.MCAR.women.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                       
                                       F = c(mean.MCAR.cov.35.AD.women.cl.40.50[2], mean.MCAR.cov.40.AD.women.cl.40.50[2],
                                             mean.MCAR.cov.45.AD.women.cl.40.50[2], mean.MCAR.cov.50.AD.women.cl.40.50[2],
                                             mean.MCAR.cov.55.AD.women.cl.40.50[2], mean.MCAR.cov.60.AD.women.cl.40.50[2],
                                             mean.MCAR.cov.65.AD.women.cl.40.50[2], mean.MCAR.cov.70.AD.women.cl.40.50[2],
                                             mean.MCAR.cov.75.AD.women.cl.40.50[2], mean.MCAR.cov.80.AD.women.cl.40.50[2],
                                             mean.MCAR.cov.85.AD.women.cl.40.50[2], mean.MCAR.cov.90.AD.women.cl.40.50[2],
                                             mean.MCAR.cov.95.AD.women.cl.40.50[2], mean.AD.num.women.true.cov.100.25.40[2]),
                                       
                                       L = c(mean.MCAR.cov.35.AD.women.cl.40.50[1], mean.MCAR.cov.40.AD.women.cl.40.50[1],
                                             mean.MCAR.cov.45.AD.women.cl.40.50[1], mean.MCAR.cov.50.AD.women.cl.40.50[1],
                                             mean.MCAR.cov.55.AD.women.cl.40.50[1], mean.MCAR.cov.60.AD.women.cl.40.50[1],
                                             mean.MCAR.cov.65.AD.women.cl.40.50[1], mean.MCAR.cov.70.AD.women.cl.40.50[1],
                                             mean.MCAR.cov.75.AD.women.cl.40.50[1], mean.MCAR.cov.80.AD.women.cl.40.50[1],
                                             mean.MCAR.cov.85.AD.women.cl.40.50[1], mean.MCAR.cov.90.AD.women.cl.40.50[1],
                                             mean.MCAR.cov.95.AD.women.cl.40.50[1], mean.AD.num.women.true.cov.100.25.40[1]),
                                       
                                       U = c(mean.MCAR.cov.35.AD.women.cl.40.50[3], mean.MCAR.cov.40.AD.women.cl.40.50[3],
                                             mean.MCAR.cov.45.AD.women.cl.40.50[3], mean.MCAR.cov.50.AD.women.cl.40.50[3],
                                             mean.MCAR.cov.55.AD.women.cl.40.50[3], mean.MCAR.cov.60.AD.women.cl.40.50[3],
                                             mean.MCAR.cov.65.AD.women.cl.40.50[3], mean.MCAR.cov.70.AD.women.cl.40.50[3],
                                             mean.MCAR.cov.75.AD.women.cl.40.50[3], mean.MCAR.cov.80.AD.women.cl.40.50[3],
                                             mean.MCAR.cov.85.AD.women.cl.40.50[3], mean.MCAR.cov.90.AD.women.cl.40.50[3],
                                             mean.MCAR.cov.95.AD.women.cl.40.50[3], mean.AD.num.women.true.cov.100.25.40[3]))


plot.mean.MCAR.women.cl.40.50 <- ggplot(mean.MCAR.women.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.mean.MCAR.women.cl.40.50.png",
       plot = plot.mean.MCAR.women.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

mean.MCAR.men.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                     
                                     F = c(mean.MCAR.cov.35.AD.men.cl.40.50[2], mean.MCAR.cov.40.AD.men.cl.40.50[2],
                                           mean.MCAR.cov.45.AD.men.cl.40.50[2], mean.MCAR.cov.50.AD.men.cl.40.50[2],
                                           mean.MCAR.cov.55.AD.men.cl.40.50[2], mean.MCAR.cov.60.AD.men.cl.40.50[2],
                                           mean.MCAR.cov.65.AD.men.cl.40.50[2], mean.MCAR.cov.70.AD.men.cl.40.50[2],
                                           mean.MCAR.cov.75.AD.men.cl.40.50[2], mean.MCAR.cov.80.AD.men.cl.40.50[2],
                                           mean.MCAR.cov.85.AD.men.cl.40.50[2], mean.MCAR.cov.90.AD.men.cl.40.50[2],
                                           mean.MCAR.cov.95.AD.men.cl.40.50[2], mean.AD.num.men.true.cov.100.40.50[2]),
                                     
                                     L = c(mean.MCAR.cov.35.AD.men.cl.40.50[1], mean.MCAR.cov.40.AD.men.cl.40.50[1],
                                           mean.MCAR.cov.45.AD.men.cl.40.50[1], mean.MCAR.cov.50.AD.men.cl.40.50[1],
                                           mean.MCAR.cov.55.AD.men.cl.40.50[1], mean.MCAR.cov.60.AD.men.cl.40.50[1],
                                           mean.MCAR.cov.65.AD.men.cl.40.50[1], mean.MCAR.cov.70.AD.men.cl.40.50[1],
                                           mean.MCAR.cov.75.AD.men.cl.40.50[1], mean.MCAR.cov.80.AD.men.cl.40.50[1],
                                           mean.MCAR.cov.85.AD.men.cl.40.50[1], mean.MCAR.cov.90.AD.men.cl.40.50[1],
                                           mean.MCAR.cov.95.AD.men.cl.40.50[1], mean.AD.num.men.true.cov.100.40.50[1]),
                                     
                                     U = c(mean.MCAR.cov.35.AD.men.cl.40.50[3], mean.MCAR.cov.40.AD.men.cl.40.50[3],
                                           mean.MCAR.cov.45.AD.men.cl.40.50[3], mean.MCAR.cov.50.AD.men.cl.40.50[3],
                                           mean.MCAR.cov.55.AD.men.cl.40.50[3], mean.MCAR.cov.60.AD.men.cl.40.50[3],
                                           mean.MCAR.cov.65.AD.men.cl.40.50[3], mean.MCAR.cov.70.AD.men.cl.40.50[3],
                                           mean.MCAR.cov.75.AD.men.cl.40.50[3], mean.MCAR.cov.80.AD.men.cl.40.50[3],
                                           mean.MCAR.cov.85.AD.men.cl.40.50[3], mean.MCAR.cov.90.AD.men.cl.40.50[3],
                                           mean.MCAR.cov.95.AD.men.cl.40.50[3], mean.AD.num.men.true.cov.100.40.50[3]))


plot.mean.MCAR.men.cl.40.50 <- ggplot(mean.MCAR.men.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.men.cl.40.50.png",
       plot = plot.mean.MCAR.men.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




## Median

# Women 15.25

med.MCAR.women.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                      
                                      F = c(med.MCAR.cov.35.AD.women.cl.15.25[2], med.MCAR.cov.40.AD.women.cl.15.25[2],
                                            med.MCAR.cov.45.AD.women.cl.15.25[2], med.MCAR.cov.50.AD.women.cl.15.25[2],
                                            med.MCAR.cov.55.AD.women.cl.15.25[2], med.MCAR.cov.60.AD.women.cl.15.25[2],
                                            med.MCAR.cov.65.AD.women.cl.15.25[2], med.MCAR.cov.70.AD.women.cl.15.25[2],
                                            med.MCAR.cov.75.AD.women.cl.15.25[2], med.MCAR.cov.80.AD.women.cl.15.25[2],
                                            med.MCAR.cov.85.AD.women.cl.15.25[2], med.MCAR.cov.90.AD.women.cl.15.25[2],
                                            med.MCAR.cov.95.AD.women.cl.15.25[2], med.AD.num.women.true.cov.100.15.25[2]),
                                      
                                      L = c(med.MCAR.cov.35.AD.women.cl.15.25[1], med.MCAR.cov.40.AD.women.cl.15.25[1],
                                            med.MCAR.cov.45.AD.women.cl.15.25[1], med.MCAR.cov.50.AD.women.cl.15.25[1],
                                            med.MCAR.cov.55.AD.women.cl.15.25[1], med.MCAR.cov.60.AD.women.cl.15.25[1],
                                            med.MCAR.cov.65.AD.women.cl.15.25[1], med.MCAR.cov.70.AD.women.cl.15.25[1],
                                            med.MCAR.cov.75.AD.women.cl.15.25[1], med.MCAR.cov.80.AD.women.cl.15.25[1],
                                            med.MCAR.cov.85.AD.women.cl.15.25[1], med.MCAR.cov.90.AD.women.cl.15.25[1],
                                            med.MCAR.cov.95.AD.women.cl.15.25[1], med.AD.num.women.true.cov.100.15.25[1]),
                                      
                                      U = c(med.MCAR.cov.35.AD.women.cl.15.25[3], med.MCAR.cov.40.AD.women.cl.15.25[3],
                                            med.MCAR.cov.45.AD.women.cl.15.25[3], med.MCAR.cov.50.AD.women.cl.15.25[3],
                                            med.MCAR.cov.55.AD.women.cl.15.25[3], med.MCAR.cov.60.AD.women.cl.15.25[3],
                                            med.MCAR.cov.65.AD.women.cl.15.25[3], med.MCAR.cov.70.AD.women.cl.15.25[3],
                                            med.MCAR.cov.75.AD.women.cl.15.25[3], med.MCAR.cov.80.AD.women.cl.15.25[3],
                                            med.MCAR.cov.85.AD.women.cl.15.25[3], med.MCAR.cov.90.AD.women.cl.15.25[3],
                                            med.MCAR.cov.95.AD.women.cl.15.25[3], med.AD.num.women.true.cov.100.15.25[3]))


plot.med.MCAR.women.cl.15.25 <- ggplot(med.MCAR.women.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.cl.15.25.png",
       plot = plot.med.MCAR.women.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

med.MCAR.men.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                    
                                    F = c(med.MCAR.cov.35.AD.men.cl.15.25[2], med.MCAR.cov.40.AD.men.cl.15.25[2],
                                          med.MCAR.cov.45.AD.men.cl.15.25[2], med.MCAR.cov.50.AD.men.cl.15.25[2],
                                          med.MCAR.cov.55.AD.men.cl.15.25[2], med.MCAR.cov.60.AD.men.cl.15.25[2],
                                          med.MCAR.cov.65.AD.men.cl.15.25[2], med.MCAR.cov.70.AD.men.cl.15.25[2],
                                          med.MCAR.cov.75.AD.men.cl.15.25[2], med.MCAR.cov.80.AD.men.cl.15.25[2],
                                          med.MCAR.cov.85.AD.men.cl.15.25[2], med.MCAR.cov.90.AD.men.cl.15.25[2],
                                          med.MCAR.cov.95.AD.men.cl.15.25[2], med.AD.num.men.true.cov.100.15.25[2]),
                                    
                                    L = c(med.MCAR.cov.35.AD.men.cl.15.25[1], med.MCAR.cov.40.AD.men.cl.15.25[1],
                                          med.MCAR.cov.45.AD.men.cl.15.25[1], med.MCAR.cov.50.AD.men.cl.15.25[1],
                                          med.MCAR.cov.55.AD.men.cl.15.25[1], med.MCAR.cov.60.AD.men.cl.15.25[1],
                                          med.MCAR.cov.65.AD.men.cl.15.25[1], med.MCAR.cov.70.AD.men.cl.15.25[1],
                                          med.MCAR.cov.75.AD.men.cl.15.25[1], med.MCAR.cov.80.AD.men.cl.15.25[1],
                                          med.MCAR.cov.85.AD.men.cl.15.25[1], med.MCAR.cov.90.AD.men.cl.15.25[1],
                                          med.MCAR.cov.95.AD.men.cl.15.25[1], med.AD.num.men.true.cov.100.15.25[1]),
                                    
                                    U = c(med.MCAR.cov.35.AD.men.cl.15.25[3], med.MCAR.cov.40.AD.men.cl.15.25[3],
                                          med.MCAR.cov.45.AD.men.cl.15.25[3], med.MCAR.cov.50.AD.men.cl.15.25[3],
                                          med.MCAR.cov.55.AD.men.cl.15.25[3], med.MCAR.cov.60.AD.men.cl.15.25[3],
                                          med.MCAR.cov.65.AD.men.cl.15.25[3], med.MCAR.cov.70.AD.men.cl.15.25[3],
                                          med.MCAR.cov.75.AD.men.cl.15.25[3], med.MCAR.cov.80.AD.men.cl.15.25[3],
                                          med.MCAR.cov.85.AD.men.cl.15.25[3], med.MCAR.cov.90.AD.men.cl.15.25[3],
                                          med.MCAR.cov.95.AD.men.cl.15.25[3], med.AD.num.men.true.cov.100.15.25[3]))


plot.med.MCAR.men.cl.15.25 <- ggplot(med.MCAR.men.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.cl.15.25.png",
       plot = plot.med.MCAR.men.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

med.MCAR.women.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                      
                                      F = c(med.MCAR.cov.35.AD.women.cl.25.40[2], med.MCAR.cov.40.AD.women.cl.25.40[2],
                                            med.MCAR.cov.45.AD.women.cl.25.40[2], med.MCAR.cov.50.AD.women.cl.25.40[2],
                                            med.MCAR.cov.55.AD.women.cl.25.40[2], med.MCAR.cov.60.AD.women.cl.25.40[2],
                                            med.MCAR.cov.65.AD.women.cl.25.40[2], med.MCAR.cov.70.AD.women.cl.25.40[2],
                                            med.MCAR.cov.75.AD.women.cl.25.40[2], med.MCAR.cov.80.AD.women.cl.25.40[2],
                                            med.MCAR.cov.85.AD.women.cl.25.40[2], med.MCAR.cov.90.AD.women.cl.25.40[2],
                                            med.MCAR.cov.95.AD.women.cl.25.40[2], med.AD.num.women.true.cov.100.25.40[2]),
                                      
                                      L = c(med.MCAR.cov.35.AD.women.cl.25.40[1], med.MCAR.cov.40.AD.women.cl.25.40[1],
                                            med.MCAR.cov.45.AD.women.cl.25.40[1], med.MCAR.cov.50.AD.women.cl.25.40[1],
                                            med.MCAR.cov.55.AD.women.cl.25.40[1], med.MCAR.cov.60.AD.women.cl.25.40[1],
                                            med.MCAR.cov.65.AD.women.cl.25.40[1], med.MCAR.cov.70.AD.women.cl.25.40[1],
                                            med.MCAR.cov.75.AD.women.cl.25.40[1], med.MCAR.cov.80.AD.women.cl.25.40[1],
                                            med.MCAR.cov.85.AD.women.cl.25.40[1], med.MCAR.cov.90.AD.women.cl.25.40[1],
                                            med.MCAR.cov.95.AD.women.cl.25.40[1], med.AD.num.women.true.cov.100.25.40[1]),
                                      
                                      U = c(med.MCAR.cov.35.AD.women.cl.25.40[3], med.MCAR.cov.40.AD.women.cl.25.40[3],
                                            med.MCAR.cov.45.AD.women.cl.25.40[3], med.MCAR.cov.50.AD.women.cl.25.40[3],
                                            med.MCAR.cov.55.AD.women.cl.25.40[3], med.MCAR.cov.60.AD.women.cl.25.40[3],
                                            med.MCAR.cov.65.AD.women.cl.25.40[3], med.MCAR.cov.70.AD.women.cl.25.40[3],
                                            med.MCAR.cov.75.AD.women.cl.25.40[3], med.MCAR.cov.80.AD.women.cl.25.40[3],
                                            med.MCAR.cov.85.AD.women.cl.25.40[3], med.MCAR.cov.90.AD.women.cl.25.40[3],
                                            med.MCAR.cov.95.AD.women.cl.25.40[3], med.AD.num.women.true.cov.100.25.40[3]))


plot.med.MCAR.women.cl.25.40 <- ggplot(med.MCAR.women.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.cl.25.40.png",
       plot = plot.med.MCAR.women.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Men: 25.40

med.MCAR.men.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                    
                                    F = c(med.MCAR.cov.35.AD.men.cl.25.40[2], med.MCAR.cov.40.AD.men.cl.25.40[2],
                                          med.MCAR.cov.45.AD.men.cl.25.40[2], med.MCAR.cov.50.AD.men.cl.25.40[2],
                                          med.MCAR.cov.55.AD.men.cl.25.40[2], med.MCAR.cov.60.AD.men.cl.25.40[2],
                                          med.MCAR.cov.65.AD.men.cl.25.40[2], med.MCAR.cov.70.AD.men.cl.25.40[2],
                                          med.MCAR.cov.75.AD.men.cl.25.40[2], med.MCAR.cov.80.AD.men.cl.25.40[2],
                                          med.MCAR.cov.85.AD.men.cl.25.40[2], med.MCAR.cov.90.AD.men.cl.25.40[2],
                                          med.MCAR.cov.95.AD.men.cl.25.40[2], med.AD.num.men.true.cov.100.25.40[2]),
                                    
                                    L = c(med.MCAR.cov.35.AD.men.cl.25.40[1], med.MCAR.cov.40.AD.men.cl.25.40[1],
                                          med.MCAR.cov.45.AD.men.cl.25.40[1], med.MCAR.cov.50.AD.men.cl.25.40[1],
                                          med.MCAR.cov.55.AD.men.cl.25.40[1], med.MCAR.cov.60.AD.men.cl.25.40[1],
                                          med.MCAR.cov.65.AD.men.cl.25.40[1], med.MCAR.cov.70.AD.men.cl.25.40[1],
                                          med.MCAR.cov.75.AD.men.cl.25.40[1], med.MCAR.cov.80.AD.men.cl.25.40[1],
                                          med.MCAR.cov.85.AD.men.cl.25.40[1], med.MCAR.cov.90.AD.men.cl.25.40[1],
                                          med.MCAR.cov.95.AD.men.cl.25.40[1], med.AD.num.men.true.cov.100.25.40[1]),
                                    
                                    U = c(med.MCAR.cov.35.AD.men.cl.25.40[3], med.MCAR.cov.40.AD.men.cl.25.40[3],
                                          med.MCAR.cov.45.AD.men.cl.25.40[3], med.MCAR.cov.50.AD.men.cl.25.40[3],
                                          med.MCAR.cov.55.AD.men.cl.25.40[3], med.MCAR.cov.60.AD.men.cl.25.40[3],
                                          med.MCAR.cov.65.AD.men.cl.25.40[3], med.MCAR.cov.70.AD.men.cl.25.40[3],
                                          med.MCAR.cov.75.AD.men.cl.25.40[3], med.MCAR.cov.80.AD.men.cl.25.40[3],
                                          med.MCAR.cov.85.AD.men.cl.25.40[3], med.MCAR.cov.90.AD.men.cl.25.40[3],
                                          med.MCAR.cov.95.AD.men.cl.25.40[3], med.AD.num.men.true.cov.100.25.40[3]))


plot.med.MCAR.men.cl.25.40 <- ggplot(med.MCAR.men.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.cl.25.40.png",
       plot = plot.med.MCAR.men.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 40.50

med.MCAR.women.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                      
                                      F = c(med.MCAR.cov.35.AD.women.cl.40.50[2], med.MCAR.cov.40.AD.women.cl.40.50[2],
                                            med.MCAR.cov.45.AD.women.cl.40.50[2], med.MCAR.cov.50.AD.women.cl.40.50[2],
                                            med.MCAR.cov.55.AD.women.cl.40.50[2], med.MCAR.cov.60.AD.women.cl.40.50[2],
                                            med.MCAR.cov.65.AD.women.cl.40.50[2], med.MCAR.cov.70.AD.women.cl.40.50[2],
                                            med.MCAR.cov.75.AD.women.cl.40.50[2], med.MCAR.cov.80.AD.women.cl.40.50[2],
                                            med.MCAR.cov.85.AD.women.cl.40.50[2], med.MCAR.cov.90.AD.women.cl.40.50[2],
                                            med.MCAR.cov.95.AD.women.cl.40.50[2], med.AD.num.women.true.cov.100.40.50[2]),
                                      
                                      L = c(med.MCAR.cov.35.AD.women.cl.40.50[1], med.MCAR.cov.40.AD.women.cl.40.50[1],
                                            med.MCAR.cov.45.AD.women.cl.40.50[1], med.MCAR.cov.50.AD.women.cl.40.50[1],
                                            med.MCAR.cov.55.AD.women.cl.40.50[1], med.MCAR.cov.60.AD.women.cl.40.50[1],
                                            med.MCAR.cov.65.AD.women.cl.40.50[1], med.MCAR.cov.70.AD.women.cl.40.50[1],
                                            med.MCAR.cov.75.AD.women.cl.40.50[1], med.MCAR.cov.80.AD.women.cl.40.50[1],
                                            med.MCAR.cov.85.AD.women.cl.40.50[1], med.MCAR.cov.90.AD.women.cl.40.50[1],
                                            med.MCAR.cov.95.AD.women.cl.40.50[1], med.AD.num.women.true.cov.100.40.50[1]),
                                      
                                      U = c(med.MCAR.cov.35.AD.women.cl.40.50[3], med.MCAR.cov.40.AD.women.cl.40.50[3],
                                            med.MCAR.cov.45.AD.women.cl.40.50[3], med.MCAR.cov.50.AD.women.cl.40.50[3],
                                            med.MCAR.cov.55.AD.women.cl.40.50[3], med.MCAR.cov.60.AD.women.cl.40.50[3],
                                            med.MCAR.cov.65.AD.women.cl.40.50[3], med.MCAR.cov.70.AD.women.cl.40.50[3],
                                            med.MCAR.cov.75.AD.women.cl.40.50[3], med.MCAR.cov.80.AD.women.cl.40.50[3],
                                            med.MCAR.cov.85.AD.women.cl.40.50[3], med.MCAR.cov.90.AD.women.cl.40.50[3],
                                            med.MCAR.cov.95.AD.women.cl.40.50[3], med.AD.num.women.true.cov.100.40.50[3]))


plot.med.MCAR.women.cl.40.50 <- ggplot(med.MCAR.women.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.cl.40.50.png",
       plot = plot.med.MCAR.women.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

med.MCAR.men.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                    
                                    F = c(med.MCAR.cov.35.AD.men.cl.40.50[2], med.MCAR.cov.40.AD.men.cl.40.50[2],
                                          med.MCAR.cov.45.AD.men.cl.40.50[2], med.MCAR.cov.50.AD.men.cl.40.50[2],
                                          med.MCAR.cov.55.AD.men.cl.40.50[2], med.MCAR.cov.60.AD.men.cl.40.50[2],
                                          med.MCAR.cov.65.AD.men.cl.40.50[2], med.MCAR.cov.70.AD.men.cl.40.50[2],
                                          med.MCAR.cov.75.AD.men.cl.40.50[2], med.MCAR.cov.80.AD.men.cl.40.50[2],
                                          med.MCAR.cov.85.AD.men.cl.40.50[2], med.MCAR.cov.90.AD.men.cl.40.50[2],
                                          med.MCAR.cov.95.AD.men.cl.40.50[2], med.AD.num.men.true.cov.100.40.50[2]),
                                    
                                    L = c(med.MCAR.cov.35.AD.men.cl.40.50[1], med.MCAR.cov.40.AD.men.cl.40.50[1],
                                          med.MCAR.cov.45.AD.men.cl.40.50[1], med.MCAR.cov.50.AD.men.cl.40.50[1],
                                          med.MCAR.cov.55.AD.men.cl.40.50[1], med.MCAR.cov.60.AD.men.cl.40.50[1],
                                          med.MCAR.cov.65.AD.men.cl.40.50[1], med.MCAR.cov.70.AD.men.cl.40.50[1],
                                          med.MCAR.cov.75.AD.men.cl.40.50[1], med.MCAR.cov.80.AD.men.cl.40.50[1],
                                          med.MCAR.cov.85.AD.men.cl.40.50[1], med.MCAR.cov.90.AD.men.cl.40.50[1],
                                          med.MCAR.cov.95.AD.men.cl.40.50[1], med.AD.num.men.true.cov.100.40.50[1]),
                                    
                                    U = c(med.MCAR.cov.35.AD.men.cl.40.50[3], med.MCAR.cov.40.AD.men.cl.40.50[3],
                                          med.MCAR.cov.45.AD.men.cl.40.50[3], med.MCAR.cov.50.AD.men.cl.40.50[3],
                                          med.MCAR.cov.55.AD.men.cl.40.50[3], med.MCAR.cov.60.AD.men.cl.40.50[3],
                                          med.MCAR.cov.65.AD.men.cl.40.50[3], med.MCAR.cov.70.AD.men.cl.40.50[3],
                                          med.MCAR.cov.75.AD.men.cl.40.50[3], med.MCAR.cov.80.AD.men.cl.40.50[3],
                                          med.MCAR.cov.85.AD.men.cl.40.50[3], med.MCAR.cov.90.AD.men.cl.40.50[3],
                                          med.MCAR.cov.95.AD.men.cl.40.50[3], med.AD.num.men.true.cov.100.40.50[3]))


plot.med.MCAR.men.cl.40.50 <- ggplot(med.MCAR.men.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.cl.40.50.png",
       plot = plot.med.MCAR.men.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



## Standard deviation


# Women 15.25

sd.MCAR.women.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                     
                                     F = c(sd.MCAR.cov.35.AD.women.cl.15.25[2], sd.MCAR.cov.40.AD.women.cl.15.25[2],
                                           sd.MCAR.cov.45.AD.women.cl.15.25[2], sd.MCAR.cov.50.AD.women.cl.15.25[2],
                                           sd.MCAR.cov.55.AD.women.cl.15.25[2], sd.MCAR.cov.60.AD.women.cl.15.25[2],
                                           sd.MCAR.cov.65.AD.women.cl.15.25[2], sd.MCAR.cov.70.AD.women.cl.15.25[2],
                                           sd.MCAR.cov.75.AD.women.cl.15.25[2], sd.MCAR.cov.80.AD.women.cl.15.25[2],
                                           sd.MCAR.cov.85.AD.women.cl.15.25[2], sd.MCAR.cov.90.AD.women.cl.15.25[2],
                                           sd.MCAR.cov.95.AD.women.cl.15.25[2], sd.AD.num.women.true.cov.100.15.25[2]),
                                     
                                     L = c(sd.MCAR.cov.35.AD.women.cl.15.25[1], sd.MCAR.cov.40.AD.women.cl.15.25[1],
                                           sd.MCAR.cov.45.AD.women.cl.15.25[1], sd.MCAR.cov.50.AD.women.cl.15.25[1],
                                           sd.MCAR.cov.55.AD.women.cl.15.25[1], sd.MCAR.cov.60.AD.women.cl.15.25[1],
                                           sd.MCAR.cov.65.AD.women.cl.15.25[1], sd.MCAR.cov.70.AD.women.cl.15.25[1],
                                           sd.MCAR.cov.75.AD.women.cl.15.25[1], sd.MCAR.cov.80.AD.women.cl.15.25[1],
                                           sd.MCAR.cov.85.AD.women.cl.15.25[1], sd.MCAR.cov.90.AD.women.cl.15.25[1],
                                           sd.MCAR.cov.95.AD.women.cl.15.25[1], sd.AD.num.women.true.cov.100.15.25[1]),
                                     
                                     U = c(sd.MCAR.cov.35.AD.women.cl.15.25[3], sd.MCAR.cov.40.AD.women.cl.15.25[3],
                                           sd.MCAR.cov.45.AD.women.cl.15.25[3], sd.MCAR.cov.50.AD.women.cl.15.25[3],
                                           sd.MCAR.cov.55.AD.women.cl.15.25[3], sd.MCAR.cov.60.AD.women.cl.15.25[3],
                                           sd.MCAR.cov.65.AD.women.cl.15.25[3], sd.MCAR.cov.70.AD.women.cl.15.25[3],
                                           sd.MCAR.cov.75.AD.women.cl.15.25[3], sd.MCAR.cov.80.AD.women.cl.15.25[3],
                                           sd.MCAR.cov.85.AD.women.cl.15.25[3], sd.MCAR.cov.90.AD.women.cl.15.25[3],
                                           sd.MCAR.cov.95.AD.women.cl.15.25[3], sd.AD.num.women.true.cov.100.15.25[3]))


plot.sd.MCAR.women.cl.15.25 <- ggplot(sd.MCAR.women.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.cl.15.25.png",
       plot = plot.sd.MCAR.women.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

sd.MCAR.men.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                   
                                   F = c(sd.MCAR.cov.35.AD.men.cl.15.25[2], sd.MCAR.cov.40.AD.men.cl.15.25[2],
                                         sd.MCAR.cov.45.AD.men.cl.15.25[2], sd.MCAR.cov.50.AD.men.cl.15.25[2],
                                         sd.MCAR.cov.55.AD.men.cl.15.25[2], sd.MCAR.cov.60.AD.men.cl.15.25[2],
                                         sd.MCAR.cov.65.AD.men.cl.15.25[2], sd.MCAR.cov.70.AD.men.cl.15.25[2],
                                         sd.MCAR.cov.75.AD.men.cl.15.25[2], sd.MCAR.cov.80.AD.men.cl.15.25[2],
                                         sd.MCAR.cov.85.AD.men.cl.15.25[2], sd.MCAR.cov.90.AD.men.cl.15.25[2],
                                         sd.MCAR.cov.95.AD.men.cl.15.25[2], sd.AD.num.men.true.cov.100.15.25[2]),
                                   
                                   L = c(sd.MCAR.cov.35.AD.men.cl.15.25[1], sd.MCAR.cov.40.AD.men.cl.15.25[1],
                                         sd.MCAR.cov.45.AD.men.cl.15.25[1], sd.MCAR.cov.50.AD.men.cl.15.25[1],
                                         sd.MCAR.cov.55.AD.men.cl.15.25[1], sd.MCAR.cov.60.AD.men.cl.15.25[1],
                                         sd.MCAR.cov.65.AD.men.cl.15.25[1], sd.MCAR.cov.70.AD.men.cl.15.25[1],
                                         sd.MCAR.cov.75.AD.men.cl.15.25[1], sd.MCAR.cov.80.AD.men.cl.15.25[1],
                                         sd.MCAR.cov.85.AD.men.cl.15.25[1], sd.MCAR.cov.90.AD.men.cl.15.25[1],
                                         sd.MCAR.cov.95.AD.men.cl.15.25[1], sd.AD.num.men.true.cov.100.15.25[1]),
                                   
                                   U = c(sd.MCAR.cov.35.AD.men.cl.15.25[3], sd.MCAR.cov.40.AD.men.cl.15.25[3],
                                         sd.MCAR.cov.45.AD.men.cl.15.25[3], sd.MCAR.cov.50.AD.men.cl.15.25[3],
                                         sd.MCAR.cov.55.AD.men.cl.15.25[3], sd.MCAR.cov.60.AD.men.cl.15.25[3],
                                         sd.MCAR.cov.65.AD.men.cl.15.25[3], sd.MCAR.cov.70.AD.men.cl.15.25[3],
                                         sd.MCAR.cov.75.AD.men.cl.15.25[3], sd.MCAR.cov.80.AD.men.cl.15.25[3],
                                         sd.MCAR.cov.85.AD.men.cl.15.25[3], sd.MCAR.cov.90.AD.men.cl.15.25[3],
                                         sd.MCAR.cov.95.AD.men.cl.15.25[3], sd.AD.num.men.true.cov.100.15.25[3]))


plot.sd.MCAR.men.cl.15.25 <- ggplot(sd.MCAR.men.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.sd.MCAR.men.cl.15.25.png",
       plot = plot.sd.MCAR.men.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

sd.MCAR.women.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                     
                                     F = c(sd.MCAR.cov.35.AD.women.cl.25.40[2], sd.MCAR.cov.40.AD.women.cl.25.40[2],
                                           sd.MCAR.cov.45.AD.women.cl.25.40[2], sd.MCAR.cov.50.AD.women.cl.25.40[2],
                                           sd.MCAR.cov.55.AD.women.cl.25.40[2], sd.MCAR.cov.60.AD.women.cl.25.40[2],
                                           sd.MCAR.cov.65.AD.women.cl.25.40[2], sd.MCAR.cov.70.AD.women.cl.25.40[2],
                                           sd.MCAR.cov.75.AD.women.cl.25.40[2], sd.MCAR.cov.80.AD.women.cl.25.40[2],
                                           sd.MCAR.cov.85.AD.women.cl.25.40[2], sd.MCAR.cov.90.AD.women.cl.25.40[2],
                                           sd.MCAR.cov.95.AD.women.cl.25.40[2], sd.AD.num.women.true.cov.100.25.40[2]),
                                     
                                     L = c(sd.MCAR.cov.35.AD.women.cl.25.40[1], sd.MCAR.cov.40.AD.women.cl.25.40[1],
                                           sd.MCAR.cov.45.AD.women.cl.25.40[1], sd.MCAR.cov.50.AD.women.cl.25.40[1],
                                           sd.MCAR.cov.55.AD.women.cl.25.40[1], sd.MCAR.cov.60.AD.women.cl.25.40[1],
                                           sd.MCAR.cov.65.AD.women.cl.25.40[1], sd.MCAR.cov.70.AD.women.cl.25.40[1],
                                           sd.MCAR.cov.75.AD.women.cl.25.40[1], sd.MCAR.cov.80.AD.women.cl.25.40[1],
                                           sd.MCAR.cov.85.AD.women.cl.25.40[1], sd.MCAR.cov.90.AD.women.cl.25.40[1],
                                           sd.MCAR.cov.95.AD.women.cl.25.40[1], sd.AD.num.women.true.cov.100.25.40[1]),
                                     
                                     U = c(sd.MCAR.cov.35.AD.women.cl.25.40[3], sd.MCAR.cov.40.AD.women.cl.25.40[3],
                                           sd.MCAR.cov.45.AD.women.cl.25.40[3], sd.MCAR.cov.50.AD.women.cl.25.40[3],
                                           sd.MCAR.cov.55.AD.women.cl.25.40[3], sd.MCAR.cov.60.AD.women.cl.25.40[3],
                                           sd.MCAR.cov.65.AD.women.cl.25.40[3], sd.MCAR.cov.70.AD.women.cl.25.40[3],
                                           sd.MCAR.cov.75.AD.women.cl.25.40[3], sd.MCAR.cov.80.AD.women.cl.25.40[3],
                                           sd.MCAR.cov.85.AD.women.cl.25.40[3], sd.MCAR.cov.90.AD.women.cl.25.40[3],
                                           sd.MCAR.cov.95.AD.women.cl.25.40[3], sd.AD.num.women.true.cov.100.25.40[3]))


plot.sd.MCAR.women.cl.25.40 <- ggplot(sd.MCAR.women.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.cl.25.40.png",
       plot = plot.sd.MCAR.women.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Men: 25.40

sd.MCAR.men.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                   
                                   F = c(sd.MCAR.cov.35.AD.men.cl.25.40[2], sd.MCAR.cov.40.AD.men.cl.25.40[2],
                                         sd.MCAR.cov.45.AD.men.cl.25.40[2], sd.MCAR.cov.50.AD.men.cl.25.40[2],
                                         sd.MCAR.cov.55.AD.men.cl.25.40[2], sd.MCAR.cov.60.AD.men.cl.25.40[2],
                                         sd.MCAR.cov.65.AD.men.cl.25.40[2], sd.MCAR.cov.70.AD.men.cl.25.40[2],
                                         sd.MCAR.cov.75.AD.men.cl.25.40[2], sd.MCAR.cov.80.AD.men.cl.25.40[2],
                                         sd.MCAR.cov.85.AD.men.cl.25.40[2], sd.MCAR.cov.90.AD.men.cl.25.40[2],
                                         sd.MCAR.cov.95.AD.men.cl.25.40[2], sd.AD.num.men.true.cov.100.25.40[2]),
                                   
                                   L = c(sd.MCAR.cov.35.AD.men.cl.25.40[1], sd.MCAR.cov.40.AD.men.cl.25.40[1],
                                         sd.MCAR.cov.45.AD.men.cl.25.40[1], sd.MCAR.cov.50.AD.men.cl.25.40[1],
                                         sd.MCAR.cov.55.AD.men.cl.25.40[1], sd.MCAR.cov.60.AD.men.cl.25.40[1],
                                         sd.MCAR.cov.65.AD.men.cl.25.40[1], sd.MCAR.cov.70.AD.men.cl.25.40[1],
                                         sd.MCAR.cov.75.AD.men.cl.25.40[1], sd.MCAR.cov.80.AD.men.cl.25.40[1],
                                         sd.MCAR.cov.85.AD.men.cl.25.40[1], sd.MCAR.cov.90.AD.men.cl.25.40[1],
                                         sd.MCAR.cov.95.AD.men.cl.25.40[1], sd.AD.num.men.true.cov.100.25.40[1]),
                                   
                                   U = c(sd.MCAR.cov.35.AD.men.cl.25.40[3], sd.MCAR.cov.40.AD.men.cl.25.40[3],
                                         sd.MCAR.cov.45.AD.men.cl.25.40[3], sd.MCAR.cov.50.AD.men.cl.25.40[3],
                                         sd.MCAR.cov.55.AD.men.cl.25.40[3], sd.MCAR.cov.60.AD.men.cl.25.40[3],
                                         sd.MCAR.cov.65.AD.men.cl.25.40[3], sd.MCAR.cov.70.AD.men.cl.25.40[3],
                                         sd.MCAR.cov.75.AD.men.cl.25.40[3], sd.MCAR.cov.80.AD.men.cl.25.40[3],
                                         sd.MCAR.cov.85.AD.men.cl.25.40[3], sd.MCAR.cov.90.AD.men.cl.25.40[3],
                                         sd.MCAR.cov.95.AD.men.cl.25.40[3], sd.AD.num.men.true.cov.100.25.40[3]))


plot.sd.MCAR.men.cl.25.40 <- ggplot(sd.MCAR.men.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.sd.MCAR.men.cl.25.40.png",
       plot = plot.sd.MCAR.men.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: 40.50

sd.MCAR.women.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                     
                                     F = c(sd.MCAR.cov.35.AD.women.cl.40.50[2], sd.MCAR.cov.40.AD.women.cl.40.50[2],
                                           sd.MCAR.cov.45.AD.women.cl.40.50[2], sd.MCAR.cov.50.AD.women.cl.40.50[2],
                                           sd.MCAR.cov.55.AD.women.cl.40.50[2], sd.MCAR.cov.60.AD.women.cl.40.50[2],
                                           sd.MCAR.cov.65.AD.women.cl.40.50[2], sd.MCAR.cov.70.AD.women.cl.40.50[2],
                                           sd.MCAR.cov.75.AD.women.cl.40.50[2], sd.MCAR.cov.80.AD.women.cl.40.50[2],
                                           sd.MCAR.cov.85.AD.women.cl.40.50[2], sd.MCAR.cov.90.AD.women.cl.40.50[2],
                                           sd.MCAR.cov.95.AD.women.cl.40.50[2], sd.AD.num.women.true.cov.100.40.50[2]),
                                     
                                     L = c(sd.MCAR.cov.35.AD.women.cl.40.50[1], sd.MCAR.cov.40.AD.women.cl.40.50[1],
                                           sd.MCAR.cov.45.AD.women.cl.40.50[1], sd.MCAR.cov.50.AD.women.cl.40.50[1],
                                           sd.MCAR.cov.55.AD.women.cl.40.50[1], sd.MCAR.cov.60.AD.women.cl.40.50[1],
                                           sd.MCAR.cov.65.AD.women.cl.40.50[1], sd.MCAR.cov.70.AD.women.cl.40.50[1],
                                           sd.MCAR.cov.75.AD.women.cl.40.50[1], sd.MCAR.cov.80.AD.women.cl.40.50[1],
                                           sd.MCAR.cov.85.AD.women.cl.40.50[1], sd.MCAR.cov.90.AD.women.cl.40.50[1],
                                           sd.MCAR.cov.95.AD.women.cl.40.50[1], sd.AD.num.women.true.cov.100.40.50[1]),
                                     
                                     U = c(sd.MCAR.cov.35.AD.women.cl.40.50[3], sd.MCAR.cov.40.AD.women.cl.40.50[3],
                                           sd.MCAR.cov.45.AD.women.cl.40.50[3], sd.MCAR.cov.50.AD.women.cl.40.50[3],
                                           sd.MCAR.cov.55.AD.women.cl.40.50[3], sd.MCAR.cov.60.AD.women.cl.40.50[3],
                                           sd.MCAR.cov.65.AD.women.cl.40.50[3], sd.MCAR.cov.70.AD.women.cl.40.50[3],
                                           sd.MCAR.cov.75.AD.women.cl.40.50[3], sd.MCAR.cov.80.AD.women.cl.40.50[3],
                                           sd.MCAR.cov.85.AD.women.cl.40.50[3], sd.MCAR.cov.90.AD.women.cl.40.50[3],
                                           sd.MCAR.cov.95.AD.women.cl.40.50[3], sd.AD.num.women.true.cov.100.40.50[3]))


plot.sd.MCAR.women.cl.40.50 <- ggplot(sd.MCAR.women.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.cl.40.50.png",
       plot = plot.sd.MCAR.women.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

sd.MCAR.men.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                   
                                   F = c(sd.MCAR.cov.35.AD.men.cl.40.50[2], sd.MCAR.cov.40.AD.men.cl.40.50[2],
                                         sd.MCAR.cov.45.AD.men.cl.40.50[2], sd.MCAR.cov.50.AD.men.cl.40.50[2],
                                         sd.MCAR.cov.55.AD.men.cl.40.50[2], sd.MCAR.cov.60.AD.men.cl.40.50[2],
                                         sd.MCAR.cov.65.AD.men.cl.40.50[2], sd.MCAR.cov.70.AD.men.cl.40.50[2],
                                         sd.MCAR.cov.75.AD.men.cl.40.50[2], sd.MCAR.cov.80.AD.men.cl.40.50[2],
                                         sd.MCAR.cov.85.AD.men.cl.40.50[2], sd.MCAR.cov.90.AD.men.cl.40.50[2],
                                         sd.MCAR.cov.95.AD.men.cl.40.50[2], sd.AD.num.men.true.cov.100.40.50[2]),
                                   
                                   L = c(sd.MCAR.cov.35.AD.men.cl.40.50[1], sd.MCAR.cov.40.AD.men.cl.40.50[1],
                                         sd.MCAR.cov.45.AD.men.cl.40.50[1], sd.MCAR.cov.50.AD.men.cl.40.50[1],
                                         sd.MCAR.cov.55.AD.men.cl.40.50[1], sd.MCAR.cov.60.AD.men.cl.40.50[1],
                                         sd.MCAR.cov.65.AD.men.cl.40.50[1], sd.MCAR.cov.70.AD.men.cl.40.50[1],
                                         sd.MCAR.cov.75.AD.men.cl.40.50[1], sd.MCAR.cov.80.AD.men.cl.40.50[1],
                                         sd.MCAR.cov.85.AD.men.cl.40.50[1], sd.MCAR.cov.90.AD.men.cl.40.50[1],
                                         sd.MCAR.cov.95.AD.men.cl.40.50[1], sd.AD.num.men.true.cov.100.40.50[1]),
                                   
                                   U = c(sd.MCAR.cov.35.AD.men.cl.40.50[3], sd.MCAR.cov.40.AD.men.cl.40.50[3],
                                         sd.MCAR.cov.45.AD.men.cl.40.50[3], sd.MCAR.cov.50.AD.men.cl.40.50[3],
                                         sd.MCAR.cov.55.AD.men.cl.40.50[3], sd.MCAR.cov.60.AD.men.cl.40.50[3],
                                         sd.MCAR.cov.65.AD.men.cl.40.50[3], sd.MCAR.cov.70.AD.men.cl.40.50[3],
                                         sd.MCAR.cov.75.AD.men.cl.40.50[3], sd.MCAR.cov.80.AD.men.cl.40.50[3],
                                         sd.MCAR.cov.85.AD.men.cl.40.50[3], sd.MCAR.cov.90.AD.men.cl.40.50[3],
                                         sd.MCAR.cov.95.AD.men.cl.40.50[3], sd.AD.num.men.true.cov.100.40.50[3]))


plot.sd.MCAR.men.cl.40.50 <- ggplot(sd.MCAR.men.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.men.cl.40.50.png",
       plot = plot.sd.MCAR.men.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# AD statistics from the truth of these individuals in transmission clusters ---------------------------


# Cov 35

## Vector

# Mean

vector.mean.MCAR.cov.35.AD.women.true.cl.15.25 <- AD.MCAR.cov.35[,19]
vector.mean.MCAR.cov.35.AD.men.true.cl.15.25 <- AD.MCAR.cov.35[,20]

vector.mean.MCAR.cov.35.AD.women.true.cl.25.40 <- AD.MCAR.cov.35[,21]
vector.mean.MCAR.cov.35.AD.men.true.cl.25.40 <- AD.MCAR.cov.35[,22]

vector.mean.MCAR.cov.35.AD.women.true.cl.40.50 <- AD.MCAR.cov.35[,23]
vector.mean.MCAR.cov.35.AD.men.true.cl.40.50 <- AD.MCAR.cov.35[,24]

# Median

vector.med.MCAR.cov.35.AD.women.true.cl.15.25 <- AD.MCAR.cov.35[,25]
vector.med.MCAR.cov.35.AD.men.true.cl.15.25 <- AD.MCAR.cov.35[,26]

vector.med.MCAR.cov.35.AD.women.true.cl.25.40 <- AD.MCAR.cov.35[,27]
vector.med.MCAR.cov.35.AD.men.true.cl.25.40 <- AD.MCAR.cov.35[,28]

vector.med.MCAR.cov.35.AD.women.true.cl.40.50 <- AD.MCAR.cov.35[,29]
vector.med.MCAR.cov.35.AD.men.true.cl.40.50 <- AD.MCAR.cov.35[,30]

# Standard deviation

vector.sd.MCAR.cov.35.AD.women.true.cl.15.25 <- AD.MCAR.cov.35[,31]
vector.sd.MCAR.cov.35.AD.men.true.cl.15.25 <- AD.MCAR.cov.35[,32]

vector.sd.MCAR.cov.35.AD.women.true.cl.25.40 <- AD.MCAR.cov.35[,33]
vector.sd.MCAR.cov.35.AD.men.true.cl.25.40 <- AD.MCAR.cov.35[,34]

vector.sd.MCAR.cov.35.AD.women.true.cl.40.50 <- AD.MCAR.cov.35[,35]
vector.sd.MCAR.cov.35.AD.men.true.cl.40.50 <- AD.MCAR.cov.35[,36]


## Summarised

# Mean

mean.MCAR.cov.35.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.35[,19])
mean.MCAR.cov.35.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.35[,20])

mean.MCAR.cov.35.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.35[,21])
mean.MCAR.cov.35.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.35[,22])

mean.MCAR.cov.35.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.35[,23])
mean.MCAR.cov.35.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.35[,24])

# Median

med.MCAR.cov.35.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.35[,25])
med.MCAR.cov.35.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.35[,26])

med.MCAR.cov.35.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.35[,27])
med.MCAR.cov.35.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.35[,28])

med.MCAR.cov.35.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.35[,29])
med.MCAR.cov.35.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.35[,30])

# Standard deviation

sd.MCAR.cov.35.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.35[,31])
sd.MCAR.cov.35.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.35[,32])

sd.MCAR.cov.35.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.35[,33])
sd.MCAR.cov.35.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.35[,34])

sd.MCAR.cov.35.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.35[,35])
sd.MCAR.cov.35.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.35[,36])


# Cov 40


## Vector

# Mean

vector.mean.MCAR.cov.40.AD.women.true.cl.15.25 <- AD.MCAR.cov.40[,19]
vector.mean.MCAR.cov.40.AD.men.true.cl.15.25 <- AD.MCAR.cov.40[,20]

vector.mean.MCAR.cov.40.AD.women.true.cl.25.40 <- AD.MCAR.cov.40[,21]
vector.mean.MCAR.cov.40.AD.men.true.cl.25.40 <- AD.MCAR.cov.40[,22]

vector.mean.MCAR.cov.40.AD.women.true.cl.40.50 <- AD.MCAR.cov.40[,23]
vector.mean.MCAR.cov.40.AD.men.true.cl.40.50 <- AD.MCAR.cov.40[,24]

# Median

vector.med.MCAR.cov.40.AD.women.true.cl.15.25 <- AD.MCAR.cov.40[,25]
vector.med.MCAR.cov.40.AD.men.true.cl.15.25 <- AD.MCAR.cov.40[,26]

vector.med.MCAR.cov.40.AD.women.true.cl.25.40 <- AD.MCAR.cov.40[,27]
vector.med.MCAR.cov.40.AD.men.true.cl.25.40 <- AD.MCAR.cov.40[,28]

vector.med.MCAR.cov.40.AD.women.true.cl.40.50 <- AD.MCAR.cov.40[,29]
vector.med.MCAR.cov.40.AD.men.true.cl.40.50 <- AD.MCAR.cov.40[,30]

# Standard deviation

vector.sd.MCAR.cov.40.AD.women.true.cl.15.25 <- AD.MCAR.cov.40[,31]
vector.sd.MCAR.cov.40.AD.men.true.cl.15.25 <- AD.MCAR.cov.40[,32]

vector.sd.MCAR.cov.40.AD.women.true.cl.25.40 <- AD.MCAR.cov.40[,33]
vector.sd.MCAR.cov.40.AD.men.true.cl.25.40 <- AD.MCAR.cov.40[,34]

vector.sd.MCAR.cov.40.AD.women.true.cl.40.50 <- AD.MCAR.cov.40[,35]
vector.sd.MCAR.cov.40.AD.men.true.cl.40.50 <- AD.MCAR.cov.40[,36]



## Summarised


# Mean

mean.MCAR.cov.40.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.40[,19])
mean.MCAR.cov.40.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.40[,20])

mean.MCAR.cov.40.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.40[,21])
mean.MCAR.cov.40.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.40[,22])

mean.MCAR.cov.40.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.40[,23])
mean.MCAR.cov.40.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.40[,24])

# Median

med.MCAR.cov.40.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.40[,25])
med.MCAR.cov.40.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.40[,26])

med.MCAR.cov.40.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.40[,27])
med.MCAR.cov.40.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.40[,28])

med.MCAR.cov.40.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.40[,29])
med.MCAR.cov.40.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.40[,30])

# Standard deviation

sd.MCAR.cov.40.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.40[,31])
sd.MCAR.cov.40.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.40[,32])

sd.MCAR.cov.40.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.40[,33])
sd.MCAR.cov.40.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.40[,34])

sd.MCAR.cov.40.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.40[,35])
sd.MCAR.cov.40.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.40[,36])



# Cov 45



# Vector


# Mean

vector.mean.MCAR.cov.45.AD.women.true.cl.15.25 <- AD.MCAR.cov.45[,19]
vector.mean.MCAR.cov.45.AD.men.true.cl.15.25 <- AD.MCAR.cov.45[,20]

vector.mean.MCAR.cov.45.AD.women.true.cl.25.40 <- AD.MCAR.cov.45[,21]
vector.mean.MCAR.cov.45.AD.men.true.cl.25.40 <- AD.MCAR.cov.45[,22]

vector.mean.MCAR.cov.45.AD.women.true.cl.40.50 <- AD.MCAR.cov.45[,23]
vector.mean.MCAR.cov.45.AD.men.true.cl.40.50 <- AD.MCAR.cov.45[,24]

# Median

vector.med.MCAR.cov.45.AD.women.true.cl.15.25 <- AD.MCAR.cov.45[,25]
vector.med.MCAR.cov.45.AD.men.true.cl.15.25 <- AD.MCAR.cov.45[,26]

vector.med.MCAR.cov.45.AD.women.true.cl.25.40 <- AD.MCAR.cov.45[,27]
vector.med.MCAR.cov.45.AD.men.true.cl.25.40 <- AD.MCAR.cov.45[,28]

vector.med.MCAR.cov.45.AD.women.true.cl.40.50 <- AD.MCAR.cov.45[,29]
vector.med.MCAR.cov.45.AD.men.true.cl.40.50 <- AD.MCAR.cov.45[,30]

# Standard deviation

vector.sd.MCAR.cov.45.AD.women.true.cl.15.25 <- AD.MCAR.cov.45[,31]
vector.sd.MCAR.cov.45.AD.men.true.cl.15.25 <- AD.MCAR.cov.45[,32]

vector.sd.MCAR.cov.45.AD.women.true.cl.25.40 <- AD.MCAR.cov.45[,33]
vector.sd.MCAR.cov.45.AD.men.true.cl.25.40 <- AD.MCAR.cov.45[,34]

vector.sd.MCAR.cov.45.AD.women.true.cl.40.50 <- AD.MCAR.cov.45[,35]
vector.sd.MCAR.cov.45.AD.men.true.cl.40.50 <- AD.MCAR.cov.45[,36]



## Summarised


# Mean

mean.MCAR.cov.45.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.45[,19])
mean.MCAR.cov.45.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.45[,20])

mean.MCAR.cov.45.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.45[,21])
mean.MCAR.cov.45.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.45[,22])

mean.MCAR.cov.45.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.45[,23])
mean.MCAR.cov.45.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.45[,24])

# Median

med.MCAR.cov.45.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.45[,25])
med.MCAR.cov.45.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.45[,26])

med.MCAR.cov.45.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.45[,27])
med.MCAR.cov.45.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.45[,28])

med.MCAR.cov.45.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.45[,29])
med.MCAR.cov.45.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.45[,30])

# Standard deviation

sd.MCAR.cov.45.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.45[,31])
sd.MCAR.cov.45.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.45[,32])

sd.MCAR.cov.45.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.45[,33])
sd.MCAR.cov.45.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.45[,34])

sd.MCAR.cov.45.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.45[,35])
sd.MCAR.cov.45.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.45[,36])


# Cov 50


## Vector


# Mean

vector.mean.MCAR.cov.50.AD.women.true.cl.15.25 <- AD.MCAR.cov.50[,19]
vector.mean.MCAR.cov.50.AD.men.true.cl.15.25 <- AD.MCAR.cov.50[,20]

vector.mean.MCAR.cov.50.AD.women.true.cl.25.40 <- AD.MCAR.cov.50[,21]
vector.mean.MCAR.cov.50.AD.men.true.cl.25.40 <- AD.MCAR.cov.50[,22]

vector.mean.MCAR.cov.50.AD.women.true.cl.40.50 <- AD.MCAR.cov.50[,23]
vector.mean.MCAR.cov.50.AD.men.true.cl.40.50 <- AD.MCAR.cov.50[,24]

# Median

vector.med.MCAR.cov.50.AD.women.true.cl.15.25 <- AD.MCAR.cov.50[,25]
vector.med.MCAR.cov.50.AD.men.true.cl.15.25 <- AD.MCAR.cov.50[,26]

vector.med.MCAR.cov.50.AD.women.true.cl.25.40 <- AD.MCAR.cov.50[,27]
vector.med.MCAR.cov.50.AD.men.true.cl.25.40 <- AD.MCAR.cov.50[,28]

vector.med.MCAR.cov.50.AD.women.true.cl.40.50 <- AD.MCAR.cov.50[,29]
vector.med.MCAR.cov.50.AD.men.true.cl.40.50 <- AD.MCAR.cov.50[,30]

# Standard deviation

vector.sd.MCAR.cov.50.AD.women.true.cl.15.25 <- AD.MCAR.cov.50[,31]
vector.sd.MCAR.cov.50.AD.men.true.cl.15.25 <- AD.MCAR.cov.50[,32]

vector.sd.MCAR.cov.50.AD.women.true.cl.25.40 <- AD.MCAR.cov.50[,33]
vector.sd.MCAR.cov.50.AD.men.true.cl.25.40 <- AD.MCAR.cov.50[,34]

vector.sd.MCAR.cov.50.AD.women.true.cl.40.50 <- AD.MCAR.cov.50[,35]
vector.sd.MCAR.cov.50.AD.men.true.cl.40.50 <- AD.MCAR.cov.50[,36]



## Summarised


# Mean

mean.MCAR.cov.50.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.50[,19])
mean.MCAR.cov.50.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.50[,20])

mean.MCAR.cov.50.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.50[,21])
mean.MCAR.cov.50.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.50[,22])

mean.MCAR.cov.50.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.50[,23])
mean.MCAR.cov.50.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.50[,24])

# Median

med.MCAR.cov.50.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.50[,25])
med.MCAR.cov.50.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.50[,26])

med.MCAR.cov.50.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.50[,27])
med.MCAR.cov.50.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.50[,28])

med.MCAR.cov.50.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.50[,29])
med.MCAR.cov.50.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.50[,30])

# Standard deviation

sd.MCAR.cov.50.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.50[,31])
sd.MCAR.cov.50.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.50[,32])

sd.MCAR.cov.50.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.50[,33])
sd.MCAR.cov.50.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.50[,34])

sd.MCAR.cov.50.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.50[,35])
sd.MCAR.cov.50.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.50[,36])



# Cov 55



## Vector


# Mean

vector.mean.MCAR.cov.55.AD.women.true.cl.15.25 <- AD.MCAR.cov.55[,19]
vector.mean.MCAR.cov.55.AD.men.true.cl.15.25 <- AD.MCAR.cov.55[,20]

vector.mean.MCAR.cov.55.AD.women.true.cl.25.40 <- AD.MCAR.cov.55[,21]
vector.mean.MCAR.cov.55.AD.men.true.cl.25.40 <- AD.MCAR.cov.55[,22]

vector.mean.MCAR.cov.55.AD.women.true.cl.40.50 <- AD.MCAR.cov.55[,23]
vector.mean.MCAR.cov.55.AD.men.true.cl.40.50 <- AD.MCAR.cov.55[,24]

# Median

vector.med.MCAR.cov.55.AD.women.true.cl.15.25 <- AD.MCAR.cov.55[,25]
vector.med.MCAR.cov.55.AD.men.true.cl.15.25 <- AD.MCAR.cov.55[,26]

vector.med.MCAR.cov.55.AD.women.true.cl.25.40 <- AD.MCAR.cov.55[,27]
vector.med.MCAR.cov.55.AD.men.true.cl.25.40 <- AD.MCAR.cov.55[,28]

vector.med.MCAR.cov.55.AD.women.true.cl.40.50 <- AD.MCAR.cov.55[,29]
vector.med.MCAR.cov.55.AD.men.true.cl.40.50 <- AD.MCAR.cov.55[,30]

# Standard deviation

vector.sd.MCAR.cov.55.AD.women.true.cl.15.25 <- AD.MCAR.cov.55[,31]
vector.sd.MCAR.cov.55.AD.men.true.cl.15.25 <- AD.MCAR.cov.55[,32]

vector.sd.MCAR.cov.55.AD.women.true.cl.25.40 <- AD.MCAR.cov.55[,33]
vector.sd.MCAR.cov.55.AD.men.true.cl.25.40 <- AD.MCAR.cov.55[,34]

vector.sd.MCAR.cov.55.AD.women.true.cl.40.50 <- AD.MCAR.cov.55[,35]
vector.sd.MCAR.cov.55.AD.men.true.cl.40.50 <- AD.MCAR.cov.55[,36]



## Summarised


# Mean

mean.MCAR.cov.55.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.55[,19])
mean.MCAR.cov.55.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.55[,20])

mean.MCAR.cov.55.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.55[,21])
mean.MCAR.cov.55.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.55[,22])

mean.MCAR.cov.55.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.55[,23])
mean.MCAR.cov.55.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.55[,24])

# Median

med.MCAR.cov.55.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.55[,25])
med.MCAR.cov.55.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.55[,26])

med.MCAR.cov.55.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.55[,27])
med.MCAR.cov.55.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.55[,28])

med.MCAR.cov.55.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.55[,29])
med.MCAR.cov.55.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.55[,30])

# Standard deviation

sd.MCAR.cov.55.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.55[,31])
sd.MCAR.cov.55.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.55[,32])

sd.MCAR.cov.55.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.55[,33])
sd.MCAR.cov.55.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.55[,34])

sd.MCAR.cov.55.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.55[,35])
sd.MCAR.cov.55.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.55[,36])



# Cov 60


## Vector

# Mean

vector.mean.MCAR.cov.60.AD.women.true.cl.15.25 <- AD.MCAR.cov.60[,19]
vector.mean.MCAR.cov.60.AD.men.true.cl.15.25 <- AD.MCAR.cov.60[,20]

vector.mean.MCAR.cov.60.AD.women.true.cl.25.40 <- AD.MCAR.cov.60[,21]
vector.mean.MCAR.cov.60.AD.men.true.cl.25.40 <- AD.MCAR.cov.60[,22]

vector.mean.MCAR.cov.60.AD.women.true.cl.40.50 <- AD.MCAR.cov.60[,23]
vector.mean.MCAR.cov.60.AD.men.true.cl.40.50 <- AD.MCAR.cov.60[,24]

# Median

vector.med.MCAR.cov.60.AD.women.true.cl.15.25 <- AD.MCAR.cov.60[,25]
vector.med.MCAR.cov.60.AD.men.true.cl.15.25 <- AD.MCAR.cov.60[,26]

vector.med.MCAR.cov.60.AD.women.true.cl.25.40 <- AD.MCAR.cov.60[,27]
vector.med.MCAR.cov.60.AD.men.true.cl.25.40 <- AD.MCAR.cov.60[,28]

vector.med.MCAR.cov.60.AD.women.true.cl.40.50 <- AD.MCAR.cov.60[,29]
vector.med.MCAR.cov.60.AD.men.true.cl.40.50 <- AD.MCAR.cov.60[,30]

# Standard deviation

vector.sd.MCAR.cov.60.AD.women.true.cl.15.25 <- AD.MCAR.cov.60[,31]
vector.sd.MCAR.cov.60.AD.men.true.cl.15.25 <- AD.MCAR.cov.60[,32]

vector.sd.MCAR.cov.60.AD.women.true.cl.25.40 <- AD.MCAR.cov.60[,33]
vector.sd.MCAR.cov.60.AD.men.true.cl.25.40 <- AD.MCAR.cov.60[,34]

vector.sd.MCAR.cov.60.AD.women.true.cl.40.50 <- AD.MCAR.cov.60[,35]
vector.sd.MCAR.cov.60.AD.men.true.cl.40.50 <- AD.MCAR.cov.60[,36]



## Summarised


# Mean

mean.MCAR.cov.60.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.60[,19])
mean.MCAR.cov.60.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.60[,20])

mean.MCAR.cov.60.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.60[,21])
mean.MCAR.cov.60.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.60[,22])

mean.MCAR.cov.60.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.60[,23])
mean.MCAR.cov.60.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.60[,24])

# Median

med.MCAR.cov.60.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.60[,25])
med.MCAR.cov.60.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.60[,26])

med.MCAR.cov.60.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.60[,27])
med.MCAR.cov.60.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.60[,28])

med.MCAR.cov.60.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.60[,29])
med.MCAR.cov.60.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.60[,30])

# Standard deviation

sd.MCAR.cov.60.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.60[,31])
sd.MCAR.cov.60.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.60[,32])

sd.MCAR.cov.60.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.60[,33])
sd.MCAR.cov.60.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.60[,34])

sd.MCAR.cov.60.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.60[,35])
sd.MCAR.cov.60.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.60[,36])



# Cov 65



## Vector

# Mean

vector.mean.MCAR.cov.65.AD.women.true.cl.15.25 <- AD.MCAR.cov.65[,19]
vector.mean.MCAR.cov.65.AD.men.true.cl.15.25 <- AD.MCAR.cov.65[,20]

vector.mean.MCAR.cov.65.AD.women.true.cl.25.40 <- AD.MCAR.cov.65[,21]
vector.mean.MCAR.cov.65.AD.men.true.cl.25.40 <- AD.MCAR.cov.65[,22]

vector.mean.MCAR.cov.65.AD.women.true.cl.40.50 <- AD.MCAR.cov.65[,23]
vector.mean.MCAR.cov.65.AD.men.true.cl.40.50 <- AD.MCAR.cov.65[,24]

# Median

vector.med.MCAR.cov.65.AD.women.true.cl.15.25 <- AD.MCAR.cov.65[,25]
vector.med.MCAR.cov.65.AD.men.true.cl.15.25 <- AD.MCAR.cov.65[,26]

vector.med.MCAR.cov.65.AD.women.true.cl.25.40 <- AD.MCAR.cov.65[,27]
vector.med.MCAR.cov.65.AD.men.true.cl.25.40 <- AD.MCAR.cov.65[,28]

vector.med.MCAR.cov.65.AD.women.true.cl.40.50 <- AD.MCAR.cov.65[,29]
vector.med.MCAR.cov.65.AD.men.true.cl.40.50 <- AD.MCAR.cov.65[,30]

# Standard deviation

vector.sd.MCAR.cov.65.AD.women.true.cl.15.25 <- AD.MCAR.cov.65[,31]
vector.sd.MCAR.cov.65.AD.men.true.cl.15.25 <- AD.MCAR.cov.65[,32]

vector.sd.MCAR.cov.65.AD.women.true.cl.25.40 <- AD.MCAR.cov.65[,33]
vector.sd.MCAR.cov.65.AD.men.true.cl.25.40 <- AD.MCAR.cov.65[,34]

vector.sd.MCAR.cov.65.AD.women.true.cl.40.50 <- AD.MCAR.cov.65[,35]
vector.sd.MCAR.cov.65.AD.men.true.cl.40.50 <- AD.MCAR.cov.65[,36]



## Summarised


# Mean

mean.MCAR.cov.65.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.65[,19])
mean.MCAR.cov.65.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.65[,20])

mean.MCAR.cov.65.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.65[,21])
mean.MCAR.cov.65.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.65[,22])

mean.MCAR.cov.65.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.65[,23])
mean.MCAR.cov.65.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.65[,24])

# Median

med.MCAR.cov.65.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.65[,25])
med.MCAR.cov.65.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.65[,26])

med.MCAR.cov.65.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.65[,27])
med.MCAR.cov.65.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.65[,28])

med.MCAR.cov.65.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.65[,29])
med.MCAR.cov.65.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.65[,30])

# Standard deviation

sd.MCAR.cov.65.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.65[,31])
sd.MCAR.cov.65.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.65[,32])

sd.MCAR.cov.65.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.65[,33])
sd.MCAR.cov.65.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.65[,34])

sd.MCAR.cov.65.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.65[,35])
sd.MCAR.cov.65.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.65[,36])



# Cov 70



## Vector


# Mean

vector.mean.MCAR.cov.70.AD.women.true.cl.15.25 <- AD.MCAR.cov.70[,19]
vector.mean.MCAR.cov.70.AD.men.true.cl.15.25 <- AD.MCAR.cov.70[,20]

vector.mean.MCAR.cov.70.AD.women.true.cl.25.40 <- AD.MCAR.cov.70[,21]
vector.mean.MCAR.cov.70.AD.men.true.cl.25.40 <- AD.MCAR.cov.70[,22]

vector.mean.MCAR.cov.70.AD.women.true.cl.40.50 <- AD.MCAR.cov.70[,23]
vector.mean.MCAR.cov.70.AD.men.true.cl.40.50 <- AD.MCAR.cov.70[,24]

# Median

vector.med.MCAR.cov.70.AD.women.true.cl.15.25 <- AD.MCAR.cov.70[,25]
vector.med.MCAR.cov.70.AD.men.true.cl.15.25 <- AD.MCAR.cov.70[,26]

vector.med.MCAR.cov.70.AD.women.true.cl.25.40 <- AD.MCAR.cov.70[,27]
vector.med.MCAR.cov.70.AD.men.true.cl.25.40 <- AD.MCAR.cov.70[,28]

vector.med.MCAR.cov.70.AD.women.true.cl.40.50 <- AD.MCAR.cov.70[,29]
vector.med.MCAR.cov.70.AD.men.true.cl.40.50 <- AD.MCAR.cov.70[,30]

# Standard deviation

vector.sd.MCAR.cov.70.AD.women.true.cl.15.25 <- AD.MCAR.cov.70[,31]
vector.sd.MCAR.cov.70.AD.men.true.cl.15.25 <- AD.MCAR.cov.70[,32]

vector.sd.MCAR.cov.70.AD.women.true.cl.25.40 <- AD.MCAR.cov.70[,33]
vector.sd.MCAR.cov.70.AD.men.true.cl.25.40 <- AD.MCAR.cov.70[,34]

vector.sd.MCAR.cov.70.AD.women.true.cl.40.50 <- AD.MCAR.cov.70[,35]
vector.sd.MCAR.cov.70.AD.men.true.cl.40.50 <- AD.MCAR.cov.70[,36]




## Summarised


# Mean

mean.MCAR.cov.70.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.70[,19])
mean.MCAR.cov.70.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.70[,20])

mean.MCAR.cov.70.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.70[,21])
mean.MCAR.cov.70.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.70[,22])

mean.MCAR.cov.70.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.70[,23])
mean.MCAR.cov.70.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.70[,24])

# Median

med.MCAR.cov.70.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.70[,25])
med.MCAR.cov.70.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.70[,26])

med.MCAR.cov.70.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.70[,27])
med.MCAR.cov.70.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.70[,28])

med.MCAR.cov.70.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.70[,29])
med.MCAR.cov.70.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.70[,30])

# Standard deviation

sd.MCAR.cov.70.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.70[,31])
sd.MCAR.cov.70.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.70[,32])

sd.MCAR.cov.70.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.70[,33])
sd.MCAR.cov.70.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.70[,34])

sd.MCAR.cov.70.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.70[,35])
sd.MCAR.cov.70.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.70[,36])



# Cov 75


## Vector


# Mean

vector.mean.MCAR.cov.75.AD.women.true.cl.15.25 <- AD.MCAR.cov.75[,19]
vector.mean.MCAR.cov.75.AD.men.true.cl.15.25 <- AD.MCAR.cov.75[,20]

vector.mean.MCAR.cov.75.AD.women.true.cl.25.40 <- AD.MCAR.cov.75[,21]
vector.mean.MCAR.cov.75.AD.men.true.cl.25.40 <- AD.MCAR.cov.75[,22]

vector.mean.MCAR.cov.75.AD.women.true.cl.40.50 <- AD.MCAR.cov.75[,23]
vector.mean.MCAR.cov.75.AD.men.true.cl.40.50 <- AD.MCAR.cov.75[,24]

# Median

vector.med.MCAR.cov.75.AD.women.true.cl.15.25 <- AD.MCAR.cov.75[,25]
vector.med.MCAR.cov.75.AD.men.true.cl.15.25 <- AD.MCAR.cov.75[,26]

vector.med.MCAR.cov.75.AD.women.true.cl.25.40 <- AD.MCAR.cov.75[,27]
vector.med.MCAR.cov.75.AD.men.true.cl.25.40 <- AD.MCAR.cov.75[,28]

vector.med.MCAR.cov.75.AD.women.true.cl.40.50 <- AD.MCAR.cov.75[,29]
vector.med.MCAR.cov.75.AD.men.true.cl.40.50 <- AD.MCAR.cov.75[,30]

# Standard deviation

vector.sd.MCAR.cov.75.AD.women.true.cl.15.25 <- AD.MCAR.cov.75[,31]
vector.sd.MCAR.cov.75.AD.men.true.cl.15.25 <- AD.MCAR.cov.75[,32]

vector.sd.MCAR.cov.75.AD.women.true.cl.25.40 <- AD.MCAR.cov.75[,33]
vector.sd.MCAR.cov.75.AD.men.true.cl.25.40 <- AD.MCAR.cov.75[,34]

vector.sd.MCAR.cov.75.AD.women.true.cl.40.50 <- AD.MCAR.cov.75[,35]
vector.sd.MCAR.cov.75.AD.men.true.cl.40.50 <- AD.MCAR.cov.75[,36]


## Summarised


# Mean

mean.MCAR.cov.75.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.75[,19])
mean.MCAR.cov.75.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.75[,20])

mean.MCAR.cov.75.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.75[,21])
mean.MCAR.cov.75.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.75[,22])

mean.MCAR.cov.75.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.75[,23])
mean.MCAR.cov.75.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.75[,24])

# Median

med.MCAR.cov.75.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.75[,25])
med.MCAR.cov.75.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.75[,26])

med.MCAR.cov.75.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.75[,27])
med.MCAR.cov.75.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.75[,28])

med.MCAR.cov.75.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.75[,29])
med.MCAR.cov.75.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.75[,30])

# Standard deviation

sd.MCAR.cov.75.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.75[,31])
sd.MCAR.cov.75.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.75[,32])

sd.MCAR.cov.75.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.75[,33])
sd.MCAR.cov.75.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.75[,34])

sd.MCAR.cov.75.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.75[,35])
sd.MCAR.cov.75.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.75[,36])



# Cov 80


## Vector

# Mean

vector.mean.MCAR.cov.80.AD.women.true.cl.15.25 <- AD.MCAR.cov.80[,19]
vector.mean.MCAR.cov.80.AD.men.true.cl.15.25 <- AD.MCAR.cov.80[,20]

vector.mean.MCAR.cov.80.AD.women.true.cl.25.40 <- AD.MCAR.cov.80[,21]
vector.mean.MCAR.cov.80.AD.men.true.cl.25.40 <- AD.MCAR.cov.80[,22]

vector.mean.MCAR.cov.80.AD.women.true.cl.40.50 <- AD.MCAR.cov.80[,23]
vector.mean.MCAR.cov.80.AD.men.true.cl.40.50 <- AD.MCAR.cov.80[,24]

# Median

vector.med.MCAR.cov.80.AD.women.true.cl.15.25 <- AD.MCAR.cov.80[,25]
vector.med.MCAR.cov.80.AD.men.true.cl.15.25 <- AD.MCAR.cov.80[,26]

vector.med.MCAR.cov.80.AD.women.true.cl.25.40 <- AD.MCAR.cov.80[,27]
vector.med.MCAR.cov.80.AD.men.true.cl.25.40 <- AD.MCAR.cov.80[,28]

vector.med.MCAR.cov.80.AD.women.true.cl.40.50 <- AD.MCAR.cov.80[,29]
vector.med.MCAR.cov.80.AD.men.true.cl.40.50 <- AD.MCAR.cov.80[,30]

# Standard deviation

vector.sd.MCAR.cov.80.AD.women.true.cl.15.25 <- AD.MCAR.cov.80[,31]
vector.sd.MCAR.cov.80.AD.men.true.cl.15.25 <- AD.MCAR.cov.80[,32]

vector.sd.MCAR.cov.80.AD.women.true.cl.25.40 <- AD.MCAR.cov.80[,33]
vector.sd.MCAR.cov.80.AD.men.true.cl.25.40 <- AD.MCAR.cov.80[,34]

vector.sd.MCAR.cov.80.AD.women.true.cl.40.50 <- AD.MCAR.cov.80[,35]
vector.sd.MCAR.cov.80.AD.men.true.cl.40.50 <- AD.MCAR.cov.80[,36]


## Summarised


# Mean

mean.MCAR.cov.80.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.80[,19])
mean.MCAR.cov.80.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.80[,20])

mean.MCAR.cov.80.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.80[,21])
mean.MCAR.cov.80.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.80[,22])

mean.MCAR.cov.80.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.80[,23])
mean.MCAR.cov.80.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.80[,24])

# Median

med.MCAR.cov.80.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.80[,25])
med.MCAR.cov.80.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.80[,26])

med.MCAR.cov.80.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.80[,27])
med.MCAR.cov.80.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.80[,28])

med.MCAR.cov.80.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.80[,29])
med.MCAR.cov.80.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.80[,30])

# Standard deviation

sd.MCAR.cov.80.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.80[,31])
sd.MCAR.cov.80.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.80[,32])

sd.MCAR.cov.80.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.80[,33])
sd.MCAR.cov.80.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.80[,34])

sd.MCAR.cov.80.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.80[,35])
sd.MCAR.cov.80.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.80[,36])



# Cov 85


## Vector

# Mean

vector.mean.MCAR.cov.85.AD.women.true.cl.15.25 <- AD.MCAR.cov.85[,19]
vector.mean.MCAR.cov.85.AD.men.true.cl.15.25 <- AD.MCAR.cov.85[,20]

vector.mean.MCAR.cov.85.AD.women.true.cl.25.40 <- AD.MCAR.cov.85[,21]
vector.mean.MCAR.cov.85.AD.men.true.cl.25.40 <- AD.MCAR.cov.85[,22]

vector.mean.MCAR.cov.85.AD.women.true.cl.40.50 <- AD.MCAR.cov.85[,23]
vector.mean.MCAR.cov.85.AD.men.true.cl.40.50 <- AD.MCAR.cov.85[,24]

# Median

vector.med.MCAR.cov.85.AD.women.true.cl.15.25 <- AD.MCAR.cov.85[,25]
vector.med.MCAR.cov.85.AD.men.true.cl.15.25 <- AD.MCAR.cov.85[,26]

vector.med.MCAR.cov.85.AD.women.true.cl.25.40 <- AD.MCAR.cov.85[,27]
vector.med.MCAR.cov.85.AD.men.true.cl.25.40 <- AD.MCAR.cov.85[,28]

vector.med.MCAR.cov.85.AD.women.true.cl.40.50 <- AD.MCAR.cov.85[,29]
vector.med.MCAR.cov.85.AD.men.true.cl.40.50 <- AD.MCAR.cov.85[,30]

# Standard deviation

vector.sd.MCAR.cov.85.AD.women.true.cl.15.25 <- AD.MCAR.cov.85[,31]
vector.sd.MCAR.cov.85.AD.men.true.cl.15.25 <- AD.MCAR.cov.85[,32]

vector.sd.MCAR.cov.85.AD.women.true.cl.25.40 <- AD.MCAR.cov.85[,33]
vector.sd.MCAR.cov.85.AD.men.true.cl.25.40 <- AD.MCAR.cov.85[,34]

vector.sd.MCAR.cov.85.AD.women.true.cl.40.50 <- AD.MCAR.cov.85[,35]
vector.sd.MCAR.cov.85.AD.men.true.cl.40.50 <- AD.MCAR.cov.85[,36]


## Summarised


# Mean

mean.MCAR.cov.85.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.85[,19])
mean.MCAR.cov.85.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.85[,20])

mean.MCAR.cov.85.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.85[,21])
mean.MCAR.cov.85.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.85[,22])

mean.MCAR.cov.85.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.85[,23])
mean.MCAR.cov.85.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.85[,24])

# Median

med.MCAR.cov.85.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.85[,25])
med.MCAR.cov.85.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.85[,26])

med.MCAR.cov.85.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.85[,27])
med.MCAR.cov.85.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.85[,28])

med.MCAR.cov.85.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.85[,29])
med.MCAR.cov.85.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.85[,30])

# Standard deviation

sd.MCAR.cov.85.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.85[,31])
sd.MCAR.cov.85.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.85[,32])

sd.MCAR.cov.85.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.85[,33])
sd.MCAR.cov.85.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.85[,34])

sd.MCAR.cov.85.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.85[,35])
sd.MCAR.cov.85.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.85[,36])



# Cov 90


## Vector

# Mean

vector.mean.MCAR.cov.90.AD.women.true.cl.15.25 <- AD.MCAR.cov.90[,19]
vector.mean.MCAR.cov.90.AD.men.true.cl.15.25 <- AD.MCAR.cov.90[,20]

vector.mean.MCAR.cov.90.AD.women.true.cl.25.40 <- AD.MCAR.cov.90[,21]
vector.mean.MCAR.cov.90.AD.men.true.cl.25.40 <- AD.MCAR.cov.90[,22]

vector.mean.MCAR.cov.90.AD.women.true.cl.40.50 <- AD.MCAR.cov.90[,23]
vector.mean.MCAR.cov.90.AD.men.true.cl.40.50 <- AD.MCAR.cov.90[,24]

# Median

vector.med.MCAR.cov.90.AD.women.true.cl.15.25 <- AD.MCAR.cov.90[,25]
vector.med.MCAR.cov.90.AD.men.true.cl.15.25 <- AD.MCAR.cov.90[,26]

vector.med.MCAR.cov.90.AD.women.true.cl.25.40 <- AD.MCAR.cov.90[,27]
vector.med.MCAR.cov.90.AD.men.true.cl.25.40 <- AD.MCAR.cov.90[,28]

vector.med.MCAR.cov.90.AD.women.true.cl.40.50 <- AD.MCAR.cov.90[,29]
vector.med.MCAR.cov.90.AD.men.true.cl.40.50 <- AD.MCAR.cov.90[,30]

# Standard deviation

vector.sd.MCAR.cov.90.AD.women.true.cl.15.25 <- AD.MCAR.cov.90[,31]
vector.sd.MCAR.cov.90.AD.men.true.cl.15.25 <- AD.MCAR.cov.90[,32]

vector.sd.MCAR.cov.90.AD.women.true.cl.25.40 <- AD.MCAR.cov.90[,33]
vector.sd.MCAR.cov.90.AD.men.true.cl.25.40 <- AD.MCAR.cov.90[,34]

vector.sd.MCAR.cov.90.AD.women.true.cl.40.50 <- AD.MCAR.cov.90[,35]
vector.sd.MCAR.cov.90.AD.men.true.cl.40.50 <- AD.MCAR.cov.90[,36]



## Summarised


# Mean

mean.MCAR.cov.90.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.90[,19])
mean.MCAR.cov.90.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.90[,20])

mean.MCAR.cov.90.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.90[,21])
mean.MCAR.cov.90.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.90[,22])

mean.MCAR.cov.90.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.90[,23])
mean.MCAR.cov.90.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.90[,24])

# Median

med.MCAR.cov.90.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.90[,25])
med.MCAR.cov.90.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.90[,26])

med.MCAR.cov.90.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.90[,27])
med.MCAR.cov.90.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.90[,28])

med.MCAR.cov.90.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.90[,29])
med.MCAR.cov.90.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.90[,30])

# Standard deviation

sd.MCAR.cov.90.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.90[,31])
sd.MCAR.cov.90.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.90[,32])

sd.MCAR.cov.90.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.90[,33])
sd.MCAR.cov.90.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.90[,34])

sd.MCAR.cov.90.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.90[,35])
sd.MCAR.cov.90.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.90[,36])




# Cov 95


## Vector

# Mean

vector.mean.MCAR.cov.95.AD.women.true.cl.15.25 <- AD.MCAR.cov.95[,19]
vector.mean.MCAR.cov.95.AD.men.true.cl.15.25 <- AD.MCAR.cov.95[,20]

vector.mean.MCAR.cov.95.AD.women.true.cl.25.40 <- AD.MCAR.cov.95[,21]
vector.mean.MCAR.cov.95.AD.men.true.cl.25.40 <- AD.MCAR.cov.95[,22]

vector.mean.MCAR.cov.95.AD.women.true.cl.40.50 <- AD.MCAR.cov.95[,23]
vector.mean.MCAR.cov.95.AD.men.true.cl.40.50 <- AD.MCAR.cov.95[,24]

# Median

vector.med.MCAR.cov.95.AD.women.true.cl.15.25 <- AD.MCAR.cov.95[,25]
vector.med.MCAR.cov.95.AD.men.true.cl.15.25 <- AD.MCAR.cov.95[,26]

vector.med.MCAR.cov.95.AD.women.true.cl.25.40 <- AD.MCAR.cov.95[,27]
vector.med.MCAR.cov.95.AD.men.true.cl.25.40 <- AD.MCAR.cov.95[,28]

vector.med.MCAR.cov.95.AD.women.true.cl.40.50 <- AD.MCAR.cov.95[,29]
vector.med.MCAR.cov.95.AD.men.true.cl.40.50 <- AD.MCAR.cov.95[,30]

# Standard deviation

vector.sd.MCAR.cov.95.AD.women.true.cl.15.25 <- AD.MCAR.cov.95[,31]
vector.sd.MCAR.cov.95.AD.men.true.cl.15.25 <- AD.MCAR.cov.95[,32]

vector.sd.MCAR.cov.95.AD.women.true.cl.25.40 <- AD.MCAR.cov.95[,33]
vector.sd.MCAR.cov.95.AD.men.true.cl.25.40 <- AD.MCAR.cov.95[,34]

vector.sd.MCAR.cov.95.AD.women.true.cl.40.50 <- AD.MCAR.cov.95[,35]
vector.sd.MCAR.cov.95.AD.men.true.cl.40.50 <- AD.MCAR.cov.95[,36]


## Summarised

# Mean

mean.MCAR.cov.95.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.95[,19])
mean.MCAR.cov.95.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.95[,20])

mean.MCAR.cov.95.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.95[,21])
mean.MCAR.cov.95.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.95[,22])

mean.MCAR.cov.95.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.95[,23])
mean.MCAR.cov.95.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.95[,24])

# Median

med.MCAR.cov.95.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.95[,25])
med.MCAR.cov.95.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.95[,26])

med.MCAR.cov.95.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.95[,27])
med.MCAR.cov.95.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.95[,28])

med.MCAR.cov.95.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.95[,29])
med.MCAR.cov.95.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.95[,30])

# Standard deviation

sd.MCAR.cov.95.AD.women.true.cl.15.25 <- quant.med(AD.MCAR.cov.95[,31])
sd.MCAR.cov.95.AD.men.true.cl.15.25 <- quant.med(AD.MCAR.cov.95[,32])

sd.MCAR.cov.95.AD.women.true.cl.25.40 <- quant.med(AD.MCAR.cov.95[,33])
sd.MCAR.cov.95.AD.men.true.cl.25.40 <- quant.med(AD.MCAR.cov.95[,34])

sd.MCAR.cov.95.AD.women.true.cl.40.50 <- quant.med(AD.MCAR.cov.95[,35])
sd.MCAR.cov.95.AD.men.true.cl.40.50 <- quant.med(AD.MCAR.cov.95[,36])



# Visualisation of AD statistics from the truth of these individuals in transmission clusters ---------------------------


## Mean



# Women 15.25

mean.MCAR.women.true.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(mean.MCAR.cov.35.AD.women.true.cl.15.25[2], mean.MCAR.cov.40.AD.women.true.cl.15.25[2],
                                                  mean.MCAR.cov.45.AD.women.true.cl.15.25[2], mean.MCAR.cov.50.AD.women.true.cl.15.25[2],
                                                  mean.MCAR.cov.55.AD.women.true.cl.15.25[2], mean.MCAR.cov.60.AD.women.true.cl.15.25[2],
                                                  mean.MCAR.cov.65.AD.women.true.cl.15.25[2], mean.MCAR.cov.70.AD.women.true.cl.15.25[2],
                                                  mean.MCAR.cov.75.AD.women.true.cl.15.25[2], mean.MCAR.cov.80.AD.women.true.cl.15.25[2],
                                                  mean.MCAR.cov.85.AD.women.true.cl.15.25[2], mean.MCAR.cov.90.AD.women.true.cl.15.25[2],
                                                  mean.MCAR.cov.95.AD.women.true.cl.15.25[2], mean.AD.num.women.true.cov.100.15.25[2]),
                                            
                                            L = c(mean.MCAR.cov.35.AD.women.true.cl.15.25[1], mean.MCAR.cov.40.AD.women.true.cl.15.25[1],
                                                  mean.MCAR.cov.45.AD.women.true.cl.15.25[1], mean.MCAR.cov.50.AD.women.true.cl.15.25[1],
                                                  mean.MCAR.cov.55.AD.women.true.cl.15.25[1], mean.MCAR.cov.60.AD.women.true.cl.15.25[1],
                                                  mean.MCAR.cov.65.AD.women.true.cl.15.25[1], mean.MCAR.cov.70.AD.women.true.cl.15.25[1],
                                                  mean.MCAR.cov.75.AD.women.true.cl.15.25[1], mean.MCAR.cov.80.AD.women.true.cl.15.25[1],
                                                  mean.MCAR.cov.85.AD.women.true.cl.15.25[1], mean.MCAR.cov.90.AD.women.true.cl.15.25[1],
                                                  mean.MCAR.cov.95.AD.women.true.cl.15.25[1], mean.AD.num.women.true.cov.100.15.25[1]),
                                            
                                            U = c(mean.MCAR.cov.35.AD.women.true.cl.15.25[3], mean.MCAR.cov.40.AD.women.true.cl.15.25[3],
                                                  mean.MCAR.cov.45.AD.women.true.cl.15.25[3], mean.MCAR.cov.50.AD.women.true.cl.15.25[3],
                                                  mean.MCAR.cov.55.AD.women.true.cl.15.25[3], mean.MCAR.cov.60.AD.women.true.cl.15.25[3],
                                                  mean.MCAR.cov.65.AD.women.true.cl.15.25[3], mean.MCAR.cov.70.AD.women.true.cl.15.25[3],
                                                  mean.MCAR.cov.75.AD.women.true.cl.15.25[3], mean.MCAR.cov.80.AD.women.true.cl.15.25[3],
                                                  mean.MCAR.cov.85.AD.women.true.cl.15.25[3], mean.MCAR.cov.90.AD.women.true.cl.15.25[3],
                                                  mean.MCAR.cov.95.AD.women.true.cl.15.25[3], mean.AD.num.women.true.cov.100.15.25[3]))


plot.mean.MCAR.women.true.cl.15.25 <- ggplot(mean.MCAR.women.true.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.women.true.cl.15.25.png",
       plot = plot.mean.MCAR.women.true.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

mean.MCAR.men.true.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                          
                                          F = c(mean.MCAR.cov.35.AD.men.true.cl.15.25[2], mean.MCAR.cov.40.AD.men.true.cl.15.25[2],
                                                mean.MCAR.cov.45.AD.men.true.cl.15.25[2], mean.MCAR.cov.50.AD.men.true.cl.15.25[2],
                                                mean.MCAR.cov.55.AD.men.true.cl.15.25[2], mean.MCAR.cov.60.AD.men.true.cl.15.25[2],
                                                mean.MCAR.cov.65.AD.men.true.cl.15.25[2], mean.MCAR.cov.70.AD.men.true.cl.15.25[2],
                                                mean.MCAR.cov.75.AD.men.true.cl.15.25[2], mean.MCAR.cov.80.AD.men.true.cl.15.25[2],
                                                mean.MCAR.cov.85.AD.men.true.cl.15.25[2], mean.MCAR.cov.90.AD.men.true.cl.15.25[2],
                                                mean.MCAR.cov.95.AD.men.true.cl.15.25[2], mean.AD.num.men.true.cov.100.15.25[2]),
                                          
                                          L = c(mean.MCAR.cov.35.AD.men.true.cl.15.25[1], mean.MCAR.cov.40.AD.men.true.cl.15.25[1],
                                                mean.MCAR.cov.45.AD.men.true.cl.15.25[1], mean.MCAR.cov.50.AD.men.true.cl.15.25[1],
                                                mean.MCAR.cov.55.AD.men.true.cl.15.25[1], mean.MCAR.cov.60.AD.men.true.cl.15.25[1],
                                                mean.MCAR.cov.65.AD.men.true.cl.15.25[1], mean.MCAR.cov.70.AD.men.true.cl.15.25[1],
                                                mean.MCAR.cov.75.AD.men.true.cl.15.25[1], mean.MCAR.cov.80.AD.men.true.cl.15.25[1],
                                                mean.MCAR.cov.85.AD.men.true.cl.15.25[1], mean.MCAR.cov.90.AD.men.true.cl.15.25[1],
                                                mean.MCAR.cov.95.AD.men.true.cl.15.25[1], mean.AD.num.men.true.cov.100.15.25[1]),
                                          
                                          U = c(mean.MCAR.cov.35.AD.men.true.cl.15.25[3], mean.MCAR.cov.40.AD.men.true.cl.15.25[3],
                                                mean.MCAR.cov.45.AD.men.true.cl.15.25[3], mean.MCAR.cov.50.AD.men.true.cl.15.25[3],
                                                mean.MCAR.cov.55.AD.men.true.cl.15.25[3], mean.MCAR.cov.60.AD.men.true.cl.15.25[3],
                                                mean.MCAR.cov.65.AD.men.true.cl.15.25[3], mean.MCAR.cov.70.AD.men.true.cl.15.25[3],
                                                mean.MCAR.cov.75.AD.men.true.cl.15.25[3], mean.MCAR.cov.80.AD.men.true.cl.15.25[3],
                                                mean.MCAR.cov.85.AD.men.true.cl.15.25[3], mean.MCAR.cov.90.AD.men.true.cl.15.25[3],
                                                mean.MCAR.cov.95.AD.men.true.cl.15.25[3], mean.AD.num.men.true.cov.100.15.25[3]))


plot.mean.MCAR.men.true.cl.15.25 <- ggplot(mean.MCAR.men.true.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.men.true.cl.15.25.png",
       plot = plot.mean.MCAR.men.true.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

mean.MCAR.women.true.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(mean.MCAR.cov.35.AD.women.true.cl.25.40[2], mean.MCAR.cov.40.AD.women.true.cl.25.40[2],
                                                  mean.MCAR.cov.45.AD.women.true.cl.25.40[2], mean.MCAR.cov.50.AD.women.true.cl.25.40[2],
                                                  mean.MCAR.cov.55.AD.women.true.cl.25.40[2], mean.MCAR.cov.60.AD.women.true.cl.25.40[2],
                                                  mean.MCAR.cov.65.AD.women.true.cl.25.40[2], mean.MCAR.cov.70.AD.women.true.cl.25.40[2],
                                                  mean.MCAR.cov.75.AD.women.true.cl.25.40[2], mean.MCAR.cov.80.AD.women.true.cl.25.40[2],
                                                  mean.MCAR.cov.85.AD.women.true.cl.25.40[2], mean.MCAR.cov.90.AD.women.true.cl.25.40[2],
                                                  mean.MCAR.cov.95.AD.women.true.cl.25.40[2], mean.AD.num.women.true.cov.100.25.40[2]),
                                            
                                            L = c(mean.MCAR.cov.35.AD.women.true.cl.25.40[1], mean.MCAR.cov.40.AD.women.true.cl.25.40[1],
                                                  mean.MCAR.cov.45.AD.women.true.cl.25.40[1], mean.MCAR.cov.50.AD.women.true.cl.25.40[1],
                                                  mean.MCAR.cov.55.AD.women.true.cl.25.40[1], mean.MCAR.cov.60.AD.women.true.cl.25.40[1],
                                                  mean.MCAR.cov.65.AD.women.true.cl.25.40[1], mean.MCAR.cov.70.AD.women.true.cl.25.40[1],
                                                  mean.MCAR.cov.75.AD.women.true.cl.25.40[1], mean.MCAR.cov.80.AD.women.true.cl.25.40[1],
                                                  mean.MCAR.cov.85.AD.women.true.cl.25.40[1], mean.MCAR.cov.90.AD.women.true.cl.25.40[1],
                                                  mean.MCAR.cov.95.AD.women.true.cl.25.40[1], mean.AD.num.women.true.cov.100.25.40[1]),
                                            
                                            U = c(mean.MCAR.cov.35.AD.women.true.cl.25.40[3], mean.MCAR.cov.40.AD.women.true.cl.25.40[3],
                                                  mean.MCAR.cov.45.AD.women.true.cl.25.40[3], mean.MCAR.cov.50.AD.women.true.cl.25.40[3],
                                                  mean.MCAR.cov.55.AD.women.true.cl.25.40[3], mean.MCAR.cov.60.AD.women.true.cl.25.40[3],
                                                  mean.MCAR.cov.65.AD.women.true.cl.25.40[3], mean.MCAR.cov.70.AD.women.true.cl.25.40[3],
                                                  mean.MCAR.cov.75.AD.women.true.cl.25.40[3], mean.MCAR.cov.80.AD.women.true.cl.25.40[3],
                                                  mean.MCAR.cov.85.AD.women.true.cl.25.40[3], mean.MCAR.cov.90.AD.women.true.cl.25.40[3],
                                                  mean.MCAR.cov.95.AD.women.true.cl.25.40[3], mean.AD.num.women.true.cov.100.25.40[3]))


plot.mean.MCAR.women.true.cl.25.40 <- ggplot(mean.MCAR.women.true.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.women.true.cl.25.40.png",
       plot = plot.mean.MCAR.women.true.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Men: 25.40

mean.MCAR.men.true.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                          
                                          F = c(mean.MCAR.cov.35.AD.men.true.cl.25.40[2], mean.MCAR.cov.40.AD.men.true.cl.25.40[2],
                                                mean.MCAR.cov.45.AD.men.true.cl.25.40[2], mean.MCAR.cov.50.AD.men.true.cl.25.40[2],
                                                mean.MCAR.cov.55.AD.men.true.cl.25.40[2], mean.MCAR.cov.60.AD.men.true.cl.25.40[2],
                                                mean.MCAR.cov.65.AD.men.true.cl.25.40[2], mean.MCAR.cov.70.AD.men.true.cl.25.40[2],
                                                mean.MCAR.cov.75.AD.men.true.cl.25.40[2], mean.MCAR.cov.80.AD.men.true.cl.25.40[2],
                                                mean.MCAR.cov.85.AD.men.true.cl.25.40[2], mean.MCAR.cov.90.AD.men.true.cl.25.40[2],
                                                mean.MCAR.cov.95.AD.men.true.cl.25.40[2], mean.AD.num.men.true.cov.100.25.40[2]),
                                          
                                          L = c(mean.MCAR.cov.35.AD.men.true.cl.25.40[1], mean.MCAR.cov.40.AD.men.true.cl.25.40[1],
                                                mean.MCAR.cov.45.AD.men.true.cl.25.40[1], mean.MCAR.cov.50.AD.men.true.cl.25.40[1],
                                                mean.MCAR.cov.55.AD.men.true.cl.25.40[1], mean.MCAR.cov.60.AD.men.true.cl.25.40[1],
                                                mean.MCAR.cov.65.AD.men.true.cl.25.40[1], mean.MCAR.cov.70.AD.men.true.cl.25.40[1],
                                                mean.MCAR.cov.75.AD.men.true.cl.25.40[1], mean.MCAR.cov.80.AD.men.true.cl.25.40[1],
                                                mean.MCAR.cov.85.AD.men.true.cl.25.40[1], mean.MCAR.cov.90.AD.men.true.cl.25.40[1],
                                                mean.MCAR.cov.95.AD.men.true.cl.25.40[1], mean.AD.num.men.true.cov.100.25.40[1]),
                                          
                                          U = c(mean.MCAR.cov.35.AD.men.true.cl.25.40[3], mean.MCAR.cov.40.AD.men.true.cl.25.40[3],
                                                mean.MCAR.cov.45.AD.men.true.cl.25.40[3], mean.MCAR.cov.50.AD.men.true.cl.25.40[3],
                                                mean.MCAR.cov.55.AD.men.true.cl.25.40[3], mean.MCAR.cov.60.AD.men.true.cl.25.40[3],
                                                mean.MCAR.cov.65.AD.men.true.cl.25.40[3], mean.MCAR.cov.70.AD.men.true.cl.25.40[3],
                                                mean.MCAR.cov.75.AD.men.true.cl.25.40[3], mean.MCAR.cov.80.AD.men.true.cl.25.40[3],
                                                mean.MCAR.cov.85.AD.men.true.cl.25.40[3], mean.MCAR.cov.90.AD.men.true.cl.25.40[3],
                                                mean.MCAR.cov.95.AD.men.true.cl.25.40[3], mean.AD.num.men.true.cov.100.25.40[3]))


plot.mean.MCAR.men.true.cl.25.40 <- ggplot(mean.MCAR.men.true.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.mean.MCAR.men.true.cl.25.40.png",
       plot = plot.mean.MCAR.men.true.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 40.50

mean.MCAR.women.true.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(mean.MCAR.cov.35.AD.women.true.cl.40.50[2], mean.MCAR.cov.40.AD.women.true.cl.40.50[2],
                                                  mean.MCAR.cov.45.AD.women.true.cl.40.50[2], mean.MCAR.cov.50.AD.women.true.cl.40.50[2],
                                                  mean.MCAR.cov.55.AD.women.true.cl.40.50[2], mean.MCAR.cov.60.AD.women.true.cl.40.50[2],
                                                  mean.MCAR.cov.65.AD.women.true.cl.40.50[2], mean.MCAR.cov.70.AD.women.true.cl.40.50[2],
                                                  mean.MCAR.cov.75.AD.women.true.cl.40.50[2], mean.MCAR.cov.80.AD.women.true.cl.40.50[2],
                                                  mean.MCAR.cov.85.AD.women.true.cl.40.50[2], mean.MCAR.cov.90.AD.women.true.cl.40.50[2],
                                                  mean.MCAR.cov.95.AD.women.true.cl.40.50[2], mean.AD.num.women.true.cov.100.25.40[2]),
                                            
                                            L = c(mean.MCAR.cov.35.AD.women.true.cl.40.50[1], mean.MCAR.cov.40.AD.women.true.cl.40.50[1],
                                                  mean.MCAR.cov.45.AD.women.true.cl.40.50[1], mean.MCAR.cov.50.AD.women.true.cl.40.50[1],
                                                  mean.MCAR.cov.55.AD.women.true.cl.40.50[1], mean.MCAR.cov.60.AD.women.true.cl.40.50[1],
                                                  mean.MCAR.cov.65.AD.women.true.cl.40.50[1], mean.MCAR.cov.70.AD.women.true.cl.40.50[1],
                                                  mean.MCAR.cov.75.AD.women.true.cl.40.50[1], mean.MCAR.cov.80.AD.women.true.cl.40.50[1],
                                                  mean.MCAR.cov.85.AD.women.true.cl.40.50[1], mean.MCAR.cov.90.AD.women.true.cl.40.50[1],
                                                  mean.MCAR.cov.95.AD.women.true.cl.40.50[1], mean.AD.num.women.true.cov.100.25.40[1]),
                                            
                                            U = c(mean.MCAR.cov.35.AD.women.true.cl.40.50[3], mean.MCAR.cov.40.AD.women.true.cl.40.50[3],
                                                  mean.MCAR.cov.45.AD.women.true.cl.40.50[3], mean.MCAR.cov.50.AD.women.true.cl.40.50[3],
                                                  mean.MCAR.cov.55.AD.women.true.cl.40.50[3], mean.MCAR.cov.60.AD.women.true.cl.40.50[3],
                                                  mean.MCAR.cov.65.AD.women.true.cl.40.50[3], mean.MCAR.cov.70.AD.women.true.cl.40.50[3],
                                                  mean.MCAR.cov.75.AD.women.true.cl.40.50[3], mean.MCAR.cov.80.AD.women.true.cl.40.50[3],
                                                  mean.MCAR.cov.85.AD.women.true.cl.40.50[3], mean.MCAR.cov.90.AD.women.true.cl.40.50[3],
                                                  mean.MCAR.cov.95.AD.women.true.cl.40.50[3], mean.AD.num.women.true.cov.100.25.40[3]))


plot.mean.MCAR.women.true.cl.40.50 <- ggplot(mean.MCAR.women.true.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.mean.MCAR.women.true.cl.40.50.png",
       plot = plot.mean.MCAR.women.true.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

mean.MCAR.men.true.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                          
                                          F = c(mean.MCAR.cov.35.AD.men.true.cl.40.50[2], mean.MCAR.cov.40.AD.men.true.cl.40.50[2],
                                                mean.MCAR.cov.45.AD.men.true.cl.40.50[2], mean.MCAR.cov.50.AD.men.true.cl.40.50[2],
                                                mean.MCAR.cov.55.AD.men.true.cl.40.50[2], mean.MCAR.cov.60.AD.men.true.cl.40.50[2],
                                                mean.MCAR.cov.65.AD.men.true.cl.40.50[2], mean.MCAR.cov.70.AD.men.true.cl.40.50[2],
                                                mean.MCAR.cov.75.AD.men.true.cl.40.50[2], mean.MCAR.cov.80.AD.men.true.cl.40.50[2],
                                                mean.MCAR.cov.85.AD.men.true.cl.40.50[2], mean.MCAR.cov.90.AD.men.true.cl.40.50[2],
                                                mean.MCAR.cov.95.AD.men.true.cl.40.50[2], mean.AD.num.men.true.cov.100.40.50[2]),
                                          
                                          L = c(mean.MCAR.cov.35.AD.men.true.cl.40.50[1], mean.MCAR.cov.40.AD.men.true.cl.40.50[1],
                                                mean.MCAR.cov.45.AD.men.true.cl.40.50[1], mean.MCAR.cov.50.AD.men.true.cl.40.50[1],
                                                mean.MCAR.cov.55.AD.men.true.cl.40.50[1], mean.MCAR.cov.60.AD.men.true.cl.40.50[1],
                                                mean.MCAR.cov.65.AD.men.true.cl.40.50[1], mean.MCAR.cov.70.AD.men.true.cl.40.50[1],
                                                mean.MCAR.cov.75.AD.men.true.cl.40.50[1], mean.MCAR.cov.80.AD.men.true.cl.40.50[1],
                                                mean.MCAR.cov.85.AD.men.true.cl.40.50[1], mean.MCAR.cov.90.AD.men.true.cl.40.50[1],
                                                mean.MCAR.cov.95.AD.men.true.cl.40.50[1], mean.AD.num.men.true.cov.100.40.50[1]),
                                          
                                          U = c(mean.MCAR.cov.35.AD.men.true.cl.40.50[3], mean.MCAR.cov.40.AD.men.true.cl.40.50[3],
                                                mean.MCAR.cov.45.AD.men.true.cl.40.50[3], mean.MCAR.cov.50.AD.men.true.cl.40.50[3],
                                                mean.MCAR.cov.55.AD.men.true.cl.40.50[3], mean.MCAR.cov.60.AD.men.true.cl.40.50[3],
                                                mean.MCAR.cov.65.AD.men.true.cl.40.50[3], mean.MCAR.cov.70.AD.men.true.cl.40.50[3],
                                                mean.MCAR.cov.75.AD.men.true.cl.40.50[3], mean.MCAR.cov.80.AD.men.true.cl.40.50[3],
                                                mean.MCAR.cov.85.AD.men.true.cl.40.50[3], mean.MCAR.cov.90.AD.men.true.cl.40.50[3],
                                                mean.MCAR.cov.95.AD.men.true.cl.40.50[3], mean.AD.num.men.true.cov.100.40.50[3]))


plot.mean.MCAR.men.true.cl.40.50 <- ggplot(mean.MCAR.men.true.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.men.true.cl.40.50.png",
       plot = plot.mean.MCAR.men.true.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




## Median

# Women 15.25

med.MCAR.women.true.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                           
                                           F = c(med.MCAR.cov.35.AD.women.true.cl.15.25[2], med.MCAR.cov.40.AD.women.true.cl.15.25[2],
                                                 med.MCAR.cov.45.AD.women.true.cl.15.25[2], med.MCAR.cov.50.AD.women.true.cl.15.25[2],
                                                 med.MCAR.cov.55.AD.women.true.cl.15.25[2], med.MCAR.cov.60.AD.women.true.cl.15.25[2],
                                                 med.MCAR.cov.65.AD.women.true.cl.15.25[2], med.MCAR.cov.70.AD.women.true.cl.15.25[2],
                                                 med.MCAR.cov.75.AD.women.true.cl.15.25[2], med.MCAR.cov.80.AD.women.true.cl.15.25[2],
                                                 med.MCAR.cov.85.AD.women.true.cl.15.25[2], med.MCAR.cov.90.AD.women.true.cl.15.25[2],
                                                 med.MCAR.cov.95.AD.women.true.cl.15.25[2], med.AD.num.women.true.cov.100.15.25[2]),
                                           
                                           L = c(med.MCAR.cov.35.AD.women.true.cl.15.25[1], med.MCAR.cov.40.AD.women.true.cl.15.25[1],
                                                 med.MCAR.cov.45.AD.women.true.cl.15.25[1], med.MCAR.cov.50.AD.women.true.cl.15.25[1],
                                                 med.MCAR.cov.55.AD.women.true.cl.15.25[1], med.MCAR.cov.60.AD.women.true.cl.15.25[1],
                                                 med.MCAR.cov.65.AD.women.true.cl.15.25[1], med.MCAR.cov.70.AD.women.true.cl.15.25[1],
                                                 med.MCAR.cov.75.AD.women.true.cl.15.25[1], med.MCAR.cov.80.AD.women.true.cl.15.25[1],
                                                 med.MCAR.cov.85.AD.women.true.cl.15.25[1], med.MCAR.cov.90.AD.women.true.cl.15.25[1],
                                                 med.MCAR.cov.95.AD.women.true.cl.15.25[1], med.AD.num.women.true.cov.100.15.25[1]),
                                           
                                           U = c(med.MCAR.cov.35.AD.women.true.cl.15.25[3], med.MCAR.cov.40.AD.women.true.cl.15.25[3],
                                                 med.MCAR.cov.45.AD.women.true.cl.15.25[3], med.MCAR.cov.50.AD.women.true.cl.15.25[3],
                                                 med.MCAR.cov.55.AD.women.true.cl.15.25[3], med.MCAR.cov.60.AD.women.true.cl.15.25[3],
                                                 med.MCAR.cov.65.AD.women.true.cl.15.25[3], med.MCAR.cov.70.AD.women.true.cl.15.25[3],
                                                 med.MCAR.cov.75.AD.women.true.cl.15.25[3], med.MCAR.cov.80.AD.women.true.cl.15.25[3],
                                                 med.MCAR.cov.85.AD.women.true.cl.15.25[3], med.MCAR.cov.90.AD.women.true.cl.15.25[3],
                                                 med.MCAR.cov.95.AD.women.true.cl.15.25[3], med.AD.num.women.true.cov.100.15.25[3]))


plot.med.MCAR.women.true.cl.15.25 <- ggplot(med.MCAR.women.true.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.true.cl.15.25.png",
       plot = plot.med.MCAR.women.true.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

med.MCAR.men.true.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(med.MCAR.cov.35.AD.men.true.cl.15.25[2], med.MCAR.cov.40.AD.men.true.cl.15.25[2],
                                               med.MCAR.cov.45.AD.men.true.cl.15.25[2], med.MCAR.cov.50.AD.men.true.cl.15.25[2],
                                               med.MCAR.cov.55.AD.men.true.cl.15.25[2], med.MCAR.cov.60.AD.men.true.cl.15.25[2],
                                               med.MCAR.cov.65.AD.men.true.cl.15.25[2], med.MCAR.cov.70.AD.men.true.cl.15.25[2],
                                               med.MCAR.cov.75.AD.men.true.cl.15.25[2], med.MCAR.cov.80.AD.men.true.cl.15.25[2],
                                               med.MCAR.cov.85.AD.men.true.cl.15.25[2], med.MCAR.cov.90.AD.men.true.cl.15.25[2],
                                               med.MCAR.cov.95.AD.men.true.cl.15.25[2], med.AD.num.men.true.cov.100.15.25[2]),
                                         
                                         L = c(med.MCAR.cov.35.AD.men.true.cl.15.25[1], med.MCAR.cov.40.AD.men.true.cl.15.25[1],
                                               med.MCAR.cov.45.AD.men.true.cl.15.25[1], med.MCAR.cov.50.AD.men.true.cl.15.25[1],
                                               med.MCAR.cov.55.AD.men.true.cl.15.25[1], med.MCAR.cov.60.AD.men.true.cl.15.25[1],
                                               med.MCAR.cov.65.AD.men.true.cl.15.25[1], med.MCAR.cov.70.AD.men.true.cl.15.25[1],
                                               med.MCAR.cov.75.AD.men.true.cl.15.25[1], med.MCAR.cov.80.AD.men.true.cl.15.25[1],
                                               med.MCAR.cov.85.AD.men.true.cl.15.25[1], med.MCAR.cov.90.AD.men.true.cl.15.25[1],
                                               med.MCAR.cov.95.AD.men.true.cl.15.25[1], med.AD.num.men.true.cov.100.15.25[1]),
                                         
                                         U = c(med.MCAR.cov.35.AD.men.true.cl.15.25[3], med.MCAR.cov.40.AD.men.true.cl.15.25[3],
                                               med.MCAR.cov.45.AD.men.true.cl.15.25[3], med.MCAR.cov.50.AD.men.true.cl.15.25[3],
                                               med.MCAR.cov.55.AD.men.true.cl.15.25[3], med.MCAR.cov.60.AD.men.true.cl.15.25[3],
                                               med.MCAR.cov.65.AD.men.true.cl.15.25[3], med.MCAR.cov.70.AD.men.true.cl.15.25[3],
                                               med.MCAR.cov.75.AD.men.true.cl.15.25[3], med.MCAR.cov.80.AD.men.true.cl.15.25[3],
                                               med.MCAR.cov.85.AD.men.true.cl.15.25[3], med.MCAR.cov.90.AD.men.true.cl.15.25[3],
                                               med.MCAR.cov.95.AD.men.true.cl.15.25[3], med.AD.num.men.true.cov.100.15.25[3]))


plot.med.MCAR.men.true.cl.15.25 <- ggplot(med.MCAR.men.true.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.true.cl.15.25.png",
       plot = plot.med.MCAR.men.true.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

med.MCAR.women.true.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                           
                                           F = c(med.MCAR.cov.35.AD.women.true.cl.25.40[2], med.MCAR.cov.40.AD.women.true.cl.25.40[2],
                                                 med.MCAR.cov.45.AD.women.true.cl.25.40[2], med.MCAR.cov.50.AD.women.true.cl.25.40[2],
                                                 med.MCAR.cov.55.AD.women.true.cl.25.40[2], med.MCAR.cov.60.AD.women.true.cl.25.40[2],
                                                 med.MCAR.cov.65.AD.women.true.cl.25.40[2], med.MCAR.cov.70.AD.women.true.cl.25.40[2],
                                                 med.MCAR.cov.75.AD.women.true.cl.25.40[2], med.MCAR.cov.80.AD.women.true.cl.25.40[2],
                                                 med.MCAR.cov.85.AD.women.true.cl.25.40[2], med.MCAR.cov.90.AD.women.true.cl.25.40[2],
                                                 med.MCAR.cov.95.AD.women.true.cl.25.40[2], med.AD.num.women.true.cov.100.25.40[2]),
                                           
                                           L = c(med.MCAR.cov.35.AD.women.true.cl.25.40[1], med.MCAR.cov.40.AD.women.true.cl.25.40[1],
                                                 med.MCAR.cov.45.AD.women.true.cl.25.40[1], med.MCAR.cov.50.AD.women.true.cl.25.40[1],
                                                 med.MCAR.cov.55.AD.women.true.cl.25.40[1], med.MCAR.cov.60.AD.women.true.cl.25.40[1],
                                                 med.MCAR.cov.65.AD.women.true.cl.25.40[1], med.MCAR.cov.70.AD.women.true.cl.25.40[1],
                                                 med.MCAR.cov.75.AD.women.true.cl.25.40[1], med.MCAR.cov.80.AD.women.true.cl.25.40[1],
                                                 med.MCAR.cov.85.AD.women.true.cl.25.40[1], med.MCAR.cov.90.AD.women.true.cl.25.40[1],
                                                 med.MCAR.cov.95.AD.women.true.cl.25.40[1], med.AD.num.women.true.cov.100.25.40[1]),
                                           
                                           U = c(med.MCAR.cov.35.AD.women.true.cl.25.40[3], med.MCAR.cov.40.AD.women.true.cl.25.40[3],
                                                 med.MCAR.cov.45.AD.women.true.cl.25.40[3], med.MCAR.cov.50.AD.women.true.cl.25.40[3],
                                                 med.MCAR.cov.55.AD.women.true.cl.25.40[3], med.MCAR.cov.60.AD.women.true.cl.25.40[3],
                                                 med.MCAR.cov.65.AD.women.true.cl.25.40[3], med.MCAR.cov.70.AD.women.true.cl.25.40[3],
                                                 med.MCAR.cov.75.AD.women.true.cl.25.40[3], med.MCAR.cov.80.AD.women.true.cl.25.40[3],
                                                 med.MCAR.cov.85.AD.women.true.cl.25.40[3], med.MCAR.cov.90.AD.women.true.cl.25.40[3],
                                                 med.MCAR.cov.95.AD.women.true.cl.25.40[3], med.AD.num.women.true.cov.100.25.40[3]))


plot.med.MCAR.women.true.cl.25.40 <- ggplot(med.MCAR.women.true.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.true.cl.25.40.png",
       plot = plot.med.MCAR.women.true.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Men: 25.40

med.MCAR.men.true.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(med.MCAR.cov.35.AD.men.true.cl.25.40[2], med.MCAR.cov.40.AD.men.true.cl.25.40[2],
                                               med.MCAR.cov.45.AD.men.true.cl.25.40[2], med.MCAR.cov.50.AD.men.true.cl.25.40[2],
                                               med.MCAR.cov.55.AD.men.true.cl.25.40[2], med.MCAR.cov.60.AD.men.true.cl.25.40[2],
                                               med.MCAR.cov.65.AD.men.true.cl.25.40[2], med.MCAR.cov.70.AD.men.true.cl.25.40[2],
                                               med.MCAR.cov.75.AD.men.true.cl.25.40[2], med.MCAR.cov.80.AD.men.true.cl.25.40[2],
                                               med.MCAR.cov.85.AD.men.true.cl.25.40[2], med.MCAR.cov.90.AD.men.true.cl.25.40[2],
                                               med.MCAR.cov.95.AD.men.true.cl.25.40[2], med.AD.num.men.true.cov.100.25.40[2]),
                                         
                                         L = c(med.MCAR.cov.35.AD.men.true.cl.25.40[1], med.MCAR.cov.40.AD.men.true.cl.25.40[1],
                                               med.MCAR.cov.45.AD.men.true.cl.25.40[1], med.MCAR.cov.50.AD.men.true.cl.25.40[1],
                                               med.MCAR.cov.55.AD.men.true.cl.25.40[1], med.MCAR.cov.60.AD.men.true.cl.25.40[1],
                                               med.MCAR.cov.65.AD.men.true.cl.25.40[1], med.MCAR.cov.70.AD.men.true.cl.25.40[1],
                                               med.MCAR.cov.75.AD.men.true.cl.25.40[1], med.MCAR.cov.80.AD.men.true.cl.25.40[1],
                                               med.MCAR.cov.85.AD.men.true.cl.25.40[1], med.MCAR.cov.90.AD.men.true.cl.25.40[1],
                                               med.MCAR.cov.95.AD.men.true.cl.25.40[1], med.AD.num.men.true.cov.100.25.40[1]),
                                         
                                         U = c(med.MCAR.cov.35.AD.men.true.cl.25.40[3], med.MCAR.cov.40.AD.men.true.cl.25.40[3],
                                               med.MCAR.cov.45.AD.men.true.cl.25.40[3], med.MCAR.cov.50.AD.men.true.cl.25.40[3],
                                               med.MCAR.cov.55.AD.men.true.cl.25.40[3], med.MCAR.cov.60.AD.men.true.cl.25.40[3],
                                               med.MCAR.cov.65.AD.men.true.cl.25.40[3], med.MCAR.cov.70.AD.men.true.cl.25.40[3],
                                               med.MCAR.cov.75.AD.men.true.cl.25.40[3], med.MCAR.cov.80.AD.men.true.cl.25.40[3],
                                               med.MCAR.cov.85.AD.men.true.cl.25.40[3], med.MCAR.cov.90.AD.men.true.cl.25.40[3],
                                               med.MCAR.cov.95.AD.men.true.cl.25.40[3], med.AD.num.men.true.cov.100.25.40[3]))


plot.med.MCAR.men.true.cl.25.40 <- ggplot(med.MCAR.men.true.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.true.cl.25.40.png",
       plot = plot.med.MCAR.men.true.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 40.50

med.MCAR.women.true.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                           
                                           F = c(med.MCAR.cov.35.AD.women.true.cl.40.50[2], med.MCAR.cov.40.AD.women.true.cl.40.50[2],
                                                 med.MCAR.cov.45.AD.women.true.cl.40.50[2], med.MCAR.cov.50.AD.women.true.cl.40.50[2],
                                                 med.MCAR.cov.55.AD.women.true.cl.40.50[2], med.MCAR.cov.60.AD.women.true.cl.40.50[2],
                                                 med.MCAR.cov.65.AD.women.true.cl.40.50[2], med.MCAR.cov.70.AD.women.true.cl.40.50[2],
                                                 med.MCAR.cov.75.AD.women.true.cl.40.50[2], med.MCAR.cov.80.AD.women.true.cl.40.50[2],
                                                 med.MCAR.cov.85.AD.women.true.cl.40.50[2], med.MCAR.cov.90.AD.women.true.cl.40.50[2],
                                                 med.MCAR.cov.95.AD.women.true.cl.40.50[2], med.AD.num.women.true.cov.100.40.50[2]),
                                           
                                           L = c(med.MCAR.cov.35.AD.women.true.cl.40.50[1], med.MCAR.cov.40.AD.women.true.cl.40.50[1],
                                                 med.MCAR.cov.45.AD.women.true.cl.40.50[1], med.MCAR.cov.50.AD.women.true.cl.40.50[1],
                                                 med.MCAR.cov.55.AD.women.true.cl.40.50[1], med.MCAR.cov.60.AD.women.true.cl.40.50[1],
                                                 med.MCAR.cov.65.AD.women.true.cl.40.50[1], med.MCAR.cov.70.AD.women.true.cl.40.50[1],
                                                 med.MCAR.cov.75.AD.women.true.cl.40.50[1], med.MCAR.cov.80.AD.women.true.cl.40.50[1],
                                                 med.MCAR.cov.85.AD.women.true.cl.40.50[1], med.MCAR.cov.90.AD.women.true.cl.40.50[1],
                                                 med.MCAR.cov.95.AD.women.true.cl.40.50[1], med.AD.num.women.true.cov.100.40.50[1]),
                                           
                                           U = c(med.MCAR.cov.35.AD.women.true.cl.40.50[3], med.MCAR.cov.40.AD.women.true.cl.40.50[3],
                                                 med.MCAR.cov.45.AD.women.true.cl.40.50[3], med.MCAR.cov.50.AD.women.true.cl.40.50[3],
                                                 med.MCAR.cov.55.AD.women.true.cl.40.50[3], med.MCAR.cov.60.AD.women.true.cl.40.50[3],
                                                 med.MCAR.cov.65.AD.women.true.cl.40.50[3], med.MCAR.cov.70.AD.women.true.cl.40.50[3],
                                                 med.MCAR.cov.75.AD.women.true.cl.40.50[3], med.MCAR.cov.80.AD.women.true.cl.40.50[3],
                                                 med.MCAR.cov.85.AD.women.true.cl.40.50[3], med.MCAR.cov.90.AD.women.true.cl.40.50[3],
                                                 med.MCAR.cov.95.AD.women.true.cl.40.50[3], med.AD.num.women.true.cov.100.40.50[3]))


plot.med.MCAR.women.true.cl.40.50 <- ggplot(med.MCAR.women.true.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.true.cl.40.50.png",
       plot = plot.med.MCAR.women.true.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

med.MCAR.men.true.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                         
                                         F = c(med.MCAR.cov.35.AD.men.true.cl.40.50[2], med.MCAR.cov.40.AD.men.true.cl.40.50[2],
                                               med.MCAR.cov.45.AD.men.true.cl.40.50[2], med.MCAR.cov.50.AD.men.true.cl.40.50[2],
                                               med.MCAR.cov.55.AD.men.true.cl.40.50[2], med.MCAR.cov.60.AD.men.true.cl.40.50[2],
                                               med.MCAR.cov.65.AD.men.true.cl.40.50[2], med.MCAR.cov.70.AD.men.true.cl.40.50[2],
                                               med.MCAR.cov.75.AD.men.true.cl.40.50[2], med.MCAR.cov.80.AD.men.true.cl.40.50[2],
                                               med.MCAR.cov.85.AD.men.true.cl.40.50[2], med.MCAR.cov.90.AD.men.true.cl.40.50[2],
                                               med.MCAR.cov.95.AD.men.true.cl.40.50[2], med.AD.num.men.true.cov.100.40.50[2]),
                                         
                                         L = c(med.MCAR.cov.35.AD.men.true.cl.40.50[1], med.MCAR.cov.40.AD.men.true.cl.40.50[1],
                                               med.MCAR.cov.45.AD.men.true.cl.40.50[1], med.MCAR.cov.50.AD.men.true.cl.40.50[1],
                                               med.MCAR.cov.55.AD.men.true.cl.40.50[1], med.MCAR.cov.60.AD.men.true.cl.40.50[1],
                                               med.MCAR.cov.65.AD.men.true.cl.40.50[1], med.MCAR.cov.70.AD.men.true.cl.40.50[1],
                                               med.MCAR.cov.75.AD.men.true.cl.40.50[1], med.MCAR.cov.80.AD.men.true.cl.40.50[1],
                                               med.MCAR.cov.85.AD.men.true.cl.40.50[1], med.MCAR.cov.90.AD.men.true.cl.40.50[1],
                                               med.MCAR.cov.95.AD.men.true.cl.40.50[1], med.AD.num.men.true.cov.100.40.50[1]),
                                         
                                         U = c(med.MCAR.cov.35.AD.men.true.cl.40.50[3], med.MCAR.cov.40.AD.men.true.cl.40.50[3],
                                               med.MCAR.cov.45.AD.men.true.cl.40.50[3], med.MCAR.cov.50.AD.men.true.cl.40.50[3],
                                               med.MCAR.cov.55.AD.men.true.cl.40.50[3], med.MCAR.cov.60.AD.men.true.cl.40.50[3],
                                               med.MCAR.cov.65.AD.men.true.cl.40.50[3], med.MCAR.cov.70.AD.men.true.cl.40.50[3],
                                               med.MCAR.cov.75.AD.men.true.cl.40.50[3], med.MCAR.cov.80.AD.men.true.cl.40.50[3],
                                               med.MCAR.cov.85.AD.men.true.cl.40.50[3], med.MCAR.cov.90.AD.men.true.cl.40.50[3],
                                               med.MCAR.cov.95.AD.men.true.cl.40.50[3], med.AD.num.men.true.cov.100.40.50[3]))


plot.med.MCAR.men.true.cl.40.50 <- ggplot(med.MCAR.men.true.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.true.cl.40.50.png",
       plot = plot.med.MCAR.men.true.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



## Standard deviation


# Women 15.25

sd.MCAR.women.true.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                          
                                          F = c(sd.MCAR.cov.35.AD.women.true.cl.15.25[2], sd.MCAR.cov.40.AD.women.true.cl.15.25[2],
                                                sd.MCAR.cov.45.AD.women.true.cl.15.25[2], sd.MCAR.cov.50.AD.women.true.cl.15.25[2],
                                                sd.MCAR.cov.55.AD.women.true.cl.15.25[2], sd.MCAR.cov.60.AD.women.true.cl.15.25[2],
                                                sd.MCAR.cov.65.AD.women.true.cl.15.25[2], sd.MCAR.cov.70.AD.women.true.cl.15.25[2],
                                                sd.MCAR.cov.75.AD.women.true.cl.15.25[2], sd.MCAR.cov.80.AD.women.true.cl.15.25[2],
                                                sd.MCAR.cov.85.AD.women.true.cl.15.25[2], sd.MCAR.cov.90.AD.women.true.cl.15.25[2],
                                                sd.MCAR.cov.95.AD.women.true.cl.15.25[2], sd.AD.num.women.true.cov.100.15.25[2]),
                                          
                                          L = c(sd.MCAR.cov.35.AD.women.true.cl.15.25[1], sd.MCAR.cov.40.AD.women.true.cl.15.25[1],
                                                sd.MCAR.cov.45.AD.women.true.cl.15.25[1], sd.MCAR.cov.50.AD.women.true.cl.15.25[1],
                                                sd.MCAR.cov.55.AD.women.true.cl.15.25[1], sd.MCAR.cov.60.AD.women.true.cl.15.25[1],
                                                sd.MCAR.cov.65.AD.women.true.cl.15.25[1], sd.MCAR.cov.70.AD.women.true.cl.15.25[1],
                                                sd.MCAR.cov.75.AD.women.true.cl.15.25[1], sd.MCAR.cov.80.AD.women.true.cl.15.25[1],
                                                sd.MCAR.cov.85.AD.women.true.cl.15.25[1], sd.MCAR.cov.90.AD.women.true.cl.15.25[1],
                                                sd.MCAR.cov.95.AD.women.true.cl.15.25[1], sd.AD.num.women.true.cov.100.15.25[1]),
                                          
                                          U = c(sd.MCAR.cov.35.AD.women.true.cl.15.25[3], sd.MCAR.cov.40.AD.women.true.cl.15.25[3],
                                                sd.MCAR.cov.45.AD.women.true.cl.15.25[3], sd.MCAR.cov.50.AD.women.true.cl.15.25[3],
                                                sd.MCAR.cov.55.AD.women.true.cl.15.25[3], sd.MCAR.cov.60.AD.women.true.cl.15.25[3],
                                                sd.MCAR.cov.65.AD.women.true.cl.15.25[3], sd.MCAR.cov.70.AD.women.true.cl.15.25[3],
                                                sd.MCAR.cov.75.AD.women.true.cl.15.25[3], sd.MCAR.cov.80.AD.women.true.cl.15.25[3],
                                                sd.MCAR.cov.85.AD.women.true.cl.15.25[3], sd.MCAR.cov.90.AD.women.true.cl.15.25[3],
                                                sd.MCAR.cov.95.AD.women.true.cl.15.25[3], sd.AD.num.women.true.cov.100.15.25[3]))


plot.sd.MCAR.women.true.cl.15.25 <- ggplot(sd.MCAR.women.true.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.true.cl.15.25.png",
       plot = plot.sd.MCAR.women.true.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

sd.MCAR.men.true.cl.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                        
                                        F = c(sd.MCAR.cov.35.AD.men.true.cl.15.25[2], sd.MCAR.cov.40.AD.men.true.cl.15.25[2],
                                              sd.MCAR.cov.45.AD.men.true.cl.15.25[2], sd.MCAR.cov.50.AD.men.true.cl.15.25[2],
                                              sd.MCAR.cov.55.AD.men.true.cl.15.25[2], sd.MCAR.cov.60.AD.men.true.cl.15.25[2],
                                              sd.MCAR.cov.65.AD.men.true.cl.15.25[2], sd.MCAR.cov.70.AD.men.true.cl.15.25[2],
                                              sd.MCAR.cov.75.AD.men.true.cl.15.25[2], sd.MCAR.cov.80.AD.men.true.cl.15.25[2],
                                              sd.MCAR.cov.85.AD.men.true.cl.15.25[2], sd.MCAR.cov.90.AD.men.true.cl.15.25[2],
                                              sd.MCAR.cov.95.AD.men.true.cl.15.25[2], sd.AD.num.men.true.cov.100.15.25[2]),
                                        
                                        L = c(sd.MCAR.cov.35.AD.men.true.cl.15.25[1], sd.MCAR.cov.40.AD.men.true.cl.15.25[1],
                                              sd.MCAR.cov.45.AD.men.true.cl.15.25[1], sd.MCAR.cov.50.AD.men.true.cl.15.25[1],
                                              sd.MCAR.cov.55.AD.men.true.cl.15.25[1], sd.MCAR.cov.60.AD.men.true.cl.15.25[1],
                                              sd.MCAR.cov.65.AD.men.true.cl.15.25[1], sd.MCAR.cov.70.AD.men.true.cl.15.25[1],
                                              sd.MCAR.cov.75.AD.men.true.cl.15.25[1], sd.MCAR.cov.80.AD.men.true.cl.15.25[1],
                                              sd.MCAR.cov.85.AD.men.true.cl.15.25[1], sd.MCAR.cov.90.AD.men.true.cl.15.25[1],
                                              sd.MCAR.cov.95.AD.men.true.cl.15.25[1], sd.AD.num.men.true.cov.100.15.25[1]),
                                        
                                        U = c(sd.MCAR.cov.35.AD.men.true.cl.15.25[3], sd.MCAR.cov.40.AD.men.true.cl.15.25[3],
                                              sd.MCAR.cov.45.AD.men.true.cl.15.25[3], sd.MCAR.cov.50.AD.men.true.cl.15.25[3],
                                              sd.MCAR.cov.55.AD.men.true.cl.15.25[3], sd.MCAR.cov.60.AD.men.true.cl.15.25[3],
                                              sd.MCAR.cov.65.AD.men.true.cl.15.25[3], sd.MCAR.cov.70.AD.men.true.cl.15.25[3],
                                              sd.MCAR.cov.75.AD.men.true.cl.15.25[3], sd.MCAR.cov.80.AD.men.true.cl.15.25[3],
                                              sd.MCAR.cov.85.AD.men.true.cl.15.25[3], sd.MCAR.cov.90.AD.men.true.cl.15.25[3],
                                              sd.MCAR.cov.95.AD.men.true.cl.15.25[3], sd.AD.num.men.true.cov.100.15.25[3]))


plot.sd.MCAR.men.true.cl.15.25 <- ggplot(sd.MCAR.men.true.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.sd.MCAR.men.true.cl.15.25.png",
       plot = plot.sd.MCAR.men.true.cl.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

sd.MCAR.women.true.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                          
                                          F = c(sd.MCAR.cov.35.AD.women.true.cl.25.40[2], sd.MCAR.cov.40.AD.women.true.cl.25.40[2],
                                                sd.MCAR.cov.45.AD.women.true.cl.25.40[2], sd.MCAR.cov.50.AD.women.true.cl.25.40[2],
                                                sd.MCAR.cov.55.AD.women.true.cl.25.40[2], sd.MCAR.cov.60.AD.women.true.cl.25.40[2],
                                                sd.MCAR.cov.65.AD.women.true.cl.25.40[2], sd.MCAR.cov.70.AD.women.true.cl.25.40[2],
                                                sd.MCAR.cov.75.AD.women.true.cl.25.40[2], sd.MCAR.cov.80.AD.women.true.cl.25.40[2],
                                                sd.MCAR.cov.85.AD.women.true.cl.25.40[2], sd.MCAR.cov.90.AD.women.true.cl.25.40[2],
                                                sd.MCAR.cov.95.AD.women.true.cl.25.40[2], sd.AD.num.women.true.cov.100.25.40[2]),
                                          
                                          L = c(sd.MCAR.cov.35.AD.women.true.cl.25.40[1], sd.MCAR.cov.40.AD.women.true.cl.25.40[1],
                                                sd.MCAR.cov.45.AD.women.true.cl.25.40[1], sd.MCAR.cov.50.AD.women.true.cl.25.40[1],
                                                sd.MCAR.cov.55.AD.women.true.cl.25.40[1], sd.MCAR.cov.60.AD.women.true.cl.25.40[1],
                                                sd.MCAR.cov.65.AD.women.true.cl.25.40[1], sd.MCAR.cov.70.AD.women.true.cl.25.40[1],
                                                sd.MCAR.cov.75.AD.women.true.cl.25.40[1], sd.MCAR.cov.80.AD.women.true.cl.25.40[1],
                                                sd.MCAR.cov.85.AD.women.true.cl.25.40[1], sd.MCAR.cov.90.AD.women.true.cl.25.40[1],
                                                sd.MCAR.cov.95.AD.women.true.cl.25.40[1], sd.AD.num.women.true.cov.100.25.40[1]),
                                          
                                          U = c(sd.MCAR.cov.35.AD.women.true.cl.25.40[3], sd.MCAR.cov.40.AD.women.true.cl.25.40[3],
                                                sd.MCAR.cov.45.AD.women.true.cl.25.40[3], sd.MCAR.cov.50.AD.women.true.cl.25.40[3],
                                                sd.MCAR.cov.55.AD.women.true.cl.25.40[3], sd.MCAR.cov.60.AD.women.true.cl.25.40[3],
                                                sd.MCAR.cov.65.AD.women.true.cl.25.40[3], sd.MCAR.cov.70.AD.women.true.cl.25.40[3],
                                                sd.MCAR.cov.75.AD.women.true.cl.25.40[3], sd.MCAR.cov.80.AD.women.true.cl.25.40[3],
                                                sd.MCAR.cov.85.AD.women.true.cl.25.40[3], sd.MCAR.cov.90.AD.women.true.cl.25.40[3],
                                                sd.MCAR.cov.95.AD.women.true.cl.25.40[3], sd.AD.num.women.true.cov.100.25.40[3]))


plot.sd.MCAR.women.true.cl.25.40 <- ggplot(sd.MCAR.women.true.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.true.cl.25.40.png",
       plot = plot.sd.MCAR.women.true.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Men: 25.40

sd.MCAR.men.true.cl.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                        
                                        F = c(sd.MCAR.cov.35.AD.men.true.cl.25.40[2], sd.MCAR.cov.40.AD.men.true.cl.25.40[2],
                                              sd.MCAR.cov.45.AD.men.true.cl.25.40[2], sd.MCAR.cov.50.AD.men.true.cl.25.40[2],
                                              sd.MCAR.cov.55.AD.men.true.cl.25.40[2], sd.MCAR.cov.60.AD.men.true.cl.25.40[2],
                                              sd.MCAR.cov.65.AD.men.true.cl.25.40[2], sd.MCAR.cov.70.AD.men.true.cl.25.40[2],
                                              sd.MCAR.cov.75.AD.men.true.cl.25.40[2], sd.MCAR.cov.80.AD.men.true.cl.25.40[2],
                                              sd.MCAR.cov.85.AD.men.true.cl.25.40[2], sd.MCAR.cov.90.AD.men.true.cl.25.40[2],
                                              sd.MCAR.cov.95.AD.men.true.cl.25.40[2], sd.AD.num.men.true.cov.100.25.40[2]),
                                        
                                        L = c(sd.MCAR.cov.35.AD.men.true.cl.25.40[1], sd.MCAR.cov.40.AD.men.true.cl.25.40[1],
                                              sd.MCAR.cov.45.AD.men.true.cl.25.40[1], sd.MCAR.cov.50.AD.men.true.cl.25.40[1],
                                              sd.MCAR.cov.55.AD.men.true.cl.25.40[1], sd.MCAR.cov.60.AD.men.true.cl.25.40[1],
                                              sd.MCAR.cov.65.AD.men.true.cl.25.40[1], sd.MCAR.cov.70.AD.men.true.cl.25.40[1],
                                              sd.MCAR.cov.75.AD.men.true.cl.25.40[1], sd.MCAR.cov.80.AD.men.true.cl.25.40[1],
                                              sd.MCAR.cov.85.AD.men.true.cl.25.40[1], sd.MCAR.cov.90.AD.men.true.cl.25.40[1],
                                              sd.MCAR.cov.95.AD.men.true.cl.25.40[1], sd.AD.num.men.true.cov.100.25.40[1]),
                                        
                                        U = c(sd.MCAR.cov.35.AD.men.true.cl.25.40[3], sd.MCAR.cov.40.AD.men.true.cl.25.40[3],
                                              sd.MCAR.cov.45.AD.men.true.cl.25.40[3], sd.MCAR.cov.50.AD.men.true.cl.25.40[3],
                                              sd.MCAR.cov.55.AD.men.true.cl.25.40[3], sd.MCAR.cov.60.AD.men.true.cl.25.40[3],
                                              sd.MCAR.cov.65.AD.men.true.cl.25.40[3], sd.MCAR.cov.70.AD.men.true.cl.25.40[3],
                                              sd.MCAR.cov.75.AD.men.true.cl.25.40[3], sd.MCAR.cov.80.AD.men.true.cl.25.40[3],
                                              sd.MCAR.cov.85.AD.men.true.cl.25.40[3], sd.MCAR.cov.90.AD.men.true.cl.25.40[3],
                                              sd.MCAR.cov.95.AD.men.true.cl.25.40[3], sd.AD.num.men.true.cov.100.25.40[3]))


plot.sd.MCAR.men.true.cl.25.40 <- ggplot(sd.MCAR.men.true.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.sd.MCAR.men.true.cl.25.40.png",
       plot = plot.sd.MCAR.men.true.cl.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: 40.50

sd.MCAR.women.true.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                          
                                          F = c(sd.MCAR.cov.35.AD.women.true.cl.40.50[2], sd.MCAR.cov.40.AD.women.true.cl.40.50[2],
                                                sd.MCAR.cov.45.AD.women.true.cl.40.50[2], sd.MCAR.cov.50.AD.women.true.cl.40.50[2],
                                                sd.MCAR.cov.55.AD.women.true.cl.40.50[2], sd.MCAR.cov.60.AD.women.true.cl.40.50[2],
                                                sd.MCAR.cov.65.AD.women.true.cl.40.50[2], sd.MCAR.cov.70.AD.women.true.cl.40.50[2],
                                                sd.MCAR.cov.75.AD.women.true.cl.40.50[2], sd.MCAR.cov.80.AD.women.true.cl.40.50[2],
                                                sd.MCAR.cov.85.AD.women.true.cl.40.50[2], sd.MCAR.cov.90.AD.women.true.cl.40.50[2],
                                                sd.MCAR.cov.95.AD.women.true.cl.40.50[2], sd.AD.num.women.true.cov.100.40.50[2]),
                                          
                                          L = c(sd.MCAR.cov.35.AD.women.true.cl.40.50[1], sd.MCAR.cov.40.AD.women.true.cl.40.50[1],
                                                sd.MCAR.cov.45.AD.women.true.cl.40.50[1], sd.MCAR.cov.50.AD.women.true.cl.40.50[1],
                                                sd.MCAR.cov.55.AD.women.true.cl.40.50[1], sd.MCAR.cov.60.AD.women.true.cl.40.50[1],
                                                sd.MCAR.cov.65.AD.women.true.cl.40.50[1], sd.MCAR.cov.70.AD.women.true.cl.40.50[1],
                                                sd.MCAR.cov.75.AD.women.true.cl.40.50[1], sd.MCAR.cov.80.AD.women.true.cl.40.50[1],
                                                sd.MCAR.cov.85.AD.women.true.cl.40.50[1], sd.MCAR.cov.90.AD.women.true.cl.40.50[1],
                                                sd.MCAR.cov.95.AD.women.true.cl.40.50[1], sd.AD.num.women.true.cov.100.40.50[1]),
                                          
                                          U = c(sd.MCAR.cov.35.AD.women.true.cl.40.50[3], sd.MCAR.cov.40.AD.women.true.cl.40.50[3],
                                                sd.MCAR.cov.45.AD.women.true.cl.40.50[3], sd.MCAR.cov.50.AD.women.true.cl.40.50[3],
                                                sd.MCAR.cov.55.AD.women.true.cl.40.50[3], sd.MCAR.cov.60.AD.women.true.cl.40.50[3],
                                                sd.MCAR.cov.65.AD.women.true.cl.40.50[3], sd.MCAR.cov.70.AD.women.true.cl.40.50[3],
                                                sd.MCAR.cov.75.AD.women.true.cl.40.50[3], sd.MCAR.cov.80.AD.women.true.cl.40.50[3],
                                                sd.MCAR.cov.85.AD.women.true.cl.40.50[3], sd.MCAR.cov.90.AD.women.true.cl.40.50[3],
                                                sd.MCAR.cov.95.AD.women.true.cl.40.50[3], sd.AD.num.women.true.cov.100.40.50[3]))


plot.sd.MCAR.women.true.cl.40.50 <- ggplot(sd.MCAR.women.true.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.true.cl.40.50.png",
       plot = plot.sd.MCAR.women.true.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

sd.MCAR.men.true.cl.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                        
                                        F = c(sd.MCAR.cov.35.AD.men.true.cl.40.50[2], sd.MCAR.cov.40.AD.men.true.cl.40.50[2],
                                              sd.MCAR.cov.45.AD.men.true.cl.40.50[2], sd.MCAR.cov.50.AD.men.true.cl.40.50[2],
                                              sd.MCAR.cov.55.AD.men.true.cl.40.50[2], sd.MCAR.cov.60.AD.men.true.cl.40.50[2],
                                              sd.MCAR.cov.65.AD.men.true.cl.40.50[2], sd.MCAR.cov.70.AD.men.true.cl.40.50[2],
                                              sd.MCAR.cov.75.AD.men.true.cl.40.50[2], sd.MCAR.cov.80.AD.men.true.cl.40.50[2],
                                              sd.MCAR.cov.85.AD.men.true.cl.40.50[2], sd.MCAR.cov.90.AD.men.true.cl.40.50[2],
                                              sd.MCAR.cov.95.AD.men.true.cl.40.50[2], sd.AD.num.men.true.cov.100.40.50[2]),
                                        
                                        L = c(sd.MCAR.cov.35.AD.men.true.cl.40.50[1], sd.MCAR.cov.40.AD.men.true.cl.40.50[1],
                                              sd.MCAR.cov.45.AD.men.true.cl.40.50[1], sd.MCAR.cov.50.AD.men.true.cl.40.50[1],
                                              sd.MCAR.cov.55.AD.men.true.cl.40.50[1], sd.MCAR.cov.60.AD.men.true.cl.40.50[1],
                                              sd.MCAR.cov.65.AD.men.true.cl.40.50[1], sd.MCAR.cov.70.AD.men.true.cl.40.50[1],
                                              sd.MCAR.cov.75.AD.men.true.cl.40.50[1], sd.MCAR.cov.80.AD.men.true.cl.40.50[1],
                                              sd.MCAR.cov.85.AD.men.true.cl.40.50[1], sd.MCAR.cov.90.AD.men.true.cl.40.50[1],
                                              sd.MCAR.cov.95.AD.men.true.cl.40.50[1], sd.AD.num.men.true.cov.100.40.50[1]),
                                        
                                        U = c(sd.MCAR.cov.35.AD.men.true.cl.40.50[3], sd.MCAR.cov.40.AD.men.true.cl.40.50[3],
                                              sd.MCAR.cov.45.AD.men.true.cl.40.50[3], sd.MCAR.cov.50.AD.men.true.cl.40.50[3],
                                              sd.MCAR.cov.55.AD.men.true.cl.40.50[3], sd.MCAR.cov.60.AD.men.true.cl.40.50[3],
                                              sd.MCAR.cov.65.AD.men.true.cl.40.50[3], sd.MCAR.cov.70.AD.men.true.cl.40.50[3],
                                              sd.MCAR.cov.75.AD.men.true.cl.40.50[3], sd.MCAR.cov.80.AD.men.true.cl.40.50[3],
                                              sd.MCAR.cov.85.AD.men.true.cl.40.50[3], sd.MCAR.cov.90.AD.men.true.cl.40.50[3],
                                              sd.MCAR.cov.95.AD.men.true.cl.40.50[3], sd.AD.num.men.true.cov.100.40.50[3]))


plot.sd.MCAR.men.true.cl.40.50 <- ggplot(sd.MCAR.men.true.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.men.true.cl.40.50.png",
       plot = plot.sd.MCAR.men.true.cl.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# AD statistics from the truth of these individuals in the entire phylogenetic tree ---------------------------



# Truth from the entire phylogenetic tree



# Cov 35

## Vector

# Mean

vector.mean.MCAR.cov.35.AD.women.tree.trans.15.25 <- AD.MCAR.cov.35[,37]
vector.mean.MCAR.cov.35.AD.men.tree.trans.15.25 <- AD.MCAR.cov.35[,38]

vector.mean.MCAR.cov.35.AD.women.tree.trans.25.40 <- AD.MCAR.cov.35[,39]
vector.mean.MCAR.cov.35.AD.men.tree.trans.25.40 <- AD.MCAR.cov.35[,40]

vector.mean.MCAR.cov.35.AD.women.tree.trans.40.50 <- AD.MCAR.cov.35[,41]
vector.mean.MCAR.cov.35.AD.men.tree.trans.40.50 <- AD.MCAR.cov.35[,42]

# Median

vector.med.MCAR.cov.35.AD.women.tree.trans.15.25 <- AD.MCAR.cov.35[,43]
vector.med.MCAR.cov.35.AD.men.tree.trans.15.25 <- AD.MCAR.cov.35[,44]

vector.med.MCAR.cov.35.AD.women.tree.trans.25.40 <- AD.MCAR.cov.35[,45]
vector.med.MCAR.cov.35.AD.men.tree.trans.25.40 <- AD.MCAR.cov.35[,46]

vector.med.MCAR.cov.35.AD.women.tree.trans.40.50 <- AD.MCAR.cov.35[,47]
vector.med.MCAR.cov.35.AD.men.tree.trans.40.50 <- AD.MCAR.cov.35[,48]

# Standard deviation

vector.sd.MCAR.cov.35.AD.women.tree.trans.15.25 <- AD.MCAR.cov.35[,49]
vector.sd.MCAR.cov.35.AD.men.tree.trans.15.25 <- AD.MCAR.cov.35[,50]

vector.sd.MCAR.cov.35.AD.women.tree.trans.25.40 <- AD.MCAR.cov.35[,51]
vector.sd.MCAR.cov.35.AD.men.tree.trans.25.40 <- AD.MCAR.cov.35[,52]

vector.sd.MCAR.cov.35.AD.women.tree.trans.40.50 <- AD.MCAR.cov.35[,53]
vector.sd.MCAR.cov.35.AD.men.tree.trans.40.50 <- AD.MCAR.cov.35[,54]


## Summarised

# Mean

mean.MCAR.cov.35.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.35[,37])
mean.MCAR.cov.35.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.35[,38])

mean.MCAR.cov.35.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.35[,39])
mean.MCAR.cov.35.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.35[,40])

mean.MCAR.cov.35.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.35[,41])
mean.MCAR.cov.35.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.35[,42])

# Median

med.MCAR.cov.35.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.35[,43])
med.MCAR.cov.35.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.35[,44])

med.MCAR.cov.35.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.35[,45])
med.MCAR.cov.35.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.35[,46])

med.MCAR.cov.35.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.35[,47])
med.MCAR.cov.35.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.35[,48])

# Standard deviation

sd.MCAR.cov.35.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.35[,49])
sd.MCAR.cov.35.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.35[,50])

sd.MCAR.cov.35.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.35[,51])
sd.MCAR.cov.35.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.35[,52])

sd.MCAR.cov.35.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.35[,53])
sd.MCAR.cov.35.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.35[,54])


# Cov 40


## Vector

# Mean

vector.mean.MCAR.cov.40.AD.women.tree.trans.15.25 <- AD.MCAR.cov.40[,37]
vector.mean.MCAR.cov.40.AD.men.tree.trans.15.25 <- AD.MCAR.cov.40[,38]

vector.mean.MCAR.cov.40.AD.women.tree.trans.25.40 <- AD.MCAR.cov.40[,39]
vector.mean.MCAR.cov.40.AD.men.tree.trans.25.40 <- AD.MCAR.cov.40[,40]

vector.mean.MCAR.cov.40.AD.women.tree.trans.40.50 <- AD.MCAR.cov.40[,41]
vector.mean.MCAR.cov.40.AD.men.tree.trans.40.50 <- AD.MCAR.cov.40[,42]

# Median

vector.med.MCAR.cov.40.AD.women.tree.trans.15.25 <- AD.MCAR.cov.40[,43]
vector.med.MCAR.cov.40.AD.men.tree.trans.15.25 <- AD.MCAR.cov.40[,44]

vector.med.MCAR.cov.40.AD.women.tree.trans.25.40 <- AD.MCAR.cov.40[,45]
vector.med.MCAR.cov.40.AD.men.tree.trans.25.40 <- AD.MCAR.cov.40[,46]

vector.med.MCAR.cov.40.AD.women.tree.trans.40.50 <- AD.MCAR.cov.40[,47]
vector.med.MCAR.cov.40.AD.men.tree.trans.40.50 <- AD.MCAR.cov.40[,48]

# Standard deviation

vector.sd.MCAR.cov.40.AD.women.tree.trans.15.25 <- AD.MCAR.cov.40[,49]
vector.sd.MCAR.cov.40.AD.men.tree.trans.15.25 <- AD.MCAR.cov.40[,50]

vector.sd.MCAR.cov.40.AD.women.tree.trans.25.40 <- AD.MCAR.cov.40[,51]
vector.sd.MCAR.cov.40.AD.men.tree.trans.25.40 <- AD.MCAR.cov.40[,52]

vector.sd.MCAR.cov.40.AD.women.tree.trans.40.50 <- AD.MCAR.cov.40[,53]
vector.sd.MCAR.cov.40.AD.men.tree.trans.40.50 <- AD.MCAR.cov.40[,54]



## Summarised


# Mean

mean.MCAR.cov.40.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.40[,37])
mean.MCAR.cov.40.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.40[,38])

mean.MCAR.cov.40.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.40[,39])
mean.MCAR.cov.40.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.40[,40])

mean.MCAR.cov.40.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.40[,41])
mean.MCAR.cov.40.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.40[,42])

# Median

med.MCAR.cov.40.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.40[,43])
med.MCAR.cov.40.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.40[,44])

med.MCAR.cov.40.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.40[,45])
med.MCAR.cov.40.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.40[,46])

med.MCAR.cov.40.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.40[,47])
med.MCAR.cov.40.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.40[,48])

# Standard deviation

sd.MCAR.cov.40.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.40[,49])
sd.MCAR.cov.40.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.40[,50])

sd.MCAR.cov.40.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.40[,51])
sd.MCAR.cov.40.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.40[,52])

sd.MCAR.cov.40.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.40[,53])
sd.MCAR.cov.40.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.40[,54])



# Cov 45



# Vector


# Mean

vector.mean.MCAR.cov.45.AD.women.tree.trans.15.25 <- AD.MCAR.cov.45[,37]
vector.mean.MCAR.cov.45.AD.men.tree.trans.15.25 <- AD.MCAR.cov.45[,38]

vector.mean.MCAR.cov.45.AD.women.tree.trans.25.40 <- AD.MCAR.cov.45[,39]
vector.mean.MCAR.cov.45.AD.men.tree.trans.25.40 <- AD.MCAR.cov.45[,40]

vector.mean.MCAR.cov.45.AD.women.tree.trans.40.50 <- AD.MCAR.cov.45[,41]
vector.mean.MCAR.cov.45.AD.men.tree.trans.40.50 <- AD.MCAR.cov.45[,42]

# Median

vector.med.MCAR.cov.45.AD.women.tree.trans.15.25 <- AD.MCAR.cov.45[,43]
vector.med.MCAR.cov.45.AD.men.tree.trans.15.25 <- AD.MCAR.cov.45[,44]

vector.med.MCAR.cov.45.AD.women.tree.trans.25.40 <- AD.MCAR.cov.45[,45]
vector.med.MCAR.cov.45.AD.men.tree.trans.25.40 <- AD.MCAR.cov.45[,46]

vector.med.MCAR.cov.45.AD.women.tree.trans.40.50 <- AD.MCAR.cov.45[,47]
vector.med.MCAR.cov.45.AD.men.tree.trans.40.50 <- AD.MCAR.cov.45[,48]

# Standard deviation

vector.sd.MCAR.cov.45.AD.women.tree.trans.15.25 <- AD.MCAR.cov.45[,49]
vector.sd.MCAR.cov.45.AD.men.tree.trans.15.25 <- AD.MCAR.cov.45[,50]

vector.sd.MCAR.cov.45.AD.women.tree.trans.25.40 <- AD.MCAR.cov.45[,51]
vector.sd.MCAR.cov.45.AD.men.tree.trans.25.40 <- AD.MCAR.cov.45[,52]

vector.sd.MCAR.cov.45.AD.women.tree.trans.40.50 <- AD.MCAR.cov.45[,53]
vector.sd.MCAR.cov.45.AD.men.tree.trans.40.50 <- AD.MCAR.cov.45[,54]



## Summarised


# Mean

mean.MCAR.cov.45.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.45[,37])
mean.MCAR.cov.45.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.45[,38])

mean.MCAR.cov.45.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.45[,39])
mean.MCAR.cov.45.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.45[,40])

mean.MCAR.cov.45.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.45[,41])
mean.MCAR.cov.45.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.45[,42])

# Median

med.MCAR.cov.45.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.45[,43])
med.MCAR.cov.45.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.45[,44])

med.MCAR.cov.45.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.45[,45])
med.MCAR.cov.45.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.45[,46])

med.MCAR.cov.45.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.45[,47])
med.MCAR.cov.45.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.45[,48])

# Standard deviation

sd.MCAR.cov.45.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.45[,49])
sd.MCAR.cov.45.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.45[,50])

sd.MCAR.cov.45.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.45[,51])
sd.MCAR.cov.45.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.45[,52])

sd.MCAR.cov.45.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.45[,53])
sd.MCAR.cov.45.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.45[,54])


# Cov 50


## Vector


# Mean

vector.mean.MCAR.cov.50.AD.women.tree.trans.15.25 <- AD.MCAR.cov.50[,37]
vector.mean.MCAR.cov.50.AD.men.tree.trans.15.25 <- AD.MCAR.cov.50[,38]

vector.mean.MCAR.cov.50.AD.women.tree.trans.25.40 <- AD.MCAR.cov.50[,39]
vector.mean.MCAR.cov.50.AD.men.tree.trans.25.40 <- AD.MCAR.cov.50[,40]

vector.mean.MCAR.cov.50.AD.women.tree.trans.40.50 <- AD.MCAR.cov.50[,41]
vector.mean.MCAR.cov.50.AD.men.tree.trans.40.50 <- AD.MCAR.cov.50[,42]

# Median

vector.med.MCAR.cov.50.AD.women.tree.trans.15.25 <- AD.MCAR.cov.50[,43]
vector.med.MCAR.cov.50.AD.men.tree.trans.15.25 <- AD.MCAR.cov.50[,44]

vector.med.MCAR.cov.50.AD.women.tree.trans.25.40 <- AD.MCAR.cov.50[,45]
vector.med.MCAR.cov.50.AD.men.tree.trans.25.40 <- AD.MCAR.cov.50[,46]

vector.med.MCAR.cov.50.AD.women.tree.trans.40.50 <- AD.MCAR.cov.50[,47]
vector.med.MCAR.cov.50.AD.men.tree.trans.40.50 <- AD.MCAR.cov.50[,48]

# Standard deviation

vector.sd.MCAR.cov.50.AD.women.tree.trans.15.25 <- AD.MCAR.cov.50[,49]
vector.sd.MCAR.cov.50.AD.men.tree.trans.15.25 <- AD.MCAR.cov.50[,50]

vector.sd.MCAR.cov.50.AD.women.tree.trans.25.40 <- AD.MCAR.cov.50[,51]
vector.sd.MCAR.cov.50.AD.men.tree.trans.25.40 <- AD.MCAR.cov.50[,52]

vector.sd.MCAR.cov.50.AD.women.tree.trans.40.50 <- AD.MCAR.cov.50[,53]
vector.sd.MCAR.cov.50.AD.men.tree.trans.40.50 <- AD.MCAR.cov.50[,54]



## Summarised


# Mean

mean.MCAR.cov.50.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.50[,37])
mean.MCAR.cov.50.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.50[,38])

mean.MCAR.cov.50.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.50[,39])
mean.MCAR.cov.50.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.50[,40])

mean.MCAR.cov.50.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.50[,41])
mean.MCAR.cov.50.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.50[,42])

# Median

med.MCAR.cov.50.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.50[,43])
med.MCAR.cov.50.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.50[,44])

med.MCAR.cov.50.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.50[,45])
med.MCAR.cov.50.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.50[,46])

med.MCAR.cov.50.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.50[,47])
med.MCAR.cov.50.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.50[,48])

# Standard deviation

sd.MCAR.cov.50.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.50[,49])
sd.MCAR.cov.50.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.50[,50])

sd.MCAR.cov.50.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.50[,51])
sd.MCAR.cov.50.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.50[,52])

sd.MCAR.cov.50.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.50[,53])
sd.MCAR.cov.50.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.50[,54])



# Cov 55



## Vector


# Mean

vector.mean.MCAR.cov.55.AD.women.tree.trans.15.25 <- AD.MCAR.cov.55[,37]
vector.mean.MCAR.cov.55.AD.men.tree.trans.15.25 <- AD.MCAR.cov.55[,38]

vector.mean.MCAR.cov.55.AD.women.tree.trans.25.40 <- AD.MCAR.cov.55[,39]
vector.mean.MCAR.cov.55.AD.men.tree.trans.25.40 <- AD.MCAR.cov.55[,40]

vector.mean.MCAR.cov.55.AD.women.tree.trans.40.50 <- AD.MCAR.cov.55[,41]
vector.mean.MCAR.cov.55.AD.men.tree.trans.40.50 <- AD.MCAR.cov.55[,42]

# Median

vector.med.MCAR.cov.55.AD.women.tree.trans.15.25 <- AD.MCAR.cov.55[,43]
vector.med.MCAR.cov.55.AD.men.tree.trans.15.25 <- AD.MCAR.cov.55[,44]

vector.med.MCAR.cov.55.AD.women.tree.trans.25.40 <- AD.MCAR.cov.55[,45]
vector.med.MCAR.cov.55.AD.men.tree.trans.25.40 <- AD.MCAR.cov.55[,46]

vector.med.MCAR.cov.55.AD.women.tree.trans.40.50 <- AD.MCAR.cov.55[,47]
vector.med.MCAR.cov.55.AD.men.tree.trans.40.50 <- AD.MCAR.cov.55[,48]

# Standard deviation

vector.sd.MCAR.cov.55.AD.women.tree.trans.15.25 <- AD.MCAR.cov.55[,49]
vector.sd.MCAR.cov.55.AD.men.tree.trans.15.25 <- AD.MCAR.cov.55[,50]

vector.sd.MCAR.cov.55.AD.women.tree.trans.25.40 <- AD.MCAR.cov.55[,51]
vector.sd.MCAR.cov.55.AD.men.tree.trans.25.40 <- AD.MCAR.cov.55[,52]

vector.sd.MCAR.cov.55.AD.women.tree.trans.40.50 <- AD.MCAR.cov.55[,53]
vector.sd.MCAR.cov.55.AD.men.tree.trans.40.50 <- AD.MCAR.cov.55[,54]



## Summarised


# Mean

mean.MCAR.cov.55.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.55[,37])
mean.MCAR.cov.55.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.55[,38])

mean.MCAR.cov.55.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.55[,39])
mean.MCAR.cov.55.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.55[,40])

mean.MCAR.cov.55.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.55[,41])
mean.MCAR.cov.55.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.55[,42])

# Median

med.MCAR.cov.55.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.55[,43])
med.MCAR.cov.55.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.55[,44])

med.MCAR.cov.55.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.55[,45])
med.MCAR.cov.55.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.55[,46])

med.MCAR.cov.55.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.55[,47])
med.MCAR.cov.55.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.55[,48])

# Standard deviation

sd.MCAR.cov.55.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.55[,49])
sd.MCAR.cov.55.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.55[,50])

sd.MCAR.cov.55.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.55[,51])
sd.MCAR.cov.55.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.55[,52])

sd.MCAR.cov.55.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.55[,53])
sd.MCAR.cov.55.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.55[,54])



# Cov 60


## Vector

# Mean

vector.mean.MCAR.cov.60.AD.women.tree.trans.15.25 <- AD.MCAR.cov.60[,37]
vector.mean.MCAR.cov.60.AD.men.tree.trans.15.25 <- AD.MCAR.cov.60[,38]

vector.mean.MCAR.cov.60.AD.women.tree.trans.25.40 <- AD.MCAR.cov.60[,39]
vector.mean.MCAR.cov.60.AD.men.tree.trans.25.40 <- AD.MCAR.cov.60[,40]

vector.mean.MCAR.cov.60.AD.women.tree.trans.40.50 <- AD.MCAR.cov.60[,41]
vector.mean.MCAR.cov.60.AD.men.tree.trans.40.50 <- AD.MCAR.cov.60[,42]

# Median

vector.med.MCAR.cov.60.AD.women.tree.trans.15.25 <- AD.MCAR.cov.60[,43]
vector.med.MCAR.cov.60.AD.men.tree.trans.15.25 <- AD.MCAR.cov.60[,44]

vector.med.MCAR.cov.60.AD.women.tree.trans.25.40 <- AD.MCAR.cov.60[,45]
vector.med.MCAR.cov.60.AD.men.tree.trans.25.40 <- AD.MCAR.cov.60[,46]

vector.med.MCAR.cov.60.AD.women.tree.trans.40.50 <- AD.MCAR.cov.60[,47]
vector.med.MCAR.cov.60.AD.men.tree.trans.40.50 <- AD.MCAR.cov.60[,48]

# Standard deviation

vector.sd.MCAR.cov.60.AD.women.tree.trans.15.25 <- AD.MCAR.cov.60[,49]
vector.sd.MCAR.cov.60.AD.men.tree.trans.15.25 <- AD.MCAR.cov.60[,50]

vector.sd.MCAR.cov.60.AD.women.tree.trans.25.40 <- AD.MCAR.cov.60[,51]
vector.sd.MCAR.cov.60.AD.men.tree.trans.25.40 <- AD.MCAR.cov.60[,52]

vector.sd.MCAR.cov.60.AD.women.tree.trans.40.50 <- AD.MCAR.cov.60[,53]
vector.sd.MCAR.cov.60.AD.men.tree.trans.40.50 <- AD.MCAR.cov.60[,54]



## Summarised


# Mean

mean.MCAR.cov.60.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.60[,37])
mean.MCAR.cov.60.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.60[,38])

mean.MCAR.cov.60.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.60[,39])
mean.MCAR.cov.60.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.60[,40])

mean.MCAR.cov.60.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.60[,41])
mean.MCAR.cov.60.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.60[,42])

# Median

med.MCAR.cov.60.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.60[,43])
med.MCAR.cov.60.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.60[,44])

med.MCAR.cov.60.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.60[,45])
med.MCAR.cov.60.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.60[,46])

med.MCAR.cov.60.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.60[,47])
med.MCAR.cov.60.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.60[,48])

# Standard deviation

sd.MCAR.cov.60.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.60[,49])
sd.MCAR.cov.60.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.60[,50])

sd.MCAR.cov.60.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.60[,51])
sd.MCAR.cov.60.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.60[,52])

sd.MCAR.cov.60.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.60[,53])
sd.MCAR.cov.60.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.60[,54])



# Cov 65



## Vector

# Mean

vector.mean.MCAR.cov.65.AD.women.tree.trans.15.25 <- AD.MCAR.cov.65[,37]
vector.mean.MCAR.cov.65.AD.men.tree.trans.15.25 <- AD.MCAR.cov.65[,38]

vector.mean.MCAR.cov.65.AD.women.tree.trans.25.40 <- AD.MCAR.cov.65[,39]
vector.mean.MCAR.cov.65.AD.men.tree.trans.25.40 <- AD.MCAR.cov.65[,40]

vector.mean.MCAR.cov.65.AD.women.tree.trans.40.50 <- AD.MCAR.cov.65[,41]
vector.mean.MCAR.cov.65.AD.men.tree.trans.40.50 <- AD.MCAR.cov.65[,42]

# Median

vector.med.MCAR.cov.65.AD.women.tree.trans.15.25 <- AD.MCAR.cov.65[,43]
vector.med.MCAR.cov.65.AD.men.tree.trans.15.25 <- AD.MCAR.cov.65[,44]

vector.med.MCAR.cov.65.AD.women.tree.trans.25.40 <- AD.MCAR.cov.65[,45]
vector.med.MCAR.cov.65.AD.men.tree.trans.25.40 <- AD.MCAR.cov.65[,46]

vector.med.MCAR.cov.65.AD.women.tree.trans.40.50 <- AD.MCAR.cov.65[,47]
vector.med.MCAR.cov.65.AD.men.tree.trans.40.50 <- AD.MCAR.cov.65[,48]

# Standard deviation

vector.sd.MCAR.cov.65.AD.women.tree.trans.15.25 <- AD.MCAR.cov.65[,49]
vector.sd.MCAR.cov.65.AD.men.tree.trans.15.25 <- AD.MCAR.cov.65[,50]

vector.sd.MCAR.cov.65.AD.women.tree.trans.25.40 <- AD.MCAR.cov.65[,51]
vector.sd.MCAR.cov.65.AD.men.tree.trans.25.40 <- AD.MCAR.cov.65[,52]

vector.sd.MCAR.cov.65.AD.women.tree.trans.40.50 <- AD.MCAR.cov.65[,53]
vector.sd.MCAR.cov.65.AD.men.tree.trans.40.50 <- AD.MCAR.cov.65[,54]



## Summarised


# Mean

mean.MCAR.cov.65.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.65[,37])
mean.MCAR.cov.65.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.65[,38])

mean.MCAR.cov.65.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.65[,39])
mean.MCAR.cov.65.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.65[,40])

mean.MCAR.cov.65.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.65[,41])
mean.MCAR.cov.65.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.65[,42])

# Median

med.MCAR.cov.65.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.65[,43])
med.MCAR.cov.65.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.65[,44])

med.MCAR.cov.65.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.65[,45])
med.MCAR.cov.65.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.65[,46])

med.MCAR.cov.65.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.65[,47])
med.MCAR.cov.65.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.65[,48])

# Standard deviation

sd.MCAR.cov.65.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.65[,49])
sd.MCAR.cov.65.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.65[,50])

sd.MCAR.cov.65.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.65[,51])
sd.MCAR.cov.65.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.65[,52])

sd.MCAR.cov.65.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.65[,53])
sd.MCAR.cov.65.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.65[,54])



# Cov 70



## Vector


# Mean

vector.mean.MCAR.cov.70.AD.women.tree.trans.15.25 <- AD.MCAR.cov.70[,37]
vector.mean.MCAR.cov.70.AD.men.tree.trans.15.25 <- AD.MCAR.cov.70[,38]

vector.mean.MCAR.cov.70.AD.women.tree.trans.25.40 <- AD.MCAR.cov.70[,39]
vector.mean.MCAR.cov.70.AD.men.tree.trans.25.40 <- AD.MCAR.cov.70[,40]

vector.mean.MCAR.cov.70.AD.women.tree.trans.40.50 <- AD.MCAR.cov.70[,41]
vector.mean.MCAR.cov.70.AD.men.tree.trans.40.50 <- AD.MCAR.cov.70[,42]

# Median

vector.med.MCAR.cov.70.AD.women.tree.trans.15.25 <- AD.MCAR.cov.70[,43]
vector.med.MCAR.cov.70.AD.men.tree.trans.15.25 <- AD.MCAR.cov.70[,44]

vector.med.MCAR.cov.70.AD.women.tree.trans.25.40 <- AD.MCAR.cov.70[,45]
vector.med.MCAR.cov.70.AD.men.tree.trans.25.40 <- AD.MCAR.cov.70[,46]

vector.med.MCAR.cov.70.AD.women.tree.trans.40.50 <- AD.MCAR.cov.70[,47]
vector.med.MCAR.cov.70.AD.men.tree.trans.40.50 <- AD.MCAR.cov.70[,48]

# Standard deviation

vector.sd.MCAR.cov.70.AD.women.tree.trans.15.25 <- AD.MCAR.cov.70[,49]
vector.sd.MCAR.cov.70.AD.men.tree.trans.15.25 <- AD.MCAR.cov.70[,50]

vector.sd.MCAR.cov.70.AD.women.tree.trans.25.40 <- AD.MCAR.cov.70[,51]
vector.sd.MCAR.cov.70.AD.men.tree.trans.25.40 <- AD.MCAR.cov.70[,52]

vector.sd.MCAR.cov.70.AD.women.tree.trans.40.50 <- AD.MCAR.cov.70[,53]
vector.sd.MCAR.cov.70.AD.men.tree.trans.40.50 <- AD.MCAR.cov.70[,54]




## Summarised


# Mean

mean.MCAR.cov.70.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.70[,37])
mean.MCAR.cov.70.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.70[,38])

mean.MCAR.cov.70.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.70[,39])
mean.MCAR.cov.70.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.70[,40])

mean.MCAR.cov.70.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.70[,41])
mean.MCAR.cov.70.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.70[,42])

# Median

med.MCAR.cov.70.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.70[,43])
med.MCAR.cov.70.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.70[,44])

med.MCAR.cov.70.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.70[,45])
med.MCAR.cov.70.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.70[,46])

med.MCAR.cov.70.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.70[,47])
med.MCAR.cov.70.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.70[,48])

# Standard deviation

sd.MCAR.cov.70.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.70[,49])
sd.MCAR.cov.70.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.70[,50])

sd.MCAR.cov.70.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.70[,51])
sd.MCAR.cov.70.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.70[,52])

sd.MCAR.cov.70.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.70[,53])
sd.MCAR.cov.70.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.70[,54])



# Cov 75


## Vector


# Mean

vector.mean.MCAR.cov.75.AD.women.tree.trans.15.25 <- AD.MCAR.cov.75[,37]
vector.mean.MCAR.cov.75.AD.men.tree.trans.15.25 <- AD.MCAR.cov.75[,38]

vector.mean.MCAR.cov.75.AD.women.tree.trans.25.40 <- AD.MCAR.cov.75[,39]
vector.mean.MCAR.cov.75.AD.men.tree.trans.25.40 <- AD.MCAR.cov.75[,40]

vector.mean.MCAR.cov.75.AD.women.tree.trans.40.50 <- AD.MCAR.cov.75[,41]
vector.mean.MCAR.cov.75.AD.men.tree.trans.40.50 <- AD.MCAR.cov.75[,42]

# Median

vector.med.MCAR.cov.75.AD.women.tree.trans.15.25 <- AD.MCAR.cov.75[,43]
vector.med.MCAR.cov.75.AD.men.tree.trans.15.25 <- AD.MCAR.cov.75[,44]

vector.med.MCAR.cov.75.AD.women.tree.trans.25.40 <- AD.MCAR.cov.75[,45]
vector.med.MCAR.cov.75.AD.men.tree.trans.25.40 <- AD.MCAR.cov.75[,46]

vector.med.MCAR.cov.75.AD.women.tree.trans.40.50 <- AD.MCAR.cov.75[,47]
vector.med.MCAR.cov.75.AD.men.tree.trans.40.50 <- AD.MCAR.cov.75[,48]

# Standard deviation

vector.sd.MCAR.cov.75.AD.women.tree.trans.15.25 <- AD.MCAR.cov.75[,49]
vector.sd.MCAR.cov.75.AD.men.tree.trans.15.25 <- AD.MCAR.cov.75[,50]

vector.sd.MCAR.cov.75.AD.women.tree.trans.25.40 <- AD.MCAR.cov.75[,51]
vector.sd.MCAR.cov.75.AD.men.tree.trans.25.40 <- AD.MCAR.cov.75[,52]

vector.sd.MCAR.cov.75.AD.women.tree.trans.40.50 <- AD.MCAR.cov.75[,53]
vector.sd.MCAR.cov.75.AD.men.tree.trans.40.50 <- AD.MCAR.cov.75[,54]


## Summarised


# Mean

mean.MCAR.cov.75.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.75[,37])
mean.MCAR.cov.75.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.75[,38])

mean.MCAR.cov.75.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.75[,39])
mean.MCAR.cov.75.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.75[,40])

mean.MCAR.cov.75.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.75[,41])
mean.MCAR.cov.75.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.75[,42])

# Median

med.MCAR.cov.75.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.75[,43])
med.MCAR.cov.75.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.75[,44])

med.MCAR.cov.75.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.75[,45])
med.MCAR.cov.75.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.75[,46])

med.MCAR.cov.75.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.75[,47])
med.MCAR.cov.75.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.75[,48])

# Standard deviation

sd.MCAR.cov.75.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.75[,49])
sd.MCAR.cov.75.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.75[,50])

sd.MCAR.cov.75.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.75[,51])
sd.MCAR.cov.75.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.75[,52])

sd.MCAR.cov.75.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.75[,53])
sd.MCAR.cov.75.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.75[,54])



# Cov 80


## Vector

# Mean

vector.mean.MCAR.cov.80.AD.women.tree.trans.15.25 <- AD.MCAR.cov.80[,37]
vector.mean.MCAR.cov.80.AD.men.tree.trans.15.25 <- AD.MCAR.cov.80[,38]

vector.mean.MCAR.cov.80.AD.women.tree.trans.25.40 <- AD.MCAR.cov.80[,39]
vector.mean.MCAR.cov.80.AD.men.tree.trans.25.40 <- AD.MCAR.cov.80[,40]

vector.mean.MCAR.cov.80.AD.women.tree.trans.40.50 <- AD.MCAR.cov.80[,41]
vector.mean.MCAR.cov.80.AD.men.tree.trans.40.50 <- AD.MCAR.cov.80[,42]

# Median

vector.med.MCAR.cov.80.AD.women.tree.trans.15.25 <- AD.MCAR.cov.80[,43]
vector.med.MCAR.cov.80.AD.men.tree.trans.15.25 <- AD.MCAR.cov.80[,44]

vector.med.MCAR.cov.80.AD.women.tree.trans.25.40 <- AD.MCAR.cov.80[,45]
vector.med.MCAR.cov.80.AD.men.tree.trans.25.40 <- AD.MCAR.cov.80[,46]

vector.med.MCAR.cov.80.AD.women.tree.trans.40.50 <- AD.MCAR.cov.80[,47]
vector.med.MCAR.cov.80.AD.men.tree.trans.40.50 <- AD.MCAR.cov.80[,48]

# Standard deviation

vector.sd.MCAR.cov.80.AD.women.tree.trans.15.25 <- AD.MCAR.cov.80[,49]
vector.sd.MCAR.cov.80.AD.men.tree.trans.15.25 <- AD.MCAR.cov.80[,50]

vector.sd.MCAR.cov.80.AD.women.tree.trans.25.40 <- AD.MCAR.cov.80[,51]
vector.sd.MCAR.cov.80.AD.men.tree.trans.25.40 <- AD.MCAR.cov.80[,52]

vector.sd.MCAR.cov.80.AD.women.tree.trans.40.50 <- AD.MCAR.cov.80[,53]
vector.sd.MCAR.cov.80.AD.men.tree.trans.40.50 <- AD.MCAR.cov.80[,54]


## Summarised


# Mean

mean.MCAR.cov.80.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.80[,37])
mean.MCAR.cov.80.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.80[,38])

mean.MCAR.cov.80.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.80[,39])
mean.MCAR.cov.80.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.80[,40])

mean.MCAR.cov.80.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.80[,41])
mean.MCAR.cov.80.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.80[,42])

# Median

med.MCAR.cov.80.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.80[,43])
med.MCAR.cov.80.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.80[,44])

med.MCAR.cov.80.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.80[,45])
med.MCAR.cov.80.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.80[,46])

med.MCAR.cov.80.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.80[,47])
med.MCAR.cov.80.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.80[,48])

# Standard deviation

sd.MCAR.cov.80.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.80[,49])
sd.MCAR.cov.80.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.80[,50])

sd.MCAR.cov.80.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.80[,51])
sd.MCAR.cov.80.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.80[,52])

sd.MCAR.cov.80.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.80[,53])
sd.MCAR.cov.80.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.80[,54])



# Cov 85


## Vector

# Mean

vector.mean.MCAR.cov.85.AD.women.tree.trans.15.25 <- AD.MCAR.cov.85[,37]
vector.mean.MCAR.cov.85.AD.men.tree.trans.15.25 <- AD.MCAR.cov.85[,38]

vector.mean.MCAR.cov.85.AD.women.tree.trans.25.40 <- AD.MCAR.cov.85[,39]
vector.mean.MCAR.cov.85.AD.men.tree.trans.25.40 <- AD.MCAR.cov.85[,40]

vector.mean.MCAR.cov.85.AD.women.tree.trans.40.50 <- AD.MCAR.cov.85[,41]
vector.mean.MCAR.cov.85.AD.men.tree.trans.40.50 <- AD.MCAR.cov.85[,42]

# Median

vector.med.MCAR.cov.85.AD.women.tree.trans.15.25 <- AD.MCAR.cov.85[,43]
vector.med.MCAR.cov.85.AD.men.tree.trans.15.25 <- AD.MCAR.cov.85[,44]

vector.med.MCAR.cov.85.AD.women.tree.trans.25.40 <- AD.MCAR.cov.85[,45]
vector.med.MCAR.cov.85.AD.men.tree.trans.25.40 <- AD.MCAR.cov.85[,46]

vector.med.MCAR.cov.85.AD.women.tree.trans.40.50 <- AD.MCAR.cov.85[,47]
vector.med.MCAR.cov.85.AD.men.tree.trans.40.50 <- AD.MCAR.cov.85[,48]

# Standard deviation

vector.sd.MCAR.cov.85.AD.women.tree.trans.15.25 <- AD.MCAR.cov.85[,49]
vector.sd.MCAR.cov.85.AD.men.tree.trans.15.25 <- AD.MCAR.cov.85[,50]

vector.sd.MCAR.cov.85.AD.women.tree.trans.25.40 <- AD.MCAR.cov.85[,51]
vector.sd.MCAR.cov.85.AD.men.tree.trans.25.40 <- AD.MCAR.cov.85[,52]

vector.sd.MCAR.cov.85.AD.women.tree.trans.40.50 <- AD.MCAR.cov.85[,53]
vector.sd.MCAR.cov.85.AD.men.tree.trans.40.50 <- AD.MCAR.cov.85[,54]


## Summarised


# Mean

mean.MCAR.cov.85.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.85[,37])
mean.MCAR.cov.85.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.85[,38])

mean.MCAR.cov.85.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.85[,39])
mean.MCAR.cov.85.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.85[,40])

mean.MCAR.cov.85.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.85[,41])
mean.MCAR.cov.85.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.85[,42])

# Median

med.MCAR.cov.85.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.85[,43])
med.MCAR.cov.85.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.85[,44])

med.MCAR.cov.85.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.85[,45])
med.MCAR.cov.85.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.85[,46])

med.MCAR.cov.85.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.85[,47])
med.MCAR.cov.85.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.85[,48])

# Standard deviation

sd.MCAR.cov.85.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.85[,49])
sd.MCAR.cov.85.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.85[,50])

sd.MCAR.cov.85.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.85[,51])
sd.MCAR.cov.85.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.85[,52])

sd.MCAR.cov.85.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.85[,53])
sd.MCAR.cov.85.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.85[,54])



# Cov 90


## Vector

# Mean

vector.mean.MCAR.cov.90.AD.women.tree.trans.15.25 <- AD.MCAR.cov.90[,37]
vector.mean.MCAR.cov.90.AD.men.tree.trans.15.25 <- AD.MCAR.cov.90[,38]

vector.mean.MCAR.cov.90.AD.women.tree.trans.25.40 <- AD.MCAR.cov.90[,39]
vector.mean.MCAR.cov.90.AD.men.tree.trans.25.40 <- AD.MCAR.cov.90[,40]

vector.mean.MCAR.cov.90.AD.women.tree.trans.40.50 <- AD.MCAR.cov.90[,41]
vector.mean.MCAR.cov.90.AD.men.tree.trans.40.50 <- AD.MCAR.cov.90[,42]

# Median

vector.med.MCAR.cov.90.AD.women.tree.trans.15.25 <- AD.MCAR.cov.90[,43]
vector.med.MCAR.cov.90.AD.men.tree.trans.15.25 <- AD.MCAR.cov.90[,44]

vector.med.MCAR.cov.90.AD.women.tree.trans.25.40 <- AD.MCAR.cov.90[,45]
vector.med.MCAR.cov.90.AD.men.tree.trans.25.40 <- AD.MCAR.cov.90[,46]

vector.med.MCAR.cov.90.AD.women.tree.trans.40.50 <- AD.MCAR.cov.90[,47]
vector.med.MCAR.cov.90.AD.men.tree.trans.40.50 <- AD.MCAR.cov.90[,48]

# Standard deviation

vector.sd.MCAR.cov.90.AD.women.tree.trans.15.25 <- AD.MCAR.cov.90[,49]
vector.sd.MCAR.cov.90.AD.men.tree.trans.15.25 <- AD.MCAR.cov.90[,50]

vector.sd.MCAR.cov.90.AD.women.tree.trans.25.40 <- AD.MCAR.cov.90[,51]
vector.sd.MCAR.cov.90.AD.men.tree.trans.25.40 <- AD.MCAR.cov.90[,52]

vector.sd.MCAR.cov.90.AD.women.tree.trans.40.50 <- AD.MCAR.cov.90[,53]
vector.sd.MCAR.cov.90.AD.men.tree.trans.40.50 <- AD.MCAR.cov.90[,54]



## Summarised


# Mean

mean.MCAR.cov.90.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.90[,37])
mean.MCAR.cov.90.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.90[,38])

mean.MCAR.cov.90.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.90[,39])
mean.MCAR.cov.90.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.90[,40])

mean.MCAR.cov.90.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.90[,41])
mean.MCAR.cov.90.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.90[,42])

# Median

med.MCAR.cov.90.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.90[,43])
med.MCAR.cov.90.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.90[,44])

med.MCAR.cov.90.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.90[,45])
med.MCAR.cov.90.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.90[,46])

med.MCAR.cov.90.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.90[,47])
med.MCAR.cov.90.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.90[,48])

# Standard deviation

sd.MCAR.cov.90.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.90[,49])
sd.MCAR.cov.90.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.90[,50])

sd.MCAR.cov.90.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.90[,51])
sd.MCAR.cov.90.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.90[,52])

sd.MCAR.cov.90.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.90[,53])
sd.MCAR.cov.90.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.90[,54])




# Cov 95


## Vector

# Mean

vector.mean.MCAR.cov.95.AD.women.tree.trans.15.25 <- AD.MCAR.cov.95[,37]
vector.mean.MCAR.cov.95.AD.men.tree.trans.15.25 <- AD.MCAR.cov.95[,38]

vector.mean.MCAR.cov.95.AD.women.tree.trans.25.40 <- AD.MCAR.cov.95[,39]
vector.mean.MCAR.cov.95.AD.men.tree.trans.25.40 <- AD.MCAR.cov.95[,40]

vector.mean.MCAR.cov.95.AD.women.tree.trans.40.50 <- AD.MCAR.cov.95[,41]
vector.mean.MCAR.cov.95.AD.men.tree.trans.40.50 <- AD.MCAR.cov.95[,42]

# Median

vector.med.MCAR.cov.95.AD.women.tree.trans.15.25 <- AD.MCAR.cov.95[,43]
vector.med.MCAR.cov.95.AD.men.tree.trans.15.25 <- AD.MCAR.cov.95[,44]

vector.med.MCAR.cov.95.AD.women.tree.trans.25.40 <- AD.MCAR.cov.95[,45]
vector.med.MCAR.cov.95.AD.men.tree.trans.25.40 <- AD.MCAR.cov.95[,46]

vector.med.MCAR.cov.95.AD.women.tree.trans.40.50 <- AD.MCAR.cov.95[,47]
vector.med.MCAR.cov.95.AD.men.tree.trans.40.50 <- AD.MCAR.cov.95[,48]

# Standard deviation

vector.sd.MCAR.cov.95.AD.women.tree.trans.15.25 <- AD.MCAR.cov.95[,49]
vector.sd.MCAR.cov.95.AD.men.tree.trans.15.25 <- AD.MCAR.cov.95[,50]

vector.sd.MCAR.cov.95.AD.women.tree.trans.25.40 <- AD.MCAR.cov.95[,51]
vector.sd.MCAR.cov.95.AD.men.tree.trans.25.40 <- AD.MCAR.cov.95[,52]

vector.sd.MCAR.cov.95.AD.women.tree.trans.40.50 <- AD.MCAR.cov.95[,53]
vector.sd.MCAR.cov.95.AD.men.tree.trans.40.50 <- AD.MCAR.cov.95[,54]


## Summarised

# Mean

mean.MCAR.cov.95.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.95[,37])
mean.MCAR.cov.95.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.95[,38])

mean.MCAR.cov.95.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.95[,39])
mean.MCAR.cov.95.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.95[,40])

mean.MCAR.cov.95.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.95[,41])
mean.MCAR.cov.95.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.95[,42])

# Median

med.MCAR.cov.95.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.95[,43])
med.MCAR.cov.95.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.95[,44])

med.MCAR.cov.95.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.95[,45])
med.MCAR.cov.95.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.95[,46])

med.MCAR.cov.95.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.95[,47])
med.MCAR.cov.95.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.95[,48])

# Standard deviation

sd.MCAR.cov.95.AD.women.tree.trans.15.25 <- quant.med(AD.MCAR.cov.95[,49])
sd.MCAR.cov.95.AD.men.tree.trans.15.25 <- quant.med(AD.MCAR.cov.95[,50])

sd.MCAR.cov.95.AD.women.tree.trans.25.40 <- quant.med(AD.MCAR.cov.95[,51])
sd.MCAR.cov.95.AD.men.tree.trans.25.40 <- quant.med(AD.MCAR.cov.95[,52])

sd.MCAR.cov.95.AD.women.tree.trans.40.50 <- quant.med(AD.MCAR.cov.95[,53])
sd.MCAR.cov.95.AD.men.tree.trans.40.50 <- quant.med(AD.MCAR.cov.95[,54])



# Visualisation of AD statistics from the truth of these individuals in the entire phylogenetic tree ---------------------------




## Mean



# Women 15.25

mean.MCAR.women.tree.trans.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                               
                                               F = c(mean.MCAR.cov.35.AD.women.tree.trans.15.25[2], mean.MCAR.cov.40.AD.women.tree.trans.15.25[2],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.15.25[2], mean.MCAR.cov.50.AD.women.tree.trans.15.25[2],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.15.25[2], mean.MCAR.cov.60.AD.women.tree.trans.15.25[2],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.15.25[2], mean.MCAR.cov.70.AD.women.tree.trans.15.25[2],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.15.25[2], mean.MCAR.cov.80.AD.women.tree.trans.15.25[2],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.15.25[2], mean.MCAR.cov.90.AD.women.tree.trans.15.25[2],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.15.25[2], mean.AD.num.women.true.cov.100.15.25[2]),
                                               
                                               L = c(mean.MCAR.cov.35.AD.women.tree.trans.15.25[1], mean.MCAR.cov.40.AD.women.tree.trans.15.25[1],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.15.25[1], mean.MCAR.cov.50.AD.women.tree.trans.15.25[1],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.15.25[1], mean.MCAR.cov.60.AD.women.tree.trans.15.25[1],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.15.25[1], mean.MCAR.cov.70.AD.women.tree.trans.15.25[1],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.15.25[1], mean.MCAR.cov.80.AD.women.tree.trans.15.25[1],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.15.25[1], mean.MCAR.cov.90.AD.women.tree.trans.15.25[1],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.15.25[1], mean.AD.num.women.true.cov.100.15.25[1]),
                                               
                                               U = c(mean.MCAR.cov.35.AD.women.tree.trans.15.25[3], mean.MCAR.cov.40.AD.women.tree.trans.15.25[3],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.15.25[3], mean.MCAR.cov.50.AD.women.tree.trans.15.25[3],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.15.25[3], mean.MCAR.cov.60.AD.women.tree.trans.15.25[3],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.15.25[3], mean.MCAR.cov.70.AD.women.tree.trans.15.25[3],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.15.25[3], mean.MCAR.cov.80.AD.women.tree.trans.15.25[3],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.15.25[3], mean.MCAR.cov.90.AD.women.tree.trans.15.25[3],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.15.25[3], mean.AD.num.women.true.cov.100.15.25[3]))


plot.mean.MCAR.women.tree.trans.15.25 <- ggplot(mean.MCAR.women.tree.trans.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.women.tree.trans.15.25.png",
       plot = plot.mean.MCAR.women.tree.trans.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

mean.MCAR.men.tree.trans.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                             
                                             F = c(mean.MCAR.cov.35.AD.men.tree.trans.15.25[2], mean.MCAR.cov.40.AD.men.tree.trans.15.25[2],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.15.25[2], mean.MCAR.cov.50.AD.men.tree.trans.15.25[2],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.15.25[2], mean.MCAR.cov.60.AD.men.tree.trans.15.25[2],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.15.25[2], mean.MCAR.cov.70.AD.men.tree.trans.15.25[2],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.15.25[2], mean.MCAR.cov.80.AD.men.tree.trans.15.25[2],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.15.25[2], mean.MCAR.cov.90.AD.men.tree.trans.15.25[2],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.15.25[2], mean.AD.num.men.true.cov.100.15.25[2]),
                                             
                                             L = c(mean.MCAR.cov.35.AD.men.tree.trans.15.25[1], mean.MCAR.cov.40.AD.men.tree.trans.15.25[1],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.15.25[1], mean.MCAR.cov.50.AD.men.tree.trans.15.25[1],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.15.25[1], mean.MCAR.cov.60.AD.men.tree.trans.15.25[1],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.15.25[1], mean.MCAR.cov.70.AD.men.tree.trans.15.25[1],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.15.25[1], mean.MCAR.cov.80.AD.men.tree.trans.15.25[1],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.15.25[1], mean.MCAR.cov.90.AD.men.tree.trans.15.25[1],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.15.25[1], mean.AD.num.men.true.cov.100.15.25[1]),
                                             
                                             U = c(mean.MCAR.cov.35.AD.men.tree.trans.15.25[3], mean.MCAR.cov.40.AD.men.tree.trans.15.25[3],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.15.25[3], mean.MCAR.cov.50.AD.men.tree.trans.15.25[3],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.15.25[3], mean.MCAR.cov.60.AD.men.tree.trans.15.25[3],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.15.25[3], mean.MCAR.cov.70.AD.men.tree.trans.15.25[3],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.15.25[3], mean.MCAR.cov.80.AD.men.tree.trans.15.25[3],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.15.25[3], mean.MCAR.cov.90.AD.men.tree.trans.15.25[3],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.15.25[3], mean.AD.num.men.true.cov.100.15.25[3]))


plot.mean.MCAR.men.tree.trans.15.25 <- ggplot(mean.MCAR.men.tree.trans.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.men.tree.trans.15.25.png",
       plot = plot.mean.MCAR.men.tree.trans.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

mean.MCAR.women.tree.trans.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                               
                                               F = c(mean.MCAR.cov.35.AD.women.tree.trans.25.40[2], mean.MCAR.cov.40.AD.women.tree.trans.25.40[2],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.25.40[2], mean.MCAR.cov.50.AD.women.tree.trans.25.40[2],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.25.40[2], mean.MCAR.cov.60.AD.women.tree.trans.25.40[2],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.25.40[2], mean.MCAR.cov.70.AD.women.tree.trans.25.40[2],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.25.40[2], mean.MCAR.cov.80.AD.women.tree.trans.25.40[2],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.25.40[2], mean.MCAR.cov.90.AD.women.tree.trans.25.40[2],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.25.40[2], mean.AD.num.women.true.cov.100.25.40[2]),
                                               
                                               L = c(mean.MCAR.cov.35.AD.women.tree.trans.25.40[1], mean.MCAR.cov.40.AD.women.tree.trans.25.40[1],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.25.40[1], mean.MCAR.cov.50.AD.women.tree.trans.25.40[1],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.25.40[1], mean.MCAR.cov.60.AD.women.tree.trans.25.40[1],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.25.40[1], mean.MCAR.cov.70.AD.women.tree.trans.25.40[1],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.25.40[1], mean.MCAR.cov.80.AD.women.tree.trans.25.40[1],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.25.40[1], mean.MCAR.cov.90.AD.women.tree.trans.25.40[1],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.25.40[1], mean.AD.num.women.true.cov.100.25.40[1]),
                                               
                                               U = c(mean.MCAR.cov.35.AD.women.tree.trans.25.40[3], mean.MCAR.cov.40.AD.women.tree.trans.25.40[3],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.25.40[3], mean.MCAR.cov.50.AD.women.tree.trans.25.40[3],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.25.40[3], mean.MCAR.cov.60.AD.women.tree.trans.25.40[3],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.25.40[3], mean.MCAR.cov.70.AD.women.tree.trans.25.40[3],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.25.40[3], mean.MCAR.cov.80.AD.women.tree.trans.25.40[3],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.25.40[3], mean.MCAR.cov.90.AD.women.tree.trans.25.40[3],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.25.40[3], mean.AD.num.women.true.cov.100.25.40[3]))


plot.mean.MCAR.women.tree.trans.25.40 <- ggplot(mean.MCAR.women.tree.trans.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.women.tree.trans.25.40.png",
       plot = plot.mean.MCAR.women.tree.trans.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")

# Men: 25.40

mean.MCAR.men.tree.trans.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                             
                                             F = c(mean.MCAR.cov.35.AD.men.tree.trans.25.40[2], mean.MCAR.cov.40.AD.men.tree.trans.25.40[2],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.25.40[2], mean.MCAR.cov.50.AD.men.tree.trans.25.40[2],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.25.40[2], mean.MCAR.cov.60.AD.men.tree.trans.25.40[2],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.25.40[2], mean.MCAR.cov.70.AD.men.tree.trans.25.40[2],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.25.40[2], mean.MCAR.cov.80.AD.men.tree.trans.25.40[2],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.25.40[2], mean.MCAR.cov.90.AD.men.tree.trans.25.40[2],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.25.40[2], mean.AD.num.men.true.cov.100.25.40[2]),
                                             
                                             L = c(mean.MCAR.cov.35.AD.men.tree.trans.25.40[1], mean.MCAR.cov.40.AD.men.tree.trans.25.40[1],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.25.40[1], mean.MCAR.cov.50.AD.men.tree.trans.25.40[1],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.25.40[1], mean.MCAR.cov.60.AD.men.tree.trans.25.40[1],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.25.40[1], mean.MCAR.cov.70.AD.men.tree.trans.25.40[1],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.25.40[1], mean.MCAR.cov.80.AD.men.tree.trans.25.40[1],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.25.40[1], mean.MCAR.cov.90.AD.men.tree.trans.25.40[1],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.25.40[1], mean.AD.num.men.true.cov.100.25.40[1]),
                                             
                                             U = c(mean.MCAR.cov.35.AD.men.tree.trans.25.40[3], mean.MCAR.cov.40.AD.men.tree.trans.25.40[3],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.25.40[3], mean.MCAR.cov.50.AD.men.tree.trans.25.40[3],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.25.40[3], mean.MCAR.cov.60.AD.men.tree.trans.25.40[3],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.25.40[3], mean.MCAR.cov.70.AD.men.tree.trans.25.40[3],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.25.40[3], mean.MCAR.cov.80.AD.men.tree.trans.25.40[3],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.25.40[3], mean.MCAR.cov.90.AD.men.tree.trans.25.40[3],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.25.40[3], mean.AD.num.men.true.cov.100.25.40[3]))


plot.mean.MCAR.men.tree.trans.25.40 <- ggplot(mean.MCAR.men.tree.trans.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.mean.MCAR.men.tree.trans.25.40.png",
       plot = plot.mean.MCAR.men.tree.trans.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 40.50

mean.MCAR.women.tree.trans.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                               
                                               F = c(mean.MCAR.cov.35.AD.women.tree.trans.40.50[2], mean.MCAR.cov.40.AD.women.tree.trans.40.50[2],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.40.50[2], mean.MCAR.cov.50.AD.women.tree.trans.40.50[2],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.40.50[2], mean.MCAR.cov.60.AD.women.tree.trans.40.50[2],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.40.50[2], mean.MCAR.cov.70.AD.women.tree.trans.40.50[2],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.40.50[2], mean.MCAR.cov.80.AD.women.tree.trans.40.50[2],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.40.50[2], mean.MCAR.cov.90.AD.women.tree.trans.40.50[2],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.40.50[2], mean.AD.num.women.true.cov.100.25.40[2]),
                                               
                                               L = c(mean.MCAR.cov.35.AD.women.tree.trans.40.50[1], mean.MCAR.cov.40.AD.women.tree.trans.40.50[1],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.40.50[1], mean.MCAR.cov.50.AD.women.tree.trans.40.50[1],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.40.50[1], mean.MCAR.cov.60.AD.women.tree.trans.40.50[1],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.40.50[1], mean.MCAR.cov.70.AD.women.tree.trans.40.50[1],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.40.50[1], mean.MCAR.cov.80.AD.women.tree.trans.40.50[1],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.40.50[1], mean.MCAR.cov.90.AD.women.tree.trans.40.50[1],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.40.50[1], mean.AD.num.women.true.cov.100.25.40[1]),
                                               
                                               U = c(mean.MCAR.cov.35.AD.women.tree.trans.40.50[3], mean.MCAR.cov.40.AD.women.tree.trans.40.50[3],
                                                     mean.MCAR.cov.45.AD.women.tree.trans.40.50[3], mean.MCAR.cov.50.AD.women.tree.trans.40.50[3],
                                                     mean.MCAR.cov.55.AD.women.tree.trans.40.50[3], mean.MCAR.cov.60.AD.women.tree.trans.40.50[3],
                                                     mean.MCAR.cov.65.AD.women.tree.trans.40.50[3], mean.MCAR.cov.70.AD.women.tree.trans.40.50[3],
                                                     mean.MCAR.cov.75.AD.women.tree.trans.40.50[3], mean.MCAR.cov.80.AD.women.tree.trans.40.50[3],
                                                     mean.MCAR.cov.85.AD.women.tree.trans.40.50[3], mean.MCAR.cov.90.AD.women.tree.trans.40.50[3],
                                                     mean.MCAR.cov.95.AD.women.tree.trans.40.50[3], mean.AD.num.women.true.cov.100.25.40[3]))


plot.mean.MCAR.women.tree.trans.40.50 <- ggplot(mean.MCAR.women.tree.trans.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.mean.MCAR.women.tree.trans.40.50.png",
       plot = plot.mean.MCAR.women.tree.trans.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

mean.MCAR.men.tree.trans.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                             
                                             F = c(mean.MCAR.cov.35.AD.men.tree.trans.40.50[2], mean.MCAR.cov.40.AD.men.tree.trans.40.50[2],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.40.50[2], mean.MCAR.cov.50.AD.men.tree.trans.40.50[2],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.40.50[2], mean.MCAR.cov.60.AD.men.tree.trans.40.50[2],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.40.50[2], mean.MCAR.cov.70.AD.men.tree.trans.40.50[2],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.40.50[2], mean.MCAR.cov.80.AD.men.tree.trans.40.50[2],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.40.50[2], mean.MCAR.cov.90.AD.men.tree.trans.40.50[2],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.40.50[2], mean.AD.num.men.true.cov.100.40.50[2]),
                                             
                                             L = c(mean.MCAR.cov.35.AD.men.tree.trans.40.50[1], mean.MCAR.cov.40.AD.men.tree.trans.40.50[1],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.40.50[1], mean.MCAR.cov.50.AD.men.tree.trans.40.50[1],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.40.50[1], mean.MCAR.cov.60.AD.men.tree.trans.40.50[1],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.40.50[1], mean.MCAR.cov.70.AD.men.tree.trans.40.50[1],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.40.50[1], mean.MCAR.cov.80.AD.men.tree.trans.40.50[1],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.40.50[1], mean.MCAR.cov.90.AD.men.tree.trans.40.50[1],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.40.50[1], mean.AD.num.men.true.cov.100.40.50[1]),
                                             
                                             U = c(mean.MCAR.cov.35.AD.men.tree.trans.40.50[3], mean.MCAR.cov.40.AD.men.tree.trans.40.50[3],
                                                   mean.MCAR.cov.45.AD.men.tree.trans.40.50[3], mean.MCAR.cov.50.AD.men.tree.trans.40.50[3],
                                                   mean.MCAR.cov.55.AD.men.tree.trans.40.50[3], mean.MCAR.cov.60.AD.men.tree.trans.40.50[3],
                                                   mean.MCAR.cov.65.AD.men.tree.trans.40.50[3], mean.MCAR.cov.70.AD.men.tree.trans.40.50[3],
                                                   mean.MCAR.cov.75.AD.men.tree.trans.40.50[3], mean.MCAR.cov.80.AD.men.tree.trans.40.50[3],
                                                   mean.MCAR.cov.85.AD.men.tree.trans.40.50[3], mean.MCAR.cov.90.AD.men.tree.trans.40.50[3],
                                                   mean.MCAR.cov.95.AD.men.tree.trans.40.50[3], mean.AD.num.men.true.cov.100.40.50[3]))


plot.mean.MCAR.men.tree.trans.40.50 <- ggplot(mean.MCAR.men.tree.trans.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.mean.MCAR.men.tree.trans.40.50.png",
       plot = plot.mean.MCAR.men.tree.trans.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




## Median

# Women 15.25

med.MCAR.women.tree.trans.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(med.MCAR.cov.35.AD.women.tree.trans.15.25[2], med.MCAR.cov.40.AD.women.tree.trans.15.25[2],
                                                    med.MCAR.cov.45.AD.women.tree.trans.15.25[2], med.MCAR.cov.50.AD.women.tree.trans.15.25[2],
                                                    med.MCAR.cov.55.AD.women.tree.trans.15.25[2], med.MCAR.cov.60.AD.women.tree.trans.15.25[2],
                                                    med.MCAR.cov.65.AD.women.tree.trans.15.25[2], med.MCAR.cov.70.AD.women.tree.trans.15.25[2],
                                                    med.MCAR.cov.75.AD.women.tree.trans.15.25[2], med.MCAR.cov.80.AD.women.tree.trans.15.25[2],
                                                    med.MCAR.cov.85.AD.women.tree.trans.15.25[2], med.MCAR.cov.90.AD.women.tree.trans.15.25[2],
                                                    med.MCAR.cov.95.AD.women.tree.trans.15.25[2], med.AD.num.women.true.cov.100.15.25[2]),
                                              
                                              L = c(med.MCAR.cov.35.AD.women.tree.trans.15.25[1], med.MCAR.cov.40.AD.women.tree.trans.15.25[1],
                                                    med.MCAR.cov.45.AD.women.tree.trans.15.25[1], med.MCAR.cov.50.AD.women.tree.trans.15.25[1],
                                                    med.MCAR.cov.55.AD.women.tree.trans.15.25[1], med.MCAR.cov.60.AD.women.tree.trans.15.25[1],
                                                    med.MCAR.cov.65.AD.women.tree.trans.15.25[1], med.MCAR.cov.70.AD.women.tree.trans.15.25[1],
                                                    med.MCAR.cov.75.AD.women.tree.trans.15.25[1], med.MCAR.cov.80.AD.women.tree.trans.15.25[1],
                                                    med.MCAR.cov.85.AD.women.tree.trans.15.25[1], med.MCAR.cov.90.AD.women.tree.trans.15.25[1],
                                                    med.MCAR.cov.95.AD.women.tree.trans.15.25[1], med.AD.num.women.true.cov.100.15.25[1]),
                                              
                                              U = c(med.MCAR.cov.35.AD.women.tree.trans.15.25[3], med.MCAR.cov.40.AD.women.tree.trans.15.25[3],
                                                    med.MCAR.cov.45.AD.women.tree.trans.15.25[3], med.MCAR.cov.50.AD.women.tree.trans.15.25[3],
                                                    med.MCAR.cov.55.AD.women.tree.trans.15.25[3], med.MCAR.cov.60.AD.women.tree.trans.15.25[3],
                                                    med.MCAR.cov.65.AD.women.tree.trans.15.25[3], med.MCAR.cov.70.AD.women.tree.trans.15.25[3],
                                                    med.MCAR.cov.75.AD.women.tree.trans.15.25[3], med.MCAR.cov.80.AD.women.tree.trans.15.25[3],
                                                    med.MCAR.cov.85.AD.women.tree.trans.15.25[3], med.MCAR.cov.90.AD.women.tree.trans.15.25[3],
                                                    med.MCAR.cov.95.AD.women.tree.trans.15.25[3], med.AD.num.women.true.cov.100.15.25[3]))


plot.med.MCAR.women.tree.trans.15.25 <- ggplot(med.MCAR.women.tree.trans.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.tree.trans.15.25.png",
       plot = plot.med.MCAR.women.tree.trans.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

med.MCAR.men.tree.trans.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(med.MCAR.cov.35.AD.men.tree.trans.15.25[2], med.MCAR.cov.40.AD.men.tree.trans.15.25[2],
                                                  med.MCAR.cov.45.AD.men.tree.trans.15.25[2], med.MCAR.cov.50.AD.men.tree.trans.15.25[2],
                                                  med.MCAR.cov.55.AD.men.tree.trans.15.25[2], med.MCAR.cov.60.AD.men.tree.trans.15.25[2],
                                                  med.MCAR.cov.65.AD.men.tree.trans.15.25[2], med.MCAR.cov.70.AD.men.tree.trans.15.25[2],
                                                  med.MCAR.cov.75.AD.men.tree.trans.15.25[2], med.MCAR.cov.80.AD.men.tree.trans.15.25[2],
                                                  med.MCAR.cov.85.AD.men.tree.trans.15.25[2], med.MCAR.cov.90.AD.men.tree.trans.15.25[2],
                                                  med.MCAR.cov.95.AD.men.tree.trans.15.25[2], med.AD.num.men.true.cov.100.15.25[2]),
                                            
                                            L = c(med.MCAR.cov.35.AD.men.tree.trans.15.25[1], med.MCAR.cov.40.AD.men.tree.trans.15.25[1],
                                                  med.MCAR.cov.45.AD.men.tree.trans.15.25[1], med.MCAR.cov.50.AD.men.tree.trans.15.25[1],
                                                  med.MCAR.cov.55.AD.men.tree.trans.15.25[1], med.MCAR.cov.60.AD.men.tree.trans.15.25[1],
                                                  med.MCAR.cov.65.AD.men.tree.trans.15.25[1], med.MCAR.cov.70.AD.men.tree.trans.15.25[1],
                                                  med.MCAR.cov.75.AD.men.tree.trans.15.25[1], med.MCAR.cov.80.AD.men.tree.trans.15.25[1],
                                                  med.MCAR.cov.85.AD.men.tree.trans.15.25[1], med.MCAR.cov.90.AD.men.tree.trans.15.25[1],
                                                  med.MCAR.cov.95.AD.men.tree.trans.15.25[1], med.AD.num.men.true.cov.100.15.25[1]),
                                            
                                            U = c(med.MCAR.cov.35.AD.men.tree.trans.15.25[3], med.MCAR.cov.40.AD.men.tree.trans.15.25[3],
                                                  med.MCAR.cov.45.AD.men.tree.trans.15.25[3], med.MCAR.cov.50.AD.men.tree.trans.15.25[3],
                                                  med.MCAR.cov.55.AD.men.tree.trans.15.25[3], med.MCAR.cov.60.AD.men.tree.trans.15.25[3],
                                                  med.MCAR.cov.65.AD.men.tree.trans.15.25[3], med.MCAR.cov.70.AD.men.tree.trans.15.25[3],
                                                  med.MCAR.cov.75.AD.men.tree.trans.15.25[3], med.MCAR.cov.80.AD.men.tree.trans.15.25[3],
                                                  med.MCAR.cov.85.AD.men.tree.trans.15.25[3], med.MCAR.cov.90.AD.men.tree.trans.15.25[3],
                                                  med.MCAR.cov.95.AD.men.tree.trans.15.25[3], med.AD.num.men.true.cov.100.15.25[3]))


plot.med.MCAR.men.tree.trans.15.25 <- ggplot(med.MCAR.men.tree.trans.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.tree.trans.15.25.png",
       plot = plot.med.MCAR.men.tree.trans.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

med.MCAR.women.tree.trans.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(med.MCAR.cov.35.AD.women.tree.trans.25.40[2], med.MCAR.cov.40.AD.women.tree.trans.25.40[2],
                                                    med.MCAR.cov.45.AD.women.tree.trans.25.40[2], med.MCAR.cov.50.AD.women.tree.trans.25.40[2],
                                                    med.MCAR.cov.55.AD.women.tree.trans.25.40[2], med.MCAR.cov.60.AD.women.tree.trans.25.40[2],
                                                    med.MCAR.cov.65.AD.women.tree.trans.25.40[2], med.MCAR.cov.70.AD.women.tree.trans.25.40[2],
                                                    med.MCAR.cov.75.AD.women.tree.trans.25.40[2], med.MCAR.cov.80.AD.women.tree.trans.25.40[2],
                                                    med.MCAR.cov.85.AD.women.tree.trans.25.40[2], med.MCAR.cov.90.AD.women.tree.trans.25.40[2],
                                                    med.MCAR.cov.95.AD.women.tree.trans.25.40[2], med.AD.num.women.true.cov.100.25.40[2]),
                                              
                                              L = c(med.MCAR.cov.35.AD.women.tree.trans.25.40[1], med.MCAR.cov.40.AD.women.tree.trans.25.40[1],
                                                    med.MCAR.cov.45.AD.women.tree.trans.25.40[1], med.MCAR.cov.50.AD.women.tree.trans.25.40[1],
                                                    med.MCAR.cov.55.AD.women.tree.trans.25.40[1], med.MCAR.cov.60.AD.women.tree.trans.25.40[1],
                                                    med.MCAR.cov.65.AD.women.tree.trans.25.40[1], med.MCAR.cov.70.AD.women.tree.trans.25.40[1],
                                                    med.MCAR.cov.75.AD.women.tree.trans.25.40[1], med.MCAR.cov.80.AD.women.tree.trans.25.40[1],
                                                    med.MCAR.cov.85.AD.women.tree.trans.25.40[1], med.MCAR.cov.90.AD.women.tree.trans.25.40[1],
                                                    med.MCAR.cov.95.AD.women.tree.trans.25.40[1], med.AD.num.women.true.cov.100.25.40[1]),
                                              
                                              U = c(med.MCAR.cov.35.AD.women.tree.trans.25.40[3], med.MCAR.cov.40.AD.women.tree.trans.25.40[3],
                                                    med.MCAR.cov.45.AD.women.tree.trans.25.40[3], med.MCAR.cov.50.AD.women.tree.trans.25.40[3],
                                                    med.MCAR.cov.55.AD.women.tree.trans.25.40[3], med.MCAR.cov.60.AD.women.tree.trans.25.40[3],
                                                    med.MCAR.cov.65.AD.women.tree.trans.25.40[3], med.MCAR.cov.70.AD.women.tree.trans.25.40[3],
                                                    med.MCAR.cov.75.AD.women.tree.trans.25.40[3], med.MCAR.cov.80.AD.women.tree.trans.25.40[3],
                                                    med.MCAR.cov.85.AD.women.tree.trans.25.40[3], med.MCAR.cov.90.AD.women.tree.trans.25.40[3],
                                                    med.MCAR.cov.95.AD.women.tree.trans.25.40[3], med.AD.num.women.true.cov.100.25.40[3]))


plot.med.MCAR.women.tree.trans.25.40 <- ggplot(med.MCAR.women.tree.trans.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.tree.trans.25.40.png",
       plot = plot.med.MCAR.women.tree.trans.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Men: 25.40

med.MCAR.men.tree.trans.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(med.MCAR.cov.35.AD.men.tree.trans.25.40[2], med.MCAR.cov.40.AD.men.tree.trans.25.40[2],
                                                  med.MCAR.cov.45.AD.men.tree.trans.25.40[2], med.MCAR.cov.50.AD.men.tree.trans.25.40[2],
                                                  med.MCAR.cov.55.AD.men.tree.trans.25.40[2], med.MCAR.cov.60.AD.men.tree.trans.25.40[2],
                                                  med.MCAR.cov.65.AD.men.tree.trans.25.40[2], med.MCAR.cov.70.AD.men.tree.trans.25.40[2],
                                                  med.MCAR.cov.75.AD.men.tree.trans.25.40[2], med.MCAR.cov.80.AD.men.tree.trans.25.40[2],
                                                  med.MCAR.cov.85.AD.men.tree.trans.25.40[2], med.MCAR.cov.90.AD.men.tree.trans.25.40[2],
                                                  med.MCAR.cov.95.AD.men.tree.trans.25.40[2], med.AD.num.men.true.cov.100.25.40[2]),
                                            
                                            L = c(med.MCAR.cov.35.AD.men.tree.trans.25.40[1], med.MCAR.cov.40.AD.men.tree.trans.25.40[1],
                                                  med.MCAR.cov.45.AD.men.tree.trans.25.40[1], med.MCAR.cov.50.AD.men.tree.trans.25.40[1],
                                                  med.MCAR.cov.55.AD.men.tree.trans.25.40[1], med.MCAR.cov.60.AD.men.tree.trans.25.40[1],
                                                  med.MCAR.cov.65.AD.men.tree.trans.25.40[1], med.MCAR.cov.70.AD.men.tree.trans.25.40[1],
                                                  med.MCAR.cov.75.AD.men.tree.trans.25.40[1], med.MCAR.cov.80.AD.men.tree.trans.25.40[1],
                                                  med.MCAR.cov.85.AD.men.tree.trans.25.40[1], med.MCAR.cov.90.AD.men.tree.trans.25.40[1],
                                                  med.MCAR.cov.95.AD.men.tree.trans.25.40[1], med.AD.num.men.true.cov.100.25.40[1]),
                                            
                                            U = c(med.MCAR.cov.35.AD.men.tree.trans.25.40[3], med.MCAR.cov.40.AD.men.tree.trans.25.40[3],
                                                  med.MCAR.cov.45.AD.men.tree.trans.25.40[3], med.MCAR.cov.50.AD.men.tree.trans.25.40[3],
                                                  med.MCAR.cov.55.AD.men.tree.trans.25.40[3], med.MCAR.cov.60.AD.men.tree.trans.25.40[3],
                                                  med.MCAR.cov.65.AD.men.tree.trans.25.40[3], med.MCAR.cov.70.AD.men.tree.trans.25.40[3],
                                                  med.MCAR.cov.75.AD.men.tree.trans.25.40[3], med.MCAR.cov.80.AD.men.tree.trans.25.40[3],
                                                  med.MCAR.cov.85.AD.men.tree.trans.25.40[3], med.MCAR.cov.90.AD.men.tree.trans.25.40[3],
                                                  med.MCAR.cov.95.AD.men.tree.trans.25.40[3], med.AD.num.men.true.cov.100.25.40[3]))


plot.med.MCAR.men.tree.trans.25.40 <- ggplot(med.MCAR.men.tree.trans.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.tree.trans.25.40.png",
       plot = plot.med.MCAR.men.tree.trans.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 40.50

med.MCAR.women.tree.trans.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                              
                                              F = c(med.MCAR.cov.35.AD.women.tree.trans.40.50[2], med.MCAR.cov.40.AD.women.tree.trans.40.50[2],
                                                    med.MCAR.cov.45.AD.women.tree.trans.40.50[2], med.MCAR.cov.50.AD.women.tree.trans.40.50[2],
                                                    med.MCAR.cov.55.AD.women.tree.trans.40.50[2], med.MCAR.cov.60.AD.women.tree.trans.40.50[2],
                                                    med.MCAR.cov.65.AD.women.tree.trans.40.50[2], med.MCAR.cov.70.AD.women.tree.trans.40.50[2],
                                                    med.MCAR.cov.75.AD.women.tree.trans.40.50[2], med.MCAR.cov.80.AD.women.tree.trans.40.50[2],
                                                    med.MCAR.cov.85.AD.women.tree.trans.40.50[2], med.MCAR.cov.90.AD.women.tree.trans.40.50[2],
                                                    med.MCAR.cov.95.AD.women.tree.trans.40.50[2], med.AD.num.women.true.cov.100.40.50[2]),
                                              
                                              L = c(med.MCAR.cov.35.AD.women.tree.trans.40.50[1], med.MCAR.cov.40.AD.women.tree.trans.40.50[1],
                                                    med.MCAR.cov.45.AD.women.tree.trans.40.50[1], med.MCAR.cov.50.AD.women.tree.trans.40.50[1],
                                                    med.MCAR.cov.55.AD.women.tree.trans.40.50[1], med.MCAR.cov.60.AD.women.tree.trans.40.50[1],
                                                    med.MCAR.cov.65.AD.women.tree.trans.40.50[1], med.MCAR.cov.70.AD.women.tree.trans.40.50[1],
                                                    med.MCAR.cov.75.AD.women.tree.trans.40.50[1], med.MCAR.cov.80.AD.women.tree.trans.40.50[1],
                                                    med.MCAR.cov.85.AD.women.tree.trans.40.50[1], med.MCAR.cov.90.AD.women.tree.trans.40.50[1],
                                                    med.MCAR.cov.95.AD.women.tree.trans.40.50[1], med.AD.num.women.true.cov.100.40.50[1]),
                                              
                                              U = c(med.MCAR.cov.35.AD.women.tree.trans.40.50[3], med.MCAR.cov.40.AD.women.tree.trans.40.50[3],
                                                    med.MCAR.cov.45.AD.women.tree.trans.40.50[3], med.MCAR.cov.50.AD.women.tree.trans.40.50[3],
                                                    med.MCAR.cov.55.AD.women.tree.trans.40.50[3], med.MCAR.cov.60.AD.women.tree.trans.40.50[3],
                                                    med.MCAR.cov.65.AD.women.tree.trans.40.50[3], med.MCAR.cov.70.AD.women.tree.trans.40.50[3],
                                                    med.MCAR.cov.75.AD.women.tree.trans.40.50[3], med.MCAR.cov.80.AD.women.tree.trans.40.50[3],
                                                    med.MCAR.cov.85.AD.women.tree.trans.40.50[3], med.MCAR.cov.90.AD.women.tree.trans.40.50[3],
                                                    med.MCAR.cov.95.AD.women.tree.trans.40.50[3], med.AD.num.women.true.cov.100.40.50[3]))


plot.med.MCAR.women.tree.trans.40.50 <- ggplot(med.MCAR.women.tree.trans.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.women.tree.trans.40.50.png",
       plot = plot.med.MCAR.women.tree.trans.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

med.MCAR.men.tree.trans.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                            
                                            F = c(med.MCAR.cov.35.AD.men.tree.trans.40.50[2], med.MCAR.cov.40.AD.men.tree.trans.40.50[2],
                                                  med.MCAR.cov.45.AD.men.tree.trans.40.50[2], med.MCAR.cov.50.AD.men.tree.trans.40.50[2],
                                                  med.MCAR.cov.55.AD.men.tree.trans.40.50[2], med.MCAR.cov.60.AD.men.tree.trans.40.50[2],
                                                  med.MCAR.cov.65.AD.men.tree.trans.40.50[2], med.MCAR.cov.70.AD.men.tree.trans.40.50[2],
                                                  med.MCAR.cov.75.AD.men.tree.trans.40.50[2], med.MCAR.cov.80.AD.men.tree.trans.40.50[2],
                                                  med.MCAR.cov.85.AD.men.tree.trans.40.50[2], med.MCAR.cov.90.AD.men.tree.trans.40.50[2],
                                                  med.MCAR.cov.95.AD.men.tree.trans.40.50[2], med.AD.num.men.true.cov.100.40.50[2]),
                                            
                                            L = c(med.MCAR.cov.35.AD.men.tree.trans.40.50[1], med.MCAR.cov.40.AD.men.tree.trans.40.50[1],
                                                  med.MCAR.cov.45.AD.men.tree.trans.40.50[1], med.MCAR.cov.50.AD.men.tree.trans.40.50[1],
                                                  med.MCAR.cov.55.AD.men.tree.trans.40.50[1], med.MCAR.cov.60.AD.men.tree.trans.40.50[1],
                                                  med.MCAR.cov.65.AD.men.tree.trans.40.50[1], med.MCAR.cov.70.AD.men.tree.trans.40.50[1],
                                                  med.MCAR.cov.75.AD.men.tree.trans.40.50[1], med.MCAR.cov.80.AD.men.tree.trans.40.50[1],
                                                  med.MCAR.cov.85.AD.men.tree.trans.40.50[1], med.MCAR.cov.90.AD.men.tree.trans.40.50[1],
                                                  med.MCAR.cov.95.AD.men.tree.trans.40.50[1], med.AD.num.men.true.cov.100.40.50[1]),
                                            
                                            U = c(med.MCAR.cov.35.AD.men.tree.trans.40.50[3], med.MCAR.cov.40.AD.men.tree.trans.40.50[3],
                                                  med.MCAR.cov.45.AD.men.tree.trans.40.50[3], med.MCAR.cov.50.AD.men.tree.trans.40.50[3],
                                                  med.MCAR.cov.55.AD.men.tree.trans.40.50[3], med.MCAR.cov.60.AD.men.tree.trans.40.50[3],
                                                  med.MCAR.cov.65.AD.men.tree.trans.40.50[3], med.MCAR.cov.70.AD.men.tree.trans.40.50[3],
                                                  med.MCAR.cov.75.AD.men.tree.trans.40.50[3], med.MCAR.cov.80.AD.men.tree.trans.40.50[3],
                                                  med.MCAR.cov.85.AD.men.tree.trans.40.50[3], med.MCAR.cov.90.AD.men.tree.trans.40.50[3],
                                                  med.MCAR.cov.95.AD.men.tree.trans.40.50[3], med.AD.num.men.true.cov.100.40.50[3]))


plot.med.MCAR.men.tree.trans.40.50 <- ggplot(med.MCAR.men.tree.trans.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.med.MCAR.men.tree.trans.40.50.png",
       plot = plot.med.MCAR.men.tree.trans.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



## Standard deviation


# Women 15.25

sd.MCAR.women.tree.trans.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                             
                                             F = c(sd.MCAR.cov.35.AD.women.tree.trans.15.25[2], sd.MCAR.cov.40.AD.women.tree.trans.15.25[2],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.15.25[2], sd.MCAR.cov.50.AD.women.tree.trans.15.25[2],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.15.25[2], sd.MCAR.cov.60.AD.women.tree.trans.15.25[2],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.15.25[2], sd.MCAR.cov.70.AD.women.tree.trans.15.25[2],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.15.25[2], sd.MCAR.cov.80.AD.women.tree.trans.15.25[2],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.15.25[2], sd.MCAR.cov.90.AD.women.tree.trans.15.25[2],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.15.25[2], sd.AD.num.women.true.cov.100.15.25[2]),
                                             
                                             L = c(sd.MCAR.cov.35.AD.women.tree.trans.15.25[1], sd.MCAR.cov.40.AD.women.tree.trans.15.25[1],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.15.25[1], sd.MCAR.cov.50.AD.women.tree.trans.15.25[1],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.15.25[1], sd.MCAR.cov.60.AD.women.tree.trans.15.25[1],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.15.25[1], sd.MCAR.cov.70.AD.women.tree.trans.15.25[1],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.15.25[1], sd.MCAR.cov.80.AD.women.tree.trans.15.25[1],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.15.25[1], sd.MCAR.cov.90.AD.women.tree.trans.15.25[1],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.15.25[1], sd.AD.num.women.true.cov.100.15.25[1]),
                                             
                                             U = c(sd.MCAR.cov.35.AD.women.tree.trans.15.25[3], sd.MCAR.cov.40.AD.women.tree.trans.15.25[3],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.15.25[3], sd.MCAR.cov.50.AD.women.tree.trans.15.25[3],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.15.25[3], sd.MCAR.cov.60.AD.women.tree.trans.15.25[3],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.15.25[3], sd.MCAR.cov.70.AD.women.tree.trans.15.25[3],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.15.25[3], sd.MCAR.cov.80.AD.women.tree.trans.15.25[3],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.15.25[3], sd.MCAR.cov.90.AD.women.tree.trans.15.25[3],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.15.25[3], sd.AD.num.women.true.cov.100.15.25[3]))


plot.sd.MCAR.women.tree.trans.15.25 <- ggplot(sd.MCAR.women.tree.trans.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.tree.trans.15.25.png",
       plot = plot.sd.MCAR.women.tree.trans.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 15.25

sd.MCAR.men.tree.trans.15.25 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                           
                                           F = c(sd.MCAR.cov.35.AD.men.tree.trans.15.25[2], sd.MCAR.cov.40.AD.men.tree.trans.15.25[2],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.15.25[2], sd.MCAR.cov.50.AD.men.tree.trans.15.25[2],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.15.25[2], sd.MCAR.cov.60.AD.men.tree.trans.15.25[2],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.15.25[2], sd.MCAR.cov.70.AD.men.tree.trans.15.25[2],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.15.25[2], sd.MCAR.cov.80.AD.men.tree.trans.15.25[2],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.15.25[2], sd.MCAR.cov.90.AD.men.tree.trans.15.25[2],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.15.25[2], sd.AD.num.men.true.cov.100.15.25[2]),
                                           
                                           L = c(sd.MCAR.cov.35.AD.men.tree.trans.15.25[1], sd.MCAR.cov.40.AD.men.tree.trans.15.25[1],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.15.25[1], sd.MCAR.cov.50.AD.men.tree.trans.15.25[1],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.15.25[1], sd.MCAR.cov.60.AD.men.tree.trans.15.25[1],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.15.25[1], sd.MCAR.cov.70.AD.men.tree.trans.15.25[1],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.15.25[1], sd.MCAR.cov.80.AD.men.tree.trans.15.25[1],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.15.25[1], sd.MCAR.cov.90.AD.men.tree.trans.15.25[1],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.15.25[1], sd.AD.num.men.true.cov.100.15.25[1]),
                                           
                                           U = c(sd.MCAR.cov.35.AD.men.tree.trans.15.25[3], sd.MCAR.cov.40.AD.men.tree.trans.15.25[3],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.15.25[3], sd.MCAR.cov.50.AD.men.tree.trans.15.25[3],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.15.25[3], sd.MCAR.cov.60.AD.men.tree.trans.15.25[3],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.15.25[3], sd.MCAR.cov.70.AD.men.tree.trans.15.25[3],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.15.25[3], sd.MCAR.cov.80.AD.men.tree.trans.15.25[3],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.15.25[3], sd.MCAR.cov.90.AD.men.tree.trans.15.25[3],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.15.25[3], sd.AD.num.men.true.cov.100.15.25[3]))


plot.sd.MCAR.men.tree.trans.15.25 <- ggplot(sd.MCAR.men.tree.trans.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.sd.MCAR.men.tree.trans.15.25.png",
       plot = plot.sd.MCAR.men.tree.trans.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Women: 25.40

sd.MCAR.women.tree.trans.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                             
                                             F = c(sd.MCAR.cov.35.AD.women.tree.trans.25.40[2], sd.MCAR.cov.40.AD.women.tree.trans.25.40[2],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.25.40[2], sd.MCAR.cov.50.AD.women.tree.trans.25.40[2],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.25.40[2], sd.MCAR.cov.60.AD.women.tree.trans.25.40[2],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.25.40[2], sd.MCAR.cov.70.AD.women.tree.trans.25.40[2],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.25.40[2], sd.MCAR.cov.80.AD.women.tree.trans.25.40[2],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.25.40[2], sd.MCAR.cov.90.AD.women.tree.trans.25.40[2],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.25.40[2], sd.AD.num.women.true.cov.100.25.40[2]),
                                             
                                             L = c(sd.MCAR.cov.35.AD.women.tree.trans.25.40[1], sd.MCAR.cov.40.AD.women.tree.trans.25.40[1],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.25.40[1], sd.MCAR.cov.50.AD.women.tree.trans.25.40[1],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.25.40[1], sd.MCAR.cov.60.AD.women.tree.trans.25.40[1],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.25.40[1], sd.MCAR.cov.70.AD.women.tree.trans.25.40[1],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.25.40[1], sd.MCAR.cov.80.AD.women.tree.trans.25.40[1],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.25.40[1], sd.MCAR.cov.90.AD.women.tree.trans.25.40[1],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.25.40[1], sd.AD.num.women.true.cov.100.25.40[1]),
                                             
                                             U = c(sd.MCAR.cov.35.AD.women.tree.trans.25.40[3], sd.MCAR.cov.40.AD.women.tree.trans.25.40[3],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.25.40[3], sd.MCAR.cov.50.AD.women.tree.trans.25.40[3],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.25.40[3], sd.MCAR.cov.60.AD.women.tree.trans.25.40[3],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.25.40[3], sd.MCAR.cov.70.AD.women.tree.trans.25.40[3],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.25.40[3], sd.MCAR.cov.80.AD.women.tree.trans.25.40[3],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.25.40[3], sd.MCAR.cov.90.AD.women.tree.trans.25.40[3],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.25.40[3], sd.AD.num.women.true.cov.100.25.40[3]))


plot.sd.MCAR.women.tree.trans.25.40 <- ggplot(sd.MCAR.women.tree.trans.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.tree.trans.25.40.png",
       plot = plot.sd.MCAR.women.tree.trans.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Men: 25.40

sd.MCAR.men.tree.trans.25.40 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                           
                                           F = c(sd.MCAR.cov.35.AD.men.tree.trans.25.40[2], sd.MCAR.cov.40.AD.men.tree.trans.25.40[2],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.25.40[2], sd.MCAR.cov.50.AD.men.tree.trans.25.40[2],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.25.40[2], sd.MCAR.cov.60.AD.men.tree.trans.25.40[2],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.25.40[2], sd.MCAR.cov.70.AD.men.tree.trans.25.40[2],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.25.40[2], sd.MCAR.cov.80.AD.men.tree.trans.25.40[2],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.25.40[2], sd.MCAR.cov.90.AD.men.tree.trans.25.40[2],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.25.40[2], sd.AD.num.men.true.cov.100.25.40[2]),
                                           
                                           L = c(sd.MCAR.cov.35.AD.men.tree.trans.25.40[1], sd.MCAR.cov.40.AD.men.tree.trans.25.40[1],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.25.40[1], sd.MCAR.cov.50.AD.men.tree.trans.25.40[1],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.25.40[1], sd.MCAR.cov.60.AD.men.tree.trans.25.40[1],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.25.40[1], sd.MCAR.cov.70.AD.men.tree.trans.25.40[1],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.25.40[1], sd.MCAR.cov.80.AD.men.tree.trans.25.40[1],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.25.40[1], sd.MCAR.cov.90.AD.men.tree.trans.25.40[1],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.25.40[1], sd.AD.num.men.true.cov.100.25.40[1]),
                                           
                                           U = c(sd.MCAR.cov.35.AD.men.tree.trans.25.40[3], sd.MCAR.cov.40.AD.men.tree.trans.25.40[3],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.25.40[3], sd.MCAR.cov.50.AD.men.tree.trans.25.40[3],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.25.40[3], sd.MCAR.cov.60.AD.men.tree.trans.25.40[3],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.25.40[3], sd.MCAR.cov.70.AD.men.tree.trans.25.40[3],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.25.40[3], sd.MCAR.cov.80.AD.men.tree.trans.25.40[3],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.25.40[3], sd.MCAR.cov.90.AD.men.tree.trans.25.40[3],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.25.40[3], sd.AD.num.men.true.cov.100.25.40[3]))


plot.sd.MCAR.men.tree.trans.25.40 <- ggplot(sd.MCAR.men.tree.trans.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


ggsave(filename = "plot.sd.MCAR.men.tree.trans.25.40.png",
       plot = plot.sd.MCAR.men.tree.trans.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



# Women: 40.50

sd.MCAR.women.tree.trans.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                             
                                             F = c(sd.MCAR.cov.35.AD.women.tree.trans.40.50[2], sd.MCAR.cov.40.AD.women.tree.trans.40.50[2],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.40.50[2], sd.MCAR.cov.50.AD.women.tree.trans.40.50[2],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.40.50[2], sd.MCAR.cov.60.AD.women.tree.trans.40.50[2],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.40.50[2], sd.MCAR.cov.70.AD.women.tree.trans.40.50[2],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.40.50[2], sd.MCAR.cov.80.AD.women.tree.trans.40.50[2],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.40.50[2], sd.MCAR.cov.90.AD.women.tree.trans.40.50[2],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.40.50[2], sd.AD.num.women.true.cov.100.40.50[2]),
                                             
                                             L = c(sd.MCAR.cov.35.AD.women.tree.trans.40.50[1], sd.MCAR.cov.40.AD.women.tree.trans.40.50[1],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.40.50[1], sd.MCAR.cov.50.AD.women.tree.trans.40.50[1],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.40.50[1], sd.MCAR.cov.60.AD.women.tree.trans.40.50[1],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.40.50[1], sd.MCAR.cov.70.AD.women.tree.trans.40.50[1],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.40.50[1], sd.MCAR.cov.80.AD.women.tree.trans.40.50[1],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.40.50[1], sd.MCAR.cov.90.AD.women.tree.trans.40.50[1],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.40.50[1], sd.AD.num.women.true.cov.100.40.50[1]),
                                             
                                             U = c(sd.MCAR.cov.35.AD.women.tree.trans.40.50[3], sd.MCAR.cov.40.AD.women.tree.trans.40.50[3],
                                                   sd.MCAR.cov.45.AD.women.tree.trans.40.50[3], sd.MCAR.cov.50.AD.women.tree.trans.40.50[3],
                                                   sd.MCAR.cov.55.AD.women.tree.trans.40.50[3], sd.MCAR.cov.60.AD.women.tree.trans.40.50[3],
                                                   sd.MCAR.cov.65.AD.women.tree.trans.40.50[3], sd.MCAR.cov.70.AD.women.tree.trans.40.50[3],
                                                   sd.MCAR.cov.75.AD.women.tree.trans.40.50[3], sd.MCAR.cov.80.AD.women.tree.trans.40.50[3],
                                                   sd.MCAR.cov.85.AD.women.tree.trans.40.50[3], sd.MCAR.cov.90.AD.women.tree.trans.40.50[3],
                                                   sd.MCAR.cov.95.AD.women.tree.trans.40.50[3], sd.AD.num.women.true.cov.100.40.50[3]))


plot.sd.MCAR.women.tree.trans.40.50 <- ggplot(sd.MCAR.women.tree.trans.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.women.tree.trans.40.50.png",
       plot = plot.sd.MCAR.women.tree.trans.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


# Men: 40.50

sd.MCAR.men.tree.trans.40.50 <- data.frame(x=c(seq(from=35, to=100, by=5)),
                                           
                                           F = c(sd.MCAR.cov.35.AD.men.tree.trans.40.50[2], sd.MCAR.cov.40.AD.men.tree.trans.40.50[2],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.40.50[2], sd.MCAR.cov.50.AD.men.tree.trans.40.50[2],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.40.50[2], sd.MCAR.cov.60.AD.men.tree.trans.40.50[2],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.40.50[2], sd.MCAR.cov.70.AD.men.tree.trans.40.50[2],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.40.50[2], sd.MCAR.cov.80.AD.men.tree.trans.40.50[2],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.40.50[2], sd.MCAR.cov.90.AD.men.tree.trans.40.50[2],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.40.50[2], sd.AD.num.men.true.cov.100.40.50[2]),
                                           
                                           L = c(sd.MCAR.cov.35.AD.men.tree.trans.40.50[1], sd.MCAR.cov.40.AD.men.tree.trans.40.50[1],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.40.50[1], sd.MCAR.cov.50.AD.men.tree.trans.40.50[1],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.40.50[1], sd.MCAR.cov.60.AD.men.tree.trans.40.50[1],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.40.50[1], sd.MCAR.cov.70.AD.men.tree.trans.40.50[1],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.40.50[1], sd.MCAR.cov.80.AD.men.tree.trans.40.50[1],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.40.50[1], sd.MCAR.cov.90.AD.men.tree.trans.40.50[1],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.40.50[1], sd.AD.num.men.true.cov.100.40.50[1]),
                                           
                                           U = c(sd.MCAR.cov.35.AD.men.tree.trans.40.50[3], sd.MCAR.cov.40.AD.men.tree.trans.40.50[3],
                                                 sd.MCAR.cov.45.AD.men.tree.trans.40.50[3], sd.MCAR.cov.50.AD.men.tree.trans.40.50[3],
                                                 sd.MCAR.cov.55.AD.men.tree.trans.40.50[3], sd.MCAR.cov.60.AD.men.tree.trans.40.50[3],
                                                 sd.MCAR.cov.65.AD.men.tree.trans.40.50[3], sd.MCAR.cov.70.AD.men.tree.trans.40.50[3],
                                                 sd.MCAR.cov.75.AD.men.tree.trans.40.50[3], sd.MCAR.cov.80.AD.men.tree.trans.40.50[3],
                                                 sd.MCAR.cov.85.AD.men.tree.trans.40.50[3], sd.MCAR.cov.90.AD.men.tree.trans.40.50[3],
                                                 sd.MCAR.cov.95.AD.men.tree.trans.40.50[3], sd.AD.num.men.true.cov.100.40.50[3]))


plot.sd.MCAR.men.tree.trans.40.50 <- ggplot(sd.MCAR.men.tree.trans.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")

ggsave(filename = "plot.sd.MCAR.men.tree.trans.40.50.png",
       plot = plot.sd.MCAR.men.tree.trans.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# GoF and errors ----------------------------------------------------------


# For proportions
################



# Difference between clusters' inference and true values in 100% coverage

# Cov 35


# 15.25

error.infer.clust.cov.100.men.15.25.cov.35 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.35.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.35 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.35)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.35)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.35.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.35 <- MRE(error.infer.clust.cov.100.men.15.25.cov.35)

error.infer.clust.cov.100.women.15.25.cov.35 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.35.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.35 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.35)
MAE.error.infer.clust.cov.100.women.15.25.cov.35 <- MAE(error.infer.clust.cov.100.women.15.25.cov.35)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.35.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.35 <- MRE(error.infer.clust.cov.100.women.15.25.cov.35)



# 25.40
error.infer.clust.cov.100.men.25.40.cov.35 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.35.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.35 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.35)
MAE.error.infer.clust.cov.100.men.25.40.cov.35 <- MAE(error.infer.clust.cov.100.men.25.40.cov.35)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.35.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.35 <- MRE(error.infer.clust.cov.100.men.25.40.cov.35)


error.infer.clust.cov.100.women.25.40.cov.35 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.35.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.35 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.35)
MAE.error.infer.clust.cov.100.women.25.40.cov.35 <- MAE(error.infer.clust.cov.100.women.25.40.cov.35)
ARMSE.error.infer.clust.cov.100.women.cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.35.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.35 <- MRE(error.infer.clust.cov.100.women.25.40.cov.35)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.35 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.35.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.35 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.35)
MAE.error.infer.clust.cov.100.men.40.50.cov.35 <- MAE(error.infer.clust.cov.100.men.40.50.cov.35)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.35.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.35 <- MRE(error.infer.clust.cov.100.men.40.50.cov.35)

error.infer.clust.cov.100.women.40.50.cov.35 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.35.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.35 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.35)
MAE.error.infer.clust.cov.100.women.40.50.cov.35 <- MAE(error.infer.clust.cov.100.women.40.50.cov.35)
ARMSE.error.infer.clust.cov.100.women.cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.35.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.35 <- MRE(error.infer.clust.cov.100.women.40.50.cov.35)

# Cov 40


# 15.25

error.infer.clust.cov.100.men.15.25.cov.40 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.40.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.40 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.40)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.40)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.40.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.40 <- MRE(error.infer.clust.cov.100.men.15.25.cov.40)


error.infer.clust.cov.100.women.15.25.cov.40 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.40.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.40 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.40)
MAE.error.infer.clust.cov.100.women.15.25.cov.40 <- MAE(error.infer.clust.cov.100.women.15.25.cov.40)
ARMSE.error.infer.clust.cov.100.women.cov.40 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.40.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.40 <- MRE(error.infer.clust.cov.100.women.15.25.cov.40)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.40 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.40.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.40 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.40)
MAE.error.infer.clust.cov.100.men.25.40.cov.40 <- MAE(error.infer.clust.cov.100.men.25.40.cov.40)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.40 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.40.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.40 <- MRE(error.infer.clust.cov.100.men.25.40.cov.40)


error.infer.clust.cov.100.women.25.40.cov.40 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.40.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.40 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.40)
MAE.error.infer.clust.cov.100.women.25.40.cov.40 <- MAE(error.infer.clust.cov.100.women.25.40.cov.40)
ARMSE.error.infer.clust.cov.100.women.cov.40 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.40.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.40 <- MRE(error.infer.clust.cov.100.women.25.40.cov.40)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.40 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.40.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.40 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.40)
MAE.error.infer.clust.cov.100.men.40.50.cov.40 <- MAE(error.infer.clust.cov.100.men.40.50.cov.40)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.40 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.40.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.40 <- MRE(error.infer.clust.cov.100.men.40.50.cov.40)


error.infer.clust.cov.100.women.40.50.cov.40 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.40.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.40 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.40)
MAE.error.infer.clust.cov.100.women.40.50.cov.40 <- MAE(error.infer.clust.cov.100.women.40.50.cov.40)
ARMSE.error.infer.clust.cov.100.women.cov.40 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.40.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.40 <- MRE(error.infer.clust.cov.100.women.40.50.cov.40)


# Cov 45


# 15.25

error.infer.clust.cov.100.men.15.25.cov.45 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.45.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.45 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.45)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.45)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.45.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.45 <- MRE(error.infer.clust.cov.100.men.15.25.cov.45)


error.infer.clust.cov.100.women.15.25.cov.45 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.45.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.45 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.45)
MAE.error.infer.clust.cov.100.women.15.25.cov.45 <- MAE(error.infer.clust.cov.100.women.15.25.cov.45)
ARMSE.error.infer.clust.cov.100.women.cov.45 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.45.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.45 <- MRE(error.infer.clust.cov.100.women.15.25.cov.45)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.45 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.45.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.45 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.45)
MAE.error.infer.clust.cov.100.men.25.40.cov.45 <- MAE(error.infer.clust.cov.100.men.25.40.cov.45)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.45 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.45.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.45 <- MRE(error.infer.clust.cov.100.men.25.40.cov.45)


error.infer.clust.cov.100.women.25.40.cov.45 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.45.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.45 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.45)
MAE.error.infer.clust.cov.100.women.25.40.cov.45 <- MAE(error.infer.clust.cov.100.women.25.40.cov.45)
ARMSE.error.infer.clust.cov.100.women.cov.45 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.45.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.45 <- MRE(error.infer.clust.cov.100.women.25.40.cov.45)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.45 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.45.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.45 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.45)
MAE.error.infer.clust.cov.100.men.40.50.cov.45 <- MAE(error.infer.clust.cov.100.men.40.50.cov.45)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.45 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.45.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.45 <- MRE(error.infer.clust.cov.100.men.40.50.cov.45)


error.infer.clust.cov.100.women.40.50.cov.45 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.45.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.45 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.45)
MAE.error.infer.clust.cov.100.women.40.50.cov.45 <- MAE(error.infer.clust.cov.100.women.40.50.cov.45)
ARMSE.error.infer.clust.cov.100.women.cov.45 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.45.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.45 <- MRE(error.infer.clust.cov.100.women.40.50.cov.45)



# Cov 50


# 15.25

error.infer.clust.cov.100.men.15.25.cov.50 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.50.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.50 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.50)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.50)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.50.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.50 <- MRE(error.infer.clust.cov.100.men.15.25.cov.50)


error.infer.clust.cov.100.women.15.25.cov.50 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.50.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.50 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.50)
MAE.error.infer.clust.cov.100.women.15.25.cov.50 <- MAE(error.infer.clust.cov.100.women.15.25.cov.50)
ARMSE.error.infer.clust.cov.100.women.cov.50 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.50.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.50 <- MRE(error.infer.clust.cov.100.women.15.25.cov.50)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.50 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.50.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.50 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.50)
MAE.error.infer.clust.cov.100.men.25.40.cov.50 <- MAE(error.infer.clust.cov.100.men.25.40.cov.50)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.50 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.50.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.50 <- MRE(error.infer.clust.cov.100.men.25.40.cov.50)


error.infer.clust.cov.100.women.25.40.cov.50 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.50.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.50 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.50)
MAE.error.infer.clust.cov.100.women.25.40.cov.50 <- MAE(error.infer.clust.cov.100.women.25.40.cov.50)
ARMSE.error.infer.clust.cov.100.women.cov.50 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.50.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.50 <- MRE(error.infer.clust.cov.100.women.25.40.cov.50)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.50 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.50.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.50 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.50)
MAE.error.infer.clust.cov.100.men.40.50.cov.50 <- MAE(error.infer.clust.cov.100.men.40.50.cov.50)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.50 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.50.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.50 <- MRE(error.infer.clust.cov.100.men.40.50.cov.50)


error.infer.clust.cov.100.women.40.50.cov.50 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.50.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.50 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.50)
MAE.error.infer.clust.cov.100.women.40.50.cov.50 <- MAE(error.infer.clust.cov.100.women.40.50.cov.50)
ARMSE.error.infer.clust.cov.100.women.cov.50 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.50.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.50 <- MRE(error.infer.clust.cov.100.women.40.50.cov.50)



# Cov 55


# 15.25

error.infer.clust.cov.100.men.15.25.cov.55 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.55.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.55 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.55)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.55)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.55.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.55 <- MRE(error.infer.clust.cov.100.men.15.25.cov.55)


error.infer.clust.cov.100.women.15.25.cov.55 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.55.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.55 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.55)
MAE.error.infer.clust.cov.100.women.15.25.cov.55 <- MAE(error.infer.clust.cov.100.women.15.25.cov.55)
ARMSE.error.infer.clust.cov.100.women.cov.55 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.55.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.55 <- MRE(error.infer.clust.cov.100.women.15.25.cov.55)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.55 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.55.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.55 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.55)
MAE.error.infer.clust.cov.100.men.25.40.cov.55 <- MAE(error.infer.clust.cov.100.men.25.40.cov.55)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.55 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.55.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.55 <- MRE(error.infer.clust.cov.100.men.25.40.cov.55)


error.infer.clust.cov.100.women.25.40.cov.55 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.55.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.55 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.55)
MAE.error.infer.clust.cov.100.women.25.40.cov.55 <- MAE(error.infer.clust.cov.100.women.25.40.cov.55)
ARMSE.error.infer.clust.cov.100.women.cov.55 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.55.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.55 <- MRE(error.infer.clust.cov.100.women.25.40.cov.55)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.55 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.55.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.55 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.55)
MAE.error.infer.clust.cov.100.men.40.50.cov.55 <- MAE(error.infer.clust.cov.100.men.40.50.cov.55)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.55 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.55.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.55 <- MRE(error.infer.clust.cov.100.men.40.50.cov.55)


error.infer.clust.cov.100.women.40.50.cov.55 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.55.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.55 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.55)
MAE.error.infer.clust.cov.100.women.40.50.cov.55 <- MAE(error.infer.clust.cov.100.women.40.50.cov.55)
ARMSE.error.infer.clust.cov.100.women.cov.55 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.55.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.55 <- MRE(error.infer.clust.cov.100.women.40.50.cov.55)



# Cov 60


# 15.25

error.infer.clust.cov.100.men.15.25.cov.60 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.60.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.60 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.60)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.60)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.60.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.60 <- MRE(error.infer.clust.cov.100.men.15.25.cov.60)


error.infer.clust.cov.100.women.15.25.cov.60 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.60.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.60 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.60)
MAE.error.infer.clust.cov.100.women.15.25.cov.60 <- MAE(error.infer.clust.cov.100.women.15.25.cov.60)
ARMSE.error.infer.clust.cov.100.women.cov.60 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.60.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.60 <- MRE(error.infer.clust.cov.100.women.15.25.cov.60)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.60 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.60.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.60 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.60)
MAE.error.infer.clust.cov.100.men.25.40.cov.60 <- MAE(error.infer.clust.cov.100.men.25.40.cov.60)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.60 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.60.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.60 <- MRE(error.infer.clust.cov.100.men.25.40.cov.60)


error.infer.clust.cov.100.women.25.40.cov.60 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.60.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.60 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.60)
MAE.error.infer.clust.cov.100.women.25.40.cov.60 <- MAE(error.infer.clust.cov.100.women.25.40.cov.60)
ARMSE.error.infer.clust.cov.100.women.cov.60 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.60.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.60 <- MRE(error.infer.clust.cov.100.women.25.40.cov.60)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.60 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.60.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.60 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.60)
MAE.error.infer.clust.cov.100.men.40.50.cov.60 <- MAE(error.infer.clust.cov.100.men.40.50.cov.60)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.60 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.60.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.60 <- MRE(error.infer.clust.cov.100.men.40.50.cov.60)


error.infer.clust.cov.100.women.40.50.cov.60 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.60.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.60 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.60)
MAE.error.infer.clust.cov.100.women.40.50.cov.60 <- MAE(error.infer.clust.cov.100.women.40.50.cov.60)
ARMSE.error.infer.clust.cov.100.women.cov.60 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.60.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.60 <- MRE(error.infer.clust.cov.100.women.40.50.cov.60)


# Cov 65


# 15.25

error.infer.clust.cov.100.men.15.25.cov.65 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.65.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.65 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.65)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.65)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.65.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.65 <- MRE(error.infer.clust.cov.100.men.15.25.cov.65)


error.infer.clust.cov.100.women.15.25.cov.65 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.65.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.65 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.65)
MAE.error.infer.clust.cov.100.women.15.25.cov.65 <- MAE(error.infer.clust.cov.100.women.15.25.cov.65)
ARMSE.error.infer.clust.cov.100.women.cov.65 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.65.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.65 <- MRE(error.infer.clust.cov.100.women.15.25.cov.65)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.65 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.65.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.65 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.65)
MAE.error.infer.clust.cov.100.men.25.40.cov.65 <- MAE(error.infer.clust.cov.100.men.25.40.cov.65)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.65 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.65.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.65 <- MRE(error.infer.clust.cov.100.men.25.40.cov.65)


error.infer.clust.cov.100.women.25.40.cov.65 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.65.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.65 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.65)
MAE.error.infer.clust.cov.100.women.25.40.cov.65 <- MAE(error.infer.clust.cov.100.women.25.40.cov.65)
ARMSE.error.infer.clust.cov.100.women.cov.65 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.65.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.65 <- MRE(error.infer.clust.cov.100.women.25.40.cov.65)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.65 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.65.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.65 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.65)
MAE.error.infer.clust.cov.100.men.40.50.cov.65 <- MAE(error.infer.clust.cov.100.men.40.50.cov.65)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.65 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.65.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.65 <- MRE(error.infer.clust.cov.100.men.40.50.cov.65)


error.infer.clust.cov.100.women.40.50.cov.65 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.65.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.65 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.65)
MAE.error.infer.clust.cov.100.women.40.50.cov.65 <- MAE(error.infer.clust.cov.100.women.40.50.cov.65)
ARMSE.error.infer.clust.cov.100.women.cov.65 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.65.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.65 <- MRE(error.infer.clust.cov.100.women.40.50.cov.65)


# Cov 70


# 15.25

error.infer.clust.cov.100.men.15.25.cov.70 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.70.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.70 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.70)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.70)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.70.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.70 <- MRE(error.infer.clust.cov.100.men.15.25.cov.70)


error.infer.clust.cov.100.women.15.25.cov.70 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.70.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.70 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.70)
MAE.error.infer.clust.cov.100.women.15.25.cov.70 <- MAE(error.infer.clust.cov.100.women.15.25.cov.70)
ARMSE.error.infer.clust.cov.100.women.cov.70 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.70.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.70 <- MRE(error.infer.clust.cov.100.women.15.25.cov.70)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.70 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.70.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.70 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.70)
MAE.error.infer.clust.cov.100.men.25.40.cov.70 <- MAE(error.infer.clust.cov.100.men.25.40.cov.70)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.70 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.70.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.70 <- MRE(error.infer.clust.cov.100.men.25.40.cov.70)


error.infer.clust.cov.100.women.25.40.cov.70 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.70.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.70 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.70)
MAE.error.infer.clust.cov.100.women.25.40.cov.70 <- MAE(error.infer.clust.cov.100.women.25.40.cov.70)
ARMSE.error.infer.clust.cov.100.women.cov.70 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.70.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.70 <- MRE(error.infer.clust.cov.100.women.25.40.cov.70)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.70 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.70.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.70 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.70)
MAE.error.infer.clust.cov.100.men.40.50.cov.70 <- MAE(error.infer.clust.cov.100.men.40.50.cov.70)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.70 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.70.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.70 <- MRE(error.infer.clust.cov.100.men.40.50.cov.70)


error.infer.clust.cov.100.women.40.50.cov.70 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.70.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.70 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.70)
MAE.error.infer.clust.cov.100.women.40.50.cov.70 <- MAE(error.infer.clust.cov.100.women.40.50.cov.70)
ARMSE.error.infer.clust.cov.100.women.cov.70 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.70.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.70 <- MRE(error.infer.clust.cov.100.women.40.50.cov.70)


# Cov 75


# 15.25

error.infer.clust.cov.100.men.15.25.cov.75 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.75.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.75 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.75)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.75)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.75.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.75 <- MRE(error.infer.clust.cov.100.men.15.25.cov.75)


error.infer.clust.cov.100.women.15.25.cov.75 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.75.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.75 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.75)
MAE.error.infer.clust.cov.100.women.15.25.cov.75 <- MAE(error.infer.clust.cov.100.women.15.25.cov.75)
ARMSE.error.infer.clust.cov.100.women.cov.75 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.75.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.75 <- MRE(error.infer.clust.cov.100.women.15.25.cov.75)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.75 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.75.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.75 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.75)
MAE.error.infer.clust.cov.100.men.25.40.cov.75 <- MAE(error.infer.clust.cov.100.men.25.40.cov.75)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.75 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.75.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.75 <- MRE(error.infer.clust.cov.100.men.25.40.cov.75)


error.infer.clust.cov.100.women.25.40.cov.75 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.75.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.75 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.75)
MAE.error.infer.clust.cov.100.women.25.40.cov.75 <- MAE(error.infer.clust.cov.100.women.25.40.cov.75)
ARMSE.error.infer.clust.cov.100.women.cov.75 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.75.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.75 <- MRE(error.infer.clust.cov.100.women.25.40.cov.75)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.75 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.75.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.75 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.75)
MAE.error.infer.clust.cov.100.men.40.50.cov.75 <- MAE(error.infer.clust.cov.100.men.40.50.cov.75)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.75 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.75.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.75 <- MRE(error.infer.clust.cov.100.men.40.50.cov.75)


error.infer.clust.cov.100.women.40.50.cov.75 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.75.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.75 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.75)
MAE.error.infer.clust.cov.100.women.40.50.cov.75 <- MAE(error.infer.clust.cov.100.women.40.50.cov.75)
ARMSE.error.infer.clust.cov.100.women.cov.75 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.75.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.75 <- MRE(error.infer.clust.cov.100.women.40.50.cov.75)


# Cov 80


# 15.25

error.infer.clust.cov.100.men.15.25.cov.80 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.80.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.80 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.80)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.80)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.80.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.80 <- MRE(error.infer.clust.cov.100.men.15.25.cov.80)


error.infer.clust.cov.100.women.15.25.cov.80 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.80.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.80 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.80)
MAE.error.infer.clust.cov.100.women.15.25.cov.80 <- MAE(error.infer.clust.cov.100.women.15.25.cov.80)
ARMSE.error.infer.clust.cov.100.women.cov.80 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.80.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.80 <- MRE(error.infer.clust.cov.100.women.15.25.cov.80)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.80 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.80.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.80 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.80)
MAE.error.infer.clust.cov.100.men.25.40.cov.80 <- MAE(error.infer.clust.cov.100.men.25.40.cov.80)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.80 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.80.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.80 <- MRE(error.infer.clust.cov.100.men.25.40.cov.80)


error.infer.clust.cov.100.women.25.40.cov.80 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.80.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.80 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.80)
MAE.error.infer.clust.cov.100.women.25.40.cov.80 <- MAE(error.infer.clust.cov.100.women.25.40.cov.80)
ARMSE.error.infer.clust.cov.100.women.cov.80 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.80.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.80 <- MRE(error.infer.clust.cov.100.women.25.40.cov.80)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.80 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.80.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.80 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.80)
MAE.error.infer.clust.cov.100.men.40.50.cov.80 <- MAE(error.infer.clust.cov.100.men.40.50.cov.80)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.80 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.80.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.80 <- MRE(error.infer.clust.cov.100.men.40.50.cov.80)


error.infer.clust.cov.100.women.40.50.cov.80 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.80.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.80 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.80)
MAE.error.infer.clust.cov.100.women.40.50.cov.80 <- MAE(error.infer.clust.cov.100.women.40.50.cov.80)
ARMSE.error.infer.clust.cov.100.women.cov.80 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.80.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.80 <- MRE(error.infer.clust.cov.100.women.40.50.cov.80)



# Cov 85


# 15.25

error.infer.clust.cov.100.men.15.25.cov.85 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.85.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.85 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.85)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.85)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.85.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.85 <- MRE(error.infer.clust.cov.100.men.15.25.cov.85)


error.infer.clust.cov.100.women.15.25.cov.85 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.85.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.85 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.85)
MAE.error.infer.clust.cov.100.women.15.25.cov.85 <- MAE(error.infer.clust.cov.100.women.15.25.cov.85)
ARMSE.error.infer.clust.cov.100.women.cov.85 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.85.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.85 <- MRE(error.infer.clust.cov.100.women.15.25.cov.85)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.85 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.85.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.85 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.85)
MAE.error.infer.clust.cov.100.men.25.40.cov.85 <- MAE(error.infer.clust.cov.100.men.25.40.cov.85)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.85 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.85.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.85 <- MRE(error.infer.clust.cov.100.men.25.40.cov.85)


error.infer.clust.cov.100.women.25.40.cov.85 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.85.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.85 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.85)
MAE.error.infer.clust.cov.100.women.25.40.cov.85 <- MAE(error.infer.clust.cov.100.women.25.40.cov.85)
ARMSE.error.infer.clust.cov.100.women.cov.85 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.85.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.85 <- MRE(error.infer.clust.cov.100.women.25.40.cov.85)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.85 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.85.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.85 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.85)
MAE.error.infer.clust.cov.100.men.40.50.cov.85 <- MAE(error.infer.clust.cov.100.men.40.50.cov.85)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.85 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.85.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.85 <- MRE(error.infer.clust.cov.100.men.40.50.cov.85)


error.infer.clust.cov.100.women.40.50.cov.85 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.85.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.85 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.85)
MAE.error.infer.clust.cov.100.women.40.50.cov.85 <- MAE(error.infer.clust.cov.100.women.40.50.cov.85)
ARMSE.error.infer.clust.cov.100.women.cov.85 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.85.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.85 <- MRE(error.infer.clust.cov.100.women.40.50.cov.85)


# Cov 90


# 15.25

error.infer.clust.cov.100.men.15.25.cov.90 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.90.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.90 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.90)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.90)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.90.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.90 <- MRE(error.infer.clust.cov.100.men.15.25.cov.90)


error.infer.clust.cov.100.women.15.25.cov.90 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.90.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.90 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.90)
MAE.error.infer.clust.cov.100.women.15.25.cov.90 <- MAE(error.infer.clust.cov.100.women.15.25.cov.90)
ARMSE.error.infer.clust.cov.100.women.cov.90 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.90.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.90 <- MRE(error.infer.clust.cov.100.women.15.25.cov.90)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.90 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.90.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.90 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.90)
MAE.error.infer.clust.cov.100.men.25.40.cov.90 <- MAE(error.infer.clust.cov.100.men.25.40.cov.90)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.90 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.90.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.90 <- MRE(error.infer.clust.cov.100.men.25.40.cov.90)


error.infer.clust.cov.100.women.25.40.cov.90 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.90.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.90 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.90)
MAE.error.infer.clust.cov.100.women.25.40.cov.90 <- MAE(error.infer.clust.cov.100.women.25.40.cov.90)
ARMSE.error.infer.clust.cov.100.women.cov.90 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.90.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.90 <- MRE(error.infer.clust.cov.100.women.25.40.cov.90)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.90 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.90.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.90 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.90)
MAE.error.infer.clust.cov.100.men.40.50.cov.90 <- MAE(error.infer.clust.cov.100.men.40.50.cov.90)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.90 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.90.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.90 <- MRE(error.infer.clust.cov.100.men.40.50.cov.90)


error.infer.clust.cov.100.women.40.50.cov.90 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.90.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.90 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.90)
MAE.error.infer.clust.cov.100.women.40.50.cov.90 <- MAE(error.infer.clust.cov.100.women.40.50.cov.90)
ARMSE.error.infer.clust.cov.100.women.cov.90 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.90.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.90 <- MRE(error.infer.clust.cov.100.women.40.50.cov.90)


# Cov 95


# 15.25

error.infer.clust.cov.100.men.15.25.cov.95 <- as.numeric(vector.MCAR.true.cov.100.prop.men15.25.F.15.25) - as.numeric(vector.MCAR.cov.95.cl.prop.men15.25.F.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.95 <- RMSE(error.infer.clust.cov.100.men.15.25.cov.95)
MAE.error.infer.clust.cov.100.men.15.25cov.35 <- MAE(error.infer.clust.cov.100.men.15.25.cov.95)
ARMSE.error.infer.clust.cov.100.men.15.25cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men15.25.F.15.25, v2=vector.MCAR.cov.95.cl.prop.men15.25.F.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.95 <- MRE(error.infer.clust.cov.100.men.15.25.cov.95)


error.infer.clust.cov.100.women.15.25.cov.95 <- as.numeric(vector.MCAR.true.cov.100.prop.women15.25.M.15.25) - as.numeric(vector.MCAR.cov.95.cl.prop.women15.25.M.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.95 <- RMSE(error.infer.clust.cov.100.women.15.25.cov.95)
MAE.error.infer.clust.cov.100.women.15.25.cov.95 <- MAE(error.infer.clust.cov.100.women.15.25.cov.95)
ARMSE.error.infer.clust.cov.100.women.cov.95 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.95.cl.prop.women15.25.M.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.95 <- MRE(error.infer.clust.cov.100.women.15.25.cov.95)

# 25.40
error.infer.clust.cov.100.men.25.40.cov.95 <- as.numeric(vector.MCAR.true.cov.100.prop.men25.40.F.25.40) - as.numeric(vector.MCAR.cov.95.cl.prop.men25.40.F.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.95 <- RMSE(error.infer.clust.cov.100.men.25.40.cov.95)
MAE.error.infer.clust.cov.100.men.25.40.cov.95 <- MAE(error.infer.clust.cov.100.men.25.40.cov.95)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.95 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men25.40.F.25.40, v2=vector.MCAR.cov.95.cl.prop.men25.40.F.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.95 <- MRE(error.infer.clust.cov.100.men.25.40.cov.95)


error.infer.clust.cov.100.women.25.40.cov.95 <- as.numeric(vector.MCAR.true.cov.100.prop.women25.40.M.25.40) - as.numeric(vector.MCAR.cov.95.cl.prop.women25.40.M.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.95 <- RMSE(error.infer.clust.cov.100.women.25.40.cov.95)
MAE.error.infer.clust.cov.100.women.25.40.cov.95 <- MAE(error.infer.clust.cov.100.women.25.40.cov.95)
ARMSE.error.infer.clust.cov.100.women.cov.95 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women25.40.M.25.40, v2=vector.MCAR.cov.95.cl.prop.women25.40.M.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.95 <- MRE(error.infer.clust.cov.100.women.25.40.cov.95)

# 40.50
error.infer.clust.cov.100.men.40.50.cov.95 <- as.numeric(vector.MCAR.true.cov.100.prop.men40.50.F.40.50) - as.numeric(vector.MCAR.cov.95.cl.prop.men40.50.F.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.95 <- RMSE(error.infer.clust.cov.100.men.40.50.cov.95)
MAE.error.infer.clust.cov.100.men.40.50.cov.95 <- MAE(error.infer.clust.cov.100.men.40.50.cov.95)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.95 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.men40.50.F.40.50, v2=vector.MCAR.cov.95.cl.prop.men40.50.F.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.95 <- MRE(error.infer.clust.cov.100.men.40.50.cov.95)


error.infer.clust.cov.100.women.40.50.cov.95 <- as.numeric(vector.MCAR.true.cov.100.prop.women40.50.M.40.50) - as.numeric(vector.MCAR.cov.95.cl.prop.women40.50.M.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.95 <- RMSE(error.infer.clust.cov.100.women.40.50.cov.95)
MAE.error.infer.clust.cov.100.women.40.50.cov.95 <- MAE(error.infer.clust.cov.100.women.40.50.cov.95)
ARMSE.error.infer.clust.cov.100.women.cov.95 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women40.50.M.40.50, v2=vector.MCAR.cov.95.cl.prop.women40.50.M.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.95 <- MRE(error.infer.clust.cov.100.women.40.50.cov.95)




MRE.error.infer.clust.cov.100.prop.men.15.25 <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                                 
                                                                 F = c(MRE.error.infer.clust.cov.100.men.15.25.cov.35, MRE.error.infer.clust.cov.100.men.15.25.cov.40,
                                                                       MRE.error.infer.clust.cov.100.men.15.25.cov.45, MRE.error.infer.clust.cov.100.men.15.25.cov.50,
                                                                       MRE.error.infer.clust.cov.100.men.15.25.cov.55, MRE.error.infer.clust.cov.100.men.15.25.cov.60,
                                                                       MRE.error.infer.clust.cov.100.men.15.25.cov.65, MRE.error.infer.clust.cov.100.men.15.25.cov.70, 
                                                                       MRE.error.infer.clust.cov.100.men.15.25.cov.75, MRE.error.infer.clust.cov.100.men.15.25.cov.80,
                                                                       MRE.error.infer.clust.cov.100.men.15.25.cov.85, MRE.error.infer.clust.cov.100.men.15.25.cov.90,
                                                                       MRE.error.infer.clust.cov.100.men.15.25.cov.95))


plot.MRE.error.infer.clust.cov.100.prop.men.15.25 <- ggplot(MRE.error.infer.clust.cov.100.prop.men.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for men pairings proportions in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.prop.men.15.25.png",
       plot = plot.MRE.error.infer.clust.cov.100.prop.men.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.prop.women.15.25 <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                             
                                                             F = c(MRE.error.infer.clust.cov.100.women.15.25.cov.35, MRE.error.infer.clust.cov.100.women.15.25.cov.40,
                                                                   MRE.error.infer.clust.cov.100.women.15.25.cov.45, MRE.error.infer.clust.cov.100.women.15.25.cov.50,
                                                                   MRE.error.infer.clust.cov.100.women.15.25.cov.55, MRE.error.infer.clust.cov.100.women.15.25.cov.60,
                                                                   MRE.error.infer.clust.cov.100.women.15.25.cov.65, MRE.error.infer.clust.cov.100.women.15.25.cov.70, 
                                                                   MRE.error.infer.clust.cov.100.women.15.25.cov.75, MRE.error.infer.clust.cov.100.women.15.25.cov.80,
                                                                   MRE.error.infer.clust.cov.100.women.15.25.cov.85, MRE.error.infer.clust.cov.100.women.15.25.cov.90,
                                                                   MRE.error.infer.clust.cov.100.women.15.25.cov.95))


plot.MRE.error.infer.clust.cov.100.prop.women.15.25 <- ggplot(MRE.error.infer.clust.cov.100.prop.women.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for women pairings proportions in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.prop.women.15.25.png",
       plot = plot.MRE.error.infer.clust.cov.100.prop.women.15.25,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")






MRE.error.infer.clust.cov.100.prop.men.25.40 <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                           
                                                           F = c(MRE.error.infer.clust.cov.100.men.25.40.cov.35, MRE.error.infer.clust.cov.100.men.25.40.cov.40,
                                                                 MRE.error.infer.clust.cov.100.men.25.40.cov.45, MRE.error.infer.clust.cov.100.men.25.40.cov.50,
                                                                 MRE.error.infer.clust.cov.100.men.25.40.cov.55, MRE.error.infer.clust.cov.100.men.25.40.cov.60,
                                                                 MRE.error.infer.clust.cov.100.men.25.40.cov.65, MRE.error.infer.clust.cov.100.men.25.40.cov.70, 
                                                                 MRE.error.infer.clust.cov.100.men.25.40.cov.75, MRE.error.infer.clust.cov.100.men.25.40.cov.80,
                                                                 MRE.error.infer.clust.cov.100.men.25.40.cov.85, MRE.error.infer.clust.cov.100.men.25.40.cov.90,
                                                                 MRE.error.infer.clust.cov.100.men.25.40.cov.95))


plot.MRE.error.infer.clust.cov.100.prop.men.25.40 <- ggplot(MRE.error.infer.clust.cov.100.prop.men.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for men pairings proportions in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.prop.men.25.40.png",
       plot = plot.MRE.error.infer.clust.cov.100.prop.men.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.prop.women.25.40 <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                             
                                                             F = c(MRE.error.infer.clust.cov.100.women.25.40.cov.35, MRE.error.infer.clust.cov.100.women.25.40.cov.40,
                                                                   MRE.error.infer.clust.cov.100.women.25.40.cov.45, MRE.error.infer.clust.cov.100.women.25.40.cov.50,
                                                                   MRE.error.infer.clust.cov.100.women.25.40.cov.55, MRE.error.infer.clust.cov.100.women.25.40.cov.60,
                                                                   MRE.error.infer.clust.cov.100.women.25.40.cov.65, MRE.error.infer.clust.cov.100.women.25.40.cov.70, 
                                                                   MRE.error.infer.clust.cov.100.women.25.40.cov.75, MRE.error.infer.clust.cov.100.women.25.40.cov.80,
                                                                   MRE.error.infer.clust.cov.100.women.25.40.cov.85, MRE.error.infer.clust.cov.100.women.25.40.cov.90,
                                                                   MRE.error.infer.clust.cov.100.women.25.40.cov.95))


plot.MRE.error.infer.clust.cov.100.prop.women.25.40 <- ggplot(MRE.error.infer.clust.cov.100.prop.women.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for women pairings proportions in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.prop.women.25.40.png",
       plot = plot.MRE.error.infer.clust.cov.100.prop.women.25.40,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.prop.men.40.50 <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                           
                                                           F = c(MRE.error.infer.clust.cov.100.men.40.50.cov.35, MRE.error.infer.clust.cov.100.men.40.50.cov.40,
                                                                 MRE.error.infer.clust.cov.100.men.40.50.cov.45, MRE.error.infer.clust.cov.100.men.40.50.cov.50,
                                                                 MRE.error.infer.clust.cov.100.men.40.50.cov.55, MRE.error.infer.clust.cov.100.men.40.50.cov.60,
                                                                 MRE.error.infer.clust.cov.100.men.40.50.cov.65, MRE.error.infer.clust.cov.100.men.40.50.cov.70, 
                                                                 MRE.error.infer.clust.cov.100.men.40.50.cov.75, MRE.error.infer.clust.cov.100.men.40.50.cov.80,
                                                                 MRE.error.infer.clust.cov.100.men.40.50.cov.85, MRE.error.infer.clust.cov.100.men.40.50.cov.90,
                                                                 MRE.error.infer.clust.cov.100.men.40.50.cov.95))


plot.MRE.error.infer.clust.cov.100.prop.men.40.50 <- ggplot(MRE.error.infer.clust.cov.100.prop.men.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for men pairings proportions in  40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.prop.men.40.50.png",
       plot = plot.MRE.error.infer.clust.cov.100.prop.men.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.prop.women.40.50 <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                             
                                                             F = c(MRE.error.infer.clust.cov.100.women.40.50.cov.35, MRE.error.infer.clust.cov.100.women.40.50.cov.40,
                                                                   MRE.error.infer.clust.cov.100.women.40.50.cov.45, MRE.error.infer.clust.cov.100.women.40.50.cov.50,
                                                                   MRE.error.infer.clust.cov.100.women.40.50.cov.55, MRE.error.infer.clust.cov.100.women.40.50.cov.60,
                                                                   MRE.error.infer.clust.cov.100.women.40.50.cov.65, MRE.error.infer.clust.cov.100.women.40.50.cov.70, 
                                                                   MRE.error.infer.clust.cov.100.women.40.50.cov.75, MRE.error.infer.clust.cov.100.women.40.50.cov.80,
                                                                   MRE.error.infer.clust.cov.100.women.40.50.cov.85, MRE.error.infer.clust.cov.100.women.40.50.cov.90,
                                                                   MRE.error.infer.clust.cov.100.women.40.50.cov.95))


plot.MRE.error.infer.clust.cov.100.prop.women.40.50 <- ggplot(MRE.error.infer.clust.cov.100.prop.women.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for women pairings proportions in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.prop.women.40.50.png",
       plot = plot.MRE.error.infer.clust.cov.100.prop.women.40.50,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# For age difference statistics
###############################


# Cov 35

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.35.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.35.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.35.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.35.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.35.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.35.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.35 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.35.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.35.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.35.mean)

error.infer.clust.cov.100.women.15.25.cov.35.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.35.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.35.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.35.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.35.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.35.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.35.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.35.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.35.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.35.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.35.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.35.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.35.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.35.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.35.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.35.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.35 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.35.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.35.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.35.med)


error.infer.clust.cov.100.women.15.25.cov.35.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.35.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.35.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.35.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.35.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.35.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.35.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.35.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.35.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.35.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.35.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.35.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.35.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.35.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.35.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.35.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.35 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.35.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.35.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.35.sd)


error.infer.clust.cov.100.women.15.25.cov.35.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.35.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.35.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.35.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.35.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.35.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.35.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.35.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.35.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.35.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.35.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.35.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.35.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.35.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.35.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.35.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.35 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.35.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.35.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.35.mean)


error.infer.clust.cov.100.women.25.40.cov.35.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.35.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.35.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.35.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.35.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.35.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.35.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.35.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.35.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.35.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.35.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.35.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.35.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.35.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.35.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.35.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.35 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.35.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.35.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.35.med)


error.infer.clust.cov.100.women.25.40.cov.35.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.35.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.35.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.35.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.35.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.35.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.35.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.35.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.35.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.35.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.35.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.35.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.35.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.35.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.35.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.35.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.35 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.35.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.35.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.35.sd)



error.infer.clust.cov.100.women.25.40.cov.35.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.35.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.35.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.35.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.35.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.35.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.35.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.35.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.35.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.35.sd)


# Aho nagarukirije uno musi ku murango ------------------------------------



# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.35.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.35.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.35.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.35.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.35.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.35.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.35 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.35.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.35.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.35.mean)



error.infer.clust.cov.100.women.40.50.cov.35.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.35.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.35.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.35.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.35.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.35.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.35.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.35.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.35.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.35.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.35.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.35.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.35.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.35.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.35.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.35.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.35 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.35.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.35.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.35.med)



error.infer.clust.cov.100.women.40.50.cov.35.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.35.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.35.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.35.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.35.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.35.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.35.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.35.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.35.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.35.med)

# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.35.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.35.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.35.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.35.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.35.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.35.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.35 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.35.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.35.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.35.sd)



error.infer.clust.cov.100.women.40.50.cov.35.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.35.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.35.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.35.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.35.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.35.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.35.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.35.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.35.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.35.sd)


# Cov 40

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.40.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.40.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.40.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.40.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.40.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.40.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.40 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.40.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.40.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.40.mean)


error.infer.clust.cov.100.women.15.25.cov.40.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.40.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.40.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.40.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.40.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.40.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.40.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.40.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.40.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.40.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.40.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.40.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.40.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.40.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.40.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.40.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.40 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.40.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.40.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.40.med)


error.infer.clust.cov.100.women.15.25.cov.40.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.40.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.40.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.40.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.40.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.40.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.40.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.40.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.40.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.40.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.40.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.40.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.40.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.40.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.40.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.40.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.40 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.40.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.40.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.40.sd)


error.infer.clust.cov.100.women.15.25.cov.40.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.40.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.40.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.40.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.40.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.40.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.40.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.40.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.40.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.40.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.40.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.40.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.40.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.40.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.40.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.40.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.40 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.40.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.40.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.40.mean)


error.infer.clust.cov.100.women.25.40.cov.40.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.40.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.40.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.40.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.40.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.40.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.40.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.40.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.40.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.40.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.40.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.40.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.40.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.40.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.40.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.40.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.40 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.40.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.40.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.40.med)


error.infer.clust.cov.100.women.25.40.cov.40.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.40.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.40.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.40.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.40.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.40.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.40.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.40.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.40.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.40.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.40.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.40.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.40.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.40.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.40.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.40.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.40 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.40.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.40.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.40.sd)


error.infer.clust.cov.100.women.25.40.cov.40.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.40.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.40.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.40.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.40.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.40.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.40.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.40.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.40.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.40.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.40.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.40.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.40.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.40.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.40.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.40.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.40 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.40.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.40.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.40.mean)


error.infer.clust.cov.100.women.40.50.cov.40.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.40.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.40.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.40.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.40.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.40.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.40.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.40.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.40.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.40.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.40.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.40.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.40.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.40.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.40.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.40.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.40 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.40.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.40.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.40.med)


error.infer.clust.cov.100.women.40.50.cov.40.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.40.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.40.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.40.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.40.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.40.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.40.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.40.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.40.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.40.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.40.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.40.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.40.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.40.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.40.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.40.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.40 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.40.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.40.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.40.sd)


error.infer.clust.cov.100.women.40.50.cov.40.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.40.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.40.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.40.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.40.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.40.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.40.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.40.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.40.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.40.sd)


# Cov 45

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.45.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.45.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.45.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.45.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.45.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.45.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.45 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.45.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.45.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.45.mean)


error.infer.clust.cov.100.women.15.25.cov.45.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.45.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.45.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.45.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.45.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.45.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.45.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.45.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.45.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.45.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.45.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.45.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.45.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.45.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.45.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.45.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.45 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.45.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.45.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.45.med)


error.infer.clust.cov.100.women.15.25.cov.45.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.45.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.45.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.45.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.45.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.45.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.45.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.45.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.45.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.45.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.45.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.45.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.45.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.45.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.45.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.45.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.45 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.45.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.45.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.45.sd)


error.infer.clust.cov.100.women.15.25.cov.45.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.45.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.45.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.45.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.45.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.45.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.45.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.45.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.45.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.45.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.45.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.45.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.45.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.45.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.45.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.45.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.45 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.45.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.45.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.45.mean)


error.infer.clust.cov.100.women.25.40.cov.45.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.45.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.45.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.45.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.45.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.45.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.45.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.45.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.45.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.45.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.45.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.45.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.45.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.45.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.45.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.45.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.45 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.45.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.45.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.45.med)


error.infer.clust.cov.100.women.25.40.cov.45.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.45.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.45.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.45.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.45.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.45.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.45.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.45.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.45.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.45.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.45.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.45.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.45.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.45.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.45.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.45.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.45 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.45.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.45.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.45.sd)


error.infer.clust.cov.100.women.25.40.cov.45.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.45.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.45.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.45.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.45.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.45.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.45.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.45.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.45.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.45.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.45.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.45.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.45.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.45.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.45.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.45.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.45 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.45.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.45.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.45.mean)


error.infer.clust.cov.100.women.40.50.cov.45.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.45.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.45.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.45.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.45.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.45.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.45.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.45.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.45.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.45.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.45.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.45.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.45.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.45.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.45.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.45.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.45 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.45.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.45.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.45.med)


error.infer.clust.cov.100.women.40.50.cov.45.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.45.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.45.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.45.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.45.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.45.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.45.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.45.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.45.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.45.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.45.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.45.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.45.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.45.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.45.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.45.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.45 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.45.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.45.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.45.sd)


error.infer.clust.cov.100.women.40.50.cov.45.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.45.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.45.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.45.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.45.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.45.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.45.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.45.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.45.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.45.sd)


# Cov 50

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.50.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.50.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.50.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.50.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.50.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.50.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.50 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.50.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.50.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.50.mean)


error.infer.clust.cov.100.women.15.25.cov.50.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.50.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.50.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.50.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.50.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.50.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.50.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.50.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.50.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.50.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.50.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.50.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.50.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.50.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.50.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.50.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.50 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.50.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.50.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.50.med)


error.infer.clust.cov.100.women.15.25.cov.50.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.50.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.50.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.50.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.50.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.50.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.50.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.50.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.50.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.50.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.50.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.50.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.50.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.50.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.50.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.50.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.50 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.50.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.50.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.50.sd)


error.infer.clust.cov.100.women.15.25.cov.50.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.50.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.50.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.50.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.50.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.50.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.50.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.50.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.50.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.50.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.50.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.50.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.50.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.50.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.50.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.50.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.50 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.50.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.50.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.50.mean)


error.infer.clust.cov.100.women.25.40.cov.50.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.50.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.50.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.50.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.50.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.50.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.50.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.50.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.50.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.50.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.50.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.50.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.50.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.50.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.50.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.50.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.50 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.50.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.50.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.50.med)


error.infer.clust.cov.100.women.25.40.cov.50.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.50.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.50.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.50.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.50.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.50.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.50.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.50.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.50.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.50.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.50.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.50.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.50.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.50.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.50.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.50.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.50 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.50.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.50.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.50.sd)


error.infer.clust.cov.100.women.25.40.cov.50.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.50.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.50.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.50.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.50.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.50.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.50.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.50.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.50.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.50.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.50.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.50.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.50.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.50.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.50.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.50.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.50 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.50.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.50.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.50.mean)


error.infer.clust.cov.100.women.40.50.cov.50.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.50.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.50.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.50.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.50.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.50.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.50.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.50.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.50.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.50.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.50.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.50.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.50.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.50.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.50.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.50.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.50 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.50.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.50.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.50.med)


error.infer.clust.cov.100.women.40.50.cov.50.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.50.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.50.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.50.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.50.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.50.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.50.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.50.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.50.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.50.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.50.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.50.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.50.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.50.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.50.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.50.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.50 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.50.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.50.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.50.sd)


error.infer.clust.cov.100.women.40.50.cov.50.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.50.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.50.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.50.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.50.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.50.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.50.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.50.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.50.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.50.sd)



# Cov 55

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.55.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.55.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.55.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.55.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.55.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.55.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.55 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.55.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.55.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.55.mean)


error.infer.clust.cov.100.women.15.25.cov.55.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.55.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.55.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.55.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.55.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.55.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.55.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.55.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.55.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.55.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.55.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.55.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.55.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.55.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.55.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.55.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.55 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.55.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.55.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.55.med)


error.infer.clust.cov.100.women.15.25.cov.55.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.55.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.55.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.55.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.55.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.55.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.55.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.55.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.55.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.55.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.55.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.55.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.55.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.55.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.55.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.55.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.55 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.55.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.55.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.55.sd)


error.infer.clust.cov.100.women.15.25.cov.55.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.55.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.55.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.55.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.55.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.55.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.55.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.55.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.55.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.55.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.55.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.55.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.55.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.55.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.55.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.55.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.55 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.55.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.55.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.55.mean)


error.infer.clust.cov.100.women.25.40.cov.55.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.55.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.55.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.55.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.55.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.55.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.55.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.55.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.55.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.55.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.55.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.55.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.55.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.55.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.55.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.55.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.55 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.55.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.55.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.55.med)


error.infer.clust.cov.100.women.25.40.cov.55.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.55.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.55.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.55.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.55.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.55.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.55.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.55.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.55.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.55.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.55.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.55.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.55.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.55.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.55.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.55.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.55 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.55.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.55.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.55.sd)


error.infer.clust.cov.100.women.25.40.cov.55.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.55.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.55.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.55.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.55.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.55.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.55.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.55.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.55.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.55.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.55.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.55.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.55.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.55.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.55.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.55.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.55 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.55.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.55.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.55.mean)


error.infer.clust.cov.100.women.40.50.cov.55.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.55.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.55.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.55.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.55.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.55.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.55.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.55.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.55.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.55.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.55.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.55.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.55.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.55.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.55.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.55.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.55 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.55.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.55.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.55.med)


error.infer.clust.cov.100.women.40.50.cov.55.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.55.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.55.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.55.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.55.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.55.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.55.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.55.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.55.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.55.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.55.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.55.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.55.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.55.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.55.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.55.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.55 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.55.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.55.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.55.sd)


error.infer.clust.cov.100.women.40.50.cov.55.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.55.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.55.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.55.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.55.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.55.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.55.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.55.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.55.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.55.sd)


# Cov 60

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.60.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.60.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.60.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.60.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.60.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.60.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.60 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.60.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.60.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.60.mean)


error.infer.clust.cov.100.women.15.25.cov.60.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.60.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.60.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.60.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.60.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.60.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.60.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.60.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.60.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.60.mean)

# Median
error.infer.clust.cov.100.men.15.25.cov.60.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.60.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.60.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.60.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.60.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.60.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.60 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.60.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.60.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.60.med)


error.infer.clust.cov.100.women.15.25.cov.60.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.60.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.60.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.60.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.60.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.60.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.60.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.60.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.60.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.60.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.60.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.60.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.60.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.60.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.60.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.60.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.60 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.60.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.60.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.60.sd)


error.infer.clust.cov.100.women.15.25.cov.60.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.60.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.60.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.60.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.60.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.60.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.60.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.60.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.60.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.60.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.60.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.60.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.60.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.60.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.60.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.60.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.60 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.60.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.60.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.60.mean)


error.infer.clust.cov.100.women.25.40.cov.60.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.60.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.60.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.60.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.60.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.60.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.60.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.60.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.60.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.60.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.60.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.60.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.60.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.60.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.60.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.60.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.60 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.60.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.60.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.60.med)


error.infer.clust.cov.100.women.25.40.cov.60.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.60.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.60.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.60.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.60.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.60.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.60.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.60.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.60.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.60.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.60.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.60.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.60.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.60.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.60.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.60.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.60 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.60.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.60.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.60.sd)


error.infer.clust.cov.100.women.25.40.cov.60.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.60.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.60.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.60.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.60.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.60.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.60.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.60.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.60.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.60.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.60.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.60.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.60.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.60.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.60.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.60.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.60 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.60.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.60.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.60.mean)


error.infer.clust.cov.100.women.40.50.cov.60.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.60.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.60.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.60.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.60.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.60.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.60.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.60.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.60.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.60.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.60.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.60.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.60.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.60.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.60.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.60.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.60 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.60.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.60.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.60.med)


error.infer.clust.cov.100.women.40.50.cov.60.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.60.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.60.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.60.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.60.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.60.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.60.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.60.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.60.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.60.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.60.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.60.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.60.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.60.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.60.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.60.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.60 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.60.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.60.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.60.sd)


error.infer.clust.cov.100.women.40.50.cov.60.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.60.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.60.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.60.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.60.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.60.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.60.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.60.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.60.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.60.sd)



# Cov 65

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.65.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.65.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.65.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.65.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.65.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.65.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.65 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.65.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.65.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.65.mean)



error.infer.clust.cov.100.women.15.25.cov.65.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.65.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.65.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.65.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.65.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.65.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.65.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.65.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.65.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.65.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.65.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.65.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.65.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.65.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.65.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.65.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.65 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.65.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.65.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.65.med)


error.infer.clust.cov.100.women.15.25.cov.65.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.65.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.65.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.65.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.65.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.65.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.65.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.65.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.65.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.65.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.65.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.65.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.65.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.65.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.65.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.65.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.65 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.65.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.65.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.65.sd)


error.infer.clust.cov.100.women.15.25.cov.65.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.65.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.65.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.65.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.65.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.65.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.65.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.65.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.65.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.65.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.65.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.65.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.65.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.65.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.65.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.65.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.65 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.65.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.65.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.65.mean)


error.infer.clust.cov.100.women.25.40.cov.65.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.65.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.65.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.65.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.65.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.65.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.65.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.65.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.65.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.65.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.65.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.65.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.65.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.65.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.65.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.65.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.65 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.65.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.65.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.65.med)


error.infer.clust.cov.100.women.25.40.cov.65.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.65.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.65.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.65.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.65.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.65.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.65.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.65.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.65.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.65.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.65.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.65.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.65.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.65.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.65.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.65.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.65 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.65.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.65.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.65.sd)


error.infer.clust.cov.100.women.25.40.cov.65.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.65.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.65.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.65.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.65.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.65.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.65.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.65.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.65.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.65.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.65.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.65.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.65.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.65.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.65.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.65.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.65 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.65.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.65.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.65.mean)


error.infer.clust.cov.100.women.40.50.cov.65.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.65.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.65.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.65.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.65.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.65.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.65.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.65.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.65.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.65.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.65.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.65.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.65.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.65.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.65.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.65.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.65 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.65.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.65.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.65.med)


error.infer.clust.cov.100.women.40.50.cov.65.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.65.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.65.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.65.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.65.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.65.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.65.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.65.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.65.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.65.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.65.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.65.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.65.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.65.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.65.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.65.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.65 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.65.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.65.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.65.sd)


error.infer.clust.cov.100.women.40.50.cov.65.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.65.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.65.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.65.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.65.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.65.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.65.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.65.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.65.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.65.sd)



# Cov 70

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.70.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.70.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.70.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.70.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.70.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.70.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.70 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.70.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.70.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.70.mean)


error.infer.clust.cov.100.women.15.25.cov.70.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.70.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.70.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.70.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.70.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.70.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.70.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.70.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.70.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.70.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.70.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.70.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.70.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.70.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.70.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.70.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.70 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.70.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.70.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.70.med)


error.infer.clust.cov.100.women.15.25.cov.70.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.70.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.70.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.70.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.70.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.70.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.70.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.70.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.70.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.70.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.70.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.70.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.70.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.70.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.70.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.70.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.70 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.70.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.70.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.70.sd)

error.infer.clust.cov.100.women.15.25.cov.70.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.70.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.70.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.70.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.70.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.70.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.70.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.70.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.70.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.70.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.70.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.70.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.70.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.70.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.70.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.70.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.70 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.70.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.70.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.70.mean)


error.infer.clust.cov.100.women.25.40.cov.70.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.70.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.70.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.70.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.70.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.70.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.70.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.70.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.70.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.70.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.70.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.70.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.70.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.70.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.70.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.70.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.70 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.70.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.70.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.70.med)


error.infer.clust.cov.100.women.25.40.cov.70.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.70.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.70.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.70.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.70.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.70.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.70.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.70.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.70.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.70.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.70.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.70.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.70.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.70.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.70.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.70.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.70 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.70.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.70.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.70.sd)


error.infer.clust.cov.100.women.25.40.cov.70.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.70.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.70.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.70.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.70.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.70.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.70.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.70.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.70.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.70.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.70.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.70.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.70.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.70.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.70.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.70.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.70 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.70.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.70.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.70.mean)



error.infer.clust.cov.100.women.40.50.cov.70.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.70.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.70.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.70.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.70.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.70.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.70.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.70.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.70.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.70.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.70.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.70.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.70.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.70.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.70.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.70.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.70 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.70.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.70.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.70.med)


error.infer.clust.cov.100.women.40.50.cov.70.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.70.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.70.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.70.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.70.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.70.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.70.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.70.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.70.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.70.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.70.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.70.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.70.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.70.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.70.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.70.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.70 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.70.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.70.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.70.sd)


error.infer.clust.cov.100.women.40.50.cov.70.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.70.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.70.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.70.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.70.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.70.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.70.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.70.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.70.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.70.sd)


# Cov 75


# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.75.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.75.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.75.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.75.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.75.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.75.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.75 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.75.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.75.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.75.mean)


error.infer.clust.cov.100.women.15.25.cov.75.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.75.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.75.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.75.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.75.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.75.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.75.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.75.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.75.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.75.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.75.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.75.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.75.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.75.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.75.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.75.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.75 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.75.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.75.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.75.med)


error.infer.clust.cov.100.women.15.25.cov.75.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.75.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.75.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.75.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.75.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.75.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.75.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.75.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.75.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.75.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.75.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.75.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.75.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.75.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.75.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.75.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.75 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.75.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.75.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.75.sd)


error.infer.clust.cov.100.women.15.25.cov.75.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.75.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.75.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.75.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.75.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.75.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.75.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.75.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.75.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.75.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.75.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.75.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.75.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.75.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.75.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.75.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.75 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.75.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.75.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.75.mean)


error.infer.clust.cov.100.women.25.40.cov.75.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.75.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.75.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.75.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.75.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.75.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.75.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.75.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.75.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.75.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.75.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.75.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.75.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.75.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.75.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.75.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.75 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.75.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.75.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.75.med)


error.infer.clust.cov.100.women.25.40.cov.75.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.75.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.75.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.75.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.75.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.75.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.75.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.75.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.75.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.75.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.75.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.75.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.75.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.75.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.75.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.75.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.75 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.75.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.75.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.75.sd)


error.infer.clust.cov.100.women.25.40.cov.75.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.75.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.75.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.75.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.75.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.75.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.75.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.75.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.75.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.75.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.75.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.75.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.75.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.75.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.75.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.75.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.75 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.75.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.75.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.75.mean)


error.infer.clust.cov.100.women.40.50.cov.75.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.75.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.75.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.75.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.75.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.75.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.75.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.75.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.75.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.75.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.75.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.75.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.75.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.75.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.75.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.75.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.75 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.75.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.75.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.75.med)


error.infer.clust.cov.100.women.40.50.cov.75.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.75.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.75.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.75.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.75.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.75.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.75.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.75.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.75.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.75.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.75.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.75.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.75.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.75.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.75.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.75.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.75 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.75.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.75.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.75.sd)


error.infer.clust.cov.100.women.40.50.cov.75.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.75.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.75.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.75.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.75.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.75.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.75.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.75.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.75.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.75.sd)


# Cov 80


# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.80.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.80.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.80.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.80.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.80.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.80.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.80 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.80.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.80.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.80.mean)


error.infer.clust.cov.100.women.15.25.cov.80.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.80.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.80.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.80.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.80.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.80.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.80.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.80.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.80.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.80.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.80.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.80.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.80.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.80.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.80.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.80.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.80 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.80.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.80.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.80.med)


error.infer.clust.cov.100.women.15.25.cov.80.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.80.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.80.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.80.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.80.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.80.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.80.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.80.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.80.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.80.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.80.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.80.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.80.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.80.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.80.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.80.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.80 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.80.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.80.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.80.sd)


error.infer.clust.cov.100.women.15.25.cov.80.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.80.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.80.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.80.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.80.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.80.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.80.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.80.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.80.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.80.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.80.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.80.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.80.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.80.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.80.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.80.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.80 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.80.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.80.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.80.mean)


error.infer.clust.cov.100.women.25.40.cov.80.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.80.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.80.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.80.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.80.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.80.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.80.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.80.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.80.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.80.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.80.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.80.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.80.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.80.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.80.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.80.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.80 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.80.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.80.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.80.med)


error.infer.clust.cov.100.women.25.40.cov.80.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.80.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.80.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.80.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.80.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.80.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.80.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.80.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.80.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.80.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.80.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.80.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.80.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.80.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.80.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.80.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.80 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.80.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.80.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.80.sd)


error.infer.clust.cov.100.women.25.40.cov.80.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.80.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.80.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.80.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.80.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.80.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.80.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.80.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.80.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.80.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.80.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.80.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.80.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.80.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.80.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.80.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.80 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.80.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.80.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.80.mean)


error.infer.clust.cov.100.women.40.50.cov.80.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.80.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.80.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.80.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.80.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.80.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.80.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.80.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.80.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.80.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.80.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.80.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.80.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.80.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.80.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.80.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.80 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.80.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.80.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.80.med)


error.infer.clust.cov.100.women.40.50.cov.80.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.80.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.80.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.80.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.80.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.80.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.80.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.80.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.80.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.80.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.80.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.80.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.80.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.80.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.80.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.80.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.80 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.80.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.80.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.80.sd)


error.infer.clust.cov.100.women.40.50.cov.80.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.80.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.80.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.80.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.80.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.80.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.80.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.80.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.80.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.80.sd)


# Cov 85

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.85.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.85.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.85.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.85.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.85.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.85.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.85 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.85.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.85.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.85.mean)


error.infer.clust.cov.100.women.15.25.cov.85.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.85.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.85.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.85.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.85.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.85.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.85.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.85.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.85.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.85.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.85.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.85.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.85.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.85.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.85.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.85.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.85 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.85.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.85.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.85.med)


error.infer.clust.cov.100.women.15.25.cov.85.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.85.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.85.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.85.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.85.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.85.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.85.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.85.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.85.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.85.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.85.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.85.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.85.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.85.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.85.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.85.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.85 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.85.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.85.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.85.sd)


error.infer.clust.cov.100.women.15.25.cov.85.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.85.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.85.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.85.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.85.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.85.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.85.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.85.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.85.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.85.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.85.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.85.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.85.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.85.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.85.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.85.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.85 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.85.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.85.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.85.mean)


error.infer.clust.cov.100.women.25.40.cov.85.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.85.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.85.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.85.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.85.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.85.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.85.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.85.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.85.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.85.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.85.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.85.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.85.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.85.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.85.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.85.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.85 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.85.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.85.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.85.med)


error.infer.clust.cov.100.women.25.40.cov.85.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.85.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.85.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.85.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.85.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.85.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.85.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.85.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.85.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.85.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.85.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.85.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.85.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.85.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.85.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.85.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.85 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.85.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.85.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.85.sd)


error.infer.clust.cov.100.women.25.40.cov.85.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.85.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.85.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.85.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.85.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.85.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.85.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.85.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.85.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.85.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.85.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.85.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.85.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.85.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.85.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.85.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.85 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.85.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.85.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.85.mean)


error.infer.clust.cov.100.women.40.50.cov.85.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.85.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.85.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.85.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.85.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.85.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.85.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.85.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.85.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.85.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.85.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.85.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.85.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.85.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.85.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.85.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.85 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.85.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.85.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.85.med)


error.infer.clust.cov.100.women.40.50.cov.85.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.85.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.85.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.85.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.85.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.85.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.85.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.85.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.85.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.85.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.85.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.85.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.85.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.85.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.85.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.85.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.85 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.85.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.85.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.85.sd)


error.infer.clust.cov.100.women.40.50.cov.85.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.85.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.85.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.85.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.85.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.85.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.85.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.85.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.85.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.85.sd)


# Cov 90

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.90.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.90.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.90.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.90.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.90.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.90.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.90 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.90.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.90.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.90.mean)


error.infer.clust.cov.100.women.15.25.cov.90.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.90.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.90.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.90.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.90.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.90.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.90.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.90.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.90.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.90.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.90.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.90.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.90.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.90.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.90.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.90.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.90 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.90.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.90.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.90.med)


error.infer.clust.cov.100.women.15.25.cov.90.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.90.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.90.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.90.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.90.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.90.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.90.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.90.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.90.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.90.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.90.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.90.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.90.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.90.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.90.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.90.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.90 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.90.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.90.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.90.sd)


error.infer.clust.cov.100.women.15.25.cov.90.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.90.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.90.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.90.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.90.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.90.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.90.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.90.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.90.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.90.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.90.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.90.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.90.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.90.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.90.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.90.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.90 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.90.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.90.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.90.mean)


error.infer.clust.cov.100.women.25.40.cov.90.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.90.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.90.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.90.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.90.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.90.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.90.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.90.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.90.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.90.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.90.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.90.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.90.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.90.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.90.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.90.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.90 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.90.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.90.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.90.med)


error.infer.clust.cov.100.women.25.40.cov.90.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.90.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.90.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.90.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.90.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.90.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.90.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.90.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.90.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.90.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.90.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.90.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.90.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.90.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.90.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.90.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.90 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.90.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.90.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.90.sd)


error.infer.clust.cov.100.women.25.40.cov.90.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.90.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.90.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.90.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.90.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.90.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.90.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.90.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.90.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.90.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.90.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.90.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.90.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.90.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.90.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.90.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.90 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.90.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.90.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.90.mean)


error.infer.clust.cov.100.women.40.50.cov.90.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.90.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.90.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.90.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.90.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.90.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.90.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.90.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.90.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.90.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.90.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.90.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.90.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.90.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.90.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.90.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.90 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.90.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.90.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.90.med)


error.infer.clust.cov.100.women.40.50.cov.90.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.90.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.90.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.90.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.90.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.90.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.90.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.90.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.90.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.90.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.90.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.90.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.90.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.90.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.90.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.90.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.90 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.90.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.90.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.90.sd)


error.infer.clust.cov.100.women.40.50.cov.90.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.90.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.90.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.90.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.90.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.90.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.90.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.90.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.90.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.90.sd)



# Cov 95

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.95.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.95.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.95.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.95.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.95.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.95.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.95 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.95.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.95.mean <- MRE(error.infer.clust.cov.100.men.15.25.cov.95.mean)


error.infer.clust.cov.100.women.15.25.cov.95.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.95.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.95.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.95.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.95.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.95.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.95.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.95.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.95.mean <- MRE(error.infer.clust.cov.100.women.15.25.cov.95.mean)


# Median
error.infer.clust.cov.100.men.15.25.cov.95.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.95.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.95.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.95.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.95.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.95.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.95 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.95.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.95.med <- MRE(error.infer.clust.cov.100.men.15.25.cov.95.med)


error.infer.clust.cov.100.women.15.25.cov.95.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.95.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.95.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.95.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.95.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.95.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.95.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.95.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.95.med <- MRE(error.infer.clust.cov.100.women.15.25.cov.95.med)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.95.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.95.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.95.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.95.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.95.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.95.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.95 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.95.AD.men.cl.15.25)
MRE.error.infer.clust.cov.100.men.15.25.cov.95.sd <- MRE(error.infer.clust.cov.100.men.15.25.cov.95.sd)


error.infer.clust.cov.100.women.15.25.cov.95.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.95.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.95.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.95.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.95.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.95.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.95.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.95.AD.women.cl.15.25)
MRE.error.infer.clust.cov.100.women.15.25.cov.95.sd <- MRE(error.infer.clust.cov.100.women.15.25.cov.95.sd)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.95.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.95.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.95.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.95.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.95.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.95.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.95 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.95.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.95.mean <- MRE(error.infer.clust.cov.100.men.25.40.cov.95.mean)


error.infer.clust.cov.100.women.25.40.cov.95.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.95.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.95.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.95.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.95.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.95.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.95.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.95.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.95.mean <- MRE(error.infer.clust.cov.100.women.25.40.cov.95.mean)


# Median
error.infer.clust.cov.100.men.25.40.cov.95.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.95.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.95.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.95.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.95.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.95.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.95 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.95.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.95.med <- MRE(error.infer.clust.cov.100.men.25.40.cov.95.med)


error.infer.clust.cov.100.women.25.40.cov.95.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.95.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.95.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.95.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.95.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.95.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.95.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.95.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.95.med <- MRE(error.infer.clust.cov.100.women.25.40.cov.95.med)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.95.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.95.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.95.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.95.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.95.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.95.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.95 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.95.AD.men.cl.25.40)
MRE.error.infer.clust.cov.100.men.25.40.cov.95.sd <- MRE(error.infer.clust.cov.100.men.25.40.cov.95.sd)


error.infer.clust.cov.100.women.25.40.cov.95.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.95.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.95.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.95.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.95.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.95.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.95.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.95.AD.women.cl.25.40)
MRE.error.infer.clust.cov.100.women.25.40.cov.95.sd <- MRE(error.infer.clust.cov.100.women.25.40.cov.95.sd)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.95.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.95.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.95.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.95.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.95.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.95.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.95 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.95.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.95.mean <- MRE(error.infer.clust.cov.100.men.40.50.cov.95.mean)


error.infer.clust.cov.100.women.40.50.cov.95.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.95.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.95.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.95.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.95.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.95.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.95.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.95.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.95.mean <- MRE(error.infer.clust.cov.100.women.40.50.cov.95.mean)


# Median
error.infer.clust.cov.100.men.40.50.cov.95.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.95.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.95.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.95.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.95.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.95.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.95 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.95.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.95.med <- MRE(error.infer.clust.cov.100.men.40.50.cov.95.med)


error.infer.clust.cov.100.women.40.50.cov.95.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.95.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.95.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.95.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.95.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.95.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.95.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.95.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.95.med <- MRE(error.infer.clust.cov.100.women.40.50.cov.95.med)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.95.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.95.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.95.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.95.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.95.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.95.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.95 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.95.AD.men.cl.40.50)
MRE.error.infer.clust.cov.100.men.40.50.cov.95.sd <- MRE(error.infer.clust.cov.100.men.40.50.cov.95.sd)


error.infer.clust.cov.100.women.40.50.cov.95.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.95.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.95.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.95.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.95.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.95.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.95.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.95.AD.women.cl.40.50)
MRE.error.infer.clust.cov.100.women.40.50.cov.95.sd <- MRE(error.infer.clust.cov.100.women.40.50.cov.95.sd)






# Visualization AD statistics errors --------------------------------------


# 15 - 25

MRE.error.infer.clust.cov.100.women.15.25.AD.mean <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                                
                                                                F = c(MRE.error.infer.clust.cov.100.women.15.25.cov.35.mean, MRE.error.infer.clust.cov.100.women.15.25.cov.40.mean,
                                                                      MRE.error.infer.clust.cov.100.women.15.25.cov.45.mean, MRE.error.infer.clust.cov.100.women.15.25.cov.50.mean,
                                                                      MRE.error.infer.clust.cov.100.women.15.25.cov.55.mean, MRE.error.infer.clust.cov.100.women.15.25.cov.60.mean,
                                                                      MRE.error.infer.clust.cov.100.women.15.25.cov.65.mean, MRE.error.infer.clust.cov.100.women.15.25.cov.70.mean, 
                                                                      MRE.error.infer.clust.cov.100.women.15.25.cov.75.mean, MRE.error.infer.clust.cov.100.women.15.25.cov.80.mean,
                                                                      MRE.error.infer.clust.cov.100.women.15.25.cov.85.mean, MRE.error.infer.clust.cov.100.women.15.25.cov.90.mean,
                                                                      MRE.error.infer.clust.cov.100.women.15.25.cov.95.mean))


plot.MRE.error.infer.clust.cov.100.women.15.25.AD.mean <- ggplot(MRE.error.infer.clust.cov.100.women.15.25.AD.mean, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for mean age difference for women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.15.25.AD.mean.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.15.25.AD.mean,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")





MRE.error.infer.clust.cov.100.men.15.25.AD.mean <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                              
                                                              F = c(MRE.error.infer.clust.cov.100.men.15.25.cov.35.mean, MRE.error.infer.clust.cov.100.men.15.25.cov.40.mean,
                                                                    MRE.error.infer.clust.cov.100.men.15.25.cov.45.mean, MRE.error.infer.clust.cov.100.men.15.25.cov.50.mean,
                                                                    MRE.error.infer.clust.cov.100.men.15.25.cov.55.mean, MRE.error.infer.clust.cov.100.men.15.25.cov.60.mean,
                                                                    MRE.error.infer.clust.cov.100.men.15.25.cov.65.mean, MRE.error.infer.clust.cov.100.men.15.25.cov.70.mean, 
                                                                    MRE.error.infer.clust.cov.100.men.15.25.cov.75.mean, MRE.error.infer.clust.cov.100.men.15.25.cov.80.mean,
                                                                    MRE.error.infer.clust.cov.100.men.15.25.cov.85.mean, MRE.error.infer.clust.cov.100.men.15.25.cov.90.mean,
                                                                    MRE.error.infer.clust.cov.100.men.15.25.cov.95.mean))


plot.MRE.error.infer.clust.cov.100.men.15.25.AD.mean <- ggplot(MRE.error.infer.clust.cov.100.men.15.25.AD.mean, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for mean age difference for men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.15.25.AD.mean.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.15.25.AD.mean,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")





MRE.error.infer.clust.cov.100.women.15.25.AD.med <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                               
                                                               F = c(MRE.error.infer.clust.cov.100.women.15.25.cov.35.med, MRE.error.infer.clust.cov.100.women.15.25.cov.40.med,
                                                                     MRE.error.infer.clust.cov.100.women.15.25.cov.45.med, MRE.error.infer.clust.cov.100.women.15.25.cov.50.med,
                                                                     MRE.error.infer.clust.cov.100.women.15.25.cov.55.med, MRE.error.infer.clust.cov.100.women.15.25.cov.60.med,
                                                                     MRE.error.infer.clust.cov.100.women.15.25.cov.65.med, MRE.error.infer.clust.cov.100.women.15.25.cov.70.med, 
                                                                     MRE.error.infer.clust.cov.100.women.15.25.cov.75.med, MRE.error.infer.clust.cov.100.women.15.25.cov.80.med,
                                                                     MRE.error.infer.clust.cov.100.women.15.25.cov.85.med, MRE.error.infer.clust.cov.100.women.15.25.cov.90.med,
                                                                     MRE.error.infer.clust.cov.100.women.15.25.cov.95.med))


plot.MRE.error.infer.clust.cov.100.women.15.25.AD.med <- ggplot(MRE.error.infer.clust.cov.100.women.15.25.AD.med, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for median age difference for women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.15.25.AD.med.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.15.25.AD.med,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.men.15.25.AD.med <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                             
                                                             F = c(MRE.error.infer.clust.cov.100.men.15.25.cov.35.med, MRE.error.infer.clust.cov.100.men.15.25.cov.40.med,
                                                                   MRE.error.infer.clust.cov.100.men.15.25.cov.45.med, MRE.error.infer.clust.cov.100.men.15.25.cov.50.med,
                                                                   MRE.error.infer.clust.cov.100.men.15.25.cov.55.med, MRE.error.infer.clust.cov.100.men.15.25.cov.60.med,
                                                                   MRE.error.infer.clust.cov.100.men.15.25.cov.65.med, MRE.error.infer.clust.cov.100.men.15.25.cov.70.med, 
                                                                   MRE.error.infer.clust.cov.100.men.15.25.cov.75.med, MRE.error.infer.clust.cov.100.men.15.25.cov.80.med,
                                                                   MRE.error.infer.clust.cov.100.men.15.25.cov.85.med, MRE.error.infer.clust.cov.100.men.15.25.cov.90.med,
                                                                   MRE.error.infer.clust.cov.100.men.15.25.cov.95.med))


plot.MRE.error.infer.clust.cov.100.men.15.25.AD.med <- ggplot(MRE.error.infer.clust.cov.100.men.15.25.AD.med, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for median age difference for men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.15.25.AD.med.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.15.25.AD.med,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.women.15.25.AD.sd <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                              
                                                              F = c(MRE.error.infer.clust.cov.100.women.15.25.cov.35.sd, MRE.error.infer.clust.cov.100.women.15.25.cov.40.sd,
                                                                    MRE.error.infer.clust.cov.100.women.15.25.cov.45.sd, MRE.error.infer.clust.cov.100.women.15.25.cov.50.sd,
                                                                    MRE.error.infer.clust.cov.100.women.15.25.cov.55.sd, MRE.error.infer.clust.cov.100.women.15.25.cov.60.sd,
                                                                    MRE.error.infer.clust.cov.100.women.15.25.cov.65.sd, MRE.error.infer.clust.cov.100.women.15.25.cov.70.sd, 
                                                                    MRE.error.infer.clust.cov.100.women.15.25.cov.75.sd, MRE.error.infer.clust.cov.100.women.15.25.cov.80.sd,
                                                                    MRE.error.infer.clust.cov.100.women.15.25.cov.85.sd, MRE.error.infer.clust.cov.100.women.15.25.cov.90.sd,
                                                                    MRE.error.infer.clust.cov.100.women.15.25.cov.95.sd))


plot.MRE.error.infer.clust.cov.100.women.15.25.AD.sd <- ggplot(MRE.error.infer.clust.cov.100.women.15.25.AD.sd, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for sdian age difference for women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.15.25.AD.sd.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.15.25.AD.sd,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.men.15.25.AD.sd <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                            
                                                            F = c(MRE.error.infer.clust.cov.100.men.15.25.cov.35.sd, MRE.error.infer.clust.cov.100.men.15.25.cov.40.sd,
                                                                  MRE.error.infer.clust.cov.100.men.15.25.cov.45.sd, MRE.error.infer.clust.cov.100.men.15.25.cov.50.sd,
                                                                  MRE.error.infer.clust.cov.100.men.15.25.cov.55.sd, MRE.error.infer.clust.cov.100.men.15.25.cov.60.sd,
                                                                  MRE.error.infer.clust.cov.100.men.15.25.cov.65.sd, MRE.error.infer.clust.cov.100.men.15.25.cov.70.sd, 
                                                                  MRE.error.infer.clust.cov.100.men.15.25.cov.75.sd, MRE.error.infer.clust.cov.100.men.15.25.cov.80.sd,
                                                                  MRE.error.infer.clust.cov.100.men.15.25.cov.85.sd, MRE.error.infer.clust.cov.100.men.15.25.cov.90.sd,
                                                                  MRE.error.infer.clust.cov.100.men.15.25.cov.95.sd))


plot.MRE.error.infer.clust.cov.100.men.15.25.AD.sd <- ggplot(MRE.error.infer.clust.cov.100.men.15.25.AD.sd, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for sdian age difference for men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.15.25.AD.sd.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.15.25.AD.sd,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# 25 - 40

MRE.error.infer.clust.cov.100.women.25.40.AD.mean <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                                
                                                                F = c(MRE.error.infer.clust.cov.100.women.25.40.cov.35.mean, MRE.error.infer.clust.cov.100.women.25.40.cov.40.mean,
                                                                      MRE.error.infer.clust.cov.100.women.25.40.cov.45.mean, MRE.error.infer.clust.cov.100.women.25.40.cov.50.mean,
                                                                      MRE.error.infer.clust.cov.100.women.25.40.cov.55.mean, MRE.error.infer.clust.cov.100.women.25.40.cov.60.mean,
                                                                      MRE.error.infer.clust.cov.100.women.25.40.cov.65.mean, MRE.error.infer.clust.cov.100.women.25.40.cov.70.mean, 
                                                                      MRE.error.infer.clust.cov.100.women.25.40.cov.75.mean, MRE.error.infer.clust.cov.100.women.25.40.cov.80.mean,
                                                                      MRE.error.infer.clust.cov.100.women.25.40.cov.85.mean, MRE.error.infer.clust.cov.100.women.25.40.cov.90.mean,
                                                                      MRE.error.infer.clust.cov.100.women.25.40.cov.95.mean))


plot.MRE.error.infer.clust.cov.100.women.25.40.AD.mean <- ggplot(MRE.error.infer.clust.cov.100.women.25.40.AD.mean, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for mean age difference for women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.25.40.AD.mean.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.25.40.AD.mean,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")





MRE.error.infer.clust.cov.100.men.25.40.AD.mean <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                              
                                                              F = c(MRE.error.infer.clust.cov.100.men.25.40.cov.35.mean, MRE.error.infer.clust.cov.100.men.25.40.cov.40.mean,
                                                                    MRE.error.infer.clust.cov.100.men.25.40.cov.45.mean, MRE.error.infer.clust.cov.100.men.25.40.cov.50.mean,
                                                                    MRE.error.infer.clust.cov.100.men.25.40.cov.55.mean, MRE.error.infer.clust.cov.100.men.25.40.cov.60.mean,
                                                                    MRE.error.infer.clust.cov.100.men.25.40.cov.65.mean, MRE.error.infer.clust.cov.100.men.25.40.cov.70.mean, 
                                                                    MRE.error.infer.clust.cov.100.men.25.40.cov.75.mean, MRE.error.infer.clust.cov.100.men.25.40.cov.80.mean,
                                                                    MRE.error.infer.clust.cov.100.men.25.40.cov.85.mean, MRE.error.infer.clust.cov.100.men.25.40.cov.90.mean,
                                                                    MRE.error.infer.clust.cov.100.men.25.40.cov.95.mean))


plot.MRE.error.infer.clust.cov.100.men.25.40.AD.mean <- ggplot(MRE.error.infer.clust.cov.100.men.25.40.AD.mean, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for mean age difference for men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.25.40.AD.mean.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.25.40.AD.mean,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")





MRE.error.infer.clust.cov.100.women.25.40.AD.med <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                               
                                                               F = c(MRE.error.infer.clust.cov.100.women.25.40.cov.35.med, MRE.error.infer.clust.cov.100.women.25.40.cov.40.med,
                                                                     MRE.error.infer.clust.cov.100.women.25.40.cov.45.med, MRE.error.infer.clust.cov.100.women.25.40.cov.50.med,
                                                                     MRE.error.infer.clust.cov.100.women.25.40.cov.55.med, MRE.error.infer.clust.cov.100.women.25.40.cov.60.med,
                                                                     MRE.error.infer.clust.cov.100.women.25.40.cov.65.med, MRE.error.infer.clust.cov.100.women.25.40.cov.70.med, 
                                                                     MRE.error.infer.clust.cov.100.women.25.40.cov.75.med, MRE.error.infer.clust.cov.100.women.25.40.cov.80.med,
                                                                     MRE.error.infer.clust.cov.100.women.25.40.cov.85.med, MRE.error.infer.clust.cov.100.women.25.40.cov.90.med,
                                                                     MRE.error.infer.clust.cov.100.women.25.40.cov.95.med))


plot.MRE.error.infer.clust.cov.100.women.25.40.AD.med <- ggplot(MRE.error.infer.clust.cov.100.women.25.40.AD.med, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for median age difference for women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.25.40.AD.med.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.25.40.AD.med,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.men.25.40.AD.med <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                             
                                                             F = c(MRE.error.infer.clust.cov.100.men.25.40.cov.35.med, MRE.error.infer.clust.cov.100.men.25.40.cov.40.med,
                                                                   MRE.error.infer.clust.cov.100.men.25.40.cov.45.med, MRE.error.infer.clust.cov.100.men.25.40.cov.50.med,
                                                                   MRE.error.infer.clust.cov.100.men.25.40.cov.55.med, MRE.error.infer.clust.cov.100.men.25.40.cov.60.med,
                                                                   MRE.error.infer.clust.cov.100.men.25.40.cov.65.med, MRE.error.infer.clust.cov.100.men.25.40.cov.70.med, 
                                                                   MRE.error.infer.clust.cov.100.men.25.40.cov.75.med, MRE.error.infer.clust.cov.100.men.25.40.cov.80.med,
                                                                   MRE.error.infer.clust.cov.100.men.25.40.cov.85.med, MRE.error.infer.clust.cov.100.men.25.40.cov.90.med,
                                                                   MRE.error.infer.clust.cov.100.men.25.40.cov.95.med))


plot.MRE.error.infer.clust.cov.100.men.25.40.AD.med <- ggplot(MRE.error.infer.clust.cov.100.men.25.40.AD.med, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for median age difference for men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.25.40.AD.med.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.25.40.AD.med,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.women.25.40.AD.sd <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                              
                                                              F = c(MRE.error.infer.clust.cov.100.women.25.40.cov.35.sd, MRE.error.infer.clust.cov.100.women.25.40.cov.40.sd,
                                                                    MRE.error.infer.clust.cov.100.women.25.40.cov.45.sd, MRE.error.infer.clust.cov.100.women.25.40.cov.50.sd,
                                                                    MRE.error.infer.clust.cov.100.women.25.40.cov.55.sd, MRE.error.infer.clust.cov.100.women.25.40.cov.60.sd,
                                                                    MRE.error.infer.clust.cov.100.women.25.40.cov.65.sd, MRE.error.infer.clust.cov.100.women.25.40.cov.70.sd, 
                                                                    MRE.error.infer.clust.cov.100.women.25.40.cov.75.sd, MRE.error.infer.clust.cov.100.women.25.40.cov.80.sd,
                                                                    MRE.error.infer.clust.cov.100.women.25.40.cov.85.sd, MRE.error.infer.clust.cov.100.women.25.40.cov.90.sd,
                                                                    MRE.error.infer.clust.cov.100.women.25.40.cov.95.sd))


plot.MRE.error.infer.clust.cov.100.women.25.40.AD.sd <- ggplot(MRE.error.infer.clust.cov.100.women.25.40.AD.sd, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for sdian age difference for women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.25.40.AD.sd.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.25.40.AD.sd,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.men.25.40.AD.sd <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                            
                                                            F = c(MRE.error.infer.clust.cov.100.men.25.40.cov.35.sd, MRE.error.infer.clust.cov.100.men.25.40.cov.40.sd,
                                                                  MRE.error.infer.clust.cov.100.men.25.40.cov.45.sd, MRE.error.infer.clust.cov.100.men.25.40.cov.50.sd,
                                                                  MRE.error.infer.clust.cov.100.men.25.40.cov.55.sd, MRE.error.infer.clust.cov.100.men.25.40.cov.60.sd,
                                                                  MRE.error.infer.clust.cov.100.men.25.40.cov.65.sd, MRE.error.infer.clust.cov.100.men.25.40.cov.70.sd, 
                                                                  MRE.error.infer.clust.cov.100.men.25.40.cov.75.sd, MRE.error.infer.clust.cov.100.men.25.40.cov.80.sd,
                                                                  MRE.error.infer.clust.cov.100.men.25.40.cov.85.sd, MRE.error.infer.clust.cov.100.men.25.40.cov.90.sd,
                                                                  MRE.error.infer.clust.cov.100.men.25.40.cov.95.sd))


plot.MRE.error.infer.clust.cov.100.men.25.40.AD.sd <- ggplot(MRE.error.infer.clust.cov.100.men.25.40.AD.sd, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for sdian age difference for men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.25.40.AD.sd.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.25.40.AD.sd,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")




# 40 - 50

MRE.error.infer.clust.cov.100.women.40.50.AD.mean <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                                
                                                                F = c(MRE.error.infer.clust.cov.100.women.40.50.cov.35.mean, MRE.error.infer.clust.cov.100.women.40.50.cov.40.mean,
                                                                      MRE.error.infer.clust.cov.100.women.40.50.cov.45.mean, MRE.error.infer.clust.cov.100.women.40.50.cov.50.mean,
                                                                      MRE.error.infer.clust.cov.100.women.40.50.cov.55.mean, MRE.error.infer.clust.cov.100.women.40.50.cov.60.mean,
                                                                      MRE.error.infer.clust.cov.100.women.40.50.cov.65.mean, MRE.error.infer.clust.cov.100.women.40.50.cov.70.mean, 
                                                                      MRE.error.infer.clust.cov.100.women.40.50.cov.75.mean, MRE.error.infer.clust.cov.100.women.40.50.cov.80.mean,
                                                                      MRE.error.infer.clust.cov.100.women.40.50.cov.85.mean, MRE.error.infer.clust.cov.100.women.40.50.cov.90.mean,
                                                                      MRE.error.infer.clust.cov.100.women.40.50.cov.95.mean))


plot.MRE.error.infer.clust.cov.100.women.40.50.AD.mean <- ggplot(MRE.error.infer.clust.cov.100.women.40.50.AD.mean, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for mean age difference for women in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.40.50.AD.mean.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.40.50.AD.mean,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")





MRE.error.infer.clust.cov.100.men.40.50.AD.mean <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                              
                                                              F = c(MRE.error.infer.clust.cov.100.men.40.50.cov.35.mean, MRE.error.infer.clust.cov.100.men.40.50.cov.40.mean,
                                                                    MRE.error.infer.clust.cov.100.men.40.50.cov.45.mean, MRE.error.infer.clust.cov.100.men.40.50.cov.50.mean,
                                                                    MRE.error.infer.clust.cov.100.men.40.50.cov.55.mean, MRE.error.infer.clust.cov.100.men.40.50.cov.60.mean,
                                                                    MRE.error.infer.clust.cov.100.men.40.50.cov.65.mean, MRE.error.infer.clust.cov.100.men.40.50.cov.70.mean, 
                                                                    MRE.error.infer.clust.cov.100.men.40.50.cov.75.mean, MRE.error.infer.clust.cov.100.men.40.50.cov.80.mean,
                                                                    MRE.error.infer.clust.cov.100.men.40.50.cov.85.mean, MRE.error.infer.clust.cov.100.men.40.50.cov.90.mean,
                                                                    MRE.error.infer.clust.cov.100.men.40.50.cov.95.mean))


plot.MRE.error.infer.clust.cov.100.men.40.50.AD.mean <- ggplot(MRE.error.infer.clust.cov.100.men.40.50.AD.mean, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for mean age difference for men in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.40.50.AD.mean.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.40.50.AD.mean,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")





MRE.error.infer.clust.cov.100.women.40.50.AD.med <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                               
                                                               F = c(MRE.error.infer.clust.cov.100.women.40.50.cov.35.med, MRE.error.infer.clust.cov.100.women.40.50.cov.40.med,
                                                                     MRE.error.infer.clust.cov.100.women.40.50.cov.45.med, MRE.error.infer.clust.cov.100.women.40.50.cov.50.med,
                                                                     MRE.error.infer.clust.cov.100.women.40.50.cov.55.med, MRE.error.infer.clust.cov.100.women.40.50.cov.60.med,
                                                                     MRE.error.infer.clust.cov.100.women.40.50.cov.65.med, MRE.error.infer.clust.cov.100.women.40.50.cov.70.med, 
                                                                     MRE.error.infer.clust.cov.100.women.40.50.cov.75.med, MRE.error.infer.clust.cov.100.women.40.50.cov.80.med,
                                                                     MRE.error.infer.clust.cov.100.women.40.50.cov.85.med, MRE.error.infer.clust.cov.100.women.40.50.cov.90.med,
                                                                     MRE.error.infer.clust.cov.100.women.40.50.cov.95.med))


plot.MRE.error.infer.clust.cov.100.women.40.50.AD.med <- ggplot(MRE.error.infer.clust.cov.100.women.40.50.AD.med, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for median age difference for women in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.40.50.AD.med.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.40.50.AD.med,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.men.40.50.AD.med <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                             
                                                             F = c(MRE.error.infer.clust.cov.100.men.40.50.cov.35.med, MRE.error.infer.clust.cov.100.men.40.50.cov.40.med,
                                                                   MRE.error.infer.clust.cov.100.men.40.50.cov.45.med, MRE.error.infer.clust.cov.100.men.40.50.cov.50.med,
                                                                   MRE.error.infer.clust.cov.100.men.40.50.cov.55.med, MRE.error.infer.clust.cov.100.men.40.50.cov.60.med,
                                                                   MRE.error.infer.clust.cov.100.men.40.50.cov.65.med, MRE.error.infer.clust.cov.100.men.40.50.cov.70.med, 
                                                                   MRE.error.infer.clust.cov.100.men.40.50.cov.75.med, MRE.error.infer.clust.cov.100.men.40.50.cov.80.med,
                                                                   MRE.error.infer.clust.cov.100.men.40.50.cov.85.med, MRE.error.infer.clust.cov.100.men.40.50.cov.90.med,
                                                                   MRE.error.infer.clust.cov.100.men.40.50.cov.95.med))


plot.MRE.error.infer.clust.cov.100.men.40.50.AD.med <- ggplot(MRE.error.infer.clust.cov.100.men.40.50.AD.med, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for median age difference for men in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.40.50.AD.med.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.40.50.AD.med,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.women.40.50.AD.sd <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                              
                                                              F = c(MRE.error.infer.clust.cov.100.women.40.50.cov.35.sd, MRE.error.infer.clust.cov.100.women.40.50.cov.40.sd,
                                                                    MRE.error.infer.clust.cov.100.women.40.50.cov.45.sd, MRE.error.infer.clust.cov.100.women.40.50.cov.50.sd,
                                                                    MRE.error.infer.clust.cov.100.women.40.50.cov.55.sd, MRE.error.infer.clust.cov.100.women.40.50.cov.60.sd,
                                                                    MRE.error.infer.clust.cov.100.women.40.50.cov.65.sd, MRE.error.infer.clust.cov.100.women.40.50.cov.70.sd, 
                                                                    MRE.error.infer.clust.cov.100.women.40.50.cov.75.sd, MRE.error.infer.clust.cov.100.women.40.50.cov.80.sd,
                                                                    MRE.error.infer.clust.cov.100.women.40.50.cov.85.sd, MRE.error.infer.clust.cov.100.women.40.50.cov.90.sd,
                                                                    MRE.error.infer.clust.cov.100.women.40.50.cov.95.sd))


plot.MRE.error.infer.clust.cov.100.women.40.50.AD.sd <- ggplot(MRE.error.infer.clust.cov.100.women.40.50.AD.sd, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for sdian age difference for women in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.women.40.50.AD.sd.png",
       plot = plot.MRE.error.infer.clust.cov.100.women.40.50.AD.sd,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")



MRE.error.infer.clust.cov.100.men.40.50.AD.sd <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                                            
                                                            F = c(MRE.error.infer.clust.cov.100.men.40.50.cov.35.sd, MRE.error.infer.clust.cov.100.men.40.50.cov.40.sd,
                                                                  MRE.error.infer.clust.cov.100.men.40.50.cov.45.sd, MRE.error.infer.clust.cov.100.men.40.50.cov.50.sd,
                                                                  MRE.error.infer.clust.cov.100.men.40.50.cov.55.sd, MRE.error.infer.clust.cov.100.men.40.50.cov.60.sd,
                                                                  MRE.error.infer.clust.cov.100.men.40.50.cov.65.sd, MRE.error.infer.clust.cov.100.men.40.50.cov.70.sd, 
                                                                  MRE.error.infer.clust.cov.100.men.40.50.cov.75.sd, MRE.error.infer.clust.cov.100.men.40.50.cov.80.sd,
                                                                  MRE.error.infer.clust.cov.100.men.40.50.cov.85.sd, MRE.error.infer.clust.cov.100.men.40.50.cov.90.sd,
                                                                  MRE.error.infer.clust.cov.100.men.40.50.cov.95.sd))


plot.MRE.error.infer.clust.cov.100.men.40.50.AD.sd <- ggplot(MRE.error.infer.clust.cov.100.men.40.50.AD.sd, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error for sdian age difference for men in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")


ggsave(filename = "plot.MRE.error.infer.clust.cov.100.men.40.50.AD.sd.png",
       plot = plot.MRE.error.infer.clust.cov.100.men.40.50.AD.sd,
       path = "~/age_mixing_AD_clusters",
       width = 10, height = 8, units = "cm")


