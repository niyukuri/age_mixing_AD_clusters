


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



# df = read.csv("~/Dropbox/25.10.2018.age.mix2/age.mixing.large.AD/Results.mcarmar.large.ADGOOD.csv")

df = read.csv("/home/niyukuri/Dropbox/25.10.2018.age.mix2/age.mixing.large.AD/editings_best_12_11_2018/Results.mcarmar.large.AD.csv")

# dr = na.omit(df)



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



# MCAR


d.MCAR <- dr %>%
  select(contains("MCAR"))



d.MCAR.cov.35 <- d.MCAR %>%
  select(contains("cov.MCAR.35.")) 
d.MCAR.cov.40 <- d.MCAR %>%
  select(contains("cov.MCAR.40.")) 
d.MCAR.cov.45 <- d.MCAR %>%
  select(contains("cov.MCAR.45.")) 
d.MCAR.cov.50 <- d.MCAR %>%
  select(contains("cov.MCAR.50.")) 
d.MCAR.cov.55 <- d.MCAR %>%
  select(contains("cov.MCAR.55.")) 
d.MCAR.cov.60 <- d.MCAR %>%
  select(contains("cov.MCAR.60.")) 
d.MCAR.cov.65 <- d.MCAR %>%
  select(contains("cov.MCAR.65.")) 
d.MCAR.cov.70 <- d.MCAR %>%
  select(contains("cov.MCAR.70.")) 
d.MCAR.cov.75 <- d.MCAR %>%
  select(contains("cov.MCAR.75.")) 
d.MCAR.cov.80 <- d.MCAR %>%
  select(contains("cov.MCAR.80.")) 
d.MCAR.cov.85 <- d.MCAR %>%
  select(contains("cov.MCAR.85.")) 
d.MCAR.cov.90 <- d.MCAR %>%
  select(contains("cov.MCAR.90.")) 
d.MCAR.cov.95 <- d.MCAR %>%
  select(contains("cov.MCAR.95.")) 




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





# Function to summarise simulation output for a single metric -------------



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


ggplot(cl.prop.men15.25.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 15 - 25 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(cl.prop.women15.25.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men 15 - 25 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.men25.40.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.women25.40.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.men40.50.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.women40.50.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.men15.25.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 25 - 40 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(cl.prop.women15.25.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men 25 - 40 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.men25.40.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.women25.40.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.men40.50.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.women40.50.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.men15.25.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 40 - 50 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(cl.prop.women15.25.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men 40 - 50 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.men25.40.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.women25.40.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.men40.50.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.prop.women40.50.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")





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


ggplot(cl.true.prop.men15.25.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 15 - 25 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(cl.true.prop.women15.25.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in 15 - 25 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.men25.40.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.women25.40.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.men40.50.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.women40.50.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")




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


ggplot(cl.true.prop.men15.25.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 25 - 40 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(cl.true.prop.women15.25.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in 25 - 40 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.men25.40.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.women25.40.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.men40.50.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.women40.50.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")




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


ggplot(cl.true.prop.men15.25.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 15 - 25 paired with women with 25 - 40 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(cl.true.prop.women15.25.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in 25 - 40 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.men25.40.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.women25.40.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.men40.50.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(cl.true.prop.women40.50.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")






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


ggplot(tree.trans.true.prop.men15.25.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings between of men in 15 - 25 paired with women in 15 - 25 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(tree.trans.true.prop.women15.25.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in 15 - 25 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(tree.trans.true.prop.men25.40.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(tree.trans.true.prop.women25.40.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(tree.trans.true.prop.men40.50.F.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 40 - 50 paired with women in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(tree.trans.true.prop.women40.50.M.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 40 - 50 paired with men in 15 - 25 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")




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


ggplot(tree.trans.true.prop.men15.25.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings between of men in 15 - 25 paired with women in 25 - 40 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(tree.trans.true.prop.women15.25.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in  25- 40 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")






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


ggplot(tree.trans.true.prop.men25.40.F.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(tree.trans.true.prop.women25.40.M.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in 25 - 40 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")





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


ggplot(tree.trans.true.prop.men15.25.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings between of men in 15 - 25 paired with women in  40 - 50 years- MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")


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



ggplot(tree.trans.true.prop.women15.25.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 15 - 25 paired with men in  40 - 50 years - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")






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


ggplot(tree.trans.true.prop.men25.40.F.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of men in 25 - 40 paired with women in  40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")



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


ggplot(tree.trans.true.prop.women25.40.M.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Proportion of pairings of women in 25 - 40 paired with men in  40 - 50 - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Proportion")




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


ggplot(mean.MCAR.women.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


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


ggplot(mean.MCAR.men.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(mean.MCAR.women.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(mean.MCAR.men.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


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


ggplot(mean.MCAR.women.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(mean.MCAR.men.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Mean age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")






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


ggplot(med.MCAR.women.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


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


ggplot(med.MCAR.men.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(med.MCAR.women.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(med.MCAR.men.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


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


ggplot(med.MCAR.women.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(med.MCAR.men.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Median age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(sd.MCAR.women.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


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


ggplot(sd.MCAR.men.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 15 - 25 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(sd.MCAR.women.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(sd.MCAR.men.cl.25.40, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 25 - 40 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")


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


ggplot(sd.MCAR.women.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of women in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")



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


ggplot(sd.MCAR.men.cl.40.50, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("SD of age difference of men in 40 - 50 and their pairs (pairings) - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Mean age difference")





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
ARMSE.error.infer.clust.cov.100.women.cov.35 <- ARMSE(v1=vector.MCAR.true.cov.100.prop.women15.25.M.15.25, v2=vector.MCAR.cov.35.cl.prop.women15.25.M.15.25)

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


dat.MRE.error.infer.clust.cov.100.men.15.25.cov.95 <- data.frame(x=c(seq(from=35, to=95, by=5)),
                                       
                                       F = c(MRE.error.infer.clust.cov.100.men.15.25.cov.35, MRE.error.infer.clust.cov.100.men.15.25.cov.40,
                                             MRE.error.infer.clust.cov.100.men.15.25.cov.45, MRE.error.infer.clust.cov.100.men.15.25.cov.50,
                                             MRE.error.infer.clust.cov.100.men.15.25.cov.55, MRE.error.infer.clust.cov.100.men.15.25.cov.60,
                                             MRE.error.infer.clust.cov.100.men.15.25.cov.65, MRE.error.infer.clust.cov.100.men.15.25.cov.70, 
                                             MRE.error.infer.clust.cov.100.men.15.25.cov.75, MRE.error.infer.clust.cov.100.men.15.25.cov.80,
                                             MRE.error.infer.clust.cov.100.men.15.25.cov.85, MRE.error.infer.clust.cov.100.men.15.25.cov.90,
                                             MRE.error.infer.clust.cov.100.men.15.25.cov.95))


ggplot(mean.MCAR.women.cl.15.25, aes(x = x, y = F)) +
  geom_point(size = 4) +
  ggtitle("Average relative error - MCAR") +
  xlab("Sequence coverage scenarios") + ylab("Error value")





# For age difference statistics
###############################


# Cov 35

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.35.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.35.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.35.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.35.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.35.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.35.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.35 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.35.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.35.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.35.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.35.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.35.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.35.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.35.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.35.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.35.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.35.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.35.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.35.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.35.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.35.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.35.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.35 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.35.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.35.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.35.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.35.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.35.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.35.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.35.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.35.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.35.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.35.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.35.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.35.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.35.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.35.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.35.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.35 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.35.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.35.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.35.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.35.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.35.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.35.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.35.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.35.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.35.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.35.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.35.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.35.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.35.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.35.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.35.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.35 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.35.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.35.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.35.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.35.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.35.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.35.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.35.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.35.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.35.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.35.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.35.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.35.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.35.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.35.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.35.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.35 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.35.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.35.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.35.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.35.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.35.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.35.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.35.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.35.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.35.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.35.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.35.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.35.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.35.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.35.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.35.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.35 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.35.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.35.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.35.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.35.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.35.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.35.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.35.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.35.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.35.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.35.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.35.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.35.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.35.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.35.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.35.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.35 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.35.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.35.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.35.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.35.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.35.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.35.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.35.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.35.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.35.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.35.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.35.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.35.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.35.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.35.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.35.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.35 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.35.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.35.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.35.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.35.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.35.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.35.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.35.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.35.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.35.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.35.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.35.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.35.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.35.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.35.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.35.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.35 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.35.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.35.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.35.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.35.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.35.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.35.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.35.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.35.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.35.AD.women.cl.40.50)


# Cov 40

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.40.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.40.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.40.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.40.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.40.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.40.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.40 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.40.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.40.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.40.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.40.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.40.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.40.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.40.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.40.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.40.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.40.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.40.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.40.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.40.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.40.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.40.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.40 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.40.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.40.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.40.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.40.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.40.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.40.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.40.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.40.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.40.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.40.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.40.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.40.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.40.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.40.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.40.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.40 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.40.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.40.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.40.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.40.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.40.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.40.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.40.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.40.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.40.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.40.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.40.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.40.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.40.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.40.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.40.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.40 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.40.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.40.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.40.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.40.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.40.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.40.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.40.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.40.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.40.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.40.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.40.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.40.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.40.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.40.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.40.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.40 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.40.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.40.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.40.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.40.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.40.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.40.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.40.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.40.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.40.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.40.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.40.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.40.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.40.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.40.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.40.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.40 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.40.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.40.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.40.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.40.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.40.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.40.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.40.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.40.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.40.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.40.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.40.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.40.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.40.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.40.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.40.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.40 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.40.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.40.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.40.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.40.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.40.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.40.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.40.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.40.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.40.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.40.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.40.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.40.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.40.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.40.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.40.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.40 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.40.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.40.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.40.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.40.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.40.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.40.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.40.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.40.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.40.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.40.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.40.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.40.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.40.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.40.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.40.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.40 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.40.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.40.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.40.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.40.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.40.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.40.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.40.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.40.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.40.AD.women.cl.40.50)


# Cov 45

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.45.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.45.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.45.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.45.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.45.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.45.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.45 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.45.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.45.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.45.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.45.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.45.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.45.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.45.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.45.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.45.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.45.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.45.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.45.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.45.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.45.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.45.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.45 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.45.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.45.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.45.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.45.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.45.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.45.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.45.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.45.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.45.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.45.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.45.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.45.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.45.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.45.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.45.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.45 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.45.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.45.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.45.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.45.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.45.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.45.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.45.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.45.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.45.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.45.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.45.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.45.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.45.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.45.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.45.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.45 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.45.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.45.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.45.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.45.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.45.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.45.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.45.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.45.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.45.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.45.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.45.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.45.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.45.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.45.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.45.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.45 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.45.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.45.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.45.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.45.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.45.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.45.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.45.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.45.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.45.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.45.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.45.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.45.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.45.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.45.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.45.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.45 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.45.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.45.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.45.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.45.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.45.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.45.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.45.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.45.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.45.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.45.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.45.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.45.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.45.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.45.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.45.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.45 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.45.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.45.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.45.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.45.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.45.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.45.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.45.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.45.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.45.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.45.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.45.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.45.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.45.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.45.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.45.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.45 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.45.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.45.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.45.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.45.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.45.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.45.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.45.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.45.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.45.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.45.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.45.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.45.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.45.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.45.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.45.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.45 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.45.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.45.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.45.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.45.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.45.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.45.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.45.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.45.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.45.AD.women.cl.40.50)


# Cov 50

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.50.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.50.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.50.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.50.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.50.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.50.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.50 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.50.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.50.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.50.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.50.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.50.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.50.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.50.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.50.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.50.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.50.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.50.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.50.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.50.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.50.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.50.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.50 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.50.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.50.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.50.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.50.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.50.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.50.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.50.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.50.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.50.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.50.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.50.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.50.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.50.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.50.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.50.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.50 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.50.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.50.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.50.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.50.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.50.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.50.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.50.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.50.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.50.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.50.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.50.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.50.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.50.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.50.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.50.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.50 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.50.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.50.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.50.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.50.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.50.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.50.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.50.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.50.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.50.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.50.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.50.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.50.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.50.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.50.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.50.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.50 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.50.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.50.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.50.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.50.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.50.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.50.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.50.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.50.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.50.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.50.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.50.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.50.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.50.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.50.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.50.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.50 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.50.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.50.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.50.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.50.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.50.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.50.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.50.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.50.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.50.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.50.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.50.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.50.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.50.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.50.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.50.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.50 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.50.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.50.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.50.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.50.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.50.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.50.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.50.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.50.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.50.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.50.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.50.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.50.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.50.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.50.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.50.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.50 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.50.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.50.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.50.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.50.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.50.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.50.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.50.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.50.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.50.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.50.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.50.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.50.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.50.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.50.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.50.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.50 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.50.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.50.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.50.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.50.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.50.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.50.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.50.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.50.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.50.AD.women.cl.40.50)



# Cov 55

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.55.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.55.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.55.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.55.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.55.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.55.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.55 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.55.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.55.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.55.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.55.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.55.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.55.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.55.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.55.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.55.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.55.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.55.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.55.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.55.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.55.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.55.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.55 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.55.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.55.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.55.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.55.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.55.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.55.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.55.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.55.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.55.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.55.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.55.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.55.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.55.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.55.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.55.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.55 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.55.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.55.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.55.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.55.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.55.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.55.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.55.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.55.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.55.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.55.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.55.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.55.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.55.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.55.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.55.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.55 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.55.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.55.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.55.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.55.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.55.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.55.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.55.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.55.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.55.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.55.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.55.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.55.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.55.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.55.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.55.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.55 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.55.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.55.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.55.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.55.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.55.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.55.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.55.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.55.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.55.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.55.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.55.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.55.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.55.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.55.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.55.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.55 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.55.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.55.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.55.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.55.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.55.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.55.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.55.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.55.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.55.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.55.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.55.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.55.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.55.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.55.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.55.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.55 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.55.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.55.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.55.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.55.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.55.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.55.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.55.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.55.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.55.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.55.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.55.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.55.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.55.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.55.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.55.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.55 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.55.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.55.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.55.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.55.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.55.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.55.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.55.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.55.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.55.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.55.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.55.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.55.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.55.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.55.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.55.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.55 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.55.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.55.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.55.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.55.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.55.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.55.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.55.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.55.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.55.AD.women.cl.40.50)


# Cov 60

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.60.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.60.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.60.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.60.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.60.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.60.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.60 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.60.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.60.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.60.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.60.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.60.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.60.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.60.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.60.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.60.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.60.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.60.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.60.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.60.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.60.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.60.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.60 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.60.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.60.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.60.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.60.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.60.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.60.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.60.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.60.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.60.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.60.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.60.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.60.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.60.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.60.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.60.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.60 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.60.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.60.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.60.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.60.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.60.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.60.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.60.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.60.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.60.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.60.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.60.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.60.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.60.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.60.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.60.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.60 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.60.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.60.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.60.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.60.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.60.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.60.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.60.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.60.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.60.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.60.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.60.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.60.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.60.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.60.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.60.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.60 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.60.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.60.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.60.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.60.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.60.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.60.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.60.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.60.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.60.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.60.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.60.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.60.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.60.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.60.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.60.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.60 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.60.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.60.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.60.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.60.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.60.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.60.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.60.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.60.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.60.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.60.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.60.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.60.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.60.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.60.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.60.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.60 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.60.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.60.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.60.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.60.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.60.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.60.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.60.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.60.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.60.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.60.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.60.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.60.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.60.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.60.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.60.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.60 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.60.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.60.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.60.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.60.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.60.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.60.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.60.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.60.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.60.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.60.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.60.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.60.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.60.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.60.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.60.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.60 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.60.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.60.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.60.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.60.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.60.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.60.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.60.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.60.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.60.AD.women.cl.40.50)



# Cov 65

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.65.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.65.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.65.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.65.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.65.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.65.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.65 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.65.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.65.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.65.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.65.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.65.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.65.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.65.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.65.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.65.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.65.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.65.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.65.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.65.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.65.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.65.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.65 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.65.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.65.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.65.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.65.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.65.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.65.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.65.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.65.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.65.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.65.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.65.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.65.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.65.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.65.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.65.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.65 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.65.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.65.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.65.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.65.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.65.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.65.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.65.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.65.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.65.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.65.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.65.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.65.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.65.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.65.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.65.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.65 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.65.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.65.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.65.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.65.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.65.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.65.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.65.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.65.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.65.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.65.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.65.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.65.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.65.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.65.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.65.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.65 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.65.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.65.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.65.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.65.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.65.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.65.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.65.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.65.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.65.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.65.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.65.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.65.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.65.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.65.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.65.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.65 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.65.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.65.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.65.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.65.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.65.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.65.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.65.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.65.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.65.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.65.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.65.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.65.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.65.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.65.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.65.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.65 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.65.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.65.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.65.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.65.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.65.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.65.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.65.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.65.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.65.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.65.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.65.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.65.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.65.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.65.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.65.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.65 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.65.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.65.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.65.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.65.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.65.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.65.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.65.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.65.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.65.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.65.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.65.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.65.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.65.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.65.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.65.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.65 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.65.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.65.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.65.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.65.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.65.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.65.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.65.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.65.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.65.AD.women.cl.40.50)



# Cov 70

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.70.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.70.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.70.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.70.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.70.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.70.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.70 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.70.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.70.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.70.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.70.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.70.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.70.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.70.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.70.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.70.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.70.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.70.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.70.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.70.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.70.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.70.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.70 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.70.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.70.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.70.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.70.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.70.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.70.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.70.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.70.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.70.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.70.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.70.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.70.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.70.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.70.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.70.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.70 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.70.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.70.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.70.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.70.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.70.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.70.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.70.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.70.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.70.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.70.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.70.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.70.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.70.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.70.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.70.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.70 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.70.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.70.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.70.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.70.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.70.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.70.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.70.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.70.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.70.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.70.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.70.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.70.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.70.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.70.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.70.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.70 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.70.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.70.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.70.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.70.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.70.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.70.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.70.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.70.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.70.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.70.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.70.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.70.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.70.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.70.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.70.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.70 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.70.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.70.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.70.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.70.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.70.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.70.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.70.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.70.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.70.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.70.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.70.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.70.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.70.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.70.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.70.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.70 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.70.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.70.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.70.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.70.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.70.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.70.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.70.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.70.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.70.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.70.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.70.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.70.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.70.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.70.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.70.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.70 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.70.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.70.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.70.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.70.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.70.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.70.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.70.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.70.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.70.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.70.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.70.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.70.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.70.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.70.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.70.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.70 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.70.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.70.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.70.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.70.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.70.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.70.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.70.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.70.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.70.AD.women.cl.40.50)


# Cov 75


# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.75.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.75.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.75.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.75.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.75.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.75.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.75 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.75.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.75.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.75.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.75.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.75.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.75.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.75.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.75.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.75.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.75.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.75.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.75.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.75.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.75.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.75.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.75 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.75.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.75.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.75.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.75.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.75.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.75.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.75.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.75.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.75.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.75.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.75.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.75.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.75.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.75.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.75.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.75 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.75.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.75.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.75.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.75.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.75.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.75.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.75.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.75.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.75.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.75.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.75.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.75.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.75.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.75.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.75.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.75 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.75.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.75.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.75.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.75.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.75.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.75.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.75.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.75.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.75.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.75.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.75.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.75.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.75.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.75.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.75.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.75 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.75.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.75.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.75.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.75.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.75.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.75.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.75.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.75.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.75.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.75.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.75.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.75.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.75.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.75.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.75.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.75 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.75.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.75.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.75.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.75.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.75.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.75.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.75.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.75.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.75.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.75.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.75.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.75.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.75.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.75.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.75.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.75 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.75.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.75.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.75.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.75.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.75.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.75.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.75.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.75.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.75.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.75.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.75.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.75.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.75.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.75.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.75.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.75 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.75.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.75.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.75.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.75.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.75.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.75.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.75.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.75.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.75.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.75.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.75.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.75.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.75.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.75.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.75.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.75 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.75.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.75.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.75.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.75.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.75.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.75.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.75.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.75.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.75.AD.women.cl.40.50)


# Cov 80


# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.80.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.80.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.80.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.80.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.80.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.80.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.80 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.80.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.80.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.80.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.80.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.80.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.80.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.80.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.80.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.80.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.80.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.80.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.80.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.80.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.80.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.80.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.80 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.80.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.80.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.80.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.80.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.80.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.80.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.80.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.80.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.80.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.80.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.80.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.80.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.80.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.80.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.80.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.80 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.80.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.80.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.80.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.80.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.80.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.80.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.80.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.80.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.80.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.80.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.80.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.80.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.80.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.80.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.80.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.80 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.80.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.80.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.80.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.80.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.80.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.80.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.80.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.80.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.80.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.80.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.80.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.80.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.80.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.80.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.80.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.80 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.80.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.80.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.80.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.80.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.80.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.80.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.80.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.80.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.80.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.80.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.80.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.80.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.80.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.80.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.80.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.80 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.80.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.80.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.80.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.80.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.80.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.80.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.80.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.80.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.80.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.80.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.80.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.80.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.80.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.80.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.80.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.80 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.80.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.80.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.80.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.80.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.80.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.80.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.80.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.80.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.80.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.80.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.80.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.80.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.80.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.80.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.80.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.80 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.80.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.80.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.80.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.80.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.80.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.80.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.80.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.80.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.80.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.80.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.80.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.80.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.80.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.80.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.80.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.80 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.80.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.80.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.80.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.80.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.80.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.80.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.80.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.80.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.80.AD.women.cl.40.50)


# Cov 85

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.85.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.85.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.85.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.85.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.85.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.85.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.85 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.85.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.85.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.85.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.85.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.85.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.85.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.85.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.85.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.85.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.85.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.85.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.85.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.85.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.85.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.85.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.85 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.85.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.85.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.85.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.85.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.85.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.85.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.85.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.85.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.85.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.85.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.85.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.85.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.85.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.85.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.85.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.85 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.85.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.85.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.85.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.85.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.85.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.85.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.85.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.85.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.85.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.85.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.85.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.85.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.85.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.85.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.85.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.85 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.85.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.85.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.85.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.85.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.85.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.85.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.85.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.85.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.85.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.85.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.85.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.85.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.85.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.85.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.85.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.85 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.85.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.85.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.85.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.85.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.85.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.85.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.85.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.85.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.85.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.85.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.85.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.85.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.85.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.85.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.85.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.85 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.85.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.85.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.85.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.85.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.85.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.85.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.85.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.85.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.85.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.85.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.85.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.85.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.85.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.85.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.85.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.85 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.85.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.85.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.85.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.85.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.85.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.85.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.85.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.85.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.85.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.85.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.85.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.85.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.85.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.85.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.85.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.85 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.85.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.85.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.85.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.85.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.85.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.85.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.85.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.85.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.85.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.85.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.85.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.85.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.85.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.85.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.85.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.85 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.85.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.85.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.85.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.85.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.85.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.85.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.85.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.85.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.85.AD.women.cl.40.50)


# Cov 90

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.90.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.90.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.90.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.90.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.90.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.90.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.90 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.90.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.90.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.90.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.90.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.90.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.90.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.90.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.90.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.90.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.90.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.90.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.90.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.90.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.90.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.90.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.90 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.90.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.90.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.90.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.90.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.90.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.90.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.90.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.90.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.90.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.90.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.90.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.90.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.90.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.90.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.90.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.90 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.90.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.90.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.90.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.90.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.90.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.90.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.90.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.90.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.90.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.90.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.90.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.90.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.90.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.90.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.90.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.90 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.90.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.90.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.90.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.90.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.90.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.90.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.90.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.90.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.90.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.90.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.90.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.90.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.90.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.90.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.90.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.90 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.90.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.90.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.90.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.90.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.90.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.90.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.90.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.90.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.90.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.90.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.90.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.90.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.90.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.90.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.90.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.90 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.90.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.90.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.90.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.90.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.90.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.90.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.90.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.90.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.90.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.90.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.90.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.90.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.90.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.90.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.90.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.90 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.90.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.90.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.90.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.90.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.90.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.90.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.90.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.90.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.90.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.90.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.90.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.90.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.90.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.90.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.90.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.90 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.90.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.90.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.90.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.90.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.90.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.90.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.90.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.90.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.90.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.90.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.90.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.90.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.90.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.90.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.90.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.90 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.90.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.90.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.90.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.90.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.90.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.90.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.90.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.90.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.90.AD.women.cl.40.50)



# Cov 95

# 15.25

# Mean
error.infer.clust.cov.100.men.15.25.cov.95.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.95.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.95.mean <- RMSE(error.infer.clust.cov.100.men.15.25.cov.95.mean)
MAE.error.infer.clust.cov.100.men.15.25.cov.95.mean <- MAE(error.infer.clust.cov.100.men.15.25.cov.95.mean)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.95 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.15.25, v2=vector.mean.MCAR.cov.95.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.95.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.15.25) - as.numeric(vector.mean.MCAR.cov.95.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.95.mean <- RMSE(error.infer.clust.cov.100.women.15.25.cov.95.mean)
MAE.error.infer.clust.cov.100.women.15.25.cov.95.mean <- MAE(error.infer.clust.cov.100.women.15.25.cov.95.mean)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.95.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.15.25, v2=vector.mean.MCAR.cov.95.AD.women.cl.15.25)

# Median
error.infer.clust.cov.100.men.15.25.cov.95.med <- as.numeric(vector.med.AD.num.men.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.95.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.95.med <- RMSE(error.infer.clust.cov.100.men.15.25.cov.95.med)
MAE.error.infer.clust.cov.100.men.15.25.cov.95.med <- MAE(error.infer.clust.cov.100.men.15.25.cov.95.med)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.95 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.15.25, v2=vector.med.MCAR.cov.95.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.95.med <- as.numeric(vector.med.AD.num.women.true.cov.100.15.25) - as.numeric(vector.med.MCAR.cov.95.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.95.med <- RMSE(error.infer.clust.cov.100.women.15.25.cov.95.med)
MAE.error.infer.clust.cov.100.women.15.25.cov.95.med <- MAE(error.infer.clust.cov.100.women.15.25.cov.95.med)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.95.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.15.25, v2=vector.med.MCAR.cov.95.AD.women.cl.15.25)


# Standard deviation
error.infer.clust.cov.100.men.15.25.cov.95.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.95.AD.men.cl.15.25)
RMSE.error.infer.clust.cov.100.men.15.25.cov.95.sd <- RMSE(error.infer.clust.cov.100.men.15.25.cov.95.sd)
MAE.error.infer.clust.cov.100.men.15.25.cov.95.sd <- MAE(error.infer.clust.cov.100.men.15.25.cov.95.sd)
ARMSE.error.infer.clust.cov.100.men.15.25.cov.95 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.15.25, v2=vector.sd.MCAR.cov.95.AD.men.cl.15.25)


error.infer.clust.cov.100.women.15.25.cov.95.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.15.25) - as.numeric(vector.sd.MCAR.cov.95.AD.women.cl.15.25)
RMSE.error.infer.clust.cov.100.women.15.25.cov.95.sd <- RMSE(error.infer.clust.cov.100.women.15.25.cov.95.sd)
MAE.error.infer.clust.cov.100.women.15.25.cov.95.sd <- MAE(error.infer.clust.cov.100.women.15.25.cov.95.sd)
ARMSE.error.infer.clust.cov.100.women.15.25.cov.95.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.15.25, v2=vector.sd.MCAR.cov.95.AD.women.cl.15.25)


# 25.40

# Mean
error.infer.clust.cov.100.men.25.40.cov.95.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.95.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.95.mean <- RMSE(error.infer.clust.cov.100.men.25.40.cov.95.mean)
MAE.error.infer.clust.cov.100.men.25.40.cov.95.mean <- MAE(error.infer.clust.cov.100.men.25.40.cov.95.mean)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.95 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.25.40, v2=vector.mean.MCAR.cov.95.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.95.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.25.40) - as.numeric(vector.mean.MCAR.cov.95.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.95.mean <- RMSE(error.infer.clust.cov.100.women.25.40.cov.95.mean)
MAE.error.infer.clust.cov.100.women.25.40.cov.95.mean <- MAE(error.infer.clust.cov.100.women.25.40.cov.95.mean)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.95.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.25.40, v2=vector.mean.MCAR.cov.95.AD.women.cl.25.40)

# Median
error.infer.clust.cov.100.men.25.40.cov.95.med <- as.numeric(vector.med.AD.num.men.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.95.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.95.med <- RMSE(error.infer.clust.cov.100.men.25.40.cov.95.med)
MAE.error.infer.clust.cov.100.men.25.40.cov.95.med <- MAE(error.infer.clust.cov.100.men.25.40.cov.95.med)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.95 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.25.40, v2=vector.med.MCAR.cov.95.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.95.med <- as.numeric(vector.med.AD.num.women.true.cov.100.25.40) - as.numeric(vector.med.MCAR.cov.95.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.95.med <- RMSE(error.infer.clust.cov.100.women.25.40.cov.95.med)
MAE.error.infer.clust.cov.100.women.25.40.cov.95.med <- MAE(error.infer.clust.cov.100.women.25.40.cov.95.med)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.95.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.25.40, v2=vector.med.MCAR.cov.95.AD.women.cl.25.40)


# Standard deviation
error.infer.clust.cov.100.men.25.40.cov.95.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.95.AD.men.cl.25.40)
RMSE.error.infer.clust.cov.100.men.25.40.cov.95.sd <- RMSE(error.infer.clust.cov.100.men.25.40.cov.95.sd)
MAE.error.infer.clust.cov.100.men.25.40.cov.95.sd <- MAE(error.infer.clust.cov.100.men.25.40.cov.95.sd)
ARMSE.error.infer.clust.cov.100.men.25.40.cov.95 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.25.40, v2=vector.sd.MCAR.cov.95.AD.men.cl.25.40)


error.infer.clust.cov.100.women.25.40.cov.95.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.25.40) - as.numeric(vector.sd.MCAR.cov.95.AD.women.cl.25.40)
RMSE.error.infer.clust.cov.100.women.25.40.cov.95.sd <- RMSE(error.infer.clust.cov.100.women.25.40.cov.95.sd)
MAE.error.infer.clust.cov.100.women.25.40.cov.95.sd <- MAE(error.infer.clust.cov.100.women.25.40.cov.95.sd)
ARMSE.error.infer.clust.cov.100.women.25.40.cov.95.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.25.40, v2=vector.sd.MCAR.cov.95.AD.women.cl.25.40)


# 40.50

# Mean
error.infer.clust.cov.100.men.40.50.cov.95.mean <- as.numeric(vector.mean.AD.num.men.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.95.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.95.mean <- RMSE(error.infer.clust.cov.100.men.40.50.cov.95.mean)
MAE.error.infer.clust.cov.100.men.40.50.cov.95.mean <- MAE(error.infer.clust.cov.100.men.40.50.cov.95.mean)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.95 <- ARMSE(v1=vector.mean.AD.num.men.true.cov.100.40.50, v2=vector.mean.MCAR.cov.95.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.95.mean <- as.numeric(vector.mean.AD.num.women.true.cov.100.40.50) - as.numeric(vector.mean.MCAR.cov.95.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.95.mean <- RMSE(error.infer.clust.cov.100.women.40.50.cov.95.mean)
MAE.error.infer.clust.cov.100.women.40.50.cov.95.mean <- MAE(error.infer.clust.cov.100.women.40.50.cov.95.mean)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.95.mean <- ARMSE(v1=vector.mean.AD.num.women.true.cov.100.40.50, v2=vector.mean.MCAR.cov.95.AD.women.cl.40.50)

# Median
error.infer.clust.cov.100.men.40.50.cov.95.med <- as.numeric(vector.med.AD.num.men.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.95.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.95.med <- RMSE(error.infer.clust.cov.100.men.40.50.cov.95.med)
MAE.error.infer.clust.cov.100.men.40.50.cov.95.med <- MAE(error.infer.clust.cov.100.men.40.50.cov.95.med)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.95 <- ARMSE(v1=vector.med.AD.num.men.true.cov.100.40.50, v2=vector.med.MCAR.cov.95.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.95.med <- as.numeric(vector.med.AD.num.women.true.cov.100.40.50) - as.numeric(vector.med.MCAR.cov.95.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.95.med <- RMSE(error.infer.clust.cov.100.women.40.50.cov.95.med)
MAE.error.infer.clust.cov.100.women.40.50.cov.95.med <- MAE(error.infer.clust.cov.100.women.40.50.cov.95.med)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.95.med <- ARMSE(v1=vector.med.AD.num.women.true.cov.100.40.50, v2=vector.med.MCAR.cov.95.AD.women.cl.40.50)


# Standard deviation
error.infer.clust.cov.100.men.40.50.cov.95.sd <- as.numeric(vector.sd.AD.num.men.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.95.AD.men.cl.40.50)
RMSE.error.infer.clust.cov.100.men.40.50.cov.95.sd <- RMSE(error.infer.clust.cov.100.men.40.50.cov.95.sd)
MAE.error.infer.clust.cov.100.men.40.50.cov.95.sd <- MAE(error.infer.clust.cov.100.men.40.50.cov.95.sd)
ARMSE.error.infer.clust.cov.100.men.40.50.cov.95 <- ARMSE(v1=vector.sd.AD.num.men.true.cov.100.40.50, v2=vector.sd.MCAR.cov.95.AD.men.cl.40.50)


error.infer.clust.cov.100.women.40.50.cov.95.sd <- as.numeric(vector.sd.AD.num.women.true.cov.100.40.50) - as.numeric(vector.sd.MCAR.cov.95.AD.women.cl.40.50)
RMSE.error.infer.clust.cov.100.women.40.50.cov.95.sd <- RMSE(error.infer.clust.cov.100.women.40.50.cov.95.sd)
MAE.error.infer.clust.cov.100.women.40.50.cov.95.sd <- MAE(error.infer.clust.cov.100.women.40.50.cov.95.sd)
ARMSE.error.infer.clust.cov.100.women.40.50.cov.95.sd <- ARMSE(v1=vector.sd.AD.num.women.true.cov.100.40.50, v2=vector.sd.MCAR.cov.95.AD.women.cl.40.50)




