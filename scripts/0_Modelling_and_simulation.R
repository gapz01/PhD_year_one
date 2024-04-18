## Header ---- 
## From paper: Avian phylogenetic and functional diversity are better conserved 
## by land-sparing than land-sharing farming in lowland tropical forests.
## Journal of Applied Ecology
##
## Purpose of script: Model occupancy of amazon birds in forest and pasture habitats
## Use the model to predict communities and simulate bird communities in agricultural landscapes
## 
## Author: Giovanny Perez, Simon Mills
## Email: gaperezzuleta1@sheffield.ac.uk, gapz01@gmail.com
## 

# 0. Load data and packages ####

# Required packages
library(flocker)         # Occupancy models
library(brms)            # Bayesian models using stan
library(tidyverse)       # Data handling and plots
library(DataCombine)     # Find&Replace values in a dataframes

# final dataframe
DF <- read_csv ("data/dataset_raw_amazonbirds.csv")

# Load lookup tables to match names (differing taxonomic nomenclature)
correct_names <- read_csv("data/names_lookup_amazonbirds.csv")
names_to_remove_from_model <- correct_names %>% 
  filter(., phylogeny == "Not_in_phylogeny") %>% 
  pull(., model)


# --Pre-processing of functional space--
func_traits <- read_csv("data/functional_traits_amazonbirds.csv") %>% 
  mutate_at(., c("Trophic_Level", "Trophic_Niche", "Primary_Lifestyle", "Nest_Placement"), as.factor) %>%
  arrange(., species) %>% 
  column_to_rownames(., "species")

# Selection of the best functional space for FD (following Palacio et.al., 2022)
# Traits definition (types of variables)
func_trait_types <- data.frame(trait_name = names(func_traits),
                               trait_type  = c(rep("Q", 13), rep("N", 4)))

# Compute species functional distance
func_dist <- funct.dist(
  sp_tr  = func_traits,
  tr_cat = func_trait_types,
  metric = "gower")

# Trait space quality
func_space_quality <- quality.fspaces(func_dist)

#The space with the best quality has the lowest quality metric.
round(func_space_quality$quality_fspaces, 3) # decision: use 5D

# Final functional spaces to compute FD (extract axes)
func_space_coords <-  
  func_space_quality$details_fspaces$sp_pc_coord[ , c("PC1", "PC2", 
                                                      "PC3", "PC4", 
                                                      "PC5")]
#Save this to be used in script 2
saveRDS(func_space_coords, "outputs/func_space_coords.rds")

# #If loading model (below)
# mod <- readRDS("outputs/mod.rds")
# class(mod) <- c("brmsfit", "flocker_fit")
# attributes(mod)$data_type <- "single"


# 1. Modelling ####

### 1.1 Hierarchical model ####

#Format data to be used in package flocker
#make observation matrix
obs_data <- DF %>% 
  select(., v1, v2, v3, v4) %>% 
  data.matrix()

#make unit covariate dataframe
unit_covs_data <- DF %>% 
  select(species, point, forest_dependency, prop,
         prop_sc, habitat2, site_sp, site, cluster)

#make event covariate dataframe
observer <- select(DF, obs1, obs2, obs3, obs4)
time <- select(DF, hps1_sc, hps2_sc, hps3_sc, hps4_sc)
event_covs_data <- list(observer = data.matrix(observer),
                time = data.matrix(time))

#convert to flocker data
mod_data <- make_flocker_data(obs = obs_data,
                                      unit_covs = unit_covs_data,
                                      event_covs = event_covs_data)

#set weakly informative priors
priors <- c(set_prior("normal(0, 5)", class="b"), 
            set_prior("normal(0, 1.7)", class="Intercept"))


# Occupancy model
mod <- flocker::flock(f_occ = ~ 1 + forest_dependency +
                        prop_sc*forest_dependency  +
                        habitat2*forest_dependency  +
                        (1 + habitat2 + prop_sc | species) +
                        site + (1|site_sp) +
                        (1|cluster),  
                      f_det = ~ 1 + time + habitat2 +
                        (1 + time | species),
                      flocker_data = mod_data,
                      prior = priors,
                      chains = 4, cores = 4, iter = 2000,
                      backend = "cmdstanr",
                      file = "outputs/mod.rds")

# check convergence
summary(mod)


### 1.2 Predict occupancy ####

# Data preparation to use for predictions
prop_scale <- function(x) (x-0.05118145)/0.1018974 # To scale and center "proportion of wf" variable

# Set up the types of point needed for simulation
# In mod$data, habitat is set 1: forest, -1: pasture
var_combns <- tibble(habitat2 = c(rep(-1, 7), 1), 
                     prop     = c(seq(.6, 0, -.1), 0),  # varying proportions of WF within pasture
                     prop_sc  = prop_scale(prop),
                     habitat  = c(paste0("pasture_", seq(.6,0, -.1)), "forest")) 

# Bind the other data needed for prediction
# setting species and habitat
pred_data <- with(mod$data, expand.grid(species = unique(species),
                                        habitat2 = unique(habitat2)))
# Forest dependency is set as a factor
fDep_lookup <- mod$data %>%                 
  select(species, forest_dependency) %>% 
  unique %>%
  arrange(species) %>%
  mutate(., forest_dependency = factor(forest_dependency, 
                                       levels=c("High", "Medium", "Low")))

# Format data needed to use in fitted_flocker function 
pred_data <- left_join(pred_data, var_combns) %>%  
  left_join(., fDep_lookup) %>%
  # note all of this not used in prediction
  mutate(cluster = "cluster_PLP_1",
         time = 1, 
         site = "PL", 
         site_sp = mod$data$site_sp[1])


# Generate predictions flocker package
preds_full <- fitted_flocker(mod, 
                             new_data   = pred_data, 
                             components = "occ",
                             re_formula = ~ (1 + habitat2 + prop_sc | species),
                             summarise  = F, 
                             draw_ids = sample(1:4000, 1000))    # number of iterations

# Gather all the information needed for simulations (predictors and predictions)
preds_full <- cbind(pred_data, preds_full)                                            

# Replacing species names to match phylogenies and functional traits names (in following sections)
preds_full <- FindReplace(data        = preds_full,
                          Var         = "species",
                          replaceData = correct_names,
                          from        = "model", 
                          to          = "phylogeny", 
                          exact       = TRUE,
                          vector      = FALSE)

# rename for ease
preds_full <-  rename_with(preds_full,
                           ~ paste0("iter_", seq(1,1000,1)),
                           starts_with("linpred"))

preds_full <- preds_full %>%
  filter(., !species %in% names_to_remove_from_model)

preds_full <- preds_full %>% arrange(., species)

species_names <- unique(preds_full$species)

#Save this to be used in script 2
saveRDS(preds_full, "outputs/preds_full.rds")



## 2. Simulate communities ####
### 2.1 Habitats ####
# presence/abscence of species at habitat points

# From this extract the posterior predictions associated with each point type,
# and save as probability matrices
# (These are used as the probability of success on the rbinom trials)
occ_mats <- preds_full %>%
  select(starts_with("iter"), "habitat") %>%
  split(., .$habitat) %>%
  lapply(., function(x) as.matrix(select(x, -habitat)))

# Use those to get prevalence using rbinom
# This is the simulation of a community for each of the 1000 iterations
Z_mats <- vector("list", length(occ_mats))
names(Z_mats) <- names(occ_mats)

for (i in 1:length(occ_mats)) {
  mat_i <- occ_mats[[i]]
  n_probs <- prod(dim(mat_i))
  Z_i <- mat_i
  Z_i[] <- rbinom(n = n_probs,        
                  size = 30,         # number of points
                  prob = mat_i)
  
  # format to community matrix
  Z_i <- t(Z_i)
  colnames(Z_i) <- species_names
  #Z_i <- ifelse(Z_i > 0, 1, 0)    # Transform to presence-absence for faster PD if needed
  Z_i <- as.data.frame(Z_i)
  Z_mats[[i]] <- Z_i
  
}

# 
# # check SR differences
# bind_rows(Z_mats, .id = "habitat") %>%
#   mutate(., iter_id = rep(seq(1,1000,1), 8), .before = 1) %>%
#   pivot_longer(., -c(habitat, iter_id),
#                names_to = "species",
#                values_to = "Abundance") %>%
#   mutate(., presence = if_else(Abundance >= 1, 1, 0)) %>%
#   group_by(., iter_id, habitat) %>%
#   summarise(SR = sum(presence)) %>%
#   group_by(., habitat) %>%
#   summarise(meanSR = mean(SR))

#Final presence absence matrices
saveRDS(Z_mats, "outputs/Z_mats.rds")

### 2.2 Sharing and sparing  ####

# Use preds_full to extract the posterior predictions associated with each point
# type and save as probability matrices (points to "mix" in simulations)
# (These are used as the probability of success on the rbinom trials)

occ_forest_mat <- preds_full %>% 
  filter(habitat == "forest") %>% 
  select(contains("iter")) %>% as.matrix

occ_pasture_0_wf_mat <- preds_full %>% 
  filter(habitat == "pasture_0") %>% 
  select(contains("iter")) %>% as.matrix

occ_pasture_low_wf_mat <- preds_full %>%
  filter(habitat == "pasture_0.1") %>%
  select(contains("iter")) %>% 
  as.matrix

occ_pasture_high_wf_mat <- preds_full %>%
  filter(habitat == "pasture_0.6") %>%
  select(contains("iter")) %>%
  as.matrix


# We use random binomial trials to simulate communities at varying landscape sizes
# create matrices of correct dimensions (note: values will be overwritten in next step)
Z_forest_mat <- 
  Z_pasture_0wf_mat <- 
  Z_pasture_low_wf_mat <- 
  Z_pasture_high_wf_mat <- occ_forest_mat   # "empty" matrices, to write into in the next step
n_probs <- prod(dim(occ_forest_mat))        # number of observations (n) for rbinom
n_pts <- c(10, seq(50, 450, 50))            # Size of management units (landscape sizes) 
species_names <- unique(preds_full$species) # species names (to put in communities)
wf_lvl <- c("low_yield" = 0.6,              # levels of WF features according to observed (field) WF values
            "high_yield" = 0.1)   

# LOW YIELD communities
comms_sh_low <- comms_sp_low <- c()         # "Empty" lists to store communities (later)

# Loop over management units
for(n_pt_i in n_pts) {                                                          
  # Sparing "landscapes"
  # simulate number of occupied forest points
  Z_forest_mat[] <- rbinom(n_probs, 
                           n_pt_i*(wf_lvl["low_yield"]), 
                           occ_forest_mat)
  # simulate number of occupied pasture points
  Z_pasture_0wf_mat[] <- rbinom(n_probs, 
                                n_pt_i*(1 - wf_lvl["low_yield"]), 
                                occ_pasture_0_wf_mat)
  # calculate species present in the spared "landscape"
  present_sparing <- (Z_forest_mat + Z_pasture_0wf_mat)
  
  # Sharing "landscapes"
  # simulate number of occupied pasture points (remember: no forest in sharing)
  Z_pasture_high_wf_mat[] <- rbinom(n_probs, 
                                    n_pt_i, 
                                    occ_pasture_high_wf_mat) # (low-yield has high_wf)
  # calculate species present in the shared "landscape"
  present_sharing <- Z_pasture_high_wf_mat                             
  
  # Format communities as species_x_site matrices
  mat_sparing <- t(present_sparing)
  colnames(mat_sparing) <- species_names
  
  mat_sharing <- t(present_sharing)
  colnames(mat_sharing) <- species_names
  
  # Save communities in a list
  comms_sh_low[[n_pt_i]] <- as.data.frame(mat_sharing)
  comms_sp_low[[n_pt_i]] <- as.data.frame(mat_sparing)
}

# HIGH YIELD communities
comms_sh_high <- comms_sp_high <- c()  # "Empty" lists to store communities (later)

# Loop over management units
for(n_pt_i in n_pts) {                                                          
  # Sparing "landscapes"
  # simulate number of occupied forest points
  Z_forest_mat[] <- rbinom(n_probs, 
                           n_pt_i*(wf_lvl["high_yield"]), 
                           occ_forest_mat)
  # simulate number of occupied pasture points
  Z_pasture_0wf_mat[] <- rbinom(n_probs, 
                                n_pt_i*(1 - wf_lvl["high_yield"]), 
                                occ_pasture_0_wf_mat)
  # calculate species present in the spared "landscape"
  present_sparing <- (Z_forest_mat + Z_pasture_0wf_mat)
  
  # Sharing "landscapes"
  # simulate number of occupied pasture points (remember: no forest in sharing)
  Z_pasture_low_wf_mat[] <- rbinom(n_probs, 
                                   n_pt_i, 
                                   occ_pasture_low_wf_mat) # (high-yield has low_wf)
  # calculate species present in the shared "landscape"
  present_sharing <- Z_pasture_low_wf_mat                             
  
  # Format communities as species_x_site matrices
  mat_sparing <- t(present_sparing)
  colnames(mat_sparing) <- species_names 
  
  mat_sharing <- t(present_sharing)
  colnames(mat_sharing) <- species_names 
  
  # Save communities in a list
  comms_sh_high[[n_pt_i]] <- as.data.frame(mat_sharing)
  comms_sp_high[[n_pt_i]] <- as.data.frame(mat_sparing)
}

# # Store simulated communities (to be used in script 2)
saveRDS(comms_sh_high, "outputs/comms_sh_high.rds")
saveRDS(comms_sh_low, "outputs/comms_sh_low.rds")
saveRDS(comms_sp_high, "outputs/comms_sp_high.rds")
saveRDS(comms_sp_low, "outputs/comms_sp_low.rds")




