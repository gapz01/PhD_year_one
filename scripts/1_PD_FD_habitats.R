## Header ----
## Script name: PD_FD_sharing_sparing
##
## Purpose of script: Compute PD and FD across forest and WF-pasture gradient
## Author: Giovanny Perez, Simon Mills
## Date Created: March 2023
## Email: gaperezzuleta1@sheffield.ac.uk, gapz01@gmail.com
##
## Notes:
##   


# 0. LOAD DATA AND PACKAGES ####

# Required packages
library(flocker)         # Occupancy models
library(brms)            # Bayesian models using stan
library(ape)             # Phylogenetic trees handling
library(PhyloMeasures)   # Fast computation of PD metrics
library(mFD)             # Computation of FD metrics and selection of trait space
library(fundiversity)    # faster FD computation
library(tidyverse)       # Data handling and plots
library(DataCombine)     # Find&Replace values in a dataframes

# Load data and model  
# Model (from Birch, in review) to predict and simulate communities
mod <- readRDS("data/Bird_flocker_model_new.rds")                  
class(mod) <- c("brmsfit", "flocker_fit")
attributes(mod)$lik_type <- "V"

# Data table to match species names to old taxonomy names (birdtree)
missing_names <- read.csv("data/missing_names.csv")

# Load phylogenies
# 1000 trees multiPhylo object downloaded from birdtree.org
phylogenies <- read.tree("data/amazon_species_phylogeny")

# Load species range size (for EDR, data from AVONET [Tobias et.al., 2022])
range_size <- read_csv("data/amazon_range_size.csv")

# --- Pre-processing of functional space ---
# Load functional traits
# from AVONET (Tobias et.al., 2022) and Elton's traits (Wilman et.al., 2014)
func_traits <- read_csv("data/amazon_traits.csv")

# Selection of the best functional space for FD (following Palacio et.al., 2022)

# Traits definition (types of variables)
func_trait_types <- data.frame(trait_name = names(func_traits),
                               trait_type  = c(rep("Q", 11), rep("F", 14)),
                               fuzzy_name  = c(rep(NA, 11),
                                               rep("Diet", 7),
                                               rep("Foraging_Strata", 7)))

# Compute species functional distance
func_dist <- funct.dist(
  sp_tr  = func_traits,
  tr_cat = func_trait_types,
  metric = "gower")

# Trait space quality
func_space_quality <- quality.fspaces(func_dist)

#The space with the best quality has the lowest quality metric.
round(func_space_quality$quality_fspaces, 3) # decision: use 6D

# Final functional spaces to compute FD (extract axes)
func_space_coords <-  
  func_space_quality$details_fspaces$sp_pc_coord[ , c("PC1", "PC2", 
                                                      "PC3", "PC4", 
                                                      "PC5", "PC6")]









# 1. DATA PREPARATION: MODEL AND SIMULATIONS ####

# Data preparation to use for predictions
prop_scale <- function(x)        # To scale and center "proportion of forest" variable
  (x-0.05118145)/0.1018974              
wf_lvl <- c("low_yield" = 0.6,   # levels of WF features according to observed (field) WF values
            "high_yield" = 0.1)                     

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
  # setting this to the mean landscape treecover
  mutate(tc_1000_sc = 0) %>%     
  # note all of this save for site not used in prediction
  mutate(time = 1, 
         site = "PL", 
         site_sp = mod$data$site_sp[1])


# Generate predictions flocker package
preds_full <- fitted_flocker(mod, 
                             new_data   = pred_data, 
                             type       = "occupancy", 
                             re_formula = ~ (1 + habitat2 + tc_1000_sc + prop_sc | species),
                             summarise  = FALSE, 
                             ndraws     = 1000)    # number of iterations

# Gather all the information needed for simulations (predictors and predictions)
preds_full <- cbind(pred_data, preds_full)                                            

# Replacing species names to match phylogenies and functional traits names (in following sections)
preds_full <- FindReplace(data        = preds_full,
                          Var         = "species",
                          replaceData = missing_names,
                          from        = "species", 
                          to          = "birdtree_name", 
                          exact       = TRUE,
                          vector      = FALSE)

preds_full <- preds_full %>% arrange(., species)


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
                size = 1,         # number of trials is 1 to make that a Bernoulli trial
                prob = mat_i)
  
  # format to community matrix
  Z_i <- t(Z_i)
  colnames(Z_i) <- species_names
  Z_i <- as.data.frame(Z_i)
  Z_mats[[i]] <- Z_i
  
}



# 2. PHYLOGENETIC DIVERSITY (PD) ####
### 2.1 PD and SES.PD metrics ####
PD_habitats <- 
  PD_vect <- 
  MPD_vect <- 
  MNTD_vect <- 
  sesPD_vect <- 
  sesMPD_vect <- 
  sesMNTD_vect <- c() # "empty" objects to write into

# PD calculation using a different (random) phylogeny for each iteration
for (j in 1:length(Z_mats)){
  for (i in 1:1000) 
   {
    PD_vect[i]      <- pd.query (phylogenies[[sample(1:1000, 1)]], Z_mats[[j]][i, ])
    MPD_vect[i]     <- mpd.query (phylogenies[[sample(1:1000, 1)]], Z_mats[[j]][i, ])
    MNTD_vect[i]    <- mntd.query (phylogenies[[sample(1:1000, 1)]], Z_mats[[j]][i, ])
    sesPD_vect[i]   <- pd.query (phylogenies[[sample(1:1000, 1)]], Z_mats[[j]][i, ],
                               standardize = TRUE, null.model  = "uniform")
    sesMPD_vect[i]  <- mpd.query (phylogenies[[sample(1:1000, 1)]], Z_mats[[j]][i, ],
                               standardize = TRUE, null.model  = "uniform")
    sesMNTD_vect[i] <- mntd.query (phylogenies[[sample(1:1000, 1)]], Z_mats[[j]][i, ],
                               standardize = TRUE, null.model  = "uniform")
  }
  PD_habitats[[j]] <- data.frame(habitat = names(Z_mats[j]),
                                 iter_id = 1:1000,
                                 PD = PD_vect,
                                 MPD = MPD_vect,
                                 MNTD = MNTD_vect,
                                 sesPD = sesPD_vect,
                                 sesMPD = sesMPD_vect,
                                 sesMNTD = sesMNTD_vect)
  }

PD_habitats <- bind_rows(PD_habitats) 

### 2.2 ED and EDR metrics ####

# Bind all communities in a dataframe and create ED and EDR columns
# (These will be replaced  for their numeric values in the next step)
Z_mats_df <- Z_mats %>%
  bind_rows(., .id = "habitat") %>% 
  mutate(., iter_id = rep(1:1000, times = 8), .after = "habitat") %>% 
  pivot_longer(., cols = -c("iter_id", "habitat"),
               names_to = "species", 
               values_to = "Abundance") %>% 
  mutate(., ED = if_else(Abundance >=1, species, NULL),
         EDR = ED) %>% 
  as.data.frame()


# Replace the ED values
ED_mats <- FindReplace(data = Z_mats_df,
                       Var = "ED",
                       replaceData = ED_values,
                       from = "species",
                       to = "ED", 
                       exact = TRUE, vector = FALSE)

# Put the EDR values
ED_mats <- FindReplace(data = ED_mats,
                       Var = "EDR",
                       replaceData = ED_values,
                       from = "species", 
                       to = "EDR", 
                       exact = TRUE, vector = FALSE)

# Summarize ED and EDR for each iteration and format variables
ED_mats <- ED_mats %>% 
  mutate(., across(c(ED, EDR), as.numeric)) %>% 
  group_by(., habitat, iter_id) %>% 
  na.omit() %>% 
  summarise(ED = sum(ED),
            EDR = sum(EDR)) %>% 
  ungroup()

# Bind everything in a data.frame
PD_habitats <- left_join(PD_habitats, ED_mats, by = c("iter_id", "habitat")) %>% 
  mutate_at(., "habitat", factor, levels = c("forest", 
                                             "pasture_0.6",
                                             "pasture_0.5",
                                             "pasture_0.4",
                                             "pasture_0.3",
                                             "pasture_0.2",
                                             "pasture_0.1",
                                             "pasture_0"))



# 3. FUNCTIONAL DIVERSITY (FD) ####

### 3.1 Distance-based FD metrics ####
FD_habitats <- c()
for (j in 1:length(Z_mats)) {
  mats <- Z_mats[[j]] %>% as.matrix()
  
  FRic_vals <- fd_fric(func_space_coords, mats, stand = T)
  FEve_vals <- fd_feve(func_space_coords, mats)
  FDiv_vals <- fd_fdiv(func_space_coords, mats)
  FDis_vals <- fd_fdis(func_space_coords, mats)
  
  FD_habitats[[j]] <- data.frame(habitat = names(Z_mats[j]),
                                 iter_id = 1:1000,
                                 FRic    = FRic_vals$FRic,
                                 FEve    = FEve_vals$FEve,
                                 FDiv    = FDiv_vals$FDiv,
                                 FDis    = FDis_vals$FDis)
}

FD_habitats <- bind_rows(FD_habitats) %>% 
  mutate_at(., "habitat", factor, levels = c("forest", "pasture_0.6",
                                             "pasture_0.5",
                                             "pasture_0.4",
                                             "pasture_0.3",
                                             "pasture_0.2",
                                             "pasture_0.1",
                                             "pasture_0"))



### 3.2 sesFD metrics ####
# (For time/computation reasons each metric (fric, feve, etc) is run separately)

###### FRic
fric_habs_rnd <- fric_habs <- c() # To write into

for (hab in names(Z_mats)) {       # looping over habitats
  for (i in 1:100) {               # looping over 100 rands
    
    rand <- randomizeMatrix(Z_mats[[hab]],   # Extract community from the list
                            "richness")      # randomize abundances within samples keeping species richness
    
    fric_habs_rnd[[i]] <- data.frame(
      fd_fric(func_space_coords, rand, stand = TRUE),  # Actual FD computation (for each randomization)
      habitat = hab)                                   # Store habitat type
  }
    fric_habs[[hab]] <- bind_rows(fric_habs_rnd)  # Store all FD values at each habitat
}

nullfric_habs <- bind_rows(fric_habs) # bind in a data frame, each iteration must have 100 FD values
                                      # this the null FD, need to summarise and get mean and sd

###### FEve
feve_habs_rnd <- feve_habs <- c()
for (hab in names(Z_mats)) {
  for (i in 1:100) {
    rand <- randomizeMatrix(Z_mats[[hab]], "richness")
    feve_habs_rnd[[i]] <- data.frame(fd_feve(func_space_coords, rand), habitat = hab)
    }
  feve_habs[[hab]] <- bind_rows(feve_habs_rnd)
  }
nullfeve_habs <- bind_rows(feve_habs)

###### FDiv
fdiv_habs_rnd <- fdiv_habs <- c() 
for (hab in names(Z_mats)) {
  for (i in 1:100) {
    rand <- randomizeMatrix(Z_mats[[hab]], "richness")
    fdiv_habs_rnd[[i]] <- data.frame(fd_fdiv(func_space_coords, rand), habitat = hab)
    }
  fdiv_habs[[hab]] <- bind_rows(fdiv_habs_rnd) 
  }
nullfdiv_habs <- bind_rows(fdiv_habs)

###### FDis
fdis_habs_rnd <- fdis_habs <- c()
for (hab in names(Z_mats)) {
  for (i in 1:100) {
    rand <- randomizeMatrix(Z_mats[[hab]], "richness")
    fdis_habs_rnd[[i]] <- data.frame(fd_fdis(func_space_coords, rand), habitat = hab)
    }
  fdis_habs[[hab]] <- bind_rows(fdis_habs_rnd)
  }
nullfdis_habs <- bind_rows(fdis_habs)


### Summarise mean null FD and sd null FD by habitat/iteration
###### FRic
nullfric_habs_summ <-  nullfric_habs %>% 
  group_by(habitat, site) %>% 
  summarise(mean_nullFRic = mean(FRic),
            sd_nullFRic = sd(FRic)) %>%
  ungroup() %>% 
  # site should be named "iter_id" and be numeric, need to split that
  separate(., site, c("drop", "iter_id"),
           sep = "_", remove = TRUE, convert = TRUE) %>% 
  select(., -drop) %>% 
  mutate_at(., "habitat", factor)

###### FEve
nullfeve_habs_summ <-  nullfeve_habs %>% 
  group_by(habitat, site) %>% 
  summarise(mean_nullFEve = mean(FEve),
            sd_nullFEve = sd(FEve)) %>%
  ungroup() %>%
  separate(., site, c("drop", "iter_id"),
           sep = "_", remove = TRUE, convert = TRUE) %>%
  select(., -drop) %>% 
  mutate_at(., "habitat", factor)

###### FDiv
nullfdiv_habs_summ <-  nullfdiv_habs %>% 
  group_by(habitat, site) %>% 
  summarise(mean_nullFDiv = mean(FDiv),
            sd_nullFDiv = sd(FDiv)) %>%
  ungroup() %>% 
  separate(., site, c("drop", "iter_id"),
           sep = "_", remove = TRUE, convert = TRUE) %>% 
  select(., -drop) %>% 
  mutate_at(., "habitat", factor)

###### FDiv
nullfdis_habs_summ <-  nullfdis_habs %>% 
  group_by(habitat, site) %>% 
  summarise(mean_nullFDis = mean(FDis),
            sd_nullFDis = sd(FDis)) %>%
  ungroup() %>% 
  separate(., site, c("drop", "iter_id"),
           sep = "_", remove = TRUE, convert = TRUE) %>% 
  select(., -drop) %>% 
  mutate_at(., "habitat", factor)

### Compute SES.FD metrics
FD_habitats <-  FD_habitats %>% 
  left_join(., nullfric_habs_summ) %>% 
  left_join(., nullfeve_habs_summ) %>% 
  left_join(., nullfdiv_habs_summ) %>% 
  left_join(., nullfdis_habs_summ) %>%
  mutate(sesFRic = (FRic - mean_nullFRic ) / sd_nullFRic,     # Actual SES.FD computations
         sesFEve = (FEve - mean_nullFEve ) / sd_nullFEve,
         sesFDiv = (FDiv - mean_nullFDiv ) / sd_nullFDiv,
         sesFDis = (FDis - mean_nullFDis ) / sd_nullFDis) %>% 
  select(., "habitat", "iter_id",
         "FRic", "FEve", "FDiv", "FDis",
         "sesFRic", "sesFEve", "sesFDiv", "sesFDis")

# 4. RESULTS ####
# Merge and save
PD_FD_habitats <- left_join(PD_habitats, FD_habitats)
#write_csv(PD_FD_habitats, file = "outputs/PD_FD_habitats.csv")
