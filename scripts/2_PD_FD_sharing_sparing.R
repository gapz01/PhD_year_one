## Header ----
## Script name: PD_FD_sharing_sparing
##
## Purpose of script: Compute PD and FD across sharing/sparing simulations
## Author: Giovanny Perez, Simon Mills
## Date Created: November 2022
## Email: gaperezzuleta1@sheffield.ac.uk, gapz01@gmail.com
##
## Notes:
## This script contains all the code for analysis in Chapter 1 paper
## Sharing/Sparing simulations vary by production level and number of units
## Using phylogenetic diversity (PD) and functional diversity (FD) indices.  


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

# Load phylogenies (for section 3)
# 1000 trees multiPhylo object downloaded from birdtree.org
phylogenies <- read.tree("data/amazon_species_phylogeny")

# Load species range size (for EDR, data from AVONET [Tobias et.al., 2022])
range_size <- read_csv("data/amazon_range_size.csv")


# --- Pre-processing of functional space ---
# Load functional traits
# from AVONET (Tobias et.al., 2022) and Elton's traits (Wilman et.al., 2014)
func_traits <- read_csv("data/amazon_traits.csv")

# Selection of the best functional space for FD (following Palacio et.al., 2022)
# Check correlations between traits
func_traits <- column_to_rownames(func_traits, "species")
corrplot::corrplot(cor(func_traits), type = "upper", diag = FALSE)

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
prop_scale <- function(x)        # To Scale and center "proportion of forest" variable (from Birch)
  (x-0.05118145)/0.1018974              
wf_lvl <- c("low_yield" = 0.6,   # levels of WF features according to observed (field) WF values
            "high_yield" = 0.1)                     

# Set up the 4 types of point needed for simulation
# In mod$data, habitat is set 1: forest, -1: pasture

var_combns <- tibble(habitat2 = c(-1, -1, -1, 1), 
                     prop     = c(wf_lvl["high_yield"],
                                  wf_lvl["low_yield"], 0, 0), 
                     prop_sc  = prop_scale(prop),
                     habitat  = c("pasture_high_yield", # i.e. low wf
                                  "pasture_low_yield",  # i.e. high wf
                                  "pasture_0wf",
                                  "forest"))

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


# Generate predictions (flocker package)
preds_full <- fitted_flocker(mod, 
                        new_data   = pred_data, 
                        type       = "occupancy", 
                        re_formula = ~ (1 + habitat2 + tc_1000_sc + prop_sc | species),
                        summarise  = FALSE, 
                        ndraws     = 1000)

# Gather all the information needed for simulations (predictors and predictions)
preds_full <- cbind(pred_data, preds_full)                                            

# Replacing species names to match phylogenies and functional traits names (in later sections)
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
occ_forest_mat <- preds_full %>% 
  filter(habitat == "forest") %>% 
  select(contains("iter")) %>% 
  as.matrix

occ_pasture_0_wf_mat <- preds_full %>% 
  filter(habitat == "pasture_0wf") %>% 
  select(contains("iter")) %>% as.matrix

occ_pasture_low_wf_mat <- preds_full %>%
  filter(habitat == "pasture_high_yield") %>%
  select(contains("iter")) %>% 
  as.matrix

occ_pasture_high_wf_mat <- preds_full %>%
  filter(habitat == "pasture_low_yield") %>%
  select(contains("iter")) %>%
  as.matrix


# 2. COMMUNITIES SIMULATION ####

# We use random binomial trials to simulate communities at varying landscape sizes
# create matrices of correct dimensions (note: values will be overwritten in next step)
Z_forest_mat <- 
  Z_pasture_0wf_mat <- 
  Z_pasture_low_wf_mat <- 
  Z_pasture_high_wf_mat <- occ_forest_mat   # "empty" matrices, to write into in the next step
n_probs <- prod(dim(occ_forest_mat))        # number of observations (n) for rbinom
n_pts <- c(10, seq(50, 450, 50))            # Size of management units (landscape sizes) 
species_names <- unique(preds_full$species) # species names (to put in communities)
  
  
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


# 3. PHYLOGENETIC DIVERSITY (PD) ####

### 3.1 PD and sesPD metrics computation ####

# LOW YIELD
# Need empty objects to fill later in the loop
catch_low_pd_sh <- 
catch_low_pd_sp <- 
PD_sparing <-
  PD_sharing <-
  sesPD_sparing <-
  sesPD_sharing <-
  MPD_sparing <-
  MPD_sharing <-
  sesMPD_sparing <-
  sesMPD_sharing <-
  MNTD_sparing <-
  MNTD_sharing <-
  sesMNTD_sparing <-
  sesMNTD_sharing <- c()

# Loop over management units
# this looks repetitive, but is to make sure computations use the exact same matrices
for(n_pt_i in n_pts) {                                                          
  mat_sparing <- comms_sp_low[[n_pt_i]] 
  mat_sharing <- comms_sh_low[[n_pt_i]]
  
  # compute the mean abundance to use for abundance-weighted ses metrics
  weight_sp <- apply(mat_sparing, 2, mean)
  weight_sh <- apply(mat_sharing, 2, mean)
  
  # Actual PD calculation using a different (random) phylogeny for each iteration
  for (i in 1:1000) { 
    # Sparing
    PD_sparing[i]      <- pd.query  (phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ])
    
    MPD_sparing[i]     <- mpd.query (phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ])
    
    MNTD_sparing[i]    <- mntd.query(phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ])
    
    sesPD_sparing[i]   <- pd.query  (phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sp)
    
    sesMPD_sparing[i]  <- mpd.query (phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sp)
    
    sesMNTD_sparing[i] <- mntd.query(phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sp)
    
    # Sharing
    PD_sharing[i]      <- pd.query  (phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ])
    
    MPD_sharing[i]     <- mpd.query (phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ])
    
    MNTD_sharing[i]    <- mntd.query(phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ])
    
    sesPD_sharing[i]   <- pd.query  (phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sh)
    
    sesMPD_sharing[i]  <- mpd.query (phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sh)
    
    sesMNTD_sharing[i] <- mntd.query(phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sh)
  }
  
  # Bind by sparing/sharing 
  catch_low_pd_sp[[n_pt_i]] <- data.frame(n_pt = n_pt_i,
                                    iter_id    = 1:1000,
                                    Yield      = "Low",
                                    Management = "Sparing",
                                    PD         = PD_sparing,
                                    sesPD      = sesPD_sparing,
                                    MPD        = MPD_sparing,
                                    sesMPD     = sesMPD_sparing,
                                    MNTD       = MNTD_sparing,
                                    sesMNTD    = sesMNTD_sparing)
  
  catch_low_pd_sh[[n_pt_i]] <- data.frame(n_pt = n_pt_i,
                                    iter_id    = 1:1000,
                                    Yield      = "Low",
                                    Management = "Sharing",
                                    PD         = PD_sharing,
                                    sesPD      = sesPD_sharing,
                                    MPD        = MPD_sharing,
                                    sesMPD     = sesMPD_sharing,
                                    MNTD       = MNTD_sharing,
                                    sesMNTD    = sesMNTD_sharing)
  }


# HIGH YIELD
# Need empty objects to fill later in the loop
catch_high_pd_sp <-
catch_high_pd_sh <- 
PD_sparing <-
  PD_sharing <-
  sesPD_sparing <-
  sesPD_sharing <-
  MPD_sparing <-
  MPD_sharing <-
  sesMPD_sparing <-
  sesMPD_sharing <-
  MNTD_sparing <-
  MNTD_sharing <-
  sesMNTD_sparing <-
  sesMNTD_sharing <- c()

# Loop over management units
# this looks repetitive, but is to make sure computations use the exact same matrices
for(n_pt_i in n_pts) {                                                          
  mat_sparing <- comms_sp_high[[n_pt_i]] 
  mat_sharing <- comms_sh_high[[n_pt_i]]
  
  # compute the mean abundance to use for abundance-weighted ses metrics
  weight_sp <- apply(mat_sparing, 2, mean)  
  weight_sh <- apply(mat_sharing, 2, mean)
  
  # Actual PD calculation using a different (random) phylogeny for each iteration
  for (i in 1:1000) { 
    # Sparing
    PD_sparing[i]      <- pd.query  (phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ])
    
    MPD_sparing[i]     <- mpd.query (phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ])
    
    MNTD_sparing[i]    <- mntd.query(phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ])
    
    sesPD_sparing[i]   <- pd.query  (phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sp)
   
    sesMPD_sparing[i]  <- mpd.query (phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sp)
    
    sesMNTD_sparing[i] <- mntd.query(phylogenies[[sample(1:1000, 1)]],
                                     mat_sparing[i, ],
                                     standardize = TRUE,
                                     null.model = "sequential",
                                     abundance.weights = weight_sp)
    
    # Sharing
    PD_sharing[i]      <- pd.query  (phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ])
    
    MPD_sharing[i]     <- mpd.query (phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ])
    
    MNTD_sharing[i]    <- mntd.query(phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ])
    
    sesPD_sharing[i]   <- pd.query  (phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sh)
    
    sesMPD_sharing[i]  <- mpd.query (phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sh)
    
    sesMNTD_sharing[i] <- mntd.query(phylogenies[[sample(1:1000, 1)]],
                                     mat_sharing[i, ],
                                     standardize = TRUE,
                                     null.model  = "sequential",
                                     abundance.weights = weight_sh)
  }
  
  # Bind by sparing/sharing 
  catch_high_pd_sp[[n_pt_i]] <- data.frame(n_pt = n_pt_i,
                                          iter_id    = 1:1000,
                                          Yield      = "High",
                                          Management = "Sparing",
                                          PD         = PD_sparing,
                                          sesPD      = sesPD_sparing,
                                          MPD        = MPD_sparing,
                                          sesMPD     = sesMPD_sparing,
                                          MNTD       = MNTD_sparing,
                                          sesMNTD    = sesMNTD_sparing)
  
  catch_high_pd_sh[[n_pt_i]] <- data.frame(n_pt = n_pt_i,
                                          iter_id    = 1:1000,
                                          Yield      = "High",
                                          Management = "Sharing",
                                          PD         = PD_sharing,
                                          sesPD      = sesPD_sharing,
                                          MPD        = MPD_sharing,
                                          sesMPD     = sesMPD_sharing,
                                          MNTD       = MNTD_sharing,
                                          sesMNTD    = sesMNTD_sharing)
}

# Bind all PD metrics (sharing/Sparing, Low/High)
PD_metrics <- bind_rows(catch_low_pd_sp, 
                        catch_low_pd_sh,
                        catch_high_pd_sp,
                        catch_high_pd_sh)


### 3.2 ED and EDR computation ####
# For ED and EDR extract the list of species on each iteration

# Get dataframe of communities (from lists) for each yield/management
# Sparing low
comms_sp_low_df <- comms_sp_low %>%
  bind_rows(., .id = "n_pt") %>% 
  mutate(., n_pt = rep(n_pts, each = 1000),
         iter_id = rep(1:1000, times = 10), .after = "n_pt") %>% 
  pivot_longer(., cols = -c("iter_id", "n_pt"),
               names_to = "species", 
               values_to = "Abundance") %>% 
  mutate(., Management = "Sparing",
         Yield = "Low")

# Sparing high
comms_sp_high_df <- comms_sp_high %>%
  bind_rows(., .id = "n_pt") %>% 
  mutate(., n_pt = rep(n_pts, each = 1000),
         iter_id = rep(1:1000, times = 10), .after = "n_pt") %>% 
  pivot_longer(., cols = -c("iter_id", "n_pt"),
               names_to = "species", 
               values_to = "Abundance") %>% 
  mutate(., Management = "Sparing",
         Yield = "High")

# Sharing low
comms_sh_low_df <- comms_sh_low %>%
  bind_rows(., .id = "n_pt") %>% 
  mutate(., n_pt = rep(n_pts, each = 1000),
         iter_id = rep(1:1000, times = 10), .after = "n_pt") %>% 
  pivot_longer(., cols = -c("iter_id", "n_pt"),
               names_to = "species", 
               values_to = "Abundance") %>% 
  mutate(., Management = "Sharing",
         Yield = "Low")

# Sharing high
comms_sh_high_df <- comms_sh_high %>%
  bind_rows(., .id = "n_pt") %>% 
  mutate(., n_pt = rep(n_pts, each = 1000),
         iter_id = rep(1:1000, times = 10), .after = "n_pt") %>% 
  pivot_longer(., cols = -c("iter_id", "n_pt"),
               names_to = "species", 
               values_to = "Abundance") %>% 
  mutate(., Management = "Sharing",
         Yield = "High")

# Bind all communities in a dataframe and create ED and EDR columns
# (These will be replaced  for their numeric values in the next step)
ED_metrics <- bind_rows(comms_sp_low_df,  
                        comms_sp_high_df,
                        comms_sh_low_df, 
                        comms_sh_high_df) %>% 
  mutate(., ED = if_else(Abundance >=1, species, NULL),
         EDR = ED) %>% 
  as.data.frame()

# Remove those big objects from memory
remove(comms_sp_low_df, comms_sp_high_df, comms_sh_low_df, comms_sh_high_df)

# Create a lookup table with ED and EDR values

# Compute ED values across the subset of phylogenies
ED_values <- c()
for (i in 1:1000) {
  ED_values[[i]] <- evol.distinct(tree = phylogenies[[i]], type = "fair.proportion")
}
ED_values <- bind_rows(ED_values) %>%
  group_by(., Species) %>%
  summarise(ED = mean(w)) %>% 
  rename(., species = Species)

# Compute EDR using range size data (AVONET, Tobias etal 2022)
ED_values <- range_size %>% 
  left_join(ED_values, ., by = "species") %>% 
  mutate(., EDR = ED/range_size) %>% 
  as.data.frame()

# Replace the ED values
ED_metrics <- FindReplace(data = ED_metrics,
                          Var = "ED",
                          replaceData = ED_values,
                          from = "species", 
                          to = "ED", 
                          exact = TRUE, vector = FALSE)

# Put the EDR values
ED_metrics <- FindReplace(data = ED_metrics,
                       Var = "EDR",
                       replaceData = ED_values,
                       from = "species", 
                       to = "EDR", 
                       exact = TRUE, vector = FALSE)

# Summarize ED and EDR for each iteration and format variables
ED_metrics <- ED_metrics %>% 
  mutate(., across(c(ED, EDR), as.numeric)) %>% 
  group_by(., n_pt, Management, Yield, iter_id) %>% 
  na.omit() %>% 
  summarise(ED = sum(ED),
            EDR = sum(EDR)) %>% 
  ungroup()



# 4. FUNCTIONAL DIVERSITY ####
# Computations were split in smaller chunks because the package is too memory-consuming


### 4.1 Distance based FD metrics #### 
# (Across management scenarios and n_pts) 

# LOW YIELD
catch_low_fd_sp <- catch_low_fd_sh <- c()   #"empty" object to fill within the loop

# Loop over management unit sizes
for(n_pt_i in n_pts) {
  # Extract matrix at each size for sharing and sparing
  mat_sparing <- comms_sp_low[[n_pt_i]] %>% as.matrix()   
  mat_sharing <- comms_sh_low[[n_pt_i]] %>% as.matrix()
  
  # Compute FD metrics (fundiversity::) 
  FRic_sparing <- fd_fric(func_space_coords, mat_sparing, stand = T)
  FEve_sparing <- fd_feve(func_space_coords, mat_sparing)
  FDiv_sparing <- fd_fdiv(func_space_coords, mat_sparing)
  FDis_sparing <- fd_fdis(func_space_coords, mat_sparing)
  
  FRic_sharing <- fd_fric(func_space_coords, mat_sharing, stand = T)
  FEve_sharing <- fd_feve(func_space_coords, mat_sharing)
  FDiv_sharing <- fd_fdiv(func_space_coords, mat_sharing)
  FDis_sharing <- fd_fdis(func_space_coords, mat_sharing) 
  
  # Bind by sharing/sparing
  catch_low_fd_sp[[n_pt_i]] <- data.frame(n_pt       = n_pt_i,
                                          iter_id    = 1:1000,
                                          Yield      = "Low",
                                          Management = "Sparing",
                                          FRic       = FRic_sparing$FRic,
                                          FEve       = FEve_sparing$FEve,
                                          FDiv       = FDiv_sparing$FDiv,
                                          FDis       = FDis_sparing$FDis)
  
  catch_low_fd_sh[[n_pt_i]] <- data.frame(n_pt       = n_pt_i,
                                          iter_id    = 1:1000,
                                          Yield      = "Low",
                                          Management = "Sharing",
                                          FRic       = FRic_sharing$FRic,
                                          FEve       = FEve_sharing$FEve,
                                          FDiv       = FDiv_sharing$FDiv,
                                          FDis       = FDis_sharing$FDis)
  
}
gc()

# HIGH YIELD
catch_high_fd_sp <- catch_high_fd_sh <- c()   #"empty" list to fill in the loop

# Loop over management units
for(n_pt_i in n_pts) {
  # Extract matrix at each size for sharing and sparing
  mat_sparing <- comms_sp_high[[n_pt_i]] %>% as.matrix()
  mat_sharing <- comms_sh_high[[n_pt_i]] %>% as.matrix()
  
  # Compute FD metrics (fundiversity::) 
  FRic_sparing <- fd_fric(func_space_coords, mat_sparing, stand = T)
  FEve_sparing <- fd_feve(func_space_coords, mat_sparing)
  FDiv_sparing <- fd_fdiv(func_space_coords, mat_sparing)
  FDis_sparing <- fd_fdis(func_space_coords, mat_sparing)
  
  FRic_sharing <- fd_fric(func_space_coords, mat_sharing, stand = T)
  FEve_sharing <- fd_feve(func_space_coords, mat_sharing)
  FDiv_sharing <- fd_fdiv(func_space_coords, mat_sharing)
  FDis_sharing <- fd_fdis(func_space_coords, mat_sharing)
  
  # Bind by sparing/sharing
  catch_high_fd_sp[[n_pt_i]] <- data.frame(n_pt       = n_pt_i,
                                           iter_id    = 1:1000,
                                           Yield      = "High",
                                           Management = "Sparing",
                                           FRic       = FRic_sparing$FRic,
                                           FEve       = FEve_sparing$FEve,
                                           FDiv       = FDiv_sparing$FDiv,
                                           FDis       = FDis_sparing$FDis)
  
  catch_high_fd_sh[[n_pt_i]] <- data.frame(n_pt       = n_pt_i,
                                           iter_id    = 1:1000,
                                           Yield      = "High",
                                           Management = "Sharing",
                                           FRic       = FRic_sharing$FRic,
                                           FEve       = FEve_sharing$FEve,
                                           FDiv       = FDiv_sharing$FDiv,
                                           FDis       = FDis_sharing$FDis)
  
}
gc()


# Bind all FD metrics in a dataframe and format
FD_metrics_dist <- bind_rows(catch_low_fd_sp, 
                             catch_low_fd_sh,
                             catch_high_fd_sp,
                             catch_high_fd_sh)

FD_metrics_dist <- mutate_at(FD_metrics_dist, 
                             c("n_pt", "Yield", "Management"), factor)

### 4.2 SES.FD metrics #### 

# The first piece of code computes null functional indices (needed for ses.FD metrics later)
# Because of time/memory heavy, decided on 100 randomizations, and split computations
# between high, low and sharing, sparing for each metric. 

#--- Only the first chunk is annotated ---#

### LOW YIELD
###### FRIC
######### Sparing_low

fd_index_rands <- fd_index_rands_at_npts <- c()   # Empty object to write into

for (n_pt_i in n_pts) {        # looping over management unit size
  for (i in 1:100) {           # looping over 100 randomizations
    
    rand <- randomizeMatrix(comms_sp_low[[n_pt_i]],   # Extract community from the list
                            "richness")               # randomize abundances within samples maintains species richness
    
    fd_index_rands[[i]] <- data.frame(
      fd_fric(func_space_coords, rand, stand = TRUE), # Actual FD computation (for each randomization)
      n_pt_i)        # Store management unit size
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)  # Store all FD values at each n_pts size
}

# Store in a data frame
null_fric_sp_low <- bind_rows(fd_index_rands_at_npts) %>%
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sparing", Yield = "Low")

######### Sharing low 
fd_index_rands <- fd_index_rands_at_npts <- c()
for (n_pt_i in n_pts) {     
  for (i in 1:100) {        
    rand <- randomizeMatrix(comms_sh_low[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fric(func_space_coords, rand, stand = TRUE), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fric_sh_low <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sharing", Yield = "Low")


###### FEVE
######### Sparing Low
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sp_low[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_feve(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_feve_sp_low <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sparing", Yield = "Low")

####### Sharing Low
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sh_low[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_feve(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_feve_sh_low <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>% 
  mutate(Management = "Sharing", Yield = "Low")


###### FDIV
######### Sparing Low
fd_index_rands <- fd_index_rands_at_npts <- c()
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sp_low[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fdiv(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fdiv_sp_low <- bind_rows(fd_index_rands_at_npts) %>%
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sparing", Yield = "Low")

######### Sharing Low
fd_index_rands <- fd_index_rands_at_npts <- c()
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sh_low[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fdiv(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fdiv_sh_low <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sharing", Yield = "Low")


###### FDIS
######### Sparing Low
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sp_low[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fdis(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fdis_sp_low <- bind_rows(fd_index_rands_at_npts) %>%
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sparing", Yield = "Low")

######### Sharing Low
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sh_low[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fdis(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fdis_sh_low <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sharing", Yield = "Low")


### HIGH YIELD
###### FRIC
######### Sparing_high 
fd_index_rands <- fd_index_rands_at_npts <- c()
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sp_high[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fric(func_space_coords, rand, stand = TRUE), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fric_sp_high <- bind_rows(fd_index_rands_at_npts) %>%
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sparing", Yield = "High")

######### Sharing high
fd_index_rands <- fd_index_rands_at_npts <- c()
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sh_high[[n_pt_i]], "richness") 
    fd_index_rands[[i]] <- data.frame(fd_fric(func_space_coords, rand, stand = TRUE), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fric_sh_high <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sharing", Yield = "High")


###### FEVE
######### Sparing high
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) { 
  for (i in 1:100) { 
    rand <- randomizeMatrix(comms_sp_high[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_feve(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_feve_sp_high <- bind_rows(fd_index_rands_at_npts) %>%
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sparing", Yield = "High")

######### Sharing high
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sh_high[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_feve(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_feve_sh_high <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>% 
  mutate(Management = "Sharing", Yield = "High")


###### FDIV
######### Sparing High
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sp_high[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fdiv(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fdiv_sp_high <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sparing", Yield = "High")

######### Sharing High
fd_index_rands <- fd_index_rands_at_npts <- c()
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sh_high[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fdiv(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands) 
}
null_fdiv_sh_high <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sharing", Yield = "High")


###### FDIS
######### Sparing High
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) { 
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sp_high[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fdis(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fdis_sp_high <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sparing", Yield = "High")

######### Sharing High
fd_index_rands <- fd_index_rands_at_npts <- c() 
for (n_pt_i in n_pts) {
  for (i in 1:100) {
    rand <- randomizeMatrix(comms_sh_high[[n_pt_i]], "richness")
    fd_index_rands[[i]] <- data.frame(fd_fdis(func_space_coords, rand), n_pt_i)
  }
  fd_index_rands_at_npts[[n_pt_i]] <- bind_rows(fd_index_rands)
}
null_fdis_sh_high <- bind_rows(fd_index_rands_at_npts) %>% 
  rename(n_pt = n_pt_i) %>%
  mutate(Management = "Sharing", Yield = "High")


# Compute  mean null FD and sd null FD values
# --- for each index separately ---

###### FRic
null_FRic <- bind_rows(null_fric_sp_low,
                       null_fric_sp_high,
                       null_fric_sh_low,
                       null_fric_sh_high) %>% 
  group_by(Management, Yield, n_pt, site) %>% 
  summarise(mean_nullFRic = mean(FRic),
            sd_nullFRic = sd(FRic)) %>% 
  ungroup() %>% 
  # site should be named "iter_id" and be numeric, need to split that
  separate(., site, c("drop", "iter_id"),
           sep = "_", remove = TRUE, convert = TRUE) %>% 
  select(., -drop) %>% 
  mutate_at(., c("n_pt", "Yield", "Management"), factor)

###### FEve
null_FEve <- bind_rows(null_feve_sp_low,
                       null_feve_sp_high,
                       null_feve_sh_low,
                       null_feve_sh_high) %>% 
  group_by(Management, Yield, n_pt, site) %>% 
  summarise(mean_nullFEve = mean(FEve),
            sd_nullFEve = sd(FEve)) %>% 
  ungroup() %>%
  separate(., site, c("drop", "iter_id"),
           sep = "_", remove = TRUE, convert = TRUE) %>%
  select(., -drop) %>% 
  mutate_at(., c("n_pt", "Yield", "Management"), factor)

###### FDiv
null_FDiv <- bind_rows(null_fdiv_sp_low,
                       null_fdiv_sp_high,
                       null_fdiv_sh_low,
                       null_fdiv_sh_high) %>% 
  group_by(Management, Yield, n_pt, site) %>% 
  summarise(mean_nullFDiv = mean(FDiv),
            sd_nullFDiv = sd(FDiv)) %>% 
  ungroup() %>%
  separate(., site, c("drop", "iter_id"),
           sep = "_", remove = TRUE, convert = TRUE) %>%
  select(., -drop) %>%
  mutate_at(., c("n_pt", "Yield", "Management"), factor)


###### FDis
null_FDis <- bind_rows(null_fdis_sp_low,
                       null_fdis_sp_high,
                       null_fdis_sh_low,
                       null_fdis_sh_high) %>% 
  group_by(Management, Yield, n_pt, site) %>% 
  summarise(mean_nullFDis = mean(FDis),
            sd_nullFDis = sd(FDis)) %>% 
  ungroup() %>% 
  separate(., site, c("drop", "iter_id"),
           sep = "_", remove = TRUE, convert = TRUE) %>% 
  select(., -drop) %>% 
  mutate_at(., c("n_pt", "Yield", "Management"), factor)


# Merge null values to observed values and compute ses metric
FD_metrics_ses <-
  left_join(null_FRic, null_FEve) %>%
  left_join(., null_FDiv) %>%
  left_join(., null_FDis) %>%
  left_join(FD_metrics_dist, .) %>%
  mutate(sesFRic = (FRic - mean_nullFRic ) / sd_nullFRic,   # Actual SES.FD computation
         sesFEve = (FEve - mean_nullFEve ) / sd_nullFEve,
         sesFDiv = (FDiv - mean_nullFDiv ) / sd_nullFDiv,
         sesFDis = (FDis - mean_nullFDis ) / sd_nullFDis) %>% 
  select(., "n_pt", "iter_id", "Yield", "Management", 
         "FRic", "FEve", "FDiv", "FDis",
         "sesFRic", "sesFEve", "sesFDiv", "sesFDis")



# 5. RESULTS ####  

# Bind all PD and FD metrics in a tidy table
PD_FD_metrics <- 
  PD_metrics %>%
  left_join(., ED_metrics) %>%
  left_join(., FD_metrics_dist)%>%
  left_join(., FD_metrics_ses) %>%
  mutate_at(., c("n_pt", "Yield", "Management"), factor) %>%
  tibble()

#write_csv(PD_FD_metrics, "outputs/PD_FD_sparing-sharing.csv")
