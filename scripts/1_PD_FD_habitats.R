## Header ---- 
## From paper: Avian phylogenetic and functional diversity are better conserved 
## by land-sparing than land-sharing farming in lowland tropical forests.
## Journal of Applied Ecology
##
## Purpose of script: Compute PD and FD across forest and WF-pasture gradient
## Author: Giovanny Perez, Simon Mills
## Email: gaperezzuleta1@sheffield.ac.uk, gapz01@gmail.com
##

# 0. Load data and packages ####

# Required packages
library(flocker)         # Occupancy models
library(brms)            # Bayesian models using stan
library(ape)             # Phylogenetic trees handling
library(PhyloMeasures)   # Fast computation of PD metrics
library(mFD)             # Computation of FD metrics and selection of trait space
library(fundiversity)    # faster FD computation
library(tidyverse)       # Data handling and plots
library(DataCombine)     # Find&Replace values in a dataframes
library(picante)         # randomizeMatrix function

# Load predicted communities (script 0)
readRDS("outputs/Z_mats.rds")

# Load lookup table to match names to phylogenies names
correct_names <- readRDS("data/amazon_species_lookup.rds")
names_to_remove_from_model <- correct_names %>% 
  filter(., phylogeny == "Not_in_phylogeny") %>% 
  pull(., model)

# Load phylogenies 
phylogenies <- readRDS("data/amazon_species_phylogeny.rds")
phylogenies <- unlist(phylogenies, recursive = F)

# For EDR
ED_values <- readRDS("data/ED_values.rds")
 
# Functional space
readRDS("outputs/func_space_coords.rds")


# 2. Phylogenetic diversity (PD) ####
### 2.1 PD and SES.PD metrics ####
PD_habitats <- 
  PD_vect <- 
  MPD_vect <- 
  MNTD_vect <- 
  sesPD_vect <- 
  sesMPD_vect <- 
  sesMNTD_vect <- 
  PD_null <- 
  MPD_null <- 
  MNTD_null <- c() # "empty" objects to write into

# PD calculation using a different (random) phylogeny for each iteration
for (j in 1:length(Z_mats)){
  print(j)
  for (i in 1:1000) 
   {
    tree <- phylogenies[[sample(1:10000, 1)]]
    comm <- Z_mats[[j]][i, ]
    PD_vect[i]      <- pd.query (tree, comm)
    MPD_vect[i]     <- mpd.query (tree, comm)
    MNTD_vect[i]    <- mntd.query (tree, comm)
    
    for (r in 1:100) {   # number of randomisations
      #compute PD for a randomized community 100 times
      rand <- randomizeMatrix(comm, "richness")
      PD_null[r] <- pd.query (tree, rand)
      MPD_null[r] <- mpd.query (tree, rand)
      MNTD_null[r] <- mntd.query (tree, rand)
    }
    #compute SES metrics
    sesPD_vect[i] <- (PD_vect[i] - mean(PD_null)) / sd(PD_null)
    sesMPD_vect[i] <- (MPD_vect[i] - mean(MPD_null)) / sd(MPD_null)
    sesMNTD_vect[i] <- (MNTD_vect[i] - mean(MNTD_null)) / sd(MNTD_null)
    
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
  mutate(., ED = if_else(Abundance >=1, species, NA),
         EDR = ED) %>% na.omit() %>% 
  as.data.frame()


# Replace the ED values
ED_mats <- FindReplace(data = Z_mats_df,
                       Var = "ED",
                       replaceData = ED_values,
                       from = "species",
                       to = "ED_mean",
                       exact = TRUE, vector = FALSE)

# Put the EDR values
ED_mats <- FindReplace(data = ED_mats,
                       Var = "EDR",
                       replaceData = ED_values,
                       from = "species",
                       to = "EDR_mean",
                       exact = TRUE, vector = FALSE)

# Summarize ED and EDR for each iteration and format variables
ED_mats <- ED_mats %>%
  mutate(., across(c(ED, EDR), as.numeric)) %>%
  group_by(., habitat, iter_id) %>%
  na.omit() %>%
  summarise(ED = sum(ED),
            EDR = sum(EDR)) %>%
  ungroup()
remove(Z_mats_df)

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

#saveRDS(PD_habitats, "outputs/PD_habitats.rds")



# 3. Functional diversity (FD) ####

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
    print(paste(hab, i))
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
    print(paste(hab, i))
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
    print(paste(hab, i))
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
    print(paste(hab, i))
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

###### FDis
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
#saveRDS(FD_habitats, "outputs/FD_habitats.rds")

# 4. Results ####
# Merge and save
PD_FD_habitats <- left_join(PD_habitats, FD_habitats)
saveRDS(PD_FD_habitats, "outputs/PD_FD_habitats.rds")
