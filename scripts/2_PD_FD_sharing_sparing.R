## Header ---- 
## From paper: Avian phylogenetic and functional diversity are better conserved 
## by land-sparing than land-sharing farming in lowland tropical forests.
## Journal of Applied Ecology
##
## Purpose of script: Compute PD and FD across land-sharing and land-sparing
## simulated landscapes
##
## Author: Giovanny Perez, Simon Mills
## Email: gaperezzuleta1@sheffield.ac.uk, gapz01@gmail.com


# 0. Load data and packages ####

# Required packages

library(ape)             # Phylogenetic trees handling
library(PhyloMeasures)   # Fast computation of PD metrics
library(mFD)             # Computation of FD metrics and selection of trait space
library(fundiversity)    # faster FD computation
library(tidyverse)       # Data handling and plots
library(DataCombine)     # Find&Replace values in a dataframes
library(picante)         # randomizeMatrix function

# Load phylogenies 
phylogenies <- readRDS("data/phylogeny_amazonbirds.rds")
phylogenies <- unlist(phylogenies, recursive = F)

# For EDR
ED_values <- read_csv("data/ED_values_amazonbirds.csv")

# functional space
func_space_coords <- readRDS("outputs/func_space_coords.rds")

# load simulated communities
comms_sh_high <- readRDS("outputs/comms_sh_high.rds")
comms_sh_low <- readRDS("outputs/comms_sh_low.rds")
comms_sp_high <- readRDS("outputs/comms_sp_high.rds")
comms_sp_low <- readRDS("outputs/comms_sp_low.rds")

# Set size of management units (landscape sizes) 
n_pts <- c(10, seq(50, 450, 50))           


# 2. Phylogenetic diversity (PD) ####

### 2.1 PD and sesPD metrics computation ####

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
  sesMNTD_sharing <- 
  PD_null <- 
  MPD_null <- 
  MNTD_null <- c()

# Loop over management units
# this looks repetitive, but is to make sure computations use the exact same matrices
for(n_pt_i in n_pts) {                                                          
  mat_sparing <- comms_sp_low[[n_pt_i]] 
  mat_sharing <- comms_sh_low[[n_pt_i]]
  
  # # compute the mean abundance to use for abundance-weighted ses metrics
  # weight_sp <- apply(mat_sparing, 2, mean)
  # weight_sh <- apply(mat_sharing, 2, mean)
  
  # Actual PD calculation using a different (random) phylogeny for each iteration
  for (i in 1:1000) { 
    # Sparing
    tree <- phylogenies[[sample(1:1000, 1)]]
    comm <- mat_sparing[i, ]
    
    PD_sparing[i]      <- pd.query  (tree, comm)
    MPD_sparing[i]     <- mpd.query (tree, comm)
    MNTD_sparing[i]    <- mntd.query(tree, comm)
    
    for (r in 1:100) {   # number of randomisations
      #compute PD for a randomized community 100 times
      rand <- randomizeMatrix(comm, "richness")
      PD_null[r] <- pd.query (tree, rand)
      MPD_null[r] <- mpd.query (tree, rand)
      MNTD_null[r] <- mntd.query (tree, rand)
    }
    
    #compute SES metrics
    sesPD_sparing[i] <- (PD_sparing[i] - mean(PD_null)) / sd(PD_null)
    sesMPD_sparing[i] <- (MPD_sparing[i] - mean(MPD_null)) / sd(MPD_null)
    sesMNTD_sparing[i] <- (MNTD_sparing[i] - mean(MNTD_null)) / sd(MNTD_null)
    
    # Sharing
    comm <- mat_sharing[i, ]
    
    PD_sharing[i]      <- pd.query  (tree, comm)
    MPD_sharing[i]     <- mpd.query (tree, comm)
    MNTD_sharing[i]    <- mntd.query(tree, comm)
    
    for (r in 1:100) {   # number of randomisations
      #compute PD for a randomized community 100 times
      rand <- randomizeMatrix(comm, "richness")
      PD_null[r] <- pd.query (tree, rand)
      MPD_null[r] <- mpd.query (tree, rand)
      MNTD_null[r] <- mntd.query (tree, rand)
    }
    
    #compute SES metrics
    sesPD_sharing[i] <- (PD_sharing[i] - mean(PD_null)) / sd(PD_null)
    sesMPD_sharing[i] <- (MPD_sharing[i] - mean(MPD_null)) / sd(MPD_null)
    sesMNTD_sharing[i] <- (MNTD_sharing[i] - mean(MNTD_null)) / sd(MNTD_null)

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
  sesMNTD_sharing <-
  PD_null <- 
  MPD_null <- 
  MNTD_null <- c()

# Loop over management units
# this looks repetitive, but is to make sure computations use the exact same matrices
for(n_pt_i in n_pts) {                                                          
  mat_sparing <- comms_sp_high[[n_pt_i]] 
  mat_sharing <- comms_sh_high[[n_pt_i]]
  
  # # compute the mean abundance to use for abundance-weighted ses metrics
  # weight_sp <- apply(mat_sparing, 2, mean)  
  # weight_sh <- apply(mat_sharing, 2, mean)
  
  # Actual PD calculation using a different (random) phylogeny for each iteration
  for (i in 1:1000) { 
    # Sparing
    tree <- phylogenies[[sample(1:1000, 1)]]
    comm <- mat_sparing[i, ]
    
    PD_sparing[i]      <- pd.query  (tree, comm)
    MPD_sparing[i]     <- mpd.query (tree, comm)
    MNTD_sparing[i]    <- mntd.query(tree, comm)
    
    for (r in 1:100) {   # number of randomisations
      #compute PD for a randomized community 100 times
      rand <- randomizeMatrix(comm, "richness")
      PD_null[r] <- pd.query (tree, rand)
      MPD_null[r] <- mpd.query (tree, rand)
      MNTD_null[r] <- mntd.query (tree, rand)
    }
    
    #compute SES metrics
    sesPD_sparing[i] <- (PD_sparing[i] - mean(PD_null)) / sd(PD_null)
    sesMPD_sparing[i] <- (MPD_sparing[i] - mean(MPD_null)) / sd(MPD_null)
    sesMNTD_sparing[i] <- (MNTD_sparing[i] - mean(MNTD_null)) / sd(MNTD_null)
    
    # Sharing
    comm <- mat_sharing[i, ]
    
    PD_sharing[i]      <- pd.query  (tree, comm)
    MPD_sharing[i]     <- mpd.query (tree, comm)
    MNTD_sharing[i]    <- mntd.query(tree, comm)
    
    for (r in 1:100) {   # number of randomisations
      #compute PD for a randomized community 100 times
      rand <- randomizeMatrix(comm, "richness")
      PD_null[r] <- pd.query (tree, rand)
      MPD_null[r] <- mpd.query (tree, rand)
      MNTD_null[r] <- mntd.query (tree, rand)
    }
    
    #compute SES metrics
    sesPD_sharing[i] <- (PD_sharing[i] - mean(PD_null)) / sd(PD_null)
    sesMPD_sharing[i] <- (MPD_sharing[i] - mean(MPD_null)) / sd(MPD_null)
    sesMNTD_sharing[i] <- (MNTD_sharing[i] - mean(MNTD_null)) / sd(MNTD_null)
    
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
PD_farming <- bind_rows(catch_low_pd_sp, 
                        catch_low_pd_sh,
                        catch_high_pd_sp,
                        catch_high_pd_sh)

PD_farming <- mutate_at(PD_farming,
                        c("n_pt", "Yield", "Management"), factor)


### 2.2 ED and EDR computation ####
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
ED_farming <- bind_rows(comms_sp_low_df,  
                        comms_sp_high_df,
                        comms_sh_low_df, 
                        comms_sh_high_df) %>% 
  mutate(., ED = if_else(Abundance >=1, species, NA),
         EDR = ED) %>% drop_na() %>% 
  as.data.frame()

# Remove those big objects from memory
remove(comms_sp_low_df, comms_sp_high_df, comms_sh_low_df, comms_sh_high_df);gc()

# Replace the ED values
ED_farming <- FindReplace(data = ED_farming,
                          Var = "ED",
                          replaceData = ED_values,
                          from = "species", 
                          to = "ED_mean", 
                          exact = TRUE, vector = FALSE)

# and the EDR values
ED_farming <- FindReplace(data = ED_farming,
                       Var = "EDR",
                       replaceData = ED_values,
                       from = "species", 
                       to = "EDR_mean", 
                       exact = TRUE, vector = FALSE)

# Summarize ED and EDR for each iteration and format variables
ED_farming <- ED_farming %>% 
  mutate(., across(c(ED, EDR), as.numeric)) %>% 
  group_by(., n_pt, Management, Yield, iter_id) %>% 
  summarise(ED = sum(ED),
            EDR = sum(EDR)) %>% 
  ungroup()

ED_farming <- mutate_at(ED_farming,
                        c("n_pt", "Yield", "Management"), factor)

# 3. Functional diversity ####
# Computations were split in smaller chunks because the package is too memory-consuming


### 3.1 Distance based FD metrics #### 
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
FD_farming_obs <- bind_rows(catch_low_fd_sp, 
                             catch_low_fd_sh,
                             catch_high_fd_sp,
                             catch_high_fd_sh)

FD_farming_obs <- mutate_at(FD_farming_obs, 
                             c("n_pt", "Yield", "Management"), factor)

### 3.2 SES.FD metrics #### 

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
FD_farming_ses <-
  left_join(null_FRic, null_FEve) %>%
  left_join(., null_FDiv) %>%
  left_join(., null_FDis) %>%
  left_join(FD_farming_obs, .) %>%
  mutate(sesFRic = (FRic - mean_nullFRic ) / sd_nullFRic,   # Actual SES.FD computation
         sesFEve = (FEve - mean_nullFEve ) / sd_nullFEve,
         sesFDiv = (FDiv - mean_nullFDiv ) / sd_nullFDiv,
         sesFDis = (FDis - mean_nullFDis ) / sd_nullFDis) %>% 
  select(., "n_pt", "iter_id", "Yield", "Management", 
         "FRic", "FEve", "FDiv", "FDis",
         "sesFRic", "sesFEve", "sesFDiv", "sesFDis")



# 4. Results ####  

# Bind all PD and FD metrics in a tidy table
PD_FD_farming <- 
  PD_farming %>%
  left_join(., ED_farming) %>%
  left_join(., FD_farming_obs) %>%
  left_join(., FD_farming_ses) %>%
  #mutate_at(., c("n_pt", "Yield", "Management"), factor) %>%
  tibble()

saveRDS(PD_FD_farming, "outputs/PD_FD_farming.rds")
