## Header ----
## Script name: PD_FD_figures
##
## Purpose of script: Plot PD and FD across habitats and sharing/sparing
## Author: Giovanny Perez
## Date Created: March 2023
## Email: gaperezzuleta1@sheffield.ac.uk, gapz01@gmail.com
##
## Notes:
##


# 0. LOAD DATA AND PACKAGES ####

# Required packages
library(tidyverse)
library (viridis)
library(patchwork)
library(ggpubr)
library(tidybayes)
library(bayestestR)

# Load dataframes with results
PD_FD_habitats <- read_csv("outputs/PD_FD_habitats.csv")
PD_FD_metrics <-  read_csv("outputs/PD_FD_sparing-sharing.csv")

# 1. HABITAT ####

# create a "gap" in the dataframe to better plot forest and WF-pastures
PD_FD_to_plot <- PD_FD_habitats %>%
  add_row(.before = 1, habitat = "gap", iter_id = 1, 
          PD = 0, MPD = 0, MNTD = 0,
          sesPD = 0, sesMPD = 0, sesMNTD = 0, 
          ED = 0, EDR = 0,
          FRic = 0, FEve = 0, FDiv = 0, FDis = 0,
          sesFRic = 0, sesFEve = 0, sesFDiv = 0, sesFDis = 0) %>% 
  mutate_at(., "habitat", factor, levels = c("forest", "gap", "pasture_0.6",
                                             "pasture_0.5", "pasture_0.4", "pasture_0.3",
                                             "pasture_0.2", "pasture_0.1", "pasture_0"))


# base function to plot 
P_func_hab <- function(x){
  ggplot(PD_FD_to_plot, aes(x = habitat, y = {{x}}, fill = habitat)) +
    geom_violin(alpha = 0.5, color = "transparent") +
    stat_summary (fun.data = mean_sd, 
                  geom = "pointrange",
                  size = 0.1,
                  position = position_dodge(width = 1),
                  color = c("#40B0A6", "transparent", rep("#E1BE6A",7)),
                  show.legend = F) +
    scale_fill_manual(values = c("#40B0A6", "transparent", rep("#E1BE6A",7))) +
    scale_x_discrete(labels=c("", "", "60", "50", "40", "30", "20", "10", "0"), 
                     expand = c(0.125,0,0,0)) +
    geom_hline(yintercept = 0, size = 0.1) +
    xlab(NULL) +
    geom_vline(xintercept = 2, linetype = "dashed", size = 0.5, color = "grey50") +
    theme_linedraw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 11),   # axis values size
          axis.title = element_text(size = 12),  # axis label size
          axis.text.x=element_blank(),           # remove x-axis values
          legend.position = "none") 
}

P_func_hab(PD) #test function

# create plots
#PD
P1 <- P_func_hab(PD) + scale_y_continuous(breaks = c(500, 1500, 2500)) + ylab("FPD")
P2 <- P_func_hab(sesPD) + ylab("SES.FPD") 
P3 <- P_func_hab(MPD) 
P4 <- P_func_hab(sesMPD) + ylab("SES.MPD") 
P5 <- P_func_hab(MNTD) 
P6 <- P_func_hab(sesMNTD) + scale_y_continuous(breaks = seq(-3, 3, by = 1.5)) + ylab("SES.MNTD") 
P7 <- P_func_hab(ED) + scale_y_continuous(breaks = c(500, 1000, 1500)) + theme(axis.text.x=element_text())
P8 <- P_func_hab(EDR*1000) + scale_y_continuous(breaks = seq(0, 1, by = 0.25)) + theme(axis.text.x=element_text())
#FD
F1 <- P_func_hab(FRic) 
F2 <- P_func_hab(sesFRic) + ylab("SES.FRic") 
F3 <- P_func_hab(FEve)
F4 <- P_func_hab(sesFEve) + scale_y_continuous(breaks = seq(-3, 3, by = 1.5)) + ylab("SES.FEve") 
F5 <- P_func_hab(FDiv)
F6 <- P_func_hab(sesFDiv) + ylab("SES.FDiv") 
F7 <- P_func_hab(FDis) + theme(axis.text.x=element_text())
F8 <- P_func_hab(sesFDis)+ scale_y_continuous(breaks = seq(-3, 3, by = 1.5)) + theme(axis.text.x=element_text()) + ylab("SES.FDis") 

#Arrange plots (except MPD and FDiv --> to SOM)
PD_FD_habs <-
  ggarrange(P1, P2, F1, F2, P5, P6, F3, F4, P7, P8, F7, F8, 
            nrow = 3, ncol = 4,
            labels = c("A", "B", "G", "H", "C", "D", "I", "J", "E", "F", "K", "L"), 
            legend = "none", common.legend = TRUE,
            align = "v", font.label = list(size = 12, face = "bold"),
            label.x = 0.1)

ggsave(filename = "habs.png",
       device = "png",
       plot = PD_FD_habs,
       path = "figures/",
       width = 30,
       height = 15,
       units = "cm",
       bg = "white")



# 2. FARM ####
# Using 100 management units 
PD_FD_metrics_100 <- PD_FD_metrics %>% filter(., n_pt == 100) # change n_pt as desired

### Function plot for two yield levels
P_func_farm <- function(x){
  ggplot(PD_FD_metrics_100, aes(x = Yield, y = {{x}}, fill = Management)) +
    geom_rect(aes(xmin=0.5,xmax=1.5,ymin=-Inf,ymax=Inf), fill="gray80") +
    geom_rect(aes(xmin=1.5,xmax=2.5,ymin=-Inf,ymax=Inf), fill="gray95") +
    geom_violin(position = position_dodge(width = 1), alpha = 0.5, scale = "width", color = "transparent") +
    scale_fill_manual(values = c("#5D3A9B", "#E66100")) +
    stat_summary (fun.data = mean_sd, 
                  geom = "pointrange",
                  size = 0.1,
                  position = position_dodge(width = 1),
                  color = c("#5D3A9B", "#E66100","#5D3A9B","#E66100"),
                  show.legend = F) +
    scale_x_discrete(expand = c(0,0,0,0)) +
    xlab(NULL) +
    geom_hline(yintercept = 0, size = 0.1) +
    theme_linedraw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position = "none")
}

P_func_farm(PD) #test plot

# PD
P21 <- P_func_farm(PD) + ylab("FPD")
P22 <- P_func_farm(sesPD) + scale_y_continuous(breaks = seq(-6, 3, by = 3)) + ylab("SESFPD")
P23 <- P_func_farm(MPD)
P24 <- P_func_farm(sesMPD) + ylab("SES.MPD")
P25 <- P_func_farm(MNTD)
P26 <- P_func_farm(sesMNTD) + ylab("SES.MNTD")
P27 <- P_func_farm(ED) + scale_y_continuous(breaks = seq(0, 5000, by = 1000)) + theme(axis.text.x = element_text())
P28 <- P_func_farm(EDR*1000) + scale_y_continuous(breaks = seq(0, 1.8, by = .6)) + theme(axis.text.x = element_text())
# FD
F21 <- P_func_farm(FRic) + scale_y_continuous(breaks = seq(0, 0.8, by = 0.2))
F22 <- P_func_farm(sesFRic) + ylim(c(-3,3)) + ylab("SES.FRic")
F23 <- P_func_farm(FEve) 
F24 <- P_func_farm(sesFEve) + ylab("SES.FEve")
F25 <- P_func_farm(FDiv)
F26 <- P_func_farm(sesFDiv) + ylab("SES.FDiv")
F27 <- P_func_farm(FDis) + theme(axis.text.x = element_text())
F28 <- P_func_farm(sesFDis) + theme(axis.text.x = element_text())+ ylab("SES.FDis")

# Arrange plots
PD_FD_farm <- 
  ggarrange(P21, P22, F21, F22, P25, P26, F23, F24, P27, P28, F27, F28, 
            nrow = 3, ncol = 4,
            labels = c("A", "B", "G", "H", "C", "D", "I", "J", "E", "F", "K", "L"), 
            legend = "none", common.legend = TRUE,
            align = "v", font.label = list(size = 12, face = "bold"),
            label.x = 0.1)

ggsave(filename = "farm.png",
       device = "png",
       plot = PD_FD_farm,
       path = "figures/",
       width = 30,
       height = 15,
       units = "cm",
       bg = "white")





## 3. DIFFERENCES ####
# compute subtractions
PD_FD_metrics_sh <- filter(PD_FD_metrics, Management == "Sharing") 
PD_FD_metrics_sp <- filter(PD_FD_metrics, Management == "Sparing")
PD_FD_metrics_diff <- left_join(PD_FD_metrics_sh,
                                PD_FD_metrics_sp, 
                                by = c("n_pt", "iter_id", "Yield"),
                                suffix = c("_sh", "_sp")) %>%
  mutate(PD = PD_sp - PD_sh, #Sparing minus sharing              
         MPD = MPD_sp - MPD_sh,
         MNTD = MNTD_sp - MNTD_sh,
         sesPD = sesPD_sp - sesPD_sh,
         sesMPD = sesMPD_sp - sesMPD_sh,
         sesMNTD = sesMNTD_sp - sesMNTD_sh,
         ED = ED_sp - ED_sh,
         EDR = EDR_sp - EDR_sh,
         FRic = FRic_sp - FRic_sh,
         FEve = FEve_sp - FEve_sh,
         FDiv = FDiv_sp - FDiv_sh,
         FDis = FDis_sp - FDis_sh,
         sesFRic = sesFRic_sp - sesFRic_sh,
         sesFEve = sesFEve_sp - sesFEve_sh,
         sesFDiv = sesFDiv_sp - sesFDiv_sh,
         sesFDis = sesFDis_sp - sesFDis_sh)


# Function plots
P_func_diff <- function(x){
  ggplot(PD_FD_metrics_diff, aes(x = n_pt, y = {{x}}, fill = Yield, color = Yield)) +
    geom_violin(position = position_dodge(width = 1), alpha = 0.5, scale = "width", size = 0.1, color = "transparent") +
    #scale_colour_manual(values = c("transparent", "grey40")) +
    scale_fill_manual(values = c("grey0", "grey70")) +
    stat_summary (fun.data = mean_sd, 
                  geom = "pointrange",
                  size = 0.001,
                  position = position_dodge(width = 1),
                  color = rep(c("grey10", "grey60"),10),
                  show.legend = F) +
    scale_x_discrete(NULL, breaks = c("10", "100", "200", "300", "400")) +
    theme_linedraw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position = "none") +
    geom_hline(yintercept = 0, linewidth = 0.2, colour = "red", linetype = "dashed")
}


P_func_diff(PD) # test plot

#PD
P31 <- P_func_diff(PD) + ylab("FPD")
P32 <- P_func_diff(sesPD) + scale_y_continuous(limits = c(-5,5), breaks = seq(-4,4,2)) + ylab("SES.FPD")
P33 <- P_func_diff(MPD) 
P34 <- P_func_diff(sesMPD)+ ylab("SES.MPD")
P35 <- P_func_diff(MNTD)
P36 <- P_func_diff(sesMNTD)+ scale_y_continuous(limits = c(-5,5), breaks = seq(-4,4,2)) + ylab("SES.MNTD")
P37 <- P_func_diff(ED) + theme(axis.text.x = element_text()) 
P38 <- P_func_diff(EDR*1000) + theme(axis.text.x = element_text()) 
#FD
F31 <- P_func_diff(FRic)
F32 <- P_func_diff(sesFRic) + scale_y_continuous(limits = c(-5,5), breaks = seq(-4,4,2)) + ylab("SES.FRic")
F33 <- P_func_diff(FEve)
F34 <- P_func_diff(sesFEve)+ scale_y_continuous(limits = c(-5,5), breaks = seq(-4,4,2)) + ylab("SES.FEve")
F35 <- P_func_diff(FDiv)
F36 <- P_func_diff(sesFDiv) + ylab("SES.FDiv")
F37 <- P_func_diff(FDis) + scale_y_continuous(limits = c(-0.025,0.025)) + theme(axis.text.x = element_text()) 
F38 <- P_func_diff(sesFDis) + theme(axis.text.x = element_text()) + ylab("SES.FDis")

# Arrange plots
PD_FD_diff <- 
  ggarrange(P31, P32, F31, F32, P35, P36, F33, F34, P37, P38, F37, F38, 
            nrow = 3, ncol = 4,
            labels = c("A", "B", "G", "H", "C", "D", "I", "J", "E", "F", "K", "L"), 
            legend = "none", common.legend = TRUE,
            align = "v", font.label = list(size = 12, face = "bold"),
            label.x = 0.1) 

ggsave(filename = "diffs.png",
       device = "png",
       plot = PD_FD_diff,
       path = "figures/",
       width = 30,
       height = 15,
       units = "cm",
       bg = "white")


# 4. SOM FIGURE ####
# Plots for MPD and FDiv (in SOM)
P3 <- P_func_hab(MPD) + theme(axis.text.x=element_text())
P4 <- P_func_hab(sesMPD) + theme(axis.text.x=element_text())
F5 <- P_func_hab(FDiv) + theme(axis.text.x=element_text())
F6 <- P_func_hab(sesFDiv) + theme(axis.text.x=element_text())
P23 <- P_func_farm(MPD) + theme(axis.text.x=element_text())
P24 <- P_func_farm(sesPD) + theme(axis.text.x=element_text())
F25 <- P_func_farm(FDiv) + theme(axis.text.x=element_text())
F26 <- P_func_farm(sesFDiv) + theme(axis.text.x=element_text())
P33 <- P_func_diff(MPD) + theme(axis.text.x=element_text())
P34 <- P_func_diff(sesMPD) + theme(axis.text.x=element_text())
F35 <- P_func_diff(FDiv) + theme(axis.text.x=element_text())
F36 <- P_func_diff(sesFDiv) + theme(axis.text.x=element_text())

fig_SOM <- 
  ggarrange(P3, P4, F5, F6, P23, P24, F25, F26, P33, P34, F35, F36, 
            nrow = 3, ncol = 4,
            labels = "AUTO", 
            legend = "none",
            align = "hv", font.label = list(size = 12, face = "bold"))

ggsave(filename = "Fig_SOM.png",
       device = "png",
       plot = fig_SOM,
       path = "figures/",
       width = 30,
       height = 15,
       units = "cm",
       bg = "white")



# 5. Additional Results ####
## Computing probability of direction
## 
## 5.1 Habitats ####
## Differences between forest and pasture-60%
## Pasture-60% and pasture-10%

PD_FD_60 <- filter(PD_FD_habitats, habitat == "pasture_0.6")
PD_FD_10 <- filter(PD_FD_habitats, habitat == "pasture_0.1")
PD_FD_Fo <- filter(PD_FD_habitats, habitat == "forest")

PD_FD_pas_diff <- left_join(PD_FD_60,
                            PD_FD_10, 
                            by = c("iter_id"),
                            suffix = c("_0.6", "_0.1")) %>%
  select(., -c(habitat_0.6, habitat_0.1)) %>% 
  mutate(., PD = PD_0.6 - PD_0.1,
         MPD = MPD_0.6 - MPD_0.1,
         MNTD = MNTD_0.6 - MNTD_0.1,
         ED = ED_0.6 - ED_0.1,
         EDR = EDR_0.6 - EDR_0.1,
         FRic = FRic_0.6 - FRic_0.1,
         FEve = FEve_0.6 - FEve_0.1,
         FDiv = FDiv_0.6 - FDiv_0.1,
         FDis = FDis_0.6 - FDis_0.1) %>%
  select(., PD:FDis) %>% 
  na.omit %>%
  mutate(pd_PD = p_direction(PD),
         pd_MPD = p_direction(MPD),
         pd_MNTD = p_direction(MNTD),
         pd_ED = p_direction(ED),
         pd_EDR = p_direction(EDR),
         pd_FRic = p_direction(FRic),
         pd_FEve = p_direction(FEve),
         pd_FDiv = p_direction(FDiv),
         pd_FDis = p_direction(FDis),
         median_PD = median(PD),
         CI_lo_PD = hdci(PD)[,1],
         CI_hi_PD = hdci(PD)[,2],
         median_MPD = median(MPD),
         CI_lo_MPD = hdci(MPD)[,1],
         CI_hi_MPD = hdci(MPD)[,2],
         median_MNTD = median(MNTD),
         CI_lo_MNTD = hdci(MNTD)[,1],
         CI_hi_MNTD = hdci(MNTD)[,2],
         median_ED = median(ED),
         CI_lo_ED = hdci(ED)[,1],
         CI_hi_ED = hdci(ED)[,2],
         median_EDR = median(EDR),
         CI_lo_EDR = hdci(EDR)[,1],
         CI_hi_EDR = hdci(EDR)[,2],
         median_FRic = median(FRic),
         CI_lo_FRic = hdci(FRic)[,1],
         CI_hi_FRic = hdci(FRic)[,2],
         median_FEve = median(FEve),
         CI_lo_FEve = hdci(FEve)[,1],
         CI_hi_FEve = hdci(FEve)[,2],
         median_FDiv = median(FDiv),
         CI_lo_FDiv = hdci(FDiv)[,1],
         CI_hi_FDiv = hdci(FDiv)[,2],
         median_FDis = median(FDis),
         CI_lo_FDis = hdci(FDis)[,1],
         CI_hi_FDis = hdci(FDis)[,2]) %>%
  select(., pd_PD:CI_hi_FDis) %>%              # change to CI_hi_FDis
summarise_all(mean)


PD_FD_hab_diff <- left_join(PD_FD_Fo,
                            PD_FD_60, 
                            by = c("iter_id"),
                            suffix = c("_Fo", "_0.6")) %>%
  select(., -c(habitat_Fo, habitat_0.6)) %>% 
  mutate(., PD = PD_Fo - PD_0.6,
         MPD = MPD_Fo - MPD_0.6,
         MNTD = MNTD_Fo - MNTD_0.6,
         ED = ED_Fo - ED_0.6,
         EDR = EDR_Fo - EDR_0.6,
         FRic = FRic_Fo - FRic_0.6,
         FEve = FEve_Fo - FEve_0.6,
         FDiv = FDiv_Fo - FDiv_0.6,
         FDis = FDis_Fo - FDis_0.6) %>%
  select(., PD:FDis) %>% 
  na.omit %>%
  mutate(pd_PD = p_direction(PD),
         pd_MPD = p_direction(MPD),
         pd_MNTD = p_direction(MNTD),
         pd_ED = p_direction(ED),
         pd_EDR = p_direction(EDR),
         pd_FRic = p_direction(FRic),
         pd_FEve = p_direction(FEve),
         pd_FDiv = p_direction(FDiv),
         pd_FDis = p_direction(FDis),
         median_PD = median(PD),
         CI_lo_PD = hdci(PD)[,1],
         CI_hi_PD = hdci(PD)[,2],
         median_MPD = median(MPD),
         CI_lo_MPD = hdci(MPD)[,1],
         CI_hi_MPD = hdci(MPD)[,2],
         median_MNTD = median(MNTD),
         CI_lo_MNTD = hdci(MNTD)[,1],
         CI_hi_MNTD = hdci(MNTD)[,2],
         median_ED = median(ED),
         CI_lo_ED = hdci(ED)[,1],
         CI_hi_ED = hdci(ED)[,2],
         median_EDR = median(EDR),
         CI_lo_EDR = hdci(EDR)[,1],
         CI_hi_EDR = hdci(EDR)[,2],
         median_FRic = median(FRic),
         CI_lo_FRic = hdci(FRic)[,1],
         CI_hi_FRic = hdci(FRic)[,2],
         median_FEve = median(FEve),
         CI_lo_FEve = hdci(FEve)[,1],
         CI_hi_FEve = hdci(FEve)[,2],
         median_FDiv = median(FDiv),
         CI_lo_FDiv = hdci(FDiv)[,1],
         CI_hi_FDiv = hdci(FDiv)[,2],
         median_FDis = median(FDis),
         CI_lo_FDis = hdci(FDis)[,1],
         CI_hi_FDis = hdci(FDis)[,2]) %>%
  select(., pd_PD:CI_hi_FDis) %>%          # change to CI_hi_FDis
  summarise_all(mean)


prob_dir_habs <- bind_rows(PD_FD_pas_diff, PD_FD_hab_diff) %>% 
  mutate(diff = c("Pasture 0.6 vs 0.1", "Pasture vs Forest"))


# Probability of direction plot
ggplot(prob_dir_habs, aes(x = diff)) +
  geom_point(aes(y = pd_PD, color = "PD")) +
  geom_point(aes(y = pd_MPD, color = "MPD")) +
  geom_point(aes(y = pd_MNTD, color = "MNTD")) +
  geom_point(aes(y = pd_ED, color = "ED")) +
  geom_point(aes(y = pd_EDR, color = "EDR")) +
  geom_point(aes(y = pd_FRic, color = "FRic")) +
  geom_point(aes(y = pd_FEve, color = "FEve")) +
  geom_point(aes(y = pd_FDis, color = "FDiv")) +
  geom_point(aes(y = pd_FDiv, color = "FDis")) +  
  geom_hline(yintercept = 0.975) +
  ylab("Probability of direction") +
  xlab("Habitat difference") +
  labs(color="Index") +
  scale_color_viridis(discrete = T)

# medians plot
ggplot(PD_FD_pas_diff) +
  geom_point(aes(y = "PD", x = median_PD)) +
  geom_errorbar(aes(y = "PD", xmin = CI_lo_PD, xmax = CI_hi_PD)) +
  geom_point(aes(y = "MPD", x = median_MPD)) +
  geom_errorbar(aes(y = "MPD", xmin = CI_lo_MPD, xmax = CI_hi_MPD)) +
  geom_point(aes(y = "MNTD", x = median_MNTD)) +
  geom_errorbar(aes(y = "MNTD", xmin = CI_lo_MNTD, xmax = CI_hi_MNTD)) +
  geom_point(aes(y = "ED", x = median_ED)) +
  geom_errorbar(aes(y = "ED", xmin = CI_lo_ED, xmax = CI_hi_ED)) +
  geom_point(aes(y = "EDR", x = median_EDR)) +
  geom_errorbar(aes(y = "EDR", xmin = CI_lo_EDR, xmax = CI_hi_EDR)) +
  geom_point(aes(y = "FRic", x = median_FRic)) +
  geom_errorbar(aes(y = "FRic", xmin = CI_lo_FRic, xmax = CI_hi_FRic)) +
  geom_point(aes(y = "FEve", x = median_FEve)) +
  geom_errorbar(aes(y = "FEve", xmin = CI_lo_FEve, xmax = CI_hi_FEve)) +
  geom_point(aes(y = "FDiv", x = median_FDiv)) +
  geom_errorbar(aes(y = "FDiv", xmin = CI_lo_FDiv, xmax = CI_hi_FDiv)) +
  geom_point(aes(y = "FDis", x = median_FDis)) +
  geom_errorbar(aes(y = "FDis", xmin = CI_lo_FDis, xmax = CI_hi_FDis)) +
  xlab("Standardised median of difference between Pasture 0.6 and pasture 0.1")+
  geom_vline(xintercept = 0, col = "red")

ggplot(PD_FD_hab_diff) +
  geom_point(aes(y = "PD", x = median_PD)) +
  geom_errorbar(aes(y = "PD", xmin = CI_lo_PD, xmax = CI_hi_PD)) +
  geom_point(aes(y = "MPD", x = median_MPD)) +
  geom_errorbar(aes(y = "MPD", xmin = CI_lo_MPD, xmax = CI_hi_MPD)) +
  geom_point(aes(y = "MNTD", x = median_MNTD)) +
  geom_errorbar(aes(y = "MNTD", xmin = CI_lo_MNTD, xmax = CI_hi_MNTD)) +
  geom_point(aes(y = "ED", x = median_ED)) +
  geom_errorbar(aes(y = "ED", xmin = CI_lo_ED, xmax = CI_hi_ED)) +
  geom_point(aes(y = "EDR", x = median_EDR)) +
  geom_errorbar(aes(y = "EDR", xmin = CI_lo_EDR, xmax = CI_hi_EDR)) +
  geom_point(aes(y = "FRic", x = median_FRic)) +
  geom_errorbar(aes(y = "FRic", xmin = CI_lo_FRic, xmax = CI_hi_FRic)) +
  geom_point(aes(y = "FEve", x = median_FEve)) +
  geom_errorbar(aes(y = "FEve", xmin = CI_lo_FEve, xmax = CI_hi_FEve)) +
  geom_point(aes(y = "FDiv", x = median_FDiv)) +
  geom_errorbar(aes(y = "FDiv", xmin = CI_lo_FDiv, xmax = CI_hi_FDiv)) +
  geom_point(aes(y = "FDis", x = median_FDis)) +
  geom_errorbar(aes(y = "FDis", xmin = CI_lo_FDis, xmax = CI_hi_FDis)) +
  xlab("Standardised median of difference between Forest and Pasture 0.6")+
  geom_vline(xintercept = 0, col = "red")


## 5.2 Sharing/sparing

# probability of direction along increasing unit
n_pts <- unique(PD_FD_metrics_diff$n_pt)
prob_dir_diff_low <- c()
for (n in n_pts) {
prob_dir_diff_low[[n]] <- PD_FD_metrics_diff %>% 
  filter(., n_pt == n, Yield == "Low") %>%
  mutate(pd_PD = p_direction(PD),
         pd_MPD = p_direction(MPD),
         pd_MNTD = p_direction(MNTD),
         pd_sesPD = p_direction(sesPD),
         pd_sesMPD = p_direction(sesMPD),
         pd_sesMNTD = p_direction(sesMNTD),
         pd_ED = p_direction(ED),
         pd_EDR = p_direction(EDR),
         pd_FRic = p_direction(FRic),
         pd_FEve = p_direction(FEve),
         pd_FDiv = p_direction(FDiv),
         pd_FDis = p_direction(FDis),
         pd_sesFRic = p_direction(sesFRic),
         pd_sesFEve = p_direction(sesFEve),
         pd_sesFDiv = p_direction(sesFDiv),
         pd_sesFDis = p_direction(sesFDis)) %>% 
  select(., pd_PD:pd_sesFDis) %>% 
  mutate_all(., as.numeric)
}
prob_dir_diff_low <- bind_rows(prob_dir_diff_low, .id = "n_pt") %>% 
  group_by(., n_pt) %>% 
  summarise_all(., mean)


prob_dir_diff_high <- c()
for (n in n_pts) {
  prob_dir_diff_high[[n]] <- PD_FD_metrics_diff %>% 
    filter(., n_pt == n, Yield == "High") %>%
    mutate(pd_PD = p_direction(PD),
           pd_MPD = p_direction(MPD),
           pd_MNTD = p_direction(MNTD),
           pd_sesPD = p_direction(sesPD),
           pd_sesMPD = p_direction(sesMPD),
           pd_sesMNTD = p_direction(sesMNTD),
           pd_ED = p_direction(ED),
           pd_EDR = p_direction(EDR),
           pd_FRic = p_direction(FRic),
           pd_FEve = p_direction(FEve),
           pd_FDiv = p_direction(FDiv),
           pd_FDis = p_direction(FDis),
           pd_sesFRic = p_direction(sesFRic),
           pd_sesFEve = p_direction(sesFEve),
           pd_sesFDiv = p_direction(sesFDiv),
           pd_sesFDis = p_direction(sesFDis)) %>% 
    select(., pd_PD:pd_sesFDis) %>% 
    mutate_all(., as.numeric)
}

prob_dir_diff_high <- bind_rows(prob_dir_diff_high, .id = "n_pt") %>% 
  mutate(., n_pt = as.numeric(n_pt)) %>% 
  group_by(., n_pt) %>% 
  summarise_all(., mean)


# Probability of direction plots
ggplot(prob_dir_diff_low, aes(x = as.numeric(n_pt))) + 
  geom_point(aes(y = pd_PD, color = "PD")) +
  geom_point(aes(y = pd_MPD, color = "MPD")) +
  geom_point(aes(y = pd_MNTD, color = "MNTD")) +
  geom_point(aes(y = pd_ED, color = "ED")) +
  geom_point(aes(y = pd_EDR, color = "EDR")) +
  geom_point(aes(y = pd_FRic, color = "FRic")) +
  geom_point(aes(y = pd_FEve, color = "FEve")) +
  geom_point(aes(y = pd_FDiv, color = "FDiv")) +
  geom_point(aes(y = pd_FDis, color = "FDis")) +
  geom_hline(yintercept = 0.975) +
  ylab("Probability of direction") +
  xlab("Number of management units") +
  ggtitle("Low Yield") +
  labs(color="Index") +
  scale_color_viridis(discrete = T)

# Plots
ggplot(prob_dir_diff_high, aes(x = as.numeric(n_pt))) + 
  geom_point(aes(y = pd_PD, color = "PD")) +
  geom_point(aes(y = pd_MPD, color = "MPD")) +
  geom_point(aes(y = pd_MNTD, color = "MNTD")) +
  geom_point(aes(y = pd_ED, color = "ED")) +
  geom_point(aes(y = pd_EDR, color = "EDR")) +
  geom_point(aes(y = pd_FRic, color = "FRic")) +
  geom_point(aes(y = pd_FEve, color = "FEve")) +
  geom_point(aes(y = pd_FDiv, color = "FDiv")) +
  geom_point(aes(y = pd_FDis, color = "FDis")) +
  geom_hline(yintercept = 0.975) +
  #ylim(0.95,1) +
  ylab("Probability of direction") +
  xlab("Number of management units") +
  ggtitle("High Yield") +
  labs(color="Index") +
  scale_color_viridis(discrete = T)



# Medians
# medians along increasing unit
n_pts <- unique(PD_FD_metrics_diff$n_pt)
medians_diff_low <- c()
for (n in n_pts) {
  medians_diff_low[[n]] <- PD_FD_metrics_diff %>%
    na.omit() %>% 
    filter(., n_pt == n, Yield == "Low") %>%
    select(PD:sesFDis) %>% 
    mutate(median_PD = median(PD),
           CI_lo_PD = hdci(PD)[,1],
           CI_hi_PD = hdci(PD)[,2],
           median_MPD = median(MPD),
           CI_lo_MPD = hdci(MPD)[,1],
           CI_hi_MPD = hdci(MPD)[,2],
           median_MNTD = median(MNTD),
           CI_lo_MNTD = hdci(MNTD)[,1],
           CI_hi_MNTD = hdci(MNTD)[,2],
           median_ED = median(ED),
           CI_lo_ED = hdci(ED)[,1],
           CI_hi_ED = hdci(ED)[,2],
           median_EDR = median(EDR),
           CI_lo_EDR = hdci(EDR)[,1],
           CI_hi_EDR = hdci(EDR)[,2],
           median_FRic = median(FRic),
           CI_lo_FRic = hdci(FRic)[,1],
           CI_hi_FRic = hdci(FRic)[,2],
           median_FEve = median(FEve),
           CI_lo_FEve = hdci(FEve)[,1],
           CI_hi_FEve = hdci(FEve)[,2],
           median_FDiv = median(FDiv),
           CI_lo_FDiv = hdci(FDiv)[,1],
           CI_hi_FDiv = hdci(FDiv)[,2],
           median_FDis = median(FDis),
           CI_lo_FDis = hdci(FDis)[,1],
           CI_hi_FDis = hdci(FDis)[,2]) %>% 
    select(., median_PD:CI_hi_FDis) %>% 
    mutate_all(., as.numeric)
}
medians_diff_low <- bind_rows(medians_diff_low, .id = "n_pt") %>% 
  group_by(., n_pt) %>% 
  summarise_all(., mean) %>% 
  mutate(., n_pt = factor(n_pt, levels = c("10", "50", "100", "150", "200", "250", "300", "350", "400", "450")))

#high
n_pts <- unique(PD_FD_metrics_diff$n_pt)
medians_diff_high <- c()
for (n in n_pts) {
  medians_diff_high[[n]] <- PD_FD_metrics_diff %>%
    na.omit() %>% 
    filter(., n_pt == n, Yield == "High") %>%
    select(PD:sesFDis) %>% 
    mutate(median_PD = median(PD),
           CI_lo_PD = hdci(PD)[,1],
           CI_hi_PD = hdci(PD)[,2],
           median_MPD = median(MPD),
           CI_lo_MPD = hdci(MPD)[,1],
           CI_hi_MPD = hdci(MPD)[,2],
           median_MNTD = median(MNTD),
           CI_lo_MNTD = hdci(MNTD)[,1],
           CI_hi_MNTD = hdci(MNTD)[,2],
           median_ED = median(ED),
           CI_lo_ED = hdci(ED)[,1],
           CI_hi_ED = hdci(ED)[,2],
           median_EDR = median(EDR),
           CI_lo_EDR = hdci(EDR)[,1],
           CI_hi_EDR = hdci(EDR)[,2],
           median_FRic = median(FRic),
           CI_lo_FRic = hdci(FRic)[,1],
           CI_hi_FRic = hdci(FRic)[,2],
           median_FEve = median(FEve),
           CI_lo_FEve = hdci(FEve)[,1],
           CI_hi_FEve = hdci(FEve)[,2],
           median_FDiv = median(FDiv),
           CI_lo_FDiv = hdci(FDiv)[,1],
           CI_hi_FDiv = hdci(FDiv)[,2],
           median_FDis = median(FDis),
           CI_lo_FDis = hdci(FDis)[,1],
           CI_hi_FDis = hdci(FDis)[,2]) %>% 
    select(., median_PD:CI_hi_FDis) %>% 
    mutate_all(., as.numeric)
}
medians_diff_high <- bind_rows(medians_diff_high, .id = "n_pt") %>% 
  group_by(., n_pt) %>% 
  summarise_all(., mean) %>% 
  mutate(., n_pt = factor(n_pt, levels = c("10", "50", "100", "150", "200", "250", "300", "350", "400", "450")))



# Medians plot
# Low
ggplot(medians_diff_low) +
  geom_point(aes(y = "PD", x = median_PD)) +
  geom_errorbar(aes(y = "PD", xmin = CI_lo_PD, xmax = CI_hi_PD)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("Low")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_low) +
  geom_point(aes(y = "MPD", x = median_MPD)) +
  geom_errorbar(aes(y = "MPD", xmin = CI_lo_MPD, xmax = CI_hi_MPD)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("Low")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_low) +
  geom_point(aes(y = "MNTD", x = median_MNTD)) +
  geom_errorbar(aes(y = "MNTD", xmin = CI_lo_MNTD, xmax = CI_hi_MNTD)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("Low")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_low) +
  geom_point(aes(y = "ED", x = median_ED)) +
  geom_errorbar(aes(y = "ED", xmin = CI_lo_ED, xmax = CI_hi_ED)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("Low")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_low) +
  geom_point(aes(y = "EDR", x = median_EDR)) +
  geom_errorbar(aes(y = "EDR", xmin = CI_lo_EDR, xmax = CI_hi_EDR)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("Low")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_low) +
  geom_point(aes(y = "FRic", x = median_FRic)) +
  geom_errorbar(aes(y = "FRic", xmin = CI_lo_FRic, xmax = CI_hi_FRic)) +
  geom_point(aes(y = "FEve", x = median_FEve)) +
  geom_errorbar(aes(y = "FEve", xmin = CI_lo_FEve, xmax = CI_hi_FEve)) +
  geom_point(aes(y = "FDiv", x = median_FDiv)) +
  geom_errorbar(aes(y = "FDiv", xmin = CI_lo_FDiv, xmax = CI_hi_FDiv)) +
  geom_point(aes(y = "FDis", x = median_FDis)) +
  geom_errorbar(aes(y = "FDis", xmin = CI_lo_FDis, xmax = CI_hi_FDis)) + 
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("Low")+
  geom_vline(xintercept = 0, col = "red")


# High
ggplot(medians_diff_high) +
  geom_point(aes(y = "PD", x = median_PD)) +
  geom_errorbar(aes(y = "PD", xmin = CI_lo_PD, xmax = CI_hi_PD)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("High")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_high) +
  geom_point(aes(y = "MPD", x = median_MPD)) +
  geom_errorbar(aes(y = "MPD", xmin = CI_lo_MPD, xmax = CI_hi_MPD)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("High")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_high) +
  geom_point(aes(y = "MNTD", x = median_MNTD)) +
  geom_errorbar(aes(y = "MNTD", xmin = CI_lo_MNTD, xmax = CI_hi_MNTD)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("High")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_high) +
  geom_point(aes(y = "ED", x = median_ED)) +
  geom_errorbar(aes(y = "ED", xmin = CI_lo_ED, xmax = CI_hi_ED)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("High")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_high) +
  geom_point(aes(y = "EDR", x = median_EDR)) +
  geom_errorbar(aes(y = "EDR", xmin = CI_lo_EDR, xmax = CI_hi_EDR)) +
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("High")+
  geom_vline(xintercept = 0, col = "red")

ggplot(medians_diff_high) +
  geom_point(aes(y = "FRic", x = median_FRic)) +
  geom_errorbar(aes(y = "FRic", xmin = CI_lo_FRic, xmax = CI_hi_FRic)) +
  geom_point(aes(y = "FEve", x = median_FEve)) +
  geom_errorbar(aes(y = "FEve", xmin = CI_lo_FEve, xmax = CI_hi_FEve)) +
  geom_point(aes(y = "FDiv", x = median_FDiv)) +
  geom_errorbar(aes(y = "FDiv", xmin = CI_lo_FDiv, xmax = CI_hi_FDiv)) +
  geom_point(aes(y = "FDis", x = median_FDis)) +
  geom_errorbar(aes(y = "FDis", xmin = CI_lo_FDis, xmax = CI_hi_FDis)) + 
  facet_wrap(~n_pt) +
  xlab("Median of difference Sparing - sharing") +
  ggtitle("High")+
  geom_vline(xintercept = 0, col = "red")
