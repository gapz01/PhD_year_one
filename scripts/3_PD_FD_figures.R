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
library(patchwork)
library(ggpubr)

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

