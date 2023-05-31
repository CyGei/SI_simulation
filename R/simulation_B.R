#THE below is very long to run, use the provided data instead
# SETUP -------------------------------------------------------------------
#devtools::install_github("CyGei/simulacr")
library(simulacr)
library(outbreaker2)
library(furrr)
library(tidyverse)
source("R/params.R")
dir <- paste0("Data/", "2023-05-30", "/")
dir.create(path = dir)

sim_list <- readRDS(paste0(dir, "sim_list.RData"))
n_LHS = 100

# OFFSET ------------------------------------------------------------------

# how reliable are the outbreaker2 estimates when 
# the natural histories’ moments were misspecified

off_gt_mu2 <- run_o2(sim_list = sim_list,
                    n_LHS = n_LHS,
                    off_gt_mu = 2) %>%
  bind_rows()

# off_incub_mu2 <- run_o2(sim_list = sim_list,
#                        n_LHS = n_LHS,
#                        off_incub_mu = 2) %>%
#   bind_rows()
# 
# off_gt_mu1_2 <- run_o2(sim_list = sim_list,
#                      n_LHS = n_LHS,
#                      off_gt_mu = 0.5) %>%
#   bind_rows()

off_incub_mu1_2 <- run_o2(sim_list = sim_list,
                        n_LHS = n_LHS,
                        off_incub_mu = 0.5) %>%
  bind_rows()


off_gt_mu4 <- run_o2(sim_list = sim_list,
                     n_LHS = n_LHS,
                     off_gt_mu = 4) %>%
  bind_rows()

off_incub_mu4 <- run_o2(sim_list = sim_list,
                        n_LHS = n_LHS,
                        off_incub_mu = 4) %>%
  bind_rows()


off_gt_sd2 <- run_o2(sim_list = sim_list,
                    n_LHS = n_LHS,
                    off_gt_sd = 2) %>%
  bind_rows()

off_incub_sd2 <- run_o2(sim_list = sim_list,
                       n_LHS = n_LHS,
                       off_incub_sd = 2) %>%
  bind_rows()

off_gt_sd4 <- run_o2(sim_list = sim_list,
                     n_LHS = n_LHS,
                     off_gt_sd = 4) %>%
  bind_rows()

off_incub_sd4 <- run_o2(sim_list = sim_list,
                       n_LHS = n_LHS,
                       off_incub_sd = 4) %>%
  bind_rows()


offset_data <- data.table::rbindlist(list(
  off_gt_mu2 = off_gt_mu2,
  off_incub_mu2 = off_incub_mu2,
  # off_gt_mu1_2 = off_gt_mu1_2,
  # off_incub_mu1_2 = off_incub_mu1_2,
  off_gt_mu4 = off_gt_mu4,
  off_incub_mu4 = off_incub_mu4,
  off_gt_sd2 = off_gt_sd2,
  off_incub_sd2 = off_incub_sd2,
  off_gt_sd4 = off_gt_sd4,
  off_incub_sd4 = off_incub_sd4
), idcol = "offset") %>% 
  mutate(offset = sub("^off_", "", offset)) %>% 
  separate(offset, into = c("parameter", "moment"), sep = "_", remove = FALSE)

saveRDS(offset_data, file=paste0(dir, "simulationB_offset_data.RData"))


offset_mean_si <- offset_data %>% 
  group_by(parameter, moment, LHS) %>% 
  summarise(serial_interval = list(serial_interval)) %>% 
  mutate(mean_si = map(serial_interval, mean, na.rm = TRUE )) %>% 
  select(mean_si) %>%
  unnest(cols = c(mean_si)) 


offset_cri <- offset_mean_si %>% summarise(mean = mean(mean_si),
                                   lwr = quantile(mean_si, 0.025),
                                   upr = quantile(mean_si, 0.975))



pB <- 
  ggplot() +
  #outbreaker2
  geom_density(data = offset_mean_si,
               aes(x = mean_si,
                   fill = "outbreaker2",
                   color = "outbreaker2"),
               alpha = 0.5) +
  geom_errorbarh(
    data = offset_cri,
    aes(xmin = lwr,
        xmax = upr,
        y = 0.4,
        color = "outbreaker2"),
    linewidth = 1,
    height = 0.2
  ) +
  geom_point(data = offset_cri,
             aes(x = mean, y = 0.4,
                 fill = "outbreaker2",
                 color = "outbreaker2"),
             size = 4) +
  
  facet_grid(moment~parameter,
             scales = "free_y")+

  
  #simulacr
  geom_vline(data = sim_mean_si,
             aes(xintercept = mean,
                 color = "truth"),
             lty = "solid",
             size = 0.8) +
  
  theme_bw()+
  scale_fill_manual(values = c("#e0218a", "purple"))+
  scale_colour_manual(values = c("#e0218a", "purple"))+
  scale_x_continuous(breaks = seq(3,10, 1), limits = c(3,10))+
  guides(fill = "none")+
  labs(x = "Mean Serial Interval",
       y = "Density",
       colour = "Method")+
  theme(legend.position = "top")
# +
#   theme(
#     panel.spacing = unit(0, "lines"),
#     strip.placement = "outside",
#     #strip.background = element_blank(),
#     strip.text.x = element_text(size = 11, 
#                                 family = "Open Sans",
#                                 face = "bold", 
#                                 color = "black", 
#                                 angle = 0, 
#                                 vjust = 0.5, 
#                                 hjust = 0.5),
#     strip.text.y = element_text(size = 10, 
#                                 family = "Open Sans",
#                                 face = "bold", 
#                                 color = "black"),
#     panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#     panel.background = element_rect(fill = NA, color = NA),
#     legend.position = "top",
#     plot.margin = unit(c(1,1,1,3), "lines")
#   )


pB




