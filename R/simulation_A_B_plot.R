library(simulacr)
library(outbreaker2)
library(furrr)
library(tidyverse)
source("R/params.R")
dir <- paste0("Data/", "2023-05-30", "/")
dir.create(path = dir)

#simulacr simulations
sim_list <- readRDS(paste0(dir, "sim_list.RData"))
sim_si <- bind_rows(sim_list, .id = "household")

sim_mean_si <- tibble(
  mean = mean(sim_si$si, na.rm = TRUE),
  lwr = quantile(sim_si$si, probs = 0.025, na.rm = TRUE),
  upr = quantile(sim_si$si, probs = 0.975, na.rm = TRUE) 
)


#misspecified reconstructions: simulationB
offset_data <- readRDS(paste0(dir, "simulationB_offset_data.RData"))

#no misspecified (NULL): simulationA
o2_mean_si <- readRDS(paste0(dir, "simulationA_data.RData")) %>% 
  group_by(LHS) %>% 
  summarise(serial_interval = list(serial_interval)) %>% 
  mutate(mean_si = map(serial_interval, mean, na.rm = TRUE )) %>% 
  select(mean_si) %>%
  unnest(cols = c(mean_si)) 
o2_cri <- o2_mean_si %>% summarise(mean = mean(mean_si),
                                   lwr = quantile(mean_si, 0.025),
                                   upr = quantile(mean_si, 0.975))


# OVERALL RESULTS ---------------------------------------------------------

offset_mean_si <- offset_data %>% 
  group_by(parameter, moment, LHS) %>% 
  summarise(serial_interval = list(serial_interval)) %>% 
  mutate(mean_si = map(serial_interval, mean, na.rm = TRUE )) %>% 
  select(mean_si) %>%
  unnest(cols = c(mean_si)) %>% 
  bind_rows(o2_mean_si %>% mutate(parameter = NULL, moment = NULL)) 


offset_cri <- offset_mean_si %>% summarise(mean = mean(mean_si),
                                           lwr = quantile(mean_si, 0.025),
                                           upr = quantile(mean_si, 0.975)) %>% 
  bind_rows(o2_cri %>% mutate(parameter = NULL, moment = NULL)) %>% 
  mutate(parameter = case_when(parameter == "gt" ~ "generation time",
                               parameter == "incub" ~ "incubation period"))

offset_mean_si <- offset_mean_si %>% 
  mutate(parameter = case_when(parameter == "gt" ~ "generation time",
                               parameter == "incub" ~ "incubation period"))

p <- 
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
  geom_vline(data = offset_mean_si %>% mutate(truth = sim_mean_si$mean),
             aes(xintercept = truth,
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


p
