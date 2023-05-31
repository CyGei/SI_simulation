# SETUP -------------------------------------------------------------------
# devtools::install_github("CyGei/simulacr")
# devtools::install_github("CyGei/simulacr", ref = "deterministic")
library(simulacr)
library(outbreaker2)
library(furrr)
library(tidyverse)
source("R/params.R")
dir <- paste0("Data/", "2023-05-30", "/")
dir.create(path = dir)

# SIMULATIONS ------------------------------------------------------------------
# simulacr::simulate_outbreak() simulates the true transmission trees 

n_cores <- parallel::detectCores() - 1
future::plan("multisession", workers = n_cores)

set.seed(123)
sim_list <- list()
n_sims <- 0
max_sims <- 100

while (n_sims < max_sims) {
  new_sim <- furrr::future_map(
    .x = seq_len(max_sims - n_sims),
    ~ simulacr::simulate_outbreak(
      duration = 100,
      population_size = sample(2:6, 1),
      R = 3,
      dist_incubation = incubation,
      dist_generation_time = generation_time
    )$data %>% 
      simulacr::get_si() %>% 
      simulacr::get_gt(),
    .options = furrr_options(seed = TRUE)
  )
  
  new_sim <- new_sim[sapply(new_sim, function(df) nrow(df) > 1)]
  n_new_sims <- length(new_sim)
  
  sim_list <- c(sim_list, new_sim)
  n_sims <- n_sims + n_new_sims
  
  if (n_sims >= max_sims) {
    break
  }
}
saveRDS(sim_list, file=paste0(dir, "sim_list.RData"))

# group by trees
sim_si <- 
  bind_rows(sim_list, .id = "household")

sim_mean_si <- tibble(
  mean = mean(sim_si$si, na.rm = TRUE),
  lwr = quantile(sim_si$si, probs = 0.025, na.rm = TRUE),
  upr = quantile(sim_si$si, probs = 0.975, na.rm = TRUE) 
)



# OUTBREAKER2 --------------------------------------------------------------
# outbreaker2 attempts to reconstruct the true transmission tree

source("R/run_o2.R")
o2 <- run_o2(sim_list = sim_list,
       n_LHS = 100)
simulationA_data <- bind_rows(o2)

saveRDS(simulationA_data, file=paste0(dir, "simulationA_data.RData"))




o2_mean_si <- simulationA_data %>% 
  group_by(LHS) %>% 
  summarise(serial_interval = list(serial_interval)) %>% 
  mutate(mean_si = map(serial_interval, mean, na.rm = TRUE )) %>% 
  select(mean_si) %>%
  unnest(cols = c(mean_si)) 


o2_cri <- o2_mean_si %>% summarise(mean = mean(mean_si),
                                   lwr = quantile(mean_si, 0.025),
                                   upr = quantile(mean_si, 0.975))


# PLOT --------------------------------------------------------------------
pA <- 
  ggplot() +
  #outbreaker2
  geom_density(data = o2_mean_si,
               aes(x = mean_si,
                   fill = "outbreaker2",
                   color = "outbreaker2"),
               alpha = 0.5) +
  geom_errorbarh(
    data = o2_cri,
    aes(xmin = lwr,
        xmax = upr,
        y = 0.6,
        color = "outbreaker2"),
    linewidth = 1,
    height = 0.2
  ) +
  geom_point(data = o2_cri,
             aes(x = mean, y = 0.6,
                 fill = "outbreaker2",
                 color = "outbreaker2"),
             size = 4) +
  
  #simulacr
  geom_vline(data = sim_mean_si,
             aes(xintercept = mean,
                 color = "truth"),
             lty = "dashed",
             size = 1) +
  theme_bw()+
  scale_fill_manual(values = c("#e0218a", "purple"))+
  scale_colour_manual(values = c("#e0218a", "purple"))+
  scale_x_continuous(breaks = seq(3,7, 0.5), limits = c(3,7))+
  guides(fill = "none")+
  labs(x = "Mean Serial Interval",
       y = "Density",
       colour = "Method")


