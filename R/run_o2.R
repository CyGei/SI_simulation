
# Helper function to retreive the SI from the outbreaker2 serial interval
get_o2_si <- function(outbreaker_result, input_data, date_col){
  n_cases <- sum(str_count(colnames(outbreaker_result), "alpha"))
  df <- outbreaker_result %>% 
    select(step, dplyr::starts_with("alpha")) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = -step, names_to = "alpha", values_to = "infector") %>% 
    mutate(infectee = readr::parse_number(alpha),
           onset_infectee = input_data[[date_col]][infectee],
           onset_infector = input_data[[date_col]][infector],
           serial_interval = as.integer(onset_infectee - onset_infector)) %>%  # mcmc_run = rep(row_number(), length.out = n(), each = n_cases)
    select(-alpha)
  return(df)
  
}


# wrapper to run outbreaker2 with using a LHS method
# sim_list refers to the input data i.e. a tibble with a case ID and sympton onset date column
# or the simulacr::simulate_outbreak()$data's output
# n_LHS = the number of LHS distributions to generate
# off... : the specific offset factor for any parameter of the LHS.
# returns a list of outbreaker2 results per household & LHS ID.
run_o2 <- function(sim_list,
                   n_LHS,
                   off_gt_mu = 1,
                   off_gt_sd = 1,
                   off_incub_mu = 1,
                   off_incub_sd = 1){
  
  source("R/params.R")
  source("R/LHS.R")
  n_LHS = n_LHS
  
  #set.seed(987)
  generation_time <- get_lhs(
    MUs = gt_mu*off_gt_mu,
    SDs = gt_sd*off_gt_sd,
    n = n_LHS
  )
  #set.seed(987)
  incubation <- get_lhs(
    MUs = incub_mu*off_incub_mu,
    SDs = incub_sd*off_incub_sd,
    n = n_LHS
  )
  
  gt_LHS <- map2(.x = generation_time$mu, .y = generation_time$mu * generation_time$cv,
                 ~simulacr::make_disc_gamma(mean = .x, sd = .y)$d(0:50) %>% 
                   tails_pmf() %>% 
                   sanitize_pmf())
  
  incub_LHS <- map2(.x = incubation$mu, .y = incubation$mu * incubation$cv,
                    ~simulacr::make_disc_gamma(mean = .x, sd = .y)$d(0:50) %>%
                      tails_pmf() %>% 
                      sanitize_pmf())
  
  
  
  config <- outbreaker2::create_config(
    move_kappa = FALSE,# do not look for missing cases
    move_pi = FALSE,  # reporting rate
    move_mu = FALSE, # mutation rate
    init_kappa = 1,# number of generations before the last sampled ancestor
    init_pi = 1, # 100% of reporting rate = all cases are reported
    find_import = TRUE,# imported cases,
    outlier_threshold = 2,
    init_tree = "star")
  
  
  future::plan(multisession, workers =  parallel::detectCores() - 1)
  
  #set.seed(987)
  out_list <- purrr::map(1:length(sim_list), function(i) {
    #cat(round(i/n_sims, digits = 2)*100,"%", "... ")
    df <- sim_list[[i]]
    lhs_list <- furrr::future_map(1:n_LHS, function(j) {
      out <- outbreaker2::outbreaker(
        data = list(
          ids = df$ids,
          dates = df$date_onset,
          w_dens = gt_LHS[[j]],
          f_dens = incub_LHS[[j]]
        ),
        config = config
      )
      out <- get_o2_si(as_tibble(out), df, "date_onset") %>%
        filter(step > 500) %>%
        mutate(LHS = j,
               household = i) %>%
        relocate(c(household, LHS), .before = step)
      
      return(out)
    }, .options = furrr_options(seed = TRUE),
    .progress = FALSE)
    
    return(lhs_list)
  })
  
  return(out_list)
  
}
