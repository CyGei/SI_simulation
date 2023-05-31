# PARAMS ------------------------------------------------------------------
#Construct the probability mass functions from values obtained in the literature

#incubation Period:
#see lit_search.R
#Quesada et.al, 2021 review
incub_mu <- c(5.2, 3.95, 4.5, 3.2, 4.2)
incub_sd <- c(1.72, 1.51, 3.4, 2.5, 4.9)

#Generation Time:
#Gayani et al 2020 1st 2 values
#S Hart 2022, 3rd,4th values
#Hart et.al 2022 last value
gt_mu <- c(6.5, 5, 5.6, 6.7, 4.9)
gt_sd <- c(2.6, 3, 2.8, 5.2, 2.2)

incubation <- simulacr::make_disc_gamma(mean = mean(incub_mu), sd = mean(incub_sd))
generation_time <-  simulacr::make_disc_gamma(mean = mean(gt_mu), sd = mean(gt_sd))


#x is a distcrete object
pmf_to_mu <- function(x) sum(x$d(0:100)* (0:100)) 
pmf_to_sd <- function(x) sqrt(
  sum(x$d(0:100) * (0:100)^2 ) - pmf_to_mu(x)^2
)
