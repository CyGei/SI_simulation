# SI_simulation

***Simulation Study for Estimating the Serial Interval: Appendix B***

**Title**: Reliability Assessment of Outbreaker2 for Serial Interval Estimation

**Abstract**:
This GitHub repository presents a simulation study conducted to evaluate the reliability of the outbreaker2 model in estimating the serial interval. The study utilises simulated data generated by the [simulacr](https://github.com/CyGei/simulacr) R package. The repository provides a reproducible example of the methods implemented in the [serial_interval](https://github.com/CyGei/serial_interval) repository.

Instructions for Reproducing Results (Appendix B):

1.  Execute the following files:

    a\. `R/simulation_A.R`: Compares the mean serial interval obtained from simulated data (simulacr) with the reconstructed serial interval (outbreaker2) using the true natural history moments.

    b\. `R/simulation_A.R`: Compares the mean serial interval obtained from simulated data (simulacr) with the reconstructed serial interval (outbreaker2) under the mispecification of natural history parameters.

    c\. `R/simulation_A_B_plot.R`: Generates a plot illustrating the results presented in Appendix B.

Please refer to the respective files for detailed code implementation.
