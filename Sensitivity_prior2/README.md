## Optimal solution for K=2 subgroups

This folder contains code to obtain the optimal solution to the problem described in Section 5 of the main manuscript and to plot Figure S1 of the Supplementary Material.


Specifically:

* The file `1.generate_data_prior.R` contains code to generate datasets from the prior model. This file use the function `generate_data_from_prior2.R` which contains a variation of prior model used in the paper. This variation is described in the caption of Supplementary Figure S3 and Table S4.

* The file `2.compute_utility_components.R` contains the code to compute the utility function.

* The file `3.expected_utility_optimization.R` contains code to compute the optimal solution and plot Figure S3 of the Supplementary Material.

* The file `4.auxiliary_augmented.R` compute the auxiliary augmented procedure on the trial generated from the model described in section 5 of the main manuscript.

* The file `5.sensitivity_table.R` create the Supplementary Table S4.
