## Compute results for K=2 subgroups 

This folder contains the code to reproduce the results of the example illustrated in Section 5 of the main manuscript 


* The R script starting with `1.number.` contains code to generate data according to the model described in Section 5. Data are saved in the folder `data/`. An instance of such generation is already included in the folder.


* The R script starting with `2.number` contains code to generate results for our method and the competitors. Specifically:

  - `2.1.auxiliary_augmented.R` contains the code to replicated the results for our Auxilary-Augmented procedure. Results are saved in the file `A_auxiliary_results.rdata`.

  - `2.2.auxiliary_augmented_b.R` contains the code to replicated the results for our Auxilary-Augmented-B procedure described in Section 5.2 of the manuscript. Results are saved in the file `B_auxiliary_results.rdata`.

  - `2.3.bonferroni.R` contains the code to replicated the results for the Bonferroni procedure. Results are saved in the file `bonferroni_results.rdata`.

  - `2.4.holm.R` contains the code to replicated the results for the Holm procedure.
Results are saved in the file `holm_results.rdata`.

  - `2.5.FAB.R` contains the code to replicated the results for the FAB procedure. Results are saved in the file `FAB_results.rdata`.

  - `2.6.auxiliary_only.R` contains the code to replicated the results for the Auxilary-Only procedure. Results are saved in the file `surrogate_results.rdata`

* The script `3.summarizeResults.R` summarize the results and produce Table 1 of the manuscript (file `table.tex`).


* The script `4.compute_FWER.R` compute the FWER for the null scenarios.
