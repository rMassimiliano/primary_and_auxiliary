## Compute results for the sequential design

This folder contains the code to reproduce the results of the example illustrated in Section 6 of the main manuscript 


* The R script `0.compile_stan_model.R` compile the prediction model used for futility stopping. The compiled models are saved in `.rds` in the folder `clean_code_stan`.

* The R script starting with `1.number.` contains code to generate data according to the model described in Section 5 and 6. Data are saved in the folder `data/`. An instance of such generation is already included in the folder.


* The R script starting with `2.number` contains code to generate results for our method and the competitors. Specifically:

  - `2.1.auxiliary_augmented.R` contains the code to replicated the results for our Auxilary-Augmented design . Results are saved in the folder `results/auxiliary_augmented/`. 

  - `2.2.primary_only.R`  contains the code to replicated the results for the primary only design. Results are saved in the folder `results/primary_only/`. 

  - `2.3.auxiliary_only.R` contains the code to replicated the results for the Auxilary-Only procedure. Results are saved in the folder `results/auxiliary_only/`. 


* The script `3.summarizeResults.R` summarize the results and produce Table 1 of the manuscript (file `table_sec6.tex`).


