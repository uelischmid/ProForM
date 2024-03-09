# PoForM
ProForM (Protection Forest Management) is a dynamic simulation model for studying the impacts of management on the protective effect in temperate mountain forest stands.

ProForM was developed by Ueli Schmid, Monika Frehner, and Harald Bugmann at ETH Zurich.  
ProFroM v 1.0 is described in [Schmid et al. 2023](https://doi.org/10.1016/j.ecolmodel.2023.110297). A complete model description is available as supplementary material to the article.  
The current version 1.1 was released in March 2024 and will be described in Schmid (2024). A description of the model changes will be available as supplementary material to the third chapter of the thesis.
[![DOI](https://zenodo.org/badge/572127011.svg)](https://zenodo.org/badge/latestdoi/572127011)  

The model may be freely used under the terms of the "GNU GENERAL PUBLIC LICENSE v3" license.
We offer no warranty and cannot be held liable for any error in our software or information.

## Installation
ProFroM is implemented in R and is available as a project for RStudio.  

### renv
Packages are versioned with `renv`.
When opening the project for the first time, activate it with `renv::activate()`. Then, restore the library with `renv::restore()`.


## Running the model
The follwoing instructions run sample simulations and assessments of the protective quality, including visual outputs.

### Simulation of forest dynamics
Source the file `code/0_run_model/1_simulation/simulation_batch_runs_examples.R`. Simulation output will be saved to
`data/processed/simoutput/`.

### Visualize simulation results
Source the file `code/0_run_model/3_visualization/1_sim/visualization_1_sim_batch_runs_example.R` to visualize
individual simulations and `code/0_run_model/3_visualization/1_sim/visualization_n_sim_batch_runs_example.R` to visually
compare multiple simulations. The graphs will be saved to
`results/sim_vis/1_sim/` and `results/sim_vis/n_sim/` respectively.

### Assessment of protective quality and visualization
Source the file `code/0_run_model/2_nais_assessment/nais_prep_assess_vis_batch_runs_examples.R`. Assessment output will be saved to
`data/processed/naisoutput/1_prep`, `data/processed/naisoutput/2_assess`, `results/nais_vis/1_sim_F2/` and `results/nais_vis/1_sim_subind/`.


## Running simulations used for third chapter of Schmid (2024)
Source the files under `code/0_run_model/c3` in order. This might take a while.