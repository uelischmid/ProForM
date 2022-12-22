# PoForM
ProForM (Protection Forest Management) is a dynamic simulation model for studying the impacts of management on the protective effect in temperate mountain forest stands.

ProForM was developed by Ueli Schmid, Monika Frehner, and Harald Bugmann at ETH Zurich.  
The current version 1.0 was released in December 2022.  
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

### Assessment of protective quality
Source the file `code/0_run_model/2_nais_assessment/nais_assessment_batch_runs_examples.R`. Assessment output will be saved to
`data/processed/naisoutput/`.

### Visualize simulation results
Source the file `code/0_run_model/3_visualization/1_sim/visualization_1_sim_batch_runs_example.R` to visualize
individual simulations and `code/0_run_model/3_visualization/1_sim/visualization_n_sim_batch_runs_example.R` to visually
compare multiple simulations. The graphs will be saved to
`results/sim_vis/1_sim/` and `results/sim_vis/n_sim/` respectively.

## Visualize assessments of protective quality
Source the file `code/0_run_model/3_visualization/2_nais/visualization_1_nais_batch_runs_example.R` to visualize
individual assessments and `code/0_run_model/3_visualization/2_nais/visualization_n_nais_batch_runs_example.R` to visually
compare multiple assessments. The graphs will be saved to
`results/nais_vis/1_sim/` and `results/nais_vis/n_sim/` respectively.

