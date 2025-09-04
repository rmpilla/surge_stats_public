# surge_stats
Code for analyzing relationships between carbon gases and environmental drivers using data from EPA's SuRGE project.

This project uses `renv` in R 4.5.0 for library management. Please run `renv::restore` on your local clone or fork to populate the environment with the required packages and version.

The computational workflow is controlled by the `targets` package and is defined in the _targets.R file. Run `targets::vis_network` to generate an overview of the `targets` pipeline. `targets::tar_make` will ensure the targets are up to date. See the `targets` user manual for details.
