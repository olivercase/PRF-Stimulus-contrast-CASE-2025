# PRF-Stimulus-contrast-CASE-2025

## Abstract

Visual cortex exhibits remarkable plasticity, with single neurons in non-human primates expanding their receptive fields under low-contrast conditions. However, whether this adaptive mechanism operates at population scales in humans remains unknown. Using functional magnetic resonance imaging and population receptive field modelling, we measured spatial integration properties across early visual areas (V1, V2, V3) under systematically varied stimulus contrast (3%, 17%, 100%). We found that the relationship between population receptive field size and eccentricity followed a non-monotonic function: spatial integration was maximal at low contrast, minimal at medium contrast, and intermediate at high contrast. Critically, we observed differential contrast sensitivity across visual areas, with the canonical hierarchical organization (V1 < V2 < V3) most pronounced at medium contrast but compressed at extreme contrast levels. These effects were driven primarily by peripheral rather than foveal modulation, suggesting a mechanism that preserves high-acuity central processing while flexibly adjusting spatial integration in the periphery. Our findings provide the first population-level evidence in humans for contrast-dependent spatial plasticity, revealing that cortical processing strategies are dynamically optimized according to stimulus quality; a principle with implications for understanding adaptive visual coding.

## Graphical Abstract

![Figure 1](https://github.com/olivercase/PRF-Stimulus-contrast-CASE-2025/blob/main/Figures/Figure%201.png?raw=true)

## Project Structure

The repository is organized into three main directories:

-   `Data/`: Contains the raw data used for the analysis.
    -   `pRF_analysis_master_table.csv`: The primary data file.
-   `Analysis/`: Contains the R scripts for the analysis and figure generation.
    -   `run_full_bayesian_analysis.R`: The main script to run the Bayesian hierarchical models.
    -   `generate_publication_figures.R`: A script to generate the publication-quality figures from the model outputs.
    -   `full_bayesian_analysis_output/`: This directory is created by `run_full_bayesian_analysis.R` and contains all the outputs of the models, including summaries, diagnostic plots, and full posterior data.
-   `Figures/`: Contains the main figure from the paper.

## How to Use

To reproduce the analysis and figures, follow these steps:

1.  **Prerequisites**: Make sure you have R and the following R packages installed: `tidyverse`, `rstanarm`, `here`, `patchwork`, `ggdist`, and `tidybayes`.

2.  **Run the Analysis**:
    -   Open R or RStudio.
    -   Set the working directory to the `Master` directory.
    -   Run the `run_full_bayesian_analysis.R` script. This will perform the full Bayesian analysis and save the results in the `Analysis/full_bayesian_analysis_output/` directory.

    ```R
    source("Analysis/run_full_bayesian_analysis.R")
    ```

3.  **Generate Figures**:
    -   After the analysis is complete, run the `generate_publication_figures.R` script to create the figures. The figures will be saved in the `Master` directory.

    ```R
    source("Analysis/generate_publication_figures.R")
    ``` 