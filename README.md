Project Title: MLS 2024 Center Forward Scouting Analysis

Repository Structure:
"scripts/": Contains the R scripts for data processing, Signal to Noise ratio and Stability plotting, and final ranking.
"data/": (Local Only) Contains raw CSV data from Wyscout and SkillCorner.

Setup:
1. Clone this repository.
2. Ensure you have an R Project (".Rproj") open in the root directory.
3. Place the raw data files in a folder named "data".
4. Run "scripts/calculating_signal_to_noise_ratio_and_stability.R" and "scripts/calculating_composite_score.R".

Installation:
The analysis requires the following R packages: tidyverse and lme4. 
You can install them by running install.packages(c("tidyverse", "lme4")).