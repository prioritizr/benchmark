
<!--- README.md is generated from README.Rmd. Please edit that file -->
Benchmark solvers
=================

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

**This project is still being developed. It does not work yet. Stay tuned!**

Summary
-------

This repository contains code for benchmarking the performance of exact algorithm solvers for conservation planning problems (see [Mittelmann 2021](http://plato.asu.edu/bench.html) for general purpose benchmarks). Specifically, these benchmarks examine the performance of the [*Gurobi*](https://www.gurobi.com/) and [*IBM CPLEX*](https://www.ibm.com/analytics/cplex-optimizer) commercial solvers. They also examine the [*CBC* (OIN-OR branch and cut)](https://projects.coin-or.org/Cbc) and [*SYMPHONY*](https://prioritizr.net/reference/add_rsymphony_solver.html) (via the *Rsymphony* and *lpsymphony R* packages) open source solvers. The benchmarks are performed using a realistic conservation planning dataset, involving 72 species and costs derived from land valuation data (see [Schuster *et al.* 2020](https://doi.org/10.7717/peerj.9258)). To investigate how well different solvers scale with different sized datasets, these benchmarks examine different versions of the dataset wherein planning units are resampled to different resolutions. The [results of benchmark analysis](https://prioritizr.net/) are reported in the [Benchmark vignette](https://prioritizr.net/articles/benchmark.html) of the [*prioritizr R* package](https://prioritizr.net/).

Usage
-----

After downloading (cloning) this repository, you can rerun the analysis on your own computer using the system command `make clean all`. **Please ensure that the `MODE` variable in the *Makefile* is set to `release` to obtain correct results.** Project maintainers can then use the `make export` command to upload the results files (`results/results.rda` and `results/solutions.tif`) to the [Results storage release](https://github.com/prioritizr/benchmark/releases/tag/v0.0.2).

Repository structure
--------------------

This repository is organized as follows: \* *data* + *raw*: raw data used to run the analysis (see [Data storage release](https://github.com/prioritizr/benchmark/releases/tag/v0.0.1) for large files). + *intermediate*: intermediate data generated during analysis. + *final*: final data created after completing the analysis. \* *code* + *parameters*: settings for running the analyses in [TOML format](https://github.com/toml-lang/toml). + [*R*](www.r-project.org): code used to run the analysis. \* *results* + `results.rda`: Rdata file containing the results. + `solutions.tif` GeoTIFF files containing solutions form benchmark runs. \* *packrat* + [*R* package management](https://rstudio.github.io/packrat/).

The files in the *results* directory are created after completing benchmark analysis. If you wish to access the results completed on our systems, please see files attached the [Results storage release](https://github.com/prioritizr/benchmark/releases/tag/v0.0.2).

### Software required

-   Software
-   *GNU make* (version 4.1+)
-   *GDAL* (version 2.2+)
-   [*Gurobi* (academic licenses are available for no cost)](http://www.gurobi.com/)
-   [*IBM CPLEX* (academic licenses are available for no cost)](https://www.ibm.com/analytics/cplex-optimizer)
-   [R (version 4.0.3+)](https://www.r-project.org)
-   System packages:
-   gdal-bin
-   libjq-dev
-   libprotobuf-dev
-   protobuf-compiler
-   libudunits2-dev
-   libgdal-dev
-   libgeos-dev
-   libproj-dev
-   libv8-dev
-   libxml2-dev
-   libssl-dev
-   libgit2-dev

References
----------

[Schuster R, Hanson JO, Strimas-Mackey M and Bennett JR (2020) Exact integer linear programming solvers outperform simulated annealing for solving conservation planning problems. *PeerJ*, **8**: e9258.](https://doi.org/10.7717/peerj.9258)

Mittelmann H (2021) Benchmarks for optimization software. <http://plato.asu.edu/bench.html> \[accessed February 2021\].
