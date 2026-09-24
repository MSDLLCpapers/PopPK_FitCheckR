# PopPK FitCheckR

PopPK FitCheckR is an R package providing a Shiny application for interactive goodness-of-fit diagnostics and correlation plots in population pharmacokinetic (popPK) modeling, built to work directly with NONMEM output tables.

## Install

Install the runtime dependencies, then install the package from GitHub:

```r
install.packages(c(
  "bslib", "dplyr", "ggplot2", "ggpmisc", "gridExtra",
  "plotly", "rlang", "scales", "shiny", "stringdist"
))
install.packages("remotes")
remotes::install_github("MSDLLCpapers/PopPK_FitCheckR")
```

## Run

```r
PopPKFitCheckR::run_app()
```

## Input files

The GOF tab accepts a NONMEM output table containing `DV`, `PRED`, `IPRED`, `CWRES`, `TIME`, and `ID` (e.g. an `sdtab`). The correlation tab accepts a parameter or covariate table (e.g. a `patab` or `cotab`). Both tabs read whitespace-delimited NONMEM tables, with or without the `TABLE NO.` banner, as well as `.csv` files.
