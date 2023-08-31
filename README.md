
<!-- README.md is generated from README.qmd. Please edit that file -->

# soils <a href="https://wa-department-of-agriculture.github.io/soils/"><img src="man/figures/logo.svg" data-align="right" height="138" /></a>

<!-- badges: start -->
<!-- badges: end -->

## Disclaimer

This repository and package are a **work in progress**.

## Overview

As part of the [Washington Soil Health
Initiative](https://washingtonsoilhealthinitiative.com/), [Washington
State Department of
Agriculture](https://agr.wa.gov/departments/land-and-water/natural-resources/soil-health)
and [Washington State University](https://soilhealth.wsu.edu/) developed
**`{soils}`** for soil health data visualization and reporting.

## Installation

Install the development version of `soils` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("WA-Department-of-Agriculture/soils")
```

## Functions

## Reports

This package can generate static `.docx` reports and interactive `.html`
reports. Click on the below screenshots to view examples.

<div fig-alt="First page of example .docx report">

[<img src="man/figures/report_docx.png" width="290" />](https://github.com/WA-Department-of-Agriculture/soils/tree/origin/inst/example_reports/)

First page of example .docx report

</div>

### Requirements

The report template uses [Quarto](https://quarto.org/docs/get-started/),
which is the next-generation version of R Markdown. Your version of
RStudio must be at least v2022.07 for editing and previewing Quarto
documents. Though it is strongly recommended that you use the [latest
release](https://posit.co/download/rstudio-desktop/) of RStudio.

To render `.docx` files, you must have Microsoft Word installed.

## Acknowledgements

Text and figures were adapted from [WSU Extension publication \#FS378E
Soil Health in Washington
Vineyards](https://pubs.extension.wsu.edu/soil-health-in-washington-vineyards "WSU Extension publication").

Please use the following citation when acknowledging our work:

Ryan JN, McIlquham M, Sarpong KA, Michel L, Potter T, Griffin LaHue D,
Gelardi DL. 2023. A Soil Health Report Template for Survey Studies. The
Washington Soil Health Initiative.
[washingtonsoilhealthinitiative.org](https://washingtonsoilhealthinitiative.com/)
