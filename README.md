
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

## Creating a New `{soils}` Project

Read the
[vignette](https://wa-department-of-agriculture.github.io/soils/docs/articles/project.html)
for a detailed how-to.

To view the vignette within RStudio, run the command
`vignette("project", "soils")`.

## R Scripts and Functions

Read the
[vignette](https://wa-department-of-agriculture.github.io/soils/docs/articles/functions.html)
for more details.

To view the vignette within RStudio, run the command
`vignette("functions", "soils")`.

## Reports

This package can generate static `.docx` reports and interactive `.html`
reports.

Read the
[vignette](https://wa-department-of-agriculture.github.io/soils/docs/articles/report.html)
for a detailed how-to.

To view the vignette within RStudio, run the command
`vignette("report", "soils")`.

Click on the below images to open the report examples.

[<img src="man/figures/report_docx.png"
data-fig-alt="First page of example .docx report" height="300"
alt="First page of example .docx report" />](https://rawcdn.githack.com/WA-Department-of-Agriculture/soils/944448976992604a12dafbf47258327be0e5ab98/inst/example_reports/example_producer_report.pdf)

[<img src="man/figures/report_html.png"
data-fig-alt="Screenshot of .html report" height="300"
alt="Screenshot of example .html report" />](https://rawcdn.githack.com/WA-Department-of-Agriculture/soils/944448976992604a12dafbf47258327be0e5ab98/inst/example_reports/example_producer_report.html)

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
