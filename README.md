
# soils <a href="https://wa-department-of-agriculture.github.io/soils/"><img src="man/figures/logo.png" data-align="right" height="138" /></a>

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

[![CRAN
status](https://www.r-pkg.org/badges/version/soils)](https://CRAN.R-project.org/package=soils)
[![:name status
badge](https://wa-department-of-agriculture.r-universe.dev/badges/:name)](https://wa-department-of-agriculture.r-universe.dev/)
[![soils status
badge](https://wa-department-of-agriculture.r-universe.dev/badges/soils)](https://wa-department-of-agriculture.r-universe.dev/soils)
[![R-CMD-check](https://github.com/WA-Department-of-Agriculture/soils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WA-Department-of-Agriculture/soils/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

As part of the [Washington Soil Health
Initiative](https://washingtonsoilhealthinitiative.com/), [Washington
State Department of
Agriculture](https://agr.wa.gov/departments/land-and-water/natural-resources/soil-health)
and [Washington State University](https://soilhealth.wsu.edu/) developed
`soils` for soil health data visualization and reporting.

`soils` gives you an RStudio project template with everything you need
to generate custom HTML and Word doc reports for each participant in
your soil health survey.

## Disclaimer

This repository and package are a **work in progress**.

## Installation

Install the development version of `soils` from our
[r-universe](https://wa-department-of-agriculture.r-universe.dev/) with:

``` r
install.packages('soils', 
                 repos = c(
                   'https://wa-department-of-agriculture.r-universe.dev',
                   'https://cloud.r-project.org')
                 )

# Load all the example data sets and functions
library(soils)
```

## Requirements

The report template uses [Quarto](https://quarto.org/docs/get-started/),
which is the next-generation version of [R
Markdown](https://quarto.org/docs/faq/rmarkdown.html).

We assume you’re working in RStudio Desktop with a version of at least
[v2022.07](https://dailies.rstudio.com/version/2022.07.2+576.pro12/) for
editing and previewing Quarto documents. Though it is strongly
recommended that you use the [latest
release](https://posit.co/download/rstudio-desktop/) of RStudio.

To render `.docx` files, you must have Microsoft Word installed.

## Creating a New `soils` Project

After installing `soils`, you can use the RStudio IDE or the
`soils::create_soils()` function to create a new RStudio project with
all the example data, Quarto files, style sheets, images, and R scripts
needed to generate some example soil health reports.

Read the article [Create New Template
Project](https://wa-department-of-agriculture.github.io/soils/articles/project.html)
to learn what goodies are bundled within `soils`.

## Parameterized Soil Health Reports

This package can help you generate custom static `.docx` reports and
interactive `.html` reports for every producer or land owner in your
soil health survey project.

Read the article [Write Soil Health
Reports](https://wa-department-of-agriculture.github.io/soils/articles/report.html)
for a detailed walk through the project and workflow for adapting this
template for your own project.

Check out our example reports:

- [MS
  Word](https://wa-department-of-agriculture.github.io/soils/articles/docx.html)
- [HTML](https://wa-department-of-agriculture.github.io/soils/articles/html.html)

<figure>
<img src="man/figures/report_docx.png"
data-fig-alt="First page of example .docx report"
alt="First page of example MS Word report" />
<figcaption aria-hidden="true">First page of example MS Word
report</figcaption>
</figure>

<figure>
<img src="man/figures/report_html.png"
data-fig-alt="Screenshot of .html report"
alt="Beginning of an example HTML report" />
<figcaption aria-hidden="true">Beginning of an example HTML
report</figcaption>
</figure>

## Acknowledgement and Citation

The below acknowledgement and citation are automatically embedded in
each report.

The Soil Health Report Template used to generate this report was
developed by Washington State Department of Agriculture and Washington
State University (WSU) as part of the Washington Soil Health Initiative.
Text and figures were adapted from [WSU Extension publication \#FS378E
Soil Health in Washington
Vineyards](https://pubs.extension.wsu.edu/soil-health-in-washington-vineyards "WSU Extension publication").

To cite {soils} in publications, please use:

> Ryan JN, McIlquham M, Sarpong KA, Michel L, Potter T, Griffin LaHue D,
> Gelardi DL. 2023. Visualize and Report Soil Health Survey Data with
> {soils}. Washington Soil Health Initiative.
> <https://github.com/WA-Department-of-Agriculture/soils>

## Credits

`soils` adapts from existing R project templating resources and
packages:

- [RStudio Project
  Templates](https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html)
- [`ratlas`](https://github.com/atlas-aai/ratlas)
- [`quartotemplate`](https://github.com/Pecners/quartotemplate)
- [`golem`](https://github.com/ThinkR-open/golem/)
