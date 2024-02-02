
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

# Overview

Introducing {soils}: an R package for all your soil health data
visualization and reporting needs. {soils} provides an RStudio project
template to generate customized, interactive soil health reports.
Democratize your data by giving back to the farmers and land managers
who contributed soil samples to your survey project – use {soils} to
empower each participant to explore and understand their data.

{soils} was produced by the [Washington State Department of
Agriculture](https://agr.wa.gov/departments/land-and-water/natural-resources/soil-health)
and [Washington State University](https://soilhealth.wsu.edu/), as part
of the [Washington Soil Health
Initiative](https://washingtonsoilhealthinitiative.com/).

# Requirements

The report template uses [Quarto](https://quarto.org/docs/get-started/),
which is the
[next-generation](https://quarto.org/docs/faq/rmarkdown.html) version of
[R Markdown](https://quarto.org/docs/faq/rmarkdown.html).

We assume you’re using [RStudio
v2022.07](https://dailies.rstudio.com/version/2022.07.2+576.pro12/) or
later for editing and previewing Quarto documents. We **strongly**
recommend you use the [latest release of
RStudio](https://posit.co/download/rstudio-desktop/) for support of all
Quarto features.

To render Microsoft Word (MS Word) documents, Microsoft Word must be
installed and activated.

**If you’re new to Quarto and Markdown formatting syntax, first take a
look at the Primers on
[Quarto](https://wa-department-of-agriculture.github.io/soils/articles/quarto.html)
and
[Markdown](https://wa-department-of-agriculture.github.io/soils/articles/markdown.html)
to learn how they’re used in {soils} and get familiar with their
features.**

# Installation

Install the development version of {soils} from our
[r-universe](https://wa-department-of-agriculture.r-universe.dev/) with:

``` r
install.packages(
  "soils",
  repos = c("https://wa-department-of-agriculture.r-universe.dev")
)
```

Or install directly from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("WA-Department-of-Agriculture/soils")
```

Load the example datasets and functions with:

``` r
library(soils)
```

# Usage

{soils} was developed to work ‘out of the box’ so you can immediately
install and render an example report. However, this means it will
require customization and content editing to fit your project.

Our recommended workflow is to **1)** create a new {soils} project,
**2)** try to render the example reports to make sure everything works
on your system, and **3)** customize the template files to use your own
data, content, and styling.

We provide a series of [Primers and
Tutorials](https://wa-department-of-agriculture.github.io/soils/articles/index.html)
to prepare and guide you through this workflow.

## 1. Create a new {soils} project

Follow along in the [**Create a {soils}
project**](https://wa-department-of-agriculture.github.io/soils/articles/project.html)
tutorial.

[create_soils.webm](https://github.com/WA-Department-of-Agriculture/soils/assets/95007373/7e8c650c-362c-4f81-9e24-3fb4b21f9d35)

## 2. Render the example reports

Follow along in the [**Render the example
reports**](https://wa-department-of-agriculture.github.io/soils/articles/render-example.html)
tutorial.

See the [**rendered example
reports**](https://wa-department-of-agriculture.github.io/soils/articles/examples.html).

### HTML

[create_html.webm](https://github.com/WA-Department-of-Agriculture/soils/assets/95007373/13d33152-b509-4651-a48d-e490be3c9464)

### MS Word

[create_docx.webm](https://github.com/WA-Department-of-Agriculture/soils/assets/95007373/b769f797-a390-4834-ac40-ab65cea7ef5f)

## 3. Create reports with your own data

To use your own data and customize the reports for your project, follow
along with these tutorials:

- [**Import
  data**](https://wa-department-of-agriculture.github.io/soils/articles/data.html)

- [**Customize &
  write**](https://wa-department-of-agriculture.github.io/soils/articles/customize.html)

- [**Render
  reports**](https://wa-department-of-agriculture.github.io/soils/articles/render.html)

As you edit the content, errors are bound to occur. Read [**tips and
workflows for
troubleshooting**](https://wa-department-of-agriculture.github.io/soils/articles/troubleshoot.html)

# Acknowledgement and citation

The below acknowledgement is automatically embedded in each report:

The Soil Health Report Template used to generate this report was
developed by Washington State Department of Agriculture and Washington
State University (WSU) as part of the Washington Soil Health Initiative.
Text and figures were adapted from [WSU Extension publication \#FS378E
Soil Health in Washington
Vineyards](https://pubs.extension.wsu.edu/soil-health-in-washington-vineyards "WSU Extension publication").

To cite {soils} in publications, please use:

> Ryan JN, McIlquham M, Sarpong KA, Michel LM, Potter TS, Griffin LaHue
> D, Gelardi DL. 2024. Visualize and Report Soil Health Survey Data with
> {soils}. Washington Soil Health Initiative.
> <https://github.com/WA-Department-of-Agriculture/soils>

## Credits

{soils} adapts from existing R project templating resources and
packages:

- [RStudio Project
  Templates](https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html)
- [{ratlas}](https://github.com/atlas-aai/ratlas)
- [{quartotemplate}](https://github.com/Pecners/quartotemplate)
- [{golem}](https://github.com/ThinkR-open/golem/)
