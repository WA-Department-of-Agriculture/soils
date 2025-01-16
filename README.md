
# soils <a href="https://wa-department-of-agriculture.github.io/soils/"><img src="man/figures/logo.png" data-align="right" height="138" /></a>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

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
template to generate customized, interactive soil health reports. These
reports include plots and tables to show how the participant’s results
compare to simple averages of results from samples of the same crop,
same county, and across the entire project.

Any scientist leading a soil health survey can use {soils} to create
custom reports for all participants. Democratize your data by giving
back to the farmers and land managers who contributed soil samples to
your soil sampling project. Use these reports to empower each
participant to explore and better understand their data.

The [Washington State Department of
Agriculture](https://agr.wa.gov/departments/land-and-water/natural-resources/soil-health)
and [Washington State University](https://soilhealth.wsu.edu/) developed
{soils} as part of the [Washington Soil Health
Initiative](https://washingtonsoilhealthinitiative.com/). Learn more
about {soils} in this [blog
post](https://washingtonsoilhealthinitiative.com/2024/03/soils-an-r-package-for-soil-health-reporting/)
or this [webinar](https://youtu.be/_8m7fTjSEOk?si=ikrCASdchiB6rDC2).

# Requirements

The report template uses [Quarto](https://quarto.org/docs/get-started/),
which is the
[next-generation](https://quarto.org/docs/faq/rmarkdown.html) version of
[R Markdown](https://quarto.org/docs/faq/rmarkdown.html).

We assume you’re using [RStudio
v2022.07](https://dailies.rstudio.com/version/2022.07.2+576.pro12/) or
later for editing and previewing Quarto documents. We **strongly
recommend** you use the [latest release of
RStudio](https://posit.co/download/rstudio-desktop/) for support of all
Quarto features. You can also download and install the [latest version
of Quarto](https://quarto.org/docs/get-started/) independently from
RStudio.

To render Microsoft Word (MS Word) documents, you must have MS Word
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

Or install from GitHub with [{pak}](https://pak.r-lib.org/index.html):

``` r
# Uncomment the below line if {pak} is not installed.
# install.packages("pak")
pak::pkg_install("WA-Department-of-Agriculture/soils")
```

Load the example datasets and functions with:

``` r
library(soils)
```

# Usage

{soils} was developed to work ‘out of the box’ so you can immediately
install and render an example report. However, you will need to
customize and edit content to fit your project.

Our recommended workflow is to **1)** create a new {soils} project,
**2)** try to render the example reports to make sure everything works
on your system, and **3)** customize the template files to use your own
data, content, and styling.

We provide a series of [Primers and
Tutorials](https://wa-department-of-agriculture.github.io/soils/articles/index.html)
to prepare and guide you through this workflow. See below for short demo
videos and links to the relevant tutorials.

## 1. Create a new {soils} project

Follow along in the [**Create a {soils}
project**](https://wa-department-of-agriculture.github.io/soils/articles/project.html)
tutorial. Choose between two report templates: **English** or
**Spanish**.

[create-soils.webm](https://github.com/WA-Department-of-Agriculture/soils/assets/95007373/12a01cf7-0efc-4948-b41e-a826dd86e6f6)

## 2. Render the example reports

Follow along in the [**Render the example
reports**](https://wa-department-of-agriculture.github.io/soils/articles/render-example.html)
tutorial.

See the [**rendered example
reports**](https://wa-department-of-agriculture.github.io/soils/articles/examples.html).

### HTML

[render-html.webm](https://github.com/WA-Department-of-Agriculture/soils/assets/95007373/a1f680a0-bed4-495a-aae7-ba85a9fa22e3)

### MS Word

[render-docx.webm](https://github.com/WA-Department-of-Agriculture/soils/assets/95007373/a617fee6-a53b-4772-ac3b-bf8d48f8fc5b)

## 3. Create reports with your own data

To use your own data, customize the reports for your project, and render
all reports, follow along with these tutorials:

- [**Import
  data**](https://wa-department-of-agriculture.github.io/soils/articles/data.html)

- [**Customize &
  write**](https://wa-department-of-agriculture.github.io/soils/articles/customize.html)

- [**Render
  reports**](https://wa-department-of-agriculture.github.io/soils/articles/render.html)

[render-reports.webm](https://github.com/WA-Department-of-Agriculture/soils/assets/95007373/b796f674-ed90-4d57-bed3-85b58c399c8f)

## Troubleshooting

As you edit the content, errors are bound to occur. Read [**tips and
workflows for
troubleshooting**](https://wa-department-of-agriculture.github.io/soils/articles/troubleshoot.html)

# Acknowledgement and citation

The below acknowledgement is automatically embedded in each report:

> This report was generated using the [{soils} R
> package](https://wa-department-of-agriculture.github.io/soils/).
> {soils} was developed by the Washington State Department of
> Agriculture and Washington State University, as part of the Washington
> Soil Health Initiative. Text and figures were adapted from [WSU
> Extension publication \#FS378E Soil Health in Washington
> Vineyards](https://pubs.extension.wsu.edu/soil-health-in-washington-vineyards).
> Learn more about {soils} in this [blog
> post](https://washingtonsoilhealthinitiative.com/2024/03/soils-an-r-package-for-soil-health-reporting/)
> or this [webinar](https://youtu.be/_8m7fTjSEOk?si=ikrCASdchiB6rDC2).

To cite {soils} in publications, please use:

> Ryan JN, McIlquham M, Sarpong KA, Michel LM, Potter TS, Griffin LaHue
> D, Gelardi DL. 2024. Visualize and Report Soil Health Data with
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

Text and figures were adapted from [WSU Extension publication \#FS378E
Soil Health in Washington
Vineyards](https://pubs.extension.wsu.edu/soil-health-in-washington-vineyards).

Report text and images were translated by Erica Tello, Eber Rivera, and
Kate Smith with WSU Food Systems and Skagit County Extension as part of
the USDA NRCS Innovation in Conservation program, led by Viva Farms
(grant number NR22-13G004).
