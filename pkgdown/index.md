
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
<style>
.iframe {
  aspect-ratio: 16 / 9;
  width: 100%;
  height: 100%;
}
</style>

## Overview

As part of the [Washington Soil Health
Initiative](https://washingtonsoilhealthinitiative.com/), the
[Washington State Department of
Agriculture](https://agr.wa.gov/departments/land-and-water/natural-resources/soil-health)
and [Washington State University](https://soilhealth.wsu.edu/) developed
{soils} for soil health data visualization and reporting.

{soils} gives you a RStudio project template with everything you need to
generate custom HTML and Microsoft Word reports for each participant in
your soil health survey.

## Quick Video Demos

### Create a {soils} Project

<iframe src="https://drive.google.com/file/d/1My0E5fq5HipvCFCRQyQ4DBmbnqt5KlwV/preview" width="640" height="360" allow="autoplay; fullscreen;">
</iframe>

### Render a MS Word Report

<iframe src="https://drive.google.com/file/d/1F6PfWzODkTq0j5cVSwagcUMTXkCMhrIr/preview" width="640" height="360" allow="autoplay;fullscreen;">
</iframe>

### Render a HTML Report

<iframe src="https://drive.google.com/file/d/1qlU0w2EN7nzoH2OGzRWEqhan-g9dVh7e/preview" width="640" height="360" allow="autoplay; fullscreen;">
</iframe>

### Render all Reports at Once

<iframe src="https://drive.google.com/file/d/1XGfplRUrLb0jiWIElQejHLZ9ig2WfEal/preview" width="640" height="360" allow="autoplay;fullscreen;">
</iframe>

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

## Installation

Install the development version of {soils} from our
[r-universe](https://wa-department-of-agriculture.r-universe.dev/) with:

``` r
install.packages(
  "usethis",
  repos = c(
    "https://wa-department-of-agriculture.r-universe.dev",
    "https://cloud.r-project.org",
    "https://ftp.osuosl.org/pub/cran/"
  )
)
```

``` r
# Load all the example data sets and functions
library(soils)
```

## Creating a New {soils} Project

Use the RStudio IDE to create a new {soils} RStudio project.

`Open RStudio` \> `File` \> `New Project` \> `New Directory` \>
**`Quarto Soil Health Report`**

<img src="man/figures/project_wizard.png" style="width:60.0%"
data-fig-alt="Screenshot of RStudio New Project Wizard with Quarto Soil Health Report selected." />

A new RStudio project will open with the template Quarto report and a
script to render all reports. Other documents and resources will appear
in the files pane.

<img src="man/figures/new_project.png"
data-fig-alt="Screenshot of new RStudio project called demo-soils. A Quarto file called producer_report.qmd is open and there is a tab for an R script called render_reports.R that renders all reports at once. The files pane is open with a project directory full of other Quarto files, styling resources, example images and data." />

**Read the article [Get
Started](https://wa-department-of-agriculture.github.io/soils/articles/get-started.html)
to learn what goodies are bundled within {soils}.**

## Example Soil Health Reports

{soils} helps you generate custom static `.docx` reports and interactive
`.html` reports for every producer or land owner in your soil health
survey project.

See demo reports rendered directly from this template project:

### [MS Word Example](https://wa-department-of-agriculture.github.io/soils/articles/docx.html)

[<img src="man/figures/report_docx.png"
data-fig-alt="First page of example .docx report" height="500"
alt="First page of example MS Word report" />](https://wa-department-of-agriculture.github.io/soils/articles/docx.html)

### [HTML Example](https://wa-department-of-agriculture.github.io/soils/articles/html.html)

[<img src="man/figures/report_html.png"
data-fig-alt="Screenshot of .html report"
alt="Beginning of an example HTML report" />](https://wa-department-of-agriculture.github.io/soils/articles/html.html)

**Read the article [Build a
Report](https://wa-department-of-agriculture.github.io/soils/articles/build-report.html)
for a detailed walk-through of the project and workflow for adapting
this template for your own project.**

## Acknowledgement and Citation

The below acknowledgement is automatically embedded in each report:

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

{soils} adapts from existing R project templating resources and
packages:

- [RStudio Project
  Templates](https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html)
- [{ratlas}](https://github.com/atlas-aai/ratlas)
- [{quartotemplate}](https://github.com/Pecners/quartotemplate)
- [{golem}](https://github.com/ThinkR-open/golem/)
