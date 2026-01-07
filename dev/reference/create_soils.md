# Create a project directory for generating soil health reports

Creates an RStudio project containing Quarto template and resources
(images, style sheets, render.R script).

## Usage

``` r
create_soils(path, template = "English", overwrite = FALSE, open = TRUE)
```

## Source

Adapted from `golem::create_golem()`.

## Arguments

- path:

  Name of project directory to be created.

- template:

  Template type. Either "English" or "Spanish".

- overwrite:

  Boolean. Overwrite the existing project?

- open:

  Boolean. Open the newly created project?

## Value

A new project directory containing template and resources.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create temporary directory
dir <- tempdir()

# Create soils project
create_soils(dir, overwrite = TRUE)

# Delete temporary directory
unlink(dir, recursive = TRUE)
} # }
```
