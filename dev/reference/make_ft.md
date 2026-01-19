# Make a flextable with column names from another dataframe

Make a flextable with column names from another dataframe

## Usage

``` r
make_ft(table, header)
```

## Arguments

- table:

  A dataframe with the contents of the desired flextable output.

- header:

  Another dataframe with three columns:

  - First column contains what the top header row should be. In our
    template, this is the abbreviation of the measurement (i.e.
    `Organic Matter`).

  - Second column, called `"key"`, contains the join key. In our
    template, this is the same as the first column.

  - Third column contains the second header row. In our template, this
    is the unit (i.e. `%`).

## Value

Formatted flextable object.

## Examples

``` r
# Read in wrangled table data
headers_path <- soils_example("headers.RDS")
headers <- readRDS(headers_path)

tables_path <- soils_example("tables.RDS")
tables <- readRDS(tables_path)

# Input dataframes
headers$chemical
#>               abbr              key     unit
#> 1               pH               pH         
#> 2               EC               EC mmhos/cm
#> 3              CEC              CEC cmolc/kg
#> 4          Total C          Total C        %
#> 5              TOC              TOC        %
#> 6      Inorganic C      Inorganic C        %
#> 7 Field or Average Field or Average         

tables$chemical
#> # A tibble: 6 × 7
#>   `Field or Average`                pH    EC   CEC `Total C`   TOC `Inorganic C`
#>   <glue>                         <dbl> <dbl> <dbl>     <dbl> <dbl>         <dbl>
#> 1 Field 01                         6.7  0.42   7.8       1.8   1.8         NA   
#> 2 Field 03                         7.6  0.6   10         1.6   1.5          0.12
#> 3 Hay/Silage Average 
#> (14 Fields)   6.1  0.43  15         2.4   2.4         NA   
#> 4 Pasture, Seeded Average 
#> (16 …    6.2  0.33  14         2.7   2.7          0.11
#> 5 County 9 Average 
#> (5 Fields)      7.1  0.48   8.7       1.7   1.6          0.11
#> 6 Project Average 
#> (100 Fields)     6.1  0.74  15         2.9   2.9          0.19

# Make the flextable
make_ft(
  table = tables$chemical,
  header = headers$chemical
) |>
  # Style the flextable
  style_ft() |>
  # Add the white line under the columns with the same units
  unit_hline(header = headers$chemical)


.cl-4316945e{}.cl-430f3a24{font-family:'Lato';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(255, 255, 255, 1.00);background-color:transparent;}.cl-430f3a2e{font-family:'Poppins';font-size:10pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-430f3a2f{font-family:'Poppins';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4312c5a4{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1.3;background-color:transparent;}.cl-4312c5ae{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1.3;background-color:transparent;}.cl-4312c5b8{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1.3;background-color:transparent;}.cl-4312e516{width:0.75in;background-color:rgba(2, 59, 44, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4312e520{width:0.75in;background-color:rgba(2, 59, 44, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4312e521{width:0.75in;background-color:rgba(2, 59, 44, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(255, 255, 255, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4312e522{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(62, 61, 61, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4312e52a{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(62, 61, 61, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4312e52b{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(62, 61, 61, 1.00);border-top: 1pt solid rgba(62, 61, 61, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4312e52c{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(62, 61, 61, 1.00);border-top: 1pt solid rgba(62, 61, 61, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}



Field or Average
```

pH

EC

CEC

Total C

TOC

Inorganic C

mmhos/cm

cmolc/kg

%

Field 01

6.7

0.42

7.8

1.8

1.8

Field 03

7.6

0.60

10.0

1.6

1.5

0.12

Hay/Silage Average   
(14 Fields)

6.1

0.43

15.0

2.4

2.4

Pasture, Seeded Average   
(16 Fields)

6.2

0.33

14.0

2.7

2.7

0.11

County 9 Average   
(5 Fields)

7.1

0.48

8.7

1.7

1.6

0.11

Project Average   
(100 Fields)

6.1

0.74

15.0

2.9

2.9

0.19
