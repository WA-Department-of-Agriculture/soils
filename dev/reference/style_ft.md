# Style a flextable

Style a flextable

## Usage

``` r
style_ft(
  ft,
  header_font = "Lato",
  body_font = "Poppins",
  header_color = "#023B2C",
  header_text_color = "white",
  border_color = "#3E3D3D"
)
```

## Arguments

- ft:

  Flextable object.

- header_font:

  Font of header text. Defaults to `"Lato"`.

- body_font:

  Font of body text. Defaults to `"Poppins"`.

- header_color:

  Background color of header cells. Defaults to WaSHI green.

- header_text_color:

  Color of header text. Defaults to white.

- border_color:

  Color of border lines. Defaults to WaSHI gray.

## Value

Styled flextable object.

## Examples

``` r
# Read in wrangled example table data
tables_path <- soils_example("tables.RDS")
tables <- readRDS(tables_path)

# Make the table
ft <- flextable::flextable(tables$Biological)
ft


.cl-542186d6{}.cl-541a0816{font-family:'Poppins';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-541d1c0e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-541d1c22{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-541d517e{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-541d5188{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-541d5192{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-541d5193{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-541d519c{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-541d51a6{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Field or Average
```

Organic Matter

Min C

POXC

PMN

ACE Protein

Field 01

4.5

36

500

67

6.7

Field 03

6.7

51

550

110

4.2

Hay/Silage Average   
(14 Fields)

5.5

37

500

92

7.8

Pasture, Seeded Average   
(16 Fields)

5.5

58

520

140

7.3

County 9 Average   
(5 Fields)

4.7

50

490

79

5.3

Project Average   
(100 Fields)

5.8

50

530

99

8.5

\# Style the table style_ft(ft)

[TABLE]
