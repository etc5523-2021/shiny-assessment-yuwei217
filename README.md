
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Access to clean fuels for cooking vs GDP per capit

## Yuwei Jiang + 32307322

This is a template that contains materials to create a shiny gadget
application for assessment 2. You will need the following packages
installed to generate the gadget from this template:

``` r
install.packages(c("shiny", "crosstalk", "plotly", "DT", "tidyverse", "here", "ggplot2", "dplyr"))
```

## How to run the app

In this shiny app, you can explore the relationships between the wealth
of a country and the proportion of the population that have access to
clean cooking fuels.  
On the chart page, you can select one or more countries and move the
year slider to explore the data. And you have the choice to hide data
points on countries where there are less than 1 million in the upper
right. The app also allow you to toggle between a linear axis and a log
axis on the chart. On the table page, it shows all the years for each
country ordered by from most recent year. And the table could update
according to a change from the selection input provided by this shiny
app.

## Session Info

setting value  
version R version 4.0.5 (2021-03-31)  
os Windows 10 x64  
system x86\_64, mingw32  
ui RStudio  
language (EN)  
collate Chinese (Simplified)\_China.936 ctype Chinese
(Simplified)\_China.936 tz Australia/Sydney  
date 2021-09-24
