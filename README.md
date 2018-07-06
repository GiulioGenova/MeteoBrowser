# ODBZ
A Shiny App to download the Open Data from the Province of Bozen 

To run the application run the following code:

==========================================================================
```R
if (!require("shiny")) install.packages("shiny")

shiny::runGitHub('GiulioGenova/ODBZ',subdir="app",launch.browser = TRUE)
```
==========================================================================

This will automatically install the packages needed on your machine (the first time you run it only) and then run the app.

A stable online version is available here:

https://euracalpenv.shinyapps.io/OpenDataSouthTyrol/

And a dev version here:

https://giuliogenova.shinyapps.io/ODBZ/
