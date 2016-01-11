# CompRiskApp
#### A Shiny app for competing risk analysis and competing risk regression analysis

CompRiskApp is a shiny application designed for clinicians interested in competing risk analysis and
competing risk regression analysis. 

A complete online version of the App is available at <https://unipg-apps.shinyapps.io/CompRiskApp/>. 

If you wish to run CompRiskApp locally, then:

1. Download and install R <https://www.r-project.org/>;

2. Download and install R Studio <https://www.rstudio.com/>;

3. Download and install the following R packages: shiny, cmprsk, reshape2, ggplot2, gridExtra, tools;
    + ```install.packages(c("shiny", "cmprsk", "reshape2", "ggplot2", "gridExtra", "tools"))```;

4. Download the current repo, open the ui.R and server.R files in R Studio, then click on «Run App»;
    + Alternatively Execute in the R console the following line:    ```shiny::runGitHub('CompRiskApp', 'francescogiovinazzi')```;
