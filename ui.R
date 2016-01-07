shinyUI(navbarPage("CompRiskApp",
                   tabPanel("Home",
                            sidebarLayout(position="right",
                                          sidebarPanel(h4("Statistical Background"),
                                                       h6("Survival Analysis and Regression"),
                                                       p("Survival analysis is a statistical technique used to investigate time to event data and consists in the estimation of 
                                             the so-called survival function. For 'event' we tipically mean death or in general failure; statistical units that exit the study 
                                             but did not experience failure are called censored. Survival function are estimated thourgh the widely known Kaplan-Meier 
                                             method, that allows to take into account right censored data. Whether we want to explore the relationship between the 
                                             survival of a statistical unit and several explanatory variables, we can instead use the Cox proportional hazard model, based on the 
                                             regression of an hazard function on the possible explanatory variables. 
                                             Both tecniques deal with only a single type of failure, for example death independently from its cause, 
                                             ingnoring the possible information hidden beyond the censored observation."),
                                                       h6("Competing Risk Survival and Regression Analysis"),
                                                       p("Competing risks arise in studies in which subjects are exposed to more than one cause of failure, when one cause precludes the others. 
                                              In these cases, treating as censored any subject who does not experience the event of interest would be wrong, because subjects experiencing 
                                              a competing risk event are censored in an informative manner. Hence the probability of one type of competing event is correctly estimated 
                                              using Comulate Incidence Functions (CIFs) which partition the probability of failure into the probability corresponding to each competing event.  
                                              Also the Cox regression model can be adapted and several approaches are possible. We refer in particular to the model for the subdistribution hazard of the CIF."),
                                                       h4("Application example"),
                                                       p("Applications of competing risks analysis occur in many fields but particularly relevant are those in medical studies. For example we could be
                                             interested in estimating the probability of relapse after a bone marrow transplantation in patient affected by leukemia. In this case we must 
                                             consider two different failure events: the relapse of the original disease (event of interest) and the death from causes related to the 
                                             transplant (competing risk event).")),
                                          mainPanel(h4("Competing Risks Survival and Regression Analysis: a Shiny App"),
                                                    p("CompRiskApp is a web application designed for clinicians and other professionals interested in the study of survival data
                                          in the presence of multiple causes of failure. It is an extremely easy-to-use tool and allows in a few clicks to perform
                                          complex statistical analysis such as competing risk survival analysis and regression analysis on competing risk data."),
                                                    p("Different operations can be performed browsing the menu in the upper navigation bar. Each page of the App is composed by a left gray side panel, where inputs must be specified, and a wider right panel, 
                                          where outputs are presented."),
                                                    hr(),
                                                    h4("The Navigation Bar"),
                                                    h5("1. Load Data"),
                                                    p("In this section you can upload and overview data before the analysis. The preferable file type to use is CSV, but also TXT files can be handled.
                                          We recommend to clean and organize data before loading it to the App. It is also recommended to use numerical labels for competing events 
                                          and alphabetical labels to identify the levels of other categorical variables."),
                                                    h5("2. Competing Risk Analysis"),
                                                    p("In this section you can perform a subgroup Competing Risk Analysis. You must specify the failure time, the indicator variable for the competing 
                                          events, and the grouping variable. Time points can be customized. It is then possible to estimate the Cumulative Incidence Functions (CIFs), the corresponding standard errors,
                                          to test equality accross groups and to build confidence intervals. Plots of the CIFs and confidence intervals are produced."),
                                                    h5("3. Competing Risk Regression Analysis"),
                                                    p("In this section you can run a multivariable regression analysis on competing risk data. The failure time and the indicator variable containing the competing event are again needed, 
                                          besides you must specify at least one covariate (predictor variable). Covariates can be both numerical or categorical, in the latter case is possible to specify the baseline levels. 
                                          The App estimates regression coefficients for each predictor, the relative risk, the standard error, the z-value and the corresponding p-value for assessing significance. 
                                          For model selection and diagnostics, the values of the most common information criteria and the plots of Schoenfeld residuals against failure time are
                                          presented."),
                                                    h5("4. Acknowledgements"),
                                                    p("Here you can find information about the CompRiskApp project and the authors. A brief bibliography is also provided.")
                                          ))
                   ),
                   tabPanel("Load Data",
                            sidebarLayout(
                              sidebarPanel(h4("Upload your Data"),
                                           p("Data should be provided in the format of a CSV (Character-Separated Values) file. Well formatted TXT files can also be handled."),
                                           hr(),
                                           fileInput('file1', h5('Choose a CSV/TXT file'),
                                                     accept=c('text/csv', 
                                                              'text/comma-separated-values,text/plain', 
                                                              '.csv')),
                                           p("When the upload is complete, an interactive table will reproduce your data in the right panel."),
                                           p("If your dataset is properly loaded (appearing with well separated rows and columns), the interactive Data Table allows paging, searching, filtering, and sorting.  
                                              In this case you can proceed with further analysis simply browsing the navigation bar menu. If columns or rows are not well separated try to redefine the input parameters below."),
                                           hr(),
                                           h5("Define the input parameters"),
                                           p("When the input parameters are well specified the Data Table allows paging, searching, filtering, and sorting."),
                                           h6("Header"),
                                           p("Does the first line of the file contain the names of the variables?"),
                                           checkboxInput('header', 'Yes', TRUE),
                                           h6("Separator"),
                                           radioButtons('sep', 'Choose the field separator character.',
                                                        c(Comma=',',
                                                          Semicolon=';',
                                                          Tab='\t'),
                                                        ','),
                                           h6("Quoting characters"),
                                           radioButtons('quote', 'Choose the set of quoting characters.',
                                                        c(None='',
                                                          'Double Quote'='"',
                                                          'Single Quote'="'"),
                                                        '"'),
                                           hr(),
                                           p("If you still have problems with tabular visualization, consider overhauling the file before loading.")
                              ),
                              mainPanel(h3("Active dataset:"),
                                        dataTableOutput('contents')
                              )
                            )     
                   ), 
                   
                   navbarMenu("Competing Risk Analysis",
                              tabPanel("Estimate CIFs",                            
                                       sidebarLayout(
                                         sidebarPanel(h4("Fit the Cumulative Incidence Function"),
                                                      h5("Variables"),
                                                      h6("Failure time variable"),
                                                      uiOutput("Drop.ftime"),
                                                      hr(),
                                                      h6("Competing events indicator"),
                                                      uiOutput("Drop.status"),
                                                      hr(),
                                                      h6("Grouping variable"),
                                                      uiOutput("Drop.group"),
                                                      hr(),
                                                      h6("Customize time points"),
                                                      textInput("timepoints", label = "You can specify customized time points in which the estimates of the cumulative incidence will be evaluated. Time points must be given as numbers separated by a comma (i.e. 0, 3, 6, 12, 24, 48, 70)"),
                                                      h6("Customize plot appearence"),
                                                      radioButtons("plotCIFbw", label = "General appearence",
                                                                   choices = list("Color" = 1, "Black and white" = 2), selected = 1),
                                                      hr(),
                                                      actionButton("goButton", "Go!")
                                         ),
                                         mainPanel(h4("Cumulative incidence function estimates from competing risks data"),
                                                   h5("Estimates at time points:"),
                                                   verbatimTextOutput('cif'),
                                                   h5("Standard Errors:"),
                                                   verbatimTextOutput('cifstder'),
                                                   h5("Test equality accross groups:"),
                                                   verbatimTextOutput('ciftest'),
                                                   h5("Estimated cumulative incicence curve:"),
                                                   plotOutput('cifplot', width = "100%", height="400px"))
                                       )),
                              tabPanel("Build Confidence Intervals",                            
                                       sidebarLayout(
                                         sidebarPanel(h4("Confidence Intervals"),
                                                      p("Confidence intervals provide useful information about uncertainty related to parameter estimates."),
                                                      selectInput("conflev", label = h6("Specify the confidence level"), choices = list("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 2),
                                                      h6("Customize plot appearence"),
                                                      radioButtons("plotConfbw", label = "General appearence",
                                                                   choices = list("Color" = 1, "Black and white" = 2), selected = 1),
                                                      hr(),           
                                                      actionButton("goButton2", "Go!")
                                         ),
                                         mainPanel(h5("Estimated lower and upper confidence limits  at given time points:"),
                                                   verbatimTextOutput('confint'),
                                                   h5("Plot of the estimated curves with corresponding confidence intervals:"),
                                                   plotOutput('confintplot', width = "100%", height = "400px")))
                              )
                   ), 
                   navbarMenu("Competing Risk Regression Analysis",
                              tabPanel("Fit a Regression Model",                            
                                       sidebarLayout(
                                         sidebarPanel(h4("Competing Risk Regression Analysis"),
                                                      h5("Variables"),
                                                      h6("Failure time variable"),
                                                      uiOutput("Drop.ftime2"),
                                                      hr(),
                                                      h6("Competing events indicator"),
                                                      uiOutput("Drop.status2"),
                                                      hr(),
                                                      h6("Numeric covariates"),
                                                      uiOutput("Drop.covnum"),
                                                      hr(),
                                                      h6("Categorical covariates"),
                                                      uiOutput("Drop.covcat"),
                                                      h6("Customize baseline category"),
                                                      textInput("basecat", label = "You can specify customized baseline categories for each categorical covariate. 
                                                                Baseline categories must be provided in text format, exactly as coded in the Data Table, separated by comma, 
                                                                without any blank space."),
                                                      p( "Note: in order to change the baseline category for just one of the categorical covariates you must enter
                                       the baseline category also for all the others. Categories must be entered in the same order used to select variables."),
                                                      hr(),
                                                      actionButton("goButton3", "Go!")
                                         ),
                                         mainPanel(h5("Model summary table:"),
                                                   verbatimTextOutput('regModel')))
                              ),
                              tabPanel("Model Selection/Diagnostics",                            
                                       sidebarLayout(sidebarPanel(h4("Model Selection"),
                                                                  p("The likelihood of the data for a given model is a first measure of goodness of the fit, but it always increases when the number of parameters also increases.
                                                 In order to avoid overfitting, information criteria that penalize the likelihood on the base of the number of estimated parameters have been proposed. Here we
                                                  compute the Akaike Information Criterion (AIC) and the Bayesian Infromation Criteria (BIC). In general, the rule-of-thumb is to choose the model with the smallest AIC or BIC value."),
                                                                  h4("Model Diagnostic"),
                                                                  p("Plotting the j-th column of the matrix of Schoenfeld residuals against the vector of unique failure times allows the evaluation of the lack of fit over time for the corresponding covariate.
                                                 If the proportional hazard subdistribution assumption holds the residuals should have locally a costant mean across time (scatterplot smoothers should be linear and almost horizontal)."),
                                                                  h6("Customize plot appearence"),
                                                                  radioButtons("plotresbw", label = "General appearence",
                                                                               choices = list("Color" = 1, "Black and white" = 2), selected = 1),
                                                                  hr(),
                                                                  actionButton("goButton4", "Go!")
                                       ),
                                       mainPanel(h5("Information Criteria:"),
                                                 verbatimTextOutput('regBIC'),
                                                 h5("Schoenfeld residuals diagnostic plots:"),
                                                 plotOutput("regPlot", width = "100%", height = "1000px")))
                              )
                              
                   ), 
                   tabPanel("Acknowledgements",                            
                            sidebarLayout(
                              sidebarPanel(h4("Acknowledgements"),
                                           p(img(src="Unipg_Logo2.gif", width = 100, align = "center"), align = "center"),
                                           p("CompRiskApp Project has been developed by the Department of Economics of the University of Perugia
                           in memory of Michele Zanzari Marchetti."),
                                           hr(),
                                           p(img(src="RStudio_Logo.png", width = 100), align = "center"),
                                           p("The App is completely written in R language and has been developed by Francesco Giovinazzi with Shiny, an R Studio Product."),
                                           hr()
                                           
                              ),
                              mainPanel(h4("Essential References"),
                                        p("This web application is inspired by two articles of L. Scrucca, A. Santucci and F. Aversa published on the Bone 
                       Marrow Transplantation Journal:"),
                                        p("[1] L. Scrucca et. Al., Competing risk analysis usign R: an easy guide for clinicians, Bone Marrow Transplantation (2007) 40, 381-387 "),
                                        p("[2] L. Scrucca et. Al., Regression modeling of competing risk using R: an in depth guide for clinicians, Bone Marrow Transplantation (2010) 45, 1388-1395")
                                        
                              ))
                   )
))