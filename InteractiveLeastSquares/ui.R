library(shiny)

# Imports our dataset and performs preprocessing on it to split it into matrices for predictors and response value. This is used as the default - user input may change this.
house_data <- read.csv("house.csv")
house_x <- as.matrix(house_data[, 1:13])
rownames(house_x) <- rep("", nrow(house_x))

# UI component setup
x_choices <- 1:ncol(house_x)
names(x_choices) <- names(house_data[, 1:13])

# UI Layout - contains no computations
ui <- fluidPage(titlePanel("Interactive Least Squares"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "x_select",
                      "Select regressors (resets modifications)",
                      x_choices,
                      multiple = TRUE
                    ),
                    actionButton("normalize_y", "Normalize Response"),
                    actionButton("normalize_all", "Normalize All Regressors"),
                    hr(),
                    uiOutput("focus_var"),
                    actionButton("normalize", "Normalize Single Regressor"),
                    actionButton("add_noise", "Add 0.1 SD of Noise to Single Regressor"),
                    
                    hr(),
                    actionButton("reset", "Reset Data")
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel(
                        "Inspect Data",
                        tabsetPanel(
                          id = "inspect",
                          tabPanel("Data",
                                   textOutput("debug"),
                                   tableOutput("dataset")),
                          tabPanel(
                            "Correlations",
                            conditionalPanel("input.x_select.length != 1",
                                             plotOutput("corrs")),
                            conditionalPanel(
                              "input.x_select.length == 1",
                              "Deselect all regressors or select more than one to view the correlation plot."
                            )
                          ),
                          tabPanel("Scatterplot", 
                                   conditionalPanel("input.x_select.length != 1",
                                                    plotOutput("scatter")),
                                   conditionalPanel(
                                     "input.x_select.length == 1",
                                     "Deselect all regressors or select more than one to view the scatter plot."
                                   )),
                          tabPanel("Regressor Histogram",
                                   plotOutput("focus_hist")
                                   )
                        )
                      ),
                      tabPanel("Matrix Form",
                               fluidRow(
                                 column(h2("Augmented X matrix"),
                                        uiOutput("mat_form"),
                                        width = 10),
                                 column(h2("Y matrix"),
                                        uiOutput("mat_form_y"),
                                        width = 2)
                               )),
                      tabPanel(
                        "Least Squares Steps",
                        tabsetPanel(
                          tabPanel("XtX",
                                   uiOutput("XtX")),
                          tabPanel("XtXinv",
                                   uiOutput("XtXinv")),
                          tabPanel("XtY",
                                   uiOutput("XtY")),
                          tabPanel("Fitted Results",
                                   fluidRow(
                                     column(h2("Beta Coefficients"),
                                            uiOutput("beta"),
                                            width = 4),
                                     column(h2("Fitted Y"),
                                            uiOutput("fit_y"),
                                            width = 4),
                                     column(h2("Residuals"),
                                            uiOutput("resid"),
                                            width = 4)
                                   ))
                        )
                      ),
                      tabPanel("Diagnostics",
                               tabsetPanel(
                                 tabPanel("Residuals",
                                          plotOutput("res_plot")),
                                 tabPanel("Error Bars",
                                          plotOutput("error_bar"))
                                 )
                               ),
                      tabPanel(
                        "About",
                        "This Shiny app was created by Hao Li and Anmei Liu for MAT 167 during ",
                        "fall quarter 2020 at UC Davis.",
                        br(),
                        br(),
                        a(href = "", "View our code"),
                        br(),
                        a(href = "", "Dataset at Kaggle"),
                        br(),
                        a(href = "", "Accompanying presentation")
                      )
                      
                    )
                  )
                ))
