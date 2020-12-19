library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(corrplot)

source("matrix_prettify.R")
source("LS_util.R")

options(scipen = 999) # Display settings

# Imports our dataset and performs preprocessing on it to split it into matrices for predictors and response value. This is used as the default - user input may change this.
house_data <- read.csv("house.csv")
house_x <- as.matrix(house_data[, 1:13])
rownames(house_x) <- rep("", nrow(house_x))
house_y <- as.matrix(house_data$MEDV) # Median home value

# Server logic (mostly contains plot generation, etc)
server <- function(input, output) {
  # Variable/helper initializations
  active_data <- reactiveVal(house_x)
  X <- reactiveVal(0)
  Y <- reactiveVal(house_y)
  
  inputs_selected <- reactive({
    return(length(input$x_select) > 0)
  })
  
  # Updates X as needed
  observe({
    X(augment_X(active_data())) # Set X to the augmented data
  })
  
  # Performs filtering on the active data (this also resets any modifications)
  observeEvent(input$x_select, {
    if (inputs_selected()) {
      cols <- as.numeric(input$x_select)
    } else {
      cols <-
        1:ncol(house_x) # If no inputs selected, select all columns
    }
    active_data(house_x[, cols]) # Set active data to original data subsetted by selections
  })
  
  # Performs normalization on the response variable
  observeEvent(input$normalize_y, {
    col_to_norm <- house_y
    normed <-
      (col_to_norm - mean(col_to_norm)) / sd(col_to_norm) # Normalizes the values
    Y(scale) # Swap in normalized data
  })
  
  # Performs normalization on the all the regressors of the active data
  observeEvent(input$normalize_all, {
    current_data <- active_data()
    current_data <- scale(current_data) # Normalize each column
    active_data(current_data) # Return the modified data to the active data
  })
  
  # Performs normalization on the selected column of the active data
  observeEvent(input$normalize, {
    current_data <- active_data()
    index <-
      as.numeric(input$focused_var) # Finds the column to normalize
    col_to_norm <-
      current_data[, index] # Collects the values in that column
    normed <-
      (col_to_norm - mean(col_to_norm)) / sd(col_to_norm) # Normalizes the values
    current_data[, index] <- normed
    active_data(current_data) # Return the modified data to the active data
  })
  
  # Adds noise to the selected column of the active data
  observeEvent(input$add_noise, {
    current_data <- active_data()
    index <-
      as.numeric(input$focused_var) # Finds the column to add noise
    col_to_noise <-
      current_data[, index] # Collects the values in that column
    noised <-
      jitter(col_to_noise, factor = 5, amount = 0) # Adds 0.1 SD of noise to the values
    current_data[, index] <- noised
    active_data(current_data) # Return the modified data to the active data
  })
  
  # Removes any modifications to the data by overwriting with the original data
  observeEvent(input$reset, {
    active_data(house_x)
    Y(house_y)
  })
  
  # UI selector for focused variable
  output$focus_var <- renderUI({
    if (inputs_selected()) {
      cols <- as.numeric(input$x_select)
    } else {
      cols <- 1:ncol(house_x)
    }
    selectInput("focused_var", "Choose regressor to focus on/modify",
                {
                  if (is.null(ncol(active_data()))) {
                    eligible <- 1
                  } else {
                    eligible <- 1:ncol(active_data())
                  }
                  names(eligible) <-
                    dimnames(house_x)[[2]][cols]
                  eligible
                })
  })
  
  # Renders all the data as a table
  output$dataset <- renderTable({
    if (length(input$x_select) > 0) {
      cols <- as.numeric(input$x_select)
    } else {
      cols <- 1:ncol(house_x)
    }
    cbind(Y = Y(), active_data())
  })
  
  # Renders the correlation plot
  output$corrs <- renderPlot(corrplot(cor(active_data())))
  
  # Renders the scatterplot
  output$scatter <- renderPlot({
    melted <- melt(active_data())
    melted$Var1 = rep(house_y, ncol(active_data()))
    ggplot(melted, aes(
      x = value,
      y = Var1,
      group = Var2,
      col = Var2
    )) +
      geom_point() + facet_wrap( ~ Var2) + labs(col = "Regressor") +
      ggtitle("Response against each active regressor")
  })
  
  # Renders a histogram for the actively selected variable
  output$focus_hist <- renderPlot({
    if (is.null(dim(active_data()))) {
      var <- active_data()
    } else {
      var <- active_data()[, as.numeric(input$focused_var)]
    }
    hist(var,
         main = paste(c(
           "Distribution of ", dimnames(active_data())[[2]][as.numeric(input$focused_var)]
         ), collapse = ""),
         xlab = dimnames(active_data())[[2]][as.numeric(input$focused_var)])
  })
  
  # Renders the augmented X matrix
  output$mat_form <- renderUI({
    withMathJax(paste(c(
      "$$",
      truncated_pretty_matrix(X()),
      "$$"
    ), collapse = ""))
  })
  
  # Renders the Y vector
  output$mat_form_y <- renderUI({
    withMathJax(paste(c(
      "$$",
      truncated_pretty_matrix(Y()),
      "$$"
    ), collapse = ""))
  })
  
  # Renders the XtX matrix
  output$XtX <- renderUI({
    withMathJax(paste(c(
      "$$",
      truncated_pretty_matrix(XtX(X()), truncate_width = 5),
      "$$"
    ), collapse = ""))
  })
  
  # Renders the inverse of the XtX matrix
  output$XtXinv <- renderUI({
    withMathJax(paste(c(
      "$$",
      truncated_pretty_matrix(XtXinv(X()), truncate_width = 5),
      "$$"
    ), collapse = ""))
  })
  
  # Renders the XtY matrix
  output$XtY <- renderUI({
    withMathJax(paste(c(
      "$$",
      truncated_pretty_matrix(XtY(X(), Y()), truncate_width = 5),
      "$$"
    ), collapse = ""))
  })
  
  # Renders the betas vector
  output$beta <- renderUI({
    withMathJax(paste(c(
      "$$",
      truncated_pretty_matrix(beta(X(), Y()), truncate_length = 20),
      "$$"
    ), collapse = ""))
  })
  
  # Renders the fitted Y vector
  output$fit_y <- renderUI({
    withMathJax(paste(c(
      "$$",
      truncated_pretty_matrix(fitted_Y(beta(X(
      ), Y(
      )), X()), truncate_length = 20),
      "$$"
    ), collapse = ""))
  })
  
  
  # Renders the residuals vector
  output$resid <- renderUI({
    withMathJax(paste(c(
      "$$",
      truncated_pretty_matrix(residuals(fitted_Y(
        beta(X(), Y()), X()
      ), Y()), truncate_length = 20),
      "$$"
    ), collapse = ""))
  })
  
  # Renders the residual plot
  output$res_plot <- renderPlot({
    res <- Y() - (X() %*% beta(X(), Y()))
    fit <- X() %*% beta(X(), Y())
    plot(fit,
         res,
         xlab = "fitted values",
         ylab = "residuals",
         main = "Residual Plot")
    abline(h = 0, col = "grey")
  })
  
  # Renders the error bar plot
  output$error_bar <- renderPlot({
    res <- Y() - (X() %*% beta(X(), Y()))
    res_abs = abs(res)
    if (is.null(dim(active_data()))) {
      var <- active_data()
    } else {
      var <- active_data()[, as.numeric(input$focused_var)]
    }
    qplot(var , Y()) + geom_errorbar(aes(
      x = var,
      ymin = Y() - res_abs,
      ymax = Y() + res_abs
    ), width = 0.25) +
      ggtitle(paste(
        c(
          "Scatterplot of ",
          dimnames(active_data())[[2]][as.numeric(input$focused_var)],
          " against response, with error bar"
        ),
        collapse = ""
      )) +
      xlab(dimnames(active_data())[[2]][as.numeric(input$focused_var)]) +
      ylab("Response")
    
  })
  
}
