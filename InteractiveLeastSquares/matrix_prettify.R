# These functions are only used to generate beautified LaTeX output for matrices.

truncated_pretty_matrix <- function(mat, truncate_length = 10, truncate_width = 10, digits = 4) {
  mat <- round(mat, digits)
  if ((length(dim(mat)) == 0) | any(dim(mat) == 1)) {
    return(truncated_pretty_vector(mat, truncate_length))
  }
  return(paste(c("\\begin{bmatrix}", 
          truncated_pretty_core(mat, truncate_length, truncate_width), 
          "\\end{bmatrix}"), collapse = ""))
}

truncated_pretty_vector <- function(vect, truncate_length = 10) {
  if (length(vect) > truncate_length) {
    true_vect <- vect[1:truncate_length]
    true_vect <- c(true_vect, "\\vdots", vect[length(vect)])
  } else {
    true_vect <- vect
  }
  return(paste(c("\\begin{bmatrix}", 
                 paste(true_vect, collapse = "\\\\"), 
                 "\\end{bmatrix}"), collapse = ""))
}

truncated_pretty_core <- function(mat, truncate_length = 10, truncate_width = 10) {
  dims <- dim(mat)

  if (dims[1] > truncate_length + 1) {
    top_mat <- mat[1:truncate_length,]
    bot_mat <- mat[dims[1],]
    if (dims[2] > truncate_width + 1) {
      top_rows <- apply(top_mat, 1, function(x) concatify_row(c(x[1:truncate_width], "\\cdots", x[dims[2]])))
      bot_rows <- concatify_row(c(bot_mat[1:truncate_width], "\\cdots", bot_mat[dims[2]]))
    } else {
      top_rows <- apply(top_mat, 1, concatify_row)
      bot_rows <- concatify_row(bot_mat)
    }
    return(concatify_matrix(top_rows, bot_rows, min(dims[2], truncate_width), truncate_width + 1 < dims[2]))
    
  } else {
    if (dims[2] > truncate_width + 1) {
      rows <- apply(mat, 1, function(x) concatify_row(c(x[1:truncate_width], "\\cdots", x[dims[2]])))
    } else {
      rows <- apply(mat, 1, concatify_row)
    }
    return(concatify_matrix(rows))
  }
}


concatify_row <- function(row, digits = 4) {
  return(paste(row, collapse = " & "))
}

concatify_matrix <- function(rows, rows2 = NA, width = NA, truncated_width = FALSE) {
  if (is.na(rows2)) {
    return(paste(rows, collapse = " \\\\ "))
  }
  dotrow <- rep("\\vdots", width)
  if (truncated_width) {
    dotrow <- c(dotrow, "\\ddots", "\\vdots")
  }
  dotrow <- concatify_row(dotrow)
  return(paste(c(rows, dotrow, rows2), collapse = " \\\\ "))
}