# functions to fix error in kableExtra
collapse_rows_gr <- function(kable_input, columns = NULL,
                             valign = c("middle", "top", "bottom"),
                             latex_hline = c("full", "major", "none", "custom"),
                             row_group_label_position = c('identity', 'stack'),
                             custom_latex_hline = NULL,
                             row_group_label_fonts = NULL,
                             headers_to_remove = NULL) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
    return(kable_input)
  }
  valign <- match.arg(valign, c("middle", "top", "bottom"))
  if (kable_format == "html") {
    return(kableExtra::collapse_rows_html(kable_input, columns, valign))
  }
  if (kable_format == "latex") {
    latex_hline <- match.arg(latex_hline, c("full", "major", "none", "custom"))
    row_group_label_position <- match.arg(row_group_label_position,
                                          c('identity', 'stack'))
    return(collapse_rows_latex_gr(kable_input, columns, latex_hline, valign,
                                  row_group_label_position, row_group_label_fonts, custom_latex_hline,
                                  headers_to_remove))
  }
}


collapse_rows_latex_gr <- function(kable_input, columns, latex_hline, valign,
                                   row_group_label_position, row_group_label_fonts,
                                   custom_latex_hline, headers_to_remove) {
  table_info <- kableExtra::magic_mirror(kable_input)
  
  
  solve_enc <- function(x) {
    #may behave differently based on Sys.setlocale settings with respect to characters
    out <- enc2utf8(as.character(base::format(x, trim = TRUE, justify = 'none')))
    mostattributes(out) <- attributes(x)
    return(out)
  }
  
  
  out <- solve_enc(kable_input)
  
  valign <- switch(
    valign,
    top = "\\[t\\]",
    middle = "",
    bottom = "\\[b\\]"
  )
 
  if (is.null(columns)) {
    columns <- seq(1, table_info$ncol)
  }
  
  kable_dt_latex <- function(x) {
    data.frame(do.call(rbind, stringr::str_split(x[-1], " & ")), stringsAsFactors = FALSE)
  }
  
  
  collapse_row_matrix <- function(kable_dt, columns, html = T)  {
    if (html) {
      column_block <- function(x) c(x, rep(0, x - 1))
    } else {
      column_block <- function(x) c(rep(0, x - 1), x)
    }
    mapping_matrix <- list()
    for (i in columns) {
      mapping_matrix[[paste0("x", i)]] <- unlist(lapply(
        rle(kable_dt[, i])$lengths, column_block))
    }
    mapping_matrix <- data.frame(mapping_matrix)
    return(mapping_matrix)
  }
  
  collapse_new_dt_item <- function(x, span, width = NULL, align, valign) {
    if (span == 0) return("")
    if (span == 1) return(x)
    
    out <- paste0(
      "\\\\multirow", valign, "\\{", -span, "\\}\\{",
      ifelse(is.null(width), "\\*", width),
      "\\}\\{",
      switch(align,
             "l" = "\\\\raggedright\\\\arraybackslash ",
             "c" = "\\\\centering\\\\arraybackslash ",
             "r" = "\\\\raggedleft\\\\arraybackslash "),
      x, "\\}"
    )
    return(out)
  }
  
  contents <- table_info$contents
  kable_dt <- kable_dt_latex(contents)
  
  collapse_matrix_rev <- collapse_row_matrix(kable_dt, columns, html = TRUE)
  collapse_matrix <- collapse_row_matrix(kable_dt, columns, html = FALSE)
  
  new_kable_dt <- kable_dt
  for (j in seq_along(columns)) {
    column_align <- table_info$align_vector_origin[columns[j]]
    column_width <- ifelse(
      is.null(table_info$column_width[[paste0("column_", columns[j])]]),
      "*", table_info$column_width[paste0("column_", columns[j])])
    for (i in seq(1:nrow(collapse_matrix))) {
      if(row_group_label_position == 'stack'){
        if(columns[j] < ncol(collapse_matrix) || collapse_matrix_rev[i, j] == 0){
          new_kable_dt[i, columns[j]] <- ''
        }
      } else {
        new_kable_dt[i, columns[j]] <- collapse_new_dt_item(
          kable_dt[i, columns[j]], collapse_matrix[i, j], column_width,
          align = column_align, valign = valign
        )
      }
    }
  }
  
  midrule_matrix <- collapse_row_matrix(kable_dt, seq(1, table_info$ncol),
                                        html = F)
  midrule_matrix[setdiff(seq(1, table_info$ncol), columns)] <- 1
  
  ex_bottom <- length(contents) - 1
  contents[2:ex_bottom] <- paste0(contents[2:ex_bottom], "\\\\\\\\")
  if (!table_info$booktabs) {
    contents[2:ex_bottom] <- paste0(contents[2:ex_bottom], "\n\\\\hline")
  }
  
  
  midline_groups <- function(x, booktabs = T) {
    diffs <- c(1, diff(x))
    start_indexes <- c(1, which(diffs > 1))
    end_indexes <- c(start_indexes - 1, length(x))
    ranges <- paste0(x[start_indexes], "-", x[end_indexes])
    if (booktabs) {
      out <- paste0("\\\\cmidrule{", ranges, "}")
    } else {
      out <- paste0("\\\\cline{", ranges, "}")
    }
    out <- paste0(out, collapse = "\n")
    return(out)
  }
  
  new_contents <- c()
  if(row_group_label_position == 'stack'){
    if(is.null(headers_to_remove)) headers_to_remove <- head(columns, -1)
    table_info$colnames[headers_to_remove] <- ''
    new_header <- paste(table_info$colnames, collapse = ' & ')
    out <- sub(contents[1], new_header, out)
    table_info$contents[1] <- new_header
  }
  if(latex_hline == 'custom' & is.null(custom_latex_hline)){
    if(row_group_label_position == 'stack'){
      custom_latex_hline = 1:2
    } else {
      custom_latex_hline = 1
    }
  }
  for (i in seq(1:nrow(collapse_matrix))) {
    new_contents[i] <- paste0(new_kable_dt[i, ], collapse = " & ")
    table_info$contents[i + 1] <- new_contents[i]
    if (i != nrow(collapse_matrix)) {
      row_midrule <- switch(
        latex_hline,
        "none" = "",
        "full" = midline_groups(which(as.numeric(midrule_matrix[i, ]) > 0),
                                table_info$booktabs),
        "major" = ifelse(
          sum(as.numeric(midrule_matrix[i, ]) > 0) == ncol(midrule_matrix),
          midline_groups(which(as.numeric(midrule_matrix[i, ]) > 0),
                         table_info$booktabs),
          ""
        ),
        "custom" = ifelse(
          sum(as.numeric(midrule_matrix[i, custom_latex_hline])) > 0,
          midline_groups(which(as.numeric(midrule_matrix[i, ]) > 0),
                         table_info$booktabs),
          ""
        )
      )
      new_contents[i] <- paste0(new_contents[i], "\\\\\\\\\n", row_midrule)
    }
    out <- sub(contents[i + 1], new_contents[i], out, perl=T)
    #out <- stringr::str_replace(out,contents[i+1],new_contents[i])
  }
  out <- gsub("\\\\addlinespace\n", "", out)
  
  out <- structure(out, format = "latex", class = "knitr_kable")
  table_info$collapse_rows <- TRUE
  attr(out, "kable_meta") <- table_info
  if(row_group_label_position == 'stack'){
    group_row_index_list <- kableExtra::collapse_rows_index(kable_dt, head(columns, -1))
    out <- kableExtra::collapse_rows_latex_stack(out, group_row_index_list, row_group_label_fonts)
  }
  return(out)
}

