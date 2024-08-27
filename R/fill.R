#' Fill a Template
#' @export
#'
#' @description
#' Fills in a TeX template with values according to spec
#'
#' @param template A character vector, each element of which is a line of LaTeX code.
#' @param spec A list describing the relationship between values and template. See Details.
#' @param values A tidy dataframe containing the values that will populate the template.
#' @param value_col The name of the column in `values` that will
fill = function(template, spec, values, value_col) {

  mismatch_rows = setdiff(names(spec$rows), names(values))
  mismatch_cols = setdiff(names(spec$cols), names(values))

  if (length(mismatch_rows) > 0) { stop('The following row keys are missing from the dataframe: ',
                                        paste(mismatch_rows, collapse = ', ')) }

  if (length(mismatch_cols) > 0) { stop('The following col keys are missing from the dataframe: ',
                                        paste(mismatch_cols, collapse = ', ')) }


  fill_shell = make_fill_shell(spec)

  fill_tb = merge(fill_shell, values, all.x = TRUE, all.y = FALSE)


  # Replace missing values
  fill_tb[[value_col]] = ifelse(is.na(fill_tb[[value_col]]), " ", fill_tb[[value_col]])


  # Fill in values
  for (ii in seq_len(nrow(fill_tb))) {

    row_num = fill_tb$row_num[ii]
    template[[row_num]] = paste(template[[row_num]], fill_tb[[value_col]][[ii]], sep = " & ")

  }

  # End every filled row with slashes
  for (row_num in unique(fill_tb$row_num)) {
    template[[row_num]] = paste(template[[row_num]], "\\\\")
  }


  # Add EOL characters
  for (ee in spec[["end_of_lines"]]) {
    for (row_num in idx_as_int(ee[["rows"]])) {
      template[[row_num]] = paste(template[[row_num]], ee[["text"]])
    }
  }

  return(template)

}

#' Fill a Template from Files
#' @export
fill_from_files = function(template_file, spec_file, values, value_col) {
  fill(template  = readLines(template_file),
       spec      = yaml::read_yaml(spec_file),
       values    = values,
       value_col = value_col)
}
