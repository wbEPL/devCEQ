#' React table preparation helpers
#' @name f_rt
#' 
NULL


#' @describeIn f_rt Prepare data for reactable table with automatic column grouping
#' 
#' @param dta A data frame to display in reactable
#' @param col_delim Delimiter string used to identify column groups (default: "__")
#' @param col_min_groups Minimum number of columns required to form a group (default: 2)
#' @param ... Additional arguments passed to \code{reactable::reactable()}
#' 
#' @details
#' The function automatically identifies column groups based on a delimiter in column names.
#' For example, columns named "Sepal__Length" and "Sepal__Width" will be grouped under "Sepal".
#' Only columns with at least \code{col_min_groups} members will form a group.
#' 
#' @return A reactable table object
#' 
#' @import reactable
#' @export
f_format_rt <- function(
  dta,
  col_delim = "__",
  col_min_groups = 1,
  ...
) {
  # Identify columns with delimiter
  col_names <- names(dta)
  has_delim <- grepl(col_delim, col_names, fixed = TRUE)
  
  # Only proceed with grouping if at least one column has delimiter
  col_groups <- NULL
  col_defs <- list()

  # introduce default columns definitions for columns named f_get_colname(c("var", "measure", "sim") to 
  # Make them wider
  special_cols <- f_get_colname(c("var", "measure", "sim"))
  for (sp_col in special_cols) {
    if (sp_col %in% col_names) {
      col_defs[[sp_col]] <- reactable::colDef(minWidth = 150)
    }
  }
  
  if (any(has_delim)) {
    # Extract group names (prefix before delimiter)
    group_names <- sapply(col_names[has_delim], function(x) {
      strsplit(x, col_delim, fixed = TRUE)[[1]][1]
    })
    
    # Count columns per group
    group_counts <- table(group_names)
    
    # Keep only groups with minimum number of columns
    valid_groups <- names(group_counts[group_counts >= col_min_groups])
    
    # Build columnGroups list only if valid groups exist
    if (length(valid_groups) > 0) {
      col_groups <- lapply(valid_groups, function(grp) {
        # Find all columns belonging to this group
        group_cols <- col_names[has_delim][grepl(paste0(grp, col_delim), col_names[has_delim], fixed = TRUE)]
        reactable::colGroup(name = grp, columns = group_cols)
      })
      
      # Create column definitions to remove group prefix from column headers
      for (col in col_names[has_delim]) {
        parts <- strsplit(col, col_delim, fixed = TRUE)[[1]]
        if (length(parts) >= 2 && parts[1] %in% valid_groups) {
          # Remove the group prefix, keep everything after delimiter
          new_name <- paste(parts[-1], collapse = col_delim)
          col_defs[[col]] <- reactable::colDef(name = new_name)
        }
      }
    }
  }
  
  # Theme
  theme <- reactable::reactableTheme( 
    cellPadding = "4px 6px",
    style = list(fontFamily = "'Open Sans', sans-serif", fontSize = "0.875rem"),
  )

  # Create reactable
  reactable::reactable(
    dta,
    columnGroups = col_groups,
    columns = if (length(col_defs) > 0) col_defs else NULL,
    defaultColDef = colDef(align = "center", vAlign = "center"),
    filterable = TRUE,
    searchable = TRUE,
    striped = TRUE,
    resizable = TRUE,
    showPageSizeOptions = TRUE,
    outlined = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    compact = TRUE,
    height = "auto",
    theme = theme,
    ...
  )
}