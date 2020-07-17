library(DT)

# creates a beautiful table
# the easiest way is to call it like that: create_dt(table)
# but can be called with specific parameters
# @param data: the table data
# optional params: 
# @param rown: vector for the row names
# @param coln: vector for the coln names
# @param capNR: the part before the caption, like "Table 1" (i dont know if automatic counting is possible)
# @param cap: the caption for the table, like "This is a caption"
# @param ext: a list of special Data Table extensions
# @param opt: a list of options for the specified extensions
create_dt = function(data, rown, coln, capNR, cap, ext, opt) {
  if (missing(rown)) {
    rown = TRUE
  }
  if (missing(coln)) {
    coln = TRUE
  }
  if (missing(capNR)) {
    capNR = ""
  }
  if (missing(cap)) {
    cap = ""
  }
  if (missing(ext)) {
    ext = list(
      "Buttons" = NULL, 
      "FixedColumns" = list(leftColumns = 2)
      )
  }
  if (missing(opt)) {
    opt = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf'),
      scrollX = TRUE
    )
  }
  caps = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    capNR, htmltools::em(cap)
  )
  if(class(rown) == "logical" && class(coln) == "logical"){
    datatable(
      data,
      class = 'cell-border stripe',
      extensions = ext,
      options = opt,
      caption = caps
    )
  }
  else if (class(rown) == "logical" ) {
    datatable(
      data,
      class = 'cell-border stripe',
      extensions = ext,
      options = opt,
      caption = caps,
      colnames = coln
    )
  } else if (class(coln) == "logical" ){
    datatable(
      data,
      class = 'cell-border stripe',
      extensions = ext,
      options = opt,
      caption = caps,
      rownames = rown
    )
  }
  else {
    datatable(
      data,
      class = 'cell-border stripe',
      extensions = ext,
      options = opt,
      caption = caps,
      rownames = rown,
      colnames = coln
    )
  }
}

