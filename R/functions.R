csvDownloadButton <- function(
  id,
  filename = "data.csv",
  label = "Download as CSV"
) {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

maskedCurrencyCell <- function(value) {
  if (!is.na(value) && value == -9) {
    "Masked"
  } else if (!is.na(value) && value == -3) {
    "Missing"
  } else {
    paste0("$", formatC(value, format = "f", big.mark = ",", digits = 0))
  }
}
