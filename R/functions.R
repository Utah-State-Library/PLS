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
