# aux functions to silence the piccr version message

test_processData <- function(...) {
  suppressMessages(processData(...))
}

test_processFiles <- function(...) {
  suppressMessages(processFiles(...))
}
