host <- Sys.getenv("HOST", unset = "0.0.0.0")
port <- as.integer(Sys.getenv("PORT", unset = "10000"))
if (is.na(port) || port <= 0) {
  port <- 10000L
}

shiny::runApp(
  appDir = normalizePath("."),
  host = host,
  port = port,
  launch.browser = FALSE
)
