install_r_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}


install_r_packages(c("shiny", "bslib", "jsonlite", "base64enc", "DBI", "RSQLite"))

python_candidates <- Sys.which(c("python", "python3"))
python_bin <- unname(python_candidates[nzchar(python_candidates)][1])
if (nzchar(python_bin)) {
  system2(
    python_bin,
    c("-m", "pip", "install", "opencv-python-headless", "rapidocr-onnxruntime")
  )
}

message("Hosted dependencies installed.")
