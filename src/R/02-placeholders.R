#' 1. LIBRARIES AND SETUP -----------------------------------------------------

# use here to construct path to output folder
output_dir <- here::here("figs")

# create the figs directory if it doesn't exist
if (!base::dir.exists(output_dir)) {
  base::dir.create(output_dir, recursive = TRUE)
}

#' 2. HELPER FUNCTION ---------------------------------------------------------

# function to generate labeled blank image
make_placeholder <- function(filename,
                             width = 800,
                             height = 400,
                             label = "placeholder") {
  grDevices::png(
    filename = base::file.path(output_dir, filename),
    width = width,
    height = height
  )
  graphics::plot.new()
  graphics::text(
    x = 0.5,
    y = 0.5,
    labels = label,
    cex = 2
  )
  grDevices::dev.off()
}

#' 3. GENERATE PLACEHOLDERS ---------------------------------------------------

make_placeholder(
  filename = "aim1-placeholder.png",
  label = "figure: aim 1"
)

make_placeholder(
  filename = "aim3-placeholder.png",
  label = "figure: aim 3"
)

make_placeholder(
  filename = "qr-placeholder.png",
  width = 300,
  height = 300,
  label = "qr code"
)
