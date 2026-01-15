#' @keywords internal
#' @keywords internal
write_tree_png <- function(lines,
                           file,
                           width = 800,
                           bg = c("white", "black")) {
  bg <- match.arg(bg)
  fg <- if (bg == "white") "black" else "white"

  # Height scales with number of lines
  line_height <- 14
  height <- max(200, length(lines) * line_height + 40)

  grDevices::png(
    filename = file,
    width = width,
    height = height,
    bg = bg
  )

  op <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(op)
    grDevices::dev.off()
  }, add = TRUE)

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))

  y <- seq(1, 0, length.out = length(lines) + 2)[-c(1, length(lines) + 2)]

  graphics::text(
    x = 0,
    y = y,
    labels = lines,
    adj = c(0, 1),
    family = "mono",
    cex = 0.9,
    col = fg
  )

  invisible(file)
}

