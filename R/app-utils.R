#' App Utils
#'
#' Functions required to execute and facililate an application user session.
#'
#' @import rdstools
#' @import fs
#'
#' @name app-utils
NULL


#' @param session shiny session object
#'
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom waiter Waiter
#'
#' @describeIn app-utils create a new waiter object
new_waiter <- function(session = NULL) {
  if (is.null(session))
    shiny::getDefaultReactiveDomain()
  waiter::Waiter$new(
    html = waiter_html("Initializing..."),
    color = get_app_colors()$bg
  )
}

#' @param msg message for waiter screen
#'
#' @importFrom shiny tagList br
#' @importFrom waiter spin_pulsar
#'
#' @describeIn app-utils get html for waiter progress page
waiter_html <- function(msg) {
  shiny::tagList(waiter::spin_pulsar(), shiny::br(), msg)
}

#' @describeIn app-utils returns TRUE if called on CI
is_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}


#' @describeIn app-utils returns TRUE if called while testing
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}


#' @describeIn app-utils Set Plot colors
get_app_colors <- function() {
  list(
    bg = "#06325f",
    fg = "#E0ECF9",
    primary    = "#187dd4",
    secondary  = "#ED9100",
    success    = "#00A651",
    info       = "#fff573",
    warning    = "#7d3be8",
    danger     = "#DB14BF"
  )
}

#' @importFrom magick image_crop image_info
#'
#' @param im image
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#'
#' @describeIn app-utils crop image based on plot brush
image_crop2 <- function(im, xmin, xmax, ymin, ymax) {
  new_width <- xmax -  xmin
  new_height <- ymax -  ymin
  adj_y <- image_info(im)$height - ymax
  im_geo <- paste0(new_width, "x", new_height, "+", xmin, "+", adj_y)
  image_crop(im, im_geo)
}

#' @importFrom magick image_quantize
#' @importFrom imager magick2cimg RGBtoHSV
#' @importFrom dplyr mutate count
#' @importFrom scales rescale
#' @import data.table
#' @importFrom stringr str_extract
#' @importFrom ggplot2 ggplot aes theme element_rect scale_fill_identity geom_text scale_color_identity theme_void coord_flip labs geom_bar
#' @importFrom grDevices hsv colorRampPalette
#'
#' @param im image
#'
#' @describeIn app-utils gen palette plot
genPalette <- function(im) {
  get_colorPal <- function(im, n=10000, cs="sRGB"){
    im |>
      image_quantize(max=n, colorspace=cs) |> ## reducing colours! different colorspace gives you different result
      magick2cimg() |> ## I'm converting, becauase I want to use as.data.frame function in imager package.
      RGBtoHSV() |>  ## i like sorting colour by hue rather than RGB (red green blue)
      as.data.frame(wide="c") |>  #3 making it wide makes it easier to output hex colour
      mutate(hex=grDevices::hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
             hue = c.1,
             sat = c.2,
             value = c.3) |>
      count(hex, hue, sat,value, sort=T) |>
      mutate(colorspace = cs) |>
      as.data.table()
  }

  my_colors <- get_colorPal(im, 10000, "sRGB")[n > 10 & value < 1 & value > 0]

  my_colors[, hex_group := str_extract(hex, "(?<=\\#).")]
  my_colors[order(hex_group, value), hex := factor(hex, levels = hex)]

  setorder(my_colors, hex_group, value)

  a <- my_colors[, .SD[1], hex_group]
  b <- my_colors[, .SD[.N], hex_group]

  colors <- rbindlist(lapply(1:nrow(a), function(i) {
    data.table(
      hex_group = a[i, hex_group],
      hex = colorRampPalette(c(a[i, hex], b[i, hex]))(5)
    )
  }))
  colors[, pos := 1:5, hex_group]


  colors[hex_group %in% LETTERS[2:6], txt_color := "black"]
  colors[is.na(txt_color), txt_color := "white"]

  colors |>
    ggplot(aes(x=hex_group, fill=hex)) +
    geom_bar() +
    theme(plot.background = element_rect(fill = "black")) +
    scale_fill_identity() +
    geom_text(aes(y = pos, label=hex, color = txt_color),
              nudge_y = -.05,
              hjust = 1,
              size=3.85) +
    scale_color_identity() +
    theme_void() +
    theme(legend.position = "none") +
    coord_flip(expand = 0) +
    labs(caption="Artwork created in the sRGB colorspace")
}

