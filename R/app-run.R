 #' Run App
#'
#' Functions to run the package shiny app
#'
#' @import shiny
#' @importFrom magick image_read image_info image_resize image_ggplot
#' @importFrom rdstools log_inf
#' @importFrom shinyWidgets panel noUiSliderInput wNumbFormat dropMenu actionBttn
#' @importFrom waiter useWaiter
#' @importFrom bslib bs_theme
#' @importFrom shinycssloaders withSpinner
#' @importFrom ggtext geom_textbox
#' @importFrom ggplot2 ggplot aes theme_void
#'
#' @name app-run
NULL

options(shiny.maxRequestSize=10000*1024^2)

#' @describeIn app-run returns app object for subsequent execution
#' @export
runPaletteApp <- function() {
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server()
  )
}


#' @describeIn app-run server function for app
app_server <- function() {
  function(input, output, session) {
    w <- new_waiter()

    r_image <- reactiveVal(NULL)

    ## Image upload
    observeEvent(input$myFile, {
      inFile <- input$myFile
      if (is.null(inFile)) return()

      img_path <- inFile$datapath

      ## Read in image, resize if necessary
      im <- magick::image_read(img_path)

      im_w <- magick::image_info(im)$width
      im_h <- magick::image_info(im)$height

      max_res <- 4000*4000
      if (im_w * im_h > max_res) {
        rdstools::log_inf(paste0("Resizing image"))
        im <- magick::image_resize(im, "2000")
      }
      r_image(im)
    })


    output$plot_1 <- renderPlot({
      im <- r_image()
      if (!is.null(im)) {
        magick::image_ggplot(im)
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggtext::geom_textbox(ggplot2::aes(.5, .5, label = "Upload Image To Continue"))
      }
    })


    r_palette <- eventReactive(input$submit, {
      req(r_image())
      im <- r_image()
      if (!is.null(input$plot_brush)) {
        xmin <- input$plot_brush$xmin
        xmax <- input$plot_brush$xmax
        ymin <- input$plot_brush$ymin
        ymax <- input$plot_brush$ymax
        im <- magick::image_crop2(r_image(), xmin, xmax, ymin, ymax)
      }
      genPalette(im)
    })

    output$plot_2 <- renderPlot(r_palette())

  }
}



#' @describeIn app-run UI function for app
app_ui <- function() {
  .colors <- get_app_colors()
  shiny::fluidPage(
    theme = bslib::bs_theme(
      version = 5,
      bg = .colors$bg,
      fg = .colors$fg,
      primary = .colors$primary,
      secondary = .colors$secondary,
      success = .colors$success,
      info = .colors$info,
      warning = .colors$warning,
      danger = .colors$danger,
      bootswatch = "materia"
    ),
    waiter::useWaiter(),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shinyWidgets::panel(
          fluidRow(
            fileInput("myFile", NULL, buttonLabel = "Select File", placeholder = "", accept = c('image/png', 'image/jpeg'), width = "100%")
          ),
          shinycssloaders::withSpinner(
            shiny::plotOutput("plot_1", width = "100%", height = "675px", brush = "plot_brush")
          ),
          footer = fluidRow(
            column(12, actionButton('submit', 'Generate Palette', width = "100%"))
          )
        ),
        shinyWidgets::panel(
          shinycssloaders::withSpinner(
            shiny::plotOutput("plot_2", width = "100%")
          )
        )
      )
    )
  )
}



