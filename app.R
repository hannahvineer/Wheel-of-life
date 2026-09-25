library(shiny)
library(bslib)
library(ggplot2)

# plot_wheel() and default_domains live in R/wheel.R (auto-sourced by Shiny).

ui <- page_sidebar(
  title = "Wheel of Life",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    width = 340,
    h5("1. Set up your wheel"),
    radioButtons(
      "domain_mode", NULL,
      choices = c("Use the standard 12 areas" = "default",
                  "Choose my own areas" = "custom")
    ),
    conditionalPanel(
      "input.domain_mode == 'custom'",
      textAreaInput(
        "custom_domains", "One life area per line",
        value = paste(default_domains, collapse = "\n"),
        rows = 12, resize = "vertical"
      ),
      helpText("Add, remove, rename or reorder lines. Minimum 3 areas.")
    ),
    actionButton("build", "Build my wheel", class = "btn-primary"),
    hr(),
    h5("2. Rate each area (0-10)"),
    helpText("Solid colour = how satisfied you are now.",
             "Translucent colour with dashed outline = how satisfied you",
             "would like to be."),
    uiOutput("rating_inputs")
  ),
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      textInput("title", NULL, value = "My Wheel of Life", width = "300px"),
      div(
        downloadButton("download_png", "PNG", class = "btn-sm btn-outline-primary"),
        downloadButton("download_csv", "CSV", class = "btn-sm btn-outline-primary")
      )
    ),
    plotOutput("wheel", height = "650px")
  )
)

server <- function(input, output, session) {

  parse_domains <- function() {
    if (input$domain_mode == "default") return(default_domains)
    d <- trimws(strsplit(input$custom_domains, "\n", fixed = TRUE)[[1]])
    unique(d[nzchar(d)])
  }

  domains <- reactiveVal(default_domains)

  # Remember ratings by domain name so they survive rebuilding the wheel.
  saved <- reactiveValues(current = list(), desired = list())

  observeEvent(input$build, {
    d <- parse_domains()
    if (length(d) < 3) {
      showNotification("Please enter at least 3 life areas.", type = "error")
      return()
    }
    old <- isolate(domains())
    for (i in seq_along(old)) {
      cur <- input[[paste0("cur_", i)]]
      des <- input[[paste0("des_", i)]]
      if (!is.null(cur)) saved$current[[old[i]]] <- cur
      if (!is.null(des)) saved$desired[[old[i]]] <- des
    }
    domains(d)
  })

  output$rating_inputs <- renderUI({
    d <- domains()
    lapply(seq_along(d), function(i) {
      cur <- isolate(saved$current[[d[i]]]) %||% 5
      des <- isolate(saved$desired[[d[i]]]) %||% 8
      div(
        class = "mb-3 pb-2 border-bottom",
        strong(d[i]),
        sliderInput(paste0("cur_", i), "Current", min = 0, max = 10,
                    value = cur, step = 1, ticks = FALSE, width = "100%"),
        sliderInput(paste0("des_", i), "Desired", min = 0, max = 10,
                    value = des, step = 1, ticks = FALSE, width = "100%")
      )
    })
  })

  ratings <- reactive({
    d <- domains()
    cur <- vapply(seq_along(d), function(i) input[[paste0("cur_", i)]] %||% NA_real_, numeric(1))
    des <- vapply(seq_along(d), function(i) input[[paste0("des_", i)]] %||% NA_real_, numeric(1))
    req(!anyNA(cur), !anyNA(des))
    data.frame(domain = d, current = cur, desired = des)
  })

  wheel_plot <- reactive(plot_wheel(ratings(), title = input$title))

  output$wheel <- renderPlot(wheel_plot(), res = 96)

  output$download_png <- downloadHandler(
    filename = function() paste0("wheel-of-life-", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, wheel_plot(), width = 9, height = 9, dpi = 200, bg = "white")
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() paste0("wheel-of-life-", Sys.Date(), ".csv"),
    content = function(file) write.csv(ratings(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
