library(shiny)
library(bslib)
library(ggplot2)

# plot_wheel(), preset_domains and preset_titles live in R/wheel.R
# (auto-sourced by Shiny).

ui <- page_sidebar(
  title = "Life Audit tool",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    width = 380,
    h5("1. Set up your wheel"),
    radioButtons(
      "domain_mode", NULL,
      choices = c("Whole life (12 areas)" = "life",
                  "Career & work (8 areas)" = "career",
                  "Choose my own areas" = "custom")
    ),
    conditionalPanel(
      "input.domain_mode == 'custom'",
      textAreaInput(
        "custom_domains", "One area per line",
        value = paste(preset_domains$life, collapse = "\n"),
        rows = 12, resize = "vertical"
      ),
      helpText("Starts from the last preset you picked. Add, remove, rename",
               "or reorder lines. Minimum 3 areas.")
    ),
    actionButton("build", "Build my wheel", class = "btn-primary"),
    hr(),
    h5("2. Rate each area (0-10)"),
    helpText("Now = how satisfied you are today (solid colour).",
             "Want = how satisfied you would like to be (translucent",
             "colour with dashed outline)."),
    tags$style(HTML("
      #rating_inputs td { vertical-align: middle; }
      #rating_inputs .form-group { margin-bottom: 0; }
      #rating_inputs input { width: 64px; padding: 2px 6px; text-align: center; }
    ")),
    uiOutput("rating_inputs")
  ),
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      textInput("title", NULL, value = "Life Audit", width = "300px"),
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
    if (input$domain_mode != "custom") return(preset_domains[[input$domain_mode]])
    d <- trimws(strsplit(input$custom_domains, "\n", fixed = TRUE)[[1]])
    unique(d[nzchar(d)])
  }

  # Seed the custom list from the most recently chosen preset.
  observeEvent(input$domain_mode, {
    if (input$domain_mode != "custom") {
      updateTextAreaInput(session, "custom_domains",
                          value = paste(preset_domains[[input$domain_mode]],
                                        collapse = "\n"))
    }
  })

  domains <- reactiveVal(preset_domains$life)

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
    # Swap the title between presets unless the user has typed their own.
    if (input$domain_mode != "custom" && input$title %in% preset_titles) {
      updateTextInput(session, "title", value = preset_titles[[input$domain_mode]])
    }
  })

  output$rating_inputs <- renderUI({
    d <- domains()
    rating_input <- function(id, value) {
      numericInput(id, NULL, value = value, min = 0, max = 10, step = 1)
    }
    rows <- lapply(seq_along(d), function(i) {
      tags$tr(
        tags$td(d[i]),
        tags$td(rating_input(paste0("cur_", i), isolate(saved$current[[d[i]]]) %||% 5)),
        tags$td(rating_input(paste0("des_", i), isolate(saved$desired[[d[i]]]) %||% 8))
      )
    })
    tags$table(
      class = "table table-sm",
      tags$thead(tags$tr(tags$th("Area"), tags$th("Now"), tags$th("Want"))),
      tags$tbody(rows)
    )
  })

  # Snap typed values back into whole numbers from 0 to 10.
  observe({
    ids <- paste0(rep(c("cur_", "des_"), each = length(domains())), seq_along(domains()))
    for (id in ids) {
      v <- input[[id]]
      if (!is.null(v) && !is.na(v) && (v < 0 || v > 10 || v != round(v))) {
        updateNumericInput(session, id, value = min(max(round(v), 0), 10))
      }
    }
  })

  ratings <- reactive({
    d <- domains()
    cur <- vapply(seq_along(d), function(i) input[[paste0("cur_", i)]] %||% NA_real_, numeric(1))
    des <- vapply(seq_along(d), function(i) input[[paste0("des_", i)]] %||% NA_real_, numeric(1))
    # Keep the last wheel on screen while a box is briefly empty mid-edit.
    req(!anyNA(cur), !anyNA(des), cancelOutput = TRUE)
    clamp <- function(x) pmin(pmax(round(x), 0), 10)
    data.frame(domain = d, current = clamp(cur), desired = clamp(des))
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
