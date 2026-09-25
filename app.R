library(shiny)
library(bslib)
library(ggplot2)

# plot_wheel(), preset_domains and preset_titles live in R/wheel.R
# (auto-sourced by Shiny).

credit <- "Developed by Hannah Vineer with Claude Code"
linkedin_url <- "https://www.linkedin.com/in/hannahvineer"

ui <- page_sidebar(
  title = "Life Audit tool",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    id = "setup",
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
               "or reorder lines. Minimum 3 areas."),
      actionButton("apply_custom", "Build my wheel", class = "btn-primary")
    ),
    hr(),
    h5("2. Rate each area (0-10)"),
    helpText("Now = how satisfied you are today (solid colour).",
             "Want = how satisfied you would like to be (translucent",
             "colour with dashed outline)."),
    tags$style(HTML("
      #rating_inputs td { vertical-align: middle; }
      #rating_inputs .form-group { margin-bottom: 0; }
      #rating_inputs input { width: 64px; padding: 2px 6px; text-align: center; }
      @media (max-width: 576px) {
        .bslib-page-main { padding: 8px; }
        .card-body { padding: 4px; }
      }
    ")),
    uiOutput("rating_inputs"),
    # Phones only: the sidebar covers the wheel, so offer a way back.
    actionButton("show_wheel", "Show my wheel", icon = icon("chart-pie"),
                 class = "btn-primary w-100 d-sm-none")
  ),
  div(
    p(class = "small mb-2",
      "Score each area of your life from 0 to 10: how satisfied you are now,",
      "and how satisfied you would like to be. The gaps show where to focus."),
    # Phones only: the sidebar starts closed, so point people to it.
    actionButton("open_setup", "Set up and score my wheel", icon = icon("sliders"),
                 class = "btn-primary w-100 d-sm-none")
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
    plotOutput("wheel", height = "auto")
  ),
  tags$footer(
    class = "text-muted small text-center",
    paste(credit, "- "),
    tags$a(
      href = linkedin_url,
      target = "_blank", rel = "noopener",
      class = "text-decoration-none",
      icon("linkedin", style = "color: #0A66C2;"),
      "linkedin.com/in/hannahvineer"
    )
  )
)

server <- function(input, output, session) {

  domains <- reactiveVal(preset_domains$life)

  # Remember ratings by domain name so they survive rebuilding the wheel.
  saved <- reactiveValues(current = list(), desired = list())

  apply_domains <- function(d) {
    if (length(d) < 3) {
      showNotification("Please enter at least 3 areas.", type = "error")
      return()
    }
    old <- domains()
    for (i in seq_along(old)) {
      cur <- input[[paste0("cur_", i)]]
      des <- input[[paste0("des_", i)]]
      if (!is.null(cur)) saved$current[[old[i]]] <- cur
      if (!is.null(des)) saved$desired[[old[i]]] <- des
    }
    domains(d)
  }

  # Presets apply as soon as they are picked; they also seed the custom list.
  observeEvent(input$domain_mode, {
    mode <- input$domain_mode
    if (mode == "custom") return()
    updateTextAreaInput(session, "custom_domains",
                        value = paste(preset_domains[[mode]], collapse = "\n"))
    apply_domains(preset_domains[[mode]])
    # Swap the title between presets unless the user has typed their own.
    if (input$title %in% preset_titles) {
      updateTextInput(session, "title", value = preset_titles[[mode]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$apply_custom, {
    d <- trimws(strsplit(input$custom_domains, "\n", fixed = TRUE)[[1]])
    apply_domains(unique(d[nzchar(d)]))
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

  observeEvent(input$open_setup, toggle_sidebar("setup", open = TRUE))
  observeEvent(input$show_wheel, toggle_sidebar("setup", open = FALSE))

  # The sidebar starts collapsed on phones; build the table anyway so the
  # wheel can draw straight away.
  outputOptions(output, "rating_inputs", suspendWhenHidden = FALSE)

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

  wheel_width <- reactive(session$clientData$output_wheel_width %||% 700)

  # Keep the wheel square and shrink its text on narrow screens.
  output$wheel <- renderPlot(
    plot_wheel(ratings(), title = input$title,
               text_scale = min(max(wheel_width() / 700, 0.45), 1)),
    height = function() min(wheel_width(), 700),
    res = 96
  )

  output$download_png <- downloadHandler(
    filename = function() paste0("wheel-of-life-", Sys.Date(), ".png"),
    content = function(file) {
      p <- plot_wheel(ratings(), title = input$title) +
        labs(caption = paste(credit, "-", sub("^https://www\\.", "", linkedin_url))) +
        theme(plot.caption = element_text(hjust = 0.5, colour = "grey45", size = 9))
      ggsave(file, p, width = 9, height = 9, dpi = 200, bg = "white")
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() paste0("wheel-of-life-", Sys.Date(), ".csv"),
    content = function(file) write.csv(ratings(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
