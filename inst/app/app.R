options(shiny.maxRequestSize = 500 * 1024^2)

## Dutch number formatting helper
fmt_n <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",")
}

## Flexible file reader for CSV/FST
load_file <- function(path, ext) {
  if (ext == "fst") {
    if (!requireNamespace("fst", quietly = TRUE)) {
      return(NULL)
    }
    return(fst::read_fst(path) |> tibble::as_tibble())
  }

  ## Try semicolon-delimited first, then comma-delimited
  result <- tryCatch(
    suppressWarnings(readr::read_delim(
      path, delim = ";",
      show_col_types = FALSE,
      name_repair = "unique_quiet"
    )),
    error = function(e) NULL
  )
  if (!is.null(result) && ncol(result) > 1) {
    if ("...1" %in% names(result)) {
      result <- result[, names(result) != "...1"]
    }
    return(result)
  }

  return(readr::read_delim(
    path, delim = ",",
    show_col_types = FALSE,
    name_repair = "unique_quiet"
  ))
}

## Reusable ggplot theme
plot_theme <- ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

## Reusable bar chart renderer
render_bar_chart <- function(data, col, fill_color, x_label) {
  shiny::renderPlot({
    shiny::req(data(), col %in% names(data()))
    data() |>
      dplyr::filter(!is.na(.data[[col]])) |>
      dplyr::count(.data[[col]]) |>
      ggplot2::ggplot(ggplot2::aes(
        x = reorder(.data[[col]], -n), y = n
      )) +
      ggplot2::geom_col(fill = fill_color) +
      ggplot2::labs(x = x_label, y = "Aantal") +
      plot_theme
  })
}

## Reusable NA quality table renderer
render_na_table <- function(data) {
  DT::renderDT({
    shiny::req(data())
    d <- data()
    na_pct <- sapply(
      d, function(x) round(mean(is.na(x)) * 100, 1)
    )
    tibble::tibble(
      Kolom = names(na_pct), `NA %` = na_pct
    ) |>
      dplyr::arrange(dplyr::desc(`NA %`)) |>
      DT::datatable(
        options = list(pageLength = 20, dom = "ftp"),
        rownames = FALSE
      ) |>
      DT::formatStyle(
        "NA %",
        backgroundColor = DT::styleInterval(
          c(25, 50, 75),
          c("white", "#fff3cd", "#f8d7da", "#dc3545")
        )
      )
  })
}

## Key columns for output filters
output_filter_cols <- list(
  INS_Inschrijvingsjaar           = "Inschrijvingsjaar",
  INS_Instelling                  = "Instelling",
  INS_Opleidingsvorm_naam         = "Opleidingsvorm",
  INS_Opleidingsfase_actueel_naam = "Opleidingsfase",
  OPL_Opleidingsnaam_CROHO_actueel = "Opleiding",
  DEM_Geslacht_naam               = "Geslacht",
  INS_Indicatie_eerste_jaars_instelling_cat = "Eerstejaars/Ouderejaars",
  INS_Vooropleiding_voor_HO_cat   = "Vooropleiding"
)

output_overview_cols <- c(
  "INS_Inschrijvingsjaar", "INS_Instelling",
  "INS_Opleidingsvorm_naam", "INS_Opleidingsfase_actueel_naam",
  "OPL_Opleidingsnaam_CROHO_actueel", "DEM_Geslacht_naam",
  "INS_Indicatie_eerste_jaars_instelling_cat",
  "INS_Vooropleiding_voor_HO_cat", "SUC_Type_uitstroom",
  "INS_Studentnummer", "DEM_Leeftijd_peildatum_1_oktober"
)


## ============================================================
## UI
## ============================================================

ui <- bslib::page_navbar(
  title = "prep1cho",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),


  ## --- Panel 1: Invoer ---
  bslib::nav_panel(
    "Invoer",
    icon = shiny::icon("upload"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        shiny::h6("EV-bestand uploaden", class = "text-muted fw-bold"),
        shiny::fileInput(
          "input_file", "Upload EV-bestand (.csv)",
          accept = ".csv"
        ),
        shiny::hr(),
        shiny::h6("Samenvatting", class = "text-muted fw-bold"),
        shiny::uiOutput("input_summary")
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          "Overzicht",
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            bslib::value_box(
              "Rijen", shiny::textOutput("input_n_rows"),
              showcase = shiny::icon("table")
            ),
            bslib::value_box(
              "Kolommen", shiny::textOutput("input_n_cols"),
              showcase = shiny::icon("table-columns")
            ),
            bslib::value_box(
              "Studenten", shiny::textOutput("input_n_students"),
              showcase = shiny::icon("users")
            )
          ),
          bslib::card(
            bslib::card_header("Data preview"),
            DT::DTOutput("input_preview")
          )
        ),
        bslib::nav_panel(
          "Datakwaliteit",
          bslib::card(
            bslib::card_header("NA-percentages per kolom"),
            DT::DTOutput("input_na_table")
          )
        )
      )
    )
  ),


  ## --- Panel 2: Pipeline ---
  bslib::nav_panel(
    "Pipeline",
    icon = shiny::icon("gears"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        shiny::h6("Pipeline", class = "text-muted fw-bold"),
        shiny::p(
          "Upload eerst een EV-bestand in de Invoer tab.",
          "Jaar en instelling worden automatisch gedetecteerd.",
          class = "text-muted small"
        ),
        shiny::uiOutput("pipeline_detected_info"),
        shiny::hr(),
        shiny::actionButton(
          "pipeline_run", "Pipeline uitvoeren",
          class = "btn-primary w-100",
          icon = shiny::icon("play")
        ),
        shiny::br(), shiny::br(),
        shiny::uiOutput("pipeline_download_ui")
      ),
      bslib::card(
        bslib::card_header("Status"),
        shiny::verbatimTextOutput("pipeline_log")
      )
    )
  ),


  ## --- Panel 3: Uitvoer ---
  bslib::nav_panel(
    "Uitvoer",
    icon = shiny::icon("chart-bar"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        shiny::h6("Data laden", class = "text-muted fw-bold"),
        shiny::fileInput(
          "output_file",
          "Upload uitvoer (.csv of .fst)",
          accept = c(".csv", ".fst")
        ),
        shiny::p(
          "Of gebruik het resultaat van de Pipeline tab.",
          class = "text-muted small"
        ),
        shiny::hr(),
        shiny::h6("Filters", class = "text-muted fw-bold"),
        shiny::uiOutput("output_filter_ui"),
        shiny::hr(),
        shiny::actionButton(
          "output_reset", "Reset filters",
          class = "btn-outline-secondary w-100"
        )
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          "Overzicht",
          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),
            bslib::value_box(
              "Inschrijvingen",
              shiny::textOutput("output_n_rows"),
              showcase = shiny::icon("users")
            ),
            bslib::value_box(
              "Opleidingen",
              shiny::textOutput("output_n_opl"),
              showcase = shiny::icon("graduation-cap")
            ),
            bslib::value_box(
              "Jaren",
              shiny::textOutput("output_n_jaren"),
              showcase = shiny::icon("calendar")
            ),
            bslib::value_box(
              "Kolommen",
              shiny::textOutput("output_n_cols"),
              showcase = shiny::icon("table-columns")
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("Inschrijvingen per jaar"),
              shiny::plotOutput("output_plot_jaar", height = "300px")
            ),
            bslib::card(
              bslib::card_header("Verdeling opleidingsfase"),
              shiny::plotOutput("output_plot_fase", height = "300px")
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("Verdeling geslacht"),
              shiny::plotOutput(
                "output_plot_geslacht", height = "300px"
              )
            ),
            bslib::card(
              bslib::card_header("Verdeling opleidingsvorm"),
              shiny::plotOutput("output_plot_vorm", height = "300px")
            )
          )
        ),
        bslib::nav_panel(
          "Datakwaliteit",
          bslib::card(
            bslib::card_header("NA-percentages per kolom"),
            shiny::p(
              "Kolommen met hoge NA-percentages kunnen duiden",
              "op mapping-fouten.",
              class = "text-muted"
            ),
            DT::DTOutput("output_na_table")
          ),
          bslib::card(
            bslib::card_header(
              "Samenvatting waarden per filtervariabele"
            ),
            DT::DTOutput("output_value_summary")
          )
        ),
        bslib::nav_panel(
          "Data verkennen",
          bslib::card(
            bslib::card_header("Gefilterde dataset"),
            shiny::p(
              shiny::textOutput("output_filtered_info"),
              class = "text-muted"
            ),
            DT::DTOutput("output_data_table")
          )
        ),
        bslib::nav_panel(
          "Visualisaties",
          bslib::layout_columns(
            col_widths = c(4, 8),
            bslib::card(
              bslib::card_header("Instellingen"),
              shiny::selectInput(
                "viz_x", "X-as", choices = NULL
              ),
              shiny::selectInput(
                "viz_fill", "Kleur (optioneel)",
                choices = c("Geen" = "")
              ),
              shiny::selectInput(
                "viz_type", "Grafiektype",
                choices = c(
                  "Staafdiagram" = "bar",
                  "Lijndiagram" = "line"
                )
              ),
              shiny::actionButton(
                "viz_go", "Teken",
                class = "btn-primary w-100"
              )
            ),
            bslib::card(
              bslib::card_header("Grafiek"),
              shiny::plotOutput("viz_plot", height = "450px")
            )
          )
        )
      )
    )
  )
)


## ============================================================
## Server
## ============================================================

server <- function(input, output, session) {

  input_data  <- shiny::reactiveVal(NULL)
  output_data <- shiny::reactiveVal(NULL)
  log_text    <- shiny::reactiveVal("")


  ## ========================================================
  ## Panel 1: Invoer
  ## ========================================================

  shiny::observeEvent(input$input_file, {
    shiny::req(input$input_file)
    input_data(load_file(input$input_file$datapath, "csv"))
  })

  output$input_n_rows <- shiny::renderText({
    shiny::req(input_data())
    return(fmt_n(nrow(input_data())))
  })

  output$input_n_cols <- shiny::renderText({
    shiny::req(input_data())
    return(ncol(input_data()))
  })

  output$input_n_students <- shiny::renderText({
    shiny::req(input_data())
    d <- input_data()
    ## Raw 1CHO uses lowercase column names
    student_col <- intersect(
      c("studentnummer", "INS_Studentnummer"),
      names(d)
    )
    if (length(student_col) > 0) {
      return(fmt_n(dplyr::n_distinct(d[[student_col[1]]])))
    }
    return("-")
  })

  output$input_summary <- shiny::renderUI({
    shiny::req(input_data())
    d <- input_data()
    shiny::tagList(
      shiny::p(
        shiny::strong(fmt_n(nrow(d))), " rijen",
        shiny::br(),
        shiny::strong(ncol(d)), " kolommen",
        class = "mb-0"
      )
    )
  })

  output$input_preview <- DT::renderDT({
    shiny::req(input_data())
    DT::datatable(
      input_data(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$input_na_table <- render_na_table(input_data)


  ## ========================================================
  ## Panel 2: Pipeline
  ## ========================================================

  ## Auto-detect BRIN and year from uploaded input data
  detected_meta <- shiny::reactive({
    shiny::req(input_data())
    return(prep1cho::detect_metadata(input_data()))
  })

  output$pipeline_detected_info <- shiny::renderUI({
    meta <- detected_meta()
    brin <- meta$institution_brin
    yr <- meta$year
    if (is.null(brin) && is.null(yr)) return(NULL)

    shiny::tagList(
      shiny::h6("Gedetecteerd", class = "text-muted fw-bold"),
      shiny::p(
        if (!is.null(brin)) {
          shiny::tagList(
            shiny::strong("Instelling: "), brin, shiny::br()
          )
        },
        if (!is.null(yr)) {
          shiny::tagList(
            shiny::strong("Jaar: "), yr
          )
        },
        class = "mb-0"
      )
    )
  })

  shiny::observeEvent(input$pipeline_run, {
    shiny::req(input_data())

    log_text("")
    output_data(NULL)

    shiny::withProgress(
      message = "Pipeline uitvoeren...",
      value = 0,
      {
        withCallingHandlers(
          tryCatch({
            shiny::incProgress(0.05, detail = "Data inlezen...")

            raw <- utils::read.csv2(
              input$input_file$datapath,
              stringsAsFactors = FALSE
            )

            shiny::incProgress(0.15, detail = "Pipeline starten...")

            result <- prep1cho::run_pipeline(enrollments = raw)

            shiny::incProgress(0.80, detail = "Klaar!")
            output_data(result)

          }, error = function(e) {
            shiny::showNotification(
              paste("Fout:", conditionMessage(e)),
              type = "error", duration = NULL
            )
          }),
          message = function(m) {
            log_text(paste0(log_text(), conditionMessage(m)))
            shiny::incProgress(0)
            invokeRestart("muffleMessage")
          }
        )
      }
    )
  })

  output$pipeline_log <- shiny::renderText({
    return(log_text())
  })

  output$pipeline_download_ui <- shiny::renderUI({
    if (!is.null(output_data())) {
      shiny::downloadButton(
        "pipeline_download", "Download CSV",
        class = "btn-success w-100",
        icon = shiny::icon("download")
      )
    }
  })

  output$pipeline_download <- shiny::downloadHandler(
    filename = function() {
      meta <- detected_meta()
      brin <- meta$institution_brin %||% "prep1cho"
      yr <- meta$year %||% format(Sys.Date(), "%Y")
      paste0("prep1cho_", brin, "_", yr, ".csv")
    },
    content = function(file) {
      shiny::req(output_data())
      utils::write.csv2(output_data(), file, row.names = FALSE)
    }
  )


  ## ========================================================
  ## Panel 3: Uitvoer
  ## ========================================================

  ## Load output from file upload
  shiny::observeEvent(input$output_file, {
    shiny::req(input$output_file)
    ext <- tools::file_ext(input$output_file$name)
    output_data(load_file(input$output_file$datapath, ext))
  })

  ## Auto-load synthetic output on startup
  shiny::observe({
    synth_path <- file.path(
      getwd(), "synth_data", "output.csv"
    )
    if (file.exists(synth_path)) {
      output_data(load_file(synth_path, "csv"))
    }
  }) |> shiny::bindEvent(TRUE, once = TRUE)


  ## Dynamic filter UI
  output$output_filter_ui <- shiny::renderUI({
    shiny::req(output_data())
    d <- output_data()
    available <- intersect(
      names(output_filter_cols), names(d)
    )

    lapply(available, function(col) {
      vals <- sort(unique(d[[col]]))
      vals <- vals[!is.na(vals)]
      shiny::selectInput(
        inputId  = paste0("of_", col),
        label    = output_filter_cols[[col]],
        choices  = c("Alle" = "", as.character(vals)),
        selected = "",
        multiple = TRUE
      )
    })
  })

  shiny::observeEvent(input$output_reset, {
    shiny::req(output_data())
    available <- intersect(
      names(output_filter_cols), names(output_data())
    )
    for (col in available) {
      shiny::updateSelectInput(
        session, paste0("of_", col), selected = ""
      )
    }
  })

  filtered_output <- shiny::reactive({
    shiny::req(output_data())
    d <- output_data()
    available <- intersect(
      names(output_filter_cols), names(d)
    )

    for (col in available) {
      vals <- input[[paste0("of_", col)]]
      if (!is.null(vals) && length(vals) > 0 && !all(vals == "")) {
        d <- d[d[[col]] %in% vals, ]
      }
    }
    return(d)
  })


  ## Update viz selectors when output loads
  shiny::observe({
    shiny::req(output_data())
    d <- output_data()
    cat_cols <- names(d)[sapply(d, function(x) {
      is.character(x) || is.factor(x) ||
        dplyr::n_distinct(x) < 30
    })]
    shiny::updateSelectInput(
      session, "viz_x",
      choices  = cat_cols,
      selected = if ("INS_Inschrijvingsjaar" %in% cat_cols) {
        "INS_Inschrijvingsjaar"
      } else {
        cat_cols[1]
      }
    )
    shiny::updateSelectInput(
      session, "viz_fill",
      choices = c("Geen" = "", cat_cols)
    )
  })


  ## Overview
  output$output_n_rows <- shiny::renderText({
    shiny::req(filtered_output())
    return(fmt_n(nrow(filtered_output())))
  })

  output$output_n_cols <- shiny::renderText({
    shiny::req(output_data())
    return(ncol(output_data()))
  })

  output$output_n_opl <- shiny::renderText({
    shiny::req(filtered_output())
    d <- filtered_output()
    if ("OPL_Opleidingsnaam_CROHO_actueel" %in% names(d)) {
      return(dplyr::n_distinct(
        d$OPL_Opleidingsnaam_CROHO_actueel
      ))
    }
    return("-")
  })

  output$output_n_jaren <- shiny::renderText({
    shiny::req(filtered_output())
    d <- filtered_output()
    if ("INS_Inschrijvingsjaar" %in% names(d)) {
      return(dplyr::n_distinct(d$INS_Inschrijvingsjaar))
    }
    return("-")
  })

  output$output_plot_jaar <- render_bar_chart(
    filtered_output, "INS_Inschrijvingsjaar",
    "#2c3e50", "Jaar"
  )
  output$output_plot_fase <- render_bar_chart(
    filtered_output, "INS_Opleidingsfase_actueel_naam",
    "#18bc9c", "Fase"
  )
  output$output_plot_geslacht <- render_bar_chart(
    filtered_output, "DEM_Geslacht_naam",
    "#3498db", "Geslacht"
  )
  output$output_plot_vorm <- render_bar_chart(
    filtered_output, "INS_Opleidingsvorm_naam",
    "#e74c3c", "Opleidingsvorm"
  )


  ## Data quality
  output$output_na_table <- render_na_table(output_data)

  output$output_value_summary <- DT::renderDT({
    shiny::req(output_data())
    d <- output_data()
    available <- intersect(
      names(output_filter_cols), names(d)
    )
    rows <- lapply(available, function(col) {
      vals <- d[[col]]
      tibble::tibble(
        Variabele        = output_filter_cols[[col]],
        Kolom            = col,
        `Unieke waarden` = dplyr::n_distinct(
          vals, na.rm = TRUE
        ),
        `NA %`           = round(
          mean(is.na(vals)) * 100, 1
        ),
        Voorbeeld        = paste(
          head(sort(unique(na.omit(vals))), 5),
          collapse = ", "
        )
      )
    })
    dplyr::bind_rows(rows) |>
      DT::datatable(
        options = list(pageLength = 20, dom = "t"),
        rownames = FALSE
      )
  })


  ## Data explore
  output$output_filtered_info <- shiny::renderText({
    shiny::req(filtered_output(), output_data())
    paste0(
      fmt_n(nrow(filtered_output())),
      " rijen van ",
      fmt_n(nrow(output_data())),
      " totaal"
    )
  })

  output$output_data_table <- DT::renderDT({
    shiny::req(filtered_output())
    d <- filtered_output()
    show_cols <- intersect(output_overview_cols, names(d))
    if (length(show_cols) > 0) d <- d[, show_cols]
    DT::datatable(
      d,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })


  ## Custom visualisation
  viz_settings <- shiny::reactiveValues(
    x = NULL, fill = NULL, type = NULL
  )

  shiny::observeEvent(input$viz_go, {
    viz_settings$x    <- input$viz_x
    viz_settings$fill <- input$viz_fill
    viz_settings$type <- input$viz_type
  })

  output$viz_plot <- shiny::renderPlot({
    shiny::req(filtered_output(), viz_settings$x)
    d <- filtered_output()
    x_col <- viz_settings$x
    fill_col <- viz_settings$fill

    if (is.null(fill_col) || fill_col == "") {
      p <- d |>
        dplyr::count(.data[[x_col]]) |>
        ggplot2::ggplot(ggplot2::aes(
          x = factor(.data[[x_col]]), y = n
        ))

      if (viz_settings$type == "bar") {
        p <- p + ggplot2::geom_col(fill = "#2c3e50")
      } else {
        p <- p +
          ggplot2::geom_line(
            ggplot2::aes(group = 1),
            color = "#2c3e50", linewidth = 1
          ) +
          ggplot2::geom_point(
            color = "#2c3e50", size = 2
          )
      }
    } else {
      p <- d |>
        dplyr::count(.data[[x_col]], .data[[fill_col]]) |>
        ggplot2::ggplot(ggplot2::aes(
          x    = factor(.data[[x_col]]),
          y    = n,
          fill = factor(.data[[fill_col]])
        ))

      if (viz_settings$type == "bar") {
        p <- p + ggplot2::geom_col(position = "dodge")
      } else {
        p <- p +
          ggplot2::geom_line(
            ggplot2::aes(
              group = factor(.data[[fill_col]]),
              color = factor(.data[[fill_col]])
            ),
            linewidth = 1
          ) +
          ggplot2::geom_point(
            ggplot2::aes(
              color = factor(.data[[fill_col]])
            ),
            size = 2
          ) +
          ggplot2::guides(fill = "none")
      }
      p <- p +
        ggplot2::labs(fill = fill_col, color = fill_col)
    }

    p +
      ggplot2::labs(x = x_col, y = "Aantal") +
      plot_theme +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::scale_color_brewer(palette = "Set2")
  })
}

shiny::shinyApp(ui, server)
