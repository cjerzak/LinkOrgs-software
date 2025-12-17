# app.R — OrgEmbed: Generate Organizational Name Embeddings with LinkOrgs
# ------------------------------------------------------------------------------
# Features
# - CSV upload or text paste, with column auto-detect + explicit selector
# - Validations & friendly notifications
# - One-click embedding generation (ML backend, v4 by default)
# - Real-time progress milestones (+ large-input confirmation)
# - Summary card + DT preview
# - Clean CSV export of embeddings
# - Basic embedding statistics (incl. PCA variance explained)
# - Modern, accessible UI (bslib theme), tooltips, and help modal
#
# Assumptions
# - Internet required on first ML backend setup/download.
# - LinkOrgs installed & accessible; otherwise the app guides you to install.
#
# Best Practices
# - Reactive parsing and caching
# - Robust tryCatch blocks with console logging
# - Clear separation of concerns with small helpers
# ------------------------------------------------------------------------------

options(shiny.maxRequestSize = 50 * 1024^2)  # accept CSVs up to ~X MB
options(error = NULL)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(shinyWidgets)
})

# conda configuration for cross-platform Docker environments
configure_python <- function() {
  cat("[conda config] Starting Python configuration...\n")
  
  # Define potential conda locations for different platforms/Docker images
  conda_paths <- c(
    Sys.getenv("CONDA_EXE"),                    # User-defined
    Sys.which("conda"),                         # System PATH
    "/opt/conda/bin/conda",                     # Common Docker location
    "/usr/local/bin/conda",                     # Alternative location
    "/home/user/miniconda3/bin/conda",          # User miniconda
    "/root/miniconda3/bin/conda",               # Root miniconda
    "/usr/bin/conda",                           # System conda
    file.path(Sys.getenv("HOME"), "miniconda3", "bin", "conda"),  # Home miniconda
    file.path(Sys.getenv("HOME"), "anaconda3", "bin", "conda")    # Home anaconda
  )
  
  # Remove empty strings and find first existing conda
  conda_paths <- conda_paths[nzchar(conda_paths)]
  conda_bin <- Find(file.exists, conda_paths)
  
  python_configured <- FALSE
  config_method <- "none"
  
  # Method 1: Try conda environment
  if (!is.null(conda_bin) && file.exists(conda_bin)) {
    cat("[conda config] Found conda at:", conda_bin, "\n")
    tryCatch({
      reticulate::use_condaenv("LinkOrgs_env", conda = conda_bin, required = TRUE)
      python_configured <- TRUE
      config_method <- "conda_env"
      cat("[reticulate] Successfully configured conda environment 'LinkOrgs_env'\n")
    }, error = function(e) {
      cat("[conda config] Failed to use conda env 'LinkOrgs_env':", conditionMessage(e), "\n")
    })
  } else {
    cat("[conda config] No conda binary found in expected locations\n")
  }
  
  # Method 2: Try RETICULATE_PYTHON environment variable
  if (!python_configured && nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
    python_path <- Sys.getenv("RETICULATE_PYTHON")
    cat("[conda config] Trying RETICULATE_PYTHON:", python_path, "\n")
    tryCatch({
      reticulate::use_python(python_path, required = TRUE)
      python_configured <- TRUE
      config_method <- "env_var"
      cat("[conda config] Successfully configured Python from RETICULATE_PYTHON\n")
    }, error = function(e) {
      cat("[conda config] Failed to use RETICULATE_PYTHON:", conditionMessage(e), "\n")
    })
  }
  
  # Method 3: Try default conda base environment as fallback
  if (!python_configured && !is.null(conda_bin)) {
    cat("[conda config] Trying default conda base environment...\n")
    tryCatch({
      reticulate::use_condaenv("base", conda = conda_bin, required = FALSE)
      python_configured <- TRUE
      config_method <- "conda_base"
      cat("[conda config] Successfully configured conda base environment\n")
    }, error = function(e) {
      cat("[conda config] Failed to use conda base environment:", conditionMessage(e), "\n")
    })
  }
  
  # Method 4: Let reticulate auto-discover
  if (!python_configured) {
    cat("[conda config] Falling back to reticulate auto-discovery...\n")
    tryCatch({
      # Force reticulate to initialize
      reticulate::py_config()
      python_configured <- TRUE
      config_method <- "auto_discovery"
      cat("[conda config] Successfully auto-discovered Python\n")
    }, error = function(e) {
      cat("[conda config] Auto-discovery failed:", conditionMessage(e), "\n")
    })
  }
  
  # Always attempt to log final configuration (this runs no matter what)
  tryCatch({
    config <- reticulate::py_config()
    cat("[conda config] FINAL CONFIG:\n")
    cat("[conda config]   Method:", config_method, "\n")
    cat("[conda config]   Python:", config$python, "\n")
    cat("[conda config]   Version:", config$version, "\n")
    cat("[conda config]   NumPy:", config$numpy, "\n")
    if (!is.null(conda_bin)) {
      cat("[reticulate]   Conda:", conda_bin, "\n")
    }
  }, error = function(e) {
    cat("[conda config] ERROR: Could not retrieve Python configuration:", conditionMessage(e), "\n")
    cat("[conda config] Configuration method attempted:", config_method, "\n")
  })
  
  return(python_configured)
}

# Execute the configuration
configure_python()

# Optional tooltips (if bsplus is available)
has_bsplus <- requireNamespace("bsplus", quietly = TRUE)
if (has_bsplus) {
  bs_tooltip <- function(id, title) bsplus::shinyInput_label_embed(id) %>%
    bsplus::bs_embed_tooltip(title, placement = "right")
} else {
  bs_tooltip <- function(id, title) NULL
}

#--- Helpers -------------------------------------------------------------------

# Nicely guess the name column from a data.frame
guess_name_col <- function(df) {
  nms <- tolower(names(df))
  patterns <- c("^names?$", "^orgnames?$", "organization", "^org$", "company", "entity", "name")
  cand <- unique(unlist(lapply(patterns, function(p) which(grepl(p, nms)))))
  if (length(cand) >= 1) names(df)[cand[1]] else names(df)[1]
}

# Parse pasted text into a data.frame with one "names" column
parse_pasted_names <- function(txt) {
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) return(data.frame(names = character(0)))
  data.frame(names = lines, stringsAsFactors = FALSE)
}

# Rename embedding columns to emb_001, emb_002, ...
rename_embed_cols <- function(df, name_col) {
  embed_cols <- setdiff(names(df), name_col)
  if (length(embed_cols) == 0) return(df)
  new_names <- sprintf("emb_%03d", seq_along(embed_cols))
  names(df)[match(embed_cols, names(df))] <- new_names
  df
}

# Extract only the numeric embedding matrix from a final result
only_embedding_matrix <- function(final_df) {
  is_num <- vapply(final_df, is.numeric, logical(1))
  final_df[, is_num, drop = FALSE]
}

# Safe notification wrapper
notify <- function(txt, type = "message", duration = 5) {
  shiny::showNotification(txt, type = type, duration = duration)
}

#--- UI ------------------------------------------------------------------------

theme <- bs_theme(bootswatch = "flatly")

ui <- page_sidebar(
  tags$head(tags$title("OrgEmbed: LinkOrgs Embeddings")),
  title = div(
    tags$a(
      "OrgEmbed:",
      href   = "https://huggingface.co/spaces/cjerzak/LinkOrgs_Online",
      target = "_blank",
      rel    = "noopener noreferrer",
      style  = "font-weight:700; text-decoration:none; color:inherit;",
      id     = "orgembed_link",
      title  = "Open the LinkOrgs Space in a new tab"
    ),
    span(" Generate Organizational Name Embeddings Using ",
         tags$a("LinkOrgs",
                href = "https://github.com/cjerzak/LinkOrgs-software",
                target = "_blank",
                style = "color: inherit; text-decoration: underline;"),
         style = "color: #D3D3D3;")
  ),
  theme = theme,
  
  sidebar = sidebar(
    width = 360,
    tags$style(HTML("
      .sidebar .shiny-input-container { margin-bottom: 12px; }
      .small-note { font-size: 0.9rem; color: #666; }
      .tight { margin-top: -6px; }
    ")),
    
    # Input mode
    radioButtons(
      "input_mode", "Input method",
      choices = c("CSV upload" = "csv", "Text paste" = "text"),
      selected = "csv", inline = TRUE
    ),
    #bs_tooltip("input_mode", "Choose how you want to provide names"),
    
    # CSV upload controls
    conditionalPanel(
      "input.input_mode == 'csv'",
      fileInput("file_csv", "Upload CSV", accept = ".csv", multiple = FALSE),
      uiOutput("col_select_ui"),
      div(class = "small-note tight",
          "Tip: We guess the organization name column but let you override it.")
    ),
    
    # Text paste controls
    conditionalPanel(
      "input.input_mode == 'text'",
      textAreaInput(
        "text_names", "Paste one name per line", rows = 6, placeholder = "Apple Inc.\nAlphabet\nMicrosoft"
      ),
      actionLink("load_examples", "Load examples"),
      #bs_tooltip("text_names", "One organization per line. Empty lines are ignored.")
    ),
    
    hr(),
    
    # Advanced options
    numericInput("max_rows", "Max rows to process", value = 5000, min = 100, step = 100),
    checkboxInput("include_names", "Include original names/columns in output", value = TRUE),
    selectInput("ml_version", "ML model version", choices = c("v1", "v2", "v3", "v4"), selected = "v4"),
    
    hr(),
    
    # Main action
    actionButton("process", "Process Names", class = "btn-primary", icon = icon("play")),
    helpText("Large inputs (> 1000 rows) will prompt for confirmation."),
    
    hr(),
    
    # Visible warning for users
    div(class = "alert alert-warning", style = "margin-top:8px; padding:8px;",
        strong("Warning: "), "Do not navigate away from page while computing embeddings! May take 10 mins to compile neural nets."
    ),
    
    # External help link (opens in new tab)
    # External help link (opens in new tab)
    tags$a(
      id = "open_help_link",
      href = "https://connorjerzak.com/linkorgs-summary/",
      target = "_blank",
      rel = "noopener",
      icon("circle-question"),
      " Technical details."
    ),
    tags$span(
      "Citation: Libgober, B., & Jerzak, C. T. (2024). Linking datasets on organizations using half a billion open-collaborated records. ",
      tags$i("Political Science Research and Methods. "),
      tags$a(
        href = "https://doi.org/10.1017/psrm.2024.55",
        target = "_blank",
        rel = "noopener",
        "https://doi.org/10.1017/psrm.2024.55"
      ),
      #". ",
    tags$a(
      href = "https://connorjerzak.com/wp-content/uploads/2024/07/LinkOrgsBib.txt",
      target = "_blank",
      rel = "noopener",
      " [.bib]"
    )
    ),
  ),
  # Main body
  layout_columns(
    col_widths = c(12),
    # Input Preview Card
    card(
      header = "1) Preview input",
      card_body(
        div(class = "small-note", "Shows up to the first 10 rows by default."),
        fluidRow(
          column(
            width = 4,
            prettySwitch("show_all_preview", "Show full table", value = FALSE)
          ),
          column(
            width = 4,
            actionButton("refresh_preview", "Refresh preview", icon = icon("arrows-rotate"))
          )
        ),
        DTOutput("input_preview")
      )
    ),
    
    # Embedding Generation & Summary Card
    card(
      header = "2) Generate embeddings",
      card_body(
        # Summary (appears after success)
        uiOutput("summary_card"),
        br(),
        conditionalPanel(
          "output.has_embeddings == true",
          strong("Embeddings preview"),
          div(class = "small-note tight", "First 5 rows; download the full CSV below."),
          DTOutput("emb_preview"),
          br(),
          downloadButton("download_embeddings", "Download Embeddings CSV", class = "btn-success")
        )
      )
    ),
    
    # Analysis Card
    conditionalPanel(
      "output.has_embeddings == true",
      card(
        header = "3) Embedding Summary",
        card_body(
          div(class = "small-note",
              "Some statistics and PCA variance explained."),
          uiOutput("stats_display")
        )
      )
    )
  )
)

#--- Server --------------------------------------------------------------------

server <- function(input, output, session) {
  
  # State ----------------------------------------------------------------------
  backend_ready <- reactiveVal(FALSE)
  embeddings_df <- reactiveVal(NULL)   # final data.frame (original + embeddings)
  pca_2d <- reactiveVal(NULL)          # data.frame with 2D PCA
  pca_10d <- reactiveVal(NULL)         # data.frame with 10D PCA
  pending_df <- reactiveVal(NULL)      # for large dataset confirmation
  large_threshold <- 1000
  
  # Help modal -----------------------------------------------------------------
  observeEvent(input$open_help_link, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = "How to use OrgEmbed",
      easyClose = TRUE, size = "l",
      tagList(
        tags$ol(
          tags$li("Choose an input method: upload a CSV or paste names."),
          tags$li("For CSV, confirm/select the column that contains organization names."),
          tags$li("Click ", tags$strong("Process Names"), " to generate embeddings."),
          tags$li("After completion, inspect the preview and click ",
                  tags$strong("Download Embeddings CSV"), " to export."),
          tags$li("Optionally, use PCA to reduce to 2 or 10 dimensions and download.")
        ),
        tags$hr(),
        tags$p(class = "small-note",
               "First-time ML backend setup needs internet to download model files.")
      )
    ))
  })
  
  # Fill examples for text paste -----------------------------------------------
  observeEvent(input$load_examples, {
    updateTextAreaInput(session, "text_names", value = "Google\nAlphabet Inc.\nMicrosoft\nMeta Platforms\nOpenAI")
  })
  
  # Reactive: parse CSV or text input ------------------------------------------
  raw_input <- reactive({
    mode <- input$input_mode
    if (identical(mode, "csv")) {
      req(input$file_csv)
      df <- tryCatch(
        read.csv(input$file_csv$datapath, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) {
          cat("[CSV read error] ", conditionMessage(e), "\n")
          notify("Could not read CSV. Ensure it's a valid .csv file.", "error", 7)
          NULL
        }
      )
      validate(need(!is.null(df) && nrow(df) > 0, "Uploaded CSV appears empty."))
      df
    } else {
      validate(need(nzchar(input$text_names), "Please paste at least one name."))
      parse_pasted_names(input$text_names)
    }
  })
  
  # Update/select name column after CSV upload
  observeEvent(raw_input(), {
    if (identical(input$input_mode, "csv")) {
      df <- raw_input()
      guessed <- guess_name_col(df)
      updateSelectInput(session, "col_select", choices = names(df), selected = guessed)
    }
  })
  
  # UI for selecting names column (CSV)
  output$col_select_ui <- renderUI({
    req(input$input_mode == "csv", raw_input())
    selectInput("col_select", "Names column", choices = names(raw_input()))
  })
  
  # Input preview (first 10 rows or full) --------------------------------------
  preview_data <- reactive({
    df <- raw_input()
    if (identical(input$input_mode, "csv")) {
      # show selected column + keep other cols for context
      # Nothing to subset here; selection is shown but preview shows all cols
    } else {
      # text mode ensures column named "names"
    }
    df
  })
  
  observeEvent(input$refresh_preview, {
    # No-op: triggers re-run of preview_data by invalidating reactives
    invisible(TRUE)
  })
  
  output$input_preview <- renderDT({
    df <- preview_data()
    req(df)
    to_show <- if (isTRUE(input$show_all_preview)) df else head(df, 10)
    datatable(
      to_show,
      options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
      rownames = FALSE
    )
  })
  
  # Large dataset confirmation flow --------------------------------------------
  proceed_with_large <- function(df) {
    pending_df(df)
    showModal(modalDialog(
      title = "Large dataset detected",
      "You are about to process ", tags$b(nrow(df)), " rows.",
      tags$p("This may take 1–5 minutes depending on your hardware and network. Proceed?"),
      footer = tagList(
        actionButton("confirm_large", "Proceed", class = "btn-primary"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  }
  
  observeEvent(input$confirm_large, {
    df <- pending_df()
    removeModal()
    if (!is.null(df)) isolate(run_embeddings(df))
    pending_df(NULL)
  })
  
  # Core: run embeddings --------------------------------------------------------
  run_embeddings <- function(df) {
    req(nrow(df) > 0)
    
    # Determine the names column
    by_col <- if (identical(input$input_mode, "csv")) {
      req(input$col_select %in% names(df))
      input$col_select
    } else {
      # text mode
      "names"
    }
    
    # Validate names column non-empty
    validate(need(any(nzchar(trimws(df[[by_col]]))), "Please provide at least one valid name."))
    # Enforce max rows
    if (nrow(df) > input$max_rows) {
      notify(sprintf("Input truncated to max_rows = %d.", input$max_rows), "warning", 6)
      df <- df[seq_len(input$max_rows), , drop = FALSE]
    }
    
    withProgress(message = "Generating embeddings...", value = 0, {
      incProgress(0.10, detail = "Parsing input...")
      # Defensive copy and clean names
      df[[by_col]] <- trimws(df[[by_col]])
      df <- df[nzchar(df[[by_col]]), , drop = FALSE]
      validate(need(nrow(df) > 0, "Please provide at least one valid name."))
      
      incProgress(0.20, detail = "Initializing model...")
      
      incProgress(0.50, detail = "Calling LinkOrgs (ML embeddings)...")
      if (!requireNamespace("LinkOrgs", quietly = TRUE)) {
        notify("Package 'LinkOrgs' not installed. See README to install.", "error", 10)
        return(invisible(NULL))
      }
      
      # Main call: ExportEmbeddingsOnly = TRUE
      rep_x <- NULL
      err <- NULL
      pdf(NULL)  # Open null device to discard plots
      tryCatch({
        rep_x <- LinkOrgs::LinkOrgs(
          x = df, y = NULL,
          by.x = by_col,
          algorithm = "ml",
          ml_version = input$ml_version,
          ExportEmbeddingsOnly = TRUE
        )
      }, error = function(e) {
        err <<- e
      })
      dev.off()  # Clean up the null device
      
      if (!is.null(err)) {
        cat("[LinkOrgs error] ", conditionMessage(err), "\n")
        notify("Embedding generation failed. Backend setup may be incomplete. Check internet/conda and retry.", "error", 10)
        return(invisible(NULL))
      }
      
      incProgress(0.80, detail = "Post-processing embeddings...")
      
      # rep_x$embedx is a data.frame with first column = by_col and remaining = embeddings
      embed_df <- rep_x$embedx
      # standardize embedding column names
      embed_df <- rename_embed_cols(embed_df, name_col = by_col)
      
      # Compose final output
      final <- if (isTRUE(input$include_names)) {
        # Bind to original df, avoiding duplicated name col
        cols_to_add <- setdiff(names(embed_df), by_col)
        cbind(df, embed_df[, cols_to_add, drop = FALSE])
      } else {
        # Return only embeddings (keep name column for context)
        embed_df
      }
      
      embeddings_df(final)
      
      incProgress(1.00, detail = "Complete!")
      notify(sprintf("Embeddings generated for %d names.", nrow(final)), "message", 5)
    })
  }
  
  # Process button: orchestrate large confirmation + run -----------------------
  observeEvent(input$process, {
    df <- raw_input()
    req(df)
    
    # Validate CSV names column selection exists
    if (identical(input$input_mode, "csv")) {
      if (!isTRUE(input$col_select %in% names(df))) {
        notify("Invalid names column—please select again.", "error", 7)
        return(invisible(NULL))
      }
    } else {
      # Text mode already enforced with req(text != "")
    }
    
    # Large dataset prompt
    if (nrow(df) > large_threshold) {
      proceed_with_large(df)
      return(invisible(NULL))
    }
    
    # Otherwise proceed immediately
    run_embeddings(df)
  })
  
  # Summary card ---------------------------------------------------------------
  output$summary_card <- renderUI({
    final <- embeddings_df()
    if (is.null(final)) {
      tagList(
        div(class = "small-note",
            "Click ", tags$strong("Process Names"),
            " to start. You'll see progress updates here.")
      )
    } else {
      emb_mat <- only_embedding_matrix(final)
      dims <- ncol(emb_mat)
      n <- nrow(final)
      bslib::card(
        bslib::card_body(
          HTML(sprintf(
            "<h4 style='margin-top:0;'>Embeddings ready</h4>
             <p class='small-note' style='margin-bottom:6px;'>
               Generated embeddings for <b>%d</b> names.
             </p>
             <p class='small-note tight'>
               Dimensions: <b>%d</b> (columns starting with <code>emb_</code>).
             </p>", n, dims
          ))
        )
      )
    }
  })
  
  # Flag for conditionalPanel
  output$has_embeddings <- reactive({
    !is.null(embeddings_df())
  })
  outputOptions(output, "has_embeddings", suspendWhenHidden = FALSE)
  
  # Embedding preview (first 5 rows) -------------------------------------------
  output$emb_preview <- renderDT({
    final <- embeddings_df(); req(final)
    to_show <- head(final, 5)
    datatable(
      to_show,
      options = list(pageLength = 5, scrollX = TRUE, dom = 'tip'),
      rownames = FALSE
    )
  })
  
  # Download full embeddings ----------------------------------------------------
  output$download_embeddings <- downloadHandler(
    filename = function() "org_embeddings.csv",
    content = function(file) {
      final <- embeddings_df(); req(final)
      write.csv(final, file, row.names = FALSE)
    }
  )
  
  # - helpful statistics 
  embedding_stats <- reactive({
    final <- embeddings_df(); req(final)
    emb <- only_embedding_matrix(final); req(ncol(emb) >= 1)
    
    # extra safety: coerce to a numeric matrix in case anything came in as characters
    emb <- as.matrix(emb)
    mode(emb) <- "numeric"
    
    pc <- prcomp(emb, center = TRUE, scale. = TRUE)
    var_exp <- pc$sdev^2 / sum(pc$sdev^2) * 100
    cum_var <- cumsum(var_exp)
    
    list(
      n    = nrow(emb),
      dims = ncol(emb),
      p1   = round(var_exp[1], 1),
      p2   = round(cum_var[min(2,  length(cum_var))],  1),
      p10  = round(cum_var[min(10, length(cum_var))],  1),
      p100 = round(cum_var[min(100,length(cum_var))],  1)
    )
  })
  

  # add to output 
  output$stats_display <- renderUI({
    stats <- embedding_stats()
    
    # Terminal-style HTML with monospace font and terminal aesthetics
    terminal_output <- HTML(paste0(
      '<div style="
      background-color: #0c0c0c;
      color: #00ff00;
      font-family: \'Courier New\', monospace;
      padding: 20px;
      border-radius: 5px;
      border: 2px solid #333;
      box-shadow: 0 0 10px rgba(0, 255, 0, 0.1);
      font-size: 14px;
      line-height: 1.6;
    ">',
      '<div style="color: #888; margin-bottom: 10px;">$ linkorg_stats --summary</div>',
      '<div style="border-bottom: 1px solid #333; margin-bottom: 15px; padding-bottom: 10px;">',
      '<span style="color: #00ff00;">═══════════════════════════════════════════════════════</span><br/>',
      '<span style="color: #00ff00;"> EMBEDDING SUMMARY STATISTICS</span><br/>',
      '<span style="color: #00ff00;">═══════════════════════════════════════════════════════</span>',
      '</div>',
      
      '<div style="margin-bottom: 8px;">',
      '<span style="color: #888;">[INFO]</span> ',
      '<span style="color: #fff;">Processing complete at:</span> ',
      '<span style="color: #0099ff;">', format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), '</span>',
      '</div>',
      
      '<div style="margin-bottom: 8px;">',
      '<span style="color: #888;">[DATA]</span> ',
      '<span style="color: #fff;">Total embeddings generated:</span> ',
      '<span style="color: #ffff00; font-weight: bold;">', stats$n, '</span>',
      '</div>',
      
      '<div style="margin-bottom: 8px;">',
      '<span style="color: #888;">[DATA]</span> ',
      '<span style="color: #fff;">Embedding dimensions:</span> ',
      '<span style="color: #ffff00; font-weight: bold;">', stats$dims, '</span>',
      '</div>',
      
      '<div style="margin-top: 15px; border-top: 1px solid #333; padding-top: 15px;">',
      '<div style="color: #00ff00; margin-bottom: 10px;">▶ Principal Component Analysis Results:</div>',
      '</div>',
      
      '<div style="margin-left: 20px; margin-bottom: 8px;">',
      '<span style="color: #888;">├─</span> ',
      '<span style="color: #fff;">PC1 variance explained:</span> ',
      '<span style="color: #00ff00;">', sprintf("%.1f%%", stats$p1), '</span>',
      '</div>',
      
      '<div style="margin-left: 20px; margin-bottom: 8px;">',
      '<span style="color: #888;">├─</span> ',
      '<span style="color: #fff;">PC1-2 cumulative variance:</span> ',
      '<span style="color: #00ff00;">', sprintf("%.1f%%", stats$p2), '</span>',
      '</div>',
      
      '<div style="margin-left: 20px; margin-bottom: 8px;">',
      '<span style="color: #888;">├─</span> ',
      '<span style="color: #fff;">PC1-10 cumulative variance:</span> ',
      '<span style="color: #00ff00;">', sprintf("%.1f%%", stats$p10), '</span>',
      '</div>',
      
      '<div style="margin-left: 20px; margin-bottom: 8px;">',
      '<span style="color: #888;">└─</span> ',
      '<span style="color: #fff;">PC1-100 cumulative variance:</span> ',
      '<span style="color: #00ff00;">', sprintf("%.1f%%", stats$p100), '</span>',
      '</div>',
      
      '<div style="margin-top: 15px; padding-top: 10px; border-top: 1px solid #333;">',
      '<span style="color: #888;">[STATUS]</span> ',
      '<span style="color: #00ff00;">✓ Analysis complete</span>',
      '</div>',
      
      '<div style="margin-top: 8px;">',
      '<span style="color: #888;">$ <span style="animation: blink 1s infinite;">_</span></span>',
      '</div>',
      
      '<style>',
      '@keyframes blink {',
      '  0%, 50% { opacity: 1; }',
      '  51%, 100% { opacity: 0; }',
      '}',
      '</style>',
      '</div>'
    ))
    
    terminal_output
  })

  
}

# Run --------------------------------------------------------------------------
shinyApp(ui, server)

