library(shiny)
library(shinyjs)
library(agricolae)
library(tidyverse)
library(flextable)
library(officer)
library(dplyr)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(DT)
library(pagedown)
library(tinytex)
# library(doebioresearch)
source("00.rbd.R")

ui <- fluidPage(
  useShinyjs(),
  
  tags$style(HTML("
    body {background-color: #FFF8E1;}
    .title-panel {
        text-align: center; font-weight: bold; font-size: 24px; color: #4b0082;}
    .footer {
        position: relative;left: 10px;bottom: 10px;font-style: italic;font-size: 14px; 
        color: green;width: 100%;text-align: left;}
    .sidebarPanel {
      position: relative;height: 100vh; padding-bottom: 40px;
      font-size: 14px; /* Decrease font size here */
      }
    .action-buttons {
      margin-bottom: 10px;}
     #dynamic_ui select, #dynamic_ui button 
                  {margin-top: 10px;}
    # .action-buttons button {font-size: 12px;padding: 10px;}
    # .shiny-input-container {font-size: 12px;width: 300px;}
    .stop-button-container {position: absolute;top: 10px;right: 10px;z-index: 1000;}
    
    table {
      border-collapse: collapse;width: 100%;font-size: 16px;text-align: center;
      background-color: white !important;margin-left: 0; margin-right: auto;}
    th {
      padding: 8px;font-weight: bold;text-align: center; vertical-align: middle;
      border: 2px solid black !important;}
    td {
      padding: 8px;text-align: center;border: 2px solid black !important;
      background-color: white !important;}
    tr:nth-child(even), tr:nth-child(odd) {background-color: white !important;}
     caption {font-size: 18px;font-weight: bold;margin-bottom: 10px;color: #4b0082;}
     
    .data-table {background-color: white;}
    .data-display-container {background-color: #d4f1c3;padding: 10px;border-radius: 5px;}
    .analysis_results_container {background-color: #d4f1c3;padding: 10px;border-radius: 5px;}
    .export_results_container {background-color: #d4f1c3;padding: 10px;border-radius: 5px;}

  ")),
  
  div(class = "title-panel", "Randomized Block Design (RBD)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      checkboxInput("header", "Header", TRUE),
      uiOutput("dynamic_ui"),
      hidden(div(id = "loading_output", tags$strong(style = "color: green;", "Analyzing... Please wait."))), 
      hidden(div(id = "final_output", tags$strong(style = "color: green;", "Output will be displayed in Analysis Results pane."))),
      
      
      br(),
      div(class = "footer", HTML("Designed and developed<br>Agronomy, TNAU")),
    ),
    mainPanel(
      tabsetPanel(
        type = "tab",
        tabPanel("Data Display", 
                 div(id = "data_display_container", tableOutput("data_table")),
                 actionButton("edit_data", "Edit Data", class = "btn btn-primary", 
                              style = "position: absolute; right: 10px; top: 10px; z-index: 1000;")
        ),
        
        tabPanel("Analysis Results", 
                 div(id = "analysis_results_container", 
                     h3(textOutput("selected_data_col"), style = "color: #4b0082; text-align: center;"),
                     uiOutput("all_results")
                 )
        ),
        # Export Results Tab
        tabPanel("Export Results", 
                 div(id = "export_results_container", 
                     h3("Export Analysis Results", style = "color: #4b0082; text-align: center; font-weight: bold;"),
                     h4("Select checkboxes to download parameters:", style = "font-weight: bold; color: #000000;"),
                     
                     checkboxInput("include_ANOVA", tags$b("Include ANOVA Table"), value = FALSE),
                     checkboxInput("include_Summary", tags$b("Include Summary"), value = FALSE),
                     checkboxInput("include_MCT", tags$b("Include Mean Comparison Test"), value = FALSE),
                     checkboxInput("include_Grouping", tags$b("Include Grouping"), value = FALSE),
                     checkboxInput("include_FinalTable", tags$b("Include Final Table"), value = FALSE),
                     br(),
                     checkboxInput("include_CombinedFinalTable", tags$b("Include combined Final Table (skip for single column)"), value = FALSE),
                     
                     br(),
                     
                     div(style = "margin-top: 20px;"),  # Adds two-line space
                     checkboxInput("select_columns", 
                                   div(tags$b("Export Specific Columns Only"), style = "font-size: 16px;"), 
                                   value = FALSE),
                     
                     conditionalPanel(
                       condition = "input.select_columns == true",
                       uiOutput("column_checkboxes")
                     ),
                     br(),br(),br(),
                     
                     downloadButton("export_word", tags$b("Export as Word")),
                     downloadButton("downloadTablePDF", tags$b("Download Table as PDF")),
                 )
        )
      )
    )
  ),
  
  div(class = "stop-button-container",actionButton("stop_app", "Stop App", class = "btn btn-danger")),
  # div(class = "footer", HTML("Designed and developed<br>Agronomy, TNAU"))
)

server <- function(input, output, session) {
  
  useShinyjs()
  loading <- reactiveVal(FALSE)
  observeEvent(input$stop_app, {
    stopApp("Session Stopped by user")
  })
  
  shinyjs::addClass("export_results_container", "export_results_container")
  
  uploaded_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = input$header, sep = ",")
    df <- as.data.frame(df)
    
    colnames(df) <- make.names(trimws(colnames(df)))
    df
  })
  
  observeEvent(input$edit_data, {
    showModal(
      modalDialog(
        title = "Edit Data",
        DT::dataTableOutput("editable_data_table"),
        footer = tagList(modalButton("Cancel"),
                         actionButton("save_edits", "Save Changes", class = "btn btn-success")
        ),size = "l"))})
  
  output$editable_data_table <- DT::renderDataTable({
    req(uploaded_data())
    
    DT::datatable(
      uploaded_data(),
      editable = "cell",  
      options = list(dom = 't',scrollX = TRUE,scrollY = "400px",paging = FALSE),
      callback = JS(
        "table.on('cellEdit', function(e, cell, row, col) {
         var input = cell.input();
         if (input.attr('type') === 'number') {
           input.attr('type', 'text');
         }
       });"
      )
    )
  }, server = FALSE)
  
  edited_data <- reactiveVal(NULL)
  
  observeEvent(input$save_edits, {
    req(input$editable_data_table_cell_edit)  
    isolate({
      new_data <- uploaded_data() 
      edit <- input$editable_data_table_cell_edit
      new_data[edit$row, edit$col] <- edit$value
      edited_data(new_data)
    })
    
    removeModal()
  })
  
  observe({
    if (!is.null(edited_data())) {
      print("Data updated successfully!")
    }})
  
  analysis_data <- reactive({
    if (!is.null(edited_data())) {
      edited_data()
    } else {
      uploaded_data()
    }
  })
  
  selected_cols <- reactiveValues(treatment = NULL, replication = NULL, data = NULL)
  
  output$dynamic_ui <- renderUI({
    req(uploaded_data())
    data_columns <- colnames(uploaded_data())
    
    tagList(
      div(class = "action-buttons",
          actionButton("show_data", "Display Data"),
          actionButton("hide_data", "Hide Data")
      ),
      selectInput("design", "Select Design", 
                  choices = c("", "CRD", "RBD", "FRBD", "SPLIT", "STRIP"), 
                  selected = ""),
      
      # RBD UI OR CRD UI
      conditionalPanel(
        condition = "input.design === 'RBD'",
        selectInput("treatment_col", "Select Treatment Column", 
                    choices = c("", data_columns)),
        conditionalPanel(
          condition = "input.treatment_col !== ''",
          selectInput("replication_col", "Select Replication Column", 
                      choices = c("", data_columns))
        ),
        conditionalPanel(
          condition = "input.replication_col !== ''",
          selectInput("data_col", "Select Data Column", 
                      choices = c("", data_columns, "All"))
        ),
        conditionalPanel(
          condition = "input.data_col !== ''",
          selectInput("multi_comp_test", "Select Multiple Comparison Test", 
                      choices = list("", "No test" = 0, "LSD" = 1, "Duncan test" = 2, "HSD" = 3))
        ),
        conditionalPanel(
          condition = "input.multi_comp_test !== ''",
          numericInput("round_digits", "Round Digits", value = 2, min = 0, max = 10, step = 1)
        ),
        conditionalPanel(
          condition = "input.multi_comp_test != ''",
          selectInput("final_table", "Select Parameter for Final Table", 
                      choices = list("",
                                     "Mean" = 1,"Mean with Group" = 2,"Mean ± SE" = 3,"Mean ± SD" = 4,
                                     "Mean ± CI" = 5, "Mean with Group ± SE" = 6, "Mean with Group ± SD" = 7, 
                                     "Mean with Group ± CI" = 8), selected = "")
        ),
        conditionalPanel(
          condition = "input.final_table !== ''",
          actionButton("run_analysis", "Analyze")
        )
      ),
    )
  })
  
  observeEvent(input$treatment_col, {
    if (!is.null(input$treatment_col) && input$treatment_col != "") {
      choices_replication <- colnames(uploaded_data())
      choices_replication <- choices_replication[choices_replication != input$treatment_col]
      
      updateSelectInput(session, "replication_col",
                        choices = c("", choices_replication))
    }
  })
  
  observeEvent(input$replication_col, {
    if (!is.null(input$replication_col) && input$replication_col != "") {
      choices_data <- colnames(uploaded_data())
      choices_data <- choices_data[choices_data != input$treatment_col & choices_data != input$replication_col]
      
      updateSelectInput(session, "data_col",
                        choices = c("", choices_data, "All"))
    }
  })
  
  observeEvent(input$show_data, {
    
    
    output$data_table <- renderTable({
      req(uploaded_data())
      if (!is.null(edited_data())) {
        edited_data()
      } else {
        uploaded_data()
      }
    }, 
    rownames = FALSE,
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE,
    classes = "data-table")
    shinyjs::addClass("data_display_container", "data-display-container")
  })
  
  observeEvent(input$hide_data, {
    output$data_table <- renderTable(NULL)
    shinyjs::removeClass("data_display_container", "data-display-container")
  })
  
  observeEvent(input$run_analysis, {
    
    loading(TRUE)
    
    shinyjs::hide("final_output") 
    shinyjs::show("loading_output")
    
    req(input$design == "RBD", input$treatment_col, input$replication_col, input$data_col, input$multi_comp_test,input$round_digits,input$final_table)
    
    data <- analysis_data()
    
    shinyjs::addClass("analysis_results_container", "analysis_results_container")
    
    # Error checking
    if (!(input$treatment_col %in% colnames(data)) ||
        !(input$replication_col %in% colnames(data)) ||
        !(input$data_col %in% colnames(data) | input$data_col == "All")) {
      output$analysis_results <- renderPrint({
        "Error: Selected columns do not exist in the uploaded data."
      })
      return()
    }
    
    if (!input$data_col == "All" && !is.numeric(data[[input$data_col]])) {
      data[[input$data_col]] <- as.numeric(data[[input$data_col]])
      if (any(is.na(data[[input$data_col]]))) {
        output$analysis_results <- renderPrint({
          "Error: The selected data column contains non-numeric values that could not be converted."
        })
        return()
      }
    }
    
    # Run analysis and store results
    analysis_result <- tryCatch({
      
      result_list <- list()
      
      if (input$data_col == "All") {
        result_list <- list()  
        data_columns <- setdiff(names(data), c(input$treatment_col, input$replication_col))
        
        for (col_name in data_columns) {
          tryCatch({
            data_col_idx <- which(names(data) == col_name)
            eval(parse(text = paste0("result <- rbd_analysis(data[", data_col_idx, "], data$", input$treatment_col,
                                     ", data$", input$replication_col, ", ", input$multi_comp_test, ", ", input$round_digits, ", ",input$final_table, ")" )))
            
            ANOVA <- eval(parse(text = paste0("result$", col_name, "$ANOVA")))
            Summary <- eval(parse(text = paste0("result$", col_name, "$Summary")))
            mct <- eval(parse(text = paste0("result$", col_name, "$mct")))
            grouping <- eval(parse(text = paste0("result$", col_name, "$group")))
            final_table <- eval(parse(text = paste0("result$", col_name, "$final_table")))
            
            result_list[[col_name]] <- list(ANOVA = ANOVA, Summary = Summary, mct = mct, grouping = grouping,final_table=final_table)
            
          }, error = function(e) {
            result_list[[col_name]] <- list(error = paste("Error analyzing column:", col_name, e$message))
          })
        }
      } else {
        data_col_idx <- which(names(data) == input$data_col)
        eval(parse(text = paste0("result <- rbd_analysis(data[", data_col_idx, "], data$", input$treatment_col,
                                 ", data$", input$replication_col, ", ", input$multi_comp_test, ", ", input$round_digits, ", ",input$final_table,")" )))
        
        ANOVA <- eval(parse(text = paste0("result$", input$data_col, "$ANOVA")))
        Summary <- eval(parse(text = paste0("result$", input$data_col, "$Summary")))
        mct <- eval(parse(text = paste0("result$", input$data_col, "$mct")))
        grouping <- eval(parse(text = paste0("result$", input$data_col, "$group")))
        final_table <- eval(parse(text = paste0("result$", input$data_col, "$final_table")))
        
        # Store results in a list format
        result_list[[input$data_col]] <- list(ANOVA = ANOVA, Summary = Summary, mct = mct, grouping = grouping,final_table=final_table)
      }
      
      result_list
      
    }, error = function(e) {
      paste("Error in analysis:", e$message)
    })
    
    output$selected_data_col <- renderText({
      req(input$data_col)
      paste("Data Column:", input$data_col)
    })
    
    #ANOVA
    output$analysis_results <- renderUI({
      if (input$data_col == "All") {
        tagList(
          lapply(names(analysis_result), function(col_name) {
            if (!is.null(analysis_result[[col_name]]$ANOVA)) {
              tagList(
                tableOutput(paste0("anova_", col_name))
              )
            }
          })
        )
      } else {
        tagList(
          tableOutput("single_anova_result")
        )
      }
    })
    
    observe({
      if (input$data_col == "All") {
        lapply(names(analysis_result), function(col_name) {
          local({
            col <- col_name
            output[[paste0("anova_", col)]] <- renderTable({
              anova_table <- analysis_result[[col]]$ANOVA
              if ("df" %in% colnames(anova_table)) {
                anova_table$df <- format(round(anova_table$df, 0), nsmall = 0)
              }
              anova_table
            }, rownames = FALSE, caption = paste("<b> ANOVA Table for", col, "</b>"), align = "c", caption.placement = "top")
          })
        })
      } else {
        output$single_anova_result <- renderTable({
          anova_table <- analysis_result[[input$data_col]]$ANOVA
          if ("df" %in% colnames(anova_table)) {
            anova_table$df <- format(round(anova_table$df, 0), nsmall = 0)
          }
          anova_table
        }, rownames = FALSE, caption = "<b> ANOVA Table </b>", align = "c", caption.placement = "top")
      }
    })
    
    #SUMMARY RESULTS
    output$summary_results <- renderUI({
      if (input$data_col == "All") {
        tagList(
          lapply(names(analysis_result), function(col_name) {
            if (!is.null(analysis_result[[col_name]]$Summary)) {
              tagList(
                tags$h4(paste("Summary for Column:", col_name)),
                tableOutput(paste0("summary_", col_name))
              )
            }
          })
        )
      } else {
        tableOutput("single_summary_result")
      }
    })
    
    observe({
      if (input$data_col == "All") {
        lapply(names(analysis_result), function(col_name) {
          local({
            col <- col_name
            output[[paste0("summary_", col)]] <- renderTable({
              analysis_result[[col]]$Summary
            }, rownames = FALSE, caption = paste("<b> Summary for", col, "</b>"), align = "c", caption.placement = "top")
          })
        })
      } else {
        output$single_summary_result <- renderTable({
          analysis_result[[input$data_col]]$Summary
        }, rownames = FALSE, caption = "<b> Summary </b>", align = "c", caption.placement = "top")
      }
    })
    
    #MEAN COMPARISON TEST 
    output$mct <- renderUI({
      if (input$data_col == "All") {
        tagList(
          lapply(names(analysis_result), function(col_name) {
            if (!is.null(analysis_result[[col_name]]$mct)) {
              tagList(
                tags$h4(paste("Mean Comparison Test for Column:", col_name)),
                uiOutput(paste0("mct_", col_name))
              )
            }
          })
        )
      } else {
        uiOutput("single_mct_result")
      }
    })
    
    observe({
      if (input$data_col == "All") {
        lapply(names(analysis_result), function(col_name) {
          local({
            col <- col_name
            output[[paste0("mct_", col)]] <- renderUI({
              if (is.character(analysis_result[[col]]$mct)) {
                p(analysis_result[[col]]$mct)
              } else {
                tableOutput(paste0("mct_table_", col))
              }
            })
            
            output[[paste0("mct_table_", col)]] <- renderTable({
              analysis_result[[col]]$mct
            }, rownames = FALSE, caption = paste("<b> Mean Comparison Test for", col, "</b>"), align = "c", caption.placement = "top")
          })
        })
      } else {
        output$single_mct_result <- renderUI({
          if (is.character(analysis_result[[input$data_col]]$mct)) {
            p(analysis_result[[input$data_col]]$mct)
          } else {
            tableOutput("single_mct_table_result")
          }
        })
        
        output$single_mct_table_result <- renderTable({
          analysis_result[[input$data_col]]$mct
        }, rownames = FALSE, caption = "<b> Mean Comparison Test </b>", align = "c", caption.placement = "top")
      }
    })
    
    #GROUPING
    output$grouping <- renderUI({
      if (input$data_col == "All") {
        tagList(
          lapply(names(analysis_result), function(col_name) {
            if (!is.null(analysis_result[[col_name]]$grouping)) {
              tagList(
                tags$h4(paste("Grouping for Column:", col_name)),
                tableOutput(paste0("grouping_", col_name))
              )
            }
          })
        )
      } else {
        tableOutput("single_grouping_result")
      }
    })
    
    observe({
      if (input$data_col == "All") {
        lapply(names(analysis_result), function(col_name) {
          local({
            col <- col_name
            output[[paste0("grouping_", col)]] <- renderTable({
              analysis_result[[col]]$grouping
            }, rownames = FALSE, caption = paste("<b> Grouping for", col, "</b>"), align = "c", caption.placement = "top")
          })
        })
      } else {
        output$single_grouping_result <- renderTable({
          analysis_result[[input$data_col]]$grouping
        }, rownames = FALSE, caption = "<b> Grouping </b>", align = "c", caption.placement = "top")
      }
    })
    
    # FINAL TABLE
    combine_final_tables <- function(analysis_result) {
      # Create a list of data frames with dynamically renamed columns
      tables <- lapply(names(analysis_result), function(col_name) {
        if (!is.null(analysis_result[[col_name]]$final_table)) {
          df <- analysis_result[[col_name]]$final_table
          col_name_from_df <- colnames(df)[2] # Dynamically get the column name from the table heading
          names(df)[2] <- paste(col_name_from_df, " (", col_name, ")", sep = "")
          df
        }
      })
      
      # Combine the data frames by merging them
      combined_table <- Reduce(function(x, y) {
        merge(x, y, by = "Treatment", all = TRUE)
      }, tables)
      
      special_rows_se <- combined_table %>% filter(Treatment == "SE (d)")
      
      special_rows_cd <- combined_table %>% filter(Treatment == "CD (p<0.05)")
      
      treatment_rows <- combined_table %>% filter(!Treatment %in% c("SE (d)", "CD (p<0.05)")) %>%
        mutate(TreatmentNumeric = ifelse(grepl("^T", Treatment), 
                                         as.numeric(gsub("T", "", Treatment)), 
                                         as.numeric(Treatment))) %>%
        arrange(TreatmentNumeric) %>% select(-TreatmentNumeric)
      
      combined_table <- bind_rows(treatment_rows, special_rows_se, special_rows_cd)
      
      return(combined_table)
    }
    
    # FINAL TABLE
    output$final_table_results <- renderUI({
      if (input$data_col == "All") {
        tableOutput("combined_final_table")
      } else {
        tableOutput("single_final_table_result")
      }
    })
    
    observe({
      if (input$data_col == "All") {
        output$combined_final_table <- renderTable({
          combined_table <- combine_final_tables(analysis_result)
          if (!is.null(combined_table)) {
            combined_table
          } else {
            data.frame(Message = "No final tables available to combine.")
          }
        }, rownames = FALSE, caption = "<b> Combined Final Table </b>", align = "c", caption.placement = "top")
      } else {
        output$single_final_table_result <- renderTable({
          final_table <- analysis_result[[input$data_col]]$final_table
          if (!is.null(final_table)) {
            col_name_from_df <- colnames(final_table)[2]
            colnames(final_table)[2] <- paste(col_name_from_df, " (", input$data_col, ")", sep = "")
            final_table
          } else {
            data.frame(Message = "No final table available for the selected column.")
          }
        }, rownames = FALSE, caption = "<b> Final Table </b>", align = "c", caption.placement = "top")
      }
    })
    
    output$all_results <- renderUI({
      if (input$data_col == "All") {
        tagList(
          lapply(names(analysis_result), function(col_name) {
            tagList(
              tags$h3(paste("Results for Column:", col_name)),
              tableOutput(paste0("anova_", col_name)),
              tableOutput(paste0("summary_", col_name)),
              uiOutput(paste0("mct_", col_name)),
              tableOutput(paste0("grouping_", col_name)),
              tableOutput(paste0("final_table_", col_name))
            )
          }),
          tableOutput("combined_final_table")
        )
      } else {
        tagList(
          tableOutput("analysis_results"),
          tableOutput("summary_results"),
          uiOutput("mct"),
          tableOutput("grouping"),
          tableOutput("single_final_table_result")
        )
      }
      
      
    })
    
    common_formatting <- function(ft) {
      ft %>%
        flextable::bold(part = "header") %>%
        flextable::align(align = "center", part = "all") %>%
        flextable::autofit() %>%
        flextable::set_table_properties(layout = "autofit") %>%
        flextable::font(fontname = "Times New Roman", part = "all") %>%
        flextable::fontsize(size = 11, part = "all") 
    }

    output$column_checkboxes <- renderUI({
      req(input$select_columns) 
      column_names <- names(analysis_result)
      checkboxGroupInput(
        "selected_columns",
        "Select Columns to Export:",
        choices = column_names,
        # selected = column_names[1:4]
      )
    })
    
    output$column_checkboxes <- renderUI({
      req(input$select_columns)
      checkboxGroupInput("selected_columns", 
                         "Select columns to export:", 
                         choices = names(analysis_result), 
                         selected = names(analysis_result))
    })
    
    output$export_word <- downloadHandler(
      filename = function() {
        paste("Analysis_Results_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        doc <- read_docx()
        
        columns_to_export <- if (input$select_columns) {
          input$selected_columns
        } else {
          names(analysis_result)
        }
        
        combined_final_table <- NULL
        if (input$include_CombinedFinalTable) {
          combined_final_table <- combine_final_tables(analysis_result)
        }
        
        for (col_name in columns_to_export) {
          col_result <- analysis_result[[col_name]]
          
          doc <- doc %>%
            body_add_par(paste("Results for Column:", col_name), style = "heading 1")
          
          if (!is.null(col_result$ANOVA) && input$include_ANOVA) {
            ft <- common_formatting(flextable(col_result$ANOVA))
            doc <- doc %>%
              body_add_par("ANOVA Table", style = "heading 2") %>%
              body_add_par(value = "", style = "Normal") %>%
              body_add_flextable(value = ft, align = "center") %>%
              body_add_par(value = "", style = "Normal")
          }
          
          if (!is.null(col_result$Summary) && input$include_Summary) {
            ft <- common_formatting(flextable(col_result$Summary))
            doc <- doc %>%
              body_add_par("Summary", style = "heading 2") %>%
              body_add_par(value = "", style = "Normal") %>%
              body_add_flextable(value = ft, align = "center") %>%
              body_add_par(value = "", style = "Normal")
          }
          
          if (!is.null(col_result$mct) && input$include_MCT) {
            doc <- doc %>%
              body_add_par("Mean Comparison Test", style = "heading 2")
            if (is.character(col_result$mct)) {
              doc <- doc %>%
                body_add_par(value = "", style = "Normal") %>%
                body_add_par(col_result$mct, style = "Normal")
            } else {
              ft <- common_formatting(flextable(col_result$mct))
              doc <- doc %>%
                body_add_par(value = "", style = "Normal") %>%
                body_add_flextable(value = ft, align = "center") %>%
                body_add_par(value = "", style = "Normal")
            }
          }
          
          if (!is.null(col_result$grouping) && input$include_Grouping) {
            ft <- common_formatting(flextable(col_result$grouping))
            doc <- doc %>%
              body_add_par("Grouping", style = "heading 2") %>%
              body_add_par(value = "", style = "Normal") %>%
              body_add_flextable(value = ft, align = "center") %>%
              body_add_par(value = "", style = "Normal")
          }
          
          if (!is.null(col_result$final_table) && input$include_FinalTable) {
            
            final_table <- col_result$final_table
            if (!is.null(final_table)) {
              col_name_from_df <- colnames(final_table)[2]
              colnames(final_table)[2] <- paste(col_name_from_df, " (", col_name, ")", sep = "")
            } else {
              final_table <- data.frame(Message = "No final table available for the selected column.")
            }
            ft_final <- common_formatting(flextable(final_table))
            doc <- doc %>%
              body_add_par("Final Table", style = "heading 2") %>%
              body_add_par(value = "", style = "Normal") %>%
              body_add_flextable(value = ft_final, align = "center") %>%
              body_add_par(value = "", style = "Normal")
          }
        }
        
        if (!is.null(combined_final_table) && input$include_CombinedFinalTable) {
          ft_combined <- common_formatting(flextable(combined_final_table))
          doc <- doc %>%
            body_add_par("Combined Final Table", style = "heading 1") %>%
            body_add_par(value = "", style = "Normal") %>%
            body_add_flextable(value = ft_combined, align = "center") %>%
            body_add_par(value = "", style = "Normal")
        }
        
        print(doc, target = file)
      }
    )
    
    
    # Server part - pdf --- to Update ----
    output$downloadTablePDF <- downloadHandler(
      filename = function() {
        paste("Analysis_Results_", Sys.Date(), ".pdf", sep = "")
        # paste("Analysis_Results_", Sys.Date(), ".html", sep = "")
      },
      content = function(file) {
        src <- normalizePath("01.Rmd")
        owd <- setwd(tempdir())  
        on.exit(setwd(owd))
        file.copy(src, "report.Rmd", overwrite = TRUE)
        out <- render("report.Rmd", output_file = file)
        file.rename(out, file)
      }
    )
    
    Sys.sleep(1)
    loading(FALSE) 
    shinyjs::hide("loading_output") 
    shinyjs::show("final_output")
    
  })
  
  
  output$final_output <- renderUI({
    if (!loading()) {
      "Here are the analysis results!"
    }
  })
  
  observeEvent(input$design, { clearResults() })
  observeEvent(input$treatment_col, { clearResults() })
  observeEvent(input$replication_col, { clearResults() })
  observeEvent(input$data_col, { clearResults() })
  observeEvent(input$multi_comp_test, { clearResults() })
  observeEvent(input$round_digits, { clearResults() })
  observeEvent(input$final_table, { clearResults() })
  
  clearResults <- function() {
    loading(FALSE)
    shinyjs::hide("loading_output")
    shinyjs::hide("final_output")
    output$all_results <- renderUI({
      tags$strong(style = "color: red;", "Press Analyze button again")
    })
  }
  
  
}

shinyApp(ui = ui, server = server)
