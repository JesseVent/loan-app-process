library(shiny)
library(processanimateR)
library(eventdataR)
library(jsonlite)
library(timevis)
library(tidyverse)
library(bupaR)
library(shinyWidgets)

animationUI <- function(id, title) {
  ns <- NS(id)
  tagList(
    h2(title),
    tags$h5(textOutput(ns("case_summary"))),
    tags$div(
      dropdown(
        tags$h3("Parameters"),
        noUiSliderInput(
          inputId = ns("percent"),
          label = "Activity Frequency",
          min = 0,
          max = 1,
          value = 0.75
        ),
        pickerInput(
          inputId = ns("type"),
          label = "Animation Type",
          choices = c("absolute", "relative","off"),
          selected = "relative"
        ),
        noUiSliderInput(
          inputId = ns("duration"),
          label = "Animation Duration",
          min = 0,
          max = 600,
          value = 120
        ),
        tags$h3("Layout Options"),
        radioGroupButtons(
          inputId = ns("orientation"),
          label = "Orientation",
          choices = c("LR", "TB"),
          selected = "LR"
        ),
        pickerInput(
          inputId = ns("splines"),
          label = "Spline Type",
          choices = c("line", "polyline", "curved", "ortho", "spline"),
          selected = "ortho"
        ),
        pickerInput(
          inputId = ns("layout"),
          label = "Layout",
          choices = c("neato", "dot", "fdp", "twopi", "circo"),
          selected = "dot"
        ),
        radioGroupButtons(
          inputId = ns("overlap"),
          label = "Overlap",
          choices = c("true", "false"),
          selected = "false"
        ),
        radioGroupButtons(
          inputId = ns("fixed"),
          label = "Fixed Size",
          choices = c("true", "false"),
          selected = "false"
        ),
        style = "unite",
        icon = icon("gear"),
        status = "danger",
        width = "300px",
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInLeftBig,
          exit = animations$fading_exits$fadeOutRightBig
        )
      )
    ),
    processanimaterOutput(ns("process")),
    h4("Selected cases"),
    textOutput(ns("token_selection")),
    h4("Selected activities"),
    textOutput(ns("activity_selection")),
    fluidRow(
      h2(textOutput(ns("activity_title"))),
      column(4, h3("Resources"), verbatimTextOutput(ns("activity_count"))),
      column(4, h3("Processing Time"), verbatimTextOutput(ns(
        "activity_pro_time"
      ))),
      column(4, h3("Throughput Time"), verbatimTextOutput(ns(
        "activity_thr_time"
      ))),
      timevisOutput(ns("activity_timeline"))
    ),
    fluidRow(h2(textOutput(
      ns("patient_selection")
    )),
    timevisOutput(ns("timeline")))
  )
}

animation <- function(input, output, session, ...) {

  patients <- eventdataR::patients

  data <- reactive({
    eventlog <- patients %>% filter_activity_frequency(percentage = input$percent)
    eventlog
  })

  output$case_summary <- renderText({
    all_cases <- patients %>% n_activities()
    filtered  <- data() %>% n_activities()
    percent   <- paste0(input$percent*100,"%")
  text <-  paste(percent, "of activities selected. Displaying", filtered,
                  "out of", all_cases, "activities.")
  return(text)
  })

  output$token_selection <- renderText({
    if (is.null(input$process_tokens)) {
      "None"
    } else {
      paste0(input$process_tokens, collapse = ",")
    }
  })

  time_log <- reactive({
    tokens <- tail(input$process_tokens, 1)
    log <- data() %>% as.data.frame() %>%
      subset(patient == as.character(tokens)) %>%
      select(handling_id, handling, registration_type, time) %>%
      group_by(handling_id, handling) %>%
      spread(registration_type, time) %>%
      ungroup(handling_id, handling) %>%
      mutate(handling = as.character(handling))
    if (length(log) == 4) {
      names(log) <- c("id", "content", "end", "start")
    }
    return(log)
  })

  output$activity_selection <- renderText({
    if (is.null(input$process_activities)) {
      "None"
    } else {
      activities <- jsonlite::fromJSON(input$process_activities)
      paste0("(",
             activities$id,
             ",",
             activities$activity,
             ")",
             collapse = ",")
    }
  })

  activity_log <- reactive({
    activities <- jsonlite::fromJSON(input$process_activities)
    log <-
      patients %>% filter_activity(head(activities$activity, 1))
  })

  activity_time_log <- reactive({
    log <- activity_log() %>% as.data.frame() %>%
      select(handling_id, handling, registration_type, time) %>%
      group_by(handling_id, handling) %>%
      spread(registration_type, time) %>%
      ungroup(handling_id, handling) %>%
      mutate(handling = as.character(handling))
    if (length(log) == 4) {
      names(log) <- c("id", "content", "end", "start")
    }
    return(log)
  })

  output$activity_title <- renderText({
    if (is.null(input$process_activities)) {
      "No Activity Selected"
    } else {
      act <- unique(activity_log()$handling)
      title <- paste("Summary details for", act)
    }
  })

  output$activity_pro_time <- renderText({
    req(input$process_activities)
    processing_time(activity_log())
  })
  output$activity_thr_time <- renderText({
    req(input$process_activities)
    throughput_time(activity_log())
  })
  output$activity_count <- renderText({
    req(input$process_activities)
    counts <- activity_presence(activity_log())
    counts <-
      paste("Absolute:",
            counts$absolute,
            "Relative:",
            counts$relative)
  })

  output$process <- renderProcessanimater(expr = {
    graph <-  processmapR::process_map(data(), performance(median, "days"), render = F)
    model <- DiagrammeR::add_global_graph_attrs(
      graph,
      # Different layouts are dot, neato, twopi, circo layouts, and curved instead of spline.
      attr = c("layout", "rankdir", "splines", "overlap", "fixedsize"),
      value = c(input$layout, input$orientation, input$splines, input$overlap, input$fixed),
      attr_type = c("graph", "graph", "graph", "graph", "graph")
    )
    animate_process(data(), model, mode = input$type, duration = input$duration)
  })

  output$timeline <- renderTimevis({
    req(input$process_tokens)
    time_log() %>% timevis(fit = TRUE)
  })

  output$activity_timeline <- renderTimevis({
    req(input$process_activities)
    max_date <- activity_time_log() %>%  summarise(max = max(end))
    max_date <- as.Date(max_date$max)
    activity_time_log() %>% timevis(fit = TRUE) %>% setWindow(max_date - 8, max_date)
  })

  output$patient_selection <- renderText({
    if (is.null(input$process_tokens)) {
      "No Patient Selected"
    } else {
      paste("Patient", tail(input$process_tokens, 1), "Timeline")
    }
  })
}

ui <- fluidPage(animationUI("module", "Select Inputs"))

server <- function(input, output, session) {
  callModule(animation, "module")
}

shinyApp(ui, server)
