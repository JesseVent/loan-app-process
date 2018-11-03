library(shiny)
library(processanimateR)
library(eventdataR)
library(jsonlite)
library(timevis)
library(tidyverse)
library(bupaR)

  animationUI <- function(id, title) {
    ns <- NS(id)
    tagList(
      h2(title),
      processanimaterOutput(ns("process")),
      h4("Selected cases"),
      textOutput(ns("token_selection")),
      h4("Selected activities"),
      textOutput(ns("activity_selection")),
      fluidRow(
        h2(textOutput(ns("activity_title"))),
        column(4, h3("Resources"), verbatimTextOutput(ns("activity_count"))),
        column(4, h3("Processing Time"), verbatimTextOutput(ns("activity_pro_time"))),
        column(4, h3("Throughput Time"), verbatimTextOutput(ns("activity_thr_time"))),
        timevisOutput(ns("activity_timeline"))),
      fluidRow(
        h2(textOutput(ns("patient_selection"))),
        timevisOutput(ns("timeline")))
    )
  }

  animation <- function(input, output, session, ...) {
 eventlog <- patients
    output$token_selection <- renderText({
      if (is.null(input$process_tokens)) {
        "None"
      } else {
        paste0(input$process_tokens, collapse = ",")
      }
    })

    time_log <- reactive({
      tokens <- tail(input$process_tokens, 1)
      log <- eventlog %>% as.data.frame() %>%
        subset(patient == as.character(tokens)) %>%
        select(handling_id, handling, registration_type, time) %>%
        group_by(handling_id, handling) %>%
        spread(registration_type, time) %>%
        ungroup(handling_id, handling) %>%
        mutate(handling=as.character(handling))
      if(length(log) == 4) {
        names(log) <- c("id","content","end","start")
      }
      return(log)
    })

    output$activity_selection <- renderText({
      if (is.null(input$process_activities)) {
        "None"
      } else {
        activities <- jsonlite::fromJSON(input$process_activities)
        paste0("(", activities$id, ",", activities$activity, ")", collapse = ",")
      }
    })

    activity_log <- reactive({
      activities <- jsonlite::fromJSON(input$process_activities)
      log <-  patients %>% filter_activity(head(activities$activity, 1))
    })

    activity_time_log <- reactive({
      log <- activity_log() %>% as.data.frame() %>%
        select(handling_id, handling, registration_type, time) %>%
        group_by(handling_id, handling) %>%
        spread(registration_type, time) %>%
        ungroup(handling_id, handling) %>%
        mutate(handling=as.character(handling))
      if(length(log) == 4) {
        names(log) <- c("id","content","end","start")
      }
      return(log)
    })

    output$activity_title <- renderText({
      if (is.null(input$process_activities)) {
        "No Activity Selected"
      } else {
        act <- unique(activity_log()$handling)
        title <- paste("Summary details for", act)}
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
      counts <- paste("Absolute:",counts$absolute, "Relative:", counts$relative)
    })
    output$process <- renderProcessanimater(expr = {
      animate_process(eventlog, ...)
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
        paste("Patient",tail(input$process_tokens, 1), "Timeline")}
    })
  }

  ui <- fluidPage(
    animationUI("module", "Select Inputs")
  )

  server <- function(input, output, session) {
    callModule(animation, "module")
  }

  shinyApp(ui, server)

