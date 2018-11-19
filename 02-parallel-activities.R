library(tidyverse)
library(lubridate)
library(purrr)
library(xesreadR)
library(processanimateR)
library(processmapR)
library(bupaR)

remove_activity <- function(activity, event_log) {
  elog  <- event_log %>% as.data.frame() %>%
  select(case_id, rank, activity_id, activity_instance, resource_id, lifecycle_id, timestamp) %>%
  group_by(case_id, rank, activity_id, activity_instance, resource_id) %>%
  spread(lifecycle_id, timestamp) %>%
  arrange(case_id, rank) %>%
  ungroup() %>%
  group_by(case_id) %>%
  mutate(next_start = lead(start))

  elog$difference <- if_else(is.na(elog$next_start),
        difftime(elog$end, lag(elog$end), units = "mins"),
        difftime(elog$next_start, elog$start, units = "mins")) %>% round(digits = 1)
  return(elog)
}

#   ____________________________________________________________________________
#   Loan Application BPI17 Event Logs                                       ####

loan <- xesreadR::read_xes("loan_data_BPI17.xes")

log  <- loan %>% as.data.frame() %>%
  select(CASE_concept_name, activity_instance_id, activity_id, resource_id,
         lifecycle_id, timestamp) %>%
  group_by(CASE_concept_name, activity_instance_id, activity_id, resource_id) %>%
  spread(lifecycle_id, timestamp) %>%
  arrange(CASE_concept_name, activity_instance_id, starttimestamp) %>%
  ungroup()

names(log)      <- c("case_id", "activity_instance_id", "activity_id", "resource_id","end","start")
log$activity_id <- as.character(log$activity_id)
log$resource_id <- as.character(log$resource_id)

#   ____________________________________________________________________________
#   Merge activities starting at same time together                         ####
log <- log %>% group_by(case_id, start) %>%
  summarise(activity_instance_id = min(activity_instance_id),
            activity_id = paste(activity_id, collapse = " & "),
            resource_id = paste(unique(resource_id), collapse = " & "),
            end = max(end))

log$start    <- as.numeric(log$start)
log$end      <- as.numeric(log$end)

#   ____________________________________________________________________________
#   Combine overlapping activities into new activity                        ####

log_df       <- log %>% arrange(start) %>%
  group_by(case_id, rank = cumsum(cummax(lag(end, default = first(end))) < start)) %>%
  summarise(activity_instance_id = min(activity_instance_id),
            activity_id = paste(activity_id, collapse = " & "),
            resource_id = paste(unique(resource_id), collapse = " & "),
            start = first(start),
            end = max(end))

log_df$start <- as.POSIXct(log_df$start, origin = "1970-01-01", tz = "UTC")
log_df$end   <- as.POSIXct(log_df$end,   origin = "1970-01-01", tz = "UTC")

log_columns  <- c("case_id", "activity_instance_id", "activity_id", "resource_id",
                  "start", "end", "rank")

#   ____________________________________________________________________________
#   Convert to eventlog class                                               ####

event <- log_df %>% ungroup() %>%
  mutate(activity_instance = 1:nrow(.)) %>%
  gather(lifecycle_id, timestamp, start, end) %>%
  arrange(case_id, rank, desc(lifecycle_id)) %>%
  filter(!is.na(timestamp)) %>%
  eventlog(case_id       = "case_id",
    activity_id          = "activity_id",
    activity_instance_id = "activity_instance",
    lifecycle_id         = "lifecycle_id",
    timestamp            = "timestamp",
    resource_id          = "resource_id")

#   ____________________________________________________________________________
#   Generate animated process map                                           ####


events <- event %>% filter_activity_frequency(percentage = 0.7)
graph  <- processmapR::process_map(events, type_edges = performance(mean, units = "days"), render = F)
model  <- DiagrammeR::add_global_graph_attrs(graph,
                                            attr      = c("rankdir", "splines"),
                                            value     = c("LR", "ortho"),
                                            attr_type = c("graph", "graph"))

animate_process(events, model, mode = "relative", duration = 240)


df <- precedence_matrix(loan, type = "relative_consequent") %>% plot()
