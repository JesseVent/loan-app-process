library(tidyverse)
library(lubridate)
library(purrr)
library(xesreadR)
library(processanimateR)
library(processmapR)
library(bupaR)

#   ____________________________________________________________________________
#   Ranks sequence of overlapping events                                    ####

rank_sequence <- function(x) {
    iterations <- seq_len(nrow(x))
    rank       <- as.numeric(1)
    x$lag      <- lag(x$overlap)
    na         <- which(is.na(x$lag))
    x$lag[na]  <- x$overlap[na]
  for (i in iterations) {
      if (x$overlap[i] != x$lag[i]) {
        rank[i] <- max(rank) + 1
      } else {
        rank[i] <- max(rank)
        }
   }
  return(rank)
}

#   ____________________________________________________________________________
#   Loan Application BPI17 Event Logs                                       ####

loan      <- xesreadR::read_xes("loan_app_data.xes")

log_df <- loan %>% as.data.frame()
  select(CASE_concept_name, activity_instance_id, activity_id, resource_id, lifecycle_id, timestamp) %>%
  group_by(CASE_concept_name, activity_instance_id, activity_id, resource_id) %>%
  spread(lifecycle_id, timestamp) %>%
  arrange(CASE_concept_name, activity_instance_id, starttimestamp) %>%
  ungroup()

names(log_df) <- c("case_id", "activity_instance_id", "activity_id", "resource_id","end","start")
log_df <- log_df[c("case_id", "activity_instance_id", "activity_id", "resource_id","start","end")]
log_df$activity_id <- as.character(log_df$activity_id)
log_df$resource_id <- as.character(log_df$resource_id)

#   ____________________________________________________________________________
#   Identify overlapping activities                                         ####

log_df <- log_df %>% group_by(case_id) %>%
  mutate(fstart = lead(start),
         fend   = lead(end))

log_df$fstart    <- ifelse(is.na(log_df$fstart), log_df$start, log_df$fstart) %>% as_datetime()
log_df$fend      <- ifelse(is.na(log_df$fend), log_df$end, log_df$fend) %>% as_datetime()
log_df$period    <- interval(log_df$start, log_df$end)
log_df$fperiod   <- interval(log_df$fstart, log_df$fend)
log_df$overlap   <- int_overlaps(log_df$period, log_df$fperiod)
log_df$aligns    <- int_aligns(log_df$period, log_df$fperiod)

#   ____________________________________________________________________________
#   Rank sequence of activities                                             ####

log_df$rank <- log_df %>%
  split(.$case_id) %>%
  purrr::map(rank_sequence) %>%
  unlist()

#   ____________________________________________________________________________
#   Combine overlapping activities into new activity                        ####

log_columns <- c("case_id", "activity_instance_id", "activity_id", "resource_id","start","end","rank")

event <- log_df %>% select(log_columns) %>%
  group_by(case_id, rank) %>%
  summarise(activity_instance_id = min(activity_instance_id),
            activity_id = paste(activity_id, collapse = " & "),
            resource_id = paste(resource_id, collapse = " & "),
            start = min(start),
            end = max(start))

#   ____________________________________________________________________________
#   Convert to eventlog class                                               ####

event <- event %>% ungroup() %>% mutate(activity_instance = 1:nrow(.)) %>%
  gather(status, timestamp, start, end) %>%
  arrange(case_id, rank, desc(status)) %>%
  filter(!is.na(timestamp)) %>%
  eventlog(
    case_id              = "case_id",
    activity_id          = "activity_id",
    activity_instance_id = "activity_instance",
    lifecycle_id         = "status",
    timestamp            = "timestamp",
    resource_id          = "resource_id"
  )

#   ____________________________________________________________________________
#   Generate animated process map                                           ####

events <- event %>% filter_activity_frequency(percentage = 0.5)
graph <- processmapR::process_map(events, type_edges = performance(mean, units = "days"), render = F)
model <- DiagrammeR::add_global_graph_attrs(graph,
                                            attr      = c("layout", "rankdir", "splines"),
                                            value     = c("dot", "LR", "ortho"),
                                            attr_type = c("graph", "graph", "graph"))

animate_process(events, model, mode = "relative", duration = 240)
