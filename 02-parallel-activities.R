library(zoo)
library(tidyverse)
library(xesreadR)
library(processanimateR)
library(processmapR)
library(lubridate)
#   ____________________________________________________________________________
#   Loan Application Event Logs                                             ####

loan      <- xesreadR::read_xes("loan_app_data.xes") %>% as.data.frame()
df        <- loan
df$.order <- NULL

log_df    <- df %>%
  group_by(CASE_concept_name, activity_instance_id, activity_id, resource_id) %>%
  spread(lifecycle_id, timestamp) %>%
  arrange(CASE_concept_name, activity_instance_id, starttimestamp) %>%
  ungroup()

names(log_df) <- c("case_id", "activity_instance_id", "activity_id", "resource_id","end","start")
log_df <- log_df[c("case_id", "activity_instance_id", "activity_id", "resource_id","start","end")]

log_df$activity_id <- as.character(log_df$activity_id)
log_df$resource_id <- as.character(log_df$resource_id)
log_df <- log_df %>% group_by(case_id) %>%
  mutate(fstart = lead(start),
         fend   = lead(end))

log_df$fstart    <- ifelse(is.na(log_df$fstart), log_df$start, log_df$fstart) %>% as_datetime()
log_df$fend      <- ifelse(is.na(log_df$fend), log_df$end, log_df$fend) %>% as_datetime()
log_df$period    <- interval(log_df$start, log_df$end)
log_df$fperiod   <- interval(log_df$fstart, log_df$fend)

ts_log           <- log_df
ts_log$overlap   <- int_overlaps(ts_log$period, ts_log$fperiod)
ts_log$aligns    <- int_aligns(ts_log$period, ts_log$fperiod)
ts_log$index     <- ""

ps               <- which(ts_log$overlap)
i                <- 1:length(ps)
ts_log$index     <- as.numeric()
ts_log$index[ps] <- i

ts_log$test      <- ts_log$index
ts_log$test      <- na.locf(ts_log$test, fromLast = TRUE)

series_seq = function(x, output) {
    from = x[1]
    to   = x[2]
    period <- seq(from, to)
}
periods$periods <- apply(periods, 1, series_seq)

