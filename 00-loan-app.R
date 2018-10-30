library(tidyverse)
library(xesreadR)
library(processanimateR)
library(processmapR)

loan  <- xesreadR::read_xes("loan_app_data.xes")

graph <- processmapR::process_map(loan, type_edges = performance(mean), render = F)
model <- DiagrammeR::add_global_graph_attrs(graph,
            attr      = c("layout", "rankdir", "splines"),
            value     = c("dot", "LR", "ortho"),
            attr_type = c("graph", "graph", "graph"))

animate_process(loan, model, mode = "relative", duration = 240)
