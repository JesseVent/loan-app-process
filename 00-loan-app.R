library(tidyverse)
library(xesreadR)
library(processanimateR)
library(processmapR)

#   ____________________________________________________________________________
#   Loan Application Event Logs                                             ####

loan  <- xesreadR::read_xes("https://solutiondesign.io/blog/data/loan_app_data.xes")

graph <- processmapR::process_map(loan, type_edges = performance(mean), render = F)
model <- DiagrammeR::add_global_graph_attrs(graph,
            attr      = c("layout", "rankdir", "splines"),
            value     = c("dot", "LR", "ortho"),
            attr_type = c("graph", "graph", "graph"))

animate_process(loan, model, mode = "relative", duration = 240)


#   ____________________________________________________________________________
#   Traffic Fines Event Logs                                                ####

fines   <- eventdataR::traffic_fines

graph_f <- processmapR::process_map(fines, type_edges = performance(mean), render = F)
model_f <- DiagrammeR::add_global_graph_attrs(graph_f,
            attr      = c("layout", "rankdir", "splines"),
            value     = c("dot", "LR", "ortho"),
            attr_type = c("graph", "graph", "graph"))

animate_process(fines, model_f, mode = "relative", duration = 240)
