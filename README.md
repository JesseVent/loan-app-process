# Process Mining
## Loan Application Data

An example of parallel/negative duration processes, unsure on how to avoid this scenario.

### Code

```R

library(xesreadR)
library(processanimateR)
library(processmapR)

loan  <- xesreadR::read_xes("https://solutiondesign.io/blog/data/loan_app_data.xes")

graph <- processmapR::process_map(loan, type_edges = performance(mean), render = F)
model <- DiagrammeR::add_global_graph_attrs(graph,
            attr      = c("layout", "rankdir", "splines"),
            value     = c("dot", "LR", "ortho"),
            attr_type = c("graph", "graph", "graph"))

animate_process(loan, model, mode = "relative", duration = 240)

```

### Output

![Loan Process Map](process.gif)
