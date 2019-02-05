library(visNetwork)
library(tidyverse)
library(eventdataR)
library(processmapR)

# Mock data ---------------------------------------------------------------

data <- tibble::tibble(measure_id = c("Antwerp", "Antwerp", "Antwerp", "Antwerp",
  "Antwerp", "Antwerp", "Antwerp", "Antwerp", "Antwerp", "Antwerp",
  "Antwerp", "Antwerp", "Antwerp", "Haarlem", "Antwerp", "Antwerp",
  "Haarlem", "Antwerp", "Haarlem", "Antwerp", "Antwerp",
  "Antwerp", "Antwerp", "Antwerp", "Antwerp", "Antwerp", "Antwerp",
  "Antwerp", "Antwerp", "Haarlem", "Antwerp", "Haarlem", "Antwerp",
  "Antwerp", "Antwerp", "Antwerp", "Haarlem", "Dordrecht", "Antwerp",
  "Antwerp", "Antwerp", "Antwerp", "Antwerp", "Antwerp",
  "Antwerp", "Haarlem", "Antwerp", "Antwerp", "Antwerp", "Antwerp",
  "Antwerp", "Antwerp", "Haarlem", "Antwerp", "Haarlem", "Antwerp",
  "Antwerp", "Antwerp", "Haarlem", "Antwerp", "Haarlem", "Antwerp",
  "Haarlem", "Antwerp", "Antwerp", "Antwerp", "Antwerp",
  "Antwerp", "Haarlem", "Antwerp", "Antwerp", "Antwerp", "Antwerp",
  "Venice", "Antwerp", "Antwerp", "Antwerp", "Antwerp", "Antwerp",
  "Antwerp", "Antwerp", "Antwerp", "Lisse", "Antwerp", "Antwerp",
  "Haarlem", "Haarlem", "Haarlem", "Haarlem", "Antwerp",
  "Haarlem", "Haarlem", "Antwerp", "Antwerp", "Haarlem", "Haarlem",
  "Antwerp", "Venice", "Haarlem", "Haarlem", "Antwerp", "Haarlem",
  "Haarlem", "Haarlem", "Haarlem", "Haarlem", "Antwerp", "Antwerp",
  "Het Vlie", "Hamburg", "Emden", "Haarlem", "Amsterdam", "Haarlem"),
table_name = c("Delft", "Haarlem", "Haarlem", "Delft", "Haarlem",
  "Delft", "Delft", "Delft", "Haarlem", "Delft", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "The Hague", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Haarlem", "Delft",
  "Haarlem", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Middelburg", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Middelburg", "The Hague", "Delft", "The Hague",
  "Delft", "Haarlem", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Haarlem", "Delft", "Delft",
  "Delft", "Delft", "Delft", "Delft", "Delft", "Delft", "Delft",
  "Delft", "Bremen", "Bremen", "Bremen", "Bremen", "Bremen", "Bremen"))

# Create nodelist ---------------------------------------------------------
measures <- tibble::tibble(label = unique(data$measure_id))
tables   <- tibble::tibble(label = unique(data$table_name))
nodes    <- full_join(measures, tables, by = "label") %>% mutate(id = row_number(.))

# Create edgelist ---------------------------------------------------------
edges <- data %>%
  group_by(measure_id, table_name) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  left_join(nodes, by = c("measure_id" = "label")) %>%
  rename(from = id) %>%
  left_join(nodes, by = c("table_name" = "label")) %>%
  rename(to = id) %>%
  select(from, to, weight)

# visNetwork Diagram ------------------------------------------------------
visNetwork(nodes, edges, width = "100%")

# With width parameter
edges$width <- edges$weight/5 + 1

visNetwork(nodes, edges, width = "100%")


# Event log visNetwork ----------------------------------------------------

patients   <- eventdataR::patients
edea_graph <- process_map(patients, type_edges = performance(mean), render = F)
edea_nodes <- edea_graph$nodes_df %>% as_tibble()
edea_edges <- edea_graph$edges_df %>% as_tibble()

visNetwork(edea_nodes, edea_edges)

visNetwork(edea_nodes, edea_edges) %>%
  visHierarchicalLayout(nodeSpacing = 50)

visNetwork(edea_nodes, edea_edges) %>%
  visLayout(randomSeed = 123, improvedLayout = TRUE)

visNetwork(edea_nodes, edea_edges) %>%
  visLayout(randomSeed = 123, improvedLayout = FALSE)

visNetwork(edea_nodes, edea_edges) %>%
  visLayout(hierarchical = TRUE)

visNetwork(edea_nodes, edea_edges) %>%
  visHierarchicalLayout(direction = "LR")
