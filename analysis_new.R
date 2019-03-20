library(tidyverse)
library(RSQLite)
library(rJava)
library(openxlsx)
source("BusData.R")
source("Visualizer.R")

#pass the name of SQlite3 database to dbConnect as a parameter
drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'nov9_bus_data.db', flags = SQLITE_RO)


#SQL query to select the data cut is passed as a parameter to dbGetQuery. Also, modify paramters within the following loops
routes_vec <- as.list(dbGetQuery(con, "SELECT DISTINCT route FROM mta_bus_data;"))
grapher <- Visualizer$new(expobj)

graph_list <- vector('list', length = length(routes_vec[[1]]))
rob_graph_list <- vector('list', length = length(routes_vec[[1]]))
bar_list <- vector('list', length = length(routes_vec[[1]]))
table_list <- vector('list', length = length(routes_vec[[1]]))

names(graph_list) <- routes_vec[[1]]
names(table_list) <- routes_vec[[1]]
names(rob_graph_list) <- routes_vec[[1]]
names(bar_list) <- routes_vec[[1]]

for (inx in seq_len(length(routes_vec[[1]]))) {
  bus_obj <- do.call(BusDataDT$new, list(con, route = routes_vec$route[inx]))
  grapher$set_bus_data(bus_obj)
  graph_list[[inx]] <- grapher$get_scatter_basic("Original")
  rob_graph_list[[inx]] <- grapher$get_scatter_basic("Robust")
  bar_list[[inx]] <- grapher$get_dodged_bar("Original")
  table_list[[inx]] <- bus_obj$get_summary_table()
}

hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#4F80BD",
                  halign = "center", valign = "center", textDecoration = "Bold",
                  border = "TopBottomLeftRight", textRotation = 60, indent = 5, fontSize = 11)

for (inx in seq_len(length(routes_vec[[1]]))) {
  if (exists("wb")) rm(wb)
  wb <- createWorkbook()
  addWorksheet(wb, routes_vec[[1]][inx])
  setColWidths(wb, 1, cols = 1:20, widths = 13)
  freezePane(wb, 1, firstActiveRow = 2)
  writeDataTable(wb, 1, table_list[[inx]], headerStyle = hs, startRow = 1, tableStyle = "TableStyleMedium1")
  print(graph_list[[inx]])
  insertPlot(wb, 1, xy = c("A", 30), width = 9.5, height = 8)
  print(rob_graph_list[[inx]])
  insertPlot(wb, 1, xy = c("I", 30), width = 9.5, height = 8)
  print(bar_list[[inx]])
  insertPlot(wb, 1, xy = c("A", 72), width = 19, height = 10)
  saveWorkbook(wb, file = paste0(routes_vec[[1]][inx], ".xlsx"))
  
  #release resources: first line clears the graphs, the second deletes objects
  if (!is.null(dev.list())) dev.off()
  rm(org_bar_plt, org_scatter, rob_scatter, bus_obj, grapher)
}


