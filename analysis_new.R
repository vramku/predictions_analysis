library(tidyverse)
library(RSQLite)
library(rJava)
library(openxlsx)
source("BusDataDT.R")
source("Visualizer.R")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'nov9_bus_data.db', flags = SQLITE_RO)

#locobj <- BusData$new(con, 0)
expobj <- BusDataDT$new(con, 1)

drip_removal <- function (data) {
  #filter the data to locate "drips" by grouping by drip_cols
  data <- as.data.table(data)
  data[, ':=' (day = day(t_stamp))]
  drip_cols <- c("vehicle", "t_predicted", "stop_gtfs_seq", "day")
  drips <- data[, if(.N > 3) .SD[-(1:3)], by = drip_cols]
  data[, c("day") := NULL]
  
  #perform an antijoin of the data and drips tables
  setkey(data, vehicle, t_predicted, t_stamp, stop_gtfs_seq)
  setkey(drips, vehicle, t_predicted, t_stamp, stop_gtfs_seq)
  data_wo_drips <- data[!drips]
}

#locdata <- locobj$get_mod_data()[[1]]

#Possible memory leak. R crashes after iterating through several loops.
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
}
# #analyze gtfs stops 
# stop_cut <- function(db_con) {
#   min_stop <- as.numeric(dbGetQuery(db_con, "SELECT MIN(stop_gtfs_seq) FROM mta_bus_data"))
#   max_stop <- as.numeric(dbGetQuery(db_con, "SELECT MAX(stop_gtfs_seq) FROM mta_bus_data"))
#   analyzed_stop_grps <- vector('list', length = (max_stop - min_stop + 1))
#   for (stop in min_stop:max_stop) {
#     analyzed_stop_grps[[stop - min_stop + 1]] <- BusData$new(db_con, 1, stop_gtfs_seq = stop)
#   }
#   analyzed_stop_grps 
# }
#stop_grps_analyzed <- stop_cut(con)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'nov9_bus_data.db', flags = SQLITE_RO)
routes_vec <- as.list(dbGetQuery(con, "SELECT DISTINCT route FROM mta_bus_data;"))
dbDisconnect(con)

hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#4F80BD",
                  halign = "center", valign = "center", textDecoration = "Bold",
                  border = "TopBottomLeftRight", textRotation = 60, indent = 5, fontSize = 11)

for (inx in seq_len(length(routes_vec[[1]]))) {
  con <- dbConnect(drv, 'nov9_bus_data.db', flags = SQLITE_RO)
  
  bus_obj <- do.call(BusDataDT$new, list(con, route = routes_vec$route[inx], opt_func = drip_removal))
  grapher <- Visualizer$new(bus_obj)
  print("Generating graphs...")
  org_scatter <- grapher$get_scatter_basic("Original")
  rob_scatter <- grapher$get_scatter_basic("Robust")
  org_bar_plt <- grapher$get_dodged_bar("Original")
  summ_tbl    <- bus_obj$get_summary_table()
  whl_mod_tbl <- as.data.table(bus_obj$get_whole_mets(), keep.rownames = T)
  #close the db connection
  dbDisconnect(con)
  
  print("Exporting as xlsx...")
  if (exists("wb")) rm(wb)
  wb <- createWorkbook()
  addWorksheet(wb, routes_vec[[1]][inx])
  setColWidths(wb, 1, cols = 1:20, widths = 13)
  freezePane(wb, 1, firstActiveRow = 2)
  writeDataTable(wb, 1, summ_tbl, headerStyle = hs, startRow = 1, tableStyle = "TableStyleMedium1")
  writeDataTable(wb, 1, whl_mod_tbl, startRow = 30, startCol = "F", tableStyle = "TableStyleMedium1")
  
  print(org_scatter)
  insertPlot(wb, 1, xy = c("A", 36), width = 9.5, height = 8)
  
  print(rob_scatter)
  insertPlot(wb, 1, xy = c("K", 36), width = 9.5, height = 8)
  
  print(org_bar_plt)
  insertPlot(wb, 1, xy = c("A", 76), width = 19, height = 10)

  saveWorkbook(wb, file = paste0(routes_vec[[1]][inx], "_no_drips4.xlsx"))
  
  #release resources: first line clears the graphs, the second deletes objects
  if (!is.null(dev.list())) dev.off()
  rm(org_bar_plt, org_scatter, rob_scatter, bus_obj, grapher)
}

