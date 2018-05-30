library(RSQLite)
library(openxlsx)
source("BusData.R")
source("Visualizer.R")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, 'layover.db', flags = SQLITE_RO)
#layover_data <- as_tibble(dbGetQuery(con, "SELECT * FROM mta_bus_data WHERE phase = 'layover_during' and stop_gtfs_seq in (0, 1, 2);"))
layover_query <- "SELECT * FROM mta_bus_data WHERE phase = 'layover_during' and stop_gtfs_seq in (0, 1, 2);"

hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#4F80BD",
                  halign = "center", valign = "center", textDecoration = "Bold",
                  border = "TopBottomLeftRight", textRotation = 60, indent = 5, fontSize = 11)

layover_obj <- do.call(BusData$new, list(con, query = layover_query))
grapher <- Visualizer$new(layover_obj)
print("Generating graphs...")
org_scatter <- grapher$get_scatter_basic("Original")
rob_scatter <- grapher$get_scatter_basic("Robust")
org_bar_plt <- grapher$get_dodged_bar("Original")
summ_tbl    <- layover_obj$get_summary_table()

print("Exporting as xlsx...")
if (exists("wb")) rm(wb)
wb <- createWorkbook()
addWorksheet(wb, "Layover_Analysis")
setColWidths(wb, 1, cols = 1:20, widths = 13)
freezePane(wb, 1, firstActiveRow = 2)
writeDataTable(wb, 1, summ_tbl, headerStyle = hs, startRow = 1, tableStyle = "TableStyleMedium1")

print(org_scatter)
insertPlot(wb, 1, xy = c("A", 30), width = 9.5, height = 8)

print(rob_scatter)
insertPlot(wb, 1, xy = c("I", 30), width = 9.5, height = 8)

print(org_bar_plt)
insertPlot(wb, 1, xy = c("A", 72), width = 19, height = 10)

print(object.size(wb))
saveWorkbook(wb, file = "Layover_Analysis.xlsx")