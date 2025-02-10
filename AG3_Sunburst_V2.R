library(data.table)
library(highcharter)
library(dplyr)
library(gapminder)
library(ggplot2)
library(plotly)
library(tidyverse)
library(haven)
library(plyr)
library(stringi)
library(sqldf)
library(DBI)
library(RSQLite)



mydb <- dbConnect(RSQLite::SQLite(),"C:/Users/thodea/Desktop/2024-10-22_data_clin/2024-10-22_data_clin.db")
dbListTables(mydb)
Substanzen <- dbGetQuery(mydb, "SELECT * FROM Substanz")





Substanzen$Number <- 1

dout <- data_to_hierarchical(Substanzen, c(Lieferregister, Bezeichnung), Number)

hchart(dout, type = "sunburst")

df <- data.frame(matrix(unlist(dout), ncol = max(lengths(dout)), byrow = TRUE))

names(df) <- names(dout[[which(lengths(dout)>0)[1]]])

################################################################################


DF <- sqldf("select Lieferregister, Bezeichnung, count(Bezeichnung) as Anzahl from Substanzen group by Lieferregister, Bezeichnung")


as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE, drop_na_nodes = TRUE){
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  numeric_hierarchy_columns <- names(which(unlist(lapply(DT, is.numeric))))
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  for(current_column in setdiff(numeric_hierarchy_columns, c("root", value_column))){
    DT[, (current_column) := apply(.SD, 1, function(x){fifelse(is.na(x), yes = NA_character_, no = toTitleCase(gsub("_"," ", paste(names(x), x, sep = ": ", collapse = " | "))))}), .SDcols = current_column]
  }
  
  hierarchyList <- list()
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    
    setnames(currentDT, length(current_columns), "labels")
    currentDT[, depth := length(current_columns)-1]
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  if(drop_na_nodes){
    hierarchyDT <- na.omit(hierarchyDT, cols = "labels")
    parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", "depth", value_column))
    hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  } else {
    parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
    hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(x["depth"] == "0", yes = NA_character_, no = paste(x[seq(2, as.integer(x["depth"])+1)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  }
  
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(c(if(is.na(x["parents"])){NULL}else{x["parents"]}, x["labels"]), collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, union(parent_columns, "depth") := NULL]
  
  return(hierarchyDT)
}

sunburstDF <- as.sunburstDF(DF, value_column = "Anzahl", add_root = FALSE)

# sunburst
plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, parents = ~parents, values= ~values, type='sunburst', branchvalues = 'relative') # branchvalues = 'total'/ branchvalues = 'relative'








