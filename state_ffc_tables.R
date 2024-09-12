# TABLES (GT)



# Create Data Object-----------------------------------------------------

state_ffc_tables <-lst(

table_2_1 <- read_excel("data/state_report_tables.xlsx", sheet = 1) %>%
  gt() %>%
  tab_header(
    title = "Table 2-1. Overview of Approaches for Estimating State-Level Energy Sector GHG Emissions") %>%
  opt_row_striping() %>%
  sub_missing(missing_text = " ") %>%
  text_replace(pattern = "CH4", replacement = ("CH<sub>4</sub>")) %>%
  text_replace(pattern = "CO2", replacement = ("CO<sub>2</sub>")) %>%
  text_replace(pattern = "N2O", replacement = ("N<sub>2</sub>O")) %>%
  tab_options(
    heading.border.bottom.style = "solid", 
    heading.border.bottom.color = "black", 
    heading.title.font.weight = "bold", 
    heading.align = "center", 
    column_labels.background.color = "steelblue",
    column_labels.border.bottom.style = "solid", 
    column_labels.border.bottom.color = "black", 
    column_labels.border.bottom.width = 3,
    table.align = "center", 
    row.striping.include_stub = TRUE,
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "lightsteelblue1") %>%
  tab_style(style = cell_text(align = "left"), 
            location = list(cells_body())) %>%
  tab_style(style = cell_text(align = "center"), 
            location = list(cells_column_labels())) %>%
  opt_footnote_marks(marks = "letters") %>%
  tab_footnote(footnote = "Emissions are not likely occurring in U.S. territories; due to a lack of available data and the nature of this category, territories not listed are not estimated.", 
               locations = list(cells_column_labels(columns = 4), 
                                cells_body(columns = 4, rows = c(3, 4, 6)))),

table_2_2 <- read_excel("data/state_report_tables.xlsx", sheet = 2) %>%
  # Add grouping columns for the gt table
  mutate(group = c(rep("Determine Activity Data", times = 7), 
                   rep("Calculate CO2 Emissions", times = 3))) %>%
  gt(groupname_col = "group") %>%
  tab_header(
    title = "Table 2-2.  Comparison of Approaches/Data Sources Used to Determine FFC Emissions") %>%
  opt_row_striping() %>%
  # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
  sub_missing(missing_text = " ") %>%
  text_replace(pattern = "CH4", replacement = ("CH<sub>4</sub>")) %>%
  text_replace(pattern = "CO2", replacement = ("CO<sub>2</sub>")) %>%
  text_replace(pattern = "N2O", replacement = ("N<sub>2</sub>O")) %>%
  tab_options(
    heading.border.bottom.style = "solid", 
    heading.border.bottom.color = "black", 
    heading.title.font.weight = "bold", 
    heading.align = "center", 
    column_labels.background.color = "steelblue",
    column_labels.border.bottom.style = "solid", 
    column_labels.border.bottom.color = "black", 
    column_labels.border.bottom.width = 3,
    row_group.background.color = "lightsteelblue3",
    table.align = "center", 
    row.striping.include_stub = TRUE,
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "lightsteelblue1") %>%
  tab_style(style = cell_text(align = "left"), 
            location = list(cells_body())) %>%
  tab_style(style = cell_text(align = "center"), 
            location = list(cells_column_labels(), cells_row_groups())),

table_2_3 <- read_excel("data/state_report_tables.xlsx", sheet = 3) %>%
  gt(groupname_col = "Source/Category", row_group_as_column = TRUE) %>%
  tab_header(
    title = md("Table 2-3. Default Data Sources for Mobile Source Non-CO<sub>2</sub> Emissions")) %>%
  opt_row_striping() %>%
  # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
  sub_missing(missing_text = " ") %>%
  text_replace(pattern = "CH4", replacement = ("CH<sub>4</sub>")) %>%
  text_replace(pattern = "CO2", replacement = ("CO<sub>2</sub>")) %>%
  text_replace(pattern = "N2O", replacement = ("N<sub>2</sub>O")) %>%
  tab_stubhead(label = "Source/Category") %>%
  tab_options(
    heading.border.bottom.style = "solid", 
    heading.border.bottom.color = "black", 
    heading.title.font.weight = "bold", 
    heading.align = "center", 
    column_labels.background.color = "steelblue",
    column_labels.border.bottom.style = "solid", 
    column_labels.border.bottom.color = "black", 
    column_labels.border.bottom.width = 3,
    table.align = "center", 
    row.striping.include_stub = TRUE,
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "lightsteelblue1") %>%
  tab_style(style = cell_text(align = "left"), 
            location = list(cells_body())) %>%
  tab_style(style = cell_text(align = "center"), 
            location = list(cells_column_labels(), cells_row_groups())) %>%
  tab_style(style = cell_fill(color = "lightsteelblue1"), 
            location = list(cells_row_groups())),

table_2_4 <- read_excel("data/state_report_tables.xlsx", sheet = 4) %>%
  gt() %>%
  tab_header(
    title = md("Table 2-4: Summary of Approaches to Disaggregate Waste Incineration Emissions Across Time Series")) %>%
  opt_row_striping() %>%
  # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
  sub_missing(missing_text = " ") %>%
  text_transform(locations = cells_body(column = `Summary of Data Used`), 
                 fn = function(x) {
                   paste("• ", x) }) %>%
  tab_options(
    heading.border.bottom.style = "solid", 
    heading.border.bottom.color = "black", 
    heading.title.font.weight = "bold", 
    heading.align = "center", 
    column_labels.background.color = "steelblue",
    column_labels.border.bottom.style = "solid", 
    column_labels.border.bottom.color = "black", 
    column_labels.border.bottom.width = 3,
    table.align = "center", 
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "lightsteelblue1") %>%
  tab_style(style = cell_text(align = "left"), 
            location = list(cells_body())) %>%
  tab_style(style = cell_text(align = "center"), 
            location = list(cells_column_labels())),

table_2_4 <- read_excel("data/state_report_tables.xlsx", sheet = 4) %>%
  gt() %>%
  tab_header(
    title = md("Table 2-4: Summary of Approaches to Disaggregate Waste Incineration Emissions Across Time Series")) %>%
  opt_row_striping() %>%
  # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
  sub_missing(missing_text = " ") %>%
  text_transform(locations = cells_body(column = `Summary of Data Used`), 
                 fn = function(x) {
                   paste("• ", x) }) %>%
  tab_options(
    heading.border.bottom.style = "solid", 
    heading.border.bottom.color = "black", 
    heading.title.font.weight = "bold", 
    heading.align = "center", 
    column_labels.background.color = "steelblue",
    column_labels.border.bottom.style = "solid", 
    column_labels.border.bottom.color = "black", 
    column_labels.border.bottom.width = 3,
    table.align = "center", 
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "lightsteelblue1") %>%
  tab_style(style = cell_text(align = "left"), 
            location = list(cells_body())) %>%
  tab_style(style = cell_text(align = "center"), 
            location = list(cells_column_labels()))

# table 2-5 must be populated with data; 
# where is this data from? Ask Vince
)