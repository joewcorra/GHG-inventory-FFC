# Datascraper

# Libraries--------------------------------------------------------------

library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)
library(rvest)
library(readxl)

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# API Keys--------------------------------------------------------------

key <- "" 

# Extract Excel files-----------------------------------------------

faa <- "https://www.faa.gov/headquartersoffices/apl/aee/icao-airplane-co2-certification-database"
# Then find elements that match a css selector using html_elements()


# Use XPath or CSS selector to find links containing 'Excel' in a child <small> element
# XPath example: '//a[.//small[contains(text(), "Excel")]]'
# CSS example: 'a:has(small:contains("Excel"))'
excel_links <- read_html(faa) %>%
  html_elements(css = 'a:contains("DB")') %>%
  html_attr("href")

# Function to download and read Excel files
read_excel_from_url <- function(url) {
  
  GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  read_excel(tf, sheet = 3)
  
}

# Apply the function to each URL and combine the results
combined_data <- excel_links %>%
  map_df(url_absolute(excel_links, faa), ~ read_excel_from_url(.))

# 'combined_data' is now your combined data frame


a <- read_excel_from_url("https://www.faa.gov/media/70966")

# Training Stuff from hadley-wickham

html <- read_html("http://rvest.tidyverse.org/")
html

html <- minimal_html("
  <h1>This is a heading</h1>
  <p id='first'>This is a paragraph</p>
  <p class='important'>This is an important paragraph</p>
")

html %>% html_elements("p")
html %>% html_elements(".important")
html %>% html_elements("#first")
html %>% html_element("p")

# Use html_elements() and html_element() together, typically 
# using html_elements() to identify elements that will become observations 
# then using html_element() to find elements that will become variables.

# Fake html data
html <- minimal_html("
  <ul>
    <li><b>C-3PO</b> is a <i>droid</i> that weighs <span class='weight'>167 kg</span></li>
    <li><b>R4-P17</b> is a <i>droid</i></li>
    <li><b>R2-D2</b> is a <i>droid</i> that weighs <span class='weight'>96 kg</span></li>
    <li><b>Yoda</b> weighs <span class='weight'>66 kg</span></li>
  </ul>
  ")

# Use html_elements() to make a vector where each element corresponds 
# to a different character:
characters <- html %>% html_elements("li")
characters

# To extract the name of each character, we use html_element(), because when 
# applied to the output of html_elements() it’s guaranteed to return 
# one response per element:
characters %>% html_element("b")
# The distinction between html_element() and html_elements() isn’t important 
# for name, but it is important for weight. We want to get one weight for each 
# character, even if there’s no weight <span>. That’s what html_element() does:
characters %>% html_element(".weight") # Retains empty index 
# Compare these results with html_elements()
characters %>% html_elements(".weight") # Skips empty values. 
# html_text2() extracts the plain text contents of an HTML element
characters %>% html_element("b") %>% html_text2()
#  html_attr() extracts data from attributes; always returns a string
html <- minimal_html("
  <p><a href='https://en.wikipedia.org/wiki/Cat'>cats</a></p>
  <p><a href='https://en.wikipedia.org/wiki/Dog'>dogs</a></p>")
html %>% 
  html_elements("p") %>% 
  html_element("a") %>% 
  html_attr("href")

# Tables
# If you’re lucky, your data will be already stored in an HTML table
html <- minimal_html("
  <table class='mytable'>
    <tr><th>x</th>   <th>y</th></tr>
    <tr><td>1.5</td> <td>2.7</td></tr>
    <tr><td>4.9</td> <td>1.3</td></tr>
    <tr><td>7.2</td> <td>8.1</td></tr>
  </table>
  ")
# html_table() returns a list containing one tibble for each table 
# on the page. Use html_element() to identify the table you want to extract
html %>%
  html_element(".mytable") %>%
  html_table()

# Finding the right selectors
# This is the hard part. Often requires trial and error
# Try the SelectorGadget 
# https://rvest.tidyverse.org/articles/selectorgadget.html

