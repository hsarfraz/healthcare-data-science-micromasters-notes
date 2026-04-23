##################
# .xlsx
##################
library(readxl)

# This section downloads an Excel dataset from GitHub and loads it into R.
# Since Excel files are binary (not plain text), they must be downloaded
# locally before being read using read_excel().

#convert github file URL to a raw URL link 
#(replace 'github.com' with 'raw.githubusercontent.com' and remove '/blob/')

# Original URL link:
#https://github.com/CambridgeICE-HDS/MSt-Healthcare-Data-Science/blob/main/Datasets/CDV%20dataset/CVD%20dataset.xlsx
# Raw URL link:
#https://raw.githubusercontent.com/CambridgeICE-HDS/MSt-Healthcare-Data-Science/main/Datasets/CDV%20dataset/CVD%20dataset.xlsx

file.url <- "https://raw.githubusercontent.com/CambridgeICE-HDS/MSt-Healthcare-Data-Science/main/Datasets/CDV%20dataset/CVD%20dataset.xlsx"

#creating a temporary file path in our computer to store the Excel file
tmp <- tempfile(fileext = ".xlsx")

#downloading the excel file and storing it in the temporary file path tmp
#'wb' (write binary) is required for excel files since they are binary 
download.file(file.url, tmp, mode = "wb")

#reading in the excel file & specifying the sheet we want 
DF <- read_excel(tmp, sheet = "cardio_train")

##################
# .csv
##################

library(readr)
file.url <- "https://raw.githubusercontent.com/CambridgeICE-HDS/MSt-Healthcare-Data-Science/refs/heads/main/Datasets/COVID-MS/GDSI_OpenDataset_Final.csv"

ddata <- read.csv(url(file.url))

DT::datatable(
  ddata,
  extensions = 'Buttons',
  options = list(
    paging = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'tB',
    buttons = c('copy', 'excel')
  ),
  class = "display"
)