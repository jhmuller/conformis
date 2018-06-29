library(readxl)
library(tibble)


fpath <- "data_orig/VidScrip Statistical Analysis Data - June 2018.xlsx"
sheets <- readxl::excel_sheets(fpath)
print(sheets)

odir <- "data_derived"
dir.create(odir, showWarnings = TRUE)

for (si in 1:length(sheets)) {
  sheet <- sheets[si]
  print(paste(si, sheet))
  df <- read_xlsx(fpath, sheet=sheet)
  sheet_name = gsub( " ", "_", sheet)
  tbl <- tbl_df(df)
  newpath <- file.path(odir, paste0(sheet_name,".csv"))
  write_csv(tbl, newpath)
}
