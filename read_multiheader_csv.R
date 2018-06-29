library(readxl)
library(tibble)
library(readr)

fpath <- "data_orig/VidScrip Statistical Analysis Data - June 2018.csv"

h1 <- readr::read_delim(fpath, delim=",", col_names = FALSE, n_max = 1, na=" ", quoted_na=FALSE, trim_ws=TRUE )
h2 <- readr::read_delim(fpath, delim=",", col_names = FALSE, n_max = 1, na=" ", quoted_na=FALSE, trim_ws=TRUE, skip=1 )

headr <- paste0(h1, "_", h2)
headr <- gsub("^_", "", headr)
headr <- gsub("_$", "", headr)
headr <- gsub(" ", "_", headr)
headr[headr == ""] <- "Unkn"
headr


data <- readr::read_delim(fpath, delim=",", col_names = headr, trim_ws=TRUE, skip=2, 
                          col_types = cols(
                            `A-Listers_-_Listed_On_Conformis_Web_Site_Locator?_(Y=1)` = col_integer(),
                            `Vidscrips?_(Y=1)` = col_integer(),
                            Date_Joined_Vidscrips = col_character(),
                            VidScrips_Visitors_thru_May_31_2018 = col_integer(),
                            Vidscrips_Views_thru_May_31_2018 = col_number(),
                            Vidscrips_View_Hours_thru_May_31_2018 = col_double(),
                            Stat_Analysis_Surgeon_ID = col_character(),
                            `2014` = col_integer(),
                            `2015` = col_integer(),
                            `2016` = col_integer(),
                            `2017_Jan` = col_integer(),
                            `2017_Feb` = col_integer(),
                            `2017_Mar` = col_integer(),
                            `2017_Apr` = col_integer(),
                            `2017_May` = col_integer(),
                            `2017_Jun` = col_integer(),
                            `2017_Jul` = col_integer(),
                            `2017_Aug` = col_integer(),
                            `2017_Sep` = col_integer(),
                            `2017_Oct` = col_integer(),
                            `2017_Nov` = col_integer(),
                            `2017_Dec` = col_integer(),
                            `2018_Jan` = col_integer(),
                            `2018_Feb` = col_integer(),
                            `2018_Mar` = col_integer(),
                            `2018_Apr` = col_integer(),
                            `2018_May` = col_integer(),
                            Unkn = col_character(),
                            Total_All_Years = col_integer(),
                            `2017_Total` = col_character(),
                            `2017_as_%_of_Est._Total_Annual` = col_character(),
                            TTM_Total = col_integer(),
                            `TTM_as_%_of_Est._Total_Annual` = col_character(),
                            Monthly_Average_of_TTM = col_character(),
                            PKR = col_character(),
                            TKR = col_character(),
                            `Total_Knee_Replacements_Performed_(Medicare_data_for_any_company's_implant)` = col_character(),
                            `Estimated_Total_Knee_Replacements_Performed_(TKRs_X_2)` = col_character(),
                            US_Sales_Region = col_character(),
                            Sales_Territory = col_character(),
                            `Core-Based_Statistical_Area_(CBSA)` = col_character(),
                            City = col_character(),
                            County = col_character(),
                            State = col_character(),
                            ZIP = col_character(),
                            Primary_Network_Affiliation = col_character(),
                            Primary_Hospital_Affiliation = col_character()
                          ))
spec(data)
for (ci in seq_along(colnames(data))){
  col <- colnames(data)[ci]
  print (paste(ci, col, typeof(data[1,ci][[1]])))
  if (is.numeric(data[1,ci][[1]])){
    # replace na with 0
    data[is.na(data[col]), col] = 0
  }
}
summary(data)
data[,'2017'] <- data[ , "2017_Jan"] + data[,'2017_Feb'] + data[,'2017_Mar'] + 
  data[,'2017_Apr'] + data[,'2017_May'] + data[,'2017_Jun'] + 
  data[,'2017_Jul'] + data[,'2017_Aug'] + data[, '2017_Sep'] + data[, '2017_Oct'] + 
  data[, '2017_Nov'] + data[, '2017_Dec']
data[,'2018'] <- data[ , "2018_Jan"] + data[,'2018_Feb'] + data[,'2018_Mar'] + 
  data[,'2018_Apr'] + data[,'2018_May']

data[, 'AllYears'] <- data[, '2014'] + data[, '2015'] + data[, '2016']+ data[,'2017'] + data[, '2018']

summary(data)

data <- data[order(data$AllYears, decreasing = TRUE), ]
head(data)
glimpse(data)
data$region <- as.factor(data$US_Sales_Region)

ggplot(data, mapping=aes(x=region, y=AllYears)) + geom_boxplot() + ylim(c(0,500))

df <- data %>% 
  gather("YYYY_MM", "Value", "2017_Jan":"2018_May") %>%
  mutate(YYYY_MM_DD = paste0(YYYY_MM,"_1" )) %>%
  mutate(date=as.Date(YYYY_MM_DD, format="%Y_%b_%d"))
glimpse(df)
summary(df)

df$region <- as.factor(df$US_Sales_Region)
reg <- group_by(df, date, region) %>%
  summarise(sales = sum(Value),
            cnt = n())
head(reg, 15)
library(ggplot2)



+ facet_wrap(~US_Sales_Region)

ggplot(reg, mapping=aes(x=date, y=sales, col=region)) + 
  geom_line(lwd=1) + facet_wrap(~region)

ggplot(reg, mapping=aes(x=date, y=sales, col=region, lty=region)) + 
  geom_line(lwd=1) + 
  geom_point(aes(pch=region))
