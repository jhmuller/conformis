library(readxl)
library(tibble)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

# read in the data
# a bit messy since there are 2 rows of header
fpath <- "data_orig/VidScrip Statistical Analysis Data - June 2018.csv"

h1 <- readr::read_delim(fpath, delim=",", col_names = FALSE, n_max = 1, na=" ", quoted_na=FALSE, trim_ws=TRUE )
h2 <- readr::read_delim(fpath, delim=",", col_names = FALSE, n_max = 1, na=" ", quoted_na=FALSE, trim_ws=TRUE, skip=1 )

headr <- paste0(h1, "_", h2)
headr <- gsub("^_", "", headr)
headr <- gsub("_$", "", headr)
headr <- gsub(" ", "_", headr)
headr[headr == ""] <- "Unkn"

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
                            Total_All_Years = col_character(),
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

# replace na with 0 for numeric data
for (ci in seq_along(colnames(data))){
  col <- colnames(data)[ci]
  print (paste(ci, col, typeof(data[1,ci][[1]])))
  if (is.numeric(data[1,ci][[1]])){
    # replace na with 0
    data[is.na(data[col]), col] = 0
  }
}

# create "yearly" data for 2017 and 2018
data[,'2017'] <- data[ , "2017_Jan"] + data[,'2017_Feb'] + data[,'2017_Mar'] + 
  data[,'2017_Apr'] + data[,'2017_May'] + data[,'2017_Jun'] + 
  data[,'2017_Jul'] + data[,'2017_Aug'] + data[, '2017_Sep'] + data[, '2017_Oct'] + 
  data[, '2017_Nov'] + data[, '2017_Dec']
data[,'2018'] <- data[ , "2018_Jan"] + data[,'2018_Feb'] + data[,'2018_Mar'] + 
  data[,'2018_Apr'] + data[,'2018_May']

#  sum across all years
data[, 'AllYears'] <- data[, '2014'] + data[, '2015'] + data[, '2016']+ data[,'2017'] + data[, '2018']

# factors for alist and vscript
data[,"alist"] <- factor(data[[1]] )
data[, "vscript"] <-  as.factor(data[[2]])

# variable for start in vscript program
data[,'vscript_start'] <- as.Date(data$Date_Joined_Vidscrips, format="%m/%d/%y")
data$sid <- data$Stat_Analysis_Surgeon_ID

# set NA region to "Unknown" and make region a factor
data$US_Sales_Region[is.na(data$US_Sales_Region)] <- "Unknown"
data$region <- as.factor(data$US_Sales_Region)


ggplot(data, mapping=aes(x=region, y=AllYears)) + 
  #geom_boxplot() + facet_wrap(~vscript)
  geom_point(mapping=aes(x=region, y=AllYears, col=vscript, pch=alist), position="jitter")  + facet_wrap(~vscript) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))



yearly <- data %>%
  gather("Year", "Sales", "2014", "2015", "2016", "2017", "2018") %>%
  mutate(YYYY_MM_DD = paste0(Year,"_05_01" )) %>%
  mutate(date=as.Date(YYYY_MM_DD, format="%Y_%m_%d"))

ggplot(data=yearly, aes(x=vscript, y=Sales )) + geom_boxplot() + facet_wrap(~Year) + ggtitle("Sales in (1) and not in (0) Vscrip")
ggplot(data=yearly, aes(x=alist, y=Sales )) + geom_boxplot() + facet_wrap(~date)

yearly_vs <- yearly[yearly$vscript == 1,]

glimpse(yearly)

ggplot(yearly_vs , aes(x=date, y=Sales)) + 
  geom_line(aes(lty=vscript, col=alist, group=sid)) + 
  geom_point(aes(col=alist), alpha=0.2) + 
  geom_line(data=yearly[!is.na(yearly$vscript_start) & yearly$vscript_start <= yearly$date ,],
            mapping=aes(x=date, y=Sales, group=sid),lwd=2,col="red", alpha=0.2) +
  facet_wrap(~region)

df <- data %>% 
  gather("YYYY_MM", "Sales", "2017_Jan":"2018_May") %>%
  mutate(YYYY_MM_DD = paste0(YYYY_MM,"_1" )) %>%
  mutate(date=as.Date(YYYY_MM_DD, format="%Y_%b_%d"))
glimpse(df)
summary(df)

df$region <- as.factor(df$US_Sales_Region)
nrow(df)
df$vscript_started <- 0
df$vscript_started[ ! is.na(df$vscript_start)] <- df$vscript_start[!is.na(df$vscript_start)] <= df$date[! is.na(df$vscript_start)]  
df$vscript_started <- as.factor(df$vscript_started)
summary(df)

# vscript 
vs <- df[df$vscript == 1 & df$date >= as.Date("20180101", format="%Y%m%d"),]
summary(vs)
glimpse(vs[vs$sid == "ST408", ])
ggplot(data=vs, aes(y=Sales,  x=vscript_started, col=vscript_started)) + geom_point(position="jitter") + facet_wrap(~date)



ggplot(data=vs, aes(x=date, y=Sales)) + geom_line(aes(group=sid), col="blue") + 
   geom_line(data=vs[!is.na(vs$vscript_start) & vs$vscript_start <= vs$date ,],
             mapping=aes(x=date, y=Sales, group=sid),lwd=2,col="red", alpha=0.2) +
  geom_point(data=vs[!is.na(vs$vscript_start) & vs$vscript_start <= vs$date ,],
            mapping=aes(x=date, y=Sales, group=sid),lwd=2,col="red", alpha=0.2) +  
  geom_point(data=vs[!is.na(vs$vscript_start),],
            mapping=aes(x=date, y=Sales, group=sid),lwd=2,col="blue", alpha=0.2) +
  geom_point(data=vs,
             mapping=aes(x=vscript_start,y=10), position="jitter") +
  facet_wrap(~region)

reg <- group_by(df, date, region) %>%
  summarise(sales = sum(Value),
            cnt = n())



# plot sales by region
ggplot(reg, mapping=aes(x=date, y=sales, col=region)) + 
  geom_line(lwd=1) + facet_wrap(~region)


#
#  Regression model
#
  # id for each doctor
df$sid = as.factor(df$Stat_Analysis_Surgeon_ID)

lmod <- lm(Sales~region + alist + vscript + vscript_started + factor(YYYY_MM) + sid, data=df)
str(lmod)
coef(summary(lmod))
X <- model.matrix(~region + alist + vscript + vscript_started + factor(YYYY_MM) + sid, data=df)
Y <- matrix(df$Sales)
gmod <- cv.glmnet(x=X, y=Y)
lmod <- lm(x=X, y=Y)
str(gmod)
head(coefficients(gmod), 18)
fitted <- predict(gmod, newx=X)
ggplot() + geom_point(aes(x=df$Sales, y=fitted, col=df$alist, pch=df$vscript), alpha=0.5) + geom_abline(intercept = 0, slope=1)
coefficients(gmod)



#
# by REgion
#
reg <- group_by(df, date, region) %>%
  summarise(sales = sum(Value),
            cnt = n())
head(reg, 15)
library(ggplot2)

