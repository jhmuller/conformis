library(readxl)
library(tibble)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(glmnet)
library(forecast)
# read in the data
# a bit messy since there are 2 rows of header
fpath <- "data_orig/VidScrip Statistical Analysis Data - July 2018.xlsx"
sheets <- readxl::excel_sheets(fpath)

data <- readxl::read_xlsx(fpath, sheet=sheets[1], col_names=FALSE, col_types="text")

odir <- "data_derived"
dir.create(odir, showWarnings = FALSE)
opath <- file.path(odir, "VidScrip.csv")
write.table(data, opath, row.names=FALSE, col.names=FALSE, sep=",")
opath <- "data_orig/VidScrip Statistical Analysis Data - July 2018.csv"
h1 <- readr::read_delim(opath, delim=",", col_names = FALSE, n_max = 1, na=" ", quoted_na=FALSE, trim_ws=TRUE )
h2 <- readr::read_delim(opath, delim=",", col_names = FALSE, n_max = 1, na=" ", quoted_na=FALSE, trim_ws=TRUE, skip=1 )

headr <- paste0(h1, "_", h2)
headr <- gsub("^NA_", "", headr)
headr <- gsub("^_", "", headr)
headr <- gsub("_$", "", headr)
headr <- gsub(" ", "_", headr)
headr[headr == ""] <- "Unkn"

data <- readr::read_delim(opath, delim=",", col_names = headr, trim_ws=TRUE, skip=2)
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

# create "yearly" data 
for (year in c('2014', '2015', '2016', '2017', '2018')){
  data[, year] <- 0
  for (mon in c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
               'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')){
    yearmon <- paste0(year,"_", mon)
    if (yearmon %in% colnames(data)){
      #print(yearmon)
      data[,year] <- data[,year] + data[, yearmon]
    }
  }
}

# factors for alist and vscript
data[,"alist"] <- factor(data[[1]] )
data[, "vscript"] <-  as.factor(data[[2]])

# variable for start in vscript program
data[,'vscript_start'] <- as.Date(data$Date_Joined_Vidscrips, format="%m/%d/%y")
data$sid <- data$Stat_Analysis_Surgeon_ID

# set NA region to "Unknown" and make region a factor
data$US_Sales_Region[is.na(data$US_Sales_Region)] <- "Unknown"
data$region <- as.factor(data$US_Sales_Region)


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


# monthly

df <- data %>% 
  gather("YYYY_MM", "Sales", "2014_Jan":"2018_Jun") %>%
  mutate(month = gsub('20.._','', YYYY_MM)) %>%  
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

# in the vs program
vs <- df[df$vscript == 1, ]
sids <- unique(vs$sid)

diffsum <- 0
diff <- rep(0, length(sids))
for (si in seq(1, length(sids))){
  # si <- 2
  sid <- sids[si]
  ss <- vs[vs$sid == sid,]
  print(si)
  svec <- ss$Sales
  start <- which(svec!=0)[1]
  start_date <- as.character(ss$date[start])
  start_yyyy_mm <- strsplit(start_date, split="-")[[1]][1:2]
  start_vec0 = as.numeric(start_yyyy_mm)
  act = ts(svec[start:length(svec)], frequency=12,
           start=start_vec)

  svec0 <- ss$Sales[ss$vscript_started==0]
  if (length(svec0) < 12){
    next
  }
  act0 = ts(svec0[start:length(svec0)], frequency=12,
            start=start_vec0)
  start1 <- length(ss$Sales[ss$vscript_started==0]) + 1
  start_date <- as.character(ss$date[start1])
  start_yyyy_mm <- strsplit(start_date, split="-")[[1]][1:2]
  start_vec1 = as.numeric(start_yyyy_mm)
  act1= ts(ss$Sales[ss$vscript_started==1], frequency=12,
           start = start_vec1)
  if (length(act1) == 0){
    next
  }
  tsmod <- tryCatch(stats::HoltWinters(act0, gamma=FALSE), 
                    error=function(e) NULL,
                      finally=print("OK"))
  if (is.null(tsmod)){
    mtitle <- paste(sid, ": history, no forecast")
    plot(act, main=mtitle, sub=si)
    next
  }
  pred1 <- forecast(tsmod, h=length(act1))
  mtitle <- paste(sid, ": Pre-program(line)\n in-program forecast (blue)\nin-program(black circles)")
  plot(pred1, main=mtitle, sub=si, pch=4)
  points(act1, col="black", pch=19)
  #points(act1, col="black")
  z <- summary(pred1)
  fcst1 <- z$`Point Forecast`
  pvec <- svec[start:length(svec)]
  pvec[(length(pvec) - length(fcst1)+1):length(pvec)] <- fcst1
  points(ts(fcst1, start=start_vec1, frequency=12), col="blue", pch=19)
  diff[si] <- sum(fcst1 - as.vector(act1))
  diffsum <- diffsum + sum(diff[si])
}
diff
diffsum

ggplot() + geom_line(aes(x=seq(1, length(act1), y=act1)))
plot(tsmod)
predict.
ggplot(ss, aes(x=date, y=Sales)) + 
  geom_line() + 
  geom_point(data=ss[ss$vscript_started==1,], aes(x=date, y=Sales), col="blue")
ss
ss$vscript_start
#
#  Regression model
#
# id for each doctor
vsdf <- df[df$vscript==1,]
lmod <- lm(Sales~region + alist + vscript_started + factor(YYYY_MM) + sid, data=vsdf)
str(lmod)
head(coef(summary(lmod)), 27)
tail (coef(summary(lmod)), 27)
# glmnet
X <- model.matrix(~region + alist  + vscript_started + factor(YYYY_MM) + sid, data=vsdf)
Y <- matrix(vsdf$Sales)
gmod <- cv.glmnet(x=X, y=Y)

head(coefficients(gmod), 18)
fitted <- predict(gmod, newx=X)
ggplot() + geom_point(aes(x=vsdf$Sales, y=fitted, col=vsdf$alist, pch=vsdf$vscript), alpha=0.5) + geom_abline(intercept = 0, slope=1)






# vscript 
vs <- df[df$vscript == 1 ,]
summary(vs)
glimpse(vs[vs$sid == "ST408", ])
ggplot(data=vs, aes(y=Sales,  x=vscript_started, col=vscript_started)) + 
  geom_boxplot() + 
  geom_point(position="jitter", alpha=0.18) + facet_wrap(~month)

library(stm)


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
# by REgion
#
reg <- group_by(df, date, region) %>%
  summarise(sales = sum(Value),
            cnt = n())
head(reg, 15)
library(ggplot2)

