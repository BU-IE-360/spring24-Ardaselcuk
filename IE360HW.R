library(data.table)
library(forecast)
library(GGally)
library(ggplot2)
library(graphics)
library(grDevices)
library(readr)
library(stats)
library(utils)

Monthly_USD_Reserves_million_ <- read_csv("~/Desktop/University/Junior/Semester 2/IE360/HW1/Monthly USD Reserves (million).csv", 
                                          col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                           `Bank USD Assets (Million)` = col_number()))
View(Monthly_USD_Reserves_million_)

Building_Sold_Turkey <- read_delim("Building Sold Turkey.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                                                        Buildings = col_number()), trim_ws = TRUE)
View(Building_Sold_Turkey)

Confidence_Index <- read_delim("Confidence Index.csv", 
                               delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                                                    Confidence = col_number()), trim_ws = TRUE)
View(Confidence_Index)

Foreign_Loan_USD_Million <- read_delim("Foreign Loan USD Million.csv", 
                                       delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                                                            Loan = col_number()), trim_ws = TRUE)
View(Foreign_Loan_USD_Million)

Employement_Rates <- read_csv("Employement Rates.csv", 
                              col_types = cols(Date = col_date(format = "%Y-%m"), 
                                               `Employement Rate` = col_number()))
View(Employement_Rates)

MA_Deposit_Interest_Rate <- read_delim("MA Deposit Interest Rate.csv", 
                                       delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                                                            `Deposit Interest Rate` = col_number()), 
                                       trim_ws = TRUE)
View(MA_Deposit_Interest_Rate)

Traded_Stocks <- read_delim("Traded Stocks.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                                                 `Traded Stocks (Thousand)` = col_number()), 
                            trim_ws = TRUE)
View(Traded_Stocks)

Gu_mru_k <- read_csv("Gümrük.csv", col_types = cols(Week = col_date(format = "%Y-%m-%d"), 
                                                      `gümrük: (Türkiye)` = col_number()), 
                     skip = 1)
View(Gu_mru_k)

House_for_Sale <- read_csv("House for Sale.csv", 
                           col_types = cols(Week = col_date(format = "%Y-%m-%d"), 
                                            `satılık ev: (Türkiye)` = col_number()), 
                           skip = 1)
View(House_for_Sale)

Kira <- read_csv("Kira.csv", col_types = cols(Week = col_date(format = "%Y-%m-%d"), 
                                              `kira: (Türkiye)` = col_number()), skip = 1)
View(Kira)

Housing_Loan_Interest <- read_delim("Housing Loan Interest.csv", 
                                    delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                                                         `Interest on Housing Loans` = col_number()), 
                                    trim_ws = TRUE)
View(Housing_Loan_Interest)

Monthly_USD_Exchange_Rates <- read_csv("Monthly USD Exchange Rates.csv", 
                                       col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                        `Exchange Rate` = col_number()))
View(Monthly_USD_Exchange_Rates)

TUFE_data <- read_delim("TUFE_data.csv", 
                        delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                                             TUFE = col_number()), trim_ws = TRUE)
View(TUFE_data)

CD_Card_Expenditure <- read_delim("CD Card Expenditure.csv", 
                                  delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m"), 
                                                                                       `Expenditures (Thousand TL)` = col_number()), 
                                  trim_ws = TRUE)
View(CD_Card_Expenditure)

Zam <- read_csv("Zam.csv", col_types = cols(Week = col_date(format = "%Y-%m-%d"), 
                                            `zam: (Türkiye)` = col_number()), skip = 1)
View(Zam)

Monthly_USD_Reserves <- data.table(Dates = as.Date(Monthly_USD_Reserves_million_$Date), Reserves = as.numeric(Monthly_USD_Reserves_million_$`Bank USD Assets (Million)`))
Monthly_USD_Reserves
str(Monthly_USD_Reserves_million_)
Monthly_USD_Reserves_million_$Reserves <- ts(Monthly_USD_Reserves_million_[,2], start = 2020, frequency = 12)
ggplot(Monthly_USD_Reserves_million_, aes(x = Date, y = Reserves)) + xlab("Date") + ylab("Reserve Assets (million USD)") + ggtitle("Monthly USD Reserve Assets (million)") + geom_line()

Houses_Sold_Turkey <- data.table(Dates = as.Date(Building_Sold_Turkey$Date), House = as.numeric(Building_Sold_Turkey$Buildings))
Houses_Sold_Turkey
str(Building_Sold_Turkey)
Building_Sold_Turkey$Houses <- ts(Building_Sold_Turkey[,2], start = 2020, frequency = 12)
ggplot(Building_Sold_Turkey, aes(x = Date, y = Buildings)) + xlab("Date") + ylab("Houses Sold (Piece)") + ggtitle("Monthly Total Houses Sold in Turkey") + geom_line()

Confidence_Indexes <- data.table(Dates = as.Date(Confidence_Index$Date), Conf_Ind = as.numeric(Confidence_Index$Confidence))
Confidence_Indexes
str(Confidence_Index)
Confidence_Index$confidence_ind <- ts(Confidence_Index[,2], start = 2020, frequency = 12)
ggplot(Confidence_Index, aes(x = Date, y = confidence_ind)) + xlab("Date") + ylab("Confidence Index (%)") + ggtitle("Monthly Consumer Confidence Index") + geom_line()

corr_check <- data.frame(Monthly_USD_Reserves_million_$Reserves,Building_Sold_Turkey$Buildings, Confidence_Index$confidence_ind)
corr_check
ggpairs(corr_check)

Foreign_Loan <- data.table(Dates = as.Date(Monthly_USD_Reserves_million_$Date), For_Loan = as.numeric(Foreign_Loan_USD_Million$Loan))
Foreign_Loan
str(Foreign_Loan_USD_Million)
Foreign_Loan_USD_Million$Loans <- ts(Foreign_Loan_USD_Million[,2], start = 2020, frequency = 12)
ggplot(Foreign_Loan_USD_Million, aes(x = Date, y = Loans)) + geom_line()

series_1 <- data.table(Dates = as.Date(Monthly_USD_Reserves_million_$Date), Reserves = as.numeric(Monthly_USD_Reserves_million_$`Bank USD Assets (Million)`), Loans = Foreign_Loan_USD_Million$Loan)
head(series_1,12)
ggpairs(series_1)

lm_1 <- lm(Reserves ~ Loans, series_1 )
summary(lm_1)
checkresiduals(lm_1$residuals)

Employ_Rate <- data.table(Dates = as.Date(Monthly_USD_Reserves_million_$Date), Empl_R = as.numeric(Employement_Rates$`Employement Rate`))
Employ_Rate
str(Employement_Rates)
Employement_Rates$Rates <- ts(Employement_Rates[,2], start = 2020, frequency = 12)
ggplot(Employement_Rates, aes(x = Date, y = Rates)) + geom_line()

series_1$Employement_Rate <- Employement_Rates[,2]
head(series_1,12)
ggpairs(series_1)

lm_1 <- lm(Reserves ~ Loans + Employement_Rate, series_1 )
summary(lm_1)
checkresiduals(lm_1$residuals)

Deposit_Interest <- data.table(Dates = as.Date(Monthly_USD_Reserves_million_$Date), Dep_Int = as.numeric(MA_Deposit_Interest_Rate$`Deposit Interest Rate`))
Deposit_Interest
str(MA_Deposit_Interest_Rate)
MA_Deposit_Interest_Rate$Interest <- ts(MA_Deposit_Interest_Rate[,2], start = 2020, frequency = 12)
ggplot(MA_Deposit_Interest_Rate, aes(x = Date, y = Interest)) + geom_line()

series_1$Interest <- MA_Deposit_Interest_Rate[,2]
head(series_1,12)
ggpairs(series_1)

lm_1 <- lm(Reserves ~ Loans + Employement_Rate + Interest, series_1 )
summary(lm_1)
checkresiduals(lm_1$residuals)

Stocks_Traded <- data.table(Dates = as.Date(Monthly_USD_Reserves_million_$Date), Stocks = as.numeric(Traded_Stocks$`Traded Stocks (Thousand)`))
Stocks_Traded
str(Traded_Stocks)
Traded_Stocks$Stocks <- ts(Traded_Stocks[,2], start = 2020, frequency = 12)
ggplot(Traded_Stocks, aes(x = Date, y = Stocks)) + geom_line()

series_1$Stocks <- Traded_Stocks[,2]
head(series_1,12)
ggpairs(series_1)

lm_1 <- lm(Reserves ~ Loans + Employement_Rate + Interest + Stocks, series_1 )
summary(lm_1)
checkresiduals(lm_1$residuals)

series_1$trend <- 1:dim(series_1)[1]
months <- 1:12
series_1 <- cbind(series_1,months)

series_1$before_gradual_normalization <- 0
condition <- series_1$trend < 17
series_1$before_gradual_normalization[condition] = 1
series_1

ggpairs(series_1)

lm_1 <- lm(Reserves ~ Loans + Employement_Rate + Interest + Stocks + trend + before_gradual_normalization, series_1 )
summary(lm_1)
checkresiduals(lm_1$residuals)

head(Gu_mru_k,18)
ggplot(Gu_mru_k, aes(x = Gu_mru_k$Week, y = Gu_mru_k$`gümrük: (Türkiye)`)) + geom_line()

bymonth <- aggregate(Gu_mru_k$`gümrük: (Türkiye)`~ month(Week) + year(Week), data = Gu_mru_k, FUN = sum)
bymonth
series_1$Gumruk_search <- bymonth[2:38,3]
series_1
ggplot(series_1, aes(x = Dates, y = Gumruk_search)) + geom_line()

ggpairs(series_1)


lm_1 <- lm(Reserves ~ Loans + Employement_Rate + Interest + Stocks + trend + before_gradual_normalization + Gumruk_search, series_1 )
summary(lm_1)
checkresiduals(lm_1$residuals)

series_1$model <- fitted(lm_1)
series_1$residual <- residuals(lm_1)
series_1
ggplot(series_1, aes(x = model, y = residual)) + geom_point()
ggplot(series_1, aes(x = Reserves, y = model)) + geom_point() + geom_smooth(method = lm)

ggplot(series_1, aes(x=Dates)) +
  geom_line(aes(y = Reserves, color = "orange")) + 
  geom_line(aes(y = model, color = "blue"))

Houses_Sold_Turkey
ggplot(Building_Sold_Turkey, aes(x = Date, y = Houses)) + xlab("Date") + ylab("Houses Sold (Piece)") + ggtitle("Monthly Total Houses Sold in Turkey") + geom_line()

series_2 <- data.table(Dates = as.Date(Building_Sold_Turkey$Date), Houses = as.numeric(Building_Sold_Turkey$Houses))
head(series_2,12)
acf(series_2$Houses,37)

series_2$trend <- 1:dim(series_2)[1]
months <- 1:12
series_2 <- cbind(series_2,months)
series_2
ggpairs(series_2)

lm_2 <- lm(Houses ~ months, series_2 )
summary(lm_2)
checkresiduals(lm_2$residuals)

House_Sale <- data.table(Dates = as.Date(House_for_Sale$Week), House_search = as.numeric(House_for_Sale$`satılık ev: (Türkiye)`))
head(House_Sale,18)
House_for_Sale$Searches <- ts(House_for_Sale[,2], start = 2020, frequency = 12)
ggplot(House_for_Sale, aes(x = House_Notices$Week, y = House_for_Sale$`satılık ev: (Türkiye)`)) + geom_line()

Search_bymonth <- aggregate(House_for_Sale$Searches~ month(Week) + year(Week), data = House_for_Sale, FUN = sum)
Search_bymonth
series_2$House_for_Sale <- Search_bymonth[2:38,3]
ggplot(series_2, aes(x = Dates, y = House_for_Sale)) + geom_line()

ggpairs(series_2)

lm_2 <- lm(Houses ~ months + House_for_Sale, series_2 )
summary(lm_2)
checkresiduals(lm_2$residuals)

Employ_Rate <- data.table(Dates = as.Date(Building_Sold_Turkey$Date), Empl_R = as.numeric(Employement_Rates$`Employement Rate`))
Employ_Rate
str(Employement_Rates)
Employement_Rates$Rates <- ts(Employement_Rates[,2], start = 2020, frequency = 12)
ggplot(Employement_Rates, aes(x = Date, y = Rates)) + geom_line()

series_2$Employment_Rate <- Employement_Rates[,2]
series_2
ggpairs(series_2)

lm_2 <- lm(Houses ~ months + House_for_Sale + Employment_Rate, series_2 )
summary(lm_2)
checkresiduals(lm_2$residuals)

Kira_Search <- data.table(Dates = as.Date(Kira$Week), Kiras = as.numeric(Kira$`kira: (Türkiye)`))
head(Kira_Search,18)
str(Kira)
Kira$Searches <- ts(Kira[,2], start = 2020, frequency = 12)
ggplot(Kira, aes(x = Kira$Week, y = Kira$`kira: (Türkiye)`)) + geom_line()

Search_bymonth <- aggregate(Kira$Searches~ month(Week) + year(Week), data = Kira, FUN = sum)
Search_bymonth
series_2$Kira_Searches <- Search_bymonth[2:38,3]
ggplot(series_2, aes(x = Dates, y = Kira_Searches)) + geom_line()

ggpairs(series_2)

lm_2 <- lm(Houses ~ months + House_for_Sale + Employment_Rate + Kira_Searches, series_2 )
summary(lm_2)
checkresiduals(lm_2$residuals)

House_Loan_Int <- data.table(Dates = as.Date(Building_Sold_Turkey$Date), Loan_Int = as.numeric(Housing_Loan_Interest$`Interest on Housing Loans`))
House_Loan_Int
str(Housing_Loan_Interest)
Housing_Loan_Interest$Int <- ts(Housing_Loan_Interest[,2], start = 2020, frequency = 12)
ggplot(Housing_Loan_Interest, aes(x = Date, y = Housing_Loan_Interest$`Interest on Housing Loans`)) + geom_line()

series_2$Housing_Int <- Housing_Loan_Interest[,2]
series_2
ggpairs(series_2)

lm_2 <- lm(Houses ~ months + House_for_Sale + Employment_Rate + Kira_Searches + Housing_Int, series_2 )
summary(lm_2)
checkresiduals(lm_2$residuals)

series_2$model <- fitted(lm_2)
series_2$residual <- residuals(lm_2)
series_2
ggplot(series_2, aes(x = model, y = residual)) + geom_point()
ggplot(series_2, aes(x = Houses, y = model)) + geom_point() + geom_smooth(method = lm)

ggplot(series_2, aes(x=Dates)) +
  geom_line(aes(y = Houses, color = "orange")) + 
  geom_line(aes(y = model, color = "blue"))

Confidence_Indexes
ggplot(Confidence_Index, aes(x = Date, y = confidence_ind)) + xlab("Date") + ylab("Confidence Index (%)") + ggtitle("Monthly Consumer Confidence Index") + geom_line()

Exchanges <- data.table(Dates = as.Date(Confidence_Index$Date), Exch_rate = as.numeric(Monthly_USD_Exchange_Rates$`Exchange Rate`))
Exchanges
str(Monthly_USD_Exchange_Rates)
Monthly_USD_Exchange_Rates$Exchange <- ts(Monthly_USD_Exchange_Rates[,2], start = 2020, frequency = 12)
ggplot(Monthly_USD_Exchange_Rates, aes(x = Date, y = Exchange)) + geom_line()

series_3 <- data.table(Dates = as.Date(Confidence_Index$Date), Confidence = as.numeric(Confidence_Index$Confidence), Exchange = as.numeric(Monthly_USD_Exchange_Rates$`Exchange Rate`))
series_3 
ggpairs(series_3)

lm_3 <- lm(Confidence ~ Exchange, series_3 )
summary(lm_3)
checkresiduals(lm_3$residuals)

Tufe <- data.table(Dates = as.Date(Confidence_Index$Date), TUFE = as.numeric(TUFE_data$TUFE))
Employ_Rate
str(TUFE_data)
TUFE_data$CPI <- ts(TUFE_data[,2], start = 2020, frequency = 12)
ggplot(TUFE_data, aes(x = Date, y = TUFE)) + geom_line()

series_3$CPI <- TUFE_data[,2]
series_3
ggpairs(series_3)

lm_3 <- lm(Confidence ~ Exchange+CPI, series_3 )
summary(lm_3)
checkresiduals(lm_3$residuals)

Card_Expend <- data.table(Dates = as.Date(Confidence_Index$Date), Expenditures = as.numeric(CD_Card_Expenditure$`Expenditures (Thousand TL)`))
Card_Expend
str(CD_Card_Expenditure)
CD_Card_Expenditure$Expen <- ts(CD_Card_Expenditure[,2], start = 2020, frequency = 12)
ggplot(CD_Card_Expenditure, aes(x = Date, y = `Expenditures (Thousand TL)` )) + geom_line()

series_3$Expenditure <- CD_Card_Expenditure[,2]
series_3
ggpairs(series_3)

lm_3 <- lm(Confidence ~ Exchange + CPI + Expenditure, series_3 )
summary(lm_3)
checkresiduals(lm_3$residuals)

Employ_Rate <- data.table(Dates = as.Date(Confidence_Index$Date), Empl_R = as.numeric(Employement_Rates$`Employement Rate`))
Employ_Rate
str(Employement_Rates)
Employement_Rates$Employment <- ts(Employement_Rates[,2], start = 2020, frequency = 12)
ggplot(Employement_Rates, aes(x = Date, y = Employment )) + geom_line()

series_3$Employment <- Employement_Rates[,2]
series_3
ggpairs(series_3)

lm_3 <- lm(Confidence ~ Exchange + CPI + Expenditure + Employment, series_3 )
summary(lm_3)
checkresiduals(lm_3$residuals)

Zam_s <- data.table(Dates = as.Date(Zam$Week), Zam_search = as.numeric(Zam$`zam: (Türkiye)`))
head(Zam_s,18)
Zam$Searches <- ts(Zam[,2], start = 2020, frequency = 12)
ggplot(Zam, aes(x = Zam$Week, y = Zam$`zam: (Türkiye)`)) + geom_line()

bymonth <- aggregate(Zam$`zam: (Türkiye)`~ month(Week) + year(Week), data = Zam, FUN = sum)
bymonth
series_3$Zam_Search <- bymonth[2:38,3]
ggplot(series_3, aes(x = Dates, y = Zam_Search)) + geom_line()

series_3
ggpairs(series_3)

lm_3 <- lm(Confidence ~ Exchange + CPI + Expenditure + Employment + Zam_Search, series_3 )
summary(lm_3)
checkresiduals(lm_3$residuals)

series_3$model <- fitted(lm_3)
series_3$residual <- residuals(lm_3)
series_3
ggplot(series_3, aes(x = model, y = residual)) + geom_point()
ggplot(series_3, aes(x = Confidence, y = model)) + geom_point() + geom_smooth(method = lm)

ggplot(series_3, aes(x=Dates)) +
  geom_line(aes(y = model, color = "blue")) +
  geom_line(aes(y = Confidence, color = "orange")) 



