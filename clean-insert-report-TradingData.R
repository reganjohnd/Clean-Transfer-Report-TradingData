library(readxl)
library(writexl)
library(lubridate)
library(gridExtra)
library(ggplot2)

setwd('{Your Working Directory Goes Here}')

df <- read_xlsx('{Your Input File Goes Here}')

### set account type based on account number
liveAccNumber <- '35281648'
ifelse(gsub("[^0-9.]", "",  df[2, 1]) == liveAccNumber, accountType <- 'Live', accountType <- 'Demo')

### adjust account balance based on external liquidity
### sum of trading account balance + external liquidity
externalLiquidity <- 0
accountBalance <- sum(as.numeric(sub(' ', '', df[nrow(df), 3])), externalLiquidity)

### format into tabular data
df <- df[-c(1:3), ]
df <- df[-c(which(df[, 1] == 'Closed P/L:', arr.ind = TRUE)[1]:nrow(df)), ]
names(df) <- df[1, ]
df <- df[-1, ]
df <- df[!is.na(df[, 1]), ]
df <- df[df[, 3] == 'sell' | df[, 3] == 'buy', ]
df$Date <- substring(df$'Open Time', 1, 10)

### timestamp test
df$TimeStamp <- substring(df$'Open Time', 12, 19)

### further cleaning of data
df$openTime <- substring(df$`Open Time`, 12, 20)
df$closeTime <- substring(df$`Close Time`, 12, 20)
df <- df[, c(-2, -9)]
colnames(df)[5] <- 'openPrice'
colnames(df)[8] <- 'closePrice'
df$openPrice <- as.numeric(df$openPrice)
df$closePrice <- as.numeric(df$closePrice)
df$PipProfit <- ifelse(df$Type == 'buy', df$closePrice - df$openPrice, df$openPrice - df$closePrice) * 10000
df$Date <- ymd(df$Date)
df$openTime <- hms(df$openTime)
df$closeTime <- hms(df$closeTime)
df$'tradeDuration(seconds)' <- as.numeric((df$closeTime - df$openTime))
df <- df[, -c(15, 16)]
df$accountType <- accountType

df$MaxDrawdownPrice <- NA
df$MaxDrawdownPoints <- NA
df$MaxDrawdownRandsOneLot <- NA

df <- df[, c(1,13, 14, 2:12, 15, 18:20, 16, 17)]
colnames(df) <- c('id',
                  'tradeDate',
                  'timeStamp',
                  'tradeType',
                  'size',
                  'currencyPair',
                  'openPrice',
                  'stopLoss',
                  'takeProfit',
                  'closePrice',
                  'commission',
                  'taxes',
                  'swap',
                  'profit',
                  'pipProfit',
                  'maxDrawdownPrice',
                  'maxDrawdownPoints',
                  'maxDrawdownRandsOneLot',
                  'tradeDuration(seconds)',
                  'accountType')

df$maxDrawdownPoints <- df$openPrice - df$maxDrawdownPrice
df$maxDrawdownRandsOneLot <- 1.6 * df$maxDrawdownPoints * 10

### CONNECT to Database
library(RMySQL)
con <-  dbConnect(RMySQL::MySQL(),
                  username = "root",
                  password = "ReganjohnD1",
                  host = 'localhost',
                  port = 3306,
                  dbname = "finsectra"
)

dbSendQuery(con, 'SET GLOBAL local_infile = true')

### INSERT data to table
dbWriteTable(conn = con, name = 'results_rogerdexpert', value = df, append = TRUE, row.names = FALSE)

### check that new entries are present
results_rogerdexpert <- dbReadTable(conn = con, name = 'results_rogerdexpert')

### profit calculations
grossProfit <- aggregate(results_rogerdexpert[results_rogerdexpert$accountType == accountType, 'profit'], by = list(results_rogerdexpert[results_rogerdexpert$accountType == accountType, 'tradeDate']), FUN= sum)$x
commission <- aggregate(results_rogerdexpert[results_rogerdexpert$accountType == accountType, 'commission'], by = list(results_rogerdexpert[results_rogerdexpert$accountType == accountType, 'tradeDate']), FUN= sum)$x
netProfit <- sum(grossProfit, commission)


### output chart with Perctage return per day
netProfit <- grossProfit + commission
perc <- round(grossProfit + commission, 2) / (accountBalance - netProfit) * 100
messagePerc <- tail(perc, n = 1) 
res <- cbind(aggregate(results_rogerdexpert[results_rogerdexpert$accountType == accountType, 'profit'], by = list(results_rogerdexpert[results_rogerdexpert$accountType == accountType, 'tradeDate']), FUN= sum), perc)
res <- res[, -2]
names(res)[2] <- 'percentage'
names(res)[1] <- 'date'
res$percentage <- round(res$percentage, 2)
res$date <- substring(res$date,first = 6, last = 10)

### generate Chart
plt <- ggplot(data = res, aes(x = date, y = percentage)) + geom_bar(stat = 'identity') +
  geom_label(label = res$percentage,
             nudge_x = 0.25, nudge_y = 0.25) +
  ggtitle(paste0("Percentage Return By Day: ", accountType)) +
  ylab('Percentage Return') +
  xlab('Date')

### Variables to save chart and table
plot <- paste(gsub('-', '', Sys.Date()), 'ResultsGraph', 'RDE', accountType, 'ReganJohnDaniels.pdf',sep = '-' )
grid <- paste(gsub('-', '', Sys.Date()), 'ResultsTable', 'RDE', accountType, 'ReganJohnDaniels.pdf',sep = '-' )

### Save chart and table
ggsave(plot, plot = plt, width = 30, height = 25, units = 'cm', path = 'graphics')
ggsave(grid, plot = grid.table(results_rogerdexpert[(nrow(results_rogerdexpert) - 2) : nrow(results_rogerdexpert), -c(3, 9, 12)]), width = 45, height = 10, units = 'cm', path = 'graphics')

### Send email to recipients
library(mailR)
emailMessage <- '{Your Email Message Goes Here}'
emailSubject <- '{Your Email Subject Goes Here}'

### import email recipients
erTmp <- read.csv('input\\emailRecipients.csv', header = FALSE)
emailRecipients <- vector(mode = 'character')
emailRecipients <- unlist(lapply(erTmp$V1, function(x) append(emailRecipients, x)))

### Send Email with attachements
send.mail(from = "{Sender Email Goes Here}",
          to = emailRecipients,
          subject = emailSubject,
          body = emailMessage,
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = '{Your Username Goes Here}', passwd = '{Your Password Goes Here}', ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = c(paste0('graphics\\', plot), paste0('graphics\\', grid)))