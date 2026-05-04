# 日経225の昨日のデータを表示するスクリプト
# Yahoo FinanceのチャートAPIを利用します。

options(repos = c(CRAN = "https://cran.rstudio.com/"))
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

library(httr)
library(jsonlite)

# Yahoo Financeのシンボル
symbol <- "%5EN225"

# 直近7日分のデータを取得
url <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/", symbol, "?range=7d&interval=1d")
response <- GET(url, user_agent("R (Yahoo Finance)"))

if (status_code(response) != 200) {
  stop("Yahoo Financeからデータを取得できませんでした。status_code=", status_code(response))
}

content_text <- content(response, as = "text", encoding = "UTF-8")
parsed <- fromJSON(content_text, flatten = TRUE)

if (is.null(parsed$chart$result) || length(parsed$chart$result) == 0) {
  stop("Yahoo Financeの応答にデータが含まれていません。")
}

result <- parsed$chart$result

# timestampとindicatorsを抽出
if (is.null(result$timestamp[[1]]) || is.null(result$`indicators.quote`[[1]])) {
  stop("Yahoo Financeの応答に有効な時系列データが含まれていません。")
}

timestamps <- result$timestamp[[1]]
quote_list <- result$`indicators.quote`[[1]]

# quote_listから価格データを抽出（リストとしてアクセス）
if (!is.list(quote_list) || is.null(quote_list$close)) {
  stop("Yahoo Financeの応答に価格情報が含まれていません。")
}

# closeがベクトルとして存在するはずだが、形式を確認して対応
close_prices <- quote_list$close
if (is.list(close_prices)) {
  close_prices <- unlist(close_prices)
}

# 同様にopen, high, lowを抽出
open_prices <- quote_list$open
if (is.list(open_prices)) open_prices <- unlist(open_prices)
high_prices <- quote_list$high
if (is.list(high_prices)) high_prices <- unlist(high_prices)
low_prices <- quote_list$low
if (is.list(low_prices)) low_prices <- unlist(low_prices)

# 日付に変換
dates <- as.Date(as.POSIXct(timestamps, origin = "1970-01-01", tz = "UTC"))

# 有効なデータがある営業日を特定
valid_idx <- which(!is.na(close_prices))

if (length(valid_idx) < 2) {
  stop("前営業日のデータが見つかりません。取得データが不足しています。")
}

# 前営業日は最後から2番目の有効なデータ
idx <- valid_idx[length(valid_idx) - 1]
actual_date <- dates[idx]

open_price <- open_prices[idx]
high_price <- high_prices[idx]
low_price <- low_prices[idx]
close_price <- close_prices[idx]

cat("Yahoo Financeから取得した日経225データ (", format(actual_date, "%Y年%m月%d日"), "):\n")
cat("始値 (Open): ", open_price, "\n")
cat("高値 (High): ", high_price, "\n")
cat("安値 (Low): ", low_price, "\n")
cat("終値 (Close): ", close_price, "\n")
