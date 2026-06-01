#********************************************************
#*　　　　TYPE B
#* n225d50ファイル TYPE B　で neuralnet() 安値予測
#*
#******************************************************** 


# RStudioでスクリプト全体を一括実行したい場合、
# 次の方法でスムーズに一気に走らせることができます 💻✨
# 
# ✅ 方法1：スクリプト全体を「Source」する
# - スクリプトファイル（たとえば main.R）をRStudioで開いている状態にします。
# - RStudioの右上にある "Source" ボタン（または Ctrl + Shift + S）をクリック。
# - これでスクリプト全体が順番に実行されます。
# - source("main.R") とするのと同じです。

# 🧠 ステップごとの構築手順（neuralnetを使用）
# 
# ① 必要パッケージのインストール・読み込み

 # install.packages("neuralnet")  # 初回のみ

　library(neuralnet)


# この .csv ファイルには "37,877" のような カンマ付きの文字列が多く含まれており、
# そのままでは R の数値演算に使えません。
# そこで、全列を数値型に変換する方法を以下にまとめました！
# 
# 🛠 すべての列を数値化するステップ
# # ファイルを読み込む（文字列として）




# ② TYPE B　のデータ読み込みと前処理

data_raw <- read.csv("D:/R/workspace/n225d50b.csv", stringsAsFactors = FALSE)

# カンマ付き文字列を数値化する関数

clean_numeric <- function(x) {
  as.numeric(gsub(",", "", x))
}

# データ全体に適用
data <- as.data.frame(lapply(data_raw, clean_numeric))



# 📊 変換後の確認方法
# str(data)
# summary(data)
# 
# これで各列が num 型（数値型）になっているはずです 👍


# View(data)


# 正規化（Min-Maxスケーリングで0-1に）

normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }

data_norm <- as.data.frame(lapply(data, normalize))

# head(data_norm,3)

#         dow       nas        sp    usdidx       crb     usdjp     nkcmd     n225h     n225l
# 1 0.1314940 0.1795883 0.1618705 0.9923664 0.2777778 0.6666667 0.1681196 0.1564640 0.1653895
# 2 0.2113296 0.1642576 0.2050360 0.9770992 0.1909722 0.4761905 0.1951017 0.1381393 0.1326096
# 3 0.3087760 0.2076216 0.2787770 1.0000000 0.1979167 0.4761905 0.2013284 0.1119613 0.1255854


# ③ 学習用とテスト用データに分割
# データの8割をランダムに選んで学習用データとして抽出するための
# インデックス番号を作成

set.seed(123)  # 再現性確保

index <- sample(1:nrow(data_norm), round(0.8 * nrow(data_norm)))

# sample:指定した数だけﾗﾝﾀﾞﾑにｲﾝﾃﾞｯｸｽを重複なしで抽出。
# 1: nrow(data_norm):ﾃﾞｰﾀ１～最終行まで
# round(0.8 * nrow(data_norm):データの80%の行数を四捨五入求める

train <- data_norm[index, ]　#index(80%)をtrainへ代入

# head(train,3)

test <- data_norm[-index, ]　#残りをtestに代入。


# ④ ニューラルネットモデルの構築 
# 
# # フォーミュラを作成（説明変数 → 目的変数） 

formula <- n225l ~ dow + nas + sp + usdidx + crb + usdjp + nkcmd

# モデル構築：隠れ層に5ユニット × 2層の場合

nn_model <- neuralnet(formula,
                      data = train,
                      hidden = c(5, 5),
                      linear.output = TRUE)



# summary(train$n225l)        # NAがないかか確認

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.05518 0.19452 0.30477 0.37248 0.55925 1.00000 


# ⑤ モデルの可視化

# plot(nn_model)


# ⑥ 予測と逆正規化

pred <- compute(nn_model, test[, 1:7])$net.result

# 目的変数の逆正規化（元のスケールに戻す）

denormalize <- function(x, orig) {
  x * (max(orig) - min(orig)) + min(orig)
}


pred_actual <- denormalize(pred, data$n225l[-index])  # テストデータに対応



# ⑦ 精度評価（MSEなど）

mse <- mean((pred_actual - data$n225l[-index])^2)

print(paste("TYPE-Bﾌｧｲﾙ:平均二乗誤差 (MSE):", round(mse, 2)))

print(paste("TYPE-Bﾌｧｲﾙ:平均的な安値の予測誤差(RMSE円) :", round(sqrt(mse), 2)))


# 📏 平均二乗誤差（MSE）とは？
# MSEは、予測値と実際の値の差を二乗して平均したものです：
# \text{MSE} = \frac{1}{n} \sum_{i=1}^{n} (y_i - \hat{y}_i)^2


# - y_i：実測値（今回の場合は実際の n225h）
# - \hat{y}_i：モデルによる予測値
# - 値が 小さいほど誤差が小さく、モデルの精度が良い とされます。
# 
# 🧐 MSE =2264799.12" は「良い」のか？

# これは「絶対値」ではなく、データのスケールによって意味が変わります。
# 今回の n225h（日経平均の高値）は、値域が 30,000～40,000円台のようなので、
# 誤差の平方根（RMSE）で見てみましょう：

# sqrt(2264799.12)  # ≈ 1504.925

# 👉 平均的な予測誤差は約1504.925円前後。
# 

# このニューラルネットモデルを使って、
# 指定された説明変数から目的変数 n225h（日経225の高値）を予測するには、
# 以下のステップで進めます ✨
# 
# 🧭 ステップ：新しい入力値から予測値を出す手順
# ① 入力ベクトルを作成（元データスケールで）
# 新しい入力データ（説明変数）

# 今日のﾃﾞｰ50today.ﾀCSVファイルの読み込み（パスは必要に応じて変更）

csv_data <- read.csv("D:/R/workspace/50today.csv")

# new_inputの作成（1行目を使用）

new_input <- data.frame(
  dow    = csv_data$dow[1],
  nas    = csv_data$nas[1],
  sp     = csv_data$sp[1],
  usdidx = csv_data$usdidx[1],
  crb    = csv_data$crb[1],
  usdjp  = csv_data$usdjp[1],
  nkcmd  = csv_data$nkcmd[1]
)


# ② 入力値を訓練データと同様に正規化（Min-Maxでスケーリング）
# このとき、正規化には元の data の min/max を使用します：

normalize <- function(x, min_x, max_x) {
  (x - min_x) / (max_x - min_x)
}

# 元データの min/max を取得（n225hは含まない）

min_vals <- sapply(data[, 1:7], min)

max_vals <- sapply(data[, 1:7], max)

# 正規化された新しいデータ

new_input_norm <- as.data.frame(as.list(
  mapply(normalize, x = new_input, min_x = min_vals, max_x = max_vals)
))


# ③ 予測実行：neuralnet モデルへ投げる

pred_result <- compute(nn_model, new_input_norm)$net.result


# ✅ 最終的な予測コード例
# # 列名揃えたら再実行

pred_result <- compute(nn_model, new_input_norm)$net.result

# 逆正規化

pred_value <- pred_result * (max(data$n225l) - min(data$n225l)) + min(data$n225l)

print(paste("TYPE-Bﾌｧｲﾙで予測された n225l の安値:", round(pred_value, 2)))

# 終わり

