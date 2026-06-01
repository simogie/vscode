#*************************************************************
#*
#* sz6　RでKERASを使ってPYTHON環境で機械学習
#*
#************************************************************
# プロンプト
# Rでpythonの仮想環境を使ってkerasで機械学習で回帰問題をやりたい。
# anacondaでpythonとcondaは以下の通りインストールしている。
# 
# (base) C:\Users\user>where python
# C:\Users\user\anaconda3\python.exe
# C:\Users\user\AppData\Local\Microsoft\WindowsApps\python.exe
# 
# (base) C:\Users\user>where conda
# C:\Users\user\anaconda3\Library\bin\conda.bat
# C:\Users\user\anaconda3\Scripts\conda.exe
# C:\Users\user\anaconda3\condabin\conda.bat
# 
# この状態でpythonの仮想環境の設定手順を教えてください。
# 
# つぎに
# データセット n225d50b.csvは1行目がタイトル、1列目から7列目が説明変数。、
# 8列目の	n225hが目的変数です。
# 
# n225d50b.csvを使い、作成したRの仮想環境でのkerasで機械学習を行い、
# データは1行だけの50today.csvで目的変数の予測値を表示する手順を教えてください。
# 予測した目的変数は元のスケールに戻した数値にしてください。



# 仮想環境の作成
# conda create -n rkeras python=3.10
# 環境をアクティベート
# bash

conda activate rkeras

# 必要ライブラリをインストール
# bash

pip install tensorflow keras scikit-learn pandas


# 🔄 ステップ②：RからPython環境を使う準備

reticulateパッケージのインストール


install.packages("reticulate")

library(reticulate)

# 作成したPython環境を指定

use_condaenv("rkeras", required = TRUE)

📊 ステップ③：データの前処理と学習

# 以下はn225d50b.csvでモデルを訓練し、50today.csvの説明変数に対して予測するRコードの流れです。


library(reticulate)

library(tensorflow)

library(keras)

# Pythonモジュールのインポート

np <- import("numpy")

pd <- import("pandas")

sk <- import("sklearn")

MinMaxScaler <- sk$preprocessing$MinMaxScaler


# データ読み込み

train_df <- pd$read_csv("n225d50b.csv")

test_df <- pd$read_csv("50today.csv")


# 説明変数と目的変数を分ける

X <- train_df$iloc[, 0:6]　　#0-6列が説明変数

y <- train_df$iloc[, 7]  # n225hは7列目


# スケーリング

scaler_X <- MinMaxScaler()

scaler_y <- MinMaxScaler()

X_scaled <- scaler_X$fit_transform(X)

y_scaled <- scaler_y$fit_transform(np$array(y)$reshape(-1L, 1L))


# モデル構築

model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = dim(X_scaled)[2]) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)


model %>% compile(
  loss = "mean_squared_error",
  optimizer = "adam"
)


# モデル学習

model %>% fit(X_scaled, y_scaled, epochs = 100, verbose = 0)

# テストデータのスケーリングと予測

X_test <- test_df$iloc[0, 0:6]$values$reshape(1L, -1L)
X_test_scaled <- scaler_X$transform(X_test)
y_pred_scaled <- model %>% predict(X_test_scaled)

# 予測値を逆スケーリング

y_pred <- scaler_y$inverse_transform(y_pred_scaled)

# 結果を表示

print(paste("予測されたn225hの値は:", round(y_pred[0, 0], 2)))


# 💡補足ポイント
# スケーラーは学習用データに基づいてフィットしてからテストに適用すること。
# 
# reshape(-1L, 1L) はRからPythonへ渡す際の形状調整に重要です。
# 
# 出力される予測値は n225h の元スケール値です。

