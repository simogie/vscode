#***************************************************************
#*　　
#*　　SZ6 用
#*  RでＫＥＲＡＳを動かす。
#*  n225d50b.csvで　安値予想計算　ランダム化なし。
#*
#*
#*
#************************************************************** 

# prompt
# Rでpythonの仮想環境を使いkerasでcpuベースで回帰問題の機械学習でをやりたい。
# anacondaでpythonとcondaは以下の通りインストールしている。
# (base) C:\Users\user>where python 
# C:\Users\user\anaconda3\python.exe 
# C:\Users\user\AppData\Local\Microsoft\WindowsApps\python.exe 
# 
# (base) C:\Users\user>where conda 
# C:\Users\user\anaconda3\Library\bin\conda.
# bat C:\Users\user\anaconda3\Scripts\conda.exe
# C:\Users\user\anaconda3\condabin\conda.bat 
# この状態でRのlibrary(reticulate)をつかったpythonの仮想環境の設定手順を教えて。 
# つぎに データセット n225d50b.csvは1行目がタイトル、1列目から7列目が説明変数。
# 8列目のn225hが目的変数です。
# n225d50b.csvを使い作成したRの仮想環境でRのlibrary(keras)や(tensorflow)でモデルを作成し、 
# データ1行だけの50today.csvで目的変数の予測値を表示する手順を教えてください。 
# ただし予測する目的変数は元のスケールに戻した数値にしてください。


# 了解です、teruhisaさん。
# RからPython仮想環境を利用してKerasでCPUベースの回帰モデルを構築し、
# n225d50b.csvで学習し、50today.csvで予測を行う流れを以下に整理しました。
# 目的変数はスケーリング前の元の値に戻して出力します。
# 
# 🛠 Python仮想環境の設定手順（reticulate使用）
# 仮想環境の作成（Anaconda Promptで）:
#   
#   bash

# conda create -n r-reticulate python=3.8
# 
# conda activate r-reticulate
# 
# pip install tensorflow keras pandas scikit-learn


# Rでreticulateに仮想環境を指定:

# library(reticulate)
# 
# use_condaenv("r-reticulate", required = TRUE)


# 📊 回帰モデル構築と予測手順（Rで実施）
# 
# 以下は、n225d50b.csvでモデル学習 → 50today.csvで予測 → 
# 元スケールに逆変換 までのフローです。


library(keras)

library(tensorflow)

library(reticulate)

library(readr)

library(dplyr)

library(scales)  # for inverse transformation

# 環境設定

use_condaenv("r-reticulate", required = TRUE)

# use_condaenv("keras-env", required = TRUE)　　#上と衝突するので使うな。

Sys.setenv(RETICULATE_PYTHON = "C:/Users/user/anaconda3/envs/r-reticulate/python.exe")





# データの読み込み

train_data <- read_csv("D:/R/workspace/n225d50b.csv")

predict_data <- read_csv("D:/R/workspace/50today.csv")



# 入力と出力の分離

x_train <- as.matrix(train_data[, 1:7])

y_train <- train_data$n225l #n225L

x_pred <- as.matrix(predict_data[, 1:7])

# スケーリング（Min-Max）

x_min <- apply(x_train, 2, min)
x_max <- apply(x_train, 2, max)
x_train_scaled <- scale(x_train, center = x_min, scale = x_max - x_min)
x_pred_scaled <- scale(x_pred, center = x_min, scale = x_max - x_min)

# 目的変数のスケーリング

y_min <- min(y_train)
y_max <- max(y_train)
y_train_scaled <- (y_train - y_min) / (y_max - y_min)

# モデル構築:最初はここからエラーが出て次に行けなかった。以下に修正法。

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = ncol(x_train_scaled)) %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1)


# Python specified in RETICULATE_PYTHON 
# このエラーは、RETICULATE_PYTHON に指定された Python のパスが存在しないために起きています。
# つまり、Rが参照しようとしている C:/Users/WIN11USER/anaconda3/envs/keras-env/python.exe が
# 存在しない、もしくは仮想環境 keras-env が作成されていない状態です。
# 解決ステップ

# ① 仮想環境の確認（Anaconda Promptで実行）
# bash
# conda info --envs

# → keras-env が存在しているか確認してください。：存在した。

# ② 仮想環境を作成する（存在していない場合）
# bash
# conda create -n keras-env python=3.8
# conda activate keras-env
# pip install tensorflow keras pandas scikit-learn

# ③ R側で仮想環境を指定する方法
# 方法A: use_condaenv() を使う

# library(reticulate)
# 
# use_condaenv("keras-env", required = TRUE)

# Unable to locate conda environment 'keras-env'.エラー」
# keras-env" を見つけられないことが原因
# 仮想環境名の確認
# まず、環境名が正しいかチェックします。Anaconda Promptで以下のコマンドを実行してください：
# 
# bash
# conda env list
# → keras-env という名前の環境が 存在するかどうか を確認：→ない。

# ないときは以下で新しい環境を作成できます：

# bash
# conda create -n keras-env python=3.8 keras tensorflow  やった。

# ③ Rで正しい環境を指定
# 環境名が正しいなら、R側ではこう指定してください：

# library(reticulate)
# 
# use_condaenv("keras-env", required = TRUE)

# その後
# py_config() でちゃんと切り替わっているかチェックすると安心


# 
# 方法B: RETICULATE_PYTHON の設定（パスが正しい場合のみ）

# Sys.setenv(RETICULATE_PYTHON = "C:/Users/WIN11USER/anaconda3/envs/keras-env/python.exe")
# 　　　　　　　　　　　　　　　　　　　　　△△　ここが変
# library(reticulate)

# ただし、パスが正しくなければこの方法は失敗します。
# まずは Anaconda Prompt で where python を keras-env アクティベート後に実行して、
# 正しいパスを確認してください。

# (r-reticulate) C:\Users\user>where python
# C:\Users\user\anaconda3\envs\r-reticulate\python.exe　→これが正解
# C:\Users\user\anaconda3\python.exe
# C:\Users\user\AppData\Local\Microsoft\WindowsApps\python.exe

# Rでreticulateを使う場合、指定すべき「正解のPython」は、
# 仮想環境 r-reticulate の python.exe です。つまり：
# 正解は
# C:\Users\user\anaconda3\envs\r-reticulate\python.exe
# このパスが、あなたが作成した仮想環境に属するPythonであり、
# RからPythonパッケージ（tensorflow, kerasなど）を利用する際に最も適しています。

# Sys.setenv(RETICULATE_PYTHON = "C:/Users/user/anaconda3/envs/r-reticulate/python.exe")


# モデルコンパイル

model %>% compile(
  optimizer = "adam",
  loss = "mse"
)

# 学習

model %>% fit(
  x = x_train_scaled,
  y = y_train_scaled,
  epochs = 100,
  batch_size = 10,
  verbose = 0
)

# 予測 & 逆スケーリング

y_pred_scaled <- model %>% predict(x_pred_scaled)

y_pred_original <- y_pred_scaled * (y_max - y_min) + y_min

print(paste("ランダム無しの安値予測値（元スケール）: ", round(y_pred_original, 2)))


# ✅ 補足
# kerasやtensorflowはR用のバインディングでPythonの実装を呼び出しているため
# reticulateで仮想環境を正しく指定することが肝心です。
# 
# モデル構造やユニット数はサンプルです。精度を高めるには検証と調整が必要です。