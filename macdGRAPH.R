# 必要パッケージ
install.packages(c("TTR"))

library(TTR)

# データ読み込み
df <- read.csv("D:/R/workspace/n225E.csv")

# 列名調整
colnames(df) <- c("date", "close")

df$date <- as.Date(df$date)
df$close <- as.numeric(df$close)

# MACD
macd <- MACD(df$close, nFast=12, nSlow=26, nSig=9)
df$macd <- macd[,1]
df$signal <- macd[,2]
df$hist <- df$macd - df$signal

df <- na.omit(df)

# スプライン
x <- 1:nrow(df)
fit <- smooth.spline(x, df$hist, spar=0.6)

# 微分
d1 <- predict(fit, x, deriv=1)$y
d2 <- predict(fit, x, deriv=2)$y

# -------- グラフ --------

# 終値
plot(df$date, df$close, type="l", main="Close Price")

# MACDヒストグラム + スプライン
plot(df$date, df$hist, type="l", main="MACD Histogram")
lines(df$date, predict(fit)$y)

# 1階微分
plot(df$date, d1, type="l", main="1st Derivative")

# 2階微分
plot(df$date, d2, type="l", main="2nd Derivative")



#********************************************************************
#グラフに「1階微分」「2階微分」と表示され、2階微分のグラフが描画されている場合、
#元のデータの「曲がり具合」や「変化の加速・減速」を視覚化している。
#それぞれの階層が持つ意味を整理すると、グラフの意図が見える。

#　① 各微分の役割
# 元のグラフ ($f(x)$): 現在の値（位置、価格、温度など）そのもの。

# 1階微分 ($f'(x)$): 変化の勢い（速度）。グラフが右上がりならプラス、右下がりならマイナス。

# 2階微分 ($f''(x)$): 変化の勢いの変化（加速度）。1階微分のグラフがさらにどう変化しているか。

#　②. 2階微分のグラフから読み取れること
# 2階微分のグラフが「正（プラス）」か「負（イナス）」かで元のデータの「勢いの方向」が分かる。

# 2階微分がプラス ($>0$):状態: 下に凸（U字型）。
#意味: 勢いが増している状態です。
# 株価や数値が下がっていても、2階微分がプラスに転じれば下げ止まりや反転の兆しとみれる。

#2階微分がマイナス ($<0$):状態: 上に凸（山型）。
#意味: 勢いが衰えている状態です。
#数値が上がっていても、2階微分がマイナスなら「上昇の限界」や「ピークアウト」が近い。

#2階微分がゼロ ($=0$):状態: 変曲点。
# 意味: 加速から減速（またはその逆）へ切り替わるタイミングです。

#3. なぜ2階微分を表示するのか？
# 1階微分（速度）だけでは、「今上がっている」ことはわかっても、
#その上がりにブレーキがかかり始めているかまでは直感的に判別しにくい。

# 2階微分をグラフ化することで数値がピークに達する一歩手前の予兆やトレンドの強弱を、
# より数学的に厳密に把握しようとしている。
#もしこれが特定の解析ソフトや自作のRスクリプトなどの出力であれば、
#データの「曲がり角」を探そうとしている局面である。


# *******************************************************
# 2026/5/16 
# 直近100日だけを表示するための修正版コード

# 結論：直近100日だけを抽出してプロットするには、
# tail() でデータを切り出してからグラフを描けばOK。
# あなたの既存コード構造を崩さずに、最小限の変更で対応できる形にする。
# 
# 以下の部分を追加するだけで動きます。

# ---- 直近100日のデータに絞る ----
df_recent <- tail(df, 100)
x_recent <- 1:nrow(df_recent)

# スプラインも直近100日で再計算
fit_recent <- smooth.spline(x_recent, df_recent$hist, spar=0.6)
d1_recent <- predict(fit_recent, x_recent, deriv=1)$y
d2_recent <- predict(fit_recent, x_recent, deriv=2)$y

# そして グラフ部分を df_recent に差し替える：

# 終値
plot(df_recent$date, df_recent$close, type="l", main="Close Price (Last 100 Days)")

# MACDヒストグラム + スプライン
plot(df_recent$date, df_recent$hist, type="l", main="MACD Histogram (Last 100 Days)")
lines(df_recent$date, predict(fit_recent)$y)

# 1階微分
plot(df_recent$date, d1_recent, type="l", main="1st Derivative (Last 100 Days)")

# 2階微分
plot(df_recent$date, d2_recent, type="l", main="2nd Derivative (Last 100 Days)")


# ポイント
# df_recent <- tail(df, 100) で直近100日だけ抽出
# 
# スプラインも100日分で再計算する必要があるため、fit_recent を作成
# 
# グラフはすべて df_recent を使う
# 
# 🔍 さらに便利な改善案（必要なら）
# 期間を変えたい場合は N <- 200 のように変数化しておくと便利
# 
# 直近100日を「営業日ベース」で扱っているので、土日祝は自動的に除外される
# 
# スプラインの spar は期間が短くなると影響が大きいので調整しても良い
# 
# 必要なら、
# ・MACDのゴールデンクロス/デッドクロス検出
# ・微分のゼロクロス点の抽出
# ・売買シグナル化
# ・ggplot2 での高品質グラフ化  
# なども作れます。
# 
# 次はどこを強化したいですか？
# MACDシグナル抽出 / 微分のゼロクロス検出 / ggplot2で描画
