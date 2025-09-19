# RMoCcapパッケージはRstudioのメニューからインストールできないので、
# 以下の2行を実行してインストールする
# 一度インストールしたら、その後は行頭に # をつけておく

install.packages("remotes")
remotes::install_github("browarsoftware/RMoCap")

# 必要パッケージ, まだインストールしていない場合はPackagesタブからインストールしておく
library(RMoCap)
library(data.table)
library(ggplot2)
library(patchwork)
library(tidyr)       # pivot_*
library(RcppRoll)    # 高速移動平均（任意）
library(rgl)

rm(list=ls()) # メモリ内をクリア

fl <- choose.files()  # 結果ファイルの選択
dat <- RMoCap::read.bvh(fl)  # データ読み込み

showfig <- TRUE  # FALSEにすると棒人間を表示しない
savefig <- FALSE  # TRUEにすると棒人間の静止画ファイルを保存する, 保存する場合tempというフォルダを作っておく必要がある

# ---- 0) メタ情報 ----
J <- length(dat$Joints)       # 記録されている関節の数
N <- dat$Frames               # フレーム数（記録時間 x 50）
FT <- dat$FrameTime
jntname <- vapply(dat$Joints, function(x) x$Name, character(1))  # 関節名の抽出

# 棒人間を描くための処理
parent_idx <- vapply(dat$Joints, function(x) as.integer(x$Parent), integer(1))
use_edge <- (parent_idx > 0) & (vapply(dat$Joints, function(x) as.integer(x$Nchannels), integer(1)) > 0)
edge_child  <- which(use_edge)
edge_parent <- parent_idx[edge_child]

# 座標データの抽出
X <- do.call(cbind, lapply(dat$Joints, function(j) j$Dxyz[, 1, drop = TRUE]))
Y <- do.call(cbind, lapply(dat$Joints, function(j) j$Dxyz[, 2, drop = TRUE]))
Z <- do.call(cbind, lapply(dat$Joints, function(j) j$Dxyz[, 3, drop = TRUE]))
colnames(X) <- colnames(Y) <- colnames(Z) <- jntname

# フレーム間の移動距離を算出
dX <- rbind(NA, diff(X))  # N×J
dY <- rbind(NA, diff(Y))
dZ <- rbind(NA, diff(Z))
dist_mat <- sqrt(dX^2 + dY^2 + dZ^2)  # 要素ごと、N×J、1行目は NA

# データの平滑化（元は rollmean(dist, 1/FrameTime)）
k <- max(1L, round(1 / FT))  # 窓幅
dist_sm <- apply(dist_mat, 2, function(col) RcppRoll::roll_mean(col, n = k, fill = NA, align = "center"))
rdist <- data.table(frame = seq_len(N), dist_sm)
setnames(rdist, old = names(rdist)[-1], new = jntname)

# ---- 3) 時系列ロングテーブル（元 ldat / wdat 相当）を一発で作る ----
base_frame <- rep(seq_len(N), times = J)     # 各関節ごとに 1..N
base_name  <- rep(jntname, each = N)         # X/Y/Z で共通に使う

DT <- data.table::rbindlist(list(
  data.table::data.table(frame = base_frame, name = base_name, axis = "x", value = as.vector(X)),
  data.table::data.table(frame = base_frame, name = base_name, axis = "y", value = as.vector(Y)),
  data.table::data.table(frame = base_frame, name = base_name, axis = "z", value = as.vector(Z))
))

# グラフ表示のためのデータ作成
ldist <- rdist |>
  pivot_longer(cols = -frame, names_to = "name", values_to = "value")

# グラフ作成
# 一度にグラフを作るのは4つの関節くらいまでが見やすい
# この下の行でグラフ表示したい関節を選ぶ/関節と番号の対応はファイル末尾参照
jnts <- c(13, 26, 31)  # 例: head / l_hand / r_hand
pick_names <- jntname[jnts]

# 関節位置のグラフ（各関節のx, y, z座標）
p_pos <- ggplot(DT[name %in% pick_names],
                aes(x = frame * FT, y = value, color = axis)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ name, ncol = length(pick_names), scales = "free_y") +
  labs(x = "time [s]", y = "position", title = "Joint positions (x/y/z)")

# 移動速度のグラフ（各関節の移動速度の変化のグラフ）
ldist_pick <- ldist[ldist$name %in% pick_names, ]
p_spd <- ggplot(ldist_pick,
                aes(x = frame * FT, y = value)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ name, ncol = length(pick_names), scales = "free_y") +
  labs(x = "time [s]", y = "frame-to-frame displacement", title = sprintf("Per-frame speed (smoothed, k=%d)", k))+
  ylim(c(0,max(ldist_pick$value, na.rm = T)))

# 位置と速度のグラフを表示
p_pos / p_spd

# 棒人間の表示
if (showfig) {
  # 予めベクトル化で child-parent の座標を取り出す関数
  make_segments_vec <- function(f) {
    # f行めの親子座標（ベクトル）
    xp <- X[f, edge_parent]; xc <- X[f, edge_child]
    yp <- Y[f, edge_parent]; yc <- Y[f, edge_child]
    zp <- Z[f, edge_parent]; zc <- Z[f, edge_child]
    # rgl::segments3d は (x1,x2,x1,x2,...) という並びを期待するので交互に並べる
    xs <- as.vector(rbind(xp, xc))
    ys <- as.vector(rbind(yp, yc))
    zs <- as.vector(rbind(zp, zc))
    list(xs = xs, ys = ys, zs = zs)
  }
  
  rgl::open3d()
  rgl::view3d(-25, 15, 40)
  
  for (ff in seq_len(N)) {
    seg <- make_segments_vec(ff)
    rgl::segments3d(seg$xs, seg$ys, seg$zs)
    rgl::points3d(seg$xs, seg$ys, seg$zs, size = 5)
    rgl::box3d()
    
    if (savefig) {
      rgl.snapshot(fmt = "png", sprintf("temp/pose%05d.png", ff))
    }
    Sys.sleep(FT)
    rgl::clear3d(type = "shapes")  # 次フレーム用に形状だけクリア（背景等は維持）
  }
}

# 関節と番号の対応, 例えば頭部は11番
# 1 "root" 2 "torso_1" 3 "torso_2" 4 "torso_3" 5 "torso_4" 6 "torso_5" 7 "torso_6" 8 "torso_7"
# 9 "neck_1" 10 "neck_2" 11 "head" 12 "EndSite12" 13 "l_shoulder" 14 "l_up_arm" 15 "l_low_arm" 16 "l_hand"
# 17 "EndSite17" 18 "r_shoulder" 19 "r_up_arm" 20 "r_low_arm" 21 "r_hand" 22 "EndSite22"
# 23 "l_up_leg" 24 "l_low_leg" 25 "l_foot" 26 "l_toes" 27 "EndSite27"
# 28 "r_up_leg" 29 "r_low_leg" 30 "r_foot" 31 "r_toes" 32 "EndSite32"
