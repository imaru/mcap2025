# 必要パッケージ
suppressPackageStartupMessages({
  library(RMoCap)
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(tidyr)       # pivot_*
  library(RcppRoll)    # 高速移動平均（任意）
  library(rgl)
})

rm(list=ls())

fl <- choose.files()
dat <- RMoCap::read.bvh(fl)

showfig <- TRUE
savefig <- FALSE

# ---- 0) メタ情報 ----
J <- length(dat$Joints)       # ジョイント数
N <- dat$Frames               # フレーム数
FT <- dat$FrameTime
jntname <- vapply(dat$Joints, function(x) x$Name, character(1))

# 親インデックスと使用フラグ（元コードに合わせて Parent>0 & Nchannels>0 を採用）
parent_idx <- vapply(dat$Joints, function(x) as.integer(x$Parent), integer(1))
use_edge <- (parent_idx > 0) & (vapply(dat$Joints, function(x) as.integer(x$Nchannels), integer(1)) > 0)
edge_child  <- which(use_edge)
edge_parent <- parent_idx[edge_child]

# ---- 1) 位置行列を一気に作る（N×J 行列を x/y/z で用意） ----
# dat$Joints[[i]]$Dxyz は N×3
X <- do.call(cbind, lapply(dat$Joints, function(j) j$Dxyz[, 1, drop = TRUE]))
Y <- do.call(cbind, lapply(dat$Joints, function(j) j$Dxyz[, 2, drop = TRUE]))
Z <- do.call(cbind, lapply(dat$Joints, function(j) j$Dxyz[, 3, drop = TRUE]))
colnames(X) <- colnames(Y) <- colnames(Z) <- jntname

# ---- 2) フレーム間の移動距離（元の dist 行列）をベクトル化で ----
dX <- rbind(NA, diff(X))  # N×J
dY <- rbind(NA, diff(Y))
dZ <- rbind(NA, diff(Z))
dist_mat <- sqrt(dX^2 + dY^2 + dZ^2)  # 要素ごと、N×J、1行目は NA

# 平滑化（元は rollmean(dist, 1/FrameTime)）
# RcppRoll::roll_mean は列ごとに走らせるのが速い
k <- max(1L, round(1 / FT))  # 窓幅
dist_sm <- apply(dist_mat, 2, function(col) RcppRoll::roll_mean(col, n = k, fill = NA, align = "center"))
rdist <- data.table(frame = seq_len(N), dist_sm)
setnames(rdist, old = names(rdist)[-1], new = jntname)

# ---- 3) 時系列ロングテーブル（元 ldat / wdat 相当）を一発で作る ----
# myDT
# 
# DT <- data.table(
#   frame = rep.int(seq_len(N), times = 3 * J),
#   axis  = rep(rep(c("x", "y", "z"), each = N), times = J),
#   name  = rep(jntname, each = 3 * N),
#   value = c(as.vector(X), as.vector(Y), as.vector(Z))
# )

base_frame <- rep(seq_len(N), times = J)     # 各関節ごとに 1..N
base_name  <- rep(jntname, each = N)         # X/Y/Z で共通に使う

DT <- data.table::rbindlist(list(
  data.table::data.table(frame = base_frame, name = base_name, axis = "x", value = as.vector(X)),
  data.table::data.table(frame = base_frame, name = base_name, axis = "y", value = as.vector(Y)),
  data.table::data.table(frame = base_frame, name = base_name, axis = "z", value = as.vector(Z))
))

# pivot_wider 相当は必要になったときに行う（下では使っていない）

# dist のロング
ldist <- rdist |>
  pivot_longer(cols = -frame, names_to = "name", values_to = "value")

# ---- 4) プロット（選択ジョイントの位置と速度を同時に）----
jnts <- c(13, 26, 31)  # 例: head / l_hand / r_hand
pick_names <- jntname[jnts]

p_pos <- ggplot(DT[name %in% pick_names],
                aes(x = frame * FT, y = value, color = axis)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ name, ncol = length(pick_names), scales = "free_y") +
  labs(x = "time [s]", y = "position", title = "Joint positions (x/y/z)")

ldist_pick <- ldist[ldist$name %in% pick_names, ]
p_spd <- ggplot(ldist_pick,
                aes(x = frame * FT, y = value)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ name, ncol = length(pick_names), scales = "free_y") +
  labs(x = "time [s]", y = "frame-to-frame displacement", title = sprintf("Per-frame speed (smoothed, k=%d)", k))+
  ylim(c(0,max(ldist_pick$value, na.rm = T)))

p_pos / p_spd

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

