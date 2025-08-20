library(jsonlite)
library(tidyr)
library(ggplot2)
library(dplyr)
library(rlist)
library(pipeR)
library(rgl)
library(patchwork)

rm(list = ls())

jntn<-c('Head','Neck','rSldr','rElbw','rWrst','lSldr','lElbw','lWrst','Wst','rHip','rknee','rAnkl','lHip','lKnee','lAnkl','rEye','lEye','rEar','lEar','lToe','lHeel','rToe','rHeel')
fr<-30

showfig<-TRUE
savefig<-TRUE

# ここからの6行を変更する必要がある
ff<-choose.files()
xwid<-320 # 動画の縦サイズ
ywid<-568 # 動画の横サイズ
height<-170 # 被写体の身長
jd<-choose.dir() # データが入っているフォルダ名
begT<-0 # 解析開始時間（秒）、0の場合は最初から
durT<-0 # 解析時間の長さ（秒）、0の場合は最後まで。
jnts<-c(4,7,0)
#jnt1<-4 # 表示する関節1, 関節番号はスクリプトの末尾参照
#jnt2<-7 # 表示する関節2

# jd<-file.choose()

files<-dir(jd,'*.json')
nf<-length(files)
njnt<-23
jofi<-c( 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,22,24)
prnt<-c(NA,0,1,2,3,1,5,6,1,8,9, 10, 8,12,13, 0, 0,15,16,14,19,11,22)

# 0: head 1: neck 2: right shoulder 3: right elbow 4: right wrist 
# 5: left shoulder 6: left elbow 7: left wrist
# 8: waist
# 9: right hip 10: right knee 11: right ankle
# 12: left hip 13: left knee 14: left ankle
# 15: right eye 16: left eye 17: right ear 18: left ear
# 19: left toe 21: left heel
# 22: right toe 24: right heel


jdf<-data.frame()

for (i in 1:nf){
  jsdt<-read_json(paste(jd, '/', files[i], sep=''))
  datkp<-jsdt$people[[1]]$pose_keypoints_2d
  xdt<-unlist(datkp[jofi*3+1])
  ydt<-ywid-unlist(datkp[jofi*3+2])
  cdt<-unlist(datkp[jofi*3+3])
  xdt[xdt==0]<-NA
  ydt[ydt==ywid]<-NA
  cdt[cdt==0]<-NA
  thisd<-cbind(i, jofi, xdt, ydt, cdt)
  jdf<-rbind(jdf,thisd)
  if (showfig){
    xs<-NULL
    ys<-NULL
    zs<-NULL
    for (j in 2:nrow(thisd)){
      xs<-c(xs,as.numeric(thisd[thisd[,2]==prnt[j],3]))
      ys<-c(ys,as.numeric(thisd[thisd[,2]==prnt[j],4]))
      zs<-c(zs, 0)
      xs<-c(xs,as.numeric(thisd[thisd[,2]==jofi[j],3]))
      ys<-c(ys,as.numeric(thisd[thisd[,2]==jofi[j],4]))
      zs<-c(zs,0)
      
    }
    rgl::view3d(0, 15, 40)
    segments3d(xs, ys, zs, na.rm=T)
    plot3d(xs,ys,zs, size = 5, add=T, na.rm=T)
    box3d()
    if (savefig)
    rgl.snapshot( fmt="png", sprintf("mpfig\\pose%05d.png", i)  )
    # ffmpeg -r 50 -i ./pose%05d.png -pix_fmt yuv420p -vcodec libx264 ./pose.mp4
    next3d()
  }
}
colnames(jdf)<-c('frame','joint','xdt','ydt','cdt')
jdf[which(jdf[,3]==0),3:5]<-NA


jdf<-jdf[jdf$frame>=begT*fr,]
if (durT>0){
  jdf<-jdf[jdf$frame<=begT*fr+durT*fr,]
}

tall<-abs(max(jdf[which(jdf$joint==0),4],na.rm=T)-min(jdf[which(jdf$joint==21),4],na.rm=T))
#smr<-data.frame(matrix(NA,length(jofi),9))
smr<-data.frame()
for (i in 1:length(jofi)){
  smr[i,1]<-jntn[i]
  smr[i,2:5]<-c(min(jdf[which(jdf$joint==jofi[i]),3],na.rm=T),max(jdf[which(jdf$joint==jofi[i]),3],na.rm=T),min(jdf[which(jdf$joint==jofi[i]),4],na.rm=T),max(jdf[which(jdf$joint==jofi[i]),4],na.rm=T))
}

smr<-cbind(smr,smr[,3]-smr[,2])
smr<-cbind(smr,smr[,5]-smr[,4])
smr[,8:9]<-smr[,6:7]/tall*height
colnames(smr)<-c('jnt','minX','maxX','minY','maxY','rangeX','rangeY','rangeX(cm)','rangeY(cm)')

# jdf$joint<-as.factor(jdf$joint)
# g<-ggplot(data=jdf[which(jdf$joint==jnt1 |jdf$joint==jnt2 ),], aes(x=xdt, y=ywid-ydt, color=joint))+geom_point(na.rm=T)
# #g<-ggplot(data=jdf, aes(x=xdt, y=500-ydt, color=joint))+geom_point()
# g<-g+xlim(c(0,xwid))+ylim(c(0,ywid))+scale_color_hue(labels=c(jntn[jnt1==jofi],jntn[jnt2==jofi]))
# plot(g)

ljdf<-pivot_longer(jdf, cols=c('xdt','ydt','cdt'), names_to = 'axis')

g<-list()
for (i in 1:length(jnts)){
  g[[i]]<-ggplot(data=ljdf[(ljdf$axis=='ydt' | ljdf$axis=='xdt') & ljdf$joint==jnts[i],], aes(x=frame/fr, y=value, color=axis))+geom_line(linewidth=1)
  g[[i]]<-g[[i]]+labs(title=jntn[jofi==jnts[i]])
}
wrap_plots(g)+plot_layout(ncol=1)

if (begT>0){
  eT<-max(begT+durT, max(ljdf$frame/fr))
  print(paste0(as.character(begT),'s-',as.character((eT)), 's'))
}
print(smr)

