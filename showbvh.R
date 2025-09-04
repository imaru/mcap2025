# install.packages("remotes")
# remotes::install_github("browarsoftware/RMoCap")

library(RMoCap)
library(rgl)
library(htmltools)
library(ggplot2)
library(patchwork)

fl<-choose.files()
dat<-read.bvh(fl)

showfig<-TRUE
savefig<-FALSE

# 1 "root" 2 "torso_1" 3 "torso_2" 4 "torso_3" 5 "torso_4" 6 "torso_5" 7 "torso_6" 8 "torso_7"
# 9 "neck_1" 10 "neck_2" 11 "head" 12 "EndSite12" 13 "l_shoulder" 14 "l_up_arm" 15 "l_low_arm" 16 "l_hand"
# 17 "EndSite17" 18 "r_shoulder" 19 "r_up_arm" 20 "r_low_arm" 21 "r_hand" 22 "EndSite22"
# 23 "l_up_leg" 24 "l_low_leg" 25 "l_foot" 26 "l_toes" 27 "EndSite27"
# 28 "r_up_leg" 29 "r_low_leg" 30 "r_foot" 31 "r_toes" 32 "EndSite32"

jnts <- c(11 ,16, 21)

# for (i in 1:length(dat$Joints)){
#   print(dat$Joints[[i]]$Name)
# }



ldat<-data.frame()
for (ff in 1:dat$Frames){
  xs<-NULL
  ys<-NULL
  zs<-NULL
  for (i in 1:length(dat$Joints)){
    #for (i in 1:10){
    #print(paste(as.character(i), dat$Joints[[i]]$Name))
    if (dat$Joints[[i]]$Parent>0 & dat$Joints[[i]]$Nchannels>0){
      xs<-c(xs, dat$Joints[[dat$Joints[[i]]$Parent]]$Dxyz[ff,1])
      ys<-c(ys, dat$Joints[[dat$Joints[[i]]$Parent]]$Dxyz[ff,2])
      zs<-c(zs, dat$Joints[[dat$Joints[[i]]$Parent]]$Dxyz[ff,3])
      xs<-c(xs, dat$Joints[[i]]$Dxyz[ff,1])
      ys<-c(ys, dat$Joints[[i]]$Dxyz[ff,2])
      zs<-c(zs, dat$Joints[[i]]$Dxyz[ff,3])
      ldat<-rbind(ldat, c(dat$Joints[[i]]$Name,ff,'x',dat$Joints[[i]]$Dxyz[ff,1]))
      ldat<-rbind(ldat, c(dat$Joints[[i]]$Name,ff,'y',dat$Joints[[i]]$Dxyz[ff,2]))
      ldat<-rbind(ldat, c(dat$Joints[[i]]$Name,ff,'z',dat$Joints[[i]]$Dxyz[ff,3]))
    }
  }
  if (showfig){
    rgl::view3d(-25, 15, 40)
    segments3d(xs, ys, zs)
    plot3d(xs,ys,zs, size = 5, add=T)
    box3d()
    if (savefig)
      rgl.snapshot( fmt="png", sprintf("temp\\pose%05d.png", ff)  )
  # ffmpeg -r 50 -i ./pose%05d.png -pix_fmt yuv420p -vcodec libx264 ./pose.mp4
    Sys.sleep(dat$FrameTime)
    next3d()
  }
}

colnames(ldat)<-c('name','frame','axis','value')

g<-list()
for (i in 1:length(jnts)){
  g[[i]]<-ggplot(data=ldat[which(ldat$name==dat$Joints[[jnts[i]]]$Name),],aes(x=as.numeric(frame)*dat$FrameTime, y=as.numeric(value), color=axis))+geom_line(linewidth=1)
  g[[i]]<-g[[i]]+labs(title=dat$Joints[[jnts[i]]]$Name)
}
wrap_plots(g)+plot_layout(ncol=1)


