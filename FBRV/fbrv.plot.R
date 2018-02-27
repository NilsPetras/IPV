fbrv.plot <- function(coor,size=1,outfilename,width=10,height=10,mycol="black",ticklabel=FALSE,correlation_arrows=FALSE,correlation_labels="all",tick=.1){
  ## (gg-)plotting function creating single factor plots
  # coor is the list returned by the function fbrv.model (or a part of the output of fbrv.calc) containing the coordinates for one factor
  graphics.off()
  
  # preparing inner correlation labels
  if(correlation_labels=="all"){
    inner_cors <- coor$inner_cors
  }
  if(correlation_labels=="inner"){
    inner_cors <- coor$inner_cors[coor$inner_cors$isneighbour==FALSE,]
  }
  if(correlation_labels=="none"){
    inner_cors <- NULL
  }
  
  # creating plot
  myfbrv <- ggplot(coor$cart_circles)+
    geom_point(aes(x=0,y=0,size=size))+
    geom_circle(aes(x0=0,y0=0,r=tick),linetype = "dotted",size=.5*min(c(size,.5)))+
    geom_segment(data = coor$cart_axes,aes(x=x2,y=y2,xend=x3,yend=y3),size=size,color="gray90")+
    geom_text(aes(x,y,label = row.names(coor$cart_circles)),family = "serif",size = 8*sqrt(size))+
    coord_fixed()+
    theme_minimal()+
    aes()+
    geom_circle(data=coor$cart_circles[1,],aes(x0=x,y0=y,r=radius),size=size,color="gray90")+
    geom_circle(data=coor$cart_circles[-1,],aes(x0=x,y0=y,r=radius),size=size,color=mycol)+
    geom_segment(data = coor$cart_axes,aes(x=x0,y=y0,xend=x1,yend=y1),size=sqrt(size)+size,col=mycol)+
    geom_text(data = coor$factor_label,aes(x=x,y=y,label=label),family = "serif",size = 8*sqrt(size),fontface="bold")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),text = element_text(size = 16,family = "serif"),plot.margin = margin(1,1,1,1,"in"))
    # adding optional axis tick label
  if(ticklabel == TRUE) myfbrv <- myfbrv +
    geom_text(data = coor$axis_tick_label,aes(x,y,label = ".1"),family = "serif",size = 4*size)
    # adding optional inner cors
  if(!is.null(inner_cors))myfbrv <- myfbrv +
    geom_text(data = inner_cors,aes(x=x,y=y,label=label),family = "serif",size = 4*sqrt(size))
  if(correlation_arrows==TRUE) myfbrv <- myfbrv +
    geom_segment(data = coor$subfactor_cor_arrows,aes(x=x1,y=y1,xend=x2,yend=y2),arrow = arrow(ends = "both",length = unit(.007*sqrt(size),"native"),type = "closed"),size=size)+
    geom_text(data = coor$subfactor_cor_labels,aes(x,y,label = cor),family = "serif",size = 6*sqrt(size))
    
  
  ggsave(paste(outfilename,".pdf",sep = ""),myfbrv,width = width ,height = height, units = "in",dpi = 3000)
  
  return(myfbrv)
}
