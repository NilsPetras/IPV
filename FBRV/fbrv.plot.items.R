fbrv.plot.items <- function(coor_items,size=1,outfilename,width=10,height=10,mycol="black",ticklabel=TRUE){
  ## (gg-)plotting function creating the actual graphic
  # coor_items is the list returned by the function fbrv.model.items containing the coor of the segments to plot
  
  graphics.off()
  
  myfbrv <- ggplot(coor_items$cart_axes)+
    geom_text(aes(x=xlabel,y=ylabel,label = row.names(coor_items$cart_axes)),family = "serif",size = 6*sqrt(size),hjust = "inward")+
    coord_fixed()+
    theme_minimal()+
    aes()+
    geom_point(aes(x=0,y=0),size=.5*size)+
    geom_circle(data = coor_items$maingrid[-seq(from = 5, by = 5, to = length(coor_items$maingrid$alpha)),],aes(x0=x,y0=y,r=r),col = "gray90",linetype = "dotted",size=min(c(size,.75)))+
    geom_circle(data = coor_items$maingrid[seq(from = 5, by = 5, to = length(coor_items$maingrid$alpha)),],aes(x0=x,y0=y,r=r),col = "gray20",linetype = "dotted",size=min(c(size,.75)))+
    geom_segment(data = coor_items$cart_axes,aes(x=0,y=0,xend=x,yend=y),arrow = arrow(ends = "last",length = unit(.02*sqrt(size),"native"),type = "closed"),size=.5*sqrt(size))+
    geom_segment(data = coor_items$items,aes(x=x1,y=y1,xend=x2,yend=y2),size=size,col = mycol)+
    geom_text(data = coor_items$factor_label,aes(x=x,y=y,label = label),family = "serif",size = 6*sqrt(size),color=mycol,fontface="bold")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),text = element_text(size = 16,family = "serif"),plot.margin = margin(1,1,1,1,"in"))
  if(ticklabel == TRUE && max(coor_items$maingrid$r)>=.5) myfbrv <- myfbrv +  geom_text(data = coor_items$axis_tick,aes(x,y,label = "0.5"),angle = (coor_items$axis_tick$phi-pi/48-pi/2)*180/pi,family = "serif",size = 2*size,col = "gray20")
  
  ggsave(paste(outfilename,".pdf",sep = ""),myfbrv,width = width ,height = height, units = "in",dpi = 3000)
  
  return(myfbrv)
}