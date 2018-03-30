fbrv.plot.items <- function(coor,size=1,file_name,
                            width=10,height=10,font="serif",
                            colour="black",colour2="black",tick_label=TRUE,
                            size_title=1,size_axes_labels=1,width_axes=1,size_arrow_heads=1,width_items=1,size_grid=1,size_tick_label=1,size_center_dot=1){
  ## (gg-)plotting function creating the actual graphic
  # coor is the list returned by the function fbrv.model.items containing the coor of the segments to plot
  
  graphics.off()
  
  myfbrv <- ggplot(coor$cart_axes)+
    geom_text(aes(x=xlabel,y=ylabel,label = row.names(coor$cart_axes)),family = font,size = 6*sqrt(size)*size_axes_labels,hjust = "inward")+
    coord_fixed()+
    theme_minimal()+
    aes()+
    geom_point(aes(x=0,y=0),size=.5*size*size_center_dot)+
    geom_circle(data = coor$maingrid[coor$maingrid$alpha==.5,],aes(x0=x,y0=y,r=r),col = "gray90",linetype = "dotted",size=min(c(size,.75))*size_grid)+
    geom_circle(data = coor$maingrid[coor$maingrid$alpha==1,],aes(x0=x,y0=y,r=r),col = "gray20",linetype = "dotted",size=min(c(size,.75))*size_grid)+
    geom_segment(data = coor$cart_axes,aes(x=0,y=0,xend=x,yend=y),arrow = arrow(ends = "last",length = unit(.02*sqrt(size)*size_arrow_heads,"native"),type = "closed"),size=.5*sqrt(size)*width_axes)+
    geom_segment(data = coor$items[coor$items$width==max(coor$items$width),],aes(x=x1,y=y1,xend=x2,yend=y2),size=1,col = colour2)+
    geom_segment(data = coor$items[coor$items$width==min(coor$items$width),],aes(x=x1,y=y1,xend=x2,yend=y2),size=1,col = colour)+
    # geom_segment(data = coor$items,aes(x=x1,y=y1,xend=x2,yend=y2),size=size*width_items,col = colour)+
    geom_text(data = coor$factor_label,aes(x=x,y=y,label = label),family = font,size = 6*sqrt(size)*size_title,col=colour,fontface="bold")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),text = element_text(size = 16,family = font),plot.margin = margin(1,1,1,1,"in"))
  if(tick_label == TRUE && max(coor$maingrid$r)>=.5) myfbrv <- myfbrv +  geom_text(data = coor$axis_tick,aes(x,y,label = label),angle = (coor$axis_tick$phi-pi/48-pi/2)*180/pi,family = font,size = 3*sqrt(size)*size_tick_label,col = "gray20")
  if(max(coor$maingrid$alpha)==.5) myfbrv <- myfbrv + geom_circle(data = coor$maingrid[coor$maingrid$alpha==.5,],aes(x0=x,y0=y,r=r),col = "gray45",linetype = "dotted",size=min(c(size,.75))*size_grid)
  
  ggsave(paste(file_name,".pdf",sep = ""),myfbrv,width = width ,height = height, units = "in",dpi = 3000)
  
  return(myfbrv)
}