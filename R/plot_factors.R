#' Plot Factors
#'
#' This function plots factors and its loadings. Nils can do a 
#' better description, cause he invented it. 
#'
#' @param coor what type, what it does?
#' @param size integer relative plot size, defaults to one.
#' @param file_name character string denoting the name of the file the plot is written to.
#' @param width integer, defaults to 10.
#' @param height integer, defaults to 10.
#' @param colour character
#' @param cor_labels logical defaults to TRUE.
#' @param tick numeric defaults to .1
#' @param font character defaults to 'serif'.
#' @param size_title integer, defaults to 1.
#' @param size_subfactor_label integer defaults to 1.
#' @param width_circles integer defaults to 1. 
#' @param size_tick integer defaults to 1. 
#' @param size_tick_label integer defaults to 1. 
#' @param size_cor_labels integer defaults to 1. 
#' @param size_center_dot integer defaults to 1. 
#' @export
plot_factors <- function(coor,size=1,file_name,
                      width=10,height=10,colour="black",cor_labels=TRUE,tick=.1,font="serif",
                      size_title=1,size_subfactor_labels=1,width_axes=1,width_circles=1,size_tick=1,size_tick_label=1,size_cor_labels=1,size_center_dot=1){
  ## (gg-)plotting function creating single factor plots
  # coor is the list returned by the function fbrv.model (or a part of the output of fbrv.calc) containing the coordinates for one factor
  graphics.off()
  
  # preparing inner cor labels
  if(cor_labels==TRUE){
    inner_cors <- coor$inner_cors
  }
  else inner_cors <- NULL
  
  # creating plot
  myfbrv <- ggplot(coor$cart_circles)+
    geom_point(aes(x=0,y=0),size=5*sqrt(size)*size_center_dot)+
    geom_circle(aes(x0=0,y0=0,r=tick),linetype = "dotted",size=.5*min(c(size,.5))*size_tick)+
    geom_segment(data = coor$cart_axes,aes(x=x2,y=y2,xend=x3,yend=y3),size=size*width_axes,color="gray90")+
    geom_text(aes(x,y,label = row.names(coor$cart_circles)),family = font,size = 8*sqrt(size)*size_subfactor_labels)+
    coord_fixed()+
    theme_minimal()+
    aes()+
    geom_text(data=coor$axis_tick,aes(x=x*tick+sign(x)*.02,y=y*tick+sign(y)*.02,label=as.character(tick)),angle = (coor$axis_tick$phi-pi/48-pi/2)*180/pi,family = font,size = 2*sqrt(size)*size_tick_label)+
    geom_circle(data=coor$cart_circles[1,],aes(x0=x,y0=y,r=radius),size=size*width_axes,color="gray90")+
    geom_circle(data=coor$cart_circles[-1,],aes(x0=x,y0=y,r=radius),size=size*width_circles,color=colour)+
    geom_segment(data = coor$cart_axes,aes(x=x0,y=y0,xend=x1,yend=y1),size=(sqrt(size)+size)*width_axes,col=colour)+
    geom_text(data = coor$factor_label,aes(x=x,y=y,label=label),family = font,size = 8*sqrt(size)*size_title,fontface="bold")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),text = element_text(size = 16,family = font),plot.margin = margin(1,1,1,1,"in"))
    # adding optional inner cors
  if(!is.null(inner_cors))myfbrv <- myfbrv +
    geom_text(data = inner_cors,aes(x=x,y=y,label=label),family = font,size = 4*sqrt(size)*size_cor_labels)
  
  ggsave(paste(file_name,".pdf",sep = ""),myfbrv,width = width ,height = height, units = "in",dpi = 3000)
  
  return(myfbrv)
}
