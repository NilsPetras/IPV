#' Title
#'
#' @param coor 
#' @param size 
#' @param file_name 
#' @param width 
#' @param height 
#' @param cor_labels 
#' @param subfactor_cor_labels 
#' @param colour 
#' @param subcolour 
#' @param extra_arrows 
#' @param tick 
#' @param font 
#' @param size_title 
#' @param size_subfactor_labels 
#' @param size_subfactor_labels_inner 
#' @param width_axes 
#' @param width_axes_inner 
#' @param width_circles 
#' @param width_circles_inner 
#' @param size_tick 
#' @param size_tick_inner 
#' @param size_tick_label 
#' @param size_cor_labels 
#' @param size_cor_labels_inner 
#' @param size_center_dot 
#' @param size_center_dot_inner 
#' @param size_extra_arrows 
#' @param size_extra_arrow_heads 
#' @param size_extra_labels 
#'
#' @return
plot_nested <- function(coor,size=1,file_name,width=10,height=10,
                      cor_labels=TRUE,subfactor_cor_labels=FALSE,
                      colour="black",subcolour="black",extra_arrows=FALSE,tick=.1,font="serif",
                      size_title=1,size_subfactor_labels=1,size_subfactor_labels_inner=1,width_axes=1,width_axes_inner=1,
                      width_circles=1,width_circles_inner=1,size_tick=1,size_tick_inner=1,size_tick_label=1,size_cor_labels=1,size_cor_labels_inner=1,
                      size_center_dot=1,size_center_dot_inner=1,size_extra_arrows=1,size_extra_arrow_heads=1,size_extra_labels=1){
  ## (gg-)plotting function creating all plots
  # coor is the list returned by the function fbrv.calc containing all coordinates for all plots
  
  graphics.off()
  
  # setting up a folder to save the files in
  
  # single factor plots
  
  # item plots
  
  # nested global plot
  
  # preparing inner cor labels
  if(cor_labels==TRUE){
    inner_cors <- coor$global$inner_cors
  }
  else inner_cors <- NULL
  
  # preparing subfactor inner cor labels
  if(subfactor_cor_labels==TRUE){
    subfactor_inner_cors <- coor$global$nested$inner_cors
  }
  else subfactor_inner_cors <- NULL
  
  globalplot <- ggplot(coor$global$cart_circles)+
    # geom_text(aes(x,y,label = row.names(coor$global$cart_circles)),family = font,size = 8*sqrt(size))+
    coord_fixed()+
    theme_minimal()+
    aes()+
    geom_text(data=coor$global$axis_tick,aes(x=coor$global$relative_scaling*(x*tick+sign(x)*.02),y=coor$global$relative_scaling*(y*tick+sign(y)*.02),label=as.character(tick)),angle = (coor$global$axis_tick$phi-pi/48-pi/2)*180/pi,family = font,size = 1.5*sqrt(size)*size_tick_label)+
    geom_circle(aes(x0=0,y0=0,r=coor$global$relative_scaling*tick),linetype = "dotted",size=coor$global$relative_scaling*.5*min(c(size,.25))*size_tick)+
    geom_segment(data = coor$global$cart_axes,aes(x=x2,y=y2,xend=x3,yend=y3),size=.5*size*width_axes,color="gray90")+
    geom_circle(data=coor$global$cart_circles[1,],aes(x0=x,y0=y,r=radius),size=.5*size*width_axes,color="gray90")+
    geom_point(aes(x=0,y=0),size=size*size_center_dot)+
    geom_segment(data = coor$global$nested$axes,aes(x=x2,y=y2,xend=x3,yend=y3),size=.3*size*width_axes_inner,color="gray90")+
    geom_point(data=coor$global$cart_circles,aes(x=x,y=y),size=.5*size*size_center_dot_inner)+
    geom_circle(data=coor$global$cart_circles[-1,],aes(x0=x,y0=y,r=radius),size=.5*size*width_circles,color=colour)+
    geom_circle(data=coor$global$cart_circles[-1,],aes(x0=x,y0=y,r=tick),size=0.5*min(c(size,.25))*size_tick_inner,linetype = "dotted")+
    geom_segment(data = coor$global$cart_axes,aes(x=x0,y=y0,xend=x1,yend=y1),size=.5*(sqrt(size)+size)*width_axes,color=colour)+
    geom_circle(data=coor$global$nested$circles,aes(x0=x,y0=y,r=radius),size=.25*size*width_circles_inner,color=subcolour)+
    geom_text(data=coor$global$nested$circles,aes(x,y,label = label),family = font,size = 2.5*sqrt(size)*size_subfactor_labels_inner)+
    geom_segment(data = coor$global$nested$axes,aes(x=x0,y=y0,xend=x1,yend=y1),size=.25*(sqrt(size)+size)*width_axes_inner,color=subcolour)+
    geom_text(data = coor$global$factor_label,aes(x=x,y=y,label=label),family = font,size = 6*sqrt(size)*size_title,fontface="bold")+
    geom_text(data = coor$global$nested$factor_label,aes(x=x,y=y,label=label),family = font,size = 4*sqrt(size)*size_subfactor_labels,fontface="bold")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),text = element_text(size = 16,family = font),plot.margin = margin(1,1,1,1,"in"))
  
  if(!is.null(subfactor_inner_cors)){globalplot <- globalplot +
    geom_text(data = subfactor_inner_cors,aes(x=x,y=y,label=label),family = font,size = 1.5*sqrt(size)*size_cor_labels_inner)}
  if(!is.null(inner_cors)){globalplot$layers <- c(geom_circle(data=coor$global$cart_inner_ring,aes(x0=x,y0=y,r=radius),size=.3*size*width_axes_inner,color="gray90"),globalplot$layers)
    globalplot <-  globalplot + geom_text(data = inner_cors,aes(x=x,y=y,label=label),family = font,size = coor$global$cor_spacing*7.5*sqrt(size)*size_cor_labels,fontface="bold")}
  if(extra_arrows==TRUE){globalplot <- globalplot +
    geom_segment(data = coor$global$arrows,aes(x=x1,y=y1,xend=x2,yend=y2),arrow = arrow(ends = "both",length = unit(.003*sqrt(size)*size_extra_arrow_heads,"native"),type = "closed"),size=.25*size*size_extra_arrows,linetype = "dotted",color="gray20")+
    geom_text(data = coor$global$arrows,aes(x=xlabel,y=ylabel,label=label),size=2*sqrt(size)*size_extra_labels,family=font,color="gray20")}
    
  ggsave(paste(file_name,".pdf",sep = ""),globalplot,width = width ,height = height, units = "in",dpi = 3000)
  
  return(globalplot)
  
  # plots <- list(global=globalplot,)
  # return(plots)
  
}