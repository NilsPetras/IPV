fbrv.draw <- function(coor,size=1,outfilename,width=10,height=10,ticklabel=FALSE,
                      correlation_labels="all",correlation_arrows=F,subfactor_correlation_labels="none",
                      subfactor_correlation_arrows=F,mycol="black",mysubcol="black",extra_arrows=F,tick=.1){
  ## (gg-)plotting function creating all plots
  # coor is the list returned by the function fbrv.calc containing all coordinates for all plots
  
  graphics.off()
  
  # setting up a folder to save the files in
  
  # single factor plots
  
  # item plots
  
  # nested global plot
  
  # preparing inner correlation labels
  if(correlation_labels=="all"){
    inner_cors <- coor$global$inner_cors
  }
  if(correlation_labels=="inner"){
    inner_cors <- coor$global$inner_cors[coor$global$inner_cors$isneighbour==FALSE,]
  }
  if(correlation_labels=="none"){
    inner_cors <- NULL
  }
  
  # preparing subfactor inner correlation labels
  if(subfactor_correlation_labels=="all"){
    subfactor_inner_cors <- coor$global$nested$inner_cors
  }
  if(subfactor_correlation_labels=="inner"){
    subfactor_inner_cors <- coor$global$nested$inner_cors[coor$global$nested$inner_cors$isneighbour==FALSE,]
  }
  if(subfactor_correlation_labels=="none"){
    subfactor_inner_cors <- NULL
  }
  
  globalplot <- ggplot(coor$global$cart_circles)+
    # geom_text(aes(x,y,label = row.names(coor$global$cart_circles)),family = "serif",size = 8*sqrt(size))+
    coord_fixed()+
    theme_minimal()+
    aes()+
    geom_circle(aes(x0=0,y0=0,r=coor$global$relative_scaling*tick),linetype = "dotted",size=coor$global$relative_scaling*.5*min(c(size,.25)))+
    geom_segment(data = coor$global$cart_axes,aes(x=x2,y=y2,xend=x3,yend=y3),size=.5*size,color="gray90")+
    geom_circle(data=coor$global$cart_circles[1,],aes(x0=x,y0=y,r=radius),size=.5*size,color="gray90")+
    geom_point(aes(x=0,y=0),size=size)+
    geom_segment(data = coor$global$nested$axes,aes(x=x2,y=y2,xend=x3,yend=y3),size=.25*(sqrt(size)+size),color="gray90")+
    geom_point(data=coor$global$cart_circles,aes(x=x,y=y),size=.5*size)+
    geom_circle(data=coor$global$cart_circles[-1,],aes(x0=x,y0=y,r=radius),size=.5*size,color=mycol)+
    geom_circle(data=coor$global$cart_circles[-1,],aes(x0=x,y0=y,r=tick),size=0.5*min(c(size,.25)),linetype = "dotted")+
    geom_segment(data = coor$global$cart_axes,aes(x=x0,y=y0,xend=x1,yend=y1),size=.5*(sqrt(size)+size),color=mycol)+
    geom_circle(data=coor$global$nested$circles,aes(x0=x,y0=y,r=radius),size=.25*size,color=mysubcol)+
    geom_text(data=coor$global$nested$circles,aes(x,y,label = labels),family = "serif",size = 2.5*sqrt(size))+
    geom_segment(data = coor$global$nested$axes,aes(x=x0,y=y0,xend=x1,yend=y1),size=.25*(sqrt(size)+size),color=mysubcol)+
    geom_text(data = coor$global$factor_label,aes(x=x,y=y,label=label),family = "serif",size = 6*sqrt(size),fontface="bold")+
    geom_text(data = coor$global$nested$factor_label,aes(x=x,y=y,label=label),family = "serif",size = 4*sqrt(size),fontface="bold")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),text = element_text(size = 16,family = "serif"),plot.margin = margin(1,1,1,1,"in"))
  
  if(!is.null(subfactor_inner_cors)){globalplot <- globalplot +
    geom_text(data = subfactor_inner_cors,aes(x=x,y=y,label=label),family = "serif",size = 1.5*sqrt(size))}
  if(subfactor_correlation_arrows==TRUE) {globalplot <- globalplot +
    geom_segment(data = coor$global$nested$subfactor_cor_arrows,aes(x=x1,y=y1,xend=x2,yend=y2),arrow = arrow(ends = "both",length = unit(.003*sqrt(size),"native"),type = "closed"),size=.25*size)+
    geom_text(data = coor$global$nested$subfactor_cor_labels,aes(x,y,label = cor),family = "serif",size = 1.5*sqrt(size)) } 
  if(!is.null(inner_cors)){globalplot$layers <- c(geom_circle(data=coor$global$cart_inner_ring,aes(x0=x,y0=y,r=radius),size=.5*size,color="gray90"),globalplot$layers)
    globalplot <-  globalplot + geom_text(data = inner_cors,aes(x=x,y=y,label=label),family = "serif",size = coor$global$correlation_spacing*5*size,fontface="bold")}
  if(extra_arrows==TRUE){globalplot <- globalplot +
    geom_segment(data = coor$global$extra_arrows,aes(x=x1,y=y1,xend=x2,yend=y2),arrow = arrow(ends = "both",length = unit(.003*sqrt(size),"native"),type = "closed"),size=.25*size,linetype = "dotted",color="gray20")+
    geom_text(data = coor$global$extra_arrows,aes(x=xlabel,y=ylabel,label=label),size=2*sqrt(size),family="serif",color="gray20")}
    
  ggsave(paste(outfilename,".pdf",sep = ""),globalplot,width = width ,height = height, units = "in",dpi = 3000)
  
  return(globalplot)
  
  # plots <- list(global=globalplot,)
  # return(plots)
  
}