densityPlot <- function(p, two_sided, qleft, qright, density=dnorm,
                        xminmax=NULL) {
  vline_right <- geom_vline(xintercept=qleft,lty=2)
  
  
  if (is.null(xminmax)) {
     minmax <- max(3, qright*1.2)
     x <- seq(-minmax,+minmax,0.1)
  } else {
    x <- seq(xminmax[1],xminmax[2], (xminmax[2]-xminmax[1])/100)
  }
  
    y <- density(x)
  y_right <- ifelse(x<qleft, 0, y)
  y_left <- ifelse(x>qright,0, y)
  
  lab <- paste0(round((1-p)*100,2),"%")
  hght <- mean(y)
  vtext_right <- geom_text(aes(x=qleft*1.2,y=hght,label=lab))
  vtext_left <- geom_text(aes(x=qright*1.2,y=hght,label=lab))
  
  
  if (two_sided) {
    vline_left <-  geom_vline(xintercept=qright,lty=2)
  } else { 
    vline_left <- NULL
    vtext_left <- NULL
  }
  
  ribbon_right_df <- data.frame(x=x,y_right=y_right)
  ribbon_left_df <- data.frame(x=x,y_left=y_left)
  ribbon_left_df[-which(ribbon_left_df$x==0)]
  ribbon_right_df[-which(ribbon_right_df$x==0)]
  
  ribbon_right <- geom_ribbon(data=ribbon_right_df,
                              aes(ymin=0,ymax=y_right,x=x),fill="lightblue")
  
  if (two_sided) {
    ribbon_left <- geom_ribbon(aes(ymin=0,ymax=y_left,x=x),fill="lightblue")
  } else {
    ribbon_left <- NULL
  } 
  

  
  plot <- ggplot(data=data.frame(x,y),
                 aes(x=x,y=y,y_left=y_left,y_right=y_right))+
    
    ylab("Density")+
    xlab("")+
    ribbon_left+
    ribbon_right+
    geom_line(lwd=2,color="navy")+
    vline_right+vline_left+
    vtext_right+
    vtext_left+
    theme_classic()+
    theme(axis.title=element_text(size=16),
          axis.text = element_text(size=16) )
  
}