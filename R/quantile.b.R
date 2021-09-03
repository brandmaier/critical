
# This file is a generated template, your changes will not be overwritten

quantileClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "quantileClass",
    inherit = quantileBase,
    private = list(
        .run = function() {

            two_sided <- as.logical(self$options$group=="Two-sided") 
            
            p <- self$options$p
            

  
            if (two_sided) {
                p <- 1-((1-p)/2)
            }
            
            q = qnorm(as.numeric(p))
            
            print("")
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            self$results$text$setContent( 
                paste0("The critical value is ",round(q,2), " ",
                       ifelse(two_sided,"(two-sided)","(one-sided)"))
                )
            
            # self$results$
        },
        .plot=function(image, ...) {
            p <- as.numeric(self$options$p)
            
            two_sided <- as.logical(self$options$group=="Two-sided") 
            
            if (two_sided) {
                p <- 1-((1-p)/2)
            }
            
           # p <- 0.95
        #    two_sided <- TRUE
            #print(two_sided)
            
            qleft <- qnorm(p,mean=0)
            qright <- qnorm(1-p,mean=0)
            
            vline_right <- geom_vline(xintercept=qleft,lty=2)
            

            
            minmax <- max(3, qright*1.2)
            
            x <- seq(-minmax,+minmax,0.1)
            y <- dnorm(x)
            y_right <- ifelse(x<qleft, 0, y)
            y_left <- ifelse(x>qright,0, y)
            
            lab <- paste0(round((1-p)*100,2),"%")
            hght <- median(y)
            vtext_right <- geom_text(aes(x=qleft*1.2,y=hght,label=lab))
            vtext_left <- geom_text(aes(x=qright*1.2,y=hght,label=lab))
            
            
            if (two_sided) {
                vline_left <-  geom_vline(xintercept=qright,lty=2)
            } else { 
                vline_left <- NULL
                vtext_left <- NULL
            }
            
            ribbon_right <- geom_ribbon(aes(ymin=0,ymax=y_right,x=x),fill="lightblue")
            
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
            
            print(plot)
            TRUE    
        }
        
        )
)
