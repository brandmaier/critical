
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
    
            plot <- densityPlot(p, two_sided, qleft, qright)        
    
            print(plot)
            TRUE    
        }
        
        )
)
