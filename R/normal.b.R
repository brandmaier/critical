
# This file is a generated template, your changes will not be overwritten

normalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "normalClass",
    inherit = normalBase,
    private = list(
      .run = function() {
        
        two_sided <- as.logical(self$options$group=="Two-sided") 
        
        p <- self$options$p
        
        mean <- as.numeric(self$options$mean)        
        sd <- as.numeric(self$options$sd)
        
        if (two_sided) {
          p <- 1-((1-p)/2)
        }
        
        q = qnorm(as.numeric(p),mean = mean, sd = sd)
        
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
        
        mean <- as.numeric(self$options$mean)        
        sd <- as.numeric(self$options$sd)
        
        two_sided <- as.logical(self$options$group=="Two-sided") 
        
        if (two_sided) {
          p <- 1-((1-p)/2)
        }
        
        # p <- 0.95
        #    two_sided <- TRUE
        #print(two_sided)
        
        qleft <- qnorm(p,mean=mean, sd = sd)
        qright <- qnorm(1-p,mean= mean, sd = sd)
        
        maxx <- max( qnorm(p, mean,sd)*1.4,
                    qnorm(0.99,mean,sd))
        minx <- min(
          qnorm(1-p, mean,sd)* (1/1.4),
          qnorm(0.01,mean,sd)
        )
        
        plot <- densityPlot(p, two_sided, qleft, qright,xminmax=c(minx,maxx),
                            density=function(x){dnorm(x,mean=mean,sd=sd)}
                            )        
        
        print(plot)
        TRUE    
      }
    )
)
