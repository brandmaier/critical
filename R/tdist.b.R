
# This file is a generated template, your changes will not be overwritten

tdistClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "tdistClass",
    inherit = tdistBase,
    private = list(
        .run = function() {

            
            two_sided <- as.logical(self$options$group=="Two-sided") 
            
            p <- self$options$p
            df <- as.numeric(self$options$df)

            
            if (two_sided) {
                p <- 1-((1-p)/2)
            }
            
            q = qt(as.numeric(p),df = df)
            
            print("")
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            self$results$text$setContent( 
                paste0("The critical value is ",round(q,2), " ",
                       ifelse(two_sided,"(two-sided)","(one-sided)"))
            )
            
        },
        .plot=function(image, ...) {
            p <- as.numeric(self$options$p)
            df <- as.numeric(self$options$df)
            
            two_sided <- as.logical(self$options$group=="Two-sided") 
            
            if (two_sided) {
                p <- 1-((1-p)/2)
            }
            
           
            
            qleft <- qt(p, df = df)
            qright <- qt(1-p, df = df)
            
            maxx <- max(qleft*1.2,
                        qt(0.99,df=df))
            
            plot <- densityPlot(p, two_sided=two_sided, 
                                qleft, qright,density=
                                    function(x){dt(x,df)},
                                xminmax=c(-maxx,maxx))        
            
            print(plot)
            TRUE    
        }
        )
)
