
# This file is a generated template, your changes will not be overwritten

fdistClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "fdistClass",
    inherit = fdistBase,
    private = list(
        .run = function() {
            p <- as.numeric(self$options$p)
            df1 <- as.numeric(self$options$df1)
            df2 <- as.numeric(self$options$df2)
            
            q = qf(p, df1, df2)
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            self$results$text$setContent( 
                paste0("The critical value is ",round(q,2), " "
                       )
            )
        },
        .plot=function(image, ...) {
            p <- as.numeric(self$options$p)
            df1 <- as.numeric(self$options$df1)
            df2 <- as.numeric(self$options$df2)
            
           # two_sided <- as.logical(self$options$group=="Two-sided") 
            
            #if (two_sided) {
            #    p <- 1-((1-p)/2)
            #}
            
            # p <- 0.95
            #    two_sided <- TRUE
            #print(two_sided)
            
            qleft <- qf(p,df1 = df1, df2 = df2)
            qright <- qf(1-p,df1 = df1, df2=df2)
            
            maxx <- max(qleft*1.2,
                        qf(0.99,df1=df1,df2=df2))
            
            plot <- densityPlot(p, two_sided=FALSE, 
                                qleft, qright,density=
                                    function(x){df(x,df1,df2)},
                                xminmax=c(0,maxx))        
            
            print(plot)
            TRUE    
        }
        )
)
