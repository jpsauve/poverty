# from https://gist.github.com/dsparks/818976
# modified to remove intercept

# CoefficientPlot
# params:
#   models: a list of models
#   alpha: the desired significance level for the confidence intervals
#   modelnames: the names of the models displayed in the plot
#   keep.intercept: whether or not the intercept should be included in the plot
# return: a corfficient plot with confidence intervals
CoefficientPlot <- function(models, alpha = 0.05, modelnames = "", keep.intercept = FALSE){
    # models must be a list()
    Multiplier <- qnorm(1 - alpha / 2)
    CoefficientTables <- lapply(models, function(x){
        if(keep.intercept & '(Intercept)' %in% rownames(summary(x)$coef)) {
            summary(x)$coef
        } else {
            summary(x)$coef[-1,,drop=FALSE]
        }
    })
    TableRows <- unlist(lapply(CoefficientTables, nrow))
    
    if(modelnames[1] == ""){
        ModelNameLabels <- rep(paste("Model", 1:length(TableRows)), TableRows)
    } else {
        ModelNameLabels <- rep(modelnames, TableRows)
    }
    
    MatrixofModels <- cbind(do.call(rbind, CoefficientTables), ModelNameLabels)
    MatrixofModels <- data.frame(cbind(rownames(MatrixofModels), MatrixofModels))
    colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName")
    MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
    MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})
    
    OutputPlot <- qplot(IV, Estimate, ymin = Estimate - Multiplier * StandardError,
                        ymax = Estimate + Multiplier * StandardError, data = MatrixofModels, geom = "pointrange",
                        ylab = NULL, xlab = NULL)
    OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
    OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + theme_bw()
    return(OutputPlot)
}
# a basic test
# df = data.frame(x=c(1,2,6), y=c(3,4,5))
# fit = lm(y~x, data = df)
# CoefficientPlot(list(fit), keep.intercept = FALSE)
