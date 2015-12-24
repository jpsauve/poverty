library(gdata)
library(caret)
library(rjson)

# Business logic for the application that investigates how poverty
# can be explained by factors outside human control

# read.data
# return
#   data.frame with one row per country and with the following columns:
#       Country.Code: ISO 3166-1 alpha-3 (cca3) country code
#       Region: Country continent
#       Annual_temp: Average annual temperature
#       mean.gdp: mean GDP over the years provided in the data
#       mean.poverty: mean poverty rate over the years provided in the data
#       mean.nr: mean natural resource rent as a percentage over the years provided in the data
#       mean.nr.abs: mean.nr multiplied by mean.gdp
#       landlocked: whether the country is land-locked
read.data = function() {
    # economic data
    df.econ = read.csv('Popular indicators/Popular indicators_Data.csv')
    # average over all years
    ss = subset(x = df.econ,
                select = grep('^X', colnames(df.econ)))
    df.econ$mean = rowMeans(ss, na.rm = TRUE)
    gdp.per.capita = subset(x = df.econ, 
                            subset = (Series.Name == 'GDP per capita (current US$)'))
    names(gdp.per.capita)[names(gdp.per.capita) == 'mean'] <- 'mean.gdp'
    poverty = subset(x = df.econ,
                     subset = (Series.Name == 'Poverty headcount ratio at national poverty lines (% of population)'))
    names(poverty)[names(poverty) == 'mean'] <- 'mean.poverty'
    
    # temperature data
    temp.df = read.xls(xls = 'cckp_historical_data_0.xls', sheet = 4, na.strings = 'NA')
    
    # natural resources
    nr.df = read.csv('ny.gdp.totl.rt.zs_Indicator_en_csv_v2/ny.gdp.totl.rt.zs_Indicator_en_csv_v2.csv', skip=4)
    # average over all years
    ss = subset(x = nr.df,
                select = grep('^X', colnames(nr.df)))
    nr.df$mean.nr = rowMeans(ss, na.rm = TRUE)

    # read regional data
    json_file <- "mledoze-countries-6757eef/countries.json"
    json_data <- fromJSON(paste(readLines(json_file), collapse=""))
    region.df = data.frame(Country.Code = 
                               sapply(json_data, function(row) row$cca3),
                           Region=sapply(json_data, function(row) row$region),
                           landlocked=sapply(json_data, function(row) row$landlocked))
                           
    # combine into one frame
    comb.df = merge(gdp.per.capita, temp.df, 
                    by.x='Country.Code', by.y='ISO_3DIGIT', 
                    all=FALSE, sort=TRUE)
    comb.df = merge(comb.df, poverty, 
                    by.x='Country.Code', by.y='Country.Code', 
                    all=FALSE, sort=TRUE)
    comb.df = merge(comb.df, nr.df, 
                    by.x='Country.Code', by.y='Country.Code', 
                    all=FALSE, sort=TRUE)
    comb.df = merge(comb.df, region.df, 
                    by.x='Country.Code', by.y='Country.Code', 
                    all=FALSE, sort=TRUE)
    
    # generate final data frame
    df = subset(comb.df, select=c('Country.Code', 'Region', 'Annual_temp', 
                                  'mean.gdp', 'mean.poverty', 'mean.nr',
                                  'landlocked'))
    df$mean.nr.abs = df$mean.nr * df$mean.gdp
    # if there's no GDP, the country is not usable
    df = df[complete.cases(df$mean.gdp),]
    return (df)
}

# predictor.info
# data.frame containing information for each 
predictor.info = data.frame(
    label = c('Average Annual Temperature', 
              'log Natural Resources', 
              'Land-locked'),
    vertical.label = c('Average\nAnnual\nTemperature', 
                       'log\nNatural\nResources', 
                       'Land-locked'),
    row.names = c('Annual_temp', 'log(mean.nr.abs)', 'landlocked')
)

# get.predictor.label
# param: pred, a predictor, one of the row.names of predictor.info
# return: a label, possibly wide
get.predictor.label = function(pred) {
    return (predictor.info[pred,]$label)
}

# get.predictor.vertical.label
# param: pred, a predictor, one of the row.names of predictor.info
# return: a label, made to be narrower than the common label
get.predictor.vertical.label = function(pred) {
    return (predictor.info[pred,]$vertical.label)
}

# get.possible.predictors
# return: all possible predictors for this application
get.possible.predictors = function() {
    return (row.names(predictor.info))
}

# what.is.not.in
# params:
#   vec: a vector
#   possibilities: all possibilities we want to check vec against
# return: all items of vec that are not in possibilities
what.is.not.in = function(vec, possibilities) {
    return (vec[sapply(vec, function(p) !(p %in% possibilities))])
}

# get.formula
# param: predictors to include in the model
# return: a formula to predict log(mean.gdp) with the predictors
get.formula = function(predictors){
    if(length(what.is.not.in(predictors, get.possible.predictors())) > 0) {
        stop(simpleError(paste('unknown predictors', predictors)))
    }
    formula = paste('log(mean.gdp) ~', paste(predictors,collapse = '+'))
    return (formula)
}

# get.model
# params:
#   formula.str: a formula as string
#   df: the data
# return: a model, with the data preprocessed by standardizing
get.model = function(formula.str, df, seed=1) {
    if(!is.na(seed)) {
        set.seed(seed)
    }
    fit = train(as.formula(formula.str), data=df, method='lm', 
                preProcess = c('center', 'scale'))
    return (fit)
}

# percent.explained
# param: a model
# partition the R^2 among the various predictors
# (careful: this may depend on the order of predictors.
# It only really works if the predictors are orthogonal.
# In this application, they appear to be)
# return: a data.frame with the anova results and an additional
#   column (PctExp) that provides the percentage of variation explained
#   by the predictors and residuals
percent.explained = function(model) {
    an <- anova(model)
    anss <- an$"Sum Sq"
    return (cbind(an,PctExp=anss/sum(anss)*100))
}

# get.region.names
# return: all possible region names
get.region.names = function() {
    c("Africa", "Americas", "Asia", "Europe", "Oceania")
}

# filter.data.by.region
# params: 
#   df: a data.frame
#   regions: region names
# return: the rows of df for the regions
filter.data.by.region = function(df, regions) {
    return (subset(df, subset = Region %in% regions))
}
