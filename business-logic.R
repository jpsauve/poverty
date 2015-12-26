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
#       Avg.Temperature: Average annual temperature
#       GDP: mean GDP over the years provided in the data
#       Poverty: mean poverty rate over the years provided in the data
#       Percent.Natural.Resources: mean natural resource rent as a percentage over the years provided in the data
#       Natural.Resources: Percent.Natural.Resources multiplied by GDP
#       Landlocked: whether the country is land-locked
# Data references
#   GDP per capita, Poverty index:
#       http://databank.worldbank.org/data/reports.aspx?Code=NY.GDP.PCAP.CD&id=af3ce82b&report_name=Popular_indicators&populartype=series&ispopular=y#
#   Temperature data:
#       http://data.worldbank.org/data-catalog/cckp_historical_data
#   Natural Resource Rent data:
#       http://data.worldbank.org/indicator/NY.GDP.TOTL.RT.ZS
#   Landlocked status
#       http://mledoze.github.io/countries/

read.data = function() {
    # economic data
    df.econ = read.csv('Popular indicators/Popular indicators_Data.csv')
    # average columns over all years
    ss = subset(x = df.econ, select = grep('^X', colnames(df.econ)))
    df.econ$mean = rowMeans(ss, na.rm = TRUE)
    # get GDP
    gdp.per.capita = subset(x = df.econ, 
                            subset = (Series.Name == 'GDP per capita (current US$)'))
    gdp.per.capita = gdp.per.capita[,c('Country.Code', 'mean')]
    names(gdp.per.capita)= c('Country.Code', 'GDP')
    # get Poverty index
    poverty = subset(x = df.econ,
                     subset = (Series.Name == 'Poverty headcount ratio at national poverty lines (% of population)'))
    poverty = poverty[,c('Country.Code', 'mean')]
    names(poverty)[names(poverty) == 'mean'] <- 'Poverty'
    
    # Temperature data
    temp.df = read.xls(xls = 'cckp_historical_data_0.xls', sheet = 4, na.strings = 'NA')
    temp.df = temp.df[,c('ISO_3DIGIT', 'Annual_temp')]
    names(temp.df) = c('Country.Code', 'Avg.Temperature')
    
    # Natural resources
    nr.df = read.csv('ny.gdp.totl.rt.zs_Indicator_en_csv_v2/ny.gdp.totl.rt.zs_Indicator_en_csv_v2.csv', skip=4)
    # average over all years
    ss = subset(x = nr.df,
                select = grep('^X', colnames(nr.df)))
    nr.df$Percent.Natural.Resources = rowMeans(ss, na.rm = TRUE)
    nr.df = nr.df[,c('Country.Code', 'Percent.Natural.Resources')]

    # read regional data
    json_file <- "mledoze-countries-6757eef/countries.json"
    json_data <- fromJSON(paste(readLines(json_file), collapse=""))
    region.df = data.frame(Country.Code = 
                               sapply(json_data, function(row) row$cca3),
                           Region=sapply(json_data, function(row) row$region),
                           Landlocked=sapply(json_data, function(row) row$landlocked))
                           
    # combine into one frame
    comb.df = merge(gdp.per.capita, temp.df, 
                    by='Country.Code', all=FALSE, sort=TRUE)
    comb.df = merge(comb.df, poverty, 
                    by='Country.Code', all=FALSE, sort=TRUE)
    comb.df = merge(comb.df, nr.df, 
                    by='Country.Code', all=FALSE, sort=TRUE)
    comb.df = merge(comb.df, region.df, 
                    by='Country.Code', all=FALSE, sort=TRUE)
    
    # generate final data frame
    comb.df$Natural.Resources = comb.df$Percent.Natural.Resources * comb.df$GDP
    # if there's no GDP, the country is not usable
    comb.df = comb.df[complete.cases(comb.df$GDP),]
    return (comb.df)
}

# predictor.info
# data.frame containing information for each 
predictor.info = data.frame(
    row.names = c('Avg.Temperature', 'log(Natural.Resources)', 'Landlocked'),
    label = c('Average Annual Temperature', 
              'log Natural Resources', 
              'Land-locked'),
    vertical.label = c('Average\nAnnual\nTemperature', 
                       'log\nNatural\nResources', 
                       'Land-locked')
)

# get.possible.predictors
# return: all possible predictors for this application
get.possible.predictors = function() {
    p = row.names(predictor.info)
    names(p) = predictor.info$label
    return (p)
}

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
    formula = paste('log(GDP) ~', paste(predictors,collapse = '+'))
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
    pe = cbind(an,PctExp=anss/sum(anss)*100)
#    row.names(pe) = c('Annual Temperature', 'Natural Resources', 'Landlocked', 'Residuals')
    return (pe)
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
