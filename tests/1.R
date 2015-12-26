test.read.data <- function() {
    df = read.data()
    checkEquals(c(165, 8), dim(df))
    checkEquals(c('Country.Code','GDP','Avg.Temperature',
                  'Poverty','Percent.Natural.Resources','Region',
                  'Landlocked','Natural.Resources'),
                names(df))
    checkEquals('AFG', as.character(df[1,]$Country.Code))
    checkEquals('Asia', as.character(df[1,]$Region))
    checkEqualsNumeric(12.92, df[1,]$Avg.Temperature, tolerance=0.00005)
    checkEqualsNumeric(365.3909, df[1,]$GDP, tolerance=0.00005)
    checkEqualsNumeric(36.05000, df[1,]$Poverty, tolerance=0.00005)
    checkEqualsNumeric(3.201104, df[1,]$Percent.Natural.Resources, tolerance=0.00005)
    checkEqualsNumeric(1169.654, df[1,]$Natural.Resources, tolerance=0.00005)
    checkTrue(df[1,]$Landlocked, 'First data entry should be Landlocked')
}

test.get.formula = function() {
    checkEquals('log(GDP) ~ Avg.Temperature',
                get.formula(c('Avg.Temperature')))
    checkEquals('log(GDP) ~ log(Natural.Resources)',
                get.formula(c('log(Natural.Resources)')))
    checkEquals('log(GDP) ~ Landlocked',
                get.formula(c('Landlocked')))
    checkEquals('log(GDP) ~ Avg.Temperature+log(Natural.Resources)',
                get.formula(c('Avg.Temperature','log(Natural.Resources)')))
    checkEquals('log(GDP) ~ Avg.Temperature+Landlocked',
                get.formula(c('Avg.Temperature','Landlocked')))
    checkEquals('log(GDP) ~ log(Natural.Resources)+Landlocked',
                get.formula(c('log(Natural.Resources)', 'Landlocked')))
    checkEquals('log(GDP) ~ Avg.Temperature+log(Natural.Resources)+Landlocked',
                get.formula(c('Avg.Temperature', 'log(Natural.Resources)', 'Landlocked')))
    checkException(get.formula('junk'), 'bad predictor')
}

test.get.model = function() {
    df = read.data()
    fit = get.model(get.formula(c('Avg.Temperature','log(Natural.Resources)')), df)
    checkEqualsNumeric(8.1492288, coef(fit$finalModel)[1], tolerance = 0.00005)
}

test.percent.explained = function() {
    df = read.data()
    fit = get.model(get.formula(c('Avg.Temperature','log(Natural.Resources)')), df)
    pe = percent.explained(fit$finalModel)
    checkEqualsNumeric(22.07440, pe['Avg.Temperature', 'PctExp'], tolerance = 0.00005)
}

test.filter.data.by.region = function() {
    df = read.data()
    ss = filter.data.by.region(df, c('Africa'))
    checkEquals(c(49, 8), dim(ss))
    ss = filter.data.by.region(df, c('Africa', 'Asia'))
    checkEquals(c(90, 8), dim(ss))
    ss = filter.data.by.region(df, c('Africa', 'junk'))
    checkEquals(c(49, 8), dim(ss))
    ss = filter.data.by.region(df, c('junk'))
    checkEquals(c(0, 8), dim(ss))
}