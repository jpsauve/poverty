test.read.data <- function() {
    df = read.data()
    checkEquals(c(165, 8), dim(df))
    checkEquals(c('Country.Code','Region','Annual_temp',
                  'mean.gdp','mean.poverty','mean.nr',
                  'landlocked','mean.nr.abs'),
                names(df))
    checkEquals('AFG', as.character(df[1,]$Country.Code))
    checkEquals('Asia', as.character(df[1,]$Region))
    checkEqualsNumeric(12.92, df[1,]$Annual_temp, tolerance=0.00005)
    checkEqualsNumeric(365.3909, df[1,]$mean.gdp, tolerance=0.00005)
    checkEqualsNumeric(36.05000, df[1,]$mean.poverty, tolerance=0.00005)
    checkEqualsNumeric(3.201104, df[1,]$mean.nr, tolerance=0.00005)
    checkEqualsNumeric(1169.654, df[1,]$mean.nr.abs, tolerance=0.00005)
    checkTrue(df[1,]$landlocked, 'First data entry should be landlocked')
}

test.get.formula = function() {
    checkEquals('log(mean.gdp) ~ Annual_temp',
                get.formula(c('Annual_temp')))
    checkEquals('log(mean.gdp) ~ log(mean.nr.abs)',
                get.formula(c('log(mean.nr.abs)')))
    checkEquals('log(mean.gdp) ~ landlocked',
                get.formula(c('landlocked')))
    checkEquals('log(mean.gdp) ~ Annual_temp+log(mean.nr.abs)',
                get.formula(c('Annual_temp','log(mean.nr.abs)')))
    checkEquals('log(mean.gdp) ~ Annual_temp+landlocked',
                get.formula(c('Annual_temp','landlocked')))
    checkEquals('log(mean.gdp) ~ log(mean.nr.abs)+landlocked',
                get.formula(c('log(mean.nr.abs)', 'landlocked')))
    checkEquals('log(mean.gdp) ~ Annual_temp+log(mean.nr.abs)+landlocked',
                get.formula(c('Annual_temp', 'log(mean.nr.abs)', 'landlocked')))
    checkException(get.formula('junk'), 'bad predictor')
}

test.get.model = function() {
    df = read.data()
    fit = get.model(get.formula(c('Annual_temp','log(mean.nr.abs)')), df)
    checkEqualsNumeric(8.1492288, coef(fit$finalModel)[1], tolerance = 0.00005)
}

test.percent.explained = function() {
    df = read.data()
    fit = get.model(get.formula(c('Annual_temp','log(mean.nr.abs)')), df)
    pe = percent.explained(fit$finalModel)
    checkEqualsNumeric(22.07440, pe['Annual_temp', 'PctExp'], tolerance = 0.00005)
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