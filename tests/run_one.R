library('RUnit')

source('business-logic.R')

test.result <- runTestFile(absFileName = 'tests/1.R', 
                           testFuncRegexp = 'test.percent.explained')
    
printTextProtocol(test.result)

