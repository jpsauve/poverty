library('RUnit')

source('business-logic.R')

test.suite <- defineTestSuite("all tests",
                              dirs = file.path("tests"),
                              testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
