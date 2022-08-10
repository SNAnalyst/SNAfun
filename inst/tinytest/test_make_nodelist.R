namedf <- data.frame(respondent = c('A', 'B', 'C'),
                     alter1 = c('D', 'C', 'A'),
                     alter2 = c('B', 'A', 'A') )
attributeV <- c(1:3)

nodelist <-  snafun::make_nodelist(names =  namedf, attribute = attributeV)



expect_equal(class(nodelist), "data.frame")

expect_equal(nodelist[,1], c("A", "B", "C", "D"))

expect_equal(nodelist[,2], c(1, 2, 3, 0))


expect_equal(ncol(nodelist), 2)

nodelist1 <-  snafun::make_nodelist(names =  namedf)

expect_equal(class(nodelist1), "character")

expect_equal(nodelist1, c("A", "B", "C", "D"))


