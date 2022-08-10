namedf <- data.frame(respondent = c('A', 'B', 'C'),
                     alter1 = c('D', 'C', 'A'),
                     alter2 = c('B', 'A', 'A') )
attributedf <- data.frame(attribute1 = 1:3, attribute2 = 4:6)

edgelist <-  snafun::make_edgelist(names =  namedf, attribute = attributedf)


expect_equal(class(edgelist), "data.frame")

expect_equal(edgelist[,1], c("A", "B", "C", "A", "B", "C"))

expect_equal(edgelist[,2], c("D", "C", "A", "B", "A", "A"))

expect_equal(edgelist[,3], c(1, 2, 3, 4, 5, 6))

expect_equal(ncol(edgelist), 3)

edgelist1 <-  snafun::make_edgelist(names =  namedf)

expect_equal(class(edgelist1), "data.frame")

expect_equal(edgelist1[,1], c("A", "B", "C", "A", "B", "C"))

expect_equal(edgelist1[,2], c("D", "C", "A", "B", "A", "A"))
