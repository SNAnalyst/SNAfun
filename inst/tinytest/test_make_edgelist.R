namedf <- data.frame(respondent = c('A', 'B', 'C'),
                     alter1 = c('D', 'C', 'A'),
                     alter2 = c('B', 'A', 'A') )
attributedf <- data.frame(attribute1 = 1:3, attribute2 = 4:6, attribute3 = 2:4)

edgelist <-  snafun::make_edgelist(names =  namedf, attribute = attributedf)
