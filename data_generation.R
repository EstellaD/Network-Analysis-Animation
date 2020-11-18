# colnames(dat) <- c("ID", "LineNames", "StartDate", "EndDate")
#
dat <- data.frame(
  ID = as.factor(c(1,2,3,4,5,6)),
  LineNames = c("A,B,C", "B", "C", "A,D", "B,D", "B,C"),
  StartDate = as.Date(c("2017-08-21", "2017-07-24", "2018-05-08", "2018-03-13", "2016-01-05", "2017-12-09"), format = "%Y-%m-%d"),
  EndDate = as.Date(c("2017-11-08", "2018-03-08", "2018-06-17", "2018-06-11", "2017-02-28", "2018-04-17"), format = "%Y-%m-%d")
)

# TODO: create random ID, LinesNames and Date generator
# TODO: use dplyr for long format separation for LineNames.
