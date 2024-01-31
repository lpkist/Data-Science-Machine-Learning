library(dslabs)
data(heights)
heights
class(heights)
str(heights)
nrow(heights)
heights[777,]
heights$sex[777]
max(heights$height)
which(heights$height == min(heights$height))
mean(heights$height)
median(heights$height)
sum(heights$sex == "Male")/nrow(heights)
sum(heights$height > 78)
sum(heights$height > 78 & heights$sex == "Female")
