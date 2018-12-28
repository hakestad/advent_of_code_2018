input <- readLines("day3_puzzle_input.txt")

# Cutting out first character (#) of strings of the puzzle input
formatted <- sapply(input, function(str) {
    substring(str, 2)
})

# Gold star 1

claims <- lapply(formatted, function(str) {
    str <- strsplit(str, " ")[[1]]
    id <- str[1]
    lt <- strsplit(str[3], ",")[[1]]
    left <- lt[1]
    top <- lt[2]
    top <- substr(top, 1, nchar(top) - 1)
    wh <- strsplit(str[4], "x")[[1]]
    width <- wh[1]
    height <- wh[2]
    
    c(as.numeric(id), as.numeric(left), as.numeric(top), as.numeric(width), as.numeric(height))
  
})

fabric  <- matrix(nrow = 1000, ncol = 1000)
names(fabric) <- seq(1:1000)
counter <- 0

for (claim in claims) {
    # id left top width height
    x_coords <- seq(claim[[2]] + 1, claim[[2]] + claim[[4]])
    y_coords <- seq((claim[[3]] + 1), claim[[3]] + claim[[5]])
   
    for (x in x_coords) {
        for (y in y_coords) {
            if (is.na(fabric[x, y])) {
                fabric[x, y] <- 1
            } else {
                fabric[x, y] <- fabric[x, y] + 1
            }
        }
    }
    counter <- counter + 1
    print(counter)
}

sum(fabric > 1, na.rm = TRUE) # 107043

# Just for fun:

require(reshape2)
require(ggplot2)

# Use melt function in reshape2 to restructure the data into a three-column list
# where first two columns are x and y axes, and third column is the value of [x, y]
fab <- melt(fabric)
names(fab) <- c("x", "y", "overlap")
# Find which rows doesn't have any values in column three of molten (converted by melt()) fabric
# and net those values to 0 instead of NA, to better represent it in a plot
nas <- which(is.na(fab[, 3]))
fab[nas, 3] <- 0

# Make a factor for column 3 (overlap), if the value is over 1, we know that claims
# to the fabric are overlapping at these points
fab$overlap <- factor(fab$overlap > 1)
levels(fab$overlap)=c("No overlap","Overlap")
qplot(x, y, fill = overlap, data = fab, geom='tile')


# Gold star 2

