# Gold star 1

input <- read.table("day1_puzzle_input.txt")
sum(input$V1) # 547

# or

freq <- 0
for (val in input$V1) {
    freq <- freq + val
}
freq

# or

freq <- 0
# <<- upate global variable in local space
sapply(input$V1, function(val) freq <<- freq + val)
freq # 547


# Gold star 2

freq <- 0
duplicates <- c()
frequencies <- c()
while(length(duplicates) == 0) {
    # Every loop, find all frequencies reached in this round of the puzzle input
    new_frequencies <- sapply(input$V1, function(val) freq <<- freq + val)
    # Combine the frequencies with the previous round of reached frequencies
    frequencies <<- c(frequencies, new_frequencies)
    # Look for duplicates
    duplicates <<- frequencies[duplicated(frequencies)]
}
# When loop is exited, it must mean a duplicate (or more) has been found
# Access the value of the first duplicate
duplicates[1] # 76414

# Note to self: I could have made it look for duplicates after each new calculated frequency,
# but adding values to a vector in a loop seems to be discouraged in R, because appending
# objects in a for loop causes the entire object to be copied on every iteration, which makes things slow
# 
# But just for fun, I wanna try that

found <- FALSE
freqs <- c()
curr_freq <- 0
while(!found) {
    for (val in input$V1) {
        curr_freq <<- curr_freq + val
        freqs <<- c(freqs, curr_freq)
        print(length(freqs))
        duplis <- freqs[duplicated(freqs)]
        if (length(duplis) > 0) {
            found = TRUE
            print(duplis[1])
            break
        }
    }
}
 # 76414 ----- but it took like 15 minutes! Haha