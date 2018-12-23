input <- read.table("day2_puzzle_input.txt")
input <- unlist(input)

# Gold star 1

counter2 <- 0
counter3 <- 0
for (str in input) {
    counter <- data.frame(row.names = "count")
    str <- strsplit(str, "")[[1]]

    for (char in str) {
       if (!is.null(counter[[char]])) {
           counter[[char]] <- counter[[char]] + 1
        } else {
           counter[[char]] <- 1
         }
    }
    
    found_two <-  FALSE
    found_three <-  FALSE

    for (count in counter) {
        
        if (count == 2) {
            found_two <- TRUE
        } else if (count == 3) {
            found_three <- TRUE
        }
        
        # Break after both types of valid counts are found, since there can be just two in one sentence
        if (found_three == TRUE && found_two == TRUE) {
            break
        }
    }
    
    if (found_two == TRUE) {
        counter2 <- counter2 + 1
    }
    if (found_three == TRUE) {
        counter3 <- counter3 + 1
    }
}


# Gold star 2

found <- FALSE
for (str in input) {
    for(str2 in input) {
        if (str == str2) {
            # do nothing, it's the same string
        } else {
            diff <- 0
            diff_idx <- 0
            s1 <- strsplit(str, "")[[1]]
            s2 <- strsplit(str2, "")[[1]]
            
            for (idx in 1:length(s1)) {
                if (s1[idx] == s2[idx]) {
                    # the two strings are equal at this position
                } else {
                    diff <- diff + 1
                    diff_idx <- idx
                }
                
                if (diff > 1) {
                    # The two strings differ on more than one index
                    break
                }
            }
            
            if (diff == 1) {
                found <- TRUE
                print("Answer:")
                first = substr(str, 1, diff_idx - 1)
                second = substr(str, diff_idx + 1, nchar(str))
                answer <- paste(first, second, sep = "")
                print(answer)
                # No need to search any longer, we have found a match
                break
            }
        }
        
    }
    if (found == TRUE) {
        # Abort further search, we have a match
        break
    }
}

