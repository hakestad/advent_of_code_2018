# From https://stackoverflow.com/questions/7723549/getting-and-removing-the-first-character-of-a-string

PopStringFactory <- setRefClass(
    "PopString",
    fields = list(
        x = "character"  
    ),
    methods = list(
        initialize = function(x)
        {
            x <<- x
        },
        pop = function(n = 1)
        {
            if(nchar(x) == 0)
            {
                warning("Nothing to pop.")
                return("")
            }
            first <- substring(x, 1, n)
            x <<- substring(x, n + 1)
            first
        }
    )
)

x <- PopStringFactory$new("hello stackoverflow")
x
## Reference class object of class "PopString"
## Field "x":
## [1] "hello stackoverflow"
replicate(nchar(x$x), x$pop())
## [1] "h" "e" "l" "l" "o" " " "s" "t" "a" "c" "k" "o" "v" "e" "r" "f" "l" "o" "w"