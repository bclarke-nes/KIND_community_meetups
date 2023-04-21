library(checkmate)
library(testthat)


#' choose_council
#'  simple function to return a chosen council area for NHS Highland
#'
#' @param council character of length one indicating chosen council area
#'
#' @return character

choose_council <- function(council = c("Highland", "Argyll and Bute")){

  council <-  match.arg(council, several.ok = TRUE)
  out <- paste("chosen council is", council)
  out
}

choose_council("Highland")
choose_council("Argyll and Bute")
choose_council(c("Highland", "Argyll and Bute"))

choose_council() # match.arg uses default arguments

choose_council("A") # partial matching - can be risky

choose_council("Argyll & Bute")

choose_council("Grampian")




## stopifnot()


greeter <- function(x){
  stopifnot(is.character(x))
  paste("Hello", x)
}

greeter("Bob")
greeter(1)



# add informational message
greeter <- function(x){
  stopifnot("x must be character" = is.character(x))
  paste("Hello", x)
}

greeter(2)
greeter("Vic")




#https://github.com/Public-Health-Scotland/phsmethods/blob/master/R/chi_check.R

#' chi_check("0101011237")
#' chi_check(c("0101201234", "3201201234"))
#'



# Does it contain no non-numeric characters?
# Is it ten digits in length?
# Do the first six digits denote a valid date?
# Is the checksum digit correct?


# possible results

  # `Valid CHI`
  # `Invalid character(s) present`
  # `Too many characters`
  #  Too few characters`
  # `Invalid date`
  # `Invalid checksum`
  # 'Missing (NA)`
  # `Missing (Blank)`


x <- "0101011237"
x2 <- "010101123A"

x3 <- c(x, x2, NA)
x4 <- c(x, NA)


checkClass(x, "character")
check_character(x, n.chars = 10, pattern = "\\d{10}") # 10 chars, numeric only
check_character(x2, n.chars = 10, pattern = "[^A-Z]{10}") #fails, because of the 'A'

# final version
check_character(x,
                min.len = 1,
                n.chars = 10,
                any.missing = FALSE,
                pattern = "\\d{10}")


vals <- c(x, x2, x3, x4)
cat(vals)
purrr::map_chr(vals,
               check_character,
               min.len = 1,
               n.chars = 10,
               any.missing = FALSE,
               pattern = "\\d{10}")

# are first 6 elements a Date?
date_val <- substr(x,1,6)
checkDate(as.Date(strptime(date_val,"%d%m%y", "UTC")),
          lower = "1900-01-01",
          upper =  Sys.Date(),
          any.missing = FALSE,
          min.len = 1L)




qassert(x,"S+[10,11)") # character, vector length 1, lower bound 10 and less than 11
qassert(x,"S+[10,10]") # also works, between 10 and 10 (inclusive)
# note difference in closing brackets
# character denoted by `s`
# no missing values denoted by UPPER CASE
# exact length of string 10 denoted by [10]



assert(check_character(x,
                       min.len = 1,
                       n.chars = 10,
                       any.missing = FALSE,
                       pattern = "\\d{10}"),
       checkDate(as.Date(strptime(substr(x,1,6),"%d%m%y", "UTC")),
                 lower = "1900-01-01",
                 upper =  Sys.Date(),
                 any.missing = FALSE,
                 min.len = 1L),
       combine = "and")


assert(check_character(x2,
                       min.len = 1,
                       n.chars = 10,
                       any.missing = FALSE,
                       pattern = "\\d{10}"),
       checkDate(as.Date(strptime(substr(x2,1,6),"%d%m%y", "UTC")),
                 lower = "1900-01-01",
                 upper =  Sys.Date(),
                 any.missing = FALSE,
                 min.len = 1L),
       combine = "and")


main_check <- function(x){
  assert(check_character(x,
                         min.len = 1,
                         n.chars = 10,
                         any.missing = FALSE,
                         pattern = "\\d{10}"),
         checkDate(as.Date(strptime(substr(x,1,6),"%d%m%y", "UTC")),
                   lower = "1900-01-01",
                   upper =  Sys.Date(),
                   any.missing = FALSE,
                   min.len = 1L),
         combine = "and")


}


purrr::map_chr(vals, main_check)
