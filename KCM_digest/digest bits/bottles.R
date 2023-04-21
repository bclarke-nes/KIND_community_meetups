bottles <- 10

while(bottles > 0) {
  if(bottles > 2) {
  cat(paste(bottles, "green bottles, hanging on the wall.", bottles, "green bottles, hanging on the wall. And if 1 green bottle should accidentally fall, there'll be", bottles - 1, "green bottles hanging on the wall.  \n"))

    bottles <- bottles - 1
    
  } else if (bottles==2) {
    cat(paste(bottles, "green bottles, hanging on the wall.", bottles, "green bottles, hanging on the wall. And if 1 green bottle should accidentally fall, there'll be", bottles - 1, "green bottle hanging on the wall.  \n"))
    
    bottles <- bottles - 1
    
  } else {
    cat(paste(bottles, "green bottle, hanging on the wall.", bottles, "green bottle, hanging on the wall. And if 1 green bottle should accidentally fall, there'll be no green bottles hanging on the wall."))
    bottles <- bottles - 1
  }

}

library(glue)
bottles <- function(n){
  
  for(i in n:1){
    plu <- function(n) ifelse(n>=2,"","s")
    ifelse(i==2, noz <- "no green bottles", noz <- glue("{i-1} green bottle{plu(i-1)}")
    for(j in 1:2) {
    cat(glue("{i} green bottle{plu(i)}, hanging on the wall. "))
             }
    cat(glue("And if one green bottle should accidentally fall, there'll be {noz} hanging on the wall.  \n  \n"))
    
    }  
}

bottles <- function(n) {
  plu_bot <- function(m) ifelse(m == 1, "bottle", "bottles")
  less_n <- function(n) ifelse(n == 1, "no", n - 1)
  cat(glue(
      "{n} green {plu_bot(n)}, hanging on the wall. {n} green {plu_bot(n)}, hanging on the wall. And if one green bottle should accidentally fall, there'll be {less_n(n)} green {plu_bot(less_n(n))} hanging on the wall.  \n  \n"))
  }

bottles(50000:1)


