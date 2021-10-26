num_bets <- 500
expected <- 6*(5/38) + -1 * (33/38)
std_err <- abs(6 - -1) * sqrt(5*33)/38

num_bets * expected

std_err / sqrt(num_bets)

std_err * sqrt(num_bets)

pnorm(0, num_bets * expected, std_err * sqrt(num_bets))

pnorm(0, -39.47368, 52.91045) 