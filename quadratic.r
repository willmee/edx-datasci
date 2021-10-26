quadratic <- function (a, b, c) {
  sqrt_component = sqrt(b*b - 4 * a * c)
  sol1 = (-b + sqrt_component) / (2 * a)
  sol2 = (-b - sqrt_component) / (2 * a)
  solutions <- list("one" = sol1, "two" = sol2)
  return(solutions)
}

sols <- quadratic(2, -1, -4)

print(paste("first solution: ", sols$one))
print(paste("second solution: ", sols$two))

