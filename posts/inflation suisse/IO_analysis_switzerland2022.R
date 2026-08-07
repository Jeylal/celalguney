library(tidyverse)

rm(list = ls())
CHE2022ttl <- read_csv("CHE2022ttl.csv") |> 
  column_to_rownames(var = "...1")

# extract inter-industry transaction flow matrix
Z = CHE2022ttl[1:50, 1:50] |> 
  as.matrix()

# extract total output of each sector
x = CHE2022ttl[1:50, "TOTAL"] |> 
  as.vector()

x_hat_inverse <- diag(1/x, ncol = 50) # x^hat^-1

A = Z%*%x_hat_inverse

A[, 4] <- 0

# computation check: Ax + f should sum to x, with f the sum of final demand external to Z 

f = CHE2022ttl[, 51:58] |> 
  as.matrix() |> 
  apply(1, sum) |> 
  as.matrix()

f = f[1:50]

x_check <- A%*%x + f # does not really sum to x...


# Compute leontief matrix (I-A)^-1

L = solve(diag(50)- A)
L[, 4] = 0
L[,50] = 0

sectors = rownames(CHE2022ttl)

rownames(L) <- sectors[1:50]

# forward linkages: rowsums of the L

forward_linkages = apply(L,1, sum) |> as.matrix()
backward_linkages = apply(L,2, sum) |> as.matrix()

linkages <- cbind(forward_linkages, backward_linkages) |> 
  as_tibble() |> 
  rename(
    "forward" = V1,
    "backward" = V2
  ) |> 
  mutate(
    sector = sectors[1:50]
  )


linkages |> 
  ggplot(aes(x = forward, y = backward, label = sector))+
  geom_text()






