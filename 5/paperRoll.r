library(lpSolve)

c = rep(1,12)

A = matrix(
    c(2,1,1,1,0,0,0,0,0,0,0,0,
      0,1,0,0,2,1,1,1,0,0,0,0,
      0,0,1,0,0,2,1,0,3,2,1,0,
      0,1,1,3,2,2,2,4,0,2,4,7),
    ncol=12,
    byrow=T
)

id=diag(rep(1,12)) # identity matrix of dimension 12
A = rbind(A,id)
dir = rep(">=",16)
b = c(96,610,395,211, rep(0,12))

lp("min", c, A, dir, b)
