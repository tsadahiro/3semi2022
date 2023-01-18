
idx = function(i,j,d){
  4*4*i + 4*j + d
}

A=c()
# cell constraints
for (i in 0:3){
  for (j in 0:3){
    row = rep(0,4^3)
    for (d in 1:4){
      row[idx(i,j,d)]=1
    }
    A = rbind(A,row)
  }
}

# row constraints
for (i in 0:3){
  for (d in 1:4){
    row = rep(0,4^3)
    for (j in 0:3){
      row[idx(i,j,d)]=1
    }
    A = rbind(A,row)
  }
}
# column constraints

for (j in 0:3){
  for (d in 1:4){
    row = rep(0,4^3)
    for (i in 0:3){
      row[idx(i,j,d)]=1
    }
    A = rbind(A,row)
  }
}

# block constraints
#for (bi in 0:1){
#    for (bj in 0:1){
#        for (d in 1:4){
#            row = rep(0,4^3)
#            for (i in 0:1){
#                for (j in 0:1){
#                    row[idx(2*bi+i,2*bj+j,d)]=1
#                }
#            }
#            A=rbind(A,row)
#        }
#    }
#}

A = rbind(A, diag(rep(1,4^3)))
A = rbind(A, diag(rep(1,4^3)))

image(A)

library(lpSolve)
c = rep(0,4^3)
dir = c(rep("=",4^2*3), rep(">=",4^3), rep("<=",4^3))
b = c(rep(1,4^2*3), rep(0,4^3), rep(1,4^3))
sol = lp("max", c, A, dir,b)

digits=rep(1:4,16)
matrix(digits[round(sol$solution)==1],nrow=4)
