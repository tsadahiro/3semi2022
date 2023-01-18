
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
for (bi in 0:1){
    for (bj in 0:1){
        for (d in 1:4){
            row = rep(0,4^3)
            for (i in 0:1){
                for (j in 0:1){
                    row[idx(2*bi+i,2*bj+j,d)]=1
                }
            }
            A=rbind(A,row)
        }
    }
}

A = rbind(A, diag(rep(1,4^3)))
A = rbind(A, diag(rep(1,4^3)))

setCoef = function(i,j,d){
    row = rep(0,9^3)
    row[idx(i,j,d)]=1
    A <<- rbind(A, row)
}

setCoef(0,0,1)
setCoef(1,3,2)
setCoef(2,2,3)
setCoef(3,3,4)

image(A)

library(lpSolve)
c = rep(0,4^3)
dir = c(rep("=",4^2*4), rep(">=",4^3), rep("<=",4^3), rep("=",4))
b = c(rep(1,4^2*4), rep(0,4^3), rep(1,4^3), rep(1,4))
sol = lp("max", c, A, dir,b)

digits=rep(1:4,16)
matrix(digits[round(sol$solution)==1],nrow=4)
