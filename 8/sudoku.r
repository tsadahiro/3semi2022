
idx = function(i,j,d){
  9*9*i + 9*j + d
}

A=c()
# cell constraints
for (i in 0:8){
  for (j in 0:8){
    row = rep(0,9^3)
    for (d in 1:9){
      row[idx(i,j,d)]=1
    }
    A = rbind(A,row)
  }
}

# row constraints
for (i in 0:8){
  for (d in 1:9){
    row = rep(0,9^3)
    for (j in 0:8){
      row[idx(i,j,d)]=1
    }
    A = rbind(A,row)
  }
}
# column constraints

for (j in 0:8){
  for (d in 1:9){
    row = rep(0,9^3)
    for (i in 0:8){
      row[idx(i,j,d)]=1
    }
    A = rbind(A,row)
  }
}

# block constraints
for (bi in 0:2){
    for (bj in 0:2){
        for (d in 1:9){
            row = rep(0,9^3)
            for (i in 0:2){
                for (j in 0:2){
                    row[idx(3*bi+i,3*bj+j,d)]=1
                }
            }
            A=rbind(A,row)
        }
    }
}

A = rbind(A, diag(rep(1,9^3)))
A = rbind(A, diag(rep(1,9^3)))

setCoef = function(i,j,d){
    row = rep(0,9^3)
    row[idx(0,2,5)]=1
    A <<- rbind(A, row)
}

setCoef(0,2,5)
setCoef(0,3,9)
setCoef(0,4,6)
setCoef(1,8,9)


image(A)

library(lpSolve)
c = rep(1,9^3)
dir = c(rep("=",9^2*4), rep(">=",9^3), rep("<=",9^3), rep("==",4))
b = c(rep(1,9^2*4), rep(0,9^3), rep(1,9^3), rep(1,4))
sol = lp("min", c, A, dir,b,int.vec=1:9^3)

digits=rep(1:9,9^2)
matrix(digits[round(sol$solution)==1],nrow=9)
