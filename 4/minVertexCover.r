library(igraph)
library(lpSolve)

#G = make_lattice(c(3,3,3))
G = erdos.renyi.game(40,0.1)
layout = layout.kamada.kawai(G,dim=2)
plot(G, layout=layout)
#rglplot(G,layout=layout.kamada.kawai(G,dim=3))


n=length(V(G))
m=length(E(G))
c = rep(1, n)
A = t(apply(ends(G,E(G)), 1, function(x){v=rep(0, n);v[x]=1;v}))
dir = rep(">=",m)
b = rep(1,m)

sol=lp(direction="min",
       objective.in=c,
       const.mat=A,
       const.dir=dir,
       const.rhs=b,
       int.vec=1:n)

pal=c("pink","cyan")
#rglplot(G,
#        layout=layout.kamada.kawai(G,dim=3),
#        vertex.color=pal[round(sol$solution)+1])
plot(G,
     layout=layout,
     vertex.color=pal[round(sol$solution)+1])
