### Data from "Paul Revere's Ride"
### by David Hackett Fisher

library(igraph)
data <- as.matrix(read.csv("data/PaulRevereAppD.csv",row.names=1))

person.net <- data %*% t(data)
group.net <- t(data) %*% data

diag(group.net) <- NA
diag(person.net) <- NA


person.g <- graph.adjacency(person.net,mode="undirected",
                            weighted=NULL, diag=FALSE)


group.g <- graph.adjacency(group.net, weighted=TRUE,
                           mode="undirected", diag=FALSE)

la <- layout.fruchterman.reingold(group.g)
e.wt <- get.edge.attribute(group.g, "weight")

pdf(file="figures/group-view.pdf", width=10, height=10)
plot(group.g, layout=la, vertex.size=15,edge.width=e.wt,
     vertex.label=V(group.g)$name)
dev.off()

png(file="figures/group-view.png", width=1000, height=1000, res=150)
plot(group.g, layout=la, vertex.size=15,edge.width=e.wt,
     vertex.label=V(group.g)$name)
dev.off()


pdf(file="figures/revere-network.pdf", width=22, height=17,pointsize=8)
la <- layout.fruchterman.reingold(person.g)
e.wt <- get.edge.attribute(person.g, "weight")
plot(person.g, layout=la, vertex.size=3,edge.width=0.1,
     vertex.label=V(person.g)$name)
dev.off()

png(file="figures/revere-network.png", width=2200, height=1700, res=150)
la <- layout.fruchterman.reingold(person.g)
e.wt <- get.edge.attribute(person.g, "weight")
plot(person.g, layout=la, vertex.size=3,edge.width=0.1,
     vertex.label=V(person.g)$name)
dev.off()



data.t <- t(data)
person2.net <- data %*% t(data)
diag(person2.net) <- NA
person2.g <- graph.adjacency(person2.net, mode="undirected", weighted=TRUE, diag=FALSE)
la <- layout.fruchterman.reingold(person2.g)
e.wt <- get.edge.attribute(person2.g, "weight")

pdf(file="figures/person-weighted-view.pdf", width=20, height=20)
plot(person2.g, layout=la, vertex.size=15,edge.width=e.wt,
     vertex.label=V(person2.g)$name)
dev.off()

png(file="figures/person-weighted-view.png", width=2000, height=2000, res=150)
plot(person2.g, layout=la, vertex.size=15,edge.width=e.wt,
     vertex.label=V(person2.g)$name)
dev.off()



### Centrality

## Betweenness
btwn.person <- betweenness(person.g)
names(btwn.person) <- V(person.g)$name
ind <- order(-btwn.person)
btwn.person[ind][1:10]

## Eigenvector
cent.eig <- evcent(person.g)
names(cent.eig$vector) <- V(person.g)$name

ind <- order(-cent.eig$vector)
cent.eig$vector[ind][1:10]

## Kleinberg authority
cent.klein <- authority.score(person.g)
names(cent.klein$vector) <- V(person.g)$name
ind <- order(-cent.klein$vector)
cent.klein$vector[ind][1:10]

## Bonacich Power
cent.bonpow <- bonpow(person.g, exponent=1)
names(cent.bonpow) <-  V(person.g)$name
ind <- order(cent.bonpow)
cent.bonpow[ind][1:10]

ind <- cent.bonpow < -1.35

col.vec <- rep("")

pdf(file="figures/revere-network-reduced.pdf", width=22, height=17,pointsize=8)
person.g.copy <- person.g
la <- layout.fruchterman.reingold(person.g.copy)
plot(person.g.copy, layout=la, vertex.size=3,
     vertex.label=V(person.g.copy)$name)
dev.off()

png(file="figures/revere-network-reduced.png", width=2200, height=1700, res=140)
person.g.copy <- person.g
la <- layout.fruchterman.reingold(person.g.copy)
plot(person.g.copy, layout=la, vertex.size=3,
     vertex.label=V(person.g.copy)$name)
dev.off()


png(file="figures/revere-test.png", width=2200, height=1700)
com <- spinglass.community(person.g, spins=5)
V(person.g)$color <- com$membership+1
person.g <- set.graph.attribute(person.g, "layout", layout.fruchterman.reingold(person.g))
plot(person.g)
dev.off()

png(file="figures/revere-test.png", width=2000, height=1000)
lay <- layout.fruchterman.reingold(person.g)
pr.id <- 200
# Plot the eigevector and betweenness centrality
par(mfrow=c(1,2))
plot(bonpow(person.g, exponent=1), betweenness(person.g))

e.rank <- rank(-evcent(person.g)$vector)
b.rank <- rank(-betweenness(person.g))
c.rank <- rank(-bonpow(person.g, exponent=1))
s.top <- c.rank < 10 | b.rank < 10
text(bonpow(person.g)[s.top], betweenness(person.g)[s.top], cex=0.6, pos=4, labels=V(person.g)$name[s.top])
V(person.g)[pr.id]$color <- "yellow"
E(person.g)$color="grey95"
plot(person.g, layout=lay, vertex.size=2,
       vertex.label.cex=0.6, vertex.label=V(person.g)$name)
dev.off()
