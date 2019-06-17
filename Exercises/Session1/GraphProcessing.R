library("igraph")
# read data file
usair <- read.graph("USAir97.net","pajek")

#info
is.directed(usair)
is.weighted(usair) # Strenght of the link: higher value=stronger link

#extract Vertices
V(usair)$id
V(usair)[3]
V(usair)[3]$id
vcount(usair)

#extract edges
E(usair)[1:10]
E(usair)[1:10]$weight
ecoun(usair)
ends(usair,20)
head_of(usair,1)
tail_of(usair,1)
ends(usair,20:30)

#Degree
us_deg <-degree(usair)
mean(us_deg)

#Weighted Degree
us_str<- strength(usair)
plot(us_str, us_deg)

#Degree Distribution
dd <- degree_distribution(usair)
plot(degree_distribution(usair),type="l")

#Distance
distances(usair,v=5) #http://igraph.org/r/doc/distances.html
max(distances(usair))
mean_distance(usair)

closeness(usair, vids=5, weights=rep(1,ecount(usair)))
1/closeness(usair, vids=5, weights=rep(1,ecount(usair)))
sum(distances(usair,v=5,weights=rep(1,ecount(usair)) ))

closeness(usair, vids=5)
1/closeness(usair, vids=5)
sum(distances(usair,v=5))

#Betweenness
us_betw<-betweenness(usair, directed=FALSE )
cor(us_deg, us_betw)
plot(us_deg,us_betw)
text(us_deg,us_betw,V(usair)$id)

us_betunw<-betweenness(usair, directed=FALSE, weights = rep(1,ecount(usair)))
cor(us_deg, us_betunw)
plot(us_deg,us_betunw)
text(us_deg,us_betunw,V(usair)$id)

plot(us_betunw, us_betw)
text(us_betunw,us_betw, V(usair)$id)

plot(us_str, us_betw)
text(us_str,us_betw,V(usair)$id)

E(usair)$dist = 1/E(usair)$weight
us_dist_betw<-betweenness(usair, directed=FALSE, weights=E(usair)$dist )
cor(us_betunw, us_dist_betw)
plot(us_betunw,us_dist_betw)
text(us_betunw,us_dist_betw,V(usair))

#Pagerank
us_prk <- page.rank(usair, directed=FALSE, damping=0.85 )$vector
cor(us_deg, us_prk)
cor(us_bet, us_prk)
plot(us_prk,us_bet)
text(us_prk,us_bet,V(usair)$id)

#Subgraph 
sub_usair <- induced_subgraph(usair,which(us_deg>mean(us_deg)),impl="auto")

#Shortest Path
sp <- get.shortest.paths(usair, 8, c(5,15, 188, 277, 316, 323) ,mode="all", output="both")
V(usair)[c(5,15, 188, 277, 316, 323)]
# 8 = Anchorage Intl
# 5 = Nome https://en.wikipedia.org/wiki/Nome_Airport
# 15= Napakiak https://en.wikipedia.org/wiki/Napakiak,_Alaska  
#188= Pueblo Memorial https://en.wikipedia.org/wiki/Pueblo_Memorial_Airport
#277= Waco Regional https://en.wikipedia.org/wiki/Waco_Regional_Airport
#316 Kahului https://en.wikipedia.org/wiki/Kahului_Airport
#323= Eugenio Maria De Hostos https://en.wikipedia.org/wiki/Eugenio_Mar%C3%ADa_de_Hostos_Airport

sp 
sp$vpath
sp$epath
unlist(sp$vpath)
unique(unlist(sp$vpath))
V(usair)[unique(unlist(sp$vpath))]


#Visualization: color,size, labels
plot(usair, layout=layout_with_fr, vertex.size=4,
     vertex.label=NA,
     vertex.color="red", edge.arrow.size=0.5)


plot(sub_usair, layout=layout_with_fr, vertex.size=4,
     vertex.label=V(sub_usair),
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)

V(sub_usair)[66]
V(usair)[66]




# Generate edge color variable:
ecol <- rep("gray80", ecount(usair))
ecol[unlist(sp$epath)] <- "orange"

# Generate edge width variable:
ew <- rep(0.5, ecount(usair))
ew[unlist(sp$epath)] <- 2

# Generate node color variable:
vcol <- rep("gray40", vcount(usair))
vcol[unlist(sp$vpath)] <- "gold"

# Generate node labels:
vlab <- rep("", vcount(usair))
vlab[unlist(sp$vpath)] <- unlist(sp$vpath)

# Generate node size variable:
vw <- rep(0.5, vcount(usair))
vw[unlist(sp$vpath)] <- 4

#Different Visualizations: Similarities vs Distances
plot(usair, layout=layout_with_fr, 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)

plot(usair, layout=layout_with_fr(usair, weight=NULL), 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)

plot(usair, layout=layout_with_fr(usair, weight=E(usair)$dist), 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)

plot(usair, layout=layout_with_drl, 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)

plot(usair, layout=layout_with_drl(usair, weight=NULL), 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)

plot(usair, layout=layout_with_drl(usair, weight=E(usair)$dist), 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)

# Kamada-Kawai
# http://igraph.org/r/doc/layout_with_kk.html
plot(usair, layout=layout_with_kk(usair, weights=NULL), 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)

plot(usair, layout=layout_with_kk(usair), 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)


plot(usair, layout=layout_with_kk(usair, weights=E(usair)$dist), 
     vertex.color=vcol, 
     vertex.size=vw, 
     vertex.label=vlab,
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)


neighborhood(usair, nodes=319)

