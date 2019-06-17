library("igraph")

# Pre processing

cond <- read.graph("small_data.gml")

is.directed(cond)
is.weighted(cond)
deg_cond <-degree(cond)
plot(degree.distribution(cond))

is.connected(cond, )


plot(cluster.distribution(cond))
clu_cond <-clusters(cond, mode="weak")
clu_cond$csize
clu_cond$membership

index_largest_Component <-which (clu_cond$csize==max(clu_cond$csize))
index_largest_Component
red_cond <-delete.vertices(cond,V(cond)[clu_cond$membership>index_largest_Component])
plot(degree.distribution(red_cond))

is.connected(red_cond )


write.graph(red_cond, "cond-mat-2003_largeComponent.net", format="pajek")
write.table(V(cond)$label, "cond-mat-2003_Labels.txt",col.names=FALSE, sep="\t")

lecond <- cluster_leading_eigen(cond)
lerdcond <- cluster_leading_eigen(red_cond)

modularity(cond, membership(lecond))
modularity(red_cond, membership(lerdcond))

length(lerdcond)
membership(lerdcond)
sizes(lerdcond)

V(red_cond)$color=lerdcond$membership
plot(red_cond, layout=layout_with_fr, vertex.size=4, vertex.label=NA )
plot(red_cond, layout=layout_with_dl, vertex.size=4, vertex.label=NA )

lvcond <- cluster_louvain(cond)
lvrdcond <- cluster_louvain(red_cond)
modularity(cond, membership(lvcond))
modularity(red_cond, membership(lvrdcond))
V(red_cond)$color=lvrdcond$membership
plot(red_cond, layout=layout_with_fr, vertex.size=4, vertex.label=NA )
sizes(lvrdcond)
plot(sizes(lvrdcond))
compare(lerdcond,lvrdcond)

