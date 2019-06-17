library(igraph)

collab <-read.graph("netscicoauthor2010.net", format="pajek")

is.connected(collab)
is.weighted(collab)

clu_info <- cluster_infomap(collab)
modularity(collab, membership(clu_info))

V(collab)$color<-membership(clu_info)

plot(collab, layout=layout_with_drl, vertex.size=4, vertex.label=NA )


clu_lvn <- cluster_louvain(collab)
modularity(collab, membership(clu_lvn))

V(collab)$color<-membership(clu_lvn)

plot(collab, layout=layout_with_drl, vertex.size=4, vertex.label=NA )

compare(clu_info$membership, clu_lvn$membership)

compare(clu_info$membership, clu_lvn$membership, method='rand')

compare(clu_info$membership, clu_lvn$membership, method='nmi')

x <- scan("netscicoauthor2010_undir.clu", what="", sep="\n")
clu_orig = as.list(x[2:553])

compare(clu_info$membership, clu_orig, method='rand')
