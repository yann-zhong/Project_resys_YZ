## ----label='Set server port', eval=TRUE------------------------------------
library(RedeR)
rdp <- RedPort() 

## ----label='Main call', eval=FALSE-----------------------------------------
#  calld(rdp)

## ----label='Add graph (not included)', eval=TRUE, include=FALSE------------
library(igraph)
g1 <- graph.lattice(c(5,5,5))
addGraph( rdp, g1, layout.kamada.kawai(g1) )

## ----label='Add graph', eval=FALSE, results='hide'-------------------------
#  library (igraph)
#  g1 <- graph.lattice(c(5,5,5))
#  addGraph( rdp, g1, layout.kamada.kawai(g1) )

## ----label='Get graph', eval=TRUE, results='hide'--------------------------
g2 <- getGraph(rdp)
resetd(rdp)

## ----label='Build subgraphs', eval=TRUE, results='hide'--------------------
g3 <- barabasi.game(10)
g4 <- barabasi.game(10)
V(g3)$name<-paste("sn",1:10,sep="")
V(g4)$name<-paste("sm",1:10,sep="")
addGraph(rdp, g3, isNest =TRUE, gcoord=c(25,25), gscale=50)
addGraph(rdp, g4, isNest =TRUE, gcoord=c(75,75), gscale=50)

## ----label='Get subgraph', eval=TRUE, results='hide'-----------------------
selectNodes(rdp,"N0")
g5 <- getGraph(rdp, status= "selected")
resetd(rdp)

## ----label='Build scale-free graph and send to the app', eval=TRUE, results='hide'----
g6 <- barabasi.game(500)
addGraph(rdp, g6, zoom=20)

## ----label='Start relax', eval=TRUE, results='hide'------------------------
relax(rdp,p2=400,p5=30,ps=T)

## ----label='Workflow 1: get a dataframe and an interactome', eval=TRUE, results='hide'----
data(ER.limma)
data(hs.inter)
dt <- ER.limma
gi <- hs.inter

## ----label='Workflow 1: extract a subgraph and set attributes to RedeR', eval=TRUE, results='hide'----
gt3  <- subg(g=gi, dat=dt[dt$degenes.t3!=0,], refcol=1)
gt3  <- att.setv(g=gt3, from="Symbol", to="nodeAlias")
gt3  <- att.setv(g=gt3, from="logFC.t3", to="nodeColor", breaks=seq(-2,2,0.4), pal=2)

## ----label='Workflow 1: extract another subgraph and set attributes to RedeR', eval=TRUE, results='hide'----
gt6  <- subg(g=gi, dat=dt[dt$degenes.t6!=0,], refcol=1)
gt6  <- att.setv(g=gt6, from="Symbol", to="nodeAlias")
gt6  <- att.setv(g=gt6, from="logFC.t6", to="nodeColor", breaks=seq(-2,2,0.4), pal=2)

## ----label='=Workflow 1: extract another subgraph and set attributes to RedeR', eval=TRUE, results='hide'----
gt12 <- subg(g=gi, dat=dt[dt$degenes.t12!=0,], refcol=1)
gt12 <- att.setv(g=gt12, from="Symbol", to="nodeAlias")
gt12 <- att.setv(g=gt12, from="logFC.t12", to="nodeColor", breaks=seq(-2,2,0.4), pal=2)

## ----label='Workflow 1: add subgraphs to the app', eval=TRUE, results='hide'----
addGraph(rdp, gt3, gcoord=c(10,25), gscale=20, isNest=TRUE, theme='tm1', zoom=30)
addGraph(rdp, gt6, gcoord=c(20,70), gscale=50, isNest=TRUE, theme='tm1', zoom=30)
addGraph(rdp, gt12, gcoord=c(70,55), gscale=80, isNest=TRUE, theme='tm1', zoom=30)

## ----label='Workflow 1: nest subgraphs', eval=TRUE, results='hide'---------
nestNodes(rdp, nodes=V(gt3)$name, parent="N1", theme='tm2')
nestNodes(rdp, nodes=V(gt6)$name, parent="N2", theme='tm2')
nestNodes(rdp, nodes=V(gt3)$name, parent="N4", theme='tm3')

## ----label='Workflow 1: assign edges to containers', eval=TRUE, results='hide'----
mergeOutEdges(rdp)

## ----label='Workflow 1: relax the network', eval=TRUE, results='hide'------
relax(rdp,50,400)

## ----label='Workflow 1: add a color legend (other types are available)', eval=TRUE, results='hide'----
scl <- gt3$legNodeColor$scale
leg <- gt3$legNodeColor$legend 
addLegend.color(rdp, colvec=scl, labvec=leg, title="node color (logFC)")

## ----label='Workflow 1: select a gene', eval=TRUE, results='hide'----------
selectNodes(rdp,"RET")

## ----label='Workflow 1: reset graph', eval=TRUE, results='hide'------------
resetd(rdp)

## ----label='get a dataframe and an igraph object', eval=TRUE, results='hide'----
data(ER.deg)
dt <- ER.deg$dat
sg <- ER.deg$ceg

## ----label='Workflow 2: map the dataframe to the graph', eval=TRUE, results='hide'----
sg <- att.mapv(sg, dat=dt, refcol=1)

## ----label='Workflow 2: set attributes to RedeR', eval=TRUE, results='hide'----
sg <- att.setv(sg, from="Symbol", to="nodeAlias")
sg <- att.setv(sg, from="logFC.t3", to="nodeColor", breaks=seq(-1,1,0.2), pal=2)	
sg <- att.setv(sg, from="ERbdist", to="nodeSize", nquant=10, isrev=TRUE, xlim=c(5,40,1))

## ----label='Workflow 2: add graph to the app', eval=TRUE, results='hide'----
addGraph(rdp,sg)

## ----label='Workflow 2: compute hierarchical clustering', eval=TRUE, results='hide'----
hc <- hclust(dist(get.adjacency(sg, attr="weight")))

## ----label='Workflow 2: map the hclust object onto the network', eval=TRUE, results='hide'----
nestID <- nesthc(rdp,hc, cutlevel=3, nmemb=5, cex=0.3, labels=V(sg)$nodeAlias)

## ----label='Workflow 2: assign edges to containers', eval=TRUE, results='hide'----
mergeOutEdges(rdp,nlev=2)

## ----label='Workflow 2: relax the network', eval=TRUE, results='hide'------
relax(rdp)

## ----label='Workflow 2: add color legend', eval=TRUE, results='hide'-------
scl <- sg$legNodeColor$scale
leg <- sg$legNodeColor$legend
addLegend.color(rdp, colvec=scl, labvec=leg, title="diff. gene expression (logFC)")

## ----label='Workflow 2: add node size legend', eval=TRUE, results='hide'----
scl <- sg$legNodeSize$scale
leg <- sg$legNodeSize$legend
addLegend.size(rdp, sizevec=scl, labvec=leg, title="bd site distance (kb)")

## ----label='Workflow 2: reset graph', eval=TRUE, results='hide'------------
resetd(rdp)

## ----label='Session information', eval=TRUE, echo=FALSE--------------------
sessionInfo()

