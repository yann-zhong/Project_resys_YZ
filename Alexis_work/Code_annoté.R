library(Biobase)
library(GEOquery)
#Charger des bibliothèques utiles

setwd("C:/Users/Alexis Trang/Documents/Cours_UPMC_M2/RESYS/Alexis_work/copy_dataset")

gset <- getGEO("GSE21257", GSEMatrix =TRUE, AnnotGPL=FALSE)
#Charge les donnés GSE21257 depuis GEO, gset est une liste avec un seul élément dedans: GSE21257_series_matrix.txt.gz
#Ce seul élément contient les infos des expériences mentionnées dans le papier sur l'osteosarcome
if (length(gset) > 1) idx <- grep("GPL10295", attr(gset, "names")) else idx <- 1
#S'il y a plus d'un élément dans gset on cherche les indices de ceux dont le mot "GPL10295" dans leur nom
#On met ces nombres dans une liste nommée idx, dans notre cas comme il n'ya qu'un élément idx = 1
gset <- gset[[idx]]
#On ne garde que les éléments qui match nos critères
ex <- exprs(gset)
#Récupère la matrice d'écart type
exprs(gset) <- log2(ex)
#Puis met toutes ces valeurs au log2


annotation <- read.csv(file = 'GPL10295-tbl-1.txt', skip = 7, sep = '\t')
#Lit le fichier "GPL10295-tbl-1.txt" et le charge dans "annotation", saute les 7 premières lignes
#annotation contient 7 éléments: ID, Illumina_Probe ID, Illumina_Gene, Search key, GB_ACC, Symbol, et Sequence
#names(annotation)<-c("ID","Illumina_Probe ID", "Illumina_Gene", "Search key", "GB_ACC", "Symbol", "Sequence")
#set les bon noms de colonnes pour annotation
gexpIDs <- annotation[,c('ID', 'Illumina_Gene', 'Symbol')]
#met dans gexpIDS les colonnes ID, Illumina_Gene, et Symbol


# Start RTN
library(RTN)
#Charger la bibliothèque et les packages qui viennent avec
data(tniData)
#Charge dans l'environnement le dataset "tniData"

#???????????????
#data(dt4rtn)
#sensé faire la mm chose que data(tniData) mais avec le dataset dt4rtn, mais ce set n'exitse pas dans data()
#???????????????

tfs <- read.table("dt4rtn_TFs.txt")
#lit le fichier txt et le met dans une variable "tfs"
tfs = setNames(tfs$V2, tfs$V1)
#set the names
names(tfs) -> bkp
#puts all the names of tfs in bkp
tfs <- annotation$ID[match(names(tfs), annotation$Symbol)]
#met dans tfs les ID des probes/genes trouvées dans le tableau dt4rtn_TFs
tfs <- as.character(tfs)
#Met des guillements autour des éléments de tfs
names(tfs) <- bkp
#remet les noms de tfs au bon endroit
tfs <- tfs[-which(is.na(tfs))]
#retire tout éléments qui à la valeur NA
rtni <- tni.constructor(expData=ex, regulatoryElements=tfs, rowAnnotation=gexpIDs, cvfilter=T)
#main entry for the TNI pipe








