BiocManager::install("Biobase")
BiocManager::install("GEOquery")
BiocManager::install("RTN")
library(Biobase)
library(GEOquery)

# load series and platform data from GEO

gset <- getGEO("GSE21257", GSEMatrix =TRUE, AnnotGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL10295", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# log2 transform
ex <- exprs(gset)
exprs(gset) <- log2(ex)

# Get the GENE NAMES
# Download full table here https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL10295
annotation <- read.csv(file = 'GPL10295.txt', skip = 7, sep = '\t')

gexpIDs <- annotation[,c('ID', 'Illumina_Gene', 'Symbol')]

# Start RTN
library(RTN)
data(tniData)
data(dt4rtn)




tfs <- read.table("dt4rtn_TFs.txt")
tfs = setNames(tfs$V2, tfs$V1)
names(tfs) -> bkp
tfs <- annotation$ID[match(names(tfs), annotation$Symbol)]
tfs <- as.character(tfs)
names(tfs) <- bkp

tfs <- tfs[-which(is.na(tfs))]

rtni <- tni.constructor(expData=ex, regulatoryElements=tfs, rowAnnotation=gexpIDs, cvfilter=T)

