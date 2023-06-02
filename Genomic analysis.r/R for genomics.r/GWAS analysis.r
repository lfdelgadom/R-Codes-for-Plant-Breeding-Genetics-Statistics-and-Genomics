#GWAS Analysis
#Intalling GAPIT
source("http://zzlab.net/GAPIT/GAPIT.library.R")
source("http://zzlab.net/GAPIT/gapit_functions.txt")

#Importing Genotyping data
myG<-read.table("W:/Andy/Co-operation/GWAS workshop_Tunisa R working group/data/Watkins_axiom35k_genotype_for_Tassel.hmp.txt",head = FALSE)
#Import Phenotyping data
myY <- read.table("W:/Andy/Co-operation/GWAS workshop_Tunisa R working group/data/Watkins_phenotype_H2006_for_Tassel.txt", head = TRUE)
#Running GWAS
myGAPIT<-GAPIT(
  Y=myY,
  G=myG,
  PCA.total=5,
  SNP.MAF =0.05,
  Multiple_analysis = TRUE,
  model=c("GLM","MLM"),
  Random.model=FALSE
)


