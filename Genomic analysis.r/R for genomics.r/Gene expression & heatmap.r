###Gene Expression Analysis ####

####Gene expression analysis #### https://www.youtube.com/watch?v=6I_8LthpCyY&list=PLe1-kjuYBZ04qLxtZ7E46f3II07vFu-w4&index=3
install.packages("pheatmap")
library(pheatmap)

sewtwd()
#read the count data, we are using normalised read counts
fpkn_genes=read.csv('fpkn_genes.cvs', row.names = 1)

dim(fpkn_genes)
head(fpkn_genes)

#generate heatmap using default paramaters
pheatmap(fpkn_genes)

#generate heatmap without clustering, we will disable clustering for both x and y axis
pheatmap(fpkn_genes, cluster_rows = F, cluster_cols=F)


#add annotation color bar
annots_col = data.frame(
  Group = c(rep("Male",6), rep("Female", 6)),
  row.names = colnames(fpkn_genes)
)
#plot
pheatmap(fkpn_genes, annotation_col=annots_col)

#exchange the angle of the x-axis labels
pheatmap(fpkn_genes, cluster_rows = FALSE, angle_col=45, cluster_cols=FALSE)


#save the heatmap plot to an output file


#open the png file
png('heatmap_unclustered.png')

#generate heatmap
pheatmap(fpkn_genes, cluster_rows = F, cluster_cols - F)

#close the file
dev.off()
