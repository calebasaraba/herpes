
##########################################################################################

#Read in potentially important libraries
library(ape)
library(phangorn)
library(phytools)

##########################################################################################

#Read in alignment data
tree.dat.phy<-read.phyDat(file='alignment_all.nex',format='nexus',type='DNA')

##########################################################################################

#Determine which model fits the data best

test.models<-modelTest(tree.dat.phy,G=TRUE,I=TRUE)
write.csv(test.models, 'max_like_model_choice.csv',row.names=FALSE)
test.models.comp<-read.csv('max_like_model_choice.csv')

#best by AIC/logLik: GTR+G+I
#best according to Mega: GTR+G

##########################################################################################

#Calculate maximum likelihood tree

init.tree<-rtree(n=length(tree.dat.phy),tip.label=names(tree.dat.phy),rooted=FALSE)
obs<-pml(init.tree,data=tree.dat.phy,k=4)

fit.tree.gi<-optim.pml(obs,model='GTR',optNni=TRUE,optGamma=TRUE,optInv=TRUE,optEdge=TRUE,control=pml.control(trace = 0))
fit.tree.g<-optim.pml(obs,model='GTR',optNni=TRUE,optGamma=TRUE,optEdge=TRUE,control=pml.control(trace = 0))

##########################################################################################

#Bootstrap
bs.gi<-bootstrap.pml(fit.tree.gi,bs=100,optNni=TRUE,control=pml.control(trace = 0))
bs.g<-bootstrap.pml(fit.tree.g,bs=100,optNni=TRUE,control=pml.control(trace = 0))
#cnet <- consensusNet(bs, p=0.2)

saveRDS(fit.tree.gi, "GTR_GI_fit.rds")
saveRDS(fit.tree.g, "GTR_G_fit.rds")

saveRDS(bs.gi, "GTR_GI_bs.rds")
saveRDS(bs.g, "GTR_G_bs.rds")
###############################################################################

#Plots

new.labels<-c()
for(i in 1:1276){
  new.labels<-c(new.labels,'')
}
fit.tree.gi$tree$tip.label<-new.labels
fit.tree.g$tree$tip.label<-new.labels

pdf(file='gtr_gi.pdf')
plotTree(fit.tree.gi$tree)
plotBS(midpoint(fit.tree.gi$tree), bs.gi, p = 50, type="p", cex=0.5, show.tip.label=F)
dev.off()

pfd(file='/ifs/scratch/msph/ehs/sck2165/herpes/gtr_g.pdf')
plotTree(fit.tree.g$tree)
plotBS(midpoint(fit.tree.g$tree), bs.g, p = 50, type="p", cex=0.5, show.tip.label=F)
dev.off()

#plot(cnet, "2D", show.edge.label=TRUE)

fit_tree_gi <- readRDS("GTR_GI_fit.rds")


###############################################################################

write.tree(fit_tree_gi$tree, file = "gtr_gi.tree", append = FALSE,
           digits = 10, tree.names = FALSE)
