find.cherries <-
function(phy) {
  focalNodes<-which(tabulate(phy$edge[which(phy$edge[,2]<=Ntip(phy)),1])==2)
  returnTaxa<-function(focalNode,phy) {
    return( c(phy$edge[which(phy$edge[,1]==focalNode ),2]))
  }
  returnBrlen<-function(focalTaxon,phy) {
    return( phy$edge.length[which(phy$edge[,2] == focalTaxon )] )
  }
  focalTaxa<-sapply(focalNodes,returnTaxa,phy)
  focalBrlen<-matrix(sapply(focalTaxa,returnBrlen,phy),ncol=2,byrow=TRUE)
  focalTaxa<-matrix(as.character(phy$tip.label[c(focalTaxa)]),ncol=2,byrow=TRUE)
  result<-data.frame(focalTaxa,focalBrlen,stringsAsFactors=FALSE)
  names(result)<-c("taxon1","taxon2","brlen1","brlen2")
  return(result)
}
