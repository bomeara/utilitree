independent.pairs <-
function(phy,multi2di=TRUE) {
  pairs<-data.frame()
  phy<-multi2di(phy)
  while(Ntip(phy)>1) { #if odd number of taxa, will have one left over when all done
    cherries<-find.cherries(phy)
    pairs<-rbind(pairs,cherries)
    phy<-drop.tip(phy,tip=unlist(cherries[,1:2]))
  }
   return(pairs)
}
