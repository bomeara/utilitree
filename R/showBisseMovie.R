showBisseMovie <-
function(q0=0.0,q1=0.555,b0=5.199,b1=2.933,d0=5.433,d1=2.319) {
  quartz()
  ani.start(nmax=1000,interval=1,title="Diversification and Character Evolution")
  bisse.ani(q0=q0,q1=q1,b0=b0,b1=b1,d0=d0,d1=d1)
  ani.stop()
}
